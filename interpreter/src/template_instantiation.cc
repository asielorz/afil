#include "template_instantiation.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "incomplete_statement.hh"
#include "complete_expression.hh"
#include "interpreter.hh"
#include "constexpr.hh"
#include "utils/algorithm.hh"
#include "utils/out.hh"
#include "utils/variant.hh"
#include "utils/overload.hh"
#include "utils/utils.hh"
#include "utils/unreachable.hh"
#include "utils/warning_macro.hh"
#include <cassert>

using namespace std::literals;

auto evaluate_constant_expression(complete::Expression const & expression, complete::Program const & program, int constant_base_index, void * outValue) noexcept -> void
{
	raise_syntax_error_if_not(is_constant_expression(expression, program, constant_base_index), "Cannot evaluate expression at compile time.");

	interpreter::ProgramStack stack;
	alloc_stack(stack, 256);

	interpreter::eval_expression(expression, stack, program);

	memcpy(outValue, pointer_at_address(stack, 0), expression_type_size(expression, program));
}

auto evaluate_array_size_expression(complete::Expression expression, complete::Program const & program, int constant_base_index) noexcept -> int
{
	expression = insert_conversion_node(std::move(expression), complete::TypeId::int_, program);
	int result;
	evaluate_constant_expression(expression, program, constant_base_index, &result);
	return result;
}

namespace instantiation
{
	auto top(ScopeStack & scope_stack) noexcept -> complete::Scope & { return *scope_stack.back().scope; }
	auto top(ScopeStackView scope_stack) noexcept -> complete::Scope const & { return *scope_stack.back().scope; }

	template <typename Stack>
	struct StackGuard
	{
		StackGuard(Stack & s) noexcept : stack(std::addressof(s)) {}
		StackGuard(StackGuard const &) = delete;
		StackGuard & operator = (StackGuard const &) = delete;
		~StackGuard() { stack->pop_back(); }

		Stack * stack;
	};

	auto next_block_scope_offset(ScopeStackView scope_stack) -> int
	{
		return scope_stack.back().scope_offset + top(scope_stack).stack_frame_size;
	}

	auto push_block_scope(ScopeStack & scope_stack, complete::Scope & scope) noexcept -> StackGuard<ScopeStack>
	{
		int const offset = next_block_scope_offset(scope_stack);
		scope_stack.push_back({&scope, ScopeType::block, offset});
		return StackGuard<ScopeStack>(scope_stack);
	}

	auto does_name_collide(ScopeStackView scope_stack, std::string_view name) noexcept -> bool
	{
		auto const visitor = overload(
			[](lookup_result::Nothing const &) { return false; },
			[](auto const &) { return true; }
		);

		auto const lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
	}

	auto does_function_name_collide(ScopeStackView scope_stack, std::string_view name) noexcept -> bool
	{
		auto const visitor = overload(
			[](lookup_result::Nothing const &) { return false; },
			[](lookup_result::OverloadSet const &) { return false; },
			[](auto const &) { return true; }
		);

		auto const lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
	}

	auto resolve_dependent_type(
		incomplete::TypeId const & dependent_type, 
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program) 
		-> expected<complete::TypeId, SyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::TypeId, SyntaxError>
			{
				complete::TypeId const type = type_with_name(base_case.name, scope_stack, template_parameters);
				raise_syntax_error_if_not(type != complete::TypeId::none, "Type not found.");
				return type;
			},
			[&](incomplete::TypeId::Pointer const & pointer) -> expected<complete::TypeId, SyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*pointer.pointee, template_parameters, scope_stack, program));
				return pointer_type_for(pointee, *program);
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::TypeId, SyntaxError>
			{
				try_call_decl(complete::TypeId const value_type, resolve_dependent_type(*array.value_type, template_parameters, scope_stack, program));

				try_call_decl(complete::Expression size_expr, instantiate_expression(*array.size, template_parameters, scope_stack, program, nullptr));
				return array_type_for(value_type, evaluate_array_size_expression(std::move(size_expr), *program, next_block_scope_offset(scope_stack)), *program);
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::TypeId, SyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*array_pointer.pointee, template_parameters, scope_stack, program));
				return array_pointer_type_for(pointee, *program);
			},
			[&](incomplete::TypeId::TemplateInstantiation const & template_instantiation) -> expected<complete::TypeId, SyntaxError>
			{
				auto const visitor = overload(
					[](lookup_result::StructTemplate result) -> complete::StructTemplateId { return result.template_id; },
					[](auto const &) -> complete::StructTemplateId { declare_unreachable(); }
				);

				auto lookup = lookup_name(scope_stack, template_instantiation.template_name);
				complete::StructTemplateId const template_id = std::visit(visitor, lookup);

				std::vector<complete::TypeId> parameters;
				parameters.reserve(template_instantiation.parameters.size());
				for (incomplete::TypeId const & parameter : template_instantiation.parameters)
					try_call(parameters.push_back, resolve_dependent_type(parameter, template_parameters, scope_stack, program));

				return instantiate_struct_template(*program, template_id, parameters);
			},
			[](incomplete::TypeId::Deduce const &) -> expected<complete::TypeId, SyntaxError>
			{
				return complete::TypeId::deduce;
			}
		);

		try_call_decl(complete::TypeId type, std::visit(visitor, dependent_type.value));
		type.is_reference = dependent_type.is_reference;
		type.is_mutable = dependent_type.is_mutable;
		return type;
	}

	auto resolve_function_template_parameter_type(
		incomplete::TypeId const & dependent_type,
		std::vector<complete::ResolvedTemplateParameter> & resolved_template_parameters,
		span<incomplete::TemplateParameter const> unresolved_template_parameters, 
		ScopeStack & scope_stack, out<complete::Program> program)
		-> expected<complete::FunctionTemplateParameterType, SyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				complete::TypeId const type = type_with_name(base_case.name, scope_stack, resolved_template_parameters);
				if (type != complete::TypeId::none)
					return complete::FunctionTemplateParameterType{complete::FunctionTemplateParameterType::BaseCase{type}, false, false};

				auto const it = std::find_if(unresolved_template_parameters, [&](incomplete::TemplateParameter const & param)
				{
					return param.name == base_case.name;
				});
				
				if (it != unresolved_template_parameters.end())
				{
					return complete::FunctionTemplateParameterType{
						complete::FunctionTemplateParameterType::TemplateParameter{static_cast<int>(it - unresolved_template_parameters.begin())}, false, false
					};
				}

				declare_unreachable();
			},
			[&](incomplete::TypeId::Pointer const & pointer) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				complete::FunctionTemplateParameterType::Pointer pointer_type;
				try_call(assign_to(pointer_type.pointee), 
					resolve_function_template_parameter_type(*pointer.pointee, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));
				return complete::FunctionTemplateParameterType{pointer_type, false, false};
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				complete::FunctionTemplateParameterType::Array array_type;
				try_call(assign_to(array_type.value_type), 
					resolve_function_template_parameter_type(*array.value_type, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));

				try_call_decl(complete::Expression size_expr, instantiate_expression(*array.size, resolved_template_parameters, scope_stack, program, nullptr));
				array_type.size = evaluate_array_size_expression(std::move(size_expr), *program, next_block_scope_offset(scope_stack));
				return complete::FunctionTemplateParameterType{array_type, false, false};
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				complete::FunctionTemplateParameterType::ArrayPointer array_pointer_type;
				try_call(assign_to(array_pointer_type.pointee),
					resolve_function_template_parameter_type(*array_pointer.pointee, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));
				return complete::FunctionTemplateParameterType{array_pointer_type, false, false};
			},
			[](incomplete::TypeId::TemplateInstantiation const & /*template_instantiation*/) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				mark_as_to_do("Dependent template instantiations");
			},
			[](incomplete::TypeId::Deduce const &) -> expected<complete::FunctionTemplateParameterType, SyntaxError>
			{
				declare_unreachable();
			}
		);

		try_call_decl(complete::FunctionTemplateParameterType type, std::visit(visitor, dependent_type.value));
		type.is_reference = dependent_type.is_reference;
		type.is_mutable = dependent_type.is_mutable;
		return type;
	}

	auto lookup_name(ScopeStackView scope_stack, std::string_view name) noexcept
		->std::variant<
			lookup_result::Nothing,
			lookup_result::Variable,
			lookup_result::Constant,
			lookup_result::GlobalVariable,
			lookup_result::OverloadSet,
			lookup_result::Type,
			lookup_result::StructTemplate
		>
	{
		using namespace complete;
		lookup_result::OverloadSet overload_set;

		bool stop_looking_for_variables = false;

		// Search the scopes in reverse order.
		int const start = static_cast<int>(scope_stack.size() - 1);
		for (int i = start; i >= 0; --i)
		{
			Scope const & scope = *scope_stack[i].scope;

			// Search variables and types only if we don't already know this is a function name.
			if (overload_set.function_ids.empty())
			{
				if (scope_stack[i].type == ScopeType::global)
				{
					auto const var = std::find_if(scope.variables, [name](Variable const & var) { return var.name == name; });
					if (var != scope.variables.end())
						return lookup_result::GlobalVariable{var->type, var->offset};
				}
				else if (!stop_looking_for_variables)
				{
					auto const var = std::find_if(scope.variables, [name](Variable const & var) { return var.name == name; });
					if (var != scope.variables.end())
						return lookup_result::Variable{var->type, var->offset};
				}

				auto const type = std::find_if(scope.types, [name](TypeName const & var) { return var.name == name; });
				if (type != scope.types.end())
					return lookup_result::Type{type->id};

				auto const struct_template = std::find_if(scope.struct_templates, [name](StructTemplateName const & var) { return var.name == name; });
				if (struct_template != scope.struct_templates.end())
					return lookup_result::StructTemplate{struct_template->id};

				auto const constant = std::find_if(scope.constants, [name](Constant const & var) { return var.name == name; });
				if (constant != scope.constants.end())
					return lookup_result::Constant{&*constant};
			}

			// Functions.
			for (FunctionName const & fn : scope.functions)
				if (fn.name == name)
					overload_set.function_ids.push_back(fn.id);

			// Function templates.
			for (FunctionTemplateName const & fn : scope.function_templates)
				if (fn.name == name)
					overload_set.function_template_ids.push_back(fn.id);

			// After we leave a function, stop looking for variables.
			if (i < start && scope_stack[i].type == ScopeType::function)
				stop_looking_for_variables = true;
		}

		if (overload_set.function_ids.empty() && overload_set.function_template_ids.empty())
			return lookup_result::Nothing();
		else
			return overload_set;
	}

	auto named_overload_set(std::string_view name, ScopeStackView scope_stack) -> complete::OverloadSet
	{
		auto const visitor = overload(
			[](lookup_result::Nothing const &) -> complete::OverloadSet
			{
				return {};
			},
			[](lookup_result::OverloadSet const & overload_set) -> complete::OverloadSet
			{
				return overload_set;
			},
			[](auto const &) -> complete::OverloadSet { declare_unreachable(); }
		);

		auto lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
	}

	auto operator_overload_set(Operator op, ScopeStackView scope_stack) noexcept -> complete::OverloadSet
	{
		return named_overload_set(operator_function_name(op), scope_stack);
	}

	auto type_with_name(std::string_view name, ScopeStackView scope_stack) noexcept -> complete::TypeId
	{
		auto const visitor = overload(
			[](lookup_result::Type const & type) -> complete::TypeId
			{
				return type.type_id;
			},
			[](auto const &) -> complete::TypeId
			{
				return complete::TypeId::none;
			}
		);

		auto lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
	}

	auto type_with_name(std::string_view name, ScopeStackView scope_stack, span<complete::ResolvedTemplateParameter const> template_parameters) noexcept -> complete::TypeId
	{
		complete::TypeId const type = type_with_name(name, scope_stack);
		if (type != complete::TypeId::none)
			return type;

		auto const it = std::find_if(template_parameters, [name](complete::ResolvedTemplateParameter const & param)
		{
			return param.name == name;
		});
		if (it != template_parameters.end())
			return it->type;

		return complete::TypeId::none;
	}

	auto type_descriptor_for(complete::TypeId type_id, complete::Program const & program) noexcept -> callc::TypeDescriptor
	{
		if (type_id.is_reference)
			return {sizeof(void *), false};

		bool const is_float = decay(type_id) == complete::TypeId::float_;

		complete::Type const & type = type_with_id(program, type_id);
		if (type.size > 8)
			mark_as_to_do("Big types");

		return {type.size, is_float};
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, SyntaxError>;

	auto instantiate_function_prototype(
		incomplete::FunctionPrototype const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		out<complete::Function> function
	) -> expected<void, SyntaxError>
	{

		for (incomplete::FunctionParameter const & parameter : incomplete_function.parameters)
		{
			try_call_decl(complete::TypeId const parameter_type, resolve_dependent_type(parameter.type, template_parameters, scope_stack, program));
			add_variable_to_scope(*function, parameter.name, parameter_type, 0, *program);
		}

		function->parameter_count = static_cast<int>(incomplete_function.parameters.size());
		function->parameter_size = function->stack_frame_size;

		if (incomplete_function.return_type.has_value())
			try_call(assign_to(function->return_type), resolve_dependent_type(*incomplete_function.return_type, template_parameters, scope_stack, program))
		else
			function->return_type = complete::TypeId::deduce;

		return success;
	}

	auto instantiate_function_body(
		incomplete::Function const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		out<complete::Function> function
	) -> expected<void, SyntaxError>
	{
		scope_stack.push_back({ &*function, ScopeType::function, 0 });

		function->statements.reserve(incomplete_function.statements.size());
		for (incomplete::Statement const & substatment : incomplete_function.statements)
		{
			try_call_decl(auto complete_substatement, instantiate_statement(substatment, template_parameters, scope_stack, program, out(function->return_type)));
			if (complete_substatement.has_value())
				function->statements.push_back(std::move(*complete_substatement));
		}

		scope_stack.pop_back();

		function->is_callable_at_compile_time = can_be_run_in_a_constant_expression(*function, *program);

		return success;
	}

	auto instantiate_function_template(
		incomplete::Function const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> expected<complete::Function, SyntaxError>
	{
		complete::Function function;

		try_call_void(instantiate_function_prototype(incomplete_function, template_parameters, scope_stack, program, out(function)));
		try_call_void(instantiate_function_body(incomplete_function, template_parameters, scope_stack, program, out(function)));

		return function;
	}

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<complete::Expression, SyntaxError>
	{
		auto const visitor = overload(
			[](incomplete::expression::Literal<int> const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				return complete::expression::Literal<int>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<float> const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				return complete::expression::Literal<float>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<bool> const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				return complete::expression::Literal<bool>{incomplete_expression.value};
			},
			[&](incomplete::expression::Literal<std::string> const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::expression::StringLiteral complete_expression;
				complete_expression.value = incomplete_expression.value;
				complete_expression.type = array_type_for(complete::TypeId::char_, static_cast<int>(complete_expression.value.size()), *program);
				return complete_expression;
			},
			[&](incomplete::expression::Identifier const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				auto const lookup = lookup_name(scope_stack, incomplete_expression.name);
				auto const lookup_visitor = overload(
					[](lookup_result::Variable const & var) -> complete::Expression
					{
						complete::expression::LocalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[](lookup_result::Constant const & var) -> complete::Expression
					{
						complete::expression::Constant complete_expression;
						complete_expression.type = var.constant->type;
						complete_expression.value = var.constant->value;
						return complete_expression;
					},
					[](lookup_result::GlobalVariable const & var) -> complete::Expression
					{
						complete::expression::GlobalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[](lookup_result::OverloadSet const & var) -> complete::Expression
					{
						complete::expression::OverloadSet complete_expression;
						complete_expression.overload_set = var; // Move?
						return complete_expression;
					},
					[](auto const &) -> complete::Expression { declare_unreachable(); }
				);
				return std::visit(lookup_visitor, lookup);
			},
			[&](incomplete::expression::MemberVariable const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::expression::MemberVariable complete_expression;
				try_call(assign_to(complete_expression.owner), instantiate_expression(*incomplete_expression.owner, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const owner_type_id = decay(expression_type_id(*complete_expression.owner, *program));
				complete::Type const & owner_type = type_with_id(*program, owner_type_id);
				raise_syntax_error_if_not(is_struct(owner_type), "Cannot access member of non struct type.");
				complete::Struct const & owner_struct = *struct_for_type(*program, owner_type);
				int const member_index = find_member_variable(owner_struct, incomplete_expression.name);
				raise_syntax_error_if_not(member_index != -1, "Member not found.");

				complete_expression.variable_offset = owner_struct.member_variables[member_index].offset;
				complete_expression.variable_type = owner_struct.member_variables[member_index].type;
				complete_expression.variable_type.is_reference = owner_type_id.is_reference;
				complete_expression.variable_type.is_mutable = owner_type_id.is_mutable;

				return complete_expression;
			},
			[&](incomplete::expression::Addressof const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				raise_syntax_error_if_not(operand_type.is_reference, "Attempted to take address of temporary.");
				complete::TypeId const pointer_type = pointer_type_for(remove_reference(operand_type), *program);

				complete::expression::ReinterpretCast complete_expression;
				complete_expression.operand = allocate(std::move(operand));
				complete_expression.return_type = pointer_type;
				return complete_expression;
			},
			[&](incomplete::expression::Dereference const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(operand, *program);
				complete::Type const operand_type = type_with_id(*program, operand_type_id);

				if (is_pointer(operand_type))
				{
					complete::TypeId pointee_type = try_get<complete::Type::Pointer>(operand_type.extra_data)->value_type;
					pointee_type.is_reference = true;

					complete::expression::ReinterpretCast complete_expression;
					complete_expression.return_type = pointee_type;
					complete_expression.operand = allocate(insert_conversion_node(std::move(operand), operand_type_id, decay(operand_type_id), *program));
					return complete_expression;
				}
				else
				{
					FunctionId const function = resolve_function_overloading_and_insert_conversions(
						operator_overload_set(Operator::dereference, scope_stack), { &operand, 1 }, { &operand_type_id, 1 }, *program);
					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found for dereference operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.push_back(std::move(operand));
					return complete_expression;
				}
			},
			[&](incomplete::expression::Subscript const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Expression array, instantiate_expression(*incomplete_expression.array, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const array_type_id = expression_type_id(array, *program);
				complete::Type const array_type = type_with_id(*program, array_type_id);

				if (is_array(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::Array>(array_type.extra_data)->value_type;
					value_type.is_reference = array_type_id.is_reference;

					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program, current_scope_return_type));

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					complete_expression.array = allocate(std::move(array));
					complete_expression.index = allocate(insert_conversion_node(std::move(index), complete::TypeId::int_, *program));
					return complete_expression;
				}
				else if (is_array_pointer(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::ArrayPointer>(array_type.extra_data)->value_type;
					value_type.is_reference = true;

					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program, current_scope_return_type));

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					complete_expression.array = allocate(insert_conversion_node(std::move(array), decay(array_type_id), *program));
					complete_expression.index = allocate(insert_conversion_node(std::move(index), complete::TypeId::int_, *program));
					return complete_expression;
				}
				else
				{
					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program, current_scope_return_type));
					complete::TypeId const index_type_id = expression_type_id(index, *program);

					complete::TypeId const param_types[] = { array_type_id, index_type_id };
					complete::Expression params[] = { std::move(array), std::move(index) };

					FunctionId const function = resolve_function_overloading_and_insert_conversions(named_overload_set("[]"sv, scope_stack), params, param_types, *program);
					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found for subscript operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(params[0]));
					complete_expression.parameters.push_back(std::move(params[1]));
					return complete_expression;
				}
			},
			[&](incomplete::expression::Function const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Function complete_function, instantiate_function_template(incomplete_expression.function, template_parameters, scope_stack, program));
				FunctionId const function_id = add_function(*program, std::move(complete_function));

				complete::expression::OverloadSet complete_expression;
				complete_expression.overload_set.function_ids.push_back(function_id);
				return complete_expression;
			},
			[&](incomplete::expression::FunctionTemplate const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::FunctionTemplate new_function_template;
				new_function_template.scope_template_parameters = template_parameters;
				new_function_template.incomplete_function = incomplete_expression.function_template;
				new_function_template.parameter_types.reserve(incomplete_expression.function_template.parameters.size());
				for (incomplete::FunctionParameter const & param : incomplete_expression.function_template.parameters)
				{
					try_call(new_function_template.parameter_types.push_back, resolve_function_template_parameter_type(
						param.type,
						template_parameters,
						incomplete_expression.function_template.template_parameters,
						scope_stack,
						program
					));
				}

				FunctionTemplateId const template_id =  add_function_template(*program, std::move(new_function_template));

				complete::expression::OverloadSet complete_expression;
				complete_expression.overload_set.function_template_ids.push_back(template_id);
				return complete_expression;
			},
			[&](incomplete::expression::FunctionCall const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				std::vector<complete::Expression> parameters;
				parameters.reserve(incomplete_expression.parameters.size());
				for (incomplete::Expression const & incomplete_param : incomplete_expression.parameters)
					try_call(parameters.push_back, instantiate_expression(incomplete_param, template_parameters, scope_stack, program, current_scope_return_type));

				if (has_type<complete::expression::OverloadSet>(parameters[0]))
				{
					std::vector<complete::TypeId> parameter_types;
					parameter_types.reserve(parameters.size() - 1);
					for (size_t i = 1; i < parameters.size(); ++i)
						parameter_types.push_back(expression_type_id(parameters[i], *program));

					FunctionId const function = resolve_function_overloading_and_insert_conversions(
						try_get<complete::expression::OverloadSet>(parameters[0])->overload_set,
						{&parameters[1], parameter_types.size()}, parameter_types, *program);

					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found.");
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters = std::move(parameters);
					complete_expression.parameters.erase(complete_expression.parameters.begin());
					return complete_expression;
				}
				else
				{
					mark_as_to_do("Overload of operator function call");
				}
			},
			[&](incomplete::expression::UnaryOperatorCall const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				FunctionId const function = resolve_function_overloading_and_insert_conversions(
					operator_overload_set(incomplete_expression.op, scope_stack), { &operand, 1 }, { &operand_type, 1 }, *program);

				raise_syntax_error_if_not(function != invalid_function_id, "Operator overload not found.");

				complete::expression::FunctionCall complete_expression;
				complete_expression.function_id = function;
				complete_expression.parameters.push_back(std::move(operand));
				return complete_expression;
			},
			[&](incomplete::expression::BinaryOperatorCall const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				Operator const op = incomplete_expression.op;

				complete::Expression operands[2];
				try_call(assign_to(operands[0]), instantiate_expression(*incomplete_expression.left, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(operands[1]), instantiate_expression(*incomplete_expression.right, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const operand_types[] = { expression_type_id(operands[0], *program), expression_type_id(operands[1], *program) };
				FunctionId const function = resolve_function_overloading_and_insert_conversions(operator_overload_set(op, scope_stack), operands, operand_types, *program);

				// Special case for built in assignment.
				if (function == invalid_function_id && op == Operator::assign)
				{
					raise_syntax_error_if_not(operand_types[0].is_reference, "Cannot assign to a temporary.");
					raise_syntax_error_if_not(operand_types[0].is_mutable, "Cannot assign to a constant.");
					complete::expression::Assignment complete_expression;
					complete_expression.destination = allocate(std::move(operands[0]));
					complete_expression.source = allocate(insert_conversion_node(std::move(operands[1]), operand_types[1], decay(operand_types[0]), *program));
					return complete_expression;
				}

				raise_syntax_error_if_not(function != invalid_function_id, "Operator overload not found.");

				if (op == Operator::not_equal || op == Operator::less || op == Operator::less_equal || op == Operator::greater || op == Operator::greater_equal)
				{
					complete::expression::RelationalOperatorCall complete_expression;
					complete_expression.op = op;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return complete_expression;
				}
				else
				{
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return complete_expression;
				}
			},
			[&](incomplete::expression::If const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::Expression condition,
					instantiate_expression(*incomplete_expression.condition, template_parameters, scope_stack, program, current_scope_return_type));
				condition = insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program);

				try_call_decl(complete::Expression then_case, instantiate_expression(*incomplete_expression.then_case, template_parameters, scope_stack, program, current_scope_return_type));
				try_call_decl(complete::Expression else_case, instantiate_expression(*incomplete_expression.else_case, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const then_type = expression_type_id(then_case, *program);
				complete::TypeId const else_type = expression_type_id(else_case, *program);
				complete::TypeId const return_type = common_type(then_type, else_type, *program);

				then_case = insert_conversion_node(std::move(then_case), then_type, return_type, *program);
				else_case = insert_conversion_node(std::move(else_case), else_type, return_type, *program);

				complete::expression::If complete_expression;
				complete_expression.condition = allocate(std::move(condition));
				complete_expression.then_case = allocate(std::move(then_case));
				complete_expression.else_case = allocate(std::move(else_case));
				return complete_expression;
			},
			[&](incomplete::expression::StatementBlock const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::expression::StatementBlock complete_expression;
				complete_expression.return_type = complete::TypeId::deduce;
				auto const guard = push_block_scope(scope_stack, complete_expression.scope);
				complete_expression.statements.reserve(incomplete_expression.statements.size());
				for (incomplete::Statement const & incomplete_substatement : incomplete_expression.statements)
				{
					try_call_decl(auto complete_substatement, instantiate_statement(incomplete_substatement, template_parameters, scope_stack, program, out(complete_expression.return_type)));
					if (complete_substatement.has_value())
						complete_expression.statements.push_back(std::move(*complete_substatement));
				}
			
				return complete_expression;
			},
			[&](incomplete::expression::Constructor const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::TypeId const constructed_type_id, resolve_dependent_type(incomplete_expression.constructed_type, template_parameters, scope_stack, program));

				if (incomplete_expression.parameters.size() == 0) // Default constructor
				{
					raise_syntax_error_if_not(is_default_constructible(constructed_type_id, *program), "Incorrect number of arguments for struct constructor.");
					return synthesize_default_constructor(constructed_type_id, *program);
				}

				complete::expression::Constructor complete_expression;
				complete_expression.constructed_type = constructed_type_id;

				complete::Type const & constructed_type = type_with_id(*program, constructed_type_id);
				if (is_struct(constructed_type))
				{
					complete::Struct const & constructed_struct = *struct_for_type(*program, constructed_type);

					raise_syntax_error_if_not(constructed_struct.member_variables.size() == incomplete_expression.parameters.size(), "Incorrect number of arguments for struct constructor.");

					size_t const n = incomplete_expression.parameters.size();
					complete_expression.parameters.reserve(n);
					for (size_t i = 0; i < n; ++i)
					{
						try_call_decl(complete::Expression complete_param,
							instantiate_expression(incomplete_expression.parameters[i], template_parameters, scope_stack, program, current_scope_return_type));

						complete_expression.parameters.push_back(insert_conversion_node(std::move(complete_param), constructed_struct.member_variables[i].type, *program));
					}
				}
				else if (is_array(constructed_type))
				{
					complete::Type::Array const & constructed_array = std::get<complete::Type::Array>(constructed_type.extra_data);
					complete::TypeId const array_type = constructed_array.value_type;
					int const array_size = constructed_array.size;
					int const param_count = static_cast<int>(incomplete_expression.parameters.size());

					raise_syntax_error_if_not(
						incomplete_expression.parameters.size() == 1 || param_count == array_size,
						"Incorrect number of arguments for array constructor.");

					complete_expression.parameters.reserve(param_count);
					for (int i = 0; i < param_count; ++i)
					{
						try_call_decl(complete::Expression complete_param,
							instantiate_expression(incomplete_expression.parameters[i], template_parameters, scope_stack, program, current_scope_return_type));

						complete_expression.parameters.push_back(insert_conversion_node(std::move(complete_param), array_type, *program));
					}
				}
				else
				{
					declare_unreachable();
				}

				return complete_expression;
			},
			[&](incomplete::expression::DesignatedInitializerConstructor const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				try_call_decl(complete::TypeId const constructed_type_id, resolve_dependent_type(incomplete_expression.constructed_type, template_parameters, scope_stack, program));
				complete::Type const & constructed_type = type_with_id(*program, constructed_type_id);

				raise_syntax_error_if_not(is_struct(constructed_type), "Designated initializers may only be used on structs.");
				raise_syntax_error_if_not(!constructed_type_id.is_reference, "Designated initializers cannot be used to initialize a reference.");

				complete::Struct const & constructed_struct = *struct_for_type(*program, constructed_type);

				size_t const member_count = constructed_struct.member_variables.size();
				auto complete_parameters = std::vector<complete::Expression>(member_count);
				auto expression_initialized = std::vector<bool>(member_count, false);

				for (incomplete::DesignatedInitializer const & initializer : incomplete_expression.parameters)
				{
					int const member_variable_index = find_member_variable(constructed_struct, initializer.member_name);
					raise_syntax_error_if_not(member_variable_index != -1, "Expected member name after '.' in designated initializer.");
					raise_syntax_error_if_not(!expression_initialized[member_variable_index], "Same member initialized twice in designated initializer.");

					try_call(assign_to(complete_parameters[member_variable_index]),
						instantiate_expression(initializer.assigned_expression, template_parameters, scope_stack, program, current_scope_return_type));

					expression_initialized[member_variable_index] = true;
				}

				// For any value that is not given an initializer, use the default if available or fail to parse.
				for (size_t i = 0; i < member_count; ++i)
				{
					if (!expression_initialized[i])
					{
						complete::MemberVariable const & variable = constructed_struct.member_variables[i];
						raise_syntax_error_if_not(variable.initializer_expression.has_value(), "Uninitialized member is not default constructible in designated initializer.");
						complete_parameters[i] = *variable.initializer_expression;
					}
				}

				complete::expression::Constructor complete_expression;
				complete_expression.constructed_type = constructed_type_id;
				complete_expression.parameters = std::move(complete_parameters);
				return complete_expression;
			},
			[&](incomplete::expression::DataCall const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::expression::ReinterpretCast complete_expression;
				try_call(assign_to(complete_expression.operand), instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(*complete_expression.operand, *program);
				complete::Type const & operand_type = type_with_id(*program, operand_type_id);
				raise_syntax_error_if_not(is_array(operand_type), "Operand of data must be of array type.");
				raise_syntax_error_if_not(operand_type_id.is_reference, "Operand of data must be of reference to array type.");
				complete::TypeId value_type = array_value_type(operand_type);
				value_type.is_mutable = operand_type_id.is_mutable;
				complete_expression.return_type = array_pointer_type_for(value_type, *program);
				return complete_expression;
			},
			[&](incomplete::expression::SizeCall const & incomplete_expression) -> expected<complete::Expression, SyntaxError>
			{
				complete::expression::Literal<int> complete_expression;
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(operand, *program);
				complete::Type const & operand_type = type_with_id(*program, operand_type_id);
				raise_syntax_error_if_not(is_array(operand_type), "Operand of data must be of array type.");
				complete_expression.value = array_size(operand_type);
				return complete_expression;
			}
		);

		return std::visit(visitor, incomplete_expression_.as_variant());
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, SyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::statement::VariableDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				// Case in which nothing is assigned to the declaration. Must be a variable of a default constructible type.
				if (!incomplete_statement.assigned_expression.has_value())
				{
					try_call_decl(complete::TypeId const var_type, resolve_dependent_type(incomplete_statement.type, template_parameters, scope_stack, program));
					raise_syntax_error_if_not(is_default_constructible(var_type, *program), "A function must be declared with a let statement.");

					raise_syntax_error_if_not(!does_name_collide(scope_stack, incomplete_statement.variable_name), "Variable name collides with another name.");
					int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

					complete::statement::VariableDeclaration complete_statement;
					complete_statement.variable_offset = var_offset;
					complete_statement.assigned_expression = synthesize_default_constructor(var_type, *program);
					return complete_statement;
				}

				// Hack for recursive functions
				if (has_type<incomplete::expression::Function>(*incomplete_statement.assigned_expression) && incomplete_statement.variable_name != "main")
				{
					incomplete::Function const & incomplete_function = try_get<incomplete::expression::Function>(*incomplete_statement.assigned_expression)->function;

					complete::Function function;
					instantiate_function_prototype(incomplete_function, template_parameters, scope_stack, program, out(function));

					raise_syntax_error_if_not(!does_function_name_collide(scope_stack, incomplete_statement.variable_name), "Function name collides with another name.");

					FunctionId const function_id = add_function(*program, function);
					top(scope_stack).functions.push_back({incomplete_statement.variable_name, function_id});

					instantiate_function_body(incomplete_function, template_parameters, scope_stack, program, out(function));
					program->functions[function_id.index] = std::move(function);

					return std::nullopt;
				}

				try_call_decl(complete::Expression expression,
					instantiate_expression(*incomplete_statement.assigned_expression, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				try_call_decl(complete::TypeId var_type, resolve_dependent_type(incomplete_statement.type, template_parameters, scope_stack, program));
				if (decay(var_type) == complete::TypeId::deduce)
					assign_without_qualifiers(var_type, assigned_expression_type);

				// Main function, which is somewhat special.
				if (incomplete_statement.variable_name == "main"sv)
				{
					auto const * const overload_set = try_get<complete::expression::OverloadSet>(expression);
					raise_syntax_error_if_not(overload_set, "Attempted to use name \"main\" to name something other than a function.");
					raise_syntax_error_if_not(overload_set->overload_set.function_template_ids.size() == 0, "main cannot be a function template.");
					raise_syntax_error_if_not(overload_set->overload_set.function_ids.size() == 1, "main function cannot be overloaded.");
					FunctionId const main_function_id = overload_set->overload_set.function_ids[0];

					// Main must not take parameters and return int.
					complete::Function const & main_function = program->functions[main_function_id.index];
					raise_syntax_error_if_not(main_function.return_type == complete::TypeId::int_, "Main must return int.");
					raise_syntax_error_if_not(main_function.parameter_count == 0, "Main cannot take parameters.");

					// There can only be one main function.
					raise_syntax_error_if_not(program->main_function == invalid_function_id, "Redefinition of main function. There can only be one main function.");

					// Bind the function as the program's main function.
					program->main_function = main_function_id;

					return std::nullopt;
				}
				else if (auto const * const overload_set = try_get<complete::expression::OverloadSet>(expression))
				{
					raise_syntax_error_if_not(var_type == complete::TypeId::function, "A function must be declared with a let statement.");

					raise_syntax_error_if_not(!does_function_name_collide(scope_stack, incomplete_statement.variable_name), "Function name collides with another name.");

					for (FunctionId const function_id : overload_set->overload_set.function_ids)
						top(scope_stack).functions.push_back({incomplete_statement.variable_name, function_id});

					for (FunctionTemplateId const template_id : overload_set->overload_set.function_template_ids)
						top(scope_stack).function_templates.push_back({incomplete_statement.variable_name, template_id});

					return std::nullopt;
				}
				else if (!var_type.is_mutable && is_constant_expression(expression, *program, next_block_scope_offset(scope_stack)))
				{
					complete::Constant constant;
					constant.type = var_type;
					constant.value.resize(type_size(*program, var_type));
					constant.name = incomplete_statement.variable_name;

					evaluate_constant_expression(
						insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program),
						*program, next_block_scope_offset(scope_stack), constant.value.data());

					raise_syntax_error_if_not(!does_name_collide(scope_stack, incomplete_statement.variable_name), "Constant name collides with another name.");

					top(scope_stack).constants.push_back(std::move(constant));

					return std::nullopt;
				}
				else
				{
					raise_syntax_error_if_not(is_convertible(assigned_expression_type, var_type, *program), "Cannot convert to variable type in variable declaration.");

					raise_syntax_error_if_not(!does_name_collide(scope_stack, incomplete_statement.variable_name), "Variable name collides with another name.");

					int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

					complete::statement::VariableDeclaration complete_statement;
					complete_statement.variable_offset = var_offset;
					complete_statement.assigned_expression = insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program);
					return complete_statement;
				}
			},
			[&](incomplete::statement::ExpressionStatement const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::statement::ExpressionStatement complete_statement;
				try_call(assign_to(complete_statement.expression), 
					instantiate_expression(incomplete_statement.expression, template_parameters, scope_stack, program, current_scope_return_type));
				return complete_statement;
			},
			[&](incomplete::statement::If const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::statement::If complete_statement;
				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				complete_statement.condition = insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program);

				try_call_decl(auto then_case, instantiate_statement(*incomplete_statement.then_case, template_parameters, scope_stack, program, current_scope_return_type));
				if (!then_case.has_value()) return make_syntax_error("Noop statement not allowed as then case of if statement.");
				complete_statement.then_case = allocate(std::move(*then_case));
				if (incomplete_statement.else_case != nullptr)
				{
					try_call_decl(auto else_case, instantiate_statement(*incomplete_statement.else_case, template_parameters, scope_stack, program, current_scope_return_type));
					if (!else_case.has_value()) return make_syntax_error("Noop statement not allowed as else case of if statement.");
					complete_statement.else_case = allocate(std::move(*else_case));
				}

				return complete_statement;
			},
			[&](incomplete::statement::StatementBlock const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::statement::StatementBlock complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);
				complete_statement.statements.reserve(incomplete_statement.statements.size());

				for (incomplete::Statement const & incomplete_substatement : incomplete_statement.statements)
				{
					try_call_decl(auto complete_substatement, instantiate_statement(incomplete_substatement, template_parameters, scope_stack, program, current_scope_return_type));
					if (complete_substatement.has_value())
						complete_statement.statements.push_back(std::move(*complete_substatement));
				}

				return complete_statement;
			},
			[&](incomplete::statement::While const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::statement::While complete_statement;

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				complete_statement.condition = insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program);

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program, current_scope_return_type));
				if (!body_statement.has_value()) return make_syntax_error("Noop statement not allowed as body of while loop.");
				complete_statement.body = allocate(std::move(*body_statement));

				return complete_statement;
			},
			[&](incomplete::statement::For const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::statement::For complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);

				try_call_decl(auto init_statement, instantiate_statement(*incomplete_statement.init_statement, template_parameters, scope_stack, program, current_scope_return_type));
				if (!init_statement.has_value()) return make_syntax_error("Noop statement not allowed as init statement of for loop.");
				complete_statement.init_statement = allocate(*init_statement);

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				complete_statement.condition = insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program);

				try_call(assign_to(complete_statement.end_expression), 
					instantiate_expression(incomplete_statement.end_expression, template_parameters, scope_stack, program, current_scope_return_type));

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program, current_scope_return_type));
				if (!body_statement.has_value()) return make_syntax_error("Noop statement not allowed as body of for loop.");
				complete_statement.body = allocate(std::move(*body_statement));

				return complete_statement;
			},
			[&](incomplete::statement::Return const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				raise_syntax_error_if_not(scope_stack.back().type != ScopeType::global, "A return statement cannot appear at the global scope.");

				complete::statement::Return complete_statement;
				try_call(assign_to(complete_statement.returned_expression),
					instantiate_expression(incomplete_statement.returned_expression, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const returned_expression_type = expression_type_id(complete_statement.returned_expression, *program);

				complete::TypeId const return_type = (*current_scope_return_type == complete::TypeId::deduce)
					? decay(returned_expression_type)
					: *current_scope_return_type;

				complete_statement.returned_expression = insert_conversion_node(std::move(complete_statement.returned_expression), returned_expression_type, return_type, *program);

				if (*current_scope_return_type == complete::TypeId::deduce)
					*current_scope_return_type = return_type;

				return complete_statement;
			},
			[](incomplete::statement::Break const &) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				return complete::statement::Break();
			},
			[](incomplete::statement::Continue const &) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				return complete::statement::Continue();
			},
			[&](incomplete::statement::StructDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				complete::Type new_type;
				new_type.size = 0;
				new_type.alignment = 1;

				complete::Struct new_struct;
				new_struct.member_variables.reserve(incomplete_statement.declared_struct.member_variables.size());

				for (incomplete::MemberVariable const & member_variable : incomplete_statement.declared_struct.member_variables)
				{
					try_call_decl(complete::TypeId const member_type, resolve_dependent_type(member_variable.type, template_parameters, scope_stack, program));
					raise_syntax_error_if_not(is_data_type(member_type), "Member variable cannot be void.");
					raise_syntax_error_if_not(!member_type.is_reference, "Member variable cannot be reference.");
					raise_syntax_error_if_not(!member_type.is_mutable, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

					add_variable_to_scope(new_struct.member_variables, new_type.size, new_type.alignment, member_variable.name, member_type, 0, *program);

					complete::MemberVariable & new_variable = new_struct.member_variables.back();
					if (member_variable.initializer_expression.has_value())
					{
						try_call_decl(complete::Expression initializer,
							instantiate_expression(*member_variable.initializer_expression, template_parameters, scope_stack, program, current_scope_return_type));

						new_variable.initializer_expression = insert_conversion_node(std::move(initializer), member_type, *program);
					}
					else if (is_default_constructible(member_type, *program))
					{
						new_variable.initializer_expression = synthesize_default_constructor(member_type, *program);
					}
				}

				raise_syntax_error_if_not(!does_name_collide(scope_stack, incomplete_statement.declared_struct.name), "Struct name collides with another name.");

				new_type.extra_data = complete::Type::Struct{ static_cast<int>(program->structs.size()) };
				complete::TypeId const new_type_id = add_type(*program, std::move(new_type));
				program->structs.push_back(std::move(new_struct));
				top(scope_stack).types.push_back({incomplete_statement.declared_struct.name, new_type_id});

				return std::nullopt;
			},
			[&](incomplete::statement::StructTemplateDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				raise_syntax_error_if_not(!does_name_collide(scope_stack, incomplete_statement.declared_struct_template.name), "Struct template name collides with another name.");

				complete::StructTemplate new_template;
				new_template.incomplete_struct = incomplete_statement.declared_struct_template;
				new_template.scope_template_parameters = template_parameters;
				auto const id = add_struct_template(*program, std::move(new_template));
				top(scope_stack).struct_templates.push_back({incomplete_statement.declared_struct_template.name, id});

				return std::nullopt;
			},
			[&](incomplete::statement::ImportBlock const & incomplete_statement) -> expected<std::optional<complete::Statement>, SyntaxError>
			{
				for (incomplete::ExternFunction const & incomplete_extern_function : incomplete_statement.imported_functions)
				{
					complete::ExternFunction extern_function;
					extern_function.function_pointer = incomplete_extern_function.function_pointer;

					complete::Function function;
					instantiate_function_prototype(incomplete_extern_function.prototype, template_parameters, scope_stack, program, out(function));
					extern_function.parameter_size = function.stack_frame_size;
					extern_function.parameter_alignment = function.stack_frame_alignment;
					extern_function.return_type = function.return_type;

					extern_function.parameter_types.reserve(function.parameter_count);
					for (int i = 0; i < function.parameter_count; ++i)
						extern_function.parameter_types.push_back(function.variables[i].type);

					if (function.parameter_count > 4)
						mark_as_to_do("Extern functions with more than 4 parameters");

					callc::TypeDescriptor parameter_type_descriptors[4];
					for (int i = 0; i < function.parameter_count; ++i)
						parameter_type_descriptors[i] = type_descriptor_for(extern_function.parameter_types[i], *program);

					callc::TypeDescriptor const return_type_descriptor = type_descriptor_for(extern_function.return_type, *program);

					extern_function.caller = callc::c_function_caller({parameter_type_descriptors, extern_function.parameter_types.size()}, return_type_descriptor);
					extern_function.is_callable_at_compile_time = false;

					FunctionId function_id;
					function_id.is_extern = true;
					function_id.index = static_cast<int>(program->extern_functions.size());
					program->extern_functions.push_back(std::move(extern_function));

					raise_syntax_error_if_not(!does_function_name_collide(scope_stack, incomplete_extern_function.name), "Extern function name collides with another name.");

					top(scope_stack).functions.push_back({incomplete_extern_function.name, function_id});
				}

				return std::nullopt;
			}
		);

		return std::visit(visitor, incomplete_statement_.as_variant());
	}

	auto instantiate_templates(span<incomplete::Statement const> incomplete_program, out<complete::Program> complete_program) noexcept -> expected<void, SyntaxError>
	{
		std::vector<complete::ResolvedTemplateParameter> template_parameters;
		ScopeStack scope_stack;
		scope_stack.push_back({&complete_program->global_scope, ScopeType::global, 0});

		for (incomplete::Statement const & incomplete_statement : incomplete_program)
		{
			try_call_decl(auto complete_statement, instantiate_statement(incomplete_statement, template_parameters, scope_stack, out(complete_program), nullptr));
			if (complete_statement)
			{
				if (!has_type<complete::statement::VariableDeclaration>(*complete_statement))
					return make_syntax_error("Only variable declarations, function declarations and struct declarations allowed at global scope.");
				
				complete_program->global_initialization_statements.push_back(std::move(*complete_statement));
			}
		}

		return success;
	}

} // namespace instantiation
