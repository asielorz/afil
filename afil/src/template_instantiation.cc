#include "template_instantiation.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "incomplete_statement.hh"
#include "incomplete_module.hh"
#include "complete_expression.hh"
#include "interpreter.hh"
#include "constexpr.hh"
#include "utils/algorithm.hh"
#include "utils/load_dll.hh"
#include "utils/out.hh"
#include "utils/overload.hh"
#include "utils/string.hh"
#include "utils/utils.hh"
#include "utils/unreachable.hh"
#include "utils/variant.hh"
#include "utils/warning_macro.hh"
#include <cassert>

using namespace std::literals;

[[nodiscard]] auto evaluate_constant_expression(complete::Expression const & expression, complete::Program const & program, int constant_base_index, void * outValue) noexcept -> expected<void, PartialSyntaxError>
{
	if (!is_constant_expression(expression, program, constant_base_index)) return make_syntax_error("Cannot evaluate expression at compile time.");

	interpreter::ProgramStack stack;
	alloc_stack(stack, 256);

	interpreter::eval_expression(expression, stack, program);

	memcpy(outValue, pointer_at_address(stack, 0), expression_type_size(expression, program));

	return success;
}

template <typename T>
auto evaluate_constant_expression_as(complete::Expression const & expression, complete::Program const & program, int constant_base_index) noexcept -> expected<T, PartialSyntaxError>
{
	T result;
	try_call_void(evaluate_constant_expression(expression, program, constant_base_index, &result));
	return result;
}

auto evaluate_array_size_expression(complete::Expression expression, complete::Program const & program, int constant_base_index) noexcept -> expected<int, PartialSyntaxError>
{
	try_call(assign_to(expression), insert_conversion_node(std::move(expression), complete::TypeId::int_, program));
	return evaluate_constant_expression_as<int>(expression, program, constant_base_index);
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

	auto bind_function_name(std::string_view name, FunctionId function_id, complete::Program & program, ScopeStack & scope_stack) -> void
	{
		top(scope_stack).functions.push_back({std::string(name), function_id});
		std::string & function_ABI_name = ABI_name(program, function_id);
		if (function_ABI_name.empty())
			function_ABI_name = name;
	}

	auto bind_function_template_name(std::string_view name, FunctionTemplateId function_template_id, complete::Program & program, ScopeStack & scope_stack) -> void
	{
		top(scope_stack).function_templates.push_back({std::string(name), function_template_id});
		std::string & function_ABI_name = ABI_name(program, function_template_id);
		if (function_ABI_name.empty())
			function_ABI_name = name;
	}

	auto bind_type_name(std::string_view name, complete::TypeId type_id, complete::Program & program, ScopeStack & scope_stack)
	{
		top(scope_stack).types.push_back({std::string(name), type_id});
		std::string & type_ABI_name = ABI_name(program, type_id);
		if (type_ABI_name.empty())
			type_ABI_name = name;
	}

	auto bind_struct_template_name(std::string_view name, complete::StructTemplateId template_id, complete::Program & program, ScopeStack & scope_stack)
	{
		top(scope_stack).struct_templates.push_back({std::string(name), template_id});
		std::string & type_ABI_name = ABI_name(program, template_id);
		if (type_ABI_name.empty())
			type_ABI_name = name;
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

	auto load_symbol(std::string_view symbol_name) noexcept -> void const *
	{
		TODO("DLLs as parameters.");
		constexpr std::string_view const loaded_dlls[] = {
			"ucrtbase"
		};
		
		for (std::string_view const lib : loaded_dlls)
		{
			DLL dll = load_library(lib);
			auto const symbol = find_symbol(dll, symbol_name);
			if (symbol)
				return symbol;
		}

		return nullptr;
	}

	auto resolve_dependent_type(
		incomplete::TypeId const & dependent_type, 
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program) 
		-> expected<complete::TypeId, PartialSyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::TypeId, PartialSyntaxError>
			{
				complete::TypeId const type = type_with_name(base_case.name, scope_stack, template_parameters);
				if (type == complete::TypeId::none) 
					return make_syntax_error(join("Type not found: ", base_case.name));
				return type;
			},
			[&](incomplete::TypeId::Pointer const & pointer) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*pointer.pointee, template_parameters, scope_stack, program));
				return pointer_type_for(pointee, *program);
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const value_type, resolve_dependent_type(*array.value_type, template_parameters, scope_stack, program));

				try_call_decl(complete::Expression size_expr, instantiate_expression(*array.size, template_parameters, scope_stack, program, nullptr));
				try_call_decl(int const size, evaluate_array_size_expression(std::move(size_expr), *program, next_block_scope_offset(scope_stack)));
				return array_type_for(value_type, size, *program);
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*array_pointer.pointee, template_parameters, scope_stack, program));
				return array_pointer_type_for(pointee, *program);
			},
			[&](incomplete::TypeId::TemplateInstantiation const & template_instantiation) -> expected<complete::TypeId, PartialSyntaxError>
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
			[](incomplete::TypeId::Deduce const &) -> expected<complete::TypeId, PartialSyntaxError>
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
		-> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
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
			[&](incomplete::TypeId::Pointer const & pointer) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::FunctionTemplateParameterType::Pointer pointer_type;
				try_call(assign_to(pointer_type.pointee), 
					resolve_function_template_parameter_type(*pointer.pointee, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));
				return complete::FunctionTemplateParameterType{std::move(pointer_type), false, false};
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::FunctionTemplateParameterType::Array array_type;
				try_call(assign_to(array_type.value_type), 
					resolve_function_template_parameter_type(*array.value_type, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));

				try_call_decl(complete::Expression size_expr, instantiate_expression(*array.size, resolved_template_parameters, scope_stack, program, nullptr));
				array_type.size = evaluate_array_size_expression(std::move(size_expr), *program, next_block_scope_offset(scope_stack));
				return complete::FunctionTemplateParameterType{std::move(array_type), false, false};
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::FunctionTemplateParameterType::ArrayPointer array_pointer_type;
				try_call(assign_to(array_pointer_type.pointee),
					resolve_function_template_parameter_type(*array_pointer.pointee, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));
				return complete::FunctionTemplateParameterType{std::move(array_pointer_type), false, false};
			},
			[&](incomplete::TypeId::TemplateInstantiation const & template_instantiation) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				auto const template_id = struct_template_with_name(template_instantiation.template_name, scope_stack);
				if (!template_id.has_value())
					return make_syntax_error("Struct template not found");

				complete::FunctionTemplateParameterType::TemplateInstantiation template_instantiation_type;
				template_instantiation_type.template_id = *template_id;
				template_instantiation_type.parameters.reserve(template_instantiation.parameters.size());
				for (incomplete::TypeId const & param : template_instantiation.parameters)
					try_call(template_instantiation_type.parameters.push_back, 
						resolve_function_template_parameter_type(param, resolved_template_parameters, unresolved_template_parameters, scope_stack, program));

				return complete::FunctionTemplateParameterType{std::move(template_instantiation_type), false, false};
			},
			[](incomplete::TypeId::Deduce const &) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				declare_unreachable();
			}
		);

		try_call_decl(complete::FunctionTemplateParameterType type, std::visit(visitor, dependent_type.value));
		type.is_reference = dependent_type.is_reference;
		type.is_mutable = dependent_type.is_mutable;
		return std::move(type);
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

	auto struct_template_with_name(std::string_view name, ScopeStackView scope_stack) noexcept -> std::optional<complete::StructTemplateId>
	{
		auto const visitor = overload(
			[](lookup_result::StructTemplate const & type) -> std::optional<complete::StructTemplateId>
			{
				return type.template_id;
			},
			[](auto const &) -> std::optional<complete::StructTemplateId>
			{
				return std::nullopt;
			}
		);

		auto lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
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

	auto test_if_expression_compiles(
		incomplete::ExpressionToTest const & expression_to_test,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> bool
	{
		auto expr = instantiate_expression(expression_to_test.expression, template_parameters, scope_stack, program, current_scope_return_type);
		if (!expr)
			return false;

		if (!expression_to_test.expected_type)
			return true;

		auto expected_return_type = resolve_dependent_type(*expression_to_test.expected_type, template_parameters, scope_stack, program);
		if (!expected_return_type)
			return false;

		return is_convertible(expression_type_id(*expr, *program), *expected_return_type, *program);
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, PartialSyntaxError>;

	[[nodiscard]] auto instantiate_function_prototype(
		incomplete::FunctionPrototype const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		out<complete::Function> function
	) -> expected<void, PartialSyntaxError>
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

	[[nodiscard]] auto instantiate_function_body(
		incomplete::Function const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		out<complete::Function> function
	) -> expected<void, PartialSyntaxError>
	{
		scope_stack.push_back({ &*function, ScopeType::function, 0 });

		function->preconditions.reserve(incomplete_function.preconditions.size());
		for (incomplete::Expression const & precondition : incomplete_function.preconditions)
		{
			try_call_decl(auto complete_precondition, instantiate_expression(precondition, template_parameters, scope_stack, program, out(function->return_type)));
			try_call(function->preconditions.push_back, insert_conversion_node(std::move(complete_precondition), complete::TypeId::bool_, *program));
		}

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
	) -> expected<complete::Function, PartialSyntaxError>
	{
		complete::Function function;

		try_call_void(instantiate_function_prototype(incomplete_function, template_parameters, scope_stack, program, out(function)));
		try_call_void(instantiate_function_body(incomplete_function, template_parameters, scope_stack, program, out(function)));

		return std::move(function);
	}

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<complete::Expression, PartialSyntaxError>
	{
		auto const visitor = overload(
			[](incomplete::expression::Literal<int> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<int>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<float> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<float>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<bool> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<bool>{incomplete_expression.value};
			},
			[&](incomplete::expression::Literal<std::string> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::StringLiteral complete_expression;
				complete_expression.value = incomplete_expression.value;
				complete_expression.type = array_type_for(complete::TypeId::char_, static_cast<int>(complete_expression.value.size()), *program);
				return complete_expression;
			},
			[](incomplete::expression::Literal<uninit_t> const &) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<uninit_t>();
			},
			[&](incomplete::expression::Identifier const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				auto const lookup = lookup_name(scope_stack, incomplete_expression.name);
				auto const lookup_visitor = overload(
					[](lookup_result::Variable const & var) -> expected<complete::Expression, PartialSyntaxError>
					{
						complete::expression::LocalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[](lookup_result::Constant const & var) -> expected<complete::Expression, PartialSyntaxError>
					{
						complete::expression::Constant complete_expression;
						complete_expression.type = var.constant->type;
						complete_expression.value = var.constant->value;
						return complete_expression;
					},
					[](lookup_result::GlobalVariable const & var) -> expected<complete::Expression, PartialSyntaxError>
					{
						complete::expression::GlobalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[&](lookup_result::OverloadSet const & var) -> expected<complete::Expression, PartialSyntaxError>
					{
						complete::expression::Constant complete_expression;
						complete_expression.type = type_for_overload_set(*program, var);
						return complete_expression;
					},
					[&](auto const &) -> expected<complete::Expression, PartialSyntaxError>
					{
						return make_syntax_error(incomplete_expression.name, join("Undeclared identifier: ", incomplete_expression.name));
					}
				);
				return std::visit(lookup_visitor, lookup);
			},
			[&](incomplete::expression::MemberVariable const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::MemberVariable complete_expression;
				try_call(assign_to(complete_expression.owner), instantiate_expression(*incomplete_expression.owner, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const owner_type_id = decay(expression_type_id(*complete_expression.owner, *program));
				complete::Type const & owner_type = type_with_id(*program, owner_type_id);
				if (!is_struct(owner_type)) 
					return make_syntax_error(incomplete_expression.owner->source, "Cannot access member of non struct type.");
				complete::Struct const & owner_struct = *struct_for_type(*program, owner_type);
				int const member_index = find_member_variable(owner_struct, incomplete_expression.name);
				if (member_index == -1) 
					return make_syntax_error(incomplete_expression.name, "Member not found.");

				complete_expression.variable_offset = owner_struct.member_variables[member_index].offset;
				complete_expression.variable_type = owner_struct.member_variables[member_index].type;
				complete_expression.variable_type.is_reference = owner_type_id.is_reference;
				complete_expression.variable_type.is_mutable = owner_type_id.is_mutable;

				return std::move(complete_expression);
			},
			[&](incomplete::expression::Addressof const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				if (!operand_type.is_reference)  
					return make_syntax_error(incomplete_expression_.source, "Attempted to take address of temporary.");
				complete::TypeId const pointer_type = pointer_type_for(remove_reference(operand_type), *program);

				complete::expression::ReinterpretCast complete_expression;
				complete_expression.operand = allocate(std::move(operand));
				complete_expression.return_type = pointer_type;
				return std::move(complete_expression);
			},
			[&](incomplete::expression::Dereference const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
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
					try_call(assign_to(complete_expression.operand), insert_conversion_node(std::move(operand), operand_type_id, decay(operand_type_id), *program));
					return std::move(complete_expression);
				}
				else
				{
					try_call_decl(FunctionId const function, resolve_function_overloading_and_insert_conversions(
						operator_overload_set(Operator::dereference, scope_stack), { &operand, 1 }, { &operand_type_id, 1 }, *program));

					if (function == invalid_function_id) 
						return make_syntax_error(incomplete_expression_.source, "Overload not found for dereference operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.push_back(std::move(operand));
					return std::move(complete_expression);
				}
			},
			[&](incomplete::expression::Subscript const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
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
					try_call(assign_to(complete_expression.index), insert_conversion_node(std::move(index), complete::TypeId::int_, *program));
					return std::move(complete_expression);
				}
				else if (is_array_pointer(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::ArrayPointer>(array_type.extra_data)->value_type;
					value_type.is_reference = true;

					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program, current_scope_return_type));

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					try_call(assign_to(complete_expression.array), insert_conversion_node(std::move(array), decay(array_type_id), *program));
					try_call(assign_to(complete_expression.index), insert_conversion_node(std::move(index), complete::TypeId::int_, *program));
					return std::move(complete_expression);
				}
				else
				{
					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program, current_scope_return_type));
					complete::TypeId const index_type_id = expression_type_id(index, *program);

					complete::TypeId const param_types[] = { array_type_id, index_type_id };
					complete::Expression params[] = { std::move(array), std::move(index) };

					try_call_decl(FunctionId const function, resolve_function_overloading_and_insert_conversions(named_overload_set("[]"sv, scope_stack), params, param_types, *program));

					if (function == invalid_function_id) 
						return make_syntax_error(incomplete_expression_.source, "Overload not found for subscript operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(params[0]));
					complete_expression.parameters.push_back(std::move(params[1]));
					return std::move(complete_expression);
				}
			},
			[&](incomplete::expression::Function const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Function complete_function, instantiate_function_template(incomplete_expression.function, template_parameters, scope_stack, program));
				FunctionId const function_id = add_function(*program, std::move(complete_function));

				complete::OverloadSet overload_set;
				overload_set.function_ids.push_back(function_id);

				complete::expression::Constant complete_expression;
				complete_expression.type = type_for_overload_set(*program, std::move(overload_set));
				return std::move(complete_expression);
			},
			[&](incomplete::expression::FunctionTemplate const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::FunctionTemplate new_function_template;
				new_function_template.scope_template_parameters = template_parameters;
				new_function_template.incomplete_function = incomplete_expression.function_template;
				new_function_template.scope_stack = scope_stack;
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

				FunctionTemplateId const template_id = add_function_template(*program, std::move(new_function_template));

				complete::OverloadSet overload_set;
				overload_set.function_template_ids.push_back(template_id);

				complete::expression::Constant complete_expression;
				complete_expression.type = type_for_overload_set(*program, std::move(overload_set));
				return std::move(complete_expression);
			},
			[&](incomplete::expression::ExternFunction const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				incomplete::ExternFunction const & incomplete_extern_function = incomplete_expression.function;

				complete::ExternFunction extern_function;
				extern_function.function_pointer = load_symbol(incomplete_extern_function.ABI_name);
				if (!extern_function.function_pointer)
					return make_syntax_error(incomplete_extern_function.ABI_name_source, join("Cannot find extern symbol \"", incomplete_extern_function.ABI_name, "\"."));
				extern_function.ABI_name = incomplete_extern_function.ABI_name;

				complete::Function function;
				try_call_void(instantiate_function_prototype(incomplete_extern_function.prototype, template_parameters, scope_stack, program, out(function)));
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

				extern_function.caller = callc::c_function_caller({ parameter_type_descriptors, extern_function.parameter_types.size() }, return_type_descriptor);
				extern_function.is_callable_at_compile_time = false;

				FunctionId function_id;
				function_id.is_extern = true;
				function_id.index = static_cast<int>(program->extern_functions.size());
				program->extern_functions.push_back(std::move(extern_function));

				complete::OverloadSet overload_set;
				overload_set.function_ids.push_back(function_id);

				complete::expression::Constant complete_expression;
				complete_expression.type = type_for_overload_set(*program, std::move(overload_set));
				return std::move(complete_expression);
			},
			[&](incomplete::expression::FunctionCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				std::vector<complete::Expression> parameters;
				parameters.reserve(incomplete_expression.parameters.size());
				for (incomplete::Expression const & incomplete_param : incomplete_expression.parameters)
					try_call(parameters.push_back, instantiate_expression(incomplete_param, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const first_param_type = expression_type_id(parameters[0], *program);
				if (first_param_type.is_function)
				{
					std::vector<complete::TypeId> parameter_types;
					parameter_types.reserve(parameters.size() - 1);
					for (size_t i = 1; i < parameters.size(); ++i)
						parameter_types.push_back(expression_type_id(parameters[i], *program));

					complete::OverloadSetView const overload_set = overload_set_for_type(*program, first_param_type);

					try_call_decl(FunctionId const function, 
						resolve_function_overloading_and_insert_conversions(overload_set, {parameters.data() + 1, parameter_types.size()}, parameter_types, *program));

					if (function == invalid_function_id)
						return make_syntax_error(incomplete_expression.parameters[0].source, "Overload not found.");
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters = std::move(parameters);
					complete_expression.parameters.erase(complete_expression.parameters.begin());
					return std::move(complete_expression);
				}
				else
				{
					mark_as_to_do("Overload of operator function call");
				}
			},
			[&](incomplete::expression::UnaryOperatorCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				try_call_decl(FunctionId const function, resolve_function_overloading_and_insert_conversions(
					operator_overload_set(incomplete_expression.op, scope_stack), { &operand, 1 }, { &operand_type, 1 }, *program));

				if (function == invalid_function_id) 
					return make_syntax_error(incomplete_expression_.source, "Operator overload not found.");

				complete::expression::FunctionCall complete_expression;
				complete_expression.function_id = function;
				complete_expression.parameters.push_back(std::move(operand));
				return std::move(complete_expression);
			},
			[&](incomplete::expression::BinaryOperatorCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				Operator const op = incomplete_expression.op;

				complete::Expression operands[2];
				try_call(assign_to(operands[0]), instantiate_expression(*incomplete_expression.left, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(operands[1]), instantiate_expression(*incomplete_expression.right, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const operand_types[] = { expression_type_id(operands[0], *program), expression_type_id(operands[1], *program) };
				try_call_decl(FunctionId const function,
					resolve_function_overloading_and_insert_conversions(operator_overload_set(op, scope_stack), operands, operand_types, *program));

				// Special case for built in assignment.
				if (function == invalid_function_id && op == Operator::assign)
				{
					if (!operand_types[0].is_reference)
						return make_syntax_error(incomplete_expression.left->source, "Cannot assign to a temporary.");
					if (!operand_types[0].is_mutable) 
						return make_syntax_error(incomplete_expression.left->source, "Cannot assign to a constant.");
					complete::expression::Assignment complete_expression;
					complete_expression.destination = allocate(std::move(operands[0]));
					try_call(assign_to(complete_expression.source), insert_conversion_node(std::move(operands[1]), operand_types[1], decay(operand_types[0]), *program));
					return std::move(complete_expression);
				}

				if (function == invalid_function_id) 
					return make_syntax_error(incomplete_expression_.source, "Operator overload not found.");

				if (op == Operator::not_equal || op == Operator::less || op == Operator::less_equal || op == Operator::greater || op == Operator::greater_equal)
				{
					complete::expression::RelationalOperatorCall complete_expression;
					complete_expression.op = op;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return std::move(complete_expression);
				}
				else
				{
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return std::move(complete_expression);
				}
			},
			[&](incomplete::expression::If const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Expression condition, instantiate_expression(*incomplete_expression.condition, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(condition), insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program));

				if (is_constant_expression(condition, *program, next_block_scope_offset(scope_stack)))
				{
					try_call_decl(bool const condition_value, evaluate_constant_expression_as<bool>(condition, *program, next_block_scope_offset(scope_stack)));

					if (condition_value)
						return instantiate_expression(*incomplete_expression.then_case, template_parameters, scope_stack, program, current_scope_return_type);
					else
						return instantiate_expression(*incomplete_expression.else_case, template_parameters, scope_stack, program, current_scope_return_type);
				}
				else
				{
					try_call_decl(complete::Expression then_case, instantiate_expression(*incomplete_expression.then_case, template_parameters, scope_stack, program, current_scope_return_type));
					try_call_decl(complete::Expression else_case, instantiate_expression(*incomplete_expression.else_case, template_parameters, scope_stack, program, current_scope_return_type));

					complete::TypeId const then_type = expression_type_id(then_case, *program);
					complete::TypeId const else_type = expression_type_id(else_case, *program);
					complete::TypeId const return_type = common_type(then_type, else_type, *program);

					try_call(assign_to(then_case), insert_conversion_node(std::move(then_case), then_type, return_type, *program));
					try_call(assign_to(else_case), insert_conversion_node(std::move(else_case), else_type, return_type, *program));

					complete::expression::If complete_expression;
					complete_expression.condition = allocate(std::move(condition));
					complete_expression.then_case = allocate(std::move(then_case));
					complete_expression.else_case = allocate(std::move(else_case));
					return std::move(complete_expression);
				}
			},
			[&](incomplete::expression::StatementBlock const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
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
			
				return std::move(complete_expression);
			},
			[&](incomplete::expression::Constructor const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const constructed_type_id, resolve_dependent_type(incomplete_expression.constructed_type, template_parameters, scope_stack, program));

				if (incomplete_expression.parameters.size() == 0) // Default constructor
				{
					if (!is_default_constructible(constructed_type_id, *program)) 
						return make_syntax_error(incomplete_expression_.source, "Cannot construct with 0 parameters a type that is not default constructible.");
					return synthesize_default_constructor(constructed_type_id, *program);
				}

				complete::expression::Constructor complete_expression;
				complete_expression.constructed_type = constructed_type_id;

				complete::Type const & constructed_type = type_with_id(*program, constructed_type_id);
				if (is_struct(constructed_type))
				{
					complete::Struct const & constructed_struct = *struct_for_type(*program, constructed_type);

					if (constructed_struct.member_variables.size() != incomplete_expression.parameters.size())
						return make_syntax_error(incomplete_expression_.source, "Incorrect number of arguments for struct constructor.");

					size_t const n = incomplete_expression.parameters.size();
					complete_expression.parameters.reserve(n);
					for (size_t i = 0; i < n; ++i)
					{
						try_call_decl(complete::Expression complete_param,
							instantiate_expression(incomplete_expression.parameters[i], template_parameters, scope_stack, program, current_scope_return_type));

						try_call(complete_expression.parameters.push_back, insert_conversion_node(std::move(complete_param), constructed_struct.member_variables[i].type, *program));
					}
				}
				else if (is_array(constructed_type))
				{
					complete::Type::Array const & constructed_array = std::get<complete::Type::Array>(constructed_type.extra_data);
					complete::TypeId const array_type = constructed_array.value_type;
					int const array_size = constructed_array.size;
					int const param_count = static_cast<int>(incomplete_expression.parameters.size());

					if (incomplete_expression.parameters.size() != 1 && param_count != array_size)
						return make_syntax_error(incomplete_expression_.source, "Incorrect number of arguments for array constructor.");

					complete_expression.parameters.reserve(param_count);
					for (int i = 0; i < param_count; ++i)
					{
						try_call_decl(complete::Expression complete_param,
							instantiate_expression(incomplete_expression.parameters[i], template_parameters, scope_stack, program, current_scope_return_type));

						try_call(complete_expression.parameters.push_back, insert_conversion_node(std::move(complete_param), array_type, *program));
					}
				}
				// Not an actual array pointer but array type with deduced size.
				else if (is_array_pointer(constructed_type))
				{
					int const param_count = static_cast<int>(incomplete_expression.parameters.size());

					complete::TypeId const value_type = pointee_type(constructed_type);
					complete::TypeId const constructed_array_type = array_type_for(value_type, param_count, *program);
					complete_expression.constructed_type = constructed_array_type;

					complete_expression.parameters.reserve(param_count);
					for (int i = 0; i < param_count; ++i)
					{
						try_call_decl(complete::Expression complete_param,
							instantiate_expression(incomplete_expression.parameters[i], template_parameters, scope_stack, program, current_scope_return_type));

						try_call(complete_expression.parameters.push_back, insert_conversion_node(std::move(complete_param), value_type, *program));
					}
				}
				else
				{
					declare_unreachable();
				}

				return std::move(complete_expression);
			},
			[&](incomplete::expression::DesignatedInitializerConstructor const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const constructed_type_id, resolve_dependent_type(incomplete_expression.constructed_type, template_parameters, scope_stack, program));
				complete::Type const & constructed_type = type_with_id(*program, constructed_type_id);

				if (!is_struct(constructed_type)) 
					return make_syntax_error(incomplete_expression_.source, "Designated initializers may only be used on structs.");
				if (constructed_type_id.is_reference) 
					return make_syntax_error(incomplete_expression_.source, "Designated initializers cannot be used to initialize a reference.");

				complete::Struct const & constructed_struct = *struct_for_type(*program, constructed_type);

				size_t const member_count = constructed_struct.member_variables.size();
				auto complete_parameters = std::vector<complete::Expression>(member_count);
				auto expression_initialized = std::vector<bool>(member_count, false);

				for (incomplete::DesignatedInitializer const & initializer : incomplete_expression.parameters)
				{
					int const member_variable_index = find_member_variable(constructed_struct, initializer.member_name);
					if (member_variable_index == -1)
						return make_syntax_error(initializer.member_name, "Expected member name after '.' in designated initializer.");
					if (expression_initialized[member_variable_index]) 
						return make_syntax_error(initializer.member_name, "Same member initialized twice in designated initializer.");

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
						if (!variable.initializer_expression.has_value()) 
							return make_syntax_error(incomplete_expression_.source, "Uninitialized member is not default constructible in designated initializer.");
						complete_parameters[i] = *variable.initializer_expression;
					}
				}

				complete::expression::Constructor complete_expression;
				complete_expression.constructed_type = constructed_type_id;
				complete_expression.parameters = std::move(complete_parameters);
				return std::move(complete_expression);
			},
			[&](incomplete::expression::DataCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::ReinterpretCast complete_expression;
				try_call(assign_to(complete_expression.operand), instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(*complete_expression.operand, *program);
				complete::Type const & operand_type = type_with_id(*program, operand_type_id);

				if (!is_array(operand_type))
					return make_syntax_error(incomplete_expression.operand->source, "Operand of data must be of array type.");
				if (!operand_type_id.is_reference) 
					return make_syntax_error(incomplete_expression.operand->source, "Operand of data must be of reference to array type.");

				complete::TypeId value_type = array_value_type(operand_type);
				value_type.is_mutable = operand_type_id.is_mutable;
				complete_expression.return_type = array_pointer_type_for(value_type, *program);
				return std::move(complete_expression);
			},
			[&](incomplete::expression::SizeCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::Literal<int> complete_expression;
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(operand, *program);
				complete::Type const & operand_type = type_with_id(*program, operand_type_id);

				if (!is_array(operand_type)) 
					return make_syntax_error(incomplete_expression.operand->source, "Operand of size must be of array type.");

				complete_expression.value = array_size(operand_type);
				return std::move(complete_expression);
			},
			[&](incomplete::expression::Compiles const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::Scope fake_scope;
				for (incomplete::expression::Compiles::FakeVariable const & fake_var : incomplete_expression.variables)
				{
					try_call_decl(complete::TypeId const var_type, resolve_dependent_type(fake_var.type, template_parameters, scope_stack, program));
					add_variable_to_scope(fake_scope, fake_var.name, var_type, 0, *program);
				}
				auto const guard = push_block_scope(scope_stack, fake_scope);

				bool all_body_expressions_compile = true;
				for (incomplete::ExpressionToTest const & expression_to_test : incomplete_expression.body)
				{
					if (!test_if_expression_compiles(expression_to_test, template_parameters, scope_stack, program, current_scope_return_type))
					{
						all_body_expressions_compile = false;
						break;
					}
				}
				return complete::expression::Literal<bool>{all_body_expressions_compile};
			}
		);

		return std::visit(visitor, incomplete_expression_.variant);
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::statement::VariableDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				// Case in which nothing is assigned to the declaration. Must be a variable of a default constructible type.
				if (!incomplete_statement.assigned_expression.has_value())
				{
					try_call_decl(complete::TypeId const var_type, resolve_dependent_type(incomplete_statement.type, template_parameters, scope_stack, program));
					if (!is_default_constructible(var_type, *program)) 
						return make_syntax_error(incomplete_statement.variable_name, "Type is not default constructible.");

					if (does_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Variable name collides with another name.");
					int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

					complete::statement::VariableDeclaration complete_statement;
					complete_statement.variable_offset = var_offset;
					complete_statement.assigned_expression = synthesize_default_constructor(var_type, *program);
					return std::move(complete_statement);
				}

				try_call_decl(complete::Expression expression,
					instantiate_expression(*incomplete_statement.assigned_expression, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				try_call_decl(complete::TypeId var_type, resolve_dependent_type(incomplete_statement.type, template_parameters, scope_stack, program));
				if (decay(var_type) == complete::TypeId::deduce)
					assign_without_qualifiers(var_type, assigned_expression_type);

				if (decay(var_type) == complete::TypeId::uninit_t)
					return make_syntax_error(incomplete_statement.variable_name, "Cannot declare variable of type uninit_t.");

				if (!var_type.is_mutable && is_constant_expression(expression, *program, next_block_scope_offset(scope_stack)))
				{
					complete::Constant constant;
					constant.type = var_type;
					constant.value.resize(type_size(*program, var_type));
					constant.name = incomplete_statement.variable_name;

					try_call(assign_to(expression), insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program));
					try_call_void(evaluate_constant_expression(expression, *program, next_block_scope_offset(scope_stack), constant.value.data()));

					if (does_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Constant name collides with another name.");

					top(scope_stack).constants.push_back(std::move(constant));

					return std::nullopt;
				}
				else
				{
					if (does_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Variable name collides with another name.");

					int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

					complete::statement::VariableDeclaration complete_statement;
					complete_statement.variable_offset = var_offset;

					if (assigned_expression_type == complete::TypeId::uninit_t)
					{
						if (var_type.is_reference) 
							return make_syntax_error(incomplete_statement.assigned_expression->source, "Cannot initialize a reference with uninit.");
						if (!var_type.is_mutable)
							return make_syntax_error(incomplete_statement.assigned_expression->source, "Cannot initialize a constant variable with uninit.");
						complete_statement.assigned_expression = std::move(expression);
					}
					else
					{
						if (!is_convertible(assigned_expression_type, var_type, *program))
							return make_syntax_error(incomplete_statement.assigned_expression->source, "Cannot convert to variable type in variable declaration.");
						try_call(assign_to(complete_statement.assigned_expression), insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program));
					}
					return std::move(complete_statement);
				}
			},
			[&](incomplete::statement::LetDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				// Hack for recursive functions
				if (has_type<incomplete::expression::Function>(incomplete_statement.assigned_expression.variant) && incomplete_statement.variable_name != "main")
				{
					incomplete::Function const & incomplete_function = try_get<incomplete::expression::Function>(incomplete_statement.assigned_expression.variant)->function;

					complete::Function function;
					try_call_void(instantiate_function_prototype(incomplete_function, template_parameters, scope_stack, program, out(function)));

					if (does_function_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Function name collides with another name.");

					FunctionId const function_id = add_function(*program, function);
					bind_function_name(incomplete_statement.variable_name, function_id, *program, scope_stack);
					function.ABI_name = incomplete_statement.variable_name;

					try_call_void(instantiate_function_body(incomplete_function, template_parameters, scope_stack, program, out(function)));
					program->functions[function_id.index] = std::move(function);

					return std::nullopt;
				}

				try_call_decl(complete::Expression expression,
					instantiate_expression(incomplete_statement.assigned_expression, template_parameters, scope_stack, program, current_scope_return_type));
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				complete::TypeId const var_type = make_mutable(make_reference(assigned_expression_type, incomplete_statement.is_reference), incomplete_statement.is_mutable);

				if (decay(var_type) == complete::TypeId::uninit_t)
					return make_syntax_error(incomplete_statement.assigned_expression.source, "Cannot declare variable of type uninit_t.");

				// Main function, which is somewhat special.
				if (incomplete_statement.variable_name == "main"sv)
				{
					if (!var_type.is_function) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Attempted to use name \"main\" to name something other than a function.");
					complete::OverloadSetView const overload_set = overload_set_for_type(*program, var_type);
					if (overload_set.function_template_ids.size() != 0)
						return make_syntax_error(incomplete_statement.assigned_expression.source, "main cannot be a function template.");
					if (overload_set.function_ids.size() != 1)
						return make_syntax_error(incomplete_statement.assigned_expression.source, "main function cannot be overloaded.");
					FunctionId const main_function_id = overload_set.function_ids[0];

					// Main must not take parameters and return int.
					complete::Function const & main_function = program->functions[main_function_id.index];
					if (main_function.return_type != complete::TypeId::int_) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Main must return int.");
					if (main_function.parameter_count != 0) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Main cannot take parameters.");

					// There can only be one main function.
					if (program->main_function != invalid_function_id) 
						return make_syntax_error(incomplete_statement.variable_name, "Redefinition of main function. There can only be one main function.");

					// Bind the function as the program's main function.
					program->main_function = main_function_id;

					return std::nullopt;
				}
				else if (var_type.is_function)
				{
					if (does_function_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Function name collides with another name.");

					complete::OverloadSetView const overload_set = overload_set_for_type(*program, var_type);

					for (FunctionId const function_id : overload_set.function_ids)
						bind_function_name(incomplete_statement.variable_name, function_id, *program, scope_stack);

					for (FunctionTemplateId const template_id : overload_set.function_template_ids)
						bind_function_template_name(incomplete_statement.variable_name, template_id, *program, scope_stack);

					return std::nullopt;
				}
				else if (!var_type.is_mutable && is_constant_expression(expression, *program, next_block_scope_offset(scope_stack)))
				{
					complete::Constant constant;
					constant.type = var_type;
					constant.value.resize(type_size(*program, var_type));
					constant.name = incomplete_statement.variable_name;

					try_call(assign_to(expression), insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program));
					try_call_void(evaluate_constant_expression(expression, *program, next_block_scope_offset(scope_stack), constant.value.data()));

					if (does_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Constant name collides with another name.");

					top(scope_stack).constants.push_back(std::move(constant));

					return std::nullopt;
				}
				else
				{
					if (!is_convertible(assigned_expression_type, var_type, *program)) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Cannot convert to variable type in variable declaration.");

					if (does_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Variable name collides with another name.");

					int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

					complete::statement::VariableDeclaration complete_statement;
					complete_statement.variable_offset = var_offset;
					try_call(assign_to(complete_statement.assigned_expression), insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program));
					return std::move(complete_statement);
				}
			},
			[&](incomplete::statement::ExpressionStatement const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::ExpressionStatement complete_statement;
				try_call(assign_to(complete_statement.expression), 
					instantiate_expression(incomplete_statement.expression, template_parameters, scope_stack, program, current_scope_return_type));
				return std::move(complete_statement);
			},
			[&](incomplete::statement::If const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(condition), insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program));

				if (is_constant_expression(condition, *program, next_block_scope_offset(scope_stack)))
				{
					try_call_decl(bool const condition_value, evaluate_constant_expression_as<bool>(condition, *program, next_block_scope_offset(scope_stack)));

					if (condition_value)
					{
						try_call_decl(auto then_case, instantiate_statement(*incomplete_statement.then_case, template_parameters, scope_stack, program, current_scope_return_type));
						if (!then_case.has_value())
							return make_syntax_error(incomplete_statement.then_case->source, "Noop statement not allowed as then case of if statement.");

						return std::move(then_case);
					}
					else
					{
						if (incomplete_statement.else_case != nullptr)
						{
							try_call_decl(auto else_case, instantiate_statement(*incomplete_statement.else_case, template_parameters, scope_stack, program, current_scope_return_type));
							if (!else_case.has_value())
								return make_syntax_error(incomplete_statement.else_case->source, "Noop statement not allowed as else case of if statement.");

							return std::move(else_case);
						}
						else
						{
							return std::nullopt;
						}
					}
				}
				else
				{
					complete::statement::If complete_statement;
					complete_statement.condition = std::move(condition);

					try_call_decl(auto then_case, instantiate_statement(*incomplete_statement.then_case, template_parameters, scope_stack, program, current_scope_return_type));
					if (!then_case.has_value())
						return make_syntax_error(incomplete_statement.then_case->source, "Noop statement not allowed as then case of if statement.");
					complete_statement.then_case = allocate(std::move(*then_case));
					if (incomplete_statement.else_case != nullptr)
					{
						try_call_decl(auto else_case, instantiate_statement(*incomplete_statement.else_case, template_parameters, scope_stack, program, current_scope_return_type));
						if (!else_case.has_value())
							return make_syntax_error(incomplete_statement.else_case->source, "Noop statement not allowed as else case of if statement.");
						complete_statement.else_case = allocate(std::move(*else_case));
					}

					return std::move(complete_statement);
				}
			},
			[&](incomplete::statement::StatementBlock const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
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

				return std::move(complete_statement);
			},
			[&](incomplete::statement::While const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::While complete_statement;

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(complete_statement.condition), insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program));

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program, current_scope_return_type));
				if (!body_statement.has_value())
					return make_syntax_error(incomplete_statement.body->source, "Noop statement not allowed as body of while loop.");
				complete_statement.body = allocate(std::move(*body_statement));

				return std::move(complete_statement);
			},
			[&](incomplete::statement::For const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::For complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);

				try_call_decl(auto init_statement, instantiate_statement(*incomplete_statement.init_statement, template_parameters, scope_stack, program, current_scope_return_type));
				if (!init_statement.has_value()) 
					return make_syntax_error(incomplete_statement.init_statement->source, "Noop statement not allowed as init statement of for loop.");
				complete_statement.init_statement = allocate(std::move(*init_statement));

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program, current_scope_return_type));
				try_call(assign_to(complete_statement.condition), insert_conversion_node(std::move(condition), complete::TypeId::bool_, *program));

				try_call(assign_to(complete_statement.end_expression), 
					instantiate_expression(incomplete_statement.end_expression, template_parameters, scope_stack, program, current_scope_return_type));

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program, current_scope_return_type));
				if (!body_statement.has_value()) 
					return make_syntax_error(incomplete_statement.body->source, "Noop statement not allowed as body of for loop.");
				complete_statement.body = allocate(std::move(*body_statement));

				return std::move(complete_statement);
			},
			[&](incomplete::statement::Return const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (scope_stack.back().type == ScopeType::global)
					return make_syntax_error(incomplete_statement_.source, "A return statement cannot appear at the global scope.");

				complete::statement::Return complete_statement;
				try_call(assign_to(complete_statement.returned_expression),
					instantiate_expression(incomplete_statement.returned_expression, template_parameters, scope_stack, program, current_scope_return_type));

				complete::TypeId const returned_expression_type = expression_type_id(complete_statement.returned_expression, *program);

				complete::TypeId const return_type = (*current_scope_return_type == complete::TypeId::deduce)
					? decay(returned_expression_type)
					: *current_scope_return_type;

				try_call(assign_to(complete_statement.returned_expression), 
					insert_conversion_node(std::move(complete_statement.returned_expression), returned_expression_type, return_type, *program));

				if (*current_scope_return_type == complete::TypeId::deduce)
					*current_scope_return_type = return_type;

				return std::move(complete_statement);
			},
			[](incomplete::statement::Break const &) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				return complete::statement::Break();
			},
			[](incomplete::statement::Continue const &) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				return complete::statement::Continue();
			},
			[&](incomplete::statement::StructDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::Type new_type;
				new_type.size = 0;
				new_type.alignment = 1;

				complete::Struct new_struct;
				new_struct.member_variables.reserve(incomplete_statement.declared_struct.member_variables.size());

				for (incomplete::MemberVariable const & member_variable : incomplete_statement.declared_struct.member_variables)
				{
					try_call_decl(complete::TypeId const member_type, resolve_dependent_type(member_variable.type, template_parameters, scope_stack, program));
					if (!is_data_type(member_type)) 
						return make_syntax_error(member_variable.name, "Member variable cannot be void.");
					if (member_type.is_reference) 
						return make_syntax_error(member_variable.name, "Member variable cannot be reference.");
					if (member_type.is_mutable) 
						return make_syntax_error(member_variable.name, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

					add_variable_to_scope(new_struct.member_variables, new_type.size, new_type.alignment, member_variable.name, member_type, 0, *program);

					complete::MemberVariable & new_variable = new_struct.member_variables.back();
					if (member_variable.initializer_expression.has_value())
					{
						try_call_decl(complete::Expression initializer,
							instantiate_expression(*member_variable.initializer_expression, template_parameters, scope_stack, program, current_scope_return_type));

						try_call(assign_to(new_variable.initializer_expression), insert_conversion_node(std::move(initializer), member_type, *program));
					}
					else if (is_default_constructible(member_type, *program))
					{
						new_variable.initializer_expression = synthesize_default_constructor(member_type, *program);
					}
				}

				if (does_name_collide(scope_stack, incomplete_statement.declared_struct.name)) 
					return make_syntax_error(incomplete_statement.declared_struct.name, "Struct name collides with another name.");

				new_type.extra_data = complete::Type::Struct{ static_cast<int>(program->structs.size()) };
				complete::TypeId const new_type_id = add_type(*program, std::move(new_type));
				program->structs.push_back(std::move(new_struct));
				bind_type_name(incomplete_statement.declared_struct.name, new_type_id, *program, scope_stack);

				return std::nullopt;
			},
			[&](incomplete::statement::StructTemplateDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (does_name_collide(scope_stack, incomplete_statement.declared_struct_template.name)) 
					return make_syntax_error(incomplete_statement.declared_struct_template.name, "Struct template name collides with another name.");

				complete::StructTemplate new_template;
				new_template.incomplete_struct = incomplete_statement.declared_struct_template;
				new_template.scope_template_parameters = template_parameters;
				new_template.scope_stack = scope_stack;
				auto const id = add_struct_template(*program, std::move(new_template));
				bind_struct_template_name(incomplete_statement.declared_struct_template.name, id, *program, scope_stack);

				return std::nullopt;
			}
		);

		return std::visit(visitor, incomplete_statement_.variant);
	}

	struct ProgramState
	{
		size_t types;
		size_t structs;
		size_t struct_templates;
		size_t overload_set_types;
		size_t functions;
		size_t extern_functions;
		size_t function_templates;
		FunctionId main_function;

		// Global scope
		int global_scope_stack_frame_size;
		int global_scope_stack_frame_alignment;
		size_t global_scope_variables;
		size_t global_scope_constants;
		size_t global_scope_functions;
		size_t global_scope_types;
		size_t global_scope_function_templates;
		size_t global_scope_struct_templates;
	};
	auto capture_state(complete::Program const & program) noexcept -> ProgramState
	{
		ProgramState program_state;

		program_state.types = program.types.size();
		program_state.structs = program.structs.size();
		program_state.struct_templates = program.struct_templates.size();
		program_state.overload_set_types = program.overload_set_types.size();
		program_state.functions = program.functions.size();
		program_state.extern_functions = program.extern_functions.size();
		program_state.function_templates = program.function_templates.size();
		program_state.main_function = program.main_function;

		program_state.global_scope_stack_frame_size = program.global_scope.stack_frame_size;
		program_state.global_scope_stack_frame_alignment = program.global_scope.stack_frame_alignment;
		program_state.global_scope_variables = program.global_scope.variables.size();
		program_state.global_scope_constants = program.global_scope.constants.size();
		program_state.global_scope_functions = program.global_scope.functions.size();
		program_state.global_scope_types = program.global_scope.types.size();
		program_state.global_scope_function_templates = program.global_scope.function_templates.size();
		program_state.global_scope_struct_templates = program.global_scope.struct_templates.size();

		return program_state;
	}

	auto restore_state(out<complete::Program> program, ProgramState const & program_state) noexcept -> void
	{
		program->types.resize(program_state.types);
		program->structs.resize(program_state.structs);
		program->struct_templates.resize(program_state.struct_templates);
		program->overload_set_types.resize(program_state.overload_set_types);
		program->functions.resize(program_state.functions);
		program->extern_functions.resize(program_state.extern_functions);
		program->function_templates.resize(program_state.function_templates);
		program->main_function = program_state.main_function;

		program->global_scope.stack_frame_size = program_state.global_scope_stack_frame_size;
		program->global_scope.stack_frame_alignment = program_state.global_scope_stack_frame_alignment;
		program->global_scope.variables.resize(program_state.global_scope_variables);
		program->global_scope.constants.resize(program_state.global_scope_constants);
		program->global_scope.functions.resize(program_state.global_scope_functions);
		program->global_scope.types.resize(program_state.global_scope_types);
		program->global_scope.function_templates.resize(program_state.global_scope_function_templates);
		program->global_scope.struct_templates.resize(program_state.global_scope_struct_templates);
	}

	[[nodiscard]] auto semantic_analysis(
		span<incomplete::Statement const> incomplete_program, 
		out<complete::Program> complete_program, 
		ScopeStack & scope_stack
	) noexcept -> expected<void, PartialSyntaxError>
	{
		std::vector<complete::ResolvedTemplateParameter> template_parameters;
		size_t const scope_stack_original_size = scope_stack.size();

		std::vector<std::variant<std::nullopt_t, complete::Statement, PartialSyntaxError>> complete_statements(incomplete_program.size(), std::nullopt);
		std::vector<size_t> unparsed_statements(incomplete_program.size());
		std::iota(unparsed_statements.begin(), unparsed_statements.end(), size_t(0));

		size_t correctly_parsed;
		do
		{
			size_t const size_before = unparsed_statements.size();
			erase_if(unparsed_statements, [&](size_t i)
			{
				ProgramState const program_state = capture_state(*complete_program);

				auto complete_statement = instantiate_statement(incomplete_program[i], template_parameters, scope_stack, out(complete_program), nullptr);
				if (complete_statement.has_value())
				{
					if (complete_statement->has_value())
						complete_statements[i] = std::move(complete_statement->value());
					else
						complete_statements[i] = std::nullopt;

					return true;
				}
				else
				{
					complete_statements[i] = std::move(complete_statement.error());
					// Reset scope stack and program since they may have been left in an invalid state after aborting compilation.
					scope_stack.resize(scope_stack_original_size);
					template_parameters.clear();
					restore_state(complete_program, program_state);
					return false;
				}
			});
			correctly_parsed = unparsed_statements.size() - size_before;
		} while (correctly_parsed > 0);

		// If any statement was left without parsing, compilation failed.
		if (unparsed_statements.size() > 0)
			return Error(std::get<PartialSyntaxError>(complete_statements[unparsed_statements[0]])); TODO("Returning multiple errors");

		for (size_t i = 0; i < incomplete_program.size(); ++i)
		{
			if (complete::Statement * complete_statement = try_get<complete::Statement>(complete_statements[i]))
			{
				if (!has_type<complete::statement::VariableDeclaration>(*complete_statement))
					return make_syntax_error(incomplete_program[i].source, "Only variable declarations, function declarations and struct declarations allowed at global scope.");

				complete_program->global_initialization_statements.push_back(std::move(*complete_statement));
			}
		}

		return success;
	}

	auto semantic_analysis(span<incomplete::Statement const> incomplete_program, out<complete::Program> complete_program) noexcept -> expected<void, PartialSyntaxError>
	{
		ScopeStack scope_stack;
		scope_stack.push_back({&complete_program->global_scope, ScopeType::global, 0});
		return semantic_analysis(incomplete_program, complete_program, scope_stack);
	}

	auto push_global_scopes_of_dependent_modules(
		span<incomplete::Module const> incomplete_modules,
		int module_index,
		span<complete::Scope> module_global_scopes, 
		out<ScopeStack> scope_stack
	) noexcept -> void
	{
		scope_stack->push_back({&module_global_scopes[module_index], ScopeType::global, 0});

		for (int dependency_index : incomplete_modules[module_index].dependencies)
			push_global_scopes_of_dependent_modules(incomplete_modules, dependency_index, module_global_scopes, scope_stack);
	}

	auto semantic_analysis(
		span<incomplete::Module const> incomplete_modules,
		span<int const> parse_order
	) noexcept -> expected<complete::Program, SyntaxError>
	{
		complete::Program program;
		std::vector<complete::Scope> module_global_scopes(incomplete_modules.size());

		ScopeStack scope_stack;
		scope_stack.push_back({&program.global_scope, ScopeType::global, 0});

		for (int i : parse_order)
		{
			scope_stack.resize(1);
			push_global_scopes_of_dependent_modules(incomplete_modules, i, module_global_scopes, out(scope_stack));
			auto analysis_result = semantic_analysis(incomplete_modules[i].statements, out(program), scope_stack);
			if (!analysis_result)
			{
				incomplete::Module::File const & file = file_that_contains(incomplete_modules[i], analysis_result.error().error_in_source);
				return Error(complete_syntax_error(std::move(analysis_result.error()), file.source, file.filename));
			}
		}

		// Fold all scopes into the program's global scope.
		for (complete::Scope & module_global_scope : module_global_scopes)
		{
			for (complete::Constant & constant : module_global_scope.constants)
				program.global_scope.constants.push_back(std::move(constant));

			for (complete::FunctionName & function : module_global_scope.functions)
				program.global_scope.functions.push_back(std::move(function));

			for (complete::FunctionTemplateName & function_template : module_global_scope.function_templates)
				program.global_scope.function_templates.push_back(std::move(function_template));

			for (complete::TypeName & type : module_global_scope.types)
				program.global_scope.types.push_back(std::move(type));

			for (complete::StructTemplateName & struct_template : module_global_scope.struct_templates)
				program.global_scope.struct_templates.push_back(std::move(struct_template));

			for (complete::Variable const & var : module_global_scope.variables)
				add_variable_to_scope(program.global_scope, var.name, var.type, 0, program);
		}

		return std::move(program);
	}

} // namespace instantiation
