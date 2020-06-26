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
#include "utils/map.hh"
#include "utils/out.hh"
#include "utils/overload.hh"
#include "utils/string.hh"
#include "utils/utils.hh"
#include "utils/unreachable.hh"
#include "utils/variant.hh"
#include "utils/warning_macro.hh"
#include <cassert>

using namespace std::literals;

auto make_syntax_error(std::string_view where, complete::ConversionNotFound conversion_error) noexcept -> Error<PartialSyntaxError>
{
	return make_syntax_error(where, join("Error in conversion from ", conversion_error.from.index, " to ", conversion_error.to.index, ": ", conversion_error.why));
}

namespace instantiation
{
	auto top(ScopeStack & scope_stack) noexcept -> complete::Scope & { return *scope_stack.back().scope; }
	auto top(ScopeStackView scope_stack) noexcept -> complete::Scope const & { return *scope_stack.back().scope; }

	auto next_block_scope_offset(ScopeStackView scope_stack) -> int
	{
		if (scope_stack.empty())
			return 0;
		else
			return scope_stack.back().scope_offset + top(scope_stack).stack_frame_size;
	}

	auto push_global_scope(ScopeStack & scope_stack, complete::Scope & scope) noexcept -> StackGuard<ScopeStack>
	{
		scope_stack.push_back({ &scope, ScopeType::global, 0 });
		return StackGuard<ScopeStack>(scope_stack);
	}

	auto push_block_scope(ScopeStack & scope_stack, complete::Scope & scope) noexcept -> StackGuard<ScopeStack>
	{
		int const offset = next_block_scope_offset(scope_stack);
		scope_stack.push_back({ &scope, ScopeType::block, offset });
		return StackGuard<ScopeStack>(scope_stack);
	}

	auto bind_function_name(std::string_view name, FunctionId function_id, complete::Program & program, ScopeStack & scope_stack) -> void
	{
		top(scope_stack).functions.push_back({ std::string(name), function_id });
		std::string & function_ABI_name = ABI_name(program, function_id);
		if (function_ABI_name.empty())
			function_ABI_name = name;
	}

	auto bind_function_template_name(std::string_view name, FunctionTemplateId function_template_id, complete::Program & program, ScopeStack & scope_stack) -> void
	{
		top(scope_stack).function_templates.push_back({ std::string(name), function_template_id });
		std::string & function_ABI_name = ABI_name(program, function_template_id);
		if (function_ABI_name.empty())
			function_ABI_name = name;
	}

	auto bind_type_name(std::string_view name, complete::TypeId type_id, complete::Program & program, ScopeStack & scope_stack)
	{
		top(scope_stack).types.push_back({ std::string(name), type_id });
		std::string & type_ABI_name = ABI_name(program, type_id);
		if (type_ABI_name.empty())
			type_ABI_name = name;
	}

	auto bind_struct_template_name(std::string_view name, complete::StructTemplateId template_id, complete::Program & program, ScopeStack & scope_stack)
	{
		top(scope_stack).struct_templates.push_back({ std::string(name), template_id });
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

	constexpr char null[sizeof(void *)] = {0};

	auto insert_implicit_conversion_node(
		complete::Expression && expr, 
		complete::TypeId from, 
		complete::TypeId to,
		SemanticAnalysisArgs args
	) noexcept -> expected<complete::Expression, complete::ConversionNotFound>
	{
		out<complete::Program> program = args.program;

		if (decay(from) == complete::TypeId::null_t && is_pointer_or_array_pointer(type_with_id(*program, to)))
		{
			complete::expression::Constant constant_null_pointer;
			constant_null_pointer.type = decay(to);
			constant_null_pointer.value = null;
			return insert_mutref_conversion_node(constant_null_pointer, to, *program);
		}

		if (auto const implicit_conversion_functions = named_overload_set("implicit", args.scope_stack))
		{
			FunctionId const conversion_function = resolve_function_overloading_for_conversions(
				*implicit_conversion_functions, from, to, *program, args.template_cache);

			if (conversion_function != function_id_constants::invalid)
			{
				complete::expression::FunctionCall conversion_call;
				conversion_call.function_id = conversion_function;

				complete::TypeId const expected_type = parameter_types_of(*program, conversion_function)[0];

				try_call(conversion_call.parameters.push_back, insert_mutref_conversion_node(std::move(expr), expected_type, *program));
				return insert_mutref_conversion_node(std::move(conversion_call), to, *program);
			}
		}

		return insert_mutref_conversion_node(std::move(expr), from, to, *program);
	}

	auto insert_implicit_conversion_node(
		complete::Expression && expr,
		complete::TypeId to,
		SemanticAnalysisArgs args
	) noexcept -> expected<complete::Expression, complete::ConversionNotFound>
	{
		complete::TypeId const from = expression_type_id(expr, *args.program);
		return insert_implicit_conversion_node(std::move(expr), from, to, args);
	}

	auto insert_implicit_conversion_node(
		complete::Expression && expr,
		complete::TypeId from,
		complete::TypeId to,
		SemanticAnalysisArgs args,
		std::string_view source
	) noexcept -> expected<complete::Expression, PartialSyntaxError>
	{
		if (auto conversion = insert_implicit_conversion_node(std::move(expr), from, to, args))
			return std::move(*conversion);
		else
			return make_syntax_error(source, join("Error in conversion from ",
				ABI_name(*args.program, conversion.error().from), " to ", ABI_name(*args.program, conversion.error().to), ": ", conversion.error().why));
	}

	auto insert_implicit_conversion_node(
		complete::Expression && expr,
		complete::TypeId to,
		SemanticAnalysisArgs args,
		std::string_view source
	) noexcept -> expected<complete::Expression, PartialSyntaxError>
	{
		complete::TypeId const from = expression_type_id(expr, *args.program);
		return insert_implicit_conversion_node(std::move(expr), from, to, args, source);
	}

	auto instantiate_and_evaluate_array_size_expression(
		incomplete::Expression const & expression,
		SemanticAnalysisArgs args
	) noexcept -> expected<int, PartialSyntaxError>
	{
		try_call_decl(complete::Expression size_expr, instantiate_expression(expression, args, nullptr));
		if (!is_constant_expression(size_expr, *args.program, next_block_scope_offset(args.scope_stack)))
			return make_syntax_error(expression.source, "Array size must be a constant expression.");

		try_call(assign_to(size_expr), insert_implicit_conversion_node(std::move(size_expr), complete::TypeId::int32, args, expression.source));

		auto result = interpreter::evaluate_constant_expression_as<int>(size_expr, args);
		if (!result)
			return make_syntax_error(expression.source, "Unmet precondition at evaluating constant expression.");

		return *result;
	}

	auto instantiate_and_evaluate_type_expression(
		incomplete::Expression const & type_expression,
		SemanticAnalysisArgs args
	) noexcept -> expected<complete::TypeId, PartialSyntaxError>
	{
		try_call_decl(complete::Expression type_expr, instantiate_expression(type_expression, args, nullptr));
		
		if (!is_constant_expression(type_expr, *args.program, next_block_scope_offset(args.scope_stack)))
			return make_syntax_error(type_expression.source, "Type expression must be a constant expression.");

		try_call(assign_to(type_expr), insert_implicit_conversion_node(std::move(type_expr), complete::TypeId::type, args, type_expression.source));

		auto const type = interpreter::evaluate_constant_expression_as<complete::TypeId>(type_expr, args);
		if (!type.has_value())
			return make_syntax_error(type_expression.source, "Unmet precondition at evaluating constant expression.");

		return type.value();
	}

	auto resolve_dependent_type(
		incomplete::TypeId const & dependent_type, 
		SemanticAnalysisArgs args
	) noexcept -> expected<complete::TypeId, PartialSyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::TypeId, PartialSyntaxError>
			{
				complete::TypeId const type = type_with_name(base_case.name, args.scope_stack, base_case.namespaces, args.template_parameters);
				if (type == complete::TypeId::none) 
					return make_syntax_error(base_case.name, join("Type not found: ", base_case.name));
				return type;
			},
			[&](incomplete::TypeId::Pointer const & pointer) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*pointer.pointee, args));
				return pointer_type_for(pointee, *args.program);
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const value_type, resolve_dependent_type(*array.value_type, args));

				try_call_decl(int const size, instantiate_and_evaluate_array_size_expression(*array.size, args));
				return array_type_for(value_type, size, *args.program);
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::TypeId, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const pointee, resolve_dependent_type(*array_pointer.pointee, args));
				return array_pointer_type_for(pointee, *args.program);
			},
			[&](incomplete::TypeId::TemplateInstantiation const & template_instantiation) -> expected<complete::TypeId, PartialSyntaxError>
			{
				auto const visitor = overload(
					[](lookup_result::StructTemplate result) -> complete::StructTemplateId { return result.template_id; },
					[](auto const &) -> complete::StructTemplateId { declare_unreachable(); }
				);

				auto lookup = lookup_name(args.scope_stack, template_instantiation.template_name, template_instantiation.namespaces);
				complete::StructTemplateId const template_id = std::visit(visitor, lookup);

				std::vector<complete::TypeId> parameters;
				parameters.reserve(template_instantiation.parameters.size());
				for (incomplete::TypeId const & parameter : template_instantiation.parameters)
					try_call(parameters.push_back, resolve_dependent_type(parameter, args));

				return instantiate_struct_template(*args.program, template_id, parameters, args.template_cache, template_instantiation.template_name);
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
		span<incomplete::TemplateParameter const> unresolved_template_parameters,
		SemanticAnalysisArgs args
	) noexcept -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
	{
		auto const visitor = overload(
			[&](incomplete::TypeId::BaseCase const & base_case) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::TypeId const type = type_with_name(base_case.name, args.scope_stack, base_case.namespaces, args.template_parameters);
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
					resolve_function_template_parameter_type(*pointer.pointee, unresolved_template_parameters, args));
				return complete::FunctionTemplateParameterType{std::move(pointer_type), false, false};
			},
			[&](incomplete::TypeId::Array const & array) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::FunctionTemplateParameterType::Array array_type;
				try_call(assign_to(array_type.value_type), 
					resolve_function_template_parameter_type(*array.value_type, unresolved_template_parameters, args));

				try_call(assign_to(array_type.size), instantiate_and_evaluate_array_size_expression(*array.size, args));
				return complete::FunctionTemplateParameterType{ std::move(array_type), false, false };
			},
			[&](incomplete::TypeId::ArrayPointer const & array_pointer) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				complete::FunctionTemplateParameterType::ArrayPointer array_pointer_type;
				try_call(assign_to(array_pointer_type.pointee),
					resolve_function_template_parameter_type(*array_pointer.pointee, unresolved_template_parameters, args));
				return complete::FunctionTemplateParameterType{std::move(array_pointer_type), false, false};
			},
			[&](incomplete::TypeId::TemplateInstantiation const & template_instantiation) -> expected<complete::FunctionTemplateParameterType, PartialSyntaxError>
			{
				auto const template_id = struct_template_with_name(template_instantiation.template_name, args.scope_stack, template_instantiation.namespaces);
				if (!template_id.has_value())
					return make_syntax_error(template_instantiation.template_name, "Struct template not found");

				complete::FunctionTemplateParameterType::TemplateInstantiation template_instantiation_type;
				template_instantiation_type.template_id = *template_id;
				template_instantiation_type.parameters.reserve(template_instantiation.parameters.size());
				for (incomplete::TypeId const & param : template_instantiation.parameters)
					try_call(template_instantiation_type.parameters.push_back, 
						resolve_function_template_parameter_type(param, unresolved_template_parameters, args));

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
			lookup_result::NamespaceNotFound,
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

	auto lookup_name(complete::Scope const & scope, std::string_view name) noexcept
		-> std::variant<
			lookup_result::Nothing,
			lookup_result::NamespaceNotFound,
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

		auto const var = std::find_if(scope.variables, [name](Variable const & var) { return var.name == name; });
		if (var != scope.variables.end())
			return lookup_result::GlobalVariable{var->type, var->offset};

		auto const type = std::find_if(scope.types, [name](TypeName const & var) { return var.name == name; });
		if (type != scope.types.end())
			return lookup_result::Type{type->id};

		auto const struct_template = std::find_if(scope.struct_templates, [name](StructTemplateName const & var) { return var.name == name; });
		if (struct_template != scope.struct_templates.end())
			return lookup_result::StructTemplate{struct_template->id};

		auto const constant = std::find_if(scope.constants, [name](Constant const & var) { return var.name == name; });
		if (constant != scope.constants.end())
			return lookup_result::Constant{&*constant};

		for (FunctionName const & fn : scope.functions)
			if (fn.name == name)
				overload_set.function_ids.push_back(fn.id);

		for (FunctionTemplateName const & fn : scope.function_templates)
			if (fn.name == name)
				overload_set.function_template_ids.push_back(fn.id);

		if (overload_set.function_ids.empty() && overload_set.function_template_ids.empty())
			return lookup_result::Nothing();
		else
			return overload_set;
	}

	auto find_namespace(ScopeStackView scope_stack, span<std::string_view const> names) -> complete::Namespace *
	{
		int const start = static_cast<int>(scope_stack.size() - 1);
		for (int i = start; i >= 0; --i)
		{
			if (scope_stack[i].type == ScopeType::global)
			{
				auto const ns = find_namespace(static_cast<complete::Namespace &>(*scope_stack[i].scope), names);
				if (ns)
					return ns;
			}
		}

		return nullptr;
	}

	auto lookup_name(ScopeStackView scope_stack, std::string_view name, span<std::string_view const> namespace_names) noexcept
		-> std::variant<
			lookup_result::Nothing,
			lookup_result::NamespaceNotFound,
			lookup_result::Variable,
			lookup_result::Constant,
			lookup_result::GlobalVariable,
			lookup_result::OverloadSet,
			lookup_result::Type,
			lookup_result::StructTemplate
		>
	{
		if (namespace_names.empty())
		{
			return lookup_name(scope_stack, name);
		}
		else
		{
			complete::Namespace * const ns = find_namespace(scope_stack, namespace_names);
			if (ns == nullptr)
				return lookup_result::NamespaceNotFound();
			else
				return lookup_name(*ns, name);
		}
	}

	auto named_overload_set(std::string_view name, ScopeStackView scope_stack) -> std::optional<complete::OverloadSet>
	{
		auto const visitor = overload(
			[](lookup_result::Nothing const &) -> std::optional<complete::OverloadSet>
			{
				return complete::OverloadSet();
			},
			[](lookup_result::OverloadSet const & overload_set) -> std::optional<complete::OverloadSet>
			{
				return overload_set;
			},
			[](auto const &) -> std::optional<complete::OverloadSet> { return std::nullopt; }
		);

		auto lookup = lookup_name(scope_stack, name);
		return std::visit(visitor, lookup);
	}

	auto operator_overload_set(Operator op, ScopeStackView scope_stack) noexcept -> complete::OverloadSet
	{
		auto set = named_overload_set(operator_function_name(op), scope_stack);
		assert(set);
		return std::move(*set);
	}

	auto type_with_name(std::string_view name, ScopeStackView scope_stack, span<std::string_view const> namespaces) noexcept -> complete::TypeId
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

		auto lookup = lookup_name(scope_stack, name, namespaces);
		return std::visit(visitor, lookup);
	}

	auto type_with_name(std::string_view name, ScopeStackView scope_stack, span<std::string_view const> namespaces, span<complete::ResolvedTemplateParameter const> template_parameters) noexcept
		-> complete::TypeId
	{
		complete::TypeId const type = type_with_name(name, scope_stack, namespaces);
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

	auto struct_template_with_name(std::string_view name, ScopeStackView scope_stack, span<std::string_view const> namespaces) noexcept -> std::optional<complete::StructTemplateId>
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

		auto lookup = lookup_name(scope_stack, name, namespaces);
		return std::visit(visitor, lookup);
	}

	auto type_descriptor_for(complete::TypeId type_id, complete::Program const & program) noexcept -> callc::TypeDescriptor
	{
		if (type_id.is_reference)
			return {sizeof(void *), false};

		bool const is_float = (decay(type_id) == complete::TypeId::float32) || (decay(type_id) == complete::TypeId::float64);

		complete::Type const & type = type_with_id(program, type_id);
		if (type.size > 8)
			mark_as_to_do("Big types");

		return {type.size, is_float};
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
	};
	struct ScopeState
	{
		int stack_frame_size;
		int stack_frame_alignment;
		size_t variables;
		size_t constants;
		size_t functions;
		size_t types;
		size_t function_templates;
		size_t struct_templates;
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

		return program_state;
	}
	auto capture_state(complete::Scope const & scope) noexcept -> ScopeState
	{
		ScopeState scope_state;

		scope_state.stack_frame_size = scope.stack_frame_size;
		scope_state.stack_frame_alignment = scope.stack_frame_alignment;
		scope_state.variables = scope.variables.size();
		scope_state.constants = scope.constants.size();
		scope_state.functions = scope.functions.size();
		scope_state.types = scope.types.size();
		scope_state.function_templates = scope.function_templates.size();
		scope_state.struct_templates = scope.struct_templates.size();

		return scope_state;
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
	}
	auto restore_state(out<complete::Scope> scope, ScopeState const & scope_state) noexcept -> void
	{
		scope->stack_frame_size = scope_state.stack_frame_size;
		scope->stack_frame_alignment = scope_state.stack_frame_alignment;
		scope->variables.resize(scope_state.variables);
		scope->constants.resize(scope_state.constants);
		scope->functions.resize(scope_state.functions);
		scope->types.resize(scope_state.types);
		scope->function_templates.resize(scope_state.function_templates);
		scope->struct_templates.resize(scope_state.struct_templates);
	}

	auto test_if_expression_compiles(
		incomplete::ExpressionToTest const & expression_to_test,
		SemanticAnalysisArgs args,
		optional_out<complete::TypeId> current_scope_return_type
	) -> bool
	{
		ProgramState const program_state = capture_state(*args.program);
		ScopeState const top_scope_state = capture_state(top(args.scope_stack));
		TemplateCache const template_cache_state = args.template_cache;
		size_t const scope_stack_size = args.scope_stack.size();

		auto expr = instantiate_expression(expression_to_test.expression, args, current_scope_return_type);
		if (!expr)
		{
			args.scope_stack.resize(scope_stack_size);
			restore_state(args.program, program_state);
			restore_state(out(top(args.scope_stack)), top_scope_state);
			args.template_cache = template_cache_state;
			return false;
		}

		if (!expression_to_test.expected_type)
			return true;

		auto expected_return_type = resolve_dependent_type(*expression_to_test.expected_type, args);
		if (!expected_return_type)
			return false;

		return is_convertible(expression_type_id(*expr, *args.program), *expected_return_type, *args.program);
	}

	auto resolve_concepts(span<incomplete::TemplateParameter const> template_parameters, SemanticAnalysisArgs args) noexcept
		-> expected<std::vector<FunctionId>, PartialSyntaxError>
	{
		std::vector<FunctionId> concepts;

		for (incomplete::TemplateParameter param : template_parameters)
		{
			if (param.concept.empty())
			{
				concepts.push_back(function_id_constants::invalid);
			}
			else
			{
				std::optional<complete::OverloadSet> set = named_overload_set(param.concept, args.scope_stack);
				if (!set)
					return make_syntax_error(param.concept, "Name does not name a concept. A concept is a function type -> bool");

				FunctionId const concept_function = resolve_function_overloading(*set, {complete::TypeId::type}, *args.program, args.template_cache);
				if (concept_function == function_id_constants::invalid || return_type(*args.program, concept_function) != complete::TypeId::bool_)
					return make_syntax_error(param.concept, "Name does not name a concept. A concept is a function type -> bool");

				concepts.push_back(concept_function);
			}
		}

		return std::move(concepts);
	}

	auto instantiate_constructor_call(
		complete::TypeId constructed_type_id,
		span<complete::Expression> parameters,
		SemanticAnalysisArgs args,
		std::string_view expression_source
	) noexcept
		-> expected<complete::Expression, PartialSyntaxError>
	{
		if (parameters.size() == 0) // Default constructor
		{
			if (!is_default_constructible(constructed_type_id, *args.program))
				return make_syntax_error(expression_source, "Cannot construct with 0 parameters a type that is not default constructible.");
			return synthesize_default_constructor(constructed_type_id, *args.program);
		}

		// Check if this can be a conversion.
		if (parameters.size() == 1)
		{
			complete::TypeId const param_type_id = expression_type_id(parameters[0], *args.program);

			// Implicit conversion
			auto const implicit_conversion = insert_implicit_conversion_node(std::move(parameters[0]), param_type_id, constructed_type_id, args);
			if (implicit_conversion.has_value())
				return std::move(*implicit_conversion);

			if (auto const explicit_conversion_functions = named_overload_set("conversion", args.scope_stack))
			{
				FunctionId const conversion_function = resolve_function_overloading_for_conversions(
					*explicit_conversion_functions, param_type_id, constructed_type_id, *args.program, args.template_cache);

				if (conversion_function != function_id_constants::invalid)
				{
					complete::expression::FunctionCall conversion_call;
					conversion_call.function_id = conversion_function;

					complete::TypeId const expected_type = parameter_types_of(*args.program, conversion_function)[0];

					try_call(conversion_call.parameters.push_back, insert_implicit_conversion_node(std::move(parameters[0]), expected_type, args, expression_source));
					return std::move(conversion_call);
				}
			}

			// Conversion to byte array pointer from any pointer type or from any pointer type to byte array pointer.
			complete::Type const & constructed_type = type_with_id(*args.program, constructed_type_id);
			complete::Type const & param_type = type_with_id(*args.program, param_type_id);
			if (((is_array_pointer(constructed_type) && decay(pointee_type(constructed_type)) == complete::TypeId::byte && is_pointer_or_array_pointer(param_type)) ||
				(is_array_pointer(param_type) && decay(pointee_type(param_type)) == complete::TypeId::byte && is_pointer_or_array_pointer(constructed_type))) &&
				complete::is_mutability_conversion_legal(pointee_type(param_type).is_mutable, pointee_type(constructed_type).is_mutable))
			{
				complete::expression::ReinterpretCast reinterpret_cast_node;
				try_call(assign_to(reinterpret_cast_node.operand), insert_implicit_conversion_node(std::move(parameters[0]), decay(param_type_id), args, expression_source));
				reinterpret_cast_node.return_type = constructed_type_id;
				return std::move(reinterpret_cast_node);
			}
		}

		complete::expression::Constructor complete_expression;
		complete_expression.constructed_type = constructed_type_id;

		complete::Type const & constructed_type = type_with_id(*args.program, constructed_type_id);
		if (is_struct(constructed_type))
		{
			complete::Struct const & constructed_struct = *struct_for_type(*args.program, constructed_type);

			if (has_compiler_generated_constructors(constructed_struct))
			{
				if (constructed_struct.member_variables.size() != parameters.size())
					return make_syntax_error(expression_source, "Incorrect number of arguments for struct constructor.");

				size_t const n = parameters.size();
				complete_expression.parameters.reserve(n);
				for (size_t i = 0; i < n; ++i)
				{
					try_call(complete_expression.parameters.push_back,
						insert_implicit_conversion_node(std::move(parameters[i]), constructed_struct.member_variables[i].type, args, expression_source));
				}
			}
			else
				return make_syntax_error(expression_source, "Compiler generated constructors are disabled for this type because it has user defined constructors.");
		}
		else if (is_array(constructed_type))
		{
			complete::Type::Array const & constructed_array = std::get<complete::Type::Array>(constructed_type.extra_data);
			complete::TypeId const array_type = constructed_array.value_type;
			int const array_size = constructed_array.size;
			int const param_count = static_cast<int>(parameters.size());

			if (parameters.size() != 1 && param_count != array_size)
				return make_syntax_error(expression_source, "Incorrect number of arguments for array constructor.");

			if (parameters.size() == 1 && array_size != 1 && !is_copy_constructible(*args.program, array_type))
				return make_syntax_error(expression_source, 
					"Single parameter array fill constructor cannot be used with types that cannot be copied. "
					"This is because the first element of the array is constructed from the given expression,"
					"then the rest are constructed as copies of the first.");

			complete_expression.parameters.reserve(param_count);
			for (int i = 0; i < param_count; ++i)
			{
				try_call(complete_expression.parameters.push_back, insert_implicit_conversion_node(std::move(parameters[i]), array_type, args, expression_source));
			}
		}
		// Not an actual array pointer but array type with deduced size.
		else if (is_array_pointer(constructed_type))
		{
			int const param_count = static_cast<int>(parameters.size());

			complete::TypeId const value_type = pointee_type(constructed_type);
			complete::TypeId const constructed_array_type = array_type_for(value_type, param_count, *args.program);
			complete_expression.constructed_type = constructed_array_type;

			complete_expression.parameters.reserve(param_count);
			for (int i = 0; i < param_count; ++i)
			{
				try_call(complete_expression.parameters.push_back, insert_implicit_conversion_node(std::move(parameters[i]), value_type, args, expression_source));
			}
		}
		else
		{
			return make_syntax_error(expression_source, "Conversion between types does not exist.");
		}

		return std::move(complete_expression);
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		SemanticAnalysisArgs args,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, PartialSyntaxError>;

	[[nodiscard]] auto instantiate_function_prototype(
		incomplete::FunctionPrototype const & incomplete_function,
		SemanticAnalysisArgs args,
		out<complete::Function> function
	) -> expected<void, PartialSyntaxError>
	{

		for (incomplete::FunctionParameter const & parameter : incomplete_function.parameters)
		{
			try_call_decl(complete::TypeId const parameter_type, resolve_dependent_type(parameter.type, args));
			add_variable_to_scope(*function, parameter.name, parameter_type, 0, *args.program);
		}

		function->parameter_count = static_cast<int>(incomplete_function.parameters.size());
		function->parameter_size = function->stack_frame_size;

		if (incomplete_function.return_type.has_value())
			try_call(assign_to(function->return_type), resolve_dependent_type(*incomplete_function.return_type, args))
		else
			function->return_type = complete::TypeId::deduce;

		return success;
	}

	[[nodiscard]] auto instantiate_function_body(
		incomplete::Function const & incomplete_function,
		SemanticAnalysisArgs args,
		out<complete::Function> function
	) -> expected<void, PartialSyntaxError>
	{
		args.scope_stack.push_back({&*function, ScopeType::function, 0});

		function->preconditions.reserve(incomplete_function.preconditions.size());
		for (incomplete::Expression const & precondition : incomplete_function.preconditions)
		{
			try_call_decl(auto complete_precondition, instantiate_expression(precondition, args, out(function->return_type)));
			try_call(function->preconditions.push_back, insert_implicit_conversion_node(
				std::move(complete_precondition), complete::TypeId::bool_, args, precondition.source));
		}

		function->statements.reserve(incomplete_function.statements.size());
		for (incomplete::Statement const & substatment : incomplete_function.statements)
		{
			try_call_decl(auto complete_substatement, instantiate_statement(substatment, args, out(function->return_type)));
			if (complete_substatement.has_value())
				function->statements.push_back(std::move(*complete_substatement));
		}

		args.scope_stack.pop_back();

		if (function->return_type == complete::TypeId::deduce)
			function->return_type = complete::TypeId::void_;

		function->is_callable_at_compile_time = can_be_run_in_a_constant_expression(*function, *args.program);
		function->is_callable_at_runtime = can_be_run_at_runtime(*function, *args.program);

		return success;
	}

	auto add_member_destructor(out<complete::Function> destructor, complete::TypeId destroyed_type, complete::TypeId member_type, int member_offset, complete::Program const & program) -> void
	{
		FunctionId const member_destructor = destructor_for(program, member_type);
		if (member_destructor != function_id_constants::invalid)
		{
			complete::expression::LocalVariable parameter_access;
			parameter_access.variable_offset = 0;
			parameter_access.variable_type = make_mutable(make_reference(destroyed_type));

			complete::expression::MemberVariable member_access;
			member_access.variable_offset = member_offset;
			member_access.variable_type = make_mutable(make_reference(member_type));
			member_access.owner = allocate(complete::Expression(parameter_access));

			complete::statement::ExpressionStatement destructor_call_statement;
			complete::expression::FunctionCall destructor_call;
			destructor_call.function_id = member_destructor;
			destructor_call.parameters.push_back(std::move(member_access));
			destructor_call_statement.expression = std::move(destructor_call);

			destructor->statements.push_back(std::move(destructor_call_statement));
		}
	}

	auto are_all_trivially_destructible(span<complete::MemberVariable const> member_variables, complete::Program const & program) -> bool
	{
		for (complete::MemberVariable const & member : member_variables)
			if (!is_trivially_destructible(program, member.type))
				return false;
		return true;
	}

	auto add_member_destructors(out<complete::Function> destructor, complete::TypeId destroyed_type, span<complete::MemberVariable const> member_variables, complete::Program const & program) -> void
	{
		for (complete::MemberVariable const & member : member_variables)
			add_member_destructor(destructor, destroyed_type, member.type, member.offset, program);

		// Recheck if the function can be called at compile time and run time now that new statements have been added.
		destructor->is_callable_at_compile_time = can_be_run_in_a_constant_expression(*destructor, program);
		destructor->is_callable_at_runtime = can_be_run_at_runtime(*destructor, program);
	}

	auto synthesize_default_destructor(complete::TypeId destroyed_type, span<complete::MemberVariable const> member_variables, complete::Program const & program) -> complete::Function
	{
		complete::Function destructor;
		destructor.return_type = complete::TypeId::void_;
		destructor.parameter_size = sizeof(void *);
		destructor.parameter_count = 1;
		add_variable_to_scope(destructor, "this", make_mutable(make_reference(destroyed_type)), 0, program);
	
		add_member_destructors(out(destructor), destroyed_type, member_variables, program);

		return destructor;
	}

	auto synthesize_array_default_destructor(complete::TypeId destroyed_type, complete::TypeId value_type, int size, complete::Program const & program) -> complete::Function
	{
		complete::Function destructor;
		destructor.return_type = complete::TypeId::void_;
		destructor.parameter_size = sizeof(void *);
		destructor.parameter_count = 1;
		add_variable_to_scope(destructor, "this", make_mutable(make_reference(destroyed_type)), 0, program);

		int const value_type_size = type_size(program, value_type);
		for (int i = 0; i < size; ++i)
			add_member_destructor(out(destructor), destroyed_type, value_type, i * value_type_size, program);

		destructor.is_callable_at_compile_time = can_be_run_in_a_constant_expression(destructor, program);
		destructor.is_callable_at_runtime = can_be_run_at_runtime(destructor, program);

		return destructor;
	}

	auto are_all_trivially_copyable(span<complete::MemberVariable const> member_variables, complete::Program const & program) -> bool
	{
		for (complete::MemberVariable const & member : member_variables)
			if (!is_trivially_copy_constructible(program, member.type))
				return false;
		return true;
	}

	auto are_all_copy_constructible(span<complete::MemberVariable const> member_variables, complete::Program const & program) -> bool
	{
		for (complete::MemberVariable const & member : member_variables)
			if (!is_copy_constructible(program, member.type))
				return false;
		return true;
	}

	auto are_all_move_constructible(span<complete::MemberVariable const> member_variables, complete::Program const & program) -> bool
	{
		for (complete::MemberVariable const & member : member_variables)
			if (!is_move_constructible(program, member.type))
				return false;
		return true;
	}

	auto add_member_copy_constructor(
		out<complete::expression::Constructor> constructor_expression, 
		complete::TypeId owner_type, complete::TypeId member_type, int member_offset, 
		complete::Program const & program) -> void
	{
		complete::expression::LocalVariable parameter_access;
		parameter_access.variable_offset = 0;
		parameter_access.variable_type = make_reference(owner_type);

		complete::expression::MemberVariable member_access;
		member_access.variable_offset = member_offset;
		member_access.variable_type = make_reference(member_type);
		member_access.owner = allocate(complete::Expression(parameter_access));

		FunctionId const member_copy_constructor = copy_constructor_for(program, member_type);
		if (member_copy_constructor == function_id_constants::invalid) // Trivially copyable member
		{
			complete::expression::Dereference member_dereference;
			member_dereference.expression = allocate(complete::Expression(std::move(member_access)));
			member_dereference.return_type = member_type;

			constructor_expression->parameters.push_back(std::move(member_dereference));
		}
		else // Member with copy constructor function
		{
			complete::expression::FunctionCall member_copy_call;
			member_copy_call.function_id = member_copy_constructor;
			member_copy_call.parameters.push_back(std::move(member_access));

			constructor_expression->parameters.push_back(std::move(member_copy_call));
		}
	}

	auto synthesize_default_copy_constructor(complete::TypeId owner_type, span<complete::MemberVariable const> member_variables, complete::Program const & program) -> complete::Function
	{
		complete::Function copy_constructor;
		copy_constructor.return_type = owner_type;
		copy_constructor.parameter_size = sizeof(void *);
		copy_constructor.parameter_count = 1;
		add_variable_to_scope(copy_constructor, "other", make_reference(owner_type), 0, program);

		complete::expression::Constructor constructor_expression;
		constructor_expression.constructed_type = owner_type;
		constructor_expression.parameters.reserve(member_variables.size());

		for (complete::MemberVariable const & member : member_variables)
			add_member_copy_constructor(out(constructor_expression), owner_type, member.type, member.offset, program);

		complete::statement::Return return_statement;
		return_statement.destroyed_stack_frame_size = 8;
		return_statement.returned_expression = std::move(constructor_expression);
		copy_constructor.statements.push_back(std::move(return_statement));

		copy_constructor.is_callable_at_compile_time = can_be_run_in_a_constant_expression(copy_constructor, program);
		copy_constructor.is_callable_at_runtime = can_be_run_at_runtime(copy_constructor, program);

		return copy_constructor;
	}

	auto synthesize_array_default_copy_constructor(complete::TypeId owner_type, complete::TypeId value_type, int size, complete::Program const & program) -> complete::Function
	{
		complete::Function copy_constructor;
		copy_constructor.return_type = owner_type;
		copy_constructor.parameter_size = sizeof(void *);
		copy_constructor.parameter_count = 1;
		add_variable_to_scope(copy_constructor, "other", make_reference(owner_type), 0, program);

		complete::expression::Constructor constructor_expression;
		constructor_expression.constructed_type = owner_type;
		constructor_expression.parameters.reserve(size);

		int const value_type_size = type_size(program, value_type);
		for (int i = 0; i < size; ++i)
			add_member_copy_constructor(out(constructor_expression), owner_type, value_type, i * value_type_size, program);

		complete::statement::Return return_statement;
		return_statement.destroyed_stack_frame_size = 8;
		return_statement.returned_expression = std::move(constructor_expression);
		copy_constructor.statements.push_back(std::move(return_statement));

		copy_constructor.is_callable_at_compile_time = can_be_run_in_a_constant_expression(copy_constructor, program);
		copy_constructor.is_callable_at_runtime = can_be_run_at_runtime(copy_constructor, program);

		return copy_constructor;
	}

	auto add_member_move_constructor(
		out<complete::expression::Constructor> constructor_expression,
		complete::TypeId owner_type, complete::TypeId member_type, int member_offset,
		complete::Program const & program) -> void
	{
		complete::expression::LocalVariable parameter_access;
		parameter_access.variable_offset = 0;
		parameter_access.variable_type = make_reference(owner_type);

		complete::expression::MemberVariable member_access;
		member_access.variable_offset = member_offset;
		member_access.variable_type = make_reference(member_type);
		member_access.owner = allocate(complete::Expression(parameter_access));

		FunctionId const member_move_constructor = move_constructor_for(program, member_type);
		if (member_move_constructor == function_id_constants::invalid) // Trivially movable member
		{
			complete::expression::Dereference member_dereference;
			member_dereference.expression = allocate(complete::Expression(std::move(member_access)));
			member_dereference.return_type = member_type;

			constructor_expression->parameters.push_back(std::move(member_dereference));
		}
		else // Member with move constructor function
		{
			complete::expression::FunctionCall member_move_call;
			member_move_call.function_id = member_move_constructor;
			member_move_call.parameters.push_back(std::move(member_access));

			constructor_expression->parameters.push_back(std::move(member_move_call));
		}
	}

	auto synthesize_default_move_constructor(complete::TypeId owner_type, span<complete::MemberVariable const> member_variables, complete::Program const & program) -> complete::Function
	{
		complete::Function move_constructor;
		move_constructor.return_type = owner_type;
		move_constructor.parameter_size = sizeof(void *);
		move_constructor.parameter_count = 1;
		add_variable_to_scope(move_constructor, "other", make_mutable(make_reference(owner_type)), 0, program);

		complete::expression::Constructor constructor_expression;
		constructor_expression.constructed_type = owner_type;
		constructor_expression.parameters.reserve(member_variables.size());

		for (complete::MemberVariable const & member : member_variables)
			add_member_move_constructor(out(constructor_expression), owner_type, member.type, member.offset, program);

		complete::statement::Return return_statement;
		return_statement.destroyed_stack_frame_size = 8;
		return_statement.returned_expression = std::move(constructor_expression);
		move_constructor.statements.push_back(std::move(return_statement));

		move_constructor.is_callable_at_compile_time = can_be_run_in_a_constant_expression(move_constructor, program);
		move_constructor.is_callable_at_runtime = can_be_run_at_runtime(move_constructor, program);

		return move_constructor;
	}

	auto synthesize_array_default_move_constructor(complete::TypeId owner_type, complete::TypeId value_type, int size, complete::Program const & program) -> complete::Function
	{
		complete::Function move_constructor;
		move_constructor.return_type = owner_type;
		move_constructor.parameter_size = sizeof(void *);
		move_constructor.parameter_count = 1;
		add_variable_to_scope(move_constructor, "other", make_mutable(make_reference(owner_type)), 0, program);

		complete::expression::Constructor constructor_expression;
		constructor_expression.constructed_type = owner_type;
		constructor_expression.parameters.reserve(size);

		int const value_type_size = type_size(program, value_type);
		for (int i = 0; i < size; ++i)
			add_member_move_constructor(out(constructor_expression), owner_type, value_type, i * value_type_size, program);

		complete::statement::Return return_statement;
		return_statement.destroyed_stack_frame_size = 8;
		return_statement.returned_expression = std::move(constructor_expression);
		move_constructor.statements.push_back(std::move(return_statement));

		move_constructor.is_callable_at_compile_time = can_be_run_in_a_constant_expression(move_constructor, program);
		move_constructor.is_callable_at_runtime = can_be_run_at_runtime(move_constructor, program);

		return move_constructor;
	}

	auto instantiate_incomplete_struct_variables(
		incomplete::Struct const & incomplete_struct,
		SemanticAnalysisArgs args
	) -> expected<InstantiatedStruct, PartialSyntaxError>
	{
		InstantiatedStruct new_struct;
		new_struct.complete_struct.member_variables.reserve(incomplete_struct.member_variables.size());

		for (incomplete::MemberVariable const & member_variable : incomplete_struct.member_variables)
		{
			try_call_decl(complete::TypeId const member_type, resolve_dependent_type(member_variable.type, args));
			if (!is_data_type(member_type))
				return make_syntax_error(member_variable.name, "Member variable cannot be void.");
			if (member_type.is_reference)
				return make_syntax_error(member_variable.name, "Member variable cannot be reference.");
			if (member_type.is_mutable)
				return make_syntax_error(member_variable.name, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

			add_variable_to_scope(new_struct.complete_struct.member_variables, new_struct.size, new_struct.alignment, member_variable.name, member_type, 0, *args.program);

			complete::MemberVariable & new_variable = new_struct.complete_struct.member_variables.back();
			if (member_variable.initializer_expression.has_value())
			{
				try_call_decl(complete::Expression initializer,
					instantiate_expression(*member_variable.initializer_expression, args, nullptr));

				try_call(assign_to(new_variable.initializer_expression),
					insert_implicit_conversion_node(std::move(initializer), member_type, args, member_variable.initializer_expression->source));
			}
			else if (is_default_constructible(member_type, *args.program))
			{
				new_variable.initializer_expression = synthesize_default_constructor(member_type, *args.program);
			}
		}

		return std::move(new_struct);
	}

	auto instantiate_incomplete_struct_functions(
		incomplete::Struct const & incomplete_struct,
		complete::TypeId new_type_id, int new_struct_id,
		SemanticAnalysisArgs args
	) -> expected<void, PartialSyntaxError>
	{
		out<complete::Program> program = args.program;

		bool const custom_destructor_declared = !has_type<nothing_t>(incomplete_struct.destructor);
		if (incomplete::Function const * incomplete_destructor = try_get<incomplete::Function>(incomplete_struct.destructor))
		{
			try_call_decl(complete::Function destructor,
				instantiate_function_template(*incomplete_destructor, args));

			if (destructor.return_type != complete::TypeId::void_)
				return make_syntax_error(incomplete_destructor->parameters[0].name, "Return type of destructor must be void.");
			if (decay(destructor.variables[0].type) != new_type_id)
				return make_syntax_error(incomplete_destructor->parameters[0].name, "Parameter type of destructor must be type of struct.");

			add_member_destructors(out(destructor), new_type_id, program->structs[new_struct_id].member_variables, *program);

			program->structs[new_struct_id].destructor = add_function(*program, std::move(destructor));
		}
		else
		{
			if (!are_all_trivially_destructible(program->structs[new_struct_id].member_variables, *program))
			{
				complete::Function destructor = instantiation::synthesize_default_destructor(new_type_id, program->structs[new_struct_id].member_variables, *program);
				program->structs[new_struct_id].destructor = add_function(*program, std::move(destructor));
			}
		}

		FunctionId default_constructor = function_id_constants::invalid;
		FunctionId copy_constructor = function_id_constants::invalid;
		FunctionId move_constructor = function_id_constants::invalid;

		bool const custom_default_constructor_declared = !has_type<nothing_t>(incomplete_struct.default_constructor);
		if (incomplete::Function const * incomplete_default_constructor = try_get<incomplete::Function>(incomplete_struct.default_constructor))
		{
			try_call_decl(complete::Function constructor_function,
				instantiate_function_template(*incomplete_default_constructor, args));

			if (constructor_function.return_type != new_type_id)
				return make_syntax_error(incomplete_default_constructor->statements[0].source, "Return type of constructor must be constructed type.");

			default_constructor = add_function(*program, std::move(constructor_function));
		}

		bool const custom_copy_constructor_declared = !has_type<nothing_t>(incomplete_struct.copy_constructor);
		if (incomplete::Function const * incomplete_copy_constructor = try_get<incomplete::Function>(incomplete_struct.copy_constructor))
		{
			try_call_decl(complete::Function constructor_function,
				instantiate_function_template(*incomplete_copy_constructor, args));

			if (constructor_function.return_type != new_type_id)
				return make_syntax_error(incomplete_copy_constructor->statements[0].source, "Return type of constructor must be constructed type.");

			if (constructor_function.variables[0].type != make_reference(new_type_id))
				return make_syntax_error(incomplete_copy_constructor->parameters[0].name, "Parameter of copy constructor must be of type T &.");

			copy_constructor = add_function(*program, std::move(constructor_function));
		}

		bool const custom_move_constructor_declared = !has_type<nothing_t>(incomplete_struct.move_constructor);
		if (incomplete::Function const * incomplete_move_constructor = try_get<incomplete::Function>(incomplete_struct.move_constructor))
		{
			try_call_decl(complete::Function constructor_function,
				instantiate_function_template(*incomplete_move_constructor, args));

			if (constructor_function.return_type != new_type_id)
				return make_syntax_error(incomplete_move_constructor->statements[0].source, "Return type of constructor must be constructed type.");

			if (constructor_function.variables[0].type != make_mutable(make_reference(new_type_id)))
				return make_syntax_error(incomplete_move_constructor->parameters[0].name, "Parameter of move constructor must be of type T mut &.");

			move_constructor = add_function(*program, std::move(constructor_function));
		}
	
		std::vector<complete::Constructor> constructors;
		constructors.reserve(incomplete_struct.constructors.size());

		for (incomplete::Constructor const & incomplete_constructor : incomplete_struct.constructors)
		{
			try_call_decl(complete::Function constructor_function,
				instantiate_function_template(incomplete_constructor, args));

			if (constructor_function.return_type != new_type_id)
				return make_syntax_error(incomplete_constructor.name, "Return type of constructor must be constructed type.");

			span<complete::Variable> const constructor_parameters = constructor_function.variables;
			FunctionId const constructor_function_id = add_function(*program, std::move(constructor_function));

			complete::Constructor constructor;
			constructor.function = constructor_function_id;
			constructor.name = incomplete_constructor.name;
			constructors.push_back(std::move(constructor));
		}

		complete::Struct & new_struct = program->structs[new_struct_id];

		new_struct.has_compiler_generated_constructors = (!custom_copy_constructor_declared && !custom_move_constructor_declared && !custom_destructor_declared);
		if (!new_struct.has_compiler_generated_constructors && !custom_default_constructor_declared)
			default_constructor = function_id_constants::deleted;

		// There are 4 type categories. Rule of 0, only destructor, destructor + move and destructor, copy and move
		// Ensure that this type falls into one of them.
		if (!(!custom_destructor_declared && !custom_copy_constructor_declared && !custom_move_constructor_declared))
		{
			if (custom_destructor_declared && !custom_copy_constructor_declared && !custom_move_constructor_declared)
			{
				copy_constructor = function_id_constants::deleted;
				move_constructor = function_id_constants::deleted;
			}
			else if (custom_destructor_declared && !custom_copy_constructor_declared && custom_move_constructor_declared)
			{
				copy_constructor = function_id_constants::deleted;
			}
			else if (!(custom_destructor_declared && custom_copy_constructor_declared && custom_move_constructor_declared))
			{
				return make_syntax_error(incomplete_struct.name,
					"A type must define either none of destructor, copy constructor and move constructor, only destructor, "
					"destructor and move constructor or the three of them. Any other combination is wrong.");
			}
		}

		// Synthesize default copy and move constructors if not provided by the user.
		if (copy_constructor == function_id_constants::invalid)
		{
			if (are_all_copy_constructible(new_struct.member_variables, *program))
			{
				if (!are_all_trivially_copyable(new_struct.member_variables, *program))
				{
					complete::Function copy_constructor_function = instantiation::synthesize_default_copy_constructor(new_type_id, new_struct.member_variables, *program);
					copy_constructor = add_function(*program, std::move(copy_constructor_function));
				}
			}
			else
			{
				copy_constructor = function_id_constants::deleted;
			}
		}
		if (move_constructor == function_id_constants::invalid)
		{
			if (are_all_move_constructible(new_struct.member_variables, *program))
			{
				if (!are_all_trivially_copyable(new_struct.member_variables, *program))
				{
					complete::Function move_constructor_function = instantiation::synthesize_default_move_constructor(new_type_id, new_struct.member_variables, *program);
					move_constructor = add_function(*program, std::move(move_constructor_function));
				}
			}
			else
			{
				move_constructor = function_id_constants::deleted;
			}
		}

		new_struct.constructors = std::move(constructors);
		new_struct.copy_constructor = copy_constructor;
		new_struct.move_constructor = move_constructor;
		new_struct.default_constructor = default_constructor;

		return success;
	}

	auto instantiate_function_template(
		incomplete::Function const & incomplete_function,
		SemanticAnalysisArgs args
	) -> expected<complete::Function, PartialSyntaxError>
	{
		complete::Function function;

		try_call_void(instantiate_function_prototype(incomplete_function, args, out(function)));
		try_call_void(instantiate_function_body(incomplete_function, args, out(function)));

		return std::move(function);
	}

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_,
		SemanticAnalysisArgs args,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<complete::Expression, PartialSyntaxError>
	{
		out<complete::Program> program = args.program;
		ScopeStack & scope_stack = args.scope_stack;

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
			[](incomplete::expression::Literal<char_t> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<char_t>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<null_t> const &) -> expected<complete::Expression, PartialSyntaxError>
			{
				return complete::expression::Literal<null_t>();
			},
			[&](incomplete::expression::Literal<incomplete::TypeId> const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::Literal<complete::TypeId> complete_expression;
				try_call(assign_to(complete_expression.value), resolve_dependent_type(incomplete_expression.value, args));
				return complete_expression;
			},
			[&](incomplete::expression::Identifier const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				auto const lookup = lookup_name(scope_stack, incomplete_expression.name, incomplete_expression.namespaces);

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
					[&](lookup_result::NamespaceNotFound) -> expected<complete::Expression, PartialSyntaxError>
					{
						return make_syntax_error(incomplete_expression.namespaces[0], "Namespace not found.");
					},
					[&](auto const &) -> expected<complete::Expression, PartialSyntaxError>
					{
						return make_syntax_error(incomplete_expression.name, join("Undeclared identifier: ", incomplete_expression.name));
					}
				);
				return std::visit(lookup_visitor, lookup);
			},
			[&](incomplete::expression::IdentifierInsideStruct const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const struct_type,
					resolve_dependent_type(incomplete_expression.type, args));

				complete::Struct const * struct_data = struct_for_type(*program, struct_type);

				if (struct_data == nullptr)
					return make_syntax_error(incomplete_expression.name, "Cannot call a named constructor on a type that is not a struct.");

				std::vector<FunctionId> constructors = constructor_overload_set(*struct_data, incomplete_expression.name);
				if (constructors.empty())
					return make_syntax_error(incomplete_expression.name, "Constructor not found.");

				complete::OverloadSet ctor_overload_set;
				ctor_overload_set.function_ids = std::move(constructors);

				complete::expression::Constant complete_expression;
				complete_expression.type = type_for_overload_set(*program, ctor_overload_set);
				return complete_expression;
			},
			[&](incomplete::expression::MemberVariable const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				complete::expression::MemberVariable complete_expression;
				try_call(assign_to(complete_expression.owner), instantiate_expression(*incomplete_expression.owner, args, current_scope_return_type));

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
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, args, current_scope_return_type));
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
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, args, current_scope_return_type));
				complete::TypeId const operand_type_id = expression_type_id(operand, *program);
				complete::Type const operand_type = type_with_id(*program, operand_type_id);

				if (is_pointer(operand_type))
				{
					complete::TypeId pointee_type = try_get<complete::Type::Pointer>(operand_type.extra_data)->value_type;
					pointee_type.is_reference = true;

					complete::expression::ReinterpretCast complete_expression;
					complete_expression.return_type = pointee_type;
					try_call(assign_to(complete_expression.operand),
						insert_implicit_conversion_node(std::move(operand), operand_type_id, decay(operand_type_id), args, incomplete_expression.operand->source));
					return std::move(complete_expression);
				}
				else
				{
					FunctionId const function = resolve_function_overloading_and_insert_conversions(
						operator_overload_set(Operator::dereference, scope_stack), {&operand, 1}, {&operand_type_id, 1}, *program, args.template_cache);

					if (function == function_id_constants::invalid)
						return make_syntax_error(incomplete_expression_.source, "Overload not found for dereference operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.push_back(std::move(operand));
					return std::move(complete_expression);
				}
			},
			[&](incomplete::expression::Subscript const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Expression array, instantiate_expression(*incomplete_expression.array, args, current_scope_return_type));
				complete::TypeId const array_type_id = expression_type_id(array, *program);
				complete::Type const array_type = type_with_id(*program, array_type_id);

				if (is_array(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::Array>(array_type.extra_data)->value_type;
					value_type.is_reference = array_type_id.is_reference;
					value_type.is_mutable = array_type_id.is_mutable;

					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, args, current_scope_return_type));

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					complete_expression.array = allocate(std::move(array));
					try_call(assign_to(complete_expression.index), 
						insert_implicit_conversion_node(std::move(index), complete::TypeId::int32, args, incomplete_expression.index->source));
					return std::move(complete_expression);
				}
				else if (is_array_pointer(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::ArrayPointer>(array_type.extra_data)->value_type;
					value_type.is_reference = true;

					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, args, current_scope_return_type));

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					try_call(assign_to(complete_expression.array), 
						insert_implicit_conversion_node(std::move(array), decay(array_type_id), args, incomplete_expression.array->source));
					try_call(assign_to(complete_expression.index), 
						insert_implicit_conversion_node(std::move(index), complete::TypeId::int32, args, incomplete_expression.index->source));
					return std::move(complete_expression);
				}
				else
				{
					try_call_decl(complete::Expression index, instantiate_expression(*incomplete_expression.index, args, current_scope_return_type));
					complete::TypeId const index_type_id = expression_type_id(index, *program);

					complete::TypeId const param_types[] = { array_type_id, index_type_id };
					complete::Expression params[] = { std::move(array), std::move(index) };

					FunctionId const function = resolve_function_overloading_and_insert_conversions(*named_overload_set("[]"sv, scope_stack), params, param_types, *program, args.template_cache);

					if (function == function_id_constants::invalid)
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
				try_call_decl(complete::Function complete_function, instantiate_function_template(incomplete_expression.function, args));
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
				new_function_template.scope_template_parameters = args.template_parameters;
				new_function_template.incomplete_function = incomplete_expression.function_template;
				new_function_template.scope_stack = scope_stack;
				new_function_template.parameter_types.reserve(incomplete_expression.function_template.parameters.size());
				for (incomplete::FunctionParameter const & param : incomplete_expression.function_template.parameters)
				{
					try_call(new_function_template.parameter_types.push_back, resolve_function_template_parameter_type(
						param.type,
						incomplete_expression.function_template.template_parameters,
						args
					));
				}

				try_call(assign_to(new_function_template.concepts), resolve_concepts(incomplete_expression.function_template.template_parameters, args));

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
				try_call_void(instantiate_function_prototype(incomplete_extern_function.prototype, args, out(function)));
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

				FunctionId function_id;
				function_id.type = FunctionId::Type::imported;
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
					try_call(parameters.push_back, instantiate_expression(incomplete_param, args, current_scope_return_type));

				complete::TypeId const first_param_type = expression_type_id(parameters[0], *program);
				if (first_param_type.is_function)
				{
					std::vector<complete::TypeId> parameter_types;
					parameter_types.reserve(parameters.size() - 1);
					for (size_t i = 1; i < parameters.size(); ++i)
						parameter_types.push_back(expression_type_id(parameters[i], *program));

					complete::OverloadSetView const overload_set = overload_set_for_type(*program, first_param_type);

					FunctionId const function = 
						resolve_function_overloading_and_insert_conversions(overload_set, {parameters.data() + 1, parameter_types.size()}, parameter_types, *program, args.template_cache);

					if (function == function_id_constants::invalid)
						return make_syntax_error(incomplete_expression.parameters[0].source, "Overload not found.");
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters = std::move(parameters);
					complete_expression.parameters.erase(complete_expression.parameters.begin());
					return std::move(complete_expression);
				}
				else if (first_param_type == complete::TypeId::type)
				{
					auto const constructed_type = 
						interpreter::evaluate_constant_expression_as<complete::TypeId>(parameters[0], args);
					if (!constructed_type.has_value())
						return make_syntax_error(incomplete_expression.parameters[0].source, "Unmet precondition when evaluation constant expression");

					return instantiate_constructor_call(
						constructed_type.value(),
						{parameters.data() + 1, parameters.size() - 1},
						args,
						incomplete_expression_.source
					);
				}
				else
				{
					mark_as_to_do("Overload of operator function call");
				}
			},
			[&](incomplete::expression::UnaryOperatorCall const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::Expression operand, instantiate_expression(*incomplete_expression.operand, args, current_scope_return_type));
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				FunctionId const function = resolve_function_overloading_and_insert_conversions(
					operator_overload_set(incomplete_expression.op, scope_stack), {&operand, 1}, {&operand_type, 1}, *program, args.template_cache);

				if (function == function_id_constants::invalid)
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
				try_call(assign_to(operands[0]), instantiate_expression(*incomplete_expression.left, args, current_scope_return_type));
				try_call(assign_to(operands[1]), instantiate_expression(*incomplete_expression.right, args, current_scope_return_type));

				complete::TypeId const operand_types[] = { expression_type_id(operands[0], *program), expression_type_id(operands[1], *program) };
				FunctionId function = resolve_function_overloading_and_insert_conversions(operator_overload_set(op, scope_stack), operands, operand_types, *program, args.template_cache);

				// Special case for built in assignment.
				if (function == function_id_constants::invalid && op == Operator::assign && is_trivially_copy_constructible(*program, decay(operand_types[0])))
				{
					if (!operand_types[0].is_reference)
						return make_syntax_error(incomplete_expression.left->source, "Cannot assign to a temporary.");
					if (!operand_types[0].is_mutable) 
						return make_syntax_error(incomplete_expression.left->source, "Cannot assign to a constant.");

					complete::expression::Assignment complete_expression;
					complete_expression.destination = allocate(std::move(operands[0]));
					try_call(assign_to(complete_expression.source), 
						insert_implicit_conversion_node(std::move(operands[1]), operand_types[1], decay(operand_types[0]), args, incomplete_expression.right->source));
					return std::move(complete_expression);
				}

				// Special case for pointer comparison
				if (function == function_id_constants::invalid && is_comparison_operator(op))
				{
					if (is_pointer_or_array_pointer(type_with_id(*program, operand_types[0])))
					{
						auto conversion = insert_implicit_conversion_node(std::move(operands[1]), decay(operand_types[0]), args);
						if (conversion.has_value())
						{
							function = (op == Operator::equal || op == Operator::not_equal) ? function_id_constants::pointer_equal_intrinsic : function_id_constants::pointer_three_way_compare_intrinsic;
							try_call(assign_to(operands[0]), insert_implicit_conversion_node(
								std::move(operands[0]), operand_types[0], decay(operand_types[0]), args, incomplete_expression.left->source));
							operands[1] = std::move(*conversion);
						}
					}
					if (function == function_id_constants::invalid && is_pointer_or_array_pointer(type_with_id(*program, operand_types[1])))
					{
						auto conversion = insert_implicit_conversion_node(std::move(operands[0]), decay(operand_types[1]), args);
						if (conversion.has_value())
						{
							function = (op == Operator::equal || op == Operator::not_equal) ? function_id_constants::pointer_equal_intrinsic : function_id_constants::pointer_three_way_compare_intrinsic;
							operands[0] = std::move(*conversion);
							try_call(assign_to(operands[1]), insert_implicit_conversion_node(
								std::move(operands[1]), operand_types[1], decay(operand_types[1]), args, incomplete_expression.right->source));
						}
					}
				}

				// Special case for pointer arithmetic
				if (function == function_id_constants::invalid && op == Operator::add)
				{
					if (is_array_pointer(type_with_id(*program, operand_types[0])))
					{
						auto conversion = insert_implicit_conversion_node(std::move(operands[1]), complete::TypeId::int32, args);
						if (conversion.has_value())
						{
							complete::TypeId const pointer_type = decay(operand_types[0]);
							try_call(assign_to(operands[0]), insert_implicit_conversion_node(
								std::move(operands[0]), operand_types[0], pointer_type, args, incomplete_expression.left->source));

							complete::expression::PointerPlusInt complete_expression;
							complete_expression.pointer = allocate(std::move(operands[0]));
							complete_expression.index = allocate(std::move(*conversion));
							complete_expression.return_type = pointer_type;
							return std::move(complete_expression);
						}
					}
					else if (is_array_pointer(type_with_id(*program, operand_types[1])))
					{
						auto conversion = insert_implicit_conversion_node(std::move(operands[0]), complete::TypeId::int32, args);
						{
							complete::TypeId const pointer_type = decay(operand_types[1]);
							try_call(assign_to(operands[1]), insert_implicit_conversion_node(
								std::move(operands[1]), operand_types[1], pointer_type, args, incomplete_expression.left->source));

							complete::expression::PointerPlusInt complete_expression;
							complete_expression.pointer = allocate(std::move(operands[1]));
							complete_expression.index = allocate(std::move(*conversion));
							complete_expression.return_type = pointer_type;
							return std::move(complete_expression);
						}
					}
				}

				if (function == function_id_constants::invalid && op == Operator::subtract && is_array_pointer(type_with_id(*program, operand_types[0])))
				{
					// Pointer minus pointer
					complete::TypeId const pointer_type = decay(operand_types[0]);
					auto conversion = insert_implicit_conversion_node(std::move(operands[1]), pointer_type, args);
					if (conversion.has_value())
					{
						complete::expression::PointerMinusPointer complete_expression;
						try_call(assign_to(complete_expression.left), insert_implicit_conversion_node(
							std::move(operands[0]), operand_types[0], pointer_type, args, incomplete_expression.left->source));
						complete_expression.right = allocate(std::move(*conversion));
						return std::move(complete_expression);
					}
					// Pointer minus int
					else
					{
						conversion = insert_implicit_conversion_node(std::move(operands[1]), complete::TypeId::int32, args);
						if (conversion.has_value())
						{
							try_call(assign_to(operands[0]), insert_implicit_conversion_node(
								std::move(operands[0]), operand_types[0], pointer_type, args, incomplete_expression.left->source));

							complete::expression::PointerMinusInt complete_expression;
							complete_expression.pointer = allocate(std::move(operands[0]));
							complete_expression.index = allocate(std::move(*conversion));
							complete_expression.return_type = pointer_type;
							return std::move(complete_expression);
						}
					}
				}

				if (function == function_id_constants::invalid)
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
				try_call_decl(complete::Expression condition, instantiate_expression(*incomplete_expression.condition, args, current_scope_return_type));
				try_call(assign_to(condition), insert_implicit_conversion_node(std::move(condition), complete::TypeId::bool_, args, incomplete_expression.condition->source));

				if (is_constant_expression(condition, *program, next_block_scope_offset(scope_stack)))
				{
					auto const condition_value = interpreter::evaluate_constant_expression_as<bool>(condition, args);
					if (!condition_value.has_value())
						return make_syntax_error(incomplete_expression.condition->source, "Unmet precondition at evaluating constant expression.");

					if (condition_value.value())
						return instantiate_expression(*incomplete_expression.then_case, args, current_scope_return_type);
					else
						return instantiate_expression(*incomplete_expression.else_case, args, current_scope_return_type);
				}
				else
				{
					try_call_decl(complete::Expression then_case, instantiate_expression(*incomplete_expression.then_case, args, current_scope_return_type));
					try_call_decl(complete::Expression else_case, instantiate_expression(*incomplete_expression.else_case, args, current_scope_return_type));

					complete::TypeId const then_type = expression_type_id(then_case, *program);
					complete::TypeId const else_type = expression_type_id(else_case, *program);
					complete::TypeId const return_type = common_type(then_type, else_type, *program);

					try_call(assign_to(then_case), insert_implicit_conversion_node(std::move(then_case), then_type, return_type, args, incomplete_expression.then_case->source));
					try_call(assign_to(else_case), insert_implicit_conversion_node(std::move(else_case), else_type, return_type, args, incomplete_expression.else_case->source));

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
					try_call_decl(auto complete_substatement, instantiate_statement(incomplete_substatement, args, out(complete_expression.return_type)));
					if (complete_substatement.has_value())
						complete_expression.statements.push_back(std::move(*complete_substatement));
				}
			
				return std::move(complete_expression);
			},
			[&](incomplete::expression::DesignatedInitializerConstructor const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId const constructed_type_id, instantiate_and_evaluate_type_expression(
					*incomplete_expression.constructed_type, args));

				complete::Type const & constructed_type = type_with_id(*program, constructed_type_id);

				if (!is_struct(constructed_type)) 
					return make_syntax_error(incomplete_expression_.source, "Designated initializers may only be used on structs.");
				if (constructed_type_id.is_reference) 
					return make_syntax_error(incomplete_expression_.source, "Designated initializers cannot be used to initialize a reference.");

				complete::Struct const & constructed_struct = *struct_for_type(*program, constructed_type);

				if (!has_compiler_generated_constructors(constructed_struct))
					return make_syntax_error(incomplete_expression_.source, "Designated initializers cannot be used on structs with user defined constructors.");

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
						instantiate_expression(initializer.assigned_expression, args, current_scope_return_type));

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
			[&](incomplete::expression::Compiles const & incomplete_expression) -> expected<complete::Expression, PartialSyntaxError>
			{
				std::vector<complete::CompilesFakeVariable> complete_fake_variables;
				complete_fake_variables.reserve(incomplete_expression.variables.size());
				for (incomplete::CompilesFakeVariable const & fake_var : incomplete_expression.variables)
				{
					try_call_decl(complete::Expression type_expr, instantiate_expression(fake_var.type, args, current_scope_return_type));
					try_call(assign_to(type_expr), insert_implicit_conversion_node(std::move(type_expr), complete::TypeId::type, args, fake_var.type.source));
					complete_fake_variables.push_back({std::move(type_expr), fake_var.name});
				}

				int const constant_base_index = next_block_scope_offset(scope_stack);
				if (std::all_of(complete_fake_variables, [&](complete::CompilesFakeVariable const & fake_var) 
					{ return is_constant_expression(fake_var.type, *program, constant_base_index); }))
				{
					complete::Scope fake_scope;

					for (size_t i = 0; i < complete_fake_variables.size(); ++i)
					{
						complete::CompilesFakeVariable const & fake_var = complete_fake_variables[i];

						auto const var_type = interpreter::evaluate_constant_expression_as<complete::TypeId>(fake_var.type, args);
						if (!var_type.has_value())
							return make_syntax_error(incomplete_expression.variables[i].type.source, "Unmet precondition at evaluating constant expression.");

						add_variable_to_scope(fake_scope, fake_var.name, var_type.value(), 0, *program);
					}

					auto const guard = push_block_scope(scope_stack, fake_scope);

					bool all_body_expressions_compile = true;
					for (incomplete::ExpressionToTest const & expression_to_test : incomplete_expression.body)
					{
						if (!test_if_expression_compiles(expression_to_test, args, current_scope_return_type))
						{
							all_body_expressions_compile = false;
							break;
						}
					}
					return complete::expression::Literal<bool>{all_body_expressions_compile};
				}
				else
				{
					complete::expression::Compiles complete_expression;
					complete_expression.variables = std::move(complete_fake_variables);
					complete_expression.body = incomplete_expression.body;
					return std::move(complete_expression);
				}
			}
		);

		// If a expression can only be run at compile time, 
		try_call_decl(complete::Expression expression, my::visit(incomplete_expression_.variant, visitor));
		if (is_constant_expression_only(expression, *program, next_block_scope_offset(scope_stack)))
		{
			complete::expression::ConstantTemporary constant;
			constant.type = expression_type_id(expression, *program);
			int const buffer_size = type_size(*program, constant.type);
			constant.value.resize(buffer_size);
			auto const result = interpreter::evaluate_constant_expression(expression, args, constant.value.data());
			if (!result)
				return make_syntax_error(incomplete_expression_.source, "Unmet precondition when evaluation constant expression");
			return std::move(constant);
		}
		else
		{
			return std::move(expression);
		}
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_,
		SemanticAnalysisArgs args,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
	{
		out<complete::Program> program = args.program;
		ScopeStack & scope_stack = args.scope_stack;

		auto const visitor = overload(
			[&](incomplete::statement::LetDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				// Hack for recursive functions
				if (has_type<incomplete::expression::Function>(incomplete_statement.assigned_expression.variant) && incomplete_statement.variable_name != "main")
				{
					incomplete::Function const & incomplete_function = try_get<incomplete::expression::Function>(incomplete_statement.assigned_expression.variant)->function;

					complete::Function function;
					try_call_void(instantiate_function_prototype(incomplete_function, args, out(function)));

					if (does_function_name_collide(scope_stack, incomplete_statement.variable_name)) 
						return make_syntax_error(incomplete_statement.variable_name, "Function name collides with another name.");

					FunctionId const function_id = add_function(*program, function);
					bind_function_name(incomplete_statement.variable_name, function_id, *program, scope_stack);
					function.ABI_name = incomplete_statement.variable_name;

					try_call_void(instantiate_function_body(incomplete_function, args, out(function)));
					program->functions[function_id.index] = std::move(function);

					return std::nullopt;
				}

				try_call_decl(complete::Expression expression,
					instantiate_expression(incomplete_statement.assigned_expression, args, current_scope_return_type));
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				complete::TypeId const var_type = make_mutable(make_reference(assigned_expression_type, incomplete_statement.is_reference), incomplete_statement.is_mutable);

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
					if (main_function.return_type != complete::TypeId::int32) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Main must return int.");
					if (main_function.parameter_count != 0) 
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Main cannot take parameters.");

					// There can only be one main function.
					if (program->main_function != function_id_constants::invalid)
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
				else if (!var_type.is_mutable && is_constant_expression(expression, *program, next_block_scope_offset(scope_stack)) && is_destructible_at_compile_time(*program, var_type))
				{
					complete::Constant constant;
					constant.type = var_type;
					constant.value.resize(type_size(*program, var_type));
					constant.name = incomplete_statement.variable_name;

					try_call(assign_to(expression), 
						insert_implicit_conversion_node(std::move(expression), assigned_expression_type, var_type, args, incomplete_statement.assigned_expression.source));
					auto const eval_result = interpreter::evaluate_constant_expression(expression, args, constant.value.data());
					if (!eval_result.has_value())
						return make_syntax_error(incomplete_statement.assigned_expression.source, "Unmet precondition at evaluating constant expression.");

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
					try_call(assign_to(complete_statement.assigned_expression), 
						insert_implicit_conversion_node(std::move(expression), assigned_expression_type, var_type, args, incomplete_statement.assigned_expression.source));
					return std::move(complete_statement);
				}
			},
			[&](incomplete::statement::PlacementLet const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				try_call_decl(complete::Expression address_expression, instantiate_expression(
					incomplete_statement.address_expression, args, current_scope_return_type));

				try_call_decl(complete::Expression assigned_expression, instantiate_expression(
					incomplete_statement.assigned_expression, args, current_scope_return_type));

				complete::TypeId const assigned_expression_type_id = expression_type_id(assigned_expression, *program);
				if (assigned_expression_type_id.is_function || assigned_expression_type_id == complete::TypeId::void_)
					return make_syntax_error(incomplete_statement.assigned_expression.source, "Assigned expression in placement let statement cannot be a function or void.");

				complete::TypeId const address_expression_type_id = expression_type_id(address_expression, *program);
				complete::Type const & address_expression_type = type_with_id(*program, address_expression_type_id);

				if (!((is_array_pointer(address_expression_type) && pointee_type(address_expression_type) == make_mutable(complete::TypeId::byte)) ||
					(is_pointer(address_expression_type) && pointee_type(address_expression_type) == make_mutable(decay(assigned_expression_type_id)))))
					return make_syntax_error(incomplete_statement.address_expression.source, "Address of placement let statement must be of type byte[] or pointer to assigned type.");

				complete::statement::PlacementLet complete_placement_let;
				try_call(assign_to(complete_placement_let.address_expression), insert_implicit_conversion_node(
					std::move(address_expression), address_expression_type_id, decay(address_expression_type_id), args, incomplete_statement.address_expression.source));
				complete_placement_let.assigned_expression = std::move(assigned_expression);
				return std::move(complete_placement_let);
			},
			[&](incomplete::statement::UninitDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				try_call_decl(complete::TypeId var_type, resolve_dependent_type(incomplete_statement.variable_type, args));
				var_type.is_mutable = true;

				if (does_name_collide(scope_stack, incomplete_statement.variable_name))
					return make_syntax_error(incomplete_statement.variable_name, "Variable name collides with another name.");

				add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

				return std::nullopt;
			},
			[&](incomplete::statement::ExpressionStatement const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::ExpressionStatement complete_statement;
				try_call(assign_to(complete_statement.expression), 
					instantiate_expression(incomplete_statement.expression, args, current_scope_return_type));
				return std::move(complete_statement);
			},
			[&](incomplete::statement::If const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, args, current_scope_return_type));
				try_call(assign_to(condition), insert_implicit_conversion_node(std::move(condition), complete::TypeId::bool_, args, incomplete_statement.condition.source));

				if (is_constant_expression(condition, *program, next_block_scope_offset(scope_stack)))
				{
					auto const condition_value = interpreter::evaluate_constant_expression_as<bool>(condition, args);
					if (!condition_value.has_value())
						return make_syntax_error(incomplete_statement.condition.source, "Unmet precondition at evaluating constant expression.");

					if (condition_value.value())
					{
						try_call_decl(auto then_case, instantiate_statement(*incomplete_statement.then_case, args, current_scope_return_type));
						if (!then_case.has_value())
							return make_syntax_error(incomplete_statement.then_case->source, "Noop statement not allowed as then case of if statement.");

						return std::move(then_case);
					}
					else
					{
						if (incomplete_statement.else_case != nullptr)
						{
							try_call_decl(auto else_case, instantiate_statement(*incomplete_statement.else_case, args, current_scope_return_type));
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

					try_call_decl(auto then_case, instantiate_statement(*incomplete_statement.then_case, args, current_scope_return_type));
					if (!then_case.has_value())
						return make_syntax_error(incomplete_statement.then_case->source, "Noop statement not allowed as then case of if statement.");
					complete_statement.then_case = allocate(std::move(*then_case));
					if (incomplete_statement.else_case != nullptr)
					{
						try_call_decl(auto else_case, instantiate_statement(*incomplete_statement.else_case, args, current_scope_return_type));
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
					try_call_decl(auto complete_substatement, instantiate_statement(incomplete_substatement, args, current_scope_return_type));
					if (complete_substatement.has_value())
						complete_statement.statements.push_back(std::move(*complete_substatement));
				}

				return std::move(complete_statement);
			},
			[&](incomplete::statement::While const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::While complete_statement;

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, args, current_scope_return_type));
				try_call(assign_to(complete_statement.condition), 
					insert_implicit_conversion_node(std::move(condition), complete::TypeId::bool_, args, incomplete_statement.condition.source));

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, args, current_scope_return_type));
				if (!body_statement.has_value())
					return make_syntax_error(incomplete_statement.body->source, "Noop statement not allowed as body of while loop.");
				complete_statement.body = allocate(std::move(*body_statement));

				return std::move(complete_statement);
			},
			[&](incomplete::statement::For const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				complete::statement::For complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);

				try_call_decl(auto init_statement, instantiate_statement(*incomplete_statement.init_statement, args, current_scope_return_type));
				if (!init_statement.has_value()) 
					return make_syntax_error(incomplete_statement.init_statement->source, "Noop statement not allowed as init statement of for loop.");
				complete_statement.init_statement = allocate(std::move(*init_statement));

				try_call_decl(complete::Expression condition, instantiate_expression(incomplete_statement.condition, args, current_scope_return_type));
				try_call(assign_to(complete_statement.condition), 
					insert_implicit_conversion_node(std::move(condition), complete::TypeId::bool_, args, incomplete_statement.condition.source));

				try_call(assign_to(complete_statement.end_expression), 
					instantiate_expression(incomplete_statement.end_expression, args, current_scope_return_type));

				try_call_decl(auto body_statement, instantiate_statement(*incomplete_statement.body, args, current_scope_return_type));
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
				complete_statement.destroyed_stack_frame_size = next_block_scope_offset(scope_stack);
				try_call(assign_to(complete_statement.returned_expression),
					instantiate_expression(incomplete_statement.returned_expression, args, current_scope_return_type));

				complete::TypeId const returned_expression_type = expression_type_id(complete_statement.returned_expression, *program);

				complete::TypeId const return_type = (*current_scope_return_type == complete::TypeId::deduce)
					? decay(returned_expression_type)
					: *current_scope_return_type;

				try_call(assign_to(complete_statement.returned_expression), insert_implicit_conversion_node(std::move(complete_statement.returned_expression), 
						returned_expression_type, return_type, args, incomplete_statement.returned_expression.source));

				if (*current_scope_return_type == complete::TypeId::deduce)
					*current_scope_return_type = return_type;

				return std::move(complete_statement);
			},
			[&](incomplete::statement::Break const &) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				return complete::statement::Break{next_block_scope_offset(scope_stack)};
			},
			[&](incomplete::statement::Continue const &) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				return complete::statement::Continue{next_block_scope_offset(scope_stack)};
			},
			[&](incomplete::statement::StructDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (does_name_collide(scope_stack, incomplete_statement.declared_struct.name))
					return make_syntax_error(incomplete_statement.declared_struct.name, "Struct name collides with another name.");

				try_call_decl(InstantiatedStruct new_struct, instantiate_incomplete_struct_variables(incomplete_statement.declared_struct, args));

				complete::Type new_type;
				new_type.size = new_struct.size;
				new_type.alignment = new_struct.alignment;
				new_type.ABI_name = incomplete_statement.declared_struct.name;
				
				auto const[new_type_id, new_struct_id] = add_struct_type(*program, std::move(new_type), std::move(new_struct.complete_struct));
				bind_type_name(incomplete_statement.declared_struct.name, new_type_id, *program, scope_stack);

				try_call_void(instantiate_incomplete_struct_functions(incomplete_statement.declared_struct, new_type_id, new_struct_id, args));

				return std::nullopt;
			},
			[&](incomplete::statement::StructTemplateDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (does_name_collide(scope_stack, incomplete_statement.declared_struct_template.name)) 
					return make_syntax_error(incomplete_statement.declared_struct_template.name, "Struct template name collides with another name.");

				complete::StructTemplate new_template;
				new_template.incomplete_struct = incomplete_statement.declared_struct_template;
				new_template.scope_template_parameters = args.template_parameters;
				new_template.scope_stack = scope_stack;
				try_call(assign_to(new_template.concepts), resolve_concepts(incomplete_statement.declared_struct_template.template_parameters, args));
				auto const id = add_struct_template(*program, std::move(new_template));
				bind_struct_template_name(incomplete_statement.declared_struct_template.name, id, *program, scope_stack);

				return std::nullopt;
			},
			[&](incomplete::statement::TypeAliasDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (does_name_collide(scope_stack, incomplete_statement.name))
					return make_syntax_error(incomplete_statement.name, "Type alias name collides with another name.");

				try_call_decl(complete::TypeId const type, instantiate_and_evaluate_type_expression(incomplete_statement.type, args));

				bind_type_name(incomplete_statement.name, type, *program, scope_stack);

				return std::nullopt;
			},
			[&](incomplete::statement::NamespaceDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				if (scope_stack.back().type != ScopeType::global)
					return make_syntax_error(incomplete_statement.names[0], "Namespace may only be declared at global or namespace scope.");

				auto * current_namespace = &static_cast<complete::Namespace &>(top(scope_stack));

				// Push all declared nested namespaces.
				for (std::string_view const name : incomplete_statement.names)
				{
					current_namespace = &add_namespace(*current_namespace, name);
					scope_stack.push_back({current_namespace, ScopeType::global, 0});
				}

				for (incomplete::Statement const & incomplete_substatement : incomplete_statement.statements)
				{
					try_call_decl(auto complete_substatement, instantiate_statement(incomplete_substatement, args, current_scope_return_type));
					if (complete_substatement.has_value())
						program->global_initialization_statements.push_back(std::move(*complete_substatement));
				}

				// Pop all namespaces pushed above.
				for (size_t i = 0; i < incomplete_statement.names.size(); ++i)
					scope_stack.pop_back();

				return std::nullopt;
			},
			[&](incomplete::statement::ConversionDeclaration const & incomplete_statement) -> expected<std::optional<complete::Statement>, PartialSyntaxError>
			{
				try_call_decl(complete::Expression function, instantiate_expression(incomplete_statement.conversion_function, args, current_scope_return_type));

				complete::TypeId const function_type = expression_type_id(function, *program);
				if (!function_type.is_function)
					return make_syntax_error(incomplete_statement.conversion_function.source, "A conversion function must be a function.");

				auto const conversion_functions = overload_set_for_type(*program, function_type);

				std::string_view const function_name = incomplete_statement.is_implicit ? "implicit"sv : "conversion"sv;

				for (FunctionId function_id : conversion_functions.function_ids)
				{
					auto const parameter_types = parameter_types_of(*program, function_id);
					if (parameter_types.size() != 1)
						return make_syntax_error(incomplete_statement.conversion_function.source, "A conversion function must take exactly one parameter.");
					if (return_type(*program, function_id) == complete::TypeId::void_)
						return make_syntax_error(incomplete_statement.conversion_function.source, "A conversion function cannot return void.");

					bind_function_name(function_name, function_id, *program, scope_stack);
				}

				for (FunctionTemplateId function_id : conversion_functions.function_template_ids)
				{
					complete::FunctionTemplate & function_template = program->function_templates[function_id.index];
					if (function_template.parameter_types.size() != 1)
						return make_syntax_error(incomplete_statement.conversion_function.source, "A conversion function must take exactly one parameter.");
					if (!function_template.incomplete_function.return_type.has_value())
						return make_syntax_error(incomplete_statement.conversion_function.source, "A conversion function template must define its return type explicitly.");

					try_call(function_template.parameter_types.push_back, resolve_function_template_parameter_type(
						*function_template.incomplete_function.return_type,
						function_template.incomplete_function.template_parameters,
						args
					));
					
					bind_function_template_name(function_name, function_id, *program, scope_stack);
				}

				return std::nullopt;
			}
		);

		return my::visit(incomplete_statement_.variant, visitor);
	}

	[[nodiscard]] auto semantic_analysis(
		span<incomplete::Statement const> incomplete_program, 
		out<complete::Program> complete_program, 
		ScopeStack & scope_stack,
		TemplateCache & template_cache
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
				ScopeState const global_scope_state = capture_state(top(scope_stack));
				TemplateCache const template_cache_state = template_cache;

				auto complete_statement = instantiate_statement(incomplete_program[i], SemanticAnalysisArgs{template_parameters, scope_stack, out(complete_program), template_cache}, nullptr);
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
					restore_state(out(top(scope_stack)), global_scope_state);
					template_cache = template_cache_state;
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

		TemplateCache template_cache;

		return semantic_analysis(incomplete_program, complete_program, scope_stack, template_cache);
	}

	auto push_global_scopes_of_dependent_modules(
		span<incomplete::Module const> incomplete_modules,
		int module_index,
		span<complete::Namespace> module_global_scopes, 
		out<ScopeStack> scope_stack
	) noexcept -> void
	{
		for (int dependency_index : incomplete_modules[module_index].dependencies)
			push_global_scopes_of_dependent_modules(incomplete_modules, dependency_index, module_global_scopes, scope_stack);

		scope_stack->push_back({&module_global_scopes[module_index], ScopeType::global, 0});
	}

	auto semantic_analysis(
		span<incomplete::Module const> incomplete_modules,
		span<int const> parse_order
	) noexcept -> expected<complete::Program, SyntaxError>
	{
		complete::Program program;
		std::vector<complete::Namespace> module_global_scopes(incomplete_modules.size());

		ScopeStack scope_stack;
		scope_stack.push_back({&program.global_scope, ScopeType::global, 0});

		TemplateCache template_cache;

		for (int i : parse_order)
		{
			scope_stack.resize(1);
			push_global_scopes_of_dependent_modules(incomplete_modules, i, module_global_scopes, out(scope_stack));
			auto analysis_result = semantic_analysis(incomplete_modules[i].statements, out(program), scope_stack, template_cache);
			if (!analysis_result)
			{
				if (analysis_result.error().error_in_source.empty())
				{
					return Error(complete_syntax_error(std::move(analysis_result.error()), "", "<source>"));
				}
				else
				{
					for (int j : parse_order)
						if (incomplete::Module::File const * file = file_that_contains(incomplete_modules[j], analysis_result.error().error_in_source))
							return Error(complete_syntax_error(std::move(analysis_result.error()), file->source, file->filename));
					declare_unreachable();
				}
			}
		}

		// Fold all scopes into the program's global scope.
		for (complete::Namespace & module_global_scope : module_global_scopes)
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

			TODO("Merge namespaces with equal nanes");
			for (complete::Namespace & nested_namespace : module_global_scope.nested_namespaces)
				program.global_scope.nested_namespaces.push_back(std::move(nested_namespace));
		}

		return std::move(program);
	}

} // namespace instantiation
