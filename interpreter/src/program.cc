#include "program.hh"
#include "lexer.hh"
#include "parser.hh"
#include "function_ptr.hh"
#include "variant.hh"
#include "parser.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "syntax_error.hh"
#include "map.hh"
#include <queue>
#include <algorithm>
#include <cassert>

using namespace std::literals;

auto built_in_types() noexcept -> std::vector<std::pair<std::string_view, Type>>
{
	return {
		{"void",  {0, 1}},
		{"int",   {4, 4}},
		{"float", {4, 4}},
		{"bool",  {1, 1}},
	};
}

template <typename T> struct index_for_type {};
template <> struct index_for_type<void> { static constexpr unsigned value = 0; };
template <> struct index_for_type<int> { static constexpr unsigned value = 1; };
template <> struct index_for_type<float> { static constexpr unsigned value = 2; };
template <> struct index_for_type<bool> { static constexpr unsigned value = 3; };
template <typename T> constexpr unsigned index_for_type_v = index_for_type<T>::value;

using mpl::BoxedType;
using mpl::box;

template <typename T>
constexpr auto id_for(BoxedType<T>) noexcept -> TypeId
{
	return TypeId::with_index(index_for_type_v<T>);
}
template <typename T>
constexpr auto id_for(BoxedType<T &>) noexcept -> TypeId
{
	return make_mutable(make_reference(TypeId::with_index(index_for_type_v<T>)));
}
template <typename T>
constexpr auto id_for(BoxedType<T const &>) noexcept -> TypeId
{
	return make_reference(TypeId::with_index(index_for_type_v<T>));
}

template <typename R, typename ... Args>
auto extern_function_descriptor(auto (*fn)(Args...) noexcept -> R) noexcept -> ExternFunction
{
	return ExternFunction{
		static_cast<int>(sizeof(std::tuple<Args...>)),
		static_cast<int>(alignof(std::tuple<Args...>)),
		id_for(box<R>),
		{id_for(box<Args>)...},
		callc::c_function_caller(fn), 
		fn
	};
}

auto default_extern_functions() noexcept -> std::vector<std::pair<std::string_view, ExternFunction>>
{
	return {
		{"+"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a + b; })},
		{"-"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a - b; })},
		{"*"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a * b; })},
		{"/"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a / b; })},
		{"%"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a % b; })},
		{"=="sv,	extern_function_descriptor(+[](int a, int b) noexcept -> bool { return a == b; })},
		{"<=>"sv,	extern_function_descriptor(+[](int a, int b) noexcept -> int { return a - b; })},
		{"-"sv,		extern_function_descriptor(+[](int a) noexcept -> int { return -a; })},

		{"+"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a + b; })},
		{"-"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"*"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a * b; })},
		{"/"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a / b; })},
		{"=="sv,	extern_function_descriptor(+[](float a, float b) noexcept -> bool { return a == b; })},
		{"<=>"sv,	extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"-"sv,		extern_function_descriptor(+[](float a) noexcept -> float { return -a; })},

		{"and"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a && b; })},
		{"or"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a || b; })},
		{"xor"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a != b; })},
		{"not"sv,	extern_function_descriptor(+[](bool a) noexcept -> bool { return !a; })},
		{"=="sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a == b; })},
	};
}

Program::Program()
{
	auto const built_in_types_to_add = built_in_types();

	types.reserve(built_in_types_to_add.size());
	global_scope.types.reserve(built_in_types_to_add.size());

	for (auto const type : built_in_types_to_add)
	{
		global_scope.types.push_back({pool_string(*this, type.first), {false, false, false, static_cast<unsigned>(types.size())}});
		types.push_back(type.second);
	}

	//*******************************************************************

	auto const extern_functions_to_add = default_extern_functions();

	extern_functions.reserve(extern_functions_to_add.size());
	global_scope.functions.reserve(extern_functions_to_add.size());

	for (auto const fn : extern_functions_to_add)
	{
		global_scope.functions.push_back({pool_string(*this, fn.first), {true, static_cast<unsigned>(extern_functions.size())}});
		extern_functions.push_back(fn.second);
	}
}

auto check_type_validness_as_overload_candidate(TypeId param_type, TypeId parsed_type, Program const & program, int & conversions) noexcept -> bool
{
	if (param_type == parsed_type)
		return true;

	if (is_convertible(parsed_type, param_type, program))
	{
		conversions++;
		return true;
	}
	else
	{
		return false;
	}
}

auto resolve_dependent_type(span<TypeId const> template_parameters, DependentTypeId dependent_type, Program & program) -> TypeId
{
	auto const visitor = overload(
		[&](DependentTypeId::BaseCase const & base_case)
		{
			if (base_case.is_dependent)
			{
				return template_parameters[base_case.index];
			}
			else
			{
				TypeId type;
				type.index = base_case.index;
				type.is_language_reseved = base_case.is_language_reserved;
				return type;
			}
		},
		[&](DependentTypeId::Pointer const & pointer)
		{
			TypeId const pointee = resolve_dependent_type(template_parameters, *pointer.pointee, program);
			return pointer_type_for(pointee, program);
		},
		[](DependentTypeId::Array const & /*array*/) -> TypeId
		{
			mark_as_to_do("Dependent array types");
		},
		[](DependentTypeId::Template const & /*template_instantiation*/) -> TypeId
		{
			mark_as_to_do("Dependent template instantiations");
		}
	);

	TypeId type = std::visit(visitor, dependent_type.value);
	type.is_reference = dependent_type.is_reference;
	type.is_mutable = dependent_type.is_mutable;
	return type;
}

// Checks if a type satisfies a pattern, and resolves missing dependent types.
auto expected_type_according_to_pattern(TypeId given_parameter, DependentTypeId expected_pattern, span<TypeId> resolved_dependent_types, Program & program) -> TypeId
{
	auto const visitor = overload(
		[&](DependentTypeId::BaseCase const & base_case)
		{
			TypeId const expected_type = decay(given_parameter);
			if (resolved_dependent_types[base_case.index] == TypeId::none)
			{
				resolved_dependent_types[base_case.index] = expected_type;
				return expected_type;
			}
			else if (resolved_dependent_types[base_case.index] == expected_type)
				return expected_type;
			else
				return TypeId::none;
		},
		[&](DependentTypeId::Pointer const & pointer)
		{
			Type const & type = type_with_id(program, given_parameter);

			if (!is_pointer(type))
				return TypeId::none; // Does not satisfy the pattern

			TypeId const pointee = pointee_type(type);
			TypeId const expected_pointee = expected_type_according_to_pattern(pointee, *pointer.pointee, resolved_dependent_types, program);

			if (expected_pointee == TypeId::none)
				return TypeId::none;
			else
				return pointer_type_for(expected_pointee, program);
		},
		[](DependentTypeId::Array const & /*array*/) -> TypeId
		{
			mark_as_to_do("Dependent array types");
		},
		[](DependentTypeId::Template const & /*template_instantiation*/) -> TypeId
		{
			mark_as_to_do("Dependent template instantiations");
		}
	);

	TypeId expected_type = std::visit(visitor, expected_pattern.value);
	expected_type.is_reference = expected_pattern.is_reference;
	expected_type.is_mutable = expected_pattern.is_mutable;
	return expected_type;
}

auto resolve_function_overloading(OverloadSet overload_set, span<TypeId const> parameters, Program & program) noexcept -> FunctionId
{
	struct Candidate
	{
		int conversions;
		FunctionId function_id;
	};
	Candidate candidates[64];
	int candidate_count = 0;

	struct TemplateCandidate
	{
		int conversions;
		FunctionTemplateId id;
	};
	TemplateCandidate template_candidates[64];
	int template_candidate_count = 0;

	for (FunctionId function_id : overload_set.function_ids)
	{
		auto const param_types = parameter_types(program, function_id);
		if (param_types.size() == parameters.size())
		{
			int conversions = 0;
			bool discard = false;
			for (size_t i = 0; i < param_types.size(); ++i)
			{
				if (!check_type_validness_as_overload_candidate(param_types[i], parameters[i], program, conversions))
				{
					discard = true;
					break;
				}
			}

			if (!discard)
			{
				candidates[candidate_count++] = Candidate{conversions, function_id};
			}
		}
	}

	TypeId resolved_dependent_types[32];
	size_t dependent_type_count = 0;

	for (FunctionTemplateId template_id : overload_set.function_template_ids)
	{
		FunctionTemplate const & fn = program.function_templates[template_id.index];
		span<FunctionTemplate::Parameter const> template_params = fn.parameters;
		if (template_params.size() == parameters.size())
		{
			dependent_type_count = fn.template_parameter_count;
			std::fill(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none);

			int conversions = 0;
			bool discard = false;
			for (size_t i = 0; i < template_params.size(); ++i)
			{
				if (!template_params[i].is_dependent)
				{
					if (!check_type_validness_as_overload_candidate(fn.scope.variables[template_params[i].index].type, parameters[i], program, conversions))
					{
						discard = true;
						break;
					}
				}
				else
				{
					DependentTypeId const dependent_type = fn.scope.dependent_variables[template_params[i].index].type;
					TypeId const expected_type = expected_type_according_to_pattern(parameters[i], dependent_type, resolved_dependent_types, program);
					if (expected_type == TypeId::none)
					{
						discard = true;
						break;
					}

					if (!check_type_validness_as_overload_candidate(expected_type, parameters[i], program, conversions))
					{
						discard = true;
						break;
					}
				}
			}

			if (!discard)
			{
				template_candidates[template_candidate_count++] = TemplateCandidate{conversions, template_id};
			}
		}
	}

	if (candidate_count == 0 && template_candidate_count == 0)
	{
		return invalid_function_id;
	}
	else
	{
		Candidate best_candidate;
		TemplateCandidate best_template_candidate;

		if (candidate_count > 1)
		{
			std::partial_sort(candidates, candidates + 2, candidates + candidate_count, [](Candidate a, Candidate b) { return a.conversions < b.conversions; });
			assert(candidates[0].conversions < candidates[1].conversions); // Ambiguous call.
		}
		best_candidate = candidates[0];

		if (template_candidate_count > 1)
		{
			std::partial_sort(template_candidates, template_candidates + 2, template_candidates + template_candidate_count, 
				[](TemplateCandidate a, TemplateCandidate b) { return a.conversions < b.conversions; });
			assert(template_candidates[0].conversions < template_candidates[1].conversions); // Ambiguous call.
		}
		best_template_candidate = template_candidates[0];

		if (template_candidate_count == 0 || candidate_count > 0 && best_candidate.conversions < best_template_candidate.conversions)
			return best_candidate.function_id;
		else
		{
			// Ensure that all template parameters have been resolved.
			assert(std::find(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none) == resolved_dependent_types + dependent_type_count);
			return instantiate_function_template(program, best_template_candidate.id, {resolved_dependent_types, dependent_type_count});
		}
	}
}

auto insert_conversion_node(expr::ExpressionTree tree, TypeId from, TypeId to, Program const & program) noexcept -> expr::ExpressionTree
{
	if (from.index == to.index)
	{
		// From reference to reference and value to value there is no conversion. A pointer is a pointer, regardless of constness.
		if (from.is_reference == to.is_reference)
			return tree;

		if (from.is_reference && !to.is_reference)
		{
			expr::DereferenceNode deref_node;
			deref_node.expression = std::make_unique<expr::ExpressionTree>(std::move(tree));
			deref_node.variable_type = to;
			return deref_node;
		}
		else
		{
			raise_syntax_error_if_not(!to.is_mutable, "Can't bind a temporary to a mutable reference.");
			mark_as_to_do("Address of temporaries");
		}
	}
	else if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) && !from.is_reference && !to.is_reference)
	{
		return std::move(tree);
	}
	raise_syntax_error("Conversion between types does not exist.");
}

auto insert_conversion_to_control_flow_condition(expr::ExpressionTree tree, Program const & program) noexcept -> expr::ExpressionTree
{
	TypeId const condition_expr_type = expression_type_id(tree, program);
	raise_syntax_error_if_not(is_convertible(condition_expr_type, TypeId::bool_, program), "Condition expression must return bool.");
	return insert_conversion_node(std::move(tree), condition_expr_type, TypeId::bool_, program);
}

auto insert_conversions(
	span<expr::ExpressionTree> parameters, 
	span<TypeId const> parsed_parameter_types, 
	span<TypeId const> target_parameter_types, 
	Program const & program) noexcept -> void
{
	for (size_t i = 0; i < target_parameter_types.size(); ++i)
	{
		if (parsed_parameter_types[i] != target_parameter_types[i])
			parameters[i] = insert_conversion_node(std::move(parameters[i]), parsed_parameter_types[i], target_parameter_types[i], program);
	}
}

auto resolve_function_overloading_and_insert_conversions(
	OverloadSet overload_set,
	span<expr::ExpressionTree> parameters,
	span<TypeId const> parsed_parameter_types,
	Program & program) noexcept -> FunctionId
{
	FunctionId const function_id = resolve_function_overloading(overload_set, parsed_parameter_types, program);
	if (function_id == invalid_function_id)
		return invalid_function_id;

	// If any conversion is needed in order to call the function, perform the conversion.
	auto const target_parameter_types = parameter_types(program, function_id);
	insert_conversions(parameters, parsed_parameter_types, target_parameter_types, program);

	return function_id;
}

auto is_struct(Type const & type) noexcept -> bool
{
	return has_type<StructType>(type.extra_data);
}

auto struct_for_type(Program const & program, Type const & type) noexcept -> Struct const *
{
	if (auto const s = try_get<StructType>(type.extra_data))
		return &program.structs[s->struct_index];
	else
		return nullptr;
}

auto struct_for_type(Program const & program, TypeId type) noexcept -> Struct const *
{
	return struct_for_type(program, type_with_id(program, type));
}

auto find_member_variable(Struct const & type, std::string_view member_name, span<char const> string_pool) noexcept -> int
{
	auto const it = std::find_if(type.member_variables.begin(), type.member_variables.end(), pooled_name_equal(string_pool, member_name));
	if (it == type.member_variables.end())
		return -1;
	else
		return static_cast<int>(it - type.member_variables.begin());
}

auto is_pointer(Type const & type) noexcept -> bool
{
	return has_type<PointerType>(type.extra_data);
}

auto pointer_type_for(TypeId pointee_type, Program & program) noexcept -> TypeId
{
	assert(!pointee_type.is_reference); // A pointer can't point at a reference.

	// If a pointer type for this type has already been created, return that.
	for (Type const & type : program.types)
		if (auto const pointer = try_get<PointerType>(type.extra_data))
			if (pointer->value_type == pointee_type)
				return TypeId::with_index(static_cast<unsigned>(&type - program.types.data()));

	// Otherwise create one.
	Type new_type;
	new_type.size = sizeof(void *);
	new_type.alignment = alignof(void *);
	new_type.extra_data = PointerType{pointee_type};
	return add_type(program, std::move(new_type));
}

auto pointee_type(Type const & pointer_type) noexcept->TypeId
{
	assert(is_pointer(pointer_type));
	return try_get<PointerType>(pointer_type.extra_data)->value_type;
}

auto pointee_type(TypeId pointer_type_id, Program const & program) noexcept -> TypeId
{
	return pointee_type(type_with_id(program, pointer_type_id));
}

auto add_type(Program & program, Type new_type) noexcept -> TypeId
{
	unsigned const type_index = static_cast<unsigned>(program.types.size());
	program.types.push_back(std::move(new_type));
	return TypeId::with_index(type_index);
}

auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &
{
	assert(!id.is_language_reseved);
	return program.types[id.index];
}

auto type_size(Program const & program, TypeId id) noexcept -> int
{
	if (id.is_reference)
		return sizeof(void *);
	else
		return type_with_id(program, id).size;
}

auto is_default_constructible(Struct const & type) noexcept -> bool
{
	return std::all_of(type.member_variables.begin(), type.member_variables.end(),
		[](MemberVariable const & var) { return var.initializer_expression.has_value(); });
}

auto is_default_constructible(TypeId type_id, Program const & program) noexcept -> bool
{
	if (type_id.is_language_reseved)
		return false;

	if (Struct const * const struct_data = struct_for_type(program, type_id))
		return is_default_constructible(*struct_data);
	else
		return false;
}

auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> expr::StructConstructorNode
{
	expr::StructConstructorNode default_constructor_node;
	default_constructor_node.constructed_type = type_id;
	default_constructor_node.parameters.reserve(struct_data.member_variables.size());
	for (MemberVariable const & var : struct_data.member_variables)
		default_constructor_node.parameters.push_back(*var.initializer_expression);
	return default_constructor_node;
}

auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>
{
	if (id.is_extern)
	{
		return program.extern_functions[id.index].parameter_types;
	}
	else
	{
		Function const & fn = program.functions[id.index];
		std::vector<TypeId> types(fn.parameter_count);
		for (int i = 0; i < fn.parameter_count; ++i)
			types[i] = fn.variables[i].type;
		return types;
	}
}

auto return_type(Program const & program, FunctionId id) noexcept -> TypeId
{
	if (id.is_extern)
		return program.extern_functions[id.index].return_type;
	else
		return program.functions[id.index].return_type;
}

struct RemappedVariableOffset
{
	int prev_offset;
	int new_offset;
};
auto remap_offset(span<RemappedVariableOffset const> variable_offset_map, int offset) noexcept -> int
{
	auto const it = std::find_if(variable_offset_map.begin(), variable_offset_map.end(), [offset](RemappedVariableOffset r)
	{
		return r.prev_offset == offset;
	});
	assert(it != variable_offset_map.end());
	return it->new_offset;
}

auto find_variable_in_stack(span<span<Variable const> const> variable_stack, PooledString name) noexcept -> Variable const &
{
	for (span<Variable const> vars : variable_stack)
		for (Variable const & var : vars)
			if (var.name.first == name.first && var.name.size == name.size)
				return var;
	declare_unreachable();
}

auto instantiate_dependent_scope(DependentScope const & scope_template, span<TypeId const> parameters, Program & program, std::vector<RemappedVariableOffset> & variable_offset_map) noexcept -> Scope
{
	Scope instantiated_scope;
	instantiated_scope.functions = scope_template.functions;
	instantiated_scope.types = scope_template.types;
	instantiated_scope.function_templates = scope_template.function_templates;
	instantiated_scope.struct_templates = scope_template.struct_templates;
	instantiated_scope.variables.reserve(scope_template.variables.size() + scope_template.dependent_variables.size());

	for (Variable const & var : scope_template.variables)
	{
		int const new_offset = add_variable_to_scope(instantiated_scope, var.name, var.type, 0, program);
		variable_offset_map.push_back({var.offset, new_offset});
	}

	for (DependentVariable const & var : scope_template.dependent_variables)
	{
		TypeId var_type = resolve_dependent_type(parameters, var.type, program);
		add_variable_to_scope(instantiated_scope, var.name, var_type, 0, program);
	}

	return instantiated_scope;
}

auto instantiate_dependent_statement(
	stmt::Statement const & statement,
	TypeId & return_type,
	Program & program,
	span<TypeId const> template_parameters,
	std::vector<RemappedVariableOffset> & variable_offset_map,
	std::vector<span<Variable const>> & variable_stack) noexcept -> stmt::Statement;

auto instantiate_dependent_expression(
	expr::ExpressionTree const & tree,
	TypeId & return_type,
	Program & program,
	span<TypeId const> template_parameters,
	std::vector<RemappedVariableOffset> & variable_offset_map,
	std::vector<span<Variable const>> & variable_stack) noexcept -> expr::ExpressionTree
{
	using namespace expr;

	auto const visitor = overload(
		[](auto const & non_dependent) -> expr::ExpressionTree { return non_dependent; },
		[&](tmp::LocalVariableNode const & var_node) -> ExpressionTree
		{
			// TODO: Index based approach?
			Variable const & var = find_variable_in_stack(variable_stack, var_node.name);

			LocalVariableNode instantiated_node;
			instantiated_node.variable_type = var.type;
			instantiated_node.variable_offset = var.offset;
			return instantiated_node;
		},
		[&](tmp::MemberVariableNode const & var_node) -> ExpressionTree
		{
			MemberVariableNode instantiated_node;
			instantiated_node.owner = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*var_node.owner, return_type, program, template_parameters, variable_offset_map, variable_stack));
			TypeId const owner_type_id = expression_type_id(*instantiated_node.owner, program);
			Type const & owner_type = type_with_id(program, owner_type_id);
			raise_syntax_error_if_not(is_struct(owner_type), "Attempted to access member of non-struct type.");
			Struct const & owner_struct = *struct_for_type(program, owner_type);

			std::string_view const var_name = get(program, var_node.name);
			int const member_variable_index = find_member_variable(owner_struct, var_name, program.string_pool);
			raise_syntax_error_if_not(member_variable_index != -1, "Expected member name after '.'.");
			Variable const & member_variable = owner_struct.member_variables[member_variable_index];

			instantiated_node.variable_type = member_variable.type;
			instantiated_node.variable_offset = member_variable.offset;
			return instantiated_node;
		},
		[&](LocalVariableNode const & var_node) -> ExpressionTree
		{
			LocalVariableNode instantiated_node;
			instantiated_node.variable_type = var_node.variable_type;
			instantiated_node.variable_offset = remap_offset(variable_offset_map, var_node.variable_offset);
			return instantiated_node;
		},
		[&](DereferenceNode const & deref_node) -> ExpressionTree
		{
			DereferenceNode instantiated_node;
			instantiated_node.expression = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*deref_node.expression, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.variable_type = remove_reference(expression_type_id(*instantiated_node.expression, program));
			return instantiated_node;
		},
		[&](AddressofNode const & addressof_node) -> ExpressionTree
		{
			AddressofNode instantiated_node;
			instantiated_node.operand = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*addressof_node.operand, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.return_type = pointer_type_for(remove_reference(expression_type_id(*instantiated_node.operand, program)), program);
			return instantiated_node;
		},
		[&](DepointerNode const & deptr_node) -> ExpressionTree
		{
			DepointerNode instantiated_node;
			instantiated_node.operand = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*deptr_node.operand, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.return_type = make_reference(std::get<PointerType>(type_with_id(program, expression_type_id(*instantiated_node.operand, program)).extra_data).value_type);
			return instantiated_node;
		},
		[&](tmp::FunctionCallNode const & fn_call_node) -> ExpressionTree
		{
			FunctionCallNode instantiated_node;

			instantiated_node.parameters.reserve(fn_call_node.parameters.size());
			for (ExpressionTree const & param : fn_call_node.parameters)
				instantiated_node.parameters.push_back(instantiate_dependent_expression(param, return_type, program, template_parameters, variable_offset_map, variable_stack));

			std::vector<TypeId> const  parameter_types = map(instantiated_node.parameters, expr::expression_type_id(program));
			instantiated_node.function_id = resolve_function_overloading_and_insert_conversions(
				fn_call_node.overload_set, 
				instantiated_node.parameters, 
				parameter_types, 
				program);
			raise_syntax_error_if_not(instantiated_node.function_id != invalid_function_id, "Function overload not found in template instantiation.");

			return instantiated_node;
		},
		[&](tmp::RelationalOperatorCallNode const & fn_call_node) -> ExpressionTree
		{
			RelationalOperatorCallNode instantiated_node;

			instantiated_node.parameters = std::make_unique<std::array<expr::ExpressionTree, 2>>();
			(*instantiated_node.parameters)[0] = instantiate_dependent_expression((*fn_call_node.parameters)[0], return_type, program, template_parameters, variable_offset_map, variable_stack);
			(*instantiated_node.parameters)[1] = instantiate_dependent_expression((*fn_call_node.parameters)[1], return_type, program, template_parameters, variable_offset_map, variable_stack);

			instantiated_node.op = fn_call_node.op;

			TypeId const parameter_types[2] = {expression_type_id((*instantiated_node.parameters)[0], program), expression_type_id((*instantiated_node.parameters)[1], program)};
			instantiated_node.function_id = resolve_function_overloading_and_insert_conversions(
				fn_call_node.overload_set,
				*instantiated_node.parameters,
				parameter_types,
				program);
			raise_syntax_error_if_not(instantiated_node.function_id != invalid_function_id, "Function overload not found in template instantiation.");

			return instantiated_node;
		},
		[&](tmp::DereferenceNode const & deref_node) -> ExpressionTree
		{
			ExpressionTree operand = instantiate_dependent_expression(*deref_node.operand, return_type, program, template_parameters, variable_offset_map, variable_stack);
			TypeId const operand_type = expression_type_id(operand, program);
			if (is_pointer(type_with_id(program, operand_type)))
			{
				expr::DepointerNode instantiated_node;
				instantiated_node.return_type = make_reference(std::get<PointerType>(type_with_id(program, operand_type).extra_data).value_type);
				instantiated_node.operand = std::make_unique<ExpressionTree>(insert_conversion_node(std::move(operand), operand_type, remove_reference(operand_type), program));

				return instantiated_node;
			}
			else
			{
				FunctionCallNode instantiated_node;
				instantiated_node.parameters.push_back(std::move(operand));

				instantiated_node.function_id = resolve_function_overloading_and_insert_conversions(
					deref_node.overload_set,
					instantiated_node.parameters,
					span<TypeId const>{&operand_type, 1},
					program);

				raise_syntax_error_if_not(instantiated_node.function_id != invalid_function_id, "Function overload not found in template instantiation.");

				return instantiated_node;
			}
		},
		[&](tmp::StructConstructorNode const & ctor_node) -> ExpressionTree
		{
			TypeId constructed_type = template_parameters[ctor_node.type.index];
			constructed_type.is_mutable = false;
			constructed_type.is_reference = false;

			Type const & type = type_with_id(program, constructed_type);
			raise_syntax_error_if_not(is_struct(type), "Constructor call syntax is only available for structs (for now).");
			Struct const & struct_data = *struct_for_type(program, type);
			auto const struct_member_types = map(struct_data.member_variables, &Variable::type);

			if (ctor_node.parameters.empty())
			{
				raise_syntax_error_if_not(is_default_constructible(struct_data), "Attempted to default construct type that is not default constructible.");
				return synthesize_default_constructor(constructed_type, struct_data);
			}

			raise_syntax_error_if_not(struct_data.member_variables.size() == ctor_node.parameters.size(), "Incorrect number of parameters in struct constructor.");

			StructConstructorNode instantiated_node;
			instantiated_node.constructed_type = constructed_type;
			instantiated_node.parameters.resize(ctor_node.parameters.size());

			for (size_t i = 0; i < ctor_node.parameters.size(); ++i)
			{
				ExpressionTree param = instantiate_dependent_expression(ctor_node.parameters[i], return_type, program, template_parameters, variable_offset_map, variable_stack);
				TypeId const param_type = expression_type_id(param, program);
				raise_syntax_error_if_not(is_convertible(param_type, struct_member_types[i], program), "Expression is not convertible to member type in constructor.");
				instantiated_node.parameters[i] = insert_conversion_node(std::move(param), param_type, struct_member_types[i], program);
			}
			return instantiated_node;
		},
		[&](AssignmentNode const & assign_node) -> ExpressionTree
		{
			AssignmentNode instantiated_node;
			instantiated_node.source = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*assign_node.source, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.destination = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*assign_node.destination, return_type, program, template_parameters, variable_offset_map, variable_stack));

			TypeId const source_type = expression_type_id(*instantiated_node.source, program);
			TypeId const dest_type = expression_type_id(*instantiated_node.destination, program);
			raise_syntax_error_if_not(
				decay(source_type) == decay(dest_type) && dest_type.is_reference && dest_type.is_reference,
				"Could not instantiate assignment operator."
			);
			*instantiated_node.source = insert_conversion_node(*std::move(instantiated_node.source), source_type, decay(source_type), program);
			return instantiated_node;
		},
		[&](IfNode const & if_node) -> ExpressionTree
		{
			IfNode instantiated_node;

			instantiated_node.condition = std::make_unique<ExpressionTree>(insert_conversion_to_control_flow_condition(
				instantiate_dependent_expression(*if_node.condition, return_type, program, template_parameters, variable_offset_map, variable_stack),
				program));

			instantiated_node.then_case = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*if_node.then_case, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.else_case = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*if_node.else_case, return_type, program, template_parameters, variable_offset_map, variable_stack));

			// Find the common type to ensure that both branches return the same type.
			if (is_dependent(*if_node.then_case) || is_dependent(*if_node.else_case))
			{
				TypeId const then_type = expression_type_id(*instantiated_node.then_case, program);
				TypeId const else_type = expression_type_id(*instantiated_node.else_case, program);
				TypeId const common = common_type(then_type, else_type, program);
				raise_syntax_error_if_not(common != TypeId::none, "Could not find common type for return types of then and else branches in if expression.");
				if (then_type != common)
					*instantiated_node.then_case = insert_conversion_node(std::move(*instantiated_node.then_case), then_type, common, program);
				if (else_type != common)
					*instantiated_node.else_case = insert_conversion_node(std::move(*instantiated_node.else_case), else_type, common, program);
			}

			return instantiated_node;
		},
		[](StatementBlockNode const & block_node) -> ExpressionTree { return block_node; },
		[&](tmp::StatementBlockNode const & block_node) -> ExpressionTree 
		{
			StatementBlockNode instantiated_node;
			instantiated_node.return_type = TypeId::deduce;

			size_t const prev_offset_map_size = variable_offset_map.size();
			instantiated_node.scope = instantiate_dependent_scope(block_node.scope, template_parameters, program, variable_offset_map);

			variable_stack.push_back(instantiated_node.scope.variables);

			instantiated_node.statements.reserve(block_node.statements.size());
			for (stmt::Statement const & statement_template : block_node.statements)
				instantiated_node.statements.push_back(
					instantiate_dependent_statement(statement_template, instantiated_node.return_type, program, template_parameters, variable_offset_map, variable_stack));

			variable_stack.pop_back();
			variable_offset_map.resize(prev_offset_map_size);

			return instantiated_node;
		},
		[&](StructConstructorNode const & ctor_node) -> ExpressionTree
		{
			StructConstructorNode instantiated_node;
			instantiated_node.constructed_type = ctor_node.constructed_type;

			Struct const & struct_data = *struct_for_type(program, ctor_node.constructed_type);

			instantiated_node.parameters.reserve(ctor_node.parameters.size());
			for (size_t i = 0; i < ctor_node.parameters.size(); ++i)
			{
				expr::ExpressionTree const & param = ctor_node.parameters[i];
				expr::ExpressionTree instantiated_param = instantiate_dependent_expression(param, return_type, program, template_parameters, variable_offset_map, variable_stack);
				TypeId const param_type = expression_type_id(instantiated_param, program);
				instantiated_param = insert_conversion_node(std::move(instantiated_param), param_type, struct_data.member_variables[i].type, program);
				instantiated_node.parameters.push_back(std::move(instantiated_param));
			}

			return instantiated_node;
		}
	);

	return std::visit(visitor, tree.as_variant());
}

auto instantiate_dependent_statement(
	stmt::Statement const & statement, 
	TypeId & return_type,
	Program & program,
	span<TypeId const> template_parameters,
	std::vector<RemappedVariableOffset> & variable_offset_map,
	std::vector<span<Variable const>> & variable_stack) noexcept -> stmt::Statement
{
	using namespace stmt;

	auto const visitor = overload(
		[&](VariableDeclarationStatement const & var_decl_node) -> Statement
		{
			VariableDeclarationStatement instantiated_node;
			instantiated_node.variable_offset = remap_offset(variable_offset_map, var_decl_node.variable_offset);
			instantiated_node.assigned_expression = instantiate_dependent_expression(var_decl_node.assigned_expression, return_type, program, template_parameters, variable_offset_map, variable_stack);
			return instantiated_node;
		},
		[&](tmp::VariableDeclarationStatement const & var_decl_node) -> Statement
		{
			VariableDeclarationStatement instantiated_node;
			Variable const & var = find_variable_in_stack(variable_stack, var_decl_node.variable_name);

			instantiated_node.variable_offset = var.offset;
			if (var_decl_node.assigned_expression)
			{
				instantiated_node.assigned_expression = instantiate_dependent_expression(*var_decl_node.assigned_expression, return_type, program, template_parameters, variable_offset_map, variable_stack);
				TypeId const assigned_expr_type = expression_type_id(instantiated_node.assigned_expression, program);
				instantiated_node.assigned_expression = insert_conversion_node(
					std::move(instantiated_node.assigned_expression),
					assigned_expr_type, var.type, program);
			}
			else
			{
				raise_syntax_error_if_not(is_default_constructible(var.type, program), "Attempted to default construct a variable of a type that is not default constructible.");
				instantiated_node.assigned_expression = synthesize_default_constructor(var.type, *struct_for_type(program, var.type));
			}

			return instantiated_node;
		},
		[&](ExpressionStatement const & expr_node) -> Statement
		{
			ExpressionStatement instantiated_node;
			instantiated_node.expression = instantiate_dependent_expression(expr_node.expression, return_type, program, template_parameters, variable_offset_map, variable_stack);
			return instantiated_node;
		},
		[&](ReturnStatement const & return_node) -> Statement
		{
			ReturnStatement instantiated_node;
			instantiated_node.returned_expression = instantiate_dependent_expression(return_node.returned_expression, return_type, program, template_parameters, variable_offset_map, variable_stack);

			TypeId const returned_expression_type = expression_type_id(instantiated_node.returned_expression, program);
			if (return_type == TypeId::deduce)
				return_type = decay(expression_type_id(instantiated_node.returned_expression, program));

			if (returned_expression_type != return_type)
				instantiated_node.returned_expression = 
				insert_conversion_node(
					std::move(instantiated_node.returned_expression),
					returned_expression_type, return_type,
					program);

			return instantiated_node;
		},
		[&](IfStatement const & if_node) -> Statement
		{
			IfStatement instantiated_node;
			instantiated_node.condition = insert_conversion_to_control_flow_condition(instantiate_dependent_expression(if_node.condition, return_type, program, template_parameters, variable_offset_map, variable_stack), program);
			instantiated_node.then_case = std::make_unique<Statement>(instantiate_dependent_statement(*if_node.then_case, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.else_case = std::make_unique<Statement>(instantiate_dependent_statement(*if_node.else_case, return_type, program, template_parameters, variable_offset_map, variable_stack));
			return instantiated_node;
		},
		[&](StatementBlock const & block_node) -> Statement
		{
			(void)block_node;
			mark_as_to_do("Dependent statement block");
		},
		[&](WhileStatement const & while_node) -> Statement
		{
			WhileStatement instantiated_node;
			instantiated_node.condition = insert_conversion_to_control_flow_condition(instantiate_dependent_expression(while_node.condition, return_type, program, template_parameters, variable_offset_map, variable_stack), program);
			instantiated_node.body = std::make_unique<Statement>(instantiate_dependent_statement(*while_node.body, return_type, program, template_parameters, variable_offset_map, variable_stack));
			return instantiated_node;
		},
		[&](ForStatement const & for_node) -> Statement
		{
			return for_node;
		},
		[&](tmp::ForStatement const & for_node) -> Statement
		{
			ForStatement instantiated_node;

			size_t const prev_offset_map_size = variable_offset_map.size();
			instantiated_node.scope = instantiate_dependent_scope(for_node.scope, template_parameters, program, variable_offset_map);

			variable_stack.push_back(instantiated_node.scope.variables);

			instantiated_node.init_statement = std::make_unique<Statement>(instantiate_dependent_statement(*for_node.init_statement, return_type, program, template_parameters, variable_offset_map, variable_stack));
			instantiated_node.end_expression = instantiate_dependent_expression(for_node.end_expression, return_type, program, template_parameters, variable_offset_map, variable_stack);
			instantiated_node.condition = insert_conversion_to_control_flow_condition(instantiate_dependent_expression(for_node.condition, return_type, program, template_parameters, variable_offset_map, variable_stack), program);
			instantiated_node.body = std::make_unique<Statement>(instantiate_dependent_statement(*for_node.body, return_type, program, template_parameters, variable_offset_map, variable_stack));

			variable_stack.pop_back();
			variable_offset_map.resize(prev_offset_map_size);

			return instantiated_node;
		},
		[&](tmp::StatementBlock const & block_node) -> Statement
		{
			StatementBlock instantiated_node;

			size_t const prev_offset_map_size = variable_offset_map.size();
			instantiated_node.scope = instantiate_dependent_scope(block_node.scope, template_parameters, program, variable_offset_map);

			variable_stack.push_back(instantiated_node.scope.variables);

			instantiated_node.statements.reserve(block_node.statements.size());
			for (Statement const & statement_template : block_node.statements)
				instantiated_node.statements.push_back(
					instantiate_dependent_statement(statement_template, return_type, program, template_parameters, variable_offset_map, variable_stack));

			variable_stack.pop_back();
			variable_offset_map.resize(prev_offset_map_size);

			return instantiated_node;
		},
		[&](BreakStatement const & break_stmt) -> Statement
		{
			return break_stmt;
		},
		[&](ContinueStatement const & continue_stmt) -> Statement
		{
			return continue_stmt;
		}
	);

	return std::visit(visitor, statement.as_variant());
}

auto instantiate_function_template(Program & program, FunctionTemplateId template_id, span<TypeId const> parameters) noexcept -> FunctionId
{
	FunctionTemplate & fn_template = program.function_templates[template_id.index];

	// If the template has already been instantiated, reuse it.
	if (auto const it = fn_template.cached_instantiations.find(parameters);
		it != fn_template.cached_instantiations.end())
		return it->second;
	
	// Instantiate the template.
	Function function;

	// Copy the scope
	function.functions = fn_template.scope.functions;
	function.types = fn_template.scope.types;
	function.function_templates = fn_template.scope.function_templates;
	function.struct_templates = fn_template.scope.struct_templates;
	function.variables.reserve(fn_template.scope.variables.size() + fn_template.scope.dependent_variables.size());
	int variable_index = 0;
	int dependent_variable_index = 0;
	
	std::vector<RemappedVariableOffset> variable_offset_map;

	for (FunctionTemplate::Parameter const & param : fn_template.parameters)
	{
		if (param.is_dependent)
		{
			DependentVariable const & var = fn_template.scope.dependent_variables[param.index];
			TypeId var_type = resolve_dependent_type(parameters, var.type, program);
			add_variable_to_scope(function, var.name, var_type, 0, program);
			function.parameter_count++;
			function.parameter_size = function.stack_frame_size;
			dependent_variable_index = param.index + 1;
		}
		else
		{
			Variable const & var = fn_template.scope.variables[param.index];
			int const new_offset = add_variable_to_scope(function, var.name, var.type, 0, program);
			function.parameter_count++;
			function.parameter_size = function.stack_frame_size;
			variable_index = param.index + 1;
			variable_offset_map.push_back({var.offset, new_offset});
		}
	}

	for (int i = variable_index; i < fn_template.scope.variables.size(); ++i)
	{
		Variable const & var = fn_template.scope.variables[i];
		int const new_offset = add_variable_to_scope(function, var.name, var.type, 0, program);
		variable_offset_map.push_back({var.offset, new_offset});
	}
	for (int i = dependent_variable_index; i < fn_template.scope.dependent_variables.size(); ++i)
	{
		DependentVariable const & var = fn_template.scope.dependent_variables[i];
		TypeId var_type = resolve_dependent_type(parameters, var.type, program);
		add_variable_to_scope(function, var.name, var_type, 0, program);
	}

	// TODO: Return type.
	function.return_type = TypeId::deduce;

	std::vector<span<Variable const>> variable_stack;
	variable_stack.push_back(function.variables);

	// Instantiate statement templates.
	function.statements.reserve(fn_template.statement_templates.size());
	for (stmt::Statement const & statement_template : fn_template.statement_templates)
	{
		function.statements.push_back(instantiate_dependent_statement(statement_template, function.return_type, program, parameters, variable_offset_map, variable_stack));
	}

	// Add function to program.
	FunctionId instantiation_id;
	instantiation_id.is_extern = false;
	instantiation_id.index = static_cast<size_t>(program.functions.size());
	program.functions.push_back(std::move(function));
	fn_template.cached_instantiations.emplace(std::vector<TypeId>(parameters.begin(), parameters.end()), instantiation_id);
	return instantiation_id;
}

auto instantiate_struct_template(Program & program, StructTemplateId template_id, span<TypeId const> parameters) noexcept -> TypeId
{
	StructTemplate & struct_template = program.struct_templates[template_id.index];

	if (auto const it = struct_template.cached_instantiations.find(parameters);
		it != struct_template.cached_instantiations.end())
		return it->second;

	Type new_type;
	new_type.size = 0;
	new_type.alignment = 1;
	Struct new_struct;

	// Magic
	for (MemberVariableTemplate const & var_template : struct_template.member_variables)
	{
		TypeId var_type = var_template.type;
		if (var_template.is_dependent)
			var_type.index = parameters[var_template.type.index].index;
		
		add_variable_to_scope(new_struct.member_variables, new_type.size, new_type.alignment, var_template.name, var_type, 0, program);
		if (var_template.initializer_expression)
		{
			std::vector<RemappedVariableOffset> variable_offset_map;
			std::vector<span<Variable const>> variable_stack;
			new_struct.member_variables.back().initializer_expression = instantiate_dependent_expression(*var_template.initializer_expression, var_type, program, parameters, variable_offset_map, variable_stack);
		}
	}

	new_type.extra_data = StructType{static_cast<int>(program.structs.size())};
	program.structs.push_back(std::move(new_struct));
	TypeId const new_type_id = TypeId::with_index(static_cast<unsigned>(program.types.size()));
	program.types.push_back(std::move(new_type));
	struct_template.cached_instantiations.emplace(std::vector<TypeId>(parameters.begin(), parameters.end()), new_type_id);
	return new_type_id;
}

auto pool_string(Program & program, std::string_view string) noexcept -> PooledString
{
	size_t const prev_size = program.string_pool.size();
	program.string_pool.insert(program.string_pool.end(), string.begin(), string.end());
	return PooledString{prev_size, string.size()};
}

auto get(Program const & program, PooledString string) noexcept -> std::string_view
{
	return get(program.string_pool, string);
}

auto get(span<char const> pool, PooledString string) noexcept -> std::string_view
{
	return std::string_view(pool.data() + string.first, string.size);
}
