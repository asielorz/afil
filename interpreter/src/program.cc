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
		{"="sv,		extern_function_descriptor(+[](int & a, int b) noexcept -> void { a = b; })},
		{"-"sv,		extern_function_descriptor(+[](int a) noexcept -> int { return -a; })},

		{"+"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a + b; })},
		{"-"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"*"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a * b; })},
		{"/"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a / b; })},
		{"=="sv,	extern_function_descriptor(+[](float a, float b) noexcept -> bool { return a == b; })},
		{"<=>"sv,	extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"="sv,		extern_function_descriptor(+[](float & a, float b) noexcept -> void { a = b; })},
		{"-"sv,		extern_function_descriptor(+[](float a) noexcept -> float { return -a; })},

		{"and"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a && b; })},
		{"or"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a || b; })},
		{"xor"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a != b; })},
		{"not"sv,	extern_function_descriptor(+[](bool a) noexcept -> bool { return !a; })},
		{"=="sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a == b; })},
		{"="sv,		extern_function_descriptor(+[](bool & a, bool b) noexcept -> void { a = b; })},
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
					if (resolved_dependent_types[dependent_type.index] != TypeId::none)
					{
						if (!check_type_validness_as_overload_candidate(resolved_dependent_types[dependent_type.index], parameters[i], program, conversions))
						{
							discard = true;
							break;
						}
					}
					else
					{
						// TODO: Patterns with dependent types.
						TypeId resolved_type;
						resolved_type.index = parameters[i].index;
						resolved_type.is_reference = dependent_type.is_reference;
						resolved_type.is_mutable = dependent_type.is_mutable;

						if (check_type_validness_as_overload_candidate(resolved_type, parameters[i], program, conversions))
						{
							resolved_dependent_types[dependent_type.index] = decay(resolved_type);
						}
						else
						{
							discard = true;
							break;
						}
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
	raise_syntax_error_if_not(function_id != invalid_function_id, "Function overload not found.");

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

auto instantiate_dependent_expression(expr::ExpressionTree const & tree, Function & function, Program & program, span<RemappedVariableOffset const> variable_offset_map) noexcept -> expr::ExpressionTree
{
	using namespace expr;

	auto const visitor = overload(
		[](auto const & non_dependent) -> expr::ExpressionTree { return non_dependent; },
		[&](tmp::LocalVariableNode const & var_node) -> ExpressionTree
		{
			// TODO: Index based approach?
			auto const it = std::find_if(function.variables.begin(), function.variables.end(), [&](Variable const & var)
			{
				return var.name.first == var_node.name.first && var.name.size == var_node.name.size;
			});
			assert(it != function.variables.end());
			Variable const & var = *it;

			LocalVariableNode instantiated_node;
			instantiated_node.variable_type = var.type;
			instantiated_node.variable_offset = var.offset;
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
			instantiated_node.expression = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*deref_node.expression, function, program, variable_offset_map));
			instantiated_node.variable_type = remove_reference(expression_type_id(*instantiated_node.expression, program));
			return instantiated_node;
		},
		[&](AddressofNode const & addressof_node) -> ExpressionTree
		{
			AddressofNode instantiated_node;
			instantiated_node.operand = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*addressof_node.operand, function, program, variable_offset_map));
			instantiated_node.return_type = pointer_type_for(remove_reference(expression_type_id(*instantiated_node.operand, program)), program);
			return instantiated_node;
		},
		[&](DepointerNode const & deptr_node) -> ExpressionTree
		{
			DepointerNode instantiated_node;
			instantiated_node.operand = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*deptr_node.operand, function, program, variable_offset_map));
			instantiated_node.return_type = make_reference(std::get<PointerType>(type_with_id(program, expression_type_id(*instantiated_node.operand, program)).extra_data).value_type);
			return instantiated_node;
		},
		[&](tmp::FunctionCallNode const & fn_call_node) -> ExpressionTree
		{
			FunctionCallNode instantiated_node;

			instantiated_node.parameters.reserve(fn_call_node.parameters.size());
			for (ExpressionTree const & param : fn_call_node.parameters)
				instantiated_node.parameters.push_back(instantiate_dependent_expression(param, function, program, variable_offset_map));

			std::vector<TypeId> const  parameter_types = map(instantiated_node.parameters, expr::expression_type_id(program));
			instantiated_node.function_id = resolve_function_overloading_and_insert_conversions(
				fn_call_node.overload_set, 
				instantiated_node.parameters, 
				parameter_types, 
				program);

			return instantiated_node;
		},
		[&](tmp::RelationalOperatorCallNode const & fn_call_node) -> ExpressionTree
		{
			RelationalOperatorCallNode instantiated_node;

			instantiated_node.parameters = std::make_unique<std::array<expr::ExpressionTree, 2>>();
			(*instantiated_node.parameters)[0] = instantiate_dependent_expression((*fn_call_node.parameters)[0], function, program,variable_offset_map);
			(*instantiated_node.parameters)[1] = instantiate_dependent_expression((*fn_call_node.parameters)[1], function, program,variable_offset_map);

			instantiated_node.op = fn_call_node.op;

			TypeId const parameter_types[2] = {expression_type_id((*instantiated_node.parameters)[0], program), expression_type_id((*instantiated_node.parameters)[1], program)};
			instantiated_node.function_id = resolve_function_overloading_and_insert_conversions(
				fn_call_node.overload_set,
				*instantiated_node.parameters,
				parameter_types,
				program);

			return instantiated_node;
		},
		[&](IfNode const & if_node) -> ExpressionTree
		{
			IfNode instantiated_node;

			instantiated_node.condition = std::make_unique<ExpressionTree>(insert_conversion_to_control_flow_condition(
				instantiate_dependent_expression(*if_node.condition, function, program, variable_offset_map),
				program));

			instantiated_node.then_case = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*if_node.then_case, function, program, variable_offset_map));
			instantiated_node.else_case = std::make_unique<ExpressionTree>(instantiate_dependent_expression(*if_node.else_case, function, program, variable_offset_map));

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
		}
		//[](StatementBlockNode const &) { TODO },
		//[](StructConstructorNode const & ctor_node) { TODO }
	);

	return std::visit(visitor, tree.as_variant());
}

auto instantiate_dependent_statement(
	stmt::Statement const & statement, 
	Function & function, 
	Program & program, 
	span<RemappedVariableOffset const> variable_offset_map) noexcept -> stmt::Statement
{
	using namespace stmt;

	auto const visitor = overload(
		[&](VariableDeclarationStatement const & var_decl_node) -> Statement
		{
			VariableDeclarationStatement instantiated_node;
			instantiated_node.variable_offset = remap_offset(variable_offset_map, var_decl_node.variable_offset);
			instantiated_node.assigned_expression = instantiate_dependent_expression(var_decl_node.assigned_expression, function, program, variable_offset_map);
			return instantiated_node;
		},
		[&](tmp::VariableDeclarationStatement const & var_decl_node) -> Statement
		{
			VariableDeclarationStatement instantiated_node;

			auto const it = std::find_if(function.variables.begin(), function.variables.end(), [&](Variable const & var)
			{
				return var.name.first == var_decl_node.variable_name.first && var.name.size == var_decl_node.variable_name.size;
			});
			assert(it != function.variables.end());
			Variable const & var = *it;

			instantiated_node.variable_offset = var.offset;
			if (var_decl_node.assigned_expression)
			{
				instantiated_node.assigned_expression = instantiate_dependent_expression(*var_decl_node.assigned_expression, function, program, variable_offset_map);
				TypeId const assigned_expr_type = expression_type_id(instantiated_node.assigned_expression, program);
				instantiated_node.assigned_expression = insert_conversion_node(
					std::move(instantiated_node.assigned_expression),
					assigned_expr_type, var.type, program);
			}
			else
				mark_as_to_do("Synthesize default constructor");

			return instantiated_node;
		},
		[&](ExpressionStatement const & expr_node) -> Statement
		{
			ExpressionStatement instantiated_node;
			instantiated_node.expression = instantiate_dependent_expression(expr_node.expression, function, program, variable_offset_map);
			return instantiated_node;
		},
		[&](ReturnStatement const & return_node) -> Statement
		{
			ReturnStatement instantiated_node;
			instantiated_node.returned_expression = instantiate_dependent_expression(return_node.returned_expression, function, program, variable_offset_map);

			TypeId const returned_expression_type = expression_type_id(instantiated_node.returned_expression, program);
			if (function.return_type == TypeId::deduce)
				function.return_type = decay(expression_type_id(instantiated_node.returned_expression, program));

			if (returned_expression_type != function.return_type)
				instantiated_node.returned_expression = 
				insert_conversion_node(
					std::move(instantiated_node.returned_expression),
					returned_expression_type, function.return_type,
					program);

			return instantiated_node;
		},
		[&](IfStatement const & if_node) -> Statement
		{
			IfStatement instantiated_node;
			instantiated_node.condition = insert_conversion_to_control_flow_condition(instantiate_dependent_expression(if_node.condition, function, program, variable_offset_map), program);
			instantiated_node.then_case = std::make_unique<Statement>(instantiate_dependent_statement(*if_node.then_case, function, program, variable_offset_map));
			instantiated_node.else_case = std::make_unique<Statement>(instantiate_dependent_statement(*if_node.else_case, function, program, variable_offset_map));
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
			instantiated_node.condition = insert_conversion_to_control_flow_condition(instantiate_dependent_expression(while_node.condition, function, program, variable_offset_map), program);
			instantiated_node.body = std::make_unique<Statement>(instantiate_dependent_statement(*while_node.body, function, program, variable_offset_map));
			return instantiated_node;
		},
		[&](ForStatement const & for_node) -> Statement
		{
			(void)for_node;
			mark_as_to_do("Dependent for statement");

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
			TypeId var_type = parameters[var.type.index];
			var_type.is_reference = var.type.is_reference;
			var_type.is_mutable = var.type.is_mutable;
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
		TypeId var_type = parameters[var.type.index];
		var_type.is_reference = var.type.is_reference;
		var_type.is_mutable = var.type.is_mutable;
		add_variable_to_scope(function, var.name, var_type, 0, program);
	}

	// TODO: Return type.
	function.return_type = TypeId::deduce;

	// Instantiate statement templates.
	function.statements.reserve(fn_template.statement_templates.size());
	for (stmt::Statement const & statement_template : fn_template.statement_templates)
	{
		function.statements.push_back(instantiate_dependent_statement(statement_template, function, program, variable_offset_map));
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
