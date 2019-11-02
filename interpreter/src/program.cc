#include "program.hh"
#include "lexer.hh"
#include "parser.hh"
#include "function_ptr.hh"
#include "variant.hh"
#include "parser.hh"
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
		span<FunctionTemplateParameter const> template_params = program.function_templates[template_id.index].parameters;
		if (template_params.size() == parameters.size())
		{
			dependent_type_count = program.function_templates[template_id.index].template_parameters.size();
			std::fill(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none);

			int conversions = 0;
			bool discard = false;
			for (size_t i = 0; i < template_params.size(); ++i)
			{
				if (has_type<TypeId>(template_params[i]))
				{
					if (!check_type_validness_as_overload_candidate(std::get<TypeId>(template_params[i]), parameters[i], program, conversions))
					{
						discard = true;
						break;
					}
				}
				else
				{
					DependentType const dependent_type = std::get<DependentType>(template_params[i]);
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

namespace parser
{
	auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, ParseParams p, Function & function) noexcept -> void;
	auto parse_function_body(span<lex::Token const> tokens, size_t & index, ParseParams p, Function & function) noexcept -> void;
}

auto instantiate_function_template(Program & program, FunctionTemplateId template_id, span<TypeId const> parameters) noexcept -> FunctionId
{
	FunctionTemplate & fn_template = program.function_templates[template_id.index];

	if (auto const it = fn_template.cached_instantiations.find(parameters);
		it != fn_template.cached_instantiations.end())
		return it->second;

	// Everything below is a hack. Instead of keeping the tokens and parsing them for each instantiation,
	// we should find a way to represent template expressions.

	ScopeStack stack;
	stack.push_back({&program.global_scope, ScopeType::global});
	Scope template_parameter_scope;
	for (size_t i = 0; i < parameters.size(); ++i)
		template_parameter_scope.types.push_back({fn_template.template_parameters[i].name, parameters[i]});

	stack.push_back({&template_parameter_scope, ScopeType::global});

	Function function;
	size_t index = 0;
	TypeId dummy;
	parser::parse_function_prototype(fn_template.tokens, index, {program, stack, dummy}, function);
	parser::parse_function_body(fn_template.tokens, index, {program, stack, dummy}, function);
	
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
