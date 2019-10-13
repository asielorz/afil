#include "program.hh"
#include "lexer.hh"
#include "parser.hh"
#include "function_ptr.hh"
#include "name_equal.hh"
#include <algorithm>
#include <cassert>

using namespace std::literals;

auto built_in_types() noexcept -> std::vector<std::pair<std::string_view, Type>>
{
	return {
		{"void",  {0, 1, -1}},
		{"int",   {4, 4, -1}},
		{"float", {4, 4, -1}},
		{"bool",  {1, 1, -1}},
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
		{"operator+"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a + b; })},
		{"operator-"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a - b; })},
		{"operator*"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a * b; })},
		{"operator/"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a / b; })},
		{"operator%"sv,		extern_function_descriptor(+[](int a, int b) noexcept -> int { return a % b; })},
		{"operator=="sv,	extern_function_descriptor(+[](int a, int b) noexcept -> bool { return a == b; })},
		{"operator<=>"sv,	extern_function_descriptor(+[](int a, int b) noexcept -> int { return a - b; })},
		{"operator="sv,		extern_function_descriptor(+[](int & a, int b) noexcept -> void { a = b; })},
		{"operator-"sv,		extern_function_descriptor(+[](int a) noexcept -> int { return -a; })},

		{"operator+"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a + b; })},
		{"operator-"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"operator*"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a * b; })},
		{"operator/"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a / b; })},
		{"operator=="sv,	extern_function_descriptor(+[](float a, float b) noexcept -> bool { return a == b; })},
		{"operator<=>"sv,	extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"operator="sv,		extern_function_descriptor(+[](float & a, float b) noexcept -> void { a = b; })},
		{"operator-"sv,		extern_function_descriptor(+[](float a) noexcept -> float { return -a; })},

		{"operator and"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a && b; })},
		{"operator or"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a || b; })},
		{"operator xor"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a != b; })},
		{"operator not"sv,	extern_function_descriptor(+[](bool a) noexcept -> bool { return !a; })},
		{"operator=="sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a == b; })},
		{"operator="sv,		extern_function_descriptor(+[](bool & a, bool b) noexcept -> void { a = b; })},
	};
}

Program::Program()
{
	auto const built_in_types_to_add = built_in_types();

	types.reserve(built_in_types_to_add.size());
	global_scope.types.reserve(built_in_types_to_add.size());

	for (auto const type : built_in_types_to_add)
	{
		global_scope.types.push_back({type.first, {false, false, false, static_cast<unsigned>(types.size())}});
		types.push_back(type.second);
	}

	//*******************************************************************

	auto const extern_functions_to_add = default_extern_functions();

	extern_functions.reserve(extern_functions_to_add.size());
	global_scope.functions.reserve(extern_functions_to_add.size());

	for (auto const fn : extern_functions_to_add)
	{
		global_scope.functions.push_back({fn.first, {true, static_cast<unsigned>(extern_functions.size())}});
		extern_functions.push_back(fn.second);
	}
}

auto resolve_function_overloading(span<FunctionId const> overload_set, span<TypeId const> parameters, Program const & program) noexcept -> FunctionId
{
	// TODO: This finds first valid candidate. It should find best match.
	for (FunctionId function_id : overload_set)
	{
		auto const param_types = parameter_types(program, function_id);
		if (std::equal(parameters.begin(), parameters.end(), param_types.begin(), param_types.end(), is_convertible))
		{
			return function_id;
		}
	}

	return invalid_function_id;
}

auto is_struct(Type const & type) noexcept -> bool
{
	return type.struct_index != -1;
}

auto find_member_variable(Struct const & type, std::string_view member_name) noexcept -> int
{
	auto const it = std::find_if(type.member_variables.begin(), type.member_variables.end(), name_equal(member_name));
	if (it == type.member_variables.end())
		return -1;
	else
		return static_cast<int>(it - type.member_variables.begin());
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
