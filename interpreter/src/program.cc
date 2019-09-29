#include "program.hh"
#include "lexer.hh"
#include "parser.hh"
#include "function_ptr.hh"
#include <algorithm>
#include <cassert>

using namespace std::literals;

auto built_in_types() noexcept -> std::vector<Type>
{
	return {
		{"int", 4, 4},
		{"float", 4, 4},
		{"bool", 1, 1},
	};
}

template <typename T> struct id_for_type {};
template <> struct id_for_type<int> { static constexpr unsigned value = 0; };
template <> struct id_for_type<float> { static constexpr unsigned value = 1; };
template <> struct id_for_type<bool> { static constexpr unsigned value = 2; };
template <typename T> constexpr unsigned id_for_type_v = id_for_type<T>::value;

template <typename R, typename ... Args>
auto extern_function_descriptor(auto (*fn)(Args...) noexcept -> R) noexcept -> ExternFunction
{
	return ExternFunction{
		static_cast<int>(sizeof(std::tuple<Args...>)),
		static_cast<int>(std::max({alignof(Args)...})),
		TypeId::with_index(id_for_type_v<R>),
		{TypeId::with_index(id_for_type_v<Args>)...},
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
		{"operator=="sv,	extern_function_descriptor(+[](int a, int b) noexcept -> bool { return a == b; })},
		{"operator<=>"sv,	extern_function_descriptor(+[](int a, int b) noexcept -> int { return a - b; })},

		{"operator+"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a + b; })},
		{"operator-"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},
		{"operator*"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a * b; })},
		{"operator/"sv,		extern_function_descriptor(+[](float a, float b) noexcept -> float { return a / b; })},
		{"operator=="sv,	extern_function_descriptor(+[](float a, float b) noexcept -> bool { return a == b; })},
		{"operator<=>"sv,	extern_function_descriptor(+[](float a, float b) noexcept -> float { return a - b; })},

		{"operator and"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a && b; })},
		{"operator or"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a || b; })},
		{"operator xor"sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a != b; })},
		{"operator=="sv,	extern_function_descriptor(+[](bool a, bool b) noexcept -> bool { return a == b; })},
	};
}

Program::Program()
{
	types = built_in_types();

	auto const extern_functions_to_add = default_extern_functions();

	extern_functions.reserve(extern_functions_to_add.size());
	global_scope.functions.reserve(extern_functions_to_add.size());

	for (auto const fn : extern_functions_to_add)
	{
		global_scope.functions.push_back({fn.first, {true, static_cast<unsigned>(extern_functions.size())}});
		extern_functions.push_back(fn.second);
	}
}

struct name_equal
{
	constexpr name_equal(std::string_view name_) noexcept : name(name_) {}

	template <typename T>
	constexpr auto operator () (T const & t) const noexcept
	{
		return t.name == name;
	}

	std::string_view name;
};

auto resolve_function_overloading(span<FunctionId const> overload_set, span<TypeId const> parameters, Program const & program) noexcept -> FunctionId
{
	for (FunctionId function_id : overload_set)
	{
		if (!function_id.is_extern)
		{
			Function const & function = program.functions[function_id.index];
			if (std::equal(
				function.variables.begin(), function.variables.begin() + function.parameter_count,
				parameters.begin(), parameters.end(),
				[](Variable const & var, TypeId type) { return var.type == type; }
			))
			{
				return function_id;
			}
		}
		else
		{
			ExternFunction const & function = program.extern_functions[function_id.index];
			if (std::equal(
				function.parameter_types.begin(), function.parameter_types.end(),
				parameters.begin(), parameters.end()
			))
			{
				return function_id;
			}
		}
	}

	return invalid_function_id;
}

auto lookup_type_name(Program const & program, std::string_view name) noexcept -> TypeId
{
	auto const type = std::find_if(program.types.begin(), program.types.end(), name_equal(name));
	if (type != program.types.end())
		return TypeId::with_index(static_cast<unsigned>(type - program.types.begin()));
	else
		return TypeId::none;
}

auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &
{
	assert(!id.is_language_reseved);
	return program.types[id.index];
}

auto is_data_type(TypeId id) noexcept -> bool
{
	return !id.is_language_reseved;
}
