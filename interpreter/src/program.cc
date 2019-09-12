#include "program.hh"
#include <algorithm>

auto built_in_types() noexcept -> std::vector<Type>
{
	return {
		{"int", 4, 4},
		{"float", 4, 4},
	};
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

auto lookup_name(Scope const & scope, std::string_view name) noexcept
	-> std::variant<
		lookup_result::NothingFound,
		lookup_result::VariableFound,
		lookup_result::FunctionFound
	>
{
	auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), name_equal(name));
	auto const func = std::find_if(scope.functions.begin(), scope.functions.end(), name_equal(name));

	if (var != scope.variables.end())
		return lookup_result::VariableFound{var->offset};

	if (func != scope.functions.end())
		return lookup_result::FunctionFound{func->id};

	return lookup_result::NothingFound();
}

auto lookup_type_name(Program const & program, std::string_view name) noexcept -> TypeId
{
	auto const type = std::find_if(program.types.begin(), program.types.end(), name_equal(name));
	if (type != program.types.end())
		return static_cast<TypeId>(type - program.types.begin());
	else
		return TypeId::none;
}

auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &
{
	return program.types[static_cast<int>(id)];
}
