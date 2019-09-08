#include "program.hh"
#include <algorithm>

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
