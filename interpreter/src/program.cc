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

auto lookup_name(Scope const & scope, Scope const & global_scope, std::string_view name) noexcept
	-> std::variant<
		lookup_result::Nothing,
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>
{
	lookup_result::OverloadSet overload_set;

	// First search the current scope.
	if (&scope != &global_scope)
	{
		// Variables.
		auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), name_equal(name));
		if (var != scope.variables.end())
			return lookup_result::Variable{ var->type, var->offset };

		// Functions.
		for (FunctionName const & fn : scope.functions)
			if (fn.name == name)
				overload_set.function_ids.push_back(fn.id);
	}

	// Search global scope.
	// If we already know the name belongs to a function, avoid looking up variables.
	if (overload_set.function_ids.empty())
	{
		auto const var = std::find_if(global_scope.variables.begin(), global_scope.variables.end(), name_equal(name));
		if (var != global_scope.variables.end())
			return lookup_result::GlobalVariable{var->type, var->offset};
	}

	// Search global scope for functions.
	for (FunctionName const & fn : global_scope.functions)
		if (fn.name == name)
			overload_set.function_ids.push_back(fn.id);

	if (overload_set.function_ids.empty())
		return lookup_result::Nothing();
	else
		return overload_set;
}

auto resolve_function_overloading(span<int const> overload_set, span<TypeId> parameters, Program const & program) noexcept -> int
{
	for (int function_id : overload_set)
	{
		Function const & function = program.functions[function_id];
		if (std::equal(
			function.variables.begin(), function.variables.begin() + function.parameter_count, 
			parameters.begin(), parameters.end(),
			[](Variable const & var, TypeId type) { return var.type == type; }
		))
			return function_id;
	}

	return -1;
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
