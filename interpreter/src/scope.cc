#include "scope.hh"
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

auto lookup_name(ScopeStack const & scope_stack, std::string_view name) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>
{
	lookup_result::OverloadSet overload_set;

	bool stop_looking_for_variables = false;

	// Search the scopes in reverse order.
	int const start = static_cast<int>(scope_stack.size() - 1);
	for (int i = start; i >= 0; --i)
	{
		Scope const & scope = *scope_stack[i].scope;

		// Search variables only if we don't already know this is a function name.
		if (overload_set.function_ids.empty())
		{
			if (scope_stack[i].type == ScopeType::global)
			{
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), name_equal(name));
				if (var != scope.variables.end())
					return lookup_result::GlobalVariable{var->type, var->offset};
			}
			else if (!stop_looking_for_variables)
			{
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), name_equal(name));
				if (var != scope.variables.end())
					return lookup_result::Variable{var->type, var->offset};
			}
		}

		// Functions.
		for (FunctionName const & fn : scope.functions)
			if (fn.name == name)
				overload_set.function_ids.push_back(fn.id);

		// After we leave a function, stop looking for variables.
		if (i < start && scope_stack[i].type == ScopeType::function)
			stop_looking_for_variables = true;
	}

	if (overload_set.function_ids.empty())
		return lookup_result::Nothing();
	else
		return overload_set;
}

auto local_variable_offset(ScopeStack const & scope_stack) noexcept -> int
{
	int const start = static_cast<int>(scope_stack.size() - 1);
	if (scope_stack[start].type == ScopeType::global || scope_stack[start].type == ScopeType::function)
		return 0;

	int size = 0;
	for (int i = start - 1; i >= 0; --i)
	{
		size += scope_stack[i].scope->stack_frame_size;
		if (scope_stack[start].type == ScopeType::global || scope_stack[start].type == ScopeType::function)
			break;
	}

	return size;
}
