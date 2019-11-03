#include "scope.hh"
#include "overload.hh"
#include "statement.hh"
#include "program.hh"
#include <algorithm>

TypeId const TypeId::void_ = {false, false, false, 0};
TypeId const TypeId::int_ = {false, false, false, 1};
TypeId const TypeId::float_ = {false, false, false, 2};
TypeId const TypeId::bool_ = {false, false, false, 3};

TypeId const TypeId::none = {true, false, false, 0};
TypeId const TypeId::function = {true, false, false, 1};
TypeId const TypeId::deduce = {true, false, false, 2};

auto is_data_type(TypeId id) noexcept -> bool
{
	return !id.is_language_reseved && id.index != TypeId::void_.index;
}

auto is_convertible(TypeId from, TypeId to, Program const & program) noexcept -> bool
{
	// A type is convertible to itself (for now).
	if (from == to)
		return true;

	if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) &&
		!from.is_reference && !to.is_reference)
	{
		TypeId const from_pointee = pointee_type(from, program);
		TypeId const to_pointee = pointee_type(to, program);
		if (from_pointee.index == to_pointee.index)
		{
			// If converting from a mutable pointer or to an immutable pointer of the same type, conversion is allowed.
			if (!to_pointee.is_mutable && from_pointee.is_mutable)
				return true;
		}
	}

	// No conversions between different types (for now).
	if (from.index != to.index)
		return false;

	// Both T and any reference are convertible to T.
	if (!to.is_reference)
		return true;

	// Both T and any reference are convertible to const &
	if (!to.is_mutable)
		return true;

	// Mutable reference is convertible to everything.
	if (from.is_mutable && from.is_reference)
		return true;

	// Otherwise there is no conversion.
	return false;
}

auto make_reference(TypeId type) noexcept -> TypeId
{
	type.is_reference = true;
	return type;
}

auto make_mutable(TypeId type) noexcept -> TypeId
{
	type.is_mutable = true;
	return type;
}

auto remove_reference(TypeId type) noexcept -> TypeId
{
	type.is_reference = false;
	return type;
}

auto decay(TypeId type) noexcept -> TypeId
{
	type.is_mutable = false;
	type.is_reference = false;
	return type;
}

auto common_type(TypeId a, TypeId b, Program const & program) noexcept -> TypeId
{
	if (a == b)
		return a;

	if (is_convertible(a, b, program))
		return b;

	if (is_convertible(b, a, program))
		return a;

	return TypeId::none;
}

auto lookup_name(ScopeStackView scope_stack, std::string_view name, span<char const> string_pool) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet,
		lookup_result::Type,
		lookup_result::StructTemplate,
		lookup_result::DependentVariable,
		lookup_result::DependentType
	>
{
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
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), pooled_name_equal(string_pool, name));
				if (var != scope.variables.end())
					return lookup_result::GlobalVariable{var->type, var->offset};
			}
			else if (!stop_looking_for_variables)
			{
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), pooled_name_equal(string_pool, name));
				if (var != scope.variables.end())
					return lookup_result::Variable{var->type, var->offset};

				if (scope_stack[i].type == ScopeType::dependent_block || scope_stack[i].type == ScopeType::dependent_function)
				{
					DependentScope const & dependent_scope = static_cast<DependentScope const &>(scope);
					auto const dep_var = std::find_if(dependent_scope.dependent_variables.begin(), dependent_scope.dependent_variables.end(), pooled_name_equal(string_pool, name));
					if (dep_var != dependent_scope.dependent_variables.end())
						return lookup_result::DependentVariable{dep_var->name , dep_var->type};
				}
			}
			
			auto const type = std::find_if(scope.types.begin(), scope.types.end(), pooled_name_equal(string_pool, name));
			if (type != scope.types.end())
				return lookup_result::Type{type->id};

			auto const struct_template = std::find_if(scope.struct_templates.begin(), scope.struct_templates.end(), pooled_name_equal(string_pool, name));
			if (struct_template != scope.struct_templates.end())
				return lookup_result::StructTemplate{struct_template->id};

			if (scope_stack[i].type == ScopeType::dependent_block || scope_stack[i].type == ScopeType::dependent_function)
			{
				DependentScope const & dependent_scope = static_cast<DependentScope const &>(scope);
				auto const dep_type = std::find_if(dependent_scope.dependent_types.begin(), dependent_scope.dependent_types.end(), pooled_name_equal(string_pool, name));
				if (dep_type != dependent_scope.dependent_types.end())
					return lookup_result::DependentType{dep_type->name, static_cast<int>(dep_type - dependent_scope.dependent_types.begin())};
			}
		}

		// Functions.
		for (FunctionName const & fn : scope.functions)
			if (get(string_pool, fn.name) == name)
				overload_set.function_ids.push_back(fn.id);

		// Function templates.
		for (FunctionTemplateName const & fn : scope.function_templates)
			if (get(string_pool, fn.name) == name)
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

auto lookup_type_name(ScopeStackView scope_stack, std::string_view name, span<char const> string_pool) noexcept -> TypeId
{
	auto const visitor = overload(
		[](auto const &) { return TypeId::none; },
		[](lookup_result::Type type) { return type.type_id; }
	);
	return std::visit(visitor, lookup_name(scope_stack, name, string_pool));
}

auto local_variable_offset(ScopeStackView scope_stack) noexcept -> int
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

auto add_variable_to_scope(Scope & scope, PooledString name, TypeId type_id, int scope_offset, Program const & program) -> int
{
	return add_variable_to_scope(scope.variables, scope.stack_frame_size, scope.stack_frame_alignment, name, type_id, scope_offset, program);
}
