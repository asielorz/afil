#pragma once

#include "scope.hh"
#include "span.hh"
#include "callc.hh"
#include <variant>

namespace stmt { struct Statement; }

struct Type
{
	std::string_view name;
	int size;
	int alignment;
};

struct Function : Scope
{
	int parameter_count = 0;     // From the variables, how many are arguments. The rest are locals.
	int parameter_size = 0;	     // Size in bytes needed for parameters.
	TypeId return_type;
	std::vector<stmt::Statement> statements;
};

struct ExternFunction
{
	int parameter_size;
	int parameter_alignment;
	TypeId return_type;
	std::vector<TypeId> parameter_types;
	callc::CFunctionCaller caller;
	void const * function_pointer;
};

struct Program
{
	Program();

	std::vector<Type> types;
	std::vector<Function> functions;
	std::vector<ExternFunction> extern_functions;
	std::vector<stmt::Statement> global_initialization_statements;
	Scope global_scope;
	FunctionId main_function = invalid_function_id;
};

// Returns id of function found or -1 on failure.
auto resolve_function_overloading(span<FunctionId const> overload_set, span<TypeId const> parameters, Program const & program) noexcept ->FunctionId;

auto lookup_type_name(Program const & program, std::string_view name) noexcept -> TypeId;
auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
auto is_data_type(TypeId id) noexcept -> bool;
auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>; // Stack allocator?
