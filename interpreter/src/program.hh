#pragma once

#include "expression.hh"
#include "scope.hh"
#include "span.hh"
#include "callc.hh"
#include <variant>
#include <optional>

namespace stmt { struct Statement; }

struct Type
{
	int size;
	int alignment;
	int struct_index;
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

struct MemberVariable : Variable
{
	std::optional<expr::ExpressionTree> initializer_expression;
};

struct Struct
{
	std::vector<MemberVariable> member_variables;
};

struct Program
{
	Program();

	std::vector<Type> types;
	std::vector<Struct> structs;
	std::vector<Function> functions;
	std::vector<ExternFunction> extern_functions;
	std::vector<stmt::Statement> global_initialization_statements;
	Scope global_scope;
	FunctionId main_function = invalid_function_id;
};

// Returns id of function found or invalid_function_id on failure.
auto resolve_function_overloading(span<FunctionId const> overload_set, span<TypeId const> parameters, Program const & program) noexcept -> FunctionId;

auto is_struct(Type const & type) noexcept -> bool;
auto find_member_variable(Struct const & type, std::string_view member_name) noexcept -> int;
auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
auto type_size(Program const & program, TypeId id) noexcept -> int;
auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>; // Stack allocator?
auto return_type(Program const & program, FunctionId id) noexcept -> TypeId;
