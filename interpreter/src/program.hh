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

namespace lookup_result
{
	struct Nothing {};
	struct Variable { TypeId variable_type; int variable_offset; };
	struct GlobalVariable { TypeId variable_type; int variable_offset; };
	struct OverloadSet { std::vector<FunctionId> function_ids; };
}
auto lookup_name(Scope const & scope, Scope const & global_scope, std::string_view name) noexcept 
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>;
// Returns id of function found or -1 on failure.
auto resolve_function_overloading(span<FunctionId const> overload_set, span<TypeId const> parameters, Program const & program) noexcept ->FunctionId;

auto lookup_type_name(Program const & program, std::string_view name) noexcept -> TypeId;
auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
auto is_data_type(TypeId id) noexcept -> bool;
