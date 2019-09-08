#pragma once

#include <vector>
#include <string_view>
#include <variant>

namespace parser { struct StatementTree; }

struct Variable
{
	std::string_view name;
	int offset;
};

struct FunctionName
{
	std::string_view name;
	int id;
};

struct Scope
{
	int stack_frame_size = 0;
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
};

struct Function : Scope
{
	int parameter_count = 0; // From the variables, how many are arguments. The rest are locals.
	std::vector<parser::StatementTree> statements;
};

struct Program
{
	std::vector<Function> functions;
	Scope global_scope;
};

// Returns offset of the requested variable, or -1 if not found.
namespace lookup_result
{
	struct NothingFound {};
	struct VariableFound { int variable_offset; };
	struct FunctionFound { int function_id; };
}
auto lookup_name(Scope const & scope, std::string_view name) noexcept 
	-> std::variant<
		lookup_result::NothingFound, 
		lookup_result::VariableFound, 
		lookup_result::FunctionFound
	>;
