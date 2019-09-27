#pragma once

#include "function_id.hh"
#include <string_view>
#include <vector>
#include <variant>

enum struct TypeId { noreturn = -3, function = -2, none = -1, int_, float_, bool_ };

struct Variable
{
	std::string_view name;
	TypeId type;
	int offset;
};

struct FunctionName
{
	std::string_view name;
	FunctionId id;
};

struct Scope
{
	int stack_frame_size = 0;
	int stack_frame_alignment = 1; // Alignment requirement of the stack frame.
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
};
enum struct ScopeType { global, function, block };

struct CurrentScope
{
	Scope * scope;
	ScopeType type;
};
using ScopeStack = std::vector<CurrentScope>;

namespace lookup_result
{
	struct Nothing {};
	struct Variable { TypeId variable_type; int variable_offset; };
	struct GlobalVariable { TypeId variable_type; int variable_offset; };
	struct OverloadSet { std::vector<FunctionId> function_ids; };
}
auto lookup_name(ScopeStack const & scope_stack, std::string_view name) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>;

auto local_variable_offset(ScopeStack const & scope_stack) noexcept -> int;
