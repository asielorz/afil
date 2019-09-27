#pragma once

#include "function_id.hh"
#include <string_view>
#include <vector>

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
