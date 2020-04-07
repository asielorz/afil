#pragma once

#include "utils/span.hh"
#include <string_view>
#include <variant>
#include <vector>

namespace complete 
{ 
	struct Expression;
	struct Statement;
	struct Program;
}
struct FunctionId;

namespace interpreter
{

	struct ProgramStack
	{
		std::vector<char> memory;
		int base_pointer = 0;
		int top_pointer = 0;
	};
	auto read_word(ProgramStack const & stack, int address) noexcept -> int;
	auto write_word(ProgramStack & stack, int address, int value) noexcept -> void;
	template <typename T> auto read(ProgramStack const & stack, int address) noexcept -> T const &;
	template <typename T> auto write(ProgramStack & stack, int address, T const & value) noexcept -> void;
	auto alloc_stack(ProgramStack & stack, int stack_size_in_bytes) noexcept -> void;
	auto pointer_at_address(ProgramStack & stack, int address) noexcept -> char *;

	enum struct ControlFlow
	{
		Nothing,
		Return,
		Break,
		Continue
	};

	// TODO: argc, argv. Decide a good stack size.
	auto run(complete::Program const & program, int stack_size = 2048) noexcept -> int;

	auto call_function(FunctionId function_id, span<complete::Expression const> parameters, ProgramStack & stack, complete::Program const & program, int return_address) noexcept -> void;
	auto eval_expression(complete::Expression const & expr, ProgramStack & stack, complete::Program const & program) noexcept -> int;
	auto eval_expression(complete::Expression const & expr, ProgramStack & stack, complete::Program const & program, int return_address) noexcept -> void;
	auto run_statement(complete::Statement const & tree, ProgramStack & stack, complete::Program const & program, int return_address) noexcept -> ControlFlow;

} // namespace interpreter

#include "interpreter.inl"
