#pragma once

#include <vector>
#include <string_view>

namespace expr { struct ExpressionTree; }
namespace parser { struct StatementTree; }
struct Program;

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

	// Return value is allocated on top of the stack. Returns address of return value.
	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int;
	// Return value is written at the given address.
	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> void;
	auto run_statement_tree(parser::StatementTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> bool;

}

#include "interpreter.inl"
