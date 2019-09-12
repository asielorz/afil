#pragma once

#include <vector>
#include <string_view>

namespace parser 
{ 
	struct ExpressionTree;
	struct StatementTree;
}
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

	auto eval_expression_tree(parser::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int;
	auto run_statement_tree(parser::StatementTree const & tree, ProgramStack & stack, Program const & program) noexcept -> bool;

}

#include "interpreter.inl"
