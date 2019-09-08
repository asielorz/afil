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

	struct ExecutionContext
	{
		std::vector<char> stack;
		int stack_base_pointer = 0;
	};
	auto read_word(ExecutionContext const & context, int address) noexcept -> int;
	auto write_word(ExecutionContext & context, int address, int value) noexcept -> void;
	auto alloc_stack(ExecutionContext & context, int stack_size_in_bytes) noexcept -> void;

	auto eval_expression_tree(parser::ExpressionTree const & tree, ExecutionContext & context, Program const & program) noexcept -> int;
	auto run_statement_tree(parser::StatementTree const & tree, ExecutionContext & context, Program const & program) noexcept -> bool;

}
