#pragma once

#include <variant>
#include <vector>
#include <string_view>

namespace expr { struct ExpressionTree; }
namespace stmt { struct Statement; }
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
	auto pointer_at_address(ProgramStack & stack, int address) noexcept -> void *;

	namespace control_flow
	{
		struct Nothing {};
		struct Return {};
		struct Break {};
		struct Continue {};
		using Variant = std::variant<Nothing, Return, Break, Continue>;
	}

	// Return value is allocated on top of the stack. Returns address of return value.
	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int;
	// Return value is written at the given address.
	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> void;

	auto run_statement(stmt::Statement const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept
		-> control_flow::Variant;

	// TODO: argc, argv. Decide a good stack size.
	auto run(Program const & program, int stack_size = 2048) noexcept -> int;

}

#include "interpreter.inl"
