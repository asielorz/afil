#pragma once

#include <string_view>
#include <variant>
#include <vector>

namespace complete { struct Program; }

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

	namespace control_flow
	{
		struct Nothing {};
		struct Return {};
		struct Break {};
		struct Continue {};
		using Variant = std::variant<Nothing, Return, Break, Continue>;
	}

	// TODO: argc, argv. Decide a good stack size.
	auto run(complete::Program const & program, int stack_size = 2048) noexcept -> int;

} // namespace interpreter

#include "interpreter.inl"
