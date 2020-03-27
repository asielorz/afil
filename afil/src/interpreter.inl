namespace interpreter
{

	template <typename T> auto read(ProgramStack const & stack, int address) noexcept -> T const &
	{
		return reinterpret_cast<T const &>(stack.memory[address]);
	}

	template <typename T> auto write(ProgramStack & stack, int address, T const & value) noexcept -> void
	{
		reinterpret_cast<T &>(stack.memory[address]) = value;
	}

	template <typename T> auto write(ProgramStack & stack, int address, T const array[], int size) noexcept -> void
	{
		memcpy(pointer_at_address(stack, address), array, size * sizeof(T));
	}

} // namespace interpreter
