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

} // namespace interpreter
