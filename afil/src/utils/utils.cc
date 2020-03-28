#include "utils.hh"

auto align(int address, int alignment) noexcept -> int
{
	int const excedent = address % alignment;
	if (excedent > 0)
		address += alignment - excedent;
	return address;
}

auto add_size_aligned(int prev_size, int size_to_add, int alignment) noexcept -> int
{
	return align(prev_size, alignment) + size_to_add;
}
