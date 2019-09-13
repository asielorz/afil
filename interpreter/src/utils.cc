#include "utils.hh"

auto align(int address, int alignment) noexcept -> int
{
	int const excedent = address % alignment;
	if (excedent > 0)
		address += alignment - excedent;
	return address;
}
