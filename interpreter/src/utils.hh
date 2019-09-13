#pragma once

auto align(int address, int alignment) noexcept -> int;

template <typename T>
constexpr auto align(T * address, size_t alignment) noexcept -> T *
{
	size_t const excedent = reinterpret_cast<size_t>(address) % alignment;
	if (excedent > 0)
		reinterpret_cast<size_t &>(address) += alignment - excedent;
	return address;
}
