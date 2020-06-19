#pragma once

auto align(int address, int alignment) noexcept -> int;
auto add_size_aligned(int prev_size, int size_to_add, int alignment) noexcept -> int;

template <typename T>
constexpr auto align(T * address, size_t alignment) noexcept -> T *
{
	size_t const excedent = reinterpret_cast<size_t>(address) % alignment;
	if (excedent > 0)
		reinterpret_cast<size_t &>(address) += alignment - excedent;
	return address;
}

auto is_divisible(int dividend, int divisor) noexcept -> bool;
auto is_aligned(int address, int alignment) noexcept -> bool;
