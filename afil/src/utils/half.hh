#pragma once

#include <cstdint>

struct half
{
	constexpr half() noexcept = default;
	
	constexpr static auto from_float(float value) noexcept -> half
	{
		uint32_t const float_state = reinterpret_cast<uint32_t &>(value);
		uint16_t const half_state = 
			((float_state >> 16) & 0b1000'0000'0000'0000) |																								// Sign
			((((float_state & 0b0111'1111'1000'0000'0000'0000'0000'0000) - 0b0011'1000'0000'0000'0000'0000'0000'0000) >> 13) & 0b0111'1100'0000'0000) |	// Exponent
			((float_state >> 13) & 0b0011'1111'1111);																									// Mantissa

		return half::with_state(half_state);
	}
	constexpr static auto with_state(uint16_t state) noexcept -> half
	{
		half h;
		h.state = state;
		return h;
	}

	constexpr auto as_float() const noexcept -> float
	{
		uint32_t const float_state =
			((state & 0b1000'0000'0000'0000) << 16) |									// Sign
			(((state & 0b0111'1100'0000'0000) + 0b0001'1100'0000'0000'0000) << 13) |	// Exponent
			((state & 0b0011'1111'1111) << 13);											// Mantissa
	}

	uint16_t state = 0;
};

constexpr auto operator + (half a, half b) noexcept -> half { return half::from_float(a.as_float() + b.as_float()); }
constexpr auto operator - (half a, half b) noexcept -> half { return half::from_float(a.as_float() - b.as_float()); }
constexpr auto operator * (half a, half b) noexcept -> half { return half::from_float(a.as_float() * b.as_float()); }
constexpr auto operator / (half a, half b) noexcept -> half { return half::from_float(a.as_float() / b.as_float()); }
constexpr auto operator - (half a) noexcept -> half { return half::from_float(-a.as_float()); }
constexpr auto operator == (half a, half b) noexcept -> bool { return a.state == b.state; }
constexpr auto operator != (half a, half b) noexcept -> bool { return !(a == b); }
constexpr auto operator < (half a, half b) noexcept -> bool { return a.as_float() < b.as_float(); }
constexpr auto operator <= (half a, half b) noexcept -> bool { return a.as_float() <= b.as_float(); }
constexpr auto operator > (half a, half b) noexcept -> bool { return a.as_float() > b.as_float(); }
constexpr auto operator >= (half a, half b) noexcept -> bool { return a.as_float() >= b.as_float(); }
