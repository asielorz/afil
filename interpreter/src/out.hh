#pragma once

#include <memory> // addressof

template <typename T>
struct out
{
	constexpr out(T & t) noexcept : out_parameter(std::addressof(t)) {}

	constexpr auto operator * () noexcept -> T & { return *out_parameter; }
	constexpr auto operator -> () noexcept -> T * { return out_parameter; }

private:
	T * out_parameter;
};
