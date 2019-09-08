#pragma once

#include <memory> // addressof

template <typename T>
struct out
{
	out(T & t) noexcept : out_parameter(std::addressof(t)) {}

	auto operator * () noexcept -> T & { return *out_parameter; }
	auto operator -> () noexcept -> T * { return out_parameter; }

private:
	T * out_parameter;
};
