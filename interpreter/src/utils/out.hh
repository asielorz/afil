#pragma once

#include <memory> // addressof

template <typename T>
struct out
{
	constexpr explicit out(T & t) noexcept : out_parameter(std::addressof(t)) {}

	template <typename U, typename = std::enable_if_t<!std::is_same_v<T, U> && std::is_base_of_v<T, U>>>
	constexpr out(out<U> other) : out_parameter(std::addressof(*other)) {}

	constexpr auto operator * () noexcept -> T & { return *out_parameter; }
	constexpr auto operator -> () noexcept -> T * { return out_parameter; }

private:
	T * out_parameter;
};
