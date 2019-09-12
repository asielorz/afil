#pragma once

#include <tuple>

template <typename ... Ts>
struct any_of
{
	constexpr any_of(Ts const & ... ts) : values_to_compare(ts...) {}

	template <typename T, typename F>
	[[nodiscard]] constexpr auto compare_first(T const & a, F f) const noexcept -> bool
	{
		return std::apply([&](auto const & ... v) { return (f(a, v) || ...); }, values_to_compare);
	}

private:
	std::tuple<Ts const & ...> values_to_compare;
};

template <typename ... Ts, typename T>
[[nodiscard]] constexpr auto operator == (any_of<Ts...> const & a, T const & b) noexcept -> bool
{
	return a.compare_first(b, std::equal_to());
}

template <typename ... Ts, typename T>
[[nodiscard]] constexpr auto operator == (T const & a, any_of<Ts...> const & b) noexcept -> bool
{
	return b == a;
}

template <typename ... Ts, typename T>
[[nodiscard]] constexpr auto operator != (any_of<Ts...> const & a, T const & b) noexcept -> bool
{
	return !(a == b);
}

template <typename ... Ts, typename T>
[[nodiscard]] constexpr auto operator != (T const & a, any_of<Ts...> const & b) noexcept -> bool
{
	return !(b == a);
}
