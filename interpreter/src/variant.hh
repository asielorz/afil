#pragma once

#include <variant>

template <typename T, typename ... Ts>
constexpr auto has_type(std::variant<Ts...> const & var) noexcept -> bool
{
	constexpr bool type_equal[] = {std::is_same_v<T, Ts>...};
	return type_equal[var.index()];
}

template <typename T, typename ... Ts>
constexpr auto try_get(std::variant<Ts...> & var) noexcept -> T *
{
	if (has_type<T>(var))
		return std::addressof(std::get<T>(var));
	else
		return nullptr;
}

template <typename T, typename ... Ts>
constexpr auto try_get(std::variant<Ts...> const & var) noexcept -> T const *
{
	if (has_type<T>(var))
		return std::addressof(std::get<T>(var));
	else
		return nullptr;
}
