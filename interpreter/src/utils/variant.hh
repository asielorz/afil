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

namespace detail
{
	template <typename ... Ts, typename ... Us>
	auto variant_union_impl(std::variant<Ts...> && v1, std::variant<Us...> && v2) noexcept -> std::variant<Ts..., Us...>;
}

template <typename Var1, typename Var2>
using variant_union = decltype(detail::variant_union_impl(std::declval<Var1>(), std::declval<Var2>()));

template <typename SupersetVariant, typename ... Ts>
auto upcast(std::variant<Ts...> v) noexcept -> SupersetVariant
{
	auto const visitor = [](auto & t)
	{
		return SupersetVariant(std::move(t));
	};

	return std::visit(visitor, v);
}
