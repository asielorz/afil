#pragma once

#include <type_traits>

template <typename T>
using function_ptr = T *;

template <typename Ret, typename T, typename ... Args>
using member_function_ptr = Ret(T::*)(Args...);

template <typename T>
struct is_function_ptr
	: std::bool_constant<std::conjunction_v<std::is_pointer<T>, std::is_function<std::remove_pointer_t<T>>>>
{};
template <typename T>
constexpr bool is_function_ptr_v = is_function_ptr<T>::value;

namespace detail
{
	template <typename T> struct function_pointer_type_base {};

	template <typename ClassType, typename Ret, typename... Args>
	struct function_pointer_type_base<Ret(ClassType::*)(Args...) const>
	{
		using type = function_ptr<auto(Args...) -> Ret>;
	};

	template <typename ClassType, typename Ret, typename... Args>
	struct function_pointer_type_base<Ret(ClassType::*)(Args...) const noexcept>
	{
		using type = function_ptr<auto(Args...) noexcept->Ret>;
	};
}

template <typename T>
struct function_pointer_type
	: public detail::function_pointer_type_base<decltype(&T::operator())>
{
	static_assert(std::is_convertible_v<T, type>);
};

template <typename Ret, typename... Args>
struct function_pointer_type<function_ptr<auto(Args...)->Ret>>
{
	using type = function_ptr<auto(Args...) -> Ret>;
};

template <typename Ret, typename... Args>
struct function_pointer_type<function_ptr<auto(Args...) noexcept -> Ret>>
{
	using type = function_ptr<auto(Args...) noexcept -> Ret>;
};

template <typename F>
constexpr auto cast_to_function_pointer(F f) noexcept -> typename function_pointer_type<F>::type
{
	return f;
}

// Operator for easily converting lambdas to function pointers. This is only necessary for MSVC compiler.
// GCC and clang do it by default.
#ifdef _MSC_VER
	template <typename F>
	constexpr auto operator +(F f) noexcept -> typename function_pointer_type<F>::type
	{
		return f;
	}
#endif
