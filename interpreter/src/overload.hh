#pragma once

#include <type_traits>

template <typename ... Ts>
struct overload : public Ts...
{
	overload(Ts ... ts) : Ts(ts)... {}
	using Ts::operator()...;
};

// Functions that return void will return the given default return value instead.
template <typename Ret, typename ... Ts>
struct overload_default_ret
{
	overload_default_ret(Ret def, Ts ... ts) : default_ret(def), fn(ts...) {}

	template <typename ... Args>
	auto operator () (Args && ... args) const -> std::enable_if_t<std::is_invocable_v<overload<Ts...>, Args...>, Ret>
	{
		if constexpr (std::is_same_v<void, decltype(fn(std::forward<Args>(args)...))>)
		{
			fn(std::forward<Args>(args)...);
			return default_ret;
		}
		else
		{
			return fn(std::forward<Args>(args)...);
		}
	}

	Ret default_ret;
	overload<Ts...> fn;
};
