#pragma once

#include <type_traits>
#include "compatibility.hh"

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

	// Although we technically only need to disable the warning for the line with
	// 'return default_ret', the state of this warning cannot be changed inside a function in MSVC
	// https://stackoverflow.com/questions/12380603/disable-warning-c4702-seems-not-work-for-vs-2012
	AFIL_IGNORE_WARNING_UNREACHABLE_CODE()
	template <typename ... Args>
	auto operator () (Args && ... args) const -> std::enable_if_t<std::is_invocable_v<overload<Ts...>, Args...>, Ret>
	{
		if constexpr (std::is_same_v<void, decltype(fn(std::forward<Args>(args)...))>)
		{
			fn(std::forward<Args>(args)...);
			
			// In the case 'fn' is a [[noreturn]] function,
			// we will get a warning in this line. However, this
			// might be intended as other instantiations might not
			// [[noreturn]]. Ideally, we would check whether 'fn' is
			// declared [[noreturn]] and handle that in some other way,
			// but there is not way to check that
			return default_ret;
		}
		else
		{
			return fn(std::forward<Args>(args)...);
		}
	}
	AFIL_WARNING_POP()

	Ret default_ret;
	overload<Ts...> fn;
};
