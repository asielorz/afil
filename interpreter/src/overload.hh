#pragma once

template <typename ... Ts>
struct overload : public Ts...
{
	overload(Ts ... ts) : Ts(ts)... {}
	using Ts::operator()...;
};
