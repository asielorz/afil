#include "callc.hh"
#include <catch2/catch.hpp>

template <typename T>
using function_ptr = T*;

namespace detail
{
	template <typename T> struct function_pointer_type_base {};

	template <typename ClassType, typename Ret, typename... Args>
	struct function_pointer_type_base<Ret(ClassType::*)(Args...) const>
	{
		using type = function_ptr<auto(Args...)->Ret>;
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
	using type = function_ptr<auto(Args...)->Ret>;
};

template <typename Ret, typename... Args>
struct function_pointer_type<function_ptr<auto(Args...) noexcept->Ret>>
{
	using type = function_ptr<auto(Args...) noexcept->Ret>;
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


TEST_CASE("Function that returns an int")
{
	auto const caller = callc::c_function_caller({}, {4, false});
	int r;
	caller(+[]() { return 1293; }, nullptr, &r);

	REQUIRE(r == 1293);
}

TEST_CASE("Function that returns a float")
{
	auto const caller = callc::c_function_caller({}, {4, true});
	float r;
	caller(+[]() { return -645.12373f; }, nullptr, &r);

	REQUIRE(r == -645.12373f);
}

TEST_CASE("Function that returns a char")
{
	auto const caller = callc::c_function_caller({}, {1, false});
	char r;
	caller(+[]() -> char { return 'F'; }, nullptr, &r);

	REQUIRE(r == 'F');
}

TEST_CASE("Function that returns a double")
{
	auto const caller = callc::c_function_caller({}, {8, true});
	double r;
	caller(+[]() { return -645.12373; }, nullptr, &r);

	REQUIRE(r == -645.12373);
}

TEST_CASE("Function that returns a uint64_t")
{
	auto const caller = callc::c_function_caller({}, {8, false});
	uint64_t r;
	caller(+[]() { return uint64_t(-1); }, nullptr, &r);

	REQUIRE(r == uint64_t(-1));
}

TEST_CASE("Function that returns a short")
{
	auto const caller = callc::c_function_caller({}, {2, false});
	short r;
	caller(+[]() { return 1200; }, nullptr, &r);

	REQUIRE(r == 1200);
}

TEST_CASE("Function: (uint64_t) -> int")
{
	callc::TypeDescriptor const args[] = {{8, false}};
	auto const caller = callc::c_function_caller(args, {4, false});
	char const * const str = "Hello, world!\n";
	int r;
	caller(+[](char const * s) { return printf(s); }, &str, &r);

	REQUIRE(r == 14);
}

TEST_CASE("Function: (float) -> int")
{
	callc::TypeDescriptor const args[] = {{4, false}};
	auto const caller = callc::c_function_caller(args, { 4, false });
	float const x = 3.141592f;
	int r;
	caller(+[](float x) { return int(x); }, &x, &r);

	REQUIRE(r == int(3.141592f));
}
