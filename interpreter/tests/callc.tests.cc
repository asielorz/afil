#include "utils/callc.hh"
#include "utils/function_ptr.hh"
#include <catch2/catch.hpp>

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
	auto const caller = callc::c_function_caller({{8, false}}, {4, false});
	char const * const str = "Hello, world!\n";
	int r;
	caller(+[](char const * s) { return printf(s); }, &str, &r);

	REQUIRE(r == 14);
}

TEST_CASE("Function: (float) -> int")
{
	auto const caller = callc::c_function_caller({{4, true}}, {4, false});
	float const x = 3.141592f;
	int r;
	caller(+[](float x) { return int(x); }, &x, &r);

	REQUIRE(r == int(3.141592f));
}

TEST_CASE("Function: (int, int) -> int")
{
	auto const caller = callc::c_function_caller({{4, false}, {4, false}}, {4, false});
	int x[] = {319634731, -41334};
	int r;
	caller(+[](int a, int b) { return a + b; }, &x, &r);

	REQUIRE(r == 319634731 - 41334);
}

TEST_CASE("Function: (double, double) -> bool")
{
	auto const caller = callc::c_function_caller({{8, true}, {8, true}}, {1, false});
	double const x1[] = {-7.123343, 41.334};
	double const x2[] = {7.123343, -41.334};
	bool r1;
	bool r2;
	
	caller(+[](double a, double b) -> bool { return a < b; }, &x1, &r1);
	caller(+[](double a, double b) -> bool { return a < b; }, &x2, &r2);

	REQUIRE(r1 == true);
	REQUIRE(r2 == false);
}

TEST_CASE("Function: (char, char, char, char) -> int")
{
	auto const caller = callc::c_function_caller({{1, false}, {1, false}, {1, false}, {1, false}}, { 4, false });
	union IntBytes
	{
		char a[4];
		int i;
	} x;
	x.a[0] = 'a'; x.a[1] = 's'; x.a[2] = 'd'; x.a[3] = 'f';
	int r;

	caller(+[](char a, char b, char c, char d) -> int 
	{ 
		IntBytes x;
		x.a[0] = a; x.a[1] = b; x.a[2] = c; x.a[3] = d;
		return x.i;
	}, &x, &r);

	REQUIRE(r == x.i);
}

TEST_CASE("Function: (ushort, ushort, ushort) -> ushort")
{
	auto const caller = callc::c_function_caller({{2, false}, {2, false}, {2, false}}, {2, false});
	unsigned short x[] = {1234, 4321, 2686};
	unsigned short r;

	caller(+[](unsigned short a, unsigned short b, unsigned short c) { return a + b + c; }, &x, &r);

	REQUIRE(r == x[0] + x[1] + x[2]);
}

TEST_CASE("Function: (double, ushort, int) -> double")
{
	auto const caller = callc::c_function_caller({{8, true}, {2, false}, {4, false}}, {8, true});
	struct Params
	{
		double d = 1.23467;
		unsigned short u = 25;
		int i = -6;
	} x;
	double r;

	caller(+[](double a, unsigned short b, int c) -> double { return a + b + c; }, &x, &r);

	REQUIRE(r == x.d + x.u + x.i);
}
