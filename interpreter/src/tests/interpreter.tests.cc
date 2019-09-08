#include "interpreter.hh"
#include <catch2/catch.hpp>

using namespace std::literals;

TEST_CASE("basic arithmetic expressions")
{
	REQUIRE(interpreter::eval_expression("3 + 4") == 7);
	REQUIRE(interpreter::eval_expression("1 + 5 + 6 - 3 + 2") == 11);
	REQUIRE(interpreter::eval_expression("3 * 4 * 5 / 6") == 10);
}

TEST_CASE("parenthesis define precedence")
{
	REQUIRE(interpreter::eval_expression("2 * (3 + 4)") == 14);
	REQUIRE(interpreter::eval_expression("6 - (4 / 2)") == 4);
}

TEST_CASE("Integer alone inside parenthesis")
{
	REQUIRE(interpreter::eval_expression("(2)") == 2);
	REQUIRE(interpreter::eval_expression("((((7))))") == 7);
	REQUIRE(interpreter::eval_expression(" ((  (( 7)   )) )") == 7);
}

TEST_CASE("Nested parenthesis")
{
	REQUIRE(interpreter::eval_expression("2 * (3 + (4 - 1 - ((2 * 2) - 3)))") == 10);
}

TEST_CASE("Multiplication has more precedence than addition")
{
	REQUIRE(interpreter::eval_expression("2 * 3 + 4") == 10);
	REQUIRE(interpreter::eval_expression("6 - 4 / 2") == 4);
	REQUIRE(interpreter::eval_expression("2 * 3 * 2 - 4 * 5 + 6 / 3 * 2 + 1 + 1 + 1 * 3") == 1);
}
