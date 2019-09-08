#include "parser.hh"
#include "interpreter.hh"
#include "lexer.hh"
#include <catch2/catch.hpp>

using namespace std::literals;

auto eval_expression(std::string_view src, interpreter::ExecutionContext const & context = interpreter::ExecutionContext()) noexcept -> int
{
	return interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src)), context);
}

TEST_CASE("basic arithmetic expressions")
{
	REQUIRE(eval_expression("3 + 4") == 3 + 4);
	REQUIRE(eval_expression("1 + 5 + 6 - 3 + 2") == 1 + 5 + 6 - 3 + 2);
	REQUIRE(eval_expression("3 * 4 * 5 / 6") == 3 * 4 * 5 / 6);
}

TEST_CASE("parenthesis define precedence")
{
	REQUIRE(eval_expression("2 * (3 + 4)") == 2 * (3 + 4));
	REQUIRE(eval_expression("6 - (4 / 2)") == 6 - (4 / 2));
}

TEST_CASE("Integer alone inside parenthesis")
{
	REQUIRE(eval_expression("(2)") == 2);
	REQUIRE(eval_expression("((((7))))") == 7);
	REQUIRE(eval_expression(" ((  (( 7)   )) )") == 7);
}

TEST_CASE("Nested parenthesis")
{
	REQUIRE(eval_expression("2 * (3 + (4 - 1 - ((2 * 2) - 3)))") == 2 * (3 + (4 - 1 - ((2 * 2) - 3))));
}

TEST_CASE("Multiplication has more precedence than addition")
{
	REQUIRE(eval_expression("2 * 3 + 4") == 2 * 3 + 4);
	REQUIRE(eval_expression("6 - 4 / 2") == 6 - 4 / 2);
	REQUIRE(eval_expression("2 * 3 * 2 - 4 * 5 + 6 / 3 * 2 + 1 + 1 + 1 * 3") == 2 * 3 * 2 - 4 * 5 + 6 / 3 * 2 + 1 + 1 + 1 * 3);
}

auto run_statement(std::string_view src, interpreter::ExecutionContext & context) noexcept -> void
{
	interpreter::run_statement_tree(parser::parse_statement(lex::tokenize(src)), context);
}

TEST_CASE("A statement declares a variable and assigns it an expression, then ends with a semicolon")
{
	interpreter::ExecutionContext context;
	run_statement("int i = 5;", context);
	REQUIRE(context.variables[0].name == "i");
	REQUIRE(context.variables[0].value == 5);

	run_statement("int foo = 5 + 6 - 3 * 2 + 8 / 3;", context);
	REQUIRE(context.variables[1].name == "foo");
	REQUIRE(context.variables[1].value == 5 + 6 - 3 * 2 + 8 / 3);
}

TEST_CASE("A variable can be accessed from expressions after it is declared")
{
	interpreter::ExecutionContext context;
	run_statement("int i = 30;", context);
	int const i = 30;
	REQUIRE(eval_expression("i", context) == i);
	REQUIRE(eval_expression("i * i - 4 + i / 6 - 3 * i", context) == i * i - 4 + i / 6 - 3 * i);
}
