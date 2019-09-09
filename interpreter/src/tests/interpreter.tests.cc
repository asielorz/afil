#include "parser.hh"
#include "interpreter.hh"
#include "lexer.hh"
#include "program.hh"
#include <catch2/catch.hpp>

using namespace std::literals;

auto eval_expression(std::string_view src)
{
	Program program;
	interpreter::ExecutionContext context;
	return interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, program.global_scope), context, program);
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

auto run_statement(std::string_view src, interpreter::ExecutionContext & context, Function & function) noexcept -> void
{
	Program program;
	interpreter::run_statement_tree(*parser::parse_statement(lex::tokenize(src), program, function), context, program);
}

auto eval_expression(std::string_view src, interpreter::ExecutionContext & context, Scope const & scope) noexcept -> int
{
	Program program;
	return interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, scope), context, program);
}

auto eval_expression(std::string_view src, interpreter::ExecutionContext & context, Program & program, Scope const & scope) noexcept -> int
{
	return interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, scope), context, program);
}

TEST_CASE("A statement declares a variable and assigns it an expression, then ends with a semicolon")
{
	interpreter::ExecutionContext context;
	alloc_stack(context, 32);
	Function function;
	run_statement("int i = 5;", context, function);
	run_statement("int foo = 5 + 6 - 3 * 2 + 8 / 3;", context, function);

	REQUIRE(function.variables.size() == 2);
	REQUIRE(function.variables[0].name == "i");
	REQUIRE(function.variables[1].name == "foo");
	REQUIRE(context.stack[context.stack_base_pointer + function.variables[0].offset] == 5);
	REQUIRE(context.stack[context.stack_base_pointer + function.variables[1].offset] == 5 + 6 - 3 * 2 + 8 / 3);
}

TEST_CASE("A variable can be accessed from expressions after it is declared")
{
	interpreter::ExecutionContext context;
	alloc_stack(context, 32);
	Function function;
	run_statement("int i = 30;", context, function);
	int const i = 30;
	REQUIRE(eval_expression("i", context, function) == i);
	REQUIRE(eval_expression("i * i - 4 + i / 6 - 3 * i", context, function) == i * i - 4 + i / 6 - 3 * i);
}

TEST_CASE("Identity function expression")
{
	Program program;
	parser::parse_expression(lex::tokenize("fn (int x) -> int { return x; }"), program, program.global_scope);
}

TEST_CASE("Use let to name a function")
{
	Program program;
	parser::parse_statement(lex::tokenize("let id = fn (int x) -> int { return x; };"), program, program.global_scope);

	REQUIRE(program.functions.size() == 1);
	REQUIRE(program.global_scope.functions[0].name == "id");
	REQUIRE(program.functions[0].variables.size() == 1);
	REQUIRE(program.functions[0].parameter_count == 1);
	REQUIRE(program.functions[0].variables[0].name == "x");
}

TEST_CASE("Call a function")
{
	Program program;
	interpreter::ExecutionContext context;
	alloc_stack(context, 32);
	parser::parse_statement(lex::tokenize("let add_one = fn (int x) -> int { return x + 1; };"), program, program.global_scope);
	REQUIRE(eval_expression("add_one(5)", context, program, program.global_scope) == 6);
}

TEST_CASE("Call  function with multiple parameters")
{
	Program program;
	interpreter::ExecutionContext context;
	alloc_stack(context, 128);
	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	REQUIRE(eval_expression("add(5, 6)", context, program, program.global_scope) == 11);

	auto const source = R"(
let add_seven = fn (int a, int b, int c, int d, int e, int f, int g) -> int 
{
	return a + b + c + d + e + f + g; 
};
)";
	parser::parse_statement(lex::tokenize(source), program, program.global_scope);
	REQUIRE(eval_expression("add_seven(1, 2, 3, 4, 5, 6, 7)", context, program, program.global_scope) == 1 + 2 + 3 + 4 + 5 + 6 + 7);
}

TEST_CASE("Nested calls")
{
	Program program;
	interpreter::ExecutionContext context;
	alloc_stack(context, 128);

	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	parser::parse_statement(lex::tokenize("let subtract = fn (int x, int y) -> int { return add(x, 0 - y); };"), program, program.global_scope);
	
	REQUIRE(eval_expression("subtract(add(2, 3), subtract(4, 1))", context, program, program.global_scope) == (2 + 3) - (4 - 1));
}

TEST_CASE("Naming a function is a valid expression (that does nothing)")
{
	Program program;
	interpreter::ExecutionContext context;
	alloc_stack(context, 128);

	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	parser::parse_expression(lex::tokenize("add"), program, program.global_scope);
}
