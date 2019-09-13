#include "parser.hh"
#include "interpreter.hh"
#include "lexer.hh"
#include "program.hh"
#include <catch2/catch.hpp>

using namespace std::literals;

auto eval_expression(std::string_view src)
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	int const return_address = interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, program.global_scope), stack, program);
	return read_word(stack, return_address);
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

auto run_statement(std::string_view src, interpreter::ProgramStack & stack, Scope & scope) noexcept -> void
{
	Program program;
	interpreter::run_statement_tree(*parser::parse_statement(lex::tokenize(src), program, scope), stack, program, 0);
}

auto eval_expression(std::string_view src, interpreter::ProgramStack & stack, Scope const & scope) noexcept -> int
{
	Program program;
	int const return_address = interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, scope), stack, program);
	return read_word(stack, return_address);
}

template <typename T>
auto eval_expression(std::string_view src, interpreter::ProgramStack & stack, Program & program) noexcept -> T
{
	if (stack.top_pointer == 0)
		stack.top_pointer = program.global_scope.stack_frame_size;
	int const return_address = interpreter::eval_expression_tree(parser::parse_expression(lex::tokenize(src), program, program.global_scope), stack, program);
	return interpreter::read<T>(stack, return_address);
}

TEST_CASE("A statement declares a variable and assigns it an expression, then ends with a semicolon")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);
	Function function;
	run_statement("int i = 5;", stack, function);
	run_statement("int foo = 5 + 6 - 3 * 2 + 8 / 3;", stack, function);

	REQUIRE(function.variables.size() == 2);
	REQUIRE(function.variables[0].name == "i");
	REQUIRE(function.variables[1].name == "foo");
	REQUIRE(read_word(stack, stack.base_pointer + function.variables[0].offset) == 5);
	REQUIRE(read_word(stack, stack.base_pointer + function.variables[1].offset) == 5 + 6 - 3 * 2 + 8 / 3);
}

TEST_CASE("A variable can be accessed from expressions after it is declared")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);
	Program program;
	run_statement("int i = 30;", stack, program.global_scope);
	int const i = 30;
	REQUIRE(eval_expression<int>("i", stack, program) == i);
	REQUIRE(eval_expression<int>("i * i - 4 + i / 6 - 3 * i", stack, program) == i * i - 4 + i / 6 - 3 * i);
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
	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);
	parser::parse_statement(lex::tokenize("let add_one = fn (int x) -> int { return x + 1; };"), program, program.global_scope);
	REQUIRE(eval_expression<int>("add_one(5)", stack, program) == 6);
}

TEST_CASE("Call  function with multiple parameters")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	REQUIRE(eval_expression<int>("add(5, 6)", stack, program) == 11);

	auto const source = R"(
let add_seven = fn (int a, int b, int c, int d, int e, int f, int g) -> int 
{
	return a + b + c + d + e + f + g; 
};
)";
	parser::parse_statement(lex::tokenize(source), program, program.global_scope);
	REQUIRE(eval_expression<int>("add_seven(1, 2, 3, 4, 5, 6, 7)", stack, program) == 1 + 2 + 3 + 4 + 5 + 6 + 7);
}

TEST_CASE("Nested calls")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);

	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	parser::parse_statement(lex::tokenize("let subtract = fn (int x, int y) -> int { return add(x, 0 - y); };"), program, program.global_scope);
	
	REQUIRE(eval_expression<int>("subtract(add(2, 3), subtract(4, 1))", stack, program) == (2 + 3) - (4 - 1));
}

TEST_CASE("Naming a function is a valid expression (that does nothing)")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);

	parser::parse_statement(lex::tokenize("let add = fn (int x, int y) -> int { return x + y; };"), program, program.global_scope);
	parser::parse_expression(lex::tokenize("add"), program, program.global_scope);
}

TEST_CASE("Immediately invoked function expression")
{
	REQUIRE(eval_expression("fn (int i, int j) -> int { return i + j; }(3, 4)") == 3 + 4);
}

TEST_CASE("Immediately invoked function expression and operator")
{
	REQUIRE(eval_expression("fn (int i, int j) -> int { return i + j; }(3, 4) - 5") == 3 + 4 - 5);
	REQUIRE(eval_expression("2 * fn (int i, int j) -> int { return i + j; }(3, 4)") == 2 * (3 + 4));
}

TEST_CASE("Floating point literal")
{
	Program program;
	parser::parse_expression(lex::tokenize("3.141592"), program, program.global_scope);
}

TEST_CASE("Floating point variable declaration")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);
	Program program;
	run_statement("float f = 5.0;", stack, program.global_scope);

	REQUIRE(program.global_scope.variables.size() == 1);
	REQUIRE(program.global_scope.variables[0].name == "f");
	REQUIRE(interpreter::read<float>(stack, stack.base_pointer + program.global_scope.variables[0].offset) == 5.0f);
	REQUIRE(eval_expression<float>("f", stack, program) == 5.0f);
}

TEST_CASE("Overloading")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);
	Program program;
	parser::parse_statement(lex::tokenize("let id = fn (int x) -> int { return x; };"), program, program.global_scope);
	parser::parse_statement(lex::tokenize("let id = fn (float x) -> float { return x; };"), program, program.global_scope);
	REQUIRE(eval_expression<int>("id(5)", stack, program) == 5);
	REQUIRE(eval_expression<float>("id(3.141592)", stack, program) == 3.141592f);
}

TEST_CASE("Hacking extern functions as a proof of concept")
{
	int(*add_fn)(int, int) = [](int a, int b) 
	{ 
		return a + b; 
	};

	Program program;
	program.extern_functions.push_back(ExternFunction{
		{ Variable{"a", TypeId::int_, 0}, Variable{"b", TypeId::int_, 0} },
		TypeId::int_,
		add_fn
	});
	program.global_scope.functions.push_back(FunctionName{"add", FunctionId{1, 0}});

	interpreter::ProgramStack stack;
	alloc_stack(stack, 32);

	REQUIRE(eval_expression<int>("add(2, 3)", stack, program) == 2 + 3);
}
