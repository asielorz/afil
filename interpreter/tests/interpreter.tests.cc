#include "parser.hh"
#include "interpreter.hh"
#include "lexer.hh"
#include "program.hh"
#include "pretty_print.hh"
#include <catch2/catch.hpp>
#include <iostream>

using namespace std::literals;

template <typename T>
auto eval_expression(std::string_view src, interpreter::ProgramStack & stack, Program & program) noexcept -> T
{
	if (stack.top_pointer == 0)
		stack.top_pointer = program.global_scope.stack_frame_size;

	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });

	auto const expr_tree = parser::parse_expression(lex::tokenize(src), program, scope_stack);
	int const return_address = interpreter::eval_expression_tree(expr_tree, stack, program);

	TypeId const expr_type = expression_type_id(expr_tree, program);
	if constexpr (std::is_pointer_v<T>)
	{
		assert(expr_type.is_reference);
		return interpreter::read<T>(stack, return_address);
	}
	else
	{
		if (expr_type.is_reference)
			return *interpreter::read<T *>(stack, return_address);
		else
			return interpreter::read<T>(stack, return_address);
	}
}

template <typename T>
auto eval_expression(std::string_view src)
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	return eval_expression<T>(src, stack, program);
}

auto run_statement(std::string_view src, interpreter::ProgramStack & stack, Scope & scope) noexcept -> void
{
	Program program;
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	scope_stack.push_back({ &scope, ScopeType::function });
	interpreter::run_statement(*parser::parse_statement(lex::tokenize(src), program, scope_stack), stack, program, 0);
}

auto pretty_print_expr(std::string_view source, Program & program)
{
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	printf("%s", pretty_print(parser::parse_expression(lex::tokenize(source), program, scope_stack), program).c_str());
}

auto parse_and_print(std::string_view src) noexcept -> void
{
	Program const program = parser::parse_source(src);
	printf("%s\n", pretty_print(program).c_str());
}

TEST_CASE("basic arithmetic expressions")
{
	REQUIRE(eval_expression<int>("3 + 4") == 3 + 4);
	REQUIRE(eval_expression<int>("1 + 5 + 6 - 3 + 2") == 1 + 5 + 6 - 3 + 2);
	REQUIRE(eval_expression<int>("3 * 4 * 5 / 6") == 3 * 4 * 5 / 6);
}

TEST_CASE("parenthesis define precedence")
{
	REQUIRE(eval_expression<int>("2 * (3 + 4)") == 2 * (3 + 4));
	REQUIRE(eval_expression<int>("6 - (4 / 2)") == 6 - (4 / 2));
}

TEST_CASE("Integer alone inside parenthesis")
{
	REQUIRE(eval_expression<int>("(2)") == 2);
	REQUIRE(eval_expression<int>("((((7))))") == 7);
	REQUIRE(eval_expression<int>(" ((  (( 7)   )) )") == 7);
}

TEST_CASE("Nested parenthesis")
{
	REQUIRE(eval_expression<int>("2 * (3 + (4 - 1 - ((2 * 2) - 3)))") == 2 * (3 + (4 - 1 - ((2 * 2) - 3))));
}

TEST_CASE("Multiplication has more precedence than addition")
{
	REQUIRE(eval_expression<int>("2 * 3 + 4") == 2 * 3 + 4);
	REQUIRE(eval_expression<int>("6 - 4 / 2") == 6 - 4 / 2);
	REQUIRE(eval_expression<int>("2 * 3 * 2 - 4 * 5 + 6 / 3 * 2 + 1 + 1 + 1 * 3") == 2 * 3 * 2 - 4 * 5 + 6 / 3 * 2 + 1 + 1 + 1 * 3);
}

TEST_CASE("A statement declares a variable and assigns it an expression, then ends with a semicolon")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
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
	alloc_stack(stack, 128);
	Program program;
	run_statement("int i = 30;", stack, program.global_scope);
	int const i = 30;
	REQUIRE(eval_expression<int>("i", stack, program) == i);
	REQUIRE(eval_expression<int>("i * i - 4 + i / 6 - 3 * i", stack, program) == i * i - 4 + i / 6 - 3 * i);
}

TEST_CASE("Identity function expression")
{
	Program program;
	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	parser::parse_expression(lex::tokenize("fn (int x) -> int { return x; }"), program, scope_stack);
}

auto parse_statement(std::string_view source, Program & program) noexcept -> void
{
	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	parser::parse_statement(lex::tokenize(source), program, scope_stack);
}

auto parse_expression(std::string_view source, Program & program) noexcept -> void
{
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	parser::parse_expression(lex::tokenize(source), program, scope_stack);
}

TEST_CASE("Use let to name a function")
{
	Program program;
	parse_statement("let id = fn (int x) -> int { return x; };", program);

	REQUIRE(program.functions.size() == 1);
	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	REQUIRE(std::get<lookup_result::OverloadSet>(lookup_name(scope_stack, "id")).function_ids.size() == 1);
	REQUIRE(program.functions[0].variables.size() == 1);
	REQUIRE(program.functions[0].parameter_count == 1);
	REQUIRE(program.functions[0].variables[0].name == "x");
}

TEST_CASE("Call a function")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	parse_statement("let add_one = fn (int x) -> int { return x + 1; };", program);
	REQUIRE(eval_expression<int>("add_one(5)", stack, program) == 6);
}

TEST_CASE("Call  function with multiple parameters")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	parse_statement("let add = fn (int x, int y) -> int { return x + y; };", program);
	REQUIRE(eval_expression<int>("add(5, 6)", stack, program) == 11);

	auto const source = R"(
let add_seven = fn (int a, int b, int c, int d, int e, int f, int g) -> int 
{
	return a + b + c + d + e + f + g; 
};
)";
	parse_statement(source, program);
	REQUIRE(eval_expression<int>("add_seven(1, 2, 3, 4, 5, 6, 7)", stack, program) == 1 + 2 + 3 + 4 + 5 + 6 + 7);
}

TEST_CASE("Nested calls")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);

	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	parse_statement("let add = fn (int x, int y) -> int { return x + y; };", program);
	parse_statement("let subtract = fn (int x, int y) -> int { return add(x, 0 - y); };", program);
	
	REQUIRE(eval_expression<int>("subtract(add(2, 3), subtract(4, 1))", stack, program) == (2 + 3) - (4 - 1));
}

TEST_CASE("Naming a function is a valid expression (that does nothing)")
{
	Program program;
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);

	parse_statement("let add = fn (int x, int y) -> int { return x + y; };", program);
	parse_expression("add", program);
}

TEST_CASE("Immediately invoked function expression")
{
	REQUIRE(eval_expression<int>("fn (int i, int j) -> int { return i + j; }(3, 4)") == 3 + 4);
}

TEST_CASE("Immediately invoked function expression and operator")
{
	REQUIRE(eval_expression<int>("fn (int i, int j) -> int { return i + j; }(3, 4) - 5") == 3 + 4 - 5);
	REQUIRE(eval_expression<int>("2 * fn (int i, int j) -> int { return i + j; }(3, 4)") == 2 * (3 + 4));
}

TEST_CASE("Floating point literal")
{
	Program program;
	parse_expression("3.141592", program);
}

TEST_CASE("Floating point variable declaration")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
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
	alloc_stack(stack, 128);
	Program program;
	parse_statement("let id = fn (int x) -> int { return x; };", program);
	parse_statement("let id = fn (float x) -> float { return x; };", program);
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
		2 * sizeof(int), alignof(int),
		TypeId::int_, {TypeId::int_, TypeId::int_},
		callc::c_function_caller(add_fn),
		add_fn
	});
	program.global_scope.functions.push_back(FunctionName{"add", FunctionId{1, 0}});

	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);

	REQUIRE(eval_expression<int>("add(2, 3)", stack, program) == 2 + 3);
}

TEST_CASE("Operators for floats")
{
	REQUIRE(eval_expression<float>("3.0 + 4.7") == 3.0f + 4.7f);
	REQUIRE(eval_expression<float>("1.12 + 5.3 + 6.09 - 3.24 + 2.85") == 1.12f + 5.3f + 6.09f - 3.24f + 2.85f);
	REQUIRE(eval_expression<float>("3.14 * 4.007 * 5.94 / 6.367") == 3.14f * 4.007f * 5.94f / 6.367f);
}

TEST_CASE("Boolean literals")
{
	Program program;
	parse_expression("true", program);
	parse_expression("false", program);
}

TEST_CASE("Declare variable of boolean type")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	Program program;
	run_statement("bool t = true;", stack, program.global_scope);
	run_statement("bool f = false;", stack, program.global_scope);

	REQUIRE(eval_expression<bool>("t", stack, program) == true);
	REQUIRE(eval_expression<bool>("f", stack, program) == false);
}

TEST_CASE("Comparisons")
{
	REQUIRE(eval_expression<bool>("3 == 3") == true);
	REQUIRE(eval_expression<bool>("5 != 5") == false);
	REQUIRE(eval_expression<bool>("1 == 2") == false);
	REQUIRE(eval_expression<bool>("7 != 2") == true);

	REQUIRE(eval_expression<bool>("3 > 4") == false);
	REQUIRE(eval_expression<bool>("6 > 2") == true);
	REQUIRE(eval_expression<bool>("2 > 2") == false);

	REQUIRE(eval_expression<bool>("3 >= 4") == false);
	REQUIRE(eval_expression<bool>("6 >= 2") == true);
	REQUIRE(eval_expression<bool>("2 >= 2") == true);

	REQUIRE(eval_expression<bool>("3 < 4") == true);
	REQUIRE(eval_expression<bool>("6 < 2") == false);
	REQUIRE(eval_expression<bool>("2 < 2") == false);

	REQUIRE(eval_expression<bool>("3 <= 4") == true);
	REQUIRE(eval_expression<bool>("6 <= 2") == false);
	REQUIRE(eval_expression<bool>("2 <= 2") == true);

	REQUIRE(eval_expression<int>("3 <=> 4") < 0);
	REQUIRE(eval_expression<int>("6 <=> 2") > 0);
	REQUIRE(eval_expression<int>("2 <=> 2") == 0);
}

TEST_CASE("Comparisons of floats")
{
	REQUIRE(eval_expression<bool>("3.5 == 3.5") == true);
	REQUIRE(eval_expression<bool>("5.5 != 5.5") == false);
	REQUIRE(eval_expression<bool>("1.5 == 2.5") == false);
	REQUIRE(eval_expression<bool>("7.5 != 2.5") == true);

	REQUIRE(eval_expression<bool>("3.5 > 4.5") == false);
	REQUIRE(eval_expression<bool>("6.5 > 2.5") == true);
	REQUIRE(eval_expression<bool>("2.5 > 2.5") == false);

	REQUIRE(eval_expression<bool>("3.5 >= 4.5") == false);
	REQUIRE(eval_expression<bool>("6.5 >= 2.5") == true);
	REQUIRE(eval_expression<bool>("2.5 >= 2.5") == true);

	REQUIRE(eval_expression<bool>("3.5 < 4.5") == true);
	REQUIRE(eval_expression<bool>("6.5 < 2.5") == false);
	REQUIRE(eval_expression<bool>("2.5 < 2.5") == false);

	REQUIRE(eval_expression<bool>("3.5 <= 4.5") == true);
	REQUIRE(eval_expression<bool>("6.5 <= 2.5") == false);
	REQUIRE(eval_expression<bool>("2.5 <= 2.5") == true);

	REQUIRE(eval_expression<int>("3.5 <=> 4.5") < 0);
	REQUIRE(eval_expression<int>("6.5 <=> 2.5") > 0);
	REQUIRE(eval_expression<int>("2.5 <=> 2.5") == 0);
}

TEST_CASE("Logical operators")
{
	REQUIRE(eval_expression<bool>("true and true") == true);
	REQUIRE(eval_expression<bool>("true and false") == false);
	REQUIRE(eval_expression<bool>("false and true") == false);
	REQUIRE(eval_expression<bool>("false and false") == false);

	REQUIRE(eval_expression<bool>("true or true") == true);
	REQUIRE(eval_expression<bool>("true or false") == true);
	REQUIRE(eval_expression<bool>("false or true") == true);
	REQUIRE(eval_expression<bool>("false or false") == false);

	REQUIRE(eval_expression<bool>("true xor true") == false);
	REQUIRE(eval_expression<bool>("true xor false") == true);
	REQUIRE(eval_expression<bool>("false xor true") == true);
	REQUIRE(eval_expression<bool>("false xor false") == false);
}

TEST_CASE("Equality for booleans")
{
	REQUIRE(eval_expression<bool>("true == true") == true);
	REQUIRE(eval_expression<bool>("true == false") == false);
	REQUIRE(eval_expression<bool>("true != true") == false);
	REQUIRE(eval_expression<bool>("true != false") == true);
	REQUIRE(eval_expression<bool>("false == false") == true);
	REQUIRE(eval_expression<bool>("false == true") == false);
	REQUIRE(eval_expression<bool>("false != false") == false);
	REQUIRE(eval_expression<bool>("false != true") == true);
}

TEST_CASE("if expression")
{
	REQUIRE(eval_expression<int>("if (true) 1 else 2") == 1);
	REQUIRE(eval_expression<int>("if (false) 1 else 2") == 2);
}

TEST_CASE("Slightly more complex if expressions")
{
	REQUIRE(eval_expression<int>("if (1 < 5) 1 + 3 * 2 else 2 - 1") == 1 + 3 * 2);
	REQUIRE(eval_expression<float>("if (2 != 2) 1.3 + 3.1 * 2.9 else 2.3 - 1.123") == 2.3f - 1.123f);
}

TEST_CASE("Fibonacci just for the fun of it")
{
	auto const src = R"(
		let fib = fn (int i) -> int
		{
			return if (i <= 1) i else fib(i - 1) + fib(i - 2);
		};
	)"sv;

	interpreter::ProgramStack stack;
	alloc_stack(stack, 2048);
	Program program;
	parse_statement(src, program);

	REQUIRE(eval_expression<int>("fib(0)", stack, program) == 0);
	REQUIRE(eval_expression<int>("fib(1)", stack, program) == 1);
	REQUIRE(eval_expression<int>("fib(2)", stack, program) == 1);
	REQUIRE(eval_expression<int>("fib(3)", stack, program) == 2);
	REQUIRE(eval_expression<int>("fib(4)", stack, program) == 3);
	REQUIRE(eval_expression<int>("fib(5)", stack, program) == 5);
	REQUIRE(eval_expression<int>("fib(6)", stack, program) == 8);
	REQUIRE(eval_expression<int>("fib(7)", stack, program) == 13);
	REQUIRE(eval_expression<int>("fib(8)", stack, program) == 21);
	REQUIRE(eval_expression<int>("fib(9)", stack, program) == 34);
	REQUIRE(eval_expression<int>("fib(10)", stack, program) == 55);
}

TEST_CASE("Declaring variables with let")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	Program program;
	run_statement("let i = 7 * 6 / 2 - 50;", stack, program.global_scope);
	run_statement("let f = 3.141592;", stack, program.global_scope);

	REQUIRE(eval_expression<int>("i", stack, program) == 7 * 6 / 2 - 50);
	REQUIRE(eval_expression<float>("f", stack, program) == 3.141592f);
}

auto parse_and_run(std::string_view src) noexcept -> int
{
	Program const program = parser::parse_source(src);
	return interpreter::run(program);
}

TEST_CASE("main function")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return 0;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

auto fib(int i) -> int
{
	if (i <= 1)
		return i;
	else
		return fib(i - 1) + fib(i - 2);
};

TEST_CASE("Main function that calls another function.")
{
	auto const src = R"(
		let fib = fn (int i) -> int
		{
			return if (i <= 1) i else fib(i - 1) + fib(i - 2);
		};		

		let main = fn () -> int
		{
			let i = fib(5);
			let j = fib(8);

			let difference = fn (int i, int j) -> int
			{
				return 
					if (i > j)
						i - j
					else
						j - i;
			};

			return difference(i, j);
		};
	)"sv;

	auto const difference = [](int i, int j)
	{
		if (i > j)
			return i - j;
		else
			return j - i;
	};

	int const i = fib(5);
	int const j = fib(8);
	REQUIRE(parse_and_run(src) == difference(i, j));
}

TEST_CASE("C++ comments")
{
	auto const src = R"(
		// Fibonacci function. Computes the ith number of the Fibonacci sequence.
		let fib = fn (int i) -> int
		{
			return if (i <= 1) i else fib(i - 1) + fib(i - 2);
		};		

		// Main function. Computes the difference between the 8th and the 5th Fibonacci numbers.
		let main = fn () -> int
		{
			let i = fib(5);
			let j = fib(8);

			// Returns the absolute value of the subtraction of i and j.
			let difference = fn (int i, int j) -> int
			{
				return 
					if (i > j)
						i - j
					else
						j - i;
			};

			return difference(i, j);
		};
	)"sv;

	auto const difference = [](int i, int j)
	{
		if (i > j)
			return i - j;
		else
			return j - i;
	};

	int const i = fib(5);
	int const j = fib(8);
	REQUIRE(parse_and_run(src) == difference(i, j));
}

TEST_CASE("C comments")
{
	auto const src = R"(
		/* Fibonacci function. Computes the ith number of the Fibonacci sequence. */
		let fib = fn (int i) -> int
		{
			return if (i <= 1) i else fib(i - 1) + fib(i - 2);
		};		

		/* Main function. Computes the
		   difference between the 8th
		   and the 5th Fibonacci numbers. */
		let main = fn () -> int
		{
			let i = fib(5);
			let j = fib(8);

			/* Returns the absolute value of the subtraction of i and j. */
			let difference = fn (int i, int j) -> int
			{
				return 
					if (i > /* Comments here just because I can */j)
						i - j
					else
						j - i;
			};

			return difference( /* Comments*/ /*here*/ /*just*/ //because 

		/*I*/   /*can*/ i, j);
		};
	)"sv;

	auto const difference = [](int i, int j)
	{
		if (i > j)
			return i - j;
		else
			return j - i;
	};

	int const i = fib(5);
	int const j = fib(8);
	REQUIRE(parse_and_run(src) == difference(i, j));
}

TEST_CASE("Accessing global variables from main function")
{
	auto const src = R"(
		let i = 5;		
		
		let main = fn () -> int
		{
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Block expression")
{
	auto const src =
		"{"
		"    int i = 3;"
		"    int j = 4;"
		"    return i * i + j * j;"
		"}";

	REQUIRE(eval_expression<int>(src) == 3 * 3 + 4 * 4);
}

TEST_CASE("Block expressions with multiple return paths")
{
	auto const src =
		"{"
		"    int i = 3;"
		"    int j = 4;"
		"    if (i > j)"
		"		 return i * i - j * j;"
		"	 else"
		"		 return j * j - i * i;"
		"}";

	REQUIRE(eval_expression<int>(src) == 4 * 4 - 3 * 3);
}

TEST_CASE("A more imperative fibonacci")
{
	auto const src = R"(
		let fib = fn (int i) -> int
		{
			if (i <= 1) 
				return i; 
			else 
				return fib(i - 1) + fib(i - 2);
		};
	)"sv;

	interpreter::ProgramStack stack;
	alloc_stack(stack, 2048);
	Program program;
	parse_statement(src, program);

	REQUIRE(eval_expression<int>("fib(0)", stack, program) == 0);
	REQUIRE(eval_expression<int>("fib(1)", stack, program) == 1);
	REQUIRE(eval_expression<int>("fib(2)", stack, program) == 1);
	REQUIRE(eval_expression<int>("fib(3)", stack, program) == 2);
	REQUIRE(eval_expression<int>("fib(4)", stack, program) == 3);
	REQUIRE(eval_expression<int>("fib(5)", stack, program) == 5);
	REQUIRE(eval_expression<int>("fib(6)", stack, program) == 8);
	REQUIRE(eval_expression<int>("fib(7)", stack, program) == 13);
	REQUIRE(eval_expression<int>("fib(8)", stack, program) == 21);
	REQUIRE(eval_expression<int>("fib(9)", stack, program) == 34);
	REQUIRE(eval_expression<int>("fib(10)", stack, program) == 55);
}

TEST_CASE("Accessing a variable from outside the block")
{
	auto const src = R"(
		let foo = fn (int i, int j) -> int
		{
			if (i > j)
			{
				int difference = i - j;
				return difference;
			}
			else
			{
				int difference = j - i;
				return difference;
			}
		};		

		let main = fn () -> int
		{
			return foo(6, 10);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 10 - 6);
}

TEST_CASE("Ensure that variables inside the block do not share an address with the variables in the function")
{
	auto const src = R"(
		let foo = fn (int i, int j) -> int
		{
			if (i > j)
			{
				int difference = i - j;
				return difference + i + j;
			}
			else
			{
				int difference = j - i;
				return difference + i + j;
			}
		};		

		let main = fn () -> int
		{
			return foo(6, 10);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 10 - 6 + 10 + 6);
}

TEST_CASE("integer assignment")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int mut i = 5;
			i = 6;
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 6);
}

TEST_CASE("while loop")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int i = 1;
			int sum = 0;
			while (i < 10)
			{
				sum = sum + i;
				i = i + 1;
			}
			return sum;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
}

TEST_CASE("for loop")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int sum = 0;
			for (int i = 1; i < 10; i = i + 1)
				sum = sum + i;

			return sum;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
}

TEST_CASE("break")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int sum = 0;
			for (int i = 1; i < 10; i = i + 1)
			{
				sum = sum + i;
				if (i == 6)
					break;
			}

			return sum;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6);
}

TEST_CASE("continue")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int sum = 0;
			for (int i = 1; i < 10; i = i + 1)
			{
				// Ignore multiples of 3.
				if (i % 3 == 0)
					continue;
				sum = sum + i;
			}

			return sum;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1 + 2 + 4 + 5 + 7 + 8);
}

TEST_CASE("function that takes a reference")
{
	auto const src = R"(
		let assign = fn (int mut & a, int b) -> void
		{
			a = b;
		};	

		let main = fn () -> int
		{
			int mut i = 5;
			assign(i, 6);
			return i;
		};
	)"sv;

	parse_and_print(src);
	REQUIRE(parse_and_run(src) == 6);
}

TEST_CASE("Reference types in the stack")
{
	auto const src = R"(
		let main = fn () -> int
		{
			int mut i = 5;
			int mut & ri = i;
			ri = 6;		// Mutate through the reference
			return i;	// Return the original value
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 6);
}

//TEST_CASE("Returning references")
//{
//	auto const src = R"(
//		let id = fn (int mut & i) -> int mut & { return i; };
//	
//		let main = fn () -> int
//		{
//			int mut i = 0;
//			int mut & ri = id(i);
//			ri = -7;
//			return i;
//		};
//	)"sv;
//
//	REQUIRE(parse_and_run(src) == -7);
//}

/*****************************************************************
Backlog
- struct
- pointers
- templates
- arrays (depends on pointers)
- strings (depends on arrays)
- importing other files
- importing functions in C
- contracts
- errors
*****************************************************************/
