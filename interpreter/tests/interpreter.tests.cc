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

	TypeId global_return_type = TypeId::none;
	auto const expr_tree = parser::parse_expression(lex::tokenize(src), {program, scope_stack, global_return_type});
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

auto run_statement(std::string_view src, interpreter::ProgramStack & stack, Program & program, Scope & scope) noexcept -> void
{
	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	if (&scope != &program.global_scope)
		scope_stack.push_back({&scope, ScopeType::function});
	TypeId type = TypeId::none;
	interpreter::run_statement(*parser::parse_statement(lex::tokenize(src), {program, scope_stack, type}), stack, program, 0);
}

auto pretty_print_expr(std::string_view source, Program & program)
{
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	TypeId type = TypeId::none;
	printf("%s", pretty_print(parser::parse_expression(lex::tokenize(source), {program, scope_stack, type}), program).c_str());
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
	Program program;
	Function function;
	run_statement("int i = 5;", stack, program, function);
	run_statement("int foo = 5 + 6 - 3 * 2 + 8 / 3;", stack, program, function);

	REQUIRE(function.variables.size() == 2);
	REQUIRE(get(program, function.variables[0].name) == "i");
	REQUIRE(get(program, function.variables[1].name) == "foo");
	REQUIRE(read_word(stack, stack.base_pointer + function.variables[0].offset) == 5);
	REQUIRE(read_word(stack, stack.base_pointer + function.variables[1].offset) == 5 + 6 - 3 * 2 + 8 / 3);
}

TEST_CASE("A variable can be accessed from expressions after it is declared")
{
	interpreter::ProgramStack stack;
	alloc_stack(stack, 128);
	Program program;
	run_statement("int i = 30;", stack, program, program.global_scope);
	int const i = 30;
	REQUIRE(eval_expression<int>("i", stack, program) == i);
	REQUIRE(eval_expression<int>("i * i - 4 + i / 6 - 3 * i", stack, program) == i * i - 4 + i / 6 - 3 * i);
}

auto parse_expression(std::string_view src, Program & program) noexcept -> void
{
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	TypeId global_return_type = TypeId::none;
	parser::parse_expression(lex::tokenize(src), {program, scope_stack, global_return_type});
}

auto parse_statement(std::string_view source, Program & program) noexcept -> void
{
	ScopeStack scope_stack;
	scope_stack.push_back({ &program.global_scope, ScopeType::global });
	TypeId global_return_type = TypeId::none;
	parser::parse_statement(lex::tokenize(source), {program, scope_stack, global_return_type});
}

TEST_CASE("Identity function expression")
{
	Program program;
	parse_expression("fn (int x) -> int { return x; }", program);
}

TEST_CASE("Use let to name a function")
{
	Program program;
	parse_statement("let id = fn (int x) -> int { return x; };", program);

	REQUIRE(program.functions.size() == 1);
	ScopeStack scope_stack;
	scope_stack.push_back({&program.global_scope, ScopeType::global});
	REQUIRE(std::get<lookup_result::OverloadSet>(lookup_name(scope_stack, "id", program.string_pool)).function_ids.size() == 1);
	REQUIRE(program.functions[0].variables.size() == 1);
	REQUIRE(program.functions[0].parameter_count == 1);
	REQUIRE(get(program, program.functions[0].variables[0].name) == "x");
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
	run_statement("float f = 5.0;", stack, program, program.global_scope);

	REQUIRE(program.global_scope.variables.size() == 1);
	REQUIRE(get(program, program.global_scope.variables[0].name) == "f");
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
	program.global_scope.functions.push_back(FunctionName{pool_string(program, "add"), FunctionId{1, 0}});

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
	run_statement("bool t = true;", stack, program, program.global_scope);
	run_statement("bool f = false;", stack, program, program.global_scope);

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
	run_statement("let i = 7 * 6 / 2 - 50;", stack, program, program.global_scope);
	run_statement("let f = 3.141592;", stack, program, program.global_scope);

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
			int mut i = 1;
			int mut sum = 0;
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
			int mut sum = 0;
			for (int mut i = 1; i < 10; i = i + 1)
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
			int mut sum = 0;
			for (int mut i = 1; i < 10; i = i + 1)
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
			int mut sum = 0;
			for (int mut i = 1; i < 10; i = i + 1)
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

TEST_CASE("Returning references")
{
	auto const src = R"(
		let id = fn (int mut & i) -> int mut & { return i; };
	
		let main = fn () -> int
		{
			int mut i = 0;
			int mut & ri = id(i);
			ri = 0 - 7;
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -7);
}

TEST_CASE("Deducing return type of functions")
{
	auto const src = R"(
		let id = fn (int i) { return i; };
	
		let main = fn () -> int
		{
			return id(5);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Negative numbers")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return -5;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -5);
}

TEST_CASE("Negation operator")
{
	auto const src = R"(
		let main = fn () -> int
		{
			if (not (3 < 4))
				return 5;
			else
				return -5;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -5);
}

TEST_CASE("Struct")
{
	auto const src = R"(
		struct vec2
		{
			int x;
			int y;
		}
	)"sv;

	parser::parse_source(src);
}

TEST_CASE("A struct can be used inside another struct")
{
	auto const src = R"(
		struct vec2
		{
			float x;
			float y;
		}

		struct AABB
		{
			vec2 min;
			vec2 max;
		}
	)"sv;

	parser::parse_source(src);
}

TEST_CASE("A struct can be constructed by giving values to each member")
{
	auto const src = R"(
		struct vec2
		{
			float x;
			float y;
		}

		let main = fn() -> int
		{
			let v = vec2(3.5, 2.22);
		};
	)"sv;

	parser::parse_source(src);
}

TEST_CASE("Member access")
{
	auto const src = R"(
		struct ivec2
		{
			int x;
			int y;
		}

		let main = fn() -> int
		{
			let v = ivec2(3, -7);
			return v.x + v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 + -7);
}

TEST_CASE("Mutating a member")
{
	auto const src = R"(
		struct ivec2
		{
			int x;
			int y;
		}

		let main = fn() -> int
		{
			let mut v = ivec2(3, -7);
			v.x = 5;
			return v.x + v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5 + -7);
}

TEST_CASE("Conversions between reference types of different mutability")
{
	Program program;
	TypeId const int_ = TypeId::int_;
	TypeId const int_ref = make_reference(int_);
	TypeId const int_mut_ref = make_mutable(int_ref);

	// Conversion to itself.
	REQUIRE(is_convertible(int_, int_, program));
	REQUIRE(is_convertible(int_ref, int_ref, program));
	REQUIRE(is_convertible(int_mut_ref, int_mut_ref, program));

	// Reference to T.
	REQUIRE(is_convertible(int_ref, int_, program));
	REQUIRE(is_convertible(int_mut_ref, int_, program));

	// Mutable reference to reference
	REQUIRE(is_convertible(int_mut_ref, int_ref, program));
	REQUIRE(!is_convertible(int_ref, int_mut_ref, program));
	REQUIRE(!is_convertible(int_, int_mut_ref, program));
}

TEST_CASE("Member access to an rvalue")
{
	auto const src = R"(
		struct ivec2
		{
			int x;
			int y;
		}

		let main = fn() -> int
		{
			return ivec2(2, 5).y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Member access to the result of a function")
{
	auto const src = R"(
		struct ivec2
		{
			int x;
			int y;
		}

		let make_vector = fn(int x, int y)
		{
			return ivec2(x, y);
		};

		let main = fn() -> int
		{
			return make_vector(2, 5).y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Designated initializers")
{
	auto const src = R"(
		struct ivec2
		{
			int x;
			int y;
		}

		let main = fn() -> int
		{
			let v = ivec2(.y = 3, .x = 7);
			return v.x - v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 7 - 3);
}

TEST_CASE("Default values for struct members")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}

		let main = fn() -> int
		{
			let v = ivec2(.y = 5);
			return v.x - v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0 - 5);
}

TEST_CASE("Default constructor for structs that have a default value for all members")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}

		let main = fn() -> int
		{
			let v = ivec2();
			return v.x + v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0 + 0);
}

TEST_CASE("Structs with default constructor do not need a default value for the type that contains them to be default constructible")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}
		struct aabb
		{
			// aabb is default constructible because ivec2 is.
			ivec2 min;
			ivec2 max;
		}

		let main = fn() -> int
		{
			let box = aabb();
			return box.min.x;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

TEST_CASE("Operator overloading")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}
		let (+) = fn(ivec2 a, ivec2 b)
		{
			return ivec2(a.x + b.x, a.y + b.y);
		};

		let main = fn() -> int
		{
			let v = ivec2(4, 5) + ivec2(-1, 3);
			return v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 8);
}

TEST_CASE("Pointers")
{
	auto const src = R"(
		let main = fn() -> int
		{
			int mut i = 5;
			int mut * pi = &i;
			*pi = 6;
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 6);
}

TEST_CASE("Conversion from immutable pointer to mutable pointer")
{
	auto const src = R"(
		let main = fn() -> int
		{
			int mut i = 5;
			int * pi = &i;
			return *pi;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("A function template lets the user define generic functions")
{
	auto const src = R"(
		let add = fn<T>(T a, T b) 
		{ 
			return a + b; 
		};

		let main = fn() -> int
		{
			return add(-3, 4);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1);
}

TEST_CASE("A template parameter may be a reference or mutable")
{
	auto const src = R"(
		let assign = fn<T>(T mut & a, T b) 
		{ 
			a = b; 
		};

		let main = fn() -> int
		{
			int mut i = 5;
			assign(i, -225);
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -225);
}

TEST_CASE("A structure template lets the user define generic structures")
{
	auto const src = R"(
		struct<T, U> pair
		{
			T first;
			U second;
		}
		
		let main = fn() -> int
		{
			let p = pair<int, int>(3, 4);
			return p.first + p.second;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 + 4);
}

TEST_CASE("A structure may contain a variable of a template type")
{
	auto const src = R"(
		struct<T> tvec2
		{
			T x;
			T y;
		}
		
		struct aabb
		{
			tvec2<int> min = tvec2<int>(0, 0);
			tvec2<int> max = tvec2<int>(0, 0);
		}
		
		let main = fn() -> int
		{
			aabb box;
			return box.min.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

TEST_CASE("Statement block in the default value of a member variable")
{
	auto const src = R"(
		struct test
		{
			int x = {
				int i = 3;
				int j = 4;
				return i * i + j * j;
			};
		}
		
		let main = fn() -> int
		{
			test t;
			return t.x;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 * 3 + 4 * 4);
}

TEST_CASE("Function template refactor: variable nodes")
{
	auto const src = R"(
		let identity = fn<T>(T x) 
		{ 
			return x; 
		};

		let main = fn() -> int
		{
			return identity(1024);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 1024);
}

TEST_CASE("Function template refactor: function call nodes")
{
	auto const src = R"(
		let add = fn<T>(T a, T b) 
		{ 
			return a + b; 
		};

		let main = fn() -> int
		{
			return add(3, 6);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 9);
}

TEST_CASE("Function template refactor: operator call nodes")
{
	auto const src = R"(
		let eq = fn<T>(T a, T b) 
		{ 
			return a == b; 
		};

		let main = fn() -> int
		{
			if (eq(3, 4))
				return 5;
			else
				return -5;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -5);
}

TEST_CASE("Function template refactor: relational operator call nodes")
{
	auto const src = R"(
		let less = fn<T>(T a, T b) 
		{ 
			return a < b; 
		};

		let main = fn() -> int
		{
			if (less(3, 4))
				return 5;
			else
				return -5;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Function template refactor: if expression")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			return 
				if (a < b)
					b - a
				else
					a - b;
		};

		let main = fn() -> int
		{
			return difference(-4, 10);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: if statement")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			if (a < b)
				return b - a;
			else
				return a - b;
		};

		let main = fn() -> int
		{
			return difference(-4, 10);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration statement of a non dependent type")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			bool less = a < b;
			if (less)
				return b - a;
			else
				return a - b;
		};

		let main = fn() -> int
		{
			return difference(-4, 10);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration statement of a non dependent type, round 2")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			bool less = a < b;
			if (less)
				return b - a;
			else
				return a - b;
		};

		let main = fn() -> int
		{
			return difference(10, -4);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration of dependent type")
{
	auto const src = R"(
		let midpoint = fn<T>(T a, T b) 
		{ 
			T m = (a + b) / 2;
			return m;
		};

		let main = fn() -> int
		{
			return midpoint(0, 10);
		};
	)"sv;

	auto const midpoint = [](auto a, auto b) 
	{
		return (a + b) / 2;
	};

	REQUIRE(parse_and_run(src) == midpoint(0, 10));
}

TEST_CASE("Function template refactor: synthesizing default constructor for variable declaration of dependent type")
{
	auto const src = R"(
		let return_default = fn<T>(T ignored) 
		{ 
			T default_constructed;
			return default_constructed;
		};

		struct DefaultConstructible
		{
			int value = 5;
		}		

		let main = fn() -> int
		{
			let x = DefaultConstructible(7);
			let y = return_default(x);
			return y.value;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Function template refactor: struct constructor of non-dependent type")
{
	auto const src = R"(
		struct ivec3
		{
			int x;
			int y;
			int z;
		}
		
		let make_vector = fn<T>(T x, T y, T z) 
		{ 
			return ivec3(x, y, z);
		};

		let main = fn() -> int
		{
			let v = make_vector(1, 2, 3);
			return v.y;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 2);
}

TEST_CASE("Structs are assignable by default")
{
	auto const src = R"(
		struct Foo
		{
			int x;
		}

		let main = fn() -> int
		{
			Foo mut f = Foo(5);
			f = Foo(6);
			return f.x;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 6);
}

TEST_CASE("Function template refactor: struct constructor of dependent type")
{
	auto const src = R"(
		struct Foo
		{
			int x = 0;
		}
		struct Bar
		{
			int a = 0;
		}

		let assign_constructed_from_int = fn<T>(T mut & x, int val)
		{
			x = T(val);
		};

		let main = fn() -> int
		{
			Foo mut f;
			Bar mut b;
			assign_constructed_from_int(f, 6);
			assign_constructed_from_int(b, 2);
			return f.x + b.a;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 8);
}

TEST_CASE("Function template refactor: Member access to dependent type")
{
	auto const src = R"(
		struct Foo
		{
			int x;
		}
		struct ivec3
		{
			int x;
			int y;
			int z;
		}

		let get_x = fn<T>(T t)
		{
			return t.x;
		};

		let main = fn() -> int
		{
			let f = Foo(6);
			let v = ivec3(1, 2, 3);
			return get_x(f) - get_x(v);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}

TEST_CASE("Member default initializer for struct templates")
{
	auto const src = R"(
		struct<T, U> test_pair
		{
			T first = -4;
			U second = -3;
		}

		let main = fn() -> int
		{
			let p = test_pair<int, int>();
			return p.first + p.second;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -7);
}

TEST_CASE("Default construct struct template without let = syntax")
{
	auto const src = R"(
		struct<T, U> test_pair
		{
			T first = -4;
			U second = -3;
		}

		let main = fn() -> int
		{
			test_pair<int, int> p;
			return p.first + p.second;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == -7);
}

TEST_CASE("For loops in dependent contexts")
{
	auto const src = R"(
		let some_sum = fn<T>(T first, T last, T step)
		{
			T mut sum = first;
			for (T mut i = first + step; i < last; i = i + step)
				sum = sum + i;
			return sum;
		};

		let main = fn() -> int
		{
			return some_sum(0, 10, 1);
		};
	)"sv;

	auto const some_sum = [](auto first, auto last, auto step)
	{
		auto sum = first;
		for (auto i = first + step; i < last; i = i + step)
			sum = sum + i;
		return sum;
	};

	REQUIRE(parse_and_run(src) == some_sum(0, 10, 1));
}

TEST_CASE("Statement blocks in dependent contexts")
{
	auto const src = R"(
		let add = fn<T>(T a, T b)
		{
			{
				return a + b;
			}
		};

		let main = fn() -> int
		{
			return add(3, 5);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 + 5);
}

TEST_CASE("Statement block expressions in dependent contexts")
{
	auto const src = R"(
		let add = fn<T>(T a, T b)
		{
			return {
				T needless_copy_1 = a;
				T needless_copy_2 = b;
				return needless_copy_1 + needless_copy_2;
			};
		};

		let main = fn() -> int
		{
			return add(3, 5);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 + 5);
}

TEST_CASE("Recursive dependent types")
{
	auto const src = R"(
		let dereference = fn<T>(T * p)
		{
			return *p;
		};

		let main = fn() -> int
		{
			int x = 25;
			return dereference(&x);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 25);
}

TEST_CASE("Declaring an array")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let a = int[4](1, 2, 3, 4);
			return 0;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

TEST_CASE("Default constructed array")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}

		let main = fn() -> int
		{
			ivec2[4] a;
			return 0;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

TEST_CASE("User can overload subscript operator for their type")
{
	auto const src = R"(
		struct ivec4
		{
			int x = 0;
			int y = 0;
			int z = 0;
			int w = 0;
		}

		let ([]) = fn(ivec4 v, int i)
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let main = fn() -> int
		{
			let v = ivec4(1, 3, 5, 7);
			return v[3] - v[1];
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 4);
}

TEST_CASE("Can overload based on mutability")
{
	auto const src = R"(
		struct ivec4
		{
			int x = 0;
			int y = 0;
			int z = 0;
			int w = 0;
		}

		let ([]) = fn(ivec4 & v, int i) -> int &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let ([]) = fn(ivec4 mut & v, int i) -> int mut &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let main = fn() -> int
		{
			let v = ivec4(1, 3, 5, 7);
			let mut mv = ivec4();

			for (int mut i = 0; i < 4; i = i + 1)
				mv[i] = v[i];

			return mv[3] - mv[1];
		};

	)"sv;

	REQUIRE(parse_and_run(src) == 4);
}

TEST_CASE("Can overload based on mutability independent of order")
{
	auto const src = R"(
		struct ivec4
		{
			int x = 0;
			int y = 0;
			int z = 0;
			int w = 0;
		}

		let ([]) = fn(ivec4 mut & v, int i) -> int mut &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let ([]) = fn(ivec4 & v, int i) -> int &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let main = fn() -> int
		{
			let v = ivec4(1, 3, 5, 7);
			let mut mv = ivec4();

			for (int mut i = 0; i < 4; i = i + 1)
				mv[i] = v[i];

			return mv[3] - mv[1];
		};

	)"sv;

	REQUIRE(parse_and_run(src) == 4);
}

TEST_CASE("Subscript on dependent types")
{
	auto const src = R"(
		struct ivec4
		{
			int x = 0;
			int y = 0;
			int z = 0;
			int w = 0;
		}

		let ([]) = fn(ivec4 v, int i)
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let subscript = fn<T>(T & array, int i)
		{
			return array[i];
		};

		let main = fn() -> int
		{
			let v = ivec4(1, 3, 5, 7);
			return subscript(v, 3) - subscript(v, 0);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 6);
}

TEST_CASE("Multiple argument subscript")
{
	auto const src = R"(
		struct foo
		{
			int value;
		}

		let ([]) = fn(foo f, int i, int j)
		{
			return f.value + i + j;
		};

		let main = fn() -> int
		{
			let f = foo(3);
			return f[4, 2];
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 3 + 4 + 2);
}

TEST_CASE("Array of dependent type")
{
	auto const src = R"(
		let foo = fn<T>(T a, T b)
		{
			let results = T[4](a + b, a - b, a * b, a / b);
			return 0;
		};

		let main = fn() -> int
		{
			return foo(8, 4);
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 0);
}

//TEST_CASE("Subscripting array types")
//{
//	auto const src = R"(
//		let main = fn() -> int
//		{
//			let a = int[4](1, 2, 3, 4);
//			return a[0] + a[1] + a[2] + a[3];
//		};
//	)"sv;
//
//	REQUIRE(parse_and_run(src) == 10);
//}

/*****************************************************************
Backlog
- templates
	- struct declaration that contains dependent types
	- function declaration with dependent types
- arrays (depends on pointers)
- strings (depends on arrays)
- importing other files
- importing functions in C
- contracts
- concepts (depends on templates)
- errors
*****************************************************************/
