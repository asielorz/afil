#include <catch2/catch.hpp>
#include "interpreter.hh"
#include "incomplete_module.hh"
#include "parser.hh"
#include "template_instantiation.hh"
#include "afil.hh"
#include "program.hh"
#include "pretty_print.hh"
#include "utils/warning_macro.hh"
#include <iostream>

using namespace std::literals;

namespace tests
{
	auto error_string(interpreter::UnmetPrecondition) -> std::string
	{
		return "Unmet precondition";
	}

	template <typename T, typename Error>
	auto assert_get(expected<T, Error> e) -> T
	{
		if (!e.has_value())
		{
			INFO(error_string(e.error()));
			REQUIRE(e.has_value());
		}
		return std::move(*e);
	}

	template <typename Error>
	auto require_ok(expected<void, Error> e) -> void
	{
		if (!e.has_value())
		{
			INFO(error_string(e.error()));
			REQUIRE(e.has_value());
		}
	}

	auto parse_source(std::string_view src) -> expected<complete::Program, SyntaxError>
	{
		incomplete::Module module_for_source;
		module_for_source.files.push_back({"<source>", std::string(src)});
		try_call_void(parser::parse_modules({&module_for_source, 1}));
		return instantiation::semantic_analysis({&module_for_source, 1}, {0});
	}

	auto parse_and_run(std::string_view src) -> int
	{
		complete::Program const program = assert_get(parse_source(src));
		return assert_get(interpreter::run(program));
	}

	auto source_compiles(std::string_view src) noexcept -> bool
	{
		return parse_source(src).has_value();
	}

	auto parse_and_print(std::string_view src) -> void
	{
		complete::Program const program = assert_get(parse_source(src));
		printf("%s", pretty_print(program).c_str());
		system("pause");
	}
}

TEST_CASE("main function")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 0);
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
	REQUIRE(tests::parse_and_run(src) == difference(i, j));
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
	REQUIRE(tests::parse_and_run(src) == difference(i, j));
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
	REQUIRE(tests::parse_and_run(src) == difference(i, j));
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

	REQUIRE(tests::parse_and_run(src) == 5);
}

TEST_CASE("Block expression")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return {
				let i = 3;
				let j = 4;
				return i * i + j * j;
			};
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3 * 3 + 4 * 4);
}

TEST_CASE("Block expressions with multiple return paths")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return {
				let i = 3;
				let j = 4;
				if (i > j)
					return i * i - j * j;
				else
					return j * j - i * i;
			};
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 4 * 4 - 3 * 3);
}

TEST_CASE("Accessing a variable from outside the block")
{
	auto const src = R"(
		let foo = fn (int i, int j) -> int
		{
			if (i > j)
			{
				let difference = i - j;
				return difference;
			}
			else
			{
				let difference = j - i;
				return difference;
			}
		};		

		let main = fn () -> int
		{
			return foo(6, 10);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 10 - 6);
}

TEST_CASE("Ensure that variables inside the block do not share an address with the variables in the function")
{
	auto const src = R"(
		let foo = fn (int i, int j) -> int
		{
			if (i > j)
			{
				let difference = i - j;
				return difference + i + j;
			}
			else
			{
				let difference = j - i;
				return difference + i + j;
			}
		};		

		let main = fn () -> int
		{
			return foo(6, 10);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 10 - 6 + 10 + 6);
}

TEST_CASE("integer assignment")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut i = 5;
			i = 6;
			return i;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("while loop")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut i = 1;
			let mut sum = 0;
			while (i < 10)
			{
				sum = sum + i;
				i = i + 1;
			}
			return sum;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
}

TEST_CASE("for loop")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut sum = 0;
			for (let mut i = 1; i < 10; i = i + 1)
				sum = sum + i;

			return sum;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
}

TEST_CASE("break")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut sum = 0;
			for (let mut i = 1; i < 10; i = i + 1)
			{
				sum = sum + i;
				if (i == 6)
					break;
			}

			return sum;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6);
}

TEST_CASE("continue")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut sum = 0;
			for (let mut i = 1; i < 10; i = i + 1)
			{
				// Ignore multiples of 3.
				if (i % 3 == 0)
					continue;
				sum = sum + i;
			}

			return sum;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 4 + 5 + 7 + 8);
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
			let mut i = 5;
			assign(i, 6);
			return i;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Reference types in the stack")
{
	auto const src = R"(
		let main = fn () -> int
		{
			let mut i = 5;
			let mut & ri = i;
			ri = 6;		// Mutate through the reference
			return i;	// Return the original value
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Returning references")
{
	auto const src = R"(
		let id = fn (int mut & i) -> int mut & { return i; };
	
		let main = fn () -> int
		{
			let mut i = 0;
			let mut & ri = id(i);
			ri = 0 - 7;
			return i;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == -7);
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

	REQUIRE(tests::parse_and_run(src) == 5);
}

TEST_CASE("Negative numbers")
{
	auto const src = R"(
		let main = fn () -> int
		{
			return -5;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == -5);
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

	REQUIRE(tests::parse_and_run(src) == -5);
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

	REQUIRE(tests::source_compiles(src));
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

	REQUIRE(tests::source_compiles(src));
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
			return 0;
		};
	)"sv;

	REQUIRE(tests::source_compiles(src));
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

	REQUIRE(tests::parse_and_run(src) == 3 + -7);
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

	REQUIRE(tests::parse_and_run(src) == 5 + -7);
}

TEST_CASE("Conversions between reference types of different mutability")
{
	complete::Program program;
	complete::TypeId const int_ = complete::TypeId::int_;
	complete::TypeId const int_ref = make_reference(int_);
	complete::TypeId const int_mut_ref = make_mutable(int_ref);

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

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == 7 - 3);
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

	REQUIRE(tests::parse_and_run(src) == 0 - 5);
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

	REQUIRE(tests::parse_and_run(src) == 0 + 0);
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

	REQUIRE(tests::parse_and_run(src) == 0);
}

TEST_CASE("Operator overloading")
{
	auto const src = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}
		let operator+ = fn(ivec2 a, ivec2 b)
		{
			return ivec2(a.x + b.x, a.y + b.y);
		};

		let main = fn() -> int
		{
			let v = ivec2(4, 5) + ivec2(-1, 3);
			return v.y;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 8);
}

TEST_CASE("Pointers")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let mut i = 5;
			let pi = &i;
			*pi = 6;
			return i;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Conversion from immutable pointer to mutable pointer")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let mut i = 5;
			let pi = &i;
			return *pi;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == -3 + 4);
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
			let mut i = 5;
			assign(i, -225);
			return i;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == -225);
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

	REQUIRE(tests::parse_and_run(src) == 3 + 4);
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
			let box = aabb();
			return box.min.y;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 0);
}

TEST_CASE("Statement block in the default value of a member variable")
{
	auto const src = R"(
		struct test
		{
			int x = {
				let i = 3;
				let j = 4;
				return i * i + j * j;
			};
		}
		
		let main = fn() -> int
		{
			let t = test();
			return t.x;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3 * 3 + 4 * 4);
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

	REQUIRE(tests::parse_and_run(src) == 1024);
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

	REQUIRE(tests::parse_and_run(src) == 9);
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

	REQUIRE(tests::parse_and_run(src) == -5);
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

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == 14);
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

	REQUIRE(tests::parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration statement of a non dependent type")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			let less = a < b;
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

	REQUIRE(tests::parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration statement of a non dependent type, round 2")
{
	auto const src = R"(
		let difference = fn<T>(T a, T b) 
		{ 
			let less = a < b;
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

	REQUIRE(tests::parse_and_run(src) == 14);
}

TEST_CASE("Function template refactor: variable declaration of dependent type")
{
	auto const src = R"(
		let midpoint = fn<T>(T a, T b) 
		{ 
			let m = (a + b) / 2;
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

	REQUIRE(tests::parse_and_run(src) == midpoint(0, 10));
}

TEST_CASE("Function template refactor: synthesizing default constructor for variable declaration of dependent type")
{
	auto const src = R"(
		let return_default = fn<T>(T ignored) 
		{ 
			let default_constructed = T();
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

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == 2);
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
			let mut f = Foo(5);
			f = Foo(6);
			return f.x;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
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

	REQUIRE(tests::parse_and_run(src) == 5);
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

	REQUIRE(tests::parse_and_run(src) == -7);
}

TEST_CASE("For loops in dependent contexts")
{
	auto const src = R"(
		let some_sum = fn<T>(T first, T last, T step)
		{
			let mut sum = first;
			for (let mut i = first + step; i < last; i = i + step)
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

	REQUIRE(tests::parse_and_run(src) == some_sum(0, 10, 1));
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

	REQUIRE(tests::parse_and_run(src) == 3 + 5);
}

TEST_CASE("Statement block expressions in dependent contexts")
{
	auto const src = R"(
		let add = fn<T>(T a, T b)
		{
			return {
				let needless_copy_1 = a;
				let needless_copy_2 = b;
				return needless_copy_1 + needless_copy_2;
			};
		};

		let main = fn() -> int
		{
			return add(3, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3 + 5);
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
			let x = 25;
			return dereference(&x);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 25);
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

	REQUIRE(tests::parse_and_run(src) == 0);
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
			let a = ivec2[4]();
			return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 0);
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

		let operator[] = fn(ivec4 v, int i)
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

	REQUIRE(tests::parse_and_run(src) == 4);
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

		let operator[] = fn(ivec4 & v, int i) -> int &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let operator[] = fn(ivec4 mut & v, int i) -> int mut &
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

			for (let mut i = 0; i < 4; i = i + 1)
				mv[i] = v[i];

			return mv[3] - mv[1];
		};

	)"sv;

	REQUIRE(tests::parse_and_run(src) == 4);
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

		let operator[] = fn(ivec4 mut & v, int i) -> int mut &
		{
			if (i == 0) return v.x;
			if (i == 1) return v.y;
			if (i == 2) return v.z;
			return v.w;
		};

		let operator[] = fn(ivec4 & v, int i) -> int &
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

			for (let mut i = 0; i < 4; i = i + 1)
				mv[i] = v[i];

			return mv[3] - mv[1];
		};

	)"sv;

	REQUIRE(tests::parse_and_run(src) == 4);
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

		let operator[] = fn(ivec4 v, int i)
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

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Multiple argument subscript")
{
	auto const src = R"(
		struct foo
		{
			int value;
		}

		let operator[] = fn(foo f, int i, int j)
		{
			return f.value + i + j;
		};

		let main = fn() -> int
		{
			let f = foo(3);
			return f[4, 2];
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3 + 4 + 2);
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

	REQUIRE(tests::parse_and_run(src) == 0);
}

TEST_CASE("Array pointer type")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let a = int[4](1, 2, 3, 4);
			let pa = data(a);
			return pa[2];
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3);
}

TEST_CASE("Function template that takes array pointer")
{
	auto const src = R"(
		let subscript = fn<T>(T[] array, int i)
		{
			return array[i];
		};
	
		let main = fn() -> int
		{
			let a = int[4](1, 2, 3, 4);
			let pa = data(a);
			return subscript(pa, 2);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3);
}

TEST_CASE("Subscripting array types")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let a = int[4](1, 2, 3, 4);
			return a[0] + a[1] + a[2] + a[3];
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 10);
}

TEST_CASE("Subscripting array lvalues")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return int[4](1, 2, 3, 4)[3];
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 4);
}

TEST_CASE("size function for arrays")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let a = int[5](1, 2, 3, 4, 5);
			let mut sum = 0;
			for (let mut i = 0; i < size(a); i = i + 1)
				sum = sum + a[i];
			return sum;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 15);
}

TEST_CASE("String literals")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let s = "En un lugar de la Mancha";
			return size(s);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 24);
}

TEST_CASE("Declaring a struct inside a template")
{
	auto const src = R"(
		let make_pair = fn<T, U>(T t, U u)
		{
			struct pair
			{
				T first;
				U second;	
			}
			return pair(t, u);
		};

		let main = fn() -> int
		{
			let p = make_pair(3, "foo");
			return p.first + size(p.second);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Declaring a function inside a template")
{
	auto const src = R"(
		let is_sorted = fn<T>(T[] array, int n)
		{
			let compare = fn(T a, T b) { return a < b; };
			for (let mut i = 1; i < n; i = i + 1)
				if (compare(array[i], array[i - 1]))
					return false;

			return true;
		};

		let main = fn() -> int
		{
			let sorted_array = int[6](1, 2, 3, 3, 3, 4);
			let array_not_sorted = int[6](1, 4, 3, 3, 3, 4);

			let mut ret = 0;
			if (is_sorted(data(sorted_array), size(sorted_array)))
				ret = ret + 1;
			if (not is_sorted(data(array_not_sorted), size(array_not_sorted)))
				ret = ret + 2;

			return ret;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3);
}

TEST_CASE("import allows to import C functions from DLLs")
{
	auto const src = R"(
		let putchar = fn(char c) -> int 
			extern_symbol("putchar");

		let abs = fn(int x) -> int 
			extern_symbol("abs");

		let print_string = fn(char[] s, int n)
		{
			for (let mut i = 0; i < n; i = i + 1)
				putchar(s[i]);
		};

		let main = fn() -> int
		{
			let s = "Hello, world!";
			print_string(data(s), size(s));
			return abs(-5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
	putchar('\n');
}

TEST_CASE("Constant expressions")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let array = int[1 + 2](0);
			return size(array);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 3);
}

TEST_CASE("Variables with values known at compile time are constants and can be read in a constant expression")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let s = 5;
			let array = int[s](0);
			return size(array);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
} 

TEST_CASE("Constant variable of pointer type")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let s = 5;
			let ps = &s;
			let array = int[*ps](0);
			return size(array);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
}

TEST_CASE("Statement blocks in constant expressions")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let s = {
				let mut i = 3;
				i = i * 2;
				return i;
			};
			let array = int[s](0);
			return size(array);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 6);
}

TEST_CASE("Function call in constant expression")
{
	auto const src = R"(
		let int_sqrt = fn(int mut number) -> int
		{
			let mut result = 0;
 			let mut result_squared = 1;
			while (number >= result_squared)
			{
				number = number - result_squared;
				result = result + 1;
				result_squared = 1 + 2 * result;
 			}
			return result;
		};

		let main = fn() -> int
		{
			let s = int_sqrt(25);
			let array = int[s](0);
			return size(array);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
}

TEST_CASE("Bitwise and")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return 25 & 7;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == (25 & 7));
}

TEST_CASE("Bitwise or")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return 25 | 7;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == (25 | 7));
}

TEST_CASE("Bitwise xor")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return 25 ^ 7;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == (25 ^ 7));
}

TEST_CASE("Bitwise not")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return ~25;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == ~25);
}

TEST_CASE("Bitwise right shift")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return 25 >> 7;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == (25 >> 7));
}

TEST_CASE("Bitwise left shift")
{
	auto const src = R"(
		let main = fn() -> int
		{
			return 25 << 7;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == (25 << 7));
}

TEST_CASE("Forgetting a variable name will result in a compiler error")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let = 5;
			return 5;
		};
	)"sv;

	expected<complete::Program, SyntaxError> program = tests::parse_source(src);
	REQUIRE(!program.has_value());
}

TEST_CASE("There is no operator + for booleans")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let b = true + false;
			return 5;
		};
	)"sv;

	expected<complete::Program, SyntaxError> program = tests::parse_source(src);
	REQUIRE(!program.has_value());
}

TEST_CASE("Functions that take a template instantiation")
{
	auto const src = R"(
		struct<T> span
		{
			T[] data_;
			int size_;
		}
		
		let operator[] = fn<T>(span<T> s, int i) -> T &
		{
			return s.data_[i];
		};
		
		let main = fn() -> int
		{
			let array = int[3](1, 2, 3);
			let s = span<int>(data(array), size(array));
			return s[1];
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 2);
}

TEST_CASE("Higher order function templates")
{
	auto const src = R"(
		let invoke = fn<F, T>(F f, T a, T b)
		{
			return f(a, b);
		};
		
		let add = fn(int a, int b) { return a + b; };

		let main = fn() -> int
		{
			return invoke(add, 4, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 9);
}

TEST_CASE("Passing an overload set to a template")
{
	auto const src = R"(
		let invoke = fn<F, T>(F f, T a, T b)
		{
			return f(a, b);
		};
		
		let add = fn(int a, int b) { return a + b; };
		let add = fn(float a, float b) { return a + b; };

		let main = fn() -> int
		{
			return invoke(add, 4, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 9);
}

TEST_CASE("Passing a template to a template")
{
	auto const src = R"(
		let invoke = fn<F, T>(F f, T a, T b)
		{
			return f(a, b);
		};
		
		let add = fn<T>(T a, T b) { return a + b; };

		let main = fn() -> int
		{
			return invoke(add, 4, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 9);
}

TEST_CASE("Passing an operator overload set to a template")
{
	auto const src = R"(
		let invoke = fn<F, T>(F f, T a, T b)
		{
			return f(a, b);
		};
		
		let main = fn() -> int
		{
			return invoke(operator+, 4, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 9);
}

TEST_CASE("Deduction of array size")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let a = int[](1, 2, 3, 4, 5);

			return size(a);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
}

TODO("Uninit with let statements")
//TEST_CASE("uninit allows for not initializing an object")
//{
//	auto const src = R"(
//		let main = fn() -> int
//		{
//			let mut a = uninit;
//			return a;
//		};
//	)"sv;
//
//	tests::parse_and_run(src);
//}

TEST_CASE("Order of declarations doesn't matter")
{
	auto const src = R"(
		let B = fn() -> int
		{
			// A defined below!!!
			return A();
		};

		let A = fn() -> int
		{
			return 4;
		};

		let main = fn() -> int
		{
			return B();
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 4);
}

TEST_CASE("A program may be composed of several modules")
{
	auto const file1 = R"(
		struct ivec2
		{
			int x = 0;
			int y = 0;
		}
		let operator+ = fn(ivec2 a, ivec2 b)
		{
			return ivec2(a.x + b.x, a.y + b.y);
		};
	)"sv;

	auto const file2 = R"(
		let main = fn() -> int
		{
			let v = ivec2(4, 5) + ivec2(-1, 3);
			return v.y;
		};
	)"sv;

	incomplete::Module modules[2];
	modules[0].files.push_back({"ivec2.afil", std::string(file1)});
	modules[1].files.push_back({"main.afil", std::string(file2)});
	modules[1].dependencies.push_back(0);
	auto const parse_order = tests::assert_get(parser::parse_modules(modules));
	complete::Program const program = tests::assert_get(instantiation::semantic_analysis(modules, parse_order));

	REQUIRE(tests::assert_get(interpreter::run(program)) == 8);
}

TEST_CASE("A module cannot access symbols from a module it does not depend on")
{
	auto const file1 = R"(
		let add = fn(int a, int b) { return a + b; };
	)"sv;

	auto const file2 = R"(
		let subtract = fn(int a, int b) { add(a, -b); };
	)"sv;

	auto const file3 = R"(
		let main = fn() -> int
		{
			return subtract(5, 2);
		};
	)"sv;

	incomplete::Module modules[3];
	modules[0].files.push_back({     "add.afil", std::string(file1)});
	modules[1].files.push_back({"subtract.afil", std::string(file2)});
	modules[2].files.push_back({    "main.afil", std::string(file3)});

	modules[2].dependencies.push_back(0);
	modules[2].dependencies.push_back(1);
	// Module 1 does not depend on module 0, which it should.

	auto const parse_order = tests::assert_get(parser::parse_modules(modules));
	auto const program = instantiation::semantic_analysis(modules, parse_order);
	REQUIRE(!program.has_value());
}

TEST_CASE("Order of declarations doesn't matter for types either")
{
	auto const src = R"(
		let main = fn() -> int
		{
			let v = ivec2(3, 4);
			return v.x + v.y;
		};

		struct ivec2
		{
			int x;
			int y;
		}
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 7);
}

TEST_CASE("Passing a temporary by constant reference")
{
	auto const src = R"(
		let add = fn(int & a, int & b)
		{
			return a + b;
		};

		let main = fn() -> int
		{
			return add(4, 5);
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 9);
}

TEST_CASE("compiles is a built in function that checks if ")
{
	auto const src = R"(
		let main = fn() -> int
		{
			if (compiles{3 + 4})
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 7);
}

TEST_CASE("compiles returns false when given an ill formed expression")
{
	auto const src = R"(
		let main = fn() -> int
		{
			if (compiles{*3})
				return *3;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 0);
}

TEST_CASE("compiles can define names for using them inside its body")
{
	auto const src = R"(
		let main = fn() -> int
		{
			if (compiles(int i, int j){i + j})
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 7);
}

TEST_CASE("compiles can have any number of expressions, separated by semicolons. All must be true for the compiles expression to be true")
{
	auto const src1 = R"(
		let main = fn() -> int
		{
			if (compiles(int i, int j){i + j; i - j; i < j})
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src1) == 7);

	auto const src2 = R"(
		let main = fn() -> int
		{
			if (compiles(int i, int j){i + j; i - j; i[j]}) // Last one i[j] does not compile
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src2) == 0);
}

TEST_CASE("A compiles expression may specify what the return type of the return type of a expression must be")
{
	auto const src1 = R"(
		let main = fn() -> int
		{
			let condition = compiles(int i, int j)
			{
				{i + j} -> int;
				{i < j} -> bool
			};

			if (condition)
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src1) == 7);

	auto const src2 = R"(
		let main = fn() -> int
		{
			let condition = compiles(int i, int j)
			{
				{i + j} -> float; // Wrong
				{i < j} -> bool
			};

			if (condition)
				return 3 + 4;
			else
				return 0;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src2) == 0);
}

TEST_CASE("Contracts let defining preconditions for a function")
{
	auto const src = R"(
		let div = fn(int dividend, int divisor) -> int
			assert{divisor != 0;}
		{
			return dividend / divisor;
		};
	)"sv;

	REQUIRE(tests::source_compiles(src));
}

TEST_CASE("Running a function out of contract at runtime will stop execution and return an error")
{
	auto const src = R"(
		let div = fn(int dividend, int divisor) -> int
			assert{divisor != 0;}
		{
			return dividend / divisor;
		};

		let main = fn() -> int
		{
			return div(5, 0);
		};
	)"sv;

	auto const program = tests::parse_source(src);
	REQUIRE(program.has_value());
	auto const run_result = interpreter::run(*program);
	REQUIRE(!run_result.has_value());
}

TEST_CASE("Running a function out of contract at compile time is a compiler error")
{
	auto const src = R"(
		let div = fn(int dividend, int divisor) -> int
			assert{divisor != 0;}
		{
			return dividend / divisor;
		};

		let main = fn() -> int
		{
			let result = div(5, 0); // Result is a compile time constant
			return result;
		};
	)"sv;

	auto const program = tests::parse_source(src);
	REQUIRE(!program.has_value());
}

TEST_CASE("Type aliases")
{
	auto const src = R"(
		type also_int = int;
		
		let main = fn() -> also_int
		{
			return 5;
		};
	)"sv;

	REQUIRE(tests::parse_and_run(src) == 5);
}

//TEST_CASE("Functions that take types as parameters")
//{
//	auto const src = R"(
//		let is_ordered = fn(type t) -> bool
//		{
//			return compiles(t i, t j)
//			{
//				{i == j} -> bool;
//				{3 <=> 4} -> int
//			};
//		};
//
//		let main = fn() -> int
//		{
//			if (is_ordered(int))
//				return 1;
//			else
//				return 0;
//		};
//	)"sv;
//
//	REQUIRE(tests::parse_and_run(src) == 1);
//}

/*****************************************************************
Backlog
- concepts
- semantic analysis on templates based on concepts (depends on concepts)
- synthesizing arithmetic operators
- destructor and copy operations
- reflection
- namespaces
- closures and lambda captures
- currying (maybe, maybe at library level?)
- some minimalistic standard library
*****************************************************************/
