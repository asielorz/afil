#include <catch2/catch.hpp>
#include "interpreter.hh"
#include "program.hh"
#include "parser.hh"
#include "template_instantiation.hh"

using namespace std::literals;

namespace tests
{
	auto parse_and_run(std::string_view src) noexcept -> int
	{
		complete::Program const program = instantiation::instantiate_templates(parser::parse_source(src));
		return interpreter::run(program);
	}

	auto parse_source(std::string_view src) noexcept -> bool
	{
		complete::Program const program = instantiation::instantiate_templates(parser::parse_source(src));
		return true;
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
				int i = 3;
				int j = 4;
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
				int i = 3;
				int j = 4;
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

	REQUIRE(tests::parse_and_run(src) == 10 - 6);
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

	REQUIRE(tests::parse_and_run(src) == 10 - 6 + 10 + 6);
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

	REQUIRE(tests::parse_and_run(src) == 6);
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

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
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

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
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

	REQUIRE(tests::parse_and_run(src) == 1 + 2 + 3 + 4 + 5 + 6);
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
			int mut i = 5;
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
			int mut i = 5;
			int mut & ri = i;
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
			int mut i = 0;
			int mut & ri = id(i);
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

	REQUIRE(tests::parse_source(src));
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

	REQUIRE(tests::parse_source(src));
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

	REQUIRE(tests::parse_source(src));
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
