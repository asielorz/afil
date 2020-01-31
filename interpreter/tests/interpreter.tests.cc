#include <catch2/catch.hpp>
#include "interpreter.hh"
#include "program.hh"
#include "parser.hh"
#include "template_instantiation.hh"

using namespace std::literals;

auto parse_and_run(std::string_view src) noexcept -> int
{
	complete::Program const program = instantiation::instantiate_templates(parser::parse_source(src));
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
