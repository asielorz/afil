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

TEST_CASE("Declaration and returning of a variable")
{
	auto const src = R"(
		let main = fn() -> int
		{
			int i = 5;
			return i;
		};
	)"sv;

	REQUIRE(parse_and_run(src) == 5);
}
