#include <catch2/catch.hpp>
#include "value_ptr.hh"

TEST_CASE("value_ptr inherits constructors from unique_ptr")
{
	auto p = value_ptr<int>(new int(4));
	REQUIRE(*p == 4);
}
