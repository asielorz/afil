#include <catch2/catch.hpp>
#include "utils/string.hh"

TEST_CASE("replace with single chars")
{
	std::string s = replace("some test string", "s", "z");
	REQUIRE(s == "zome tezt ztring");
}

TEST_CASE("replace with nothing")
{
	std::string s = replace("some test string", "s", "");
	REQUIRE(s == "ome tet tring");
}

TEST_CASE("replace of different sizes")
{
	std::string s = replace("some test string", "st", "asdf");
	REQUIRE(s == "some teasdf asdfring");
}

TEST_CASE("replace doesn't find anything")
{
	std::string s = replace("some test string", "foo", "bar");
	REQUIRE(s == "some test string");
}
