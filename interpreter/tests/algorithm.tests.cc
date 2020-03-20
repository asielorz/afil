#include "utils/algorithm.hh"
#include <catch2/catch.hpp>

TEST_CASE("filter_in_place removes all elements that do not satisfy an algorithm, in place")
{
	auto const is_even = [](size_t i) -> bool
	{
		return i % 2 == 0;
	};

	std::vector<size_t> indices(1000);
	std::iota(indices.begin(), indices.end(), size_t(0));

	erase_if(indices, is_even);
	REQUIRE(indices.size() == 500);
	for (size_t i : indices)
		REQUIRE(!is_even(i));
}
