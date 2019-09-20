#pragma once

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

auto run_tests() noexcept -> int
{
	int const result = Catch::Session().run();
	if (result != 0)
		system("pause");
	return result;
}

#ifdef UNIT_TESTS
int main()
{
	return run_tests();
}
#endif
