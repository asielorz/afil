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

auto program_main_(int argc, char const * const argv[]) -> int;

#undef main
auto main(int argc, char const * const argv[]) -> int
{
	int const unit_test_result = run_tests();

#ifdef UNIT_TESTS
	static_cast<void>(argc, argv);
	return unit_test_result;
#else
	if (unit_test_result != 0)
		return unit_test_result;
	else
		return program_main_(argc, argv);
#endif
}
