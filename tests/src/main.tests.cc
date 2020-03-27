#pragma once

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

auto run_tests(int argc, char const * const argv[]) noexcept -> int
{
	int const result = Catch::Session().run(argc, argv);
	if (result != 0)
	{
		::MessageBoxA(nullptr, "One of the tests failed. Look at the console for more information.", "A test failed", MB_OK | MB_ICONINFORMATION);
		system("pause");
	}
	return result;
}

auto main(int argc, char const * const argv[]) -> int
{
	return run_tests(argc, argv);
}
