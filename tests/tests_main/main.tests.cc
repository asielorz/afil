#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include "utils/compatibility.hh"

#if AFIL_WINDOWS
#	include <Windows.h>
#endif

static void message_box(char const * title, char const * message) noexcept
{
#if AFIL_WINDOWS
	::MessageBoxA(nullptr, message, title, MB_OK | MB_ICONINFORMATION);

#else
	constexpr char title_color[] = "\u001b[41;27m";
	constexpr char message_color[] = "\u001b[31m";
	constexpr char reset_color[] = "\u001b[0m";

	std::fprintf(stderr, "%s%s:%s\n%s%s%s\n", title_color, title, reset_color, message_color, message, reset_color);
#endif
}

auto run_tests(int argc, char const * const argv[]) noexcept -> int
{
	int const result = Catch::Session().run(argc, argv);
	if (result != 0)
	{
		message_box("A test failed", "One of the tests failed. Look at the console for more information.");
		system_pause();
	}
	return result;
}

auto main(int argc, char const * const argv[]) -> int
{
	return run_tests(argc, argv);
}
