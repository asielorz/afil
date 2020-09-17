#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include "utils/compatibility.hh"

#if AFIL_WINDOWS
#	include <Windows.h>
#endif

#include <cstring>
#include <cstdio>

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

static auto run_tests(int argc, char const * argv[]) noexcept -> int
{
	bool interactive = false;
	for (int i = 1; i < argc; ++i)
	{
		if (std::strcmp("--afil-interactive-tests", argv[i]) != 0)
			continue;

		interactive = true;

		for (int j = i; j < argc; ++j)
			argv[j] = argv[j + 1];

		--argc;
		break;
	}

	int const result = Catch::Session().run(argc, argv);
	if (result != 0 && interactive)
	{
		message_box("A test failed", "One of the tests failed. Look at the console for more information.");
		system_pause();
	}
	return result;
}

auto main(int argc, char const * argv[]) -> int
{
	return run_tests(argc, argv);
}
