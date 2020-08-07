#include "unreachable.hh"
#include "utils/compatibility.hh"
#include <cstdlib>
#include <cstdio>

#if AFIL_WINDOWS
#	include <Windows.h>
#endif

namespace unreachable_locals
{
	[[noreturn]] static void handle_unreachable(char const * title, char const * message) noexcept
	{
#if AFIL_WINDOWS
		int const ret = ::MessageBoxA(nullptr, message, title, MB_RETRYCANCEL | MB_ICONERROR);
		if (ret == IDRETRY)
			__debugbreak();
		
		std::quick_exit(1);

#else
		constexpr char title_color[] = "\u001b[41;27m";
		constexpr char message_color[] = "\u001b[31m";
		constexpr char reset_color[] = "\u001b[0m";

		std::fprintf(stderr, "%s%s:%s\n%s%s%s", title_color, title, reset_color, message_color, message, reset_color);
		std::getchar();
		std::quick_exit(1);
#endif
	}
}

[[noreturn]] auto declare_unreachable() noexcept -> void
{
	unreachable_locals::handle_unreachable("Unreachable code", "Control flow is not allowed to reach this point");
}

[[noreturn]] auto mark_as_to_do() noexcept -> void
{
	unreachable_locals::handle_unreachable("TODO", "This part of the code has not been implemented yet.");
}

[[noreturn]] auto mark_as_to_do(char const what[]) noexcept -> void
{
	char buffer[1024];
	AFIL_SPRINTF(buffer, "This part of the code (%s) has not been implemented yet.", what);

	unreachable_locals::handle_unreachable("TODO", buffer);
}
