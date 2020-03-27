#include "unreachable.hh"
#include <cstdlib>
#include <cstdio>
#include <Windows.h>

[[noreturn]] auto declare_unreachable() noexcept -> void
{
	int const ret = ::MessageBoxA(nullptr, "Control flow is not allowed to reach this point", "Unreachable code", MB_RETRYCANCEL | MB_ICONERROR);
	if (ret == IDRETRY)
		__debugbreak();
	else
		quick_exit(1);
}

[[noreturn]] auto mark_as_to_do() noexcept -> void
{
	int const ret = ::MessageBoxA(nullptr, "This part of the code has not been implemented yet.", "TODO", MB_RETRYCANCEL | MB_ICONERROR);
	if (ret == IDRETRY)
		__debugbreak();
	else
		quick_exit(1);
}

[[noreturn]] auto mark_as_to_do(char const what[]) noexcept -> void
{
	char buffer[1024];
	sprintf_s(buffer, "This part of the code (%s) has not been implemented yet.", what);
	int const ret = ::MessageBoxA(nullptr, buffer, "TODO", MB_RETRYCANCEL | MB_ICONERROR);
	if (ret == IDRETRY)
		__debugbreak();
	else
		quick_exit(1);
}
