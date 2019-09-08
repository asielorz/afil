#include "unreachable.hh"
#include <cstdlib>
#include <Windows.h>

auto declare_unreachable() noexcept -> void
{
	int const ret = ::MessageBoxA(nullptr, "Control flow is not allowed to reach this point", "Unreachable code", MB_RETRYCANCEL | MB_ICONERROR);
	if (ret == IDRETRY)
		__debugbreak();
	else
		quick_exit(1);
}
