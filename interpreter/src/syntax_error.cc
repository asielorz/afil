#include "syntax_error.hh"
#include <cstdlib>
#include <Windows.h>

[[noreturn]] auto raise_syntax_error(char const msg[]) noexcept -> void
{
	int const ret = ::MessageBoxA(nullptr, msg, "Syntax error", MB_RETRYCANCEL | MB_ICONERROR);
	if (ret == IDRETRY)
		__debugbreak();
	else
		quick_exit(1);
}

auto raise_syntax_error_if_not(bool condition, char const msg[]) noexcept -> void
{
	if (!condition)
		raise_syntax_error(msg);
}

auto make_syntax_error(std::string_view msg) noexcept -> SyntaxError
{
	SyntaxError error;
	error.error_message = msg;
	return error;
}
