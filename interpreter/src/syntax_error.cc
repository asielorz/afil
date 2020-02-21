#include "syntax_error.hh"
#include <cstdlib>
#include <Windows.h>

auto make_syntax_error(std::string_view msg) noexcept -> Error<SyntaxError>
{
	SyntaxError error;
	error.error_message = msg;
	return error;
}
