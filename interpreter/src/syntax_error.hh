#pragma once

#include "utils/expected.hh"
#include <string>

[[noreturn]] auto raise_syntax_error(char const msg[]) noexcept -> void;
auto raise_syntax_error_if_not(bool condition, char const msg[]) noexcept -> void;

struct SyntaxError
{
	std::string error_message;
};

auto make_syntax_error(std::string_view msg) noexcept -> Error<SyntaxError>;
