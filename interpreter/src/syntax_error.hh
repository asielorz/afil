#pragma once

#include "utils/expected.hh"
#include <string>

struct SyntaxError
{
	std::string error_message;
};

auto make_syntax_error(std::string_view msg) noexcept -> Error<SyntaxError>;
