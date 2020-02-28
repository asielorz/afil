#pragma once

#include "utils/expected.hh"
#include <string>
#include <iosfwd>

namespace lex { struct Token; }

struct PartialSyntaxError
{
	std::string error_message;
	char const * error_start_in_source;
	char const * error_end_in_source;
};

struct SyntaxError
{
	std::string error_message;
	std::string line_with_error;
	int row;
	int column;
	int error_length;
};

auto make_syntax_error(std::string_view msg) noexcept -> Error<PartialSyntaxError>;

auto make_syntax_error(
	char const * error_start_in_source,
	char const * error_end_in_source,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>;

auto make_syntax_error(
	lex::Token token,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>;

auto complete_syntax_error(PartialSyntaxError const & partial_error, std::string_view source) noexcept -> SyntaxError;
auto operator << (std::ostream & os, SyntaxError const & error) noexcept -> std::ostream &;
