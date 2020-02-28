#include "syntax_error.hh"
#include "lexer.hh"
#include "utils/string.hh"
#include <cstdlib>
#include <cassert>
#include <Windows.h>

auto make_syntax_error(std::string_view msg) noexcept -> Error<PartialSyntaxError>
{
	return make_syntax_error(nullptr, nullptr, msg);
}

auto make_syntax_error(
	char const * error_start_in_source,
	char const * error_end_in_source,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>
{
	PartialSyntaxError error;
	error.error_message = msg;
	error.error_start_in_source = error_start_in_source;
	error.error_end_in_source = error_end_in_source;
	return error;
}

auto make_syntax_error(
	lex::Token token,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>
{
	return make_syntax_error(token.source.data(), token.source.data() + token.source.size(), msg);
}

auto complete_syntax_error(PartialSyntaxError const & partial_error, std::string_view source) noexcept -> SyntaxError
{
	if (!partial_error.error_start_in_source)
	{
		SyntaxError error;
		error.error_message = partial_error.error_message;
		error.row = 1;
		error.column = 1;
		error.error_length = 1;
		error.line_with_error = " ";
		return error;
	}
	else
	{
		char const * const line_start = std::find(std::make_reverse_iterator(partial_error.error_start_in_source), std::make_reverse_iterator(source.data()), '\n').base();
		char const * const line_end = std::find(partial_error.error_end_in_source, source.data() + source.size(), '\n');

		SyntaxError error;
		error.error_message = partial_error.error_message;
		error.row = 1 + static_cast<int>(std::count(source.data(), partial_error.error_start_in_source, '\n'));
		error.column = static_cast<int>(partial_error.error_start_in_source - line_start) + 3 * static_cast<int>(std::count(line_start, partial_error.error_start_in_source, '\t'));
		error.error_length = static_cast<int>(partial_error.error_end_in_source - partial_error.error_start_in_source);
		error.line_with_error = replace(std::string_view(line_start, line_end - line_start), "\t", "    ");

		return error;
	}
}

auto operator << (std::ostream & os, SyntaxError const & error) noexcept -> std::ostream &
{
	os << "Error:" << error.row << ':' << error.column << ':' << error.error_message << '\n';
	os << error.line_with_error << '\n';
	for (int i = 0; i < error.column; ++i) os << ' ';
	os << '^';
	for (int i = 0; i < error.error_length - 1; ++i) os << '~';
	os << '\n';
	return os;
}
