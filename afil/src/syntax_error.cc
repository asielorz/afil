#include "syntax_error.hh"
#include "lexer.hh"
#include "utils/string.hh"
#include <cstdlib>
#include <cassert>
#include <algorithm>

auto make_syntax_error(
	std::string_view error_in_source,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>
{
	PartialSyntaxError error;
	error.error_in_source = error_in_source;
	error.error_message = msg;
	return error;
}

auto make_syntax_error(
	lex::Token token,
	std::string_view msg) noexcept -> Error<PartialSyntaxError>
{
	return make_syntax_error(token.source, msg);
}

auto complete_syntax_error(PartialSyntaxError const & partial_error, std::string_view source, std::string_view filename) noexcept -> SyntaxError
{
	if (partial_error.error_in_source.empty())
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
		char const * const line_start = std::find(std::make_reverse_iterator(partial_error.error_in_source.data()), std::make_reverse_iterator(source.data()), '\n').base();
		char const * const line_end = std::find(partial_error.error_in_source.data() + partial_error.error_in_source.size(), source.data() + source.size(), '\n');

		SyntaxError error;
		error.error_message = partial_error.error_message;
		error.row = 1 + static_cast<int>(std::count(source.data(), partial_error.error_in_source.data(), '\n'));
		error.column = static_cast<int>(partial_error.error_in_source.data() - line_start) + 3 * static_cast<int>(std::count(line_start, partial_error.error_in_source.data(), '\t'));
		error.error_length = static_cast<int>(partial_error.error_in_source.size());
		error.line_with_error = replace(std::string_view(line_start, line_end - line_start), "\t", "    ");
		error.filename = filename;

		return error;
	}
}

auto make_complete_syntax_error(
	std::string_view error_in_source,
	std::string_view msg,
	std::string_view source,
	std::string_view filename) noexcept->Error<SyntaxError>
{
	return Error(complete_syntax_error(std::move(make_syntax_error(error_in_source, msg).value), source, filename));
}

auto error_string(SyntaxError const & error) noexcept -> std::string
{
	return join(
		error.filename, ':', error.row, ':', error.column, ": error: ", error.error_message, '\n',
		error.line_with_error, '\n',
		std::string(error.column, ' '), '^', std::string(error.error_length - 1, '~'), '\n'
	);
}

auto operator << (std::ostream & os, SyntaxError const & error) noexcept -> std::ostream &
{
	return os << error_string(error);
}
