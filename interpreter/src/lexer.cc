#include "lexer.hh"
#include "unreachable.hh"
#include "multicomparison.hh"
#include <cassert>

namespace lex
{

	auto is_number(char c) noexcept -> bool
	{
		return (c >= '0') && (c <= '9');
	}

	auto is_operator(char c) noexcept -> bool
	{
		return (c == '+') || (c == '-') || (c == '*') || (c == '/');
	}

	auto is_reserved_for_the_language(char c) noexcept -> bool
	{
		return (c == '(') || (c == ')') || (c == '{') || (c == '}') || (c == ';') || (c == ',') || (c == '=');
	}

	auto is_whitespace(char c) noexcept -> bool
	{
		return (c == ' ') || (c == '\n') || (c == '\t');
	}

	auto is_valid_identifier_char(char c) noexcept -> bool
	{
		return !(is_operator(c) || is_reserved_for_the_language(c) || is_whitespace(c));
	}

	auto is_valid_after_literal(char c) noexcept -> bool
	{
		return !is_valid_identifier_char(c);
	}

	auto is_arrow(std::string_view src, int index) noexcept -> bool
	{
		return 
			(src.size() - index) >= 2 && 
			src[index] == '-' &&
			src[index + 1] == '>';
	}

	auto end_reached(std::string_view src, int index) noexcept -> bool
	{
		return index >= static_cast<int>(src.size());
	}

	// Skips all whitespace and returns the number of characters skipped.
	auto skip_whitespace(std::string_view src, int index) noexcept -> int
	{
		int chars_skipped = 0;
		while (!end_reached(src, index + chars_skipped) && is_whitespace(src[index + chars_skipped]))
			chars_skipped++;
		return chars_skipped;
	}

	auto token_length_literal_int(std::string_view src, int index) noexcept -> int
	{
		int length = 0;
		while (!end_reached(src, index + length) && is_number(src[index + length]))
			++length;
		return length;
	}

	auto token_type_and_length_number(std::string_view src, int index) noexcept -> std::pair<Token::Type, int>
	{
		bool dot_read = false;
		bool exp_read = false;

		int end = index + 1;
		for (;; ++end)
		{
			if (end_reached(src, end))
				break;
			if (is_number(src[end]))
				continue;
			else if (!dot_read && src[end] == '.')
				dot_read = true;
			else if (!exp_read && (src[end] == 'e' || src[end] == 'E'))
			{
				exp_read = true;
				// Account for unary plus and minus in the exponent
				if (src[end + 1] == '+' || src[end + 1] == '-')
					++end;
			}
			else if (is_valid_after_literal(src[end]))
				break;
			else
				declare_unreachable();
		}

		return { 
			(dot_read || exp_read) ? Token::Type::literal_float : Token::Type::literal_int,
			end - index
		};
	}

	auto token_length_identifier(std::string_view src, int index) noexcept -> int
	{
		int length = 1;
		while (!end_reached(src, index + length) && is_valid_identifier_char(src[index + length]))
			++length;
		return length;
	}

	auto next_token_type_and_length(std::string_view src, int index) noexcept -> std::pair<Token::Type, int>
	{
		if (is_number(src[index]))		return token_type_and_length_number(src, index);
		if (is_arrow(src, index))		return {Token::Type::arrow,				2};
		if (is_operator(src[index]))	return {Token::Type::operator_,			1};
		if (src[index] == '(')			return {Token::Type::open_parenthesis,	1};
		if (src[index] == ')')			return {Token::Type::close_parenthesis,	1};
		if (src[index] == '{')			return {Token::Type::open_brace,		1};
		if (src[index] == '}')			return {Token::Type::close_brace,		1};
		if (src[index] == ';')			return {Token::Type::semicolon,			1};
		if (src[index] == ',')			return {Token::Type::comma,				1};
		if (src[index] == '=')			return {Token::Type::assignment,		1};
		else							return {Token::Type::identifier,		token_length_identifier(src, index)};
	}

	std::vector<Token> tokenize(std::string_view src)
	{
		std::vector<Token> result;
		int index = skip_whitespace(src, 0);

		while (!end_reached(src, index))
		{
			auto const [token_type, token_length] = next_token_type_and_length(src, index);
			Token token;
			token.type = token_type;
			token.source = src.substr(index, token_length);
			index += token_length;

			// After a literal we must find whitespace, an operator, a delimiter or the end of the source.
			if (token.type == any_of(Token::Type::literal_int, Token::Type::literal_float))
			{
				assert(end_reached(src, index) || is_valid_after_literal(src[index]));
			}

			index += skip_whitespace(src, index);
			result.push_back(token);
		}

		return result;
	}

} // namespace tr
