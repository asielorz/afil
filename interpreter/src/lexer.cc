#include "lexer.hh"
#include "unreachable.hh"
#include <cassert>

namespace lex
{

	auto is_number(char c) noexcept -> bool
	{
		return (c >= '0') && (c < '9');
	}

	auto is_operator(char c) noexcept -> bool
	{
		return (c == '+') || (c == '-') || (c == '*') || (c == '/');
	}

	auto is_reserved_for_the_language(char c) noexcept -> bool
	{
		return (c == '(') || (c == ')') || (c == ';') || (c == '=');
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

	auto next_token_type(std::string_view src, int index) noexcept -> Token::Type
	{
		if (is_number(src[index]))		return Token::Type::literal_int;
		if (is_operator(src[index]))	return Token::Type::operator_;
		if (src[index] == '(')			return Token::Type::open_parenthesis;
		if (src[index] == ')')			return Token::Type::close_parenthesis;
		if (src[index] == ';')			return Token::Type::semicolon;
		if (src[index] == '=')			return Token::Type::assignment;
		else							return Token::Type::identifier;
	}

	auto token_length_literal_int(std::string_view src, int index) noexcept -> int
	{
		int length = 0;
		while (!end_reached(src, index + length) && is_number(src[index + length]))
			++length;
		return length;
	}

	auto token_length_identifier(std::string_view src, int index) noexcept -> int
	{
		int length = 1;
		while (!end_reached(src, index + length) && is_valid_identifier_char(src[index + length]))
			++length;
		return length;
	}

	auto next_token_length(std::string_view src, int index, Token::Type type) noexcept -> int
	{
		switch (type)
		{
			case Token::Type::identifier:			return token_length_identifier(src, index);
			case Token::Type::literal_int:			return token_length_literal_int(src, index);
			case Token::Type::operator_:			return 1;
			case Token::Type::open_parenthesis:		return 1;
			case Token::Type::close_parenthesis:	return 1;
			case Token::Type::semicolon:			return 1;
			case Token::Type::assignment:			return 1;
		}
		declare_unreachable();
	}

	std::vector<Token> tokenize(std::string_view src)
	{
		std::vector<Token> result;
		int index = skip_whitespace(src, 0);

		while (!end_reached(src, index))
		{
			Token token;
			token.type = next_token_type(src, index);
			int const token_length = next_token_length(src, index, token.type);
			token.source = src.substr(index, token_length);
			index += token_length;

			// After a literal we must find whitespace, an operator, a delimiter or the end of the source.
			if (token.type == Token::Type::literal_int)
			{
				assert(end_reached(src, index) || is_valid_after_literal(src[index]));
			}

			index += skip_whitespace(src, index);
			result.push_back(token);
		}

		return result;
	}

} // namespace tr
