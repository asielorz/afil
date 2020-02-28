#include "lexer.hh"
#include "syntax_error.hh"
#include "utils/unreachable.hh"
#include "utils/multicomparison.hh"
#include <cassert>

using namespace std::literals;

namespace lex
{

	auto starts_with(std::string_view s, int index, std::string_view pattern) noexcept -> bool
	{
		int const pattern_length = static_cast<int>(pattern.size());
		if (pattern_length > static_cast<int>(s.size()) - index)
			return false;
		return std::equal(pattern.begin(), pattern.end(), s.begin() + index);
	}

	auto is_number(char c) noexcept -> bool
	{
		return (c >= '0') && (c <= '9');
	}

	auto is_first_char_of_operator(char c) noexcept -> bool
	{
		return
			(c == '+') || (c == '-') || (c == '*') || (c == '/') || (c == '%') || (c == '<') || 
			(c == '>') || (c == '=') || (c == '!') || (c == '&') || (c == '|') || (c == '~') || (c == '^');
	}

	auto is_operator(std::string_view src, int index) noexcept -> bool
	{
		char const c = src[index];
		return 
			is_first_char_of_operator(c)
			|| starts_with(src, index, "=="sv)
			|| starts_with(src, index, "!="sv)
			|| starts_with(src, index, "and"sv)
			|| starts_with(src, index, "or"sv)
			|| starts_with(src, index, "xor"sv)
			|| starts_with(src, index, "not"sv)
			;
	}

	auto is_reserved_for_the_language(char c) noexcept -> bool
	{
		return (c == '(') || (c == ')') || (c == '{') || (c == '}') || (c == '[') || (c == ']') || (c == ';') || (c == ',') || (c == '.');
	}

	auto is_whitespace(char c) noexcept -> bool
	{
		return (c == ' ') || (c == '\n') || (c == '\t');
	}

	auto is_valid_identifier_char(char c) noexcept -> bool
	{
		return !(is_first_char_of_operator(c) || is_reserved_for_the_language(c) || is_whitespace(c));
	}

	auto is_valid_after_literal(char c) noexcept -> bool
	{
		return !is_valid_identifier_char(c);
	}

	auto is_arrow(std::string_view src, int index) noexcept -> bool
	{
		return starts_with(src, index, "->"sv);
	}

	auto is_boolean(std::string_view src, int index) noexcept -> bool
	{
		return starts_with(src, index, "true"sv) || starts_with(src, index, "false"sv);
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

	auto skip_comments(std::string_view src, int index) noexcept -> expected<int, PartialSyntaxError>
	{
		if (starts_with(src, index, "//"sv))
		{
			size_t const newline = src.find('\n', index);
			if (newline == std::string_view::npos)
				return static_cast<int>(src.size()) - index;
			else
				return static_cast<int>(newline + 1) - index;
		}
		else if (starts_with(src, index, "/*"sv))
		{
			size_t const comment_end = src.find("*/"sv, index);
			if (comment_end == std::string_view::npos) return make_syntax_error(src.data() + index, src.data() + index + 2, "A C comment must be closed.");
			return static_cast<int>(comment_end + 2) - index;
		}
		else
			return 0;
	}

	auto skip_whitespace_and_comments(std::string_view src, int index) noexcept -> expected<int, PartialSyntaxError>
	{
		int original_index = index;
		for (;;)
		{
			int const whitespace_chars = skip_whitespace(src, index);
			try_call_decl(int const comment_chars, skip_comments(src, index + whitespace_chars));
			if (whitespace_chars + comment_chars == 0)
				break;
			else
				index += whitespace_chars + comment_chars;
		}
		return index - original_index;
	}

	auto token_length_literal_int(std::string_view src, int index) noexcept -> int
	{
		int length = 0;
		while (!end_reached(src, index + length) && is_number(src[index + length]))
			++length;
		return length;
	}

	auto token_type_and_length_number(std::string_view src, int index) noexcept -> expected<std::pair<Token::Type, int>, PartialSyntaxError>
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
				return make_syntax_error(src.data() + end, src.data() + end + 1, "Unrecognized char in number literal.");
		}

		return std::pair<Token::Type, int>{
			(dot_read || exp_read) ? Token::Type::literal_float : Token::Type::literal_int,
			end - index
		};
	}

	auto token_length_operator(std::string_view src, int index) noexcept -> int
	{
		constexpr std::string_view three_char_ops[] = { "<=>", "and", "xor", "not" };
		for (auto const s : three_char_ops)
			if (starts_with(src, index, s))
				return 3;

		constexpr std::string_view two_char_ops[] = { "==", "!=", "<=", ">=", "or", ">>", "<<" };
		for (auto const s : two_char_ops)
			if (starts_with(src, index, s))
				return 2;

		return 1;
	}

	auto token_length_string(std::string_view src, int index) noexcept -> int
	{
		int length = 1;
		while (!end_reached(src, index + length) && src[index + length] != '"')
			++length;

		++length; // The closing '"' is part of the string.
		return length;
	}

	auto token_length_identifier(std::string_view src, int index) noexcept -> int
	{
		int length = 1;
		while (!end_reached(src, index + length) && is_valid_identifier_char(src[index + length]))
			++length;
		return length;
	}

	auto next_token_type_and_length(std::string_view src, int index) noexcept -> expected<std::pair<Token::Type, int>, PartialSyntaxError>
	{
		if (is_number(src[index]))		return token_type_and_length_number(src, index);
		if (is_arrow(src, index))		return std::pair<Token::Type, int>{Token::Type::arrow,				2};
		if (is_operator(src, index))	return std::pair<Token::Type, int>{Token::Type::operator_,			token_length_operator(src, index)};
		if (src[index] == '"')			return std::pair<Token::Type, int>{Token::Type::literal_string,		token_length_string(src, index)};
		if (src[index] == '(')			return std::pair<Token::Type, int>{Token::Type::open_parenthesis,	1};
		if (src[index] == ')')			return std::pair<Token::Type, int>{Token::Type::close_parenthesis,	1};
		if (src[index] == '{')			return std::pair<Token::Type, int>{Token::Type::open_brace,			1};
		if (src[index] == '}')			return std::pair<Token::Type, int>{Token::Type::close_brace,		1};
		if (src[index] == '[')			return std::pair<Token::Type, int>{Token::Type::open_bracket,		1};
		if (src[index] == ']')			return std::pair<Token::Type, int>{Token::Type::close_bracket,		1};
		if (src[index] == ';')			return std::pair<Token::Type, int>{Token::Type::semicolon,			1};
		if (src[index] == ',')			return std::pair<Token::Type, int>{Token::Type::comma,				1};
		if (src[index] == '.')			return std::pair<Token::Type, int>{Token::Type::period,				1};
		if (is_boolean(src, index))		return std::pair<Token::Type, int>{Token::Type::literal_bool,		src[index] == 't' ? 4 : 5};
		else							return std::pair<Token::Type, int>{Token::Type::identifier,			token_length_identifier(src, index)};
	}

	auto tokenize(std::string_view src) noexcept -> expected<std::vector<Token>, PartialSyntaxError>
	{
		std::vector<Token> result;
		try_call_decl(int index, skip_whitespace_and_comments(src, 0));

		while (!end_reached(src, index))
		{
			try_call_decl(auto token_type_and_length, next_token_type_and_length(src, index));
			auto const [token_type, token_length] = token_type_and_length;
			Token token;
			token.type = token_type;
			token.source = src.substr(index, token_length);
			index += token_length;

			// After a literal we must find whitespace, an operator, a delimiter or the end of the source.
			if (token.type == any_of(Token::Type::literal_int, Token::Type::literal_float) || 
				token.source == any_of("and"sv, "or"sv, "xor"sv))
			{
				if (!end_reached(src, index) && !is_valid_after_literal(src[index])) 
					return make_syntax_error(src.data() + index, src.data() + index + 1, "Expected whitespace, operator or delimiter after literal.");
			}

			try_call_decl(int const comment_length, skip_whitespace_and_comments(src, index));
			index += comment_length;
			result.push_back(token);
		}

		return std::move(result);
	}

} // namespace tr
