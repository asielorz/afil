#pragma once

#include "syntax_error.hh"
#include <string_view>
#include <vector>

namespace lex
{

	struct Token
	{
		enum struct Type
		{
			identifier,
			literal_int,
			literal_float,
			literal_bool,
			literal_string,
			operator_,								// + - / *
			open_parenthesis, close_parenthesis,	// ( )
			open_brace, close_brace,				// { }
			open_bracket, close_bracket,			// [ ]
			semicolon,								// ;
			comma,									// ,
			period,									// .
			arrow,									// ->
		};

		std::string_view source;
		Type type;
	};
	auto tokenize(std::string_view src) noexcept -> expected<std::vector<Token>, PartialSyntaxError>;

} // namespace tr
