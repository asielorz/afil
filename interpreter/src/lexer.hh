#pragma once

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
			operator_,								// + - / *
			open_parenthesis, close_parenthesis,	// ( )
			open_brace, close_brace,				// { }
			semicolon,								// ;
			comma,									// ,
			arrow,									// ->
		};

		std::string_view source;
		Type type;
	};
	std::vector<Token> tokenize(std::string_view src);

} // namespace tr
