#pragma once

#include <string_view>
#include <vector>

namespace lex
{

	struct Token
	{
		enum struct Type
		{
			identifier, literal_int, operator_, open_parenthesis, close_parenthesis
		};

		std::string_view source;
		Type type;
	};
	std::vector<Token> tokenize(std::string_view src);

} // namespace tr
