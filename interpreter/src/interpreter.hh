#pragma once

#include <vector>
#include <string_view>

namespace parser 
{ 
	struct ExpressionTree;
	struct StatementTree;
}

namespace interpreter
{

	struct ExecutionContext
	{
		struct Variable
		{
			std::string_view name;
			int value;
		};
		std::vector<Variable> variables;
	};

	auto eval_expression_tree(parser::ExpressionTree const & tree, ExecutionContext const & context) noexcept -> int;
	auto run_statement_tree(parser::StatementTree const & tree, ExecutionContext & context) noexcept -> void;

}
