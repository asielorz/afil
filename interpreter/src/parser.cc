#include "parser.hh"
#include "lexer.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include <vector>
#include <cassert>
#include <charconv>

using TokenType = lex::Token::Type;

namespace parser
{

	auto precedence(Operator op) noexcept -> int
	{
		constexpr int precedences[] = { 0, 0, 1, 1 };
		return precedences[static_cast<int>(op)];
	}

	auto parse_int(std::string_view token_source) noexcept -> int
	{
		int value;
		std::from_chars(token_source.data(), token_source.data() + token_source.size(), value);
		return value;
	}

	auto parse_operator(std::string_view token_source) noexcept -> Operator
	{
		switch (token_source[0])
		{
		case '+': return Operator::add;
		case '-': return Operator::subtract;
		case '*': return Operator::multiply;
		case '/': return Operator::divide;
		}
		declare_unreachable();
	}

	auto is_operator_node(ExpressionTree const & tree) noexcept -> bool
	{
		return tree.index() == 1;
	}

	auto insert_expression(ExpressionTree & tree, ExpressionTree & new_node) noexcept -> void
	{
		OperatorNode & tree_op = std::get<OperatorNode>(tree);
		OperatorNode & new_op = std::get<OperatorNode>(new_node);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = std::make_unique<ExpressionTree>(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!is_operator_node(*tree_op.right))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = std::make_unique<ExpressionTree>(std::move(new_node));
		}
		else
			insert_expression(*tree_op.right, new_node);
	}

	auto resolve_operator_precedence(span<ExpressionTree> operands, span<Operator const> operators) noexcept -> ExpressionTree
	{
		if (operators.empty())
		{
			assert(operands.size() == 1);
			return std::move(operands[0]);
		}

		ExpressionTree root = [=]{
			return OperatorNode(operators[0],
				std::make_unique<ExpressionTree>(std::move(operands[0])),
				std::make_unique<ExpressionTree>(std::move(operands[1]))
			);
		}();


		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = ExpressionTree(OperatorNode(operators[i], nullptr, std::make_unique<ExpressionTree>(std::move(operands[i + 1]))));
			insert_expression(root, new_node);
		}
		return root;
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index) noexcept -> ExpressionTree
	{
		std::vector<ExpressionTree> operands;
		std::vector<Operator> operators;

		// Parse the expression.
		while (index < tokens.size())
		{
			if (tokens[index].type == TokenType::literal_int)
			{
				operands.push_back(ExpressionTree(parse_int(tokens[index].source)));
				index++;
			}
			else if (tokens[index].type == TokenType::identifier)
			{
				VariableNode var_node;
				var_node.variable_name = tokens[index].source;
				operands.push_back(var_node);
				index++;
			}
			else if (tokens[index].type == TokenType::open_parenthesis)
			{
				index++;
				operands.push_back(parse_subexpression(tokens, index));
			}
			else declare_unreachable(); // TODO: Actual error handling.

			if (index < tokens.size())
			{
				// If we encounter a close parenthesis, end subexpression.
				if (tokens[index].type == TokenType::close_parenthesis)
				{
					index++;
					return resolve_operator_precedence(operands, operators);
				}
				else if (tokens[index].type == TokenType::operator_)
				{
					operators.push_back(parse_operator(tokens[index].source));
					index++;
				}
				else declare_unreachable(); // TODO: Actual error handling.
			}
		}

		return resolve_operator_precedence(operands, operators);
	}

	auto parse_expression(span<lex::Token const> tokens) noexcept -> ExpressionTree
	{
		size_t index = 0;
		return parse_subexpression(tokens, index);
	}

	auto parse_statement(span<lex::Token const> tokens) noexcept -> StatementTree
	{
		// A statement has the following form:
		// int [var_name] = [expr];

		// A statement begins with the type of the declared variable.
		assert(tokens[0].source == "int");

		// The second tokeb of the statement is the variable name.
		assert(tokens[1].type == TokenType::identifier);
		StatementTree tree;
		tree.variable_name = tokens[1].source;

		// The third token is a '='.
		assert(tokens[2].type == TokenType::assignment);

		// A statement ends with a semicolon.
		assert(tokens.back().type == TokenType::semicolon);

		// The rest is the expression assigned to the variable.
		tree.assigned_expression = parse_expression(tokens.subspan(3, tokens.size() - 4));

		return tree;
	}

} //namespace parser
