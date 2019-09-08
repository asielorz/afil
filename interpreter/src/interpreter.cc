#include "interpreter.hh"
#include "lexer.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include <vector>
#include <cassert>
#include <charconv>

enum struct Operator
{
	add, subtract, multiply, divide
};

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

struct ExpressionTree
{
	explicit ExpressionTree(int v) noexcept : value(v) {}
	explicit ExpressionTree(Operator o) noexcept : op(o) {}

	union
	{
		Operator op;
		int value;
	};
	std::unique_ptr<ExpressionTree> left;
	std::unique_ptr<ExpressionTree> right;
};

auto is_operator_node(ExpressionTree const & tree) noexcept -> bool
{
	return static_cast<bool>(tree.left);
}

auto insert_expression(ExpressionTree & tree, ExpressionTree & new_node) noexcept -> void
{
	if (precedence(new_node.op) <= precedence(tree.op))
	{
		new_node.left = std::make_unique<ExpressionTree>(std::move(tree));
		tree = std::move(new_node);
	}
	else if (!is_operator_node(*tree.right))
	{
		new_node.left = std::move(tree.right);
		tree.right = std::make_unique<ExpressionTree>(std::move(new_node));
	}
	else
		insert_expression(*tree.right, new_node);
}

auto resolve_operator_precedence(span<ExpressionTree> operands, span<Operator const> operators) noexcept -> ExpressionTree
{
	if (operators.empty())
	{
		assert(operands.size() == 1);
		return std::move(operands[0]);
	}

	ExpressionTree root(operators[0]);
	root.left = std::make_unique<ExpressionTree>(std::move(operands[0]));
	root.right = std::make_unique<ExpressionTree>(std::move(operands[1]));

	for (size_t i = 1; i < operators.size(); ++i)
	{
		auto new_node = ExpressionTree(operators[i]);
		new_node.right = std::make_unique<ExpressionTree>(std::move(operands[i + 1]));
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
		using Type = lex::Token::Type;

		if (tokens[index].type == Type::literal_int)
		{
			operands.push_back(ExpressionTree(parse_int(tokens[index].source)));
			index++;
		}
		else if (tokens[index].type == Type::open_parenthesis)
		{
			index++;
			operands.push_back(parse_subexpression(tokens, index));
		}
		else declare_unreachable(); // TODO: Actual error handling.

		if (index < tokens.size())
		{
			// If we encounter a close parenthesis, end subexpression.
			if (tokens[index].type == Type::close_parenthesis)
			{
				index++;
				return resolve_operator_precedence(operands, operators);
			}
			else if (tokens[index].type == Type::operator_)
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

auto eval_expression_tree(ExpressionTree const & tree) noexcept -> int
{
	if (!tree.left)
		return tree.value;

	int const lval = eval_expression_tree(*tree.left);
	int const rval = eval_expression_tree(*tree.right);
	switch (tree.op)
	{
		case Operator::add:		 return lval + rval;
		case Operator::subtract: return lval - rval;
		case Operator::multiply: return lval * rval;
		case Operator::divide:	 return lval / rval;
	}
	assert(false);
	return 0;
}

namespace interpreter
{

	auto eval_expression(std::string_view src) noexcept -> int
	{
		return eval_expression_tree(parse_expression(lex::tokenize(src)));
	}

}
