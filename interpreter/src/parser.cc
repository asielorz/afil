#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include "overload.hh"
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

	auto next_statement_tokens(span<lex::Token const> tokens, size_t & index) noexcept -> span<lex::Token const>
	{
		size_t length = 0;
		while (tokens[index + length].type != TokenType::semicolon)
			length++;
		length++; // Include the semicolon.

		auto const result_tokens = tokens.subspan(index, length);
		index += length;
		return result_tokens;
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, Program & program) noexcept -> FunctionNode
	{
		// Skip fn token.
		index++;

		// Arguments must be between parenthesis
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		Function function;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			// TODO: Types
			assert(tokens[index].source == "int");
			index++;

			Variable argument;
			argument.name = tokens[index].source;
			argument.offset = function.stack_frame_size;
			function.stack_frame_size += sizeof(int);
			function.variables.push_back(argument);
			function.parameter_count++;
			index++;

			assert(tokens[index].type == TokenType::comma || tokens[index].type == TokenType::close_parenthesis);
			index++;
		}

		// Return type is introduced with an arrow
		assert(tokens[index].type == TokenType::arrow);
		index++;

		// Return type. By now only int supported.
		assert(tokens[index].source == "int");
		index++;

		// Body of the function is enclosed by braces.
		assert(tokens[index].type == TokenType::open_brace);
		index++;

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			auto statement_tree = parse_statement(next_statement_tokens(tokens, index), program, function);
			if (statement_tree)
				function.statements.push_back(std::move(*statement_tree));
		}

		// Skip closing brace.
		index++;

		// Add the function to the program.
		program.functions.push_back(std::move(function));
		return FunctionNode{static_cast<int>(program.functions.size() - 1)};
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree;

	auto parse_function_call_expression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope, int function_id) noexcept -> FunctionCallNode
	{
		// Parameter list starts with (
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		// Parse parameters
		int const param_count = program.functions[function_id].parameter_count;

		FunctionCallNode node;
		node.function_id = function_id;
		node.parameters.reserve(param_count);

		for (int i = 0; i < param_count; ++i)
		{
			node.parameters.push_back(parse_subexpression(tokens, index, program, scope));

			if (i < param_count - 1)
			{
				assert(tokens[index].type == TokenType::comma);
				index++;
			}
		}

		return node;
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		std::vector<ExpressionTree> operands;
		std::vector<Operator> operators;

		if (tokens[index].source == "fn")
		{
			return parse_function_expression(tokens, index, program);
			//auto const func_node = parse_function_expression(tokens, index, program);
			//return parse_function_call_expression(tokens, index, program, scope, func_node.function_id);
		}

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
				auto const if_var_found = [&](lookup_result::VariableFound result)
				{
					VariableNode var_node;
					var_node.variable_offset = result.variable_offset;
					operands.push_back(var_node);
					index++;
				};
				auto const if_fn_found = [&](lookup_result::FunctionFound result)
				{
					FunctionNode func_node;
					func_node.function_id = result.function_id;
					index++;
					operands.push_back(parse_function_call_expression(tokens, index, program, scope, result.function_id));
				};
				auto const if_nothing_found = [&](lookup_result::NothingFound)
				{
					declare_unreachable();
				};
				auto const lookup = lookup_name(scope, tokens[index].source);
				std::visit(overload(if_var_found, if_fn_found, if_nothing_found), lookup);
			}
			else if (tokens[index].type == TokenType::open_parenthesis)
			{
				index++;
				operands.push_back(parse_subexpression(tokens, index, program, scope));
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
				// If we encounter a comma, end subexpression but don't consume the comma.
				// TODO: Think of a consistent way of doing this.
				if (tokens[index].type == TokenType::comma)
				{
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

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		size_t index = 0;
		return parse_subexpression(tokens, index, program, scope);
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> VariableDeclarationStatementNode
	{
		// A variable declaration statement has the following form:
		// int [var_name] = [expr];

		VariableDeclarationStatementNode node;

		// A statement begins with the type of the declared variable.
		assert(tokens[0].source == "int");

		// The second token of the statement is the variable name.
		assert(tokens[1].type == TokenType::identifier);
		Variable local;
		local.name = tokens[1].source;
		local.offset = scope.stack_frame_size;
		scope.stack_frame_size += sizeof(int);
		node.variable_offset = local.offset;
		scope.variables.push_back(local);

		// The third token is a '='.
		assert(tokens[2].type == TokenType::assignment);

		// The rest is the expression assigned to the variable.
		node.assigned_expression = parse_expression(tokens.subspan(3, tokens.size() - 4), program, scope);

		return node;
	}

	auto parse_return_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> ReturnStatementNode
	{
		// A return statement has the following form:
		// return [expr];

		ReturnStatementNode node;
		node.returned_expression = parse_expression(tokens.subspan(1, tokens.size() - 2), program, scope);
		return node;
	}

	auto parse_function_declaration_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::nullopt_t
	{
		// A function declaration statement has the following form:
		// let [name] = [function expr];

		assert(tokens[1].type == TokenType::identifier);
		assert(tokens[2].type == TokenType::assignment);

		size_t index = 3;
		FunctionNode const func_node = parse_function_expression(tokens.subspan(0, tokens.size() - 1), index, program);
		FunctionName func_name;
		func_name.name = tokens[1].source;
		func_name.id = func_node.function_id;
		scope.functions.push_back(func_name);

		return std::nullopt;
	}

	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<StatementTree>
	{
		// A statement ends with a semicolon.
		assert(tokens.back().type == TokenType::semicolon);

		if (tokens[0].source == "return")
			return parse_return_statement(tokens, program, scope);
		// This will change when let is also used to declare constants, but that's a problem of future Asier.
		else if (tokens[0].source == "let")
			return parse_function_declaration_statement(tokens, program, scope);
		else
			return parse_variable_declaration_statement(tokens, program, scope);
	}

} //namespace parser
