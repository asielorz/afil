#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "multicomparison.hh"
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

	template <typename T>
	auto parse_number_literal(std::string_view token_source) noexcept -> T
	{
		T value;
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
		return tree.index() == 2;
	}

	auto expression_type(ExpressionTree const & tree, Program const & program) noexcept -> Type
	{
		return type_with_id(program, expression_type_id(tree, program));
	}

	auto expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> TypeId
	{
		auto const visitor = overload(
			[](int) { return TypeId::int_; },
			[](float) { return TypeId::float_; },
			[&](parser::OperatorNode const & op_node) { return expression_type_id(*op_node.left, program); },
			[](parser::LocalVariableNode const & var_node) { return var_node.variable_type; },
			[](parser::GlobalVariableNode const & var_node) { return var_node.variable_type; },
			[](parser::FunctionNode const &) { return TypeId::function; },
			[&](parser::FunctionCallNode const & func_call_node) 
			{
				if (func_call_node.function_id.is_extern)
					return program.extern_functions[func_call_node.function_id.index].return_type;
				else
					return program.functions[func_call_node.function_id.index].return_type;
			}
		);
		return std::visit(visitor, tree.as_variant());
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

	auto are_expression_tree_types_valid(ExpressionTree const & tree, Program const & program) noexcept -> bool
	{
		if (is_operator_node(tree))
		{
			auto const & node = std::get<OperatorNode>(tree);
			return
				are_expression_tree_types_valid(*node.left, program) &&
				are_expression_tree_types_valid(*node.right, program) &&
				expression_type_id(*node.left, program) == expression_type_id(*node.right, program) &&
				expression_type_id(*node.left, program) == any_of(TypeId::int_, TypeId::float_);
		}
		else
			return true;
	}

	auto resolve_operator_precedence(span<ExpressionTree> operands, span<Operator const> operators, Program const & program) noexcept -> ExpressionTree
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

		assert(are_expression_tree_types_valid(root, program));
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

	auto align(int address, int alignment) noexcept -> int
	{
		int const excedent = address % alignment;
		if (excedent > 0)
			address += alignment - excedent;
		return address;
	}

	auto add_variable_to_scope(Scope & scope, std::string_view name, TypeId type_id, Program const & program) -> int
	{
		Type const & type = type_with_id(program, type_id);

		Variable var;
		var.name = name;
		var.type = type_id;
		var.offset = align(scope.stack_frame_size, type.alignment);
		scope.stack_frame_size = var.offset + type.size;
		scope.variables.push_back(var);
		return var.offset;
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
			TypeId const type_found = lookup_type_name(program, tokens[index].source);
			assert(type_found != TypeId::none);
			index++;

			add_variable_to_scope(function, tokens[index].source, type_found, program);
			function.parameter_count++;
			index++;

			assert(tokens[index].type == TokenType::comma || tokens[index].type == TokenType::close_parenthesis);
			index++;
		}

		// Return type is introduced with an arrow
		assert(tokens[index].type == TokenType::arrow);
		index++;

		// Return type. By now only int supported.
		function.return_type = lookup_type_name(program, tokens[index].source);
		assert(function.return_type != TypeId::none);
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
		auto const func_id = FunctionId{0, static_cast<unsigned>(program.functions.size() - 1)};
		return FunctionNode{func_id};
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree;

	auto parse_function_call_expression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope, span<FunctionId const> overload_set) noexcept -> FunctionCallNode
	{
		// Parameter list starts with (
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		// Parse parameters.
		int param_count = 0;

		FunctionCallNode node;
		node.parameters.reserve(param_count);

		std::vector<TypeId> parameter_types;

		for (;;)
		{
			// Parse a parameter.
			node.parameters.push_back(parse_subexpression(tokens, index, program, scope));
			parameter_types.push_back(expression_type_id(node.parameters.back(), program));

			// If after a parameter we find a close parenthesis, end parameter list.
			if (tokens[index].type == TokenType::close_parenthesis)
			{
				index++;
				break;
			}
			// If we find a comma, parse next parameter.
			else if (tokens[index].type == TokenType::comma)
				index++;
			// Anything else we find is wrong.
			else
				declare_unreachable();
		}

		node.function_id = resolve_function_overloading(overload_set, parameter_types, program);
		assert(node.function_id != invalid_function_id);

		return node;
	}

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		if (tokens[index].source == "fn")
		{
			auto const func_node = parse_function_expression(tokens, index, program);
			// Opening parenthesis after a function means a function call.
			if (tokens[index].type == TokenType::open_parenthesis)
				return parse_function_call_expression(tokens, index, program, scope, span<FunctionId const>(&func_node.function_id, 1));
			else
				return func_node;
		}
		else if (tokens[index].type == TokenType::literal_int)
		{
			return ExpressionTree(parse_number_literal<int>(tokens[index++].source));
		}
		else if (tokens[index].type == TokenType::literal_float)
		{
			return ExpressionTree(parse_number_literal<float>(tokens[index++].source));
		}
		else if (tokens[index].type == TokenType::identifier)
		{
			auto const if_var_found = [&](lookup_result::Variable result) -> ExpressionTree
			{
				LocalVariableNode var_node;
				var_node.variable_type = result.variable_type;
				var_node.variable_offset = result.variable_offset;
				index++;
				return var_node;
			};
			auto const if_global_var_found = [&](lookup_result::GlobalVariable result) -> ExpressionTree
			{
				GlobalVariableNode var_node;
				var_node.variable_type = result.variable_type;
				var_node.variable_offset = result.variable_offset;
				index++;
				return var_node;
			};
			auto const if_fn_found = [&](lookup_result::OverloadSet result) -> ExpressionTree
			{
				index++;
				if (tokens[index].type == TokenType::open_parenthesis)
					return parse_function_call_expression(tokens, index, program, scope, result.function_ids);
				else
					return FunctionNode{result.function_ids[0]}; // TODO: Overload set node?
			};
			auto const if_nothing_found = [](lookup_result::Nothing) -> ExpressionTree { declare_unreachable(); };
			auto const lookup = lookup_name(scope, program.global_scope, tokens[index].source);
			return std::visit(overload(if_var_found, if_global_var_found, if_fn_found, if_nothing_found), lookup);
		}
		else if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			auto expr = parse_subexpression(tokens, index, program, scope);

			// Next token must be close parenthesis.
			assert(tokens[index].type == TokenType::close_parenthesis);
			index++;

			return expr;
		}
		else declare_unreachable(); // TODO: Actual error handling.
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		std::vector<ExpressionTree> operands;
		std::vector<Operator> operators;

		// Parse the expression.
		while (index < tokens.size())
		{
			operands.push_back(parse_single_expression(tokens, index, program, scope));

			// If the next token is an operator, parse the operator and repeat. Otherwise end the loop and return the expression.
			if (index < tokens.size() && tokens[index].type == TokenType::operator_)
			{
				operators.push_back(parse_operator(tokens[index].source));
				index++;
			}
			else break;
		}

		return resolve_operator_precedence(operands, operators, program);
	}

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		size_t index = 0;
		return parse_subexpression(tokens, index, program, scope);
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> VariableDeclarationStatementNode
	{
		// A variable declaration statement has the following form:
		// [type] [var_name] = [expr];

		VariableDeclarationStatementNode node;

		// A statement begins with the type of the declared variable.
		TypeId const type_found = lookup_type_name(program, tokens[0].source);
		assert(type_found != TypeId::none);

		// The second token of the statement is the variable name.
		assert(tokens[1].type == TokenType::identifier);
		node.variable_offset = add_variable_to_scope(scope, tokens[1].source, type_found, program);

		// The third token is a '='.
		assert(tokens[2].type == TokenType::assignment);

		// The rest is the expression assigned to the variable.
		node.assigned_expression = parse_expression(tokens.subspan(3, tokens.size() - 4), program, scope);
		// Require that the expression assigned to the variable has the same type as the variable.
		assert(expression_type_id(node.assigned_expression, program) == type_found);

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
