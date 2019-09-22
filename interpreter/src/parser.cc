#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "multicomparison.hh"
#include "utils.hh"
#include <vector>
#include <cassert>
#include <charconv>

using TokenType = lex::Token::Type;

using expr::ExpressionTree;
using expr::OperatorTree;
using expr::OperatorNode;
using expr::Operator;

using namespace std::literals;

namespace parser
{

	template <typename T>
	auto parse_number_literal(std::string_view token_source) noexcept -> T
	{
		T value;
		std::from_chars(token_source.data(), token_source.data() + token_source.size(), value);
		return value;
	}

	auto parse_operator(std::string_view token_source) noexcept -> Operator
	{
		if (token_source == "=="sv) return Operator::equal;
		if (token_source == "!="sv) return Operator::not_equal;
		if (token_source == "<="sv) return Operator::less_equal;
		if (token_source == ">="sv) return Operator::greater_equal;
		if (token_source == "<=>"sv) return Operator::three_way_compare;
		if (token_source == "and"sv) return Operator::and_;
		if (token_source == "or"sv) return Operator::or_;
		if (token_source == "xor"sv) return Operator::xor_;

		switch (token_source[0])
		{
			case '+': return Operator::add;
			case '-': return Operator::subtract;
			case '*': return Operator::multiply;
			case '/': return Operator::divide;
			case '<': return Operator::less;
			case '>': return Operator::greater;
		}
		declare_unreachable();
	}

	auto insert_expression(OperatorTree & tree, OperatorTree & new_node) noexcept -> void
	{
		OperatorNode & tree_op = std::get<OperatorNode>(tree);
		OperatorNode & new_op = std::get<OperatorNode>(new_node);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = std::make_unique<OperatorTree>(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!is_operator_node(*tree_op.right))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = std::make_unique<OperatorTree>(std::move(new_node));
		}
		else
			insert_expression(*tree_op.right, new_node);
	}

	auto resolve_operator_precedence(span<ExpressionTree> operands, span<Operator const> operators) noexcept -> OperatorTree
	{
		if (operators.empty())
		{
			assert(operands.size() == 1);
			return std::move(operands[0]);
		}

		OperatorTree root = [=]{
			return expr::OperatorNode(operators[0],
				std::make_unique<OperatorTree>(std::move(operands[0])),
				std::make_unique<OperatorTree>(std::move(operands[1]))
			);
		}();

		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = OperatorTree(OperatorNode(operators[i], nullptr, std::make_unique<OperatorTree>(std::move(operands[i + 1]))));
			insert_expression(root, new_node);
		}

		return root;
	}

	auto resolve_operator_overloading(OperatorTree tree, Program const & program, Scope const & scope) noexcept -> ExpressionTree
	{
		if (is_operator_node(tree))
		{
			auto & node = std::get<OperatorNode>(tree);
			ExpressionTree left = resolve_operator_overloading(std::move(*node.left), program, scope);
			ExpressionTree right = resolve_operator_overloading(std::move(*node.right), program, scope);
			Operator const op = node.op;

			auto const visitor = overload(
				[](lookup_result::Nothing) -> ExpressionTree { declare_unreachable(); },
				[](lookup_result::Variable) -> ExpressionTree { declare_unreachable(); },
				[](lookup_result::GlobalVariable) -> ExpressionTree { declare_unreachable(); },
				[&](lookup_result::OverloadSet const & overload_set) -> ExpressionTree
				{
					TypeId const operand_types[] = {expression_type_id(left, program), expression_type_id(right, program)};
					auto const function_id = resolve_function_overloading(overload_set.function_ids, operand_types, program);
					if (function_id != invalid_function_id)
					{
						if (op == Operator::not_equal || op == Operator::less || op == Operator::greater ||
							op == Operator::less_equal || op == Operator::greater_equal)
						{
							expr::RelationalOperatorCallNode func_node;
							func_node.function_id = function_id;
							func_node.op = op;
							func_node.parameters = std::make_unique<std::array<ExpressionTree, 2>>();
							(*func_node.parameters)[0] = std::move(left);
							(*func_node.parameters)[1] = std::move(right);
							return func_node;
						}
						else
						{
							expr::FunctionCallNode func_node;
							func_node.function_id = function_id;
							func_node.parameters.reserve(2);
							func_node.parameters.push_back(std::move(left));
							func_node.parameters.push_back(std::move(right));
							return func_node;
						}
					}
					else declare_unreachable();
				}
			);

			auto const lookup = lookup_name(scope, program.global_scope, operator_function_name(op));
			return std::visit(visitor, lookup);
		}
		else
			return std::move(std::get<ExpressionTree>(tree));
	}

	auto next_statement_tokens(span<lex::Token const> tokens, size_t & index) noexcept -> span<lex::Token const>
	{
		size_t length = 0;
		for (int scope_depth = 0; length < tokens.size(); length++)
		{
			if (tokens[index + length].type == TokenType::semicolon && scope_depth == 0)
			{
				length++; // Include the semicolon.
				break;
			}
			else if (tokens[index + length].type == TokenType::open_brace)
				scope_depth++;
			else if (tokens[index + length].type == TokenType::close_brace)
				scope_depth--;
		}

		auto const result_tokens = tokens.subspan(index, length);
		index += length;
		return result_tokens;
	}

	auto add_variable_to_scope(Scope & scope, std::string_view name, TypeId type_id, Program const & program) -> int
	{
		Type const & type = type_with_id(program, type_id);

		Variable var;
		var.name = name;
		var.type = type_id;
		var.offset = align(scope.stack_frame_size, type.alignment);
		scope.stack_frame_size = var.offset + type.size;
		scope.stack_frame_alignment = std::max(scope.stack_frame_alignment, type.alignment);
		scope.variables.push_back(var);
		return var.offset;
	}

	auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, Program & program, Function & function) noexcept -> void
	{
		// Parameters must be between parenthesis.
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			TypeId const type_found = lookup_type_name(program, tokens[index].source);
			assert(type_found != TypeId::none);
			index++;

			add_variable_to_scope(function, tokens[index].source, type_found, program);
			function.parameter_count++;
			function.parameter_size = function.stack_frame_size;
			index++;

			if (tokens[index].type == TokenType::close_parenthesis)
				break;

			assert(tokens[index].type == TokenType::comma);
			index++;
		}

		// After parameters close parenthesis.
		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		// Return type is introduced with an arrow.
		assert(tokens[index].type == TokenType::arrow);
		index++;

		// Return type. By now only int supported.
		function.return_type = lookup_type_name(program, tokens[index].source);
		assert(function.return_type != TypeId::none);
		index++;
	}

	auto parse_function_body(span<lex::Token const> tokens, size_t & index, Program & program, Function & function) noexcept -> void
	{
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
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, Program & program) noexcept -> expr::FunctionNode
	{
		// Skip fn token.
		index++;

		Function function;

		parse_function_prototype(tokens, index, program, function);
		parse_function_body(tokens, index, program, function);

		// Add the function to the program.
		program.functions.push_back(std::move(function));
		auto const func_id = FunctionId{0, static_cast<unsigned>(program.functions.size() - 1)};

		return expr::FunctionNode{func_id};
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> ExpressionTree;

	auto parse_function_call_expression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope, span<FunctionId const> overload_set) noexcept -> expr::FunctionCallNode
	{
		// Parameter list starts with (
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		// Parse parameters.
		int param_count = 0;

		expr::FunctionCallNode node;
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

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, Program & program, Scope const & scope) noexcept -> expr::IfNode
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		expr::IfNode if_node;
		if_node.condition = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, program, scope));
		assert(expression_type_id(*if_node.condition, program) == TypeId::bool_);

		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		if_node.then_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, program, scope));

		// Expect keyword else to separate then and else cases.
		assert(tokens[index].source == "else");
		index++;

		if_node.else_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, program, scope));

		assert(expression_type_id(*if_node.then_case, program) == expression_type_id(*if_node.else_case, program));

		return if_node;
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
		else if (tokens[index].source == "if")
		{
			return parse_if_expression(tokens, index, program, scope);
		}
		else if (tokens[index].type == TokenType::literal_int)
		{
			return ExpressionTree(parse_number_literal<int>(tokens[index++].source));
		}
		else if (tokens[index].type == TokenType::literal_float)
		{
			return ExpressionTree(parse_number_literal<float>(tokens[index++].source));
		}
		else if (tokens[index].type == TokenType::literal_bool)
		{
			return ExpressionTree(tokens[index++].source[0] == 't'); // if it starts with t it must be bool, and otherwise it must be false.
		}
		else if (tokens[index].type == TokenType::identifier)
		{
			auto const visitor = overload(
				[&](lookup_result::Variable result) -> ExpressionTree
				{
					expr::LocalVariableNode var_node;
					var_node.variable_type = result.variable_type;
					var_node.variable_offset = result.variable_offset;
					index++;
					return var_node;
				},
				[&](lookup_result::GlobalVariable result) -> ExpressionTree
				{
					expr::GlobalVariableNode var_node;
					var_node.variable_type = result.variable_type;
					var_node.variable_offset = result.variable_offset;
					index++;
					return var_node;
				},
				[&](lookup_result::OverloadSet result) -> ExpressionTree
				{
					index++;
					if (tokens[index].type == TokenType::open_parenthesis)
						return parse_function_call_expression(tokens, index, program, scope, result.function_ids);
					else
						return expr::FunctionNode{ result.function_ids[0] }; // TODO: Overload set node?
				},
				[](lookup_result::Nothing) -> ExpressionTree { declare_unreachable(); }
			);
			auto const lookup = lookup_name(scope, program.global_scope, tokens[index].source);
			return std::visit(visitor, lookup);
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

		return resolve_operator_overloading(resolve_operator_precedence(operands, operators), program, scope);
	}

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> ExpressionTree
	{
		size_t index = 0;
		ExpressionTree tree = parse_subexpression(tokens, index, program, scope);
		assert(index == tokens.size()); // Ensure that all tokens were read.
		return tree;
	}

	auto register_declared_variable()
	{

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

	auto bind_function_name(std::string_view function_name, FunctionId function_id, Program & program, Scope & scope) noexcept -> void
	{
		// Special rules for the main function.
		if (function_name == "main")
		{
			// Main function must be in the global scope.
			assert(&scope == &program.global_scope);

			// Main cannot be a extern function.
			assert(!function_id.is_extern);

			// Main must not take parameters and return int.
			Function const & main_function = program.functions[function_id.index];
			assert(main_function.return_type == TypeId::int_);
			assert(main_function.parameter_count == 0);

			// There can only be one main function.
			assert(program.main_function == invalid_function_id);

			// Bind the function as the program's main function.
			program.main_function = function_id;

			// Main function does not go to the list of function names. You cannot lookup main.
		}
		else
		{
			scope.functions.push_back({function_name, function_id});
		}
	}

	auto parse_function_declaration_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::nullopt_t
	{
		// A function declaration statement has the following form:
		// let [name] = [function expr];

		assert(tokens[1].type == TokenType::identifier);
		assert(tokens[2].type == TokenType::assignment);

		// Adding the function to the program before parsing the body allows the body of the function to find itself and be recursive.

		// Add a new function to the program.
		Function & function = program.functions.emplace_back();
		// Add the function name to the scope.
		auto const function_id = FunctionId{0, static_cast<unsigned>(program.functions.size() - 1)};
		size_t index = 4;
		parse_function_prototype(tokens, index, program, function);
		bind_function_name(tokens[1].source, function_id, program, scope);

		// Operate on a local temporary to avoid invalidation of the reference on reallocation.
		Function temp;
		temp.parameter_count = function.parameter_count;
		temp.parameter_size = function.parameter_size;
		temp.stack_frame_size = function.stack_frame_size;
		temp.stack_frame_alignment = function.stack_frame_alignment;
		temp.return_type = function.return_type;
		temp.variables = function.variables;
		parse_function_body(tokens, index, program, temp);
		program.functions[function_id.index] = std::move(temp);

		return std::nullopt;
	}

	auto parse_let_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<StatementTree>
	{
		// If we have directly a function expression, parse it in a special way to handle recursion. TODO: Think of generalizing
		// binding names to function expressions somehow.
		if (tokens[3].source == "fn")
			return parse_function_declaration_statement(tokens, program, scope);
		else
		{
			assert(tokens[1].type == TokenType::identifier);
			assert(tokens[2].type == TokenType::assignment);

			// If the expression returns a function, bind it to its name and return a noop.
			expr::ExpressionTree expression = parse_expression(tokens.subspan(3, tokens.size() - 4), program, scope);
			TypeId const var_type = expression_type_id(expression, program);
			if (var_type == TypeId::function)
			{
				FunctionId const function_id = std::get<expr::FunctionNode>(expression).function_id;
				bind_function_name(tokens[1].source, function_id, program, scope);
				return std::nullopt;
			}
			else
			{
				VariableDeclarationStatementNode node;
				node.variable_offset = add_variable_to_scope(scope, tokens[1].source, var_type, program);
				node.assigned_expression = std::move(expression);
				return node;
			}
		}
	}

	auto parse_expression_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> ExpressionStatementNode
	{
		ExpressionStatementNode node;
		node.expression = parse_expression(tokens.subspan(0, tokens.size() - 1), program, scope);
		return node;
	}

	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<StatementTree>
	{
		// A statement ends with a semicolon.
		assert(tokens.back().type == TokenType::semicolon);

		if (tokens[0].source == "return")
			return parse_return_statement(tokens, program, scope);
		// This will change when let is also used to declare constants, but that's a problem of future Asier.
		else if (tokens[0].source == "let")
			return parse_let_statement(tokens, program, scope);
		else if (lookup_type_name(program, tokens[0].source) != TypeId::none)
			return parse_variable_declaration_statement(tokens, program, scope);
		else
			return parse_expression_statement(tokens, program, scope);
	}

	auto parse_source(std::string_view src) noexcept -> Program
	{
		auto const tokens = lex::tokenize(src);
		Program program;

		size_t index = 0;
		while (index < tokens.size())
		{
			auto statement = parse_statement(next_statement_tokens(tokens, index), program, program.global_scope);
			if (statement)
			{
				assert(statement->index() != 2); // An expression statement is not allowed at the global scope.
				program.global_initialization_statements.push_back(std::move(*statement));
			}
		}

		return program;
	}

} //namespace parser
