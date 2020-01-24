#include "syntax_error.hh"
#include "statement.hh"
#include "parser.hh"
#include "lexer.hh"
#include "span.hh"
#include "variant.hh"
#include "unreachable.hh"
#include <optional>
#include <cassert>

using TokenType = lex::Token::Type;
using namespace std::literals;

namespace parser
{

	constexpr std::string_view keywords[] = {
		// Control flow
		"if",
		"else",
		"for",
		"while",
		"break",
		"continue",

		// Built in types
		"void",
		"int",
		"float",
		"bool",

		// Declarations
		"fn",
		"main",
		"struct",
		"let",
		"mut",

		// Operators
		"and",
		"or",
		"not",
		"xor",
	};
	auto is_function_name_taken(std::string_view name_to_test, incomplete::ScopeStackView scope_stack) noexcept -> bool;
	auto is_name_taken(std::string_view name_to_test, incomplete::ScopeStackView scope_stack) noexcept -> bool;

	template <typename C>
	auto is_name_locally_taken(std::string_view name_to_test, C const & registered_names) noexcept -> bool
	{
		for (auto const & n : registered_names)
			if (name_to_test == n.name)
				return true;
		return false;
	}

	auto is_keyword(std::string_view name) noexcept -> bool
	{
		// TODO: Binary search
		return std::find(keywords, std::end(keywords), name) != std::end(keywords);
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
		if (token_source == "not"sv) return Operator::not;

		switch (token_source[0])
		{
			case '+': return Operator::add;
			case '-': return Operator::subtract;
			case '*': return Operator::multiply;
			case '/': return Operator::divide;
			case '%': return Operator::modulo;
			case '<': return Operator::less;
			case '>': return Operator::greater;
			case '=': return Operator::assign;
			case '&': return Operator::addressof;
		}
		declare_unreachable();
	}

	auto is_unary_operator(lex::Token const & token) noexcept -> bool
	{
		return token.type == TokenType::operator_ &&
			token.source == "-"sv ||
			token.source == "&"sv ||
			token.source == "*"sv ||
			token.source == "not"sv;
	}

	template <typename T>
	auto parse_number_literal(std::string_view token_source) noexcept -> T
	{
		T value;
		std::from_chars(token_source.data(), token_source.data() + token_source.size(), value);
		return value;
	}

	auto insert_expression(incomplete::ExpressionTree & tree, incomplete::ExpressionTree & new_node) noexcept -> void
	{
		using incomplete::expression::OperatorCall;
		OperatorCall & tree_op = std::get<OperatorCall>(tree);
		OperatorCall & new_op = std::get<OperatorCall>(new_node);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = std::make_unique<incomplete::ExpressionTree>(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!has_type<OperatorCall>(*tree_op.right))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = std::make_unique<incomplete::ExpressionTree>(std::move(new_node));
		}
		else
			insert_expression(*tree_op.right, new_node);
	}

	auto resolve_operator_precedence(span<incomplete::ExpressionTree> operands, span<Operator const> operators) noexcept -> incomplete::ExpressionTree
	{
		if (operators.empty())
		{
			assert(operands.size() == 1);
			return std::move(operands[0]);
		}

		incomplete::ExpressionTree root = incomplete::expression::OperatorCall{ operators[0],
				std::make_unique<incomplete::ExpressionTree>(std::move(operands[0])),
				std::make_unique<incomplete::ExpressionTree>(std::move(operands[1]))
		};

		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = incomplete::ExpressionTree(incomplete::expression::OperatorCall{operators[i], nullptr, std::make_unique<incomplete::ExpressionTree>(std::move(operands[i + 1]))});
			insert_expression(root, new_node);
		}

		return root;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> std::optional<incomplete::DependentTypeId>;

	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************

	auto parse_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::ExpressionTree;

	auto parse_comma_separated_expression_list(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack,
		TokenType opener = TokenType::open_parenthesis, TokenType delimiter = TokenType::close_parenthesis) noexcept -> std::vector<incomplete::ExpressionTree>
	{
		std::vector<incomplete::ExpressionTree> parsed_expressions;

		raise_syntax_error_if_not(tokens[index].type == opener, "Expected '('.");
		index++;

		// Check for empty list.
		if (tokens[index].type == TokenType::close_parenthesis)
		{
			index++;
			return parsed_expressions;
		}

		for (;;)
		{
			// Parse a parameter.
			parsed_expressions.push_back(parse_expression(tokens, index, scope_stack));

			// If after a parameter we find a delimiter token, end parameter list.
			if (tokens[index].type == delimiter)
			{
				index++;
				break;
			}
			// If we find a comma, parse next parameter.
			else if (tokens[index].type == TokenType::comma)
				index++;
			// Anything else we find is wrong.
			else
				raise_syntax_error("Expected ')' or ',' after expression.");
		}

		return parsed_expressions;
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::ExpressionTree;
	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept->incomplete::ExpressionTree;
	auto parse_statement_block_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept->incomplete::ExpressionTree;
	auto parse_unary_operator(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept->incomplete::ExpressionTree;

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::ExpressionTree
	{
		if (tokens[index].source == "fn")
			return parse_function_expression(tokens, index, scope_stack);
		else if (tokens[index].source == "if")
			return parse_if_expression(tokens, index, scope_stack);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block_expression(tokens, index, scope_stack);
		else if (tokens[index].type == TokenType::literal_int)
			return incomplete::expression::Literal<int>{parse_number_literal<int>(tokens[index++].source)};
		else if (tokens[index].type == TokenType::literal_float)
			return incomplete::expression::Literal<int>{parse_number_literal<float>(tokens[index++].source)};
		else if (tokens[index].type == TokenType::literal_bool)
			return incomplete::expression::Literal<bool>{tokens[index++].source[0] == 't'}; // if it starts with t it must be true, and otherwise it must be false.
		else if (tokens[index].type == TokenType::identifier)
		{
			// TODO
			mark_as_to_do("Identifiers");
		}
		else if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			auto expr = parse_expression(tokens, index, scope_stack);

			// Next token must be close parenthesis.
			raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after parenthesized expression.");
			index++;

			return expr;
		}
		else if (is_unary_operator(tokens[index]))
			return parse_unary_operator(tokens, index, scope_stack);
		else
			raise_syntax_error("Unrecognized token. Expected expression.");
	}

	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::ExpressionTree
	{
		incomplete::ExpressionTree tree = parse_single_expression(tokens, index, scope_stack);

		while (index < tokens.size())
		{
			// Loop to possibly parse chains of member accesses.
			if (tokens[index].type == TokenType::period)
			{
				index++;

				raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected member name after '.'.");
				std::string_view const member_name = tokens[index].source;
				index++;

				incomplete::expression::MemberVariable var_node;
				var_node.name = member_name;
				var_node.owner = std::make_unique<incomplete::ExpressionTree>(std::move(tree));
				tree = std::move(var_node);
			}
			// Subscript
			else if (tokens[index].type == TokenType::open_bracket)
			{
				std::vector<incomplete::ExpressionTree> params = parse_comma_separated_expression_list(tokens, index, scope_stack, TokenType::open_bracket, TokenType::close_bracket);

				if (params.size() == 1)
				{
					incomplete::expression::Subscript node;
					node.array = std::make_unique<incomplete::ExpressionTree>(std::move(tree));
					node.index = std::make_unique<incomplete::ExpressionTree>(std::move(params[0]));
					tree = std::move(node);
				}
				else
				{
					incomplete::expression::FunctionCall node;
					node.parameters.reserve(params.size() + 2);
					node.parameters.push_back(incomplete::expression::OverloadSetNode{"[]"});
					node.parameters.push_back(std::move(tree));
					for (auto & param : params)
						node.parameters.push_back(std::move(param));
					tree = std::move(node);
				}
			}
			// Function call
			else if (tokens[index].type == TokenType::open_parenthesis)
			{
				std::vector<incomplete::ExpressionTree> params = parse_comma_separated_expression_list(tokens, index, scope_stack, TokenType::open_bracket, TokenType::close_bracket);
				incomplete::expression::FunctionCall node;
				node.parameters.reserve(params.size() + 1);
				node.parameters.push_back(std::move(tree));
				for (auto & param : params)
					node.parameters.push_back(std::move(param));
				tree = std::move(node);
			}
			else break;
		}
	}

	auto parse_expression(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::ExpressionTree
	{
		std::vector<incomplete::ExpressionTree> operands;
		std::vector<Operator> operators;

		while (index < tokens.size())
		{
			operands.push_back(parse_expression_and_trailing_subexpressions(tokens, index, scope_stack));

			// If the next token is an operator, parse the operator and repeat. Otherwise end the loop and return the expression.
			if (index < tokens.size() && tokens[index].type == TokenType::operator_)
			{
				operators.push_back(parse_operator(tokens[index].source));
				index++;
			}
			else break;
		}

		return resolve_operator_precedence(operands, operators);
	}

	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************

	auto parse_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> std::optional<incomplete::Statement>;

	auto parse_let_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::VariableDeclaration
	{
		// Skip let token.
		index++;

		bool is_mutable = false;

		// Look for mutable qualifier.
		if (tokens[index].source == "mut")
		{
			is_mutable = true;
			index++;
		}

		std::string_view name;
		bool is_operator = false;

		// Parsing an operator function
		if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			if (tokens[index].type == TokenType::open_bracket)
			{
				index++;
				raise_syntax_error_if_not(tokens[index].type == TokenType::close_bracket, "Expected operator after '(' in function declaration.");
				name = "[]"sv;
			}
			else
			{
				raise_syntax_error_if_not(tokens[index].type == TokenType::operator_, "Expected operator after '(' in function declaration.");
				name = tokens[index].source;
			}
			index++;
			raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after operator in function declaration.");
			index++;

			is_operator = true;
		}
		else
		{
			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after let.");
			name = tokens[index].source;
			index++;
		}

		raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' after identifier in let declaration.");
		index++;

		// If the expression returns a function, bind it to its name and return a noop.
		incomplete::ExpressionTree expression = parse_expression(tokens, index, scope_stack);

		incomplete::Variable var;
		var.name = name;
		scope_stack.back()->variables.push_back(std::move(var));

		incomplete::statement::VariableDeclaration statement;
		statement.variable_name = name;
		statement.assigned_expression = expression;
		return statement;
	}

	auto parse_return_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::Return
	{
		raise_syntax_error_if_not(scope_stack.size() > 1, "A return statement cannot appear in the global scope.");

		// Skip return token.
		index++;

		incomplete::statement::Return statement;
		statement.returned_expression = parse_expression(tokens, index, scope_stack);

		return statement;
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::If
	{
		// Skip the if
		index++;

		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after if.");
		index++;

		incomplete::statement::If statement;
		statement.condition = parse_expression(tokens, index, scope_stack);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in if statement.");
		index++;

		auto then_case = parse_statement(tokens, index, scope_stack);
		raise_syntax_error_if_not(then_case.has_value(), "No-op statement not allowed as body of if statement branch.");
		statement.then_case = std::make_unique<incomplete::Statement>(*std::move(then_case));

		// For if statement else is optional.
		if (tokens[index].source == "else")
		{
			// Skip else token.
			index++;
			auto else_case = parse_statement(tokens, index, scope_stack);
			raise_syntax_error_if_not(else_case.has_value(), "No-op statement not allowed as body of if statement branch.");
			statement.else_case = std::make_unique<incomplete::Statement>(*std::move(else_case));
		}

		return statement;
	}

	auto parse_statement_block(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::StatementBlock
	{
		// Skip opening }
		index++;

		incomplete::statement::StatementBlock statement_block;

		scope_stack.push_back(&statement_block.scope);

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			auto statement = parse_statement(tokens, index, scope_stack);
			if (statement)
				statement_block.statements.push_back(std::move(*statement));
		}
		scope_stack.pop_back();

		// Skip closing brace.
		index++;

		return statement_block;
	}

	auto parse_while_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::While
	{
		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after while.");
		index++;

		incomplete::statement::While statement;

		// Parse condition. Must return bool.
		statement.condition = parse_expression(tokens, index, scope_stack);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in while statement.");
		index++;

		// Parse body
		auto body = parse_statement(tokens, index, scope_stack);
		raise_syntax_error_if_not(body.has_value(), "No-op statement not allowed as body of while statement.");
		statement.body = std::make_unique<incomplete::Statement>(std::move(*body));

		return statement;
	}

	auto parse_for_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::For
	{
		// Syntax of for loop
		// for (declaration-or-expression; condition-expression; end-expression)
		//	   statement 

		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after for.");
		index++;

		incomplete::statement::For for_statement;
		scope_stack.push_back(&for_statement.scope);

		// Parse init statement. Must be an expression or a declaration.
		auto init_statement = parse_statement(tokens, index, scope_stack);
		raise_syntax_error_if_not(
			init_statement.has_value() && (
				has_type<incomplete::statement::VariableDeclaration>(*init_statement) || 
				has_type<incomplete::statement::ExpressionStatement>(*init_statement)),
			"init-statement of a for statement must be a variable declaration or an expression.");
		for_statement.init_statement = std::make_unique<incomplete::Statement>(std::move(*init_statement));

		// Parse condition. Must return bool.
		for_statement.condition = parse_expression(tokens, index, scope_stack);

		// Parse ; after condition.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after for statement condition.");
		index++;

		// Parse end expression.
		for_statement.end_expression = parse_expression(tokens, index, scope_stack);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after for statement end expression.");
		index++;

		// Parse body
		auto body = parse_statement(tokens, index, scope_stack);
		raise_syntax_error_if_not(body.has_value(), "No-op statement not allowed as body of for statement.");
		for_statement.body = std::make_unique<incomplete::Statement>(std::move(*body));

		scope_stack.pop_back();

		return for_statement;
	}

	template <typename Stmt>
	auto parse_break_or_continue_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> Stmt
	{
		// Should check if parsing a loop and otherwise give an error.

		static_cast<void>(tokens, p);
		index++;
		return Stmt();
	}

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> std::nullopt_t
	{
		// Skip struct token.
		index++;

		//if (tokens[index].source == "<"sv)
		//	return parse_template_struct_declaration(tokens, index, p);

		// Parse name
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after struct.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as struct name.");
		raise_syntax_error_if_not(!is_name_taken(tokens[index].source, scope_stack), "More than one struct with the same name.");
		auto const type_name = tokens[index].source;
		index++;

		// Skip { token.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_brace, "Expected '{' after struct name.");
		index++;

		incomplete::Struct str;
		str.name = type_name;

		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			incomplete::MemberVariable var;
			auto var_type = parse_type_name(tokens, index, scope_stack);
			raise_syntax_error_if_not(var_type.has_value(), "Expected type in struct member declaration.");
			var.type = std::move(*var_type);
			raise_syntax_error_if_not(!var.type.is_reference, "Member variable cannot be reference.");
			raise_syntax_error_if_not(!var.type.is_mutable, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name in member variable declaration.");
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as member variable name.");
			raise_syntax_error_if_not(!is_name_locally_taken(tokens[index].source, str.member_variables), "More than one member variable with the same name.");
			var.name = tokens[index].source;
			index++;

			// Initialization expression.
			if (tokens[index].source == "=")
			{
				index++;
				var.initializer_expression = parse_expression(tokens, index, scope_stack);
			}

			raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "");
			str.member_variables.push_back(std::move(var));
			index++;
		}

		// Skip } token.
		index++;

		scope_stack.back()->structs.push_back(std::move(str));

		return std::nullopt;
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack, incomplete::DependentTypeId type) noexcept -> incomplete::statement::VariableDeclaration
	{
		incomplete::statement::VariableDeclaration statement;
		
		// The second token of the statement is the variable name.
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as variable name.");
		raise_syntax_error_if_not(!is_name_taken(tokens[index].source, scope_stack), "More than one local variable with the same name.");
		std::string_view const var_name = tokens[index].source;
		index++;

		statement.variable_name = var_name;

		incomplete::Variable var;
		var.name = var_name;
		var.type = type;
		scope_stack.back()->variables.push_back(std::move(var));

		if (tokens[index].type == TokenType::semicolon)
			return statement;

		// The third token is a '='.
		raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' or ';' after variable name in declaration.");
		index++;

		// The rest is the expression assigned to the variable.
		statement.assigned_expression = parse_expression(tokens, index, scope_stack);
		return statement;
	}

	auto parse_expression_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> incomplete::statement::ExpressionStatement
	{
		incomplete::statement::ExpressionStatement statement;
		statement.expression = parse_expression(tokens, index, scope_stack);
		return statement;
	}

	auto parse_statement(span<lex::Token const> tokens, size_t & index, incomplete::ScopeStack & scope_stack) noexcept -> std::optional<incomplete::Statement>
	{
		std::optional<incomplete::Statement> result;

		if (tokens[index].source == "let")
			result = parse_let_statement(tokens, index, scope_stack);
		else if (tokens[index].source == "return")
			result = parse_return_statement(tokens, index, scope_stack);
		// With if, {}, while and for statements, return to avoid checking for final ';' because it is not needed.
		else if (tokens[index].source == "if")
			return parse_if_statement(tokens, index, scope_stack);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block(tokens, index, scope_stack);
		else if (tokens[index].source == "while")
			return parse_while_statement(tokens, index, scope_stack);
		else if (tokens[index].source == "for")
			return parse_for_statement(tokens, index, scope_stack);
		else if (tokens[index].source == "break")
			result = parse_break_or_continue_statement<incomplete::statement::Break>(tokens, index, scope_stack);
		else if (tokens[index].source == "continue")
			result = parse_break_or_continue_statement<incomplete::statement::Continue>(tokens, index, scope_stack);
		else if (tokens[index].source == "struct")
			return parse_struct_declaration(tokens, index, scope_stack);
		else
		{
			auto const parsed_type = parse_type_name(tokens, index, scope_stack);
			if (parsed_type.has_value())
				result = parse_variable_declaration_statement(tokens, index, scope_stack, *parsed_type);
			else
				result = parse_expression_statement(tokens, index, scope_stack);
		}

		// A statement must end with a semicolon.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after statement.");
		index++;

		return result;
	}

	auto parse_source(std::string_view src) noexcept -> incomplete::Scope
	{
		auto const tokens = lex::tokenize(src);
		incomplete::Scope global_scope;
		std::vector<incomplete::Statement> statements;

		incomplete::ScopeStack scope_stack;
		scope_stack.push_back(&global_scope);

		size_t index = 0;
		while (index < tokens.size())
		{
			auto statement = parse_statement(tokens, index, scope_stack);
			if (statement)
			{
				raise_syntax_error_if_not(!has_type<incomplete::statement::ExpressionStatement>(*statement), "An expression statement is not allowed at the global scope.");
				statements.push_back(std::move(*statement));
			}
		}

		return global_scope;
	}

}
