#include "syntax_error.hh"
#include "statement.hh"
#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "utils/span.hh"
#include "utils/variant.hh"
#include "utils/overload.hh"
#include "utils/unreachable.hh"
#include "utils/out.hh"
#include <string>
#include <optional>
#include <cassert>
#include <charconv>

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

	struct TypeName
	{
		std::string name;
		enum struct Type { type, struct_template, template_parameter } type;
	};

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

	auto insert_expression(incomplete::Expression & tree, incomplete::Expression & new_node) noexcept -> void
	{
		using incomplete::expression::BinaryOperatorCall;
		BinaryOperatorCall & tree_op = std::get<BinaryOperatorCall>(tree);
		BinaryOperatorCall & new_op = std::get<BinaryOperatorCall>(new_node);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = std::make_unique<incomplete::Expression>(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!has_type<BinaryOperatorCall>(*tree_op.right))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = std::make_unique<incomplete::Expression>(std::move(new_node));
		}
		else
			insert_expression(*tree_op.right, new_node);
	}

	auto resolve_operator_precedence(span<incomplete::Expression> operands, span<Operator const> operators) noexcept -> incomplete::Expression
	{
		using incomplete::expression::BinaryOperatorCall;

		if (operators.empty())
		{
			assert(operands.size() == 1);
			return std::move(operands[0]);
		}

		incomplete::Expression root = BinaryOperatorCall{ operators[0],
				std::make_unique<incomplete::Expression>(std::move(operands[0])),
				std::make_unique<incomplete::Expression>(std::move(operands[1]))
		};

		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = incomplete::Expression(BinaryOperatorCall{operators[i], nullptr, std::make_unique<incomplete::Expression>(std::move(operands[i + 1]))});
			insert_expression(root, new_node);
		}

		return root;
	}

	auto pointer_type_for(incomplete::TypeId type) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId pointer_type;
		pointer_type.is_reference = false;
		pointer_type.is_mutable = false;
		pointer_type.value = incomplete::TypeId::Pointer{std::make_unique<incomplete::TypeId>(type)};
		return pointer_type;
	}

	auto array_type_for(incomplete::TypeId type, int size) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId array_type;
		array_type.is_reference = false;
		array_type.is_mutable = false;
		array_type.value = incomplete::TypeId::Array{std::make_unique<incomplete::TypeId>(type), size};
		return array_type;
	}

	auto array_pointer_type_for(incomplete::TypeId type) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId pointer_type;
		pointer_type.is_reference = false;
		pointer_type.is_mutable = false;
		pointer_type.value = incomplete::TypeId::ArrayPointer{std::make_unique<incomplete::TypeId>(type)};
		return pointer_type;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, span<TypeName const> type_names) noexcept -> std::optional<incomplete::TypeId>;

	auto parse_mutable_pointer_and_array(span<lex::Token const> tokens, size_t & index, incomplete::TypeId type) noexcept -> incomplete::TypeId
	{
		// Look for mutable qualifier.
		if (tokens[index].source == "mut"sv)
		{
			type.is_mutable = true;
			index++;
		}

		// Look for pointer type
		if (tokens[index].source == "*"sv)
		{
			incomplete::TypeId pointer_type = pointer_type_for(type);
			index++;
			return parse_mutable_pointer_and_array(tokens, index, std::move(pointer_type));
		}

		// Look for array type
		if (tokens[index].type == TokenType::open_bracket)
		{
			index++;

			if (tokens[index].type == TokenType::close_bracket)
			{
				incomplete::TypeId pointer_type = array_pointer_type_for(type);
				index++;
				return parse_mutable_pointer_and_array(tokens, index, std::move(pointer_type));
			}
			else
			{
				raise_syntax_error_if_not(tokens[index].type == TokenType::literal_int, "Expected integral constant after [ in array type.");
				int const size = parse_number_literal<int>(tokens[index].source);
				raise_syntax_error_if_not(size > 0, "Array size must be greater than 0.");
				index++;
				raise_syntax_error_if_not(tokens[index].type == TokenType::close_bracket, "Expected ] after array size.");
				index++;
				incomplete::TypeId array_type = array_type_for(type, size);
				return parse_mutable_pointer_and_array(tokens, index, std::move(array_type));
			}
		}

		return type;
	}

	auto parse_template_instantiation_parameter_list(span<lex::Token const> tokens, size_t & index, span<TypeName const> type_names) noexcept -> std::vector<incomplete::TypeId>
	{
		raise_syntax_error_if_not(tokens[index].source == "<", "Expected '<' after template name.");
		index++;

		std::vector<incomplete::TypeId> template_parameters;

		while (true)
		{
			auto type = parse_type_name(tokens, index, type_names);
			raise_syntax_error_if_not(type.has_value(), "Expected type in template instantiation parameter list.");
			template_parameters.push_back(std::move(*type));

			if (tokens[index].source == ">")
				break;

			raise_syntax_error_if_not(tokens[index].type == TokenType::comma, "Expected comma ',' after template parameter.");
			index++;
		}

		raise_syntax_error_if_not(tokens[index].source == ">", "Expected comma '>' at the end of template parameter list.");
		index++;

		return template_parameters;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, span<TypeName const> type_names) noexcept -> std::optional<incomplete::TypeId>
	{
		using namespace incomplete;
		std::string_view const name_to_look_up = tokens[index].source;
		auto const it = std::find_if(type_names.rbegin(), type_names.rend(), [name_to_look_up](TypeName const & type_name) { return type_name.name == name_to_look_up; });
		if (it == type_names.rend())
			return std::nullopt;

		index++;

		int const found_index = static_cast<int>(&*it - type_names.data());

		switch (it->type)
		{
			case TypeName::Type::type:
			{
				TypeId::BaseCase base_case;
				base_case.index = found_index;
				base_case.is_language_reserved = false;
				base_case.is_dependent = false;
				TypeId type;
				type.is_mutable = false;
				type.is_reference = false;
				type.value = base_case;

				return parse_mutable_pointer_and_array(tokens, index, type);
			}
			case TypeName::Type::struct_template:
			{
				TypeId::TemplateInstantiation template_instantiation;
				template_instantiation.template_index = found_index;
				template_instantiation.parameters = parse_template_instantiation_parameter_list(tokens, index, type_names);
				TypeId type;
				type.is_mutable = false;
				type.is_reference = false;
				type.value = std::move(template_instantiation);

				return parse_mutable_pointer_and_array(tokens, index, std::move(type));
			}
			case TypeName::Type::template_parameter:
			{
				TypeId::BaseCase base_case;
				base_case.index = found_index;
				base_case.is_language_reserved = false;
				base_case.is_dependent = true;
				TypeId type;
				type.is_mutable = false;
				type.is_reference = false;
				type.value = base_case;

				return parse_mutable_pointer_and_array(tokens, index, type);
			}
		}

		declare_unreachable();
	}

	auto parse_template_parameter_list(span<lex::Token const> tokens, size_t & index) noexcept -> std::vector<incomplete::TemplateParameter>
	{
		// Skip < token.
		index++;

		std::vector<incomplete::TemplateParameter> parsed_parameters;

		for (;;)
		{
			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier.");

			incomplete::TemplateParameter param;
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as template parameter name.");
			param.name = tokens[index].source;
			parsed_parameters.push_back(std::move(param));
			index++;

			if (tokens[index].source == ">")
			{
				index++;
				break;
			}
			raise_syntax_error_if_not(tokens[index].type == TokenType::comma, "Expected '>' or ',' after template parameter.");
			index++;
		}

		return parsed_parameters;
	}

	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************

	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression;
	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression;
	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Statement;

	auto parse_comma_separated_expression_list(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names,
		TokenType opener = TokenType::open_parenthesis, TokenType delimiter = TokenType::close_parenthesis) noexcept -> std::vector<incomplete::Expression>
	{
		std::vector<incomplete::Expression> parsed_expressions;

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
			parsed_expressions.push_back(parse_expression(tokens, index, type_names));

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

	auto parse_function(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::Function> function) noexcept -> void
	{
		// Parameters must be between parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after fn.");
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			incomplete::Variable var;
			var.type = parse_type_name(tokens, index, type_names);

			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after function parameter type.");
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as function parameter name.");
			var.name = tokens[index].source;
			function->parameter_count++;
			index++;

			if (tokens[index].type == TokenType::close_parenthesis)
				break;

			raise_syntax_error_if_not(tokens[index].type == TokenType::comma, "Expected ',' or ')' after function parameter.");
			index++;
		}

		// After parameters close parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after function parameter list.");
		index++;

		// Return type is introduced with an arrow (optional).
		if (tokens[index].type == TokenType::arrow)
		{
			index++;
			function->return_type = parse_type_name(tokens, index, type_names);
		}

		// Body of the function is enclosed by braces.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_brace, "Expected '{' at start of function body.");
		index++;

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			function->statements.push_back(parse_statement(tokens, index, type_names));
		}

		// Skip closing brace.
		index++;
	}

	auto parse_function_template_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		incomplete::FunctionTemplate function;
		function.template_parameters = parse_template_parameter_list(tokens, index);

		size_t const type_name_stack_size = type_names.size();
		for (incomplete::TemplateParameter const & param : function.template_parameters)
			type_names.push_back({param.name, TypeName::Type::template_parameter});
		
		parse_function(tokens, index, type_names, out(function));

		type_names.resize(type_name_stack_size);

		return incomplete::expression::FunctionTemplate{std::move(function)};
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		// Skip fn token.
		index++;

		if (tokens[index].source == "<"sv)
		{
			return parse_function_template_expression(tokens, index, type_names);
		}

		incomplete::Function function;
		parse_function(tokens, index, type_names, out(function));

		return incomplete::expression::Function{std::move(function)};
	}

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::expression::If
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after if.");
		index++;

		incomplete::expression::If if_node;
		if_node.condition = std::make_unique<incomplete::Expression>(parse_expression(tokens, index, type_names));

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after if condition.");
		index++;

		if_node.then_case = std::make_unique<incomplete::Expression>(parse_expression(tokens, index, type_names));

		// Expect keyword else to separate then and else cases.
		raise_syntax_error_if_not(tokens[index].source == "else", "Expected keyword \"else\" after if expression body.");
		index++;

		if_node.else_case = std::make_unique<incomplete::Expression>(parse_expression(tokens, index, type_names));

		return if_node;
	}
	
	auto all_branches_return(incomplete::Statement const & statement) noexcept -> bool
	{
		auto const visitor = overload(
			[](auto const &) { return false; }, // Default case, does not return
			[](incomplete::statement::Return const &) { return true; },
			[&](incomplete::statement::If const & if_node)
			{
				return all_branches_return(*if_node.then_case)
					&& all_branches_return(*if_node.else_case);
			},
			[&](incomplete::statement::StatementBlock  const & block_node)
			{
				return all_branches_return(block_node.statements.back());
			}
		);
		return std::visit(visitor, statement.as_variant());
	}

	auto parse_statement_block_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::expression::StatementBlock
	{
		// Skip opening brace.
		index++;

		incomplete::expression::StatementBlock node;

		// Parse all statements in the function.
		size_t const stack_size = type_names.size();
		while (tokens[index].type != TokenType::close_brace)
		{
			node.statements.push_back(parse_statement(tokens, index, type_names));
		}
		type_names.resize(stack_size);

		// Ensure that all branches return.
		raise_syntax_error_if_not(all_branches_return(node.statements.back()), "Not all branches of statement block expression return.");

		// Skip closing brace.
		raise_syntax_error_if_not(tokens[index].type == TokenType::close_brace, "Expected '}' at the end of statement block expression.");
		index++;

		return node;
	}

	auto parse_unary_operator(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		Operator const op = parse_operator(tokens[index].source);
		index++;

		incomplete::Expression operand = parse_expression_and_trailing_subexpressions(tokens, index, type_names);

		if (op == Operator::addressof)
		{
			incomplete::expression::Addressof addressof_node;
			addressof_node.operand = std::make_unique<incomplete::Expression>(std::move(operand));
			return addressof_node;
		}
		else if (op == Operator::dereference)
		{
			incomplete::expression::Dereference deref_node;
			deref_node.operand = std::make_unique<incomplete::Expression>(std::move(operand));
			return deref_node;
		}
		else
		{
			incomplete::expression::UnaryOperatorCall op_node;
			op_node.op = op;
			op_node.operand = std::make_unique<incomplete::Expression>(std::move(operand));
			return op_node;
		}
	}

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		if (tokens[index].source == "fn")
			return parse_function_expression(tokens, index, type_names);
		else if (tokens[index].source == "if")
			return parse_if_expression(tokens, index, type_names);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block_expression(tokens, index, type_names);
		else if (tokens[index].type == TokenType::literal_int)
			return incomplete::expression::Literal<int>{parse_number_literal<int>(tokens[index++].source)};
		else if (tokens[index].type == TokenType::literal_float)
			return incomplete::expression::Literal<float>{parse_number_literal<float>(tokens[index++].source)};
		else if (tokens[index].type == TokenType::literal_bool)
			return incomplete::expression::Literal<bool>{tokens[index++].source[0] == 't'}; // if it starts with t it must be true, and otherwise it must be false.
		else if (tokens[index].type == TokenType::identifier)
		{
			// It can be either a constructor call or the naming of a variable/function
			auto type = parse_type_name(tokens, index, type_names);
			if (type)
			{
				incomplete::expression::Constructor ctor_node;
				ctor_node.constructed_type = std::move(*type);
				ctor_node.parameters = parse_comma_separated_expression_list(tokens, index, type_names);
				return ctor_node;
			}
			else
			{
				incomplete::expression::Identifier id_node;
				id_node.name = tokens[index].source;
				index++;
				return id_node;
			}
		}
		else if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			auto expr = parse_expression(tokens, index, type_names);

			// Next token must be close parenthesis.
			raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after parenthesized expression.");
			index++;

			return expr;
		}
		else if (is_unary_operator(tokens[index]))
			return parse_unary_operator(tokens, index, type_names);
		else
			raise_syntax_error("Unrecognized token. Expected expression.");
	}

	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		incomplete::Expression tree = parse_single_expression(tokens, index, type_names);

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
				var_node.owner = std::make_unique<incomplete::Expression>(std::move(tree));
				tree = std::move(var_node);
			}
			// Subscript
			else if (tokens[index].type == TokenType::open_bracket)
			{
				std::vector<incomplete::Expression> params = parse_comma_separated_expression_list(tokens, index, type_names, TokenType::open_bracket, TokenType::close_bracket);

				if (params.size() == 1)
				{
					incomplete::expression::Subscript node;
					node.array = std::make_unique<incomplete::Expression>(std::move(tree));
					node.index = std::make_unique<incomplete::Expression>(std::move(params[0]));
					tree = std::move(node);
				}
				else
				{
					incomplete::expression::FunctionCall node;
					node.parameters.reserve(params.size() + 2);
					node.parameters.push_back(incomplete::expression::Identifier{"[]"});
					node.parameters.push_back(std::move(tree));
					for (auto & param : params)
						node.parameters.push_back(std::move(param));
					tree = std::move(node);
				}
			}
			// Function call
			else if (tokens[index].type == TokenType::open_parenthesis)
			{
				std::vector<incomplete::Expression> params = parse_comma_separated_expression_list(tokens, index, type_names, TokenType::open_bracket, TokenType::close_bracket);
				incomplete::expression::FunctionCall node;
				node.parameters.reserve(params.size() + 1);
				node.parameters.push_back(std::move(tree));
				for (auto & param : params)
					node.parameters.push_back(std::move(param));
				tree = std::move(node);
			}
			else break;
		}

		return tree;
	}

	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Expression
	{
		std::vector<incomplete::Expression> operands;
		std::vector<Operator> operators;

		while (index < tokens.size())
		{
			operands.push_back(parse_expression_and_trailing_subexpressions(tokens, index, type_names));

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

	auto parse_let_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::VariableDeclaration
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
		incomplete::Expression expression = parse_expression(tokens, index, type_names);

		incomplete::statement::VariableDeclaration statement;
		statement.variable_name = name;
		statement.assigned_expression = expression;
		return statement;
	}

	auto parse_return_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::Return
	{
		// Skip return token.
		index++;

		incomplete::statement::Return statement;
		statement.returned_expression = parse_expression(tokens, index, type_names);

		return statement;
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::If
	{
		// Skip the if
		index++;

		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after if.");
		index++;

		incomplete::statement::If statement;
		statement.condition = parse_expression(tokens, index, type_names);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in if statement.");
		index++;

		statement.then_case = std::make_unique<incomplete::Statement>(parse_statement(tokens, index, type_names));

		// For if statement else is optional.
		if (tokens[index].source == "else")
		{
			// Skip else token.
			index++;
			statement.else_case = std::make_unique<incomplete::Statement>(parse_statement(tokens, index, type_names));
		}

		return statement;
	}

	auto parse_statement_block(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::StatementBlock
	{
		// Skip opening }
		index++;

		incomplete::statement::StatementBlock statement_block;

		size_t const stack_size = type_names.size();

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			statement_block.statements.push_back(parse_statement(tokens, index, type_names));
		}
		type_names.resize(stack_size);

		// Skip closing brace.
		index++;

		return statement_block;
	}

	auto parse_while_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::While
	{
		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after while.");
		index++;

		incomplete::statement::While statement;

		// Parse condition. Must return bool.
		statement.condition = parse_expression(tokens, index, type_names);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in while statement.");
		index++;

		// Parse body
		statement.body = std::make_unique<incomplete::Statement>(parse_statement(tokens, index, type_names));

		return statement;
	}

	auto parse_for_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::For
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
		size_t const stack_size = type_names.size();

		// Parse init statement. Must be an expression or a declaration.
		incomplete::Statement init_statement = parse_statement(tokens, index, type_names);
		raise_syntax_error_if_not(
				has_type<incomplete::statement::VariableDeclaration>(init_statement) || 
				has_type<incomplete::statement::ExpressionStatement>(init_statement),
			"init-statement of a for statement must be a variable declaration or an expression.");
		for_statement.init_statement = std::make_unique<incomplete::Statement>(std::move(init_statement));

		// Parse condition. Must return bool.
		for_statement.condition = parse_expression(tokens, index, type_names);

		// Parse ; after condition.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after for statement condition.");
		index++;

		// Parse end expression.
		for_statement.end_expression = parse_expression(tokens, index, type_names);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after for statement end expression.");
		index++;

		// Parse body
		for_statement.body = std::make_unique<incomplete::Statement>(parse_statement(tokens, index, type_names));

		type_names.resize(stack_size);

		return for_statement;
	}

	template <typename Stmt>
	auto parse_break_or_continue_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> Stmt
	{
		// Should check if parsing a loop and otherwise give an error.

		static_cast<void>(tokens, type_names);
		index++;
		return Stmt();
	}

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Statement
	{
		// Skip struct token.
		index++;

		//if (tokens[index].source == "<"sv)
		//	return parse_template_struct_declaration(tokens, index, p);

		// Parse name
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after struct.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as struct name.");
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
			auto var_type = parse_type_name(tokens, index, type_names);
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
				var.initializer_expression = parse_expression(tokens, index, type_names);
			}

			raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "");
			str.member_variables.push_back(std::move(var));
			index++;
		}

		// Skip } token.
		index++;

		type_names.push_back({str.name, TypeName::Type::type});

		return incomplete::statement::StructDeclaration{std::move(str)};
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept -> incomplete::statement::VariableDeclaration
	{
		incomplete::statement::VariableDeclaration statement;
		
		// The second token of the statement is the variable name.
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as variable name.");
		std::string_view const var_name = tokens[index].source;
		index++;

		statement.variable_name = var_name;

		if (tokens[index].type == TokenType::semicolon)
			return statement;

		// The third token is a '='.
		raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' or ';' after variable name in declaration.");
		index++;

		// The rest is the expression assigned to the variable.
		statement.assigned_expression = parse_expression(tokens, index, type_names);
		return statement;
	}

	auto parse_expression_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::statement::ExpressionStatement
	{
		incomplete::statement::ExpressionStatement statement;
		statement.expression = parse_expression(tokens, index, type_names);
		return statement;
	}

	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> incomplete::Statement
	{
		incomplete::Statement result;

		if (tokens[index].source == "let")
			result = parse_let_statement(tokens, index, type_names);
		else if (tokens[index].source == "return")
			result = parse_return_statement(tokens, index, type_names);
		// With if, {}, while and for statements, return to avoid checking for final ';' because it is not needed.
		else if (tokens[index].source == "if")
			return parse_if_statement(tokens, index, type_names);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block(tokens, index, type_names);
		else if (tokens[index].source == "while")
			return parse_while_statement(tokens, index, type_names);
		else if (tokens[index].source == "for")
			return parse_for_statement(tokens, index, type_names);
		else if (tokens[index].source == "break")
			result = parse_break_or_continue_statement<incomplete::statement::Break>(tokens, index, type_names);
		else if (tokens[index].source == "continue")
			result = parse_break_or_continue_statement<incomplete::statement::Continue>(tokens, index, type_names);
		else if (tokens[index].source == "struct")
			return parse_struct_declaration(tokens, index, type_names);
		else
		{
			auto const parsed_type = parse_type_name(tokens, index, type_names);
			if (parsed_type.has_value())
				result = parse_variable_declaration_statement(tokens, index, type_names, *parsed_type);
			else
				result = parse_expression_statement(tokens, index, type_names);
		}

		// A statement must end with a semicolon.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after statement.");
		index++;

		return result;
	}

	auto parse_source(std::string_view src) noexcept -> incomplete::Program
	{
		auto const tokens = lex::tokenize(src);
		incomplete::Program program;

		std::vector<TypeName> type_names;
		type_names.reserve(16);
		type_names.push_back({"int",   TypeName::Type::type});
		type_names.push_back({"float", TypeName::Type::type});
		type_names.push_back({"bool",  TypeName::Type::type});

		size_t index = 0;
		while (index < tokens.size())
		{
			incomplete::Statement statement = parse_statement(tokens, index, type_names);
			raise_syntax_error_if_not(!has_type<incomplete::statement::ExpressionStatement>(statement), "An expression statement is not allowed at the global scope.");
			program.global_initialization_statements.push_back(std::move(statement));
		}

		return program;
	}

}
