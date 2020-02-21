#include "parser.hh"
#include "incomplete_statement.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "lexer.hh"
#include "utils/algorithm.hh"
#include "utils/load_dll.hh"
#include "utils/out.hh"
#include "utils/overload.hh"
#include "utils/span.hh"
#include "utils/string.hh"
#include "utils/unreachable.hh"
#include "utils/variant.hh"
#include <cassert>
#include <charconv>
#include <optional>
#include <filesystem>
#include <string>

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
		"char",

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
		if (token_source == "not"sv) return Operator::not_;
		if (token_source == "<<"sv) return Operator::bit_shift_left;
		if (token_source == ">>"sv) return Operator::bit_shift_right;

		switch (token_source[0])
		{
			case '+': return Operator::add;
			case '-': return Operator::subtract;
			case '*': return Operator::multiply;
			case '/': return Operator::divide;
			case '%': return Operator::modulo;
			case '<': return Operator::less;
			case '>': return Operator::greater;
			case '&': return Operator::bitwise_and;
			case '|': return Operator::bitwise_or;
			case '^': return Operator::bitwise_xor;
			case '~': return Operator::bitwise_not;
			case '=': return Operator::assign;
		}
		declare_unreachable();
	}

	auto parse_operator_tokens(span<lex::Token const> tokens, size_t & index) noexcept -> expected<std::string_view, SyntaxError>
	{
		if (tokens[index].type == TokenType::open_bracket)
		{
			index++;
			if (tokens[index].type != TokenType::close_bracket) 
				return make_syntax_error("Expected operator after '(' in function declaration.");
			index++;
			return "[]"sv;
		}
		else
		{
			if (tokens[index].type != TokenType::operator_) 
				return make_syntax_error("Expected operator name after keyword \"operator\".");
			return tokens[index++].source;
		}
	}

	auto is_unary_operator(lex::Token const & token) noexcept -> bool
	{
		return token.type == TokenType::operator_ &&
			token.source == "-"sv ||
			token.source == "&"sv ||
			token.source == "*"sv ||
			token.source == "~"sv ||
			token.source == "not"sv;
	}

	template <typename T>
	auto parse_number_literal(std::string_view token_source) noexcept -> T
	{
		T value;
		std::from_chars(token_source.data(), token_source.data() + token_source.size(), value);
		return value;
	}

	auto parse_string_literal(std::string_view token_source) noexcept -> std::string
	{
		return std::string(token_source.substr(1, token_source.size() - 2));
	}

	auto insert_expression(incomplete::Expression & tree, incomplete::Expression & new_node) noexcept -> void
	{
		using incomplete::expression::BinaryOperatorCall;
		BinaryOperatorCall & tree_op = std::get<BinaryOperatorCall>(tree);
		BinaryOperatorCall & new_op = std::get<BinaryOperatorCall>(new_node);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = allocate(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!has_type<BinaryOperatorCall>(*tree_op.right))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = allocate(std::move(new_node));
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
				allocate(std::move(operands[0])),
				allocate(std::move(operands[1]))
		};

		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = incomplete::Expression(BinaryOperatorCall{operators[i], nullptr, allocate(std::move(operands[i + 1]))});
			insert_expression(root, new_node);
		}

		return root;
	}

	auto pointer_type_for(incomplete::TypeId type) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId pointer_type;
		pointer_type.is_reference = false;
		pointer_type.is_mutable = false;
		pointer_type.value = incomplete::TypeId::Pointer{allocate(std::move(type))};
		return pointer_type;
	}

	auto array_type_for(incomplete::TypeId type, incomplete::Expression size) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId array_type;
		array_type.is_reference = false;
		array_type.is_mutable = false;
		array_type.value = incomplete::TypeId::Array{allocate(std::move(type)), allocate(std::move(size))};
		return array_type;
	}

	auto array_pointer_type_for(incomplete::TypeId type) noexcept -> incomplete::TypeId
	{
		incomplete::TypeId pointer_type;
		pointer_type.is_reference = false;
		pointer_type.is_mutable = false;
		pointer_type.value = incomplete::TypeId::ArrayPointer{allocate(std::move(type))};
		return pointer_type;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<std::optional<incomplete::TypeId>, SyntaxError>;
	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>;
	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>;
	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Statement, SyntaxError>;

	auto parse_mutable_pointer_and_array(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept 
		-> expected<incomplete::TypeId, SyntaxError>
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
			incomplete::TypeId pointer_type = pointer_type_for(std::move(type));
			index++;
			return parse_mutable_pointer_and_array(tokens, index, type_names, std::move(pointer_type));
		}

		// Look for array type
		if (tokens[index].type == TokenType::open_bracket)
		{
			index++;

			if (tokens[index].type == TokenType::close_bracket)
			{
				incomplete::TypeId pointer_type = array_pointer_type_for(std::move(type));
				index++;
				return parse_mutable_pointer_and_array(tokens, index, type_names, std::move(pointer_type));
			}
			else
			{
				try_call_decl(incomplete::Expression size, parse_expression(tokens, index, type_names));
				if (tokens[index].type != TokenType::close_bracket) return make_syntax_error("Expected ] after array size.");
				index++;
				incomplete::TypeId array_type = array_type_for(std::move(type), std::move(size));
				return parse_mutable_pointer_and_array(tokens, index, type_names, std::move(array_type));
			}
		}

		return std::move(type);
	}

	auto parse_mutable_pointer_array_and_reference(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept 
		-> expected<incomplete::TypeId, SyntaxError>
	{
		try_call(assign_to(type), parse_mutable_pointer_and_array(tokens, index, type_names, std::move(type)));

		// Look for reference qualifier.
		if (tokens[index].source == "&"sv)
		{
			type.is_reference = true;
			index++;
		}

		return std::move(type);
	}

	auto parse_template_instantiation_parameter_list(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<std::vector<incomplete::TypeId>, SyntaxError>
	{
		if (tokens[index].source != "<") return make_syntax_error("Expected '<' after template name.");
		index++;

		std::vector<incomplete::TypeId> template_parameters;

		while (true)
		{
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (!type.has_value()) return make_syntax_error("Expected type in template instantiation parameter list.");
			template_parameters.push_back(std::move(*type));

			if (tokens[index].source == ">")
				break;

			if(!(tokens[index].type == TokenType::comma)) return make_syntax_error("Expected comma ',' after template parameter.");
			index++;
		}

		if (tokens[index].source != ">") return make_syntax_error("Expected comma '>' at the end of template parameter list.");
		index++;

		return template_parameters;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<std::optional<incomplete::TypeId>, SyntaxError>
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
			case TypeName::Type::template_parameter:
			{
				TypeId::BaseCase base_case;
				base_case.name = name_to_look_up;
				TypeId type;
				type.is_mutable = false;
				type.is_reference = false;
				type.value = base_case;

				return parse_mutable_pointer_array_and_reference(tokens, index, type_names, std::move(type));
			}
			case TypeName::Type::struct_template:
			{
				TypeId::TemplateInstantiation template_instantiation;
				template_instantiation.template_name = name_to_look_up;
				try_call(assign_to(template_instantiation.parameters), parse_template_instantiation_parameter_list(tokens, index, type_names));
				TypeId type;
				type.is_mutable = false;
				type.is_reference = false;
				type.value = std::move(template_instantiation);

				return parse_mutable_pointer_array_and_reference(tokens, index, type_names, std::move(type));
			}
		}

		declare_unreachable();
	}

	auto parse_template_parameter_list(span<lex::Token const> tokens, size_t & index) noexcept -> expected<std::vector<incomplete::TemplateParameter>, SyntaxError>
	{
		// Skip < token.
		index++;

		std::vector<incomplete::TemplateParameter> parsed_parameters;

		for (;;)
		{
			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier.");

			incomplete::TemplateParameter param;
			if (is_keyword(tokens[index].source)) return make_syntax_error("Cannot use a keyword as template parameter name.");
			param.name = tokens[index].source;
			parsed_parameters.push_back(std::move(param));
			index++;

			if (tokens[index].source == ">")
			{
				index++;
				break;
			}
			if (tokens[index].type != TokenType::comma) return make_syntax_error("Expected '>' or ',' after template parameter.");
			index++;
		}

		return parsed_parameters;
	}

	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************
	//******************************************************************************************************************************************************************

	auto parse_comma_separated_expression_list(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names,
		TokenType opener = TokenType::open_parenthesis, TokenType delimiter = TokenType::close_parenthesis) noexcept -> expected<std::vector<incomplete::Expression>, SyntaxError>
	{
		std::vector<incomplete::Expression> parsed_expressions;

		if (tokens[index].type != opener) return make_syntax_error("Expected '('.");
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
			try_call(parsed_expressions.push_back, parse_expression(tokens, index, type_names));

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
				return make_syntax_error("Expected ')' or ',' after expression.");
		}

		return parsed_expressions;
	}

	auto parse_designated_initializer_list(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) 
		-> expected<std::vector<incomplete::DesignatedInitializer>, SyntaxError>
	{
		// Parameter list starts with (
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after type name in struct constructor.");
		index++;

		std::vector<incomplete::DesignatedInitializer> initializers;

		for (;;)
		{
			// A member initializer by name starts with a period.
			if (tokens[index].type != TokenType::period) return make_syntax_error("Expected '.' in designated initializer.");
			index++;

			incomplete::DesignatedInitializer parameter;

			// Find the member to initialize.
			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected member name after '.' in designated initializer.");
			parameter.member_name = tokens[index].source;
			index++;

			// Next token must be =
			if (tokens[index].source != "=") return make_syntax_error("Expected '=' after member name in designated initializer.");
			index++;

			// Parse a parameter.
			try_call(assign_to(parameter.assigned_expression), parse_expression(tokens, index, type_names));

			initializers.push_back(std::move(parameter));

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
				return make_syntax_error("Expected ')' or ',' after designated initializer.");
		}

		return initializers;
	}

	[[nodiscard]] auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::FunctionPrototype> function) noexcept 
		-> expected<void, SyntaxError>
	{
		// Parameters must be between parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after fn.");
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			incomplete::FunctionParameter var;
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (!type.has_value()) return make_syntax_error("Parameter type not found.");
			var.type = std::move(*type);

			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after function parameter type.");
			if (is_keyword(tokens[index].source)) return make_syntax_error("Cannot use a keyword as function parameter name.");
			var.name = tokens[index].source;
			function->parameters.push_back(std::move(var));
			index++;

			if (tokens[index].type == TokenType::close_parenthesis)
				break;

			if (tokens[index].type != TokenType::comma) return make_syntax_error("Expected ',' or ')' after function parameter.");
			index++;
		}

		// After parameters close parenthesis.
		if (tokens[index].type != TokenType::close_parenthesis)  return make_syntax_error("Expected ')' after function parameter list.");
		index++;

		// Return type is introduced with an arrow (optional).
		if (tokens[index].type == TokenType::arrow)
		{
			index++;
			try_call(assign_to(function->return_type), parse_type_name(tokens, index, type_names));
		}

		return success;
	}

	[[nodiscard]] auto parse_function_body(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::Function> function) noexcept -> expected<void, SyntaxError>
	{
		// Body of the function is enclosed by braces.
		if (tokens[index].type != TokenType::open_brace) return make_syntax_error("Expected '{' at start of function body.");
		index++;

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			try_call(function->statements.push_back, parse_statement(tokens, index, type_names));
		}

		// Skip closing brace.
		index++;

		return success;
	}

	auto parse_function_template_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
	{
		incomplete::FunctionTemplate function;
		try_call(assign_to(function.template_parameters), parse_template_parameter_list(tokens, index));

		size_t const type_name_stack_size = type_names.size();
		for (incomplete::TemplateParameter const & param : function.template_parameters)
			type_names.push_back({param.name, TypeName::Type::template_parameter});
		
		try_call_void(parse_function_prototype(tokens, index, type_names, out(function)));
		try_call_void(parse_function_body(tokens, index, type_names, out(function)));

		type_names.resize(type_name_stack_size);

		return incomplete::expression::FunctionTemplate{std::move(function)};
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
	{
		// Skip fn token.
		index++;

		if (tokens[index].source == "<"sv)
		{
			return parse_function_template_expression(tokens, index, type_names);
		}

		incomplete::Function function;
		try_call_void(parse_function_prototype(tokens, index, type_names, out(function)));
		try_call_void(parse_function_body(tokens, index, type_names, out(function)));

		return incomplete::expression::Function{std::move(function)};
	}

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::expression::If, SyntaxError>
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after if.");
		index++;

		incomplete::expression::If if_node;
		try_call(assign_to(if_node.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after if condition.");
		index++;

		try_call(assign_to(if_node.then_case), parse_expression(tokens, index, type_names));

		// Expect keyword else to separate then and else cases.
		if (tokens[index].source != "else") return make_syntax_error("Expected keyword \"else\" after if expression body.");
		index++;

		try_call(assign_to(if_node.else_case), parse_expression(tokens, index, type_names));

		return std::move(if_node);
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

	auto parse_statement_block_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::expression::StatementBlock, SyntaxError>
	{
		// Skip opening brace.
		index++;

		incomplete::expression::StatementBlock node;

		// Parse all statements in the function.
		size_t const stack_size = type_names.size();
		while (tokens[index].type != TokenType::close_brace)
		{
			try_call(node.statements.push_back, parse_statement(tokens, index, type_names));
		}
		type_names.resize(stack_size);

		// Ensure that all branches return.
		if (!all_branches_return(node.statements.back())) return make_syntax_error("Not all branches of statement block expression return.");

		// Skip closing brace.
		if (tokens[index].type != TokenType::close_brace) return make_syntax_error("Expected '}' at the end of statement block expression.");
		index++;

		return node;
	}

	auto parse_unary_operator(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
	{
		Operator const op = parse_operator(tokens[index].source);
		index++;

		incomplete::Expression operand;
		try_call(assign_to(operand), parse_expression_and_trailing_subexpressions(tokens, index, type_names));

		if (op == Operator::addressof)
		{
			incomplete::expression::Addressof addressof_node;
			addressof_node.operand = allocate(std::move(operand));
			return std::move(addressof_node);
		}
		else if (op == Operator::dereference)
		{
			incomplete::expression::Dereference deref_node;
			deref_node.operand = allocate(std::move(operand));
			return std::move(deref_node);
		}
		else
		{
			incomplete::expression::UnaryOperatorCall op_node;
			op_node.op = op;
			op_node.operand = allocate(std::move(operand));
			return std::move(op_node);
		}
	}

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
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
		else if (tokens[index].type == TokenType::literal_string)
			return incomplete::expression::Literal<std::string>{parse_string_literal(tokens[index++].source)};
		else if (tokens[index].source == "operator")
		{
			index++;
			try_call_decl(std::string_view const name, parse_operator_tokens(tokens, index));
			incomplete::expression::Identifier id_node;
			id_node.name = name;
			return std::move(id_node);
		}
		else if (tokens[index].source == "data")
		{
			index++;
			incomplete::expression::DataCall data_node;
			if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after data.");
			index++;
			try_call(assign_to(data_node.operand), parse_expression(tokens, index, type_names));
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after operand for data.");
			index++;
			return std::move(data_node);
		}
		else if (tokens[index].source == "size")
		{
			index++;
			incomplete::expression::SizeCall size_node;
			if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after size.");
			index++;
			try_call(assign_to(size_node.operand), parse_expression(tokens, index, type_names));
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after operand for size.");
			index++;
			return std::move(size_node);
		}
		else if (tokens[index].type == TokenType::identifier)
		{
			// It can be either a constructor call or the naming of a variable/function
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (type.has_value())
			{
				if (tokens[index + 1].type == TokenType::period)
				{
					incomplete::expression::DesignatedInitializerConstructor ctor_node;
					ctor_node.constructed_type = std::move(*type);
					try_call(assign_to(ctor_node.parameters), parse_designated_initializer_list(tokens, index, type_names));
					return std::move(ctor_node);
				}
				else
				{
					incomplete::expression::Constructor ctor_node;
					ctor_node.constructed_type = std::move(*type);
					try_call(assign_to(ctor_node.parameters), parse_comma_separated_expression_list(tokens, index, type_names));
					return std::move(ctor_node);
				}
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
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after parenthesized expression.");
			index++;

			return expr;
		}
		else if (is_unary_operator(tokens[index]))
			return parse_unary_operator(tokens, index, type_names);
		else
			return make_syntax_error("Unrecognized token. Expected expression.");
	}

	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
	{
		try_call_decl(incomplete::Expression tree, parse_single_expression(tokens, index, type_names));

		while (index < tokens.size())
		{
			// Loop to possibly parse chains of member accesses.
			if (tokens[index].type == TokenType::period)
			{
				index++;

				if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected member name after '.'.");
				std::string_view const member_name = tokens[index].source;
				index++;

				incomplete::expression::MemberVariable var_node;
				var_node.name = member_name;
				var_node.owner = allocate(std::move(tree));
				tree = std::move(var_node);
			}
			// Subscript
			else if (tokens[index].type == TokenType::open_bracket)
			{
				try_call_decl(std::vector<incomplete::Expression> params, parse_comma_separated_expression_list(tokens, index, type_names, TokenType::open_bracket, TokenType::close_bracket));

				if (params.size() == 1)
				{
					incomplete::expression::Subscript node;
					node.array = allocate(std::move(tree));
					node.index = allocate(std::move(params[0]));
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
				try_call_decl(std::vector<incomplete::Expression> params, parse_comma_separated_expression_list(tokens, index, type_names));
				incomplete::expression::FunctionCall node;
				node.parameters.reserve(params.size() + 1);
				node.parameters.push_back(std::move(tree));
				for (auto & param : params)
					node.parameters.push_back(std::move(param));
				tree = std::move(node);
			}
			else break;
		}

		return std::move(tree);
	}

	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, SyntaxError>
	{
		std::vector<incomplete::Expression> operands;
		std::vector<Operator> operators;

		while (index < tokens.size())
		{
			try_call(operands.push_back, parse_expression_and_trailing_subexpressions(tokens, index, type_names));

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

	auto parse_let_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::VariableDeclaration, SyntaxError>
	{
		// Skip let token.
		index++;

		bool is_mutable = false;
		bool is_reference = false;

		// Look for mutable qualifier.
		if (tokens[index].source == "mut"sv)
		{
			is_mutable = true;
			index++;
		}

		// Look for mutable qualifier.
		if (tokens[index].source == "&"sv)
		{
			is_reference = true;
			index++;
		}

		std::string_view name;

		// Parsing an operator function
		if (tokens[index].source == "operator"sv)
		{
			index++;
			try_call(assign_to(name), parse_operator_tokens(tokens, index));
		}
		else
		{
			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after let.");
			name = tokens[index].source;
			index++;
		}

		if (tokens[index].source != "=") return make_syntax_error("Expected '=' after identifier in let declaration.");
		index++;

		// If the expression returns a function, bind it to its name and return a noop.
		try_call_decl(incomplete::Expression expression, parse_expression(tokens, index, type_names));

		incomplete::statement::VariableDeclaration statement;
		statement.variable_name = name;
		statement.assigned_expression = std::move(expression);
		statement.type.value = incomplete::TypeId::Deduce();
		statement.type.is_mutable = is_mutable;
		statement.type.is_reference = is_reference;
		return std::move(statement);
	}

	auto parse_return_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::Return, SyntaxError>
	{
		// Skip return token.
		index++;

		incomplete::statement::Return statement;
		try_call(assign_to(statement.returned_expression), parse_expression(tokens, index, type_names));

		return std::move(statement);
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::If, SyntaxError>
	{
		// Skip the if
		index++;

		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after if.");
		index++;

		incomplete::statement::If statement;
		try_call(assign_to(statement.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after condition in if statement.");
		index++;

		try_call(assign_to(statement.then_case), parse_statement(tokens, index, type_names));

		// For if statement else is optional.
		if (tokens[index].source == "else")
		{
			// Skip else token.
			index++;
			try_call(assign_to(statement.else_case), parse_statement(tokens, index, type_names));
		}

		return std::move(statement);
	}

	auto parse_statement_block(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::StatementBlock, SyntaxError>
	{
		// Skip opening }
		index++;

		incomplete::statement::StatementBlock statement_block;

		size_t const stack_size = type_names.size();

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			try_call(statement_block.statements.push_back, parse_statement(tokens, index, type_names));
		}
		type_names.resize(stack_size);

		// Skip closing brace.
		index++;

		return statement_block;
	}

	auto parse_while_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::While, SyntaxError>
	{
		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after while.");
		index++;

		incomplete::statement::While statement;

		// Parse condition. Must return bool.
		try_call(assign_to(statement.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after condition in while statement.");
		index++;

		// Parse body
		try_call(assign_to(statement.body), parse_statement(tokens, index, type_names));

		return std::move(statement);
	}

	auto parse_for_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::For, SyntaxError>
	{
		// Syntax of for loop
		// for (declaration-or-expression; condition-expression; end-expression)
		//	   statement 

		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after for.");
		index++;

		incomplete::statement::For for_statement;
		size_t const stack_size = type_names.size();

		// Parse init statement. Must be an expression or a declaration.
		try_call_decl(incomplete::Statement init_statement, parse_statement(tokens, index, type_names));
		if (!(has_type<incomplete::statement::VariableDeclaration>(init_statement) || 
			  has_type<incomplete::statement::ExpressionStatement>(init_statement)))
			return make_syntax_error("init-statement of a for statement must be a variable declaration or an expression.");
		for_statement.init_statement = allocate(std::move(init_statement));

		// Parse condition. Must return bool.
		try_call(assign_to(for_statement.condition), parse_expression(tokens, index, type_names));

		// Parse ; after condition.
		if (tokens[index].type != TokenType::semicolon) return make_syntax_error("Expected ';' after for statement condition.");
		index++;

		// Parse end expression.
		try_call(assign_to(for_statement.end_expression), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after for statement end expression.");
		index++;

		// Parse body
		try_call(assign_to(for_statement.body), parse_statement(tokens, index, type_names));

		type_names.resize(stack_size);

		return std::move(for_statement);
	}

	template <typename Stmt>
	auto parse_break_or_continue_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> Stmt
	{
		// Should check if parsing a loop and otherwise give an error.

		static_cast<void>(tokens, type_names);
		index++;
		return Stmt();
	}

	auto parse_struct(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Struct, SyntaxError>
	{
		incomplete::Struct declared_struct;

		// Parse name
		if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after struct.");
		if (is_keyword(tokens[index].source)) return make_syntax_error("Cannot use a keyword as struct name.");
		auto const type_name = tokens[index].source;
		index++;

		// Skip { token.
		if (tokens[index].type != TokenType::open_brace) return make_syntax_error("Expected '{' after struct name.");
		index++;

		declared_struct.name = type_name;

		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			incomplete::MemberVariable var;
			try_call_decl(auto var_type, parse_type_name(tokens, index, type_names));
			if (!var_type.has_value()) return make_syntax_error("Expected type in struct member declaration.");
			var.type = std::move(*var_type);
			if (var.type.is_reference) return make_syntax_error("Member variable cannot be reference.");
			if (var.type.is_mutable) return make_syntax_error("Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after type name in member variable declaration.");
			if (is_keyword(tokens[index].source)) return make_syntax_error("Cannot use a keyword as member variable name.");
			if (is_name_locally_taken(tokens[index].source, declared_struct.member_variables)) return make_syntax_error("More than one member variable with the same name.");
			var.name = tokens[index].source;
			index++;

			// Initialization expression.
			if (tokens[index].source == "=")
			{
				index++;
				try_call(assign_to(var.initializer_expression), parse_expression(tokens, index, type_names));
			}

			if (tokens[index].type != TokenType::semicolon) return make_syntax_error("Expected semicolon after struct member.");
			declared_struct.member_variables.push_back(std::move(var));
			index++;
		}

		// Skip } token.
		index++;

		return declared_struct;
	}

	auto parse_struct_template_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::statement::StructTemplateDeclaration, SyntaxError>
	{
		incomplete::StructTemplate struct_template;
		try_call(assign_to(struct_template.template_parameters), parse_template_parameter_list(tokens, index));

		size_t const type_name_stack_size = type_names.size();
		for (incomplete::TemplateParameter const & param : struct_template.template_parameters)
			type_names.push_back({param.name, TypeName::Type::template_parameter});

		try_call(assign_to(static_cast<incomplete::Struct &>(struct_template)), parse_struct(tokens, index, type_names));

		type_names.resize(type_name_stack_size);

		type_names.push_back({struct_template.name, TypeName::Type::struct_template});

		return incomplete::statement::StructTemplateDeclaration{struct_template};
	}

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Statement, SyntaxError>
	{
		// Skip struct token.
		index++;

		if (tokens[index].source == "<"sv)
			return parse_struct_template_declaration(tokens, index, type_names);

		try_call_decl(incomplete::Struct declared_struct, parse_struct(tokens, index, type_names));
		type_names.push_back({ declared_struct.name, TypeName::Type::type});
		return incomplete::statement::StructDeclaration{std::move(declared_struct)};
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept 
		-> expected<incomplete::statement::VariableDeclaration, SyntaxError>
	{
		incomplete::statement::VariableDeclaration statement;
		
		// The second token of the statement is the variable name.
		if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after type name.");
		if (is_keyword(tokens[index].source)) return make_syntax_error("Cannot use a keyword as variable name.");
		std::string_view const var_name = tokens[index].source;
		index++;

		statement.variable_name = var_name;
		statement.type = std::move(type);

		if (tokens[index].type == TokenType::semicolon)
			return std::move(statement);

		// The third token is a '='.
		if (tokens[index].source != "=") return make_syntax_error("Expected '=' or ';' after variable name in declaration.");
		index++;

		// The rest is the expression assigned to the variable.
		try_call(assign_to(statement.assigned_expression), parse_expression(tokens, index, type_names));
		return std::move(statement);
	}

	auto parse_expression_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::ExpressionStatement, SyntaxError>
	{
		incomplete::statement::ExpressionStatement statement;
		try_call(assign_to(statement.expression), parse_expression(tokens, index, type_names));
		return std::move(statement);
	}

	auto parse_import_block(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, std::string_view library_name) noexcept 
		-> expected<incomplete::statement::ImportBlock, SyntaxError>
	{
		if (tokens[index].type != TokenType::open_brace) return make_syntax_error("Expected { after library name.");
		index++;

		DLL const library = load_library(library_name);

		incomplete::statement::ImportBlock import_block;

		while (tokens[index].type != TokenType::close_brace)
		{
			if (tokens[index].source != "let") return make_syntax_error("Expected function declaration in import block.");
			index++;
			if (tokens[index].type != TokenType::identifier) return make_syntax_error("Expected identifier after let.");
			std::string_view const function_name = tokens[index].source;
			index++;

			if (tokens[index].source != "=") return make_syntax_error("Expected '=' after function name.");
			index++;

			if (tokens[index].source != "fn") return make_syntax_error("Expected function declaration after '=' in import block.");
			index++;

			incomplete::FunctionPrototype function_prototype;
			try_call_void(parse_function_prototype(tokens, index, type_names, out(function_prototype)));

			if (tokens[index].source != "extern_symbol"sv) return make_syntax_error("Expected keyword \"extern_symbol\" after function prototype.");
			if (!function_prototype.return_type.has_value()) return make_syntax_error("Cannot omit return type of imported extern function.");

			index++;
			if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error("Expected '(' after extern_symbol.");
			index++;
			std::string_view const extern_symbol_name = tokens[index].source;
			index++;
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error("Expected ')' after extern_symbol name.");
			index++;

			auto const module_handle = load_library(library_name);
			void const * const extern_symbol_address = find_symbol(module_handle, parse_string_literal(extern_symbol_name));
			if (extern_symbol_address == nullptr) return make_syntax_error("Extern symbol not found.");

			incomplete::ExternFunction extern_function;
			extern_function.name = function_name;
			extern_function.prototype = std::move(function_prototype);
			extern_function.function_pointer = extern_symbol_address;

			import_block.imported_functions.push_back(std::move(extern_function));
			
			if (tokens[index].type != TokenType::semicolon) return make_syntax_error("Missing ';' after function declaration.");
			index++;
		}

		// Skip '}'
		index++;

		return import_block;
	}

	[[nodiscard]] auto parse_global_scope(
		std::string_view src,
		std::vector<TypeName> & type_names,
		std::vector<std::filesystem::path> & imported_files,
		out<std::vector<incomplete::Statement>> global_initialization_statements
	) noexcept -> expected<void, SyntaxError>;

	[[nodiscard]] auto parse_import_declaration(
		span<lex::Token const> tokens, 
		size_t & index, 
		std::vector<TypeName> & type_names,
		std::vector<std::filesystem::path> & imported_files,
		out<std::vector<incomplete::Statement>> global_initialization_statements
	) noexcept -> expected<void, SyntaxError>
	{
		// Skip import keyword.
		index++;

		if (tokens[index].type != TokenType::literal_string) return make_syntax_error("Expected string literal with library name after import.");
		std::string const file_name = parse_string_literal(tokens[index].source);
		index++;

		if (tokens[index].type == TokenType::open_brace)
		{
			try_call(global_initialization_statements->push_back, parse_import_block(tokens, index, type_names, file_name));
		}
		else
		{
			if (tokens[index].type != TokenType::semicolon) return make_syntax_error("Expected semicolon after file name in import declaration.");
			index++;

			std::filesystem::path canonical_file_name = std::filesystem::canonical(file_name);

			// If the file has not been imported.
			if (std::find(imported_files, canonical_file_name) == imported_files.end())
			{
				std::string const source = load_whole_file(canonical_file_name);
				imported_files.push_back(std::move(canonical_file_name));
				try_call_void(parse_global_scope(source, type_names, imported_files, global_initialization_statements));
			}
		}

		return success;
	}

	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Statement, SyntaxError>
	{
		incomplete::Statement result;

		if (tokens[index].source == "let")
			try_call(assign_to(result), parse_let_statement(tokens, index, type_names))
		else if (tokens[index].source == "return")
			try_call(assign_to(result), parse_return_statement(tokens, index, type_names))
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
			try_call_decl(auto parsed_type, parse_type_name(tokens, index, type_names));
			if (parsed_type.has_value())
				try_call(assign_to(result), parse_variable_declaration_statement(tokens, index, type_names, std::move(*parsed_type)))
			else
				try_call(assign_to(result), parse_expression_statement(tokens, index, type_names));
		}

		// A statement must end with a semicolon.
		if (tokens[index].type != TokenType::semicolon) return make_syntax_error("Expected ';' after statement.");
		index++;

		return std::move(result);
	}

	[[nodiscard]] auto parse_global_scope(
		std::string_view src,
		std::vector<TypeName> & type_names,
		std::vector<std::filesystem::path> & imported_files,
		out<std::vector<incomplete::Statement>> global_initialization_statements
	) noexcept -> expected<void, SyntaxError>
	{
		try_call_decl(auto const tokens, lex::tokenize(src));

		size_t index = 0;
		while (index < tokens.size())
		{
			if (tokens[index].source == "import")
			{
				try_call_void(parse_import_declaration(tokens, index, type_names, imported_files, global_initialization_statements));
			}
			else
			{
				try_call_decl(incomplete::Statement statement, parse_statement(tokens, index, type_names));
				if (has_type<incomplete::statement::ExpressionStatement>(statement)) return make_syntax_error("An expression statement is not allowed at the global scope.");
				global_initialization_statements->push_back(std::move(statement));
			}
		}

		return success;
	}

	auto parse_source(std::string_view src, complete::Program const & program) noexcept -> expected<std::vector<incomplete::Statement>, SyntaxError>
	{
		std::vector<incomplete::Statement> global_initialization_statements;

		std::vector<TypeName> type_names;
		type_names.reserve(16 + program.global_scope.types.size() + program.global_scope.struct_templates.size());

		for (complete::TypeName const & name : program.global_scope.types)
			type_names.push_back({name.name, TypeName::Type::type});

		for (complete::StructTemplateName const & name : program.global_scope.struct_templates)
			type_names.push_back({name.name, TypeName::Type::struct_template});

		std::vector<std::filesystem::path> imported_files;

		try_call_void(parse_global_scope(src, type_names, imported_files, out(global_initialization_statements)));

		return std::move(global_initialization_statements);
	}

}
