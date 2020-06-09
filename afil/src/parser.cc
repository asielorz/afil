#include "parser.hh"
#include "incomplete_statement.hh"
#include "incomplete_module.hh"
#include "syntax_error.hh"
#include "lexer.hh"
#include "utils/algorithm.hh"
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
		"null"

		// Declarations
		"fn",
		"main",
		"struct",
		"let",
		"uninit",
		"mut",
		"namespace",
		"conversion",
		"implicit",
		"constructor",
		"destructor",

		// Operators
		"and",
		"or",
		"not",
		"xor",
	};

	constexpr std::string_view built_in_type_name_keywords[] = {
		"void",
		"int8",
		"int16",
		"int32",
		"int64",
		"uint8",
		"uint16",
		"uint32",
		"uint64",
		"float32",
		"float64",
		"bool",
		"char",
		"type",
		"null_t"
	};

	auto built_in_type_names() noexcept -> std::vector<TypeName>
	{
		std::vector<TypeName> type_names;
		type_names.reserve(std::size(built_in_type_name_keywords));
		for (std::string_view const name : built_in_type_name_keywords)
			type_names.push_back({name, TypeName::Type::type});
		return type_names;
	}

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
		return std::find(keywords, std::end(keywords), name) != std::end(keywords) || 
			std::find(built_in_type_name_keywords, std::end(built_in_type_name_keywords), name) != std::end(built_in_type_name_keywords);
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

	auto parse_operator_tokens(span<lex::Token const> tokens, size_t & index) noexcept -> expected<std::string_view, PartialSyntaxError>
	{
		if (tokens[index].type == TokenType::open_bracket)
		{
			index++;
			if (tokens[index].type != TokenType::close_bracket) 
				return make_syntax_error(tokens[index], "Expected operator after '(' in function declaration.");
			index++;
			return "[]"sv;
		}
		else
		{
			if (tokens[index].type != TokenType::operator_) 
				return make_syntax_error(tokens[index], "Expected operator name after keyword \"operator\".");
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

	auto parse_char_literal(std::string_view token_source) noexcept -> char_t
	{
		assert(token_source.length() == 3);
		return token_source[1];
	}

	auto insert_expression(incomplete::Expression & tree, incomplete::Expression & new_node) noexcept -> void
	{
		using incomplete::expression::BinaryOperatorCall;
		BinaryOperatorCall & tree_op = std::get<BinaryOperatorCall>(tree.variant);
		BinaryOperatorCall & new_op = std::get<BinaryOperatorCall>(new_node.variant);

		if (precedence(new_op.op) <= precedence(tree_op.op))
		{
			new_op.left = allocate(std::move(tree));
			tree = std::move(new_node);
		}
		else if (!has_type<BinaryOperatorCall>(tree_op.right->variant))
		{
			new_op.left = std::move(tree_op.right);
			tree_op.right = allocate(std::move(new_node));
		}
		else
			insert_expression(*tree_op.right, new_node);
	}

	auto resolve_operator_precedence(span<incomplete::Expression> operands, span<std::string_view const> operators) noexcept -> incomplete::Expression
	{
		using incomplete::expression::BinaryOperatorCall;

		if (operators.empty())
		{
			assert(operands.size() == 1);
			return std::move(operands[0]);
		}

		incomplete::Expression root = incomplete::Expression(BinaryOperatorCall{parse_operator(operators[0]),
				allocate(std::move(operands[0])),
				allocate(std::move(operands[1]))
		}, operators[0]);

		for (size_t i = 1; i < operators.size(); ++i)
		{
			auto new_node = incomplete::Expression(BinaryOperatorCall{parse_operator(operators[i]), nullptr, allocate(std::move(operands[i + 1]))}, operators[i]);
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

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<std::optional<incomplete::TypeId>, PartialSyntaxError>;
	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, PartialSyntaxError>;
	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, PartialSyntaxError>;
	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Statement, PartialSyntaxError>;

	auto parse_namespaces(span<lex::Token const> tokens, size_t & index) noexcept -> expected<std::vector<std::string_view>, PartialSyntaxError>
	{
		std::vector<std::string_view> namespaces;

		while (tokens[index + 1].type == TokenType::scope_resolution)
		{
			namespaces.push_back(tokens[index].source);
			index += 2;

			if (tokens[index].type != TokenType::identifier)
				return make_syntax_error(tokens[index].source, "Expected identifier after \"::\".");
		}

		return std::move(namespaces);
	}

	auto parse_mutable_pointer_and_array(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept 
		-> expected<incomplete::TypeId, PartialSyntaxError>
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
				if (tokens[index].type != TokenType::close_bracket) return make_syntax_error(tokens[index], "Expected ] after array size.");
				index++;
				incomplete::TypeId array_type = array_type_for(std::move(type), std::move(size));
				return parse_mutable_pointer_and_array(tokens, index, type_names, std::move(array_type));
			}
		}

		return std::move(type);
	}

	auto parse_mutable_pointer_array_and_reference(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, incomplete::TypeId type) noexcept 
		-> expected<incomplete::TypeId, PartialSyntaxError>
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
		-> expected<std::vector<incomplete::TypeId>, PartialSyntaxError>
	{
		if (tokens[index].source != "<") return make_syntax_error(tokens[index], "Expected '<' after template name.");
		index++;

		std::vector<incomplete::TypeId> template_parameters;

		while (true)
		{
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (!type.has_value()) return make_syntax_error(tokens[index], "Expected type in template instantiation parameter list.");
			template_parameters.push_back(std::move(*type));

			if (tokens[index].source == ">")
				break;

			if(!(tokens[index].type == TokenType::comma)) return make_syntax_error(tokens[index], "Expected comma ',' after template parameter.");
			index++;
		}

		if (tokens[index].source != ">") return make_syntax_error(tokens[index], "Expected comma '>' at the end of template parameter list.");
		index++;

		return template_parameters;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<std::optional<incomplete::TypeId>, PartialSyntaxError>
	{
		using namespace incomplete;
		
		size_t const index_start = index;
		try_call_decl(std::vector<std::string_view> namespaces, parse_namespaces(tokens, index));
		
		std::string_view const name_to_look_up = tokens[index].source;
		auto const it = std::find_if(type_names.rbegin(), type_names.rend(), [name_to_look_up](TypeName const & type_name) { return type_name.name == name_to_look_up; });
		if (it == type_names.rend())
		{
			index = index_start;
			return std::nullopt;
		}

		index++;

		int const found_index = static_cast<int>(&*it - type_names.data());

		switch (it->type)
		{
			case TypeName::Type::type:
			case TypeName::Type::template_parameter:
			{
				TypeId::BaseCase base_case;
				base_case.name = name_to_look_up;
				base_case.namespaces = std::move(namespaces);
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
				template_instantiation.namespaces = std::move(namespaces);
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

	auto parse_template_parameter_list(span<lex::Token const> tokens, size_t & index) noexcept -> expected<std::vector<incomplete::TemplateParameter>, PartialSyntaxError>
	{
		// Skip < token.
		index++;

		std::vector<incomplete::TemplateParameter> parsed_parameters;

		for (;;)
		{
			if (tokens[index].type != TokenType::identifier) 
				return make_syntax_error(tokens[index], "Expected identifier.");

			incomplete::TemplateParameter param;
			param.name = tokens[index].source;
			index++;

			if (tokens[index].type == TokenType::identifier)
			{
				param.concept = param.name;
				param.name = tokens[index].source;
				index++;
			}

			if (is_keyword(param.name))
				return make_syntax_error(param.name, "Cannot use a keyword as template parameter name.");
			if (!param.concept.empty() && is_keyword(param.concept))
				return make_syntax_error(param.concept, "Cannot use a keyword as concept name.");

			parsed_parameters.push_back(std::move(param));

			if (tokens[index].source == ">")
			{
				index++;
				break;
			}
			if (tokens[index].type != TokenType::comma) return make_syntax_error(tokens[index], "Expected '>' or ',' after template parameter.");
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
		TokenType opener = TokenType::open_parenthesis, TokenType delimiter = TokenType::close_parenthesis) noexcept -> expected<std::vector<incomplete::Expression>, PartialSyntaxError>
	{
		std::vector<incomplete::Expression> parsed_expressions;

		if (tokens[index].type != opener) return make_syntax_error(tokens[index], "Expected '('.");
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
				return make_syntax_error(tokens[index], "Expected ')' or ',' after expression.");
		}

		return parsed_expressions;
	}

	auto parse_designated_initializer_list(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) 
		-> expected<std::vector<incomplete::DesignatedInitializer>, PartialSyntaxError>
	{
		// Parameter list starts with (
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after type name in struct constructor.");
		index++;

		std::vector<incomplete::DesignatedInitializer> initializers;

		for (;;)
		{
			// A member initializer by name starts with a period.
			if (tokens[index].type != TokenType::period) return make_syntax_error(tokens[index], "Expected '.' in designated initializer.");
			index++;

			incomplete::DesignatedInitializer parameter;

			// Find the member to initialize.
			if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected member name after '.' in designated initializer.");
			parameter.member_name = tokens[index].source;
			index++;

			// Next token must be =
			if (tokens[index].source != "=") return make_syntax_error(tokens[index], "Expected '=' after member name in designated initializer.");
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
				return make_syntax_error(tokens[index], "Expected ')' or ',' after designated initializer.");
		}

		return initializers;
	}

	[[nodiscard]] auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::FunctionPrototype> function) noexcept 
		-> expected<void, PartialSyntaxError>
	{
		// Parameters must be between parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after fn.");
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			incomplete::FunctionParameter var;
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (!type.has_value()) return make_syntax_error(tokens[index], "Parameter type not found.");
			var.type = std::move(*type);

			if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected identifier after function parameter type.");
			if (is_keyword(tokens[index].source)) return make_syntax_error(tokens[index], "Cannot use a keyword as function parameter name.");
			var.name = tokens[index].source;
			function->parameters.push_back(std::move(var));
			index++;

			if (tokens[index].type == TokenType::close_parenthesis)
				break;

			if (tokens[index].type != TokenType::comma) return make_syntax_error(tokens[index], "Expected ',' or ')' after function parameter.");
			index++;
		}

		// After parameters close parenthesis.
		if (tokens[index].type != TokenType::close_parenthesis)  return make_syntax_error(tokens[index], "Expected ')' after function parameter list.");
		index++;

		// Return type is introduced with an arrow (optional).
		if (tokens[index].type == TokenType::arrow)
		{
			index++;
			try_call(assign_to(function->return_type), parse_type_name(tokens, index, type_names));
		}

		return success;
	}

	[[nodiscard]] auto parse_function_contract(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::Function> function) noexcept -> expected<void, PartialSyntaxError>
	{
		if (tokens[index].source == "assert")
		{
			index++;

			if (tokens[index].type != TokenType::open_brace)
				return make_syntax_error(tokens[index].source, "Expected '{' after \"assert\" keyword.");
			index++;

			while (tokens[index].type != TokenType::close_brace)
			{
				try_call(function->preconditions.push_back, parse_expression(tokens, index, type_names));
				
				if (tokens[index].type != TokenType::semicolon)
					return make_syntax_error(tokens[index].source, "Expected ';' after expression in assert block.");
				index++;
			}

			index++;
		}

		return success;
	}

	[[nodiscard]] auto parse_function_body(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names, out<incomplete::Function> function) noexcept -> expected<void, PartialSyntaxError>
	{
		// Body of the function is enclosed by braces.
		if (tokens[index].type != TokenType::open_brace) return make_syntax_error(tokens[index], "Expected '{' at start of function body.");
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

	auto parse_function_template_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::expression::Variant, PartialSyntaxError>
	{
		incomplete::FunctionTemplate function;
		try_call(assign_to(function.template_parameters), parse_template_parameter_list(tokens, index));

		size_t const type_name_stack_size = type_names.size();
		for (incomplete::TemplateParameter const & param : function.template_parameters)
			type_names.push_back({param.name, TypeName::Type::template_parameter});
		
		try_call_void(parse_function_prototype(tokens, index, type_names, out(function)));
		try_call_void(parse_function_contract(tokens, index, type_names, out(function)));
		try_call_void(parse_function_body(tokens, index, type_names, out(function)));

		type_names.resize(type_name_stack_size);

		return incomplete::expression::FunctionTemplate{std::move(function)};
	}

	auto parse_extern_function_expression(span<lex::Token const> tokens, size_t & index, incomplete::FunctionPrototype function_prototype) noexcept
		-> expected<incomplete::expression::Variant, PartialSyntaxError>
	{
		if (!function_prototype.return_type.has_value()) 
			return make_syntax_error(tokens[index], "Cannot omit return type of imported extern function.");

		index++;
		if (tokens[index].type != TokenType::open_parenthesis)
			return make_syntax_error(tokens[index], "Expected '(' after extern_symbol.");
		index++;
		std::string_view const extern_symbol_name = tokens[index].source;
		index++;
		if (tokens[index].type != TokenType::close_parenthesis)
			return make_syntax_error(tokens[index], "Expected ')' after extern_symbol name.");
		index++;

		incomplete::ExternFunction extern_function;
		extern_function.ABI_name = parse_string_literal(extern_symbol_name);
		extern_function.ABI_name_source = extern_symbol_name;
		extern_function.prototype = std::move(function_prototype);

		return incomplete::expression::ExternFunction{std::move(extern_function)};
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::expression::Variant, PartialSyntaxError>
	{
		// Skip fn token.
		index++;

		if (tokens[index].source == "<"sv)
		{
			return parse_function_template_expression(tokens, index, type_names);
		}

		incomplete::Function function;
		try_call_void(parse_function_prototype(tokens, index, type_names, out(function)));

		if (tokens[index].source == "extern_symbol")
			return parse_extern_function_expression(tokens, index, std::move(function));

		try_call_void(parse_function_contract(tokens, index, type_names, out(function)));
		try_call_void(parse_function_body(tokens, index, type_names, out(function)));

		return incomplete::expression::Function{std::move(function)};
	}

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::expression::If, PartialSyntaxError>
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after if.");
		index++;

		incomplete::expression::If if_node;
		try_call(assign_to(if_node.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after if condition.");
		index++;

		try_call(assign_to(if_node.then_case), parse_expression(tokens, index, type_names));

		// Expect keyword else to separate then and else cases.
		if (tokens[index].source != "else") return make_syntax_error(tokens[index], "Expected keyword \"else\" after if expression body.");
		index++;

		try_call(assign_to(if_node.else_case), parse_expression(tokens, index, type_names));

		return std::move(if_node);
	}
	
	auto all_branches_return(incomplete::statement::Variant const & statement) noexcept -> bool
	{
		auto const visitor = overload(
			[](auto const &) { return false; }, // Default case, does not return
			[](incomplete::statement::Return const &) { return true; },
			[&](incomplete::statement::If const & if_node)
			{
				return all_branches_return(if_node.then_case->variant)
					&& all_branches_return(if_node.else_case->variant);
			},
			[&](incomplete::statement::StatementBlock  const & block_node)
			{
				return all_branches_return(block_node.statements.back().variant);
			}
		);
		return std::visit(visitor, statement);
	}

	auto parse_statement_block_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::expression::StatementBlock, PartialSyntaxError>
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
		if (!all_branches_return(node.statements.back().variant)) return make_syntax_error(tokens[index], "Not all branches of statement block expression return.");

		// Skip closing brace.
		if (tokens[index].type != TokenType::close_brace) return make_syntax_error(tokens[index], "Expected '}' at the end of statement block expression.");
		index++;

		return node;
	}

	auto parse_compiles_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept
		-> expected<incomplete::expression::Compiles, PartialSyntaxError>
	{
		// Skip compiles token.
		index++;

		incomplete::expression::Compiles compiles_expr;

		// Variable list.
		if (tokens[index].type == TokenType::open_parenthesis)
		{
			// Skip open parenthesis.
			index++;

			while (true)
			{
				try_call_decl(auto type_expr, parse_expression(tokens, index, type_names));

				if (tokens[index].type != TokenType::identifier || is_keyword(tokens[index].source))
					return make_syntax_error(tokens[index].source, "Expected name after type in parameter list of compiles expression.");

				compiles_expr.variables.push_back({std::move(type_expr), tokens[index].source});
				index++;

				if (tokens[index].type == TokenType::comma)
				{
					index++;
				}
				else if (tokens[index].type == TokenType::close_parenthesis)
				{
					index++;
					break;
				}
				else
				{
					return make_syntax_error(tokens[index].source, "Expected ')' or ',' after variable name in parameter list of compiles expression.");
				}
			}
		}

		// Expect { after compiles.
		if (tokens[index].type != TokenType::open_brace)
			return make_syntax_error(tokens[index].source, "Expected '{' after \"compiles\" keyword.");
		index++;

		while (true)
		{
			incomplete::ExpressionToTest expr_to_test;

			if (tokens[index].type == TokenType::open_brace)
			{
				index++;
				try_call(assign_to(expr_to_test.expression), parse_expression(tokens, index, type_names));

				if (tokens[index].type != TokenType::close_brace)
					return make_syntax_error(tokens[index].source, "Expected '}' after expression in body of compiles expression.");
				index++;

				if (tokens[index].type != TokenType::arrow)
					return make_syntax_error(tokens[index].source, "Expected \"->\" after '}' in body of compiles expression.");
				index++;

				try_call(assign_to(expr_to_test.expected_type), parse_type_name(tokens, index, type_names));
			}
			else
			{
				try_call(assign_to(expr_to_test.expression), parse_expression(tokens, index, type_names));
			}
			compiles_expr.body.push_back(std::move(expr_to_test));

			if (tokens[index].type == TokenType::semicolon)
			{
				index++;
			}
			else if (tokens[index].type == TokenType::close_brace)
			{
				index++;
				break;
			}
			else
			{
				return make_syntax_error(tokens[index].source, "Expected '}' or ';' after expression in body of compiles expression.");
			}
		}

		return std::move(compiles_expr);
	}

	auto parse_unary_operator(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::expression::Variant, PartialSyntaxError>
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

	auto parse_expression_variant(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::expression::Variant, PartialSyntaxError>
	{
		if (tokens[index].source == "fn")
			return parse_function_expression(tokens, index, type_names);
		else if (tokens[index].source == "if")
			return parse_if_expression(tokens, index, type_names);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block_expression(tokens, index, type_names);
		else if (tokens[index].type == TokenType::literal_int)
			return incomplete::expression::Literal<int>(parse_number_literal<int>(tokens[index++].source));
		else if (tokens[index].type == TokenType::literal_float)
			return incomplete::expression::Literal<float>(parse_number_literal<float>(tokens[index++].source));
		else if (tokens[index].type == TokenType::literal_bool)
			return incomplete::expression::Literal<bool>(tokens[index++].source[0] == 't');
		else if (tokens[index].type == TokenType::literal_string)
			return incomplete::expression::Literal<std::string>(parse_string_literal(tokens[index++].source));
		else if (tokens[index].type == TokenType::literal_char)
			return incomplete::expression::Literal<char_t>(parse_char_literal(tokens[index++].source));
		else if (tokens[index].source == "null")
		{
			index++;
			return incomplete::expression::Literal<null_t>();
		}
		else if (tokens[index].source == "operator")
		{
			index++;
			try_call_decl(std::string_view const name, parse_operator_tokens(tokens, index));
			incomplete::expression::Identifier id_node;
			id_node.name = name;
			return std::move(id_node);
		}
		else if (tokens[index].source == "compiles")
			return parse_compiles_expression(tokens, index, type_names);
		else if (tokens[index].source == "data")
		{
			index++;
			incomplete::expression::DataCall data_node;
			if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after data.");
			index++;
			try_call(assign_to(data_node.operand), parse_expression(tokens, index, type_names));
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after operand for data.");
			index++;
			return std::move(data_node);
		}
		else if (tokens[index].source == "size")
		{
			index++;
			incomplete::expression::SizeCall size_node;
			if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after size.");
			index++;
			try_call(assign_to(size_node.operand), parse_expression(tokens, index, type_names));
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after operand for size.");
			index++;
			return std::move(size_node);
		}
		else if (tokens[index].type == TokenType::identifier)
		{
			// It can be either a constructor call or the naming of a variable/function
			try_call_decl(auto type, parse_type_name(tokens, index, type_names));
			if (type.has_value())
			{
				//if (tokens[index].type == TokenType::open_parenthesis && tokens[index + 1].type == TokenType::period)
				//{
				//	incomplete::expression::DesignatedInitializerConstructor ctor_node;
				//	ctor_node.constructed_type = std::move(*type);
				//	try_call(assign_to(ctor_node.parameters), parse_designated_initializer_list(tokens, index, type_names));
				//	return std::move(ctor_node);
				//}
				//else
				//{
					return incomplete::expression::Literal<incomplete::TypeId>(std::move(*type));
				//}
			}
			else
			{
				incomplete::expression::Identifier id_node;
				try_call(assign_to(id_node.namespaces), parse_namespaces(tokens, index));

				id_node.name = tokens[index].source;
				index++;
				return id_node;
			}
		}
		else if (is_unary_operator(tokens[index]))
			return parse_unary_operator(tokens, index, type_names);
		else
			return make_syntax_error(tokens[index], "Unrecognized token. Expected expression.");
	}

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, PartialSyntaxError>
	{
		if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			auto expr = parse_expression(tokens, index, type_names);

			// Next token must be close parenthesis.
			if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after parenthesized expression.");
			index++;

			return expr;
		}
		else
		{
			auto const expr_source_start = begin_ptr(tokens[index].source);
			try_call_decl(incomplete::expression::Variant var, parse_expression_variant(tokens, index, type_names));
			auto const expr_source_end = end_ptr(tokens[index - 1].source);
			return incomplete::Expression(std::move(var), make_string_view(expr_source_start, expr_source_end));
		}
	}

	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, PartialSyntaxError>
	{
		try_call_decl(incomplete::Expression tree, parse_single_expression(tokens, index, type_names));

		while (index < tokens.size())
		{
			// Loop to possibly parse chains of member accesses.
			if (tokens[index].type == TokenType::period)
			{
				index++;

				if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected member name after '.'.");
				std::string_view const member_name = tokens[index].source;
				index++;

				auto const expr_source_start = begin_ptr(tree.source);
				auto const expr_source_end = end_ptr(member_name);

				incomplete::expression::MemberVariable var_node;
				var_node.name = member_name;
				var_node.owner = allocate(std::move(tree));
				tree = incomplete::Expression(std::move(var_node), make_string_view(expr_source_start, expr_source_end));
			}
			// Subscript
			else if (tokens[index].type == TokenType::open_bracket)
			{
				try_call_decl(std::vector<incomplete::Expression> params, parse_comma_separated_expression_list(tokens, index, type_names, TokenType::open_bracket, TokenType::close_bracket));

				auto const expr_source_start = begin_ptr(tree.source);
				auto const expr_source_end = end_ptr(tokens[index - 1].source);

				if (params.size() == 1)
				{
					incomplete::expression::Subscript node;
					node.array = allocate(std::move(tree));
					node.index = allocate(std::move(params[0]));
					tree = incomplete::Expression(std::move(node), make_string_view(expr_source_start, expr_source_end));
				}
				else
				{
					incomplete::expression::FunctionCall node;
					node.parameters.reserve(params.size() + 2);
					node.parameters.push_back(incomplete::Expression(incomplete::expression::Identifier("[]"), make_string_view(expr_source_start, expr_source_end)));
					node.parameters.push_back(std::move(tree));
					for (auto & param : params)
						node.parameters.push_back(std::move(param));
					tree = incomplete::Expression(std::move(node), make_string_view(expr_source_start, expr_source_end));
				}
			}
			// Function call
			else if (tokens[index].type == TokenType::open_parenthesis)
			{
				if (tokens[index + 1].type == TokenType::period)
				{
					incomplete::expression::DesignatedInitializerConstructor ctor_node;
					ctor_node.constructed_type = allocate(std::move(tree));
					try_call(assign_to(ctor_node.parameters), parse_designated_initializer_list(tokens, index, type_names));

					auto const expr_source_start = begin_ptr(tree.source);
					auto const expr_source_end = end_ptr(tokens[index - 1].source);

					tree = incomplete::Expression(std::move(ctor_node), make_string_view(expr_source_start, expr_source_end));
				}
				else
				{
					try_call_decl(std::vector<incomplete::Expression> params, parse_comma_separated_expression_list(tokens, index, type_names));

					auto const expr_source_start = begin_ptr(tree.source);
					auto const expr_source_end = end_ptr(tokens[index - 1].source);

					incomplete::expression::FunctionCall node;
					node.parameters.reserve(params.size() + 1);
					node.parameters.push_back(std::move(tree));
					for (auto & param : params)
						node.parameters.push_back(std::move(param));
					tree = incomplete::Expression(std::move(node), make_string_view(expr_source_start, expr_source_end));
				}
			}
			else break;
		}

		return std::move(tree);
	}

	auto parse_expression(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Expression, PartialSyntaxError>
	{
		std::vector<incomplete::Expression> operands;
		std::vector<std::string_view> operators;

		while (index < tokens.size())
		{
			try_call(operands.push_back, parse_expression_and_trailing_subexpressions(tokens, index, type_names));

			// If the next token is an operator, parse the operator and repeat. Otherwise end the loop and return the expression.
			if (index < tokens.size() && tokens[index].type == TokenType::operator_)
			{
				operators.push_back(tokens[index].source);
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

	auto parse_let_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::LetDeclaration, PartialSyntaxError>
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
			if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected identifier after let.");
			name = tokens[index].source;
			index++;
		}

		if (tokens[index].source != "=") return make_syntax_error(tokens[index], "Expected '=' after identifier in let declaration.");
		index++;

		// If the expression returns a function, bind it to its name and return a noop.
		try_call_decl(incomplete::Expression expression, parse_expression(tokens, index, type_names));

		incomplete::statement::LetDeclaration statement;
		statement.variable_name = name;
		statement.assigned_expression = std::move(expression);
		statement.is_mutable = is_mutable;
		statement.is_reference = is_reference;
		return std::move(statement);
	}

	auto parse_uninit_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::UninitDeclaration, PartialSyntaxError>
	{
		// Skip uninit token.
		index++;

		std::string_view const type_source = tokens[index].source;
		try_call_decl(auto type, parse_type_name(tokens, index, type_names));
		if (!type)
			return make_syntax_error(type_source, "Expected type after uninit keyword.");

		if (type->is_reference)
			return make_syntax_error(type_source, "Type in uninit declaration cannot be a reference.");

		if (type->is_mutable)
			return make_syntax_error(type_source, "uninit implies mutable. Redundant mut keyword.");


		if (tokens[index].type != TokenType::identifier)
			return make_syntax_error(tokens[index].source, "Expected identifier after type name.");

		std::string_view const variable_name = tokens[index].source;
		index++;

		if (is_keyword(variable_name))
			return make_syntax_error(variable_name, "Cannot use a keyword as variable name.");

		incomplete::statement::UninitDeclaration uninit_statement;
		uninit_statement.variable_name = variable_name;
		uninit_statement.variable_type = std::move(*type);

		return std::move(uninit_statement);
	}

	auto parse_return_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::Return, PartialSyntaxError>
	{
		// Skip return token.
		index++;

		incomplete::statement::Return statement;
		try_call(assign_to(statement.returned_expression), parse_expression(tokens, index, type_names));

		return std::move(statement);
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::If, PartialSyntaxError>
	{
		// Skip the if
		index++;

		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after if.");
		index++;

		incomplete::statement::If statement;
		try_call(assign_to(statement.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after condition in if statement.");
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

	auto parse_statement_block(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::StatementBlock, PartialSyntaxError>
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

	auto parse_while_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::While, PartialSyntaxError>
	{
		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after while.");
		index++;

		incomplete::statement::While statement;

		// Parse condition. Must return bool.
		try_call(assign_to(statement.condition), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after condition in while statement.");
		index++;

		// Parse body
		try_call(assign_to(statement.body), parse_statement(tokens, index, type_names));

		return std::move(statement);
	}

	auto parse_for_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::For, PartialSyntaxError>
	{
		// Syntax of for loop
		// for (declaration-or-expression; condition-expression; end-expression)
		//	   statement 

		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		if (tokens[index].type != TokenType::open_parenthesis) return make_syntax_error(tokens[index], "Expected '(' after for.");
		index++;

		incomplete::statement::For for_statement;
		size_t const stack_size = type_names.size();

		// Parse init statement. Must be an expression or a declaration.
		try_call_decl(incomplete::Statement init_statement, parse_statement(tokens, index, type_names));
		if (!(has_type<incomplete::statement::LetDeclaration>(init_statement.variant) || 
			  has_type<incomplete::statement::ExpressionStatement>(init_statement.variant)))
			return make_syntax_error(tokens[index], "init-statement of a for statement must be a variable declaration or an expression.");
		for_statement.init_statement = allocate(std::move(init_statement));

		// Parse condition. Must return bool.
		try_call(assign_to(for_statement.condition), parse_expression(tokens, index, type_names));

		// Parse ; after condition.
		if (tokens[index].type != TokenType::semicolon) return make_syntax_error(tokens[index], "Expected ';' after for statement condition.");
		index++;

		// Parse end expression.
		try_call(assign_to(for_statement.end_expression), parse_expression(tokens, index, type_names));

		if (tokens[index].type != TokenType::close_parenthesis) return make_syntax_error(tokens[index], "Expected ')' after for statement end expression.");
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

	auto parse_struct(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Struct, PartialSyntaxError>
	{
		incomplete::Struct declared_struct;

		// Parse name
		if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected identifier after struct.");
		if (is_keyword(tokens[index].source)) return make_syntax_error(tokens[index], "Cannot use a keyword as struct name.");
		auto const type_name = tokens[index].source;
		index++;

		// Skip { token.
		if (tokens[index].type != TokenType::open_brace) return make_syntax_error(tokens[index], "Expected '{' after struct name.");
		index++;

		declared_struct.name = type_name;

		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			if (tokens[index].source == "constructor")
			{
				index++;
				incomplete::Constructor constructor;
				
				if (tokens[index].type != TokenType::identifier)
					return make_syntax_error(tokens[index].source, "Expected identifier after keyword \"constructor\".");
				if (is_keyword(tokens[index].source))
					return make_syntax_error(tokens[index].source, "Cannot use a keyword as constructor name.");

				constructor.name = tokens[index].source;
				index++;

				try_call_void(parse_function_prototype(tokens, index, type_names, out(constructor)));
				try_call_void(parse_function_body(tokens, index, type_names, out(constructor)));
				declared_struct.constructors.push_back(std::move(constructor));
			}
			else if (tokens[index].source == "destructor")
			{
				if (declared_struct.destructor.has_value())
					return make_syntax_error(tokens[index].source, "Cannot declare more than one destructor for a struct.");

				index++;
				incomplete::Function destructor;
				std::string_view const first_param_source = tokens[index + 1].source;
				try_call_void(parse_function_prototype(tokens, index, type_names, out(destructor)));
				if (destructor.parameters.size() != 1)
					return make_syntax_error(first_param_source, "Destructor must have exactly one parameter");
				if (!destructor.parameters[0].type.is_mutable)
					return make_syntax_error(first_param_source, "Type of destructor must be mutable");
				if (!destructor.parameters[0].type.is_reference)
					return make_syntax_error(first_param_source, "Type of destructor must be a reference");
				try_call_void(parse_function_body(tokens, index, type_names, out(destructor)));

				declared_struct.destructor = std::move(destructor);
			}
			else
			{
				incomplete::MemberVariable var;
				try_call_decl(auto var_type, parse_type_name(tokens, index, type_names));
				if (!var_type.has_value()) return make_syntax_error(tokens[index], "Expected type in struct member declaration.");
				var.type = std::move(*var_type);
				if (var.type.is_reference) return make_syntax_error(tokens[index], "Member variable cannot be reference.");
				if (var.type.is_mutable) return make_syntax_error(tokens[index], "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

				if (tokens[index].type != TokenType::identifier) return make_syntax_error(tokens[index], "Expected identifier after type name in member variable declaration.");
				if (is_keyword(tokens[index].source)) return make_syntax_error(tokens[index], "Cannot use a keyword as member variable name.");
				if (is_name_locally_taken(tokens[index].source, declared_struct.member_variables)) return make_syntax_error(tokens[index], "More than one member variable with the same name.");
				var.name = tokens[index].source;
				index++;

				// Initialization expression.
				if (tokens[index].source == "=")
				{
					index++;
					try_call(assign_to(var.initializer_expression), parse_expression(tokens, index, type_names));
				}

				if (tokens[index].type != TokenType::semicolon) return make_syntax_error(tokens[index], "Expected semicolon after struct member.");
				declared_struct.member_variables.push_back(std::move(var));
				index++;
			}
		}

		// Skip } token.
		index++;

		return declared_struct;
	}

	auto parse_struct_template_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::statement::StructTemplateDeclaration, PartialSyntaxError>
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

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::statement::Variant, PartialSyntaxError>
	{
		// Skip struct token.
		index++;

		if (tokens[index].source == "<"sv)
			return parse_struct_template_declaration(tokens, index, type_names);

		try_call_decl(incomplete::Struct declared_struct, parse_struct(tokens, index, type_names));
		type_names.push_back({declared_struct.name, TypeName::Type::type});
		return incomplete::statement::StructDeclaration{std::move(declared_struct)};
	}

	auto parse_type_alias(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept
		-> expected<incomplete::statement::TypeAliasDeclaration, PartialSyntaxError>
	{
		// Skip type token.
		index++;

		if (tokens[index].type != TokenType::identifier)
			return make_syntax_error(tokens[index].source, "Expected identifier after \"type\" keyword.");

		std::string_view const alias_name = tokens[index].source;
		if (is_keyword(alias_name))
			return make_syntax_error(tokens[index].source, "Canot use a keyword as type alias name.");
		index++;
		
		if (tokens[index].source != "=")
			return make_syntax_error(tokens[index].source, "Expected '=' after type alias name.");
		index++;

		try_call_decl(auto type, parse_expression(tokens, index, type_names));

		type_names.push_back({alias_name, TypeName::Type::type});

		incomplete::statement::TypeAliasDeclaration type_alias_statement;
		type_alias_statement.name = alias_name;
		type_alias_statement.type = std::move(type);
		return std::move(type_alias_statement);
	}

	auto parse_namespace_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept
		-> expected<incomplete::statement::NamespaceDeclaration, PartialSyntaxError>
	{
		// Skip namespace token
		index++;

		incomplete::statement::NamespaceDeclaration namespace_declaration;

		if (tokens[index].type != TokenType::identifier)
			return make_syntax_error(tokens[index].source, "Expected identifier after keyword \"namespace\".");
		namespace_declaration.names.push_back(tokens[index].source);
		index++;

		while (tokens[index].type == TokenType::scope_resolution)
		{
			index++;
			if (tokens[index].type != TokenType::identifier)
				return make_syntax_error(tokens[index].source, "Expected identifier after \"::\".");
			namespace_declaration.names.push_back(tokens[index].source);
			index++;
		}

		for (std::string_view const namespace_name : namespace_declaration.names)
			if (is_keyword(namespace_name))
				return make_syntax_error(namespace_name, "Cannot use a keyword as namespace name.");

		if (tokens[index].type != TokenType::open_brace)
			return make_syntax_error(tokens[index].source, "Expected '{' after namespace name.");
		index++;

		while (tokens[index].type != TokenType::close_brace)
			try_call(namespace_declaration.statements.push_back, parse_statement(tokens, index, type_names));
		index++;

		return std::move(namespace_declaration);
	}

	auto parse_conversion_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept
		-> expected<incomplete::statement::ConversionDeclaration, PartialSyntaxError>
	{
		// Skip conversion token
		index++;

		incomplete::statement::ConversionDeclaration conversion_decl;
		try_call(assign_to(conversion_decl.conversion_function), parse_expression(tokens, index, type_names));
		return std::move(conversion_decl);
	}

	auto parse_implicit_conversion_declaration(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept
		-> expected<incomplete::statement::ConversionDeclaration, PartialSyntaxError>
	{
		// Skip conversion token
		index++;

		if (tokens[index].source != "conversion")
			return make_syntax_error(tokens[index].source, "Expected keyword \"conversion\" after \"explicit\".");
		index++;

		incomplete::statement::ConversionDeclaration conversion_decl;
		conversion_decl.is_implicit = true;
		try_call(assign_to(conversion_decl.conversion_function), parse_expression(tokens, index, type_names));
		return std::move(conversion_decl);
	}

	auto parse_expression_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::statement::ExpressionStatement, PartialSyntaxError>
	{
		incomplete::statement::ExpressionStatement statement;
		try_call(assign_to(statement.expression), parse_expression(tokens, index, type_names));
		return std::move(statement);
	}

	auto parse_statement_variant(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept 
		-> expected<incomplete::statement::Variant, PartialSyntaxError>
	{
		incomplete::statement::Variant result;

		if (tokens[index].source == "let")
			try_call(assign_to(result), parse_let_statement(tokens, index, type_names))
		else if (tokens[index].source == "uninit")
			try_call(assign_to(result), parse_uninit_statement(tokens, index, type_names))
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
		else if (tokens[index].source == "type")
			try_call(assign_to(result), parse_type_alias(tokens, index, type_names))
		else if (tokens[index].source == "namespace")
			return parse_namespace_declaration(tokens, index, type_names);
		else if (tokens[index].source == "conversion")
			try_call(assign_to(result), parse_conversion_declaration(tokens, index, type_names))
		else if (tokens[index].source == "implicit")
			try_call(assign_to(result), parse_implicit_conversion_declaration(tokens, index, type_names))
		else
			try_call(assign_to(result), parse_expression_statement(tokens, index, type_names));

		// A statement must end with a semicolon.
		if (tokens[index].type != TokenType::semicolon) return make_syntax_error(tokens[index], "Expected ';' after statement.");
		index++;

		return std::move(result);
	}

	auto parse_statement(span<lex::Token const> tokens, size_t & index, std::vector<TypeName> & type_names) noexcept -> expected<incomplete::Statement, PartialSyntaxError>
	{
		auto const expr_source_start = begin_ptr(tokens[index].source);
		try_call_decl(incomplete::statement::Variant var, parse_statement_variant(tokens, index, type_names));
		auto const expr_source_end = end_ptr(tokens[index - 1].source);
		return incomplete::Statement(std::move(var), make_string_view(expr_source_start, expr_source_end));
	}

	auto parse_global_scope(
		span<lex::Token const> tokens,
		std::vector<TypeName> & type_names,
		std::vector<incomplete::Statement> global_initialization_statements
	) noexcept -> expected<std::vector<incomplete::Statement>, PartialSyntaxError>
	{
		size_t index = 0;
		while (index < tokens.size())
		{
			try_call_decl(incomplete::Statement statement, parse_statement(tokens, index, type_names));

			if (has_type<incomplete::statement::ExpressionStatement>(statement.variant)) 
				return make_syntax_error(tokens[index], "An expression statement is not allowed at the global scope.");

			global_initialization_statements.push_back(std::move(statement));
		}

		return global_initialization_statements;
	}

	auto parse_type_names(span<lex::Token const> tokens, std::vector<TypeName> type_names) noexcept -> std::vector<TypeName>
	{
		int brace_level = 0;

		for (size_t i = 0; i < tokens.size(); ++i)
		{
			if (tokens[i].type == TokenType::open_brace)
			{
				brace_level++;
			}
			else if (tokens[i].type == TokenType::close_brace)
			{
				brace_level--;
			}
			else if (brace_level == 0 && tokens[i].source == "struct")
			{
				i++;
				if (i < tokens.size())
				{
					if (tokens[i].source == "<")
					{
						while (i < tokens.size() && tokens[i].source != ">")
							i++;
						i++;

						if (i < tokens.size() && tokens[i].type == TokenType::identifier && !is_keyword(tokens[i].source))
						{
							type_names.push_back({tokens[i].source, TypeName::Type::struct_template});
						}
					}
					else
					{
						if (tokens[i].type == TokenType::identifier && !is_keyword(tokens[i].source))
						{
							type_names.push_back({tokens[i].source, TypeName::Type::type});
						}
					}
				}
			}
		}

		return type_names;
	}

	auto scan_type_names(
		span<incomplete::Module const> modules,
		span<std::vector<TypeName> const> module_type_names, int index,
		out<std::vector<TypeName>> type_names) noexcept -> void
	{
		for (TypeName const & type_name : module_type_names[index])
			type_names->push_back(type_name);

		for (int const dependency_index : modules[index].dependencies)
			scan_type_names(modules, module_type_names, dependency_index, type_names);
	}

	[[nodiscard]] auto parse_module(span<incomplete::Module> modules, int index, span<std::vector<TypeName>> module_type_names) noexcept -> expected<void, SyntaxError>
	{
		std::vector<lex::Token> tokens;
		for (auto const & file : modules[index].files)
		{
			auto new_tokens = lex::tokenize(file.source, std::move(tokens));
			if (new_tokens.has_value())
				tokens = std::move(*new_tokens);
			else
				return Error(complete_syntax_error(new_tokens.error(), file.source, file.filename));
		}

		std::vector<TypeName> type_names = built_in_type_names();
		scan_type_names(modules, module_type_names, index, out(type_names));
		type_names = parse_type_names(tokens, std::move(type_names));
		size_t const imported_type_count = type_names.size();

		auto statements = parse_global_scope(tokens, type_names);
		if (!statements.has_value())
		{
			incomplete::Module::File const & file = file_that_contains(modules[index], statements.error().error_in_source);
			return Error(complete_syntax_error(std::move(statements.error()), file.source, file.filename));
		}

		type_names.erase(type_names.begin(), type_names.begin() + imported_type_count);
		module_type_names[index] = std::move(type_names);
		modules[index].statements = std::move(*statements);

		return success;
	}

	[[nodiscard]] auto parse_modules(span<incomplete::Module> modules) noexcept -> expected<std::vector<int>, SyntaxError>
	{
		try_call_decl(std::vector<int> const sorted_modules, sort_modules_by_dependencies(modules));
		std::vector<std::vector<TypeName>> module_type_names(modules.size());

		for (int i : sorted_modules)
			try_call_void(parse_module(modules, i, module_type_names));

		return sorted_modules;
	}

} // namespace parser
