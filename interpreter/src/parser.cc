#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
#include "syntax_error.hh"
#include "overload.hh"
#include "multicomparison.hh"
#include "utils.hh"
#include "variant.hh"
#include "map.hh"
#include <cassert>
#include <charconv>

using TokenType = lex::Token::Type;

using expr::ExpressionTree;
using expr::OperatorTree;
using expr::OperatorNode;
using expr::Operator;
using expr::Literal;

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

	auto is_keyword(std::string_view name) noexcept -> bool
	{
		// TODO: Binary search
		return std::find(keywords, std::end(keywords), name) != std::end(keywords);
	}

	template <typename C>
	auto is_name_taken(std::string_view name_to_test, C const & registered_names, Program const & program) noexcept -> bool
	{
		for (auto const & n : registered_names)
			if (name_to_test == get(program, n.name))
				return true;
		return false;
	}

	auto is_name_taken(std::string_view name_to_test, Scope const & current_scope, Program const & program) noexcept -> bool
	{
		return is_name_taken(name_to_test, current_scope.variables, program)
			|| is_name_taken(name_to_test, current_scope.functions, program)
			|| is_name_taken(name_to_test, current_scope.function_templates, program)
			|| is_name_taken(name_to_test, current_scope.types, program)
			|| is_name_taken(name_to_test, current_scope.struct_templates, program)
			;
	}

	auto is_function_name_taken(std::string_view name_to_test, Scope const & current_scope, Program const & program) noexcept -> bool
	{
		// Functions don't test against functions because they can overload.
		// TODO: Disable function redefinition if overload is ambiguous.
		return is_name_taken(name_to_test, current_scope.variables, program)
			|| is_name_taken(name_to_test, current_scope.types, program)
			|| is_name_taken(name_to_test, current_scope.struct_templates, program)
			;
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
	auto top(ScopeStack & stack) noexcept -> Scope & { return *stack.back().scope; }

	auto is_unary_operator(lex::Token const & token) noexcept -> bool
	{
		return token.type == TokenType::operator_ &&
			token.source == "-"sv ||
			token.source == "&"sv ||
			token.source == "*"sv ||
			token.source == "not"sv;
	}

	auto insert_conversion_node(ExpressionTree tree, TypeId from, TypeId to, Program const & program) noexcept -> ExpressionTree
	{
		if (from.index == to.index)
		{
			// From reference to reference and value to value there is no conversion. A pointer is a pointer, regardless of constness.
			if (from.is_reference == to.is_reference)
				return tree;

			if (from.is_reference && !to.is_reference)
			{
				expr::DereferenceNode deref_node;
				deref_node.expression = std::make_unique<ExpressionTree>(std::move(tree));
				deref_node.variable_type = to;
				return deref_node;
			}
			else
			{
				raise_syntax_error_if_not(!to.is_mutable, "Can't bind a temporary to a mutable reference.");
				mark_as_to_do("Address of temporaries");
			}
		}
		else if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) && !from.is_reference && !to.is_reference)
		{
			return std::move(tree);
		}
		raise_syntax_error("Conversion between types does not exist.");
	}

	auto insert_conversions(span<ExpressionTree> parameters, span<TypeId const> parsed_parameter_types, span<TypeId const> target_parameter_types, Program const & program)
	{
		for (size_t i = 0; i < target_parameter_types.size(); ++i)
		{
			if (parsed_parameter_types[i] != target_parameter_types[i])
				parameters[i] = insert_conversion_node(std::move(parameters[i]), parsed_parameter_types[i], target_parameter_types[i], program);
		}
	}

	auto resolve_function_overloading_and_insert_conversions(
		OverloadSet overload_set,
		span<ExpressionTree> parameters,
		span<TypeId const> parsed_parameter_types,
		Program & program) noexcept -> FunctionId
	{
		FunctionId const function_id = resolve_function_overloading(overload_set, parsed_parameter_types, program);
		raise_syntax_error_if_not(function_id != invalid_function_id, "Function overload not found.");

		// If any conversion is needed in order to call the function, perform the conversion.
		auto const target_parameter_types = parameter_types(program, function_id);
		insert_conversions(parameters, parsed_parameter_types, target_parameter_types, program);

		return function_id;
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

	auto resolve_operator_overloading(OperatorTree tree, Program & program, ScopeStack const & scope_stack) noexcept -> ExpressionTree
	{
		if (is_operator_node(tree))
		{
			auto & node = std::get<OperatorNode>(tree);
			ExpressionTree operands[2] = {
				resolve_operator_overloading(std::move(*node.left), program, scope_stack),
				resolve_operator_overloading(std::move(*node.right), program, scope_stack) 
			};
			ExpressionTree & left = operands[0];
			ExpressionTree & right = operands[1];
			Operator const op = node.op;

			auto const visitor = overload(
				[](auto) -> ExpressionTree { raise_syntax_error("Looked up name does not name an overload set."); },
				[&](lookup_result::OverloadSet const & overload_set) -> ExpressionTree
				{
					TypeId const operand_types[] = {expression_type_id(left, program), expression_type_id(right, program)};
					auto const function_id = resolve_function_overloading_and_insert_conversions(overload_set, operands, operand_types, program);
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
					else raise_syntax_error("Operator overload not found.");
				}
			);

			auto const lookup = lookup_name(scope_stack, operator_function_name(op), program.string_pool);
			return std::visit(visitor, lookup);
		}
		else
			return std::move(std::get<ExpressionTree>(tree));
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree;
	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree;
	auto parse_substatement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::optional<stmt::Statement>;

	auto parse_mutable_and_pointer(span<lex::Token const> tokens, size_t & index, TypeId type, Program & program) noexcept -> TypeId
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
			TypeId const pointer_type = pointer_type_for(type, program);
			index++;
			return parse_mutable_and_pointer(tokens, index, pointer_type, program);
		}
		else
			return type;
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, ScopeStackView scope_stack, Program & program) noexcept -> TypeId;

	auto parse_template_instantiation_parameter_list(span<lex::Token const> tokens, size_t & index, ScopeStackView scope_stack, Program & program, StructTemplateId template_id) noexcept -> TypeId
	{
		raise_syntax_error_if_not(tokens[index].source == "<", "Expected '<' after template name.");
		index++;

		StructTemplate const struct_template = program.struct_templates[template_id.index];
		size_t const template_parameter_count = struct_template.template_parameters.size();
		
		std::vector<TypeId> parameters(template_parameter_count);
		for (size_t i = 0; i < template_parameter_count; ++i)
		{
			parameters[i] = parse_type_name(tokens, index, scope_stack, program);
			if (i < template_parameter_count - 1)
			{
				raise_syntax_error_if_not(tokens[index].type == TokenType::comma, "Expected comma ',' after template parameter.");
				index++;
			}
		}

		raise_syntax_error_if_not(tokens[index].source == ">", "Expected comma '>' at the end of template parameter list.");
		index++;

		return instantiate_struct_template(program, template_id, parameters);
	}

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, ScopeStackView scope_stack, Program & program) noexcept -> TypeId
	{
		// Lookup type name.
		auto const visitor = overload(
			[](auto const &) { return TypeId::none; },
			[&](lookup_result::Type type) { index++;  return type.type_id; },
			[&](lookup_result::StructTemplate struct_template) { index++; return parse_template_instantiation_parameter_list(tokens, index, scope_stack, program, struct_template.template_id); }
		);
		std::string_view const type_name = tokens[index].source;
		TypeId type_found = std::visit(visitor, lookup_name(scope_stack, type_name, program.string_pool));

		if (type_found == TypeId::none)
			return TypeId::none;

		type_found = parse_mutable_and_pointer(tokens, index, type_found, program);

		// Look for reference qualifier.
		if (tokens[index].source == "&"sv)
		{
			type_found.is_reference = true;
			index++;
		}

		return type_found;
	}

	auto parse_template_parameter_list(span<lex::Token const> tokens, size_t & index, Program & program) noexcept -> std::vector<TemplateParameter>
	{
		// Skip < token.
		index++;

		std::vector<TemplateParameter> parsed_parameters;

		for (;;)
		{
			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier.");

			TemplateParameter param;
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as template parameter name.");
			param.name = pool_string(program, tokens[index].source);
			parsed_parameters.push_back(param);
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

	auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, ParseParams p, Function & function) noexcept -> void
	{
		// Parameters must be between parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after fn.");
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			TypeId const arg_type = parse_type_name(tokens, index, p.scope_stack, p.program);
			raise_syntax_error_if_not(is_data_type(arg_type), "Cannot use 'void' as a function parameter type.");

			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after function parameter type.");
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as function parameter name.");
			raise_syntax_error_if_not(!is_name_taken(tokens[index].source, function.variables, p.program), "More than one function parameter with the same name.");
			add_variable_to_scope(function, pool_string(p.program, tokens[index].source), arg_type, 0, p.program);
			function.parameter_count++;
			function.parameter_size = function.stack_frame_size;
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

			// Return type. By now only int supported.
			function.return_type = parse_type_name(tokens, index, p.scope_stack, p.program);
		}
		else
			function.return_type = TypeId::deduce;
	}

	auto parse_function_body(span<lex::Token const> tokens, size_t & index, ParseParams p, Function & function) noexcept -> void
	{
		// Body of the function is enclosed by braces.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_brace, "Expected '{' at start of function body.");
		index++;

		p.scope_stack.push_back({&function, ScopeType::function });

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			auto statement = parse_substatement(tokens, index, {p.program, p.scope_stack, function.return_type});
			if (statement)
				function.statements.push_back(std::move(*statement));
		}

		p.scope_stack.pop_back();

		// Skip closing brace.
		index++;
	}

	auto parse_function_template_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> expr::FunctionTemplateNode
	{
		FunctionTemplate fn_template;
		fn_template.template_parameters = parse_template_parameter_list(tokens, index, p.program);

		size_t template_tokens_start = index;

		// Parse parameters.
		{
			// Parameters must be between parenthesis.
			raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' at start of function template parameter list.");
			index++;

			// Parse arguments.
			while (tokens[index].type == TokenType::identifier)
			{
				TypeId const arg_type = parse_type_name(tokens, index, p.scope_stack, p.program);
				if (arg_type != TypeId::none)
				{
					fn_template.parameters.push_back(arg_type);
				}
				else
				{
					// Check if the argument is a dependent type.
					std::string_view const type_name = tokens[index].source;
					index++;

					auto const it = std::find_if(fn_template.template_parameters.begin(), fn_template.template_parameters.end(), [&](TemplateParameter const & param)
					{
						return get(p.program, param.name) == type_name;
					});
					raise_syntax_error_if_not(it != fn_template.template_parameters.end(), "Expected type name in function parameter.");

					DependentType dependent_arg_type;
					dependent_arg_type.flat_value = 0;
					dependent_arg_type.index = static_cast<unsigned>(it - fn_template.template_parameters.begin());

					// Look for mutable qualifier.
					if (tokens[index].source == "mut"sv)
					{
						dependent_arg_type.is_mutable = true;
						index++;
					}

					// Look for reference qualifier.
					if (tokens[index].source == "&"sv)
					{
						dependent_arg_type.is_reference = true;
						index++;
					}

					// By now it won't support pointers.

					fn_template.parameters.push_back(dependent_arg_type);
				}

				// Skip parameter name token.
				raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type in function parameter list.");
				index++;

				if (tokens[index].type == TokenType::close_parenthesis)
					break;

				raise_syntax_error_if_not(tokens[index].type == TokenType::comma, "Expected ',' or ')' after parameter.");
				index++;
			}
		}

		// Look for opening {
		while (tokens[index].type != TokenType::open_brace)
			index++;
		index++;
		int scope_depth = 1;

		for (;;)
		{
			if (tokens[index].type == TokenType::open_brace)
				scope_depth++;
			else if (tokens[index].type == TokenType::close_brace)
				scope_depth--;

			index++;

			if (scope_depth == 0)
				break;
		}

		fn_template.tokens = tokens.subspan(template_tokens_start, index - template_tokens_start);

		expr::FunctionTemplateNode node;
		node.function_template_id.index = static_cast<unsigned>(p.program.function_templates.size());
		p.program.function_templates.push_back(std::move(fn_template));
		return node;
	}

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::variant<expr::FunctionNode, expr::FunctionTemplateNode>
	{
		// Skip fn token.
		index++;

		if (tokens[index].source == "<"sv)
		{
			return parse_function_template_expression(tokens, index, p);
		}

		Function function;

		parse_function_prototype(tokens, index, p, function);
		parse_function_body(tokens, index, p, function);

		// Add the function to the program.
		p.program.functions.push_back(std::move(function));
		auto const func_id = FunctionId{0, static_cast<unsigned>(p.program.functions.size() - 1)};

		return expr::FunctionNode{func_id};
	}

	auto parse_comma_separated_expression_list(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::vector<ExpressionTree>
	{
		std::vector<ExpressionTree> parsed_expressions;

		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '('.");
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
			parsed_expressions.push_back(parse_subexpression(tokens, index, p));

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
				raise_syntax_error("Expected ')' or ',' after expression.");
		}

		return parsed_expressions;
	}

	auto parse_function_call_expression(span<lex::Token const> tokens, size_t & index, ParseParams p, OverloadSet overload_set) noexcept -> expr::FunctionCallNode
	{
		expr::FunctionCallNode node;
		node.parameters = parse_comma_separated_expression_list(tokens, index, p);
		std::vector<TypeId> const  parsed_parameter_types = map(node.parameters, expr::expression_type_id(p.program));
		node.function_id = resolve_function_overloading_and_insert_conversions(overload_set, node.parameters, parsed_parameter_types, p.program);

		return node;
	}

	auto parse_struct_member_initializer_by_name_list(span<lex::Token const> tokens, size_t & index, ParseParams p, Struct const & struct_data) noexcept -> std::vector<ExpressionTree>
	{
		auto parsed_expressions = std::vector<ExpressionTree>(struct_data.member_variables.size());
		auto expression_initialized = std::vector<bool>(struct_data.member_variables.size(), false);

		// Parameter list starts with (
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after type name in struct constructor.");
		index++;

		for (;;)
		{
			// A member initializer by name starts with a period.
			raise_syntax_error_if_not(tokens[index].type == TokenType::period, "Expected '.' in designated initializer.");
			index++;

			// Find the member to initialize.
			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected member name after '.' in designated initializer.");
			std::string_view const member_name = tokens[index].source;
			index++;

			int const member_variable_index = find_member_variable(struct_data, member_name, p.program.string_pool);
			raise_syntax_error_if_not(member_variable_index != -1, "Expected member name after '.' in designated initializer.");
			raise_syntax_error_if_not(!expression_initialized[member_variable_index], "Same member initialized twice in designated initializer.");

			// Next token must be =
			raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' after member name in designated initializer.");
			index++;

			// Parse a parameter.
			parsed_expressions[member_variable_index] = parse_subexpression(tokens, index, p);
			expression_initialized[member_variable_index] = true;

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
				raise_syntax_error("Expected ')' or ',' after designated initializer.");
		}

		// For any value that is not given an initializer, use the default if available or fail to parse.
		for (size_t i = 0; i < parsed_expressions.size(); ++i)
		{
			if (!expression_initialized[i])
			{
				MemberVariable const & variable = struct_data.member_variables[i];
				raise_syntax_error_if_not(variable.initializer_expression.has_value(), "Uninitialized member is not default constructible in designated initializer.");
				parsed_expressions[i] = *variable.initializer_expression;
			}
		}
		
		return parsed_expressions;
	}

	auto parse_struct_constructor_expression(span<lex::Token const> tokens, size_t & index, ParseParams p, TypeId type_id) noexcept -> expr::StructConstructorNode
	{
		Type const & type = type_with_id(p.program, type_id);
		raise_syntax_error_if_not(is_struct(type), "Constructor call syntax is only available for structs (for now).");
		Struct const & struct_data = *struct_for_type(p.program, type);
		auto const struct_member_types = map(struct_data.member_variables, &Variable::type);

		expr::StructConstructorNode node;

		if (tokens[index + 1].type == TokenType::period)
			node.parameters = parse_struct_member_initializer_by_name_list(tokens, index, p, struct_data);
		else
			node.parameters = parse_comma_separated_expression_list(tokens, index, p);

		node.constructed_type = type_id;

		// If constructing from no parameters, check if the struct is default constructible
		// and add default constructor if so.
		if (node.parameters.empty())
		{
			// TODO: Recursive default construction.

			node.parameters.reserve(struct_data.member_variables.size());
			for (MemberVariable const & var : struct_data.member_variables)
			{
				raise_syntax_error_if_not(var.initializer_expression.has_value(), "Attempted to default construct a struct with a member that is not default constructible.");
				node.parameters.push_back(*var.initializer_expression);
			}
		}

		raise_syntax_error_if_not(struct_member_types.size() == node.parameters.size(), "Number of parameters to constructor call does not match number of member variables.");
		for (size_t i = 0; i < node.parameters.size(); ++i)
		{
			TypeId const parsed_type = expression_type_id(node.parameters[i], p.program);
			raise_syntax_error_if_not(is_convertible(parsed_type, struct_member_types[i], p.program), "Expression is not convertible to member type in constructor.");
			node.parameters[i] = insert_conversion_node(std::move(node.parameters[i]), parsed_type, struct_member_types[i], p.program);
		}

		return node;
	}

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> expr::IfNode
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after if.");
		index++;

		expr::IfNode if_node;
		if_node.condition = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));
		raise_syntax_error_if_not(expression_type_id(*if_node.condition, p.program) == TypeId::bool_, "Condition expression of if must return bool.");

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after if condition.");
		index++;

		if_node.then_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));

		// Expect keyword else to separate then and else cases.
		raise_syntax_error_if_not(tokens[index].source == "else", "Expected keyword \"else\" after if expression body.");
		index++;

		if_node.else_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));

		// Ensure that both branches return the same type.
		TypeId const then_type = expression_type_id(*if_node.then_case, p.program);
		TypeId const else_type = expression_type_id(*if_node.else_case, p.program);
		TypeId const common = common_type(then_type, else_type, p.program);
		raise_syntax_error_if_not(common != TypeId::none, "Could not find common type for return types of then and else branches in if expression.");
		if (then_type != common)
			*if_node.then_case = insert_conversion_node(std::move(*if_node.then_case), then_type, common, p.program);
		if (else_type != common)
			*if_node.else_case = insert_conversion_node(std::move(*if_node.else_case), else_type, common, p.program);

		return if_node;
	}

	auto all_branches_return(stmt::Statement const & statement, Program const & program) noexcept -> bool
	{
		auto const visitor = overload(
			[](auto const &) { return false; }, // Default case, does not return
			[](stmt::ReturnStatement const &) { return true; },
			[&](stmt::IfStatement const & if_node)
			{
				return all_branches_return(*if_node.then_case, program)
					&& all_branches_return(*if_node.else_case, program);
			},
			[&](stmt::StatementBlock const & block_node)
			{
				return all_branches_return(block_node.statements.back(), program);
			}
		);
		return std::visit(visitor, statement.as_variant());
	}

	auto parse_statement_block_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> expr::StatementBlockNode
	{
		// Skip opening brace.
		index++;

		expr::StatementBlockNode node;
		node.return_type = TypeId::deduce;

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			p.scope_stack.push_back({&node.scope, ScopeType::block});
			auto statement = parse_substatement(tokens, index, {p.program, p.scope_stack, node.return_type});
			p.scope_stack.pop_back();
			if (statement)
				node.statements.push_back(std::move(*statement));
		}

		// Ensure that all branches return.
		raise_syntax_error_if_not(all_branches_return(node.statements.back(), p.program), "Not all branches of statement block expression return.");

		// Skip closing brace.
		raise_syntax_error_if_not(tokens[index].type == TokenType::close_brace, "Expected '}' at the end of statement block expression.");
		index++;

		return node;
	}

	auto parse_unary_operator(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree
	{
		Operator const op = parse_operator(tokens[index].source);
		index++;

		auto operand = parse_expression_and_trailing_subexpressions(tokens, index, p);
		TypeId const operand_type = expression_type_id(operand, p.program);

		// Addressof is a particular case that cannot be overloaded and is implemented by the compiler.
		if (op == Operator::addressof)
		{
			raise_syntax_error_if_not(operand_type.is_reference, "Cannot take the address of a temporary.");
			expr::AddressofNode node;
			node.return_type = pointer_type_for(remove_reference(operand_type), p.program);
			node.operand = std::make_unique<ExpressionTree>(std::move(operand));
			return node;
		}
		// Built in dereference
		else if (op == Operator::dereference && is_pointer(type_with_id(p.program, operand_type)))
		{
			expr::DepointerNode node;
			node.return_type = make_reference(std::get<PointerType>(type_with_id(p.program, operand_type).extra_data).value_type);
			node.operand = std::make_unique<ExpressionTree>(insert_conversion_node(std::move(operand), operand_type, remove_reference(operand_type), p.program));

			return node;
		}
		else
		{
			auto const visitor = overload(
				[](auto) -> expr::FunctionCallNode { raise_syntax_error("Name does not name an operator overload set."); },
				[&](lookup_result::OverloadSet const & overload_set) -> expr::FunctionCallNode
				{
					TypeId const operand_type = expression_type_id(operand, p.program);
					auto const function_id = resolve_function_overloading_and_insert_conversions(overload_set, { &operand, 1 }, { &operand_type, 1 }, p.program);
					if (function_id != invalid_function_id)
					{
						expr::FunctionCallNode func_node;
						func_node.function_id = function_id;
						func_node.parameters.push_back(std::move(operand));
						return func_node;
					}
					else raise_syntax_error("Overload not found.");
				}
			);

			std::string_view const function_name = operator_function_name(op);
			auto const lookup = lookup_name(p.scope_stack, function_name, p.program.string_pool);
			return std::visit(visitor, lookup);
		}
	}

	auto parse_single_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree
	{
		if (tokens[index].source == "fn")
		{
			auto const node = parse_function_expression(tokens, index, p);
			// Opening parenthesis after a function means a function call.
			if (tokens[index].type == TokenType::open_parenthesis)
			{
				OverloadSet overload_set;
				std::visit(overload(
					[&](expr::FunctionNode const & fn_node) { overload_set.function_ids = span<FunctionId const>(&fn_node.function_id, 1); },
					[&](expr::FunctionTemplateNode const & fn_template_node) { overload_set.function_template_ids = span<FunctionTemplateId const>(&fn_template_node.function_template_id, 1); }
				), node);
				return parse_function_call_expression(tokens, index, p, overload_set);
			}
			else
				return upcast<ExpressionTree>(node);
		}
		else if (tokens[index].source == "if")
		{
			return parse_if_expression(tokens, index, p);
		}
		else if (tokens[index].type == TokenType::open_brace)
		{
			return parse_statement_block_expression(tokens, index, p);
		}
		else if (tokens[index].type == TokenType::literal_int)
		{
			return ExpressionTree(Literal<int>{parse_number_literal<int>(tokens[index++].source)});
		}
		else if (tokens[index].type == TokenType::literal_float)
		{
			return ExpressionTree(Literal<float>{parse_number_literal<float>(tokens[index++].source)});
		}
		else if (tokens[index].type == TokenType::literal_bool)
		{
			return ExpressionTree(Literal<bool>{tokens[index++].source[0] == 't'}); // if it starts with t it must be bool, and otherwise it must be false.
		}
		else if (tokens[index].type == TokenType::identifier)
		{
			auto const visitor = overload(
				[&](lookup_result::Variable result) -> ExpressionTree
				{
					expr::LocalVariableNode var_node;
					var_node.variable_type = result.variable_type;
					var_node.variable_offset = result.variable_offset;
					return var_node;
				},
				[&](lookup_result::GlobalVariable result) -> ExpressionTree
				{
					expr::GlobalVariableNode var_node;
					var_node.variable_type = result.variable_type;
					var_node.variable_offset = result.variable_offset;
					return var_node;
				},
				[&](lookup_result::OverloadSet result) -> ExpressionTree
				{
					if (tokens[index].type == TokenType::open_parenthesis)
						return parse_function_call_expression(tokens, index, p, result);
					else
						return expr::FunctionNode{result.function_ids[0]}; // TODO: Overload set node?
				},
				[&](lookup_result::Type result) -> ExpressionTree 
				{
					return parse_struct_constructor_expression(tokens, index, p, result.type_id);
				},
				[&](lookup_result::StructTemplate result) -> ExpressionTree
				{
					TypeId const template_instantiation = parse_template_instantiation_parameter_list(tokens, index, p.scope_stack, p.program, result.template_id);
					return parse_struct_constructor_expression(tokens, index, p, template_instantiation);
				},
				[](lookup_result::Nothing) -> ExpressionTree { raise_syntax_error("Name lookup failed."); }
			);
			auto const lookup = lookup_name(p.scope_stack, tokens[index].source, p.program.string_pool);
			index++;
			return std::visit(visitor, lookup);
		}
		else if (tokens[index].type == TokenType::open_parenthesis)
		{
			index++;
			auto expr = parse_subexpression(tokens, index, p);

			// Next token must be close parenthesis.
			raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after parenthesized expression.");
			index++;

			return expr;
		}
		else if (is_unary_operator(tokens[index]))
		{
			return parse_unary_operator(tokens, index, p);
		}
		else raise_syntax_error("Unrecognized token. Expected expression."); // TODO: Actual error handling.
	}

	auto parse_expression_and_trailing_subexpressions(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree
	{
		ExpressionTree tree = parse_single_expression(tokens, index, p);

		while (index < tokens.size())
		{
			// Loop to possibly parse chains of member accesses.
			if (tokens[index].type == TokenType::period)
			{
				index++;
				TypeId const last_operand_type_id = expression_type_id(tree, p.program);
				Type const & last_operand_type = type_with_id(p.program, last_operand_type_id);
				raise_syntax_error_if_not(is_struct(last_operand_type), "Member access only allowed for struct types.");
				Struct const & last_operand_struct = *struct_for_type(p.program, last_operand_type);

				raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected member name after '.'.");
				std::string_view const member_name = tokens[index].source;
				int const member_variable_index = find_member_variable(last_operand_struct, member_name, p.program.string_pool);
				raise_syntax_error_if_not(member_variable_index != -1, "Expected member name after '.'.");
				index++;
				Variable const & member_variable = last_operand_struct.member_variables[member_variable_index];

				expr::MemberVariableNode var_node;
				var_node.owner = std::make_unique<ExpressionTree>(std::move(tree));
				var_node.variable_type = member_variable.type;

				if (last_operand_type_id.is_reference)
				{
					var_node.variable_type.is_reference = true;

					if (last_operand_type_id.is_mutable)
						var_node.variable_type.is_mutable = true;
				}
				var_node.variable_offset = member_variable.offset;
				tree = std::move(var_node);
			}
			else break;
		}

		return tree;
	}

	auto parse_subexpression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> ExpressionTree
	{
		std::vector<ExpressionTree> operands;
		std::vector<Operator> operators;

		while (index < tokens.size())
		{
			operands.push_back(parse_expression_and_trailing_subexpressions(tokens, index, p));

			// If the next token is an operator, parse the operator and repeat. Otherwise end the loop and return the expression.
			if (index < tokens.size() && tokens[index].type == TokenType::operator_)
			{
				operators.push_back(parse_operator(tokens[index].source));
				index++;
			}
			else break;
		}

		return resolve_operator_overloading(resolve_operator_precedence(operands, operators), p.program, p.scope_stack);
	}

	auto parse_expression(span<lex::Token const> tokens, ParseParams p) noexcept -> ExpressionTree
	{
		size_t index = 0;
		ExpressionTree tree = parse_subexpression(tokens, index, p);
		assert(index == tokens.size()); // Ensure that all tokens were read.
		return tree;
	}

	auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data)
	{
		expr::StructConstructorNode default_constructor_node;
		default_constructor_node.constructed_type = type_id;
		default_constructor_node.parameters.reserve(struct_data.member_variables.size());
		for (MemberVariable const & var : struct_data.member_variables)
			default_constructor_node.parameters.push_back(*var.initializer_expression);
		return default_constructor_node;
	}

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::VariableDeclarationStatement
	{
		// A variable declaration statement has the following form:
		// [type] [var_name] = [expr];

		stmt::VariableDeclarationStatement node;

		// A statement begins with the type of the declared variable.
		TypeId const type_found = parse_type_name(tokens, index, p.scope_stack, p.program);

		// The second token of the statement is the variable name.
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as variable name.");
		raise_syntax_error_if_not(!is_name_taken(tokens[index].source, top(p.scope_stack), p.program), "More than one local variable with the same name.");
		node.variable_offset = add_variable_to_scope(top(p.scope_stack), pool_string(p.program, tokens[index].source), type_found, local_variable_offset(p.scope_stack), p.program);
		index++;

		if (tokens[index].type == TokenType::semicolon)
		{
			raise_syntax_error_if_not(is_default_constructible(type_found, p.program), "Attempted to default construct a type that is not default constructible.");
			node.assigned_expression = synthesize_default_constructor(type_found, *struct_for_type(p.program, type_found));
			return node;
		}

		// The third token is a '='.
		raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' or ';' after variable name in declaration.");
		index++;

		// The rest is the expression assigned to the variable.
		node.assigned_expression = parse_subexpression(tokens, index, p);
		TypeId const assigned_type = expression_type_id(node.assigned_expression, p.program);
		// Require that the expression assigned to the variable is convertible to the type of the variable.
		raise_syntax_error_if_not(is_convertible(assigned_type, type_found, p.program), "Cannot convert initializer expression type to declared variable type.");
		if (assigned_type != type_found)
			node.assigned_expression = insert_conversion_node(std::move(node.assigned_expression), assigned_type, type_found, p.program);

		return node;
	}

	auto bind_function_name(std::string_view function_name, FunctionId function_id, Program & program, Scope & scope) noexcept -> void
	{
		// Special rules for the main function.
		if (function_name == "main")
		{
			raise_syntax_error_if_not(&scope == &program.global_scope, "Main function must be in the global scope.");
			raise_syntax_error_if_not(!function_id.is_extern, "Main cannot be a extern function.");

			// Main must not take parameters and return int.
			Function const & main_function = program.functions[function_id.index];
			raise_syntax_error_if_not(main_function.return_type == TypeId::int_, "Main must return int.");
			raise_syntax_error_if_not(main_function.parameter_count == 0, "Main cannot take parameters.");

			// There can only be one main function.
			raise_syntax_error_if_not(program.main_function == invalid_function_id, "Redefinition of main function. There can only be one main function.");

			// Bind the function as the program's main function.
			program.main_function = function_id;

			// Main function does not go to the list of function names. You cannot lookup main.
		}
		else
		{
			raise_syntax_error_if_not(!is_keyword(function_name), "Cannot use a keyword as function name.");
			scope.functions.push_back({pool_string(program, function_name), function_id});
		}
	}

	auto parse_function_declaration_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::nullopt_t
	{
		// A function declaration statement has the following form:
		// let [name] = [function expr];

		// Skip the let.
		index++;

		lex::Token const id_token = tokens[index];
		raise_syntax_error_if_not(id_token.type == TokenType::identifier, "Expected identifier after let.");
		index++;
		raise_syntax_error_if_not(tokens[index].source == "=", "Expected '=' after identifier in function declaration.");
		index++;

		// Skip the fn.
		index++;

		// It's a template
		if (tokens[index].source == "<")
		{
			expr::FunctionTemplateNode template_node = parse_function_template_expression(tokens, index, p);
			raise_syntax_error_if_not(!is_keyword(id_token.source), "Cannot use a keyword as function template name.");
			raise_syntax_error_if_not(!is_function_name_taken(id_token.source, top(p.scope_stack), p.program), "More than one function template with the same name.");
			top(p.scope_stack).function_templates.push_back({pool_string(p.program, id_token.source), template_node.function_template_id});
		}
		else
		{
			// Adding the function to the program before parsing the body allows the body of the function to find itself and be recursive.

			// Add a new function to the program.
			Function & function = p.program.functions.emplace_back();
			// Add the function name to the scope.
			auto const function_id = FunctionId{ 0, static_cast<unsigned>(p.program.functions.size() - 1) };
			parse_function_prototype(tokens, index, p, function);
			bind_function_name(id_token.source, function_id, p.program, top(p.scope_stack));

			// Operate on a local temporary to avoid invalidation of the reference on reallocation.
			Function temp;
			temp.parameter_count = function.parameter_count;
			temp.parameter_size = function.parameter_size;
			temp.stack_frame_size = function.stack_frame_size;
			temp.stack_frame_alignment = function.stack_frame_alignment;
			temp.return_type = function.return_type;
			temp.variables = function.variables;
			parse_function_body(tokens, index, p, temp);
			p.program.functions[function_id.index] = std::move(temp);
		}

		return std::nullopt;
	}

	auto parse_let_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::optional<stmt::Statement>
	{
		// If we have directly a function expression, parse it in a special way to handle recursion. 
		// TODO: Think of generalizing binding names to function expressions somehow.
		if (tokens[index + 3].source == "fn")
			return parse_function_declaration_statement(tokens, index, p);
		else
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
				raise_syntax_error_if_not(tokens[index].type == TokenType::operator_, "Expected operator after '(' in function declaration.");
				name = tokens[index].source;
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
			expr::ExpressionTree expression = parse_subexpression(tokens, index, p);
			TypeId const expr_type = expression_type_id(expression, p.program);
			
			if (expr_type == TypeId::function)
			{
				raise_syntax_error_if_not(!is_mutable, "A function cannot be mutable.");
				FunctionId const function_id = std::get<expr::FunctionNode>(expression).function_id;
				bind_function_name(name, function_id, p.program, top(p.scope_stack));
				return std::nullopt;
			}
			else
			{
				raise_syntax_error_if_not(is_data_type(expr_type), "Cannot declare a variable of type void.");
				raise_syntax_error_if_not(!is_operator, "An operator name can only be bound to a function.");
				TypeId const var_type = is_mutable ? make_mutable(decay(expr_type)) : decay(expr_type);
				stmt::VariableDeclarationStatement node;
				raise_syntax_error_if_not(!is_keyword(name), "Cannot use a keyword as variable name.");
				raise_syntax_error_if_not(!is_name_taken(name, top(p.scope_stack), p.program), "More than one local variable with the same name.");
				node.variable_offset = add_variable_to_scope(top(p.scope_stack), pool_string(p.program, name), var_type, local_variable_offset(p.scope_stack), p.program);
				if (var_type == expr_type)
					node.assigned_expression = std::move(expression);
				else
					node.assigned_expression = insert_conversion_node(std::move(expression), expr_type, var_type, p.program);
				return node;
			}
		}
	}

	auto parse_expression_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::ExpressionStatement
	{
		stmt::ExpressionStatement node;
		node.expression = parse_subexpression(tokens, index, p);
		return node;
	}

	auto parse_return_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::ReturnStatement
	{
		raise_syntax_error_if_not(p.current_return_type != TypeId::none, "A return statement cannot appear in the global scope.");

		// Skip return token.
		index++;

		ExpressionTree return_expr = parse_subexpression(tokens, index, p);

		TypeId const return_expr_type = expression_type_id(return_expr, p.program);

		if (p.current_return_type == TypeId::deduce)
			p.current_return_type = decay(return_expr_type);

		if (return_expr_type != p.current_return_type)
			return_expr = insert_conversion_node(std::move(return_expr), return_expr_type, p.current_return_type, p.program);

		stmt::ReturnStatement node;
		node.returned_expression = std::move(return_expr);

		// Cannot return special types that do not represent data types.
		raise_syntax_error_if_not(is_data_type(expression_type_id(node.returned_expression, p.program)), "Cannot return special types that do not represent data types.");

		return node;
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::IfStatement
	{
		// Skip the if
		index++;

		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after if.");
		index++;

		stmt::IfStatement node;
		node.condition = parse_subexpression(tokens, index, p);
		raise_syntax_error_if_not(expression_type_id(node.condition, p.program) == TypeId::bool_, "Condition of an if statement must return bool.");

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in if statement.");
		index++;

		auto then_case = parse_substatement(tokens, index, p);
		raise_syntax_error_if_not(then_case.has_value(), "No-op statement not allowed as body of if statement branch.");
		node.then_case = std::make_unique<stmt::Statement>(*std::move(then_case));

		// For if statement else is optional.
		if (tokens[index].source == "else")
		{
			// Skip else token.
			index++;
			auto else_case = parse_substatement(tokens, index, p);
			raise_syntax_error_if_not(else_case.has_value(), "No-op statement not allowed as body of if statement branch.");
			node.else_case = std::make_unique<stmt::Statement>(*std::move(else_case));
		}

		return node;
	}

	auto parse_statement_block(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::StatementBlock
	{
		// Skip opening }
		index++;

		stmt::StatementBlock node;
		p.scope_stack.push_back({&node.scope, ScopeType::block});

		// Parse all statements in the function.
		while (tokens[index].type != TokenType::close_brace)
		{
			auto statement = parse_substatement(tokens, index, p);
			if (statement)
				node.statements.push_back(std::move(*statement));
		}
		p.scope_stack.pop_back();

		// Skip closing brace.
		index++;

		return node;
	}

	auto parse_while_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::WhileStatement
	{
		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after while.");
		index++;

		stmt::WhileStatement node;

		// Parse condition. Must return bool.
		node.condition = parse_subexpression(tokens, index, p);
		raise_syntax_error_if_not(expression_type_id(node.condition, p.program) == TypeId::bool_, "Condition of while statement must return bool.");

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after condition in while statement.");
		index++;

		// Parse body
		auto body = parse_substatement(tokens, index, p);
		raise_syntax_error_if_not(body.has_value(), "No-op statement not allowed as body of while statement.");
		node.body = std::make_unique<stmt::Statement>(std::move(*body));

		return node;
	}

	auto parse_for_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::ForStatement
	{
		// Syntax of for loop
		// for (declaration-or-expression; condition-expression; end-expression)
		//	   statement 

		// Skip while token.
		index++;

		// Condition goes inside parenthesis.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_parenthesis, "Expected '(' after for.");
		index++;

		stmt::ForStatement for_node;
		p.scope_stack.push_back({&for_node.scope, ScopeType::block});

		// Parse init statement. Must be an expression or a declaration.
		auto init_statement = parse_substatement(tokens, index, p);
		raise_syntax_error_if_not(
			init_statement.has_value() && (has_type<stmt::VariableDeclarationStatement>(*init_statement) || has_type<stmt::ExpressionStatement>(*init_statement)),
			"init-statement of a for statement must be a variable declaration or an expression.");
		for_node.init_statement = std::make_unique<stmt::Statement>(std::move(*init_statement));

		// Parse condition. Must return bool.
		for_node.condition = parse_subexpression(tokens, index, p);
		raise_syntax_error_if_not(expression_type_id(for_node.condition, p.program) == TypeId::bool_, "Condition of for statement must return bool.");

		// Parse ; after condition.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after for statement condition.");
		index++;

		// Parse end expression.
		for_node.end_expression = parse_subexpression(tokens, index, p);

		raise_syntax_error_if_not(tokens[index].type == TokenType::close_parenthesis, "Expected ')' after for statement end expression.");
		index++;

		// Parse body
		auto body = parse_substatement(tokens, index, p);
		raise_syntax_error_if_not(body.has_value(), "No-op statement not allowed as body of for statement.");
		for_node.body = std::make_unique<stmt::Statement>(std::move(*body));

		p.scope_stack.pop_back();

		return for_node;
	}

	template <typename Stmt>
	auto parse_break_or_continue_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> Stmt
	{
		// Should check if parsing a loop and otherwise give an error.

		static_cast<void>(tokens, p);
		index++;
		return Stmt();
	}

	auto parse_template_struct_declaration(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::nullopt_t
	{
		StructTemplate new_struct_template;
		new_struct_template.template_parameters = parse_template_parameter_list(tokens, index, p.program);

		// Parse name
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after struct.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as struct name.");
		raise_syntax_error_if_not(!is_name_taken(tokens[index].source, top(p.scope_stack), p.program), "More than one struct template with the same name.");
		PooledString const new_template_name = pool_string(p.program, tokens[index].source);
		index++;

		// Skip { token.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_brace, "Expected '{' after struct name.");
		index++;

		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			MemberVariableTemplate member;

			TypeId const member_type = parse_type_name(tokens, index, p.scope_stack, p.program);
			if (member_type != TypeId::none)
			{
				raise_syntax_error_if_not(is_data_type(member_type), "Member variable cannot be void.");
				raise_syntax_error_if_not(!member_type.is_reference, "Member variable cannot be reference.");
				raise_syntax_error_if_not(!member_type.is_mutable, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

				member.type = member_type;
				member.is_dependent = false;
			}
			else
			{
				std::string_view const member_type_name = tokens[index].source;
				index++;
				auto const it = std::find_if(new_struct_template.template_parameters.begin(), new_struct_template.template_parameters.end(), [&](TemplateParameter const & param)
				{
					return get(p.program, param.name) == member_type_name;
				});
				raise_syntax_error_if_not(it != new_struct_template.template_parameters.end(), "Expected type name in member variable declaration.");

				TypeId dependent_member_type;
				dependent_member_type.flat_value = 0;
				dependent_member_type.index = static_cast<unsigned>(it - new_struct_template.template_parameters.begin());

				member.type = dependent_member_type;
				member.is_dependent = true;
			}
			
			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name.");
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as member variable name.");
			raise_syntax_error_if_not(
				!is_name_taken(tokens[index].source, new_struct_template.member_variables, p.program),
				"More than one member variable with the same name.");
			member.name = pool_string(p.program, tokens[index].source);
			index++;

			// TODO: Initializer expression.
			if (tokens[index].source == "=")
				mark_as_to_do("Member default initializer expressions for struct templates");

			new_struct_template.member_variables.push_back(member);

			raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after member name.");
			index++;
		}

		// Skip } token.
		index++;

		StructTemplateId new_template_id;
		new_template_id.index = static_cast<unsigned>(p.program.struct_templates.size());
		p.program.struct_templates.push_back(std::move(new_struct_template));
		top(p.scope_stack).struct_templates.push_back(StructTemplateName{new_template_name, new_template_id});

		return std::nullopt;
	}

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::nullopt_t
	{
		// Skip struct token.
		index++;

		if (tokens[index].source == "<"sv)
			return parse_template_struct_declaration(tokens, index, p);

		// Parse name
		raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after struct.");
		raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as struct name.");
		raise_syntax_error_if_not(
			!is_name_taken(tokens[index].source, top(p.scope_stack), p.program),
			"More than one struct with the same name.");
		PooledString const type_name = pool_string(p.program, tokens[index].source);
		index++;

		Type new_type;
		new_type.size = 0;
		new_type.alignment = 1;

		// Skip { token.
		raise_syntax_error_if_not(tokens[index].type == TokenType::open_brace, "Expected '{' after struct name.");
		index++;

		Struct str;
		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			TypeId const member_type = parse_type_name(tokens, index, p.scope_stack, p.program);
			raise_syntax_error_if_not(is_data_type(member_type), "Member variable cannot be void.");
			raise_syntax_error_if_not(!member_type.is_reference, "Member variable cannot be reference.");
			raise_syntax_error_if_not(!member_type.is_mutable, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

			raise_syntax_error_if_not(tokens[index].type == TokenType::identifier, "Expected identifier after type name in member variable declaration.");
			raise_syntax_error_if_not(!is_keyword(tokens[index].source), "Cannot use a keyword as member variable name.");
			raise_syntax_error_if_not(!is_name_taken(tokens[index].source, str.member_variables, p.program), "More than one member variable with the same name.");
			PooledString const name = pool_string(p.program, tokens[index].source);
			add_variable_to_scope(str.member_variables, new_type.size, new_type.alignment, name, member_type, 0, p.program);
			index++;

			// Initialization expression.
			if (tokens[index].source == "=")
			{
				index++;
				str.member_variables.back().initializer_expression = parse_subexpression(tokens, index, p);
			}
			else
			{
				// If the type is default constructible synthesize an initializer expression from the default constructor.
				Type const & type_data = type_with_id(p.program, member_type);
				if (is_struct(type_data))
				{
					Struct const & struct_data = *struct_for_type(p.program, type_data);

					if (is_default_constructible(struct_data))
						str.member_variables.back().initializer_expression = synthesize_default_constructor(member_type, struct_data);
				}
			}

			raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "");
			index++;
		}

		// Skip } token.
		index++;

		new_type.extra_data = StructType{static_cast<int>(p.program.structs.size())};
		TypeId const new_type_id = add_type(p.program, std::move(new_type));
		p.program.structs.push_back(std::move(str));
		top(p.scope_stack).types.push_back({type_name, new_type_id});

		return std::nullopt;
	}

	auto parse_substatement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> std::optional<stmt::Statement>
	{
		std::optional<stmt::Statement> result;

		if (tokens[index].source == "let")
			result = parse_let_statement(tokens, index, p);
		else if (tokens[index].source == "return")
			result = parse_return_statement(tokens, index, p);
		// With if, {}, while and for statements, return to avoid checking for final ';' because it is not needed.
		else if (tokens[index].source == "if")
			return parse_if_statement(tokens, index, p);
		else if (tokens[index].type == TokenType::open_brace)
			return parse_statement_block(tokens, index, p);
		else if (tokens[index].source == "while")
			return parse_while_statement(tokens, index, p);
		else if (tokens[index].source == "for")
			return parse_for_statement(tokens, index, p);
		else if (tokens[index].source == "break")
			result = parse_break_or_continue_statement<stmt::BreakStatement>(tokens, index, p);
		else if (tokens[index].source == "continue")
			result = parse_break_or_continue_statement<stmt::ContinueStatement>(tokens, index, p);
		else if (tokens[index].source == "struct")
			return parse_struct_declaration(tokens, index, p);
		else if (lookup_type_name(p.scope_stack, tokens[index].source, p.program.string_pool) != TypeId::none)
			result = parse_variable_declaration_statement(tokens, index, p);
		else
			result = parse_expression_statement(tokens, index, p);

		// A statement must end with a semicolon.
		raise_syntax_error_if_not(tokens[index].type == TokenType::semicolon, "Expected ';' after statement.");
		index++;

		return result;
	}

	auto parse_statement(span<lex::Token const> tokens, ParseParams p) noexcept -> std::optional<stmt::Statement>
	{
		size_t index = 0;
		return parse_substatement(tokens, index, p);
	}

	auto parse_source(std::string_view src) noexcept -> Program
	{
		auto const tokens = lex::tokenize(src);
		Program program;

		ScopeStack scope_stack;
		scope_stack.push_back({&program.global_scope, ScopeType::global});

		size_t index = 0;
		while (index < tokens.size())
		{
			TypeId global_return_type = TypeId::none;
			auto statement = parse_substatement(tokens, index, {program, scope_stack, global_return_type});
			if (statement)
			{
				raise_syntax_error_if_not(!has_type<stmt::ExpressionStatement>(*statement), "An expression statement is not allowed at the global scope.");
				program.global_initialization_statements.push_back(std::move(*statement));
			}
		}

		return program;
	}

} //namespace parser
