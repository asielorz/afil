#include "parser.hh"
#include "lexer.hh"
#include "program.hh"
#include "out.hh"
#include "span.hh"
#include "unreachable.hh"
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

			expr::DereferenceNode deref_node;
			deref_node.expression = std::make_unique<ExpressionTree>(std::move(tree));
			deref_node.variable_type = to;
			return deref_node;
		}
		else if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) && !from.is_reference && !to.is_reference)
		{
			return std::move(tree);
		}
		declare_unreachable();
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
		span<FunctionId const> overload_set,
		span<ExpressionTree> parameters,
		span<TypeId const> parsed_parameter_types,
		Program const & program) noexcept -> FunctionId
	{
		FunctionId const function_id = resolve_function_overloading(overload_set, parsed_parameter_types, program);
		assert(function_id != invalid_function_id);

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

	auto resolve_operator_overloading(OperatorTree tree, Program const & program, ScopeStack const & scope_stack) noexcept -> ExpressionTree
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
				[](auto) -> ExpressionTree { declare_unreachable(); },
				[&](lookup_result::OverloadSet const & overload_set) -> ExpressionTree
				{
					TypeId const operand_types[] = {expression_type_id(left, program), expression_type_id(right, program)};
					auto const function_id = resolve_function_overloading_and_insert_conversions(overload_set.function_ids, operands, operand_types, program);
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

			auto const lookup = lookup_name(scope_stack, operator_function_name(op), program.string_pool);
			return std::visit(visitor, lookup);
		}
		else
			return std::move(std::get<ExpressionTree>(tree));
	}

	template <typename T>
	auto add_variable_to_scope(
		std::vector<T> & variables, int & scope_size, int & scope_alignment, 
		PooledString name, TypeId type_id, int scope_offset, Program const & program) -> int
	{
		Type const & type = type_with_id(program, type_id);
		int const size = type_id.is_reference ? sizeof(void *) : type.size;
		int const alignment = type_id.is_reference ? alignof(void *) : type.alignment;

		T var;
		var.name = name;
		var.type = type_id;
		var.offset = scope_offset + align(scope_size, alignment);
		scope_size = var.offset + size;
		scope_alignment = std::max(scope_alignment, alignment);
		variables.push_back(std::move(var));
		return var.offset;
	}

	auto add_variable_to_scope(Scope & scope, PooledString name, TypeId type_id, int scope_offset, Program const & program) -> int
	{
		return add_variable_to_scope(scope.variables, scope.stack_frame_size, scope.stack_frame_alignment, name, type_id, scope_offset, program);
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

	auto parse_type_name(span<lex::Token const> tokens, size_t & index, ScopeStackView scope_stack, Program & program) noexcept -> TypeId
	{
		// Lookup type name.
		TypeId type_found = lookup_type_name(scope_stack, tokens[index].source, program.string_pool);
		index++;
		assert(type_found != TypeId::none);

		type_found = parse_mutable_and_pointer(tokens, index, type_found, program);

		// Look for reference qualifier.
		if (tokens[index].source == "&"sv)
		{
			type_found.is_reference = true;
			index++;
		}

		return type_found;
	}

	auto parse_function_prototype(span<lex::Token const> tokens, size_t & index, ParseParams p, Function & function) noexcept -> void
	{
		// Parameters must be between parenthesis.
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		// Parse arguments.
		while (tokens[index].type == TokenType::identifier)
		{
			TypeId const arg_type = parse_type_name(tokens, index, p.scope_stack, p.program);
			assert(is_data_type(arg_type)); // An argument cannot be void

			add_variable_to_scope(function, pool_string(p.program, tokens[index].source), arg_type, 0, p.program);
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
		assert(tokens[index].type == TokenType::open_brace);
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

	auto parse_function_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> expr::FunctionNode
	{
		// Skip fn token.
		index++;

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

		// Parameter list starts with (
		assert(tokens[index].type == TokenType::open_parenthesis);
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
				declare_unreachable();
		}

		return parsed_expressions;
	}

	auto parse_function_call_expression(span<lex::Token const> tokens, size_t & index, ParseParams p, span<FunctionId const> overload_set) noexcept -> expr::FunctionCallNode
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
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		for (;;)
		{
			// A member initializer by name starts with a period.
			assert(tokens[index].type == TokenType::period);
			index++;

			// Find the member to initialize.
			assert(tokens[index].type == TokenType::identifier);
			std::string_view const member_name = tokens[index].source;
			index++;

			int const member_variable_index = find_member_variable(struct_data, member_name, p.program.string_pool);
			assert(member_variable_index != -1);
			assert(!expression_initialized[member_variable_index]); // Avoid initializing the same variable twice.

			// Next token must be =
			assert(tokens[index].source == "=");
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
				declare_unreachable();
		}

		// For any value that is not given an initializer, use the default if available or fail to parse.
		for (size_t i = 0; i < parsed_expressions.size(); ++i)
		{
			if (!expression_initialized[i])
			{
				MemberVariable const & variable = struct_data.member_variables[i];
				assert(variable.initializer_expression.has_value());
				parsed_expressions[i] = *variable.initializer_expression;
			}
		}
		
		return parsed_expressions;
	}

	auto parse_struct_constructor_expression(span<lex::Token const> tokens, size_t & index, ParseParams p, TypeId type_id) noexcept -> expr::StructConstructorNode
	{
		Type const & type = type_with_id(p.program, type_id);
		assert(is_struct(type)); // Type must be a struct (by now).
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
				assert(var.initializer_expression.has_value());
				node.parameters.push_back(*var.initializer_expression);
			}
		}

		assert(struct_member_types.size() == node.parameters.size());
		for (size_t i = 0; i < node.parameters.size(); ++i)
		{
			TypeId const parsed_type = expression_type_id(node.parameters[i], p.program);
			assert(is_convertible(parsed_type, struct_member_types[i], p.program));
			node.parameters[i] = insert_conversion_node(std::move(node.parameters[i]), parsed_type, struct_member_types[i], p.program);
		}

		return node;
	}

	auto parse_if_expression(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> expr::IfNode
	{
		// Skip if token
		index++;

		// Condition goes between parenthesis.
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		expr::IfNode if_node;
		if_node.condition = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));
		assert(expression_type_id(*if_node.condition, p.program) == TypeId::bool_);

		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		if_node.then_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));

		// Expect keyword else to separate then and else cases.
		assert(tokens[index].source == "else");
		index++;

		if_node.else_case = std::make_unique<ExpressionTree>(parse_subexpression(tokens, index, p));

		// Ensure that both branches return the same type.
		TypeId const then_type = expression_type_id(*if_node.then_case, p.program);
		TypeId const else_type = expression_type_id(*if_node.else_case, p.program);
		TypeId const common = common_type(then_type, else_type, p.program);
		assert(common != TypeId::none);
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
		assert(all_branches_return(node.statements.back(), p.program));

		// Skip closing brace.
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
			assert(operand_type.is_reference);
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
				[](auto) -> expr::FunctionCallNode { declare_unreachable(); },
				[&](lookup_result::OverloadSet const & overload_set) -> expr::FunctionCallNode
				{
					TypeId const operand_type = expression_type_id(operand, p.program);
					auto const function_id = resolve_function_overloading_and_insert_conversions(overload_set.function_ids, { &operand, 1 }, { &operand_type, 1 }, p.program);
					if (function_id != invalid_function_id)
					{
						expr::FunctionCallNode func_node;
						func_node.function_id = function_id;
						func_node.parameters.push_back(std::move(operand));
						return func_node;
					}
					else declare_unreachable();
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
			auto const func_node = parse_function_expression(tokens, index, p);
			// Opening parenthesis after a function means a function call.
			if (tokens[index].type == TokenType::open_parenthesis)
				return parse_function_call_expression(tokens, index, p, span<FunctionId const>(&func_node.function_id, 1));
			else
				return func_node;
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
						return parse_function_call_expression(tokens, index, p, result.function_ids);
					else
						return expr::FunctionNode{result.function_ids[0]}; // TODO: Overload set node?
				},
				[&](lookup_result::Type result) -> ExpressionTree 
				{
					return parse_struct_constructor_expression(tokens, index, p, result.type_id);
				},
				[](lookup_result::Nothing) -> ExpressionTree { declare_unreachable(); }
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
			assert(tokens[index].type == TokenType::close_parenthesis);
			index++;

			return expr;
		}
		else if (is_unary_operator(tokens[index]))
		{
			return parse_unary_operator(tokens, index, p);
		}
		else declare_unreachable(); // TODO: Actual error handling.
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
				assert(is_struct(last_operand_type));
				Struct const & last_operand_struct = *struct_for_type(p.program, last_operand_type);

				assert(tokens[index].type == TokenType::identifier);
				std::string_view const member_name = tokens[index].source;
				int const member_variable_index = find_member_variable(last_operand_struct, member_name, p.program.string_pool);
				assert(member_variable_index != -1);
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

	auto parse_variable_declaration_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::VariableDeclarationStatement
	{
		// A variable declaration statement has the following form:
		// [type] [var_name] = [expr];

		stmt::VariableDeclarationStatement node;

		// A statement begins with the type of the declared variable.
		TypeId const type_found = parse_type_name(tokens, index, p.scope_stack, p.program);

		// The second token of the statement is the variable name.
		assert(tokens[index].type == TokenType::identifier);
		node.variable_offset = add_variable_to_scope(top(p.scope_stack), pool_string(p.program, tokens[index].source), type_found, local_variable_offset(p.scope_stack), p.program);
		index++;

		// The third token is a '='.
		assert(tokens[index].source == "=");
		index++;

		// The rest is the expression assigned to the variable.
		node.assigned_expression = parse_subexpression(tokens, index, p);
		TypeId const assigned_type = expression_type_id(node.assigned_expression, p.program);
		// Require that the expression assigned to the variable is convertible to the type of the variable.
		assert(is_convertible(assigned_type, type_found, p.program));
		if (assigned_type != type_found)
			node.assigned_expression = insert_conversion_node(std::move(node.assigned_expression), assigned_type, type_found, p.program);

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
		assert(id_token.type == TokenType::identifier);
		index++;
		assert(tokens[index].source == "=");
		index++;

		// Skip the fn.
		index++;

		// Adding the function to the program before parsing the body allows the body of the function to find itself and be recursive.

		// Add a new function to the program.
		Function & function = p.program.functions.emplace_back();
		// Add the function name to the scope.
		auto const function_id = FunctionId{0, static_cast<unsigned>(p.program.functions.size() - 1)};
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
				assert(tokens[index].type == TokenType::operator_);
				name = tokens[index].source;
				index++;
				assert(tokens[index].type == TokenType::close_parenthesis);
				index++;

				is_operator = true;
			}
			else
			{
				assert(tokens[index].type == TokenType::identifier);
				name = tokens[index].source;
				index++;
			}

			assert(tokens[index].source == "=");
			index++;

			// If the expression returns a function, bind it to its name and return a noop.
			expr::ExpressionTree expression = parse_subexpression(tokens, index, p);
			TypeId const expr_type = expression_type_id(expression, p.program);
			
			if (expr_type == TypeId::function)
			{
				assert(!is_mutable); // A function cannot be mutable.
				FunctionId const function_id = std::get<expr::FunctionNode>(expression).function_id;
				bind_function_name(name, function_id, p.program, top(p.scope_stack));
				return std::nullopt;
			}
			else
			{
				assert(is_data_type(expr_type));
				assert(!is_operator);
				TypeId const var_type = is_mutable ? make_mutable(decay(expr_type)) : decay(expr_type);
				stmt::VariableDeclarationStatement node;
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
		assert(p.current_return_type != TypeId::none);

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
		assert(is_data_type(expression_type_id(node.returned_expression, p.program)));

		return node;
	}

	auto parse_if_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) noexcept -> stmt::IfStatement
	{
		// Skip the if
		index++;

		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		stmt::IfStatement node;
		node.condition = parse_subexpression(tokens, index, p);
		assert(expression_type_id(node.condition, p.program) == TypeId::bool_);

		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		auto then_case = parse_substatement(tokens, index, p);
		assert(then_case.has_value()); // Do not allow no-ops as cases of ifs.
		node.then_case = std::make_unique<stmt::Statement>(*std::move(then_case));

		// For if statement else is optional.
		if (tokens[index].source == "else")
		{
			// Skip else token.
			index++;
			auto else_case = parse_substatement(tokens, index, p);
			assert(else_case.has_value());  // Do not allow no-ops as cases of ifs.
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
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		stmt::WhileStatement node;

		// Parse condition. Must return bool.
		node.condition = parse_subexpression(tokens, index, p);
		assert(expression_type_id(node.condition, p.program) == TypeId::bool_);

		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		// Parse body
		auto body = parse_substatement(tokens, index, p);
		assert(body.has_value()); // Do not allow no-ops as bodies of whiles.
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
		assert(tokens[index].type == TokenType::open_parenthesis);
		index++;

		stmt::ForStatement for_node;
		p.scope_stack.push_back({&for_node.scope, ScopeType::block});

		// Parse init statement. Must be an expression or a declaration.
		auto init_statement = parse_substatement(tokens, index, p);
		assert(init_statement.has_value() && (has_type<stmt::VariableDeclarationStatement>(*init_statement) || has_type<stmt::ExpressionStatement>(*init_statement)));
		for_node.init_statement = std::make_unique<stmt::Statement>(std::move(*init_statement));

		// Parse condition. Must return bool.
		for_node.condition = parse_subexpression(tokens, index, p);
		assert(expression_type_id(for_node.condition, p.program) == TypeId::bool_);

		// Parse ; after condition.
		assert(tokens[index].type == TokenType::semicolon);
		index++;

		// Parse end expression.
		for_node.end_expression = parse_subexpression(tokens, index, p);

		assert(tokens[index].type == TokenType::close_parenthesis);
		index++;

		// Parse body
		auto body = parse_substatement(tokens, index, p);
		assert(body.has_value()); // Do not allow no-ops as bodies of whiles.
		for_node.body = std::make_unique<stmt::Statement>(std::move(*body));

		p.scope_stack.pop_back();

		return for_node;
	}

	template <typename Stmt>
	auto parse_break_or_continue_statement(span<lex::Token const> tokens, size_t & index, ParseParams p) -> Stmt
	{
		// Should check if parsing a loop and otherwise give an error.

		static_cast<void>(tokens, p);
		index++;
		return Stmt();
	}

	auto parse_struct_declaration(span<lex::Token const> tokens, size_t & index, ParseParams p) -> std::nullopt_t
	{
		// Skip struct token.
		index++;

		// Parse name
		assert(tokens[index].type == TokenType::identifier);
		PooledString const type_name = pool_string(p.program, tokens[index].source);
		index++;

		Type new_type;
		new_type.size = 0;
		new_type.alignment = 1;

		// Skip { token.
		assert(tokens[index].type == TokenType::open_brace);
		index++;

		Struct str;
		// Parse member variables.
		while (tokens[index].type != TokenType::close_brace)
		{
			TypeId const type = parse_type_name(tokens, index, p.scope_stack, p.program);
			assert(is_data_type(type));
			assert(!type.is_reference);
			
			assert(tokens[index].type == TokenType::identifier);
			PooledString const name = pool_string(p.program, tokens[index].source);
			add_variable_to_scope(str.member_variables, new_type.size, new_type.alignment, name, type, 0, p.program);
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
				Type const & type_data = type_with_id(p.program, type);
				if (is_struct(type_data))
				{
					Struct const & struct_data = *struct_for_type(p.program, type_data);

					if (std::all_of(struct_data.member_variables.begin(), struct_data.member_variables.end(),
						[](MemberVariable const & var) { return var.initializer_expression.has_value(); }))
					{
						expr::StructConstructorNode default_constructor_node;
						default_constructor_node.constructed_type = type;
						default_constructor_node.parameters.reserve(struct_data.member_variables.size());
						for (MemberVariable const & var : struct_data.member_variables)
							default_constructor_node.parameters.push_back(*var.initializer_expression);

						str.member_variables.back().initializer_expression = std::move(default_constructor_node);
					}
				}
			}

			assert(tokens[index].type == TokenType::semicolon);
			index++;
		}

		// Skip } token.
		assert(tokens[index].type == TokenType::close_brace);
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
		assert(tokens[index].type == TokenType::semicolon);
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
				assert(!has_type<stmt::ExpressionStatement>(*statement)); // An expression statement is not allowed at the global scope.
				program.global_initialization_statements.push_back(std::move(*statement));
			}
		}

		return program;
	}

} //namespace parser
