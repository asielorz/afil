#include "expression.hh"
#include "statement.hh"
#include "program.hh"
#include "overload.hh"
#include "unreachable.hh"
#include "string.hh"

namespace expr
{

	auto precedence(Operator op) noexcept -> int
	{
		constexpr int precedences[] = { 6, 6, 7, 7, 3, 3, 4, 4, 4, 4, 5, 2, 0, 1, -1 };
		return precedences[static_cast<int>(op)];
	}

	auto operator_function_name(Operator op) noexcept -> std::string_view
	{
		switch (op)
		{
			case Operator::add:					return "operator+";
			case Operator::subtract:			return "operator-";
			case Operator::multiply:			return "operator*";
			case Operator::divide:				return "operator/";
			case Operator::equal:				return "operator==";
			case Operator::not_equal:			return "operator==";
			case Operator::less:				return "operator<=>";
			case Operator::less_equal:			return "operator<=>";
			case Operator::greater:				return "operator<=>";
			case Operator::greater_equal:		return "operator<=>";
			case Operator::three_way_compare:	return "operator<=>";
			case Operator::and_:				return "operator and";
			case Operator::or_:					return "operator or";
			case Operator::xor_:				return "operator xor";
			case Operator::assign:				return "operator=";
		}
		declare_unreachable();
	}

	auto is_operator_node(OperatorTree const & tree) noexcept -> bool
	{
		return tree.index() == 0;
	}

	auto expression_type(ExpressionTree const & tree, Program const & program) noexcept -> Type
	{
		return type_with_id(program, expression_type_id(tree, program));
	}

	auto expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> TypeId
	{
		auto const visitor = overload(
			[](Literal<int>) { return TypeId::int_; },
			[](Literal<float>) { return TypeId::float_; },
			[](Literal<bool>) { return TypeId::bool_; },
			[](DereferenceNode const & deref_node) { return deref_node.variable_type; },
			[](LocalVariableNode const & var_node) { return var_node.variable_type; },
			[](GlobalVariableNode const & var_node) { return var_node.variable_type; },
			[](FunctionNode const &) { return TypeId::function; },
			[&](FunctionCallNode const & func_call_node) { return return_type(program, func_call_node.function_id); },
			[](RelationalOperatorCallNode const &) { return TypeId::bool_; },
			[&](IfNode const & if_node) { return expression_type_id(*if_node.then_case, program); },
			[](StatementBlockNode const & block_node) { return block_node.return_type; }
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto to_string(TypeId id, Program const & program) noexcept -> std::string
	{
		Type const & type = type_with_id(program, id);
		auto type_name = std::string(type.name);
		if (id.is_mutable)
			type_name += " mut";
		if (id.is_reference)
			type_name += " &";
		return type_name;
	}

	auto indent(int indentation_level) noexcept -> std::string
	{
		return std::string(indentation_level, '\t');
	}

	auto pretty_print_function(FunctionId function_id, Program const & program) noexcept -> std::string
	{
		auto const found_id = std::find_if(program.global_scope.functions.begin(), program.global_scope.functions.end(),
			[function_id](FunctionName const & fn_name) { return fn_name.id == function_id; });
		
		std::string str;

		if (found_id == program.global_scope.functions.end())
			str = "function(";
		else
			str = std::string(found_id->name) + '(';

		auto const param_types = parameter_types(program, function_id);
		for (TypeId const & id : param_types)
			str += to_string(id, program) + (&id == &param_types.back() ? "" : ", ");
		str += ") -> ";
		str += to_string(return_type(program, function_id), program) + '\n';
		return str;
	}

	auto pretty_print_rec(ExpressionTree const & tree, Program const & program, int indentation_level) noexcept -> std::string
	{
		auto const visitor = overload(
			[&](Literal<int> literal) { return join(indent(indentation_level), "literal<int>: ", literal.value, '\n'); },
			[&](Literal<float> literal) { return join(indent(indentation_level), "literal<float>: ", literal.value, '\n'); },
			[&](Literal<bool> literal) { return join(indent(indentation_level), "literal<bool>: ", literal.value, '\n'); },
			[&](DereferenceNode const & deref_node) 
			{
				return join(
					indent(indentation_level), "dereference<", to_string(deref_node.variable_type, program), ">\n",
					pretty_print_rec(*deref_node.expression, program, indentation_level + 1));
			},
			[&](LocalVariableNode const & var_node) 
			{
				return join(indent(indentation_level), "local<", to_string(var_node.variable_type, program), ">: ", var_node.variable_offset, '\n');
			},
			[&](GlobalVariableNode const & var_node) 
			{
				auto const found_id = std::find_if(program.global_scope.variables.begin(), program.global_scope.variables.end(),
					[var_node](Variable const & var) { return var.offset == var_node.variable_offset; });

				return join(indent(indentation_level), "global<", to_string(var_node.variable_type, program), ">: ", found_id->name, '\n');
			},
			[&](FunctionNode const & func_node) 
			{ 
				return join(indent(indentation_level), pretty_print_function(func_node.function_id, program));
			},
			[&](FunctionCallNode const & func_call_node)
			{
				std::string str = join(indent(indentation_level), pretty_print_function(func_call_node.function_id, program));
				
				for (ExpressionTree const & param : func_call_node.parameters)
					str += pretty_print_rec(param, program, indentation_level + 1);

				return str;
			},
			[&](RelationalOperatorCallNode const & rel_op_node) 
			{
				std::string str = indent(indentation_level);
				switch (rel_op_node.op)
				{
					case Operator::less:			str += "operator < ";  break;
					case Operator::less_equal:		str += "operator <= "; break;
					case Operator::greater:			str += "operator > ";  break;
					case Operator::greater_equal:	str += "operator >= "; break;
					case Operator::not_equal:		str += "operator != "; break;
				}
				str += '\n';
				str += pretty_print_rec((*rel_op_node.parameters)[0], program, indentation_level + 1);
				str += pretty_print_rec((*rel_op_node.parameters)[1], program, indentation_level + 1);
				return str;
			},
			[&](IfNode const & if_node)
			{
				return join(
					indent(indentation_level), "if node\n",
					pretty_print_rec(*if_node.condition, program, indentation_level + 1),
					pretty_print_rec(*if_node.then_case, program, indentation_level + 1),
					pretty_print_rec(*if_node.else_case, program, indentation_level + 1)
				);
			},
			[&](StatementBlockNode const & block_node)
			{
				return join(indent(indentation_level), "{ ", to_string(block_node.return_type, program) ," }"); 
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto pretty_print(ExpressionTree const & tree, Program const & program) noexcept -> std::string
	{
		return pretty_print_rec(tree, program, 0);
	}

} // namespace expr
