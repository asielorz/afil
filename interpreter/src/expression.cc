#include "expression.hh"
#include "program.hh"
#include "overload.hh"
#include "unreachable.hh"

namespace expr
{

	auto precedence(Operator op) noexcept -> int
	{
		constexpr int precedences[] = { 6, 6, 7, 7, 3, 3, 4, 4, 4, 4, 5, 2, 0, 1 };
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
			[](LocalVariableNode const & var_node) { return var_node.variable_type; },
			[](GlobalVariableNode const & var_node) { return var_node.variable_type; },
			[](FunctionNode const &) { return TypeId::function; },
			[&](FunctionCallNode const & func_call_node)
			{
				if (func_call_node.function_id.is_extern)
					return program.extern_functions[func_call_node.function_id.index].return_type;
				else
					return program.functions[func_call_node.function_id.index].return_type;
			},
			[](RelationalOperatorCallNode const &) { return TypeId::bool_; },
			[&](IfNode const & if_node) { return expression_type_id(*if_node.then_case, program); },
			[](ReturnNode const &) { return TypeId::noreturn; }
		);
		return std::visit(visitor, tree.as_variant());
	}

} // namespace expr
