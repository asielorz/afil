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
		switch (op)
		{
			case Operator::add:					return 6;
			case Operator::subtract:			return 6;
			case Operator::multiply:			return 7;
			case Operator::divide:				return 7;
			case Operator::modulo:				return 7;
			case Operator::equal:				return 3;
			case Operator::not_equal:			return 3;
			case Operator::less:				return 4;
			case Operator::less_equal:			return 4;
			case Operator::greater:				return 4;
			case Operator::greater_equal:		return 4;
			case Operator::three_way_compare:	return 5;
			case Operator::and_:				return 2;
			case Operator::or_:					return 0;
			case Operator::xor_:				return 1;
			case Operator::not:					return 2;
			case Operator::assign:				return -1;
		}
		declare_unreachable();
	}

	auto operator_function_name(Operator op) noexcept -> std::string_view
	{
		switch (op)
		{
			case Operator::add:					return "+";
			case Operator::subtract:			return "-";
			case Operator::multiply:			return "*";
			case Operator::divide:				return "/";
			case Operator::modulo:				return "%";
			case Operator::equal:				return "==";
			case Operator::not_equal:			return "==";
			case Operator::less:				return "<=>";
			case Operator::less_equal:			return "<=>";
			case Operator::greater:				return "<=>";
			case Operator::greater_equal:		return "<=>";
			case Operator::three_way_compare:	return "<=>";
			case Operator::and_:				return "and";
			case Operator::or_:					return "or";
			case Operator::xor_:				return "xor";
			case Operator::not:					return "not";
			case Operator::assign:				return "=";
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
			[](VariableNode const & var_node) { return make_reference(var_node.variable_type); },
			[](MemberVariableNode const & var_node) { return var_node.variable_type; },
			[](FunctionNode const &) { return TypeId::function; },
			[&](FunctionCallNode const & func_call_node) { return return_type(program, func_call_node.function_id); },
			[](RelationalOperatorCallNode const &) { return TypeId::bool_; },
			[&](IfNode const & if_node) { return expression_type_id(*if_node.then_case, program); },
			[](StatementBlockNode const & block_node) { return block_node.return_type; },
			[](StructConstructorNode const & constructor_node) { return constructor_node.constructed_type; }
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto expression_type_size(ExpressionTree const & tree, Program const & program) noexcept -> int
	{
		return type_size(program, expression_type_id(tree, program));
	}

} // namespace expr
