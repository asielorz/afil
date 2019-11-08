#include "statement.hh"
#include "overload.hh"

namespace stmt
{
	auto is_dependent(Statement const & statement) noexcept -> bool
	{
		auto const visitor = overload(
			[&](VariableDeclarationStatement const & var_decl_node)
			{
				return is_dependent(var_decl_node.assigned_expression);
			},
			[&](ExpressionStatement const & expr_node)
			{
				return is_dependent(expr_node.expression);
			},
			[&](ReturnStatement const & return_node)
			{
				return is_dependent(return_node.returned_expression);
			},
			[&](IfStatement const & if_node)
			{
				return is_dependent(if_node.condition) || is_dependent(*if_node.then_case) || is_dependent(*if_node.else_case);

			},
			[&](StatementBlock const & block_node)
			{
				static_cast<void>(block_node); // TODO
				return false;
			},
			[&](WhileStatement const & while_node)
			{
				return is_dependent(while_node.condition) || is_dependent(*while_node.body);
			},
			[&](ForStatement const & for_node)
			{
				static_cast<void>(for_node); // TODO
				return false;
			},
			[](BreakStatement const &)
			{
				return false;
			},
			[](ContinueStatement const &)
			{
				return false;
			},
			[](tmp::DependentNode const &)
			{
				return true;
			}
		);
		return std::visit(visitor, statement.as_variant());
	}

}
