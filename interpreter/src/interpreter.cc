#include "interpreter.hh"
#include "parser.hh"
#include "unreachable.hh"
#include "overload.hh"
#include <cassert>

namespace interpreter
{

	auto eval_expression_tree(parser::ExpressionTree const & tree, ExecutionContext const & context) noexcept -> int
	{
		auto const visitor = overload(
			[](int literal_value) { return literal_value; },
			[&](parser::OperatorNode const & op_node) 
			{
				int const lval = eval_expression_tree(*op_node.left, context);
				int const rval = eval_expression_tree(*op_node.right, context);
				switch (op_node.op)
				{
					case parser::Operator::add:		 return lval + rval;
					case parser::Operator::subtract: return lval - rval;
					case parser::Operator::multiply: return lval * rval;
					case parser::Operator::divide:	 return lval / rval;
				}
				declare_unreachable();
			},
			[&](parser::VariableNode const & var_node)
			{
				auto const it = std::find_if(context.variables.begin(), context.variables.end(), 
					[=](ExecutionContext::Variable const & var) { return var.name == var_node.variable_name; });
				assert(it != context.variables.end());
				return it->value;
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto run_statement_tree(parser::StatementTree const & tree, ExecutionContext & context) noexcept -> void
	{
		ExecutionContext::Variable new_variable;
		new_variable.name = tree.variable_name;
		new_variable.value = eval_expression_tree(tree.assigned_expression, context);
		context.variables.push_back(new_variable);
	}

}

