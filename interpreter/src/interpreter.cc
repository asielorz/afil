#include "interpreter.hh"
#include "parser.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "program.hh"
#include <cassert>

namespace interpreter
{

	auto read_word(ExecutionContext const & context, int address) noexcept -> int
	{
		return reinterpret_cast<int const &>(context.stack[address]);
	}

	auto write_word(ExecutionContext & context, int address, int value) noexcept -> void
	{
		reinterpret_cast<int &>(context.stack[address]) = value;
	}

	auto alloc_stack(ExecutionContext & context, int stack_size_in_bytes) noexcept -> void
	{
		context.stack.resize(stack_size_in_bytes);
	}

	auto eval_expression_tree(parser::ExpressionTree const & tree, ExecutionContext & context, Program const & program) noexcept -> int
	{
		auto const visitor = overload(
			[](int literal_value) { return literal_value; },
			[&](parser::OperatorNode const & op_node) 
			{
				int const lval = eval_expression_tree(*op_node.left, context, program);
				int const rval = eval_expression_tree(*op_node.right, context, program);
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
				int const address = context.stack_base_pointer + var_node.variable_offset;
				return read_word(context, address);
			},
			[](parser::FunctionNode const & func_node)
			{
				return func_node.function_id; // TODO
			},
			[&](parser::FunctionCallNode const & func_call_node)
			{
				Function const & func = program.functions[func_call_node.function_id];

				// Evaluate the expressions that yield the parameters of the function.
				int parameters[32];
				for (int i = 0; i < func_call_node.parameters.size(); ++i)
					parameters[i] = eval_expression_tree(func_call_node.parameters[i], context, program);

				// Write current ebp to the stack right after our stack frame.
				const int THIS_IS_A_HACK = program.global_scope.stack_frame_size;
				write_word(context, context.stack_base_pointer + THIS_IS_A_HACK, context.stack_base_pointer);
				// Write parameters of the function.
				for (int i = 0; i < func_call_node.parameters.size(); ++i)
					write_word(context, context.stack_base_pointer + THIS_IS_A_HACK + (i + 1) * sizeof(int), parameters[i]);
				context.stack_base_pointer += THIS_IS_A_HACK + sizeof(int);

				for (auto const & statement : func.statements)
					if (run_statement_tree(statement, context, program))
						break;

				return read_word(context, context.stack_base_pointer + THIS_IS_A_HACK);
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto run_statement_tree(parser::StatementTree const & tree, ExecutionContext & context, Program const & program) noexcept -> bool
	{
		auto const visitor = overload(
			[&](parser::VariableDeclarationStatementNode const & node) 
			{
				int const address = context.stack_base_pointer + node.variable_offset;
				int const value = eval_expression_tree(node.assigned_expression, context, program);
				write_word(context, address, value);
				return false;
			},
			[&](parser::ReturnStatementNode const & var_node)
			{
				// Read the previous ebp from the stack.
				int const prev_ebp = read_word(context, context.stack_base_pointer - 4);
				int const return_value = eval_expression_tree(var_node.returned_expression, context, program);
				write_word(context, context.stack_base_pointer - 4, return_value);
				context.stack_base_pointer = prev_ebp;
				return true; // Return true to indicate that the function should end.
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

}
