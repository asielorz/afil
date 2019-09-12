#include "interpreter.hh"
#include "parser.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "program.hh"
#include <cassert>

namespace interpreter
{

	auto read_word(ProgramStack const & stack, int address) noexcept -> int
	{
		return read<int>(stack, address);
	}

	auto write_word(ProgramStack & stack, int address, int value) noexcept -> void
	{
		write(stack, address, value);
	}

	auto alloc_stack(ProgramStack & stack, int stack_size_in_bytes) noexcept -> void
	{
		stack.memory.resize(stack_size_in_bytes);
	}

	auto eval_expression_tree(parser::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int
	{
		auto const visitor = overload(
			[](int literal_value) { return literal_value; },
			[](float literal_value) { return (int)literal_value; }, // TODO
			[&](parser::OperatorNode const & op_node) 
			{
				int const lval = eval_expression_tree(*op_node.left, stack, program);
				int const rval = eval_expression_tree(*op_node.right, stack, program);
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
				int const address = stack.base_pointer + var_node.variable_offset;
				return read_word(stack, address);
			},
			[](parser::FunctionNode const & func_node)
			{
				return func_node.function_id; // TODO
			},
			[&](parser::FunctionCallNode const & func_call_node)
			{
				Function const & func = program.functions[func_call_node.function_id];

				int const parameter_size = func.parameter_count * sizeof(int); // TODO: Parameter size for different types

				// Push stack pointer to the end of parameters. This is to avoid that the expressions that compute
				// the parameters of the function overwrite the memory reserved for the parameters.
				int const ebp_address = stack.top_pointer;
				int const parameters_start = ebp_address + sizeof(int);
				stack.top_pointer += parameter_size;

				// Evaluate the expressions that yield the parameters of the function.
				for (int i = 0; i < func_call_node.parameters.size(); ++i)
					write_word(stack, parameters_start + i * sizeof(int), eval_expression_tree(func_call_node.parameters[i], stack, program));

				// Write esp to the stack so that we can return to our stack frame when the function ends.
				write_word(stack, ebp_address, stack.base_pointer);

				// Move the stack pointers.
				stack.base_pointer = parameters_start;
				stack.top_pointer = parameters_start + func.stack_frame_size;

				// Run the function.
				for (auto const & statement : func.statements)
					if (run_statement_tree(statement, stack, program))
						break;

				// Read the return value from the stack.
				return read_word(stack, stack.top_pointer);
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto run_statement_tree(parser::StatementTree const & tree, ProgramStack & stack, Program const & program) noexcept -> bool
	{
		auto const visitor = overload(
			[&](parser::VariableDeclarationStatementNode const & node) 
			{
				int const address = stack.base_pointer + node.variable_offset;
				int const value = eval_expression_tree(node.assigned_expression, stack, program);
				write_word(stack, address, value);
				return false;
			},
			[&](parser::ReturnStatementNode const & var_node)
			{
				// Read the previous ebp from the stack.
				int const prev_ebp_address = stack.base_pointer - 4;
				int const prev_ebp = read_word(stack, prev_ebp_address);
				int const return_value = eval_expression_tree(var_node.returned_expression, stack, program);
				write_word(stack, prev_ebp_address, return_value);
				stack.top_pointer = prev_ebp_address;
				stack.base_pointer = prev_ebp;
				return true; // Return true to indicate that the function should end.
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

}
