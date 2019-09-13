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

	// TODO: Alignment
	auto alloc(ProgramStack & stack, int size) noexcept -> int
	{
		int const address = stack.top_pointer;
		stack.top_pointer += size;
		return address;
	}

	auto free_up_to(ProgramStack & stack, int address) noexcept -> void
	{
		stack.top_pointer = address;
	}

	template <typename T>
	auto push_to_stack_top(ProgramStack & stack, T const & value) noexcept -> int
	{
		int const address = alloc_in_stack(stack, sizeof(T));
		write(stack, address, value);
		return address;
	}

	auto eval_expression_tree(parser::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int
	{
		int const address = alloc(stack, expression_type(tree, program).size);
		eval_expression_tree(tree, stack, program, address);
		return address;
	}

	auto eval_expression_tree(parser::ExpressionTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> void
	{
		auto const visitor = overload(
			[&](int literal_value) { write(stack, return_address, literal_value); },
			[&](float literal_value) { write(stack, return_address, literal_value); },
			[&](parser::OperatorNode const & op_node) 
			{
				// TODO: Operators for non-ints
				int const stack_top = stack.top_pointer;
				int const lval = read_word(stack, eval_expression_tree(*op_node.left, stack, program));
				int const rval = read_word(stack, eval_expression_tree(*op_node.right, stack, program));
				int result;
				switch (op_node.op)
				{
					case parser::Operator::add:		 result = lval + rval; break;
					case parser::Operator::subtract: result = lval - rval; break;
					case parser::Operator::multiply: result = lval * rval; break;
					case parser::Operator::divide:	 result = lval / rval; break;
					default: declare_unreachable();
				}
				
				free_up_to(stack, stack_top);
				write(stack, return_address, result);
			},
			[&](parser::VariableNode const & var_node)
			{
				int const address = stack.base_pointer + var_node.variable_offset;
				write_word(stack, return_address, read_word(stack, address));
			},
			[&](parser::FunctionNode const & func_node) // Not sure if I like this. Maybe evaluating a function node should just be an error or a noop?
			{
				write(stack, return_address, func_node.function_id);
			},
			[&](parser::FunctionCallNode const & func_call_node)
			{
				Function const & func = program.functions[func_call_node.function_id];

				int const parameter_size = func.parameter_count * sizeof(int); // TODO: Parameter size for different types

				// Write ebp to the stack so that we can return to our stack frame when the function ends.
				int const ebp_address = alloc(stack, sizeof(int));
				write_word(stack, ebp_address, stack.base_pointer);

				// Push stack pointer to the end of parameters. This is to avoid that the expressions that compute
				// the parameters of the function overwrite the memory reserved for the parameters.
				int const parameters_start = alloc(stack, parameter_size);

				// Evaluate the expressions that yield the parameters of the function.
				for (int i = 0, next_parameter_address = parameters_start; i < func_call_node.parameters.size(); ++i)
				{
					eval_expression_tree(func_call_node.parameters[i], stack, program, next_parameter_address);
					next_parameter_address += expression_type(func_call_node.parameters[i], program).size;
				}

				// Move the stack pointers.
				stack.base_pointer = parameters_start;
				stack.top_pointer = parameters_start + func.stack_frame_size;

				// Run the function.
				for (auto const & statement : func.statements)
					if (run_statement_tree(statement, stack, program, return_address))
						break;
			}
		);
		std::visit(visitor, tree.as_variant());
	}

	auto run_statement_tree(parser::StatementTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> bool
	{
		auto const visitor = overload(
			[&](parser::VariableDeclarationStatementNode const & node) 
			{
				int const address = stack.base_pointer + node.variable_offset;
				eval_expression_tree(node.assigned_expression, stack, program, address);
				return false;
			},
			[&](parser::ReturnStatementNode const & var_node)
			{
				// Read the previous ebp from the stack.
				int const prev_ebp_address = stack.base_pointer - 4;
				int const prev_ebp = read_word(stack, prev_ebp_address);
				eval_expression_tree(var_node.returned_expression, stack, program, return_address);
				stack.top_pointer = prev_ebp_address + expression_type(var_node.returned_expression, program).size;
				stack.base_pointer = prev_ebp;
				return true; // Return true to indicate that the function should end.
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

}
