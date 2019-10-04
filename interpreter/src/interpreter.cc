#include "interpreter.hh"
#include "parser.hh"
#include "unreachable.hh"
#include "overload.hh"
#include "program.hh"
#include "utils.hh"
#include "variant.hh"
#include <cassert>

namespace interpreter
{

	struct StackGuard
	{
		int * stack_top_pointer;
		int old_top_pointer;

		explicit StackGuard(ProgramStack & stack)
			: stack_top_pointer(&stack.top_pointer)
			, old_top_pointer(stack.top_pointer)
		{}

		StackGuard(StackGuard const &) = delete;
		StackGuard(StackGuard &&) = delete;
		StackGuard & operator = (StackGuard const &) = delete;
		StackGuard & operator = (StackGuard &&) = delete;

		~StackGuard()
		{
			*stack_top_pointer = old_top_pointer;
		}
	};

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

	auto alloc(ProgramStack & stack, int size, int alignment = 4) noexcept -> int
	{
		int const address = align(stack.top_pointer, alignment);
		stack.top_pointer += size;
		return address;
	}

	auto free_up_to(ProgramStack & stack, int address) noexcept -> void
	{
		stack.top_pointer = address;
	}

	auto pointer_at_address(ProgramStack & stack, int address) noexcept -> void *
	{
		return stack.memory.data() + address;
	}

	template <typename T>
	auto push_to_stack_top(ProgramStack & stack, T const & value) noexcept -> int
	{
		int const address = alloc_in_stack(stack, sizeof(T));
		write(stack, address, value);
		return address;
	}

	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program) noexcept -> int
	{
		int const address = alloc(stack, expression_type_size(tree, program));
		eval_expression_tree(tree, stack, program, address);
		return address;
	}

	auto call_function(FunctionId function_id, span<expr::ExpressionTree const> parameters, ProgramStack & stack, Program const & program, int return_address) noexcept -> void
	{
		if (!function_id.is_extern)
		{
			Function const & func = program.functions[function_id.index];

			int const parameter_size = func.parameter_size;

			// Write ebp to the stack so that we can return to our stack frame when the function ends.
			int const ebp_address = alloc(stack, sizeof(int));
			write_word(stack, ebp_address, stack.base_pointer);

			// Push stack pointer to the end of parameters. This is to avoid that the expressions that compute
			// the parameters of the function overwrite the memory reserved for the parameters.
			int const parameters_start = alloc(stack, parameter_size, func.stack_frame_alignment);

			// Evaluate the expressions that yield the parameters of the function.
			for (int i = 0, next_parameter_address = parameters_start; i < parameters.size(); ++i)
			{
				eval_expression_tree(parameters[i], stack, program, next_parameter_address);
				next_parameter_address += expression_type_size(parameters[i], program);
			}

			// Move the stack pointers.
			stack.base_pointer = parameters_start;
			stack.top_pointer = parameters_start + func.stack_frame_size;

			// Run the function.
			for (auto const & statement : func.statements)
			{
				auto const cf = run_statement(statement, stack, program, return_address);
				if (auto const ret = try_get<control_flow::Return>(cf))
					break;
			}

			// Read the previous ebp from the stack.
			int const prev_ebp_address = stack.base_pointer - sizeof(int);
			int const prev_ebp = read_word(stack, prev_ebp_address);
			stack.top_pointer = prev_ebp_address + type_size(program, func.return_type);
			stack.base_pointer = prev_ebp;
		}
		else
		{
			ExternFunction const & func = program.extern_functions[function_id.index];

			int const parameter_size = func.parameter_size;
			int const prev_stack_top = stack.top_pointer;
			int const parameters_start = alloc(stack, parameter_size, func.parameter_alignment);

			// Evaluate the expressions that yield the parameters of the function.
			for (int i = 0, next_parameter_address = parameters_start; i < parameters.size(); ++i)
			{
				eval_expression_tree(parameters[i], stack, program, next_parameter_address);
				next_parameter_address += expression_type_size(parameters[i], program);
			}
			
			func.caller(func.function_pointer, pointer_at_address(stack, parameters_start), pointer_at_address(stack, return_address));
			free_up_to(stack, prev_stack_top);
		}
	}

	auto eval_variable_node(TypeId variable_type, int address, ProgramStack & stack, int return_address) noexcept -> void
	{
		if (variable_type.is_reference)
		{
			auto const pointer = read<void const *>(stack, address);
			write(stack, return_address, pointer);
		}
		else
		{
			write(stack, return_address, pointer_at_address(stack, address));
		}
	}

	auto eval_expression_tree(expr::ExpressionTree const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept -> void
	{
		auto const visitor = overload(
			[&](expr::Literal<int> literal) { write(stack, return_address, literal.value); },
			[&](expr::Literal<float> literal) { write(stack, return_address, literal.value); },
			[&](expr::Literal<bool> literal) { write(stack, return_address, literal.value); },
			[&](expr::LocalVariableNode const & var_node)
			{
				int const address = stack.base_pointer + var_node.variable_offset;
				eval_variable_node(var_node.variable_type, address, stack, return_address);
			},
			[&](expr::GlobalVariableNode const & var_node)
			{
				int const address = var_node.variable_offset;
				eval_variable_node(var_node.variable_type, address, stack, return_address);
			},
			[&](expr::DereferenceNode const & deref_node)
			{
				StackGuard const g(stack);
				int const pointer_address = eval_expression_tree(*deref_node.expression, stack, program);
				auto const pointer = read<void const *>(stack, pointer_address);
				memcpy(pointer_at_address(stack, return_address), pointer, type_size(program, deref_node.variable_type));
			},
			[&](expr::FunctionNode const & func_node) // Not sure if I like this. Maybe evaluating a function node should just be an error or a noop?
			{
				write(stack, return_address, func_node.function_id);
			},
			[&](expr::FunctionCallNode const & func_call_node)
			{
				call_function(func_call_node.function_id, func_call_node.parameters, stack, program, return_address);
			},
			[&](expr::RelationalOperatorCallNode const & op_node)
			{
				if (op_node.op == expr::Operator::not_equal)
				{
					// Call operator ==.
					call_function(op_node.function_id, *op_node.parameters, stack, program, return_address);
					// Negate the result.
					write(stack, return_address, !read<bool>(stack, return_address)); 
				}
				else // We need to call operator <=>, and convert the int it returns into a boolean.
				{
					int const prev_stack_top = stack.top_pointer;
					int const temp_storage = alloc(stack, sizeof(int), alignof(int));
					call_function(op_node.function_id, *op_node.parameters, stack, program, temp_storage);

					int const three_way_result = read_word(stack, temp_storage);
					bool boolean_result;
					switch (op_node.op)
					{
						case expr::Operator::less:			boolean_result = three_way_result <  0; break;
						case expr::Operator::less_equal:	boolean_result = three_way_result <= 0; break;
						case expr::Operator::greater:		boolean_result = three_way_result >  0; break;
						case expr::Operator::greater_equal:	boolean_result = three_way_result >= 0; break;
						default: declare_unreachable();
					}

					// Write final result to return address.
					write(stack, return_address, boolean_result);
					free_up_to(stack, prev_stack_top);
				}
			},
			[&](expr::IfNode const & if_node)
			{
				int const result_addr = eval_expression_tree(*if_node.condition, stack, program);
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				expr::ExpressionTree const & expr = condition ? *if_node.then_case : *if_node.else_case;
				eval_expression_tree(expr, stack, program, return_address);
			},
			[&](expr::StatementBlockNode const & block_node)
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the function.
				for (auto const & statement : block_node.statements)
				{
					auto const cf = run_statement(statement, stack, program, return_address);
					if (auto const ret = try_get<control_flow::Return>(cf))
						break;
				}
			}
		);
		std::visit(visitor, tree.as_variant());
	}

	auto run_statement(stmt::Statement const & tree, ProgramStack & stack, Program const & program, int return_address) noexcept
		-> control_flow::Variant
	{
		auto const visitor = overload_default_ret(control_flow::Variant(),
			[&](stmt::VariableDeclarationStatement const & node)
			{
				int const address = stack.base_pointer + node.variable_offset;
				eval_expression_tree(node.assigned_expression, stack, program, address);
			},
			[&](stmt::ExpressionStatement const & expr_node)
			{
				eval_expression_tree(expr_node.expression, stack, program, stack.top_pointer);
			},
			[&](stmt::ReturnStatement const & return_node)
			{
				eval_expression_tree(return_node.returned_expression, stack, program, return_address);
				return control_flow::Return{};
			},
			[&](stmt::IfStatement const & if_node) -> control_flow::Variant
			{
				int const result_addr = eval_expression_tree(if_node.condition, stack, program);
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				stmt::Statement const * const statement = condition ? if_node.then_case.get() : if_node.else_case.get();
				if (statement)
					return run_statement(*statement, stack, program, return_address);
				else
					return control_flow::Nothing();
			},
			[&](stmt::StatementBlock const & block_node) -> control_flow::Variant
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the statements.
				for (auto const & statement : block_node.statements)
				{
					auto const cf = run_statement(statement, stack, program, return_address);
					if (has_type<control_flow::Return>(cf) || has_type<control_flow::Break>(cf) || has_type<control_flow::Continue>(cf))
						return cf;
				}

				return control_flow::Nothing();
			},
			[&](stmt::WhileStatement const & while_node) -> control_flow::Variant
			{
				for (;;)
				{
					int const result_addr = eval_expression_tree(while_node.condition, stack, program);
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						auto const cf = run_statement(*while_node.body, stack, program, return_address);
						if (has_type<control_flow::Return>(cf))
							return cf;
						if (has_type<control_flow::Break>(cf))
							return control_flow::Nothing();
					}
					else
					{
						return control_flow::Nothing();
					}
				}
			},
			[&](stmt::ForStatement const & for_node) -> control_flow::Variant
			{
				// Allocate stack frame for the scope.
				StackGuard const stack_guard(stack);
				stack.top_pointer += for_node.scope.stack_frame_size;

				// Run init statement.
				run_statement(*for_node.init_statement, stack, program, return_address);

				for (;;)
				{
					// Check condition.
					int const result_addr = eval_expression_tree(for_node.condition, stack, program);
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						// Run body.
						auto const cf = run_statement(*for_node.body, stack, program, return_address);
						if (has_type<control_flow::Return>(cf))
							return cf;
						if (has_type<control_flow::Break>(cf))
							return control_flow::Nothing();

						// Run end expression.
						eval_expression_tree(for_node.end_expression, stack, program, stack.top_pointer);
					}
					else
					{
						return control_flow::Nothing();
					}
				}
			},
			[&](stmt::BreakStatement const &) -> control_flow::Variant
			{
				return control_flow::Break();
			},
			[&](stmt::ContinueStatement const &) -> control_flow::Variant
			{
				return control_flow::Continue();
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto run(Program const & program, int stack_size) noexcept -> int
	{
		assert(program.main_function != invalid_function_id);

		ProgramStack stack;
		alloc_stack(stack, stack_size);

		// Initialization of globals.
		alloc(stack, program.global_scope.stack_frame_size);
		for (auto const & statement : program.global_initialization_statements)
			run_statement(statement, stack, program, 0);

		// Run main.
		int const return_address = alloc(stack, sizeof(int), alignof(int));
		call_function(program.main_function, {}, stack, program, return_address);
		return read<int>(stack, return_address);
	}

}
