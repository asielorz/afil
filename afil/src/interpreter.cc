#include "interpreter.hh"
#include "program.hh"
#include "utils/overload.hh"
#include "utils/unreachable.hh"
#include "utils/utils.hh"
#include "utils/variant.hh"
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

	auto pointer_at_address(ProgramStack & stack, int address) noexcept -> char *
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

	auto call_function(FunctionId function_id, span<complete::Expression const> parameters, ProgramStack & stack, complete::Program const & program, int return_address) noexcept -> expected<void, UnmetPrecondition>
	{
		if (function_id.type == FunctionId::Type::instrinsic)
		{
			int const stack_top = stack.top_pointer;

			switch (function_id.index)
			{
				case 0: // int + int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) + read<int>(stack, b_address));
					break;
				}
				case 1: // int - int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) - read<int>(stack, b_address));
					break;
				}
				case 2: // int * int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) * read<int>(stack, b_address));
					break;
				}
				case 3: // int / int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) / read<int>(stack, b_address));
					break;
				}
				case 4: // int % int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) % read<int>(stack, b_address));
					break;
				}
				case 5: // int == int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) == read<int>(stack, b_address));
					break;
				}
				case 6: // int <=> int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) - read<int>(stack, b_address));
					break;
				}
				case 7: // -int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					write(stack, return_address, -read<int>(stack, a_address));
					break;
				}
				case 8: // int & int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) & read<int>(stack, b_address));
					break;
				}
				case 9: // int | int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) | read<int>(stack, b_address));
					break;
				}
				case 10: // int ^ int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) ^ read<int>(stack, b_address));
					break;
				}
				case 11: // ~int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					write(stack, return_address, ~read<int>(stack, a_address));
					break;
				}
				case 12: // int >> int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) >> read<int>(stack, b_address));
					break;
				}
				case 13: // int << int
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<int>(stack, a_address) << read<int>(stack, b_address));
					break;
				}

				case 14: // float + float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) + read<float>(stack, b_address));
					break;
				}
				case 15: // float - float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) - read<float>(stack, b_address));
					break;
				}
				case 16: // float * float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) * read<float>(stack, b_address));
					break;
				}
				case 17: // float / float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) / read<float>(stack, b_address));
					break;
				}
				case 18: // float == float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) == read<float>(stack, b_address));
					break;
				}
				case 19: // float <=> float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<float>(stack, a_address) - read<float>(stack, b_address));
					break;
				}
				case 20: // -float
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					write(stack, return_address, -read<float>(stack, a_address));
					break;
				}

				case 21: // char + char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<unsigned char>(stack, a_address) + read<unsigned char>(stack, b_address));
					break;
				}
				case 22: // char - char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<unsigned char>(stack, a_address) - read<unsigned char>(stack, b_address));
					break;
				}
				case 23: // char * char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<unsigned char>(stack, a_address) * read<unsigned char>(stack, b_address));
					break;
				}
				case 24: // char / char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<unsigned char>(stack, a_address) / read<unsigned char>(stack, b_address));
					break;
				}
				case 25: // char == char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<unsigned char>(stack, a_address) == read<unsigned char>(stack, b_address));
					break;
				}
				case 26: // char <=> char
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, int(read<unsigned char>(stack, a_address)) - int(read<unsigned char>(stack, b_address)));
					break;
				}
				
				case 27: // bool and bool
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<bool>(stack, a_address) && read<bool>(stack, b_address));
					break;
				}
				case 28: // bool or bool
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<bool>(stack, a_address) || read<bool>(stack, b_address));
					break;
				}
				case 29: // bool xor bool
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<bool>(stack, a_address) != read<bool>(stack, b_address));
					break;
				}
				case 30: // not bool
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					write(stack, return_address, !read<bool>(stack, a_address));
					break;
				}
				case 31: // bool == bool
				{
					try_call_decl(int const a_address, eval_expression(parameters[0], stack, program));
					try_call_decl(int const b_address, eval_expression(parameters[1], stack, program));
					write(stack, return_address, read<bool>(stack, a_address) == read<bool>(stack, b_address));
					break;
				}
			}

			free_up_to(stack, stack_top);
		}
		else if (function_id.type == FunctionId::Type::program)
		{
			complete::Function const & func = program.functions[function_id.index];

			int const parameter_size = func.parameter_size;

			// Save previous stack frame bounds.
			int const prev_ebp = stack.base_pointer;
			int const prev_esp = stack.top_pointer;

			// Allocate memory for temporaries passed by reference.
			int size_of_temporaries_passed_by_reference = 0;
			int alignment_of_temporaries_passed_by_reference = 1;
			for (size_t i = 0; i < parameters.size(); ++i)
			{
				complete::TypeId const param_type = expression_type_id(parameters[i], program);
				if (!param_type.is_reference && func.variables[i].type.is_reference)
				{
					complete::Type const & param_type_data = type_with_id(program, param_type);
					size_of_temporaries_passed_by_reference = add_size_aligned(size_of_temporaries_passed_by_reference, param_type_data.size, param_type_data.alignment);
					alignment_of_temporaries_passed_by_reference = std::max(alignment_of_temporaries_passed_by_reference, param_type_data.alignment);
				}
			}
			int const temporaries_start = alloc(stack, size_of_temporaries_passed_by_reference, alignment_of_temporaries_passed_by_reference);

			// Allocate memory for the parameters.
			int const parameters_start = alloc(stack, parameter_size, func.stack_frame_alignment);

			// Evaluate the expressions that yield the parameters of the function.
			for (
				int i = 0, next_parameter_address = parameters_start, next_temporary_address = temporaries_start;
				i < parameters.size(); 
				++i
			)
			{
				complete::TypeId const param_type = expression_type_id(parameters[i], program);
				if (!param_type.is_reference && func.variables[i].type.is_reference)
				{
					try_call_void(eval_expression(parameters[i], stack, program, next_temporary_address));
					write(stack, next_parameter_address, pointer_at_address(stack, next_temporary_address));
					next_temporary_address += expression_type_size(parameters[i], program);
					next_parameter_address += sizeof(void *);
				}
				else
				{
					try_call_void(eval_expression(parameters[i], stack, program, next_parameter_address));
					next_parameter_address += expression_type_size(parameters[i], program);
				}
			}

			// Move the stack pointers.
			stack.base_pointer = parameters_start;
			stack.top_pointer = parameters_start + func.stack_frame_size;

			// Run the preconditions
			int const precondition_count = static_cast<int>(func.preconditions.size());
			for (int i = 0; i < precondition_count; ++i)
			{
				try_call_decl(int const precondition_return_address, eval_expression(func.preconditions[i], stack, program));
				bool const precondition_ok = read<bool>(stack, precondition_return_address);
				if (!precondition_ok)
					return Error(UnmetPrecondition{function_id, i});
			}
			stack.top_pointer = parameters_start + func.stack_frame_size;

			// Run the function.
			for (auto const & statement : func.statements)
			{
				try_call_decl(auto const cf, run_statement(statement, stack, program, return_address));
				if (cf == ControlFlow::Return)
					break;
			}

			// Restore previous stack frame.
			stack.top_pointer = prev_esp;
			stack.base_pointer = prev_ebp;
		}
		else
		{
			complete::ExternFunction const & func = program.extern_functions[function_id.index];

			int const parameter_size = func.parameter_size;
			int const prev_stack_top = stack.top_pointer;
			int const parameters_start = alloc(stack, parameter_size, func.parameter_alignment);

			// Evaluate the expressions that yield the parameters of the function.
			for (int i = 0, next_parameter_address = parameters_start; i < parameters.size(); ++i)
			{
				try_call_void(eval_expression(parameters[i], stack, program, next_parameter_address));
				next_parameter_address += expression_type_size(parameters[i], program);
			}

			func.caller(func.function_pointer, pointer_at_address(stack, parameters_start), pointer_at_address(stack, return_address));
			free_up_to(stack, prev_stack_top);
		}

		return success;
	}

	auto eval_variable_node(complete::TypeId variable_type, int address, ProgramStack & stack, int return_address) noexcept -> void
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

	auto eval_expression(complete::Expression const & tree, ProgramStack & stack, complete::Program const & program) noexcept -> expected<int, UnmetPrecondition>
	{
		int const address = alloc(stack, expression_type_size(tree, program));
		try_call_void(eval_expression(tree, stack, program, address));
		return address;
	}

	auto eval_expression(complete::Expression const & expr, ProgramStack & stack, complete::Program const & program, int return_address) noexcept -> expected<void, UnmetPrecondition>
	{
		using namespace complete;

		auto const visitor = overload_default_ret(expected<void, UnmetPrecondition>(success),
			[&](expression::Literal<int> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<float> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<bool> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<uninit_t>) {},
			[&](expression::Literal<TypeId> literal) { write(stack, return_address, literal.value); },
			[&](expression::StringLiteral literal) 
			{
				write(stack, return_address, literal.value.data(), static_cast<int>(literal.value.size())); 
			},
			[&](expression::LocalVariable const & var_node)
			{
				int const address = stack.base_pointer + var_node.variable_offset;
				eval_variable_node(var_node.variable_type, address, stack, return_address);
			},
			[&](expression::GlobalVariable const & var_node)
			{
				int const address = var_node.variable_offset;
				eval_variable_node(var_node.variable_type, address, stack, return_address);
			},
			[&](expression::MemberVariable const & var_node) -> expected<void, UnmetPrecondition>
			{
				// If the owner is an lvalue, return a reference to the member.
				try_call_decl(int const owner_address, eval_expression(*var_node.owner, stack, program));
				if (expression_type_id(*var_node.owner, program).is_reference)
				{
					char * const owner_ptr = read<char *>(stack, owner_address);
					write(stack, return_address, owner_ptr + var_node.variable_offset);
				}
				// If the owner is an rvalue, return the member by value.
				else
				{
					int const variable_size = type_size(program, var_node.variable_type);
					memcpy(pointer_at_address(stack, return_address), pointer_at_address(stack, owner_address + var_node.variable_offset), variable_size);
				}
				return success;
			},
			[&](expression::Constant const & constant_node)
			{
				//write(stack, return_address, constant_node.value.data(), static_cast<int>(constant_node.value.size()));
				write(stack, return_address, constant_node.value.data());
			},
			[&](expression::Dereference const & deref_node) -> expected<void, UnmetPrecondition>
			{
				StackGuard const g(stack);
				try_call_decl(int const pointer_address, eval_expression(*deref_node.expression, stack, program));
				auto const pointer = read<void const *>(stack, pointer_address);
				memcpy(pointer_at_address(stack, return_address), pointer, type_size(program, deref_node.return_type));
				return success;
			},
			[&](expression::ReinterpretCast const & addressof_node)
			{
				return eval_expression(*addressof_node.operand, stack, program, return_address);
			},
			[&](expression::Subscript const & subscript_node) -> expected<void, UnmetPrecondition>
			{
				TypeId const array_type_id = expression_type_id(*subscript_node.array, program);
				Type const & array_type = type_with_id(program, array_type_id);

				StackGuard const g(stack);
				
				if (array_type_id.is_reference || is_array_pointer(array_type))
				{
					try_call_decl(int const array_address, eval_expression(*subscript_node.array, stack, program));
					try_call_decl(int const index_address, eval_expression(*subscript_node.index, stack, program));
					char const * const array = read<char const *>(stack, array_address);
					int const index = read<int>(stack, index_address);
					int const value_type_size = type_size(program, remove_reference(subscript_node.return_type));
					write(stack, return_address, array + index * value_type_size);
				}
				else // array rvalue
				{
					try_call_decl(int const array_address, eval_expression(*subscript_node.array, stack, program));
					try_call_decl(int const index_address, eval_expression(*subscript_node.index, stack, program));
					char const * const array = pointer_at_address(stack, array_address);
					int const index = read<int>(stack, index_address);
					int const value_type_size = type_size(program, remove_reference(subscript_node.return_type));
					memcpy(pointer_at_address(stack, return_address), array + index * value_type_size, value_type_size);
				}
				return success;
			},
			[&](expression::FunctionCall const & func_call_node)
			{
				return call_function(func_call_node.function_id, func_call_node.parameters, stack, program, return_address);
			},
			[&](expression::RelationalOperatorCall const & op_node) -> expected<void, UnmetPrecondition>
			{
				if (op_node.op == Operator::not_equal)
				{
					// Call operator ==.
					try_call_void(call_function(op_node.function_id, op_node.parameters, stack, program, return_address));
					// Negate the result.
					write(stack, return_address, !read<bool>(stack, return_address));
				}
				else // We need to call operator <=>, and convert the int it returns into a boolean.
				{
					int const prev_stack_top = stack.top_pointer;
					int const temp_storage = alloc(stack, sizeof(int), alignof(int));
					try_call_void(call_function(op_node.function_id, op_node.parameters, stack, program, temp_storage));

					int const three_way_result = read_word(stack, temp_storage);
					bool boolean_result;
					switch (op_node.op)
					{
						case Operator::less:			boolean_result = three_way_result < 0; break;
						case Operator::less_equal:		boolean_result = three_way_result <= 0; break;
						case Operator::greater:			boolean_result = three_way_result > 0; break;
						case Operator::greater_equal:	boolean_result = three_way_result >= 0; break;
						default: declare_unreachable();
					}

					// Write final result to return address.
					write(stack, return_address, boolean_result);
					free_up_to(stack, prev_stack_top);
				}
				return success;
			},
			[&](expression::Assignment const & assign_node) -> expected<void, UnmetPrecondition>
			{
				try_call_decl(const int dest_address, eval_expression(*assign_node.destination, stack, program));
				try_call_decl(const int source_address, eval_expression(*assign_node.source, stack, program));
				memcpy(read<void *>(stack, dest_address), pointer_at_address(stack, source_address), expression_type_size(*assign_node.source, program));
				free_up_to(stack, dest_address);
				return success;
			},
			[&](expression::If const & if_node) -> expected<void, UnmetPrecondition>
			{
				try_call_decl(int const result_addr, eval_expression(*if_node.condition, stack, program));
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				Expression const & branch = condition ? *if_node.then_case : *if_node.else_case;
				try_call_void(eval_expression(branch, stack, program, return_address));
				return success;
			},
			[&](expression::StatementBlock const & block_node) -> expected<void, UnmetPrecondition>
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the function.
				for (auto const & statement : block_node.statements)
				{
					try_call_decl(auto const cf, run_statement(statement, stack, program, return_address));
					if (cf == ControlFlow::Return)
						break;
				}
				return success;
			},
			[&](expression::Constructor const & ctor_node) -> expected<void, UnmetPrecondition>
			{
				Type const & constructed_type = type_with_id(program, ctor_node.constructed_type);

				if (is_struct(constructed_type))
				{
					Struct const & struct_data = *struct_for_type(program, constructed_type);
					for (size_t i = 0; i < struct_data.member_variables.size(); ++i)
						try_call_void(eval_expression(ctor_node.parameters[i], stack, program, return_address + struct_data.member_variables[i].offset));
				}
				else if (is_array(constructed_type))
				{
					Type::Array const array = std::get<Type::Array>(type_with_id(program, ctor_node.constructed_type).extra_data);
					int const value_type_size = type_size(program, array.value_type);

					// Fill constructor
					if (ctor_node.parameters.size() == 1)
					{
						for (int i = 0; i < array.size; ++i)
							try_call_void(eval_expression(ctor_node.parameters[0], stack, program, return_address + value_type_size * i));
					}
					// Regular constructor
					else
					{
						for (int i = 0; i < array.size; ++i)
							try_call_void(eval_expression(ctor_node.parameters[i], stack, program, return_address + value_type_size * i));
					}
				}
				else
				{
					declare_unreachable();
				}
				return success;
			}
		);
		return std::visit(visitor, expr.as_variant());
	}

	auto run_statement(complete::Statement const & tree, ProgramStack & stack, complete::Program const & program, int return_address) noexcept
		-> expected<ControlFlow, UnmetPrecondition>
	{
		using namespace complete;

		auto const visitor = overload(
			[&](statement::VariableDeclaration const & node) -> expected<ControlFlow, UnmetPrecondition>
			{
				int const address = stack.base_pointer + node.variable_offset;
				try_call_void(eval_expression(node.assigned_expression, stack, program, address));
				return ControlFlow::Nothing;
			},
			[&](statement::ExpressionStatement const & expr_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression(expr_node.expression, stack, program, stack.top_pointer));
				return ControlFlow::Nothing;
			},
			[&](statement::Return const & return_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression(return_node.returned_expression, stack, program, return_address));
				return ControlFlow::Return;
			},
			[&](statement::If const & if_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_decl(int const result_addr, eval_expression(if_node.condition, stack, program));
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				Statement const * const branch = condition ? if_node.then_case.get() : if_node.else_case.get();
				if (branch)
					return run_statement(*branch, stack, program, return_address);
				else
					return ControlFlow::Nothing;
			},
			[&](statement::StatementBlock const & block_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the statements.
				for (auto const & statement : block_node.statements)
				{
					try_call_decl(auto const cf, run_statement(statement, stack, program, return_address));
					if (cf == ControlFlow::Return || cf == ControlFlow::Break || cf == ControlFlow::Continue)
						return cf;
				}

				return ControlFlow::Nothing;
			},
			[&](statement::While const & while_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				for (;;)
				{
					try_call_decl(int const result_addr, eval_expression(while_node.condition, stack, program));
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						try_call_decl(auto const cf, run_statement(*while_node.body, stack, program, return_address));
						if (cf == ControlFlow::Return)
							return cf;
						if (cf == ControlFlow::Break)
							return ControlFlow::Nothing;
					}
					else
					{
						return ControlFlow::Nothing;
					}
				}
			},
			[&](statement::For const & for_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				// Allocate stack frame for the scope.
				StackGuard const stack_guard(stack);
				stack.top_pointer += for_node.scope.stack_frame_size;

				// Run init statement.
				try_call_void(run_statement(*for_node.init_statement, stack, program, return_address));

				for (;;)
				{
					// Check condition.
					try_call_decl(int const result_addr, eval_expression(for_node.condition, stack, program));
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						// Run body.
						try_call_decl(auto const cf, run_statement(*for_node.body, stack, program, return_address));
						if (cf == ControlFlow::Return)
							return cf;
						if (cf == ControlFlow::Break)
							return ControlFlow::Nothing;

						// Run end expression.
						try_call_void(eval_expression(for_node.end_expression, stack, program, stack.top_pointer));
					}
					else
					{
						return ControlFlow::Nothing;
					}
				}
			},
			[&](statement::Break const &) -> expected<ControlFlow, UnmetPrecondition>
			{
				return ControlFlow::Break;
			},
			[&](statement::Continue const &) -> expected<ControlFlow, UnmetPrecondition>
			{
				return ControlFlow::Continue;
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto run(complete::Program const & program, int stack_size) noexcept -> expected<int, UnmetPrecondition>
	{
		assert(program.main_function != invalid_function_id);

		ProgramStack stack;
		alloc_stack(stack, stack_size);

		// Initialization of globals.
		alloc(stack, program.global_scope.stack_frame_size);
		for (auto const & statement : program.global_initialization_statements)
			try_call_void(run_statement(statement, stack, program, 0));

		// Run main.
		int const return_address = alloc(stack, sizeof(int), alignof(int));
		try_call_void(call_function(program.main_function, {}, stack, program, return_address));
		return read<int>(stack, return_address);
	}

} // namespace interpreter
