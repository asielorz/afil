#include "interpreter.hh"
#include "program.hh"

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

	auto alloc(ProgramStack & stack, int size, int alignment) noexcept -> int
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

	auto call_extern_function(complete::ExternFunction const & function, span<complete::Expression const> parameters, ProgramStack & stack, RuntimeContext context, int return_address)
		-> expected<void, UnmetPrecondition>
	{
		int const parameter_size = function.parameter_size;
		int const prev_stack_top = stack.top_pointer;
		int const parameters_start = alloc(stack, parameter_size, function.parameter_alignment);

		// Evaluate the expressions that yield the parameters of the function.
		for (int i = 0, next_parameter_address = parameters_start; i < parameters.size(); ++i)
		{
			try_call_void(eval_expression(parameters[i], stack, context, next_parameter_address));
			next_parameter_address += expression_type_size(parameters[i], context.program);
		}

		function.caller(function.function_pointer, pointer_at_address(stack, parameters_start), pointer_at_address(stack, return_address));
		free_up_to(stack, prev_stack_top);

		return success;
	}
	auto call_extern_function(complete::ExternFunction const & function, span<complete::Expression const> parameters, ProgramStack & stack, CompileTimeContext context, int return_address)
		->expected<void, UnmetPrecondition>
	{
		static_cast<void>(function, parameters, stack, context, return_address);
		declare_unreachable();
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

	auto run(complete::Program const & program, int stack_size) noexcept -> expected<int, UnmetPrecondition>
	{
		assert(program.main_function != invalid_function_id);

		ProgramStack stack;
		alloc_stack(stack, stack_size);

		// Initialization of globals.
		alloc(stack, program.global_scope.stack_frame_size);
		for (auto const & statement : program.global_initialization_statements)
			try_call_void(run_statement(statement, stack, RuntimeContext{program}, 0));

		// Run main.
		int const return_address = alloc(stack, sizeof(int), alignof(int));
		try_call_void(call_function(program.main_function, {}, stack, RuntimeContext{program}, return_address));
		return read<int>(stack, return_address);
	}

} // namespace interpreter
