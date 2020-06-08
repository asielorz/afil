#include "interpreter.hh"
#include "constexpr.hh"
#include "program.hh"
#include "template_instantiation.hh"

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
		stack.top_pointer = address + size;
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

	auto call_extern_function(complete::ExternFunction const & function, ProgramStack & stack, RuntimeContext context, int return_address)
		-> expected<void, UnmetPrecondition>
	{
		static_cast<void>(context);
		function.caller(function.function_pointer, pointer_at_address(stack, stack.base_pointer), pointer_at_address(stack, return_address));
		return success;
	}
	auto call_extern_function(complete::ExternFunction const & function, ProgramStack & stack, CompileTimeContext context, int return_address)
		->expected<void, UnmetPrecondition>
	{
		static_cast<void>(function, stack, context, return_address);
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

	auto detail::eval_compiles_expression_impl(complete::expression::Compiles const & compiles_expr, ProgramStack & stack, CompileTimeContext context, int return_address) noexcept 
		-> expected<void, UnmetPrecondition>
	{
		complete::Scope fake_scope;

		for (complete::CompilesFakeVariable const & fake_var : compiles_expr.variables)
		{
			try_call_decl(int const type_address, eval_expression(fake_var.type, stack, context));
			complete::TypeId const var_type = read<complete::TypeId>(stack, type_address);

			add_variable_to_scope(fake_scope, fake_var.name, var_type, 0, context.program);
		}

		auto const guard = instantiation::push_block_scope(context.scope_stack, fake_scope);

		bool all_body_expressions_compile = true;
		for (incomplete::ExpressionToTest const & expression_to_test : compiles_expr.body)
		{
			if (!instantiation::test_if_expression_compiles(expression_to_test, context.template_parameters, context.scope_stack, out(context.program), nullptr))
			{
				all_body_expressions_compile = false;
				break;
			}
		}

		write(stack, return_address, all_body_expressions_compile);
		return success;
	}

	[[nodiscard]] auto evaluate_constant_expression(
		complete::Expression const & expression, 
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		instantiation::ScopeStack & scope_stack,
		complete::Program & program, 
		void * outValue
	) noexcept
		-> expected<void, UnmetPrecondition>
	{
		assert(is_constant_expression(expression, program, next_block_scope_offset(scope_stack)));

		interpreter::ProgramStack stack;
		alloc_stack(stack, 256);

		try_call_void(interpreter::eval_expression(expression, stack, interpreter::CompileTimeContext{program, template_parameters, scope_stack}));

		memcpy(outValue, pointer_at_address(stack, 0), expression_type_size(expression, program));

		return success;
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
