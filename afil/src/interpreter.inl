namespace interpreter
{

	template <typename T> auto read(ProgramStack const & stack, int address) noexcept -> T const &
	{
		return reinterpret_cast<T const &>(stack.memory[address]);
	}

	template <typename T> auto write(ProgramStack & stack, int address, T const & value) noexcept -> void
	{
		reinterpret_cast<T &>(stack.memory[address]) = value;
	}

	template <typename T> auto push(ProgramStack & stack, T const & value) -> void
	{
		int const address = alloc(stack, sizeof(T), alignof(T));
		write(stack, address, value);
	}

	template <typename T> auto write(ProgramStack & stack, int address, T const array[], int size) noexcept -> void
	{
		memcpy(pointer_at_address(stack, address), array, size * sizeof(T));
	}

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

	auto call_extern_function(complete::ExternFunction const & function, span<complete::Expression const> parameters, ProgramStack & stack, RuntimeContext context, int return_address)
		->expected<void, UnmetPrecondition>;
	auto call_extern_function(complete::ExternFunction const & function, span<complete::Expression const> parameters, ProgramStack & stack, CompileTimeContext context, int return_address)
		->expected<void, UnmetPrecondition>;

	auto eval_variable_node(complete::TypeId variable_type, int address, ProgramStack & stack, int return_address) noexcept -> void;

	template <typename Ret, typename A, typename B>
	inline auto call_intrinsic_function(ProgramStack & stack, int return_address, Ret(*function)(A, B)) -> void
	{
		int const a_address = stack.base_pointer;
		int const b_address = align(a_address + sizeof(A), alignof(B));
		write(stack, return_address, function(read<A>(stack, a_address), read<B>(stack, b_address)));
	}

	template <typename Ret, typename A>
	auto call_intrinsic_function(ProgramStack & stack, int return_address, Ret(*function)(A)) -> void
	{
		int const a_address = stack.base_pointer;
		write(stack, return_address, function(read<A>(stack, a_address)));
	}

	template <typename ExecutionContext>
	auto destroy_variable(int address, complete::TypeId type, ProgramStack & stack, ExecutionContext context) noexcept
		-> expected<void, UnmetPrecondition>
	{
		FunctionId const destructor = destructor_for(context.program, type);
		if (destructor != invalid_function_id)
		{
			try_call_void(call_function(destructor, stack, context, 0, [address](int parameters_start, ProgramStack & stack)
			{
				write(stack, parameters_start, pointer_at_address(stack, address));
			}));
		}

		return success;
	}

	template <typename ExecutionContext>
	auto destroy_variables_in_scope_up_to(complete::Scope const & scope, int destroyed_stack_frame_size, ProgramStack & stack, ExecutionContext context) -> void
	{
		int const stack_frame_start = stack.base_pointer;
		for (auto it = scope.variables.rbegin(); it != scope.variables.rend(); ++it)
		{
			complete::Variable const & var = *it;
			if (var.offset < destroyed_stack_frame_size)
				destroy_variable(stack_frame_start + var.offset, var.type, stack, context);
		}
	}

	template <typename ExecutionContext>
	auto destroy_all_variables_in_scope(complete::Scope const & scope, ProgramStack & stack, ExecutionContext context) -> void
	{
		destroy_variables_in_scope_up_to(scope, scope.stack_frame_size + 1, stack, context);
	}

	template <typename ExecutionContext>
	auto copy_variable(int from, int to, complete::TypeId type, ProgramStack & stack, ExecutionContext context) noexcept
		-> expected<void, UnmetPrecondition>
	{
		FunctionId const copy_constructor = copy_constructor_for(context.program, type);
		assert(copy_constructor != deleted_function_id);
		if (copy_constructor == invalid_function_id)
		{
			memcpy(pointer_at_address(stack, to), pointer_at_address(stack, from), type_size(context.program, type));
		}
		else
		{
			try_call_void(call_function(copy_constructor, stack, context, to, [from](int parameters_start, ProgramStack & stack)
			{
				write(stack, parameters_start, pointer_at_address(stack, from));
			}));
		}

		return success;
	}

	template <typename ExecutionContext>
	auto call_function_with_parameters_already_set(FunctionId function_id, ProgramStack & stack, ExecutionContext context, int return_address) noexcept
		-> expected<void, UnmetPrecondition>
	{
		if (function_id.type == FunctionId::Type::intrinsic)
		{
			int const stack_top = stack.top_pointer;

			switch (function_id.index)
			{
				case 0: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a + b; }); break;
				case 1: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a - b; }); break;
				case 2: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a * b; }); break;
				case 3: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a / b; }); break;
				case 4: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a % b; }); break;
				case 5: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> bool { return a == b; }); break;
				case 6: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> order_t { return a - b; }); break; // <=>
				case 7: call_intrinsic_function(stack, return_address, +[](int8_t a) -> int8_t { return -a; }); break;
				case 8: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a & b; }); break;
				case 9: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a | b; }); break;
				case 10: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a ^ b; }); break;
				case 11: call_intrinsic_function(stack, return_address, +[](int8_t a) -> int8_t { return ~a; }); break;
				case 12: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a >> b; }); break;
				case 13: call_intrinsic_function(stack, return_address, +[](int8_t a, int8_t b) -> int8_t { return a << b; }); break;

				case 14: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a + b; }); break;
				case 15: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a - b; }); break;
				case 16: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a * b; }); break;
				case 17: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a / b; }); break;
				case 18: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a % b; }); break;
				case 19: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> bool { return a == b; }); break;
				case 20: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> order_t { return a - b; }); break; // <=>
				case 21: call_intrinsic_function(stack, return_address, +[](int16_t a) -> int16_t { return -a; }); break;
				case 22: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a & b; }); break;
				case 23: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a | b; }); break;
				case 24: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a ^ b; }); break;
				case 25: call_intrinsic_function(stack, return_address, +[](int16_t a) -> int16_t { return ~a; }); break;
				case 26: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a >> b; }); break;
				case 27: call_intrinsic_function(stack, return_address, +[](int16_t a, int16_t b) -> int16_t { return a << b; }); break;

				case 28: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a + b; }); break;
				case 29: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a - b; }); break;
				case 30: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a * b; }); break;
				case 31: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a / b; }); break;
				case 32: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a % b; }); break;
				case 33: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> bool { return a == b; }); break;
				case 34: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> order_t { return a - b; }); break; // <=>
				case 35: call_intrinsic_function(stack, return_address, +[](int32_t a) -> int32_t { return -a; }); break;
				case 36: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a & b; }); break;
				case 37: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a | b; }); break;
				case 38: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a ^ b; }); break;
				case 39: call_intrinsic_function(stack, return_address, +[](int32_t a) -> int32_t { return ~a; }); break;
				case 40: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a >> b; }); break;
				case 41: call_intrinsic_function(stack, return_address, +[](int32_t a, int32_t b) -> int32_t { return a << b; }); break;

				case 42: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a + b; }); break;
				case 43: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a - b; }); break;
				case 44: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a * b; }); break;
				case 45: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a / b; }); break;
				case 46: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a % b; }); break;
				case 47: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> bool { return a == b; }); break;
				case 48: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> order_t { return order_t(a - b); }); break; // <=>
				case 49: call_intrinsic_function(stack, return_address, +[](int64_t a) -> int64_t { return -a; }); break;
				case 50: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a & b; }); break;
				case 51: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a | b; }); break;
				case 52: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a ^ b; }); break;
				case 53: call_intrinsic_function(stack, return_address, +[](int64_t a) -> int64_t { return ~a; }); break;
				case 54: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a >> b; }); break;
				case 55: call_intrinsic_function(stack, return_address, +[](int64_t a, int64_t b) -> int64_t { return a << b; }); break;

				case 56: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a + b; }); break;
				case 57: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a - b; }); break;
				case 58: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a * b; }); break;
				case 59: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a / b; }); break;
				case 60: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a % b; }); break;
				case 61: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> bool { return a == b; }); break;
				case 62: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> order_t { return int(a) - int(b); }); break; // <=>
				case 63: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a & b; }); break;
				case 64: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a | b; }); break;
				case 65: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a ^ b; }); break;
				case 66: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> uint8_t { return ~a; }); break;
				case 67: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a >> b; }); break;
				case 68: call_intrinsic_function(stack, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a << b; }); break;

				case 69: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a + b; }); break;
				case 70: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a - b; }); break;
				case 71: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a * b; }); break;
				case 72: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a / b; }); break;
				case 73: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a % b; }); break;
				case 74: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> bool { return a == b; }); break;
				case 75: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> order_t { return int(a) - int(b); }); break; // <=>
				case 76: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a & b; }); break;
				case 77: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a | b; }); break;
				case 78: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a ^ b; }); break;
				case 79: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> uint16_t { return ~a; }); break;
				case 80: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a >> b; }); break;
				case 81: call_intrinsic_function(stack, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a << b; }); break;

				case 83: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a - b; }); break;
				case 82: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a + b; }); break;
				case 84: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a * b; }); break;
				case 85: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a / b; }); break;
				case 86: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a % b; }); break;
				case 87: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> bool { return a == b; }); break;
				case 88: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> order_t { return int(a) - int(b); }); break; // <=>
				case 89: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a & b; }); break;
				case 90: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a | b; }); break;
				case 91: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a ^ b; }); break;
				case 92: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> uint32_t { return ~a; }); break;
				case 93: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a >> b; }); break;
				case 94: call_intrinsic_function(stack, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a << b; }); break;

				case 95: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a + b; }); break;
				case 96: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a - b; }); break;
				case 97: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a * b; }); break;
				case 98: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a / b; }); break;
				case 99: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a % b; }); break;
				case 100: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> bool { return a == b; }); break;
				case 101: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> order_t { return int(a) - int(b); }); break; // <=>
				case 102: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a & b; }); break;
				case 103: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a | b; }); break;
				case 104: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a ^ b; }); break;
				case 105: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> uint64_t { return ~a; }); break;
				case 106: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a >> b; }); break;
				case 107: call_intrinsic_function(stack, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a << b; }); break;

				case 108: call_intrinsic_function(stack, return_address, +[](float a, float b) -> float { return a + b; }); break;
				case 109: call_intrinsic_function(stack, return_address, +[](float a, float b) -> float { return a - b; }); break;
				case 110: call_intrinsic_function(stack, return_address, +[](float a, float b) -> float { return a * b; }); break;
				case 111: call_intrinsic_function(stack, return_address, +[](float a, float b) -> float { return a / b; }); break;
				case 112: call_intrinsic_function(stack, return_address, +[](float a, float b) -> bool { return a == b; }); break;
				case 113: call_intrinsic_function(stack, return_address, +[](float a, float b) -> float { return a - b; }); break; // <=>
				case 114: call_intrinsic_function(stack, return_address, +[](float a) -> float { return -a; }); break;

				case 115: call_intrinsic_function(stack, return_address, +[](double a, double b) -> double { return a + b; }); break;
				case 116: call_intrinsic_function(stack, return_address, +[](double a, double b) -> double { return a - b; }); break;
				case 117: call_intrinsic_function(stack, return_address, +[](double a, double b) -> double { return a * b; }); break;
				case 118: call_intrinsic_function(stack, return_address, +[](double a, double b) -> double { return a / b; }); break;
				case 119: call_intrinsic_function(stack, return_address, +[](double a, double b) -> bool { return a == b; }); break;
				case 120: call_intrinsic_function(stack, return_address, +[](double a, double b) -> float { return float(a - b); }); break; // <=>
				case 121: call_intrinsic_function(stack, return_address, +[](double a) -> double { return -a; }); break;

				case 122: call_intrinsic_function(stack, return_address, +[](bool a, bool b) -> bool { return a && b; }); break;
				case 123: call_intrinsic_function(stack, return_address, +[](bool a, bool b) -> bool { return a || b; }); break;
				case 124: call_intrinsic_function(stack, return_address, +[](bool a, bool b) -> bool { return a != b; }); break;
				case 125: call_intrinsic_function(stack, return_address, +[](bool a) -> bool { return !a; }); break;
				case 126: call_intrinsic_function(stack, return_address, +[](bool a, bool b) -> bool { return a == b; }); break;

				// Conversions
				case 127: call_intrinsic_function(stack, return_address, +[](int16_t a) -> int8_t { return int8_t(a); }); break;
				case 128: call_intrinsic_function(stack, return_address, +[](int32_t a) -> int8_t { return int8_t(a); }); break;
				case 129: call_intrinsic_function(stack, return_address, +[](int64_t a) -> int8_t { return int8_t(a); }); break;
				case 130: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> int8_t { return int8_t(a); }); break;
				case 131: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> int8_t { return int8_t(a); }); break;
				case 132: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> int8_t { return int8_t(a); }); break;
				case 133: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> int8_t { return int8_t(a); }); break;
				case 134: call_intrinsic_function(stack, return_address, +[](float a) -> int8_t { return int8_t(a); }); break;
				case 135: call_intrinsic_function(stack, return_address, +[](double a) -> int8_t { return int8_t(a); }); break;

				case 136: call_intrinsic_function(stack, return_address, +[](int8_t a) -> int16_t { return int16_t(a); }); break;
				case 137: call_intrinsic_function(stack, return_address, +[](int32_t a) -> int16_t { return int16_t(a); }); break;
				case 138: call_intrinsic_function(stack, return_address, +[](int64_t a) -> int16_t { return int16_t(a); }); break;
				case 139: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> int16_t { return int16_t(a); }); break;
				case 140: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> int16_t { return int16_t(a); }); break;
				case 141: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> int16_t { return int16_t(a); }); break;
				case 142: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> int16_t { return int16_t(a); }); break;
				case 143: call_intrinsic_function(stack, return_address, +[](float a) -> int16_t { return int16_t(a); }); break;
				case 144: call_intrinsic_function(stack, return_address, +[](double a) -> int16_t { return int16_t(a); }); break;

				case 145: call_intrinsic_function(stack, return_address, +[](int8_t a) -> int32_t { return int32_t(a); }); break;
				case 146: call_intrinsic_function(stack, return_address, +[](int16_t a) -> int32_t { return int32_t(a); }); break;
				case 147: call_intrinsic_function(stack, return_address, +[](int64_t a) -> int32_t { return int32_t(a); }); break;
				case 148: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> int32_t { return int32_t(a); }); break;
				case 149: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> int32_t { return int32_t(a); }); break;
				case 150: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> int32_t { return int32_t(a); }); break;
				case 151: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> int32_t { return int32_t(a); }); break;
				case 152: call_intrinsic_function(stack, return_address, +[](float a) -> int32_t { return int32_t(a); }); break;
				case 153: call_intrinsic_function(stack, return_address, +[](double a) -> int32_t { return int32_t(a); }); break;

				case 154: call_intrinsic_function(stack, return_address, +[](int8_t a) -> int64_t { return int64_t(a); }); break;
				case 155: call_intrinsic_function(stack, return_address, +[](int16_t a) -> int64_t { return int64_t(a); }); break;
				case 156: call_intrinsic_function(stack, return_address, +[](int32_t a) -> int64_t { return int64_t(a); }); break;
				case 157: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> int64_t { return int64_t(a); }); break;
				case 158: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> int64_t { return int64_t(a); }); break;
				case 159: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> int64_t { return int64_t(a); }); break;
				case 160: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> int64_t { return int64_t(a); }); break;
				case 161: call_intrinsic_function(stack, return_address, +[](float a) -> int64_t { return int64_t(a); }); break;
				case 162: call_intrinsic_function(stack, return_address, +[](double a) -> int64_t { return int64_t(a); }); break;

				case 163: call_intrinsic_function(stack, return_address, +[](int8_t a) -> uint8_t { return uint8_t(a); }); break;
				case 164: call_intrinsic_function(stack, return_address, +[](int16_t a) -> uint8_t { return uint8_t(a); }); break;
				case 165: call_intrinsic_function(stack, return_address, +[](int32_t a) -> uint8_t { return uint8_t(a); }); break;
				case 166: call_intrinsic_function(stack, return_address, +[](int64_t a) -> uint8_t { return uint8_t(a); }); break;
				case 167: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> uint8_t { return uint8_t(a); }); break;
				case 168: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> uint8_t { return uint8_t(a); }); break;
				case 169: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> uint8_t { return uint8_t(a); }); break;
				case 170: call_intrinsic_function(stack, return_address, +[](float a) -> uint8_t { return uint8_t(a); }); break;
				case 171: call_intrinsic_function(stack, return_address, +[](double a) -> uint8_t { return uint8_t(a); }); break;

				case 172: call_intrinsic_function(stack, return_address, +[](int8_t a) -> uint16_t { return uint16_t(a); }); break;
				case 173: call_intrinsic_function(stack, return_address, +[](int16_t a) -> uint16_t { return uint16_t(a); }); break;
				case 174: call_intrinsic_function(stack, return_address, +[](int32_t a) -> uint16_t { return uint16_t(a); }); break;
				case 175: call_intrinsic_function(stack, return_address, +[](int64_t a) -> uint16_t { return uint16_t(a); }); break;
				case 176: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> uint16_t { return uint16_t(a); }); break;
				case 177: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> uint16_t { return uint16_t(a); }); break;
				case 178: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> uint16_t { return uint16_t(a); }); break;
				case 179: call_intrinsic_function(stack, return_address, +[](float a) -> uint16_t { return uint16_t(a); }); break;
				case 180: call_intrinsic_function(stack, return_address, +[](double a) -> uint16_t { return uint16_t(a); }); break;

				case 181: call_intrinsic_function(stack, return_address, +[](int8_t a) -> uint32_t { return uint32_t(a); }); break;
				case 182: call_intrinsic_function(stack, return_address, +[](int16_t a) -> uint32_t { return uint32_t(a); }); break;
				case 183: call_intrinsic_function(stack, return_address, +[](int32_t a) -> uint32_t { return uint32_t(a); }); break;
				case 184: call_intrinsic_function(stack, return_address, +[](int64_t a) -> uint32_t { return uint32_t(a); }); break;
				case 185: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> uint32_t { return uint32_t(a); }); break;
				case 186: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> uint32_t { return uint32_t(a); }); break;
				case 187: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> uint32_t { return uint32_t(a); }); break;
				case 188: call_intrinsic_function(stack, return_address, +[](float a) -> uint32_t { return uint32_t(a); }); break;
				case 189: call_intrinsic_function(stack, return_address, +[](double a) -> uint32_t { return uint32_t(a); }); break;

				case 190: call_intrinsic_function(stack, return_address, +[](int8_t a) -> uint64_t { return uint64_t(a); }); break;
				case 191: call_intrinsic_function(stack, return_address, +[](int16_t a) -> uint64_t { return uint64_t(a); }); break;
				case 192: call_intrinsic_function(stack, return_address, +[](int32_t a) -> uint64_t { return uint64_t(a); }); break;
				case 193: call_intrinsic_function(stack, return_address, +[](int64_t a) -> uint64_t { return uint64_t(a); }); break;
				case 194: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> uint64_t { return uint64_t(a); }); break;
				case 195: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> uint64_t { return uint64_t(a); }); break;
				case 196: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> uint64_t { return uint64_t(a); }); break;
				case 197: call_intrinsic_function(stack, return_address, +[](float a) -> uint64_t { return uint64_t(a); }); break;
				case 198: call_intrinsic_function(stack, return_address, +[](double a) -> uint64_t { return uint64_t(a); }); break;

				case 199: call_intrinsic_function(stack, return_address, +[](int8_t a) -> float { return float(a); }); break;
				case 200: call_intrinsic_function(stack, return_address, +[](int16_t a) -> float { return float(a); }); break;
				case 201: call_intrinsic_function(stack, return_address, +[](int32_t a) -> float { return float(a); }); break;
				case 202: call_intrinsic_function(stack, return_address, +[](int64_t a) -> float { return float(a); }); break;
				case 203: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> float { return float(a); }); break;
				case 204: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> float { return float(a); }); break;
				case 205: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> float { return float(a); }); break;
				case 206: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> float { return float(a); }); break;
				case 207: call_intrinsic_function(stack, return_address, +[](double a) -> float { return float(a); }); break;

				case 208: call_intrinsic_function(stack, return_address, +[](int8_t a) -> double { return double(a); }); break;
				case 209: call_intrinsic_function(stack, return_address, +[](int16_t a) -> double { return double(a); }); break;
				case 210: call_intrinsic_function(stack, return_address, +[](int32_t a) -> double { return double(a); }); break;
				case 211: call_intrinsic_function(stack, return_address, +[](int64_t a) -> double { return double(a); }); break;
				case 212: call_intrinsic_function(stack, return_address, +[](uint8_t a) -> double { return double(a); }); break;
				case 213: call_intrinsic_function(stack, return_address, +[](uint16_t a) -> double { return double(a); }); break;
				case 214: call_intrinsic_function(stack, return_address, +[](uint32_t a) -> double { return double(a); }); break;
				case 215: call_intrinsic_function(stack, return_address, +[](uint64_t a) -> double { return double(a); }); break;
				case 216: call_intrinsic_function(stack, return_address, +[](float a) -> double { return double(a); }); break;
			}

			free_up_to(stack, stack_top);
		}
		else if (function_id.type == FunctionId::Type::program)
		{
			complete::Function const & func = context.program.functions[function_id.index];

			// Run the preconditions
			int const parameters_start = stack.base_pointer;
			int const precondition_count = static_cast<int>(func.preconditions.size());
			for (int i = 0; i < precondition_count; ++i)
			{
				try_call_decl(int const precondition_return_address, eval_expression(func.preconditions[i], stack, context));
				bool const precondition_ok = read<bool>(stack, precondition_return_address);
				if (!precondition_ok)
					return Error(UnmetPrecondition{ function_id, i });
			}
			stack.top_pointer = parameters_start + func.stack_frame_size;

			bool returned = false;

			// Run the function.
			for (auto const & statement : func.statements)
			{
				try_call_decl(ControlFlow const cf, run_statement(statement, stack, context, return_address));
				if (cf.type == ControlFlowType::Return)
				{
					returned = true;
					destroy_variables_in_scope_up_to(func, cf.destroyed_stack_frame_size, stack, context);
					break;
				}
			}

			// If function did not return early, destroy all variables at the end of the function.
			if (!returned)
				destroy_all_variables_in_scope(func, stack, context);
		}
		else
		{
			complete::ExternFunction const & func = context.program.extern_functions[function_id.index];
			return call_extern_function(func, stack, context, return_address);
		}

		return success;
	}

	template <typename ExecutionContext, typename SetParameters>
	auto call_function(FunctionId function_id, ProgramStack & stack, ExecutionContext context, int return_address, SetParameters set_parameters) noexcept
		-> expected<void, UnmetPrecondition>
	{
		// Save previous stack frame bounds.
		int const prev_ebp = stack.base_pointer;
		int const prev_esp = stack.top_pointer;

		int const destructor_stack_frame_size = stack_frame_size(context.program, function_id);
		int const destructor_stack_frame_alignment = parameter_alignment(context.program, function_id);

		int const parameters_start = alloc(stack, destructor_stack_frame_size, destructor_stack_frame_alignment);
		set_parameters(parameters_start, stack);

		// Move the stack pointers.
		stack.base_pointer = parameters_start;
		stack.top_pointer = parameters_start + destructor_stack_frame_size;

		try_call_void(call_function_with_parameters_already_set(function_id, stack, context, return_address));

		// Restore previous stack frame.
		stack.top_pointer = prev_esp;
		stack.base_pointer = prev_ebp;

		return success;
	}

	template <typename ExecutionContext>
	auto call_function(FunctionId function_id, span<complete::Expression const> parameters, ProgramStack & stack, ExecutionContext context, int return_address) noexcept
		-> expected<void, UnmetPrecondition>
	{
		int const param_size = stack_frame_size(context.program, function_id);
		int const param_alignment = parameter_alignment(context.program, function_id);
		auto const parameter_types = parameter_types_of(context.program, function_id);

		// Save previous stack frame bounds.
		int const prev_ebp = stack.base_pointer;
		int const prev_esp = stack.top_pointer;

		// Allocate memory for temporaries passed by reference.
		int size_of_temporaries_passed_by_reference = 0;
		int alignment_of_temporaries_passed_by_reference = 1;
		for (size_t i = 0; i < parameters.size(); ++i)
		{
			complete::TypeId const param_type = expression_type_id(parameters[i], context.program);
			if (!param_type.is_reference && parameter_types[i].is_reference)
			{
				complete::Type const & param_type_data = type_with_id(context.program, param_type);
				size_of_temporaries_passed_by_reference = add_size_aligned(size_of_temporaries_passed_by_reference, param_type_data.size, param_type_data.alignment);
				alignment_of_temporaries_passed_by_reference = std::max(alignment_of_temporaries_passed_by_reference, param_type_data.alignment);
			}
		}
		int const temporaries_start = alloc(stack, size_of_temporaries_passed_by_reference, alignment_of_temporaries_passed_by_reference);

		// Allocate memory for the parameters.
		int const parameters_start = alloc(stack, param_size, param_alignment);

		// Evaluate the expressions that yield the parameters of the function.
		int const parameters_size = static_cast<int>(parameters.size());
		for (
			int i = 0, next_parameter_address = parameters_start, next_temporary_address = temporaries_start;
			i < parameters_size;
			++i
			)
		{
			complete::TypeId const param_type = expression_type_id(parameters[i], context.program);
			if (!param_type.is_reference && parameter_types[i].is_reference)
			{
				try_call_void(eval_expression(parameters[i], stack, context, next_temporary_address));
				write(stack, next_parameter_address, pointer_at_address(stack, next_temporary_address));
				next_temporary_address += expression_type_size(parameters[i], context.program);
				next_parameter_address += sizeof(void *);
			}
			else
			{
				try_call_void(eval_expression(parameters[i], stack, context, next_parameter_address));
				next_parameter_address += expression_type_size(parameters[i], context.program);
			}
		}

		// Move the stack pointers.
		stack.base_pointer = parameters_start;
		stack.top_pointer = parameters_start + param_size;

		try_call_void(call_function_with_parameters_already_set(function_id, stack, context, return_address));

		// Destroy temporaries.
		for (int i = 0, next_temporary_address = temporaries_start; i < parameters_size; ++i)
		{
			complete::TypeId const param_type = expression_type_id(parameters[i], context.program);
			if (!param_type.is_reference && parameter_types[i].is_reference)
			{
				destroy_variable(next_temporary_address, param_type, stack, context);
				next_temporary_address += expression_type_size(parameters[i], context.program);
			}
		}

		// Restore previous stack frame.
		stack.top_pointer = prev_esp;
		stack.base_pointer = prev_ebp;

		return success;
	}

	template <typename ExecutionContext>
	auto eval_expression(complete::Expression const & tree, ProgramStack & stack, ExecutionContext context) noexcept -> expected<int, UnmetPrecondition>
	{
		complete::TypeId const & expr_type = expression_type_id(tree, context.program);
		int const address = alloc(stack, type_size(context.program, expr_type), type_alignment(context.program, expr_type));
		try_call_void(eval_expression(tree, stack, context, address));
		return address;
	}

	template <typename ExecutionContext>
	auto eval_expression_and_discard_result(complete::Expression const & tree, ProgramStack & stack, ExecutionContext context) noexcept -> expected<void, UnmetPrecondition>
	{
		StackGuard const stack_guard(stack);
		try_call_decl(int const discarded_variable_address, eval_expression(tree, stack, context));
		complete::TypeId const & discarded_variable_type = expression_type_id(tree, context.program);
		destroy_variable(discarded_variable_address, discarded_variable_type, stack, context);
		return success;
	}

	namespace detail
	{
		inline auto eval_type_literal_expression(ProgramStack &, RuntimeContext, int)
		{
			return [](complete::expression::Literal<complete::TypeId>) { declare_unreachable(); };
		}
		inline auto eval_type_literal_expression(ProgramStack & stack, CompileTimeContext, int return_address)
		{
			return [&stack, return_address](complete::expression::Literal<complete::TypeId> literal) { write(stack, return_address, literal.value); };
		}

		inline auto eval_compiles_expression(ProgramStack &, RuntimeContext, int) noexcept
		{
			return [](complete::expression::Compiles const &) { declare_unreachable(); };
		}
		auto eval_compiles_expression_impl(complete::expression::Compiles const & compiles_expr, ProgramStack & stack, CompileTimeContext context, int return_address) noexcept
			-> expected<void, UnmetPrecondition>;
		inline auto eval_compiles_expression(ProgramStack & stack, CompileTimeContext context, int return_address) noexcept
		{
			return [=, &stack](complete::expression::Compiles const & compiles_expr) { return eval_compiles_expression_impl(compiles_expr, stack, context, return_address); };
		}
	} // namespace detail

	template <typename ExecutionContext>
	auto eval_expression(complete::Expression const & expr, ProgramStack & stack, ExecutionContext context, int return_address) noexcept -> expected<void, UnmetPrecondition>
	{
		using namespace complete;

		auto const visitor = overload_default_ret(expected<void, UnmetPrecondition>(success),
			[&](expression::Literal<int> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<float> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<bool> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<char_t> literal) { write(stack, return_address, literal.value); },
			[&](expression::Literal<null_t>) {},
			detail::eval_type_literal_expression(stack, context, return_address),
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
				try_call_decl(int const owner_address, eval_expression(*var_node.owner, stack, context));
				complete::TypeId const owner_type = expression_type_id(*var_node.owner, context.program);
				if (owner_type.is_reference)
				{
					char * const owner_ptr = read<char *>(stack, owner_address);
					write(stack, return_address, owner_ptr + var_node.variable_offset);
				}
				// If the owner is an rvalue, return the member by value.
				else
				{
					int const variable_size = type_size(context.program, var_node.variable_type);
					memcpy(pointer_at_address(stack, return_address), pointer_at_address(stack, owner_address + var_node.variable_offset), variable_size);
					destroy_variable(owner_address, owner_type, stack, context);
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
				try_call_decl(int const pointer_address, eval_expression(*deref_node.expression, stack, context));
				auto const pointer = read<void const *>(stack, pointer_address);
				memcpy(pointer_at_address(stack, return_address), pointer, type_size(context.program, deref_node.return_type));
				return success;
			},
			[&](expression::ReinterpretCast const & addressof_node)
			{
				return eval_expression(*addressof_node.operand, stack, context, return_address);
			},
			[&](expression::Subscript const & subscript_node) -> expected<void, UnmetPrecondition>
			{
				TypeId const array_type_id = expression_type_id(*subscript_node.array, context.program);
				Type const & array_type = type_with_id(context.program, array_type_id);

				StackGuard const g(stack);
				
				if (array_type_id.is_reference || is_array_pointer(array_type))
				{
					try_call_decl(int const array_address, eval_expression(*subscript_node.array, stack, context));
					try_call_decl(int const index_address, eval_expression(*subscript_node.index, stack, context));
					char const * const array = read<char const *>(stack, array_address);
					int const index = read<int>(stack, index_address);
					int const value_type_size = type_size(context.program, remove_reference(subscript_node.return_type));
					write(stack, return_address, array + index * value_type_size);
				}
				else // array rvalue
				{
					try_call_decl(int const array_address, eval_expression(*subscript_node.array, stack, context));
					try_call_decl(int const index_address, eval_expression(*subscript_node.index, stack, context));
					char const * const array = pointer_at_address(stack, array_address);
					int const index = read<int>(stack, index_address);
					int const value_type_size = type_size(context.program, remove_reference(subscript_node.return_type));
					memcpy(pointer_at_address(stack, return_address), array + index * value_type_size, value_type_size);
					destroy_variable(array_address, array_type_id, stack, context);
				}
				return success;
			},
			[&](expression::FunctionCall const & func_call_node)
			{
				return call_function(func_call_node.function_id, func_call_node.parameters, stack, context, return_address);
			},
			[&](expression::RelationalOperatorCall const & op_node) -> expected<void, UnmetPrecondition>
			{
				if (op_node.op == Operator::not_equal)
				{
					// Call operator ==.
					try_call_void(call_function(op_node.function_id, op_node.parameters, stack, context, return_address));
					// Negate the result.
					write(stack, return_address, !read<bool>(stack, return_address));
				}
				else // We need to call operator <=>, and convert the int it returns into a boolean.
				{
					int const prev_stack_top = stack.top_pointer;
					int const temp_storage = alloc(stack, sizeof(int), alignof(int));
					try_call_void(call_function(op_node.function_id, op_node.parameters, stack, context, temp_storage));

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
				try_call_decl(const int dest_address, eval_expression(*assign_node.destination, stack, context));
				try_call_decl(const int source_address, eval_expression(*assign_node.source, stack, context));
				memcpy(read<void *>(stack, dest_address), pointer_at_address(stack, source_address), expression_type_size(*assign_node.source, context.program));
				destroy_variable(source_address, expression_type_id(*assign_node.source, context.program), stack, context);
				free_up_to(stack, dest_address);
				return success;
			},
			[&](expression::If const & if_node) -> expected<void, UnmetPrecondition>
			{
				try_call_decl(int const result_addr, eval_expression(*if_node.condition, stack, context));
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				Expression const & branch = condition ? *if_node.then_case : *if_node.else_case;
				try_call_void(eval_expression(branch, stack, context, return_address));
				return success;
			},
			[&](expression::StatementBlock const & block_node) -> expected<void, UnmetPrecondition>
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the function.
				for (auto const & statement : block_node.statements)
				{
					try_call_decl(ControlFlow const cf, run_statement(statement, stack, context, return_address));
					if (cf.type == ControlFlowType::Return)
					{
						destroy_variables_in_scope_up_to(block_node.scope, cf.destroyed_stack_frame_size, stack, context);
						break;
					}
				}
				return success;
			},
			[&](expression::Constructor const & ctor_node) -> expected<void, UnmetPrecondition>
			{
				Type const & constructed_type = type_with_id(context.program, ctor_node.constructed_type);

				if (is_struct(constructed_type))
				{
					Struct const & struct_data = *struct_for_type(context.program, constructed_type);
					for (size_t i = 0; i < struct_data.member_variables.size(); ++i)
						try_call_void(eval_expression(ctor_node.parameters[i], stack, context, return_address + struct_data.member_variables[i].offset));
				}
				else if (is_array(constructed_type))
				{
					Type::Array const array = std::get<Type::Array>(type_with_id(context.program, ctor_node.constructed_type).extra_data);
					int const value_type_size = type_size(context.program, array.value_type);

					// Fill constructor
					if (ctor_node.parameters.size() == 1)
					{
						try_call_void(eval_expression(ctor_node.parameters[0], stack, context, return_address));
						for (int i = 1; i < array.size; ++i)
							copy_variable(return_address, return_address + value_type_size * i, array.value_type, stack, context);
					}
					// Regular constructor
					else
					{
						for (int i = 0; i < array.size; ++i)
							try_call_void(eval_expression(ctor_node.parameters[i], stack, context, return_address + value_type_size * i));
					}
				}
				else
				{
					declare_unreachable();
				}
				return success;
			},
			detail::eval_compiles_expression(stack, context, return_address)
		);
		return std::visit(visitor, expr.as_variant());
	}

	template <typename ExecutionContext>
	auto run_statement(complete::Statement const & tree, ProgramStack & stack, ExecutionContext context, int return_address) noexcept
		-> expected<ControlFlow, UnmetPrecondition>
	{
		using namespace complete;

		auto const visitor = overload(
			[&](statement::VariableDeclaration const & node) -> expected<ControlFlow, UnmetPrecondition>
			{
				int const address = stack.base_pointer + node.variable_offset;
				try_call_void(eval_expression(node.assigned_expression, stack, context, address));
				return ControlFlow_Nothing;
			},
			[&](statement::ExpressionStatement const & expr_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression_and_discard_result(expr_node.expression, stack, context));
				return ControlFlow_Nothing;
			},
			[&](statement::Return const & return_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression(return_node.returned_expression, stack, context, return_address));
				return ControlFlow{ControlFlowType::Return, return_node.destroyed_stack_frame_size};
			},
			[&](statement::If const & if_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_decl(int const result_addr, eval_expression(if_node.condition, stack, context));
				bool const condition = read<bool>(stack, result_addr);
				free_up_to(stack, result_addr);

				Statement const * const branch = condition ? if_node.then_case.get() : if_node.else_case.get();
				if (branch)
					return run_statement(*branch, stack, context, return_address);
				else
					return ControlFlow_Nothing;
			},
			[&](statement::StatementBlock const & block_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the statements.
				for (auto const & statement : block_node.statements)
				{
					try_call_decl(ControlFlow const cf, run_statement(statement, stack, context, return_address));
					if (cf.type == ControlFlowType::Return || cf.type == ControlFlowType::Break || cf.type == ControlFlowType::Continue)
					{
						destroy_variables_in_scope_up_to(block_node.scope, cf.destroyed_stack_frame_size, stack, context);
						return cf;
					}
				}

				destroy_all_variables_in_scope(block_node.scope, stack, context);
				return ControlFlow_Nothing;
			},
			[&](statement::While const & while_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				for (;;)
				{
					try_call_decl(int const result_addr, eval_expression(while_node.condition, stack, context));
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						try_call_decl(ControlFlow const cf, run_statement(*while_node.body, stack, context, return_address));
						if (cf.type == ControlFlowType::Return)
							return cf;
						if (cf.type == ControlFlowType::Break)
							return ControlFlow_Nothing;
					}
					else
					{
						return ControlFlow_Nothing;
					}
				}
			},
			[&](statement::For const & for_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				// Allocate stack frame for the scope.
				StackGuard const stack_guard(stack);
				stack.top_pointer += for_node.scope.stack_frame_size;

				// Run init statement.
				try_call_void(run_statement(*for_node.init_statement, stack, context, return_address));

				for (;;)
				{
					// Check condition.
					try_call_decl(int const result_addr, eval_expression(for_node.condition, stack, context));
					bool const condition = read<bool>(stack, result_addr);
					free_up_to(stack, result_addr);

					if (condition)
					{
						// Run body.
						try_call_decl(ControlFlow const cf, run_statement(*for_node.body, stack, context, return_address));
						if (cf.type == ControlFlowType::Return)
						{
							destroy_variables_in_scope_up_to(for_node.scope, cf.destroyed_stack_frame_size, stack, context);
							return cf;
						}
						if (cf.type == ControlFlowType::Break)
						{
							destroy_variables_in_scope_up_to(for_node.scope, cf.destroyed_stack_frame_size, stack, context);
							return ControlFlow_Nothing;
						}

						// Run end expression.
						try_call_void(eval_expression_and_discard_result(for_node.end_expression, stack, context));
					}
					else
					{
						destroy_all_variables_in_scope(for_node.scope, stack, context);
						return ControlFlow_Nothing;
					}
				}
			},
			[&](statement::Break const & break_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				return ControlFlow{ControlFlowType::Break, break_node.destroyed_stack_frame_size};
			},
			[&](statement::Continue const & continue_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				return ControlFlow{ControlFlowType::Continue, continue_node.destroyed_stack_frame_size};
			}
		);
		return std::visit(visitor, tree.as_variant());
	}

	template <typename T>
	auto evaluate_constant_expression_as(
		complete::Expression const & expression,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		instantiation::ScopeStack & scope_stack,
		complete::Program & program) noexcept
		-> expected<T, UnmetPrecondition>
	{
		T result;
		try_call_void(evaluate_constant_expression(expression, template_parameters, scope_stack, program, &result));
		return result;
	}

} // namespace interpreter
