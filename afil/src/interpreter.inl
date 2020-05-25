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

	template <typename ExecutionContext, typename Ret, typename A, typename B>
	[[nodiscard]] auto call_intrinsic_function(span<complete::Expression const> parameters, ProgramStack & stack, ExecutionContext context, int return_address, Ret(*function)(A, B)) 
		-> expected<void, UnmetPrecondition>
	{
		try_call_decl(int const a_address, eval_expression(parameters[0], stack, context));
		try_call_decl(int const b_address, eval_expression(parameters[1], stack, context));
		write(stack, return_address, function(read<A>(stack, a_address), read<B>(stack, b_address)));
		return success;
	}

	template <typename ExecutionContext, typename Ret, typename A>
	[[nodiscard]] auto call_intrinsic_function(span<complete::Expression const> parameters, ProgramStack & stack, ExecutionContext context, int return_address, Ret(*function)(A))
		-> expected<void, UnmetPrecondition>
	{
		try_call_decl(int const a_address, eval_expression(parameters[0], stack, context));
		write(stack, return_address, function(read<A>(stack, a_address)));
		return success;
	}

	template <typename ExecutionContext>
	auto call_function(FunctionId function_id, span<complete::Expression const> parameters, ProgramStack & stack, ExecutionContext context, int return_address) noexcept 
		-> expected<void, UnmetPrecondition>
	{
		if (function_id.type == FunctionId::Type::intrinsic)
		{
			int const stack_top = stack.top_pointer;

			switch (function_id.index)
			{
				case 0: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a + b; })); break;
				case 1: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a - b; })); break;
				case 2: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a * b; })); break;
				case 3: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a / b; })); break;
				case 4: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a % b; })); break;
				case 5: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> bool { return a == b; })); break;
				case 6: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> order_t { return a - b; })); break; // <=>
				case 7: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> int8_t { return -a; })); break;
				case 8: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a & b; })); break;
				case 9: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a | b; })); break;
				case 10: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a ^ b; })); break;
				case 11: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> int8_t { return ~a; })); break;
				case 12: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a >> b; })); break;
				case 13: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a, int8_t b) -> int8_t { return a << b; })); break;

				case 14: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a + b; })); break;
				case 15: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a - b; })); break;
				case 16: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a * b; })); break;
				case 17: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a / b; })); break;
				case 18: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a % b; })); break;
				case 19: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> bool { return a == b; })); break;
				case 20: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> order_t { return a - b; })); break; // <=>
				case 21: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> int16_t { return -a; })); break;
				case 22: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a & b; })); break;
				case 23: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a | b; })); break;
				case 24: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a ^ b; })); break;
				case 25: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> int16_t { return ~a; })); break;
				case 26: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a >> b; })); break;
				case 27: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a, int16_t b) -> int16_t { return a << b; })); break;

				case 28: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a + b; })); break;
				case 29: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a - b; })); break;
				case 30: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a * b; })); break;
				case 31: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a / b; })); break;
				case 32: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a % b; })); break;
				case 33: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> bool { return a == b; })); break;
				case 34: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> order_t { return a - b; })); break; // <=>
				case 35: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> int32_t { return -a; })); break;
				case 36: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a & b; })); break;
				case 37: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a | b; })); break;
				case 38: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a ^ b; })); break;
				case 39: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> int32_t { return ~a; })); break;
				case 40: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a >> b; })); break;
				case 41: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a, int32_t b) -> int32_t { return a << b; })); break;

				case 42: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a + b; })); break;
				case 43: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a - b; })); break;
				case 44: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a * b; })); break;
				case 45: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a / b; })); break;
				case 46: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a % b; })); break;
				case 47: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> bool { return a == b; })); break;
				case 48: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> order_t { return order_t(a - b); })); break; // <=>
				case 49: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> int64_t { return -a; })); break;
				case 50: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a & b; })); break;
				case 51: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a | b; })); break;
				case 52: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a ^ b; })); break;
				case 53: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> int64_t { return ~a; })); break;
				case 54: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a >> b; })); break;
				case 55: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a, int64_t b) -> int64_t { return a << b; })); break;

				case 56: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a + b; })); break;
				case 57: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a - b; })); break;
				case 58: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a * b; })); break;
				case 59: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a / b; })); break;
				case 60: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a % b; })); break;
				case 61: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> bool { return a == b; })); break;
				case 62: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> order_t { return int(a) - int(b); })); break; // <=>
				case 63: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a & b; })); break;
				case 64: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a | b; })); break;
				case 65: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a ^ b; })); break;
				case 66: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> uint8_t { return ~a; })); break;
				case 67: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a >> b; })); break;
				case 68: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a, uint8_t b) -> uint8_t { return a << b; })); break;

				case 69: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a + b; })); break;
				case 70: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a - b; })); break;
				case 71: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a * b; })); break;
				case 72: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a / b; })); break;
				case 73: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a % b; })); break;
				case 74: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> bool { return a == b; })); break;
				case 75: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> order_t { return int(a) - int(b); })); break; // <=>
				case 76: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a & b; })); break;
				case 77: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a | b; })); break;
				case 78: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a ^ b; })); break;
				case 79: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> uint16_t { return ~a; })); break;
				case 80: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a >> b; })); break;
				case 81: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a, uint16_t b) -> uint16_t { return a << b; })); break;

				case 83: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a - b; })); break;
				case 82: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a + b; })); break;
				case 84: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a * b; })); break;
				case 85: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a / b; })); break;
				case 86: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a % b; })); break;
				case 87: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> bool { return a == b; })); break;
				case 88: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> order_t { return int(a) - int(b); })); break; // <=>
				case 89: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a & b; })); break;
				case 90: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a | b; })); break;
				case 91: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a ^ b; })); break;
				case 92: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> uint32_t { return ~a; })); break;
				case 93: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a >> b; })); break;
				case 94: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a, uint32_t b) -> uint32_t { return a << b; })); break;

				case 95: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a + b; })); break;
				case 96: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a - b; })); break;
				case 97: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a * b; })); break;
				case 98: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a / b; })); break;
				case 99: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a % b; })); break;
				case 100: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> bool { return a == b; })); break;
				case 101: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> order_t { return int(a) - int(b); })); break; // <=>
				case 102: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a & b; })); break;
				case 103: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a | b; })); break;
				case 104: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a ^ b; })); break;
				case 105: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> uint64_t { return ~a; })); break;
				case 106: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a >> b; })); break;
				case 107: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a, uint64_t b) -> uint64_t { return a << b; })); break;

				case 108: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> float { return a + b; })); break;
				case 109: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> float { return a - b; })); break;
				case 110: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> float { return a * b; })); break;
				case 111: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> float { return a / b; })); break;
				case 112: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> bool { return a == b; })); break;
				case 113: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a, float b) -> float { return a - b; })); break; // <=>
				case 114: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> float { return -a; })); break;

				case 115: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> double { return a + b; })); break;
				case 116: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> double { return a - b; })); break;
				case 117: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> double { return a * b; })); break;
				case 118: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> double { return a / b; })); break;
				case 119: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> bool { return a == b; })); break;
				case 120: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a, double b) -> float { return float(a - b); })); break; // <=>
				case 121: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> double { return -a; })); break;

				case 122: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](bool a, bool b) -> bool { return a && b; })); break;
				case 123: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](bool a, bool b) -> bool { return a || b; })); break;
				case 124: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](bool a, bool b) -> bool { return a != b; })); break;
				case 125: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](bool a) -> bool { return !a; })); break;
				case 126: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](bool a, bool b) -> bool { return a == b; })); break;

				// Conversions
				case 127: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> int8_t { return int8_t(a); })); break;
				case 128: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> int8_t { return int8_t(a); })); break;
				case 129: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> int8_t { return int8_t(a); })); break;
				case 130: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> int8_t { return int8_t(a); })); break;
				case 131: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> int8_t { return int8_t(a); })); break;
				case 132: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> int8_t { return int8_t(a); })); break;
				case 133: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> int8_t { return int8_t(a); })); break;
				case 134: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> int8_t { return int8_t(a); })); break;
				case 135: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> int8_t { return int8_t(a); })); break;

				case 136: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> int16_t { return int16_t(a); })); break;
				case 137: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> int16_t { return int16_t(a); })); break;
				case 138: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> int16_t { return int16_t(a); })); break;
				case 139: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> int16_t { return int16_t(a); })); break;
				case 140: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> int16_t { return int16_t(a); })); break;
				case 141: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> int16_t { return int16_t(a); })); break;
				case 142: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> int16_t { return int16_t(a); })); break;
				case 143: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> int16_t { return int16_t(a); })); break;
				case 144: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> int16_t { return int16_t(a); })); break;

				case 145: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> int32_t { return int32_t(a); })); break;
				case 146: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> int32_t { return int32_t(a); })); break;
				case 147: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> int32_t { return int32_t(a); })); break;
				case 148: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> int32_t { return int32_t(a); })); break;
				case 149: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> int32_t { return int32_t(a); })); break;
				case 150: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> int32_t { return int32_t(a); })); break;
				case 151: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> int32_t { return int32_t(a); })); break;
				case 152: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> int32_t { return int32_t(a); })); break;
				case 153: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> int32_t { return int32_t(a); })); break;

				case 154: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> int64_t { return int64_t(a); })); break;
				case 155: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> int64_t { return int64_t(a); })); break;
				case 156: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> int64_t { return int64_t(a); })); break;
				case 157: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> int64_t { return int64_t(a); })); break;
				case 158: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> int64_t { return int64_t(a); })); break;
				case 159: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> int64_t { return int64_t(a); })); break;
				case 160: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> int64_t { return int64_t(a); })); break;
				case 161: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> int64_t { return int64_t(a); })); break;
				case 162: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> int64_t { return int64_t(a); })); break;

				case 163: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> uint8_t { return uint8_t(a); })); break;
				case 164: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> uint8_t { return uint8_t(a); })); break;
				case 165: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> uint8_t { return uint8_t(a); })); break;
				case 166: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> uint8_t { return uint8_t(a); })); break;
				case 167: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> uint8_t { return uint8_t(a); })); break;
				case 168: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> uint8_t { return uint8_t(a); })); break;
				case 169: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> uint8_t { return uint8_t(a); })); break;
				case 170: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> uint8_t { return uint8_t(a); })); break;
				case 171: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> uint8_t { return uint8_t(a); })); break;

				case 172: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> uint16_t { return uint16_t(a); })); break;
				case 173: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> uint16_t { return uint16_t(a); })); break;
				case 174: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> uint16_t { return uint16_t(a); })); break;
				case 175: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> uint16_t { return uint16_t(a); })); break;
				case 176: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> uint16_t { return uint16_t(a); })); break;
				case 177: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> uint16_t { return uint16_t(a); })); break;
				case 178: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> uint16_t { return uint16_t(a); })); break;
				case 179: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> uint16_t { return uint16_t(a); })); break;
				case 180: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> uint16_t { return uint16_t(a); })); break;

				case 181: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> uint32_t { return uint32_t(a); })); break;
				case 182: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> uint32_t { return uint32_t(a); })); break;
				case 183: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> uint32_t { return uint32_t(a); })); break;
				case 184: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> uint32_t { return uint32_t(a); })); break;
				case 185: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> uint32_t { return uint32_t(a); })); break;
				case 186: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> uint32_t { return uint32_t(a); })); break;
				case 187: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> uint32_t { return uint32_t(a); })); break;
				case 188: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> uint32_t { return uint32_t(a); })); break;
				case 189: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> uint32_t { return uint32_t(a); })); break;

				case 190: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> uint64_t { return uint64_t(a); })); break;
				case 191: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> uint64_t { return uint64_t(a); })); break;
				case 192: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> uint64_t { return uint64_t(a); })); break;
				case 193: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> uint64_t { return uint64_t(a); })); break;
				case 194: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> uint64_t { return uint64_t(a); })); break;
				case 195: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> uint64_t { return uint64_t(a); })); break;
				case 196: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> uint64_t { return uint64_t(a); })); break;
				case 197: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> uint64_t { return uint64_t(a); })); break;
				case 198: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> uint64_t { return uint64_t(a); })); break;

				case 199: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> float { return float(a); })); break;
				case 200: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> float { return float(a); })); break;
				case 201: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> float { return float(a); })); break;
				case 202: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> float { return float(a); })); break;
				case 203: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> float { return float(a); })); break;
				case 204: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> float { return float(a); })); break;
				case 205: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> float { return float(a); })); break;
				case 206: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> float { return float(a); })); break;
				case 207: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](double a) -> float { return float(a); })); break;

				case 208: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int8_t a) -> double { return double(a); })); break;
				case 209: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int16_t a) -> double { return double(a); })); break;
				case 210: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int32_t a) -> double { return double(a); })); break;
				case 211: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](int64_t a) -> double { return double(a); })); break;
				case 212: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint8_t a) -> double { return double(a); })); break;
				case 213: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint16_t a) -> double { return double(a); })); break;
				case 214: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint32_t a) -> double { return double(a); })); break;
				case 215: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](uint64_t a) -> double { return double(a); })); break;
				case 216: try_call_void(call_intrinsic_function(parameters, stack, context, return_address, +[](float a) -> double { return double(a); })); break;
			}

			free_up_to(stack, stack_top);
		}
		else if (function_id.type == FunctionId::Type::program)
		{
			complete::Function const & func = context.program.functions[function_id.index];

			int const parameter_size = func.parameter_size;

			// Save previous stack frame bounds.
			int const prev_ebp = stack.base_pointer;
			int const prev_esp = stack.top_pointer;

			// Allocate memory for temporaries passed by reference.
			int size_of_temporaries_passed_by_reference = 0;
			int alignment_of_temporaries_passed_by_reference = 1;
			for (size_t i = 0; i < parameters.size(); ++i)
			{
				complete::TypeId const param_type = expression_type_id(parameters[i], context.program);
				if (!param_type.is_reference && func.variables[i].type.is_reference)
				{
					complete::Type const & param_type_data = type_with_id(context.program, param_type);
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
				complete::TypeId const param_type = expression_type_id(parameters[i], context.program);
				if (!param_type.is_reference && func.variables[i].type.is_reference)
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
			stack.top_pointer = parameters_start + func.stack_frame_size;

			// Run the preconditions
			int const precondition_count = static_cast<int>(func.preconditions.size());
			for (int i = 0; i < precondition_count; ++i)
			{
				try_call_decl(int const precondition_return_address, eval_expression(func.preconditions[i], stack, context));
				bool const precondition_ok = read<bool>(stack, precondition_return_address);
				if (!precondition_ok)
					return Error(UnmetPrecondition{ function_id, i });
			}
			stack.top_pointer = parameters_start + func.stack_frame_size;

			// Run the function.
			for (auto const & statement : func.statements)
			{
				try_call_decl(auto const cf, run_statement(statement, stack, context, return_address));
				if (cf == ControlFlow::Return)
					break;
			}

			// Restore previous stack frame.
			stack.top_pointer = prev_esp;
			stack.base_pointer = prev_ebp;
		}
		else
		{
			complete::ExternFunction const & func = context.program.extern_functions[function_id.index];
			return call_extern_function(func, parameters, stack, context, return_address);
		}

		return success;
	}

	template <typename ExecutionContext>
	auto eval_expression(complete::Expression const & tree, ProgramStack & stack, ExecutionContext context) noexcept -> expected<int, UnmetPrecondition>
	{
		complete::TypeId const & expr_type = expression_type_id(tree, context.program);
		int const address = alloc(stack, type_size(context.program, expr_type));
		try_call_void(eval_expression(tree, stack, context, address));
		return address;
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
			[&](expression::Literal<uninit_t>) {},
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
				if (expression_type_id(*var_node.owner, context.program).is_reference)
				{
					char * const owner_ptr = read<char *>(stack, owner_address);
					write(stack, return_address, owner_ptr + var_node.variable_offset);
				}
				// If the owner is an rvalue, return the member by value.
				else
				{
					int const variable_size = type_size(context.program, var_node.variable_type);
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
					try_call_decl(auto const cf, run_statement(statement, stack, context, return_address));
					if (cf == ControlFlow::Return)
						break;
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
						for (int i = 0; i < array.size; ++i)
							try_call_void(eval_expression(ctor_node.parameters[0], stack, context, return_address + value_type_size * i));
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
				return ControlFlow::Nothing;
			},
			[&](statement::ExpressionStatement const & expr_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression(expr_node.expression, stack, context, stack.top_pointer));
				return ControlFlow::Nothing;
			},
			[&](statement::Return const & return_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				try_call_void(eval_expression(return_node.returned_expression, stack, context, return_address));
				return ControlFlow::Return;
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
					return ControlFlow::Nothing;
			},
			[&](statement::StatementBlock const & block_node) -> expected<ControlFlow, UnmetPrecondition>
			{
				StackGuard const stack_guard(stack);
				stack.top_pointer += block_node.scope.stack_frame_size;

				// Run the statements.
				for (auto const & statement : block_node.statements)
				{
					try_call_decl(auto const cf, run_statement(statement, stack, context, return_address));
					if (cf == ControlFlow::Return || cf == ControlFlow::Break || cf == ControlFlow::Continue)
						return cf;
				}

				return ControlFlow::Nothing;
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
						try_call_decl(auto const cf, run_statement(*while_node.body, stack, context, return_address));
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
						try_call_decl(auto const cf, run_statement(*for_node.body, stack, context, return_address));
						if (cf == ControlFlow::Return)
							return cf;
						if (cf == ControlFlow::Break)
							return ControlFlow::Nothing;

						// Run end expression.
						try_call_void(eval_expression(for_node.end_expression, stack, context, stack.top_pointer));
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
