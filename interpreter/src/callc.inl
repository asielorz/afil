namespace callc
{

	template <typename T>
	constexpr auto type_descriptor_for() noexcept -> TypeDescriptor
	{
		return {
			sizeof(T),
			std::is_same_v<T, float> || std::is_same_v<T, double>
		};
	}

	template <typename R, typename ... Args>
	auto c_function_caller(auto(*)(Args...)->R) noexcept -> CFunctionCaller
	{
		TypeDescriptor const arguments[] = { type_descriptor_for<Args>()... };
		return c_function_caller(arguments, type_descriptor_for<R>());
	}

}