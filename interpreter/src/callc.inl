namespace mpl
{
	template <typename T>
	struct BoxedType
	{
		using type = T;
	};
	template <typename T>
	constexpr auto box = BoxedType<T>();
}

namespace callc
{

	template <typename T>
	constexpr auto type_descriptor_for(mpl::BoxedType<T>) noexcept -> TypeDescriptor
	{
		return {
			sizeof(T),
			std::is_same_v<T, float> || std::is_same_v<T, double>
		};
	}
	template <typename T>
	constexpr auto type_descriptor_for(mpl::BoxedType<T &>) noexcept -> TypeDescriptor
	{
		return { sizeof(T *), false };
	}

	template <typename R, typename ... Args>
	auto c_function_caller(auto(*)(Args...)->R) noexcept -> CFunctionCaller
	{
		using mpl::box;
		TypeDescriptor const arguments[] = {type_descriptor_for(box<Args>)...};
		return c_function_caller(arguments, type_descriptor_for(box<R>));
	}

}