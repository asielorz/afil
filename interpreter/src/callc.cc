#include "callc.hh"
#include "unreachable.hh"
#include "utils.hh"

namespace callc
{

	template <typename T>
	using function_ptr = T *;

	namespace callers
	{

		template <typename T, typename U>
		auto next_field(U const * p) -> T const *
		{
			return reinterpret_cast<T const *>(align(p + 1, alignof(T)));
		}

		template <typename R>
		auto caller_0_args(void const * function, void const *, void * return_value) noexcept -> void
		{
			*static_cast<R *>(return_value) = function_ptr<auto() noexcept -> R>(function)();
		}

		template <typename R, typename A1>
		auto caller_1_arg(void const * function, void const * arg_data, void * return_value) noexcept -> void
		{
			*static_cast<R *>(return_value) = function_ptr<auto(A1) noexcept -> R>(function)(
				*reinterpret_cast<A1 const *>(arg_data)
			);
		}

		template <typename R, typename A1, typename A2>
		auto caller_2_args(void const * function, void const * arg_data, void * return_value) noexcept -> void
		{
			*static_cast<R *>(return_value) = function_ptr<auto(A1, A2) noexcept -> R>(function)(
				*reinterpret_cast<A1 const *>(arg_data),
				*next_field<A2>(reinterpret_cast<A1 const *>(arg_data))
			);
		}

		template <typename R, typename A1, typename A2, typename A3>
		auto caller_3_args(void const * function, void const * arg_data, void * return_value) noexcept -> void
		{
			*static_cast<R *>(return_value) = function_ptr<auto(A1, A2, A3) noexcept -> R>(function)(
				*reinterpret_cast<A1 const *>(arg_data),
				*next_field<A2>(reinterpret_cast<A1 const *>(arg_data)),
				*next_field<A3>(next_field<A2>(reinterpret_cast<A1 const *>(arg_data)))
			);
		}

		template <typename R, typename A1, typename A2, typename A3, typename A4>
		auto caller_4_args(void const * function, void const * arg_data, void * return_value) noexcept -> void
		{
			*static_cast<R *>(return_value) = function_ptr<auto(A1, A2, A3, A4) noexcept -> R>(function)(
				*reinterpret_cast<A1 const *>(arg_data),
				*next_field<A2>(reinterpret_cast<A1 const *>(arg_data)),
				*next_field<A3>(next_field<A2>(reinterpret_cast<A1 const *>(arg_data))),
				*next_field<A4>(next_field<A3>(next_field<A2>(reinterpret_cast<A1 const *>(arg_data))))
			);
		}
	} // namespace callers

#define C_FUNCTION_CALLER_IMPL_BODY(type, next_template_to_call)											\
	if (type.is_float)																						\
	{																										\
		switch (type.size)																					\
		{																									\
			case 4: return next_template_to_call<Ts..., float>(param_types);								\
			case 8: return next_template_to_call<Ts..., double>(param_types);								\
			default: declare_unreachable();																	\
		}																									\
	}																										\
	else																									\
	{																										\
		switch (type.size)																					\
		{																									\
			case 1: return next_template_to_call<Ts..., uint8_t>(param_types);								\
			case 2: return next_template_to_call<Ts..., uint16_t>(param_types);								\
			case 4: return next_template_to_call<Ts..., uint32_t>(param_types);								\
			case 8: return next_template_to_call<Ts..., uint64_t>(param_types);								\
			default: declare_unreachable();																	\
		}																									\
	}																										\


	//***************************************************************************************************************************************************
	// 0 args

	template <typename R>
	auto c_function_caller_impl_0_args_1(span<TypeDescriptor const>) noexcept -> CFunctionCaller
	{
		return &callers::caller_0_args<R>;
	}

	template <typename ... Ts>
	auto c_function_caller_impl_0_args_0(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		C_FUNCTION_CALLER_IMPL_BODY(return_value_type, c_function_caller_impl_0_args_1);
	}

	//***************************************************************************************************************************************************
	// 1 args

	template <typename ... Ts>
	auto c_function_caller_impl_1_args_1([[maybe_unused]] span<TypeDescriptor const> param_types) noexcept -> CFunctionCaller
	{
		if constexpr (sizeof...(Ts) == 2)
		{
			return &callers::caller_1_arg<Ts...>;
		}
		else
		{
			C_FUNCTION_CALLER_IMPL_BODY(param_types[sizeof...(Ts) - 1], c_function_caller_impl_1_args_1);
		}
	}

	template <typename ... Ts>
	auto c_function_caller_impl_1_args_0(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		C_FUNCTION_CALLER_IMPL_BODY(return_value_type, c_function_caller_impl_1_args_1);
	}

	//***************************************************************************************************************************************************
	// 2 args

	template <typename ... Ts>
	auto c_function_caller_impl_2_args_1([[maybe_unused]] span<TypeDescriptor const> param_types) noexcept -> CFunctionCaller
	{
		if constexpr (sizeof...(Ts) == 3)
		{
			return &callers::caller_2_args<Ts...>;
		}
		else
		{
			C_FUNCTION_CALLER_IMPL_BODY(param_types[sizeof...(Ts) - 1], c_function_caller_impl_2_args_1);
		}
	}

	template <typename ... Ts>
	auto c_function_caller_impl_2_args_0(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		C_FUNCTION_CALLER_IMPL_BODY(return_value_type, c_function_caller_impl_2_args_1);
	}

	//***************************************************************************************************************************************************
	// 3 args

	template <typename ... Ts>
	auto c_function_caller_impl_3_args_1([[maybe_unused]] span<TypeDescriptor const> param_types) noexcept -> CFunctionCaller
	{
		if constexpr (sizeof...(Ts) == 4)
		{
			return &callers::caller_3_args<Ts...>;
		}
		else
		{
			C_FUNCTION_CALLER_IMPL_BODY(param_types[sizeof...(Ts) - 1], c_function_caller_impl_3_args_1);
		}
	}

	template <typename ... Ts>
	auto c_function_caller_impl_3_args_0(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		C_FUNCTION_CALLER_IMPL_BODY(return_value_type, c_function_caller_impl_3_args_1);
	}

	//***************************************************************************************************************************************************
	// 4 args

	template <typename ... Ts>
	auto c_function_caller_impl_4_args_1([[maybe_unused]] span<TypeDescriptor const> param_types) noexcept -> CFunctionCaller
	{
		if constexpr (sizeof...(Ts) == 5)
		{
			return &callers::caller_4_args<Ts...>;
		}
		else
		{
			C_FUNCTION_CALLER_IMPL_BODY(param_types[sizeof...(Ts) - 1], c_function_caller_impl_4_args_1);
		}
	}

	template <typename ... Ts>
	auto c_function_caller_impl_4_args_0(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		C_FUNCTION_CALLER_IMPL_BODY(return_value_type, c_function_caller_impl_4_args_1);
	}

	//***************************************************************************************************************************************************
	// c_function_caller

	auto c_function_caller(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller
	{
		switch (param_types.size())
		{
			case 0: return c_function_caller_impl_0_args_0(param_types, return_value_type);
			case 1: return c_function_caller_impl_1_args_0(param_types, return_value_type);
			case 2: return c_function_caller_impl_2_args_0(param_types, return_value_type);
			case 3: return c_function_caller_impl_3_args_0(param_types, return_value_type);
			case 4: return c_function_caller_impl_4_args_0(param_types, return_value_type);
			default: declare_unreachable(); // TODO
		}
	}

} // namespace callc
