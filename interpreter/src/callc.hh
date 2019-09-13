#pragma once

#include "span.hh"

namespace callc
{

	struct TypeDescriptor
	{
		int size;
		bool is_float;
	};

	using CFunctionCaller = auto(*)(void const * function, void const * arg_data, void * return_value) noexcept -> void;

	auto c_function_caller(span<TypeDescriptor const> param_types, TypeDescriptor return_value_type) noexcept -> CFunctionCaller;

}
