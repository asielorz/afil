#pragma once

#include "compatibility.hh"
#include "out.hh"
#include "string.hh"
#include "unreachable.hh"

#include <charconv>
#include <type_traits>
#include <string_view>

template <typename T>
inline auto from_chars(std::string_view string, out<T> value) noexcept -> std::from_chars_result
{
#if !(defined __cpp_lib_to_chars || (defined _MSC_VER && _MSC_VER >= 1915 /* Visual Studio 2017 15.8*/))
	AFIL_MESSAGE_DEBUG("std::from_chars of floating-point types not available. Using std::stof/stod/stold instead")
	if constexpr (std::is_floating_point_v<T>)
	{
		try
		{
			T result_value;

			if constexpr (std::is_same_v<T, float>)
				result_value = std::stof(std::string(string));
			else if constexpr (std::is_same_v<T, double>)
				result_value = std::stod(std::string(string));
			else
			{
				static_assert(std::is_same_v<T, long double>);
				result_value = std::stold(std::string(string));
			}

			*value = result_value;

			std::from_chars_result result;
			result.ec = {};
			result.ptr = end_ptr(string);

			return result;
		}
		catch (const std::invalid_argument &)
		{
			std::from_chars_result result;
			result.ec = std::errc::invalid_argument;
			result.ptr = begin_ptr(string);

			return result;
		}
		catch (const std::out_of_range &)
		{
			std::from_chars_result result;
			result.ec = std::errc::result_out_of_range;

			// Technically it should point to the first character
			// not matching the pattern, but we can't get that information
			result.ptr = begin_ptr(string);

			return result;
		}
		catch (...)
		{
			declare_unreachable();
		}
	}
	else
#endif
		return std::from_chars(string.data(), string.data() + string.size(), *value);

}
