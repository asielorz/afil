#pragma once

#include <vector>
#include <array>

namespace detail
{
	template <typename T>
	struct span_impl
	{
		using value_type = T;
		using size_type = size_t;
		using iterator = T *;

		constexpr span_impl(T data_[], size_t size_) noexcept : data_array(data_), data_size(size_) {}

		constexpr auto operator [] (size_t i) const noexcept -> T & { return data_array[i]; }
		constexpr auto data() const noexcept -> T * { return data_array; }
		constexpr auto size() const noexcept -> size_t { return data_size; }
		constexpr auto empty() const noexcept -> bool { return size() == 0; }
		constexpr auto begin() const noexcept -> T * { return data(); }
		constexpr auto end() const noexcept -> T * { return data() + size(); }
		constexpr auto front() const noexcept -> T & { return data()[0]; }
		constexpr auto back() const noexcept -> T & { return data()[size() - 1]; }

	protected:
		T * data_array;
		size_t data_size;
	};
} // namespace detail

template <typename T>
struct span : public detail::span_impl<T>
{
	constexpr span(T data_[], size_t size_) noexcept : detail::span_impl<T>(data_, size_) {}
	template <size_t N>	constexpr span(T(&array)[N]) noexcept : detail::span_impl<T>(array, N) {}
	template <size_t N>	constexpr span(std::array<T, N> & array) noexcept : detail::span_impl<T>(array.data(), N) {}
	span(std::vector<T> & v) noexcept : detail::span_impl<T>(v.data(), v.size()) {}
};

template <typename T>
struct span<T const> : public detail::span_impl<T const>
{
	constexpr span(T const data_[], size_t size_) noexcept : detail::span_impl<T const>(data_, size_) {}
	constexpr span(span<T> s) noexcept : detail::span_impl<T>(s.data(), s.size()) {}
	template <size_t N>	constexpr span(T(&data_)[N]) noexcept : detail::span_impl<T const>(data_, N) {}
	template <size_t N>	constexpr span(T const(&data_)[N]) noexcept : detail::span_impl<T const>(data_, N) {}
	template <size_t N>	constexpr span(std::array<T, N> const & array) noexcept : detail::span_impl<T const>(array.data(), N) {}
	span(std::vector<T> const & v) noexcept : detail::span_impl<T const>(v.data(), v.size()) {}
};
