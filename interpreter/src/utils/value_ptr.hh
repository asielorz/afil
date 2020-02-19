#pragma once

#include <memory>

// unique_ptr but it's copiable and does deep copy.
template <typename T>
struct value_ptr : public std::unique_ptr<T>
{
	static_assert(!std::is_array_v<T>, "T can't be an array type because we cannot copy it. If the array is of a size known in compile-time, you can use std::array");

	using std::unique_ptr<T>::unique_ptr;
	value_ptr() noexcept = default;
	value_ptr(value_ptr &&) noexcept = default;
	value_ptr & operator = (value_ptr &&) noexcept = default;

	value_ptr(value_ptr const & other)
	{
		if (other)
			this->reset(new T(*other));
	}
	value_ptr & operator = (value_ptr const & other)
	{
		if (other)
			this->reset(new T(*other));
		else
			this->reset(nullptr);

		return *this;
	}
};

template <typename T>
auto allocate(T t) -> value_ptr<T>
{
	return std::make_unique<T>(std::move(t));
}

template <typename T>
struct assign_to;

template <typename T>
struct assign_to<value_ptr<T>>
{
	explicit assign_to(value_ptr<T> & var) noexcept : variable_to_assign(std::addressof(var)) {}

	auto operator() (value_ptr<T> const & t) const noexcept -> void { *variable_to_assign = t; }
	auto operator() (value_ptr<T> && t) const noexcept -> void { *variable_to_assign = std::move(t); }
	auto operator() (T const & t) const noexcept -> void { *variable_to_assign = allocate(t); }
	auto operator() (T && t) const noexcept -> void { *variable_to_assign = allocate(std::move(t)); }

private:
	value_ptr<T> * variable_to_assign;
};
