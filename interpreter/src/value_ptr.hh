#pragma once

#include <memory>

// unique_ptr but it's copiable and does deep copy.
template <typename T>
struct value_ptr : public std::unique_ptr<T>
{
	using std::unique_ptr<T>::unique_ptr;
	value_ptr() noexcept = default;
	value_ptr(value_ptr &&) noexcept = default;
	value_ptr & operator = (value_ptr &&) noexcept = default;

	value_ptr(value_ptr const & other)
	{
		if (other)
			reset(new T(*other));
	}
	value_ptr & operator = (value_ptr const & other)
	{
		if (other)
			reset(new T(*other));
		else
			reset(nullptr);

		return *this;
	}
};
