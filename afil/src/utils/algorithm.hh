#pragma once

#include <algorithm>
#include <numeric>

namespace std
{

	template <typename InputRange, typename UnaryPredicate>
	auto all_of(InputRange && range, UnaryPredicate p) -> decltype(all_of(begin(range), end(range), p)) 
	{
		return all_of(begin(range), end(range), p);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto any_of(InputRange && range, UnaryPredicate p) -> decltype(any_of(begin(range), end(range), p))
	{
		return any_of(begin(range), end(range), p);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto none_of(InputRange && range, UnaryPredicate p) -> decltype(none_of(begin(range), end(range), p))
	{
		return none_of(begin(range), end(range), p);
	}

	template <typename InputRange, typename T>
	auto count(InputRange && range, const T & value) -> decltype(count(begin(range), end(range), value))
	{
		return count(begin(range), end(range), value);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto count_if(InputRange && range, UnaryPredicate p) -> decltype(count_if(begin(range), end(range), p))
	{
		return count_if(begin(range), end(range), p);
	}

	template <typename InputRange, typename T>
	auto find(InputRange && range, const T & value) -> decltype(find(begin(range), end(range), value))
	{
		return find(begin(range), end(range), value);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto find_if(InputRange && range, UnaryPredicate p) -> decltype(find_if(begin(range), end(range), p))
	{
		return find_if(begin(range), end(range), p);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto find_if_not(InputRange && range, UnaryPredicate p) -> decltype(find_if_not(begin(range), end(range), p))
	{
		return find_if_not(begin(range), end(range), p);
	}

	template <typename RandomRange>
	auto sort(RandomRange && range) -> decltype(sort(begin(range), end(range)))
	{
		return sort(begin(range), end(range));
	}

	template <typename RandomRange, typename Compare>
	auto sort(RandomRange && range, Compare comp) -> decltype(sort(begin(range), end(range), comp))
	{
		return sort(begin(range), end(range), comp);
	}

	template <typename InputRange, typename T>
	auto remove(InputRange && range, const T & value) -> decltype(remove(begin(range), end(range), value))
	{
		return remove(begin(range), end(range), value);
	}

	template <typename InputRange, typename UnaryPredicate>
	auto remove_if(InputRange && range, UnaryPredicate p) -> decltype(remove_if(begin(range), end(range), p))
	{
		return remove_if(begin(range), end(range), p);
	}

	template <typename InputRange1, typename InputRange2>
	auto equal(InputRange1 && range1, InputRange2 && range2) -> decltype(equal(begin(range1), end(range1), begin(range2), end(range2)))
	{
		return equal(begin(range1), end(range1), begin(range2), end(range2));
	}

	template <typename InputRange1, typename InputRange2, typename BinaryPredicate>
	auto equal(InputRange1 && range1, InputRange2 && range2, BinaryPredicate p) -> decltype(equal(begin(range1), end(range1), begin(range2), end(range2), p))
	{
		return equal(begin(range1), end(range1), begin(range2), end(range2), p);
	}

	template <typename ForwardRange, typename T>
	auto iota(ForwardRange & range, T value) -> decltype(std::iota(begin(range), end(range), value))
	{
		return std::iota(begin(range), end(range), value);
	}

} // namespace std

template <typename Container, typename Pred>
void erase_if(Container & v, Pred p)
{
	auto new_end = std::remove_if(v.begin(), v.end(), p);
	v.erase(new_end, v.end());
}
