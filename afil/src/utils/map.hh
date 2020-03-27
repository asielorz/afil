#pragma once

#include <vector>
#include <iterator>

template <typename Range, typename F>
auto map(Range const & range, F const & function) noexcept -> std::vector<std::decay_t<decltype(std::invoke(function, *std::begin(range)))>>
{
	using ResultType = std::decay_t<decltype(std::invoke(function, *std::begin(range)))>;
	std::vector<ResultType> result;
	result.reserve(std::distance(std::begin(range), std::end(range)));
	for (auto const & elem : range)
		result.push_back(std::invoke(function, elem));
	return result;
}
