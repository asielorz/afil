#pragma once

#include <string_view>

enum struct Operator
{
	add, subtract, multiply, divide, modulo,
	equal, not_equal, less, less_equal, greater, greater_equal, three_way_compare,
	and_, or_, xor_, not_,
	bitwise_and, bitwise_or, bitwise_xor, bitwise_not, bit_shift_left, bit_shift_right,
	assign,

	// Aliases
	addressof = bitwise_and,
	dereference = multiply
};
auto precedence(Operator op) noexcept -> int;
auto operator_function_name(Operator op) noexcept -> std::string_view;
auto is_comparison_operator(Operator op) noexcept -> bool;
