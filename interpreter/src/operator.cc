#include "operator.hh"
#include "utils/unreachable.hh"

auto precedence(Operator op) noexcept -> int
{
	switch (op)
	{
		case Operator::multiply:			return 7;
		case Operator::divide:				return 7;
		case Operator::modulo:				return 7;
		case Operator::add:					return 6;
		case Operator::subtract:			return 6;
		case Operator::three_way_compare:	return 5;
		case Operator::less:				return 4;
		case Operator::less_equal:			return 4;
		case Operator::greater:				return 4;
		case Operator::greater_equal:		return 4;
		case Operator::equal:				return 3;
		case Operator::not_equal:			return 3;
		case Operator::and_:				return 2;
		case Operator::not_:				return 2;
		case Operator::xor_:				return 1;
		case Operator::or_:					return 0;
		case Operator::assign:				return -1;
	}
	declare_unreachable();
}

auto operator_function_name(Operator op) noexcept -> std::string_view
{
	switch (op)
	{
		case Operator::add:					return "+";
		case Operator::subtract:			return "-";
		case Operator::multiply:			return "*";
		case Operator::divide:				return "/";
		case Operator::modulo:				return "%";
		case Operator::equal:				return "==";
		case Operator::not_equal:			return "==";
		case Operator::less:				return "<=>";
		case Operator::less_equal:			return "<=>";
		case Operator::greater:				return "<=>";
		case Operator::greater_equal:		return "<=>";
		case Operator::three_way_compare:	return "<=>";
		case Operator::and_:				return "and";
		case Operator::or_:					return "or";
		case Operator::xor_:				return "xor";
		case Operator::not_:				return "not";
		case Operator::assign:				return "=";
		case Operator::addressof:			return "&";
	}
	declare_unreachable();
}
