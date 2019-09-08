#pragma once

#include <string_view>

namespace interpreter
{

	auto eval_expression(std::string_view src) noexcept -> int;

}
