#pragma once

#include <string>

namespace complete
{
	struct Program;
	struct Function;
}

auto pretty_print(complete::Program const & program) noexcept -> std::string;
auto pretty_print(complete::Function const & function, complete::Program const & program) noexcept -> std::string;
