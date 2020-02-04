#pragma once

#include <string>

namespace complete
{
	struct Expression;
	struct Statement;
	struct Program;
}

auto pretty_print(complete::Expression const & expression, complete::Program const & program, int indentation_level = 0) noexcept -> std::string;
auto pretty_print(complete::Statement const & statement, complete::Program const & program, int indentation_level = 0) noexcept -> std::string;
auto pretty_print(complete::Program const & program) noexcept -> std::string;
