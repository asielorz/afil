#pragma once

#include <string>

namespace complete
{
	struct Program;
}

namespace c_transpiler
{

	auto transpile_to_c(complete::Program const & program) noexcept -> std::string;

}
