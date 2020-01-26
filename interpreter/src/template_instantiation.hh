#pragma once

#include "utils/span.hh"
namespace incomplete { struct Statement; }
namespace complete { struct Program; }

namespace instantiation
{

	auto instantiate_templates(span<incomplete::Statement const> program) noexcept -> complete::Program;

} // namespace instantiation
