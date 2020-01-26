#pragma once

namespace incomplete { struct Program; }
namespace complete { struct Program; }

namespace instantiation
{

	auto instantiate_templates(incomplete::Program const & program) noexcept -> complete::Program;

} // namespace instantiation
