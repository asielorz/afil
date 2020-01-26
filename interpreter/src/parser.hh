#pragma once

namespace incomplete { struct Program; }

namespace parser
{

	auto parse_source(std::string_view src) noexcept -> incomplete::Program;

} // namespace parser
