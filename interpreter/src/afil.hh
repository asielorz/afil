#pragma once

#include "utils/out.hh"
#include <string_view>

namespace complete { struct Program; }

namespace afil
{

	[[nodiscard]] auto parse_source(std::string_view source) noexcept -> complete::Program;
	auto parse_source(std::string_view source, out<complete::Program> program) noexcept -> void;

} // namespace afil
