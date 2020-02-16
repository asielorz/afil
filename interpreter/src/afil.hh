#pragma once

#include "utils/out.hh"
#include "syntax_error.hh"
#include "utils/expected.hh"
#include <string_view>

namespace complete { struct Program; }

namespace afil
{

	[[nodiscard]] auto parse_source(std::string_view source) noexcept -> expected<complete::Program, SyntaxError>;
	[[nodiscard]] auto parse_source(std::string_view source, out<complete::Program> program) noexcept -> expected<void, SyntaxError>;

} // namespace afil
