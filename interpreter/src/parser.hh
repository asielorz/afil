#pragma once

#include "syntax_error.hh"
#include "utils/expected.hh"
#include <vector>
namespace complete { struct Program; }
namespace incomplete { struct Statement; }

namespace parser
{

	auto parse_source(std::string_view src, complete::Program const & program) noexcept -> expected<std::vector<incomplete::Statement>, PartialSyntaxError>;

} // namespace parser
