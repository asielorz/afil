#pragma once

#include <vector>
namespace complete { struct Program; }
namespace incomplete { struct Statement; }

namespace parser
{

	auto parse_source(std::string_view src, complete::Program const & program) noexcept ->std::vector<incomplete::Statement>;

} // namespace parser
