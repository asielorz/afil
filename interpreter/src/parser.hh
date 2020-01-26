#pragma once

#include <vector>
namespace incomplete { struct Statement; }

namespace parser
{

	auto parse_source(std::string_view src) noexcept ->std::vector<incomplete::Statement>;

} // namespace parser
