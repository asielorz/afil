#pragma once

#include "syntax_error.hh"
#include "utils/expected.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include <vector>
namespace incomplete { struct Statement; struct Module; }

namespace parser
{

	[[nodiscard]] auto parse_modules(span<incomplete::Module> modules) noexcept -> expected<std::vector<int>, SyntaxError>;

	[[nodiscard]] auto parse_global_scope(
		span<lex::Token const> tokens,
		std::vector<incomplete::Statement> global_initialization_statements = {}
	) noexcept -> expected<std::vector<incomplete::Statement>, PartialSyntaxError>;

} // namespace parser
