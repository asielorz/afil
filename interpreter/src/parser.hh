#pragma once

#include "syntax_error.hh"
#include "utils/expected.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include <vector>
namespace incomplete { struct Statement; struct Module; }

namespace parser
{

	struct TypeName
	{
		std::string_view name;
		enum struct Type { type, struct_template, template_parameter } type;
	};

	[[nodiscard]] auto parse_modules(span<incomplete::Module> modules) noexcept -> expected<void, SyntaxError>;

	[[nodiscard]] auto parse_global_scope(
		span<lex::Token const> tokens,
		std::vector<TypeName> & type_names,
		std::vector<incomplete::Statement> global_initialization_statements = {}
	) noexcept -> expected<std::vector<incomplete::Statement>, PartialSyntaxError>;

} // namespace parser
