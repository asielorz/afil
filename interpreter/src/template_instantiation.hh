#pragma once

#include "utils/out.hh"
#include "utils/span.hh"
#include <vector>

namespace incomplete 
{
	struct Statement;
	struct Function;
}
namespace complete 
{
	struct Program;
	struct Function;
	struct TypeId;
	struct Scope;
}

namespace instantiation
{
	enum struct ScopeType { global, function, block };
	struct CurrentScope
	{
		complete::Scope * scope;
		ScopeType type;
		int scope_offset;
	};

	using ScopeStack = std::vector<CurrentScope>;
	using ScopeStackView = span<const CurrentScope>;

	auto instantiate_templates(span<incomplete::Statement const> program) noexcept -> complete::Program;

	auto instantiate_function_template(
		incomplete::Function const & incomplete_function,
		std::vector<complete::TypeId> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> complete::Function;

} // namespace instantiation
