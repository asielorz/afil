#pragma once

#include "complete_scope.hh"
#include "utils/value_ptr.hh"
#include <variant>

namespace complete
{

	struct Expression
	{

	};

	struct Program;
	struct Type;
	auto expression_type(Expression const & tree, Program const & program) noexcept -> Type const &;
	auto expression_type_id(Expression const & tree, Program const & program) noexcept -> TypeId;

} // namespace complete
