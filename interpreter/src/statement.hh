#pragma once

#include "expression.hh"

namespace stmt
{

	struct VariableDeclarationStatement
	{
		int variable_offset;
		expr::ExpressionTree assigned_expression;
	};

	struct ExpressionStatement
	{
		expr::ExpressionTree expression;
	};

	struct Statement : public std::variant<VariableDeclarationStatement, ExpressionStatement>
	{
		using Base = std::variant<VariableDeclarationStatement, ExpressionStatement>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

}
