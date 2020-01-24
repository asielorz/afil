#pragma once

#include "expression.hh"

namespace incomplete
{

	struct Statement;

	namespace statement
	{
		struct VariableDeclaration
		{
			std::string variable_name;
			ExpressionTree assigned_expression;
		};

		struct ExpressionStatement
		{
			ExpressionTree expression;
		};

		struct Return
		{
			ExpressionTree returned_expression;
		};

		struct If
		{
			ExpressionTree condition;
			value_ptr<Statement> then_case;
			value_ptr<Statement> else_case;
		};

		struct StatementBlock
		{
			Scope scope;
			std::vector<Statement> statements;
		};

		struct While
		{
			ExpressionTree condition;
			value_ptr<Statement> body;
		};

		struct For
		{
			Scope scope;
			value_ptr<Statement> init_statement;
			ExpressionTree condition;
			ExpressionTree end_expression;
			value_ptr<Statement> body;
		};

		struct Break {};
		struct Continue {};

		namespace detail
		{
			using StatementBase = std::variant<
				VariableDeclaration, ExpressionStatement,
				If, StatementBlock, While, For,
				Return, Break, Continue
			>;
		} // namespace detail
	} // namespace statement

	struct Statement : public statement::detail::StatementBase
	{
		using Base = statement::detail::StatementBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

} // namespace incomplete
