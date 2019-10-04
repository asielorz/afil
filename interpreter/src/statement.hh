#pragma once

#include "expression.hh"

namespace stmt
{

	struct Statement;

	struct VariableDeclarationStatement
	{
		int variable_offset;
		expr::ExpressionTree assigned_expression;
	};

	struct ExpressionStatement
	{
		expr::ExpressionTree expression;
	};

	struct ReturnStatement
	{
		expr::ExpressionTree returned_expression;
	};

	struct IfStatement
	{
		expr::ExpressionTree condition;
		std::unique_ptr<Statement> then_case;
		std::unique_ptr<Statement> else_case;
	};

	struct StatementBlock
	{
		Scope scope;
		std::vector<stmt::Statement> statements;
	};

	struct WhileStatement
	{
		expr::ExpressionTree condition;
		std::unique_ptr<Statement> body;
	};

	struct ForStatement
	{
		Scope scope;
		std::unique_ptr<Statement> init_statement;
		expr::ExpressionTree condition;
		expr::ExpressionTree end_expression;
		std::unique_ptr<Statement> body;
	};

	struct BreakStatement {};
	struct ContinueStatement {};

	namespace detail
	{
		using StatementBase = std::variant<
			VariableDeclarationStatement, ExpressionStatement, 
			IfStatement, StatementBlock, WhileStatement, ForStatement,
			ReturnStatement, BreakStatement, ContinueStatement
		>;
	}

	struct Statement : public detail::StatementBase
	{
		using Base = detail::StatementBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

}
