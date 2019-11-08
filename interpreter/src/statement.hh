#pragma once

#include "expression.hh"
#include <optional>

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
		value_ptr<Statement> then_case;
		value_ptr<Statement> else_case;
	};

	struct StatementBlock
	{
		Scope scope;
		std::vector<stmt::Statement> statements;
	};

	struct WhileStatement
	{
		expr::ExpressionTree condition;
		value_ptr<Statement> body;
	};

	struct ForStatement
	{
		Scope scope;
		value_ptr<Statement> init_statement;
		expr::ExpressionTree condition;
		expr::ExpressionTree end_expression;
		value_ptr<Statement> body;
	};

	struct BreakStatement {};
	struct ContinueStatement {};

	namespace tmp
	{
		// Tag type. All dependent nodes inherit from it.
		struct DependentNode
		{
		protected:
			DependentNode() noexcept = default;
		};

		struct VariableDeclarationStatement : DependentNode
		{
			PooledString variable_name;
			std::optional<expr::ExpressionTree> assigned_expression;
		};
	}

	namespace detail
	{
		using StatementBase = std::variant<
			VariableDeclarationStatement, ExpressionStatement, 
			IfStatement, StatementBlock, WhileStatement, ForStatement,
			ReturnStatement, BreakStatement, ContinueStatement,

			tmp::VariableDeclarationStatement
		>;
	}

	struct Statement : public detail::StatementBase
	{
		using Base = detail::StatementBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto is_dependent(Statement const & statement) noexcept -> bool;

}
