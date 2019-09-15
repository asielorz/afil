#pragma once

#include "span.hh"
#include "expression.hh"
#include <string_view>
#include <variant>
#include <optional>

struct Scope;
namespace lex {	struct Token; }

namespace parser
{

	struct VariableDeclarationStatementNode
	{
		int variable_offset;
		expr::ExpressionTree assigned_expression;
	};

	struct ReturnStatementNode
	{
		expr::ExpressionTree returned_expression;
	};

	struct StatementTree : public std::variant<VariableDeclarationStatementNode, ReturnStatementNode>
	{
		using Base = std::variant<VariableDeclarationStatementNode, ReturnStatementNode>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> expr::ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<StatementTree>;

} // namespace parser

