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

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> expr::ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<Statement>;

	// Tokenize and parse a source file and return the resulting program.
	auto parse_source(std::string_view src) noexcept -> Program;

} // namespace parser

