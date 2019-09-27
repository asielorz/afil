#pragma once

#include "span.hh"
#include "statement.hh"
#include <string_view>
#include <variant>
#include <optional>

struct Scope;
enum struct ScopeType;
namespace lex {	struct Token; }

namespace parser
{

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope, ScopeType scope_type) noexcept -> expr::ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope, ScopeType scope_type) noexcept -> std::optional<stmt::Statement>;

	// Tokenize and parse a source file and return the resulting program.
	auto parse_source(std::string_view src) noexcept -> Program;

} // namespace parser

