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

	struct ParseParams
	{
		Program & program;
		ScopeStack & scope_stack;
		TypeId & current_return_type;
	};

	auto parse_expression(span<lex::Token const> tokens, ParseParams p) noexcept -> expr::ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens, ParseParams p) noexcept -> std::optional<stmt::Statement>;

	// Tokenize and parse a source file and return the resulting program.
	auto parse_source(std::string_view src) noexcept -> Program;

} // namespace parser

