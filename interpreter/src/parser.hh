#pragma once

#include "span.hh"
#include <string_view>
#include <variant>
#include <optional>

struct Program;
struct Scope;
namespace lex {	struct Token; }

namespace parser
{

	enum struct Operator
	{
		add, subtract, multiply, divide
	};

	struct ExpressionTree;

	struct OperatorNode
	{
		explicit OperatorNode(Operator o) noexcept : op(o) {}
		OperatorNode(Operator o, std::unique_ptr<ExpressionTree> l, std::unique_ptr<ExpressionTree> r) noexcept 
			: op(o), left(std::move(l)), right(std::move(r)) {}

		Operator op;
		std::unique_ptr<ExpressionTree> left;
		std::unique_ptr<ExpressionTree> right;
	};

	struct VariableNode
	{
		int variable_offset;
	};

	struct FunctionNode
	{
		int function_id;
	};

	struct FunctionCallNode
	{
		int function_id;
		std::vector<ExpressionTree> parameters;
	};

	struct ExpressionTree : public std::variant<int, OperatorNode, VariableNode, FunctionNode, FunctionCallNode> 
	{
		using Base = std::variant<int, OperatorNode, VariableNode, FunctionNode, FunctionCallNode>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto is_operator_node(ExpressionTree const & tree) noexcept -> bool;

	struct VariableDeclarationStatementNode
	{
		int variable_offset;
		ExpressionTree assigned_expression;
	};

	struct ReturnStatementNode
	{
		ExpressionTree returned_expression;
	};

	struct StatementTree : public std::variant<VariableDeclarationStatementNode, ReturnStatementNode>
	{
		using Base = std::variant<VariableDeclarationStatementNode, ReturnStatementNode>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto parse_expression(span<lex::Token const> tokens, Program & program, Scope const & scope) noexcept -> ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens, Program & program, Scope & scope) noexcept -> std::optional<StatementTree>;

} // namespace parser

