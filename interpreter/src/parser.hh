#pragma once

#include "span.hh"
#include <string_view>
#include <variant>

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
		std::string_view variable_name;
	};

	struct ExpressionTree : public std::variant<int, OperatorNode, VariableNode> 
	{
		using Base = std::variant<int, OperatorNode, VariableNode>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto is_operator_node(ExpressionTree const & tree) noexcept -> bool;

	struct StatementTree
	{
		std::string_view variable_name;
		ExpressionTree assigned_expression;
	};

	auto parse_expression(span<lex::Token const> tokens) noexcept -> ExpressionTree;
	auto parse_statement(span<lex::Token const> tokens) noexcept -> StatementTree;

} // namespace parser

