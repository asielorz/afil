#pragma once

#include "function_id.hh"
#include "scope.hh"
#include <memory>
#include <vector>
#include <variant>
#include <array>

struct Program;
struct Type;

namespace stmt
{
	struct Statement;
}

namespace expr
{

	enum struct Operator
	{
		add, subtract, multiply, divide, 
		equal, not_equal, less, less_equal, greater, greater_equal, three_way_compare,
		and_, or_, xor_,
		assign
	};
	auto precedence(Operator op) noexcept -> int;
	auto operator_function_name(Operator op) noexcept -> std::string_view;

	struct ExpressionTree;

	template <typename T>
	struct Literal
	{
		T value;
	};

	struct VariableNode
	{
		TypeId variable_type;
		int variable_offset;
	};
	struct LocalVariableNode : VariableNode {};
	struct GlobalVariableNode : VariableNode {};

	struct DereferenceNode
	{
		TypeId variable_type;
		std::unique_ptr<ExpressionTree> expression;
	};

	struct FunctionNode
	{
		FunctionId function_id;
	};

	struct FunctionCallNode
	{
		FunctionId function_id;
		std::vector<ExpressionTree> parameters;
	};

	struct RelationalOperatorCallNode // Convert <=> to < <= > >= and != to ==
	{
		FunctionId function_id;
		Operator op;
		std::unique_ptr<std::array<ExpressionTree, 2>> parameters;
	};

	struct IfNode
	{
		std::unique_ptr<ExpressionTree> condition;
		std::unique_ptr<ExpressionTree> then_case;
		std::unique_ptr<ExpressionTree> else_case;
	};

	struct StatementBlockNode
	{
		Scope scope;
		std::vector<stmt::Statement> statements;
		TypeId return_type;
	};

	namespace detail
	{
		using ExpressionTreeBase = std::variant<
			Literal<int>, Literal<float>, Literal<bool>,
			DereferenceNode,
			LocalVariableNode, GlobalVariableNode, 
			FunctionNode, FunctionCallNode, RelationalOperatorCallNode,
			IfNode, StatementBlockNode
		>;
	}

	struct ExpressionTree : public detail::ExpressionTreeBase
	{
		using Base = detail::ExpressionTreeBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	struct OperatorTree;

	struct OperatorNode
	{
		explicit OperatorNode(Operator o) noexcept : op(o) {}
		OperatorNode(Operator o, std::unique_ptr<OperatorTree> l, std::unique_ptr<OperatorTree> r) noexcept
			: op(o), left(std::move(l)), right(std::move(r)) {}

		Operator op;
		std::unique_ptr<OperatorTree> left;
		std::unique_ptr<OperatorTree> right;
	};

	struct OperatorTree : public std::variant<OperatorNode, ExpressionTree>
	{
		using Base = std::variant<OperatorNode, ExpressionTree>;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	auto is_operator_node(OperatorTree const & tree) noexcept -> bool;
	auto expression_type(ExpressionTree const & tree, Program const & program) noexcept -> Type;
	auto expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> TypeId;
	auto pretty_print(ExpressionTree const & tree, Program const & program) noexcept -> std::string;

} // namespace expr
