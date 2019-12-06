#pragma once

#include "function_id.hh"
#include "scope.hh"
#include "value_ptr.hh"
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
		add, subtract, multiply, divide, modulo,
		equal, not_equal, less, less_equal, greater, greater_equal, three_way_compare,
		and_, or_, xor_, not,
		assign,
		addressof,

		// Aliases
		dereference = multiply
	};
	auto precedence(Operator op) noexcept -> int;
	auto operator_function_name(Operator op) noexcept->std::string_view;

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
	struct MemberVariableNode : VariableNode 
	{
		value_ptr<ExpressionTree> owner;
	};

	struct DereferenceNode
	{
		TypeId variable_type;
		value_ptr<ExpressionTree> expression;
	};

	struct AddressofNode
	{
		TypeId return_type;
		value_ptr<ExpressionTree> operand;
	};

	struct DepointerNode
	{
		TypeId return_type;
		value_ptr<ExpressionTree> operand;
	};

	struct FunctionNode
	{
		FunctionId function_id;
	};

	struct FunctionTemplateNode
	{
		FunctionTemplateId function_template_id;
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
		value_ptr<std::array<ExpressionTree, 2>> parameters;
	};

	struct AssignmentNode
	{
		value_ptr<ExpressionTree> destination;
		value_ptr<ExpressionTree> source;
	};

	struct IfNode
	{
		value_ptr<ExpressionTree> condition;
		value_ptr<ExpressionTree> then_case;
		value_ptr<ExpressionTree> else_case;
	};

	struct StatementBlockNode
	{
		Scope scope;
		std::vector<stmt::Statement> statements;
		TypeId return_type;
	};

	struct StructConstructorNode
	{
		TypeId constructed_type;
		std::vector<ExpressionTree> parameters;
	};

	// Template nodes.
	namespace tmp
	{
		// Tag type. All dependent nodes inherit from it.
		struct DependentNode
		{
		protected:
			DependentNode() noexcept = default;
		};

		struct LocalVariableNode : DependentNode
		{
			DependentTypeId type;
			PooledString name;
		};

		struct MemberVariableNode : DependentNode
		{
			value_ptr<ExpressionTree> owner;
			PooledString name;
		};

		struct FunctionCallNode : DependentNode
		{
			lookup_result::OverloadSet overload_set;
			std::vector<ExpressionTree> parameters;
		};

		struct RelationalOperatorCallNode : DependentNode // Convert <=> to < <= > >= and != to ==
		{
			lookup_result::OverloadSet overload_set;
			Operator op;
			value_ptr<std::array<ExpressionTree, 2>> parameters;
		};

		struct StructConstructorNode : DependentNode
		{
			DependentTypeId::BaseCase type;
			std::vector<ExpressionTree> parameters;
		};

		struct StatementBlockNode : DependentNode
		{
			DependentScope scope;
			std::vector<stmt::Statement> statements;
		};

	}

	namespace detail
	{
		using ExpressionTreeBase = std::variant<
			Literal<int>, Literal<float>, Literal<bool>,
			DereferenceNode, AddressofNode, DepointerNode,
			LocalVariableNode, GlobalVariableNode, MemberVariableNode,
			FunctionNode, FunctionTemplateNode, FunctionCallNode, RelationalOperatorCallNode, AssignmentNode,
			IfNode, StatementBlockNode,
			StructConstructorNode,

			tmp::LocalVariableNode, tmp::MemberVariableNode,
			tmp::FunctionCallNode, tmp::RelationalOperatorCallNode,
			tmp::StructConstructorNode,
			tmp::StatementBlockNode
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
	auto maybe_dependent_expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> std::variant<TypeId, DependentTypeId>;
	auto expression_type_size(ExpressionTree const & tree, Program const & program) noexcept -> int;
	auto is_dependent(ExpressionTree const & tree) noexcept -> bool;

	// Version that curries the program reference so that it can be used with map.
	inline auto expression_type_id(Program const & program) noexcept
	{ 
		return [&program](ExpressionTree const & tree) noexcept -> TypeId
		{ 
			return expression_type_id(tree, program); 
		};
	}

} // namespace expr
