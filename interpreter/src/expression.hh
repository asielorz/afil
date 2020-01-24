#pragma once

#include "scope.hh"

struct OverloadSet
{
	std::vector<FunctionId> functions;
	std::vector<FunctionTemplateId> function_templates;
};

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
auto operator_function_name(Operator op) noexcept -> std::string_view;
auto operator_overload_set(Operator op, incomplete::ScopeStackView scope_stack, span<char const> string_pool) -> OverloadSet;

namespace incomplete
{

	struct Statement;
	struct ExpressionTree;

	namespace expression
	{

		template <typename T>
		struct Literal
		{
			T value;
		};

		struct Variable
		{
			std::string name;
		};
		struct LocalVariable : Variable {};
		struct GlobalVariable : Variable {};
		struct MemberVariable : Variable
		{
			value_ptr<ExpressionTree> owner;
		};

		struct Addressof
		{
			value_ptr<ExpressionTree> operand;
		};

		struct Dereference
		{
			value_ptr<ExpressionTree> operand;
		};

		struct Subscript
		{
			value_ptr<ExpressionTree> array;
			value_ptr<ExpressionTree> index;
		};

		struct OverloadSetNode
		{
			std::string name;
		};

		struct FunctionCall
		{
			std::vector<ExpressionTree> parameters;
		};

		struct OperatorCall
		{
			Operator op;
			value_ptr<ExpressionTree> left;
			value_ptr<ExpressionTree> right;
		};

		struct If
		{
			value_ptr<ExpressionTree> condition;
			value_ptr<ExpressionTree> then_case;
			value_ptr<ExpressionTree> else_case;
		};

		struct StatementBlock
		{
			Scope scope;
			std::vector<incomplete::Statement> statements;
		};

		struct Constructor
		{
			DependentTypeId constructed_type;
			std::vector<ExpressionTree> parameters;
		};

		namespace detail
		{
			using ExpressionTreeBase = std::variant<
				Literal<int>, Literal<float>, Literal<bool>,
				Dereference, Addressof, Subscript,
				LocalVariable, GlobalVariable, MemberVariable,
				OverloadSetNode, FunctionCall, OperatorCall,
				If, StatementBlock,
				Constructor
			>;
		}

	} // namespace expression

	struct ExpressionTree : public expression::detail::ExpressionTreeBase
	{
		using Base = expression::detail::ExpressionTreeBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

} // namespace incomplete
