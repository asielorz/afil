#pragma once

#include "scope.hh"

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

namespace incomplete
{

	struct Statement;
	struct Expression;

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
			value_ptr<Expression> owner;
		};

		struct Addressof
		{
			value_ptr<Expression> operand;
		};

		struct Dereference
		{
			value_ptr<Expression> operand;
		};

		struct Subscript
		{
			value_ptr<Expression> array;
			value_ptr<Expression> index;
		};

		struct Function
		{
			incomplete::Function function;
		};

		struct FunctionTemplate
		{
			incomplete::FunctionTemplate function_template;
		};

		struct OverloadSetNode
		{
			std::string name;
		};

		struct FunctionCall
		{
			std::vector<Expression> parameters;
		};

		struct OperatorCall
		{
			Operator op;
			value_ptr<Expression> left;
			value_ptr<Expression> right;
		};

		struct If
		{
			value_ptr<Expression> condition;
			value_ptr<Expression> then_case;
			value_ptr<Expression> else_case;
		};

		struct StatementBlock
		{
			std::vector<incomplete::Statement> statements;
		};

		struct Constructor
		{
			TypeId constructed_type;
			std::vector<Expression> parameters;
		};

		namespace detail
		{
			using ExpressionTreeBase = std::variant<
				Literal<int>, Literal<float>, Literal<bool>,
				Dereference, Addressof, Subscript,
				LocalVariable, GlobalVariable, MemberVariable,
				Function, FunctionTemplate,	OverloadSetNode, FunctionCall, OperatorCall,
				If, StatementBlock,
				Constructor
			>;
		}

	} // namespace expression

	struct Expression : public expression::detail::ExpressionTreeBase
	{
		using Base = expression::detail::ExpressionTreeBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	struct MemberVariable
	{
		std::string name;
		TypeId type;
		std::optional<Expression> initializer_expression;
	};

	struct Struct
	{
		std::string name;
		std::vector<MemberVariable> member_variables;
	};

	struct StructTemplate : Struct
	{
		std::vector<TemplateParameter> template_parameters;
	};

} // namespace incomplete

namespace complete
{

	struct Expression
	{

	};

} // namespace complete
