#pragma once

#include "operator.hh"
#include "function_id.hh"
#include "complete_scope.hh"
#include "utils/value_ptr.hh"
#include "utils/span.hh"
#include <variant>

namespace complete
{
	struct Expression;
	struct Statement;
	struct OverloadSet
	{
		std::vector<FunctionId> function_ids;
		std::vector<FunctionTemplateId> function_template_ids;
	};

	namespace expression
	{
		template <typename T>
		struct Literal
		{
			T value;
		};

		struct StringLiteral
		{
			std::string value;
			TypeId type;
		};

		struct Variable
		{
			TypeId variable_type;
			int variable_offset;
		};
		struct LocalVariable : Variable {};
		struct GlobalVariable : Variable {};
		struct MemberVariable : Variable
		{
			value_ptr<Expression> owner;
		};

		struct Constant
		{
			TypeId type;
			span<char const> value;
		};

		struct OverloadSet
		{
			complete::OverloadSet overload_set;
		};

		struct FunctionCall
		{
			FunctionId function_id;
			std::vector<Expression> parameters;
		};

		struct RelationalOperatorCall
		{
			Operator op;
			FunctionId function_id;
			std::vector<Expression> parameters;
		};

		struct Constructor
		{
			TypeId constructed_type;
			std::vector<Expression> parameters;
		};

		struct Dereference
		{
			value_ptr<Expression> expression;
			TypeId return_type;
		};

		struct ReinterpretCast
		{
			value_ptr<Expression> operand;
			TypeId return_type;
		};

		struct Subscript
		{
			value_ptr<Expression> array;
			value_ptr<Expression> index;
			TypeId return_type;
		};

		struct If
		{
			value_ptr<Expression> condition;
			value_ptr<Expression> then_case;
			value_ptr<Expression> else_case;
		};

		struct Assignment
		{
			value_ptr<Expression> destination;
			value_ptr<Expression> source;
		};

		struct StatementBlock
		{
			Scope scope;
			std::vector<Statement> statements;
			TypeId return_type;
		};
		
		namespace detail
		{
			using ExpressionTreeBase = std::variant<
				Literal<int>, Literal<float>, Literal<bool>, StringLiteral,
				LocalVariable, GlobalVariable, MemberVariable, Constant,
				OverloadSet, FunctionCall, RelationalOperatorCall, Assignment,
				Constructor,
				Dereference, ReinterpretCast, Subscript,
				If, StatementBlock
			>;
		} // namespace detail
	} // namespace expression

	struct Expression : public expression::detail::ExpressionTreeBase
	{
		using Base = expression::detail::ExpressionTreeBase;
		using Base::Base;
		constexpr auto as_variant() noexcept -> Base & { return *this; }
		constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	};

	struct Program;
	struct Type;
	auto expression_type(Expression const & tree, Program const & program) noexcept -> Type const &;
	auto expression_type_id(Expression const & tree, Program const & program) noexcept -> TypeId;
	auto expression_type_size(Expression const & tree, Program const & program) noexcept -> int;
	auto is_constant_expression(Expression const & expr, int constant_base_index) noexcept -> bool;

} // namespace complete
