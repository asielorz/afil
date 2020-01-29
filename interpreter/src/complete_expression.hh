#pragma once

#include "function_id.hh"
#include "complete_scope.hh"
#include "utils/value_ptr.hh"
#include <variant>

namespace complete
{
	struct Expression;
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

		struct OverloadSet
		{
			complete::OverloadSet overload_set;
		};

		namespace detail
		{
			using ExpressionTreeBase = std::variant<
				Literal<int>, Literal<float>, Literal<bool>,
				LocalVariable, GlobalVariable, MemberVariable,
				OverloadSet
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

} // namespace complete
