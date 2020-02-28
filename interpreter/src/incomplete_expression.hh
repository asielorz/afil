#pragma once

#include "built_in_structures.hh"
#include "incomplete_scope.hh"
#include "operator.hh"

namespace incomplete
{

	struct Statement;
	struct Expression;
	struct DesignatedInitializer;

	namespace expression
	{

		template <typename T>
		struct Literal
		{
			T value;
		};

		struct Identifier
		{
			std::string name;
		};

		struct MemberVariable
		{
			std::string name;
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

		struct FunctionCall
		{
			std::vector<Expression> parameters;
		};

		struct UnaryOperatorCall
		{
			Operator op;
			value_ptr<Expression> operand;
		};

		struct BinaryOperatorCall
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

		struct DesignatedInitializerConstructor
		{
			TypeId constructed_type;
			std::vector<DesignatedInitializer> parameters;
		};

		struct DataCall
		{
			value_ptr<Expression> operand;
		};

		struct SizeCall
		{
			value_ptr<Expression> operand;
		};

		namespace detail
		{
			using ExpressionBase = std::variant<
				Literal<int>, Literal<float>, Literal<bool>, Literal<std::string>, Literal<uninit_t>,
				Dereference, Addressof, Subscript,
				Identifier, MemberVariable,
				Function, FunctionTemplate,	
				FunctionCall, UnaryOperatorCall, BinaryOperatorCall,
				If, StatementBlock,
				Constructor, DesignatedInitializerConstructor,
				DataCall, SizeCall
			>;
		}

	} // namespace expression

	//struct Expression : public expression::detail::ExpressionBase
	//{
	//	using Base = expression::detail::ExpressionBase;
	//	using Base::Base;
	//	constexpr auto as_variant() noexcept -> Base & { return *this; }
	//	constexpr auto as_variant() const noexcept -> Base const & { return *this; }
	//};
	struct Expression
	{
		Expression() noexcept = default;
		Expression(expression::detail::ExpressionBase var, std::string_view src) noexcept
			: variant(std::move(var))
			, source(src)
		{}

		expression::detail::ExpressionBase variant;
		std::string_view source;
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

	struct DesignatedInitializer
	{
		std::string member_name;
		Expression assigned_expression;
	};

} // namespace incomplete

