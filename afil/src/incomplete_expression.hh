#pragma once

#include "built_in_structures.hh"
#include "incomplete_scope.hh"
#include "operator.hh"

namespace incomplete
{

	struct Statement;
	struct Expression;
	struct DesignatedInitializer;
	struct ExpressionToTest;

	namespace expression
	{

		template <typename T>
		struct Literal
		{
			Literal() noexcept = default;
			Literal(T val) noexcept : value(std::move(val)) {}

			T value;
		};

		struct Identifier
		{
			std::string_view name;
		};

		struct MemberVariable
		{
			std::string_view name;
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

		struct ExternFunction
		{
			incomplete::ExternFunction function;
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

		struct Compiles
		{
			struct FakeVariable
			{
				TypeId type;
				std::string_view name;
			};
			

			std::vector<FakeVariable> variables;
			std::vector<ExpressionToTest> body;
		};

		using Variant = std::variant<
			Literal<int>, Literal<float>, Literal<bool>, Literal<std::string>, Literal<uninit_t>, Literal<incomplete::TypeId>,
			Dereference, Addressof, Subscript,
			Identifier, MemberVariable,
			Function, FunctionTemplate,	ExternFunction,
			FunctionCall, UnaryOperatorCall, BinaryOperatorCall,
			If, StatementBlock,
			Constructor, DesignatedInitializerConstructor,
			DataCall, SizeCall, Compiles
		>;

	} // namespace expression

	struct Expression
	{
		Expression() noexcept = default;
		Expression(expression::Variant var, std::string_view src) noexcept
			: variant(std::move(var))
			, source(src)
		{}

		expression::Variant variant;
		std::string_view source;
	};

	struct MemberVariable
	{
		std::string_view name;
		TypeId type;
		std::optional<Expression> initializer_expression;
	};

	struct Struct
	{
		std::string_view name;
		std::vector<MemberVariable> member_variables;
	};

	struct StructTemplate : Struct
	{
		std::vector<TemplateParameter> template_parameters;
	};

	struct DesignatedInitializer
	{
		std::string_view member_name;
		Expression assigned_expression;
	};

	struct ExpressionToTest
	{
		Expression expression;
		std::optional<TypeId> expected_type;
	};

} // namespace incomplete

