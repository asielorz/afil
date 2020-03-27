#pragma once

#include "incomplete_expression.hh"

namespace incomplete
{

	struct Statement;

	namespace statement
	{
		struct VariableDeclaration
		{
			std::string variable_name;
			std::optional<Expression> assigned_expression;
			incomplete::TypeId type;
		};

		struct LetDeclaration
		{
			std::string variable_name;
			Expression assigned_expression;
			bool is_mutable;
			bool is_reference;
		};

		struct ExpressionStatement
		{
			Expression expression;
		};

		struct Return
		{
			Expression returned_expression;
		};

		struct If
		{
			Expression condition;
			value_ptr<Statement> then_case;
			value_ptr<Statement> else_case;
		};

		struct StatementBlock
		{
			//Scope scope;
			std::vector<Statement> statements;
		};

		struct While
		{
			Expression condition;
			value_ptr<Statement> body;
		};

		struct For
		{
			//Scope scope;
			value_ptr<Statement> init_statement;
			Expression condition;
			Expression end_expression;
			value_ptr<Statement> body;
		};

		struct Break {};
		struct Continue {};

		struct ImportBlock
		{
			std::vector<ExternFunction> imported_functions;
		};

		struct StructDeclaration
		{
			Struct declared_struct;
		};

		struct StructTemplateDeclaration
		{
			StructTemplate declared_struct_template;
		};

		using Variant = std::variant<
			VariableDeclaration, LetDeclaration, ExpressionStatement,
			If, StatementBlock, While, For,
			Return, Break, Continue,
			StructDeclaration, StructTemplateDeclaration,
			ImportBlock
		>;

	} // namespace statement

	struct Statement
	{
		Statement() noexcept = default;
		Statement(statement::Variant var, std::string_view src) noexcept
			: variant(std::move(var))
			, source(src)
		{}

		statement::Variant variant;
		std::string_view source;
	};

} // namespace incomplete
