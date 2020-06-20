#pragma once

#include "incomplete_expression.hh"

namespace incomplete
{

	struct Statement;

	namespace statement
	{
		struct LetDeclaration
		{
			std::string variable_name;
			Expression assigned_expression;
			bool is_mutable;
			bool is_reference;
		};

		struct PlacementLet
		{
			//std::string variable_name;
			Expression address_expression;
			Expression assigned_expression;
		};

		struct UninitDeclaration
		{
			std::string variable_name;
			TypeId variable_type;
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
			std::vector<Statement> statements;
		};

		struct While
		{
			Expression condition;
			value_ptr<Statement> body;
		};

		struct For
		{
			value_ptr<Statement> init_statement;
			Expression condition;
			Expression end_expression;
			value_ptr<Statement> body;
		};

		struct Break {};
		struct Continue {};

		struct StructDeclaration
		{
			Struct declared_struct;
		};

		struct StructTemplateDeclaration
		{
			StructTemplate declared_struct_template;
		};

		struct TypeAliasDeclaration
		{
			std::string_view name;
			Expression type;
		};

		struct NamespaceDeclaration
		{
			std::vector<std::string_view> names;
			std::vector<Statement> statements;
		};

		struct ConversionDeclaration
		{
			bool is_implicit = false;
			Expression conversion_function;
		};

		using Variant = std::variant<
			LetDeclaration, PlacementLet, UninitDeclaration, ExpressionStatement,
			If, StatementBlock, While, For,
			Return, Break, Continue,
			StructDeclaration, StructTemplateDeclaration, TypeAliasDeclaration,
			NamespaceDeclaration, ConversionDeclaration
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
