#include "template_instantiation.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "utils/out.hh"
#include "utils/variant.hh"
#include "utils/overload.hh"

enum struct ScopeType { global, function, block };
struct CurrentScope
{
	complete::Scope * scope;
	ScopeType type;
};
using ScopeStack = std::vector<CurrentScope>;
using ScopeStackView = span<const CurrentScope>;

namespace instantiation
{

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression, 
		std::vector<complete::TypeId> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> complete::Expression
	{
		(void)(incomplete_expression, program, template_parameters);
		return {};
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement, 
		std::vector<complete::TypeId> & template_parameters, 
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> std::optional<complete::Statement>
	{
		(void)(program, template_parameters);

		auto const visitor = overload(
			[](incomplete::statement::VariableDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::ExpressionStatement const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::If const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::StatementBlock const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::While const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::For const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::Return const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::Break const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Break();
			},
			[](incomplete::statement::Continue const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Continue();
			},
			[](incomplete::statement::StructDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return std::nullopt;
			},
			[](incomplete::statement::StructTemplateDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return std::nullopt;
			}
		);

		return std::visit(visitor, incomplete_statement.as_variant());
	}

	auto instantiate_templates(incomplete::Program const & incomplete_program) noexcept -> complete::Program
	{
		complete::Program complete_program;
		std::vector<complete::TypeId> template_parameters;
		ScopeStack scope_stack;
		scope_stack.push_back({&complete_program.global_scope, ScopeType::global});

		for (incomplete::Statement const & incomplete_statement : incomplete_program.global_initialization_statements)
		{
			auto complete_statement = instantiate_statement(incomplete_statement, template_parameters, scope_stack, out(complete_program));
			if (complete_statement)
			{
				raise_syntax_error_if_not(
					has_type<complete::statement::VariableDeclaration>(*complete_statement),
					"Only variable declarations, function declarations and struct declarations allowed at global scope."
				);
				complete_program.global_initialization_statements.push_back(std::move(*complete_statement));
			}
		}

		return complete_program;
	}

} // namespace instantiation
