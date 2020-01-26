#include "template_instantiation.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "utils/out.hh"
#include "utils/variant.hh"

namespace instantiation
{

	auto instantiate_expression(incomplete::Expression const & incomplete_expression, out<complete::Program> program) -> complete::Expression
	{
		(void)(incomplete_expression, program);
		return {};
	}

	auto instantiate_statement(incomplete::Statement const & incomplete_statement, out<complete::Program> program) -> std::optional<complete::Statement>
	{
		(void)(incomplete_statement, program);
		return {};
	}

	auto instantiate_templates(incomplete::Program const & incomplete_program) noexcept -> complete::Program
	{
		complete::Program complete_program;

		for (incomplete::Statement const & incomplete_statement : incomplete_program.global_initialization_statements)
		{
			auto complete_statement = instantiate_statement(incomplete_statement, out(complete_program));
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
