#include "afil.hh"
#include "program.hh"
#include "parser.hh"
#include "template_instantiation.hh"

namespace afil
{

	auto parse_source(std::string_view source) noexcept -> expected<complete::Program, SyntaxError>
	{
		complete::Program program;
		try_call_void(parse_source(source, out(program)));
		return std::move(program);
	}

	auto parse_source(std::string_view source, out<complete::Program> program) noexcept -> expected<void, SyntaxError>
	{
		auto incomplete_program = parser::parse_source(source, *program);
		if (!incomplete_program.has_value())
			return Error(complete_syntax_error(incomplete_program.error(), source));

		auto semantic_analysis_result = instantiation::semantic_analysis(*incomplete_program, program);
		if (!semantic_analysis_result.has_value())
			return Error(complete_syntax_error(semantic_analysis_result.error(), source));
		else
			return success;
	}

} // namespace afil
