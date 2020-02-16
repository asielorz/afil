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
		std::vector<incomplete::Statement> incomplete_program;
		try_call(assign_to(incomplete_program), parser::parse_source(source, *program));
		/*try_call_void*/(instantiation::instantiate_templates(incomplete_program, program));
		return expected<void, SyntaxError>();
	}

} // namespace afil
