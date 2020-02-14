#include "afil.hh"
#include "program.hh"
#include "parser.hh"
#include "template_instantiation.hh"

namespace afil
{

	auto parse_source(std::string_view source) noexcept -> complete::Program
	{
		complete::Program program;
		parse_source(source, out(program));
		return program;
	}

	auto parse_source(std::string_view source, out<complete::Program> program) noexcept -> void
	{
		instantiation::instantiate_templates(parser::parse_source(source, *program), program);
	}

} // namespace afil
