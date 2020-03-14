#include "afil.hh"
#include "program.hh"
#include "parser.hh"
#include "incomplete_module.hh"
#include "template_instantiation.hh"

namespace afil
{

	auto parse_module(std::string_view module_name, std::string_view source) noexcept -> expected<complete::Program, SyntaxError>
	{
		try_call_decl(std::vector<incomplete::Module> incomplete_modules, incomplete::load_module_and_dependencies(module_name, source));
		try_call_void(parser::parse_modules(incomplete_modules));

		complete::Program program;
		for (incomplete::Module const & incomplete_module : incomplete_modules)
		{
			auto semantic_analysis_result = instantiation::semantic_analysis(incomplete_module.statements, out(program));
			if (!semantic_analysis_result.has_value())
				return Error(complete_syntax_error(semantic_analysis_result.error(), source));
		}

		return std::move(program);
	}

} // namespace afil
