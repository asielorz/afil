#include "afil.hh"
#include "program.hh"
#include "parser.hh"
#include "incomplete_module.hh"
#include "template_instantiation.hh"
#include "utils/string.hh"

namespace afil
{

	auto parse_module(std::string_view module_name) noexcept -> expected<complete::Program, SyntaxError>
	{
		std::string const module_path = incomplete::module_path_from_name(module_name);
		auto source = load_whole_file(std::string_view(module_path));
		if (!source.has_value())
			return Error(make_complete_syntax_error("", join("Cannot open module file ", module_path), "", ""));
		return parse_module(module_name, *source);
	}

	auto parse_module(std::string_view module_name, std::string_view source) noexcept -> expected<complete::Program, SyntaxError>
	{
		try_call_decl(std::vector<incomplete::Module> incomplete_modules, incomplete::load_module_and_dependencies(module_name, source));
		try_call_decl(std::vector<int> const parse_order, parser::parse_modules(incomplete_modules));

		complete::Program program;
		for (int i : parse_order)
		{
			auto semantic_analysis_result = instantiation::semantic_analysis(incomplete_modules[i].statements, out(program));
			if (!semantic_analysis_result.has_value())
				return Error(complete_syntax_error(semantic_analysis_result.error(), source));
		}

		return std::move(program);
	}

} // namespace afil
