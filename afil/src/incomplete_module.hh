#pragma once

#include "syntax_error.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include <vector>
#include <string>

namespace parser
{
	struct TypeName;
}

namespace incomplete
{

	struct Statement;

	struct Module
	{
		struct File
		{
			std::string filename;
			std::string source;
		};

		std::vector<File> files;
		std::vector<int> dependencies;
		std::vector<incomplete::Statement> statements;
	};

	auto load_module_and_dependencies(std::string_view module_name, std::string_view source) noexcept -> expected<std::vector<incomplete::Module>, SyntaxError>;
	auto sort_modules_by_dependencies(span<incomplete::Module const> modules) noexcept -> expected<std::vector<int>, SyntaxError>;
	auto module_path_from_name(std::string_view module_name) noexcept -> std::string;
	auto file_that_contains(Module const & module, std::string_view src) noexcept -> Module::File const *;

} // namespace incomplete
