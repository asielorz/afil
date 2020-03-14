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
		std::vector<parser::TypeName> type_names;
	};

	auto load_module_and_dependencies(std::string_view module_name, std::string_view source) noexcept -> expected<std::vector<incomplete::Module>, SyntaxError>;
	auto sort_modules_by_dependencies(span<incomplete::Module const> modules) noexcept -> expected<std::vector<int>, SyntaxError>;
	auto scan_type_names(span<incomplete::Module const> modules, int index, out<std::vector<parser::TypeName>> type_names) noexcept -> void;
	auto module_path_from_name(std::string_view module_name) noexcept -> std::string;

} // namespace incomplete
