#include "incomplete_module.hh"
#include "lexer.hh"
#include "incomplete_statement.hh"
#include "parser.hh"
#include "utils/algorithm.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include "utils/string.hh"
#include "utils/variant.hh"
#include "utils/load_dll.hh"
#include <string>
#include <filesystem>
#include <map>

using namespace std::literals;

namespace incomplete
{

	auto sort_modules_by_dependencies(span<incomplete::Module const> modules) noexcept -> expected<std::vector<int>, SyntaxError>
	{
		std::vector<int> modules_to_sort(modules.size());
		std::iota(modules_to_sort, 0);
		std::vector<int> sorted_modules;
		sorted_modules.reserve(modules.size());
		std::vector<bool> has_been_inserted(modules.size(), false);

		auto const has_index_been_inserted = [&](int i)
		{
			return has_been_inserted[i];
		};

		while (!modules_to_sort.empty())
		{
			int inserted = 0;
			for (size_t i = 0; i < modules_to_sort.size();)
			{
				int const module_index = modules_to_sort[i];
				if (std::all_of(modules[module_index].dependencies, has_index_been_inserted))
				{
					has_been_inserted[module_index] = true;
					sorted_modules.push_back(module_index);
					std::swap(modules_to_sort[i], modules_to_sort.back());
					modules_to_sort.pop_back();
					inserted++;
				}
				else
					i++;
			}

			if (inserted == 0 && !modules_to_sort.empty())
				return make_complete_syntax_error("", "There is a circular dependency", "", "");
		}

		return sorted_modules;
	}

	constexpr std::string_view import_keyword = "import";
	constexpr std::string_view module_extension = ".afilm";

	[[nodiscard]] auto scan_dependencies(
		std::string_view module_name,
		std::string_view module_source,
		std::vector<incomplete::Module> & modules,
		std::map<std::filesystem::path, int> & already_scanned_modules,
		std::map<std::filesystem::path, std::string_view> & already_loaded_files) noexcept -> expected<void, SyntaxError>
	{
		incomplete::Module & current_module = modules.emplace_back();

		std::string_view source = ignore_empty_lines(module_source);

		while (!source.empty())
		{
			std::string_view const first_line = extract_first_line(source);

			if (starts_with(first_line, import_keyword))
			{
				std::string_view const dependency_name = ignore_whitespace(first_line.substr(import_keyword.size()));
				std::filesystem::path canonical_path = std::filesystem::canonical(join(dependency_name, module_extension));

				auto const it = already_scanned_modules.find(canonical_path);
				if (it != already_scanned_modules.end())
				{
					current_module.dependencies.push_back(it->second);
				}
				else
				{
					std::optional<std::string> dependency_source = load_whole_file(canonical_path);
					if (!dependency_source.has_value())
						return make_complete_syntax_error(module_name, "Could not open module file.", module_source, module_name);

					int const dependency_index = static_cast<int>(modules.size());
					current_module.dependencies.push_back(dependency_index);
					already_scanned_modules[std::move(canonical_path)] = dependency_index;
					try_call_void(scan_dependencies(dependency_name, std::move(*dependency_source), modules, already_scanned_modules, already_loaded_files));
				}
			}
			else if (starts_with(first_line, "file"))
			{
				std::string_view const file_name = ignore_whitespace(first_line.substr(import_keyword.size()));
				std::filesystem::path canonical_path = std::filesystem::canonical(file_name);

				auto const it = already_loaded_files.find(canonical_path);
				if (it != already_loaded_files.end())
					return make_complete_syntax_error(first_line, join("File already loaded for module ", it->second), module_source, module_name);

				std::optional<std::string> file_source = load_whole_file(canonical_path);
				if (!file_source.has_value())
					return make_complete_syntax_error(first_line, join("Could not open file ", canonical_path), module_source, module_name);

				current_module.files.push_back({ std::string(file_name), std::move(*file_source) });
				already_loaded_files[std::move(canonical_path)] = module_name;
			}
			else
			{
				return make_complete_syntax_error(first_line, "Expected file or import in module file.", module_source, module_name);
			}
		}

		return success;
	}

	auto load_module_and_dependencies(std::string_view module_name, std::string_view source) noexcept -> expected<std::vector<incomplete::Module>, SyntaxError>
	{
		std::vector<incomplete::Module> modules;
		std::map<std::filesystem::path, int> already_scanned_modules;
		std::map<std::filesystem::path, std::string_view> already_loaded_files;

		try_call_void(scan_dependencies(module_name, source, modules, already_scanned_modules, already_loaded_files));
		return std::move(modules);
	}

	auto scan_type_names(span<incomplete::Module const> modules, int index, out<std::vector<parser::TypeName>> type_names) noexcept -> void
	{
		for (parser::TypeName const & type_name : modules[index].type_names)
			type_names->push_back(type_name);

		for (int const dependency_index : modules[index].dependencies)
			scan_type_names(modules, dependency_index, type_names);
	}

} // namespace incomplete

