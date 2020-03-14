#include "lexer.hh"
#include "incomplete_statement.hh"
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

namespace parser
{
	struct TypeName
	{
		std::string_view name;
		enum struct Type { type, struct_template, template_parameter } type;
	};

	auto built_in_type_names() noexcept -> std::vector<TypeName>
	{
		return {
			{"void",		TypeName::Type::type},
			{"int",			TypeName::Type::type},
			{"float",		TypeName::Type::type},
			{"bool",		TypeName::Type::type},
			{"char",		TypeName::Type::type},
			{"type",		TypeName::Type::type},
			{"uninit_t",	TypeName::Type::type}
		};
	}
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
}

namespace parser
{

	constexpr std::string_view import_keyword = "import";
	constexpr std::string_view module_extension = ".afilm";

	using TokenType = lex::Token::Type;

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

				current_module.files.push_back({std::string(file_name), std::move(*file_source)});
				already_loaded_files[std::move(canonical_path)] = module_name;
			}
			else
			{
				return make_complete_syntax_error(first_line, "Expected file or import in module file.", module_source, module_name);
			}
		}

		return success;
	}

	auto load_module(std::string_view module_name, std::string_view source) noexcept -> expected<std::vector<incomplete::Module>, SyntaxError>
	{
		std::vector<incomplete::Module> modules;
		std::map<std::filesystem::path, int> already_scanned_modules;
		std::map<std::filesystem::path, std::string_view> already_loaded_files;

		try_call_void(scan_dependencies(module_name, source, modules, already_scanned_modules, already_loaded_files));
		return std::move(modules);
	}

	auto scan_type_names(span<incomplete::Module const> modules, int index, out<std::vector<TypeName>> type_names) noexcept -> void
	{
		for (TypeName const & type_name : modules[index].type_names)
				type_names->push_back(type_name);

		for (int const dependency_index : modules[index].dependencies)
			scan_type_names(modules, dependency_index, type_names);
	}

	[[nodiscard]] auto parse_global_scope(
		span<lex::Token const> tokens,
		std::vector<TypeName> & type_names,
		out<std::vector<incomplete::Statement>> global_initialization_statements
	) noexcept -> expected<void, PartialSyntaxError>;

	[[nodiscard]] auto parse_module(span<incomplete::Module> modules, int index) noexcept -> expected<void, SyntaxError>
	{
		std::vector<lex::Token> tokens;
		for (auto const & file : modules[index].files)
		{
			auto new_tokens = lex::tokenize(file.source, std::move(tokens));
			if (new_tokens.has_value())
				tokens = std::move(*new_tokens);
			else
				return Error(complete_syntax_error(new_tokens.error(), file.source, file.filename));
		}

		std::vector<TypeName> type_names = built_in_type_names();
		scan_type_names(modules, index, out(type_names));
		size_t const imported_type_count = type_names.size();

		std::vector<incomplete::Statement> statements;

		auto result = parse_global_scope(tokens, type_names, out(statements));
		if (!result.has_value())
		{
			std::string_view const source_with_error = result.error().error_in_source;
			auto const file = std::find_if(modules[index].files, [=](const incomplete::Module::File & file)
			{
				return is_contained_in(file.source, source_with_error);
			});
			assert(file != modules[index].files.end());

			return Error(complete_syntax_error(std::move(result.error()), file->source, file->filename));
		}

		type_names.erase(type_names.begin(), type_names.begin() + imported_type_count);
		modules[index].type_names = std::move(type_names);
		modules[index].statements = std::move(statements);

		return success;
	}

	[[nodiscard]] auto parse_modules(span<incomplete::Module> modules) noexcept -> expected<void, SyntaxError>
	{

	}

}
