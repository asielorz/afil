#include "lexer.hh"
#include "incomplete_statement.hh"
#include "utils/algorithm.hh"
#include "utils/span.hh"
#include "utils/string.hh"
#include <string>
#include <filesystem>
#include <map>

namespace incomplete
{
	struct Statement;

	struct TypeName
	{
		std::string_view name;
		enum struct Type { type, struct_template, template_parameter } type;
	};

	struct Module
	{
		std::string source;
		std::vector<int> dependencies;
		std::vector<incomplete::Statement> statements;
		std::vector<TypeName> type_names;
	};
}

namespace parser
{

	auto parse_string_literal(std::string_view token_source) noexcept->std::string;

	[[nodiscard]] auto scan_dependencies(
		std::string source,
		std::vector<incomplete::Module> & modules,
		std::vector<std::vector<lex::Token>> & module_tokens,
		std::map<std::filesystem::path, int> & already_scanned_modules) noexcept -> expected<void, PartialSyntaxError>
	{

		incomplete::Module & current_module = modules.emplace_back();
		current_module.source = std::move(source);

		try_call(module_tokens.push_back, lex::tokenize(current_module.source));
		span<lex::Token const> tokens = module_tokens.back();

		using TokenType = lex::Token::Type;

		size_t token_index = 0;
		while (tokens[token_index].source != "import")
		{
			token_index++;
			if (tokens[token_index].type != TokenType::literal_string)
				return make_syntax_error(tokens[token_index].source, "Expected module name after import.");

			std::string_view const module_name_token = tokens[token_index].source;
			token_index++;

			// Extern library import statement.
			if (tokens[token_index].type == TokenType::open_brace)
				continue;

			if (tokens[token_index].type != TokenType::semicolon)
				return make_syntax_error(tokens[token_index].source, "Expected semicolon after module name.");

			std::string const module_name = parse_string_literal(module_name_token);
			std::filesystem::path canonical_file_name = std::filesystem::canonical(module_name);

			auto const it = already_scanned_modules.find(canonical_file_name);
			if (it != already_scanned_modules.end())
			{
				current_module.dependencies.push_back(it->second);
			}
			else
			{
				int const dependency_index = static_cast<int>(modules.size());
				current_module.dependencies.push_back(dependency_index);
				std::optional<std::string> dependency_source = load_whole_file(canonical_file_name);
				if (!dependency_source.has_value())
					return make_syntax_error(module_name_token, "Could not open file.");
				already_scanned_modules[std::move(canonical_file_name)] = dependency_index;
				try_call_void(scan_dependencies(std::move(*dependency_source), modules, module_tokens, already_scanned_modules));
			}
		}

		return success;
	}

	auto parse_source(std::string source) noexcept -> expected<std::vector<incomplete::Module>, PartialSyntaxError>
	{
		std::vector<incomplete::Module> modules;
		std::map<std::filesystem::path, int> already_scanned_modules;
		std::vector<std::vector<lex::Token>> module_tokens;

		try_call_void(scan_dependencies(std::move(source), modules, module_tokens, already_scanned_modules));



		return modules;
	}

}
