#include "string.hh"
#include <fstream>

auto is_null_terminated(std::string_view str) noexcept -> bool
{
	return str.data()[str.size()] == '\0';
}

auto load_whole_file(std::string_view path) noexcept -> std::optional<std::string>
{
	auto const null_terminated_path = make_null_terminated(path);
	std::ifstream file(null_terminated_path.data());
	if (file.is_open())
		return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
	else
		return std::nullopt;
}

auto load_whole_file(std::filesystem::path path) noexcept -> std::optional<std::string>
{
	std::ifstream file(path);
	if (file.is_open())
		return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
	else
		return std::nullopt;
}

auto replace(std::string_view string, std::string_view old_substr, std::string_view new_substr) noexcept -> std::string
{
	assert(old_substr.size() > 0);

	std::string result;
	for (size_t i = 0; i < string.size();)
	{
		size_t const new_i = string.find(old_substr, i);
		if (new_i == std::string_view::npos)
		{
			result += string.substr(i, string.size() - i);
			break;
		}

		result += string.substr(i, new_i - i);
		result += new_substr;
		i = new_i + old_substr.size();
	}

	return result;
}

auto make_string_view(char const first[], char const last[]) noexcept -> std::string_view
{
	assert(last >= first);
	auto const size = last - first;
	return std::string_view(first, size);
}

auto begin_ptr(std::string_view sv) noexcept -> char const *
{
	return sv.data();
}

auto end_ptr(std::string_view sv) noexcept -> char const *
{
	return sv.data() + sv.size();
}

auto count_lines(std::string_view text) noexcept -> int
{
	// Empty string has 0 lines.
	if (text.empty())
		return 0;
	else
		return static_cast<int>(1 + std::count(begin(text), end(text), '\n'));
}

auto split_first_line(std::string_view text) noexcept -> std::pair<std::string_view, std::string_view>
{
	auto const newline = text.find('\n');

	if (newline == std::string_view::npos)
		return { text, std::string_view() };
	else
		return { text.substr(0, newline), text.substr(newline + 1) };
}

auto extract_first_line(std::string_view & text) noexcept -> std::string_view
{
	auto const[line, rest] = split_first_line(text);
	text = rest;
	return line;
}

auto ignore_empty_lines(std::string_view text) noexcept -> std::string_view
{
	while (!text.empty())
	{
		auto const[line, rest] = split_first_line(text);
		if (!std::all_of(begin(line), end(line), is_whitespace))
			return text;
		text = rest;
	}
	return text;
}

auto ignore_starting_whitespace(std::string_view text) noexcept -> std::string_view
{
	size_t i = 0;
	while (i < text.size() && is_whitespace_or_newline(text[i]))
		++i;
	return text.substr(i);
}

auto ignore_trailing_whitespace(std::string_view text) noexcept -> std::string_view
{
	size_t i = text.size();
	while (i > 0 && is_whitespace_or_newline(text[i - 1]))
		--i;
	return text.substr(0, i);
}

auto ignore_whitespace(std::string_view text) noexcept -> std::string_view
{
	return ignore_starting_whitespace(ignore_trailing_whitespace(text));
}

auto starts_with(std::string_view string, std::string_view prefix) noexcept -> bool
{
	return std::equal(begin(prefix), end(prefix), string.data());
}

auto is_contained_in(std::string_view string, std::string_view substr) noexcept -> bool
{
	return substr.data() >= string.data() && end_ptr(substr) <= end_ptr(string);
}

auto indent(int indentation_level) noexcept -> std::string
{
	return std::string(indentation_level, '\t');
}
