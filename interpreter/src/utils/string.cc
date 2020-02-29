#include "string.hh"
#include <fstream>

auto is_null_terminated(std::string_view str) noexcept -> bool
{
	return str.data()[str.size()] == '\0';
}

auto load_whole_file(std::string_view path) noexcept -> std::string
{
	auto const null_terminated_path = make_null_terminated(path);
	std::ifstream file(null_terminated_path.data());
	assert(file);
	return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

auto load_whole_file(std::filesystem::path path) noexcept -> std::string
{
	std::ifstream file(path);
	assert(file);
	return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
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
