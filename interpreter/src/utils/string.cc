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
