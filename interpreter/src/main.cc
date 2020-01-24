#pragma once

#include <fstream>

std::string load_whole_file(char const path[]) noexcept
{
	std::ifstream file(path);
	return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

auto main(int argc, char const * const argv[]) -> int
{
	(void)(argc, argv);
	return 0;
}
