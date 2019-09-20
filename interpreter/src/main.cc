#pragma once

#include "program.hh"
#include "parser.hh"
#include "interpreter.hh"
#include <fstream>

std::string load_whole_file(char const path[]) noexcept
{
	std::ifstream file(path);
	return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

auto main(int argc, char const * const argv[]) -> int
{
	if (argc == 2)
		return interpreter::run(parser::parse_source(load_whole_file(argv[1])));

	return -1;
}
