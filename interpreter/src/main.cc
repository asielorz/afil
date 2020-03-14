#pragma once

#include "afil.hh"
#include "interpreter.hh"
#include <iostream>

auto main(int argc, char const * const argv[]) -> int
{
	(void)(argc, argv);

	auto program = afil::parse_module("main");
	if (program.has_value())
		return interpreter::run(*program);
	else
	{
		std::cout << program.error() << '\n';
		system("pause");
		return -1;
	}
}
