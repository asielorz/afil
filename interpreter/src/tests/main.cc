#pragma once

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

int main()
{
	if (Catch::Session().run())
		system("pause");
}
