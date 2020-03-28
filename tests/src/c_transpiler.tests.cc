#include "c_transpiler.hh"
#include "program.hh"
#include <catch2/catch.hpp>
#include <fstream>

using namespace std::literals;

#if 0
TEST_CASE("Fibonacci deleteme")
{
	auto const src = R"(
		struct vec3
		{
			float x;
			float y;
			float z;
		}
		
		struct aabb
		{
			vec3 min;
			vec3 max;
		}

		let is_in_range = fn(float x, float min, float max) -> bool
		{
			return x >= min and x <= max;
		};

		let intersects = fn(aabb box, vec3 point) -> bool
		{
			return 
				is_in_range(point.x, box.min.x, box.max.x) and
				is_in_range(point.y, box.min.y, box.max.y) and
				is_in_range(point.z, box.min.z, box.max.z);
		};

		let main = fn() -> int
		{
			aabb mut box = uninit;
			box.min = vec3(0.0, 0.0, 0.0);
			box.max = vec3(3.0, 3.0, 3.0);
			if (intersects(box, vec3(1.0, 1.0, 1.0)))
				return 0;
			else
				return 1;
		};
	)"sv;

	complete::Program const & program = tests::assert_get(tests::parse_source(src));
	std::cout << "**************************************************************************************\n";
	std::cout << c_transpiler::transpile_to_c(program);
	std::ofstream file("test_output.c");
	file << c_transpiler::transpile_to_c(program);
	std::cout << "**************************************************************************************\n";

	system("pause");
}
#endif
