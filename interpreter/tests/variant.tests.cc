#include <catch2/catch.hpp>
#include "variant.hh"

TEST_CASE("variant_union returns the union of two variant types")
{
	using u = variant_union<
		std::variant<int, float, bool>,
		std::variant<unsigned, double, char>
	>;
	static_assert(std::is_same_v<u, std::variant<int, float, bool, unsigned, double, char>>);
}

TEST_CASE("A variant can be upcasted to a variant type that is a superset of it")
{
	using var_t = std::variant<int, float>;
	using superset_t = std::variant<int, float, char>;

	superset_t const v = upcast<superset_t>(var_t(5));
	REQUIRE(has_type<int>(v));
	REQUIRE(*try_get<int>(v) == 5);
}

TEST_CASE("Types do not need the first or in order")
{
	using var_t = std::variant<int, float>;
	using superset_t = std::variant<char, float, bool, int>;

	superset_t const v = upcast<superset_t>(var_t(5));
	REQUIRE(has_type<int>(v));
	REQUIRE(*try_get<int>(v) == 5);
}

TEST_CASE("Casting of rvalues will move")
{
	struct MoveOnly
	{
		MoveOnly() noexcept = default;
		MoveOnly(const MoveOnly &) = delete;
		MoveOnly(MoveOnly &&) noexcept = default;
	};

	using var_t = std::variant<int, float, MoveOnly>;
	using superset_t = std::variant<char, float, MoveOnly, bool, int>;

	superset_t const v = upcast<superset_t>(var_t(MoveOnly()));
	REQUIRE(has_type<MoveOnly>(v));
}
