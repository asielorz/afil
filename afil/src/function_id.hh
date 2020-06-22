#pragma once

struct FunctionId
{
	enum struct Type : unsigned { program, imported, intrinsic };

	FunctionId() noexcept = default;
	constexpr FunctionId(Type type_, unsigned index_) noexcept : type(type_), index(index_) {}

	Type type : 2;
	unsigned index : 30;
};
constexpr FunctionId invalid_function_id = {FunctionId::Type::intrinsic, (1u << 30u) - 1u};
constexpr FunctionId deleted_function_id = {FunctionId::Type::intrinsic, (1u << 30u) - 2u};
constexpr FunctionId pointer_equal_intrinsic_32_bit = {FunctionId::Type::intrinsic, 87};
constexpr FunctionId pointer_three_way_compare_intrinsic_32_bit = {FunctionId::Type::intrinsic, 88};
constexpr FunctionId pointer_equal_intrinsic_64_bit = {FunctionId::Type::intrinsic, 100};
constexpr FunctionId pointer_three_way_compare_intrinsic_64_bit = {FunctionId::Type::intrinsic, 101};
constexpr FunctionId pointer_equal_intrinsic = (sizeof(void *) == 4) ? pointer_equal_intrinsic_32_bit : pointer_equal_intrinsic_64_bit;
constexpr FunctionId pointer_three_way_compare_intrinsic = (sizeof(void *) == 4) ? pointer_three_way_compare_intrinsic_32_bit : pointer_three_way_compare_intrinsic_64_bit;

constexpr auto operator == (FunctionId a, FunctionId b) noexcept -> bool { return a.type == b.type && a.index == b.index; }
constexpr auto operator != (FunctionId a, FunctionId b) noexcept -> bool { return !(a == b); }

struct FunctionTemplateId
{
	unsigned is_intrinsic : 1;
	unsigned index : 31;
};
