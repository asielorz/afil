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
constexpr FunctionId pointer_equal_intrinsic = {FunctionId::Type::intrinsic, 100}; // TODO: Depends on pointer size
constexpr FunctionId pointer_three_way_compare_intrinsic = {FunctionId::Type::intrinsic, 101};

constexpr auto operator == (FunctionId a, FunctionId b) noexcept -> bool { return a.type == b.type && a.index == b.index; }
constexpr auto operator != (FunctionId a, FunctionId b) noexcept -> bool { return !(a == b); }

struct FunctionTemplateId { unsigned index; };
