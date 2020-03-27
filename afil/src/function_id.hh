#pragma once

struct FunctionId
{
	unsigned is_extern : 1;
	unsigned index : 31;

};
constexpr FunctionId invalid_function_id = { 1, (1u << 31u) - 1u };

constexpr auto operator == (FunctionId a, FunctionId b) noexcept -> bool { return a.is_extern == b.is_extern && a.index == b.index; }
constexpr auto operator != (FunctionId a, FunctionId b) noexcept -> bool { return !(a == b); }

struct FunctionTemplateId { unsigned index; };
