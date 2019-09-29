#pragma once

#include "function_id.hh"
#include <string_view>
#include <vector>
#include <variant>

struct TypeId
{
	#pragma warning (disable : 4201)
	union
	{
		struct
		{
			unsigned is_language_reseved	: 1;
			unsigned is_mutable				: 1;
			unsigned is_reference			: 1;
			unsigned index					: 29;
		};
		unsigned flat_value;
	};

	static constexpr auto with_index(unsigned index) noexcept -> TypeId { return TypeId{false, false, false, index}; }

	static TypeId const int_;
	static TypeId const float_;
	static TypeId const bool_;

	static TypeId const none;
	static TypeId const function;
};

constexpr auto operator == (TypeId a, TypeId b) noexcept -> bool { return a.flat_value == b.flat_value; };
constexpr auto operator != (TypeId a, TypeId b) noexcept -> bool { return !(a == b); };

auto is_convertible(TypeId from, TypeId to) noexcept -> bool;
auto make_reference(TypeId type) noexcept -> TypeId;

struct Variable
{
	std::string_view name;
	TypeId type;
	int offset;
};

struct FunctionName
{
	std::string_view name;
	FunctionId id;
};

struct Scope
{
	int stack_frame_size = 0;
	int stack_frame_alignment = 1; // Alignment requirement of the stack frame.
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
};
enum struct ScopeType { global, function, block };

struct CurrentScope
{
	Scope * scope;
	ScopeType type;
};
using ScopeStack = std::vector<CurrentScope>;

namespace lookup_result
{
	struct Nothing {};
	struct Variable { TypeId variable_type; int variable_offset; };
	struct GlobalVariable { TypeId variable_type; int variable_offset; };
	struct OverloadSet { std::vector<FunctionId> function_ids; };
}
auto lookup_name(ScopeStack const & scope_stack, std::string_view name) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>;

auto local_variable_offset(ScopeStack const & scope_stack) noexcept -> int;
