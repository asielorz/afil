#pragma once

#include "function_id.hh"
#include "span.hh"
#include <string_view>
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

	static TypeId const void_; // TODO: Maybe void can be confusing so think of another name?
	static TypeId const int_;
	static TypeId const float_;
	static TypeId const bool_;

	static TypeId const none;
	static TypeId const function;
	static TypeId const deduce;
};

constexpr auto operator == (TypeId a, TypeId b) noexcept -> bool { return a.flat_value == b.flat_value; };
constexpr auto operator != (TypeId a, TypeId b) noexcept -> bool { return !(a == b); };

auto is_data_type(TypeId id) noexcept -> bool;
auto is_convertible(TypeId from, TypeId to) noexcept -> bool;
auto make_reference(TypeId type) noexcept -> TypeId;
auto make_mutable(TypeId type) noexcept -> TypeId;
auto remove_reference(TypeId type) noexcept -> TypeId;
auto decay(TypeId type) noexcept -> TypeId;
auto common_type(TypeId a, TypeId b) noexcept -> TypeId; // Returns TypeID::none if there is no common type.

struct Program;

struct PooledString
{
	size_t first;
	size_t size;
};

struct Variable
{
	PooledString name;
	TypeId type;
	int offset;
};

struct FunctionName
{
	PooledString name;
	FunctionId id;
};

struct TypeName
{
	PooledString name;
	TypeId id;
};

struct Scope
{
	int stack_frame_size = 0;
	int stack_frame_alignment = 1;
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
	std::vector<TypeName> types;
};
enum struct ScopeType { global, function, block };

struct CurrentScope
{
	Scope * scope;
	ScopeType type;
};
using ScopeStack = std::vector<CurrentScope>;
using ScopeStackView = span<const CurrentScope>;

namespace lookup_result
{
	struct Nothing {};
	struct Variable { TypeId variable_type; int variable_offset; };
	struct GlobalVariable { TypeId variable_type; int variable_offset; };
	struct OverloadSet { std::vector<FunctionId> function_ids; };
	struct Type { TypeId type_id; };
}
auto lookup_name(ScopeStackView scope_stack, std::string_view name, span<char const> string_pool) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet,
		lookup_result::Type
	>;
auto lookup_type_name(ScopeStackView program, std::string_view name, span<char const> string_pool) noexcept -> TypeId;

auto local_variable_offset(ScopeStackView scope_stack) noexcept -> int;
