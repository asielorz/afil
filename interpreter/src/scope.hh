#pragma once

#include "function_id.hh"
#include "span.hh"
#include "utils.hh"
#include "value_ptr.hh"
#include <string_view>
#include <variant>

struct Program;

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
	static constexpr auto zero_initialized() noexcept -> TypeId { return with_index(0); }

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
auto is_convertible(TypeId from, TypeId to, Program const & program) noexcept -> bool;
auto make_reference(TypeId type) noexcept -> TypeId;
auto make_mutable(TypeId type) noexcept -> TypeId;
auto remove_reference(TypeId type) noexcept -> TypeId;
auto decay(TypeId type) noexcept -> TypeId;
auto common_type(TypeId a, TypeId b, Program const & program) noexcept -> TypeId; // Returns TypeID::none if there is no common type.

struct Program;

struct FunctionTemplateId { unsigned index; };
struct StructTemplateId { unsigned index; };

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

struct FunctionTemplateName
{
	PooledString name;
	FunctionTemplateId id;
};

struct StructTemplateName
{
	PooledString name;
	StructTemplateId id;
};

struct Scope
{
	int stack_frame_size = 0;
	int stack_frame_alignment = 1;
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
	std::vector<TypeName> types;
	std::vector<FunctionTemplateName> function_templates;
	std::vector<StructTemplateName> struct_templates;
};
enum struct ScopeType { global, function, block, dependent_function, dependent_block };

struct DependentTypeId
{
	union BaseCase
	{
		struct
		{
			unsigned is_language_reserved : 1;
			unsigned is_dependent : 1;
			unsigned index : 30;
		};
		unsigned flat_value;
	};
	struct Pointer
	{
		value_ptr<DependentTypeId> pointee;
	};
	struct Array
	{
		value_ptr<DependentTypeId> pointee;
		int size;
	};
	struct Template
	{
		StructTemplateId template_id;
		std::vector<DependentTypeId> parameters;
	};

	std::variant<BaseCase, Pointer, Array> value;
	bool is_mutable : 1;
	bool is_reference : 1;

	static auto with_index(unsigned index) noexcept -> DependentTypeId;
	static DependentTypeId const unknown;
};

struct DependentVariable
{
	PooledString name;
	DependentTypeId type;
};
struct DependentType
{
	PooledString name;
};
struct DependentScope : Scope
{
	std::vector<DependentVariable> dependent_variables;
	std::vector<DependentType> dependent_types;
};

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
	struct OverloadSet { std::vector<FunctionId> function_ids; std::vector<FunctionTemplateId> function_template_ids; };
	struct Type { TypeId type_id; };
	struct StructTemplate { StructTemplateId template_id; };
	struct DependentType { PooledString name; int index; };
	struct DependentVariable { PooledString name; DependentTypeId type; };
}
auto lookup_name(ScopeStackView scope_stack, std::string_view name, span<char const> string_pool) noexcept
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet,
		lookup_result::Type,
		lookup_result::StructTemplate,
		lookup_result::DependentVariable,
		lookup_result::DependentType
	>;

auto local_variable_offset(ScopeStackView scope_stack) noexcept -> int;

template <typename T>
auto add_variable_to_scope(
	std::vector<T> & variables, int & scope_size, int & scope_alignment,
	PooledString name, TypeId type_id, int scope_offset, Program const & program) -> int
{
	Type const & type = type_with_id(program, type_id);
	int const size = type_id.is_reference ? sizeof(void *) : type.size;
	int const alignment = type_id.is_reference ? alignof(void *) : type.alignment;

	T var;
	var.name = name;
	var.type = type_id;
	var.offset = scope_offset + align(scope_size, alignment);
	scope_size = var.offset + size;
	scope_alignment = std::max(scope_alignment, alignment);
	variables.push_back(std::move(var));
	return var.offset;
}

auto add_variable_to_scope(Scope & scope, PooledString name, TypeId type_id, int scope_offset, Program const & program) -> int;
