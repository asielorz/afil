#pragma once

#include "expression.hh"
#include "scope.hh"
#include "span.hh"
#include "callc.hh"
#include "lexer.hh"
#include <variant>
#include <optional>
#include <map>

namespace stmt { struct Statement; }

struct BuiltInType{};
struct PointerType
{
	TypeId value_type;
};
struct ArrayType
{
	TypeId value_type;
	int size;
};
struct StructType
{
	int struct_index;
};

struct Type
{
	int size;
	int alignment;
	std::variant<BuiltInType, PointerType, ArrayType, StructType> extra_data;
};

struct Function : Scope
{
	int parameter_count = 0;     // From the variables, how many are arguments. The rest are locals.
	int parameter_size = 0;	     // Size in bytes needed for parameters.
	TypeId return_type;
	std::vector<stmt::Statement> statements;
};

struct ExternFunction
{
	int parameter_size;
	int parameter_alignment;
	TypeId return_type;
	std::vector<TypeId> parameter_types;
	callc::CFunctionCaller caller;
	void const * function_pointer;
};

struct MemberVariable : Variable
{
	std::optional<expr::ExpressionTree> initializer_expression;
};

struct Struct
{
	std::vector<MemberVariable> member_variables;
};

struct MemcmpRanges
{
	using is_transparent = std::true_type;

	template <typename T, typename U>
	[[nodiscard]] constexpr auto operator () (T const & a, U const & b) const noexcept -> bool
	{
		return memcmp(a.data(), b.data(), a.size() * sizeof(*a.data())) < 0;
	}
};

struct FunctionTemplate
{
	struct Parameter
	{
		unsigned is_dependent : 1;
		unsigned index : 31;
	};

	DependentScope scope;
	int template_parameter_count;
	std::vector<Parameter> parameters;
	std::vector<stmt::Statement> statement_templates;
	std::map<std::vector<TypeId>, FunctionId, MemcmpRanges> cached_instantiations;
};

struct MemberVariableTemplate
{
	PooledString name;
	bool is_dependent;
	TypeId type;
	std::optional<expr::ExpressionTree> initializer_expression;
};

struct StructTemplate
{
	std::vector<DependentType> template_parameters;
	std::vector<MemberVariableTemplate> member_variables;
	std::map<std::vector<TypeId>, TypeId, MemcmpRanges> cached_instantiations;
};

struct Program
{
	Program();

	std::vector<Type> types;
	std::vector<Struct> structs;
	std::vector<StructTemplate> struct_templates;
	std::vector<Function> functions;
	std::vector<FunctionTemplate> function_templates;
	std::vector<ExternFunction> extern_functions;
	std::vector<stmt::Statement> global_initialization_statements;
	std::vector<char> string_pool;
	Scope global_scope;
	FunctionId main_function = invalid_function_id;
};

// Returns id of function found or invalid_function_id on failure.
struct OverloadSet
{
	constexpr OverloadSet() noexcept = default;
	OverloadSet(lookup_result::OverloadSet const & lookup_overload_set) noexcept
		: function_ids(lookup_overload_set.function_ids)
		, function_template_ids(lookup_overload_set.function_template_ids)
	{}

	span<FunctionId const> function_ids;
	span<FunctionTemplateId const> function_template_ids;
};
auto resolve_function_overloading(OverloadSet overload_set, span<TypeId const> parameters, Program & program) noexcept -> FunctionId;
auto insert_conversion_node(expr::ExpressionTree tree, TypeId from, TypeId to, Program const & program) noexcept->expr::ExpressionTree;
auto insert_conversion_to_control_flow_condition(expr::ExpressionTree tree, Program const & program) noexcept->expr::ExpressionTree;
auto insert_conversions(
	span<expr::ExpressionTree> parameters, 
	span<TypeId const> parsed_parameter_types, 
	span<TypeId const> target_parameter_types, 
	Program const & program) noexcept -> void;
auto resolve_function_overloading_and_insert_conversions(
	OverloadSet overload_set,
	span<expr::ExpressionTree> parameters,
	span<TypeId const> parsed_parameter_types,
	Program & program) noexcept -> FunctionId;

auto add_type(Program & program, Type new_type) noexcept -> TypeId;
auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
auto type_size(Program const & program, TypeId id) noexcept -> int;
auto is_default_constructible(Struct const & type) noexcept -> bool;
auto is_default_constructible(TypeId type, Program const & program) noexcept -> bool;
auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> expr::StructConstructorNode;
auto synthesize_default_constructor(TypeId type_id, ArrayType array_data, Program const & program) noexcept -> expr::ArrayConstructorNode;
auto synthesize_default_constructor(TypeId type_id, Program const & program) noexcept -> expr::ExpressionTree;

auto is_struct(Type const & type) noexcept -> bool;
auto struct_for_type(Program const & program, Type const & type) noexcept -> Struct const *;
auto struct_for_type(Program const & program, TypeId type) noexcept -> Struct const *;
auto find_member_variable(Struct const & type, std::string_view member_name, span<char const> string_pool) noexcept -> int;

auto is_pointer(Type const & type) noexcept -> bool;
auto pointer_type_for(TypeId pointee_type, Program & program) noexcept -> TypeId;
auto pointee_type(Type const & pointer_type) noexcept -> TypeId;
auto pointee_type(TypeId pointer_type_id, Program const & program) noexcept -> TypeId;

auto is_array(Type const & type) noexcept -> bool;
auto array_type_for(TypeId value_type, int size, Program & program) noexcept -> TypeId;

auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>; // Stack allocator?
auto return_type(Program const & program, FunctionId id) noexcept -> TypeId;

auto instantiate_function_template(Program & program, FunctionTemplateId template_id, span<TypeId const> parameters) noexcept -> FunctionId;
auto instantiate_struct_template(Program & program, StructTemplateId template_id, span<TypeId const> parameters) noexcept -> TypeId;

auto pool_string(Program & program, std::string_view string) noexcept -> PooledString;
auto get(Program const & program, PooledString string) noexcept -> std::string_view;
auto get(span<char const> pool, PooledString string) noexcept -> std::string_view;

struct pooled_name_equal
{
	constexpr pooled_name_equal(span<char const> pool_, std::string_view name_) noexcept : pool(pool_), name(name_) {}

	template <typename T>
	constexpr auto operator () (T const & t) const noexcept
	{
		return get(pool, t.name) == name;
	}

	span<char const> pool;
	std::string_view name;
};
