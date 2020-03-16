#pragma once

#include "complete_module.hh"
#include "complete_statement.hh"
#include "incomplete_statement.hh"
#include "complete_scope.hh"
#include "complete_expression.hh"
#include "function_id.hh"
#include "syntax_error.hh"
#include "utils/callc.hh"
#include <optional>
#include <map>

namespace complete
{

	struct Program
	{
		Program();
		Program(Program const &) = delete;
		Program(Program &&) = default;
		Program & operator = (Program const &) = delete;
		Program & operator = (Program &&) = default;

		std::vector<Type> types;
		std::vector<Struct> structs;
		std::vector<StructTemplate> struct_templates;
		std::vector<OverloadSet> overload_set_types;
		std::vector<Function> functions;
		std::vector<ExternFunction> extern_functions;
		std::vector<FunctionTemplate> function_templates;
		std::vector<Statement> global_initialization_statements;
		Scope global_scope;
		FunctionId main_function = invalid_function_id;
	};

	auto add_type(Program & program, Type new_type) noexcept -> TypeId;
	auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
	auto type_size(Program const & program, TypeId id) noexcept -> int;
	auto type_alignment(Program const & program, TypeId id) noexcept -> int;
	auto is_default_constructible(Struct const & type) noexcept -> bool;
	auto is_default_constructible(TypeId type, Program const & program) noexcept -> bool;
	auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> expression::Constructor;
	auto synthesize_default_constructor(TypeId type_id, Type::Array array_data, Program const & program) noexcept -> expression::Constructor;
	auto synthesize_default_constructor(TypeId type_id, Program const & program) noexcept -> expression::Constructor;

	auto add_struct_template(Program & program, StructTemplate new_template) noexcept -> StructTemplateId;
	auto is_struct(Type const & type) noexcept -> bool;
	auto struct_for_type(Program const & program, Type const & type) noexcept -> Struct const *;
	auto struct_for_type(Program const & program, TypeId type) noexcept -> Struct const *;
	auto find_member_variable(Struct const & type, std::string_view member_name) noexcept -> int;

	auto is_pointer(Type const & type) noexcept -> bool;
	auto pointer_type_for(TypeId pointee_type, Program & program) noexcept -> TypeId;
	auto pointee_type(Type const & pointer_type) noexcept -> TypeId;
	auto pointee_type(TypeId pointer_type_id, Program const & program) noexcept -> TypeId;

	auto is_array(Type const & type) noexcept -> bool;
	auto array_type_for(TypeId pointee_type, int size, Program & program) noexcept -> TypeId;
	auto array_size(Type const & array_type) noexcept -> int;
	auto array_value_type(Type const & array_type) noexcept -> TypeId;
	auto array_value_type(TypeId array_type_id, Program const & program) noexcept -> TypeId;

	auto is_array_pointer(Type const & type) noexcept -> bool;
	auto array_pointer_type_for(TypeId value_type, Program & program) noexcept -> TypeId;

	auto add_function(Program & program, Function new_function) noexcept -> FunctionId;
	auto add_function_template(Program & program, FunctionTemplate new_function_template) noexcept -> FunctionTemplateId;
	auto parameter_types_of(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>;
	auto return_type(Program const & program, FunctionId id) noexcept -> TypeId;
	auto is_callable_at_compile_time(Program const & program, FunctionId id) noexcept -> bool;

	auto instantiate_function_template(Program & program, FunctionTemplateId template_id, span<TypeId const> parameters) noexcept -> expected<FunctionId, PartialSyntaxError>;
	auto instantiate_struct_template(Program & program, StructTemplateId template_id, span<TypeId const> parameters) noexcept -> expected<TypeId, PartialSyntaxError>;

	auto insert_conversion_node(Expression tree, TypeId from, TypeId to, Program const & program) noexcept -> expected<Expression, PartialSyntaxError>;
	auto insert_conversion_node(Expression tree, TypeId to, Program const & program) noexcept -> expected<Expression, PartialSyntaxError>;

	auto resolve_function_overloading(OverloadSetView overload_set, span<TypeId const> parameters, Program & program) noexcept -> FunctionId;
	auto resolve_function_overloading_and_insert_conversions(OverloadSetView overload_set, span<Expression> parameters, span<TypeId const> parameter_types, Program & program) noexcept
		-> expected<FunctionId, PartialSyntaxError>;

	[[nodiscard]] auto insert_conversions(
		span<Expression> parameters,
		span<TypeId const> parsed_parameter_types,
		span<TypeId const> target_parameter_types,
		Program const & program) noexcept -> expected<void, PartialSyntaxError>;

	auto type_for_overload_set(Program & program, OverloadSet overload_set) noexcept -> TypeId;
	auto overload_set_for_type(Program const & program, TypeId overload_set_type) noexcept -> OverloadSetView;

} // namespace complete

