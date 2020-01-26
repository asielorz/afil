#pragma once

#include "complete_statement.hh"
#include "complete_scope.hh"
#include "complete_expression.hh"
#include "function_id.hh"
#include "utils/callc.hh"
#include <optional>

namespace complete
{

	struct Type
	{
		struct BuiltIn {};
		struct Pointer
		{
			TypeId value_type;
		};
		struct Array
		{
			TypeId value_type;
			int size;
		};
		struct ArrayPointer
		{
			TypeId value_type;
		};
		struct Struct
		{
			int struct_index;
		};

		int size;
		int alignment;
		std::variant<BuiltIn, Pointer, Array, ArrayPointer, Struct> extra_data;
	};

	struct Function : Scope
	{
		int parameter_count = 0;     // From the variables, how many are arguments. The rest are locals.
		int parameter_size = 0;	     // Size in bytes needed for parameters.
		TypeId return_type;
		std::vector<Statement> statements;
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
		std::optional<Expression> initializer_expression;
	};
	struct Struct
	{
		std::vector<MemberVariable> member_variables;
	};

	struct Program
	{
		std::vector<Type> types;
		std::vector<Struct> structs;
		//std::vector<StructTemplate> struct_templates;
		std::vector<Function> functions;
		std::vector<ExternFunction> extern_functions;
		//std::vector<FunctionTemplate> function_templates;
		std::vector<Statement> global_initialization_statements;
		Scope global_scope;
		FunctionId main_function = invalid_function_id;
	};


	auto add_type(Program & program, Type new_type) noexcept -> TypeId;
	auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
	auto type_size(Program const & program, TypeId id) noexcept -> int;
	auto is_default_constructible(Struct const & type) noexcept -> bool;
	auto is_default_constructible(TypeId type, Program const & program) noexcept -> bool;
	auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> Expression;
	auto synthesize_default_constructor(TypeId type_id, Type::Array array_data, Program const & program) noexcept -> Expression;
	auto synthesize_default_constructor(TypeId type_id, Program const & program) noexcept -> Expression;

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
	auto array_value_type(Type const & array_type) noexcept -> TypeId;
	auto array_value_type(TypeId array_type_id, Program const & program) noexcept -> TypeId;

	auto is_array_pointer(Type const & type) noexcept -> bool;
	auto array_pointer_type_for(TypeId value_type, Program & program) noexcept -> TypeId;

	auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>;
	auto return_type(Program const & program, FunctionId id) noexcept -> TypeId;

	auto insert_conversion_node(Expression tree, TypeId from, TypeId to, Program const & program) noexcept -> Expression;

} // namespace complete

