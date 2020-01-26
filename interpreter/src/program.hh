#pragma once

#include "scope.hh"
#include "statement.hh"
#include "function_id.hh"
#include "utils/callc.hh"

namespace incomplete
{

	struct Program
	{
		//Scope global_scope;
		std::vector<Statement> global_initialization_statements;
	};

} // namespace incomplete

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

} // namespace complete

