#pragma once

#include "utils/value_ptr.hh"
#include <vector>
#include <variant>
#include <optional>
#include <string>

namespace incomplete
{

	struct Expression;

	struct TypeId
	{
		#pragma warning (disable : 4201)
		struct BaseCase
		{
			std::string name;
		};
		struct Pointer
		{
			value_ptr<TypeId> pointee;
		};
		struct Array
		{
			value_ptr<TypeId> value_type;
			value_ptr<Expression> size;
		};
		struct ArrayPointer
		{
			value_ptr<TypeId> pointee;
		};
		struct TemplateInstantiation
		{
			std::string template_name;
			std::vector<TypeId> parameters;
		};
		struct Deduce{};

		std::variant<BaseCase, Pointer, Array, ArrayPointer, TemplateInstantiation, Deduce> value;
		bool is_mutable : 1;
		bool is_reference : 1;

		static TypeId const unknown;
	};

	struct Variable
	{
		std::string_view name;
		std::optional<TypeId> type;
	};

	struct Function;
	struct FunctionTemplate;
	struct Struct;
	struct StructTemplate;

	struct TemplateParameter
	{
		std::string_view name;
	};

	struct Statement;
	struct FunctionParameter
	{
		std::string_view name;
		TypeId type;
	};

	struct FunctionPrototype
	{
		std::vector<FunctionParameter> parameters;
		std::optional<TypeId> return_type;
	};
	struct Function : FunctionPrototype
	{
		std::vector<Statement> statements;
	};
	struct FunctionTemplate : Function 
	{
		std::vector<TemplateParameter> template_parameters;
	};

	struct ExternFunction
	{
		std::string_view name;
		incomplete::FunctionPrototype prototype;
		void const * function_pointer;
	};

} // namespace incomplete
