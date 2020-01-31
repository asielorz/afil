#pragma once

#include "utils/value_ptr.hh"
#include <vector>
#include <variant>
#include <optional>

namespace incomplete
{

	struct TypeId
	{
		#pragma warning (disable : 4201)
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
			value_ptr<TypeId> pointee;
		};
		struct Array
		{
			value_ptr<TypeId> value_type;
			int size;
		};
		struct ArrayPointer
		{
			value_ptr<TypeId> pointee;
		};
		struct TemplateInstantiation
		{
			int template_index;
			std::vector<TypeId> parameters;
		};

		std::variant<BaseCase, Pointer, Array, ArrayPointer, TemplateInstantiation> value;
		bool is_mutable : 1;
		bool is_reference : 1;

		static auto with_index(unsigned index) noexcept -> TypeId;
		static TypeId const unknown;
	};

	struct Variable
	{
		std::string name;
		std::optional<TypeId> type;
	};

	struct Function;
	struct FunctionTemplate;
	struct Struct;
	struct StructTemplate;

	struct TemplateParameter
	{
		std::string name;
	};

	struct Statement;
	struct FunctionParameter
	{
		std::string name;
		TypeId type;
	};

	struct Function
	{
		std::vector<FunctionParameter> parameters;
		std::vector<Statement> statements;
		std::optional<TypeId> return_type;
	};
	struct FunctionTemplate : Function 
	{
		std::vector<TemplateParameter> template_parameters;
	};

} // namespace incomplete
