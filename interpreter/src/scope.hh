#pragma once

#include "function_id.hh"
#include "utils/value_ptr.hh"
#include "utils/span.hh"
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
	struct Function
	{
		//Scope scope;
		std::vector<Statement> statements;
		int parameter_count;
		std::optional<TypeId> return_type;
	};
	struct FunctionTemplate : Function 
	{
		std::vector<TemplateParameter> template_parameters;
	};

} // namespace incomplete

namespace complete
{

	struct TypeId
	{
		union
		{
			struct
			{
				unsigned is_language_reseved : 1;
				unsigned is_mutable : 1;
				unsigned is_reference : 1;
				unsigned index : 29;
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

	struct Variable
	{
		std::string name;
		TypeId type;
		int offset;
	};

	struct Scope
	{

	};

} // namespace complete
