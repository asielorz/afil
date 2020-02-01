#pragma once

#include "function_id.hh"
#include <vector>
#include <string>

namespace complete
{

	struct Program;

	struct TypeId
	{
		#pragma warning (disable : 4201)
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

		static constexpr auto with_index(unsigned index) noexcept -> TypeId { return TypeId{ false, false, false, index }; }
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
	auto make_reference(TypeId type) noexcept->TypeId;
	auto make_mutable(TypeId type) noexcept->TypeId;
	auto remove_reference(TypeId type) noexcept->TypeId;
	auto decay(TypeId type) noexcept->TypeId;
	auto common_type(TypeId a, TypeId b, Program const & program) noexcept->TypeId; // Returns TypeID::none if there is no common type.
	auto assign_without_qualifiers(TypeId & dst, TypeId src) noexcept -> void;

	struct Variable
	{
		std::string name;
		TypeId type;
		int offset;
	};

	struct FunctionName
	{
		std::string name;
		FunctionId id;
	};

	struct TypeName
	{
		std::string name;
		TypeId id;
	};

	struct FunctionTemplateName
	{
		std::string name;
		FunctionTemplateId id;
	};

	struct StructTemplateId { unsigned index; };
	struct StructTemplateName
	{
		std::string name;
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

} // namespace complete
