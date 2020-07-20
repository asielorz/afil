#pragma once

#include "utils/compatibility.hh"
#include "function_id.hh"
#include "utils/utils.hh"
#include <algorithm>
#include <string>
#include <vector>

namespace complete
{

	struct Program;

	struct TypeId
	{
		union
		{
			AFIL_IGNORE_WARNING_NAMELESS_STRUCT()
			struct
			{
				unsigned is_mutable : 1;
				unsigned is_reference : 1;
				unsigned is_function : 1;
				unsigned index : 29;
			};
			AFIL_WARNING_POP()
			unsigned flat_value;
		};

		static constexpr auto with_index(unsigned index) noexcept -> TypeId { return TypeId{false, false, false, index}; }
		static constexpr auto zero_initialized() noexcept -> TypeId { return with_index(0); }

		static TypeId const void_; // TODO: Maybe void can be confusing so think of another name?
		static TypeId const int8;
		static TypeId const int16;
		static TypeId const int32;
		static TypeId const int64;
		static TypeId const uint8;
		static TypeId const uint16;
		static TypeId const uint32;
		static TypeId const uint64;
		static TypeId const float32;
		static TypeId const float64;
		static TypeId const bool_;
		static TypeId const char_;
		static TypeId const byte;
		static TypeId const type;
		static TypeId const null_t;

		static TypeId const none;
		static TypeId const deduce;
	};

	constexpr auto operator == (TypeId a, TypeId b) noexcept -> bool { return a.flat_value == b.flat_value; }
	constexpr auto operator != (TypeId a, TypeId b) noexcept -> bool { return !(a == b); }

	auto is_data_type(TypeId id) noexcept -> bool;
	auto is_convertible(TypeId from, TypeId to, Program const & program) noexcept -> bool;
	auto make_reference(TypeId type, bool is_reference = true) noexcept->TypeId;
	auto make_mutable(TypeId type, bool is_mutable = true) noexcept->TypeId;
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
	constexpr auto operator == (StructTemplateId a, StructTemplateId b) noexcept -> bool { return a.index == b.index; }
	constexpr auto operator != (StructTemplateId a, StructTemplateId b) noexcept -> bool { return !(a == b); }

	struct StructTemplateName
	{
		std::string name;
		StructTemplateId id;
	};

	struct Constant
	{
		std::string name;
		std::vector<char> value;
		TypeId type;
	};

	struct Scope
	{
		int stack_frame_size = 0;
		int stack_frame_alignment = 1;
		std::vector<Variable> variables;
		std::vector<Constant> constants;
		std::vector<FunctionName> functions;
		std::vector<TypeName> types;
		std::vector<FunctionTemplateName> function_templates;
		std::vector<StructTemplateName> struct_templates;
	};

	template <typename T>
	auto add_variable_to_scope(
		std::vector<T> & variables, int & scope_size, int & scope_alignment,
		std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int;

	auto add_variable_to_scope(complete::Scope & scope, std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int;

} // namespace complete

//************************************************************************************************************************

namespace complete
{
	struct Type;
	auto type_size(Program const & program, TypeId id) noexcept -> int;
	auto type_alignment(Program const & program, TypeId id) noexcept -> int;

	template <typename T>
	auto add_variable_to_scope(
		std::vector<T> & variables, int & scope_size, int & scope_alignment,
		std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int
	{
		int const size = type_size(program, type_id);
		int const alignment = type_alignment(program, type_id);

		T var;
		var.name = name;
		var.type = type_id;
		var.offset = scope_offset + align(scope_size, alignment);
		scope_size = var.offset + size;
		scope_alignment = std::max(scope_alignment, alignment);
		variables.push_back(std::move(var));
		return var.offset;
	}

} // namespace complete