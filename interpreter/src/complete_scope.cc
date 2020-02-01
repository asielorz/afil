#pragma once

#include "complete_scope.hh"
#include "program.hh"

namespace complete
{

	TypeId const TypeId::void_ = { false, false, false, 0 };
	TypeId const TypeId::int_ = { false, false, false, 1 };
	TypeId const TypeId::float_ = { false, false, false, 2 };
	TypeId const TypeId::bool_ = { false, false, false, 3 };

	TypeId const TypeId::none = { true, false, false, 0 };
	TypeId const TypeId::function = { true, false, false, 1 };
	TypeId const TypeId::deduce = { true, false, false, 2 };

	auto is_data_type(TypeId id) noexcept -> bool
	{
		return !id.is_language_reseved && id.index != TypeId::void_.index;
	}

	auto is_convertible(TypeId from, TypeId to, Program const & program) noexcept -> bool
	{
		// A type is convertible to itself (for now).
		if (from == to)
			return true;

		if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) &&
			!from.is_reference && !to.is_reference)
		{
			TypeId const from_pointee = pointee_type(from, program);
			TypeId const to_pointee = pointee_type(to, program);
			if (from_pointee.index == to_pointee.index)
			{
				// If converting from a mutable pointer or to an immutable pointer of the same type, conversion is allowed.
				if (!to_pointee.is_mutable && from_pointee.is_mutable)
					return true;
			}
		}

		// No conversions between different types (for now).
		if (from.index != to.index)
			return false;

		// Both T and any reference are convertible to T.
		if (!to.is_reference)
			return true;

		// Both T and any reference are convertible to const &
		if (!to.is_mutable)
			return true;

		// Mutable reference is convertible to everything.
		if (from.is_mutable && from.is_reference)
			return true;

		// Otherwise there is no conversion.
		return false;
	}

	auto make_reference(TypeId type) noexcept -> TypeId
	{
		type.is_reference = true;
		return type;
	}

	auto make_mutable(TypeId type) noexcept -> TypeId
	{
		type.is_mutable = true;
		return type;
	}

	auto remove_reference(TypeId type) noexcept -> TypeId
	{
		type.is_reference = false;
		return type;
	}

	auto decay(TypeId type) noexcept -> TypeId
	{
		type.is_mutable = false;
		type.is_reference = false;
		return type;
	}

	auto common_type(TypeId a, TypeId b, Program const & program) noexcept -> TypeId
	{
		if (a == b)
			return a;

		if (is_convertible(a, b, program))
			return b;

		if (is_convertible(b, a, program))
			return a;

		return TypeId::none;
	}

	auto assign_without_qualifiers(TypeId & dst, TypeId src) noexcept -> void
	{
		dst.is_language_reseved = src.is_language_reseved;
		dst.index = src.index;
	}

	auto add_variable_to_scope(complete::Scope & scope, std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int
	{
		return add_variable_to_scope(scope.variables, scope.stack_frame_size, scope.stack_frame_alignment, name, type_id, scope_offset, program);
	}

} // namespace complete
