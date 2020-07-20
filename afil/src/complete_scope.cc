#include "complete_scope.hh"
#include "program.hh"

namespace complete
{

	TypeId const TypeId::void_ = {false, false, false, 0};
	TypeId const TypeId::int8 = {false, false, false, 1 };
	TypeId const TypeId::int16 = {false, false, false, 2 };
	TypeId const TypeId::int32 = {false, false, false, 3 };
	TypeId const TypeId::int64 = {false, false, false, 4 };
	TypeId const TypeId::uint8 = {false, false, false, 5 };
	TypeId const TypeId::uint16 = {false, false, false, 6 };
	TypeId const TypeId::uint32 = {false, false, false, 7 };
	TypeId const TypeId::uint64 = {false, false, false, 8 };
	TypeId const TypeId::float32 = {false, false, false, 9 };
	TypeId const TypeId::float64 = {false, false, false, 10 };
	TypeId const TypeId::bool_ = {false, false, false, 11};
	TypeId const TypeId::char_ = TypeId::uint8;
	TypeId const TypeId::byte = TypeId::uint8;
	TypeId const TypeId::type = {false, false, false, 12};
	TypeId const TypeId::null_t = {false, false, false, 13};

	TypeId const TypeId::none = {false, false, false, (1 << 29) - 1};
	TypeId const TypeId::deduce = {false, false, false, (1 << 29) - 1 };

	auto is_data_type(TypeId id) noexcept -> bool
	{
		return id.index != TypeId::void_.index && id.index != TypeId::none.index && id.index != TypeId::deduce.index;
	}

	auto is_convertible(TypeId from, TypeId to, Program const & program) noexcept -> bool
	{
		// A type is convertible to itself (for now).
		if (from == to)
			return true;

		// Conversions between same types but different mutable/reference qualifiers.
		if (from.index == to.index)
		{
			// Both T and any reference are convertible to T.
			if (!to.is_reference)
				return true;

			// Both T and any reference are convertible to const &
			if (!to.is_mutable)
				return true;

			// Mutable reference is convertible to everything.
			if (from.is_mutable && from.is_reference)
				return true;
		}
		// Conversions between different types.
		else
		{
			// Function types are not convertible to anything.
			if (from.is_function || to.is_function)
				return false;

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
		}

		// Otherwise there is no conversion.
		return false;
	}

	auto make_reference(TypeId type, bool is_reference) noexcept -> TypeId
	{
		type.is_reference = is_reference;
		return type;
	}

	auto make_mutable(TypeId type, bool is_mutable) noexcept -> TypeId
	{
		type.is_mutable = is_mutable;
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
		dst.is_function = src.is_function;
		dst.index = src.index;
	}

	auto add_variable_to_scope(complete::Scope & scope, std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int
	{
		return add_variable_to_scope(scope.variables, scope.stack_frame_size, scope.stack_frame_alignment, name, type_id, scope_offset, program);
	}

} // namespace complete
