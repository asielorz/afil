#include "program.hh"
#include "utils/variant.hh"
#include "utils/unreachable.hh"
#include <cassert>

namespace complete
{

	auto add_type(Program & program, Type new_type) noexcept -> TypeId
	{
		unsigned const type_index = static_cast<unsigned>(program.types.size());
		program.types.push_back(std::move(new_type));
		return TypeId::with_index(type_index);
	}

	auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &
	{
		assert(!id.is_language_reseved);
		return program.types[id.index];
	}

	auto type_size(Program const & program, TypeId id) noexcept -> int
	{
		if (id.is_reference)
			return sizeof(void *);
		else
			return type_with_id(program, id).size;
	}

	auto is_default_constructible(Struct const & type) noexcept -> bool
	{
		return std::all_of(type.member_variables.begin(), type.member_variables.end(),
			[](MemberVariable const & var) { return var.initializer_expression.has_value(); });
	}

	auto is_default_constructible(TypeId type_id, Program const & program) noexcept -> bool
	{
		if (type_id.is_language_reseved)
			return false;

		Type const & type = type_with_id(program, type_id);

		if (Struct const * const struct_data = struct_for_type(program, type))
			return is_default_constructible(*struct_data);
		else if (Type::Array const * array = try_get<Type::Array>(type.extra_data))
			return is_default_constructible(array->value_type, program);
		else
			return false;
	}

	auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> Expression
	{
		(void)(type_id, struct_data);
		mark_as_to_do("synthesize_default_constructor");
	}

	auto synthesize_default_constructor(TypeId type_id, Type::Array array_data, Program const & program) noexcept -> Expression
	{
		(void)(type_id, array_data, program);
		mark_as_to_do("synthesize_default_constructor");
	}

	auto synthesize_default_constructor(TypeId type_id, Program const & program) noexcept -> Expression
	{
		(void)(type_id, program);
		mark_as_to_do("synthesize_default_constructor");
	}

	auto is_struct(Type const & type) noexcept -> bool
	{
		return has_type<Type::Struct>(type.extra_data);
	}

	auto struct_for_type(Program const & program, Type const & type) noexcept -> Struct const *
	{
		if (auto const s = try_get<Type::Struct>(type.extra_data))
			return &program.structs[s->struct_index];
		else
			return nullptr;
	}

	auto struct_for_type(Program const & program, TypeId type) noexcept -> Struct const *
	{
		return struct_for_type(program, type_with_id(program, type));
	}

	auto find_member_variable(Struct const & type, std::string_view member_name) noexcept -> int
	{
		auto const it = std::find_if(type.member_variables.begin(), type.member_variables.end(),
			[member_name](MemberVariable const & var) { return var.name == member_name; });

		if (it == type.member_variables.end())
			return -1;
		else
			return static_cast<int>(it - type.member_variables.begin());
	}

	auto is_pointer(Type const & type) noexcept -> bool
	{
		return has_type<Type::Pointer>(type.extra_data);
	}

	auto pointer_type_for(TypeId pointee_type, Program & program) noexcept -> TypeId
	{
		assert(!pointee_type.is_reference); // A pointer can't point at a reference.

		// If a pointer type for this type has already been created, return that.
		for (Type const & type : program.types)
			if (auto const pointer = try_get<Type::Pointer>(type.extra_data))
				if (pointer->value_type == pointee_type)
					return TypeId::with_index(static_cast<unsigned>(&type - program.types.data()));

		// Otherwise create one.
		Type new_type;
		new_type.size = sizeof(void *);
		new_type.alignment = alignof(void *);
		new_type.extra_data = Type::Pointer{ pointee_type };
		return add_type(program, std::move(new_type));
	}

	auto pointee_type(Type const & pointer_type) noexcept->TypeId
	{
		assert(is_pointer(pointer_type) || is_array_pointer(pointer_type));
		if (is_pointer(pointer_type))
			return try_get<Type::Pointer>(pointer_type.extra_data)->value_type;
		else
			return try_get<Type::ArrayPointer>(pointer_type.extra_data)->value_type;
	}

	auto pointee_type(TypeId pointer_type_id, Program const & program) noexcept -> TypeId
	{
		return pointee_type(type_with_id(program, pointer_type_id));
	}

	auto is_array(Type const & type) noexcept -> bool
	{
		return has_type<Type::Array>(type.extra_data);
	}

	auto array_type_for(TypeId value_type, int size, Program & program) noexcept -> TypeId
	{
		assert(!value_type.is_reference); // An array can't contain references.
		assert(!value_type.is_mutable); // An array can't contain mutable stuff.

		// If an array type for this type has already been created, return that.
		for (Type const & type : program.types)
			if (auto const array = try_get<Type::Array>(type.extra_data))
				if (array->value_type == value_type && array->size == size)
					return TypeId::with_index(static_cast<unsigned>(&type - program.types.data()));

		Type const & value_type_data = type_with_id(program, value_type);

		// Otherwise create one.
		Type new_type;
		new_type.size = value_type_data.size * size;
		new_type.alignment = value_type_data.alignment;
		new_type.extra_data = Type::Array{value_type, size};
		return add_type(program, std::move(new_type));
	}

	auto array_value_type(Type const & array_type) noexcept -> TypeId
	{
		assert(is_array(array_type));
		return try_get<Type::Array>(array_type.extra_data)->value_type;
	}

	auto array_value_type(TypeId array_type_id, Program const & program) noexcept -> TypeId
	{
		return array_value_type(type_with_id(program, array_type_id));
	}

	auto is_array_pointer(Type const & type) noexcept -> bool
	{
		return has_type<Type::ArrayPointer>(type.extra_data);
	}

	auto array_pointer_type_for(TypeId pointee_type, Program & program) noexcept -> TypeId
	{
		assert(!pointee_type.is_reference); // A pointer can't point at a reference.

		// If a pointer type for this type has already been created, return that.
		for (Type const & type : program.types)
			if (auto const pointer = try_get<Type::ArrayPointer>(type.extra_data))
				if (pointer->value_type == pointee_type)
					return TypeId::with_index(static_cast<unsigned>(&type - program.types.data()));

		// Otherwise create one.
		Type new_type;
		new_type.size = sizeof(void *);
		new_type.alignment = alignof(void *);
		new_type.extra_data = Type::ArrayPointer{pointee_type};
		return add_type(program, std::move(new_type));
	}

	auto parameter_types(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>
	{
		if (id.is_extern)
		{
			return program.extern_functions[id.index].parameter_types;
		}
		else
		{
			Function const & fn = program.functions[id.index];
			std::vector<TypeId> types(fn.parameter_count);
			for (int i = 0; i < fn.parameter_count; ++i)
				types[i] = fn.variables[i].type;
			return types;
		}
	}

	auto return_type(Program const & program, FunctionId id) noexcept -> TypeId
	{
		if (id.is_extern)
			return program.extern_functions[id.index].return_type;
		else
			return program.functions[id.index].return_type;
	}

	auto insert_conversion_node(Expression tree, TypeId to, Program const & program) noexcept -> Expression
	{
		return insert_conversion_node(std::move(tree), expression_type_id(tree, program), to, program);
	}

} // namespace complete
