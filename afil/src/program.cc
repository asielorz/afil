#include "program.hh"
#include "interpreter.hh"
#include "syntax_error.hh"
#include "template_instantiation.hh"
#include "utils/algorithm.hh"
#include "utils/callc.hh"
#include "utils/function_ptr.hh"
#include "utils/overload.hh"
#include "utils/string.hh"
#include "utils/unreachable.hh"
#include "utils/variant.hh"
#include "utils/warning_macro.hh"
#include <cassert>

using namespace std::literals;

namespace complete
{

	auto built_in_types() noexcept -> std::vector<std::pair<std::string_view, Type>>
	{
		return {
			{"void",		{0, 1}},
			{"int8",		{1, 1}},
			{"int16",		{2, 2}},
			{"int32",		{4, 4}},
			{"int64",		{8, 8}},
			{"uint8",		{1, 1}},
			{"uint16",		{2, 2}},
			{"uint32",		{4, 4}},
			{"uint64",		{8, 8}},
			{"float32",		{4, 4}},
			{"float64",		{8, 8}},
			{"bool",		{1, 1}},
			{"type",		{4, 4}},
			{"null_t",		{0, 1}},
		};
	}

	template <typename T> struct index_for_type {};
	template <> struct index_for_type<void> { static constexpr unsigned value = 0; };
	template <> struct index_for_type<int8_t> { static constexpr unsigned value = 1; };
	template <> struct index_for_type<int16_t> { static constexpr unsigned value = 2; };
	template <> struct index_for_type<int32_t> { static constexpr unsigned value = 3; };
	template <> struct index_for_type<int64_t> { static constexpr unsigned value = 4; };
	template <> struct index_for_type<uint8_t> { static constexpr unsigned value = 5; };
	template <> struct index_for_type<uint16_t> { static constexpr unsigned value = 6; };
	template <> struct index_for_type<uint32_t> { static constexpr unsigned value = 7; };
	template <> struct index_for_type<uint64_t> { static constexpr unsigned value = 8; };
	template <> struct index_for_type<float> { static constexpr unsigned value = 9; };
	template <> struct index_for_type<double> { static constexpr unsigned value = 10; };
	template <> struct index_for_type<bool> { static constexpr unsigned value = 11; };
	template <> struct index_for_type<complete::TypeId> { static constexpr unsigned value = 12; };
	template <typename T> constexpr unsigned index_for_type_v = index_for_type<T>::value;

	using mpl::BoxedType;
	using mpl::box;

	template <typename T>
	constexpr auto id_for(BoxedType<T>) noexcept -> TypeId
	{
		return TypeId::with_index(index_for_type_v<T>);
	}
	template <typename T>
	constexpr auto id_for(BoxedType<T &>) noexcept -> TypeId
	{
		return make_mutable(make_reference(TypeId::with_index(index_for_type_v<T>)));
	}
	template <typename T>
	constexpr auto id_for(BoxedType<T const &>) noexcept -> TypeId
	{
		return make_reference(TypeId::with_index(index_for_type_v<T>));
	}

	template <typename R, typename ... Args>
	auto intrinsic_function_descriptor_helper(std::string_view name, auto (*)(Args...) -> R) noexcept -> IntrinsicFunction
	{
		return IntrinsicFunction{
			id_for(box<R>),
			{id_for(box<Args>)...},
			name
		};
	}

	template <typename F>
	auto intrinsic_function_descriptor(std::string_view name) noexcept -> IntrinsicFunction
	{
		return intrinsic_function_descriptor_helper(name, static_cast<F *>(nullptr));
	}

	const IntrinsicFunction intrinsic_functions[] = {
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("+"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("-"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("*"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("/"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("%"sv),
		intrinsic_function_descriptor<bool(int8_t, int8_t)>("=="sv),
		intrinsic_function_descriptor<order_t(int8_t, int8_t)>("<=>"sv),
		intrinsic_function_descriptor<int8_t(int8_t)>("-"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("&"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("|"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("^"sv),
		intrinsic_function_descriptor<int8_t(int8_t)>("~"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>(">>"sv),
		intrinsic_function_descriptor<int8_t(int8_t, int8_t)>("<<"sv),

		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("+"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("-"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("*"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("/"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("%"sv),
		intrinsic_function_descriptor<bool(int16_t, int16_t)>("=="sv),
		intrinsic_function_descriptor<order_t(int16_t, int16_t)>("<=>"sv),
		intrinsic_function_descriptor<int16_t(int16_t)>("-"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("&"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("|"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("^"sv),
		intrinsic_function_descriptor<int16_t(int16_t)>("~"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>(">>"sv),
		intrinsic_function_descriptor<int16_t(int16_t, int16_t)>("<<"sv),

		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("+"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("-"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("*"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("/"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("%"sv),
		intrinsic_function_descriptor<bool(int32_t, int32_t)>("=="sv),
		intrinsic_function_descriptor<order_t(int32_t, int32_t)>("<=>"sv),
		intrinsic_function_descriptor<int32_t(int32_t)>("-"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("&"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("|"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("^"sv),
		intrinsic_function_descriptor<int32_t(int32_t)>("~"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>(">>"sv),
		intrinsic_function_descriptor<int32_t(int32_t, int32_t)>("<<"sv),

		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("+"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("-"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("*"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("/"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("%"sv),
		intrinsic_function_descriptor<bool(int64_t, int64_t)>("=="sv),
		intrinsic_function_descriptor<order_t(int64_t, int64_t)>("<=>"sv),
		intrinsic_function_descriptor<int64_t(int64_t)>("-"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("&"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("|"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("^"sv),
		intrinsic_function_descriptor<int64_t(int64_t)>("~"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>(">>"sv),
		intrinsic_function_descriptor<int64_t(int64_t, int64_t)>("<<"sv),

		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("+"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("-"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("*"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("/"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("%"sv),
		intrinsic_function_descriptor<bool(uint8_t, uint8_t)>("=="sv),
		intrinsic_function_descriptor<order_t(uint8_t, uint8_t)>("<=>"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("&"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("|"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("^"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t)>("~"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>(">>"sv),
		intrinsic_function_descriptor<uint8_t(uint8_t, uint8_t)>("<<"sv),

		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("+"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("-"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("*"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("/"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("%"sv),
		intrinsic_function_descriptor<bool(uint16_t, uint16_t)>("=="sv),
		intrinsic_function_descriptor<order_t(uint16_t, uint16_t)>("<=>"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("&"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("|"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("^"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t)>("~"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>(">>"sv),
		intrinsic_function_descriptor<uint16_t(uint16_t, uint16_t)>("<<"sv),

		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("+"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("-"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("*"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("/"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("%"sv),
		intrinsic_function_descriptor<bool(uint32_t, uint32_t)>("=="sv),
		intrinsic_function_descriptor<order_t(uint32_t, uint32_t)>("<=>"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("&"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("|"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("^"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t)>("~"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>(">>"sv),
		intrinsic_function_descriptor<uint32_t(uint32_t, uint32_t)>("<<"sv),

		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("+"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("-"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("*"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("/"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("%"sv),
		intrinsic_function_descriptor<bool(uint64_t, uint64_t)>("=="sv),
		intrinsic_function_descriptor<order_t(uint64_t, uint64_t)>("<=>"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("&"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("|"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("^"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t)>("~"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>(">>"sv),
		intrinsic_function_descriptor<uint64_t(uint64_t, uint64_t)>("<<"sv),
									  
		intrinsic_function_descriptor<float(float, float)>("+"sv),
		intrinsic_function_descriptor<float(float, float)>("-"sv),
		intrinsic_function_descriptor<float(float, float)>("*"sv),
		intrinsic_function_descriptor<float(float, float)>("/"sv),
		intrinsic_function_descriptor<bool(float, float)>("=="sv),
		intrinsic_function_descriptor<order_t(float, float)>("<=>"sv),
		intrinsic_function_descriptor<float(float)>("-"sv),

		intrinsic_function_descriptor<double(double, double)>("+"sv),
		intrinsic_function_descriptor<double(double, double)>("-"sv),
		intrinsic_function_descriptor<double(double, double)>("*"sv),
		intrinsic_function_descriptor<double(double, double)>("/"sv),
		intrinsic_function_descriptor<bool(double, double)>("=="sv),
		intrinsic_function_descriptor<order_t(double, double)>("<=>"sv),
		intrinsic_function_descriptor<double(double)>("-"sv),

		intrinsic_function_descriptor<bool(bool, bool)>("and"sv),
		intrinsic_function_descriptor<bool(bool, bool)>("or"sv),
		intrinsic_function_descriptor<bool(bool, bool)>("xor"sv),
		intrinsic_function_descriptor<bool(bool)>("not"sv),
		intrinsic_function_descriptor<bool(bool, bool)>("=="sv),

		intrinsic_function_descriptor<int8_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(float)>("conversion"sv),
		intrinsic_function_descriptor<int8_t(double)>("conversion"sv),

		intrinsic_function_descriptor<int16_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(float)>("conversion"sv),
		intrinsic_function_descriptor<int16_t(double)>("conversion"sv),

		intrinsic_function_descriptor<int32_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(float)>("conversion"sv),
		intrinsic_function_descriptor<int32_t(double)>("conversion"sv),

		intrinsic_function_descriptor<int64_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(float)>("conversion"sv),
		intrinsic_function_descriptor<int64_t(double)>("conversion"sv),

		intrinsic_function_descriptor<uint8_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(float)>("conversion"sv),
		intrinsic_function_descriptor<uint8_t(double)>("conversion"sv),

		intrinsic_function_descriptor<uint16_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(float)>("conversion"sv),
		intrinsic_function_descriptor<uint16_t(double)>("conversion"sv),

		intrinsic_function_descriptor<uint32_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(float)>("conversion"sv),
		intrinsic_function_descriptor<uint32_t(double)>("conversion"sv),

		intrinsic_function_descriptor<uint64_t(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(float)>("conversion"sv),
		intrinsic_function_descriptor<uint64_t(double)>("conversion"sv),

		intrinsic_function_descriptor<float(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<float(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<float(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<float(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<float(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<float(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<float(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<float(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<float(double)>("conversion"sv),

		intrinsic_function_descriptor<double(int8_t)>("conversion"sv),
		intrinsic_function_descriptor<double(int16_t)>("conversion"sv),
		intrinsic_function_descriptor<double(int32_t)>("conversion"sv),
		intrinsic_function_descriptor<double(int64_t)>("conversion"sv),
		intrinsic_function_descriptor<double(uint8_t)>("conversion"sv),
		intrinsic_function_descriptor<double(uint16_t)>("conversion"sv),
		intrinsic_function_descriptor<double(uint32_t)>("conversion"sv),
		intrinsic_function_descriptor<double(uint64_t)>("conversion"sv),
		intrinsic_function_descriptor<double(float)>("conversion"sv),
	};

	Program::Program()
	{
		auto const built_in_types_to_add = built_in_types();

		types.reserve(built_in_types_to_add.size());
		global_scope.types.reserve(built_in_types_to_add.size() + 1);

		for (auto const type : built_in_types_to_add)
		{
			global_scope.types.push_back({std::string(type.first), {false, false, false, static_cast<unsigned>(types.size())}});
			types.push_back(type.second);
			types.back().ABI_name = type.first;
		}
		global_scope.types.push_back({"char", {false, false, false, 5}}); // Add char as typedef for uint8

		//*******************************************************************

		size_t const intrinsic_function_count = std::size(intrinsic_functions);
		global_scope.functions.reserve(intrinsic_function_count);

		for (size_t i = 0; i < intrinsic_function_count; ++i)
		{
			auto const & fn = intrinsic_functions[i];
			global_scope.functions.push_back({std::string(fn.name), {FunctionId::Type::intrinsic, static_cast<unsigned>(i)}});
		}
	}

	auto add_type(Program & program, Type new_type) noexcept -> TypeId
	{
		unsigned const type_index = static_cast<unsigned>(program.types.size());
		program.types.push_back(std::move(new_type));
		return TypeId::with_index(type_index);
	}

	auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &
	{
		assert(!id.is_function && id.index < program.types.size());
		return program.types[id.index];
	}

	auto type_size(Program const & program, TypeId id) noexcept -> int
	{
		if (id.is_reference)
			return sizeof(void *);
		else if (id.is_function)
			return 0;
		else
			return type_with_id(program, id).size;
	}

	auto type_alignment(Program const & program, TypeId id) noexcept -> int
	{
		if (id.is_reference)
			return alignof(void *);
		else if (id.is_function)
			return 1;
		else
			return type_with_id(program, id).alignment;
	}

	auto is_default_constructible(Struct const & type) noexcept -> bool
	{
		return std::all_of(type.member_variables, [](MemberVariable const & var) { return var.initializer_expression.has_value(); });
	}

	auto is_default_constructible(TypeId type_id, Program const & program) noexcept -> bool
	{
		if (type_id.is_function)
			return false;

		Type const & type = type_with_id(program, type_id);

		if (Struct const * const struct_data = struct_for_type(program, type))
			return is_default_constructible(*struct_data);
		else if (Type::Array const * array = try_get<Type::Array>(type.extra_data))
			return is_default_constructible(array->value_type, program);
		else
			return false;
	}

	auto synthesize_default_constructor(TypeId type_id, Struct const & struct_data) noexcept -> expression::Constructor
	{
		expression::Constructor default_constructor_node;
		default_constructor_node.constructed_type = type_id;
		default_constructor_node.parameters.reserve(struct_data.member_variables.size());
		for (MemberVariable const & var : struct_data.member_variables)
			default_constructor_node.parameters.push_back(*var.initializer_expression);
		return default_constructor_node;
	}

	auto synthesize_default_constructor(TypeId type_id, Type::Array array_data, Program const & program) noexcept -> expression::Constructor
	{
		expression::Constructor default_constructor_node;
		default_constructor_node.constructed_type = type_id;
		default_constructor_node.parameters.reserve(array_data.size);
		default_constructor_node.parameters.push_back(synthesize_default_constructor(array_data.value_type, program));
		for (int i = 1; i < array_data.size; ++i)
			default_constructor_node.parameters.push_back(default_constructor_node.parameters[0]);
		return default_constructor_node;
	}

	auto synthesize_default_constructor(TypeId type_id, Program const & program) noexcept -> expression::Constructor
	{
		Type const & type = type_with_id(program, type_id);
		if (is_struct(type))
			return synthesize_default_constructor(type_id, *struct_for_type(program, type));
		else if (is_array(type))
			return synthesize_default_constructor(type_id, std::get<Type::Array>(type.extra_data), program);

		declare_unreachable();
	}

	auto ABI_name(Program & program, TypeId id) noexcept -> std::string &
	{
		return program.types[id.index].ABI_name;
	}

	auto ABI_name(Program const & program, TypeId id) noexcept -> std::string_view
	{
		return type_with_id(program, id).ABI_name;
	}

	auto is_trivially_destructible(Program const & program, TypeId id) noexcept -> bool
	{
		return destructor_for(program, id) == invalid_function_id;
	}

	auto destructor_for(Program const & program, TypeId id) noexcept -> FunctionId
	{
		if (id.is_reference || id.is_function)
			return invalid_function_id;

		Type const & type = type_with_id(program, id);
		if (Type::Struct const * const struct_data = try_get<Type::Struct>(type.extra_data))
			return program.structs[struct_data->struct_index].destructor;
		else
			return invalid_function_id;
	}

	auto add_struct_type(Program & program, Type new_type, Struct new_struct) -> std::pair<TypeId, int>
	{
		auto const [new_type_id, new_struct_id] = add_struct_type_without_destructor(program, std::move(new_type), std::move(new_struct));

		if (std::any_of(program.structs[new_struct_id].member_variables, [&](complete::MemberVariable const & var) { return !is_trivially_destructible(program, var.type); }))
		{
			complete::Function destructor = instantiation::synthesize_default_destructor(new_type_id, program.structs[new_struct_id].member_variables, program);
			program.structs[new_struct_id].destructor = add_function(program, std::move(destructor));
		}

		return {new_type_id, new_struct_id};
	}

	auto add_struct_type_without_destructor(Program & program, Type new_type, Struct new_struct) -> std::pair<TypeId, int>
	{
		int const new_struct_id = static_cast<int>(program.structs.size());
		new_type.extra_data = Type::Struct{ new_struct_id };
		program.structs.push_back(std::move(new_struct));

		TypeId const new_type_id = add_type(program, std::move(new_type));

		return {new_type_id, new_struct_id};
	}

	auto add_struct_template(Program & program, StructTemplate new_template) noexcept -> StructTemplateId
	{
		StructTemplateId const template_id = StructTemplateId{static_cast<unsigned>(program.struct_templates.size())};
		program.struct_templates.push_back(std::move(new_template));
		return template_id;
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
		auto const it = std::find_if(type.member_variables, [member_name](MemberVariable const & var) { return var.name == member_name; });

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
		new_type.extra_data = Type::Pointer{pointee_type};
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

	auto array_size(Type const & array_type) noexcept -> int
	{
		assert(is_array(array_type));
		return try_get<Type::Array>(array_type.extra_data)->size;
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

	auto add_function(Program & program, Function new_function) noexcept -> FunctionId
	{
		FunctionId const function_id = FunctionId{FunctionId::Type::program, static_cast<unsigned>(program.functions.size())};
		program.functions.push_back(std::move(new_function));
		return function_id;
	}

	auto add_function_template(Program & program, FunctionTemplate new_function_template) noexcept -> FunctionTemplateId
	{
		FunctionTemplateId const function_template_id = FunctionTemplateId{static_cast<unsigned>(program.function_templates.size())};
		program.function_templates.push_back(std::move(new_function_template));
		return function_template_id;
	}

	auto parameter_types_of(Program const & program, FunctionId id) noexcept -> std::vector<TypeId>
	{
		if (id.type == FunctionId::Type::intrinsic)
		{
			return intrinsic_functions[id.index].parameter_types;
		}
		else if (id.type == FunctionId::Type::imported)
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

	auto parameter_size(Program const & program, FunctionId id) noexcept -> int
	{
		if (id.type == FunctionId::Type::intrinsic)
		{
			return 16; // Overshoots but works for all intrinsics and 16 is not that much anyway
		}
		else if (id.type == FunctionId::Type::imported)
		{
			return program.extern_functions[id.index].parameter_size;
		}
		else
		{
			return program.functions[id.index].parameter_size;
		}
	}

	auto parameter_alignment(Program const & program, FunctionId id) noexcept -> int
	{
		if (id.type == FunctionId::Type::intrinsic)
		{
			return 8; // Overshoots but works for all intrinsics and 16 is not that much anyway
		}
		else if (id.type == FunctionId::Type::imported)
		{
			return program.extern_functions[id.index].parameter_alignment;
		}
		else
		{
			return program.functions[id.index].stack_frame_alignment;
		}
	}

	auto stack_frame_size(Program const & program, FunctionId id) noexcept -> int
	{
		if (id.type == FunctionId::Type::intrinsic)
		{
			return 16; // Overshoots but works for all intrinsics and 16 is not that much anyway
		}
		else if (id.type == FunctionId::Type::imported)
		{
			return program.extern_functions[id.index].parameter_size;
		}
		else
		{
			return program.functions[id.index].stack_frame_size;
		}
	}

	auto return_type(Program const & program, FunctionId id) noexcept -> TypeId
	{
		if (id.type == FunctionId::Type::intrinsic)
			return intrinsic_functions[id.index].return_type;
		else if (id.type == FunctionId::Type::imported)
			return program.extern_functions[id.index].return_type;
		else
			return program.functions[id.index].return_type;
	}

	auto is_callable_at_compile_time(Program const & program, FunctionId id) noexcept -> bool
	{
		if (id.type == FunctionId::Type::intrinsic)
			return true;
		else if (id.type == FunctionId::Type::imported)
			return false;
		else
			return program.functions[id.index].is_callable_at_compile_time;
	}

	auto is_callable_at_runtime(Program const & program, FunctionId id) noexcept -> bool
	{
		if (id.type == FunctionId::Type::intrinsic)
			return true;
		else if (id.type == FunctionId::Type::imported)
			return true;
		else
			return program.functions[id.index].is_callable_at_runtime;
	}

	auto find_namespace(Namespace & current_namespace, std::string_view name) noexcept -> Namespace *
	{
		auto const it = std::find_if(current_namespace.nested_namespaces, [name](Namespace const & ns) { return ns.name == name; });
		if (it != current_namespace.nested_namespaces.end())
			return &*it;
		else 
			return nullptr;
	}

	auto find_namespace(Namespace & current_namespace, span<std::string_view const> names) noexcept -> complete::Namespace *
	{
		assert(!names.empty());

		complete::Namespace * ns = &current_namespace;
		for (std::string_view const name : names)
		{
			ns = find_namespace(*ns, name);
			if (ns == nullptr)
				return nullptr;
		}

		return ns;
	}

	auto add_namespace(Namespace & current_namespace, std::string_view name) noexcept -> Namespace &
	{
		auto const it = find_namespace(current_namespace, name);
		if (it != nullptr)
		{
			return *it;
		}
		else
		{
			Namespace & new_namespace = current_namespace.nested_namespaces.emplace_back();
			new_namespace.name = name;
			return new_namespace;
		}
	}

	auto ABI_name(Program & program, FunctionId id) noexcept -> std::string &
	{
		assert(id.type != FunctionId::Type::intrinsic);
		if (id.type == FunctionId::Type::imported)
			return program.extern_functions[id.index].ABI_name;
		else
			return program.functions[id.index].ABI_name;
	}

	auto ABI_name(Program const & program, FunctionId id) noexcept -> std::string_view
	{
		assert(id.type != FunctionId::Type::intrinsic);
		if (id.type == FunctionId::Type::imported)
			return program.extern_functions[id.index].ABI_name;
		else
			return program.functions[id.index].ABI_name;
	}

	auto ABI_name(Program & program, FunctionTemplateId id) noexcept -> std::string &
	{
		return program.function_templates[id.index].ABI_name;
	}

	auto ABI_name(Program const & program, FunctionTemplateId id) noexcept -> std::string_view
	{
		return program.function_templates[id.index].ABI_name;
	}

	auto ABI_name(Program & program, StructTemplateId id) noexcept -> std::string &
	{
		return program.struct_templates[id.index].ABI_name;
	}

	auto ABI_name(Program const & program, StructTemplateId id) noexcept -> std::string_view
	{
		return program.struct_templates[id.index].ABI_name;
	}

	auto type_for_overload_set(Program & program, OverloadSet overload_set) noexcept -> TypeId
	{
		TypeId new_type_id;
		new_type_id.flat_value = 0;
		new_type_id.is_function = true;
		new_type_id.index = static_cast<unsigned>(program.overload_set_types.size());
		program.overload_set_types.push_back(std::move(overload_set));
		return new_type_id;
	}

	auto overload_set_for_type(Program const & program, TypeId overload_set_type) noexcept -> OverloadSetView
	{
		assert(overload_set_type.is_function);
		return program.overload_set_types[overload_set_type.index];
	}

	auto instantiate_function_template(Program & program, FunctionTemplateId template_id, span<TypeId const> parameters) noexcept -> expected<FunctionId, PartialSyntaxError>
	{
		FunctionTemplate & function_template = program.function_templates[template_id.index];
		assert(parameters.size() == function_template.incomplete_function.template_parameters.size());

		auto const cached_instantiation = function_template.cached_instantiations.find(parameters);
		if (cached_instantiation != function_template.cached_instantiations.end())
			return cached_instantiation->second;

		std::vector<ResolvedTemplateParameter> all_template_parameters;
		all_template_parameters.reserve(function_template.scope_template_parameters.size() + parameters.size());
		for (ResolvedTemplateParameter const id : function_template.scope_template_parameters) 
			all_template_parameters.push_back(id);

		for (size_t i = 0; i < parameters.size(); ++i) 
			all_template_parameters.push_back({std::string(function_template.incomplete_function.template_parameters[i].name), parameters[i]});

		instantiation::ScopeStack scope_stack = function_template.scope_stack;

		try_call_decl(Function instantiated_function,
			instantiation::instantiate_function_template(function_template.incomplete_function, all_template_parameters, scope_stack, out(program)));

		instantiated_function.ABI_name = function_template.ABI_name;
		FunctionId const instantiated_function_id = add_function(program, std::move(instantiated_function));

		auto parameters_to_insert = std::vector<TypeId>(parameters.begin(), parameters.end());
		function_template.cached_instantiations.emplace(std::move(parameters_to_insert), instantiated_function_id);

		return instantiated_function_id;
	}

	auto check_concepts(
		span<FunctionId const> concepts, 
		span<TypeId const> parameters, 
		std::vector<ResolvedTemplateParameter> template_parameters,
		instantiation::ScopeStack scope_stack,
		Program& program
	) noexcept -> size_t
	{
		assert(concepts.size() == parameters.size());

		expression::FunctionCall function_call;
		for (size_t i = 0; i < concepts.size(); ++i)
		{
			if (concepts[i] != invalid_function_id)
			{
				function_call.function_id = concepts[i];
				function_call.parameters.push_back(expression::Literal<TypeId>{parameters[i]});
				auto const concept_passed = interpreter::evaluate_constant_expression_as<bool>(function_call, template_parameters, scope_stack, program);
				if (!concept_passed.has_value() || !concept_passed.value())
					return i;
			}
			function_call.parameters.clear();
		}

		return concepts.size();
	}

	auto instantiate_struct_template(Program & program, StructTemplateId template_id, span<TypeId const> parameters, std::string_view instantiation_in_source) noexcept 
		-> expected<TypeId, PartialSyntaxError>
	{
		StructTemplate & struct_template = program.struct_templates[template_id.index];

		if (auto const it = struct_template.cached_instantiations.find(parameters);
			it != struct_template.cached_instantiations.end())
			return it->second;

		if (parameters.size() != struct_template.incomplete_struct.template_parameters.size())
			return make_syntax_error(instantiation_in_source, "Incorrect number of parameters for function template instantiation.");

		size_t const failed_concept = check_concepts(struct_template.concepts, parameters, struct_template.scope_template_parameters, struct_template.scope_stack, program);
		if (failed_concept != parameters.size())
			return make_syntax_error(
				instantiation_in_source, 
				join("Struct template parameter does not satisfy concept \"", struct_template.incomplete_struct.template_parameters[failed_concept].concept, "\"."));

		Type new_type;
		new_type.size = 0;
		new_type.alignment = 1;
		new_type.ABI_name = struct_template.ABI_name;
		Struct new_struct;

		std::vector<ResolvedTemplateParameter> all_template_parameters;
		all_template_parameters.reserve(struct_template.scope_template_parameters.size() + parameters.size());
		for (ResolvedTemplateParameter const id : struct_template.scope_template_parameters)
			all_template_parameters.push_back(id);

		for (size_t i = 0; i < parameters.size(); ++i)
			all_template_parameters.push_back({std::string(struct_template.incomplete_struct.template_parameters[i].name), parameters[i]});

		instantiation::ScopeStack scope_stack = struct_template.scope_stack;

		for (incomplete::MemberVariable const & var_template : struct_template.incomplete_struct.member_variables)
		{
			try_call_decl(TypeId const var_type, instantiation::resolve_dependent_type(var_template.type, all_template_parameters, scope_stack, out(program)));

			add_variable_to_scope(new_struct.member_variables, new_type.size, new_type.alignment, var_template.name, var_type, 0, program);
			if (var_template.initializer_expression)
			{
				try_call(assign_to(new_struct.member_variables.back().initializer_expression), instantiation::instantiate_expression(
					*var_template.initializer_expression, all_template_parameters, scope_stack, out(program), nullptr));
			}
		}

		Type::TemplateInstantiation template_instantiation;
		template_instantiation.template_id = template_id;
		template_instantiation.parameters.assign(parameters.begin(), parameters.end());
		new_type.template_instantiation = std::move(template_instantiation);

		auto const[new_type_id, new_struct_id] = add_struct_type(program, std::move(new_type), std::move(new_struct));
		struct_template.cached_instantiations.emplace(std::vector<TypeId>(parameters.begin(), parameters.end()), new_type_id);

		return new_type_id;
	}

	auto insert_mutref_conversion_node_impl(Expression && expr, TypeId from, TypeId to, Program const & program, bool allow_address_of_temporary) noexcept -> expected<Expression, ConversionNotFound>
	{
		if (from.index == to.index)
		{
			// From reference to reference and value to value there is no conversion. A pointer is a pointer, regardless of mutability.
			if (from.is_reference == to.is_reference)
				return std::move(expr);

			if (from.is_reference && !to.is_reference)
			{
				expression::Dereference deref_node;
				deref_node.expression = allocate(std::move(expr));
				deref_node.return_type = to;
				return std::move(deref_node);
			}
			else
			{
				if (to.is_mutable)
					return Error(ConversionNotFound(from, to, "Can't bind a temporary to a mutable reference."));

				if (allow_address_of_temporary)
					return std::move(expr);
				else 
					return Error(ConversionNotFound(from, to, "Cannot take address of temporary."));
			}
		}
		else if (is_pointer(type_with_id(program, from)) && is_pointer(type_with_id(program, to)) && !from.is_reference && !to.is_reference)
		{
			return std::move(expr);
		}
		return Error(ConversionNotFound(from, to, "Conversion between types does not exist."));
	}

	auto insert_mutref_conversion_node(Expression && expr, TypeId from, TypeId to, Program const & program) noexcept -> expected<Expression, ConversionNotFound>
	{
		return insert_mutref_conversion_node_impl(std::move(expr), from, to, program, false);
	}

	auto insert_mutref_conversion_node(Expression && expr, TypeId to, Program const & program) noexcept -> expected<Expression, ConversionNotFound>
	{
		complete::TypeId const from = expression_type_id(expr, program);
		return insert_mutref_conversion_node(std::move(expr), from, to, program);
	}

	auto insert_mutref_conversion_node(Expression && expr, TypeId from, TypeId to, Program const & program, std::string_view source) noexcept -> expected<Expression, PartialSyntaxError>
	{
		if (auto conversion = insert_mutref_conversion_node(std::move(expr), from, to, program))
			return std::move(*conversion);
		else
			return make_syntax_error(source, join("Error in conversion from ", 
				ABI_name(program, conversion.error().from), " to ", ABI_name(program, conversion.error().to), ": ", conversion.error().why));
	}

	auto insert_mutref_conversion_node(Expression && expr, TypeId to, Program const & program, std::string_view source) noexcept -> expected<Expression, PartialSyntaxError>
	{
		complete::TypeId const from = expression_type_id(expr, program);
		return insert_mutref_conversion_node(std::move(expr), from, to, program, source);
	}

	auto check_type_validness_as_overload_candidate(TypeId param_type, TypeId parsed_type, Program const & program, int & conversions) noexcept -> bool
	{
		if (param_type == parsed_type)
			return true;

		if (is_convertible(parsed_type, param_type, program))
		{
			conversions++;
			return true;
		}
		else
		{
			return false;
		}
	}

	// Checks if a type satisfies a pattern, and resolves missing dependent types.
	auto expected_type_according_to_pattern(
		TypeId given_parameter, 
		FunctionTemplateParameterType const & expected_pattern,
		span<TypeId> resolved_dependent_types, 
		Program & program) -> TypeId
	{
		auto const visitor = overload(
			[](FunctionTemplateParameterType::BaseCase const & base_case)
			{
				return base_case.type;
			},
			[&](FunctionTemplateParameterType::TemplateParameter const & param)
			{
				TypeId const expected_type = decay(given_parameter);
				if (resolved_dependent_types[param.index] == TypeId::none)
				{
					resolved_dependent_types[param.index] = expected_type;
					return expected_type;
				}
				else if (resolved_dependent_types[param.index] == expected_type)
					return expected_type;
				else
					return TypeId::none;
			},
			[&](FunctionTemplateParameterType::Pointer const & pointer)
			{
				Type const & type = type_with_id(program, given_parameter);

				if (!is_pointer(type))
					return TypeId::none; // Does not satisfy the pattern

				TypeId const pointee = pointee_type(type);
				TypeId const expected_pointee = expected_type_according_to_pattern(pointee, *pointer.pointee, resolved_dependent_types, program);

				if (expected_pointee == TypeId::none)
					return TypeId::none;
				else
					return pointer_type_for(expected_pointee, program);
			},
			[](FunctionTemplateParameterType::Array const & /*array*/) -> TypeId
			{
				mark_as_to_do("Dependent array types");
			},
			[&](FunctionTemplateParameterType::ArrayPointer const & pointer)
			{
				Type const & type = type_with_id(program, given_parameter);

				if (!is_array_pointer(type))
					return TypeId::none; // Does not satisfy the pattern

				TypeId const pointee = pointee_type(type);
				TypeId const expected_pointee = expected_type_according_to_pattern(pointee, *pointer.pointee, resolved_dependent_types, program);

				if (expected_pointee == TypeId::none)
					return TypeId::none;
				else
					return array_pointer_type_for(expected_pointee, program);
			},
			[&](FunctionTemplateParameterType::TemplateInstantiation const & template_instantiation) -> TypeId
			{
				Type const & type = type_with_id(program, given_parameter);
				if (!type.template_instantiation.has_value() || type.template_instantiation->template_id != template_instantiation.template_id)
					return TypeId::none;

				if (template_instantiation.parameters.size() != type.template_instantiation->parameters.size())
					return TypeId::none;

				size_t const n = template_instantiation.parameters.size();
				for (size_t i = 0; i < n; ++i)
				{
					TypeId const expected_param = expected_type_according_to_pattern(
						type.template_instantiation->parameters[i],
						template_instantiation.parameters[i],
						resolved_dependent_types, program);

					if (expected_param == TypeId::none)
						return TypeId::none;
				}

				return decay(given_parameter);
			}
		);

		TypeId expected_type = std::visit(visitor, expected_pattern.value);
		expected_type.is_reference = expected_pattern.is_reference;
		expected_type.is_mutable = expected_pattern.is_mutable;
		return expected_type;
	}

	auto resolve_function_overloading(OverloadSetView overload_set, span<TypeId const> parameters, Program & program) noexcept -> FunctionId
	{
		struct Candidate
		{
			int conversions;
			FunctionId function_id;
		};
		Candidate candidates[64];
		int candidate_count = 0;

		struct TemplateCandidate
		{
			int conversions;
			FunctionTemplateId id;
		};
		TemplateCandidate template_candidates[64];
		int template_candidate_count = 0;

		for (FunctionId function_id : overload_set.function_ids)
		{
			auto const param_types = parameter_types_of(program, function_id);
			if (param_types.size() == parameters.size())
			{
				int conversions = 0;
				bool discard = false;
				for (size_t i = 0; i < param_types.size(); ++i)
				{
					if (!check_type_validness_as_overload_candidate(param_types[i], parameters[i], program, conversions))
					{
						discard = true;
						break;
					}
				}

				if (!discard)
				{
					candidates[candidate_count++] = Candidate{ conversions, function_id };
				}
			}
		}

		TypeId resolved_dependent_types[32];
		size_t dependent_type_count = 0;

		for (FunctionTemplateId template_id : overload_set.function_template_ids)
		{
			FunctionTemplate const & fn = program.function_templates[template_id.index];
			span<FunctionTemplateParameterType const> const fn_parameters = fn.parameter_types;
			if (fn_parameters.size() == parameters.size())
			{
				dependent_type_count = fn.incomplete_function.template_parameters.size();
				std::fill(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none);

				int conversions = 0;
				bool discard = false;
				for (size_t i = 0; i < parameters.size(); ++i)
				{
					TypeId const expected_type = expected_type_according_to_pattern(parameters[i], fn_parameters[i], resolved_dependent_types, program);
					if (expected_type == TypeId::none)
					{
						discard = true;
						break;
					}

					if (!check_type_validness_as_overload_candidate(expected_type, parameters[i], program, conversions))
					{
						discard = true;
						break;
					}
				}

				if (check_concepts(fn.concepts, {resolved_dependent_types, dependent_type_count}, fn.scope_template_parameters, fn.scope_stack, program) != fn.concepts.size())
					discard = true;

				if (!discard)
				{
					template_candidates[template_candidate_count++] = TemplateCandidate{conversions, template_id};
				}
			}
		}

		if (candidate_count == 0 && template_candidate_count == 0)
		{
			return invalid_function_id;
		}
		else
		{
			Candidate best_candidate;
			TemplateCandidate best_template_candidate;

			if (candidate_count > 1)
			{
				std::partial_sort(candidates, candidates + 2, candidates + candidate_count, [](Candidate a, Candidate b) { return a.conversions < b.conversions; });
				assert(candidates[0].conversions < candidates[1].conversions); // Ambiguous call.
			}
			best_candidate = candidates[0];

			if (template_candidate_count > 1)
			{
				std::partial_sort(template_candidates, template_candidates + 2, template_candidates + template_candidate_count,
					[](TemplateCandidate a, TemplateCandidate b) { return a.conversions < b.conversions; });
				assert(template_candidates[0].conversions < template_candidates[1].conversions); // Ambiguous call.
			}
			best_template_candidate = template_candidates[0];

			if (template_candidate_count == 0 || candidate_count > 0 && best_candidate.conversions < best_template_candidate.conversions)
				return best_candidate.function_id;
			else
			{
				// Ensure that all template parameters have been resolved.
				assert(std::find(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none) == resolved_dependent_types + dependent_type_count);
				auto function_id = instantiate_function_template(program, best_template_candidate.id, {resolved_dependent_types, dependent_type_count});
				assert(function_id.has_value());
				return *function_id;
			}
		}
	}

	auto check_function_template_as_conversion_candidate(
		FunctionTemplateId template_id, TypeId from, TypeId to, 
		size_t & dependent_type_count, span<TypeId> resolved_dependent_types, int & conversions,
		Program & program) -> bool
	{
		FunctionTemplate const & fn = program.function_templates[template_id.index];
		span<FunctionTemplateParameterType const> const fn_parameters = fn.parameter_types;
		assert(fn_parameters.size() == 2);

		dependent_type_count = fn.incomplete_function.template_parameters.size();
		std::fill(resolved_dependent_types.begin(), resolved_dependent_types.begin() + dependent_type_count, TypeId::none);

		TypeId const expected_type_from = expected_type_according_to_pattern(from, fn_parameters[0], resolved_dependent_types, program);
		if (expected_type_from == TypeId::none)
			return false;

		if (!check_type_validness_as_overload_candidate(expected_type_from, from, program, conversions))
			return false;

		TypeId const expected_type_to = expected_type_according_to_pattern(to, fn_parameters[1], resolved_dependent_types, program);
		if (expected_type_to == TypeId::none)
			return false;

		if (!check_type_validness_as_overload_candidate(to, expected_type_to, program, conversions))
			return false;

		if (check_concepts(fn.concepts, resolved_dependent_types.subspan(0, dependent_type_count), fn.scope_template_parameters, fn.scope_stack, program) != fn.concepts.size())
			return false;

		return true;
	}

	auto resolve_function_overloading_for_conversions(OverloadSetView overload_set, TypeId from, TypeId to, Program & program) noexcept -> FunctionId
	{
		struct Candidate
		{
			int conversions;
			FunctionId function_id;
		};
		Candidate candidates[64];
		int candidate_count = 0;

		for (FunctionId function_id : overload_set.function_ids)
		{
			auto const param_types = parameter_types_of(program, function_id);
			assert(param_types.size() == 1);
			TypeId const input_types[] = {from, return_type(program, function_id) };
			TypeId const expected_types[] = {parameter_types_of(program, function_id)[0], to};
			{
				int conversions = 0;
				bool discard = false;
				for (size_t i = 0; i < 2; ++i)
				{
					if (!check_type_validness_as_overload_candidate(expected_types[i], input_types[i], program, conversions))
					{
						discard = true;
						break;
					}
				}

				if (!discard)
				{
					candidates[candidate_count++] = Candidate{conversions, function_id};
				}
			}
		}

		struct TemplateCandidate
		{
			int conversions;
			FunctionTemplateId id;
		};
		TemplateCandidate template_candidates[64];
		int template_candidate_count = 0;
		TypeId resolved_dependent_types[32];
		size_t dependent_type_count = 0;

		for (FunctionTemplateId template_id : overload_set.function_template_ids)
		{
			int conversions = 0;
			if (check_function_template_as_conversion_candidate(template_id, from, to, dependent_type_count, resolved_dependent_types, conversions, program))
			{
				template_candidates[template_candidate_count++] = TemplateCandidate{conversions, template_id};
			}
		}

		if (candidate_count == 0 && template_candidate_count == 0)
		{
			return invalid_function_id;
		}
		else
		{
			Candidate best_candidate;
			TemplateCandidate best_template_candidate;

			if (candidate_count > 1)
			{
				std::partial_sort(candidates, candidates + 2, candidates + candidate_count, [](Candidate a, Candidate b) { return a.conversions < b.conversions; });
				assert(candidates[0].conversions < candidates[1].conversions); // Ambiguous call.
			}
			best_candidate = candidates[0];

			if (template_candidate_count > 1)
			{
				std::partial_sort(template_candidates, template_candidates + 2, template_candidates + template_candidate_count,
					[](TemplateCandidate a, TemplateCandidate b) { return a.conversions < b.conversions; });
				assert(template_candidates[0].conversions < template_candidates[1].conversions); // Ambiguous call.
			}
			best_template_candidate = template_candidates[0];

			if (template_candidate_count == 0 || candidate_count > 0 && best_candidate.conversions < best_template_candidate.conversions)
				return best_candidate.function_id;
			else
			{
				// Ensure that all template parameters have been resolved.
				assert(std::find(resolved_dependent_types, resolved_dependent_types + dependent_type_count, TypeId::none) == resolved_dependent_types + dependent_type_count);
				auto function_id = instantiate_function_template(program, best_template_candidate.id, { resolved_dependent_types, dependent_type_count });
				assert(function_id.has_value());
				return *function_id;
			}
		}
	}

	auto resolve_function_overloading_and_insert_conversions(OverloadSetView overload_set, span<Expression> parameters, span<TypeId const> parameter_types, Program & program) noexcept
		-> FunctionId
	{
		FunctionId const function_id = resolve_function_overloading(overload_set, parameter_types, program);
		if (function_id == invalid_function_id)
			return invalid_function_id;

		// If any conversion is needed in order to call the function, perform the conversion.
		auto const target_parameter_types = parameter_types_of(program, function_id);
		for (size_t i = 0; i < target_parameter_types.size(); ++i)
		{
			if (parameter_types[i] != target_parameter_types[i])
				parameters[i] = std::move(*insert_mutref_conversion_node_impl(std::move(parameters[i]), parameter_types[i], target_parameter_types[i], program, true));
		}

		return function_id;
	}

} // namespace complete
