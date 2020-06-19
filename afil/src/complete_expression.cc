#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/overload.hh"
#include "utils/warning_macro.hh"

namespace complete
{

	auto expression_type(Expression const & tree, Program const & program) noexcept -> Type const &
	{
		return type_with_id(program, expression_type_id(tree, program));
	}

	auto expression_type_id(Expression const & tree, Program const & program) noexcept -> TypeId
	{
		auto const visitor = overload(
			[](expression::Literal<int>) { return TypeId::int32; },
			[](expression::Literal<float>) { return TypeId::float32; },
			[](expression::Literal<bool>) { return TypeId::bool_; },
			[](expression::Literal<char_t>) { return TypeId::char_; },
			[](expression::StringLiteral const & str_node) { return str_node.type; },
			[](expression::Literal<null_t>) { return TypeId::null_t; },
			[](expression::Literal<TypeId>) { return TypeId::type; },
			[](expression::Variable const & var_node) { return make_reference(var_node.variable_type); },
			[&](expression::MemberVariable const & var_node)
			{
				TypeId const owner_type = expression_type_id(*var_node.owner, program);
				TypeId var_type = var_node.variable_type;
				var_type.is_reference = owner_type.is_reference;
				var_type.is_mutable = owner_type.is_mutable;
				return var_type;
			},
			[](expression::Constant const & constant) { return make_reference(constant.type); },
			[&](expression::FunctionCall const & func_call_node) { return return_type(program, func_call_node.function_id); },
			[](expression::RelationalOperatorCall const &) { return TypeId::bool_; },
			[](expression::Constructor const & ctor_node) { return ctor_node.constructed_type; },
			[](expression::Dereference const & deref_node) { return deref_node.return_type; },
			[](expression::ReinterpretCast const & cast_node) { return cast_node.return_type; },
			[](expression::Subscript const & subscript_node) { return subscript_node.return_type; },
			[](expression::PointerPlusInt const & ptr_arithmetic_node) { return ptr_arithmetic_node.return_type; },
			[](expression::PointerMinusInt const & ptr_arithmetic_node) { return ptr_arithmetic_node.return_type; },
			[](expression::PointerMinusPointer const &) { return complete::TypeId::int32; },
			[&](expression::If const & if_node) { return expression_type_id(*if_node.then_case, program); },
			[](expression::StatementBlock const & block_node) { return block_node.return_type; },
			[](expression::Assignment const &) { return TypeId::void_; },
			[](expression::Compiles const &) { return TypeId::bool_; }
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto expression_type_size(Expression const & tree, Program const & program) noexcept -> int
	{
		return type_size(program, expression_type_id(tree, program));
	}

} // namespace complete
