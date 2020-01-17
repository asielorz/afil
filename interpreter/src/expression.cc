#include "expression.hh"
#include "statement.hh"
#include "program.hh"
#include "overload.hh"
#include "unreachable.hh"
#include "string.hh"
#include "variant.hh"
#include <cassert>

namespace expr
{

	auto precedence(Operator op) noexcept -> int
	{
		switch (op)
		{
			case Operator::add:					return 6;
			case Operator::subtract:			return 6;
			case Operator::multiply:			return 7;
			case Operator::divide:				return 7;
			case Operator::modulo:				return 7;
			case Operator::equal:				return 3;
			case Operator::not_equal:			return 3;
			case Operator::less:				return 4;
			case Operator::less_equal:			return 4;
			case Operator::greater:				return 4;
			case Operator::greater_equal:		return 4;
			case Operator::three_way_compare:	return 5;
			case Operator::and_:				return 2;
			case Operator::or_:					return 0;
			case Operator::xor_:				return 1;
			case Operator::not:					return 2;
			case Operator::assign:				return -1;
		}
		declare_unreachable();
	}

	auto operator_function_name(Operator op) noexcept -> std::string_view
	{
		switch (op)
		{
			case Operator::add:					return "+";
			case Operator::subtract:			return "-";
			case Operator::multiply:			return "*";
			case Operator::divide:				return "/";
			case Operator::modulo:				return "%";
			case Operator::equal:				return "==";
			case Operator::not_equal:			return "==";
			case Operator::less:				return "<=>";
			case Operator::less_equal:			return "<=>";
			case Operator::greater:				return "<=>";
			case Operator::greater_equal:		return "<=>";
			case Operator::three_way_compare:	return "<=>";
			case Operator::and_:				return "and";
			case Operator::or_:					return "or";
			case Operator::xor_:				return "xor";
			case Operator::not:					return "not";
			case Operator::assign:				return "=";
			case Operator::addressof:			return "&";
		}
		declare_unreachable();
	}

	auto operator_overload_set(Operator op, ScopeStackView scope_stack, span<char const> string_pool) -> lookup_result::OverloadSet
	{
		auto const visitor = overload(
			[](auto) -> lookup_result::OverloadSet { declare_unreachable(); },
			[](lookup_result::Nothing) { return lookup_result::OverloadSet(); },
			[](lookup_result::OverloadSet overload_set) { return overload_set; }
		);

		std::string_view const function_name = operator_function_name(op);
		return std::visit(visitor, lookup_name(scope_stack, function_name, string_pool));
	}

	auto is_operator_node(OperatorTree const & tree) noexcept -> bool
	{
		return tree.index() == 0;
	}

	auto expression_type(ExpressionTree const & tree, Program const & program) noexcept -> Type
	{
		return type_with_id(program, expression_type_id(tree, program));
	}

	auto expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> TypeId
	{
		auto const type = maybe_dependent_expression_type_id(tree, program);
		assert(has_type<TypeId>(type));
		return std::get<TypeId>(type);
	}

	auto maybe_dependent_expression_type_id(ExpressionTree const & tree, Program const & program) noexcept -> std::variant<TypeId, DependentTypeId>
	{
		using Ret = std::variant<TypeId, DependentTypeId>;

		auto const visitor = overload(
			[](Literal<int>) -> Ret { return TypeId::int_; },
			[](Literal<float>) -> Ret { return TypeId::float_; },
			[](Literal<bool>) -> Ret { return TypeId::bool_; },
			[](DereferenceNode const & deref_node) -> Ret { return deref_node.variable_type; },
			[](AddressofNode const & addressof_node) -> Ret { return addressof_node.return_type; },
			[](DepointerNode const & deptr_node) -> Ret { return deptr_node.return_type; },
			[](SubscriptNode const & subscript_node) -> Ret { return subscript_node.return_type; },
			[](VariableNode const & var_node) -> Ret { return make_reference(var_node.variable_type); },
			[&](MemberVariableNode const & var_node) -> Ret 
			{
				auto const owner_type = maybe_dependent_expression_type_id(*var_node.owner, program);
				if (TypeId const * owner_type_id = try_get<TypeId>(owner_type))
				{
					TypeId var_type = var_node.variable_type;
					var_type.is_reference = owner_type_id->is_reference;
					var_type.is_mutable = owner_type_id->is_mutable;
					return var_type;
				}
				declare_unreachable(); // A MemberVariableNode can't have a dependent owner. That's what tmp::MemberVariableNode is for.
			},
			[](FunctionNode const &) -> Ret { return TypeId::function; },
			[](FunctionTemplateNode const &) -> Ret { return TypeId::function; },
			[&](FunctionCallNode const & func_call_node) -> Ret { return return_type(program, func_call_node.function_id); },
			[](RelationalOperatorCallNode const &) -> Ret { return TypeId::bool_; },
			[](AssignmentNode const &) -> Ret { return TypeId::void_; },
			[&](IfNode const & if_node) -> Ret { return maybe_dependent_expression_type_id(*if_node.then_case, program); },
			[](StatementBlockNode const & block_node) -> Ret { return block_node.return_type; },
			[](StructConstructorNode const & constructor_node) -> Ret { return constructor_node.constructed_type; },
			[](ArrayConstructorNode const & constructor_node) -> Ret { return constructor_node.constructed_type; },
			[](tmp::LocalVariableNode const & var_node) -> Ret { return var_node.type; },
			[](tmp::MemberVariableNode const &) -> Ret { return DependentTypeId::unknown; },
			[](tmp::FunctionCallNode const &) -> Ret { return DependentTypeId::unknown; },
			[](tmp::RelationalOperatorCallNode const &) -> Ret { return TypeId::bool_; },
			[](tmp::DereferenceNode const &) -> Ret { return DependentTypeId::unknown; },
			[](tmp::StructConstructorNode const &) -> Ret { return TypeId::bool_; },
			[](tmp::StatementBlockNode const &) -> Ret { return DependentTypeId::unknown; }
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto expression_type_size(ExpressionTree const & tree, Program const & program) noexcept -> int
	{
		return type_size(program, expression_type_id(tree, program));
	}

	struct Anything
	{
		template <typename T>
		Anything(const T &) noexcept {}
	};

	auto is_dependent(ExpressionTree const & tree) noexcept -> bool
	{
		auto const visitor = overload(
			[](tmp::DependentNode const &) { return true; },
			[](Anything) { return false; },

			[](DereferenceNode const & deref_node) { return is_dependent(*deref_node.expression); },
			[](AddressofNode const & addressof_node) { return is_dependent(*addressof_node.operand); },
			[](DepointerNode const & deptr_node) { return is_dependent(*deptr_node.operand); },
			[](SubscriptNode const & subscript_node) { return is_dependent(*subscript_node.array) || is_dependent(*subscript_node.index); },
			[](IfNode const & if_node) { return is_dependent(*if_node.condition) || is_dependent(*if_node.then_case) || is_dependent(*if_node.else_case); },
			[](AssignmentNode const & assign_node) { return is_dependent(*assign_node.source) || is_dependent(*assign_node.source);  },
			//[](StatementBlockNode const &) { TODO },
			[](StructConstructorNode const & ctor_node) { return std::any_of(ctor_node.parameters.begin(), ctor_node.parameters.end(), is_dependent); },
			[](ArrayConstructorNode const & ctor_node) { return std::any_of(ctor_node.parameters.begin(), ctor_node.parameters.end(), is_dependent); }
		);
		return std::visit(visitor, tree.as_variant());
	}

} // namespace expr
