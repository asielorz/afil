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
			[](expression::Literal<int>) { return TypeId::int_; },
			[](expression::Literal<float>) { return TypeId::float_; },
			[](expression::Literal<bool>) { return TypeId::bool_; },
			[](expression::StringLiteral const & str_node) { return str_node.type; },
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
			[](expression::OverloadSet const &) { return TypeId::function; },
			[&](expression::FunctionCall const & func_call_node) { return return_type(program, func_call_node.function_id); },
			[](expression::RelationalOperatorCall const &) { return TypeId::bool_; },
			[](expression::Constructor const & ctor_node) { return ctor_node.constructed_type; },
			[](expression::Dereference const & deref_node) { return deref_node.return_type; },
			[](expression::ReinterpretCast const & deref_node) { return deref_node.return_type; },
			[](expression::Subscript const & subscript_node) { return subscript_node.return_type; },
			[&](expression::If const & if_node) { return expression_type_id(*if_node.then_case, program); },
			[](expression::StatementBlock const & block_node) { return block_node.return_type; },
			[](expression::Assignment const &) { return TypeId::void_; }
		);
		return std::visit(visitor, tree.as_variant());
	}

	auto expression_type_size(Expression const & tree, Program const & program) noexcept -> int
	{
		return type_size(program, expression_type_id(tree, program));
	}

	auto can_be_run_in_a_constant_expression(Statement const & stmt, int constant_base_index) -> bool
	{
		auto const visitor = overload(
			[=](statement::VariableDeclaration const & var_node) { return is_constant_expression(var_node.assigned_expression, constant_base_index); },
			[=](statement::ExpressionStatement const & expr_node) { return is_constant_expression(expr_node.expression, constant_base_index); },
			[=](statement::Return const & return_node) { return is_constant_expression(return_node.returned_expression, constant_base_index); },
			[=](statement::If const & if_node) 
			{
				return 
					is_constant_expression(if_node.condition, constant_base_index) &&
					can_be_run_in_a_constant_expression(*if_node.then_case, constant_base_index) &&
					can_be_run_in_a_constant_expression(*if_node.else_case, constant_base_index);
			},
			[=](statement::StatementBlock const & block_node)
			{
				return std::all_of(block_node.statements.begin(), block_node.statements.end(),
					[&](Statement const & statement) {return can_be_run_in_a_constant_expression(statement, 0); });
			},
			[=](statement::While const & while_node)
			{
				return
					is_constant_expression(while_node.condition, constant_base_index) &&
					can_be_run_in_a_constant_expression(*while_node.body, constant_base_index);
			},
			[=](statement::For const & for_node)
			{
				return
					can_be_run_in_a_constant_expression(*for_node.init_statement, constant_base_index) &&
					is_constant_expression(for_node.condition, constant_base_index) &&
					is_constant_expression(for_node.end_expression, constant_base_index) &&
					can_be_run_in_a_constant_expression(*for_node.body, constant_base_index);
			},
			[](statement::Break) { return true; },
			[](statement::Continue) { return true; }
		);
		return std::visit(visitor, stmt.as_variant());
	}

	auto is_constant_expression(Expression const & expr, int constant_base_index) noexcept -> bool
	{
		auto const visitor = overload(
			[](expression::Literal<int>) { return true; },
			[](expression::Literal<float>) { return true; },
			[](expression::Literal<bool>) { return true; },
			[](expression::StringLiteral const &) { return true; },
			[=](expression::LocalVariable const & var_node) { return var_node.variable_offset >= constant_base_index; },
			[](expression::GlobalVariable const &) { return false; },
			[=](expression::MemberVariable const & var_node) { return is_constant_expression(*var_node.owner, constant_base_index); },
			[](expression::Constant const &) { return true; },
			[](expression::OverloadSet const &) { return true; },
			TODO("Determine whether the function can be called at compile time or not")
			[=](expression::FunctionCall const & func_call_node) 
			{
				return std::all_of(func_call_node.parameters.begin(), func_call_node.parameters.end(),
					[=](Expression const & param) { return is_constant_expression(param, constant_base_index); });
			},
			[=](expression::RelationalOperatorCall const & op_call_node)
			{
				return std::all_of(op_call_node.parameters.begin(), op_call_node.parameters.end(),
					[=](Expression const & param) { return is_constant_expression(param, constant_base_index); });
			},
			[=](expression::Constructor const & ctor_node) 
			{
				return std::all_of(ctor_node.parameters.begin(), ctor_node.parameters.end(), 
					[=](Expression const & param) { return is_constant_expression(param, constant_base_index); });
			},
			[=](expression::Dereference const & deref_node) { return is_constant_expression(*deref_node.expression, constant_base_index); },
			[=](expression::ReinterpretCast const & cast_node) { return is_constant_expression(*cast_node.operand, constant_base_index); },
			[=](expression::Subscript const & subscript_node) 
			{
				return 
					is_constant_expression(*subscript_node.array, constant_base_index) && 
					is_constant_expression(*subscript_node.index, constant_base_index); 
			},
			[=](expression::If const & if_node) 
			{
				return
					is_constant_expression(*if_node.condition, constant_base_index) && 
					is_constant_expression(*if_node.then_case, constant_base_index) && 
					is_constant_expression(*if_node.else_case, constant_base_index);
			},
			[=](expression::StatementBlock const & block_node) 
			{
				return std::all_of(block_node.statements.begin(), block_node.statements.end(), 
					[=](Statement const & statement) {return can_be_run_in_a_constant_expression(statement, constant_base_index); });
			},
			[=](expression::Assignment const & assign_node) 
			{
				return 
					is_constant_expression(*assign_node.source, constant_base_index) &&
					is_constant_expression(*assign_node.destination, constant_base_index);
			}
		);
		return std::visit(visitor, expr.as_variant());
	}

} // namespace complete
