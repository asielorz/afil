#include "constexpr.hh"
#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/algorithm.hh"
#include "utils/overload.hh"

namespace complete
{

	auto can_be_run_in_a_constant_expression(Statement const & stmt, Program const & program, int constant_base_index) noexcept -> bool
	{
		auto const visitor = overload(
			[&](statement::VariableDeclaration const & var_node) { return is_constant_expression(var_node.assigned_expression, program, constant_base_index); },
			[&](statement::ExpressionStatement const & expr_node) { return is_constant_expression(expr_node.expression, program, constant_base_index); },
			[&](statement::Return const & return_node) { return is_constant_expression(return_node.returned_expression, program, constant_base_index); },
			[&](statement::If const & if_node)
			{
				return
					is_constant_expression(if_node.condition, program, constant_base_index) &&
					can_be_run_in_a_constant_expression(*if_node.then_case, program, constant_base_index) &&
					(if_node.else_case == nullptr || can_be_run_in_a_constant_expression(*if_node.else_case, program, constant_base_index));
			},
			[&](statement::StatementBlock const & block_node)
			{
				return std::all_of(block_node.statements,
					[&](Statement const & statement) {return can_be_run_in_a_constant_expression(statement, program, constant_base_index); });
			},
			[&](statement::While const & while_node)
			{
				return
					is_constant_expression(while_node.condition, program, constant_base_index) &&
					can_be_run_in_a_constant_expression(*while_node.body, program, constant_base_index);
			},
			[&](statement::For const & for_node)
			{
				return
					can_be_run_in_a_constant_expression(*for_node.init_statement, program, constant_base_index) &&
					is_constant_expression(for_node.condition, program, constant_base_index) &&
					is_constant_expression(for_node.end_expression, program, constant_base_index) &&
					can_be_run_in_a_constant_expression(*for_node.body, program, constant_base_index);
			},
			[](statement::Break) { return true; },
			[](statement::Continue) { return true; }
		);
		return std::visit(visitor, stmt.as_variant());
	}

	auto is_constant_expression(Expression const & expr, Program const & program, int constant_base_index) noexcept -> bool
	{
		auto const visitor = overload(
			[](expression::Literal<int>) { return true; },
			[](expression::Literal<float>) { return true; },
			[](expression::Literal<bool>) { return true; },
			[](expression::Literal<uninit_t>) { return true; },
			[](expression::Literal<TypeId>) { return true; },
			[](expression::StringLiteral const &) { return true; },
			[&](expression::LocalVariable const & var_node) { return var_node.variable_offset >= constant_base_index; },
			[](expression::GlobalVariable const &) { return false; },
			[&](expression::MemberVariable const & var_node) { return is_constant_expression(*var_node.owner, program, constant_base_index); },
			[](expression::Constant const &) { return true; },
			//[](expression::OverloadSet const &) { return true; },
			[&](expression::FunctionCall const & func_call_node)
			{
				return
					is_callable_at_compile_time(program, func_call_node.function_id) &&
					std::all_of(func_call_node.parameters,
						[&](Expression const & param) { return is_constant_expression(param, program, constant_base_index); });
			},
			[&](expression::RelationalOperatorCall const & op_call_node)
			{
				return std::all_of(op_call_node.parameters,
					[&](Expression const & param) { return is_constant_expression(param, program, constant_base_index); });
			},
			[&](expression::Constructor const & ctor_node)
			{
				return std::all_of(ctor_node.parameters,
					[&](Expression const & param) { return is_constant_expression(param, program, constant_base_index); });
			},
				[&](expression::Dereference const & deref_node) { return is_constant_expression(*deref_node.expression, program, constant_base_index); },
				[&](expression::ReinterpretCast const & cast_node) { return is_constant_expression(*cast_node.operand, program, constant_base_index); },
				[&](expression::Subscript const & subscript_node)
			{
				return
					is_constant_expression(*subscript_node.array, program, constant_base_index) &&
					is_constant_expression(*subscript_node.index, program, constant_base_index);
			},
			[&](expression::If const & if_node)
			{
				return
					is_constant_expression(*if_node.condition, program, constant_base_index) &&
					is_constant_expression(*if_node.then_case, program, constant_base_index) &&
					is_constant_expression(*if_node.else_case, program, constant_base_index);
			},
			[&](expression::StatementBlock const & block_node)
			{
				return std::all_of(block_node.statements,
					[&](Statement const & statement) {return can_be_run_in_a_constant_expression(statement, program, constant_base_index); });
			},
			[&](expression::Assignment const & assign_node)
			{
				return
					is_constant_expression(*assign_node.source, program, constant_base_index) &&
					is_constant_expression(*assign_node.destination, program, constant_base_index);
			}
		);
		return std::visit(visitor, expr.as_variant());
	}

	auto can_be_run_in_a_constant_expression(Function const & function, Program const & program) noexcept -> bool
	{
		return 
			std::all_of(function.preconditions, [&](Expression const & expr) { return is_constant_expression(expr, program, 0); }) &&
			std::all_of(function.statements, [&](Statement const & stmt) { return can_be_run_in_a_constant_expression(stmt, program, 0); });
	}

} // namespace complete
