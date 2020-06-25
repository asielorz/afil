#pragma once

namespace complete
{

	struct Expression;
	struct Statement;
	struct Program;
	struct Function;

	auto can_be_run_in_a_constant_expression(Statement const & stmt, Program const & program, int constant_base_index) noexcept -> bool;
	auto is_constant_expression(Expression const & expr, Program const & program, int constant_base_index) noexcept -> bool;
	auto is_constant_expression_only(Expression const & expr, Program const & program, int constant_base_index) noexcept -> bool;
	auto can_be_run_in_a_constant_expression(Function const & function, Program const & program) noexcept -> bool;

	auto can_be_run_at_runtime(Statement const & stmt, Program const & program) noexcept -> bool;
	auto can_be_run_at_runtime(Expression const & expr, Program const & program) noexcept -> bool;
	auto can_be_run_at_runtime(Function const & function, Program const & program) noexcept -> bool;

} // namespace complete
