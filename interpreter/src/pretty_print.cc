#include "pretty_print.hh"
#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/overload.hh"
#include "utils/string.hh"
#include "utils/unreachable.hh"
#include <set>

using namespace complete;
using namespace std::literals;

// TODO: Search all scopes

auto type_name(TypeId type, Program const & program) noexcept -> std::string_view
{
	for (TypeName const & t : program.global_scope.types)
		if (t.id == type)
			return t.name;

	// TODO: Pointers, references, mutable, arrays...

	return "???"sv;
}

auto function_name(FunctionId fn, Program const & program) noexcept -> std::string_view
{
	for (FunctionName const & t : program.global_scope.functions)
		if (t.id == fn)
			return t.name;

	if (fn == program.main_function)
		return "main"sv;

	return "???"sv;
}

auto function_template_name(FunctionTemplateId fn, Program const & program) noexcept -> std::string_view
{
	for (FunctionTemplateName const & t : program.global_scope.function_templates)
		if (t.id.index == fn.index)
			return t.name;

	return "???"sv;
}

auto member_variable_name(TypeId owner_type, int variable_offset, Program const & program) noexcept -> std::string_view
{
	Struct const & struct_data = *struct_for_type(program, owner_type);
	for (MemberVariable const & var : struct_data.member_variables)
		if (var.offset == variable_offset)
			return var.name;

	return "???"sv;
}

auto indent(int indentation_level) noexcept -> std::string
{
	return std::string(indentation_level, '\t');
}

auto relational_operator_name(Operator op) noexcept -> std::string_view
{
	switch (op)
	{
		case Operator::less:			return "<";
		case Operator::less_equal:		return "<=";
		case Operator::greater:			return ">";
		case Operator::greater_equal:	return ">=";
		case Operator::not_equal:		return "!=";
		default:						declare_unreachable();
	}
}

auto pretty_print(Expression const & expression, Program const & program, int indentation_level) noexcept -> std::string
{

	auto const visitor = overload(
		[](expression::Literal<int> literal_expr) { return join("literal<int>: ", literal_expr.value, '\n'); },
		[](expression::Literal<float> literal_expr) { return join("literal<float>: ", literal_expr.value, '\n'); },
		[](expression::Literal<bool> literal_expr) { return join("literal<bool>: ", literal_expr.value, '\n'); },
		[&](expression::Variable const & var_expr) 
		{ 
			return join("variable<", type_name(var_expr.variable_type, program), ">: ", var_expr.variable_offset, '\n');
		},
		[&](expression::MemberVariable const & var_expr)
		{
			return join("member access\n",
				pretty_print(*var_expr.owner, program, indentation_level + 1),
				indent(indentation_level + 1), member_variable_name(expression_type_id(*var_expr.owner, program), var_expr.variable_offset, program), '\n'
			);
		},
		[&](expression::OverloadSet const & overload_set_expr) 
		{
			std::set<std::string_view> function_names;
			for (FunctionId const id : overload_set_expr.overload_set.function_ids)
				function_names.insert(function_name(id, program));
			for (FunctionTemplateId const id : overload_set_expr.overload_set.function_template_ids)
				function_names.insert(function_template_name(id, program));

			if (function_names.size() == 1)
				return join("overload set: ", *function_names.begin(), '\n');
			else
			{
				std::string str = "overload set\n";
				for (std::string_view const name : function_names)
					str += join(indent(indentation_level + 1), name, '\n');
				return str;
			}
		},
		[&](expression::FunctionCall const & func_call_expr)
		{
			std::string str = join("function call: ", function_name(func_call_expr.function_id, program), '\n');
			for (Expression const & param : func_call_expr.parameters)
				str += pretty_print(param, program, indentation_level + 1);
			return str;
		},
		[&](expression::RelationalOperatorCall const & op_call_expr)
		{
			std::string str = join("relational operator call: ", relational_operator_name(op_call_expr.op), '\n');
			for (Expression const & param : op_call_expr.parameters)
				str += pretty_print(param, program, indentation_level + 1);
			return str;
		},
		[&](expression::Constructor const & ctor_expr)
		{
			std::string str = join("constructor: ", type_name(ctor_expr.constructed_type, program), '\n');
			for (Expression const & param : ctor_expr.parameters)
				str += pretty_print(param, program, indentation_level + 1);
			return str;
		},
		[&](expression::Dereference const & deref_expr) 
		{
			return join("dereference\n",
				pretty_print(*deref_expr.expression, program, indentation_level + 1)
			);
		},
		[&](expression::ReinterpretCast const & reinterpret_cast_expr) 
		{
			return join("reinterpret cast<", type_name(reinterpret_cast_expr.return_type, program),">\n",
				pretty_print(*reinterpret_cast_expr.operand, program, indentation_level + 1)
			);
		},
		[&](expression::Subscript const & subscript_expr) 
		{
			return join("subscript\n",
				pretty_print(*subscript_expr.array, program, indentation_level + 1),
				pretty_print(*subscript_expr.index, program, indentation_level + 1)
			);
		},
		[&](expression::If const & if_expr) 
		{
			return join("if\n",
				pretty_print(*if_expr.condition, program, indentation_level + 1),
				pretty_print(*if_expr.then_case, program, indentation_level + 1),
				pretty_print(*if_expr.else_case, program, indentation_level + 1)
			);
		},
		[&](expression::StatementBlock const & block_expr) 
		{
			std::string str = "statement block\n";
			for (Statement const & statement : block_expr.statements)
				str += pretty_print(statement, program, indentation_level + 1);
			return str;
		},
		[&](expression::Assignment const & assignment_expr) 
		{ 
			return join("assignment\n",
				pretty_print(*assignment_expr.destination, program, indentation_level + 1),
				pretty_print(*assignment_expr.source, program, indentation_level + 1)
			);
		}
	);

	return indent(indentation_level) + std::visit(visitor, expression.as_variant());
}

auto pretty_print(Statement const & statement, Program const & program, int indentation_level) noexcept -> std::string
{
	auto const visitor = overload(
		[&](statement::VariableDeclaration const & var_decl_stmt)
		{
			return join("variable decalaration: ", var_decl_stmt.variable_offset, "\n",
				pretty_print(var_decl_stmt.assigned_expression, program, indentation_level + 1)
			);
		},
		[&](statement::ExpressionStatement const & expr_stmt)
		{
			return join("expression\n",
				pretty_print(expr_stmt.expression, program, indentation_level + 1)
			);
		},
		[&](statement::Return const & return_stmt)
		{
			return join("return\n",
				pretty_print(return_stmt.returned_expression, program, indentation_level + 1)
			);
		},
		[&](statement::If const & if_stmt)
		{
			return join("if\n",
				pretty_print(if_stmt.condition, program, indentation_level + 1),
				pretty_print(*if_stmt.then_case, program, indentation_level + 1),
				if_stmt.else_case ? pretty_print(*if_stmt.else_case, program, indentation_level + 1) : ""
			);
		},
		[&](statement::StatementBlock const & block_stmt)
		{
			std::string str = "statement block\n";
			for (Statement const & statement : block_stmt.statements)
				str += pretty_print(statement, program, indentation_level + 1);
			return str;
		},
		[&](statement::While const & while_stmt)
		{
			return join("while\n",
				pretty_print(while_stmt.condition, program, indentation_level + 1),
				pretty_print(*while_stmt.body, program, indentation_level + 1)
			);
		},
		[&](statement::For const & for_stmt)
		{
			return join("for\n",
				pretty_print(*for_stmt.init_statement, program, indentation_level + 1),
				pretty_print(for_stmt.condition, program, indentation_level + 1),
				pretty_print(for_stmt.end_expression, program, indentation_level + 1),
				pretty_print(*for_stmt.body, program, indentation_level + 1)
			);
		},
		[&](statement::Break const &)
		{
			return std::string("break\n");
		},
		[&](statement::Continue const &)
		{
			return std::string("continue\n");
		}
	);

	return indent(indentation_level) + std::visit(visitor, statement.as_variant());

}

auto pretty_print(Program const & program) noexcept -> std::string
{
	std::string str;

	if (!program.global_initialization_statements.empty())
	{
		str += "global initialization statements\n";
		for (Statement const & statement : program.global_initialization_statements)
			str += pretty_print(statement, program, 1);
		str += '\n';
	}
	for (Function const & fn : program.functions)
	{
		str += function_name(FunctionId{false, static_cast<unsigned>(&fn - program.functions.data())}, program);
		str += '(';
		for (int i = 0; i < fn.parameter_count; ++i)
			str += join(type_name(fn.variables[i].type, program), ' ', fn.variables[i].name, (i == fn.parameter_count - 1) ? "" : ", ");
		str += ") -> ";
		str += type_name(fn.return_type, program);
		str += '\n';
		for (Statement const & statement : fn.statements)
			str += pretty_print(statement, program, 1);
		str += '\n';
	}

	return str;
}
