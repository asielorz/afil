#include "pretty_print.hh"
#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/overload.hh"
#include "utils/string.hh"
#include "utils/unreachable.hh"
#include <set>
#include "utils/warning_macro.hh"

using namespace complete;
using namespace std::literals;

using ScopeStack = std::vector<Scope const *>;
using ScopeStackView = span<Scope const * const>;

auto type_name(TypeId type_id, Program const & program) noexcept -> std::string;

auto decayed_type_name(TypeId type_id, Program const & program) noexcept -> std::string
{
	Type const & type = type_with_id(program, type_id);

	if (is_pointer(type))
		return join(type_name(pointee_type(type), program), " *");

	if (is_array(type))
		return join(type_name(array_value_type(type), program), '[', array_size(type), ']');

	if (is_array_pointer(type))
		return join(type_name(pointee_type(type), program), "[]");

	for (TypeName const & t : program.global_scope.types)
		if (t.id == type_id)
			return t.name;

	for (Function const & fn : program.functions)
		for (TypeName const & t : fn.types)
			if (t.id == type_id)
				return t.name;

	return "???"s;
}

auto type_name(TypeId type_id, Program const & program) noexcept -> std::string
{
	std::string name = decayed_type_name(decay(type_id), program);

	if (type_id.is_mutable)
		name += " mut";

	if (type_id.is_reference)
		name += " &";

	return name;
}

auto function_name(FunctionId fn_id, Program const & program) noexcept -> std::string_view
{
	if (fn_id == program.main_function)
		return "main"sv;

	for (FunctionName const & t : program.global_scope.functions)
		if (t.id == fn_id)
			return t.name;

	for (Function const & fn : program.functions)
		for (FunctionName const & t : fn.functions)
			if (t.id == fn_id)
				return t.name;

	return "???"sv;
}

auto variable_with_offset(ScopeStackView scope_stack, int offset) noexcept -> Variable const &
{
	for (auto it = scope_stack.rbegin(); it != scope_stack.rend(); ++it)
	{
		Scope const & scope = **it;
		for (Variable const & var : scope.variables)
			if (var.offset == offset)
				return var;
	}

	declare_unreachable();
}

auto function_template_name(FunctionTemplateId fn_id, Program const & program) noexcept -> std::string_view
{
	for (FunctionTemplateName const & t : program.global_scope.function_templates)
		if (t.id.index == fn_id.index)
			return t.name;

	for (Function const & fn : program.functions)
		for (FunctionTemplateName const & t : fn.function_templates)
			if (t.id.index == fn_id.index)
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

auto print_constant(TypeId type, void const * data) noexcept -> std::string
{
	if (type == complete::TypeId::int_)
		return join(*static_cast<int const *>(data));
	if (type == complete::TypeId::float_)
		return join(*static_cast<float const *>(data));
	if (type == complete::TypeId::bool_)
		return join(*static_cast<bool const *>(data));
	if (type == complete::TypeId::char_)
		return join(*static_cast<char const *>(data));
	else
		return "???";
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

auto pretty_print(Statement const & statement, Program const & program, ScopeStack & scope_stack, int indentation_level) noexcept -> std::string;

auto pretty_print(Expression const & expression, Program const & program, ScopeStack & scope_stack, int indentation_level) noexcept -> std::string
{
	auto const visitor = overload(
		[](expression::Literal<int> literal_expr) { return join("literal<int>: ", literal_expr.value, '\n'); },
		[](expression::Literal<float> literal_expr) { return join("literal<float>: ", literal_expr.value, '\n'); },
		[](expression::Literal<bool> literal_expr) { return join("literal<bool>: ", literal_expr.value, '\n'); },
		[](expression::Literal<uninit_t>) { return "literal<uninit_t>: uninit\n"s; },
		[&](expression::Literal<TypeId> literal_type) { return "literal<type>: ", type_name(literal_type.value, program), "\n"s; },
		[&](expression::StringLiteral literal_expr) 
		{
			return join("literal<", type_name(literal_expr.type, program),">: \"", literal_expr.value, "\"\n");
		},
		[&](expression::Variable const & var_expr) 
		{ 
			return join("variable<", type_name(var_expr.variable_type, program), ">: ", variable_with_offset(scope_stack, var_expr.variable_offset).name, '\n');
		},
		[&](expression::MemberVariable const & var_expr)
		{
			return join("member access<", type_name(var_expr.variable_type, program), ">\n",
				pretty_print(*var_expr.owner, program, scope_stack, indentation_level + 1),
				indent(indentation_level + 1), member_variable_name(expression_type_id(*var_expr.owner, program), var_expr.variable_offset, program), '\n'
			);
		},
		[&](expression::Constant const & constant_expr)
		{
			return join("constant<", type_name(constant_expr.type, program), ">: ", print_constant(constant_expr.type, constant_expr.value.data()), "\n");
		},
		[&](expression::FunctionCall const & func_call_expr)
		{
			std::string str = join("function call: ", function_name(func_call_expr.function_id, program), '\n');
			for (Expression const & param : func_call_expr.parameters)
				str += pretty_print(param, program, scope_stack, indentation_level + 1);

			return str;
		},
		[&](expression::RelationalOperatorCall const & op_call_expr)
		{
			std::string str = join("relational operator call: ", relational_operator_name(op_call_expr.op), '\n');
			for (Expression const & param : op_call_expr.parameters)
				str += pretty_print(param, program, scope_stack, indentation_level + 1);
			return str;
		},
		[&](expression::Constructor const & ctor_expr)
		{
			std::string str = join("constructor: ", type_name(ctor_expr.constructed_type, program), '\n');
			for (Expression const & param : ctor_expr.parameters)
				str += pretty_print(param, program, scope_stack, indentation_level + 1);
			return str;
		},
		[&](expression::Dereference const & deref_expr) 
		{
			return join("dereference\n",
				pretty_print(*deref_expr.expression, program, scope_stack, indentation_level + 1)
			);
		},
		[&](expression::ReinterpretCast const & reinterpret_cast_expr) 
		{
			return join("reinterpret cast<", type_name(reinterpret_cast_expr.return_type, program),">\n",
				pretty_print(*reinterpret_cast_expr.operand, program, scope_stack, indentation_level + 1)
			);
		},
		[&](expression::Subscript const & subscript_expr) 
		{
			return join("subscript\n",
				pretty_print(*subscript_expr.array, program, scope_stack, indentation_level + 1),
				pretty_print(*subscript_expr.index, program, scope_stack, indentation_level + 1)
			);
		},
		[&](expression::If const & if_expr) 
		{
			return join("if\n",
				pretty_print(*if_expr.condition, program, scope_stack, indentation_level + 1),
				pretty_print(*if_expr.then_case, program, scope_stack, indentation_level + 1),
				pretty_print(*if_expr.else_case, program, scope_stack, indentation_level + 1)
			);
		},
		[&](expression::StatementBlock const & block_expr) 
		{
			scope_stack.push_back(&block_expr.scope);

			std::string str = "statement block\n";
			for (Statement const & statement : block_expr.statements)
				str += pretty_print(statement, program, scope_stack, indentation_level + 1);

			scope_stack.pop_back();
			return str;
		},
		[&](expression::Assignment const & assignment_expr) 
		{ 
			return join("assignment\n",
				pretty_print(*assignment_expr.destination, program, scope_stack, indentation_level + 1),
				pretty_print(*assignment_expr.source, program, scope_stack, indentation_level + 1)
			);
		},
		[&](expression::Compiles const &)
		{
			return join("compiles\n");
		}
	);

	return indent(indentation_level) + std::visit(visitor, expression.as_variant());
}

auto pretty_print(Statement const & statement, Program const & program, ScopeStack & scope_stack, int indentation_level) noexcept -> std::string
{
	auto const visitor = overload(
		[&](statement::VariableDeclaration const & var_decl_stmt)
		{
			Variable const & var = variable_with_offset(scope_stack, var_decl_stmt.variable_offset);

			return join("variable decalaration<", type_name(var.type, program), ">: ", var.name, "\n",
				pretty_print(var_decl_stmt.assigned_expression, program, scope_stack, indentation_level + 1)
			);
		},
		[&](statement::ExpressionStatement const & expr_stmt)
		{
			return join("expression\n",
				pretty_print(expr_stmt.expression, program, scope_stack, indentation_level + 1)
			);
		},
		[&](statement::Return const & return_stmt)
		{
			return join("return\n",
				pretty_print(return_stmt.returned_expression, program, scope_stack, indentation_level + 1)
			);
		},
		[&](statement::If const & if_stmt)
		{
			return join("if\n",
				pretty_print(if_stmt.condition, program, scope_stack, indentation_level + 1),
				pretty_print(*if_stmt.then_case, program, scope_stack, indentation_level + 1),
				if_stmt.else_case ? pretty_print(*if_stmt.else_case, program, scope_stack, indentation_level + 1) : ""
			);
		},
		[&](statement::StatementBlock const & block_stmt)
		{
			scope_stack.push_back(&block_stmt.scope);

			std::string str = "statement block\n";
			for (Statement const & statement : block_stmt.statements)
				str += pretty_print(statement, program, scope_stack, indentation_level + 1);

			scope_stack.pop_back();
			return str;
		},
		[&](statement::While const & while_stmt)
		{
			return join("while\n",
				pretty_print(while_stmt.condition, program, scope_stack, indentation_level + 1),
				pretty_print(*while_stmt.body, program, scope_stack, indentation_level + 1)
			);
		},
		[&](statement::For const & for_stmt)
		{
			scope_stack.push_back(&for_stmt.scope);

			std::string str = join("for\n",
				pretty_print(*for_stmt.init_statement, program, scope_stack, indentation_level + 1),
				pretty_print(for_stmt.condition, program, scope_stack, indentation_level + 1),
				pretty_print(for_stmt.end_expression, program, scope_stack, indentation_level + 1),
				pretty_print(*for_stmt.body, program, scope_stack, indentation_level + 1)
			);

			scope_stack.pop_back();
			return str;
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

	ScopeStack scope_stack;
	scope_stack.push_back(&program.global_scope);

	if (!program.global_initialization_statements.empty())
	{
		str += "global initialization statements\n";
		for (Statement const & statement : program.global_initialization_statements)
			str += pretty_print(statement, program, scope_stack, 1);
		str += '\n';
	}
	
	for (Function const & fn : program.functions)
		str += pretty_print(fn, program);

	return str;
}

auto pretty_print(complete::Function const & function, complete::Program const & program) noexcept -> std::string
{
	ScopeStack scope_stack;
	scope_stack.push_back(&program.global_scope);
	scope_stack.push_back(&function);

	std::string str = std::string(function_name(FunctionId(FunctionId::Type::program, static_cast<unsigned>(&function - program.functions.data())), program));
	str += '(';
	for (int i = 0; i < function.parameter_count; ++i)
		str += join(type_name(function.variables[i].type, program), ' ', function.variables[i].name, (i == function.parameter_count - 1) ? "" : ", ");
	str += ") -> ";
	str += type_name(function.return_type, program);
	str += '\n';
	for (Statement const & statement : function.statements)
		str += pretty_print(statement, program, scope_stack, 1);
	str += '\n';
	return str;
}
