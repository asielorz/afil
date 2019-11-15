#include "pretty_print.hh"
#include "statement.hh"
#include "program.hh"
#include "string.hh"
#include "overload.hh"
#include "variant.hh"

using expr::ExpressionTree;
using stmt::Statement;

auto to_string(TypeId id, Program const & program) noexcept -> std::string
{
	if (id.is_language_reseved)
		return "reserved!!! fix this";

	TypeId const decayed_id = decay(id);

	std::string type_name;
	if (is_pointer(type_with_id(program, decayed_id)))
	{
		type_name = to_string(std::get<PointerType>(type_with_id(program, decayed_id).extra_data).value_type, program);
		type_name += " *";
	}
	else
	{
		// TODO: Maybe a more exhaustive search? Same for functions.
		auto const found_type = std::find_if(program.global_scope.types.begin(), program.global_scope.types.end(),
			[decayed_id](TypeName const & type_name) { return type_name.id == decayed_id; });

		if (found_type == program.global_scope.types.end())
			type_name = "<type>";
		else
			type_name = std::string(get(program, found_type->name));
	}

	if (id.is_mutable)
		type_name += " mut";
	if (id.is_reference)
		type_name += " &";
	return type_name;
}

auto indent(int indentation_level) noexcept -> std::string
{
	return std::string(indentation_level, '\t');
}

namespace lex { auto is_operator(std::string_view src, int index) noexcept -> bool; }

auto pretty_print_function_node(FunctionId function_id, Program const & program) noexcept -> std::string
{
	auto const found_fn = std::find_if(program.global_scope.functions.begin(), program.global_scope.functions.end(),
		[function_id](FunctionName const & fn_name) { return fn_name.id == function_id; });
	
	std::string str;

	if (function_id == program.main_function)
		str = "main(";
	else if (found_fn == program.global_scope.functions.end())
		str = "<function>(";
	else
	{
		std::string_view const function_name = get(program, found_fn->name);
		
		if (lex::is_operator(function_name, 0))
			str += "operator ";

		str += std::string(function_name) + '(';
	}

	auto const param_types = parameter_types(program, function_id);
	for (TypeId const & id : param_types)
		str += to_string(id, program) + (&id == &param_types.back() ? "" : ", ");
	str += ") -> ";
	str += to_string(return_type(program, function_id), program) + '\n';
	return str;
}

auto pretty_print_rec(stmt::Statement const & tree, Program const & program, int indentation_level) noexcept -> std::string;

auto pretty_print_rec(ExpressionTree const & tree, Program const & program, int indentation_level) noexcept -> std::string
{
	using namespace expr;

	auto const visitor = overload(
		[&](Literal<int> literal) { return join(indent(indentation_level), "literal<int>: ", literal.value, '\n'); },
		[&](Literal<float> literal) { return join(indent(indentation_level), "literal<float>: ", literal.value, '\n'); },
		[&](Literal<bool> literal) { return join(indent(indentation_level), "literal<bool>: ", literal.value, '\n'); },
		[&](DereferenceNode const & deref_node) 
		{
			return join(
				indent(indentation_level), "dereference<", to_string(deref_node.variable_type, program), ">\n",
				pretty_print_rec(*deref_node.expression, program, indentation_level + 1));
		},
		[&](AddressofNode const & addressof_node)
		{
			return join(
				indent(indentation_level), "addressof<", to_string(addressof_node.return_type, program), ">\n",
				pretty_print_rec(*addressof_node.operand, program, indentation_level + 1)
			);
		},
		[&](DepointerNode const & deptr_node)
		{
			return join(
				indent(indentation_level), "depointer<", to_string(deptr_node.return_type, program), ">\n",
				pretty_print_rec(*deptr_node.operand, program, indentation_level + 1)
			);
		},
		[&](LocalVariableNode const & var_node) 
		{
			return join(indent(indentation_level), "local<", to_string(var_node.variable_type, program), ">: ", var_node.variable_offset, '\n');
		},
		[&](GlobalVariableNode const & var_node) 
		{
			auto const found_var = std::find_if(program.global_scope.variables.begin(), program.global_scope.variables.end(),
				[var_node](Variable const & var) { return var.offset == var_node.variable_offset; });

			return join(indent(indentation_level), "global<", to_string(var_node.variable_type, program), ">: ", get(program, found_var->name), '\n');
		},
		[&](MemberVariableNode const & var_node)
		{
			Struct const & s = *struct_for_type(program, expression_type_id(*var_node.owner, program));
			auto const found_var = std::find_if(s.member_variables.begin(), s.member_variables.end(), 
				[&](Variable const & var) { return var.offset == var_node.variable_offset; });

			return join(
				indent(indentation_level), "member<", to_string(decay(var_node.variable_type), program), ">: ", get(program, found_var->name), '\n',
				pretty_print_rec(*var_node.owner, program, indentation_level + 1));
		},
		[&](FunctionNode const & func_node) 
		{ 
			return join(indent(indentation_level), pretty_print_function_node(func_node.function_id, program));
		},
		[&](FunctionTemplateNode const & func_template_node)
		{
			return join(indent(indentation_level), "function template ", func_template_node.function_template_id.index);
		},
		[&](FunctionCallNode const & func_call_node)
		{
			std::string str = join(indent(indentation_level), pretty_print_function_node(func_call_node.function_id, program));
			
			for (ExpressionTree const & param : func_call_node.parameters)
				str += pretty_print_rec(param, program, indentation_level + 1);

			return str;
		},
		[&](RelationalOperatorCallNode const & rel_op_node) 
		{
			std::string str = indent(indentation_level);
			switch (rel_op_node.op)
			{
				case Operator::less:			str += "operator < ";  break;
				case Operator::less_equal:		str += "operator <= "; break;
				case Operator::greater:			str += "operator > ";  break;
				case Operator::greater_equal:	str += "operator >= "; break;
				case Operator::not_equal:		str += "operator != "; break;
			}
			str += '\n';
			str += pretty_print_rec((*rel_op_node.parameters)[0], program, indentation_level + 1);
			str += pretty_print_rec((*rel_op_node.parameters)[1], program, indentation_level + 1);
			return str;
		},
		[&](AssignmentNode const & assign_node)
		{
			return join(
				indent(indentation_level), "assignment\n",
				pretty_print_rec(*assign_node.destination, program, indentation_level + 1),
				pretty_print_rec(*assign_node.source, program, indentation_level + 1));
		},
		[&](IfNode const & if_node)
		{
			return join(
				indent(indentation_level), "if expression\n",
				pretty_print_rec(*if_node.condition, program, indentation_level + 1),
				pretty_print_rec(*if_node.then_case, program, indentation_level + 1),
				pretty_print_rec(*if_node.else_case, program, indentation_level + 1)
			);
		},
		[&](StatementBlockNode const & block_node)
		{
			std::string str = join(indent(indentation_level), "statement block expression<", to_string(block_node.return_type, program), ">\n");
			for (stmt::Statement const & statement : block_node.statements)
				str += pretty_print_rec(statement, program, indentation_level + 1);
			return str;
		},
		[&](StructConstructorNode const & ctor_node)
		{
			std::string str = join(indent(indentation_level), to_string(ctor_node.constructed_type, program) ,'\n');
			for (ExpressionTree const & param : ctor_node.parameters)
				str += pretty_print_rec(param, program, indentation_level + 1);
			return str;
		},
		[&](tmp::LocalVariableNode const &)
		{
			return join(indent(indentation_level), "dependent local", '\n');
		},
		[&](tmp::MemberVariableNode const & var_node)
		{
			return join(indent(indentation_level), '.', get(program, var_node.name), '\n',
				pretty_print_rec(*var_node.owner, program, indentation_level + 1));
		},
		[&](tmp::FunctionCallNode const & func_call_node)
		{
			std::string str = join(indent(indentation_level), "dependent function call\n");

			for (ExpressionTree const & param : func_call_node.parameters)
				str += pretty_print_rec(param, program, indentation_level + 1);

			return str;
		},
		[&](tmp::RelationalOperatorCallNode const & rel_op_node)
		{
			std::string str = indent(indentation_level);
			str += "dependent ";
			switch (rel_op_node.op)
			{
				case Operator::less:			str += "operator < ";  break;
				case Operator::less_equal:		str += "operator <= "; break;
				case Operator::greater:			str += "operator > ";  break;
				case Operator::greater_equal:	str += "operator >= "; break;
				case Operator::not_equal:		str += "operator != "; break;
			}
			str += '\n';
			str += pretty_print_rec((*rel_op_node.parameters)[0], program, indentation_level + 1);
			str += pretty_print_rec((*rel_op_node.parameters)[1], program, indentation_level + 1);
			return str;
		},
		[&](tmp::StructConstructorNode const & ctor_node)
		{
			std::string str = join(indent(indentation_level), "dependent constructor", '\n');
			for (ExpressionTree const & param : ctor_node.parameters)
				str += pretty_print_rec(param, program, indentation_level + 1);
			return str;
		}
	);
	return std::visit(visitor, tree.as_variant());
}

auto pretty_print_rec(Statement const & statement, Program const & program, int indentation_level) noexcept -> std::string
{
	using namespace stmt;

	auto const visitor = overload(
		[&](VariableDeclarationStatement const & node)
		{
			return
				join(indent(indentation_level), "variable declaration: ", node.variable_offset, '\n',
				pretty_print_rec(node.assigned_expression, program, indentation_level + 1));
		},
		[&](ExpressionStatement const & expr_node)
		{
			return pretty_print_rec(expr_node.expression, program, indentation_level);
		},
		[&](ReturnStatement const & return_node)
		{
			return
				join(indent(indentation_level), "return\n",
				pretty_print_rec(return_node.returned_expression, program, indentation_level + 1));
		},
		[&](IfStatement const & if_node)
		{
			return join(
				indent(indentation_level), "if statement\n",
				pretty_print_rec(if_node.condition, program, indentation_level + 1),
				pretty_print_rec(*if_node.then_case, program, indentation_level + 1),
				pretty_print_rec(*if_node.else_case, program, indentation_level + 1)
			);
		},
		[&](StatementBlock const & block_node)
		{
			std::string str = join(indent(indentation_level), "statement block\n");
			for (Statement const & statement : block_node.statements)
				str += pretty_print_rec(statement, program, indentation_level + 1);
			return str;
		},
		[&](WhileStatement const & while_node)
		{
			return join(indent(indentation_level), "while\n",
				pretty_print_rec(while_node.condition, program, indentation_level + 1),
				pretty_print_rec(*while_node.body, program, indentation_level + 1)
			);
		},
		[&](ForStatement const & for_node)
		{
			return join(indent(indentation_level), "for\n",
				pretty_print_rec(*for_node.init_statement, program, indentation_level + 1),
				pretty_print_rec(for_node.condition, program, indentation_level + 1),
				pretty_print_rec(for_node.end_expression, program, indentation_level + 1),
				pretty_print_rec(*for_node.body, program, indentation_level + 1)
			);
		},
		[&](BreakStatement const &)
		{
			return join(indent(indentation_level), "break\n");
		},
		[&](ContinueStatement const &)
		{
			return join(indent(indentation_level), "continue\n");
		},
		[&](tmp::VariableDeclarationStatement const & var_node)
		{
			return
				join(indent(indentation_level), "dependent variable declaration: ", get(program, var_node.variable_name), '\n',
					var_node.assigned_expression.has_value() ? pretty_print_rec(*var_node.assigned_expression, program, indentation_level + 1) : "");
		}
	);
	return std::visit(visitor, statement.as_variant());
}

auto pretty_print(ExpressionTree const & tree, Program const & program) noexcept -> std::string
{
	return pretty_print_rec(tree, program, 0);
}

auto pretty_print(Statement const & statement, Program const & program) noexcept -> std::string
{
	return pretty_print_rec(statement, program, 0);
}

auto pretty_print(Function const & function, Program const & program) noexcept -> std::string
{
	FunctionId id;
	id.is_extern = false;
	id.index = &function - program.functions.data();
	std::string str = pretty_print_function_node(id, program);

	for (Statement const & statement : function.statements)
		str += pretty_print_rec(statement, program, 1);

	return str;
}

auto pretty_print(Program const & program) noexcept -> std::string
{
	std::string str;
	for (Function const & fn : program.functions)
		str += pretty_print(fn, program);
	return str;
}
