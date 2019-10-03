#include "pretty_print.hh"
#include "statement.hh"
#include "program.hh"
#include "string.hh"
#include "overload.hh"

using expr::ExpressionTree;
using stmt::Statement;

auto to_string(TypeId id, Program const & program) noexcept -> std::string
{
	Type const & type = type_with_id(program, id);
	auto type_name = std::string(type.name);
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

auto pretty_print_function_node(FunctionId function_id, Program const & program) noexcept -> std::string
{
	auto const found_id = std::find_if(program.global_scope.functions.begin(), program.global_scope.functions.end(),
		[function_id](FunctionName const & fn_name) { return fn_name.id == function_id; });
	
	std::string str;

	if (found_id == program.global_scope.functions.end())
		str = "function(";
	else
		str = std::string(found_id->name) + '(';

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
		[&](LocalVariableNode const & var_node) 
		{
			return join(indent(indentation_level), "local<", to_string(var_node.variable_type, program), ">: ", var_node.variable_offset, '\n');
		},
		[&](GlobalVariableNode const & var_node) 
		{
			auto const found_id = std::find_if(program.global_scope.variables.begin(), program.global_scope.variables.end(),
				[var_node](Variable const & var) { return var.offset == var_node.variable_offset; });

			return join(indent(indentation_level), "global<", to_string(var_node.variable_type, program), ">: ", found_id->name, '\n');
		},
		[&](FunctionNode const & func_node) 
		{ 
			return join(indent(indentation_level), pretty_print_function_node(func_node.function_id, program));
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
