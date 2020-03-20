#include "c_transpiler.hh"
#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/string.hh"
#include "utils/out.hh"
#include "utils/overload.hh"
#include "utils/unreachable.hh"
#include "utils/variant.hh"
#include "utils/warning_macro.hh"

using namespace std::literals;

namespace c_transpiler
{

	constexpr std::string_view built_in_operator_table[] = {
		"+"sv,
		"-"sv,
		"*"sv,
		"/"sv,
		"%"sv,
		"=="sv,
		"-"sv,
		"-"sv,
		"&"sv,
		"|"sv,
		"^"sv,
		"~"sv,
		">>"sv,
		"<<"sv,

		"+"sv,
		"-"sv,
		"*"sv,
		"/"sv,
		"=="sv,
		"-"sv,
		"-"sv,

		"+"sv,
		"-"sv,
		"*"sv,
		"/"sv,
		"=="sv,
		"-"sv,

		"&&"sv,
		"||"sv,
		"^"sv,
		"!"sv,
		"=="sv
	};

	auto write_constant(complete::TypeId type, span<char const> bytes, complete::Program const & program) noexcept -> std::string
	{
		static_cast<void>(program);

		char const * memory = bytes.data();
		if (type.is_reference)
		{
			memory = *reinterpret_cast<char const * const *>(memory);
		}
		type = decay(type);

		if (type == complete::TypeId::int_)
			return to_string(*reinterpret_cast<int const *>(memory));
		if (type == complete::TypeId::float_)
			return join(*reinterpret_cast<float const *>(memory), 'f');
		if (type == complete::TypeId::bool_)
			return *reinterpret_cast<bool const *>(memory) ? "1" : "0";
		if (type == complete::TypeId::char_)
			return to_string(*memory);

		mark_as_to_do("Conversion of this type to string is not implemented yet.");
	}

	auto function_name(FunctionId function_id, complete::Program const & program) noexcept -> std::string
	{
		static_cast<void>(program);
		TODO("Transpiling calls to extern functions");

		if (function_id == program.main_function)
			return "main";

		if (function_id.is_extern)
			return join("extern_function_", function_id.index);
		else
			return join("function_", function_id.index);
	}

	auto type_name(complete::TypeId type_id, complete::Program const & program) noexcept -> std::string
	{
		if (type_id == complete::TypeId::int_) return "int";
		if (type_id == complete::TypeId::float_) return "float";
		if (type_id == complete::TypeId::bool_) return "int";
		if (type_id == complete::TypeId::char_) return "char";

		complete::Type const type = type_with_id(program, type_id);
		if (is_pointer(type))
			return join(type_name(pointee_type(type), program), " *");

		return join("type_", decay(type_id).flat_value);
	}

	auto transpile_expression(complete::Expression const & expr, complete::Program const & program, std::string & c_source) noexcept -> void;

	auto write_function_call(FunctionId function_id, span<complete::Expression const> parameters, complete::Program const & program, std::string & c_source)
	{
		if (function_id.is_extern && function_id.index < std::size(built_in_operator_table))
		{
			c_source += '(';
			if (parameters.size() == 1)
			{
				c_source += join(' ', built_in_operator_table[function_id.index], ' ');
				transpile_expression(parameters[0], program, c_source);
			}
			else
			{
				assert(parameters.size() == 2);
				transpile_expression(parameters[0], program, c_source);
				c_source += join(' ', built_in_operator_table[function_id.index], ' ');
				transpile_expression(parameters[1], program, c_source);
			}
			c_source += ')';
		}
		else
		{
			c_source += function_name(function_id, program);
			c_source += '(';
			size_t const param_count = parameters.size();
			if (param_count > 0)
			{
				for (size_t i = 0; i < param_count - 1; ++i)
				{
					transpile_expression(parameters[i], program, c_source);
					c_source += ", ";
				}
				transpile_expression(parameters.back(), program, c_source);
			}
			c_source += ')';
		}
	}

	auto transpile_expression(complete::Expression const & expr, complete::Program const & program, std::string & c_source) noexcept -> void
	{
		using namespace complete;

		auto const visitor = overload(
			[&](expression::Literal<int> literal) { c_source += to_string(literal.value); },
			[&](expression::Literal<float> literal) { c_source += join(literal.value, 'f'); },
			[&](expression::Literal<bool> literal) { c_source += literal.value ? "1" : "0"; },
			[&](expression::Literal<uninit_t>) {},
			[&](expression::StringLiteral literal) { c_source += join('"', literal.value, '"'); },
			[&](expression::LocalVariable const & var_node)
			{
				c_source += join("&local_", var_node.variable_offset);
			},
			[&](expression::GlobalVariable const & var_node)
			{
				c_source += join("&global_", var_node.variable_offset);
			},
			[&](expression::MemberVariable const & var_node)
			{
				transpile_expression(*var_node.owner, program, c_source);
				c_source += '.';
				c_source += join("member_", var_node.variable_offset);
			},
			[&](expression::Constant const & constant_node)
			{
				c_source += write_constant(constant_node.type, constant_node.value, program);
			},
			[&](expression::Dereference const & deref_node)
			{
				if (has_type<expression::Constant>(*deref_node.expression))
				{
					transpile_expression(*deref_node.expression, program, c_source);
				}
				else
				{
					std::string expr_source;
					transpile_expression(*deref_node.expression, program, expr_source);

					// Cancel dereference addressof
					if (expr_source[0] == '&')
					{
						c_source += expr_source.substr(1);
					}
					else
					{
						c_source += '*';
						c_source += expr_source;
					}
				}
			},
			[&](expression::ReinterpretCast const & addressof_node)
			{
				transpile_expression(*addressof_node.operand, program, c_source);
			},
			[&](expression::Subscript const & subscript_node)
			{
				transpile_expression(*subscript_node.array, program, c_source);
				c_source += '[';
				transpile_expression(*subscript_node.index, program, c_source);
				c_source += ']';
			},
			[&](expression::FunctionCall const & func_call_node)
			{
				write_function_call(func_call_node.function_id, func_call_node.parameters, program, c_source);
			},
			[&](expression::RelationalOperatorCall const & op_node)
			{
				c_source += '(';
				write_function_call(op_node.function_id, op_node.parameters, program, c_source);

				switch (op_node.op)
				{
					case Operator::not_equal:		c_source += " != 0"; break;
					case Operator::less:			c_source += " < 0" ; break;
					case Operator::less_equal:		c_source += " <= 0"; break;
					case Operator::greater:			c_source += " > 0" ; break;
					case Operator::greater_equal:	c_source += " >= 0"; break;
					default: declare_unreachable();
				}

				c_source += ')';

			},
			[&](expression::Assignment const & assign_node)
			{
				transpile_expression(*assign_node.destination, program, c_source);
				c_source += " = ";
				transpile_expression(*assign_node.source, program, c_source);
			},
			[&](expression::If const & if_node)
			{
				c_source += '(';
				c_source += '(';
				transpile_expression(*if_node.condition, program, c_source);
				c_source += ") ? ";
				transpile_expression(*if_node.then_case, program, c_source);
				c_source += " : ";
				transpile_expression(*if_node.else_case, program, c_source);
				c_source += ')';
			},
			[&](expression::StatementBlock const & block_node)
			{
				static_cast<void>(block_node);
				mark_as_to_do("Transpiling block nodes.");
			},
			[&](expression::Constructor const & ctor_node)
			{
				Type const & constructed_type = type_with_id(program, ctor_node.constructed_type);

				if (is_struct(constructed_type))
				{
					
				}
				else if (is_array(constructed_type))
				{
					c_source += '{';

					if (ctor_node.parameters.size() == 1)
					{
						size_t const size = array_size(constructed_type);
						std::string parameter_source;
						transpile_expression(ctor_node.parameters[0], program, parameter_source);

						for (size_t i = 0; i < size - 1; ++i)
						{
							c_source += parameter_source;
							c_source += ", ";
						}
						c_source += parameter_source;
					}
					else
					{
						size_t const size = array_size(constructed_type);
						assert(size == ctor_node.parameters.size());

						for (size_t i = 0; i < size - 1; ++i)
						{
							transpile_expression(ctor_node.parameters[i], program, c_source);
							c_source += ", ";
						}
						transpile_expression(ctor_node.parameters.back(), program, c_source);
					}

					c_source += '}';
				}
				else
				{
					declare_unreachable();
				}
			}
		);
		std::visit(visitor, expr.as_variant());
	}

	auto transpile_statement(complete::Statement const & tree,complete::Program const & program, int indentation, std::string & c_source) noexcept -> void
	{
		using namespace complete;

		auto const visitor = overload(
			[&](statement::VariableDeclaration const & node)
			{
				c_source += indent(indentation);
				c_source += type_name(expression_type_id(node.assigned_expression, program), program);
				c_source += ' ';
				c_source += join("local_", node.variable_offset);
				c_source += " = ";
				transpile_expression(node.assigned_expression, program, c_source);
				c_source += ";\n";
			},
			[&](statement::ExpressionStatement const & expr_node)
			{
				c_source += indent(indentation);
				transpile_expression(expr_node.expression, program, c_source);
				c_source += ";\n";
			},
			[&](statement::Return const & return_node)
			{
				c_source += indent(indentation);
				c_source += "return ";
				transpile_expression(return_node.returned_expression, program, c_source);
				c_source += ";\n";
			},
			[&](statement::If const & if_node)
			{
				c_source += indent(indentation);
				c_source += "if (";
				transpile_expression(if_node.condition, program, c_source);
				c_source += ")\n";
				transpile_statement(*if_node.then_case, program, indentation + 1, c_source);
				if (if_node.else_case)
				{
					c_source += indent(indentation);
					c_source += "else\n";
					transpile_statement(*if_node.else_case, program, indentation + 1, c_source);
				}
			},
			[&](statement::StatementBlock const & block_node)
			{
				c_source += indent(indentation);
				c_source += "{\n";
				for (Statement const & statement : block_node.statements)
				{
					transpile_statement(statement, program, indentation + 1, c_source);
				}
				c_source += indent(indentation);
				c_source += "}\n";
			},
			[&](statement::While const & while_node)
			{
				c_source += indent(indentation);
				c_source += "while (";
				transpile_expression(while_node.condition, program, c_source);
				c_source += ")\n";
				transpile_statement(*while_node.body, program, indentation + 1, c_source);
			},
			[&](statement::For const & for_node)
			{
				c_source += indent(indentation) + "{\n";
				transpile_statement(*for_node.init_statement, program, indentation + 1, c_source);
				c_source += indent(indentation + 1) + "while (";
				transpile_expression(for_node.condition, program, c_source);
				c_source += ")\n";
				c_source += indent(indentation + 1) + "{\n";
				transpile_statement(*for_node.body, program, indentation + 2, c_source);
				c_source += indent(indentation + 2);
				transpile_expression(for_node.end_expression, program, c_source);
				c_source += ";\n";
				c_source += indent(indentation + 1) + "}\n";
				c_source += indent(indentation) + "}\n";
			},
			[&](statement::Break const &)
			{
				c_source += indent(indentation) + "break;\n";
			},
			[&](statement::Continue const &)
			{
				c_source += indent(indentation) + "continue;\n";
			}
		);
		std::visit(visitor, tree.as_variant());
	}

	auto write_function_prototype(complete::Function const & function, FunctionId function_id, complete::Program const & program, std::string & c_source) noexcept -> void
	{
		c_source += type_name(function.return_type, program);
		c_source += ' ';
		c_source += function_name(function_id, program);
		c_source += '(';

		if (function.parameter_count > 0)
		{
			for (int i = 0; i < function.parameter_count; ++i)
			{
				c_source += type_name(function.variables[i].type, program);
				c_source += ' ';
				c_source += join("local_", function.variables[i].offset);

				if (i < function.parameter_count - 1)
					c_source += ", ";
			}
		}
		else
		{
			c_source += "void";
		}

		c_source += ')';
	}

	auto write_function(complete::Function const & function, FunctionId function_id, complete::Program const & program, std::string & c_source) noexcept -> void
	{
		write_function_prototype(function, function_id, program, c_source);
		c_source += '\n';
		c_source += "{\n";

		for (complete::Statement const & statement : function.statements)
			transpile_statement(statement, program, 1, c_source);

		c_source += "}\n\n";
	}

	auto transpile_to_c(complete::Program const & program) noexcept -> std::string
	{
		std::string c_source;

		// Write structs
		for (size_t i = 0; i < program.types.size(); ++i)
		{
			complete::Type const & type = program.types[i];
			if (is_struct(type))
			{
				c_source += "typedef struct\n";
				c_source += "{\n";

				complete::Struct const & struct_data = *struct_for_type(program, type);
				for (complete::MemberVariable const & member_var : struct_data.member_variables)
					c_source += join('\t', type_name(member_var.type, program), ' ', "member_", member_var.offset, ";// ", member_var.name, '\n');

				c_source += "} ";
				c_source += type_name(complete::TypeId::with_index(unsigned(i)), program);
				c_source += ";\n\n";
			}
		}

		// Write prototypes of all functions.
		for (complete::Function const & function : program.functions)
		{
			auto const id = FunctionId{false, unsigned(&function - program.functions.data())};
			if (id != program.main_function)
			{
				write_function_prototype(function, id, program, c_source);
				c_source += ";\n";
			}
		}

		c_source += "\n";

		// Write all functions.
		for (complete::Function const & function : program.functions)
		{
			auto const id = FunctionId{false, unsigned(&function - program.functions.data())};

			write_function(function, id, program, c_source);
			c_source += "\n";
		}

		return c_source;
	}

} // namespace c_transpiler
