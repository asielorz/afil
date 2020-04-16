#include "c_transpiler.hh"
#include "complete_expression.hh"
#include "complete_statement.hh"
#include "program.hh"
#include "utils/algorithm.hh"
#include "utils/map.hh"
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

	auto type_name(complete::TypeId type_id, complete::Program const & program) noexcept -> std::string
	{
		std::string_view const type_ABI_name = ABI_name(program, type_id);
		if (!type_ABI_name.empty())
			return std::string(type_ABI_name);

		complete::Type const type = type_with_id(program, type_id);
		if (is_pointer(type) || is_array_pointer(type))
			return join(type_name(pointee_type(type), program), " *");

		return join("type_", decay(type_id).flat_value);
	}

	auto mangle(std::string name, FunctionId function_id, complete::Program const & program) noexcept -> std::string
	{
		complete::TypeId const ret = return_type(program, function_id);
		auto const param_types = parameter_types_of(program, function_id);

		for (complete::TypeId const param : param_types)
		{
			name += "__";
			name += type_name(param, program); // TODO: function that returns ABI name of type (for pointers mainly)
		}
		name += "__";
		name += type_name(ret, program);
		return name;
	}

	auto function_name(FunctionId function_id, complete::Program const & program) noexcept -> std::string
	{
		if (function_id == program.main_function)
			return "main";

		std::string function_ABI_name = std::string(ABI_name(program, function_id));
		if (!function_ABI_name.empty())
			return mangle(function_ABI_name, function_id, program);

		if (function_id.type == FunctionId::Type::imported)
			return mangle(join("extern_function_", function_id.index), function_id, program);
		else
			return mangle(join("function_", function_id.index), function_id, program);
	}

	auto write_function_call(FunctionId function_id, span<std::string const> parameters, complete::Program const & program, std::string & c_source)
	{
		if (function_id.type == FunctionId::Type::instrinsic)
		{
			c_source += '(';
			if (parameters.size() == 1)
			{
				c_source += join(' ', built_in_operator_table[function_id.index], ' ');
				c_source += parameters[0];
			}
			else
			{
				assert(parameters.size() == 2);
				c_source += parameters[0];
				c_source += join(' ', built_in_operator_table[function_id.index], ' ');
				c_source += parameters[1];
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
					c_source += parameters[i];
					c_source += ", ";
				}
				c_source += parameters.back();
			}
			c_source += ')';
		}
	}

	auto write_scope_locals(span<complete::Variable const> vars, complete::Program const & program, int indentation, std::string & c_source) noexcept -> void
	{
		for (complete::Variable const & var : vars)
		{
			c_source += indent(indentation);
			c_source += type_name(var.type, program);
			c_source += ' ';
			c_source += join("local_", var.offset);
			c_source += ";\n";
		}
	}

	auto apply_dereference_node(std::string expr_source) noexcept -> std::string
	{
		// Cancel dereference addressof
		if (expr_source[0] == '&')
		{
			return expr_source.substr(1);
		}
		else
		{
			return join('*', expr_source);
		}
	}

	auto can_be_written_inline(complete::Expression const & expr) noexcept -> bool
	{
		using namespace complete;

		auto const visitor = overload(
			[&](expression::Literal<int>) { return true; },
			[&](expression::Literal<float>) { return true; },
			[&](expression::Literal<bool>) { return true; },
			[&](expression::Literal<uninit_t>) { return false; },
			[&](expression::Literal<TypeId>) { return false; },
			[&](expression::StringLiteral) { return true; },
			[&](expression::LocalVariable const &) { return true; },
			[&](expression::GlobalVariable const &) { return true; },
			[&](expression::MemberVariable const & var_node) { return can_be_written_inline(*var_node.owner); },
			[&](expression::Constant const &) { return true; },
			[&](expression::Dereference const & deref_node) { return can_be_written_inline(*deref_node.expression); },
			[&](expression::ReinterpretCast const & addressof_node) { return can_be_written_inline(*addressof_node.operand); },
			[&](expression::Subscript const & subscript_node) { return can_be_written_inline(*subscript_node.array) && can_be_written_inline(*subscript_node.index); },
			[&](expression::FunctionCall const & func_call_node) { return std::all_of(func_call_node.parameters, can_be_written_inline); },
			[&](expression::RelationalOperatorCall const & op_node) { return std::all_of(op_node.parameters, can_be_written_inline); },
			[&](expression::Assignment const &) { return false; },
			[&](expression::If const &) { return false; },
			[&](expression::StatementBlock const &) { return false; },
			[&](expression::Constructor const &) { return false; }
		);
		return std::visit(visitor, expr.as_variant());
	}

	auto transpile_expression_inline(
		complete::Expression const & expr, 
		complete::Program const & program, 
		std::string & c_source
	) noexcept -> void
	{
		using namespace complete;

		auto const visitor = overload(
			[&](expression::Literal<int> literal) 
			{
				c_source += to_string(literal.value);
			},
			[&](expression::Literal<float> literal) 
			{
				c_source += join(literal.value, 'f');
			},
			[&](expression::Literal<bool> literal)
			{
				c_source += literal.value ? "1" : "0"; 
			},
			[&](expression::Literal<uninit_t>) {},
			[&](expression::Literal<TypeId>) { declare_unreachable(); },
			[&](expression::StringLiteral literal)
			{
				c_source += join('"', literal.value, '"'); 
			},
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
				assert(can_be_written_inline(*var_node.owner));
				transpile_expression_inline(*var_node.owner, program, c_source);
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
					transpile_expression_inline(*deref_node.expression, program, c_source);
				}
				else
				{
					std::string expr_source;
					transpile_expression_inline(*deref_node.expression, program, expr_source);
					c_source += apply_dereference_node(std::move(expr_source));
				}
			},
			[&](expression::ReinterpretCast const & addressof_node)
			{
				transpile_expression_inline(*addressof_node.operand, program, c_source);
			},
			[&](expression::Subscript const & subscript_node)
			{
				transpile_expression_inline(*subscript_node.array, program, c_source);
				c_source += '[';
				transpile_expression_inline(*subscript_node.index, program, c_source);
				c_source += ']';
			},
			[&](expression::FunctionCall const & func_call_node)
			{
				auto const parameter_source = map(func_call_node.parameters, [&](Expression const & param) 
				{
					std::string param_source;
					transpile_expression_inline(param, program, param_source);
					return param_source;
				});
				write_function_call(func_call_node.function_id, parameter_source, program, c_source);
			},
			[&](expression::RelationalOperatorCall const & op_node)
			{
				c_source += '(';
				std::string parameter_source[2];
				transpile_expression_inline(op_node.parameters[0], program, parameter_source[0]);
				transpile_expression_inline(op_node.parameters[1], program, parameter_source[1]);
				write_function_call(op_node.function_id, parameter_source, program, c_source);

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
			[&](expression::Assignment const &)		{ declare_unreachable(); },
			[&](expression::If const &)				{ declare_unreachable(); },
			[&](expression::StatementBlock const &)	{ declare_unreachable(); },
			[&](expression::Constructor const &)	{ declare_unreachable(); }
		);
		std::visit(visitor, expr.as_variant());
	}

	auto add_temp_var(std::vector<std::string> & temp_vars, std::string type) -> std::string
	{
		size_t const index = temp_vars.size();
		temp_vars.push_back(std::move(type));
		return join("expr_temp_", index);
	}

	auto assign_to(std::string_view destination) noexcept -> std::string
	{
		if (destination.empty())
			return "";
		else
			return join(destination, " = ");
	}
	
	auto transpile_statement(
		complete::Statement const & tree,
		complete::Program const & program,
		int indentation,
		std::string_view return_destination,
		std::string & c_source
	) noexcept -> void;

	auto transpile_expression_as_statement(
		complete::Expression const & expr,
		complete::Program const & program,
		std::string_view destination,
		int indentation,
		std::vector<std::string> & temp_vars,
		std::string & c_source) noexcept -> void
	{
		using namespace complete;

		if (can_be_written_inline(expr))
		{
			c_source += indent(indentation);
			c_source += assign_to(destination);
			transpile_expression_inline(expr, program, c_source);
			c_source += ";\n";
		}
		else
		{
			auto const visitor = overload(
				[&](expression::Literal<int>)	{ declare_unreachable(); },
				[&](expression::Literal<float>) { declare_unreachable(); },
				[&](expression::Literal<bool>)	{ declare_unreachable(); },
				[&](expression::Literal<uninit_t>) {},
				[&](expression::Literal<TypeId>) { declare_unreachable(); },
				[&](expression::StringLiteral)	{ declare_unreachable(); },
				[&](expression::LocalVariable const &)	{ declare_unreachable(); },
				[&](expression::GlobalVariable const &) { declare_unreachable(); },
				[&](expression::MemberVariable const & var_node)
				{
					std::string const temp_var_name = add_temp_var(temp_vars, type_name(expression_type_id(*var_node.owner, program), program));
					transpile_expression_as_statement(*var_node.owner, program, temp_var_name, indentation, temp_vars, c_source);
					c_source += indent(indentation);
					c_source += assign_to(destination);
					c_source += temp_var_name;
					c_source += '.';
					c_source += join("member_", var_node.variable_offset);
					c_source += ";\n";
				},
				[&](expression::Constant const &) { declare_unreachable(); },
				[&](expression::Dereference const & deref_node)
				{
					std::string const temp_var_name = add_temp_var(temp_vars, type_name(expression_type_id(*deref_node.expression, program), program));
					transpile_expression_as_statement(*deref_node.expression, program, temp_var_name, indentation, temp_vars, c_source);
					c_source += indent(indentation);
					c_source += assign_to(destination);
					c_source += '*';
					c_source += temp_var_name;
					c_source += ";\n";
				},
				[&](expression::ReinterpretCast const & addressof_node)
				{
					transpile_expression_as_statement(*addressof_node.operand, program, destination, indentation, temp_vars, c_source);
				},
				[&](expression::Subscript const & subscript_node)
				{
					std::string const array_temp_var_name = add_temp_var(temp_vars, type_name(expression_type_id(*subscript_node.array, program), program));
					std::string const index_temp_var_name = add_temp_var(temp_vars, "int");
					transpile_expression_as_statement(*subscript_node.array, program, array_temp_var_name, indentation, temp_vars, c_source);
					transpile_expression_as_statement(*subscript_node.index, program, index_temp_var_name, indentation, temp_vars, c_source);
					c_source += indent(indentation);
					c_source += assign_to(destination);
					c_source += array_temp_var_name;
					c_source += '[';
					c_source += index_temp_var_name;
					c_source += ']';
					c_source += ";\n";
				},
				[&](expression::FunctionCall const & func_call_node)
				{
					std::vector<std::string> temp_var_names(func_call_node.parameters.size());
					for (size_t i = 0; i < func_call_node.parameters.size(); ++i)
					{
						Expression const & param = func_call_node.parameters[i];
						temp_var_names[i] = add_temp_var(temp_vars, type_name(expression_type_id(param, program), program));
						transpile_expression_as_statement(param, program, temp_var_names[i], indentation, temp_vars, c_source);
					}

					c_source += indent(indentation);
					c_source += assign_to(destination);
					write_function_call(func_call_node.function_id, temp_var_names, program, c_source);
					c_source += ";\n";
				},
				[&](expression::RelationalOperatorCall const & op_node)
				{
					std::string temp_var_names[2];
					temp_var_names[0] = add_temp_var(temp_vars, type_name(expression_type_id(op_node.parameters[0], program), program));
					temp_var_names[1] = add_temp_var(temp_vars, type_name(expression_type_id(op_node.parameters[1], program), program));
					transpile_expression_as_statement(op_node.parameters[0], program, temp_var_names[0], indentation, temp_vars, c_source);
					transpile_expression_as_statement(op_node.parameters[1], program, temp_var_names[1], indentation, temp_vars, c_source);

					c_source += indent(indentation);
					c_source += assign_to(destination);
					c_source += '(';
					write_function_call(op_node.function_id, temp_var_names, program, c_source);

					switch (op_node.op)
					{
						case Operator::not_equal:		c_source += " != 0"; break;
						case Operator::less:			c_source += " < 0"; break;
						case Operator::less_equal:		c_source += " <= 0"; break;
						case Operator::greater:			c_source += " > 0"; break;
						case Operator::greater_equal:	c_source += " >= 0"; break;
						default: declare_unreachable();
					}

					c_source += ')';
					c_source += ";\n";
				},
				[&](expression::Assignment const & assign_node)
				{
					if (can_be_written_inline(*assign_node.source))
					{
						c_source += indent(indentation);
						std::string assignment_destination;
						transpile_expression_inline(*assign_node.destination, program, assignment_destination);
						c_source += apply_dereference_node(std::move(assignment_destination));
						c_source += " = ";
						transpile_expression_inline(*assign_node.source, program, c_source);
						c_source += ";\n";
					}
					else
					{
						std::string const temp_var_name = add_temp_var(temp_vars, type_name(expression_type_id(*assign_node.source, program), program));
						transpile_expression_as_statement(*assign_node.source, program, temp_var_name, indentation, temp_vars, c_source);

						c_source += indent(indentation);
						std::string assignment_destination;
						transpile_expression_inline(*assign_node.destination, program, assignment_destination);
						c_source += apply_dereference_node(std::move(assignment_destination));
						c_source += " = ";
						c_source += temp_var_name;
						c_source += ";\n";
					}
				},
				[&](expression::If const & if_node)
				{
					bool const condition_inline = can_be_written_inline(*if_node.condition);
					if (condition_inline)
					{
						c_source += indent(indentation);
						c_source += "if (";
						transpile_expression_inline(*if_node.condition, program, c_source);
						c_source += ")\n";
					}
					else
					{
						c_source += indent(indentation);
						c_source += "{\n";
						indentation++;

						std::string const condition_var_name = add_temp_var(temp_vars, "int");
						transpile_expression_as_statement(*if_node.condition, program, condition_var_name, indentation, temp_vars, c_source);

						c_source += indent(indentation);
						c_source += "if (";
						c_source += condition_var_name;
						c_source += ")\n";
					}

					c_source += indent(indentation);
					c_source += "{\n";

					transpile_expression_as_statement(*if_node.then_case, program, destination, indentation + 1, temp_vars, c_source);

					c_source += indent(indentation);
					c_source += "}\n";

					c_source += indent(indentation);
					c_source += "else\n";
					c_source += indent(indentation);
					c_source += "{\n";

					transpile_expression_as_statement(*if_node.else_case, program, destination, indentation + 1, temp_vars, c_source);

					c_source += indent(indentation);
					c_source += "}\n";

					if (!condition_inline)
					{
						c_source += indent(indentation - 1);
						c_source += "}\n";
					}
				},
				[&](expression::StatementBlock const & block_node)
				{
					c_source += indent(indentation);
					c_source += "{\n";

					write_scope_locals(block_node.scope.variables, program, indentation + 1, c_source);

					for (Statement const & s : block_node.statements)
						transpile_statement(s, program, indentation + 1, join(destination, " = "), c_source);

					c_source += indent(indentation);
					c_source += "}\n";
				},
				[&](expression::Constructor const & ctor_node)
				{
					Type const & constructed_type = type_with_id(program, ctor_node.constructed_type);
					if (is_struct(constructed_type))
					{
						Struct const & constructed_struct = *struct_for_type(program, constructed_type);

						if (destination.empty())
						{
							size_t const size = ctor_node.parameters.size();
							for (size_t i = 0; i < size; ++i)
							{
								transpile_expression_as_statement(ctor_node.parameters[i], program, "", indentation, temp_vars, c_source);
							}
						}
						else
						{
							size_t const size = ctor_node.parameters.size();
							for (size_t i = 0; i < size; ++i)
							{
								std::string const param_destination = join(destination, '.', "member_", constructed_struct.member_variables[i].offset);
								transpile_expression_as_statement(ctor_node.parameters[i], program, param_destination, indentation, temp_vars, c_source);
							}
						}
					}
					else if (is_array(constructed_type))
					{
						mark_as_to_do();
					}
					else
					{
						declare_unreachable();
					}
				}
			);
			std::visit(visitor, expr.as_variant());
		}
	}

	auto write_expression_as_statement_in_its_own_scope_and_dont_close_the_scope(
		complete::Expression const & expr,
		complete::Program const & program,
		std::string_view destination,
		int indentation,
		std::string & c_source
	) -> void
	{
		c_source += indent(indentation);
		c_source += "{\n";

		std::vector<std::string> temp_vars;

		bool const is_return = destination == "return ";
		if (is_return)
			destination = add_temp_var(temp_vars, type_name(expression_type_id(expr, program), program));

		std::string expr_source;
		transpile_expression_as_statement(expr, program, destination, indentation + 1, temp_vars, expr_source);

		for (size_t i = 0; i < temp_vars.size(); ++i)
		{
			c_source += indent(indentation + 1);
			c_source += temp_vars[i];
			c_source += ' ';
			c_source += join("expr_temp_", i);
			c_source += ";\n";
		}
		c_source += '\n';
		c_source += expr_source;

		if (is_return)
		{
			c_source += indent(indentation + 1);
			c_source += "return ";
			c_source += destination;
			c_source += ";\n";
		}
	}

	auto write_expression_as_statement_in_its_own_scope(
		complete::Expression const & expr, 
		complete::Program const & program, 
		std::string_view destination, 
		int indentation, 
		std::string & c_source
	) -> void
	{
		write_expression_as_statement_in_its_own_scope_and_dont_close_the_scope(expr, program, destination, indentation, c_source);
		c_source += indent(indentation);
		c_source += "}\n";
	}

	auto transpile_statement(
		complete::Statement const & tree,
		complete::Program const & program, 
		int indentation, 
		std::string_view return_destination,
		std::string & c_source
	) noexcept -> void
	{
		using namespace complete;

		auto const visitor = overload(
			[&](statement::VariableDeclaration const & node)
			{
				if (can_be_written_inline(node.assigned_expression))
				{
					c_source += indent(indentation);
					c_source += join("local_", node.variable_offset);
					c_source += " = ";
					transpile_expression_inline(node.assigned_expression, program, c_source);
					c_source += ";\n";
				}
				else if (expression_type_id(node.assigned_expression, program) != complete::TypeId::uninit_t) // If constructing from uninit do nothing
				{
					c_source += indent(indentation);
					std::string const var_name = join("local_", node.variable_offset);
					c_source += var_name;
					c_source += ";\n";
					write_expression_as_statement_in_its_own_scope(node.assigned_expression, program, var_name, indentation, c_source);
				}
			},
			[&](statement::ExpressionStatement const & expr_node)
			{
				if (can_be_written_inline(expr_node.expression))
				{
					c_source += indent(indentation);
					transpile_expression_inline(expr_node.expression, program, c_source);
					c_source += ";\n";
				}
				else
				{
					write_expression_as_statement_in_its_own_scope(expr_node.expression, program, "", indentation, c_source);
				}
			},
			[&](statement::Return const & return_node)
			{
				if (can_be_written_inline(return_node.returned_expression))
				{
					c_source += indent(indentation);
					c_source += return_destination;
					transpile_expression_inline(return_node.returned_expression, program, c_source);
					c_source += ";\n";
				}
				else
				{
					write_expression_as_statement_in_its_own_scope(return_node.returned_expression, program, return_destination, indentation, c_source);
				}
			},
			[&](statement::If const & if_node)
			{
				bool const condition_inline = can_be_written_inline(if_node.condition);
				if (condition_inline)
				{
					c_source += indent(indentation);
					c_source += "if (";
					transpile_expression_inline(if_node.condition, program, c_source);
					c_source += ")\n";
				}
				else
				{
					c_source += indent(indentation);
					c_source += "{\n";
					indentation++;

					c_source += indent(indentation) + "int if_condition;\n";
					write_expression_as_statement_in_its_own_scope(if_node.condition, program, "if_condition", indentation, c_source);

					c_source += indent(indentation);
					c_source += "if (";
					c_source += "if_condition";
					c_source += ")\n";
				}

				c_source += indent(indentation);
				c_source += "{\n";

				transpile_statement(*if_node.then_case, program, indentation + 1, return_destination, c_source);

				c_source += indent(indentation);
				c_source += "}\n";

				if (if_node.else_case != nullptr)
				{
					c_source += indent(indentation);
					c_source += "else\n";
					c_source += indent(indentation);
					c_source += "{\n";

					transpile_statement(*if_node.else_case, program, indentation + 1, return_destination, c_source);

					c_source += indent(indentation);
					c_source += "}\n";
				}

				if (!condition_inline)
				{
					c_source += indent(indentation - 1);
					c_source += "}\n";
				}
			},
			[&](statement::StatementBlock const & block_node)
			{
				c_source += indent(indentation);
				c_source += "{\n";

				write_scope_locals(block_node.scope.variables, program, indentation + 1, c_source);

				for (Statement const & statement : block_node.statements)
				{
					transpile_statement(statement, program, indentation + 1, return_destination, c_source);
				}
				c_source += indent(indentation);
				c_source += "}\n";
			},
			[&](statement::While const & while_node)
			{
				c_source += indent(indentation);
				c_source += "while (";
				transpile_expression_inline(while_node.condition, program, c_source);
				c_source += ")\n";
				transpile_statement(*while_node.body, program, indentation + 1, return_destination, c_source);
			},
			[&](statement::For const & for_node)
			{
				c_source += indent(indentation) + "{\n";
				write_scope_locals(for_node.scope.variables, program, indentation + 1, c_source);
				transpile_statement(*for_node.init_statement, program, indentation + 1, return_destination, c_source);
				c_source += indent(indentation + 1) + "while (";
				transpile_expression_inline(for_node.condition, program, c_source);
				c_source += ")\n";
				c_source += indent(indentation + 1) + "{\n";
				transpile_statement(*for_node.body, program, indentation + 2, return_destination, c_source);
				c_source += indent(indentation + 2);
				write_expression_as_statement_in_its_own_scope(for_node.end_expression, program, "", indentation + 2, c_source);
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
		if (function.return_type.is_reference)
			c_source += " *";
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

		write_scope_locals({function.variables.data() + function.parameter_count, function.variables.size() - function.parameter_count}, program, 1, c_source);

		// Write preconditions.
		for (complete::Expression const & precondition : function.preconditions)
		{
			if (can_be_written_inline(precondition))
			{
				c_source += "\tassert(";
				transpile_expression_inline(precondition, program, c_source);
				c_source += ");\n";
			}
			else
			{
				c_source += "\tint precondition_met;";
				write_expression_as_statement_in_its_own_scope_and_dont_close_the_scope(precondition, program, "precondition_met", 1, c_source);
				c_source += "\tassert(precondition_met);";
			}
		}

		for (complete::Statement const & statement : function.statements)
			transpile_statement(statement, program, 1, "return ", c_source);

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
			auto const id = FunctionId(FunctionId::Type::program, unsigned(&function - program.functions.data()));
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
			auto const id = FunctionId(FunctionId::Type::program, unsigned(&function - program.functions.data()));

			write_function(function, id, program, c_source);
		}

		return c_source;
	}

} // namespace c_transpiler
