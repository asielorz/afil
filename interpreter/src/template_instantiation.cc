#include "template_instantiation.hh"
#include "program.hh"
#include "syntax_error.hh"
#include "incomplete_statement.hh"
#include "complete_expression.hh"
#include "utils/out.hh"
#include "utils/variant.hh"
#include "utils/overload.hh"
#include "utils/utils.hh"
#include "utils/unreachable.hh"
#include "utils/warning_macro.hh"

using namespace std::literals;

enum struct ScopeType { global, function, block };
struct CurrentScope
{
	complete::Scope * scope;
	ScopeType type;
	int scope_offset;
};
using ScopeStack = std::vector<CurrentScope>;
using ScopeStackView = span<const CurrentScope>;

auto top(ScopeStack & scope_stack) noexcept -> complete::Scope & { return *scope_stack.back().scope; }

template <typename Stack>
struct StackGuard
{
	StackGuard(Stack & s) noexcept : stack(std::addressof(s)) {}
	StackGuard(StackGuard const &) = delete;
	StackGuard & operator = (StackGuard const &) = delete;
	~StackGuard() { stack->pop_back(); }

	Stack * stack;
};
auto push_block_scope(ScopeStack & scope_stack, complete::Scope & scope) noexcept -> StackGuard<ScopeStack>
{
	int const offset = scope_stack.back().scope_offset + top(scope_stack).stack_frame_size;
	scope_stack.push_back({&scope, ScopeType::block, offset});
	return StackGuard<ScopeStack>(scope_stack);
}

template <typename T>
auto add_variable_to_scope(
	std::vector<T> & variables, int & scope_size, int & scope_alignment,
	std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int
{
	complete::Type const & type = type_with_id(program, type_id);
	int const size = type_id.is_reference ? sizeof(void *) : type.size;
	int const alignment = type_id.is_reference ? alignof(void *) : type.alignment;

	T var;
	var.name = name;
	var.type = type_id;
	var.offset = scope_offset + align(scope_size, alignment);
	scope_size = var.offset + size;
	scope_alignment = std::max(scope_alignment, alignment);
	variables.push_back(std::move(var));
	return var.offset;
}

auto add_variable_to_scope(complete::Scope & scope, std::string_view name, complete::TypeId type_id, int scope_offset, complete::Program const & program) -> int
{
	return add_variable_to_scope(scope.variables, scope.stack_frame_size, scope.stack_frame_alignment, name, type_id, scope_offset, program);
}

auto resolve_dependent_type(incomplete::TypeId dependent_type, span<complete::TypeId const> template_parameters, complete::Program & program) -> complete::TypeId
{
	auto const visitor = overload(
		[&](incomplete::TypeId::BaseCase const & base_case)
		{
			if (base_case.is_dependent)
			{
				return template_parameters[base_case.index];
			}
			else
			{
				complete::TypeId type;
				type.index = base_case.index;
				type.is_language_reseved = base_case.is_language_reserved;
				return type;
			}
		},
		[&](incomplete::TypeId::Pointer const & pointer)
		{
			complete::TypeId const pointee = resolve_dependent_type(*pointer.pointee, template_parameters, program);
			return pointer_type_for(pointee, program);
		},
		[&](incomplete::TypeId::Array const & array)
		{
			complete::TypeId const value_type = resolve_dependent_type(*array.value_type, template_parameters, program);
			return array_type_for(value_type, array.size, program);
		},
		[&](incomplete::TypeId::ArrayPointer const & array_pointer)
		{
			complete::TypeId const pointee = resolve_dependent_type(*array_pointer.pointee, template_parameters, program);
			return array_pointer_type_for(pointee, program);
		},
		[](incomplete::TypeId::TemplateInstantiation const & /*template_instantiation*/) -> complete::TypeId
		{
			mark_as_to_do("Dependent template instantiations");
		}
	);

	complete::TypeId type = std::visit(visitor, dependent_type.value);
	type.is_reference = dependent_type.is_reference;
	type.is_mutable = dependent_type.is_mutable;
	return type;
}

namespace lookup_result
{
	struct Nothing {};
	struct Variable { complete::TypeId variable_type; int variable_offset; };
	struct GlobalVariable { complete::TypeId variable_type; int variable_offset; };
	struct OverloadSet : complete::OverloadSet {};
	struct Type { complete::TypeId type_id; };
	struct StructTemplate { complete::StructTemplateId template_id; };
}
auto lookup_name(ScopeStackView scope_stack, std::string_view name) noexcept
	-> std::variant<
		lookup_result::Nothing,
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet,
		lookup_result::Type,
		lookup_result::StructTemplate
	>
{
	using namespace complete;
	lookup_result::OverloadSet overload_set;

	bool stop_looking_for_variables = false;

	// Search the scopes in reverse order.
	int const start = static_cast<int>(scope_stack.size() - 1);
	for (int i = start; i >= 0; --i)
	{
		Scope const & scope = *scope_stack[i].scope;

		// Search variables and types only if we don't already know this is a function name.
		if (overload_set.function_ids.empty())
		{
			if (scope_stack[i].type == ScopeType::global)
			{
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), [name](Variable const & var) { return var.name == name; });
				if (var != scope.variables.end())
					return lookup_result::GlobalVariable{ var->type, var->offset };
			}
			else if (!stop_looking_for_variables)
			{
				auto const var = std::find_if(scope.variables.begin(), scope.variables.end(), [name](Variable const & var) { return var.name == name; });
				if (var != scope.variables.end())
					return lookup_result::Variable{ var->type, var->offset };
			}

			auto const type = std::find_if(scope.types.begin(), scope.types.end(), [name](Variable const & var) { return var.name == name; });
			if (type != scope.types.end())
				return lookup_result::Type{ type->id };

			auto const struct_template = std::find_if(scope.struct_templates.begin(), scope.struct_templates.end(), [name](Variable const & var) { return var.name == name; });
			if (struct_template != scope.struct_templates.end())
				return lookup_result::StructTemplate{ struct_template->id };
		}

		// Functions.
		for (FunctionName const & fn : scope.functions)
			if (fn.name == name)
				overload_set.function_ids.push_back(fn.id);

		// Function templates.
		for (FunctionTemplateName const & fn : scope.function_templates)
			if (fn.name == name)
				overload_set.function_template_ids.push_back(fn.id);

		// After we leave a function, stop looking for variables.
		if (i < start && scope_stack[i].type == ScopeType::function)
			stop_looking_for_variables = true;
	}

	if (overload_set.function_ids.empty() && overload_set.function_template_ids.empty())
		return lookup_result::Nothing();
	else
		return overload_set;
}

complete::OverloadSet named_overload_set(std::string_view name, ScopeStackView scope_stack)
{
	auto const visitor = overload(
		[](lookup_result::Nothing const &) -> complete::OverloadSet
		{
			return {};
		},
			[](lookup_result::OverloadSet const & overload_set) -> complete::OverloadSet
		{
			return overload_set;
		},
			[](auto const &) -> complete::Expression { declare_unreachable(); }
		);

	auto lookup = lookup_name(scope_stack, name);
	return std::visit(visitor, lookup);
}

complete::OverloadSet operator_overload_set(Operator op, ScopeStackView scope_stack)
{
	return named_overload_set(operator_function_name(op), scope_stack);
}

namespace instantiation
{

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_, 
		std::vector<complete::TypeId> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> complete::Expression
	{
		auto const visitor = overload(
			[](incomplete::expression::Literal<int> const & incomplete_expression) -> complete::Expression
			{
				return complete::expression::Literal<int>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<float> const & incomplete_expression) -> complete::Expression
			{
				return complete::expression::Literal<float>{incomplete_expression.value};
			},
			[](incomplete::expression::Literal<bool> const & incomplete_expression) -> complete::Expression
			{
				return complete::expression::Literal<bool>{incomplete_expression.value};
			},
			[&](incomplete::expression::Identifier const & incomplete_expression) -> complete::Expression
			{
				auto const lookup = lookup_name(scope_stack, incomplete_expression.name);
				auto const lookup_visitor = overload(
					[](lookup_result::Variable const & var) -> complete::Expression
					{
						complete::expression::LocalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[](lookup_result::GlobalVariable const & var) -> complete::Expression
					{
						complete::expression::GlobalVariable complete_expression;
						complete_expression.variable_type = var.variable_type;
						complete_expression.variable_offset = var.variable_offset;
						return complete_expression;
					},
					[](lookup_result::OverloadSet const & var) -> complete::Expression
					{
						complete::expression::OverloadSet complete_expression;
						complete_expression.overload_set = var; // Move?
						return complete_expression;
					},
					[](auto const &) -> complete::Expression { declare_unreachable(); }
				);
				return std::visit(lookup_visitor, lookup);
			},
			[&](incomplete::expression::MemberVariable const & incomplete_expression) -> complete::Expression
			{
				complete::expression::MemberVariable complete_expression;
				complete_expression.owner = allocate(instantiate_expression(*incomplete_expression.owner, template_parameters, scope_stack, program));
				
				complete::TypeId const owner_type_id = decay(expression_type_id(*complete_expression.owner, *program));
				complete::Type const & owner_type = type_with_id(*program, owner_type_id);
				raise_syntax_error_if_not(is_struct(owner_type), "Cannot access member of non struct type.");
				complete::Struct const & owner_struct = *struct_for_type(*program, owner_type);
				int const member_index = find_member_variable(owner_struct, incomplete_expression.name);
				raise_syntax_error_if_not(member_index != -1, "Member not found.");

				complete_expression.variable_offset = owner_struct.member_variables[member_index].offset;
				complete_expression.variable_type = owner_struct.member_variables[member_index].type;

				return complete_expression;
			},
			[&](incomplete::expression::Addressof const & incomplete_expression) -> complete::Expression
			{
				complete::Expression operand = instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program);
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				raise_syntax_error_if_not(operand_type.is_reference, "Attempted to take address of temporary.");
				complete::TypeId const pointer_type = pointer_type_for(remove_reference(operand_type), *program);

				complete::expression::ReinterpretCast complete_expression;
				complete_expression.operand = allocate(std::move(operand));
				complete_expression.return_type = pointer_type;
				return complete_expression;
			},
			[&](incomplete::expression::Dereference const & incomplete_expression) -> complete::Expression
			{
				complete::Expression operand = instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program);
				complete::TypeId const operand_type_id = expression_type_id(operand, *program);
				complete::Type const operand_type = type_with_id(*program, operand_type_id);

				if (is_pointer(operand_type))
				{
					complete::TypeId pointee_type = try_get<complete::Type::Pointer>(operand_type.extra_data)->value_type;
					pointee_type.is_reference = true;

					complete::expression::ReinterpretCast complete_expression;
					complete_expression.return_type = pointee_type;
					complete_expression.operand = allocate(std::move(operand));
				}
				else
				{
					FunctionId const function = resolve_function_overloading_and_insert_conversions(
						operator_overload_set(Operator::dereference, scope_stack), {&operand, 1}, {&operand_type_id, 1}, *program);
					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found for dereference operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.push_back(std::move(operand));
					return complete_expression;
				}
			},
			[&](incomplete::expression::Subscript const & incomplete_expression) -> complete::Expression
			{
				complete::Expression array = instantiate_expression(*incomplete_expression.array, template_parameters, scope_stack, program);
				complete::TypeId const array_type_id = expression_type_id(array, *program);
				complete::Type const array_type = type_with_id(*program, array_type_id);

				if (is_array(array_type))
				{
					complete::TypeId value_type = try_get<complete::Type::Array>(array_type.extra_data)->value_type;
					value_type.is_reference = array_type_id.is_reference;

					complete::Expression index = instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program);

					complete::expression::Subscript complete_expression;
					complete_expression.return_type = value_type;
					complete_expression.array = allocate(std::move(array));
					complete_expression.index = allocate(insert_conversion_node(std::move(index), complete::TypeId::int_, *program));
					return complete_expression;
				}
				else
				{
					complete::Expression index = instantiate_expression(*incomplete_expression.index, template_parameters, scope_stack, program);
					complete::TypeId const index_type_id = expression_type_id(index, *program);

					complete::TypeId const param_types[] = {array_type_id, index_type_id};
					complete::Expression params[] = {std::move(array), std::move(index)};

					FunctionId const function = resolve_function_overloading_and_insert_conversions(named_overload_set("[]"sv, scope_stack), params, param_types, *program);
					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found for subscript operator.");

					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(params[0]));
					complete_expression.parameters.push_back(std::move(params[1]));
					return complete_expression;
				}
			},
			[&](incomplete::expression::Function const &) -> complete::Expression
			{
				mark_as_to_do("Instantiation of functions");
			},
			[&](incomplete::expression::FunctionTemplate const &) -> complete::Expression
			{
				mark_as_to_do("Instantiation of function templates");
			},
			[&](incomplete::expression::FunctionCall const & incomplete_expression) -> complete::Expression
			{
				std::vector<complete::Expression> parameters;
				parameters.reserve(incomplete_expression.parameters.size());
				for (incomplete::Expression const & incomplete_param : incomplete_expression.parameters)
					parameters.push_back(instantiate_expression(incomplete_param, template_parameters, scope_stack, program));

				if (has_type<complete::expression::OverloadSet>(parameters[0]))
				{
					std::vector<complete::TypeId> parameter_types;
					parameter_types.reserve(parameters.size() - 1);
					for (size_t i = 1; i < parameters.size(); ++i)
						parameter_types.push_back(expression_type_id(parameters[i], *program));

					FunctionId const function = resolve_function_overloading_and_insert_conversions(
						try_get<complete::expression::OverloadSet>(parameters[0])->overload_set,
						{&parameters[1], parameter_types.size()}, parameter_types, *program);

					raise_syntax_error_if_not(function != invalid_function_id, "Overload not found.");
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters = std::move(parameters);
					complete_expression.parameters.erase(complete_expression.parameters.begin());
					return complete_expression;
				}
				else
				{
					mark_as_to_do("Overload of operator function call");
				}
			},
			[&](incomplete::expression::UnaryOperatorCall & incomplete_expression) -> complete::Expression
			{
				complete::Expression operand = instantiate_expression(*incomplete_expression.operand, template_parameters, scope_stack, program);
				complete::TypeId const operand_type = expression_type_id(operand, *program);
				FunctionId const function = resolve_function_overloading_and_insert_conversions(
					operator_overload_set(incomplete_expression.op), {&operand, 1}, {&operand_type, 1}, *program);

				raise_syntax_error_if_not(function != invalid_function_id, "Operator overload not found.");

				complete::expression::FunctionCall complete_expression;
				complete_expression.function_id = function;
				complete_expression.parameters.push_back(std::move(operand));
				return complete_expression;
			},
			[&](incomplete::expression::BinaryOperatorCall & incomplete_expression) -> complete::Expression
			{
				complete::Expression operands[] = { 
					instantiate_expression(*incomplete_expression.left, template_parameters, scope_stack, program),
					instantiate_expression(*incomplete_expression.right, template_parameters, scope_stack, program)
				};
				complete::TypeId const operand_types[] = { expression_type_id(operands[0], *program),expression_type_id(operands[1], *program) };
				FunctionId const function = resolve_function_overloading_and_insert_conversions(operator_overload_set(incomplete_expression.op), operands, operand_types, *program);
				
				raise_syntax_error_if_not(function != invalid_function_id, "Operator overload not found.");

				Operator const op = incomplete_expression.op;
				if (op == Operator::not_equal || op == Operator::less_equal || op == Operator::greater || op == Operator::greater_equal)
				{
					complete::expression::RelationalOperatorCall complete_expression;
					complete_expression.op = op;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return complete_expression;
				}
				else
				{
					complete::expression::FunctionCall complete_expression;
					complete_expression.function_id = function;
					complete_expression.parameters.reserve(2);
					complete_expression.parameters.push_back(std::move(operands[0]));
					complete_expression.parameters.push_back(std::move(operands[1]));
					return complete_expression;
				}
			},
			[&](incomplete::expression::If & incomplete_expression) -> complete::Expression
			{
				complete::Expression condition = insert_conversion_node(
					instantiate_expression(*incomplete_expression.condition, template_parameters, scope_stack, program),
					complete::TypeId::bool_, *program);

				complete::Expression then_case = instantiate_expression(*incomplete_expression.then_case, template_parameters, scope_stack, program);
				complete::Expression else_case = instantiate_expression(*incomplete_expression.else_case, template_parameters, scope_stack, program);

				complete::TypeId const then_type = expression_type_id(then_case, *program);
				complete::TypeId const else_type = expression_type_id(else_case, *program);
				complete::TypeId const return_type = common_type(then_type, else_type, *program);

				then_case = insert_conversion_node(std::move(then_case), then_type, return_type, *program);
				else_case = insert_conversion_node(std::move(else_case), else_type, return_type, *program);

				complete::expression::If complete_expression;
				complete_expression.condition = allocate(std::move(condition));
				complete_expression.then_case = allocate(std::move(then_case));
				complete_expression.else_case = allocate(std::move(else_case));
				return complete_expression;
			}
		);

		return std::visit(visitor, incomplete_expression_);
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement_, 
		std::vector<complete::TypeId> & template_parameters, 
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> std::optional<complete::Statement>
	{
		auto const visitor = overload(
			[&](incomplete::statement::VariableDeclaration const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::Expression expression = instantiate_expression(incomplete_statement.assigned_expression, template_parameters, scope_stack, program);
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				complete::TypeId const var_type = incomplete_statement.type.has_value()
					? resolve_dependent_type(*incomplete_statement.type, template_parameters, *program)
					: decay(assigned_expression_type);

				// If it's a function somehow(expression)
				TODO("Function declaration statements");

				raise_syntax_error_if_not(is_convertible(assigned_expression_type, var_type, *program), "Cannot convert to variable type in variable declaration.");

				int const var_offset = add_variable_to_scope(top(scope_stack), incomplete_statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

				complete::statement::VariableDeclaration complete_statement;
				complete_statement.variable_offset = var_offset;
				complete_statement.assigned_expression = insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program);
				return complete_statement;
			},
			[&](incomplete::statement::ExpressionStatement const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::statement::ExpressionStatement complete_statement;
				complete_statement.expression = instantiate_expression(incomplete_statement.expression, template_parameters, scope_stack, program);
				return complete_statement;
			},
			[&](incomplete::statement::If const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::statement::If complete_statement;
				complete_statement.condition = insert_conversion_node(
					instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program),
					complete::TypeId::bool_, *program);

				complete_statement.then_case = allocate(*instantiate_statement(*incomplete_statement.then_case, template_parameters, scope_stack, program));
				complete_statement.else_case = allocate(*instantiate_statement(*incomplete_statement.then_case, template_parameters, scope_stack, program));

				return complete_statement;
			},
			[&](incomplete::statement::StatementBlock const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::statement::StatementBlock complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);
				complete_statement.statements.reserve(incomplete_statement.statements.size());

				for (incomplete::Statement const & incomplete_substatement : incomplete_statement.statements)
				{
					auto complete_substatement = instantiate_statement(incomplete_substatement, template_parameters, scope_stack, program);
					if (complete_substatement.has_value())
						complete_statement.statements.push_back(std::move(*complete_substatement));
				}

				return complete_statement;
			},
			[&](incomplete::statement::While const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::statement::While complete_statement;

				complete_statement.condition = insert_conversion_node(
					instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program),
					complete::TypeId::bool_, *program);

				complete_statement.body = allocate(*instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program));

				return complete_statement;
			},
			[&](incomplete::statement::For const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::statement::For complete_statement;
				auto const guard = push_block_scope(scope_stack, complete_statement.scope);

				auto init_statement = instantiate_statement(*incomplete_statement.init_statement, template_parameters, scope_stack, program);
				raise_syntax_error_if_not(init_statement.has_value() && (
					has_type<complete::statement::VariableDeclaration>(*init_statement) ||
					has_type<complete::statement::ExpressionStatement>(*init_statement)), 
					"Only variable declaration or expression statements accepted as init statements of for loops"); TODO("Maybe move this to parser?")

				complete_statement.init_statement = allocate(*init_statement);

				complete_statement.condition = insert_conversion_node(
					instantiate_expression(incomplete_statement.condition, template_parameters, scope_stack, program),
					complete::TypeId::bool_, *program);

				complete_statement.end_expression = instantiate_expression(incomplete_statement.end_expression, template_parameters, scope_stack, program);
				complete_statement.body = allocate(*instantiate_statement(*incomplete_statement.body, template_parameters, scope_stack, program));

				return complete_statement;
			},
			[&](incomplete::statement::Return const & incomplete_statement) -> std::optional<complete::Statement>
			{
				TODO("Return type. Conversion to return type.")
				complete::statement::Return complete_statement;
				complete_statement.returned_expression = instantiate_expression(incomplete_statement.returned_expression, template_parameters, scope_stack, program);

				return complete_statement;
			},
			[](incomplete::statement::Break const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Break();
			},
			[](incomplete::statement::Continue const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Continue();
			},
			[&](incomplete::statement::StructDeclaration const & incomplete_statement) -> std::optional<complete::Statement>
			{
				complete::Type new_type;
				new_type.size = 0;
				new_type.alignment = 1;

				complete::Struct new_struct;
				new_struct.member_variables.reserve(incomplete_statement.declared_struct.member_variables.size());

				for (incomplete::MemberVariable const & member_variable : incomplete_statement.declared_struct.member_variables)
				{
					complete::TypeId const member_type = resolve_dependent_type(member_variable.type, template_parameters, *program);
					raise_syntax_error_if_not(is_data_type(member_type), "Member variable cannot be void.");
					raise_syntax_error_if_not(!member_type.is_reference, "Member variable cannot be reference.");
					raise_syntax_error_if_not(!member_type.is_mutable, "Member variable cannot be mutable. Mutability of members is inherited from mutability of object that contains them.");

					add_variable_to_scope(new_struct.member_variables, new_type.size, new_type.alignment, member_variable.name, member_type, 0, *program);

					complete::MemberVariable & new_variable = new_struct.member_variables.back();
					if (member_variable.initializer_expression.has_value())
					{
						new_variable.initializer_expression = insert_conversion_node(
							instantiate_expression(*member_variable.initializer_expression, template_parameters, scope_stack, program),
							member_type, *program
						);
					}
					else if (is_default_constructible(member_type, *program))
					{
						new_variable.initializer_expression = synthesize_default_constructor(member_type, *program);
					}
				}

				new_type.extra_data = complete::Type::Struct{static_cast<int>(program->structs.size())};
				complete::TypeId const new_type_id = add_type(*program, std::move(new_type));
				program->structs.push_back(std::move(new_struct));
				top(scope_stack).types.push_back({incomplete_statement.declared_struct.name, new_type_id});

				return std::nullopt;
			},
			[](incomplete::statement::StructTemplateDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				TODO("Struct templates");
				mark_as_to_do("Struct templates");
				//return std::nullopt;
			}
		);

		return std::visit(visitor, incomplete_statement_.as_variant());
	}

	auto instantiate_templates(span<incomplete::Statement const> incomplete_program) noexcept -> complete::Program
	{
		complete::Program complete_program;
		std::vector<complete::TypeId> template_parameters;
		ScopeStack scope_stack;
		scope_stack.push_back({&complete_program.global_scope, ScopeType::global, 0});

		for (incomplete::Statement const & incomplete_statement : incomplete_program)
		{
			auto complete_statement = instantiate_statement(incomplete_statement, template_parameters, scope_stack, out(complete_program));
			if (complete_statement)
			{
				raise_syntax_error_if_not(
					has_type<complete::statement::VariableDeclaration>(*complete_statement),
					"Only variable declarations, function declarations and struct declarations allowed at global scope."
				);
				complete_program.global_initialization_statements.push_back(std::move(*complete_statement));
			}
		}

		return complete_program;
	}

} // namespace instantiation
