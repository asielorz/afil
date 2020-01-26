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

enum struct ScopeType { global, function, block };
struct CurrentScope
{
	complete::Scope * scope;
	ScopeType type;
	int scope_offset;
};
using ScopeStack = std::vector<CurrentScope>;
using ScopeStackView = span<const CurrentScope>;

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

auto top(ScopeStack & scope_stack) noexcept -> complete::Scope & { return *scope_stack.back().scope; }

auto resolve_dependent_type(span<complete::TypeId const> template_parameters, incomplete::TypeId dependent_type, complete::Program & program) -> complete::TypeId
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
			complete::TypeId const pointee = resolve_dependent_type(template_parameters, *pointer.pointee, program);
			return pointer_type_for(pointee, program);
		},
		[&](incomplete::TypeId::Array const & array)
		{
			complete::TypeId const value_type = resolve_dependent_type(template_parameters, *array.value_type, program);
			return array_type_for(value_type, array.size, program);
		},
		[&](incomplete::TypeId::ArrayPointer const & array_pointer)
		{
			complete::TypeId const pointee = resolve_dependent_type(template_parameters, *array_pointer.pointee, program);
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

namespace instantiation
{

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression, 
		std::vector<complete::TypeId> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> complete::Expression
	{
		(void)(incomplete_expression, program, template_parameters, scope_stack);
		return {};
	}

	auto instantiate_statement(
		incomplete::Statement const & incomplete_statement, 
		std::vector<complete::TypeId> & template_parameters, 
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> std::optional<complete::Statement>
	{
		(void)(program, template_parameters);

		auto const visitor = overload(
			[&](incomplete::statement::VariableDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				complete::Expression expression = instantiate_expression(statement.assigned_expression, template_parameters, scope_stack, program);
				complete::TypeId const assigned_expression_type = expression_type_id(expression, *program);
				complete::TypeId const var_type = statement.type.has_value()
					? resolve_dependent_type(template_parameters, *statement.type, *program)
					: decay(assigned_expression_type);

				// If it's a function somehow(expression)
				// TODO

				raise_syntax_error_if_not(is_convertible(assigned_expression_type, var_type, *program), "Cannot convert to variable type in variable declaration.");

				int const var_offset = add_variable_to_scope(top(scope_stack), statement.variable_name, var_type, scope_stack.back().scope_offset, *program);

				complete::statement::VariableDeclaration complete_statement;
				complete_statement.variable_offset = var_offset;
				complete_statement.assigned_expression = insert_conversion_node(std::move(expression), assigned_expression_type, var_type, *program);
				return complete_statement;
			},
			[](incomplete::statement::ExpressionStatement const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::If const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::StatementBlock const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::While const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::For const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::Return const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return complete::Statement();
			},
			[](incomplete::statement::Break const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Break();
			},
			[](incomplete::statement::Continue const &) -> std::optional<complete::Statement>
			{
				return complete::statement::Continue();
			},
			[](incomplete::statement::StructDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return std::nullopt;
			},
			[](incomplete::statement::StructTemplateDeclaration const & statement) -> std::optional<complete::Statement>
			{
				(void)(statement);
				return std::nullopt;
			}
		);

		return std::visit(visitor, incomplete_statement.as_variant());
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
