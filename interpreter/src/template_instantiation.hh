#pragma once

#include "program.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include <vector>
#include <variant>

namespace instantiation
{
	enum struct ScopeType { global, function, block };
	struct CurrentScope
	{
		complete::Scope * scope;
		ScopeType type;
		int scope_offset;
	};

	using ScopeStack = std::vector<CurrentScope>;
	using ScopeStackView = span<const CurrentScope>;

	auto instantiate_templates(span<incomplete::Statement const> program) noexcept -> complete::Program;

	auto instantiate_function_template(
		incomplete::Function const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> complete::Function;

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> complete::Expression;

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
		->std::variant<
			lookup_result::Nothing,
			lookup_result::Variable,
			lookup_result::GlobalVariable,
			lookup_result::OverloadSet,
			lookup_result::Type,
			lookup_result::StructTemplate
		>;
	auto type_with_name(std::string_view name, ScopeStackView scope_stack) noexcept -> complete::TypeId;
	auto type_with_name(std::string_view name, ScopeStackView scope_stack, span<complete::ResolvedTemplateParameter const> template_parameters) noexcept -> complete::TypeId;

	auto resolve_dependent_type(
		incomplete::TypeId const & dependent_type,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program)
		-> complete::TypeId;

} // namespace instantiation
