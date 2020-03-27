#pragma once

#include "program.hh"
#include "utils/out.hh"
#include "utils/span.hh"
#include "scope_stack.hh"
#include "syntax_error.hh"
#include <vector>
#include <variant>

namespace incomplete { struct Module; }
namespace complete { struct Module; }

namespace instantiation
{
	
	//[[nodiscard]] auto semantic_analysis(span<incomplete::Statement const> incomplete_program, out<complete::Program> complete_program) noexcept -> expected<void, PartialSyntaxError>;

	auto semantic_analysis(
		span<incomplete::Module const> incomplete_modules,
		span<int const> parse_order
	) noexcept -> expected<complete::Program, SyntaxError>;

	auto instantiate_function_template(
		incomplete::Function const & incomplete_function,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program
	) -> expected<complete::Function, PartialSyntaxError>;

	auto instantiate_expression(
		incomplete::Expression const & incomplete_expression_,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program,
		optional_out<complete::TypeId> current_scope_return_type
	) -> expected<complete::Expression, PartialSyntaxError>;

	namespace lookup_result
	{
		struct Nothing {};
		struct Variable { complete::TypeId variable_type; int variable_offset; };
		struct Constant { complete::Constant const * constant; };
		struct GlobalVariable { complete::TypeId variable_type; int variable_offset; };
		struct OverloadSet : complete::OverloadSet {};
		struct Type { complete::TypeId type_id; };
		struct StructTemplate { complete::StructTemplateId template_id; };
	}
	auto lookup_name(ScopeStackView scope_stack, std::string_view name) noexcept
		->std::variant<
			lookup_result::Nothing,
			lookup_result::Variable,
			lookup_result::Constant,
			lookup_result::GlobalVariable,
			lookup_result::OverloadSet,
			lookup_result::Type,
			lookup_result::StructTemplate
		>;
	auto type_with_name(std::string_view name, ScopeStackView scope_stack) noexcept -> complete::TypeId;
	auto type_with_name(std::string_view name, ScopeStackView scope_stack, span<complete::ResolvedTemplateParameter const> template_parameters) noexcept -> complete::TypeId;
	auto struct_template_with_name(std::string_view name, ScopeStackView scope_stack) noexcept -> std::optional<complete::StructTemplateId>;

	auto resolve_dependent_type(
		incomplete::TypeId const & dependent_type,
		std::vector<complete::ResolvedTemplateParameter> & template_parameters,
		ScopeStack & scope_stack,
		out<complete::Program> program)
		-> expected<complete::TypeId, PartialSyntaxError>;

} // namespace instantiation