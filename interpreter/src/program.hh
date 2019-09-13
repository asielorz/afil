#pragma once

#include "span.hh"
#include <string_view>
#include <variant>

namespace parser { struct StatementTree; }

struct Type
{
	std::string_view name;
	int size;
	int alignment;
};
enum struct TypeId { function = -2, none = -1, int_, float_ };

struct Variable
{
	std::string_view name;
	TypeId type;
	int offset;
};

struct FunctionName
{
	std::string_view name;
	int id;
};

struct Scope
{
	int stack_frame_size = 0;
	std::vector<Variable> variables;
	std::vector<FunctionName> functions;
};

struct Function : Scope
{
	int parameter_count = 0; // From the variables, how many are arguments. The rest are locals.
	TypeId return_type;
	std::vector<parser::StatementTree> statements;
};

auto built_in_types() noexcept -> std::vector<Type>;

struct Program
{
	std::vector<Type> types = built_in_types();
	std::vector<Function> functions;
	Scope global_scope;
};

namespace lookup_result
{
	struct Nothing {};
	struct Variable { TypeId variable_type; int variable_offset; };
	struct GlobalVariable { TypeId variable_type; int variable_offset; };
	struct OverloadSet { std::vector<int> function_ids; };
}
auto lookup_name(Scope const & scope, Scope const & global_scope, std::string_view name) noexcept 
	-> std::variant<
		lookup_result::Nothing, 
		lookup_result::Variable,
		lookup_result::GlobalVariable,
		lookup_result::OverloadSet
	>;
// Returns id of function found or -1 on failure.
auto resolve_function_overloading(span<int const> overload_set, span<TypeId> parameters, Program const & program) noexcept -> int;

auto lookup_type_name(Program const & program, std::string_view name) noexcept -> TypeId;
auto type_with_id(Program const & program, TypeId id) noexcept -> Type const &;
