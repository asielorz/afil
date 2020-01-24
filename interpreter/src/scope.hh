#pragma once

#include "function_id.hh"
#include "value_ptr.hh"
#include "span.hh"
#include <vector>
#include <variant>
#include <optional>

namespace incomplete
{

	struct DependentTypeId
	{
		union BaseCase
		{
			struct
			{
				unsigned is_language_reserved : 1;
				unsigned is_dependent : 1;
				unsigned index : 30;
			};
			unsigned flat_value;
		};
		struct Pointer
		{
			value_ptr<DependentTypeId> pointee;
		};
		struct Array
		{
			value_ptr<DependentTypeId> value_type;
			int size;
		};
		struct ArrayPointer
		{
			value_ptr<DependentTypeId> pointee;
		};

		std::variant<BaseCase, Pointer, Array, ArrayPointer> value;
		bool is_mutable : 1;
		bool is_reference : 1;

		static auto with_index(unsigned index) noexcept -> DependentTypeId;
		static DependentTypeId const unknown;
	};

	struct Variable
	{
		std::string name;
		std::optional<DependentTypeId> type;
	};

	struct Function;
	struct Struct;

	struct Scope
	{
		std::vector<Variable> variables;
		std::vector<Function> functions;
		std::vector<Struct> structs;
	};

	using ScopeStack = std::vector<Scope *>;
	using ScopeStackView = span<Scope *>;

	struct Statement;
	struct Function
	{
		Scope scope;
		std::vector<Statement> statements;
		int parameter_count;
	};

	struct ExpressionTree;
	struct MemberVariable
	{
		std::string name;
		DependentTypeId type;
		std::optional<ExpressionTree> initializer_expression;
	};

	struct Struct
	{
		std::string name;
		std::vector<MemberVariable> member_variables;
	};

} // namespace incomplete
