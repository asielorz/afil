#pragma once

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

} // namespace instantiation
