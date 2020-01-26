#include "incomplete_scope.hh"

namespace incomplete
{

	auto TypeId::with_index(unsigned index) noexcept -> TypeId
	{
		TypeId::BaseCase base_case;
		base_case.index = index;
		base_case.is_dependent = true;
		base_case.is_language_reserved = false;

		TypeId type;
		type.is_mutable = false;
		type.is_reference = false;
		type.value = base_case;
		return type;
	}

} // namespace incomplete
