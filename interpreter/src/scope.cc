#include "scope.hh"

namespace incomplete
{

	auto DependentTypeId::with_index(unsigned index) noexcept -> DependentTypeId
	{
		DependentTypeId::BaseCase base_case;
		base_case.index = index;
		base_case.is_dependent = true;
		base_case.is_language_reserved = false;

		DependentTypeId type;
		type.is_mutable = false;
		type.is_reference = false;
		type.value = base_case;
		return type;
	}

} // namespace incomplete
