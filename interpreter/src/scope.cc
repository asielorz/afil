#include "scope.hh"
#include "expression.hh"

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

namespace complete
{

	TypeId const TypeId::void_ =  {false, false, false, 0};
	TypeId const TypeId::int_ =   {false, false, false, 1};
	TypeId const TypeId::float_ = {false, false, false, 2};
	TypeId const TypeId::bool_ =  {false, false, false, 3};

	TypeId const TypeId::none =     {true, false, false, 0};
	TypeId const TypeId::function = {true, false, false, 1};
	TypeId const TypeId::deduce =   {true, false, false, 2};

} // namespace complete
