#pragma once

#include "compatibility.hh"

#define AFIL_WARNING_HEADER(warning_code)			\
	__FILE__ "(" AFIL_INVOKE_UNARY(AFIL_STRINGIZE, __LINE__) ") : Warning " warning_code " : "


#define WARNING(WarningCode, WarningText) AFIL_MESSAGE_INFO(AFIL_WARNING_HEADER(WarningCode) WarningText)

#define TODO(WarningText) WARNING("TODO", WarningText)
#define HACK(WarningText) WARNING("HACK", WarningText)
