#pragma once

// Statements like:
//		#pragma message(Reminder "Fix this problem!")
// Which will cause messages like:
//		C:\Source\Project\main.cpp(47): Reminder: Fix this problem!
// to show up during compiles.  Note that you can NOT use the
// words "error" or "warning" in your reminders, since it will
// make the IDE think it should abort execution.  You can double
// click on these messages and jump to the line in question.
#define TO_STRING(Expression) #Expression 
#define INVOKE_UNARY(UnaryFunction, Argument) UnaryFunction(Argument)

#define WarningHeader(WarningCode)			\
	__FILE__ "(" INVOKE_UNARY(TO_STRING, __LINE__) ") : Warning " WarningCode " : "

#define WARNING(WarningCode, WarningText) __pragma(message(WarningHeader(WarningCode) WarningText))

#define TODO(WarningText) WARNING("TODO", WarningText)
#define HACK(WarningText) WARNING("HACK", WarningText)
