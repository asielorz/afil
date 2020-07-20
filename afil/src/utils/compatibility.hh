#pragma once

#include <cstddef>
#include <cstdio>
#include <cstdlib>

#define AFIL_STRINGIZE_IMPL(x) #x
#define AFIL_STRINGIZE(x) AFIL_STRINGIZE_IMPL(x)

#define AFIL_INVOKE_UNARY(unary_function, argument) unary_function(argument)

#define AFIL_EXPAND_IMPL(x) x
#define AFIL_EXPAND(x) AFIL_EXPAND_IMPL(x)

#define AFIL_MESSAGE_LEVEL_DISABLED 0
#define AFIL_MESSAGE_LEVEL_INFO 1
#define AFIL_MESSAGE_LEVEL_DEBUG 2
#define AFIL_MESSAGE_LEVEL_DIAG 3

#ifndef AFIL_MESSAGE_LEVEL
#	define AFIL_MESSAGE_LEVEL AFIL_MESSAGE_LEVEL_INFO
#endif

#define AFIL_MSVC false
#define AFIL_GCC false
#define AFIL_CLANG false

// clang seems to also define _MSC_VER and __GNUC__
#ifdef __clang__
#	undef AFIL_CLANG
#	define AFIL_CLANG true
#elif defined __GNUC__
#	undef AFIL_GCC
#	define AFIL_GCC true
#elif defined _MSC_VER
#	undef AFIL_MSVC
#	define AFIL_MSVC true
#endif

#if AFIL_MSVC
#	define AFIL_MSVC_PRAGMA(x) __pragma(x)
#	if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_DIAG // Check manually because AFIL_MESSAGE_LEVEL_DIAG is not yet defined
#		pragma message("MSVC detected: v" AFIL_STRINGIZE(_MSC_VER))
#	endif
#else
#	define AFIL_MSVC_PRAGMA(x)
#endif

#if AFIL_GCC
#	define AFIL_GCC_PRAGMA(x) _Pragma(#x)
#	if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_DIAG // Check manually because AFIL_MESSAGE_LEVEL_DIAG is not yet defined
#		pragma message("gcc detected: v" AFIL_STRINGIZE(__GNUC__) "." AFIL_STRINGIZE(__GNUC_MINOR__) "." AFIL_STRINGIZE(__GNUC_PATCHLEVEL__))
#	endif
#else
#	define AFIL_GCC_PRAGMA(x)
#endif

#if AFIL_CLANG
#	define AFIL_CLANG_PRAGMA(x) _Pragma(#x)
#	if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_DIAG // Check manually because AFIL_MESSAGE_LEVEL_DIAG is not yet defined
#		pragma message("clang detected: v" AFIL_STRINGIZE(__clang_major__) "." AFIL_STRINGIZE(__clang_minor__) "." AFIL_STRINGIZE(__clang_patchlevel__))
#	endif
#else
#	define AFIL_CLANG_PRAGMA(x)
#endif

#if AFIL_MSVC
#	define AFIL_PRAGMA_MESSAGE(x) __pragma(message(x))
#else
#	define AFIL_PRAGMA_MESSAGE(x) _Pragma(AFIL_STRINGIZE(message(x)))
#endif

#if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_INFO
#	define AFIL_MESSAGE_INFO(x) AFIL_PRAGMA_MESSAGE(x)
#else
#	define AFIL_MESSAGE_INFO(x)
#endif

#if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_DEBUG
#	define AFIL_MESSAGE_DEBUG(x) AFIL_PRAGMA_MESSAGE(x)
#else
#	define AFIL_MESSAGE_DEBUG(x)
#endif

#if AFIL_MESSAGE_LEVEL >= AFIL_MESSAGE_LEVEL_DIAG
#	define AFIL_MESSAGE_DIAG(x) AFIL_PRAGMA_MESSAGE(x)
#else
#	define AFIL_MESSAGE_DIAG(x)
#endif

#define AFIL_WARNING_PUSH() \
	AFIL_MSVC_PRAGMA(warning(push))\
	AFIL_GCC_PRAGMA(GCC diagnostic push)\
	AFIL_CLANG_PRAGMA(clang diagnostic push)

#define AFIL_WARNING_POP() \
	AFIL_MSVC_PRAGMA(warning(pop))\
	AFIL_GCC_PRAGMA(GCC diagnostic pop)\
	AFIL_CLANG_PRAGMA(clang diagnostic pop)

#define AFIL_IGNORE_WARNING_NAMELESS_STRUCT() \
	AFIL_WARNING_PUSH()	\
	AFIL_MSVC_PRAGMA(warning(disable : 4201)) /* nameless struct/union */ \
	AFIL_GCC_PRAGMA(GCC diagnostic ignored "-Wpedantic") /* there is no more specific flag */ \
	AFIL_CLANG_PRAGMA(clang diagnostic ignored "-Wgnu-anonymous-struct") \
	AFIL_CLANG_PRAGMA(clang diagnostic ignored "-Wnested-anon-types")

#ifdef _WIN32
#	define AFIL_WINDOWS true
#else
#	define AFIL_WINDOWS false
#endif

// We use a macro so that MSVC's sprintf_s can deduce the size of 'buffer'
#ifdef _MSC_VER
#	define AFIL_SPRINTF(buffer, format, ...) sprintf_s(buffer, format, __VA_ARGS__)
#else
#	define AFIL_SPRINTF(buffer, format, ...) std::sprintf(buffer, format, __VA_ARGS__)
#endif

inline void system_pause()
{
#if AFIL_WINDOWS
	std::system("pause");
#else
	std::fputs("Press any key to continue . . . ", stderr);
	std::getchar();
#endif
}

#if AFIL_MSVC
#	define AFIL_FORCEINLINE __forceinline
#elif AFIL_CLANG || AFIL_GCC
#	define AFIL_FORCEINLINE __attribute__((always_inline)) inline
#else
#	define AFIL_FORCEINLINE inline
#endif
