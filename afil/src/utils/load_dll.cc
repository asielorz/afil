#include "load_dll.hh"
#include "compatibility.hh"
#include "string.hh"

#if AFIL_WINDOWS
#	include <Windows.h>
#else
#	include <dlfcn.h>
#endif

DLL::~DLL() noexcept
{
	if (handle)
	{
#if AFIL_WINDOWS
		FreeLibrary((HMODULE)handle);
#else
		dlclose(handle);
#endif
	}
}

DLL load_library(std::string_view path)
{
	char null_terminated_path[256];
	memcpy(null_terminated_path, path.data(), path.size());

	constexpr char dynamic_lib_extension[] =
#if AFIL_WINDOWS
		".dll";
#else
		".so";
#endif

	memcpy(null_terminated_path + path.size(), dynamic_lib_extension, sizeof(dynamic_lib_extension));

#if AFIL_WINDOWS
	return DLL(GetModuleHandleA(null_terminated_path));
#else
	return DLL(dlopen(null_terminated_path, RTLD_LAZY));
#endif
}

void const * find_symbol(DLL const & dll, std::string_view symbol_name)
{
	auto const null_terminated_symbol_name = make_null_terminated(symbol_name);

#if AFIL_WINDOWS
	return (void const *)GetProcAddress((HMODULE)dll.handle, null_terminated_symbol_name.data());
#else
	return dlsym(dll.handle, null_terminated_symbol_name.data());
#endif
}
