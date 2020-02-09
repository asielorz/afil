#include "load_dll.hh"
#include "string.hh"
#include "Windows.h"

DLL::~DLL() noexcept
{
	if (handle)
		FreeLibrary((HMODULE)handle);
}

DLL load_library(std::string_view path)
{
	auto const null_terminated_path = make_null_terminated(path);
	return DLL(GetModuleHandle(null_terminated_path.data()));
}

void const * find_symbol(DLL const & dll, std::string_view symbol_name)
{
	auto const null_terminated_symbol_name = make_null_terminated(symbol_name);
	return (void const *)GetProcAddress((HMODULE)dll.handle, null_terminated_symbol_name.data());
}
