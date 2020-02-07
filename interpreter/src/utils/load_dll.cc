#include "load_dll.hh"
#include "string.hh"
#include "Windows.h"

Module const * get_loaded_module(std::string_view path)
{
	auto const null_terminated_path = make_null_terminated(path);
	return (Module const *)GetModuleHandle(null_terminated_path.data());
}

void const * find_symbol(Module const * dll, std::string_view symbol_name)
{
	auto const null_terminated_symbol_name = make_null_terminated(symbol_name);
	return (void const *)GetProcAddress((HMODULE)dll, null_terminated_symbol_name.data());
}

