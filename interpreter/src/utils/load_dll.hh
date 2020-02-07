#pragma  once

#include <string_view>

struct Module;

Module const * get_loaded_module(std::string_view path);
void const * find_symbol(Module const * dll, std::string_view symbol_name);
