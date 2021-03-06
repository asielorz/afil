#pragma once

#include <string_view>
#include <utility>

struct DLL
{
	DLL(void * h) noexcept : handle(h) {}
	DLL(DLL const & other) = delete;
	DLL & operator = (DLL const & other) = delete;
	DLL(DLL && other) noexcept : handle(std::exchange(other.handle, nullptr)) {}
	DLL & operator = (DLL && other) noexcept { handle = std::exchange(other.handle, nullptr); return *this; }
	~DLL() noexcept;

	void * handle;
};

DLL load_library(std::string_view path);
void const * find_symbol(DLL const & dll, std::string_view symbol_name);
