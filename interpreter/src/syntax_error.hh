#pragma once

[[noreturn]] auto raise_syntax_error(char const msg[]) noexcept -> void;
auto raise_syntax_error_if_not(bool condition, char const msg[]) noexcept -> void;
