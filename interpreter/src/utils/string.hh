#pragma once

#include <string>
#include <array>
#include <cassert>
#include <filesystem>
#include <optional>

template <typename T>
auto to_string(T const & t) noexcept -> decltype(std::to_string(t))
{
	return std::to_string(t);
}

constexpr auto to_string(std::string const & s) noexcept -> std::string const &
{
	return s;
}

inline auto to_string(char const * s) noexcept -> std::string
{
	return s;
}

inline auto to_string(std::string_view s) noexcept -> std::string
{
	return std::string(s);
}

inline auto to_string(bool b) noexcept -> std::string
{
	return b ? "true" : "false";
}

inline auto to_string(char c) noexcept -> std::string
{
	return std::string(1, c);
}

inline auto to_string(std::filesystem::path p) noexcept -> std::string
{
	return p.string();
}

template <typename ... Ts>
auto join(Ts const & ... ts) noexcept -> std::string
{
	return (to_string(ts) + ...);
}

template <size_t N = 256>
auto make_null_terminated(std::string_view str) noexcept -> std::array<char, N>
{
	assert(str.size() < N);
	std::array<char, N> buffer;
	memcpy(buffer.data(), str.data(), str.size());
	buffer[str.size()] = '\0';
	return buffer;
}

auto is_null_terminated(std::string_view str) noexcept -> bool;
auto load_whole_file(std::string_view path) noexcept -> std::optional<std::string>;
auto load_whole_file(std::filesystem::path path) noexcept -> std::optional<std::string>;
auto replace(std::string_view string, std::string_view old_substr, std::string_view new_substr) noexcept -> std::string;

auto make_string_view(char const first[], char const last[]) noexcept -> std::string_view;
auto begin_ptr(std::string_view sv) noexcept -> char const *;
auto end_ptr(std::string_view sv) noexcept -> char const *;

auto count_lines(std::string_view text) noexcept -> int;
auto split_first_line(std::string_view text) noexcept -> std::pair<std::string_view, std::string_view>;
auto extract_first_line(std::string_view & text) noexcept -> std::string_view;
auto ignore_empty_lines(std::string_view text) noexcept -> std::string_view; // Removes all empty lines from the beginning.
auto ignore_starting_whitespace(std::string_view text) noexcept -> std::string_view;
auto ignore_trailing_whitespace(std::string_view text) noexcept -> std::string_view;
auto ignore_whitespace(std::string_view text) noexcept -> std::string_view;
auto starts_with(std::string_view string, std::string_view prefix) noexcept -> bool;
auto is_contained_in(std::string_view string, std::string_view substr) noexcept -> bool;

constexpr auto is_whitespace = [](char c) noexcept -> bool { return c == ' ' || c == '\t'; };
constexpr auto is_whitespace_or_newline = [](char c) noexcept -> bool { return c == ' ' || c == '\t' || c == '\n'; };
constexpr auto is_number = [](char c) noexcept -> bool { return (c >= '0' && c <= '9'); };
constexpr auto is_letter = [](char c) noexcept -> bool { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); };
