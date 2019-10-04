#pragma once

#include <string>

namespace expr { struct ExpressionTree; }
namespace stmt { struct Statement; }
struct Function;
struct Program;

auto pretty_print(expr::ExpressionTree const & tree, Program const & program) noexcept -> std::string;
auto pretty_print(stmt::Statement const & statement, Program const & program) noexcept -> std::string;
auto pretty_print(Function const & function, Program const & program) noexcept->std::string;
auto pretty_print(Program const & program) noexcept->std::string;
