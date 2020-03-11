#pragma once

#include "incomplete_statement.hh"
#include "syntax_error.hh"
#include "utils/expected.hh"
#include "utils/span.hh"
#include <vector>
#include <string>

/*******************************************************************************************************
Program
	Es un programa ejecutable
	run: Program -> int // argc, argv en un futuro
	Necesita:
		funciones, functiones externas
		tipos, estructuras,
		statements globales,
		tamaño del scope global,
		función main

Module
	Es un archivo parseado
	link: Module a, Module b... -> Program
	Necesita:
		funciones, functiones externas
		tipos, estructuras,
		statements globales,
		scope global,
		función main
		estructuras y funciones template
		El código del archivo (Las templates tienen estructuras incompletas que dependen del código)
		Identificadores de los módulos en los que depende este módulo

Problema:
	Todos los ids son índices, porque hasta ahora había un solo array.
	A partir de ahora va a haber N arrays, uno por módulo.
Posible solución:
	Nuevos tipos de id que tienen un identificador de módulo además de índice en el array.
	Posibilidad: índice del módulo en la lista de módulos de los que depende este + índice en el array.
	link hace la traducción de estos ids intermedios a los tipos de id que estamos usando ahora.
********************************************************************************************************/
#if 1
struct ModuleId
{
	// ????
};

namespace incomplete
{
	struct Module
	{
		std::vector<incomplete::Statement> statements;
		std::string source;
		std::vector<ModuleId> dependencies;
	};
}

namespace complete
{
	struct Module
	{
		std::vector<Type> types;
		std::vector<Struct> structs;
		std::vector<StructTemplate> struct_templates;
		std::vector<OverloadSet> overload_set_types;
		std::vector<Function> functions;
		std::vector<ExternFunction> extern_functions;
		std::vector<FunctionTemplate> function_templates;
		std::vector<Statement> global_initialization_statements;
		Scope global_scope;
		FunctionId main_function = invalid_function_id;
		std::string source;
		std::vector<ModuleId> dependencies;
	};
}

auto semantic_analysis(incomplete::Module incomplete_module, span<complete::Module const> dependencies) -> expected<complete::Module, SyntaxError>;

struct Program;
auto link(span<complete::Module const> modules) -> Program;
#endif
