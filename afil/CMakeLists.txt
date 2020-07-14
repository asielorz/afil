set(UTILS_FILES
	src/utils/algorithm.hh
	src/utils/callc.cc
	src/utils/callc.hh
	src/utils/callc.inl
	src/utils/expected.hh
	src/utils/function_ptr.hh
	src/utils/half.hh
	src/utils/load_dll.cc
	src/utils/load_dll.hh
	src/utils/map.hh
	src/utils/multicomparison.hh
	src/utils/out.hh
	src/utils/overload.hh
	src/utils/span.hh
	src/utils/string.cc
	src/utils/string.hh
	src/utils/unreachable.cc
	src/utils/unreachable.hh
	src/utils/utils.cc
	src/utils/utils.hh
	src/utils/value_ptr.hh
	src/utils/variant.hh
	src/utils/warning_macro.hh
)
set(AFIL_FILES
	src/afil.cc
	src/afil.hh
	src/built_in_structures.hh
	src/c_transpiler.cc
	src/c_transpiler.hh
	src/complete_expression.cc
	src/complete_expression.hh
	src/complete_scope.cc
	src/complete_scope.hh
	src/complete_statement.hh
	src/constexpr.cc
	src/constexpr.hh
	src/function_id.hh
	src/incomplete_expression.cc
	src/incomplete_expression.hh
	src/incomplete_module.cc
	src/incomplete_module.hh
	src/incomplete_scope.cc
	src/incomplete_scope.hh
	src/incomplete_statement.cc
	src/incomplete_statement.hh
	src/interpreter.cc
	src/interpreter.hh
	src/interpreter.inl
	src/lexer.cc
	src/lexer.hh
	src/operator.cc
	src/operator.hh
	src/parser.cc
	src/parser.hh
	src/pretty_print.cc
	src/pretty_print.hh
	src/program.cc
	src/program.hh
	src/scope_stack.hh
	src/syntax_error.cc
	src/syntax_error.hh
	src/template_instantiation.cc
	src/template_instantiation.hh
)

add_library(afil_lib OBJECT
	${UTILS_FILES}
	${AFIL_FILES}
	
	span.natvis
)

source_group("utils" FILES
	${UTILS_FILES}
)
source_group("afil" FILES
	${AFIL_FILES}
)

target_compile_features(afil_lib
	PUBLIC
		cxx_std_17
)

target_compile_definitions(afil_lib
	PRIVATE
		$<$<CONFIG:Debug>:AFIL_DEBUG>
		AFIL_BUILD_TYPE=$<CONFIG>
)

if (MSVC)
	target_compile_options(afil_lib
		PRIVATE
			/bigobj
			/MP # Multi-core compilation
  	)
endif()
