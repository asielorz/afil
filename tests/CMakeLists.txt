add_subdirectory(tests_main)

add_executable(afil_tests
	src/algorithm.tests.cc
	src/c_transpiler.tests.cc
	src/callc.tests.cc
	src/interpreter.tests.cc
	src/span.tests.cc
	src/string.tests.cc
	src/value_ptr.tests.cc
	src/variant.tests.cc
)

target_include_directories(afil_lib
    PUBLIC
        "${CMAKE_SOURCE_DIR}/afil/src/"
)

target_link_libraries(afil_tests
	PRIVATE
		afil_lib
		tests_main
		CONAN_PKG::catch2
)

target_compile_definitions(afil_tests
	PRIVATE
		$<$<CONFIG:Debug>:AFIL_DEBUG>
		AFIL_BUILD_TYPE=$<CONFIG>
)

if (MSVC)
	target_compile_options(afil_tests
		PRIVATE
			/MP # Multi-core compilation
  	)
endif()

add_test(
	NAME unit_tests
	COMMAND afil_tests
)
