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
	set_target_properties(afil_tests
		PROPERTIES
			# Enable message box and system("")
			VS_DEBUGGER_COMMAND_ARGUMENTS "--afil-interactive-tests"
	)
endif()

# Automatically enable catch2 to generate ctest targets

foreach(catch2_root CONAN_CATCH2_ROOT CONAN_CATCH2_ROOT_DEBUG CONAN_CATCH2_ROOT_RELEASE CONAN_CATCH2_ROOT_RELWITHDEBINFO CONAN_CATCH2_ROOT_MINSIZEREL)
	if (EXISTS ${${catch2_root}})
		include(${${catch2_root}}/lib/cmake/Catch2/Catch.cmake)
		break()
	endif()
endforeach()

# automatically discover tests that are defined in catch.
# Set TEST_PREFIX to whatever you want
catch_discover_tests(
	afil_tests
	TEST_PREFIX "tests."
)
