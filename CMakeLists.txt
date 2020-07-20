cmake_minimum_required(VERSION 3.10)
project(afil CXX)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)
set(CMAKE_CXX_EXTENSIONS OFF)

option(ENABLE_IPO "Enable Iterprocedural Optimization, aka Link Time Optimization (LTO)" OFF)
option(FAIL_IF_IPO_NOT_SUPPORTED "If ENABLE_IPO is true, and it is not supported, consider it a failure" ON)

if(ENABLE_IPO)
	include(CheckIPOSupported)
	check_ipo_supported(RESULT ipo_supported OUTPUT check_ipo_supported_output LANGUAGES CXX)
	
	if(ipo_supported)
		set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
	else()
		if (FAIL_IF_IPO_NOT_SUPPORTED)
			message(FATAL_ERROR "IPO is not supported: ${check_ipo_supported_output}")
		else()
			message(WARNING "IPO is not supported: ${check_ipo_supported_output}")
		endif()
	endif()
endif()

set(CMAKE_CONFIGURATION_TYPES Debug Release)
# Download automatically, you can also just copy the conan.cmake file
if (NOT EXISTS "${CMAKE_BINARY_DIR}/conan.cmake")
	message(STATUS
	  "Downloading conan.cmake from https://github.com/conan-io/cmake-conan"
	)
	file(DOWNLOAD "https://github.com/conan-io/cmake-conan/raw/v0.15/conan.cmake"
	   "${CMAKE_BINARY_DIR}/conan.cmake"
	)
endif()

include(${CMAKE_BINARY_DIR}/conan.cmake)

enable_testing()

#conan_add_remote(
#	NAME bincrafters
#	URL https://api.bintray.com/conan/bincrafters/public-conan
#)

conan_cmake_run(
	CONANFILE tests/conanfile.txt
	BASIC_SETUP
	CMAKE_TARGETS # individual targets to link to
	BUILD missing
)

option(TREAT_WARNINGS_AS_ERRORS "Treat warnings as errors" TRUE)

if (MSVC)
	add_link_options(/time)
	add_compile_options(/W4)

	if (TREAT_WARNINGS_AS_ERRORS)
		add_compile_options(/WX)
	endif()

	if (MSVC_VERSION GREATER_EQUAL 1910) # Visual Studio 2017 or greater
		add_compile_options(
			/permissive-
			/diagnostics:caret
		)
	endif()
endif()

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU") # clang or gcc
	if (TREAT_WARNINGS_AS_ERRORS)
		add_compile_options(-Werror)
	endif()

	add_compile_options(
		-Wall
		-Wextra
		-pedantic
		-Wdouble-promotion
		#-Wconversion
		#-Wnull-dereference
		#-Wsign-conversion
	)
endif()

add_subdirectory(afil)
add_subdirectory(tests)
add_subdirectory(exe)