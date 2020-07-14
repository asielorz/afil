add_executable(afil
	src/main.cc
)

target_link_libraries(afil
	PRIVATE
		afil_lib
)

target_include_directories(afil
    PUBLIC
        "${CMAKE_SOURCE_DIR}/afil/src/"
)

target_compile_definitions(afil
	PRIVATE
		$<$<CONFIG:Debug>:AFIL_DEBUG>
		AFIL_BUILD_TYPE=$<CONFIG>
)

set_target_properties(afil PROPERTIES
	UNITY_BUILD OFF # Single-file target
)
