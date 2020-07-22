add_library(tests_main OBJECT
    main.tests.cc
)

target_link_libraries(tests_main
    PUBLIC
        CONAN_PKG::catch2
)

target_compile_features(tests_main
    PUBLIC
        cxx_std_17
)

set_target_properties(tests_main PROPERTIES
	UNITY_BUILD OFF # Single-file target
)
