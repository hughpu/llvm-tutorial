
file(GLOB srcs CONFIGURE_DEPENDS src/*.cpp include/kaleidoscope/*.h)

add_executable(kaleidoscope ${srcs})
target_include_directories(kaleidoscope PUBLIC include)
target_link_libraries(kaleidoscope PUBLIC ${LLVM_LIBS})