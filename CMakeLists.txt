cmake_minimum_required(VERSION 3.10)

project(Kaleidoscope VERSION 0.1 LANGUAGES CXX C)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
find_package(LLVM REQUIRED CONFIG)

# Include LLVM directories and definitions
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

# Map LLVM components to library names
llvm_map_components_to_libnames(LLVM_LIBS core support)
message(STATUS "LLVM_LIBS: ${LLVM_LIBS}")
message(STATUS "LLVM_INCLUDE_DIRS: ${LLVM_INCLUDE_DIRS}")
message(STATUS "LLVM_DEFINITIONS: ${LLVM_DEFINITIONS}")


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED true)
set(CMAKE_CXX_EXTENSIONS OFF)


add_subdirectory(kaleidoscope)
