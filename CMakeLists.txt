cmake_minimum_required(VERSION 3.27)
project(functional)
include(FetchContent)
FetchContent_Declare(
    asmjit
    GIT_REPOSITORY https://github.com/asmjit/asmjit
    GIT_TAG master
)

set(CMAKE_CXX_STANDARD 23)

add_executable(functional main.cpp)
FetchContent_MakeAvailable(asmjit)

target_link_libraries(functional PRIVATE asmjit)
