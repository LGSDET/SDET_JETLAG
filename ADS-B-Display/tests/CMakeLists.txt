cmake_minimum_required(VERSION 3.10)
project(HealthMonitorTests)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

if(MSVC)
  set(CMAKE_C_FLAGS_DEBUG   "/MDd /Zi /Ob0 /Od /RTC1" CACHE STRING "" FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG "/MDd /Zi /Ob0 /Od /RTC1" CACHE STRING "" FORCE)
  set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreadedDebugDLL" CACHE STRING "" FORCE)
endif()

add_subdirectory(
    ${CMAKE_SOURCE_DIR}/../external/googletest
    ${CMAKE_BINARY_DIR}/external/googletest-build
)

set(TEST_SOURCES
    HealthMonitor_Alert_test.cpp
    HealthMonitor_Comm_Parsing_test.cpp
    ../HealthMonitor_Alert.cpp
    ../HealthMonitor_Comm_Parsing.cpp
)

# GCC/Clang에서만 커버리지 옵션 추가
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(--coverage)
    add_link_options(--coverage)
endif()

add_executable(health_monitor_test ${TEST_SOURCES})
target_link_libraries(health_monitor_test PRIVATE gtest_main)
target_include_directories(health_monitor_test PRIVATE ${CMAKE_SOURCE_DIR}/..)

include(GoogleTest)
gtest_discover_tests(health_monitor_test)