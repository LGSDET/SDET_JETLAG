cmake_minimum_required(VERSION 3.14)
project(ADS-B-Display-Tests)

# C++ 표준 설정
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Google Test 설정
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG release-1.12.1
)

# Google Test 빌드 설정
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)
set(BUILD_GTEST ON CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(googletest)

# zlib 찾기
find_package(ZLIB REQUIRED)

# 컴파일러 경고 설정
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# 테스트 활성화
enable_testing()

# 테스트 실행 파일 설정
add_executable(unit_tests
    test_health_monitor.cpp
    HealthMonitorMetrics.cpp
)

# 헤더 파일 경로 추가
target_include_directories(unit_tests PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Google Test와 zlib 링크
target_link_libraries(unit_tests PRIVATE 
    gtest 
    gtest_main
    ZLIB::ZLIB
)

# 테스트 등록
add_test(NAME unit_tests COMMAND unit_tests) 