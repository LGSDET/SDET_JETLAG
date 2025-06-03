cmake_minimum_required(VERSION 3.10)
project(HealthMonitorTests)

# C++ 표준 설정
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Google Test 설정
include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.13.0.zip
)
FetchContent_MakeAvailable(googletest)

# zlib 라이브러리 찾기
find_package(ZLIB REQUIRED)

# 테스트 소스 파일
set(TEST_SOURCES
    test_health_monitor.cpp
    TestMetricParser.cpp
)

# 테스트 실행 파일 타겟
add_executable(health_monitor_test ${TEST_SOURCES})
target_link_libraries(health_monitor_test 
    PRIVATE 
    GTest::gtest_main
    ZLIB::ZLIB
)

# 테스트 활성화
include(GoogleTest)
gtest_discover_tests(health_monitor_test) 