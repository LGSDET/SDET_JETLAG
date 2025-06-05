cmake_minimum_required(VERSION 3.10)
project(ADS_B_Display)

# C++ 표준 설정
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# 컴파일러 경고 설정
if(MSVC)
    add_compile_options(/W4)
else()
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# VCL 라이브러리 경로 설정 (실제 경로로 수정 필요)
set(VCL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/vcl_include)

# Google Test 설정
include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.13.0.zip
)
FetchContent_MakeAvailable(googletest)

# 메인 프로그램 소스 파일
set(SOURCES
    HealthMonitor_Comm_Parsing.cpp
    HealthMonitor_UI.cpp
    HealthMonitor_Alert.cpp
    HealthMonitor_Comm_TCPSocket.cpp
)

# 메인 프로그램 헤더 파일
set(HEADERS
    HealthMonitor_MetricData.h
    HealthMonitor_Comm_Parsing.h
    HealthMonitor_UI.h
    HealthMonitor_Alert.h
    HealthMonitor_Comm_TCPSocket.h
)

# 메인 프로그램 타겟
add_executable(health_monitor ${SOURCES} ${HEADERS})
target_include_directories(health_monitor PRIVATE ${VCL_INCLUDE_DIR})

# 테스트 소스 파일
set(TEST_SOURCES
    tests/HealthMonitor_Alert_test.cpp
    tests/HealthMonitor_Comm_Parsing_test.cpp
    HealthMonitor_Comm_Parsing.cpp  # 테스트에 필요한 실제 구현 파일
    HealthMonitor_Alert.cpp  # Alert 테스트에 필요한 실제 구현 파일
)

# 테스트 실행 파일 타겟
add_executable(health_monitor_test ${TEST_SOURCES})
target_link_libraries(health_monitor_test PRIVATE GTest::gtest_main)
target_include_directories(health_monitor_test PRIVATE ${CMAKE_SOURCE_DIR} ${VCL_INCLUDE_DIR})

# 테스트 활성화
include(GoogleTest)
gtest_discover_tests(health_monitor_test) 