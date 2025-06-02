#include "HealthMonitorMetrics.h"
#include <gtest/gtest.h>
#include <iomanip>
#include <sstream>
#include <zlib.h>

// CPU 메트릭 파싱 테스트
TEST(HealthMonitorTest, ParseCPUMetric) {
  // 정상적인 케이스
  CPUMetricData result1 = HealthMonitorMetrics::ParseCPUMetric("45.5/100.0");
  EXPECT_TRUE(result1.isValid);
  EXPECT_DOUBLE_EQ(45.5, result1.usage);

  // 잘못된 형식
  CPUMetricData result2 = HealthMonitorMetrics::ParseCPUMetric("invalid");
  EXPECT_FALSE(result2.isValid);

  // 빈 문자열
  CPUMetricData result3 = HealthMonitorMetrics::ParseCPUMetric("");
  EXPECT_FALSE(result3.isValid);
}

// 메모리 메트릭 파싱 테스트
TEST(HealthMonitorTest, ParseMemoryMetric) {
  // 정상적인 케이스
  MemoryMetricData result1 =
      HealthMonitorMetrics::ParseMemoryMetric("2048/8192");
  EXPECT_TRUE(result1.isValid);
  EXPECT_EQ(2048, result1.currentUsage);
  EXPECT_EQ(8192, result1.totalMemory);

  // 잘못된 형식
  MemoryMetricData result2 = HealthMonitorMetrics::ParseMemoryMetric("invalid");
  EXPECT_FALSE(result2.isValid);

  // 빈 문자열
  MemoryMetricData result3 = HealthMonitorMetrics::ParseMemoryMetric("");
  EXPECT_FALSE(result3.isValid);
}

// 온도 메트릭 파싱 테스트
TEST(HealthMonitorTest, ParseTemperatureMetric) {
  // 정상적인 케이스
  TemperatureMetricData result1 =
      HealthMonitorMetrics::ParseTemperatureMetric("65.5/85.0");
  EXPECT_TRUE(result1.isValid);
  EXPECT_DOUBLE_EQ(65.5, result1.temperature);
  EXPECT_DOUBLE_EQ(85.0, result1.maxTemperature);

  // 잘못된 형식
  TemperatureMetricData result2 =
      HealthMonitorMetrics::ParseTemperatureMetric("invalid");
  EXPECT_FALSE(result2.isValid);

  // 빈 문자열
  TemperatureMetricData result3 =
      HealthMonitorMetrics::ParseTemperatureMetric("");
  EXPECT_FALSE(result3.isValid);
}

// 디스크 메트릭 파싱 테스트
TEST(HealthMonitorTest, ParseDiskMetric) {
  // 정상적인 케이스
  DiskMetricData result1 = HealthMonitorMetrics::ParseDiskMetric("75/100");
  EXPECT_TRUE(result1.isValid);
  EXPECT_EQ(75, result1.usagePercent);

  // 잘못된 형식
  DiskMetricData result2 = HealthMonitorMetrics::ParseDiskMetric("invalid");
  EXPECT_FALSE(result2.isValid);

  // 빈 문자열
  DiskMetricData result3 = HealthMonitorMetrics::ParseDiskMetric("");
  EXPECT_FALSE(result3.isValid);
}

// Uptime 메트릭 파싱 테스트
TEST(HealthMonitorTest, ParseUptimeMetric) {
  // 일수가 있는 케이스
  UptimeMetricData result1 =
      HealthMonitorMetrics::ParseUptimeMetric("5d 12:34:56");
  EXPECT_TRUE(result1.isValid);
  EXPECT_EQ(5, result1.days);
  EXPECT_EQ("12:34:56", result1.timeStr);

  // 일수가 없는 케이스
  UptimeMetricData result2 =
      HealthMonitorMetrics::ParseUptimeMetric("12:34:56");
  EXPECT_TRUE(result2.isValid);
  EXPECT_EQ(0, result2.days);
  EXPECT_EQ("12:34:56", result2.timeStr);

  // 잘못된 형식
  UptimeMetricData result3 = HealthMonitorMetrics::ParseUptimeMetric("invalid");
  EXPECT_FALSE(result3.isValid);
}

// CRC32 검증 테스트
TEST(HealthMonitorTest, VerifyCRC32) {
  // 유효한 CRC32
  std::string validData = "CPU:45.5/100.0|MEM:2048/8192|TEMP:65.5/85.0|DISK:75/"
                          "100|UPTIME:5d 12:34:56";
  std::string crcData = validData + "|CRC=";

  // CRC32 계산
  uLong crc = crc32(0L, Z_NULL, 0);
  crc = crc32(crc, (const Bytef *)validData.c_str(), validData.length());

  // 16진수 문자열로 변환
  std::stringstream ss;
  ss << std::hex << std::setw(8) << std::setfill('0') << crc;
  std::string calculatedCRC = ss.str();

  // 실제 CRC32 값으로 테스트
  EXPECT_TRUE(HealthMonitorMetrics::VerifyCRC32(crcData + calculatedCRC,
                                                calculatedCRC));

  // 잘못된 CRC32
  EXPECT_FALSE(
      HealthMonitorMetrics::VerifyCRC32(crcData + calculatedCRC, "87654321"));

  // CRC32가 없는 데이터
  std::string invalidData = "CPU:45.5/100.0|MEM:2048/8192";
  EXPECT_FALSE(HealthMonitorMetrics::VerifyCRC32(invalidData, calculatedCRC));
}