#include <gtest/gtest.h>
#include "../HealthMonitor_Comm_Parsing.h"

// ---- CPUMetricData ----
TEST(HealthMonitorCommParsingTest, ParseCPUMetric_ValidBoundaryZero) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("0/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.usage, 0.0);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_ValidBoundaryHundred) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("100/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.usage, 100.0);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_NegativeValue) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("-1/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.usage, -1.0);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_EmptyInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_NoSlash) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("100");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_AlphaInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseCPUMetric("abc/def");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.usage, 0.0);
}

// ---- MemoryMetricData ----
TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_ValidBoundaryZero) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseMemoryMetric("0/1024");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.currentUsage, 0);
    EXPECT_EQ(data.totalMemory, 1024);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_ValidBoundaryMax) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseMemoryMetric("1024/1024");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.currentUsage, 1024);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_EmptyInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseMemoryMetric("");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_NoSlash) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseMemoryMetric("1024");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_AlphaInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseMemoryMetric("abc/xyz");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.currentUsage, 0);
    EXPECT_EQ(data.totalMemory, 0);
}

// ---- TemperatureMetricData ----
TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_ValidBoundaryZero) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseTemperatureMetric("0/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.temperature, 0.0);
    EXPECT_DOUBLE_EQ(data.maxTemperature, 100.0);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_ValidBoundaryMax) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseTemperatureMetric("100/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.temperature, 100.0);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_EmptyInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseTemperatureMetric("");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_NoSlash) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseTemperatureMetric("100");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_AlphaInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseTemperatureMetric("abc/xyz");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.temperature, 0.0);
    EXPECT_DOUBLE_EQ(data.maxTemperature, 0.0);
}

// ---- DiskMetricData ----
TEST(HealthMonitorCommParsingTest, ParseDiskMetric_ValidBoundaryZero) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseDiskMetric("0");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, 0);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_ValidBoundaryHundred) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseDiskMetric("100");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, 100);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_NegativeValue) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseDiskMetric("-1");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, -1);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_EmptyInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseDiskMetric("");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, 0);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_AlphaInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseDiskMetric("abc");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, 0);
}

// ---- UptimeMetricData ----
TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_ValidInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseUptimeMetric("1d 00:00:00");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 1);
    EXPECT_EQ(data.timeStr, "00:00:00");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_ZeroDay) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseUptimeMetric("0d 23:59:59");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "23:59:59");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_OnlyTime) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseUptimeMetric("05:00:00");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "05:00:00");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_EmptyInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseUptimeMetric(" ");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_AlphaInput) {
    THealthMonitorCommunication comm;
    auto data = comm.ParseUptimeMetric("abc");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "abc");
}