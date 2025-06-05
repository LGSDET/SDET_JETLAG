#include <gtest/gtest.h>
#include "../HealthMonitor_Comm_Parsing.h"

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_Valid) {
    THealthMonitorCommunication comm;
    CPUMetricData data = comm.ParseCPUMetric("55.5/100");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.usage, 55.5);
}

TEST(HealthMonitorCommParsingTest, ParseCPUMetric_Invalid) {
    THealthMonitorCommunication comm;
    CPUMetricData data = comm.ParseCPUMetric("invalid");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_Valid) {
    THealthMonitorCommunication comm;
    MemoryMetricData data = comm.ParseMemoryMetric("512/1024");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.currentUsage, 512);
    EXPECT_EQ(data.totalMemory, 1024);
}

TEST(HealthMonitorCommParsingTest, ParseMemoryMetric_Invalid) {
    THealthMonitorCommunication comm;
    MemoryMetricData data = comm.ParseMemoryMetric("abc");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_Valid) {
    THealthMonitorCommunication comm;
    TemperatureMetricData data = comm.ParseTemperatureMetric("45.2/85.0");
    EXPECT_TRUE(data.isValid);
    EXPECT_DOUBLE_EQ(data.temperature, 45.2);
    EXPECT_DOUBLE_EQ(data.maxTemperature, 85.0);
}

TEST(HealthMonitorCommParsingTest, ParseTemperatureMetric_Invalid) {
    THealthMonitorCommunication comm;
    TemperatureMetricData data = comm.ParseTemperatureMetric("bad");
    EXPECT_FALSE(data.isValid);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_Valid) {
    THealthMonitorCommunication comm;
    DiskMetricData data = comm.ParseDiskMetric("77");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.usagePercent, 77);

    DiskMetricData data2 = comm.ParseDiskMetric("88/100");
    EXPECT_TRUE(data2.isValid);
    EXPECT_EQ(data2.usagePercent, 88);
}

TEST(HealthMonitorCommParsingTest, ParseDiskMetric_Invalid) {
    THealthMonitorCommunication comm;
    DiskMetricData data = comm.ParseDiskMetric("abc");
    EXPECT_TRUE(data.isValid); // usagePercent will be 0, but isValid is set to true
    EXPECT_EQ(data.usagePercent, 0);
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_DaysAndTime) {
    THealthMonitorCommunication comm;
    UptimeMetricData data = comm.ParseUptimeMetric("2d 12:34:56");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 2);
    EXPECT_EQ(data.timeStr, "12:34:56");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_OnlyTime) {
    THealthMonitorCommunication comm;
    UptimeMetricData data = comm.ParseUptimeMetric("05:00:00");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "05:00:00");
}

TEST(HealthMonitorCommParsingTest, ParseUptimeMetric_Invalid) {
    THealthMonitorCommunication comm;
    UptimeMetricData data = comm.ParseUptimeMetric("");
    EXPECT_TRUE(data.isValid);
    EXPECT_EQ(data.days, 0);
    EXPECT_EQ(data.timeStr, "");
}