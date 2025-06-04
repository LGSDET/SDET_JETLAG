#include <gtest/gtest.h>
#include <string>
#include "../HealthMonitor_Alert.h"

TEST(THealthMonitorAlertTest, CPUAlertTriggered) {
    THealthMonitorAlert alert;
    CPUMetricData data{true, 85.0};
    EXPECT_TRUE(alert.IsCPUAlert(data));
    std::string msg = alert.GetCPUAlertMessage(data);
    EXPECT_NE(msg.find("CPU usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, CPUAlertNotTriggered) {
    THealthMonitorAlert alert;
    CPUMetricData data{true, 50.0};
    EXPECT_FALSE(alert.IsCPUAlert(data));
    EXPECT_EQ(alert.GetCPUAlertMessage(data), "");
}

TEST(THealthMonitorAlertTest, MemoryAlertTriggered) {
    THealthMonitorAlert alert;
    MemoryMetricData data{true, 950, 1000};
    EXPECT_TRUE(alert.IsMemoryAlert(data));
    std::string msg = alert.GetMemoryAlertMessage(data);
    EXPECT_NE(msg.find("Memory usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, MemoryAlertNotTriggered) {
    THealthMonitorAlert alert;
    MemoryMetricData data{true, 500, 1000};
    EXPECT_FALSE(alert.IsMemoryAlert(data));
    EXPECT_EQ(alert.GetMemoryAlertMessage(data), "");
}

TEST(THealthMonitorAlertTest, TemperatureAlertTriggered) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{true, 80.0};
    EXPECT_TRUE(alert.IsTemperatureAlert(data));
    std::string msg = alert.GetTemperatureAlertMessage(data);
    EXPECT_NE(msg.find("CPU temperature"), std::string::npos);
}

TEST(THealthMonitorAlertTest, TemperatureAlertNotTriggered) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{true, 60.0};
    EXPECT_FALSE(alert.IsTemperatureAlert(data));
    EXPECT_EQ(alert.GetTemperatureAlertMessage(data), "");
}

TEST(THealthMonitorAlertTest, DiskAlertTriggered) {
    THealthMonitorAlert alert;
    DiskMetricData data{true, 95};
    EXPECT_TRUE(alert.IsDiskAlert(data));
    std::string msg = alert.GetDiskAlertMessage(data);
    EXPECT_NE(msg.find("Disk usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, DiskAlertNotTriggered) {
    THealthMonitorAlert alert;
    DiskMetricData data{true, 50};
    EXPECT_FALSE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertMessage(data), "");
}