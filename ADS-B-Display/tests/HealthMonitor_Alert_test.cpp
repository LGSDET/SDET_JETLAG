#include <gtest/gtest.h>
#include "../HealthMonitor_Alert.h"

// CPU Usage BVA
TEST(THealthMonitorAlertTest, CPUAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    CPUMetricData data{79.0, true};
    EXPECT_FALSE(alert.IsCPUAlert(data));
    EXPECT_EQ(alert.GetCPUAlertType(data), AlertType::NONE);
}
TEST(THealthMonitorAlertTest, CPUAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    CPUMetricData data{80.0, true};
    EXPECT_TRUE(alert.IsCPUAlert(data));
    EXPECT_EQ(alert.GetCPUAlertType(data), AlertType::CPU_HIGH);
}

// Memory Usage BVA
TEST(THealthMonitorAlertTest, MemoryAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    MemoryMetricData data{790, 1000, true}; // 79%
    EXPECT_FALSE(alert.IsMemoryAlert(data));
    EXPECT_EQ(alert.GetMemoryAlertType(data), AlertType::NONE);
}
TEST(THealthMonitorAlertTest, MemoryAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    MemoryMetricData data{800, 1000, true}; // 80%
    EXPECT_TRUE(alert.IsMemoryAlert(data));
    EXPECT_EQ(alert.GetMemoryAlertType(data), AlertType::MEMORY_INSUFFICIENT);
}

// CPU Temperature BVA
TEST(THealthMonitorAlertTest, TemperatureAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{69.0, 100.0, true};
    EXPECT_FALSE(alert.IsTemperatureAlert(data));
    EXPECT_EQ(alert.GetTemperatureAlertType(data), AlertType::NONE);
}
TEST(THealthMonitorAlertTest, TemperatureAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{70.0, 100.0, true};
    EXPECT_TRUE(alert.IsTemperatureAlert(data));
    EXPECT_EQ(alert.GetTemperatureAlertType(data), AlertType::HIGH_TEMPERATURE);
}

// Disk Usage BVA
TEST(THealthMonitorAlertTest, DiskAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    DiskMetricData data{89, true};
    EXPECT_FALSE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertType(data), AlertType::NONE);
}
TEST(THealthMonitorAlertTest, DiskAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    DiskMetricData data{90, true};
    EXPECT_TRUE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertType(data), AlertType::DISK_SPACE_LOW);
}