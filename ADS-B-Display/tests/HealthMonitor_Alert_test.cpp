#include <gtest/gtest.h>
#include "../HealthMonitor_Alert.h"

// 80도 5초 연속: 알람이 와야 함
TEST(THealthMonitorAlertTest, CPUAlert_80Degree_5Seconds_Alert) {
    THealthMonitorAlert alert;
    CPUMetricData data{80.0, true};

    // 첫 호출: 알람이 오면 안 됨
    EXPECT_FALSE(alert.IsCPUAlert(data));

    // 5초 동안 80도 유지
    std::this_thread::sleep_for(std::chrono::seconds(5));

    // 5초 후 다시 호출: 알람이 와야 함
    EXPECT_TRUE(alert.IsCPUAlert(data));
}

// 80도 3초 유지 후 79도로 떨어지면 알람이 오면 안 됨
TEST(THealthMonitorAlertTest, CPUAlert_80Degree_3Seconds_Then79_NoAlert) {
    THealthMonitorAlert alert;
    CPUMetricData data80{80.0, true};
    CPUMetricData data79{79.0, true};

    // 80도 3초 유지
    EXPECT_FALSE(alert.IsCPUAlert(data80));
    std::this_thread::sleep_for(std::chrono::seconds(3));
    EXPECT_FALSE(alert.IsCPUAlert(data80));

    // 79도로 떨어짐: 상태 초기화
    EXPECT_FALSE(alert.IsCPUAlert(data79));

    // 다시 80도: 새로 시작, 알람이 오면 안 됨
    EXPECT_FALSE(alert.IsCPUAlert(data80));
}

// CPU Usage Min Boundary
TEST(THealthMonitorAlertTest, CPUAlert_MinValue_NoAlert) {
    THealthMonitorAlert alert;
    CPUMetricData data{0.0, true};
    EXPECT_FALSE(alert.IsCPUAlert(data));
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
// Memory Usage Min Boundary
TEST(THealthMonitorAlertTest, MemoryAlert_MinValue_NoAlert) {
    THealthMonitorAlert alert;
    MemoryMetricData data{0, 1000, true}; // 0%
    EXPECT_FALSE(alert.IsMemoryAlert(data));
    EXPECT_EQ(alert.GetMemoryAlertType(data), AlertType::NONE);
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
// CPU Temperature Min Boundary
TEST(THealthMonitorAlertTest, TemperatureAlert_MinValue_NoAlert) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{0.0, 100.0, true};
    EXPECT_FALSE(alert.IsTemperatureAlert(data));
    EXPECT_EQ(alert.GetTemperatureAlertType(data), AlertType::NONE);
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
// Disk Usage Min Boundary
TEST(THealthMonitorAlertTest, DiskAlert_MinValue_NoAlert) {
    THealthMonitorAlert alert;
    DiskMetricData data{0, true};
    EXPECT_FALSE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertType(data), AlertType::NONE);
}
