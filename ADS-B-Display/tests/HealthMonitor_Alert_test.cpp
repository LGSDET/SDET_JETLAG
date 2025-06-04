#include <gtest/gtest.h>
#include "../HealthMonitor_Alert.h"

// CPU Usage BVA
TEST(THealthMonitorAlertTest, CPUAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
<<<<<<< HEAD
    CPUMetricData data{79.0, true};
=======
    CPUMetricData data{85.0, true};
    EXPECT_TRUE(alert.IsCPUAlert(data));
    std::string msg = alert.GetCPUAlertMessage(data);
    EXPECT_NE(msg.find("CPU usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, CPUAlertNotTriggered) {
    THealthMonitorAlert alert;
    CPUMetricData data{50.0, true};
>>>>>>> 1f233d3e (작업 중인 변경사항 임시 커밋)
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
<<<<<<< HEAD
    MemoryMetricData data{790, 1000, true}; // 79%
=======
    MemoryMetricData data{950, 1000, true};
    EXPECT_TRUE(alert.IsMemoryAlert(data));
    std::string msg = alert.GetMemoryAlertMessage(data);
    EXPECT_NE(msg.find("Memory usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, MemoryAlertNotTriggered) {
    THealthMonitorAlert alert;
    MemoryMetricData data{500, 1000, true};
>>>>>>> 1f233d3e (작업 중인 변경사항 임시 커밋)
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
<<<<<<< HEAD
    TemperatureMetricData data{69.0, 100.0, true};
=======
    TemperatureMetricData data{80.0, 100.0, true};
    EXPECT_TRUE(alert.IsTemperatureAlert(data));
    std::string msg = alert.GetTemperatureAlertMessage(data);
    EXPECT_NE(msg.find("CPU temperature"), std::string::npos);
}

TEST(THealthMonitorAlertTest, TemperatureAlertNotTriggered) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{60.0, 100.0, true};
>>>>>>> 1f233d3e (작업 중인 변경사항 임시 커밋)
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
<<<<<<< HEAD
    DiskMetricData data{89, true};
    EXPECT_FALSE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertType(data), AlertType::NONE);
=======
    DiskMetricData data{95, true};
    EXPECT_TRUE(alert.IsDiskAlert(data));
    std::string msg = alert.GetDiskAlertMessage(data);
    EXPECT_NE(msg.find("Disk usage"), std::string::npos);
}

TEST(THealthMonitorAlertTest, DiskAlertNotTriggered) {
    THealthMonitorAlert alert;
    DiskMetricData data{50, true};
    EXPECT_FALSE(alert.IsDiskAlert(data));
    EXPECT_EQ(alert.GetDiskAlertMessage(data), "");
}

// CPU Usage Boundary Value
TEST(THealthMonitorAlertTest, CPUAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    CPUMetricData data{79.0, true};
    EXPECT_FALSE(alert.IsCPUAlert(data));
}
TEST(THealthMonitorAlertTest, CPUAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    CPUMetricData data{80.0, true};
    EXPECT_TRUE(alert.IsCPUAlert(data));
}

// Memory Usage Boundary Value
TEST(THealthMonitorAlertTest, MemoryAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    MemoryMetricData data{790, 1000, true}; // 79%
    EXPECT_FALSE(alert.IsMemoryAlert(data));
}
TEST(THealthMonitorAlertTest, MemoryAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    MemoryMetricData data{800, 1000, true}; // 80%
    EXPECT_TRUE(alert.IsMemoryAlert(data));
}

// CPU Temperature Boundary Value
TEST(THealthMonitorAlertTest, TemperatureAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{69.0, 100.0, true};
    EXPECT_FALSE(alert.IsTemperatureAlert(data));
}
TEST(THealthMonitorAlertTest, TemperatureAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    TemperatureMetricData data{70.0, 100.0, true};
    EXPECT_TRUE(alert.IsTemperatureAlert(data));
}

// Disk Usage Boundary Value
TEST(THealthMonitorAlertTest, DiskAlert_JustBeforeWarning) {
    THealthMonitorAlert alert;
    DiskMetricData data{89, true};
    EXPECT_FALSE(alert.IsDiskAlert(data));
>>>>>>> 1f233d3e (작업 중인 변경사항 임시 커밋)
}
TEST(THealthMonitorAlertTest, DiskAlert_AtWarningThreshold) {
    THealthMonitorAlert alert;
    DiskMetricData data{90, true};
    EXPECT_TRUE(alert.IsDiskAlert(data));
<<<<<<< HEAD
    EXPECT_EQ(alert.GetDiskAlertType(data), AlertType::DISK_SPACE_LOW);
}
=======
}
>>>>>>> 1f233d3e (작업 중인 변경사항 임시 커밋)
