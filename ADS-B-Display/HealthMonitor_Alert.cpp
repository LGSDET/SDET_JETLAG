#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_Alert.h"

THealthMonitorAlert::THealthMonitorAlert() {
  // 기본 임계값 설정
  cpuThreshold = 80.0;         // 80% CPU 사용률
  memoryThreshold = 90.0;      // 90% 메모리 사용률
  temperatureThreshold = 75.0; // 75도
  diskThreshold = 90.0;        // 90% 디스크 사용률
}

void THealthMonitorAlert::SetCPUThreshold(double threshold) {
  cpuThreshold = threshold;
}

void THealthMonitorAlert::SetMemoryThreshold(double threshold) {
  memoryThreshold = threshold;
}

void THealthMonitorAlert::SetTemperatureThreshold(double threshold) {
  temperatureThreshold = threshold;
}

void THealthMonitorAlert::SetDiskThreshold(double threshold) {
  diskThreshold = threshold;
}

bool THealthMonitorAlert::IsCPUAlert(const CPUMetricData &data) const {
  return data.isValid && data.usage >= cpuThreshold;
}

bool THealthMonitorAlert::IsMemoryAlert(const MemoryMetricData &data) const {
  if (!data.isValid)
    return false;
  double usagePercent =
      (static_cast<double>(data.currentUsage) / data.totalMemory) * 100.0;
  return usagePercent >= memoryThreshold;
}

bool THealthMonitorAlert::IsTemperatureAlert(
    const TemperatureMetricData &data) const {
  return data.isValid && data.temperature >= temperatureThreshold;
}

bool THealthMonitorAlert::IsDiskAlert(const DiskMetricData &data) const {
  return data.isValid && data.usagePercent >= diskThreshold;
}

String
THealthMonitorAlert::GetCPUAlertMessage(const CPUMetricData &data) const {
  if (!IsCPUAlert(data))
    return "";
  return "Warning: CPU usage (" + FloatToStrF(data.usage, ffFixed, 7, 1) +
         "%) exceeds threshold (" + FloatToStrF(cpuThreshold, ffFixed, 7, 1) +
         "%)";
}

String
THealthMonitorAlert::GetMemoryAlertMessage(const MemoryMetricData &data) const {
  if (!IsMemoryAlert(data))
    return "";
  double usagePercent =
      (static_cast<double>(data.currentUsage) / data.totalMemory) * 100.0;
  return "Warning: Memory usage (" + FloatToStrF(usagePercent, ffFixed, 7, 1) +
         "%) exceeds threshold (" +
         FloatToStrF(memoryThreshold, ffFixed, 7, 1) + "%)";
}

String THealthMonitorAlert::GetTemperatureAlertMessage(
    const TemperatureMetricData &data) const {
  if (!IsTemperatureAlert(data))
    return "";
  return "Warning: CPU temperature (" +
         FloatToStrF(data.temperature, ffFixed, 7, 1) +
         "°C) exceeds threshold (" +
         FloatToStrF(temperatureThreshold, ffFixed, 7, 1) + "°C)";
}

String
THealthMonitorAlert::GetDiskAlertMessage(const DiskMetricData &data) const {
  if (!IsDiskAlert(data))
    return "";
  return "Warning: Disk usage (" + IntToStr(data.usagePercent) +
         "%) exceeds threshold (" + FloatToStrF(diskThreshold, ffFixed, 7, 1) +
         "%)";
}