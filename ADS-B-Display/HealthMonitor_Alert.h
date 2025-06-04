#ifndef HEALTHMONITOR_ALERT_H
#define HEALTHMONITOR_ALERT_H

#include "HealthMonitor_MetricData.h"
#include <chrono>

// Alert types for better testability and UI decoupling
enum class AlertType {
  NONE,
  CPU_HIGH,
  MEMORY_INSUFFICIENT,
  HIGH_TEMPERATURE,
  DISK_SPACE_LOW
};

class THealthMonitorAlert {
private:
  // Alert thresholds (업데이트된 기준값)
  double cpuThreshold;         // CPU: 80% 이상 5초 유지
  double memoryThreshold;      // Memory: 80% 이상
  double temperatureThreshold; // Temperature: 70°C 이상
  double diskThreshold;        // Disk: 90% 이상
  
  // CPU 지속 시간 추적을 위한 변수들
  mutable bool cpuHighState;
  mutable std::chrono::steady_clock::time_point cpuHighStartTime;
  static const int CPU_SUSTAINED_DURATION_SECONDS = 5;

public:
  THealthMonitorAlert();

  // Set thresholds
  void SetCPUThreshold(double threshold);
  void SetMemoryThreshold(double threshold);
  void SetTemperatureThreshold(double threshold);
  void SetDiskThreshold(double threshold);
  
  // Get thresholds (for unit testing)
  double GetCPUThreshold() const { return cpuThreshold; }
  double GetMemoryThreshold() const { return memoryThreshold; }
  double GetTemperatureThreshold() const { return temperatureThreshold; }
  double GetDiskThreshold() const { return diskThreshold; }

  // Check if metrics exceed thresholds
  bool IsCPUAlert(const CPUMetricData &data) const;
  bool IsMemoryAlert(const MemoryMetricData &data) const;
  bool IsTemperatureAlert(const TemperatureMetricData &data) const;
  bool IsDiskAlert(const DiskMetricData &data) const;
  
  // Get alert type for UI handling
  AlertType GetCPUAlertType(const CPUMetricData &data) const;
  AlertType GetMemoryAlertType(const MemoryMetricData &data) const;
  AlertType GetTemperatureAlertType(const TemperatureMetricData &data) const;
  AlertType GetDiskAlertType(const DiskMetricData &data) const;
  
  // Reset CPU sustained state (for testing)
  void ResetCPUSustainedState() const;
};

#endif