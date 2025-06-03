#ifndef HEALTHMONITOR_ALERT_H
#define HEALTHMONITOR_ALERT_H

#include "HealthMonitor_MetricData.h"

class THealthMonitorAlert {
private:
  // Alert thresholds
  double cpuThreshold;
  double memoryThreshold;
  double temperatureThreshold;
  double diskThreshold;

public:
  THealthMonitorAlert();

  // Set thresholds
  void SetCPUThreshold(double threshold);
  void SetMemoryThreshold(double threshold);
  void SetTemperatureThreshold(double threshold);
  void SetDiskThreshold(double threshold);

  // Check if metrics exceed thresholds
  bool IsCPUAlert(const CPUMetricData &data) const;
  bool IsMemoryAlert(const MemoryMetricData &data) const;
  bool IsTemperatureAlert(const TemperatureMetricData &data) const;
  bool IsDiskAlert(const DiskMetricData &data) const;

  // Get alert messages
  String GetCPUAlertMessage(const CPUMetricData &data) const;
  String GetMemoryAlertMessage(const MemoryMetricData &data) const;
  String GetTemperatureAlertMessage(const TemperatureMetricData &data) const;
  String GetDiskAlertMessage(const DiskMetricData &data) const;
};

#endif