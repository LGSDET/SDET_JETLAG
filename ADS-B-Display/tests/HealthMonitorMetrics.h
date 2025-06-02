#ifndef HEALTH_MONITOR_METRICS_H
#define HEALTH_MONITOR_METRICS_H

#include <algorithm>
#include <cstdint>
#include <iomanip>
#include <string>

using namespace std;

// 각 메트릭의 데이터를 저장할 구조체들
struct CPUMetricData {
  double usage;
  bool isValid;

  CPUMetricData() : usage(0.0), isValid(false) {}
};

struct MemoryMetricData {
  int currentUsage;
  int totalMemory;
  bool isValid;

  MemoryMetricData() : currentUsage(0), totalMemory(0), isValid(false) {}
};

struct TemperatureMetricData {
  double temperature;
  double maxTemperature;
  bool isValid;

  TemperatureMetricData()
      : temperature(0.0), maxTemperature(85.0), isValid(false) {}
};

struct DiskMetricData {
  int usagePercent;
  bool isValid;

  DiskMetricData() : usagePercent(0), isValid(false) {}
};

struct UptimeMetricData {
  int days;
  string timeStr;
  bool isValid;

  UptimeMetricData() : days(0), timeStr(""), isValid(false) {}
};

class HealthMonitorMetrics {
public:
  static CPUMetricData ParseCPUMetric(const string &value);
  static MemoryMetricData ParseMemoryMetric(const string &value);
  static TemperatureMetricData ParseTemperatureMetric(const string &value);
  static DiskMetricData ParseDiskMetric(const string &value);
  static UptimeMetricData ParseUptimeMetric(const string &value);
  static bool VerifyCRC32(const string &data, const string &receivedCRC);
};

#endif