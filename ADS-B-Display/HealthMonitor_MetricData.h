#ifndef HEALTHMONITOR_METRICDATA_H
#define HEALTHMONITOR_METRICDATA_H

#include <string>

struct CPUMetricData {
  double usage;
  bool isValid;
};

struct MemoryMetricData {
  int currentUsage;
  int totalMemory;
  bool isValid;
};

struct TemperatureMetricData {
  double temperature;
  double maxTemperature;
  bool isValid;
};

struct DiskMetricData {
  int usagePercent;
  bool isValid;
};

struct UptimeMetricData {
  int days;
  std::string timeStr;
  bool isValid;
};

#endif