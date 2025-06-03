#ifndef HEALTHMONITOR_COMMUNICATION_H
#define HEALTHMONITOR_COMMUNICATION_H

#include "HealthMonitor_MetricData.h"
#include <IdTCPClient.hpp>
#include <vcl.h>

class THealthMonitorCommunication {
private:
  TIdTCPClient *MonitorTCPClient;
  bool isConnected;

  bool VerifyCRC32(const String &data, const String &receivedCRC);

public:
  THealthMonitorCommunication(TComponent *Owner);
  ~THealthMonitorCommunication();

  bool Connect(const String &ipAddress, int port = 5001);
  void Disconnect();
  bool IsConnected() const { return isConnected; }

  // Parse functions
  CPUMetricData ParseCPUMetric(const String &value);
  MemoryMetricData ParseMemoryMetric(const String &value);
  TemperatureMetricData ParseTemperatureMetric(const String &value);
  DiskMetricData ParseDiskMetric(const String &value);
  UptimeMetricData ParseUptimeMetric(const String &value);
  bool ParseSystemInfo(const String &data);

  // Event handlers
  void __fastcall OnConnected(TObject *Sender);
  void __fastcall OnDisconnected(TObject *Sender);

  // System info update
  void UpdateSystemInfo();
};

#endif