#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_Communication.h"
#include <zlib.h>

THealthMonitorCommunication::THealthMonitorCommunication(TComponent *Owner) {
  isConnected = false;
  MonitorTCPClient = new TIdTCPClient(Owner);
  MonitorTCPClient->Port = 5001;
  MonitorTCPClient->OnConnected = OnConnected;
  MonitorTCPClient->OnDisconnected = OnDisconnected;
}

THealthMonitorCommunication::~THealthMonitorCommunication() {
  if (MonitorTCPClient->Connected()) {
    MonitorTCPClient->Disconnect();
  }
  delete MonitorTCPClient;
}

bool THealthMonitorCommunication::Connect(const String &ipAddress, int port) {
  try {
    MonitorTCPClient->Host = ipAddress;
    MonitorTCPClient->Port = port;
    MonitorTCPClient->ConnectTimeout = 5000;
    MonitorTCPClient->ReadTimeout = 5000;
    MonitorTCPClient->Connect();
    return true;
  } catch (Exception &e) {
    return false;
  }
}

void THealthMonitorCommunication::Disconnect() {
  if (MonitorTCPClient->Connected()) {
    MonitorTCPClient->Disconnect();
  }
}

void __fastcall THealthMonitorCommunication::OnConnected(TObject *Sender) {
  isConnected = true;
}

void __fastcall THealthMonitorCommunication::OnDisconnected(TObject *Sender) {
  isConnected = false;
}

bool THealthMonitorCommunication::VerifyCRC32(const String &data,
                                              const String &receivedCRC) {
  try {
    int crcPos = data.Pos("|CRC=");
    if (crcPos <= 0)
      return false;

    String pureData = data.SubString(1, crcPos - 1);
    uLong crc = crc32(0L, Z_NULL, 0);
    AnsiString ansiData = pureData;
    crc = crc32(crc, (const Bytef *)ansiData.c_str(), ansiData.Length());
    String calculatedCRC = IntToHex(static_cast<__int64>(crc), 8).LowerCase();
    return calculatedCRC == receivedCRC.LowerCase();
  } catch (...) {
    return false;
  }
}

void THealthMonitorCommunication::UpdateSystemInfo() {
  if (!MonitorTCPClient->Connected())
    return;

  try {
    MonitorTCPClient->Socket->WriteLn("GET_SYSTEM_INFO");
    String response = MonitorTCPClient->Socket->ReadLn();
    ParseSystemInfo(response);
  } catch (Exception &e) {
    MonitorTCPClient->Disconnect();
  }
}

bool THealthMonitorCommunication::ParseSystemInfo(const String &data) {
  try {
    int crcPos = data.Pos("|CRC=");
    if (crcPos <= 0) {
      throw Exception("CRC not found in data");
    }

    String crcValue = data.SubString(crcPos + 5, 8);
    if (!VerifyCRC32(data, crcValue)) {
      throw Exception("CRC verification failed");
    }

    String pureData = data.SubString(1, crcPos - 1);
    TStringList *items = new TStringList();
    items->Delimiter = '|';
    items->DelimitedText = pureData;

    for (int i = 0; i < items->Count; i++) {
      String item = items->Strings[i];
      String key = item.SubString(1, item.Pos(":") - 1);
      String value = item.SubString(item.Pos(":") + 1, item.Length());

      if (key == "CPU") {
        cpuData = ParseCPUMetric(value);
      } else if (key == "MEM") {
        memoryData = ParseMemoryMetric(value);
      } else if (key == "TEMP") {
        temperatureData = ParseTemperatureMetric(value);
      } else if (key == "DISK") {
        diskData = ParseDiskMetric(value);
      } else if (key == "UPTIME") {
        uptimeData = ParseUptimeMetric(value);
      }
    }

    delete items;
    return true;
  } catch (Exception &e) {
    MonitorTCPClient->Disconnect();
    return false;
  }
}

CPUMetricData THealthMonitorCommunication::ParseCPUMetric(const String &value) {
  CPUMetricData result = {0.0, false};
  try {
    String current = value.SubString(1, value.Pos("/") - 1);
    result.usage = StrToFloat(current);
    result.isValid = true;
  } catch (...) {
  }
  return result;
}

MemoryMetricData
THealthMonitorCommunication::ParseMemoryMetric(const String &value) {
  MemoryMetricData result = {0, 0, false};
  try {
    String current = value.SubString(1, value.Pos("/") - 1);
    String maximum = value.SubString(value.Pos("/") + 1, value.Length());
    result.currentUsage = StrToInt(current);
    result.totalMemory = StrToInt(maximum);
    result.isValid = true;
  } catch (...) {
  }
  return result;
}

TemperatureMetricData
THealthMonitorCommunication::ParseTemperatureMetric(const String &value) {
  TemperatureMetricData result = {0.0, 85.0, false};
  try {
    String current = value.SubString(1, value.Pos("/") - 1);
    String maximum = value.SubString(value.Pos("/") + 1, value.Length());
    result.temperature = StrToFloat(current);
    result.maxTemperature = StrToFloat(maximum);
    result.isValid = true;
  } catch (...) {
  }
  return result;
}

DiskMetricData
THealthMonitorCommunication::ParseDiskMetric(const String &value) {
  DiskMetricData result = {0, false};
  try {
    String current = value.SubString(1, value.Pos("/") - 1);
    result.usagePercent = StrToInt(current.Trim());
    result.isValid = true;
  } catch (...) {
  }
  return result;
}

UptimeMetricData
THealthMonitorCommunication::ParseUptimeMetric(const String &value) {
  UptimeMetricData result = {0, "", false};
  try {
    String uptimeStr = value.Trim();
    if (uptimeStr.Pos("d ") > 0) {
      result.days = StrToInt(uptimeStr.SubString(1, uptimeStr.Pos("d") - 1));
      result.timeStr =
          uptimeStr.SubString(uptimeStr.Pos(" ") + 1, uptimeStr.Length());
    } else {
      result.days = 0;
      result.timeStr = uptimeStr;
    }
    result.isValid = true;
  } catch (...) {
  }
  return result;
}