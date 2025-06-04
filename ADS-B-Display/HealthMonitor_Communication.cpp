#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_Communication.h"
#include <zlib.h>
#include <chrono>

const int MAX_LATENCY_MS = 5000;  // 최대 허용 지연시간 5초

THealthMonitorCommunication::THealthMonitorCommunication(TComponent *Owner) {
  isConnected = false;
  MonitorTCPClient = new TIdTCPClient(Owner);
  MonitorTCPClient->Port = 5001;
  MonitorTCPClient->OnConnected = OnConnected;
  MonitorTCPClient->OnDisconnected = OnDisconnected;
  currentLatency = 0;
  ResetTimer();
}

void THealthMonitorCommunication::ResetTimer() {
  timerStart = std::chrono::duration_cast<std::chrono::milliseconds>(
      std::chrono::steady_clock::now().time_since_epoch()).count();
}

__int64 THealthMonitorCommunication::GetElapsedTime() {
  auto now = std::chrono::duration_cast<std::chrono::milliseconds>(
      std::chrono::steady_clock::now().time_since_epoch()).count();
  return now - timerStart;
}

THealthMonitorCommunication::~THealthMonitorCommunication() {
  if (MonitorTCPClient->Connected()) {
    MonitorTCPClient->Disconnect();
  }
  delete MonitorTCPClient;
}

bool THealthMonitorCommunication::Connect(const String &ipAddress, int port) {
  try {
    if (!MonitorTCPClient) {
      return false;
    }
    
    // 이전 연결이 있다면 완전히 정리
    if (MonitorTCPClient->Connected()) {
      Disconnect();
    }
    
    // 소켓 재초기화
    if (MonitorTCPClient->Socket) {
      MonitorTCPClient->Socket->Close();
      MonitorTCPClient->Socket->Open();
    }
    
    // 연결 설정
    MonitorTCPClient->Host = ipAddress;
    MonitorTCPClient->Port = port;
    MonitorTCPClient->ConnectTimeout = 5000;
    MonitorTCPClient->ReadTimeout = 5000;
    currentLatency = 0;  // 연결 시 지연시간 초기화
    
    // 연결 시도
    MonitorTCPClient->Connect();
    return true;
  } catch (...) {
    // 연결 실패 시 상태 초기화
    isConnected = false;
    currentLatency = 0;
    if (MonitorTCPClient && MonitorTCPClient->Socket) {
      MonitorTCPClient->Socket->Close();
    }
    return false;
  }
}

void THealthMonitorCommunication::Disconnect() {
  try {
    if (MonitorTCPClient && MonitorTCPClient->Connected()) {
      MonitorTCPClient->Disconnect();
    }
    // 연결 해제 후 상태 초기화
    isConnected = false;
    currentLatency = 0;
    if (MonitorTCPClient && MonitorTCPClient->Socket) {
      MonitorTCPClient->Socket->Close();
    }
  } catch (...) {
    // 예외가 발생해도 상태는 초기화
    isConnected = false;
    currentLatency = 0;
  }
}

void __fastcall THealthMonitorCommunication::OnConnected(TObject *Sender) {
  isConnected = true;
  ResetTimer();  // 연결 성공 시 타이머 초기화
}

void __fastcall THealthMonitorCommunication::OnDisconnected(TObject *Sender) {
  try {
    isConnected = false;
    currentLatency = 0;
    if (MonitorTCPClient && MonitorTCPClient->Socket) {
      MonitorTCPClient->Socket->Close();
    }
  } catch (...) {
    // 예외가 발생해도 상태는 초기화
    isConnected = false;
    currentLatency = 0;
  }
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
  try {
    if (!MonitorTCPClient || !MonitorTCPClient->Connected() || !MonitorTCPClient->Socket) {
      Disconnect();
      return;
    }

    ResetTimer();  // 요청 전 타이머 초기화
    MonitorTCPClient->Socket->WriteLn("GET_SYSTEM_INFO");
    String response = MonitorTCPClient->Socket->ReadLn();
    ParseSystemInfo(response);
  } catch (...) {
    Disconnect();
  }
}

bool THealthMonitorCommunication::IsLatencyExceeded() const {
  return currentLatency > MAX_LATENCY_MS;
}

void THealthMonitorCommunication::UpdateLatency(__int64 serverTime) {
  __int64 clientTime = GetElapsedTime();
  currentLatency = clientTime - serverTime;
  
  // 음수 지연시간 보정
  if (currentLatency < 0) {
    currentLatency = 0;
  }
  
  // 지연시간 초과시 연결 종료
  if (IsLatencyExceeded()) {
    Disconnect();
  }
  
  // 다음 측정을 위해 타이머 리셋
  ResetTimer();
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

    // 서버의 타이머 값 파싱 및 지연시간 업데이트
    String timerStr = items->Strings[0];
    if (timerStr.Pos("TIMER=") == 1) {
      String serverTimeStr = timerStr.SubString(7, timerStr.Length() - 6);
      __int64 serverTime = StrToInt64(serverTimeStr);
      UpdateLatency(serverTime);
    }

    // 나머지 시스템 정보 파싱
    for (int i = 1; i < items->Count; i++) {
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
    Disconnect();
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
      // VCL String을 std::string으로 변환
      String timeVclStr = uptimeStr.SubString(uptimeStr.Pos(" ") + 1, uptimeStr.Length());
      result.timeStr = AnsiString(timeVclStr).c_str();
    } else {
      result.days = 0;
      // VCL String을 std::string으로 변환
      result.timeStr = AnsiString(uptimeStr).c_str();
    }
    result.isValid = true;
  } catch (...) {
  }
  return result;
}