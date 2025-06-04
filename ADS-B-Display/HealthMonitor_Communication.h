#ifndef HealthMonitor_CommunicationH
#define HealthMonitor_CommunicationH

#include <System.Classes.hpp>
#include <IdTCPClient.hpp>
#include "HealthMonitor_MetricData.h"

class THealthMonitorCommunication {
private:
    TIdTCPClient* MonitorTCPClient;
    bool isConnected;
    __int64 timerStart;  // 타이머 시작 시간 (밀리초)
    
    void __fastcall OnConnected(TObject* Sender);
    void __fastcall OnDisconnected(TObject* Sender);
    bool VerifyCRC32(const String& data, const String& receivedCRC);
    
    // 메트릭 파싱 함수들
    CPUMetricData ParseCPUMetric(const String& value);
    MemoryMetricData ParseMemoryMetric(const String& value);
    TemperatureMetricData ParseTemperatureMetric(const String& value);
    DiskMetricData ParseDiskMetric(const String& value);
    UptimeMetricData ParseUptimeMetric(const String& value);
    
    // 타이머 관련 함수들
    void ResetTimer();
    __int64 GetElapsedTime();
    
    // 지연시간 관련 함수
    void UpdateLatency(__int64 serverTime);
    
    // 메트릭 데이터 저장
    CPUMetricData cpuData;
    MemoryMetricData memoryData;
    TemperatureMetricData temperatureData;
    DiskMetricData diskData;
    UptimeMetricData uptimeData;
    
public:
    THealthMonitorCommunication(TComponent* Owner);
    ~THealthMonitorCommunication();
    
    bool Connect(const String& ipAddress, int port);
    void Disconnect();
    void UpdateSystemInfo();
    bool ParseSystemInfo(const String& data);
    bool IsConnected() const { return isConnected; }
    bool IsLatencyExceeded() const;  // 지연시간 초과 여부 확인
    
    // 메트릭 데이터 getter 함수들
    const CPUMetricData& GetCPUData() const { return cpuData; }
    const MemoryMetricData& GetMemoryData() const { return memoryData; }
    const TemperatureMetricData& GetTemperatureData() const { return temperatureData; }
    const DiskMetricData& GetDiskData() const { return diskData; }
    const UptimeMetricData& GetUptimeData() const { return uptimeData; }
    
    // 지연시간 관련
    int currentLatency;  // 현재 지연시간 (밀리초)
};

#endif