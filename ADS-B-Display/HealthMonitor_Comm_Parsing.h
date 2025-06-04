#ifndef HealthMonitor_Comm_ParsingH
#define HealthMonitor_Comm_ParsingH

#include "HealthMonitor_MetricData.h"
#include <string>
#include <cstdint>

// 순수 C++ 데이터 파싱 클래스 - VCL/네트워크 독립적
class THealthMonitorCommunication {
    // 테스트 클래스에서 private 메서드 접근 허용
    friend class TestHealthMonitorCommunication;
    
private:
    int64_t timerStart;  // 타이머 시작 시간 (밀리초)
    
    // 순수 파싱 함수들 (VCL 독립적)
    bool VerifyCRC32(const std::string& data, const std::string& receivedCRC);
    CPUMetricData ParseCPUMetric(const std::string& value);
    MemoryMetricData ParseMemoryMetric(const std::string& value);
    TemperatureMetricData ParseTemperatureMetric(const std::string& value);
    DiskMetricData ParseDiskMetric(const std::string& value);
    UptimeMetricData ParseUptimeMetric(const std::string& value);
    
    // 타이머 관련 함수들
    void ResetTimer();
    int64_t GetElapsedTime();
    void UpdateLatency(int64_t serverTime);
    
    // 메트릭 데이터 저장
    CPUMetricData cpuData;
    MemoryMetricData memoryData;
    TemperatureMetricData temperatureData;
    DiskMetricData diskData;
    UptimeMetricData uptimeData;
    
public:
    THealthMonitorCommunication();
    ~THealthMonitorCommunication();
    
    // 순수 파싱 인터페이스 (네트워크 독립적)
    bool ParseSystemInfo(const std::string& data);
    bool IsLatencyExceeded() const;
    
    // 메트릭 데이터 getter 함수들
    const CPUMetricData& GetCPUData() const { return cpuData; }
    const MemoryMetricData& GetMemoryData() const { return memoryData; }
    const TemperatureMetricData& GetTemperatureData() const { return temperatureData; }
    const DiskMetricData& GetDiskData() const { return diskData; }
    const UptimeMetricData& GetUptimeData() const { return uptimeData; }
    
    // 지연시간 관련
    int currentLatency;  // 현재 지연시간 (밀리초)
    
    // 타이머 제어 (UI에서 호출)
    void StartTimer();
    int64_t GetCurrentElapsedTime();
};

#endif