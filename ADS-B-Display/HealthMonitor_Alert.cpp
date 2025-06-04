#include "HealthMonitor_Alert.h"

THealthMonitorAlert::THealthMonitorAlert() {
  // 업데이트된 기본 임계값 설정 (요구사항에 따라)
  cpuThreshold = 80.0;         // 80% CPU 사용률 (5초 유지)
  memoryThreshold = 80.0;      // 80% 메모리 사용률 (기존 90%에서 변경)
  temperatureThreshold = 70.0; // 70도 (기존 75도에서 변경)
  diskThreshold = 90.0;        // 90% 디스크 사용률 (변경 없음)
  
  // CPU 지속 상태 초기화
  cpuHighState = false;
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
  if (!data.isValid) {
    // 데이터가 유효하지 않으면 CPU 고부하 상태 초기화
    cpuHighState = false;
    return false;
  }
  
  auto currentTime = std::chrono::steady_clock::now();
  
  if (data.usage >= cpuThreshold) {
    if (!cpuHighState) {
      // CPU가 처음으로 임계값을 초과한 경우
      cpuHighState = true;
      cpuHighStartTime = currentTime;
      return false; // 아직 5초가 지나지 않았으므로 알람 없음
    } else {
      // CPU가 이미 임계값을 초과한 상태인 경우
      auto duration = std::chrono::duration_cast<std::chrono::seconds>(
          currentTime - cpuHighStartTime);
      return duration.count() >= CPU_SUSTAINED_DURATION_SECONDS;
    }
  } else {
    // CPU가 임계값 미만인 경우 상태 초기화
    cpuHighState = false;
    return false;
  }
}

bool THealthMonitorAlert::IsMemoryAlert(const MemoryMetricData &data) const {
  if (!data.isValid || data.totalMemory <= 0)
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

AlertType THealthMonitorAlert::GetCPUAlertType(const CPUMetricData &data) const {
  return IsCPUAlert(data) ? AlertType::CPU_HIGH : AlertType::NONE;
}

AlertType THealthMonitorAlert::GetMemoryAlertType(const MemoryMetricData &data) const {
  return IsMemoryAlert(data) ? AlertType::MEMORY_INSUFFICIENT : AlertType::NONE;
}

AlertType THealthMonitorAlert::GetTemperatureAlertType(const TemperatureMetricData &data) const {
  return IsTemperatureAlert(data) ? AlertType::HIGH_TEMPERATURE : AlertType::NONE;
}

AlertType THealthMonitorAlert::GetDiskAlertType(const DiskMetricData &data) const {
  return IsDiskAlert(data) ? AlertType::DISK_SPACE_LOW : AlertType::NONE;
}

void THealthMonitorAlert::ResetCPUSustainedState() const {
  cpuHighState = false;
}