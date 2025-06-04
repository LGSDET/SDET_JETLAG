#include "HealthMonitor_Comm_Parsing.h"
#include <chrono>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <stdexcept>
#include <vector>
#include <zlib.h>

const int MAX_LATENCY_MS = 5000;  // 최대 허용 지연시간 5초

// 표준 C++ 문자열 파싱 유틸리티 함수들
namespace {
    double StringToDouble(const std::string& str) {
        try {
            return std::stod(str);
        } catch (...) {
            return 0.0;
        }
    }
    
    int StringToInt(const std::string& str) {
        try {
            return std::stoi(str);
        } catch (...) {
            return 0;
        }
    }
    
    int64_t StringToInt64(const std::string& str) {
        try {
            return std::stoll(str);
        } catch (...) {
            return 0;
        }
    }
    
    std::string IntToHexString(uint32_t value) {
        std::stringstream ss;
        ss << std::hex << std::setw(8) << std::setfill('0') << value;
        return ss.str();
    }
    
    std::string ToLowerCase(const std::string& str) {
        std::string result = str;
        std::transform(result.begin(), result.end(), result.begin(), ::tolower);
        return result;
    }
    
    // 문자열에서 특정 구분자로 분할
    std::vector<std::string> Split(const std::string& str, char delimiter) {
        std::vector<std::string> tokens;
        std::stringstream ss(str);
        std::string token;
        
        while (std::getline(ss, token, delimiter)) {
            tokens.push_back(token);
        }
        return tokens;
    }
    
    // 문자열에서 부분 문자열 찾기 (VCL Pos 함수 대체)
    size_t FindSubstring(const std::string& str, const std::string& substr) {
        size_t pos = str.find(substr);
        return (pos != std::string::npos) ? pos + 1 : 0;  // VCL 스타일로 1-based 인덱스
    }
    
    // 부분 문자열 추출 (VCL SubString 함수 대체)
    std::string Substring(const std::string& str, size_t start, size_t length) {
        if (start < 1 || start > str.length()) return "";
        return str.substr(start - 1, length);  // 1-based to 0-based 변환
    }
    
    // 문자열 앞뒤 공백 제거
    std::string Trim(const std::string& str) {
        size_t start = str.find_first_not_of(" \t\n\r");
        if (start == std::string::npos) return "";
        size_t end = str.find_last_not_of(" \t\n\r");
        return str.substr(start, end - start + 1);
    }
    
    // CRC32 계산 (zlib 라이브러리 사용)
    uint32_t CalculateCRC32(const std::string& data) {
        uLong crc = crc32(0L, Z_NULL, 0);
        crc = crc32(crc, reinterpret_cast<const Bytef*>(data.c_str()), data.length());
        return static_cast<uint32_t>(crc);
    }
}

THealthMonitorCommunication::THealthMonitorCommunication() {
    currentLatency = 0;
    ResetTimer();
}

THealthMonitorCommunication::~THealthMonitorCommunication() {
    // 순수 파싱 클래스이므로 정리할 리소스 없음
}

void THealthMonitorCommunication::ResetTimer() {
    timerStart = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();
}

int64_t THealthMonitorCommunication::GetElapsedTime() {
    auto now = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::steady_clock::now().time_since_epoch()).count();
    return now - timerStart;
}

void THealthMonitorCommunication::StartTimer() {
    ResetTimer();
}

int64_t THealthMonitorCommunication::GetCurrentElapsedTime() {
    return GetElapsedTime();
}

bool THealthMonitorCommunication::IsLatencyExceeded() const {
    return currentLatency > MAX_LATENCY_MS;
}

void THealthMonitorCommunication::UpdateLatency(int64_t serverTime) {
    int64_t clientTime = GetElapsedTime();
    currentLatency = static_cast<int>(clientTime - serverTime);
    
    // 음수 지연시간 보정
    if (currentLatency < 0) {
        currentLatency = 0;
    }
    
    ResetTimer();
}

bool THealthMonitorCommunication::VerifyCRC32(const std::string& data, const std::string& receivedCRC) {
    try {
        size_t crcPos = FindSubstring(data, "|CRC=");
        if (crcPos == 0) return false;
        
        std::string pureData = Substring(data, 1, crcPos - 1);
        uint32_t calculatedCRC = CalculateCRC32(pureData);
        std::string calculatedCRCStr = ToLowerCase(IntToHexString(calculatedCRC));
        return calculatedCRCStr == ToLowerCase(receivedCRC);
    } catch (...) {
        return false;
    }
}

bool THealthMonitorCommunication::ParseSystemInfo(const std::string& data) {
    try {
        size_t crcPos = FindSubstring(data, "|CRC=");
        if (crcPos == 0) {
            throw std::runtime_error("CRC not found in data");
        }
        
        std::string crcValue = Substring(data, crcPos + 5, 8);
        if (!VerifyCRC32(data, crcValue)) {
            throw std::runtime_error("CRC verification failed");
        }
        
        std::string pureData = Substring(data, 1, crcPos - 1);
        std::vector<std::string> items = Split(pureData, '|');
        
        if (items.empty()) return false;
        
        // 서버의 타이머 값 파싱 및 지연시간 업데이트
        std::string timerStr = items[0];
        if (FindSubstring(timerStr, "TIMER=") == 1) {
            std::string serverTimeStr = Substring(timerStr, 7, timerStr.length() - 6);
            int64_t serverTime = StringToInt64(serverTimeStr);
            UpdateLatency(serverTime);
        }
        
        // 나머지 시스템 정보 파싱
        for (size_t i = 1; i < items.size(); i++) {
            std::string item = items[i];
            size_t colonPos = FindSubstring(item, ":");
            if (colonPos == 0) continue;
            
            std::string key = Substring(item, 1, colonPos - 1);
            std::string value = Substring(item, colonPos + 1, item.length());
            
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
        
        return true;
    } catch (...) {
        return false;
    }
}

CPUMetricData THealthMonitorCommunication::ParseCPUMetric(const std::string& value) {
    CPUMetricData result = {0.0, false};
    try {
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
        
        std::string current = Substring(value, 1, slashPos - 1);
        result.usage = StringToDouble(current);
        result.isValid = true;
    } catch (...) {
    }
    return result;
}

MemoryMetricData THealthMonitorCommunication::ParseMemoryMetric(const std::string& value) {
    MemoryMetricData result = {0, 0, false};
    try {
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
        
        std::string current = Substring(value, 1, slashPos - 1);
        std::string maximum = Substring(value, slashPos + 1, value.length());
        result.currentUsage = StringToInt(current);
        result.totalMemory = StringToInt(maximum);
        result.isValid = true;
    } catch (...) {
    }
    return result;
}

TemperatureMetricData THealthMonitorCommunication::ParseTemperatureMetric(const std::string& value) {
    TemperatureMetricData result = {0.0, 85.0, false};
    try {
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
        
        std::string current = Substring(value, 1, slashPos - 1);
        std::string maximum = Substring(value, slashPos + 1, value.length());
        result.temperature = StringToDouble(current);
        result.maxTemperature = StringToDouble(maximum);
        result.isValid = true;
    } catch (...) {
    }
    return result;
}

DiskMetricData THealthMonitorCommunication::ParseDiskMetric(const std::string& value) {
    DiskMetricData result = {0, false};
    try {
        size_t slashPos = FindSubstring(value, "/");
        std::string current = (slashPos > 0) ? Substring(value, 1, slashPos - 1) : value;
        result.usagePercent = StringToInt(Trim(current));
        result.isValid = true;
    } catch (...) {
    }
    return result;
}

UptimeMetricData THealthMonitorCommunication::ParseUptimeMetric(const std::string& value) {
    UptimeMetricData result = {0, "", false};
    try {
        std::string uptimeStr = Trim(value);
        size_t dPos = FindSubstring(uptimeStr, "d ");
        if (dPos > 0) {
            result.days = StringToInt(Substring(uptimeStr, 1, dPos - 1));
            size_t spacePos = FindSubstring(uptimeStr, " ");
            if (spacePos > 0) {
                result.timeStr = Substring(uptimeStr, spacePos + 1, uptimeStr.length());
            }
        } else {
            result.days = 0;
            result.timeStr = uptimeStr;
        }
        result.isValid = true;
    } catch (...) {
    }
    return result;
}