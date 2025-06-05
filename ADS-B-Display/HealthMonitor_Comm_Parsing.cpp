#include "HealthMonitor_Comm_Parsing.h"
#include <chrono>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <stdexcept>
#include <vector>
 
const int MAX_LATENCY_MS = 5000;  // 최대 허용 지연시간 5초
 
// 표준 C++ 문자열 파싱 유틸리티 함수들
namespace {
    // 문자열이 유효한 숫자인지 검사하는 함수 추가
    bool IsValidDouble(const std::string& str) {
        if (str.empty()) return false;
        
        std::string trimmed = str;
        // 앞뒤 공백 제거
        size_t start = trimmed.find_first_not_of(" \t\n\r");
        if (start == std::string::npos) return false;
        size_t end = trimmed.find_last_not_of(" \t\n\r");
        trimmed = trimmed.substr(start, end - start + 1);
        
        if (trimmed.empty()) return false;
        
        // 첫 문자가 부호이거나 숫자여야 함
        size_t idx = 0;
        if (trimmed[0] == '+' || trimmed[0] == '-') {
            idx = 1;
            if (trimmed.length() == 1) return false; // 부호만 있는 경우
        }
        
        bool hasDigit = false;
        bool hasDot = false;
        
        for (; idx < trimmed.length(); ++idx) {
            char c = trimmed[idx];
            if (std::isdigit(c)) {
                hasDigit = true;
            } else if (c == '.' && !hasDot) {
                hasDot = true;
            } else {
                return false; // 유효하지 않은 문자
            }
        }
        
        return hasDigit; // 최소한 하나의 숫자는 있어야 함
    }
    
    bool IsValidInt(const std::string& str) {
        if (str.empty()) return false;
        
        std::string trimmed = str;
        // 앞뒤 공백 제거
        size_t start = trimmed.find_first_not_of(" \t\n\r");
        if (start == std::string::npos) return false;
        size_t end = trimmed.find_last_not_of(" \t\n\r");
        trimmed = trimmed.substr(start, end - start + 1);
        
        if (trimmed.empty()) return false;
        
        // 첫 문자가 부호이거나 숫자여야 함
        size_t idx = 0;
        if (trimmed[0] == '+' || trimmed[0] == '-') {
            idx = 1;
            if (trimmed.length() == 1) return false; // 부호만 있는 경우
        }
        
        bool hasDigit = false;
        
        for (; idx < trimmed.length(); ++idx) {
            char c = trimmed[idx];
            if (std::isdigit(c)) {
                hasDigit = true;
            } else {
                return false; // 유효하지 않은 문자
            }
        }
        
        return hasDigit; // 최소한 하나의 숫자는 있어야 함
    }

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
 
    // ----------- CRC32 계산 함수 (zlib 없이 자체 구현) -----------
    uint32_t CalculateCRC32(const std::string& data) {
        static const uint32_t table[256] = {
            // 256개 값의 테이블 (생략, 아래에서 자동 생성)
        };
        static bool tableInitialized = false;
        static uint32_t crcTable[256];
 
        if (!tableInitialized) {
            for (uint32_t i = 0; i < 256; ++i) {
                uint32_t c = i;
                for (int j = 0; j < 8; ++j) {
                    if (c & 1)
                        c = 0xEDB88320U ^ (c >> 1);
                    else
                        c = c >> 1;
                }
                crcTable[i] = c;
            }
            tableInitialized = true;
        }
 
        uint32_t crc = 0xFFFFFFFFU;
        for (unsigned char ch : data) {
            crc = crcTable[(crc ^ ch) & 0xFF] ^ (crc >> 8);
        }
        return crc ^ 0xFFFFFFFFU;
    }
}
 
THealthMonitorCommunication::THealthMonitorCommunication() {
    currentLatency = 0;
    crcFailureCount = 0;
    isNetworkError = false;
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
        if (crcPos == 0) {
            // CRC 실패 카운터 증가
            crcFailureCount++;
            if (crcFailureCount >= 3) {
                isNetworkError = true;
            }
            return false;
        }
       
        std::string pureData = Substring(data, 1, crcPos - 1);
        uint32_t calculatedCRC = CalculateCRC32(pureData);
        std::string calculatedCRCStr = ToLowerCase(IntToHexString(calculatedCRC));
        bool crcValid = calculatedCRCStr == ToLowerCase(receivedCRC);
        
        if (crcValid) {
            // CRC 성공 시 카운터 리셋 및 네트워크 오류 해제
            crcFailureCount = 0;
            isNetworkError = false;
        } else {
            // CRC 실패 카운터 증가
            crcFailureCount++;
            if (crcFailureCount >= 3) {
                isNetworkError = true;
            }
        }
        
        return crcValid;
    } catch (...) {
        // 예외 발생 시에도 CRC 실패로 처리
        crcFailureCount++;
        if (crcFailureCount >= 3) {
            isNetworkError = true;
        }
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
        // 빈 문자열 체크
        if (value.empty() || Trim(value).empty()) {
            return result;
        }
        
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
       
        std::string current = Trim(Substring(value, 1, slashPos - 1));
        // 빈 문자열이나 "//" 같은 경우 체크
        if (current.empty()) {
            return result;
        }
        
        // 숫자 유효성 검사 추가
        if (!IsValidDouble(current)) {
            return result;
        }
        
        result.usage = StringToDouble(current);
        
        // CPU 사용률이 100%를 초과하면 유효하지 않음
        if (result.usage > 100.0 || result.usage < 0.0) {
            return result;
        }
        
        result.isValid = true;
    } catch (...) {
    }
    return result;
}
 
MemoryMetricData THealthMonitorCommunication::ParseMemoryMetric(const std::string& value) {
    MemoryMetricData result = {0, 0, false};
    try {
        // 빈 문자열 체크
        if (value.empty() || Trim(value).empty()) {
            return result;
        }
        
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
       
        std::string current = Trim(Substring(value, 1, slashPos - 1));
        std::string maximum = Trim(Substring(value, slashPos + 1, value.length()));
        
        // 빈 문자열이나 "//" 같은 경우 체크
        if (current.empty() || maximum.empty()) {
            return result;
        }
        
        // 숫자 유효성 검사 추가
        if (!IsValidInt(current) || !IsValidInt(maximum)) {
            return result;
        }
        
        result.currentUsage = StringToInt(current);
        result.totalMemory = StringToInt(maximum);
        
        // 현재 사용량이 총 메모리보다 크거나 음수이면 유효하지 않음
        if (result.currentUsage > result.totalMemory || result.currentUsage < 0 || result.totalMemory <= 0) {
            return result;
        }
        
        result.isValid = true;
    } catch (...) {
    }
    return result;
}
 
TemperatureMetricData THealthMonitorCommunication::ParseTemperatureMetric(const std::string& value) {
    TemperatureMetricData result = {0.0, 85.0, false};
    try {
        // 빈 문자열 체크
        if (value.empty() || Trim(value).empty()) {
            return result;
        }
        
        size_t slashPos = FindSubstring(value, "/");
        if (slashPos == 0) return result;
       
        std::string current = Trim(Substring(value, 1, slashPos - 1));
        std::string maximum = Trim(Substring(value, slashPos + 1, value.length()));
        
        // 빈 문자열이나 "//" 같은 경우 체크
        if (current.empty() || maximum.empty()) {
            return result;
        }
        
        // 숫자 유효성 검사 추가
        if (!IsValidDouble(current) || !IsValidDouble(maximum)) {
            return result;
        }
        
        result.temperature = StringToDouble(current);
        result.maxTemperature = StringToDouble(maximum);
        
        // 현재 온도가 최대 온도보다 크거나 음수이면 유효하지 않음
        // 또한 최대 온도가 0 이하이면 유효하지 않음
        if (result.temperature > result.maxTemperature || result.temperature < -50.0 || 
            result.maxTemperature <= 0.0 || result.maxTemperature > 200.0) {
            return result;
        }
        
        result.isValid = true;
    } catch (...) {
    }
    return result;
}
 
DiskMetricData THealthMonitorCommunication::ParseDiskMetric(const std::string& value) {
    DiskMetricData result = {0, false};
    try {
        // 빈 문자열 체크
        if (value.empty() || Trim(value).empty()) {
            return result;
        }
        
        size_t slashPos = FindSubstring(value, "/");
        std::string current = (slashPos > 0) ? Substring(value, 1, slashPos - 1) : value;
        current = Trim(current);
        
        // 빈 문자열이나 "//" 같은 경우 체크
        if (current.empty()) {
            return result;
        }
        
        // 숫자 유효성 검사 추가
        if (!IsValidInt(current)) {
            return result;
        }
        
        result.usagePercent = StringToInt(current);
        
        // 디스크 사용률이 100%를 초과하거나 음수이면 유효하지 않음
        if (result.usagePercent > 100 || result.usagePercent < 0) {
            return result;
        }
        
        result.isValid = true;
    } catch (...) {
    }
    return result;
}
 
UptimeMetricData THealthMonitorCommunication::ParseUptimeMetric(const std::string& value) {
    UptimeMetricData result = {0, "", false};
    try {
        // 빈 문자열 체크
        if (value.empty() || Trim(value).empty()) {
            return result;
        }
        
        std::string uptimeStr = Trim(value);
        
        // 유효하지 않은 문자들이 포함된 경우 체크 (uptime 형식에 맞지 않는 문자들)
        // 유효한 문자: 숫자, 'd', ':', ' '
        bool hasValidChars = true;
        bool hasDigit = false;
        for (char c : uptimeStr) {
            if (std::isdigit(c)) {
                hasDigit = true;
            } else if (c != 'd' && c != ':' && c != ' ') {
                hasValidChars = false;
                break;
            }
        }
        if (!hasValidChars || !hasDigit) {
            return result;
        }
        
        size_t dPos = FindSubstring(uptimeStr, "d ");
        if (dPos > 0) {
            // "Xd HH:MM:SS" 형식
            std::string daysPart = Trim(Substring(uptimeStr, 1, dPos - 1));
            if (daysPart.empty() || !std::isdigit(daysPart[0])) {
                return result;
            }
            result.days = StringToInt(daysPart);
            
            size_t spacePos = FindSubstring(uptimeStr, " ");
            if (spacePos > 0) {
                result.timeStr = Trim(Substring(uptimeStr, spacePos + 1, uptimeStr.length()));
                // 시간 부분이 비어있으면 유효하지 않음
                if (result.timeStr.empty()) {
                    return result;
                }
            }
        } else {
            // "HH:MM:SS" 형식만 있는 경우
            result.days = 0;
            result.timeStr = uptimeStr;
            
            // 최소한 숫자와 콜론이 포함되어야 함 (시간 형식)
            if (FindSubstring(uptimeStr, ":") == 0 && uptimeStr.find_first_of("0123456789") == std::string::npos) {
                return result;
            }
        }
        
        // 최종 검증: timeStr이 비어있지 않아야 함
        if (result.timeStr.empty()) {
            return result;
        }
        
        result.isValid = true;
    } catch (...) {
    }
    return result;
}