#include "HealthMonitorMetrics.h"
#include <iomanip>
#include <regex>
#include <sstream>
#include <zlib.h>

CPUMetricData HealthMonitorMetrics::ParseCPUMetric(const string &value) {
  CPUMetricData result;
  try {
    size_t pos = value.find("/");
    if (pos != string::npos) {
      result.usage = stod(value.substr(0, pos));
      result.isValid = true;
    }
  } catch (...) {
  }
  return result;
}

MemoryMetricData HealthMonitorMetrics::ParseMemoryMetric(const string &value) {
  MemoryMetricData result;
  try {
    size_t pos = value.find("/");
    if (pos != string::npos) {
      result.currentUsage = stoi(value.substr(0, pos));
      result.totalMemory = stoi(value.substr(pos + 1));
      result.isValid = true;
    }
  } catch (...) {
  }
  return result;
}

TemperatureMetricData
HealthMonitorMetrics::ParseTemperatureMetric(const string &value) {
  TemperatureMetricData result;
  try {
    size_t pos = value.find("/");
    if (pos != string::npos) {
      result.temperature = stod(value.substr(0, pos));
      result.maxTemperature = stod(value.substr(pos + 1));
      result.isValid = true;
    }
  } catch (...) {
  }
  return result;
}

DiskMetricData HealthMonitorMetrics::ParseDiskMetric(const string &value) {
  DiskMetricData result;
  try {
    size_t pos = value.find("/");
    if (pos != string::npos) {
      result.usagePercent = stoi(value.substr(0, pos));
      result.isValid = true;
    }
  } catch (...) {
  }
  return result;
}

UptimeMetricData HealthMonitorMetrics::ParseUptimeMetric(const string &value) {
  UptimeMetricData result;
  try {
    string uptimeStr = value;

    // 정규식 패턴: 일수가 있는 경우와 없는 경우 모두 처리
    regex timePattern("^(?:(\\d+)d )?([0-9]{2}:[0-9]{2}:[0-9]{2})$");
    smatch matches;

    if (regex_match(uptimeStr, matches, timePattern)) {
      if (matches[1].matched) {
        result.days = stoi(matches[1].str());
      }
      result.timeStr = matches[2].str();
      result.isValid = true;
    }
  } catch (...) {
  }
  return result;
}

bool HealthMonitorMetrics::VerifyCRC32(const string &data,
                                       const string &receivedCRC) {
  try {
    size_t crcPos = data.find("|CRC=");
    if (crcPos == string::npos)
      return false;

    string pureData = data.substr(0, crcPos);

    // CRC32 계산
    uLong crc = crc32(0L, Z_NULL, 0);
    crc = crc32(crc, (const Bytef *)pureData.c_str(), pureData.length());

    // 16진수 문자열로 변환
    stringstream ss;
    ss << hex << setw(8) << setfill('0') << crc;
    string calculatedCRC = ss.str();

    // 대소문자 구분 없이 비교
    transform(calculatedCRC.begin(), calculatedCRC.end(), calculatedCRC.begin(),
              ::tolower);
    string receivedCRCLower = receivedCRC;
    transform(receivedCRCLower.begin(), receivedCRCLower.end(),
              receivedCRCLower.begin(), ::tolower);

    return calculatedCRC == receivedCRCLower;
  } catch (...) {
    return false;
  }
}