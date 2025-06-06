![image](https://github.com/user-attachments/assets/8ddc41bd-db80-48e2-bbaf-48603a192501)
# Health Dashboard

라즈베리파이 시스템 상태를 실시간으로 모니터링하는 Windows 기반 GUI 애플리케이션입니다.

A Windows-based GUI application for real-time monitoring of Raspberry Pi system status.

## 📋 개요 / Overview

Health Dashboard는 ADS-B Display 애플리케이션의 서브 모듈로, TCP 소켓을 통해 원격 라즈베리파이 서버로부터 시스템 메트릭을 수집하여 실시간으로 모니터링할 수 있는 기능을 제공합니다.

Health Dashboard is a sub-module of the ADS-B Display application that provides real-time monitoring capabilities by collecting system metrics from remote Raspberry Pi servers via TCP sockets.

## ✨ 주요 기능 / Key Features

### 🔍 실시간 모니터링 / Real-time Monitoring
- **CPU 사용률 / CPU Usage**: 실시간 프로세서 부하 상태 / Real-time processor load status
- **메모리 사용량 / Memory Usage**: 현재 메모리 사용률 및 총 용량 / Current memory usage and total capacity
- **CPU 온도 / CPU Temperature**: 시스템 온도 모니터링 (과열 방지) / System temperature monitoring (overheating prevention)
- **디스크 사용량 / Disk Usage**: 저장공간 사용률 / Storage space utilization
- **시스템 가동시간 / System Uptime**: 부팅 후 경과 시간 / Elapsed time since boot


### ⚠️ 지능형 알림 시스템 / Intelligent Alert System
- **임계값 기반 알림 / Threshold-based Alerts**: 각 메트릭별 사용자 정의 임계값 / User-defined thresholds for each metric
- **지속적 모니터링 / Sustained Monitoring**: CPU는 5초 지속 초과 시에만 알림 / CPU alerts only after 5 seconds of sustained threshold violation
- **시각적 표시 / Visual Indication**: 색상 코딩 (활성: 빨강, 해제: 회색) / Color coding (active: red, cleared: gray)
- **타임스탬프 / Timestamp**: 정확한 오류 발생 시간 기록 / Accurate error occurrence time recording

### 🌐 안정적인 네트워크 통신 / Reliable Network Communication
- **TCP 소켓 / TCP Socket**: 포트 5001을 통한 안정적 통신 / Reliable communication via port 5001
- **CRC32 검증 / CRC32 Verification**: 데이터 무결성 보장 / Data integrity assurance
- **지연시간 모니터링 / Latency Monitoring**: 네트워크 상태 실시간 표시 / Real-time network status display
- **자동 재연결 / Auto Reconnection**: 연결 오류 시 자동 복구 / Automatic recovery on connection errors

## 🛠️ 시스템 요구사항 / System Requirements

### 클라이언트 (Windows) / Client (Windows)
- **OS**: Windows 10 이상 / Windows 10 or later
- **개발환경 / Development Environment**: Borland C++ Builder (VCL 지원 / VCL support)

### 서버 (라즈베리파이) / Server (Raspberry Pi)
- **OS**: Raspberry Pi OS (Debian 기반 / Debian-based)
- **Python**: 3.7 이상 / 3.7 or later
- **필수 패키지 / Required Packages**:
  ```bash
  sudo apt update
  sudo apt install python3-pip
  pip3 install psutil gpiozero
  ```

## 🚀 설치 및 설정 / Installation and Setup

### 1. 라즈베리파이 서버 설정 / Raspberry Pi Server Setup

```bash
# 프로젝트 파일 복사 / Copy project file
scp raspberry_monitor_server.py pi@your_pi_ip:~/

# 실행 권한 부여 / Grant execution permission
chmod +x raspberry_monitor_server.py

# 서버 실행 / Run server
python3 raspberry_monitor_server.py
```

### 2. Windows 클라이언트 빌드 / Windows Client Build

1. **Borland C++ Builder에서 프로젝트 열기 / Open project in Borland C++ Builder**:
   ```
   ADS-B-Display.cbproj
   ```

2. **필요한 파일들이 포함되어 있는지 확인 / Verify that required files are included**:
   - `HealthMonitor_UI.cpp/.h/.dfm`
   - `HealthMonitor_Alert.cpp/.h`
   - `HealthMonitor_MetricData.h`
   - `HealthMonitor_Comm_Parsing.cpp/.h`
   - `HealthMonitor_Comm_TCPSocket.cpp/.h`

3. **프로젝트 빌드 / Build project**:
   - `Build` → `Build ADS-B-Display`

## 📖 사용 방법 / Usage Guide

### 1. 서버 시작 / Start Server
```bash
# 라즈베리파이에서 / On Raspberry Pi
python3 raspberry_monitor_server.py
```

### 2. 클라이언트 연결 / Client Connection
1. ADS-B Display 애플리케이션 실행 / Run ADS-B Display application
2. 메뉴에서 **Health Monitor** 클릭 / Click **Health Monitor** in menu
3. IP 주소 입력 (기본값: `192.168.0.190`) / Enter IP address (default: `192.168.0.190`)
4. **Connect** 버튼 클릭 / Click **Connect** button

### 3. 모니터링 시작 / Start Monitoring
- 연결 성공 시 실시간 데이터 표시 시작 / Real-time data display begins when connection is successful
- 각 메트릭은 진행 표시줄과 수치로 표시 / Each metric is displayed with progress bars and numerical values
- 임계값 초과 시 자동 알림 표시 / Automatic alerts when thresholds are exceeded

## ⚙️ 설정 옵션 / Configuration Options

### 알림 임계값 / Alert Thresholds (HealthMonitor_Alert.h)

```cpp
// 기본 임계값 / Default Thresholds
CPU_THRESHOLD = 80.0%        // CPU 사용률 / CPU Usage
MEMORY_THRESHOLD = 80.0%     // 메모리 사용률 / Memory Usage
TEMPERATURE_THRESHOLD = 70.0°C  // CPU 온도 / CPU Temperature
DISK_THRESHOLD = 90.0%       // 디스크 사용률 / Disk Usage

// CPU 지속 시간 / CPU Sustained Duration
CPU_SUSTAINED_DURATION = 5초 / 5 seconds
```

### 네트워크 설정 / Network Settings

```cpp
// 기본 설정 / Default Settings
DEFAULT_PORT = 5001
DEFAULT_IP = "192.168.0.190"
UPDATE_INTERVAL = 100ms      // UI 업데이트 / UI Update
DATA_INTERVAL = 1000ms       // 서버 데이터 수집 / Server Data Collection
LATENCY_TIMEOUT = 5000ms     // 연결 타임아웃 / Connection Timeout
```

## 📊 데이터 프로토콜 / Data Protocol

### 메시지 형식 / Message Format
```
TIMER={elapsed_time}|CPU:{usage}/{max}|MEM:{used}/{total}|TEMP:{temp}/{max}|DISK:{percent}/100|UPTIME:{time}|POWER:{voltage}V/{current}A|CRC={checksum}
```

### 예시 / Example
```
TIMER=15|CPU:25.3/100.0|MEM:512/1024|TEMP:45.2/85.0|DISK:67/100|UPTIME:2d 14:30:25|POWER:5.1V/0.6A|CRC=A1B2C3D4
```

## 🔧 문제 해결 / Troubleshooting

### 연결 문제 / Connection Issues

**문제 / Issue**: "연결할 수 없습니다" / "Cannot connect"
- **해결방법 / Solution**:
  1. 라즈베리파이 IP 주소 확인 / Check Raspberry Pi IP address
  2. 포트 5001이 열려있는지 확인 / Verify port 5001 is open
  3. 방화벽 설정 확인 / Check firewall settings
  4. 서버가 실행 중인지 확인 / Confirm server is running
  5. 연결 최대 대수 확인(1대) / Check the maximum client of connection

**문제 / Issue**: "지연시간 초과" / "Timeout exceeded"
- **해결방법 / Solution**:
  1. 네트워크 연결 상태 확인 / Check network connection status
  2. 라즈베리파이 부하 상태 확인 / Check Raspberry Pi load status
  3. 무선 연결의 경우 신호 강도 확인 / Check signal strength for wireless connection

### 데이터 문제 / Data Issues

**문제 / Issue**: "CRC 오류" / "CRC error"
- **해결방법 / Solution**:
  1. 네트워크 안정성 확인 / Check network stability
  2. 서버 재시작 / Restart server
  3. 클라이언트 재연결 / Reconnect client

**문제 / Issue**: "센서 데이터 오류" / "Sensor data error"
- **해결방법 / Solution**:
  1. 라즈베리파이 하드웨어 상태 확인 / Check Raspberry Pi hardware status
  2. 권한 문제 확인 (`sudo` 필요할 수 있음) / Check permission issues (`sudo` may be required)

## 🧪 테스트 / Testing

### 테스트 구조 / Test Structure
```
tests/
├── CMakeLists.txt                          # CMake 빌드 설정 / CMake build configuration
├── HealthMonitor_Alert_test.cpp            # 알림 시스템 테스트 / Alert system tests
├── HealthMonitor_Comm_Parsing_test.cpp     # 통신 파싱 테스트 / Communication parsing tests
└── build/                                  # 빌드 결과물 / Build artifacts
```

### 단위 테스트 / Unit Testing

#### CMake를 사용한 빌드 / Build with CMake
```bash
# tests 디렉토리로 이동 / Navigate to tests directory
cd ADS-B-Display/tests

# 빌드 디렉토리 생성 / Create build directory
mkdir -p build && cd build

# CMake 구성 / Configure CMake
cmake ..

# 빌드 실행 / Execute build
make

# 테스트 실행 / Run tests
./health_monitor_test
```

#### Google Test를 사용한 테스트 실행 / Running Tests with Google Test
```bash
# 모든 테스트 실행 / Run all tests
./health_monitor_test
```

### 테스트 범위 / Test Coverage

#### 알림 시스템 테스트 / Alert System Tests
- **경계값 분석 / Boundary Value Analysis**: 임계값 경계 테스트
- **지속 시간 테스트 / Duration Tests**: CPU 5초 지속 알림 테스트
- **유효성 검사 / Validation Tests**: 잘못된 데이터 처리 테스트
- **상태 관리 / State Management**: 알림 상태 리셋 테스트

#### 통신 파싱 테스트 / Communication Parsing Tests
- **데이터 파싱 / Data Parsing**: 모든 메트릭 파싱 테스트
- **CRC 검증 / CRC Verification**: 데이터 무결성 검증 테스트
- **오류 처리 / Error Handling**: 잘못된 형식 처리 테스트
- **네트워크 오류 / Network Errors**: 연결 실패 시나리오 테스트

### 통합 테스트 / Integration Testing
1. **라즈베리파이 서버 시작 / Start Raspberry Pi server**
   ```bash
   python3 raspberry_monitor_server.py
   ```

2. **Windows 클라이언트 연결 / Connect Windows client**
   - Health Monitor UI 실행 / Run Health Monitor UI
   - 연결 테스트 / Test connection

3. **전체 시스템 검증 / Full System Verification**
   - 모든 메트릭 정상 표시 확인 / Verify all metrics display properly
   - 알림 기능 테스트 / Test alert functionality
   - 네트워크 오류 복구 테스트 / Test network error recovery

## 📈 확장 가능성 / Scalability

### 추가 메트릭 / Additional Metrics
- 네트워크 트래픽 / Network traffic

### 새로운 기능 / New Features
- 히스토리 그래프 / Historical graphs
- 데이터 로깅 / Data logging
- 이메일 알림 / Email notifications