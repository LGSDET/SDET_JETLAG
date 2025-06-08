![image](https://github.com/user-attachments/assets/8ddc41bd-db80-48e2-bbaf-48603a192501)
# Health Dashboard

<div align="center">

### 📂 Repository Navigation

**[📖 README](#readme)** • **[🇰🇷 한국어](#한국어)** • **[🇺🇸 English](#english)** • **[📋 License](#license)** • **[🤝 Code of Conduct](#code-of-conduct)** • **[🧪 Testing](#testing)**

</div>

---

## README

라즈베리파이 시스템 상태를 실시간으로 모니터링하는 Windows 기반 GUI 애플리케이션입니다.

A Windows-based GUI application for real-time monitoring of Raspberry Pi system status.

### 🎯 Quick Overview
- **Purpose**: Real-time Raspberry Pi system monitoring
- **Platform**: Windows GUI Application (VCL Framework)
- **Communication**: TCP Socket (Port 5001)
- **Data Integrity**: CRC32 Verification
- **Language**: C++ (Borland C++ Builder)

### 🚀 Quick Start
```bash
# Server (Raspberry Pi)
python3 raspberry_monitor_server.py

# Client (Windows)
# Build ADS-B-Display.cbproj in C++ Builder
```

### 📊 Key Metrics Monitored
| Metric | Threshold | Description |
|--------|-----------|-------------|
| 🖥️ CPU | 80% (5s sustained) | Real-time processor load |
| 💾 Memory | 80% | RAM usage monitoring |
| 🌡️ Temperature | 70°C | CPU temperature tracking |
| 💿 Disk | 90% | Storage space utilization |
| ⏱️ Uptime | - | System boot time |

---

## 한국어

<details>
<summary><h3>📋 개요</h3></summary>

Health Dashboard는 ADS-B Display 애플리케이션의 서브 모듈로, TCP 소켓을 통해 원격 라즈베리파이 서버로부터 시스템 메트릭을 수집하여 실시간으로 모니터링할 수 있는 Windows 기반 GUI 애플리케이션입니다.

</details>

<details>
<summary><h3>✨ 주요 기능</h3></summary>

#### 🔍 실시간 모니터링
- **CPU 사용률**: 실시간 프로세서 부하 상태
- **메모리 사용량**: 현재 메모리 사용률 및 총 용량
- **CPU 온도**: 시스템 온도 모니터링 (과열 방지)
- **디스크 사용량**: 저장공간 사용률
- **시스템 가동시간**: 부팅 후 경과 시간

#### ⚠️ 지능형 알림 시스템
- **임계값 기반 알림**: 각 메트릭별 사용자 정의 임계값
- **지속적 모니터링**: CPU는 5초 지속 초과 시에만 알림
- **시각적 표시**: 색상 코딩 (활성: 빨강, 해제: 회색)
- **타임스탬프**: 정확한 오류 발생 시간 기록

#### 🌐 안정적인 네트워크 통신
- **TCP 소켓**: 포트 5001을 통한 안정적 통신
- **CRC32 검증**: 데이터 무결성 보장
- **지연시간 모니터링**: 네트워크 상태 실시간 표시
- **자동 재연결**: 연결 오류 시 자동 복구

</details>

<details>
<summary><h3>🛠️ 시스템 요구사항</h3></summary>

#### 클라이언트 (Windows)
- **OS**: Windows 10 이상
- **개발환경**: Borland C++ Builder (VCL 지원)

#### 서버 (라즈베리파이)
- **OS**: Raspberry Pi OS (Debian 기반)
- **Python**: 3.7 이상
- **필수 패키지**:
  ```bash
  sudo apt update
  sudo apt install python3-pip
  pip3 install psutil gpiozero
  ```

</details>

<details>
<summary><h3>🚀 설치 및 설정</h3></summary>

#### 1. 라즈베리파이 서버 설정

```bash
# 프로젝트 파일 복사
scp raspberry_monitor_server.py pi@your_pi_ip:~/

# 실행 권한 부여
chmod +x raspberry_monitor_server.py

# 서버 실행
python3 raspberry_monitor_server.py
```

#### 2. Windows 클라이언트 빌드

1. **Borland C++ Builder에서 프로젝트 열기**:
   ```
   ADS-B-Display.cbproj
   ```

2. **필요한 파일들이 포함되어 있는지 확인**:
   - `HealthMonitor_UI.cpp/.h/.dfm`
   - `HealthMonitor_Alert.cpp/.h`
   - `HealthMonitor_MetricData.h`
   - `HealthMonitor_Comm_Parsing.cpp/.h`
   - `HealthMonitor_Comm_TCPSocket.cpp/.h`

3. **프로젝트 빌드**:
   - `Build` → `Build ADS-B-Display`

</details>

<details>
<summary><h3>📖 사용 방법</h3></summary>

#### 1. 서버 시작
```bash
# 라즈베리파이에서
python3 raspberry_monitor_server.py
```

#### 2. 클라이언트 연결
1. ADS-B Display 애플리케이션 실행
2. 메뉴에서 **Health Monitor** 클릭
3. IP 주소 입력 (기본값: `192.168.0.190`)
4. **Connect** 버튼 클릭

#### 3. 모니터링 시작
- 연결 성공 시 실시간 데이터 표시 시작
- 각 메트릭은 진행 표시줄과 수치로 표시
- 임계값 초과 시 자동 알림 표시

</details>

<details>
<summary><h3>⚙️ 설정 옵션</h3></summary>

#### 알림 임계값 (HealthMonitor_Alert.h)

```cpp
// 기본 임계값
CPU_THRESHOLD = 80.0%        // CPU 사용률
MEMORY_THRESHOLD = 80.0%     // 메모리 사용률
TEMPERATURE_THRESHOLD = 70.0°C  // CPU 온도
DISK_THRESHOLD = 90.0%       // 디스크 사용률

// CPU 지속 시간
CPU_SUSTAINED_DURATION = 5초
```

#### 네트워크 설정

```cpp
// 기본 설정
DEFAULT_PORT = 5001
DEFAULT_IP = "192.168.0.190"
UPDATE_INTERVAL = 100ms      // UI 업데이트
DATA_INTERVAL = 1000ms       // 서버 데이터 수집
LATENCY_TIMEOUT = 5000ms     // 연결 타임아웃
```

</details>

<details>
<summary><h3>📊 데이터 프로토콜</h3></summary>

#### 메시지 형식
```
TIMER={elapsed_time}|CPU:{usage}/{max}|MEM:{used}/{total}|TEMP:{temp}/{max}|DISK:{percent}/100|UPTIME:{time}|POWER:{voltage}V/{current}A|CRC={checksum}
```

#### 예시
```
TIMER=15|CPU:25.3/100.0|MEM:512/1024|TEMP:45.2/85.0|DISK:67/100|UPTIME:2d 14:30:25|POWER:5.1V/0.6A|CRC=A1B2C3D4
```

</details>

<details>
<summary><h3>🔧 문제 해결</h3></summary>

#### 연결 문제

**문제**: "연결할 수 없습니다"
- **해결방법**:
  1. 라즈베리파이 IP 주소 확인
  2. 포트 5001이 열려있는지 확인
  3. 방화벽 설정 확인
  4. 서버가 실행 중인지 확인
  5. 연결 최대 대수 확인(1대)

**문제**: "지연시간 초과"
- **해결방법**:
  1. 네트워크 연결 상태 확인
  2. 라즈베리파이 부하 상태 확인
  3. 무선 연결의 경우 신호 강도 확인

#### 데이터 문제

**문제**: "CRC 오류"
- **해결방법**:
  1. 네트워크 안정성 확인
  2. 서버 재시작
  3. 클라이언트 재연결

**문제**: "센서 데이터 오류"
- **해결방법**:
  1. 라즈베리파이 하드웨어 상태 확인
  2. 권한 문제 확인 (`sudo` 필요할 수 있음)

</details>

---

## English

<details>
<summary><h3>📋 Overview</h3></summary>

A Windows-based GUI application for real-time monitoring of Raspberry Pi system status. Health Dashboard is a sub-module of the ADS-B Display application that provides real-time monitoring capabilities by collecting system metrics from remote Raspberry Pi servers via TCP sockets.

</details>

<details>
<summary><h3>✨ Key Features</h3></summary>

#### 🔍 Real-time Monitoring
- **CPU Usage**: Real-time processor load status
- **Memory Usage**: Current memory usage and total capacity
- **CPU Temperature**: System temperature monitoring (overheating prevention)
- **Disk Usage**: Storage space utilization
- **System Uptime**: Elapsed time since boot

#### ⚠️ Intelligent Alert System
- **Threshold-based Alerts**: User-defined thresholds for each metric
- **Sustained Monitoring**: CPU alerts only after 5 seconds of sustained threshold violation
- **Visual Indication**: Color coding (active: red, cleared: gray)
- **Timestamp**: Accurate error occurrence time recording

#### 🌐 Reliable Network Communication
- **TCP Socket**: Reliable communication via port 5001
- **CRC32 Verification**: Data integrity assurance
- **Latency Monitoring**: Real-time network status display
- **Auto Reconnection**: Automatic recovery on connection errors

</details>

<details>
<summary><h3>🛠️ System Requirements</h3></summary>

#### Client (Windows)
- **OS**: Windows 10 or later
- **Development Environment**: Borland C++ Builder (VCL support)

#### Server (Raspberry Pi)
- **OS**: Raspberry Pi OS (Debian-based)
- **Python**: 3.7 or later
- **Required Packages**:
  ```bash
  sudo apt update
  sudo apt install python3-pip
  pip3 install psutil gpiozero
  ```

</details>

<details>
<summary><h3>🚀 Installation and Setup</h3></summary>

#### 1. Raspberry Pi Server Setup

```bash
# Copy project file
scp raspberry_monitor_server.py pi@your_pi_ip:~/

# Grant execution permission
chmod +x raspberry_monitor_server.py

# Run server
python3 raspberry_monitor_server.py
```

#### 2. Windows Client Build

1. **Open project in Borland C++ Builder**:
   ```
   ADS-B-Display.cbproj
   ```

2. **Verify that required files are included**:
   - `HealthMonitor_UI.cpp/.h/.dfm`
   - `HealthMonitor_Alert.cpp/.h`
   - `HealthMonitor_MetricData.h`
   - `HealthMonitor_Comm_Parsing.cpp/.h`
   - `HealthMonitor_Comm_TCPSocket.cpp/.h`

3. **Build project**:
   - `Build` → `Build ADS-B-Display`

</details>

<details>
<summary><h3>📖 Usage Guide</h3></summary>

#### 1. Start Server
```bash
# On Raspberry Pi
python3 raspberry_monitor_server.py
```

#### 2. Client Connection
1. Run ADS-B Display application
2. Click **Health Monitor** in menu
3. Enter IP address (default: `192.168.0.190`)
4. Click **Connect** button

#### 3. Start Monitoring
- Real-time data display begins when connection is successful
- Each metric is displayed with progress bars and numerical values
- Automatic alerts when thresholds are exceeded

</details>

<details>
<summary><h3>⚙️ Configuration Options</h3></summary>

#### Alert Thresholds (HealthMonitor_Alert.h)

```cpp
// Default Thresholds
CPU_THRESHOLD = 80.0%        // CPU Usage
MEMORY_THRESHOLD = 80.0%     // Memory Usage
TEMPERATURE_THRESHOLD = 70.0°C  // CPU Temperature
DISK_THRESHOLD = 90.0%       // Disk Usage

// CPU Sustained Duration
CPU_SUSTAINED_DURATION = 5 seconds
```

#### Network Settings

```cpp
// Default Settings
DEFAULT_PORT = 5001
DEFAULT_IP = "192.168.0.190"
UPDATE_INTERVAL = 100ms      // UI Update
DATA_INTERVAL = 1000ms       // Server Data Collection
LATENCY_TIMEOUT = 5000ms     // Connection Timeout
```

</details>

<details>
<summary><h3>📊 Data Protocol</h3></summary>

#### Message Format
```
TIMER={elapsed_time}|CPU:{usage}/{max}|MEM:{used}/{total}|TEMP:{temp}/{max}|DISK:{percent}/100|UPTIME:{time}|POWER:{voltage}V/{current}A|CRC={checksum}
```

#### Example
```
TIMER=15|CPU:25.3/100.0|MEM:512/1024|TEMP:45.2/85.0|DISK:67/100|UPTIME:2d 14:30:25|POWER:5.1V/0.6A|CRC=A1B2C3D4
```

</details>

<details>
<summary><h3>🔧 Troubleshooting</h3></summary>

#### Connection Issues

**Issue**: "Cannot connect"
- **Solution**:
  1. Check Raspberry Pi IP address
  2. Verify port 5001 is open
  3. Check firewall settings
  4. Confirm server is running
  5. Check the maximum number of connections (1 client)

**Issue**: "Timeout exceeded"
- **Solution**:
  1. Check network connection status
  2. Check Raspberry Pi load status
  3. Check signal strength for wireless connection

#### Data Issues

**Issue**: "CRC error"
- **Solution**:
  1. Check network stability
  2. Restart server
  3. Reconnect client

**Issue**: "Sensor data error"
- **Solution**:
  1. Check Raspberry Pi hardware status
  2. Check permission issues (`sudo` may be required)

</details>

---

## License

```
MIT License

Copyright (c) 2024 Health Dashboard

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

---

## Code of Conduct

### Our Pledge

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone, regardless of age, body size, visible or invisible disability, ethnicity, sex characteristics, gender identity and expression, level of experience, education, socio-economic status, nationality, personal appearance, race, religion, or sexual identity and orientation.

### Our Standards

Examples of behavior that contributes to a positive environment:
- Using welcoming and inclusive language
- Being respectful of differing viewpoints and experiences
- Gracefully accepting constructive criticism
- Focusing on what is best for the community
- Showing empathy towards other community members

Examples of unacceptable behavior:
- The use of sexualized language or imagery, and sexual attention or advances
- Trolling, insulting/derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others' private information without explicit permission
- Other conduct which could reasonably be considered inappropriate

### Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported to the community leaders responsible for enforcement. All complaints will be reviewed and investigated promptly and fairly.

---

## Testing

<details>
<summary><h3>🧪 Test Structure</h3></summary>

```
tests/
├── CMakeLists.txt                          # CMake build configuration
├── HealthMonitor_Alert_test.cpp            # Alert system tests
├── HealthMonitor_Comm_Parsing_test.cpp     # Communication parsing tests
└── build/                                  # Build artifacts
```

</details>

<details>
<summary><h3>🔧 Unit Testing</h3></summary>

#### Build with CMake
```bash
# Navigate to tests directory
cd ADS-B-Display/tests

# Create build directory
mkdir -p build && cd build

# Configure CMake
cmake ..

# Execute build
make

# Run tests
./health_monitor_test
```

</details>

<details>
<summary><h3>📈 Test Coverage</h3></summary>

#### Alert System Tests
- **Boundary Value Analysis**: Threshold boundary testing
- **Duration Tests**: CPU 5-second sustained alert testing
- **Validation Tests**: Invalid data handling tests
- **State Management**: Alert state reset tests

#### Communication Parsing Tests
- **Data Parsing**: All metric parsing tests
- **CRC Verification**: Data integrity verification tests
- **Error Handling**: Invalid format handling tests
- **Network Errors**: Connection failure scenario tests

</details>

---

<div align="center">

### 🎯 Project Status

![GitHub last commit](https://img.shields.io/github/last-commit/yourusername/health-dashboard)
![GitHub issues](https://img.shields.io/github/issues/yourusername/health-dashboard)
![GitHub pull requests](https://img.shields.io/github/issues-pr/yourusername/health-dashboard)
![License](https://img.shields.io/github/license/yourusername/health-dashboard)

**Health Dashboard** • Made with ❤️ for monitoring Raspberry Pi systems

</div>