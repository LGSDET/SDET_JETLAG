#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_UI.h"
#include "HealthMonitor_Comm_TCPSocket.h"

// Form 리소스 파일 연결
#pragma resource "*.dfm"
THealthMonitorUI *HealthMonitorUI;

// VCL Forms 디자이너가 생성한 DFM을 사용하도록 설정
static inline void ValidCtrCheck(THealthMonitorUI *) {
  new THealthMonitorUI(NULL);
}

__fastcall THealthMonitorUI::THealthMonitorUI(TComponent *Owner)
    : TForm(Owner) {
  // 파싱 클래스와 네트워크 클래스 초기화
  Communication = new THealthMonitorCommunication();  // 순수 파싱 클래스
  Network = new THealthMonitorNetwork(this);          // VCL 네트워크 클래스
  AlertMonitor = new THealthMonitorAlert();           // 알림 시스템 초기화

  // 네트워크 콜백 설정
  Network->SetOnConnected([this]() { HandleConnectionStateChange(true); });
  Network->SetOnDisconnected([this]() { HandleConnectionStateChange(false); });

  // 타이머 설정 (100ms마다 업데이트하여 더 빠른 응답성 제공)
  UpdateTimer->Interval = 100;
  UpdateTimer->Enabled = false;

  // 지연시간 레이블 생성 및 설정 (이미 DFM에 정의되어 있음)
  LatencyLabel->Caption = "Latency: 0 ms";
  LatencyLabel->Font->Color = clGreen;
  
  // 네트워크 오류 레이블 초기화
  NetworkErrorLabel->Caption = "네트워크 오류";
  NetworkErrorLabel->Font->Color = clRed;
  NetworkErrorLabel->Font->Style = TFontStyles() << fsBold;
  NetworkErrorLabel->Visible = false;  // 초기에는 숨김
  
  // 폼 크기가 변경될 때 위치 조정
  this->OnResize = FormResize;
}

void __fastcall THealthMonitorUI::FormCreate(TObject *Sender) {
  // 초기 UI 설정
  CPUProgressBar->Min = 0;
  CPUProgressBar->Max = 100;
  MemoryProgressBar->Min = 0;
  MemoryProgressBar->Max = 100;
  TempProgressBar->Min = 0;
  TempProgressBar->Max = 85; // CPU 최대 온도 85도
  DiskProgressBar->Min = 0;
  DiskProgressBar->Max = 100;

  IPAddressEdit->Text = "192.168.0.190"; // 기본 IP 주소
  ConnectButton->Caption = "Connect";
}

void __fastcall THealthMonitorUI::FormDestroy(TObject *Sender) {
  try {
    if (UpdateTimer) {
      UpdateTimer->Enabled = false;
    }
    if (Network) {
      Network->Disconnect();
      delete Network;
      Network = nullptr;
    }
    if (Communication) {
      delete Communication;
      Communication = nullptr;
    }
    if (AlertMonitor) {
      delete AlertMonitor;
      AlertMonitor = nullptr;
    }
  } catch (...) {
    // 정리 작업 중 예외 발생시 무시
  }
}

void __fastcall THealthMonitorUI::ConnectButtonClick(TObject *Sender) {
  if (!Network->IsConnected()) {
    // VCL String을 std::string으로 변환
    std::string ipAddress = AnsiString(IPAddressEdit->Text).c_str();
    if (Network->Connect(ipAddress, 5001)) {
      // 연결 성공 - 콜백에서 HandleConnectionStateChange 호출됨
    } else {
      ShowMessage("Connection Failed");
    }
  } else {
    Network->Disconnect();
    // 연결 해제 - 콜백에서 HandleConnectionStateChange 호출됨
  }
}

void __fastcall THealthMonitorUI::FormResize(TObject *Sender) {
  if (LatencyLabel) {
    LatencyLabel->Left = this->ClientWidth - LatencyLabel->Width - 20;
  }
  
  if (NetworkErrorLabel) {
    // 네트워크 오류 레이블을 지연시간 레이블 아래에 위치
    NetworkErrorLabel->Left = this->ClientWidth - NetworkErrorLabel->Width - 20;
    if (LatencyLabel) {
      NetworkErrorLabel->Top = LatencyLabel->Top + LatencyLabel->Height + 5;
    }
  }
}

void __fastcall THealthMonitorUI::UpdateTimerTimer(TObject *Sender) {
  static int updateCounter = 0;
  
  if (!Network || !Communication) {
    UpdateTimer->Enabled = false;
    return;
  }
  
  // 지연시간은 매 타이머 틱마다 업데이트
  UpdateLatencyDisplay(Communication->currentLatency);
  
  // 네트워크 오류 상태 업데이트
  UpdateNetworkErrorDisplay(Communication->IsNetworkError());
  
  // 지연시간 초과로 연결이 끊어졌는지 확인
  if (!Network->IsConnected()) {
    HandleConnectionStateChange(false);
    if (Communication->IsLatencyExceeded()) {
      ShowMessage("서버와의 연결이 지연시간 초과로 종료되었습니다. (5초 초과)");
    }
    return;
  }
  
  // 시스템 정보는 1초마다 업데이트 (10번의 타이머 틱마다)
  if (++updateCounter >= 10) {
    updateCounter = 0;
    try {
      // 네트워크를 통해 데이터 요청
      if (Network->SendCommand("GET_SYSTEM_INFO")) {
        Communication->StartTimer();  // 타이머 시작
        std::string response = Network->ReceiveResponse();
        
        if (!response.empty() && Communication->ParseSystemInfo(response)) {
          // Update UI with parsed metric data
          UpdateCPUUI(Communication->GetCPUData());
          UpdateMemoryUI(Communication->GetMemoryData());
          UpdateTemperatureUI(Communication->GetTemperatureData());
          UpdateDiskUI(Communication->GetDiskData());
          UpdateUptimeUI(Communication->GetUptimeData());
          
          // 알림 확인 및 표시
          CheckAndShowAlerts();
          
          // 지연시간 초과 확인
          if (Communication->IsLatencyExceeded()) {
            Network->Disconnect();
          }
        }
      }
    } catch (...) {
      // 예외 발생시 연결 종료
      HandleConnectionStateChange(false);
    }
  }
}

void __fastcall THealthMonitorUI::MonitorTCPClientConnected(TObject *Sender) {
  HandleConnectionStateChange(true);
}

void __fastcall THealthMonitorUI::MonitorTCPClientDisconnected(TObject *Sender) {
  HandleConnectionStateChange(false);
}

void THealthMonitorUI::ResetUIElements() {
  // 모든 프로그레스바 초기화
  CPUProgressBar->Position = 0;
  MemoryProgressBar->Position = 0;
  TempProgressBar->Position = 0;
  DiskProgressBar->Position = 0;
  
  // 모든 레이블 초기화
  CPULabel->Caption = "CPU Usage: 0.0% / 100.0%";
  MemoryLabel->Caption = "Memory Usage: 0 MB / 0 MB";
  TempLabel->Caption = "CPU Temperature: 0.0°C / 85.0°C";
  DiskLabel->Caption = "Disk Usage: 0% / 100%";
  UptimeLabel->Caption = "Uptime: 0 days 00:00:00";
  
  // 지연시간 레이블 초기화
  LatencyLabel->Caption = "Latency: 0 ms";
  LatencyLabel->Font->Color = clGreen;
  
  // 네트워크 오류 레이블 초기화
  if (NetworkErrorLabel) {
    NetworkErrorLabel->Visible = false;
  }
  
  // 모든 경고 레이블 초기화
  ClearMetricAlert(CPUAlertLabel);
  ClearMetricAlert(MemoryAlertLabel);
  ClearMetricAlert(TempAlertLabel);
  ClearMetricAlert(DiskAlertLabel);
}

void THealthMonitorUI::HandleConnectionStateChange(bool connected) {
  try {
    if (connected) {
      ConnectButton->Caption = "Disconnect";
      UpdateTimer->Enabled = true;
    } else {
      ConnectButton->Caption = "Connect";
      UpdateTimer->Enabled = false;
      ResetUIElements();
    }
  } catch (...) {
    // UI 컴포넌트 접근 중 예외 발생시 타이머는 반드시 중지
    if (UpdateTimer) {
      UpdateTimer->Enabled = false;
    }
  }
}

void THealthMonitorUI::UpdateCPUUI(const CPUMetricData &data) {
  if (!data.isValid) {
    CPULabel->Caption = "CPU Usage: Error";
    return;
  }
  CPUProgressBar->Position = static_cast<int>(data.usage + 0.5);
  CPULabel->Caption =
      "CPU Usage: " + FloatToStrF(data.usage, ffFixed, 7, 1) + "%";
}

void THealthMonitorUI::UpdateMemoryUI(const MemoryMetricData &data) {
  if (!data.isValid) {
    MemoryLabel->Caption = "Memory Usage: Error";
    return;
  }
  MemoryProgressBar->Position = (data.currentUsage * 100) / data.totalMemory;
  MemoryLabel->Caption = "Memory Usage: " + IntToStr(data.currentUsage) + "/" +
                         IntToStr(data.totalMemory) + " MB";
}

void THealthMonitorUI::UpdateTemperatureUI(const TemperatureMetricData &data) {
  if (!data.isValid) {
    TempLabel->Caption = "CPU Temperature: Error";
    return;
  }
  TempProgressBar->Position = static_cast<int>(data.temperature + 0.5);
  TempLabel->Caption =
      "CPU Temperature: " + FloatToStrF(data.temperature, ffFixed, 7, 1) + "°C";
}

void THealthMonitorUI::UpdateDiskUI(const DiskMetricData &data) {
  if (!data.isValid) {
    DiskLabel->Caption = "Disk Usage: Error";
    return;
  }
  DiskProgressBar->Position = data.usagePercent;
  DiskLabel->Caption = "Disk Usage: " + IntToStr(data.usagePercent) + "%";
}

void THealthMonitorUI::UpdateUptimeUI(const UptimeMetricData &data) {
  if (!data.isValid) {
    UptimeLabel->Caption = "Uptime: Error";
    return;
  }
  if (data.days > 0) {
    UptimeLabel->Caption =
        "Uptime: " + IntToStr(data.days) + " days " + AnsiString(data.timeStr.c_str());
  } else {
    UptimeLabel->Caption = "Uptime: 0 days " + AnsiString(data.timeStr.c_str());
  }
}

void THealthMonitorUI::UpdateLatencyDisplay(int latency) {
  try {
    if (!LatencyLabel) return;
    
    // 0 이하의 지연시간은 0으로 보정
    int displayLatency = (latency <= 0) ? 0 : latency;
    
    LatencyLabel->Caption = "Latency: " + IntToStr(displayLatency) + " ms";
    
    // 지연시간에 따라 색상 변경
    if (displayLatency > 500) {
      LatencyLabel->Font->Color = clRed;
    } else {
      LatencyLabel->Font->Color = clGreen;
    }
    
    // 레이블 위치 업데이트 (텍스트가 바뀌면서 너비가 변할 수 있음)
    LatencyLabel->Left = this->ClientWidth - LatencyLabel->Width - 20;
  } catch (...) {
    // UI 업데이트 중 예외 발생시 무시
  }
}

void THealthMonitorUI::UpdateNetworkErrorDisplay(bool isNetworkError) {
  try {
    if (!NetworkErrorLabel) return;
    
    if (isNetworkError) {
      NetworkErrorLabel->Visible = true;
      NetworkErrorLabel->Caption = "네트워크 오류";
      NetworkErrorLabel->Font->Color = clRed;
      
      // 지연시간 레이블 아래에 위치
      NetworkErrorLabel->Left = this->ClientWidth - NetworkErrorLabel->Width - 20;
      if (LatencyLabel) {
        NetworkErrorLabel->Top = LatencyLabel->Top + LatencyLabel->Height + 5;
      }
    } else {
      NetworkErrorLabel->Visible = false;
    }
  } catch (...) {
    // UI 업데이트 중 예외 발생시 무시
  }
}

void THealthMonitorUI::CheckAndShowAlerts() {
  if (!AlertMonitor) return;
  
  // 각 메트릭에 대해 알림 확인
  CPUMetricData cpuData = Communication->GetCPUData();
  MemoryMetricData memData = Communication->GetMemoryData();
  TemperatureMetricData tempData = Communication->GetTemperatureData();
  DiskMetricData diskData = Communication->GetDiskData();
  
  // CPU 알림 확인
  AlertType cpuAlert = AlertMonitor->GetCPUAlertType(cpuData);
  if (cpuAlert != AlertType::NONE) {
    String message = "CPU 부하 높음 - 사용률 " + 
                     FloatToStrF(cpuData.usage, ffFixed, 7, 1) + 
                     "%가 5초 이상 80% 임계값을 초과";
    ShowMetricAlert(CPUAlertLabel, cpuAlert, message);
  } else {
    ClearMetricAlert(CPUAlertLabel);
  }
  
  // 메모리 알림 확인
  AlertType memAlert = AlertMonitor->GetMemoryAlertType(memData);
  if (memAlert != AlertType::NONE) {
    double usagePercent = (static_cast<double>(memData.currentUsage) / memData.totalMemory) * 100.0;
    String message = "메모리 부족 - 사용률 " + 
                     FloatToStrF(usagePercent, ffFixed, 7, 1) + 
                     "%가 80% 임계값을 초과";
    ShowMetricAlert(MemoryAlertLabel, memAlert, message);
  } else {
    ClearMetricAlert(MemoryAlertLabel);
  }
  
  // 온도 알림 확인
  AlertType tempAlert = AlertMonitor->GetTemperatureAlertType(tempData);
  if (tempAlert != AlertType::NONE) {
    String message = "고온 - CPU 온도 " + 
                     FloatToStrF(tempData.temperature, ffFixed, 7, 1) + 
                     "°C가 70°C 임계값을 초과";
    ShowMetricAlert(TempAlertLabel, tempAlert, message);
  } else {
    ClearMetricAlert(TempAlertLabel);
  }
  
  // 디스크 알림 확인
  AlertType diskAlert = AlertMonitor->GetDiskAlertType(diskData);
  if (diskAlert != AlertType::NONE) {
    String message = "디스크 용량 부족 - 사용률 " + 
                     IntToStr(diskData.usagePercent) + 
                     "%가 90% 임계값을 초과";
    ShowMetricAlert(DiskAlertLabel, diskAlert, message);
  } else {
    ClearMetricAlert(DiskAlertLabel);
  }
}

void THealthMonitorUI::ShowMetricAlert(TLabel *alertLabel, AlertType alertType, const String &message) {
  if (!alertLabel) return;
  
  // 현재 시간 문자열 생성 (yy-mm-dd, hh:mm:ss 형식)
  String timeStr = GetCurrentTimeString();
  
  // 경고 메시지 조합: [시간] 메시지
  String fullMessage = "[" + timeStr + "] " + message;
  
  // 레이블에 설정
  alertLabel->Caption = fullMessage;
  alertLabel->Visible = true;
  alertLabel->Font->Color = clRed;
}

void THealthMonitorUI::ClearMetricAlert(TLabel *alertLabel) {
  if (!alertLabel) return;
  
  alertLabel->Visible = false;
  alertLabel->Caption = "";
}

String THealthMonitorUI::GetCurrentTimeString() const {
  TDateTime now = Now();
  
  // yy-mm-dd, hh:mm:ss 형식으로 포맷
  String dateStr = FormatDateTime("yy-mm-dd", now);
  String timeStr = FormatDateTime("hh:nn:ss", now);
  
  return dateStr + ", " + timeStr;
}