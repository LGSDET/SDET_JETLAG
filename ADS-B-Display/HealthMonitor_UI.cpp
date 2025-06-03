#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_UI.h"

// Form 리소스 파일 연결
#pragma resource "*.dfm"
THealthMonitorUI *HealthMonitorUI;

// VCL Forms 디자이너가 생성한 DFM을 사용하도록 설정
static inline void ValidCtrCheck(THealthMonitorUI *) {
  new THealthMonitorUI(NULL);
}

__fastcall THealthMonitorUI::THealthMonitorUI(TComponent *Owner)
    : TForm(Owner) {
  Communication = new THealthMonitorCommunication(this);

  // 타이머 설정 (1초마다 업데이트)
  UpdateTimer->Interval = 1000;
  UpdateTimer->Enabled = false;
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
  UpdateTimer->Enabled = false;
  delete Communication;
}

void __fastcall THealthMonitorUI::ConnectButtonClick(TObject *Sender) {
  if (!Communication->IsConnected()) {
    if (Communication->Connect(IPAddressEdit->Text)) {
      ConnectButton->Caption = "Disconnect";
      UpdateTimer->Enabled = true;
    } else {
      ShowMessage("Connection Failed");
    }
  } else {
    Communication->Disconnect();
    UpdateTimer->Enabled = false;
    ConnectButton->Caption = "Connect";
  }
}

void __fastcall THealthMonitorUI::UpdateTimerTimer(TObject *Sender) {
  Communication->UpdateSystemInfo();
}

void __fastcall THealthMonitorUI::MonitorTCPClientConnected(TObject *Sender) {
    // 연결이 성공했을 때의 처리
    ConnectButton->Caption = "Disconnect";
    UpdateTimer->Enabled = true;
    
    // 초기 상태 업데이트 요청
    Communication->UpdateSystemInfo();
}

void __fastcall THealthMonitorUI::MonitorTCPClientDisconnected(TObject *Sender) {
    // 연결이 끊어졌을 때의 처리
    ConnectButton->Caption = "Connect";
    UpdateTimer->Enabled = false;
    
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
        "Uptime: " + IntToStr(data.days) + " days " + data.timeStr;
  } else {
    UptimeLabel->Caption = "Uptime: 0 days " + data.timeStr;
  }
}