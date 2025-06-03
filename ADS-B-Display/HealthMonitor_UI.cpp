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

  // 지연시간 레이블 생성 및 설정
  LatencyLabel = new TLabel(this);
  LatencyLabel->Parent = this;
  LatencyLabel->Font->Size = 10;
  LatencyLabel->Font->Style = TFontStyles() << fsBold;  // 볼드체
  LatencyLabel->Caption = "Latency: 0 ms";
  LatencyLabel->Font->Color = clGreen;  // 초기 색상은 초록색
  
  // 폼의 오른쪽 위에 위치
  LatencyLabel->Top = 10;
  LatencyLabel->Left = this->ClientWidth - LatencyLabel->Width - 20;
  
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
  UpdateTimer->Enabled = false;
  delete Communication;
}

void __fastcall THealthMonitorUI::ConnectButtonClick(TObject *Sender) {
  if (!Communication->IsConnected()) {
    if (Communication->Connect(IPAddressEdit->Text)) {
      HandleConnectionStateChange(true);
    } else {
      ShowMessage("Connection Failed");
    }
  } else {
    Communication->Disconnect();
    HandleConnectionStateChange(false);
  }
}

void __fastcall THealthMonitorUI::FormResize(TObject *Sender) {
  if (LatencyLabel) {
    LatencyLabel->Left = this->ClientWidth - LatencyLabel->Width - 20;
  }
}

void __fastcall THealthMonitorUI::UpdateTimerTimer(TObject *Sender) {
  Communication->UpdateSystemInfo();
  
  // 지연시간 초과로 연결이 끊어졌는지 확인
  if (!Communication->IsConnected()) {
    HandleConnectionStateChange(false);
    ShowMessage("서버와의 연결이 지연시간 초과로 종료되었습니다. (5초 초과)");
    return;
  }
  
  // Update UI with stored metric data
  UpdateCPUUI(Communication->GetCPUData());
  UpdateMemoryUI(Communication->GetMemoryData());
  UpdateTemperatureUI(Communication->GetTemperatureData());
  UpdateDiskUI(Communication->GetDiskData());
  UpdateUptimeUI(Communication->GetUptimeData());
  UpdateLatencyDisplay(Communication->currentLatency);
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
}

void THealthMonitorUI::HandleConnectionStateChange(bool connected) {
  if (connected) {
    ConnectButton->Caption = "Disconnect";
    UpdateTimer->Enabled = true;
  } else {
    ConnectButton->Caption = "Connect";
    UpdateTimer->Enabled = false;
    ResetUIElements();
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
        "Uptime: " + IntToStr(data.days) + " days " + data.timeStr;
  } else {
    UptimeLabel->Caption = "Uptime: 0 days " + data.timeStr;
  }
}

void THealthMonitorUI::UpdateLatencyDisplay(int latency) {
  // 0 이하의 지연시간은 0으로 보정
  int displayLatency = (latency <= 0) ? 0 : latency;
  
  LatencyLabel->Caption = "Latency: " + IntToStr(displayLatency) + " ms";
  
  // 지연시간에 따라 색상 변경
  if (displayLatency > 500) {
    LatencyLabel->Font->Color = clRed;
  } else {
    LatencyLabel->Font->Color = clGreen;
  }
}