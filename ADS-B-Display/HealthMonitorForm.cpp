#include <vcl.h>
#pragma hdrstop

#include "HealthMonitorForm.h"

#pragma package(smart_init)
#pragma resource "*.dfm"

THealthMonitorForm *HealthMonitorForm;

__fastcall THealthMonitorForm::THealthMonitorForm(TComponent* Owner)
    : TForm(Owner)
{
    isConnected = false;
    
    // TCP 클라이언트 초기화
    MonitorTCPClient = new TIdTCPClient(this);
    MonitorTCPClient->Port = 5001;  // 모니터링용 포트
    MonitorTCPClient->OnConnected = MonitorTCPClientConnected;
    MonitorTCPClient->OnDisconnected = MonitorTCPClientDisconnected;
    
    // 타이머 설정 (1초마다 업데이트)
    UpdateTimer->Interval = 1000;
    UpdateTimer->Enabled = false;
}

void __fastcall THealthMonitorForm::FormCreate(TObject *Sender)
{
    // 초기 UI 설정
    CPUProgressBar->Min = 0;
    CPUProgressBar->Max = 100;
    MemoryProgressBar->Min = 0;
    MemoryProgressBar->Max = 100;
    TempProgressBar->Min = 0;
    TempProgressBar->Max = 100;
    DiskProgressBar->Min = 0;
    DiskProgressBar->Max = 100;
    
    IPAddressEdit->Text = "192.168.0.190";  // 기본 IP 주소
    ConnectButton->Caption = "연결";
}

void __fastcall THealthMonitorForm::FormDestroy(TObject *Sender)
{
    if (MonitorTCPClient->Connected()) {
        MonitorTCPClient->Disconnect();
    }
    UpdateTimer->Enabled = false;
}

void __fastcall THealthMonitorForm::ConnectButtonClick(TObject *Sender)
{
    if (!isConnected) {
        try {
            MonitorTCPClient->Host = IPAddressEdit->Text;
            MonitorTCPClient->Connect();
        }
        catch (Exception &e) {
            ShowMessage("연결 실패: " + e.Message);
        }
    }
    else {
        MonitorTCPClient->Disconnect();
        UpdateTimer->Enabled = false;
        ConnectButton->Caption = "연결";
        isConnected = false;
    }
}

void __fastcall THealthMonitorForm::MonitorTCPClientConnected(TObject *Sender)
{
    ConnectButton->Caption = "연결 해제";
    isConnected = true;
    UpdateTimer->Enabled = true;
}

void __fastcall THealthMonitorForm::MonitorTCPClientDisconnected(TObject *Sender)
{
    ConnectButton->Caption = "연결";
    isConnected = false;
    UpdateTimer->Enabled = false;
}

void __fastcall THealthMonitorForm::UpdateTimerTimer(TObject *Sender)
{
    UpdateSystemInfo();
}

void THealthMonitorForm::UpdateSystemInfo()
{
    if (!MonitorTCPClient->Connected()) return;
    
    try {
        // 시스템 정보 요청
        MonitorTCPClient->Socket->WriteLn("GET_SYSTEM_INFO");
        
        // 응답 수신
        String response = MonitorTCPClient->Socket->ReadLn();
        ParseSystemInfo(response);
    }
    catch (Exception &e) {
        ShowMessage("데이터 수신 오류: " + e.Message);
        MonitorTCPClient->Disconnect();
    }
}

void THealthMonitorForm::ParseSystemInfo(const String& data)
{
    // 데이터 형식: "CPU:50|MEM:60|TEMP:45|DISK:75"
    try {
        TStringList* items = new TStringList();
        items->Delimiter = '|';
        items->DelimitedText = data;
        
        for (int i = 0; i < items->Count; i++) {
            String item = items->Strings[i];
            String key = item.SubString(1, item.Pos(":") - 1);
            int value = StrToInt(item.SubString(item.Pos(":") + 1, item.Length()));
            
            if (key == "CPU") {
                CPUProgressBar->Position = value;
                CPULabel->Caption = "CPU 사용량: " + IntToStr(value) + "%";
            }
            else if (key == "MEM") {
                MemoryProgressBar->Position = value;
                MemoryLabel->Caption = "메모리 사용량: " + IntToStr(value) + "%";
            }
            else if (key == "TEMP") {
                TempProgressBar->Position = value;
                TempLabel->Caption = "CPU 온도: " + IntToStr(value) + "°C";
            }
            else if (key == "DISK") {
                DiskProgressBar->Position = value;
                DiskLabel->Caption = "디스크 사용량: " + IntToStr(value) + "%";
            }
        }
        
        delete items;
    }
    catch (Exception &e) {
        ShowMessage("데이터 파싱 오류: " + e.Message);
    }
} 