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
    TempProgressBar->Max = 85;  // CPU 최대 온도 85도
    DiskProgressBar->Min = 0;
    DiskProgressBar->Max = 100;
    
    IPAddressEdit->Text = "192.168.0.190";  // 기본 IP 주소
    ConnectButton->Caption = "Connect";
    
    // 초기 상태를 비활성화로 설정
    ClearAndDisableValues();
}

void THealthMonitorForm::ClearAndDisableValues()
{
    // 프로그레스바 초기화
    CPUProgressBar->Position = 0;
    MemoryProgressBar->Position = 0;
    TempProgressBar->Position = 0;
    DiskProgressBar->Position = 0;
    
    // 레이블 텍스트 비활성화 상태로 변경
    CPULabel->Caption = "CPU Usage: N/A";
    CPULabel->Font->Color = clGray;
    
    MemoryLabel->Caption = "Memory Usage: N/A";
    MemoryLabel->Font->Color = clGray;
    
    TempLabel->Caption = "CPU Temperature: N/A";
    TempLabel->Font->Color = clGray;
    
    DiskLabel->Caption = "Disk Usage: N/A";
    DiskLabel->Font->Color = clGray;
    
    UptimeLabel->Caption = "Uptime: N/A";
    UptimeLabel->Font->Color = clGray;
    
    PowerLabel->Caption = "Power: N/A";
    PowerLabel->Font->Color = clGray;
    
    // 프로그레스바 비활성화 색상으로 변경
    CPUProgressBar->State = pbError;
    MemoryProgressBar->State = pbError;
    TempProgressBar->State = pbError;
    DiskProgressBar->State = pbError;
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
            MonitorTCPClient->Port = 5001;
            MonitorTCPClient->ConnectTimeout = 5000;  // 5초 타임아웃 설정
            MonitorTCPClient->ReadTimeout = 5000;     // 읽기 타임아웃 설정
            MonitorTCPClient->Connect();
        }
        catch (Exception &e) {
            ShowMessage("Connection Failed: " + e.Message);
            ClearAndDisableValues();  // 연결 실패 시 값 초기화
        }
    }
    else {
        MonitorTCPClient->Disconnect();
        UpdateTimer->Enabled = false;
        ConnectButton->Caption = "Connect";
        isConnected = false;
        ClearAndDisableValues();  // 연결 해제 시 값 초기화
    }
}

void __fastcall THealthMonitorForm::MonitorTCPClientConnected(TObject *Sender)
{
    ConnectButton->Caption = "Disconnect";
    isConnected = true;
    UpdateTimer->Enabled = true;
    
    // 연결 시 프로그레스바 상태 정상으로 변경
    CPUProgressBar->State = pbNormal;
    MemoryProgressBar->State = pbNormal;
    TempProgressBar->State = pbNormal;
    DiskProgressBar->State = pbNormal;
    
    // 레이블 색상을 활성화 상태로 변경
    CPULabel->Font->Color = clWindowText;
    MemoryLabel->Font->Color = clWindowText;
    TempLabel->Font->Color = clWindowText;
    DiskLabel->Font->Color = clWindowText;
    UptimeLabel->Font->Color = clWindowText;
    PowerLabel->Font->Color = clWindowText;
}

void __fastcall THealthMonitorForm::MonitorTCPClientDisconnected(TObject *Sender)
{
    ConnectButton->Caption = "Connect";
    isConnected = false;
    UpdateTimer->Enabled = false;
    ClearAndDisableValues();  // 연결 해제 시 값 초기화
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
    // 데이터 형식: "CPU:50.5/100|MEM:1024/4096|TEMP:45.7/85|DISK:75/100|UPTIME:01:30:45|POWER:5.1V/2.1A"
    try {
        TStringList* items = new TStringList();
        items->Delimiter = '|';
        items->DelimitedText = data;
        
        for (int i = 0; i < items->Count; i++) {
            String item = items->Strings[i];
            String key = item.SubString(1, item.Pos(":") - 1);
            String valueStr = item.SubString(item.Pos(":") + 1, item.Length());
            
            if (key == "CPU") {
                // CPU:현재/최대
                String current = valueStr.SubString(1, valueStr.Pos("/") - 1);
                String maximum = valueStr.SubString(valueStr.Pos("/") + 1, valueStr.Length());
                double currentVal = StrToFloat(current);
                CPUProgressBar->Position = static_cast<int>(currentVal + 0.5);
                CPULabel->Caption = "CPU Usage: " + FloatToStrF(currentVal, ffFixed, 7, 1) + "/" + maximum + "%";
            }
            else if (key == "MEM") {
                // MEM:현재/최대 (MB)
                String current = valueStr.SubString(1, valueStr.Pos("/") - 1);
                String maximum = valueStr.SubString(valueStr.Pos("/") + 1, valueStr.Length());
                int currentVal = StrToInt(current);
                int maxVal = StrToInt(maximum);
                MemoryProgressBar->Position = (currentVal * 100) / maxVal;
                MemoryLabel->Caption = "Memory Usage: " + current + "/" + maximum + " MB";
            }
            else if (key == "TEMP") {
                // TEMP:현재/최대
                String current = valueStr.SubString(1, valueStr.Pos("/") - 1);
                String maximum = valueStr.SubString(valueStr.Pos("/") + 1, valueStr.Length());
                double currentVal = StrToFloat(current);
                TempProgressBar->Position = static_cast<int>(currentVal + 0.5);
                TempLabel->Caption = "CPU Temperature: " + FloatToStrF(currentVal, ffFixed, 7, 1) + "/" + maximum + "°C";
            }
            else if (key == "DISK") {
                // DISK:현재/최대
                String current = valueStr.SubString(1, valueStr.Pos("/") - 1);
                String maximum = valueStr.SubString(valueStr.Pos("/") + 1, valueStr.Length());
                int currentVal = StrToInt(current);
                DiskProgressBar->Position = currentVal;
                DiskLabel->Caption = "Disk Usage: " + current + "/" + maximum + "%";
            }
            else if (key == "UPTIME") {
                // UPTIME:HH:MM:SS
                UptimeLabel->Caption = "Uptime: " + valueStr;
            }
            else if (key == "POWER") {
                // POWER:전압V/전류A
                String voltage = valueStr.SubString(1, valueStr.Pos("/") - 1);
                String current = valueStr.SubString(valueStr.Pos("/") + 1, valueStr.Length());
                PowerLabel->Caption = "Power: " + voltage + "/" + current;
            }
        }
        
        delete items;
    }
    catch (Exception &e) {
        ShowMessage("데이터 파싱 오류: " + e.Message);
    }
} 