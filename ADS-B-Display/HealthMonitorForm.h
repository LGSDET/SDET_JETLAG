#ifndef HealthMonitorFormH
#define HealthMonitorFormH

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <Vcl.ExtCtrls.hpp>

// 각 메트릭의 데이터를 저장할 구조체들
struct CPUMetricData {
    double usage;
    bool isValid;
};

struct MemoryMetricData {
    int currentUsage;
    int totalMemory;
    bool isValid;
};

struct TemperatureMetricData {
    double temperature;
    double maxTemperature;
    bool isValid;
};

struct DiskMetricData {
    int usagePercent;
    bool isValid;
};

struct UptimeMetricData {
    int days;
    String timeStr;
    bool isValid;
};

class THealthMonitorForm : public TForm
{
__published:
    TPanel *MainPanel;
    TLabel *CPULabel;
    TLabel *MemoryLabel;
    TLabel *TempLabel;
    TLabel *DiskLabel;
    TLabel *UptimeLabel;
    TProgressBar *CPUProgressBar;
    TProgressBar *MemoryProgressBar;
    TProgressBar *TempProgressBar;
    TProgressBar *DiskProgressBar;
    TButton *ConnectButton;
    TEdit *IPAddressEdit;
    TTimer *UpdateTimer;
    TIdTCPClient *MonitorTCPClient;
    
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall ConnectButtonClick(TObject *Sender);
    void __fastcall UpdateTimerTimer(TObject *Sender);
    void __fastcall MonitorTCPClientConnected(TObject *Sender);
    void __fastcall MonitorTCPClientDisconnected(TObject *Sender);
    
private:
    bool isConnected;
    void UpdateSystemInfo();
    bool ParseSystemInfo(const String& data);
    bool VerifyCRC32(const String& data, const String& receivedCRC);
    
    // 파싱 함수들 - 테스트 가능한 값 반환
    CPUMetricData ParseCPUMetric(const String& value);
    MemoryMetricData ParseMemoryMetric(const String& value);
    TemperatureMetricData ParseTemperatureMetric(const String& value);
    DiskMetricData ParseDiskMetric(const String& value);
    UptimeMetricData ParseUptimeMetric(const String& value);
    
    // UI 업데이트 함수들
    void UpdateCPUUI(const CPUMetricData& data);
    void UpdateMemoryUI(const MemoryMetricData& data);
    void UpdateTemperatureUI(const TemperatureMetricData& data);
    void UpdateDiskUI(const DiskMetricData& data);
    void UpdateUptimeUI(const UptimeMetricData& data);
    
public:
    __fastcall THealthMonitorForm(TComponent* Owner);
};

extern PACKAGE THealthMonitorForm *HealthMonitorForm;

#endif 