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

class THealthMonitorForm : public TForm
{
__published:
    TPanel *MainPanel;
    TLabel *CPULabel;
    TLabel *MemoryLabel;
    TLabel *TempLabel;
    TLabel *DiskLabel;
    TLabel *UptimeLabel;
    TLabel *PowerLabel;
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
    void ParseSystemInfo(const String& data);
    void ClearAndDisableValues();
    bool VerifyCRC32(const String& data, const String& receivedCRC);
    
public:
    __fastcall THealthMonitorForm(TComponent* Owner);
};

extern PACKAGE THealthMonitorForm *HealthMonitorForm;

#endif 