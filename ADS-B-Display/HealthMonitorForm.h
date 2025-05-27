#ifndef HealthMonitorFormH
#define HealthMonitorFormH

#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <ComCtrls.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>

class THealthMonitorForm : public TForm
{
__published:
    TPanel *MainPanel;
    TLabel *CPULabel;
    TLabel *MemoryLabel;
    TLabel *TempLabel;
    TLabel *DiskLabel;
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
    
public:
    __fastcall THealthMonitorForm(TComponent* Owner);
};

extern PACKAGE THealthMonitorForm *HealthMonitorForm;

#endif 