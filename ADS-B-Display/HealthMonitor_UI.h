#ifndef HEALTHMONITOR_UI_H
#define HEALTHMONITOR_UI_H

#include <vcl.h>
#pragma hdrstop

#include "HealthMonitor_Communication.h"
#include "HealthMonitor_MetricData.h"

class THealthMonitorUI : public TForm {
__published:
  TProgressBar *CPUProgressBar;
  TProgressBar *MemoryProgressBar;
  TProgressBar *TempProgressBar;
  TProgressBar *DiskProgressBar;

  TLabel *CPULabel;
  TLabel *MemoryLabel;
  TLabel *TempLabel;
  TLabel *DiskLabel;
  TLabel *UptimeLabel;

  TEdit *IPAddressEdit;
  TButton *ConnectButton;
  TTimer *UpdateTimer;

  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ConnectButtonClick(TObject *Sender);
  void __fastcall UpdateTimerTimer(TObject *Sender);

private:  // User declarations
  THealthMonitorCommunication *Communication;

public:   // User declarations
  __fastcall THealthMonitorUI(TComponent *Owner);

  void UpdateCPUUI(const CPUMetricData &data);
  void UpdateMemoryUI(const MemoryMetricData &data);
  void UpdateTemperatureUI(const TemperatureMetricData &data);
  void UpdateDiskUI(const DiskMetricData &data);
  void UpdateUptimeUI(const UptimeMetricData &data);
};

extern PACKAGE THealthMonitorUI *HealthMonitorUI;

#endif