#ifndef HEALTHMONITOR_UI_H
#define HEALTHMONITOR_UI_H

#include "HealthMonitor_Communication.h"
#include "HealthMonitor_MetricData.h"
#include <vcl.h>

class THealthMonitorUI : public TForm {
private:
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

  THealthMonitorCommunication *Communication;

  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ConnectButtonClick(TObject *Sender);
  void __fastcall UpdateTimerTimer(TObject *Sender);

public:
  __fastcall THealthMonitorUI(TComponent *Owner);

  void UpdateCPUUI(const CPUMetricData &data);
  void UpdateMemoryUI(const MemoryMetricData &data);
  void UpdateTemperatureUI(const TemperatureMetricData &data);
  void UpdateDiskUI(const DiskMetricData &data);
  void UpdateUptimeUI(const UptimeMetricData &data);
};

extern PACKAGE THealthMonitorUI *HealthMonitorUI;

#endif