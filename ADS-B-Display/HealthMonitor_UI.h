#ifndef HEALTHMONITOR_UI_H
#define HEALTHMONITOR_UI_H

#include <vcl.h>
#include <ExtCtrls.hpp>  // TPanel
#include <ComCtrls.hpp>  // TProgressBar
#include <StdCtrls.hpp>  // TLabel, TEdit, TButton
#include <IdTCPClient.hpp>  // TIdTCPClient
#pragma hdrstop

#include "HealthMonitor_Communication.h"
#include "HealthMonitor_MetricData.h"
#include "HealthMonitor_Alert.h"  // 알림 시스템 추가

class THealthMonitorUI : public TForm {
__published:
  TPanel *MainPanel;
  
  TProgressBar *CPUProgressBar;
  TProgressBar *MemoryProgressBar;
  TProgressBar *TempProgressBar;
  TProgressBar *DiskProgressBar;

  TLabel *CPULabel;
  TLabel *MemoryLabel;
  TLabel *TempLabel;
  TLabel *DiskLabel;
  TLabel *UptimeLabel;
  TLabel *LatencyLabel;  // 지연시간 표시 레이블

  // 각 메트릭별 경고 표시 레이블 추가
  TLabel *CPUAlertLabel;
  TLabel *MemoryAlertLabel;
  TLabel *TempAlertLabel;
  TLabel *DiskAlertLabel;

  TEdit *IPAddressEdit;
  TButton *ConnectButton;
  TTimer *UpdateTimer;
  TIdTCPClient *MonitorTCPClient;  // TCP 클라이언트 추가

  void __fastcall FormCreate(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall ConnectButtonClick(TObject *Sender);
  void __fastcall UpdateTimerTimer(TObject *Sender);
  void __fastcall MonitorTCPClientConnected(TObject *Sender);
  void __fastcall MonitorTCPClientDisconnected(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);

private:
  THealthMonitorCommunication *Communication;
  THealthMonitorAlert *AlertMonitor;  // 알림 시스템 추가
  
  // UI 관리 함수들
  void ResetUIElements();  // UI 요소들을 기본값으로 초기화
  void HandleConnectionStateChange(bool connected);  // 연결 상태 변경 처리
  void UpdateLatencyDisplay(int latency);  // 지연시간 표시 업데이트
  
  // 메트릭 데이터 UI 업데이트 함수들
  void UpdateCPUUI(const CPUMetricData &data);
  void UpdateMemoryUI(const MemoryMetricData &data);
  void UpdateTemperatureUI(const TemperatureMetricData &data);
  void UpdateDiskUI(const DiskMetricData &data);
  void UpdateUptimeUI(const UptimeMetricData &data);
  
  // 알림 관련 함수들
  void CheckAndShowAlerts();  // 모든 메트릭에 대해 알림 확인 및 표시
  void ShowMetricAlert(TLabel *alertLabel, AlertType alertType, const String &message);
  void ClearMetricAlert(TLabel *alertLabel);
  String GetCurrentTimeString() const;  // 현재 시간을 문자열로 반환

public:
  __fastcall THealthMonitorUI(TComponent *Owner);
};

extern PACKAGE THealthMonitorUI *HealthMonitorUI;

#endif