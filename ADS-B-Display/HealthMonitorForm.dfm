object HealthMonitorForm: THealthMonitorForm
  Left = 0
  Top = 0
  Caption = 'Raspi-HealthMonitoring'
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 400
    Align = alClient
    TabOrder = 0
    object CPULabel: TLabel
      Left = 16
      Top = 16
      Width = 120
      Height = 13
      Caption = 'CPU 사용량: 0%'
    end
    object MemoryLabel: TLabel
      Left = 16
      Top = 80
      Width = 120
      Height = 13
      Caption = '메모리 사용량: 0%'
    end
    object TempLabel: TLabel
      Left = 16
      Top = 144
      Width = 120
      Height = 13
      Caption = 'CPU 온도: 0°C'
    end
    object DiskLabel: TLabel
      Left = 16
      Top = 208
      Width = 120
      Height = 13
      Caption = '디스크 사용량: 0%'
    end
    object CPUProgressBar: TProgressBar
      Left = 16
      Top = 35
      Width = 560
      Height = 20
      TabOrder = 0
    end
    object MemoryProgressBar: TProgressBar
      Left = 16
      Top = 99
      Width = 560
      Height = 20
      TabOrder = 1
    end
    object TempProgressBar: TProgressBar
      Left = 16
      Top = 163
      Width = 560
      Height = 20
      TabOrder = 2
    end
    object DiskProgressBar: TProgressBar
      Left = 16
      Top = 227
      Width = 560
      Height = 20
      TabOrder = 3
    end
    object ConnectButton: TButton
      Left = 16
      Top = 272
      Width = 75
      Height = 25
      Caption = '연결'
      TabOrder = 4
      OnClick = ConnectButtonClick
    end
    object IPAddressEdit: TEdit
      Left = 104
      Top = 274
      Width = 121
      Height = 21
      TabOrder = 5
      Text = '192.168.0.190'
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 1000
    OnTimer = UpdateTimerTimer
    Left = 544
    Top = 344
  end
  object MonitorTCPClient: TIdTCPClient
    OnConnected = MonitorTCPClientConnected
    OnDisconnected = MonitorTCPClientDisconnected
    Left = 456
    Top = 344
  end
end 