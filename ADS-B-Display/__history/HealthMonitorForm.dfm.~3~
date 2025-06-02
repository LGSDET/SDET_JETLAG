object HealthMonitorForm: THealthMonitorForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Raspberry Pi Health Monitor'
  ClientHeight = 350
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object MainPanel: TPanel
    Left = 8
    Top = 8
    Width = 384
    Height = 334
    TabOrder = 0
    object CPULabel: TLabel
      Left = 16
      Top = 16
      Width = 350
      Height = 13
      Caption = 'CPU Usage: 0.0% / 100.0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoryLabel: TLabel
      Left = 16
      Top = 72
      Width = 350
      Height = 13
      Caption = 'Memory Usage: 0 MB / 0 MB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TempLabel: TLabel
      Left = 16
      Top = 128
      Width = 350
      Height = 13
      Caption = 'CPU Temperature: 0.0°C / 85.0°C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DiskLabel: TLabel
      Left = 16
      Top = 184
      Width = 350
      Height = 13
      Caption = 'Disk Usage: 0% / 100%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object UptimeLabel: TLabel
      Left = 16
      Top = 240
      Width = 350
      Height = 13
      Caption = 'Uptime: 0 days 00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CPUProgressBar: TProgressBar
      Left = 16
      Top = 35
      Width = 350
      Height = 20
      TabOrder = 0
    end
    object MemoryProgressBar: TProgressBar
      Left = 16
      Top = 91
      Width = 350
      Height = 20
      TabOrder = 1
    end
    object TempProgressBar: TProgressBar
      Left = 16
      Top = 147
      Width = 350
      Height = 20
      Max = 85
      TabOrder = 2
    end
    object DiskProgressBar: TProgressBar
      Left = 16
      Top = 203
      Width = 350
      Height = 20
      TabOrder = 3
    end
    object ConnectButton: TButton
      Left = 16
      Top = 299
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 4
      OnClick = ConnectButtonClick
    end
    object IPAddressEdit: TEdit
      Left = 97
      Top = 301
      Width = 269
      Height = 21
      TabOrder = 5
      Text = '192.168.0.190'
    end
  end
  object UpdateTimer: TTimer
    Enabled = False
    Interval = 1000
    OnTimer = UpdateTimerTimer
    Left = 360
    Top = 8
  end
  object MonitorTCPClient: TIdTCPClient
    OnConnected = MonitorTCPClientConnected
    OnDisconnected = MonitorTCPClientDisconnected
    Left = 456
    Top = 344
  end
end 