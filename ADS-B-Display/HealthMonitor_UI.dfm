object HealthMonitorUI: THealthMonitorUI
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Raspberry Pi Health Monitor'
  ClientHeight = 450
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 450
    Align = alClient
    TabOrder = 0
    object LatencyLabel: TLabel
      Left = 680
      Top = 10
      Width = 100
      Height = 20
      Alignment = taRightJustify
      Caption = 'Latency: 0 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object NetworkErrorLabel: TLabel
      Left = 680
      Top = 35
      Width = 100
      Height = 20
      Alignment = taRightJustify
      Caption = 'Network Error'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object CPULabel: TLabel
      Left = 16
      Top = 16
      Width = 152
      Height = 13
      Caption = 'CPU Usage: 0.0% / 100.0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CPUAlertLabel: TLabel
      Left = 26
      Top = 56
      Width = 400
      Height = 13
      Caption = ''
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object MemoryLabel: TLabel
      Left = 16
      Top = 72
      Width = 157
      Height = 13
      Caption = 'Memory Usage: 0 MB / 0 MB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object MemoryAlertLabel: TLabel
      Left = 26
      Top = 112
      Width = 400
      Height = 13
      Caption = ''
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object TempLabel: TLabel
      Left = 16
      Top = 128
      Width = 185
      Height = 13
      Caption = 'CPU Temperature: 0.0'#176'C / 85.0'#176'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TempAlertLabel: TLabel
      Left = 26
      Top = 168
      Width = 400
      Height = 13
      Caption = ''
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object DiskLabel: TLabel
      Left = 16
      Top = 184
      Width = 134
      Height = 13
      Caption = 'Disk Usage: 0% / 100%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DiskAlertLabel: TLabel
      Left = 26
      Top = 224
      Width = 400
      Height = 13
      Caption = ''
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object UptimeLabel: TLabel
      Left = 16
      Top = 240
      Width = 135
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
    OnTimer = UpdateTimerTimer
    Left = 360
    Top = 8
  end
  object MonitorTCPClient: TIdTCPClient
    OnDisconnected = MonitorTCPClientDisconnected
    OnConnected = MonitorTCPClientConnected
    ConnectTimeout = 0
    Port = 0
    ReadTimeout = -1
    Left = 456
    Top = 344
  end
end
