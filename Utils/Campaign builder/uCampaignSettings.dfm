object fCampaignSettings: TfCampaignSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Campaign Settings'
  ClientHeight = 136
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object CampaignNameLabel: TLabel
    Left = 8
    Top = 11
    Width = 80
    Height = 16
    AutoSize = False
    Caption = 'Campaign name'
    Layout = tlCenter
  end
  object CampaignIDLabel: TLabel
    Left = 8
    Top = 41
    Width = 65
    Height = 16
    AutoSize = False
    Caption = 'Campaign ID'
    Layout = tlCenter
  end
  object Label1: TLabel
    Left = 8
    Top = 70
    Width = 114
    Height = 13
    Caption = 'Node Animation (frame)'
  end
  object edtName: TEdit
    Left = 127
    Top = 7
    Width = 200
    Height = 24
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = 'Campaign name'
  end
  object bOk: TButton
    Left = 172
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 254
    Top = 103
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edtShortName: TMaskEdit
    Left = 127
    Top = 37
    Width = 58
    Height = 24
    EditMask = '>LLL;1;_'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 3
    ParentFont = False
    TabOrder = 3
    Text = '   '
  end
  object UpDownNodeAnimation: TUpDown
    Left = 185
    Top = 67
    Width = 16
    Height = 21
    Associate = EditNodeAnimation
    Max = 255
    Position = 5
    TabOrder = 4
  end
  object EditNodeAnimation: TEdit
    Left = 127
    Top = 67
    Width = 58
    Height = 21
    TabOrder = 5
    Text = '5'
  end
end
