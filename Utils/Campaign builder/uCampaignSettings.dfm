object fCampaignSettings: TfCampaignSettings
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Campaign Settings'
  ClientHeight = 169
  ClientWidth = 302
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
  object ShortNameLabel: TLabel
    Left = 8
    Top = 41
    Width = 65
    Height = 16
    AutoSize = False
    Caption = 'Short name'
    Layout = tlCenter
  end
  object IntoVideoLabel: TLabel
    Left = 8
    Top = 71
    Width = 80
    Height = 16
    AutoSize = False
    Caption = 'Intro video'
    Layout = tlCenter
  end
  object edtName: TEdit
    Left = 95
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
  object edtIntroVideo: TEdit
    Left = 95
    Top = 67
    Width = 200
    Height = 24
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 200
    ParentFont = False
    TabOrder = 1
  end
  object bOk: TButton
    Left = 137
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
  end
  object bCancel: TButton
    Left = 219
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object CheckNodeAnimation: TCheckBox
    Left = 8
    Top = 104
    Width = 145
    Height = 17
    Caption = 'Node Animation'
    TabOrder = 4
  end
  object edtShortName: TMaskEdit
    Left = 95
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
    TabOrder = 5
    Text = '   '
  end
end
