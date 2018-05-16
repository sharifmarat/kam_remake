object FormLogistics: TFormLogistics
  Left = 0
  Top = 0
  Caption = 'FormLogistics'
  ClientHeight = 615
  ClientWidth = 607
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 609
    Height = 617
    TabOrder = 0
    Tabs.Strings = (
      'Deliveries'
      'Offers'
      'Demands')
    TabIndex = 0
    OnChange = TabControl1Change
    object DeliveriesList: TListView
      Left = 4
      Top = 24
      Width = 601
      Height = 589
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'Resource'
          Width = 120
        end
        item
          Caption = 'From'
          Width = 120
        end
        item
          Caption = 'To'
          Width = 120
        end
        item
          Caption = 'Serf'
          Width = 100
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
    object DemandsList: TListView
      Left = 4
      Top = 24
      Width = 601
      Height = 589
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'Resource'
          Width = 120
        end
        item
          Caption = 'To'
          Width = 120
        end
        item
          Caption = 'Type'
          Width = 80
        end
        item
          Caption = 'Importance'
          Width = 80
        end
        item
          Caption = 'Performed'
          Width = 80
        end
        item
          Caption = 'Deleted'
          Width = 80
        end>
      TabOrder = 2
      ViewStyle = vsReport
      Visible = False
    end
    object OffersList: TListView
      Left = 4
      Top = 24
      Width = 601
      Height = 589
      Align = alClient
      Columns = <
        item
          Caption = '#'
          Width = 30
        end
        item
          Caption = 'Resource'
          Width = 120
        end
        item
          Caption = 'HouseFrom'
          Width = 120
        end
        item
          Caption = 'Count'
          Width = 80
        end
        item
          Caption = 'Performed'
          Width = 80
        end
        item
          Caption = 'Deleted'
          Width = 80
        end>
      TabOrder = 1
      ViewStyle = vsReport
      Visible = False
    end
  end
end
