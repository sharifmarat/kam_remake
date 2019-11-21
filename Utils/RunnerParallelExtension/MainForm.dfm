object Paraller_Runner: TParaller_Runner
  Left = 0
  Top = 0
  Caption = 'Paraller_Runner'
  ClientHeight = 660
  ClientWidth = 1401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    1401
    660)
  PixelsPerInch = 96
  TextHeight = 13
  object lbClasses: TListBox
    Left = 8
    Top = 8
    Width = 185
    Height = 113
    ItemHeight = 13
    Items.Strings = (
      'TKMRunnerGA_CityBuilder'
      'TKMRunnerGA_CityPlanner'
      'TKMRunnerGA_Farm'
      'TKMRunnerGA_Forest'
      'TKMRunnerGA_HandLogistics'
      'TKMRunnerGA_Quarry'
      'TKMRunnerGA_RoadPlanner'
      'TKMRunnerGA_TestParRun')
    TabOrder = 3
  end
  object bRunSimulation: TButton
    Left = 104
    Top = 527
    Width = 89
    Height = 42
    Caption = 'Run'
    TabOrder = 1
    OnClick = bRunSimulationClick
  end
  object pcMainPages: TPageControl
    Left = 199
    Top = 8
    Width = 1202
    Height = 644
    ActivePage = tsGenes
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 2
    object tsFitness: TTabSheet
      Caption = 'Fitness'
      DesignSize = (
        1194
        616)
      object imgFitness: TImage
        Left = 3
        Top = 3
        Width = 1188
        Height = 610
        Anchors = [akLeft, akTop, akRight, akBottom]
      end
    end
    object tsGenes: TTabSheet
      Caption = 'Genes'
      ImageIndex = 1
      DesignSize = (
        1194
        616)
      object imgGenes: TImage
        Left = 3
        Top = 3
        Width = 1188
        Height = 610
        Anchors = [akLeft, akTop, akRight, akBottom]
      end
      object tbGeneSwitch: TTrackBar
        Left = 3
        Top = 0
        Width = 486
        Height = 33
        Anchors = [akTop, akRight]
        Max = 25
        Position = 10
        TabOrder = 0
        Visible = False
        OnChange = tbGeneSwitchChange
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Times'
      ImageIndex = 2
      DesignSize = (
        1194
        616)
      object Image3: TImage
        Left = 3
        Top = 3
        Width = 1188
        Height = 610
        Anchors = [akLeft, akTop, akRight, akBottom]
      end
    end
    object tsLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 3
      object mLog: TMemo
        Left = 0
        Top = 0
        Width = 1194
        Height = 616
        Align = alClient
        TabOrder = 0
      end
    end
  end
  object gbGA: TGroupBox
    Left = 8
    Top = 252
    Width = 185
    Height = 269
    Caption = 'GA parameters'
    TabOrder = 0
    object lGenerations: TLabel
      Left = 96
      Top = 24
      Width = 62
      Height = 13
      Caption = 'Generations:'
    end
    object lPopulation: TLabel
      Left = 10
      Top = 24
      Width = 54
      Height = 13
      Caption = 'Population:'
    end
    object lTournament: TLabel
      Left = 10
      Top = 71
      Width = 125
      Height = 13
      Caption = 'Individuals in tournament:'
    end
    object lResetGene: TLabel
      Left = 10
      Top = 118
      Width = 109
      Height = 13
      Caption = 'Probability reset gene:'
    end
    object lNormalMutation: TLabel
      Left = 10
      Top = 165
      Width = 134
      Height = 13
      Caption = 'Probability normal mutation:'
    end
    object lVariance: TLabel
      Left = 10
      Top = 212
      Width = 138
      Height = 13
      Caption = 'Variance of normal mutation:'
    end
    object seGenerations: TSpinEdit
      Left = 96
      Top = 43
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 0
      Value = 10
    end
    object sePopulation: TSpinEdit
      Left = 10
      Top = 43
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 1
      Value = 10
    end
    object eStartVariance: TEdit
      Left = 10
      Top = 231
      Width = 80
      Height = 21
      TabOrder = 2
      Text = '0.1'
    end
    object eEndVariance: TEdit
      Left = 96
      Top = 231
      Width = 80
      Height = 21
      TabOrder = 3
      Text = '0.1'
    end
    object eEndGaussMut: TEdit
      Left = 96
      Top = 185
      Width = 80
      Height = 21
      TabOrder = 4
      Text = '0.1'
    end
    object eStartResetGene: TEdit
      Left = 10
      Top = 137
      Width = 80
      Height = 21
      TabOrder = 5
      Text = '0.1'
    end
    object eEndResetGene: TEdit
      Left = 96
      Top = 137
      Width = 80
      Height = 21
      TabOrder = 6
      Text = '0.1'
    end
    object eStartGaussMut: TEdit
      Left = 10
      Top = 184
      Width = 80
      Height = 21
      TabOrder = 7
      Text = '0.1'
    end
    object seEndTournament: TSpinEdit
      Left = 96
      Top = 90
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 8
      Value = 10
    end
    object seStartTournament: TSpinEdit
      Left = 10
      Top = 90
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 9
      Value = 10
    end
  end
  object gbSim: TGroupBox
    Left = 8
    Top = 136
    Width = 185
    Height = 109
    Caption = 'Simulation parameters'
    TabOrder = 4
    object lThreads: TLabel
      Left = 10
      Top = 61
      Width = 43
      Height = 13
      Caption = 'Threads:'
    end
    object lMaps: TLabel
      Left = 10
      Top = 14
      Width = 29
      Height = 13
      Caption = 'Maps:'
    end
    object lDuration: TLabel
      Left = 96
      Top = 14
      Width = 72
      Height = 13
      Caption = 'Duration (min):'
    end
    object seThreads: TSpinEdit
      Left = 10
      Top = 77
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 0
      Value = 10
    end
    object seMaps: TSpinEdit
      Left = 10
      Top = 33
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 1
      Value = 10
    end
    object seDuration: TSpinEdit
      Left = 96
      Top = 33
      Width = 80
      Height = 22
      MaxValue = 1000000
      MinValue = 0
      TabOrder = 2
      Value = 60
    end
  end
  object bLoad: TButton
    Left = 8
    Top = 527
    Width = 90
    Height = 42
    Caption = 'Load'
    TabOrder = 5
    OnClick = bLoadClick
  end
end
