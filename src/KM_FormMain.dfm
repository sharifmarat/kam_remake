object FormMain: TFormMain
  Left = 221
  Top = 419
  HelpType = htKeyword
  BorderStyle = bsNone
  ClientHeight = 785
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 765
    Width = 521
    Height = 20
    Panels = <
      item
        Text = 'KMR r7000+ / OpenGL 4.0.0 - Build 9.99.99.99999'
        Width = 275
      end
      item
        Text = 'Map size: 999x999'
        Width = 110
      end
      item
        Text = 'Cursor: 1999:1999'
        Width = 105
      end
      item
        Text = 'Tile: 999.9:999.9 [999:999]'
        Width = 140
      end
      item
        Text = 'Time: 99:99:99'
        Width = 90
      end
      item
        Text = '999.9 FPS (999)'
        Width = 85
      end
      item
        Text = 'Obj: 99999999'
        Width = 90
      end
      item
        Text = 'Control ID: 9999'
        Width = 80
      end>
  end
  object mainGroup: TCategoryPanelGroup
    Left = 261
    Top = 0
    Width = 260
    Height = 765
    VertScrollBar.Tracking = True
    Align = alRight
    DoubleBuffered = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -11
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = []
    HeaderHeight = 18
    HeaderStyle = hsThemed
    ParentDoubleBuffered = False
    TabOrder = 1
    object cpLogs: TCategoryPanel
      Top = 408
      Height = 179
      Caption = 'Logs'
      TabOrder = 0
      ExplicitTop = 434
      ExplicitWidth = 241
      object chkLogCommands: TCheckBox
        Left = 120
        Top = 8
        Width = 73
        Height = 17
        Caption = 'Commands'
        TabOrder = 0
        OnClick = ControlsUpdate
      end
      object chkLogDelivery: TCheckBox
        Left = 8
        Top = 8
        Width = 65
        Height = 17
        Caption = 'Delivery'
        TabOrder = 1
        OnClick = ControlsUpdate
      end
      object chkLogGameTick: TCheckBox
        Left = 8
        Top = 40
        Width = 87
        Height = 17
        Caption = 'Game tick'
        TabOrder = 2
        OnClick = ControlsUpdate
      end
      object chkLogNetConnection: TCheckBox
        Left = 8
        Top = 24
        Width = 95
        Height = 17
        Caption = 'Net connection'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = ControlsUpdate
      end
      object chkLogRngChecks: TCheckBox
        Left = 120
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Random Checks'
        TabOrder = 4
        OnClick = ControlsUpdate
      end
      object chkLogsShowInChat: TCheckBox
        Left = 8
        Top = 138
        Width = 137
        Height = 17
        Caption = 'Show logs in MP chat'
        TabOrder = 5
        OnClick = ControlsUpdate
      end
      object RGLogNetPackets: TRadioGroup
        Left = 8
        Top = 58
        Width = 161
        Height = 78
        Caption = 'Net packets log level'
        ItemIndex = 0
        Items.Strings = (
          'None '
          'All but commands/ping/fps'
          'All but ping/fps'
          'All packets')
        TabOrder = 6
        OnClick = ControlsUpdate
      end
    end
    object cpGraphicTweaks: TCategoryPanel
      Top = 384
      Height = 24
      Caption = 'Graphic tweaks'
      Collapsed = True
      TabOrder = 1
      ExplicitTop = 410
      ExplicitWidth = 241
      ExpandedHeight = 124
      object Label1: TLabel
        Left = 101
        Top = 55
        Width = 60
        Height = 13
        Caption = 'Building step'
      end
      object Label3: TLabel
        Left = 101
        Top = 7
        Width = 27
        Height = 13
        Caption = 'Angle'
      end
      object Label4: TLabel
        Left = 101
        Top = 23
        Width = 27
        Height = 13
        Caption = 'Angle'
      end
      object Label7: TLabel
        Left = 101
        Top = 39
        Width = 27
        Height = 13
        Caption = 'Angle'
      end
      object tbAngleX: TTrackBar
        Left = 5
        Top = 7
        Width = 95
        Height = 17
        Max = 90
        Min = -90
        PageSize = 5
        Frequency = 5
        TabOrder = 0
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = ControlsUpdate
      end
      object tbAngleY: TTrackBar
        Left = 5
        Top = 23
        Width = 95
        Height = 17
        Max = 90
        Min = -90
        PageSize = 5
        Frequency = 5
        TabOrder = 1
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = ControlsUpdate
      end
      object tbAngleZ: TTrackBar
        Left = 5
        Top = 39
        Width = 95
        Height = 17
        Max = 90
        Min = -90
        PageSize = 5
        Frequency = 5
        TabOrder = 2
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = ControlsUpdate
      end
      object tbBuildingStep: TTrackBar
        Left = 5
        Top = 55
        Width = 95
        Height = 17
        Max = 100
        TabOrder = 3
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = ControlsUpdate
      end
      object chkSnowHouses: TCheckBox
        Left = 8
        Top = 78
        Width = 86
        Height = 17
        Caption = 'Snow houses'
        TabOrder = 4
        OnClick = ControlsUpdate
      end
    end
    object cpUserInreface: TCategoryPanel
      Top = 360
      Height = 24
      Caption = 'User Interface'
      Collapsed = True
      TabOrder = 2
      ExplicitTop = 386
      ExplicitWidth = 241
      ExpandedHeight = 72
      object chkUIControlsBounds: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Controls bounds'
        TabOrder = 0
        OnClick = ControlsUpdate
      end
      object chkUIControlsID: TCheckBox
        Left = 120
        Top = 8
        Width = 73
        Height = 17
        Caption = 'Controls IDs'
        TabOrder = 1
        OnClick = ControlsUpdate
      end
      object chkUITextBounds: TCheckBox
        Left = 8
        Top = 24
        Width = 81
        Height = 17
        Caption = 'Text bounds'
        TabOrder = 2
        OnClick = ControlsUpdate
      end
    end
    object cpAI: TCategoryPanel
      Top = 336
      Height = 24
      Caption = 'AI'
      Collapsed = True
      TabOrder = 3
      ExplicitTop = 362
      ExplicitWidth = 241
      ExpandedHeight = 166
      object Label5: TLabel
        Left = 106
        Top = 111
        Width = 32
        Height = 13
        Caption = 'Margin'
        Visible = False
      end
      object Label6: TLabel
        Left = 106
        Top = 127
        Width = 47
        Height = 13
        Caption = 'Threshold'
        Visible = False
      end
      object chkAIEye: TCheckBox
        Left = 120
        Top = 24
        Width = 97
        Height = 17
        Caption = 'AI Eye'
        TabOrder = 0
        OnClick = ControlsUpdate
      end
      object chkBevel: TCheckBox
        Left = 120
        Top = 8
        Width = 88
        Height = 17
        Caption = 'Bevel'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = ControlsUpdate
      end
      object chkBuildAI: TCheckBox
        Left = 8
        Top = 24
        Width = 86
        Height = 17
        Caption = 'Build AI'
        TabOrder = 2
        OnClick = ControlsUpdate
      end
      object chkCombatAI: TCheckBox
        Left = 8
        Top = 88
        Width = 86
        Height = 17
        Caption = 'Combat AI'
        TabOrder = 3
        OnClick = ControlsUpdate
      end
      object chkShowAvoid: TCheckBox
        Left = 8
        Top = 56
        Width = 86
        Height = 17
        Caption = 'Avoid building'
        TabOrder = 4
        OnClick = ControlsUpdate
      end
      object chkShowBalance: TCheckBox
        Left = 8
        Top = 8
        Width = 88
        Height = 17
        Caption = 'AI balance text'
        TabOrder = 5
        OnClick = ControlsUpdate
      end
      object chkShowDefences: TCheckBox
        Left = 120
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Defences'
        TabOrder = 6
        OnClick = ControlsUpdate
      end
      object chkShowEyeRoutes: TCheckBox
        Left = 120
        Top = 72
        Width = 88
        Height = 17
        Caption = 'Routes'
        TabOrder = 7
        OnClick = ControlsUpdate
      end
      object chkShowFlatArea: TCheckBox
        Left = 120
        Top = 56
        Width = 88
        Height = 17
        Caption = 'Flat area'
        TabOrder = 8
        OnClick = ControlsUpdate
      end
      object chkShowNavMesh: TCheckBox
        Left = 8
        Top = 72
        Width = 86
        Height = 17
        Caption = 'Navmesh'
        TabOrder = 9
        OnClick = ControlsUpdate
      end
      object chkShowOwnership: TCheckBox
        Left = 8
        Top = 40
        Width = 86
        Height = 17
        Caption = 'Ownership'
        TabOrder = 10
        OnClick = ControlsUpdate
      end
      object chkShowSoil: TCheckBox
        Left = 120
        Top = 40
        Width = 88
        Height = 17
        Caption = 'Soil'
        TabOrder = 11
        OnClick = ControlsUpdate
      end
      object tbOwnMargin: TTrackBar
        Left = 2
        Top = 111
        Width = 101
        Height = 17
        Max = 255
        Min = 64
        Position = 64
        TabOrder = 12
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        Visible = False
        OnChange = ControlsUpdate
      end
      object tbOwnThresh: TTrackBar
        Left = 2
        Top = 127
        Width = 101
        Height = 17
        Max = 255
        Min = 64
        Position = 64
        TabOrder = 13
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        Visible = False
        OnChange = ControlsUpdate
      end
    end
    object cpPerfLogs: TCategoryPanel
      Top = 312
      Height = 24
      Caption = 'Perf Logs'
      Collapsed = True
      TabOrder = 4
      ExplicitTop = 338
      ExpandedHeight = 660
    end
    object cpDebugRender: TCategoryPanel
      Top = 73
      Height = 215
      Caption = 'Debug Render'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 5
      object Label2: TLabel
        Left = 100
        Top = 4
        Width = 49
        Height = 13
        Caption = 'Passability'
      end
      object btnUpdateUI: TButton
        Left = 5
        Top = 156
        Width = 79
        Height = 17
        Caption = 'Update UI'
        TabOrder = 0
        OnClick = Button_UpdateUI_Click
      end
      object chkHands: TCheckBox
        Left = 8
        Top = 117
        Width = 76
        Height = 17
        Caption = 'Hands'
        TabOrder = 1
        OnClick = ControlsUpdate
      end
      object chkSelectedObjInfo: TCheckBox
        Left = 120
        Top = 133
        Width = 84
        Height = 17
        Caption = 'Selection Info'
        TabOrder = 2
        OnClick = ControlsUpdate
      end
      object chkSelectionBuffer: TCheckBox
        Left = 8
        Top = 56
        Width = 97
        Height = 17
        Caption = 'Unit hitbox'
        TabOrder = 3
        OnClick = ControlsUpdate
      end
      object chkShowFPS: TCheckBox
        Left = 120
        Top = 156
        Width = 76
        Height = 17
        Caption = 'FPS'
        TabOrder = 4
        OnClick = ControlsUpdate
      end
      object chkShowGameTick: TCheckBox
        Left = 120
        Top = 172
        Width = 76
        Height = 17
        Caption = 'Game tick'
        TabOrder = 5
        OnClick = ControlsUpdate
      end
      object chkShowRoutes: TCheckBox
        Left = 8
        Top = 24
        Width = 81
        Height = 17
        Caption = 'Unit routes'
        TabOrder = 6
        OnClick = ControlsUpdate
      end
      object chkShowTerrainIds: TCheckBox
        Left = 120
        Top = 24
        Width = 79
        Height = 17
        Caption = 'Terrain IDs'
        TabOrder = 7
        OnClick = ControlsUpdate
      end
      object chkShowTerrainKinds: TCheckBox
        Left = 120
        Top = 40
        Width = 79
        Height = 17
        Caption = 'Terrain Kinds'
        TabOrder = 8
        OnClick = ControlsUpdate
      end
      object chkShowWires: TCheckBox
        Left = 8
        Top = 40
        Width = 76
        Height = 17
        Caption = 'Terrain wires'
        TabOrder = 9
        OnClick = ControlsUpdate
      end
      object chkSkipRender: TCheckBox
        Left = 8
        Top = 72
        Width = 81
        Height = 17
        Caption = 'Skip Render'
        TabOrder = 10
        OnClick = ControlsUpdate
      end
      object chkSkipSound: TCheckBox
        Left = 8
        Top = 88
        Width = 81
        Height = 17
        Caption = 'Skip Sound'
        TabOrder = 11
        OnClick = ControlsUpdate
      end
      object chkTilesGrid: TCheckBox
        Left = 120
        Top = 72
        Width = 79
        Height = 17
        Caption = 'Tiles grid'
        TabOrder = 12
        OnClick = ControlsUpdate
      end
      object chkUIDs: TCheckBox
        Left = 120
        Top = 117
        Width = 79
        Height = 17
        Caption = 'UIDs by T'
        TabOrder = 13
        OnClick = ControlsUpdate
      end
      object tbPassability: TTrackBar
        Left = 2
        Top = 4
        Width = 95
        Height = 17
        Max = 14
        PageSize = 1
        TabOrder = 14
        ThumbLength = 14
        TickMarks = tmBoth
        TickStyle = tsNone
        OnChange = ControlsUpdate
      end
      object chkJamMeter: TCheckBox
        Left = 120
        Top = 88
        Width = 79
        Height = 17
        Caption = 'Jam meter'
        TabOrder = 15
        OnClick = ControlsUpdate
      end
      object chkShowTerrainOverlays: TCheckBox
        Left = 120
        Top = 56
        Width = 95
        Height = 17
        Caption = 'Terrain overlays'
        TabOrder = 16
        OnClick = ControlsUpdate
      end
    end
    object cpGameAdv: TCategoryPanel
      Top = 49
      Height = 24
      Caption = 'Game additional'
      Collapsed = True
      TabOrder = 6
      ExpandedHeight = 136
      object chkLoadUnsupSaves: TCheckBox
        Left = 12
        Top = 8
        Width = 157
        Height = 17
        Caption = 'Allow load unsupported saves'
        TabOrder = 0
        OnClick = ControlsUpdate
      end
      object RGPlayer: TRadioGroup
        Left = 8
        Top = 30
        Width = 225
        Height = 75
        BiDiMode = bdLeftToRight
        Caption = ' Select player '
        Columns = 6
        ItemIndex = 0
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15'
          '16'
          '17'
          '18')
        ParentBiDiMode = False
        TabOrder = 1
        OnClick = RGPlayerClick
      end
    end
    object cpGameControls: TCategoryPanel
      Top = 0
      Height = 49
      Caption = 'Game '
      TabOrder = 7
      object chkSuperSpeed: TCheckBox
        Left = 8
        Top = 5
        Width = 75
        Height = 17
        Hint = 'Autosave is disabled while on very fast speedup'
        Caption = 'Speed x200'
        TabOrder = 0
        OnClick = chkSuperSpeedClick
      end
      object Button_Stop: TButton
        Left = 89
        Top = 5
        Width = 89
        Height = 17
        Caption = 'Stop the game'
        TabOrder = 1
        OnClick = Button_StopClick
      end
    end
    object cpScripting: TCategoryPanel
      Top = 288
      Height = 24
      Caption = 'Scripting'
      Collapsed = True
      TabOrder = 8
      ExpandedHeight = 50
      object chkDebugScripting: TCheckBox
        Left = 8
        Top = 8
        Width = 97
        Height = 17
        Hint = 
          'Show exect error position (col/row/module), but significantly sl' +
          'ow down script execution'
        Caption = 'Debug Scripting '
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = ControlsUpdate
      end
    end
  end
  object OpenDialog1: TOpenDialog
    InitialDir = '.'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 24
    Top = 64
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 8
    object File1: TMenuItem
      Caption = 'File'
      object OpenMissionMenu: TMenuItem
        Caption = 'Start mission...'
        OnClick = Open_MissionMenuClick
      end
      object MenuItem1: TMenuItem
        Caption = 'Edit mission...'
        OnClick = MenuItem1Click
      end
      object SaveEditableMission1: TMenuItem
        Caption = 'Save editable mission...'
        Enabled = False
        OnClick = SaveEditableMission1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object LoadSavThenRpl: TMenuItem
        Caption = 'Load .sav then .rpl'
        OnClick = LoadSavThenRplClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object ReloadSettings: TMenuItem
        Caption = 'Reload settings.ini'
        OnClick = ReloadSettingsClick
      end
      object SaveSettings: TMenuItem
        Caption = 'Save settings.ini'
        OnClick = SaveSettingsClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object ReloadLibx: TMenuItem
        Caption = 'Reload Libx (translations)'
        OnClick = ReloadLibxClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = ExitClick
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object Debug_PrintScreen: TMenuItem
        Caption = 'PrintScreen'
        OnClick = Debug_PrintScreenClick
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Debug_EnableCheats: TMenuItem
        Caption = 'Debug Cheats'
        OnClick = Debug_EnableCheatsClick
      end
      object Debug_UnlockCmpMissions: TMenuItem
        Caption = 'Unlock campaigns missions'
        OnClick = Debug_UnlockCmpMissionsClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Debug_ShowPanel: TMenuItem
        Caption = 'Show Debug panel'
        OnClick = Debug_ShowPanelClick
      end
      object Debug_ShowLogistics: TMenuItem
        Caption = 'Show Logistics'
        OnClick = Debug_ShowLogisticsClick
      end
    end
    object Export1: TMenuItem
      Caption = 'Export Data'
      object Resources1: TMenuItem
        Caption = 'Resources'
        object Export_Fonts1: TMenuItem
          Caption = 'Fonts'
          OnClick = Export_Fonts1Click
        end
        object Export_Sounds1: TMenuItem
          Caption = 'Sounds'
          OnClick = Export_Sounds1Click
        end
        object Other1: TMenuItem
          Caption = '-'
          Enabled = False
        end
        object Export_TreesRX: TMenuItem
          Caption = 'Trees.rx'
          OnClick = Export_TreesRXClick
        end
        object Export_HousesRX: TMenuItem
          Caption = 'Houses.rx'
          OnClick = Export_HousesRXClick
        end
        object Export_UnitsRX: TMenuItem
          Caption = 'Units.rx'
          OnClick = Export_UnitsRXClick
        end
        object Export_GUIRX: TMenuItem
          Caption = 'GUI.rx'
          OnClick = Export_GUIClick
        end
        object Export_GUIMainRX: TMenuItem
          Caption = 'GUI Main.rx'
          OnClick = Export_GUIMainRXClick
        end
        object Export_Custom: TMenuItem
          Caption = 'Custom'
          OnClick = Export_CustomClick
        end
        object Export_Tileset: TMenuItem
          Caption = 'Tileset'
          OnClick = Export_TilesetClick
        end
      end
      object AnimData1: TMenuItem
        Caption = '-'
        Enabled = False
      end
      object Export_TreeAnim1: TMenuItem
        Caption = 'Tree Anim'
        OnClick = Export_TreeAnim1Click
      end
      object Export_HouseAnim1: TMenuItem
        Caption = 'House Anim'
        OnClick = Export_HouseAnim1Click
      end
      object Export_UnitAnim1: TMenuItem
        Caption = 'Unit Anim'
        object UnitAnim_All: TMenuItem
          Caption = 'All'
          OnClick = UnitAnim_AllClick
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object Soldiers: TMenuItem
          Caption = 'Soldiers'
          OnClick = SoldiersClick
        end
        object Civilians1: TMenuItem
          Caption = 'Civilians'
          OnClick = Civilians1Click
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ResourceValues1: TMenuItem
        Caption = 'Resource Values'
        OnClick = ResourceValues1Click
      end
      object Export_Deliverlists1: TMenuItem
        Caption = 'Deliver lists'
        OnClick = Export_Deliverlists1Click
      end
      object HousesDat1: TMenuItem
        Caption = 'Houses Dat'
        OnClick = HousesDat1Click
      end
      object ScriptData1: TMenuItem
        Caption = 'Script Data'
        OnClick = Export_ScriptDataClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object GameStats: TMenuItem
        Caption = 'Game statistics'
        object ExportGameStats: TMenuItem
          Caption = 'Export'
          Enabled = False
          OnClick = ExportGameStatsClick
        end
        object ValidateGameStats: TMenuItem
          Caption = 'Validate'
          OnClick = ValidateGameStatsClick
        end
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object ExportMainMenu: TMenuItem
        Caption = 'Export MainMenu'
        OnClick = Debug_ExportMenuClick
      end
      object ExportUIPages: TMenuItem
        Caption = 'Export UI pages'
        OnClick = Debug_ExportUIPagesClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object mnExportRngChecks: TMenuItem
        Caption = 'Random checks'
        OnClick = mnExportRngChecksClick
      end
    end
    object About1: TMenuItem
      Caption = 'About..'
      OnClick = AboutClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 24
    Top = 120
  end
end
