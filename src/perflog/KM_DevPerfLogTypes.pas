unit KM_DevPerfLogTypes;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonTypes;

type
  TKMPerfLogKind = (plkCPU, plkGFX);

  TPerfSectionDev = (
    psNone,
    psGameTick,
      psGameSave,
      psHands,
        psUnits,
        psDelivery,
        psWalkConnect,
      psGameFOW,
      psPathfinding,
      psHungarian,
      psAIFields,
      psAI,
        psAICityAdv,
        psAIArmyAdv,
        psAICityCls,
        psAIArmyCls,
      psTerrain,
      psTerrainFinder,
      psScripting,
      psMinimap,
    psFrameFullC,                 // Full render frame as seen by gMain
    psFrameFullG,                 // Full render frame on GPU (doublecheck TKMPerfLogGFXStack)
        psFrameTerrain,
          psFrameTerrainBase,
            psFrameTiles,
            psFrameWater,
            psFrameTilesLayers,
            psFrameOverlays,
            psFrameLighting,
            psFrameShadows,
        psFrameRenderList,
        psFrameFOW,
      psFrameUpdateVBO,
      psFrameGui                  // Frame of the Gameplay GUI
  );
  TPerfSectionSet = set of TPerfSectionDev;

const
  // Tabs are for GUI structure
  // Can not use typed constants within another constants declaration :(
  // http://stackoverflow.com/questions/28699518/
  // Avoid Green, it blends with mostly green terrain
  SECTION_INFO: array [TPerfSectionDev] of record
    Name: string;
//    ClassName: TKMPerfLogClass;
    Kind: TKMPerfLogKind;
    Color: TKMColor3f;
  end = (
    (Name: 'All';                     Kind: plkCPU; Color: (R:0;G:0;B:0);),
    (Name: 'GameTick';                Kind: plkCPU; Color: (R:1.0;G:1;B:0);),
    (Name: '   GameSave';             Kind: plkCPU; Color: (R:0.3;G:1;B:0.7);),
    (Name: '   Hands';                Kind: plkCPU; Color: (R:1;G:0.25;B:0);),
    (Name: '     Units';              Kind: plkCPU; Color: (R:0;G:0.5;B:0.5);),
    (Name: '     Deliveries';         Kind: plkCPU; Color: (R:0.75;G:0.25;B:0.25);),
    (Name: '     WalkConnect';        Kind: plkCPU; Color: (R:1;G:0.75;B:0.75);),
    (Name: '   FOW';                  Kind: plkCPU; Color: (R:0;G:0.75;B:0);),
    (Name: '   Pathfinding';          Kind: plkCPU; Color: (R:0.0;G:1;B:0.75);),
    (Name: '   HungarianReorder';     Kind: plkCPU; Color: (R:1.0;G:0;B:1);),
    (Name: '   AIFields';             Kind: plkCPU; Color: (R:0;G:0.5;B:1);),
    (Name: '   AI';                   Kind: plkCPU; Color: (R:0;G:0.75;B:1);),
    (Name: '     AI City Advanced';   Kind: plkCPU; Color: (R:0;G:0.75;B:0.25);),
    (Name: '     AI Army Advanced';   Kind: plkCPU; Color: (R:0.25;G:0.75;B:1);),
    (Name: '     AI City Classic';    Kind: plkCPU; Color: (R:0.25;G:0.5;B:1);),
    (Name: '     AI Army Classic';    Kind: plkCPU; Color: (R:0.5;G:0.5;B:0.25);),
    (Name: '   Terrain';              Kind: plkCPU; Color: (R:0.5;G:0.5;B:0.5);),
    (Name: '   TerrainFinder';        Kind: plkCPU; Color: (R:0;G:1;B:1);),
    (Name: '   Scripting';            Kind: plkCPU; Color: (R:1;G:0.25;B:0.75);),
    (Name: '   Minimap';              Kind: plkCPU; Color: (R:0.7;G:0;B:0.9);),
    (Name: 'Render.CPU';              Kind: plkCPU; Color: (R:1.0;G:0;B:0);),
    (Name: 'Render.GFX';              Kind: plkGFX; Color: (R:0;G:1;B:1);),
    (Name: '     Terrain';            Kind: plkGFX; Color: (R:0;G:0.25;B:0.25);),
    (Name: '       TerBase';          Kind: plkGFX; Color: (R:0;G:0.5;B:0.25);),
    (Name: '         Tiles';          Kind: plkGFX; Color: (R:0.25;G:0;B:0.25);),
    (Name: '         Water';          Kind: plkGFX; Color: (R:0.25;G:0;B:0.5);),
    (Name: '         Layers';         Kind: plkGFX; Color: (R:0.5;G:0;B:0.25);),
    (Name: '         Overlays';       Kind: plkGFX; Color: (R:0.5;G:0.5;B:0.25);),
    (Name: '         Light';          Kind: plkGFX; Color: (R:0.5;G:0.25;B:0);),
    (Name: '         Shadows';        Kind: plkGFX; Color: (R:0.75;G:0.5;B:0);),
    (Name: '     RenderList';         Kind: plkGFX; Color: (R:0.5;G:1;B:0.75);),
    (Name: '     FOWRender';          Kind: plkGFX; Color: (R:0.75;G:1;B:0.75);),
    (Name: '   UpdateVBO';            Kind: plkCPU; Color: (R:0.5;G:0.5;B:1);),
    (Name: '   GUI';                  Kind: plkGFX; Color: (R:1.0;G:0.25;B:0);)
  );

  function GetSectionName(aSection: TPerfSectionDev): string;

const
  LOW_PERF_SECTION = TPerfSectionDev(1);

implementation
uses
  TypInfo;


function GetSectionName(aSection: TPerfSectionDev): string;
begin
  Result := GetEnumName(TypeInfo(TPerfSectionDev), Integer(aSection));
  Result := Copy(Result, 3, Length(Result) - 2);
end;


end.
