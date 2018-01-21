unit KM_AIFields;
{$I KaM_Remake.inc}
interface
uses
  KM_NavMesh, KM_AIInfluences, KM_Eye,
  KM_CommonClasses, KM_Points;


type
  //Master class for Influence maps, NavMeshe and other terrain representations
  //that are helpful in decision making by Mayour/General
  TKMAIFields = class
  private
    fNavMesh: TKMNavMesh;
    fInfluences: TKMInfluences;
    fEye: TKMEye;
  public
    constructor Create();
    destructor Destroy(); override;

    property NavMesh: TKMNavMesh read fNavMesh;
    property Influences: TKMInfluences read fInfluences;
    property Eye: TKMEye read fEye write fEye;

    procedure AfterMissionInit();

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


var
  gAIFields: TKMAIFields;


implementation
uses
  SysUtils,
  KM_Defaults;


{ TKMAIFields }
constructor TKMAIFields.Create();
begin
  inherited;

  fNavMesh := TKMNavMesh.Create();
  fInfluences := TKMInfluences.Create(fNavMesh);
  fEye := TKMEye.Create();
end;


destructor TKMAIFields.Destroy();
begin
  FreeAndNil(fNavMesh);
  FreeAndNil(fInfluences);
  FreeAndNil(fEye);
  inherited;
end;


procedure TKMAIFields.AfterMissionInit();
begin
  if not AI_GEN_NAVMESH then
    Exit;

  fNavMesh.AfterMissionInit();
  fInfluences.AfterMissionInit();
  fEye.AfterMissionInit();
end;


procedure TKMAIFields.Save(SaveStream: TKMemoryStream);
begin
  fNavMesh.Save(SaveStream);
  fInfluences.Save(SaveStream);
  fEye.Save(SaveStream);
end;


procedure TKMAIFields.Load(LoadStream: TKMemoryStream);
begin
  fNavMesh.Load(LoadStream);
  fInfluences.Load(LoadStream);
  fEye.Load(LoadStream);
end;


procedure TKMAIFields.UpdateState(aTick: Cardinal);
begin
  fNavMesh.UpdateState(aTick);
  fInfluences.UpdateState(aTick);
  fEye.UpdateState(aTick);
end;


//Render debug symbols
procedure TKMAIFields.Paint(aRect: TKMRect);
begin
  fEye.Paint(aRect);  // Debug (remove)

  if AI_GEN_INFLUENCE_MAPS then
    fInfluences.Paint(aRect);

  if AI_GEN_NAVMESH then
    fNavMesh.Paint(aRect);
end;


end.
