unit KM_NavMeshFloodPositioning;
{$I KaM_Remake.inc}
interface
uses
   Math, KM_Defaults, KM_CommonTypes,
   KM_Points, KM_NavMeshFloodFill;


type
  TNavMeshFloodPositioning = class(TNavMeshFloodFill)
  private
    fCount: Word;
  protected
    fPointArray: TKMPointArray;

    function CanBeVisited(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
  public
    function FindPositions(aCount: Word; aInitIdxArray: TKMWordArray; out aPointArray: TKMPointArray): Boolean;
  end;


implementation
uses
  KM_AIFields;


{ TNavMeshFloodPositioning }
function TNavMeshFloodPositioning.CanBeVisited(const aIdx: Word): Boolean;
begin
  Result := fCount < Length(fPointArray);
end;


procedure TNavMeshFloodPositioning.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
begin
  inherited MarkAsVisited(aIdx, aDistance, aPoint);
  if (aDistance > 0) AND CanBeVisited(aIdx) then // Distance > 0 -> ignore init points (center of polygon instead of edge, may cause traffic)
  begin
    fPointArray[fCount] := aPoint;
    fCount := fCount + 1;
  end;
end;


function TNavMeshFloodPositioning.FindPositions(aCount: Word; aInitIdxArray: TKMWordArray; out aPointArray: TKMPointArray): Boolean;
begin
  SetLength(fPointArray, aCount);
  fCount := 0;

  FillPolygons(High(aInitIdxArray), aInitIdxArray);

  aPointArray := fPointArray;
  Result := (fCount = aCount);
end;


end.

