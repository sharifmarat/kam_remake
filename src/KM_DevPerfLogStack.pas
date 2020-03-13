unit KM_DevPerfLogStack;
{$I KM_CompilerDirectives.inc}
interface
uses
  Classes, Math, StrUtils, SysUtils, KromUtils;


type
  TKMPerfLogStack = class
  protected
    fSectionNames: TStringList; // String contains Key, Object contains integer Count
    fCount: Integer;
    fTimes: array of array of Int64; // in usec
    fCaptions: array of record
      AvgBase, Middle: Single;
    end;

    fPrevSection: Integer;
    fThisSection: Integer;
  public
    Enabled: Boolean;
    Display: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure GetSectionsStats(aList: TStringList);
    procedure Render(aLeft, aWidth, aHeight, aScaleY: Integer; aEmaAlpha: Single; aFrameBudget: Integer; aSmoothing: Boolean);

    // Should have no virtual/abstract methods for the "if Self = nil then Exit" to work
    // Otherwise check does not work and causes AV
  end;

  // GPU logging
  // Async through gRenderLow.Queries
  TKMPerfLogStackGFX = class(TKMPerfLogStack)
  private
    fGPUQueryList: array of Integer;
  protected
    procedure SectionEnterI(aSection: Integer; aCount: Boolean = True);
    procedure SectionLeave;
  public
    procedure FrameBegin;
    procedure SectionEnter(aName: string; aCount: Boolean = True);
    procedure SectionRollback;
    procedure FrameEnd;
  end;

  TKMPerfLogStackCPU = class(TKMPerfLogStack)
  private
    fEnterTime: Int64;
    fInTick: Boolean;
  protected
    procedure SectionEnterI(aSection: Integer; aCount: Boolean = True);
    procedure SectionLeave;
  public
    HighPrecision: Boolean;
    constructor Create;
    procedure TickBegin;
    procedure SectionEnter(aName: string; aCount: Boolean = True);
    procedure SectionRollback;
    procedure TickEnd;
  end;


implementation
uses
  KM_RenderLow, KM_RenderTypes, KM_RenderUI, KM_ResTypes,
  KM_Vertexes, KM_Utils;


{ TKMPerfLogStack }
constructor TKMPerfLogStack.Create;
begin
  inherited;

  fSectionNames := TStringList.Create;
end;


destructor TKMPerfLogStack.Destroy;
begin
  fSectionNames.Free;
  inherited;
end;


procedure TKMPerfLogStack.Clear;
begin
  fCount := 0;
  fSectionNames.Clear;
end;


// Get total stats in msec
procedure TKMPerfLogStack.GetSectionsStats(aList: TStringList);
var
  I, K: Integer;
  secTime, totalTime: Single;
begin
  aList.Clear;

  totalTime := 0;

  for I := 0 to fSectionNames.Count - 1 do
  begin
    // Sum section length
    secTime := 0;
    for K := 0 to fCount - 1 do
      secTime := secTime + fTimes[K,I] / 1000;

    // Times are cumulative. Convert them back into separate
    secTime := secTime - totalTime;
    totalTime := totalTime + secTime;
    aList.AddObject(fSectionNames[I], TObject(secTime));
  end;

  aList.AddObject('[Total]', TObject(totalTime));
end;


procedure TKMPerfLogStack.Render(aLeft, aWidth, aHeight, aScaleY: Integer; aEmaAlpha: Single; aFrameBudget: Integer; aSmoothing: Boolean);
const
  HALF_CAPTION_HEIGHT = 10;
var
  I, K, L: Integer;
  cCount: Integer;
  vaFill: TKMVertex2Array;
  vaLine: TKMVertex2Array;
  ty: Integer;
  accum1, accum2, accum3: Single;
begin
  if not Display then Exit;
  if fCount <= 1 then Exit;

  cCount := Min(fCount - 1, aWidth);
  SetLength(vaFill, cCount * 2);
  SetLength(vaLine, cCount);

  for I := 0 to fSectionNames.Count - 1 do
  begin
    accum1 := aHeight;
    accum2 := aHeight;
    accum3 := aHeight;

    // Do not render newest time, it has not been complete nor stacked yet
    for K := cCount - 1 downto 0 do
    begin
      // Skip current time, it's not finalized yet
      L := (fCount - 2) - K;

      // Fill is made with hundreds of 1px lines, so we get pixel-perfect fill between 2 charts
      if I = 0 then
      begin
        vaFill[K*2]   := TKMVertex2.New(aLeft + K + 0.5, aHeight + 0.5);
        vaFill[K*2+1] := TKMVertex2.New(aLeft + K + 0.5, aHeight + 0.5 - fTimes[L,I] / 1000 / aFrameBudget * aScaleY);
      end else
      begin
        vaFill[K*2]   := TKMVertex2.New(aLeft + K + 0.5, aHeight + 0.5 - fTimes[L,I-1] / 1000 / aFrameBudget * aScaleY);
        vaFill[K*2+1] := TKMVertex2.New(aLeft + K + 0.5, aHeight + 0.5 - fTimes[L,I] / 1000 / aFrameBudget * aScaleY);
      end;

      vaLine[K] := TKMVertex2.New(aLeft + K + 0.5, aHeight + 0.5 - fTimes[L,I] / 1000 / aFrameBudget * aScaleY);

      if aSmoothing then
      begin
        // Exponential Moving Average
        accum1 := aEmaAlpha * vaLine[K].Y + (1 - aEmaAlpha) * accum1;
        vaLine[K].Y := accum1;

        accum2 := aEmaAlpha * vaFill[K*2].Y + (1 - aEmaAlpha) * accum2;
        vaFill[K*2].Y := accum2;

        accum3 := aEmaAlpha * vaFill[K*2+1].Y + (1 - aEmaAlpha) * accum3;
        vaFill[K*2+1].Y := accum3;
      end;
    end;

    // Fill
    gRenderLow.Draw.RenderLine2(vaFill, TKMColor4f.New(TKMColor3f.Generic(I), 0.4), 1, lmPairs);

    // Border
    gRenderLow.Draw.RenderLine2(vaLine, TKMColor4f.White.Alpha50, 1, lmStrip);

    // Sections captions
    if (fCaptions[I].AvgBase - fCaptions[I].Middle) / 1000 / aFrameBudget * aScaleY > HALF_CAPTION_HEIGHT then
    begin
      ty := Round(fCaptions[I].Middle / 1000 / aFrameBudget * aScaleY);
      TKMRenderUI.WriteText(aLeft + 4, Trunc(aHeight + 0.5 - ty - 7), 0, 1,
        fSectionNames[I] + ' x' + IntToStr(Integer(fSectionNames.Objects[I])), fntMini, taLeft);
    end;
  end;
end;


{ TKMPerfLogStackCPU }
constructor TKMPerfLogStackCPU.Create;
begin
  inherited;

  HighPrecision := True;
end;


procedure TKMPerfLogStackCPU.TickBegin;
var
  I: Integer;
begin
  if not Enabled then Exit;

  fPrevSection := -1;
  fThisSection := -1;

  Inc(fCount);

  if fCount >= Length(fTimes) then
    SetLength(fTimes, Length(fTimes) + 1024, fSectionNames.Count);

  for I := 0 to fSectionNames.Count - 1 do
   fSectionNames.Objects[I] := TObject(0);

  SectionEnter('TickBegin');
  fInTick := True;
end;


procedure TKMPerfLogStackCPU.SectionEnter(aName: string; aCount: Boolean = True);
var
  I: Integer;
begin
  if (Self = nil) or not Enabled or not fInTick then Exit;

  Assert(aName <> '');

  I := fSectionNames.IndexOf(aName);
  if I = -1 then
  begin
    I := fSectionNames.Add(aName);
    Assert(I = fSectionNames.Count - 1);

    SetLength(fTimes, Length(fTimes), fSectionNames.Count);
    SetLength(fCaptions, fSectionNames.Count);
  end;

  SectionEnterI(I, aCount);
end;


procedure TKMPerfLogStackCPU.SectionRollback;
begin
  if (Self = nil) or not Enabled or not fInTick then Exit;

  SectionEnterI(fPrevSection, False);
end;


procedure TKMPerfLogStackCPU.SectionEnterI(aSection: Integer; aCount: Boolean = True);
begin
  if fThisSection <> -1 then
    SectionLeave;

  fPrevSection := fThisSection;
  fThisSection := aSection;

  if aCount then
    fSectionNames.Objects[fThisSection] := TObject(Integer(fSectionNames.Objects[fThisSection]) + 1);

  if HighPrecision then
    fEnterTime := TimeGetUsec
  else
    fEnterTime := TimeGet;
end;


procedure TKMPerfLogStackCPU.SectionLeave;
var
  T: UInt64;
begin
  if not Enabled or not fInTick then Exit;

  // Get us time from previous frame
  if HighPrecision then
    T := TimeGetUsecSince(fEnterTime)
  else
    T := TimeGetSince(fEnterTime) * 1000;

  // Sum times, since same section could be entered more than once
  fTimes[fCount - 1, fThisSection] := fTimes[fCount - 1, fThisSection] + T;
end;


procedure TKMPerfLogStackCPU.TickEnd;
const
  LERP_AVG = 0.025;
var
  I: Integer;
begin
  if not Enabled then Exit;

  SectionLeave;

  fThisSection := -1;

  // Stack times to render them simpler
  for I := 1 to fSectionNames.Count - 1 do
    fTimes[fCount - 1, I] := fTimes[fCount - 1, I - 1] + fTimes[fCount - 1, I];

  // Calculate averages for
  if fCount > 0 then
  for I := 0 to fSectionNames.Count - 1 do
    fCaptions[I].AvgBase := Lerp(fCaptions[I].AvgBase, fTimes[fCount - 1, I], LERP_AVG);

  for I := 0 to fSectionNames.Count - 1 do
  if I = 0 then
    fCaptions[I].Middle := fCaptions[I].AvgBase / 2
  else
    fCaptions[I].Middle := (fCaptions[I-1].AvgBase + fCaptions[I].AvgBase) / 2;

  fInTick := False;
end;


{ TKMPerfLogStackGFX }
procedure TKMPerfLogStackGFX.FrameBegin;
var
  I: Integer;
begin
  if not Enabled then Exit;

  fThisSection := -1;

  Inc(fCount);

  if fCount >= Length(fTimes) then
    SetLength(fTimes, Length(fTimes) + 1024, fSectionNames.Count);

  for I := 0 to fSectionNames.Count - 1 do
   fSectionNames.Objects[I] := TObject(0);

  SectionEnter('FrameBegin');
end;


procedure TKMPerfLogStackGFX.SectionEnter(aName: string; aCount: Boolean = True);
var
  I: Integer;
begin
  if (Self = nil) or not Enabled then Exit;

  Assert(aName <> '');

  I := fSectionNames.IndexOf(aName);
  if I = -1 then
  begin
    I := fSectionNames.Add(aName);
    SetLength(fGPUQueryList, fSectionNames.Count);
    fGPUQueryList[I] := gRenderLow.Query.QueriesGen;

    SetLength(fTimes, Length(fTimes), fSectionNames.Count);
    SetLength(fCaptions, fSectionNames.Count);
  end;

  SectionEnterI(I, aCount);
end;


procedure TKMPerfLogStackGFX.SectionRollback;
begin
  if (Self = nil) or not Enabled then Exit;

  SectionEnterI(fPrevSection, False);
end;


procedure TKMPerfLogStackGFX.SectionEnterI(aSection: Integer; aCount: Boolean = True);
begin
  if (Self = nil) or not Enabled then Exit;

  if fThisSection <> -1 then
    SectionLeave;

  fPrevSection := fThisSection;
  fThisSection := aSection;

  if aCount then
    fSectionNames.Objects[fThisSection] := TObject(Integer(fSectionNames.Objects[fThisSection]) + 1);

  gRenderLow.Query.QueriesBegin(fGPUQueryList[fThisSection]);
end;


procedure TKMPerfLogStackGFX.SectionLeave;
var
  T: UInt64;
begin
  if not Enabled then Exit;

  gRenderLow.Query.QueriesEnd(fGPUQueryList[fThisSection]);

  // Get us time from previous frame
  T := gRenderLow.Query.QueriesTime(fGPUQueryList[fThisSection]);
  T := Round(T / 1000);

  // Sum times for same section could be entered more than once
  fTimes[fCount - 1, fThisSection] := fTimes[fCount - 1, fThisSection] + T;
end;


procedure TKMPerfLogStackGFX.FrameEnd;
const
  LERP_AVG = 0.025;
var
  I: Integer;
begin
  if not Enabled then Exit;

  SectionLeave;

  fThisSection := -1;

  // Stack times to render them simpler
  for I := 1 to fSectionNames.Count - 1 do
    fTimes[fCount - 1, I] := fTimes[fCount - 1, I - 1] + fTimes[fCount - 1, I];

  // Calculate averages for
  if fCount > 0 then
  for I := 0 to fSectionNames.Count - 1 do
    fCaptions[I].AvgBase := Lerp(fCaptions[I].AvgBase, fTimes[fCount - 1, I], LERP_AVG);

  for I := 0 to fSectionNames.Count - 1 do
  if I = 0 then
    fCaptions[I].Middle := fCaptions[I].AvgBase / 2
  else
    fCaptions[I].Middle := (fCaptions[I-1].AvgBase + fCaptions[I].AvgBase) / 2;
end;


end.
