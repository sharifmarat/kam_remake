unit KM_GameSavedReplays;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses;

type
  TKMSavedReplay = class
  private
    fStream: TKMemoryStream;
    fName: String; // Name of the save on the pop up menu
    // Tick / Description
  public
    constructor Create(aStream: TKMemoryStream; aName: String);
    destructor Destroy(); override;

    property Stream: TKMemoryStream read fStream;
    property Name: String read fName;
  end;

  TKMSavedReplays = class
  private
    fReplaySaves: TKMList;

    function GetCount(): Integer;
    function GetSave(aIdx: Integer): TKMSavedReplay;
    function GetStream(aIdx: Integer): TKMemoryStream;
  public
    constructor Create();
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Replay[aIdx: Integer]: TKMSavedReplay read GetSave;
    property Stream[aIdx: Integer]: TKMemoryStream read GetStream; default;

    procedure NewSave(aStream: TKMemoryStream; aName: String);
  end;

implementation
uses
  SysUtils;


{ TKMSavedReplays }
constructor TKMSavedReplays.Create();
begin
  fReplaySaves := TKMList.Create();
end;


destructor TKMSavedReplays.Destroy();
begin
  fReplaySaves.Free; // TKMList will free all objects of the list
  inherited;
end;


function TKMSavedReplays.GetCount(): Integer;
begin
  Result := fReplaySaves.Count;
end;


function TKMSavedReplays.GetSave(aIdx: Integer): TKMSavedReplay;
begin
  Result := nil;
  if (fReplaySaves.Count > aIdx) AND (aIdx >= 0) then
    Result := TKMSavedReplay( fReplaySaves[aIdx] );
end;


function TKMSavedReplays.GetStream(aIdx: Integer): TKMemoryStream;
var
  Rpl: TKMSavedReplay;
begin
  Result := nil;
  Rpl := Replay[aIdx];
  if (Rpl <> nil) then
    Result := Rpl.Stream;
end;


procedure TKMSavedReplays.NewSave(aStream: TKMemoryStream; aName: String);
begin
  fReplaySaves.Add( TKMSavedReplay.Create(aStream, aName) );
end;



{ TKMSavedReplay }
constructor TKMSavedReplay.Create(aStream: TKMemoryStream; aName: String);
begin
  fStream := aStream;
  fName := aName;
end;


destructor TKMSavedReplay.Destroy();
begin
  fStream.Free;
  inherited;
end;


end.
