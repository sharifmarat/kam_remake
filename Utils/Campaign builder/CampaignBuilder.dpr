program CampaignBuilder;
{$I ..\..\KaM_Remake.inc}

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  KM_Campaigns in '..\..\src\KM_Campaigns.pas',
  KM_ResSprites in '..\..\src\res\KM_ResSprites.pas',
  KM_ResSpritesEdit in '..\..\src\res\KM_ResSpritesEdit.pas',
  RenderPanel in 'RenderPanel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
