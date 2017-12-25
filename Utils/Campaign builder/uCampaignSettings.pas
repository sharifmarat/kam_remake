unit uCampaignSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Mask, Vcl.StdCtrls;

type
  TfCampaignSettings = class(TForm)
    edtName: TEdit;
    CampaignNameLabel: TLabel;
    ShortNameLabel: TLabel;
    IntoVideoLabel: TLabel;
    edtIntroVideo: TEdit;
    bOk: TButton;
    bCancel: TButton;
    CheckNodeAnimation: TCheckBox;
    edtShortName: TMaskEdit;
    procedure edtShortNameKeyPress(Sender: TObject; var Key: Char);
  private

  public

  end;

implementation

{$R *.dfm}

procedure TfCampaignSettings.edtShortNameKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['A'..'Z', 'a'..'z', #8 {Backspace}]) then
    Key := #0;
end;

end.
