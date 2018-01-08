unit uCampaignSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Mask, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfCampaignSettings = class(TForm)
    edtName: TEdit;
    CampaignNameLabel: TLabel;
    CampaignIDLabel: TLabel;
    bOk: TButton;
    bCancel: TButton;
    edtShortName: TMaskEdit;
    UpDownNodeAnimation: TUpDown;
    EditNodeAnimation: TEdit;
    Label1: TLabel;
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
