unit PictureMaps;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus, Math,
  System.Actions, Vcl.ActnList, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
  TfPictureMaps = class(TForm)
    List: TListView;
    bOk: TButton;
    bAdd: TButton;
    bReplace: TButton;
    bDelete: TButton;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    Preview: TImage;
    dlgOpenPicture: TOpenDialog;
    aAdd: TAction;
    aReplace: TAction;
    aDelete: TAction;
    Add1: TMenuItem;
    Replace1: TMenuItem;
    N1: TMenuItem;
    Delete1: TMenuItem;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure aAddExecute(Sender: TObject);
    procedure aReplaceExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure ListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    procedure UpdateItem(AItem: TListItem);
    procedure UpdatePreview;
  public

  end;

implementation

{$R *.dfm}

uses Main;

procedure TfPictureMaps.FormCreate(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  List.Items.BeginUpdate;
  try
    for i := 0 to MainForm.Sprites.RXData.Count - 1 do
      UpdateItem(List.Items.Add);
  finally
    List.Items.EndUpdate;
  end;
end;

procedure TfPictureMaps.UpdatePreview;
var
  Bitmap: TBitmap;
begin
  Preview.Picture.Bitmap.Canvas.Brush.Color := clBlack;//clBtnFace;
  Preview.Picture.Bitmap.Canvas.FillRect(Preview.Picture.Bitmap.Canvas.ClipRect);
  if not Assigned(List.Selected) then
    Exit;

  Bitmap := TBitmap.Create;
  try
    MainForm.Sprites.GetImageToBitmap(List.Selected.Index + 1, Bitmap, nil);
    Preview.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TfPictureMaps.ListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  UpdatePreview;
end;

procedure TfPictureMaps.UpdateItem(AItem: TListItem);
begin
  if MainForm.Sprites.RXData.Flag[AItem.Index + 1] = 0 then
  begin
    AItem.Caption := IntToStr(AItem.Index + 1) + '.';
    AItem.ImageIndex := 1;
  end
  else
  begin
    AItem.Caption := Format('%d. %dx%d', [AItem.Index + 1, MainForm.Sprites.RXData.Size[AItem.Index + 1].X, MainForm.Sprites.RXData.Size[AItem.Index + 1].Y]);
    AItem.ImageIndex := 0;
  end;
end;

procedure TfPictureMaps.aAddExecute(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
begin
  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenPicture.FileName);
  dlgOpenPicture.Options := dlgOpenPicture.Options + [ofAllowMultiSelect];
  if not dlgOpenPicture.Execute then
    Exit;

  try
    for i := 0 to dlgOpenPicture.Files.Count - 1 do
    begin
      Item := List.Items.Add;
      MainForm.Sprites.AddImage(ExtractFilePath(dlgOpenPicture.Files[i]),
        ExtractFileName(dlgOpenPicture.Files[i]), Item.Index + 1);
      UpdateItem(Item);
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TfPictureMaps.aDeleteExecute(Sender: TObject);
begin
  if Assigned(List.Selected) then
  begin
    MainForm.Sprites.Delete(List.Selected.Index + 1);
    UpdateItem(List.Selected);
  end;
end;

procedure TfPictureMaps.aReplaceExecute(Sender: TObject);
begin
  if not Assigned(List.Selected) then
    Exit;

  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenPicture.FileName);
  dlgOpenPicture.Options := dlgOpenPicture.Options - [ofAllowMultiSelect];

  if not dlgOpenPicture.Execute then Exit;
  try
    MainForm.Sprites.AddImage(ExtractFilePath(dlgOpenPicture.FileName),
      ExtractFileName(dlgOpenPicture.FileName), List.Selected.Index + 1);
    UpdateItem(List.Selected);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.
