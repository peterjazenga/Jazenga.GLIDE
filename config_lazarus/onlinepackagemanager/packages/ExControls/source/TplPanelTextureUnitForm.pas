unit TplPanelTextureUnitForm;

interface

uses
  LCLIntf, LCLType, LMessages,
  Classes, Graphics, Forms, Controls, TplPanelTextureUnit, Buttons,
  StdCtrls, ComCtrls,  Dialogs, SysUtils,
  Grids, CheckLst;

type
  TEZShadeDlg = class(TForm)
    CheckListBox1: TCheckListBox;
    procedure ButtonClose(Sender: TObject);
    procedure UpdateItems;
    procedure FormShow(Sender: TObject);
    procedure CheckListChange(Sender: TObject);
  public
    CurrItem     : integer;
    EditorShades : TEZShades;
    ShadeObjects : TShadeObjects;
  end;

var
  EZShadeDlg: TEZShadeDlg;

implementation


procedure TEZShadeDlg.ButtonClose(Sender: TObject);
begin
 Close;
end;

procedure TEZShadeDlg.UpdateItems;
var
  I,J : integer;
begin
  CheckListBox1.Items.Clear;
  for I := 0 to Length(ShadeObjects)-1 do begin
     J := CheckListBox1.Items.Add(ShadeObjects[I].Name);
     if (ShadeObjects[I].Act <> saNone) then CheckListBox1.Checked[J] := True
     else CheckListBox1.Checked[J] := False;
     if (ShadeObjects[I].Inherit = 'None') or (ShadeObjects[I].Visible = 0) then
     begin
       CheckListBox1.ItemEnabled[J] := False;
       ShadeObjects[I].Act          := saNone;
     end;
  end;
end;

procedure TEZShadeDlg.FormShow(Sender: TObject);
begin
 UpdateItems;
end;

procedure TEZShadeDlg.CheckListChange(Sender: TObject);
var
  I : Integer;
begin
  CurrItem := -1;
  for I := 0 to CheckListBox1.Items.Count-1 do if CheckListBox1.Selected[I] then CurrItem := I;

  if (CurrItem > -1) then begin
    if      ShadeObjects[CurrItem].Inherit = 'Window' then ShadeObjects[CurrItem].Act := saWindow
    else if ShadeObjects[CurrItem].Inherit = 'Shape'  then ShadeObjects[CurrItem].Act := saShape
    else if ShadeObjects[CurrItem].Inherit = 'Label'  then ShadeObjects[CurrItem].Act := saText
    else if ShadeObjects[CurrItem].Inherit = 'EZTex'  then ShadeObjects[CurrItem].Act := saShape
    else ShadeObjects[CurrItem].Act := saNone;
    if not CheckListBox1.Checked[CurrItem] then ShadeObjects[CurrItem].Act := saNone;
  end;

  UpdateItems;
end;

end.
