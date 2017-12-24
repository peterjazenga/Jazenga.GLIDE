
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLCDScreenEditor;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Messages, ExtCtrls, Buttons,
  LCLtype, lresources, PropEdits, ComponentEditors,
  TplLCDScreenUnit;

type
  TLCDScreenEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: integer); override;
    function GetVerb(Index: integer): string; //override;
    function GetVerbCount: integer; override;
  end;


  TLCDScreenLinesEditorProperty = class(TPropertyEditor)
    procedure Edit; override;
    function GetValue: string; //override;
    function GetAttributes: TPropertyAttributes; override;
  end;


  TplLCDScreenLinesEditorForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    CancelButton: TButton;
    OkButton: TButton;
    LinesMemo: TMemo;
    BrutLabel: TLabel;
    PreviewLabel: TLabel;
    SpBPanel: TPanel;
    BoldSpB: TSpeedButton;
    ItalicSpB: TSpeedButton;
    StrikeSpB: TSpeedButton;
    UnderlineSpB: TSpeedButton;
    InverseSpB: TSpeedButton;
    BlinkSpB: TSpeedButton;
    PreviewSB: TScrollBox;
    PreviewLCD: TplLCDScreen;

    procedure FormShow(Sender: TObject);
    procedure LinesMemoChange(Sender: TObject);
    procedure LinesMemoKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure LinesMemoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure LinesMemoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure SetLCDTag(LCDTag: string);
    procedure BoldSpBClick(Sender: TObject);
    procedure ItalicSpBClick(Sender: TObject);
    procedure StrikeSpBClick(Sender: TObject);
    procedure UnderlineSpBClick(Sender: TObject);
    procedure InverseSpBClick(Sender: TObject);
    procedure BlinkSpBClick(Sender: TObject);
    procedure DeleteLCDTagSpBClick(Sender: TObject);
  end;


var
  LCDScreenLinesEditorForm: TplLCDScreenLinesEditorForm;

implementation

//================= TLCDScreenEditor =========================================
function TLCDScreenEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

function TLCDScreenEditor.GetVerb(Index: integer): string;
begin
  Result := 'LCDScreen Lines Editor';
end;

procedure TLCDScreenEditor.ExecuteVerb(Index: integer);
var
  F: TplLCDScreenLinesEditorForm;
begin
  F := TplLCDScreenLinesEditorForm.Create(Application);
  try
    F.Caption := 'TplLCDScreen Lines Editor - ' + (Component as TplLCDScreen).Name;

    F.PreviewLCD.Font.Color := (Component as TplLCDScreen).Font.Color;
    F.PreviewLCD.PixelOff := (Component as TplLCDScreen).PixelOff;
    F.PreviewLCD.Color := (Component as TplLCDScreen).Color;
    F.PreviewLCD.Font := (Component as TplLCDScreen).Font;

    F.LinesMemo.Lines.Assign((Component as TplLCDScreen).Lines);

    if F.ShowModal = mrOk then
      (Component as TplLCDScreen).Lines.Assign(F.LinesMemo.Lines);

    try
      Designer.Modified;
    except
    end;

  finally
    F.Free;
  end;
end;

//===================== TLCDScreenLinesEditorProperty =============================

function TLCDScreenLinesEditorProperty.GetValue: string;
begin
  Result := '(TStringList)';
end;


function TLCDScreenLinesEditorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;


procedure TLCDScreenLinesEditorProperty.Edit;
var
  F: TplLCDScreenLinesEditorForm;
  C: TPersistent;
begin
  C := GetComponent(0);
  F := TplLCDScreenLinesEditorForm.Create(Application);

  try
    F.Caption := 'TplLCDScreen Lines Editor - ' + (C as TplLCDScreen).Name;

    F.PreviewLCD.Font.Color := (C as TplLCDScreen).Font.Color;
    F.PreviewLCD.PixelOff := (C as TplLCDScreen).PixelOff;
    F.PreviewLCD.Color := (C as TplLCDScreen).Color;
    F.PreviewLCD.Font := (C as TplLCDScreen).Font;

    F.LinesMemo.Lines.Assign((C as TplLCDScreen).Lines);

    if F.ShowModal = mrOk then
      (C as TplLCDScreen).Lines.Assign(F.LinesMemo.Lines);

    // try Designer.Modified; except end;

  finally
    F.Free;
  end;
end;

//====================== TplLCDScreenLinesEditorForm =============================

procedure TplLCDScreenLinesEditorForm.LinesMemoChange(Sender: TObject);
var
  currentline: byte;
begin
  // currentline := LinesMemo.Perform(EM_LINEFROMCHAR, -1, 0);
  PreviewLCD.Lines.BeginUpdate;
  PreviewLCD.Lines.Clear;
  PreviewLCD.Lines.Append(LinesMemo.Lines[currentline]);
  PreviewLCD.Lines.EndUpdate;
end;

procedure TplLCDScreenLinesEditorForm.LinesMemoKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  LinesMemoMouseMove(Self, Shift, 0, 0);
end;

procedure TplLCDScreenLinesEditorForm.LinesMemoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if LinesMemo.SelLength <> 0 then
  begin
    BoldSpB.Enabled := True;
    ItalicSpB.Enabled := True;
    StrikeSpB.Enabled := True;
    UnderlineSpB.Enabled := True;
    InverseSpB.Enabled := True;
    BlinkSpB.Enabled := True;
  end
  else
  begin
    BoldSpB.Enabled := False;
    ItalicSpB.Enabled := False;
    StrikeSpB.Enabled := False;
    UnderlineSpB.Enabled := False;
    InverseSpB.Enabled := False;
    BlinkSpB.Enabled := False;
  end;
end;

procedure TplLCDScreenLinesEditorForm.LinesMemoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  LinesMemoChange(Sender);
end;

procedure TplLCDScreenLinesEditorForm.SetLCDTag(LCDTag: string);
var
  str: Tcaption;
  oldsel: integer;
begin
  with LinesMemo do
  begin
    //currentline := Perform(EM_LINEFROMCHAR, -1, 0);
    oldSel := SelStart;
    str := Text;
    Insert('<' + LCDTag + '>', str, SelStart + 1);
    Insert('</' + LCDTag + '>', str, SelStart + SelLength + Length(LCDTag) + 3);
    Text := str;
  end;
  LinesMemo.SelStart := oldsel + 2 + Length(LCDTag);
  LinesMemoChange(nil);
end;


procedure TplLCDScreenLinesEditorForm.BoldSpBClick(Sender: TObject);
begin
  SetLCDTag('b');
end;

procedure TplLCDScreenLinesEditorForm.ItalicSpBClick(Sender: TObject);
begin
  SetLCDTag('i');
end;

procedure TplLCDScreenLinesEditorForm.StrikeSpBClick(Sender: TObject);
begin
  SetLCDTag('s');
end;

procedure TplLCDScreenLinesEditorForm.UnderlineSpBClick(Sender: TObject);
begin
  SetLCDTag('u');
end;

procedure TplLCDScreenLinesEditorForm.InverseSpBClick(Sender: TObject);
begin
  SetLCDTag('inv');
end;

procedure TplLCDScreenLinesEditorForm.BlinkSpBClick(Sender: TObject);
begin
  SetLCDTag('bl');
end;

procedure TplLCDScreenLinesEditorForm.DeleteLCDTagSpBClick(Sender: TObject);
var
  currentline: byte;
begin
  if LinesMemo.SelLength = 0 then
  begin
    //   currentline := LinesMemo.Perform(EM_LINEFROMCHAR, -1, 0);
    if CountLCDTag(LinesMemo.Lines[currentline]) <> 0 then
      LinesMemo.Lines[currentline] := RemoveLCDTag(LinesMemo.Lines[currentline])
    else
      beep;
  end
  else if CountLCDTag(LinesMemo.SelText) <> 0 then
    LinesMemo.SelText := RemoveLCDTag(LinesMemo.SelText)
  else
    beep;
end;

procedure TplLCDScreenLinesEditorForm.FormShow(Sender: TObject);
begin
  LinesMemo.SelStart := 0;
  LinesMemoChange(Sender);
end;

initialization
  {$I TplLCDScreenEditor.lrs}

end.
