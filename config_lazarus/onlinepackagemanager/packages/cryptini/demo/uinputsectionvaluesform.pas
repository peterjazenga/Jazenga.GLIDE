unit uInputSectionValuesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

const
  C_NUMBEROFCONTROLS = 8; // Only need to change it here

type

  { TInputSectionValuesForm }

  TInputSectionValuesForm = class(TForm)
    cmd_Cancel: TBitBtn;
    cmd_Close: TBitBtn;
    edt_NewSectionName: TEdit;
    lbl_Invisible2: TLabel;
    Lbl_SectionName: TLabel;
    Grp_NewSectionValues: TGroupBox;
    lbl_Invisible1: TLabel;
    procedure edt_NewSectionNameEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure DisableSectionNameEdit;
    procedure InitControls;
    procedure ProcessCheckbox(Sender: TObject);
    procedure ProcessValueEdit(Sender: TObject);
  public
    ValueLabelArray: array[0..C_NUMBEROFCONTROLS - 1] of TLabel;
    IdentEditArray: array[0..C_NUMBEROFCONTROLS - 1] of TEdit;
    ValueEditArray: array[0..C_NUMBEROFCONTROLS - 1] of TEdit;
    ValueCheckBoxArray: array[0..C_NUMBEROFCONTROLS - 1] of TCheckBox;
    sSectionName: string;
    NumberOfControls: integer;
  end;

var
  InputSectionValuesForm: TInputSectionValuesForm;

implementation

{$R *.lfm}

{ TInputSectionValuesForm }
procedure TInputSectionValuesForm.InitControls;
var
  iCount: integer;
begin
  BeginFormUpdate;
  NumberOfControls := High(ValueLabelArray) + 1;
  for iCount := Low(ValueLabelArray) to High(ValueLabelArray) do
  begin
    ValueLabelArray[iCount] := TLabel.Create(Grp_NewSectionValues);
    ValueLabelArray[iCount].Caption := Format('Key and Value %d', [iCount + 1]);
    ValueLabelArray[iCount].Tag := iCount;
    ValueLabelArray[iCount].parent := Grp_NewSectionValues;

    IdentEditArray[iCount] := TEdit.Create(Grp_NewSectionValues);
    IdentEditArray[iCount].Text := 'Input Key here';
    IdentEditArray[iCount].Tag := iCount;
    //         IdentEditArray[iCount].OnEditingDone:=@ProcessEdit;
    IdentEditArray[iCount].parent := Grp_NewSectionValues;

    ValueEditArray[iCount] := TEdit.Create(Grp_NewSectionValues);
    ValueEditArray[iCount].Text := 'Input value here';
    ValueEditArray[iCount].Tag := iCount;
    ValueEditArray[iCount].OnEditingDone := @ProcessValueEdit;
    ValueEditArray[iCount].parent := Grp_NewSectionValues;

    ValueCheckBoxArray[iCount] := TCheckBox.Create(Grp_NewSectionValues);
    ValueCheckBoxArray[iCount].Caption := 'Integer?';
    ValueCheckBoxArray[iCount].Checked := False;
    ValueCheckBoxArray[iCount].Tag := iCount;
    ValueCheckBoxArray[iCount].TabStop:=FALSE;
    ValueCheckBoxArray[iCount].OnClick := @ProcessCheckbox;
    ValueCheckBoxArray[iCount].parent := Grp_NewSectionValues;
  end;
  EndFormUpdate;
end;

procedure TInputSectionValuesForm.ProcessCheckbox(Sender: TObject);
// Triggered by OnClick event
var
  TempCheckBox: TCheckBox;
  iTag, iTest: integer;
begin
  TempCheckBox := Sender as TCheckBox;
  iTag := TempCheckBox.Tag;
  // Validate associated Edit text
  if ValueCheckBoxArray[iTag].Checked then
    if (TryStrToInt(ValueEditArray[iTag].Text, iTest) = False) then
    begin
      ValueCheckBoxArray[iTag].Checked := False;
      ShowMessageFmt('%s is not an Integer! Please edit it first.',
        [ValueEditArray[iTag].Text]);
    end;
end;

procedure TInputSectionValuesForm.ProcessValueEdit(Sender: TObject);
// Triggered by OnEditingDone event
var
  TempEdit: TEdit;
  iTag, iTest: integer;

begin
  TempEdit := Sender as TEdit;
  iTag := TempEdit.Tag;
  // Auto check/uncheck associated checkbox
  if TryStrToInt(TempEdit.Text, iTest) then
    ValueCheckBoxArray[iTag].Checked := True
  else
    ValueCheckBoxArray[iTag].Checked := False;
  // cmd_Close.SetFocus; // Enable this if you put a ShowMessage in this proc
end;

procedure TInputSectionValuesForm.DisableSectionNameEdit;
begin
  edt_NewSectionName.Enabled := False;
end;

procedure TInputSectionValuesForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title + ' - Make New Section';
  Icon := Application.Icon;
  InitControls;
  sSectionName := edt_NewSectionName.Text;
end;

procedure TInputSectionValuesForm.edt_NewSectionNameEditingDone(Sender: TObject);
begin
  sSectionName := edt_NewSectionName.Text;
end;

procedure TInputSectionValuesForm.FormShow(Sender: TObject);
var
  iCount: integer;
begin
  // Reinitialise controls
  edt_NewSectionName.Enabled := True;
  for iCount := 0 to NumberOfControls - 1 do
  begin
    ;
    IdentEditArray[iCount].Text := Format('Input Key %d here', [iCount + 1]);
    ValueEditArray[iCount].Text := Format('Input value %d here', [iCount + 1]);
    ValueCheckBoxArray[iCount].Checked := False;
  end;
  edt_NewSectionName.Text := 'My New Section';
end;

end.
