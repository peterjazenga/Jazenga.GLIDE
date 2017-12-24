unit umemoform;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, SysUtils,
  Buttons, Classes, Dialogs,Controls;

type

  { TShowINIForm }

  TShowINIForm = class(TForm)
    cmd_Abort: TBitBtn;
    cmd_saveChanges: TButton;
    cmd_Close: TBitBtn;
    Memo_INI: TMemo;
    procedure cmd_CloseClick(Sender: TObject);
    procedure cmd_saveChangesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo_INIChange(Sender: TObject);
  private
  public
    bDirty: boolean;
    sINIFilePath: string;
    // Help = MakeReadonly, OpenINI = MakeWriteable
    procedure MakeReadOnly;
    procedure MakeWriteable;
  end;

var
  ShowINIForm: TShowINIForm;

implementation
// Uses umainform;
{$R *.lfm}

{ TShowINIForm }
procedure TShowINIForm.MakeReadOnly;
begin
   Memo_INI.Readonly:=TRUE;
end;

procedure TShowINIForm.MakeWriteable;
begin
  Memo_INI.Readonly:=FALSE;
end;

procedure TShowINIForm.cmd_saveChangesClick(Sender: TObject);
begin
  if bDirty then
    try
     {$I+}
      Memo_INI.Lines.SaveToFile(sINIFilePath);
      ShowMessage('Changes saved OK.');
      Close;
    except
      On E: Exception do
        ShowMessageFmt('Oops! Error: %s', [E.Message]);
    end;
end;

procedure TShowINIForm.cmd_CloseClick(Sender: TObject);
begin
  if bDirty then
    If MessageDlg('Discard changes?',mtConfirmation,[MBYES,MBNO],0,MBNO) = mrNo then
      Begin
         cmd_saveChanges.Click;
         Exit;
      end;
  Close;
end;

procedure TShowINIForm.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
end;

procedure TShowINIForm.FormShow(Sender: TObject);
begin
  bDirty := False;
  cmd_saveChanges.Enabled := False;
end;

procedure TShowINIForm.Memo_INIChange(Sender: TObject);
begin
  bDirty := True;
  cmd_saveChanges.Enabled := True;
end;

end.
