unit Unit1;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, charencstreams,
  ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnOpen: TButton;
    btnSave: TButton;
    cbBOM: TCheckBox;
    cbForceType: TCheckBox;
    UniEnc: TComboBox;
    ANSIEnc: TComboBox;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure ANSIEncChange(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbBOMChange(Sender: TObject);
    procedure cbForceTypeChange(Sender: TObject);
    procedure UniEncChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    fCES: TCharEncStream;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fCES := TCharEncStream.Create;
  GetSupportedUniStreamTypes(UniEnc.Items);
  UniEnc.ItemIndex := 0;
  GetSupportedANSIEncodings(ANSIEnc.Items);
  ANSIEnc.ItemIndex := ANSIEnc.Items.IndexOf(GetSystemEncoding);
end;

procedure TForm1.cbBOMChange(Sender: TObject);
begin
  fCES.HasBOM := cbBOM.Checked;
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    if not cbForceType.Checked then fCES.Reset;
    fCES.LoadFromFile(OpenDialog1.FileName);
    Memo1.text := fCES.UTF8Text;
    cbBOM.Checked := fCES.HasBOM;
    cbForceType.Checked := fCES.ForceType;
    UniEnc.ItemIndex := Ord(fCES.UniStreamType);
    ANSIEnc.Enabled := fCES.UniStreamType = ufANSI;
    if ANSIEnc.Enabled then ANSIEnc.ItemIndex := ANSIEnc.Items.IndexOf(fCES.ANSIEnc) else ANSIEnc.ItemIndex := 0;
  end;
end;

procedure TForm1.ANSIEncChange(Sender: TObject);
begin
  fCES.ANSIEnc := ANSIEnc.Items[ANSIEnc.ItemIndex];
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  fCES.HaveType := true;
  SaveDialog1.InitialDir := ExtractFileDir(OpenDialog1.FileName);
  if fCES.UniStreamType = ufANSI then
    SaveDialog1.FileName := 'Test_' + fCES.ANSIEnc + '.txt'
  else
    SaveDialog1.FileName := 'Test_' + UniStreamTypesStrings[Ord(fCES.UniStreamType)] + '_' + BoolToStr(fCES.HasBOM, true) + '.txt';
  if SaveDialog1.Execute then
  begin
    fCES.UTF8Text := Memo1.text;
    fCES.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.cbForceTypeChange(Sender: TObject);
begin
  fCES.ForceType := cbForceType.Checked;
end;


procedure TForm1.UniEncChange(Sender: TObject);
begin
  fCES.UniStreamType := TUniStreamTypes(UniEnc.ItemIndex);
  ANSIEnc.Enabled := fCES.UniStreamType = ufANSI;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fCES.Free;
end;

initialization
{$I unit1.lrs}

end.
