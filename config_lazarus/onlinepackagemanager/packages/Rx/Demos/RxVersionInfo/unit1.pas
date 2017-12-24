unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RxAboutDialog, DividerBevel, Forms, Controls,
  Graphics, Dialogs, StdCtrls, EditBtn;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DividerBevel1: TDividerBevel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RxAboutDialog1: TRxAboutDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses RxVersInfo;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  FRxVersionInfo: TRxVersionInfo;
begin
  FRxVersionInfo:=TRxVersionInfo.Create(nil);
  try
    if FileNameEdit1.FileName<>'' then
      FRxVersionInfo.FileName:=FileNameEdit1.FileName;
    if FRxVersionInfo.Valid then
    begin
      Edit1.TextHint:='';
      Edit1.Text:=FRxVersionInfo.FileName;
      Edit2.Text:=FRxVersionInfo.FileLongVersion;
      Edit3.Text:=FRxVersionInfo.CompanyName;
      Edit4.Text:=FRxVersionInfo.FileDescription;
{
  property ProductLongVersion: TLongVersion read GetProductLongVersion;
  property Translation: Pointer read GetTranslation;
  property VersionLanguage: TVersionLanguage read GetVersionLanguage;
  property VersionCharSet: TVersionCharSet read GetVersionCharSet;
  property VersionNum: Longint read GetVersionNum;
  property Comments: string read GetComments;
  property FileVersion: string read GetFileVersion;
  property InternalName: string read GetInternalName;
  property LegalCopyright: string read GetLegalCopyright;
  property LegalTrademarks: string read GetLegalTrademarks;
  property OriginalFilename: string read GetOriginalFilename;
  property ProductVersion: string read GetProductVersion;
  property ProductName: string read GetProductName;
  property SpecialBuild: string read GetSpecialBuild;
  property PrivateBuild: string read GetPrivateBuild;
  property Values[const VerName: string]: string read GetVerValue;
  property VerFileDate: TDateTime read GetVerFileDate;
}
    end
    else
    begin
      Edit1.Text:='';
      Edit1.TextHint:='Resourse information not found.';
      Edit2.Text:='';
      Edit3.Text:='';
      Edit4.Text:='';
    end;
  finally
    FRxVersionInfo.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RxAboutDialog1.Execute;
end;

end.

