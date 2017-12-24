unit ufrmsaveoption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, Buttons, udm;

type

  { TfrmSaveOptions }

  TfrmSaveOptions = class (TForm )
    actOK : TAction;
    AL : TActionList;
    Button1 : TButton;
    Button2 : TButton;
    btnSelectDir : TButton;
    btnSelectAll: TButton;
    btnUnselectAll: TButton;
    edtDocAsComments : TCheckBox;
    edtInterface : TCheckBox;
    edtProxy : TCheckBox;
    edtImplementation : TCheckBox;
    edtBinder : TCheckBox;
    edtOutputDir : TEdit;
    edtWrappedParams : TCheckBox;
    GroupBox1 : TGroupBox;
    Label1 : TLabel;
    Panel1 : TPanel;
    Panel2 : TPanel;
    SD : TSelectDirectoryDialog;
    procedure actOKExecute (Sender : TObject );
    procedure actOKUpdate (Sender : TObject );
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnSelectDirClick (Sender : TObject );
    procedure btnUnselectAllClick(Sender: TObject);
  private
    procedure SelectAll(const ADoSelect : Boolean);
  public

  end; 

var
  frmSaveOptions : TfrmSaveOptions;

implementation

{$R *.lfm}

{ TfrmSaveOptions }

procedure TfrmSaveOptions.actOKUpdate (Sender : TObject );
begin
  TAction(Sender).Enabled :=
    ( Trim(edtOutputDir.Text) <> '' ) and
    ( edtInterface.Checked or edtProxy.Checked or
      edtImplementation.Checked or edtBinder.Checked
    );
end;

procedure TfrmSaveOptions.btnSelectAllClick(Sender: TObject);
begin
  SelectAll(True);
end;

procedure TfrmSaveOptions.btnSelectDirClick (Sender : TObject );
begin
{$IFNDEF WST_IDE}
  SD.InitialDir := DM.Options.ReadString(ClassName(),sLAST_PATH,SD.InitialDir);
{$ENDIF WST_IDE}
  SD.FileName := edtOutputDir.Text;
  if SD.Execute() then begin
    edtOutputDir.Text := SD.FileName;
  {$IFNDEF WST_IDE}
    DM.Options.WriteString(ClassName(),sLAST_PATH,edtOutputDir.Text);
  {$ENDIF WST_IDE}
  end;
end;

procedure TfrmSaveOptions.btnUnselectAllClick(Sender: TObject);
begin
  SelectAll(False);
end;

procedure TfrmSaveOptions.SelectAll(const ADoSelect: Boolean);
begin
  edtBinder.Checked := ADoSelect;
  edtImplementation.Checked := edtBinder.Checked;
  edtImplementation.Checked := edtBinder.Checked;
  edtInterface.Checked := edtBinder.Checked;
  edtProxy.Checked := edtBinder.Checked;
  //edtWrappedParams.Checked := edtBinder.Checked;
end;


procedure TfrmSaveOptions.actOKExecute (Sender : TObject );
begin
  ForceDirectories(edtOutputDir.Text);
  ModalResult := mrOK;
end;

end.

