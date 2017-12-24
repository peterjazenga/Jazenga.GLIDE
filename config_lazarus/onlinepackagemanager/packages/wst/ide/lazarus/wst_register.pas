unit wst_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


  procedure Register();
  
implementation
uses MenuIntf, LazIDEIntf,
     wstimportdlg, uwsttypelibraryedit;

procedure ShowImportDialog(Sender: TObject);
var
  f : TformImport;
begin
  f := TformImport.Create(nil);
  try
    if ( LazarusIDE.ActiveProject <> nil ) and
       ( LazarusIDE.ActiveProject.MainFile <> nil )
    then begin
      f.edtOutputDir.Text := ExtractFileDir(LazarusIDE.ActiveProject.MainFile.Filename);
    end;
    f.ShowModal();
  finally
    f.Release();
  end;
end;

procedure ShowTypeLibraryDialog(Sender: TObject);
var
  f : TfWstTypeLibraryEdit;
begin
  f := TfWstTypeLibraryEdit.Create(nil);
  try
    f.ShowModal();
  finally
    f.Release();
  end;
end;

var
  itmWstSection : TIDEMenuSection;

procedure Register();
begin
  itmWstSection := RegisterIDESubMenu(mnuProject,'itmWstSection','Web Services Toolkit',nil,nil);
  RegisterIDEMenuCommand(itmWstSection,'itmWstImportWsdlFile','Import WSDL File ...',nil,@ShowImportDialog);
  RegisterIDEMenuCommand(itmWstSection,'itmWstImportWsdlFile','Type Library Editor ...',nil,@ShowTypeLibraryDialog);
end;
  
  
end.

