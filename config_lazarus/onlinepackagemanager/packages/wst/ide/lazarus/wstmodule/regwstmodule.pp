unit regwstmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazIDEIntf, ProjectIntf, FormEditingIntf, wstmodule;

Type
  { TFileDescWSTModule }

  TFileDescWSTModule = class(TFileDescPascalUnitWithResource)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
  end;

Procedure Register;

implementation

Var
  FileDescriptorWSTModule: TFileDescWSTModule;

Procedure Register;

begin
  FileDescriptorWSTModule:=TFileDescWSTModule.Create;
  RegisterProjectFileDescriptor(FileDescriptorWSTModule);
  FormEditingHook.RegisterDesignerBaseClass(TWSTModule);
end;

{ TFileDescWSTModule }

constructor TFileDescWSTModule.Create;
begin
  inherited Create;
  Name:='WST Request processing Module';
  ResourceClass:=TWSTModule;
  UseCreateFormStatements:=False;
end;

function TFileDescWSTModule.GetInterfaceUsesSection: string;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+', HTTPDefs, websession, fpHTTP, wstmodule';
end;

function TFileDescWSTModule.GetLocalizedName: string;
begin
  Result:='WST Request processing Module';
end;

function TFileDescWSTModule.GetLocalizedDescription: string;
begin
  Result:='WST Request processing Module'#13#10+
          'Module to process WST (Web Services Toolkit) requests and return the result';
end;

function TFileDescWSTModule.GetImplementationSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:=inherited GetImplementationSource(Filename, SourceName, ResourceName);
  Result:=Result+'  RegisterHTTPModule(''T'+ResourceName+''',T'+ResourceName+');'+LineEnding;
end;

initialization

finalization
  FreeAndNil(FileDescriptorWSTModule)
end.

