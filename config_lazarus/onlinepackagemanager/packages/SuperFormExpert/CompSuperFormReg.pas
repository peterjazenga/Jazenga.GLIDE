unit CompSuperFormReg;

interface

{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}


uses
{$IFDEF FPC}
  Classes,
{$ELSE}
  ExptIntf, EditIntf, Windows, Consts, StdCtrls, DesignIntf,
{$ENDIF}
  SysUtils, CompSuperForm;

{$IFNDEF FPC}
type
  TSuperFormExpert = class(TIExpert)
  public
    function GetStyle: TExpertStyle; override;
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;

  TSuperFormProjectExpert = class(TIExpert)
  public
    function GetStyle: TExpertStyle; override;
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetIDString: string; override;
    function GetMenuText: string; override;
    procedure Execute; override;
  end;

  TSuperFormProjectCreator = class(TIProjectCreator)
  public
    function Existing: boolean; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function NewProjectSource(const ProjectName: string): string; override;
    procedure NewDefaultModule; override;
    procedure NewProjectResource(Module: TIModuleInterface); override;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFNDEF FPC}
uses
  ToolIntf, TypInfo, DesignEditors;

const
  CRLF = #13#10;
{$ENDIF}

procedure Register;
begin
{$IFDEF FPC}
  RegisterClass(TSuperForm);
{$ELSE}
  RegisterCustomModule(TSuperForm, TCustomModule);
  RegisterLibraryExpert(TSuperFormExpert.Create);
  RegisterLibraryExpert(TSuperFormProjectExpert.Create);
{$ENDIF}
  //        RegisterPropertyEditor(TypeInfo(TDFSVersion), TSuperForm, 'Version', TDFSVersionProperty);
end;

{$IFNDEF FPC}
type
  TSuperFormModuleCreator = class(TIModuleCreatorEx)
  private
    FAncestorIdent: string;
    FAncestorClass: TClass;
    FFormIdent: string;
    FUnitIdent: string;
    FFileName: string;
  public
    function Existing: boolean; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function GetFormName: string; override;
    function GetAncestorName: string; override;
    function NewModuleSource(const UnitIdent, FormIdent, AncestorIdent: string):
      string; override;
    function GetIntfName: string; override;
    function NewIntfSource(const UnitIdent, FormIdent, AncestorIdent: string):
      string; override;
    procedure FormCreated(Form: TIFormInterface); override;
  end;

  { TSuperFormModuleCreator }

function TSuperFormModuleCreator.Existing: boolean;
begin
  Result := FALSE;
end;

function TSuperFormModuleCreator.GetFileName: string;
begin
  Result := '';
end;

function TSuperFormModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TSuperFormModuleCreator.GetFormName: string;
begin
  Result := FFormIdent;
end;

function TSuperFormModuleCreator.GetAncestorName: string;
begin
  Result := FAncestorIdent;
end;

function GetCustomFormUnit(const AClass: TClass): string;
begin
  Result := GetTypeData(PTypeInfo(AClass.ClassInfo))^.UnitName;
end;

function TSuperFormModuleCreator.GetIntfName: string;
begin
  Result := '';
end;

const
  COMMENT_LINE = '//---------------------------------------------------------------------------'
    + CRLF;

function TSuperFormModuleCreator.NewIntfSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := ''; // Delphi doesn't use this
end;

function TSuperFormModuleCreator.NewModuleSource(const UnitIdent, FormIdent,
  AncestorIdent: string): string;
begin
  Result := 'unit ' + FUnitIdent + ';' + CRLF + CRLF +
    'interface' + CRLF + CRLF +
    'uses' + CRLF +
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs';

  if (FAncestorIdent <> 'Form') and (FAncestorIdent <> 'DataModule') then
    Result := Result + ',' + CRLF +
      '  ' + GetCustomFormUnit(FAncestorClass);

  Result := Result + ';' + CRLF + CRLF +
    'type' + CRLF +
    '  T' + FFormIdent + ' = class(' + FAncestorClass.ClassName + ')' + CRLF +
    '  private' + CRLF +
    CRLF +
    (*
'    { Private declarations }' + CRLF +
'  protected' + CRLF +
'    { Protected declarations }' + CRLF +
*)
  '  public' + CRLF +
    CRLF +
    (*
'    { Public declarations }' + CRLF +
'  published' + CRLF +
'    { Published declarations }' + CRLF +
*)
  '  end;' + CRLF + CRLF +
    'var' + CRLF +
    '  ' + FFormIdent + ' : T' + FFormIdent + ';' + CRLF + CRLF +
    'implementation' + CRLF + CRLF +
    '{$R *.DFM}' + CRLF + CRLF +
    'end.' + CRLF;
end;

procedure TSuperFormModuleCreator.FormCreated(Form: TIFormInterface);
begin
  // do nothing
end;

{ TSuperFormExpert }

function TSuperFormExpert.GetStyle: TExpertStyle;
begin
  // Make it show up in the object repository (File | New)
  Result := esForm;
end;

function TSuperFormExpert.GetName: string;
begin
  // official name
  Result := 'Super Form'
end;

function TSuperFormExpert.GetAuthor: string;
begin
  Result := 'Philippe Cazaux-Moutou 2002';
end;

function TSuperFormExpert.GetComment: string;
begin
  Result := 'Création d''une SuperForm dans le projet courant';
end;

function TSuperFormExpert.GetPage: string;
begin
  Result := 'Philippe';
end;

function TSuperFormExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'TSUPERFORM');
end;

function TSuperFormExpert.GetState: TExpertState;
begin
  // not used in a esForm expert
  Result := [esEnabled];
end;

function TSuperFormExpert.GetIDString: string;
begin
  // must be unique
  Result := 'DelphiPCMComponents.TSuperFormWizard';
end;

function TSuperFormExpert.GetMenuText: string;
begin
  Result := '';
    // not used for esForm, just here to shut up the compiler warning.
end;

procedure TSuperFormExpert.Execute;
var
  IModuleCreator: TSuperFormModuleCreator;
  IModule: TIModuleInterface;
begin
  IModuleCreator := TSuperFormModuleCreator.Create;
  try
    IModuleCreator.FAncestorIdent := 'SuperForm'; // Don't include the 'T'!!!!
    IModuleCreator.FAncestorClass := TSuperForm;
    ToolServices.GetNewModuleAndClassName(IModuleCreator.FAncestorIdent,
      IModuleCreator.FUnitIdent, IModuleCreator.FFormIdent,
        IModuleCreator.FFileName);
    IModule := ToolServices.ModuleCreateEx(IModuleCreator, [cmShowSource,
      cmShowForm, cmMarkModified, cmAddToProject, cmUnNamed]);
    IModule.Free;
  finally
    IModuleCreator.Free;
  end;
end;

{ TSuperFormProjectExpert }

function TSuperFormProjectExpert.GetStyle: TExpertStyle;
begin
  // Make it show up in the object repository (File | New)
  Result := esProject;
end;

function TSuperFormProjectExpert.GetName: string;
begin
  // official name
  Result := 'Super Form Application'
end;

function TSuperFormProjectExpert.GetAuthor: string;
begin
  Result := 'Philippe Cazaux-Moutou';
end;

function TSuperFormProjectExpert.GetComment: string;
begin
  Result := 'Nouveau projet Super form';
end;

function TSuperFormProjectExpert.GetPage: string;
begin
  Result := 'Philippe';
end;

function TSuperFormProjectExpert.GetGlyph: HICON;
begin
  Result := LoadIcon(hInstance, 'TSUPERFORM');
end;

function TSuperFormProjectExpert.GetState: TExpertState;
begin
  // not used in a esForm expert
  Result := [esEnabled];
end;

function TSuperFormProjectExpert.GetIDString: string;
begin
  // must be unique
  Result := 'DelphiPCMComponents.TSuperFormProjectWizard';
end;

function TSuperFormProjectExpert.GetMenuText: string;
begin
  Result := '';
    // not used for esForm, just here to shut up the compiler warning.
end;

procedure TSuperFormProjectExpert.Execute;
var
  ModIntf: TIModuleInterface;
  ProjCreator: TSuperFormProjectCreator;
begin
  ProjCreator := TSuperFormProjectCreator.Create;
  ModIntf := ToolServices.ProjectCreate(ProjCreator, [cpApplication,
    cpCanShowSource]);
  ModIntf.Free;
  ProjCreator.Free;
end;

{ TSuperFormProjectCreator }

function TSuperFormProjectCreator.Existing: boolean;
begin
  Result := FALSE;
end;

function TSuperFormProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TSuperFormProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

procedure TSuperFormProjectCreator.NewDefaultModule;
var
  IModuleCreator: TSuperFormModuleCreator;
  IModule: TIModuleInterface;
begin
  IModuleCreator := TSuperFormModuleCreator.Create;
  try
    IModuleCreator.FAncestorIdent := 'SuperForm'; // Don't include the 'T'!!!!
    IModuleCreator.FAncestorClass := TSuperForm;
    ToolServices.GetNewModuleAndClassName(IModuleCreator.FAncestorIdent,
      IModuleCreator.FUnitIdent, IModuleCreator.FFormIdent,
        IModuleCreator.FFileName);
    IModule := ToolServices.ModuleCreateEx(IModuleCreator, [cmShowSource,
      cmShowForm, cmMainForm, cmMarkModified, cmAddToProject, cmUnNamed]);
    IModule.Free;
  finally
    IModuleCreator.Free;
  end;
end;

procedure TSuperFormProjectCreator.NewProjectResource(
  Module: TIModuleInterface);
begin
  Module.Free;
end;

function TSuperFormProjectCreator.NewProjectSource(
  const ProjectName: string): string;
begin
  Result := Format('program %s;' + CRLF + CRLF, [ProjectName]) +
    'uses' + CRLF +
    '  Forms;' + CRLF + CRLF +
    '{$R *.RES}' + CRLF + CRLF +
    'begin' + CRLF +
    '  Application.Initialize;' + CRLF +
    '  Application.Run;' + CRLF +
    'end.' + CRLF;
end;
{$ENDIF}

end.

