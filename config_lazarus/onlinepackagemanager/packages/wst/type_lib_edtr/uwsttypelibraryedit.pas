{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit uwsttypelibraryedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, ActnList, Contnrs,
  pastree, pascal_parser_intf, logger_intf, LCLVersion,
  SynHighlighterPas, SynEdit, StdCtrls, SynHighlighterXML
  {$IFDEF WST_IDE},ProjectIntf{$ENDIF};

type

  TSearchArea = ( saEditor, saTree );
  
  { TfWstTypeLibraryEdit }

  TfWstTypeLibraryEdit = class(TForm)
    actExit: TAction;
    actExport: TAction;
    actAbout: TAction;
    actEnumCreate: TAction;
    actCompoundCreate: TAction;
    actIntfCreate: TAction;
    actFullExpand: TAction;
    actFullCollapse: TAction;
    actDelete : TAction;
    actArrayCreate : TAction;
    actEditSearch : TAction;
    actClone : TAction;
    actAddXsdImport : TAction;
    actShowOptions: TAction;
    actSaveXSD : TAction;
    actTreeSearch : TAction;
    actRecordCreate : TAction;
    actTypeALiasCreate : TAction;
    actSave : TAction;
    actNewFile: TAction;
    actRefreshView: TAction;
    actUpdateObject: TAction;
    actSaveAs: TAction;
    actOpenFile: TAction;
    AL: TActionList;
    FD : TFindDialog;
    MainMenu1: TMainMenu;
    edtDocumentation : TMemo;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33 : TMenuItem;
    MenuItem34 : TMenuItem;
    MenuItem35 : TMenuItem;
    MenuItem36 : TMenuItem;
    MenuItem37 : TMenuItem;
    MenuItem38 : TMenuItem;
    MenuItem39 : TMenuItem;
    MenuItem40 : TMenuItem;
    MenuItem41 : TMenuItem;
    MenuItem42 : TMenuItem;
    MenuItem43 : TMenuItem;
    MenuItem44 : TMenuItem;
    MenuItem45 : TMenuItem;
    MenuItem46 : TMenuItem;
    MenuItem47 : TMenuItem;
    MenuItem48 : TMenuItem;
    MenuItem49 : TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50 : TMenuItem;
    MenuItem51 : TMenuItem;
    MenuItem52 : TMenuItem;
    MenuItem53 : TMenuItem;
    MenuItem54 : TMenuItem;
    MenuItem55 : TMenuItem;
    MenuItem56 : TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7 : TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mmoLog: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OD: TOpenDialog;
    odOpenXSD : TOpenDialog;
    PCInfos : TPageControl;
    PC: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SD: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2 : TSplitter;
    srcInterface: TSynEdit;
    SB: TStatusBar;
    srcImp: TSynEdit;
    srcBinder: TSynEdit;
    srcProxy: TSynEdit;
    srcWSDL: TSynEdit;
    edtXSD : TSynEdit;
    SynPasSyn1: TSynPasSyn;
    SynXMLSyn1: TSynXMLSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tvDependency : TTreeView;
    tsDependencies : TTabSheet;
    tsXSD : TTabSheet;
    tsDocumentation : TTabSheet;
    tsWSDL: TTabSheet;
    tsLog: TTabSheet;
    tsImp: TTabSheet;
    tsBinder: TTabSheet;
    tsInterface: TTabSheet;
    tsProxy: TTabSheet;
    trvSchema: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actAddXsdImportExecute(Sender : TObject);
    procedure actAddXsdImportUpdate(Sender : TObject);
    procedure actArrayCreateExecute(Sender : TObject);
    procedure actCloneExecute(Sender : TObject);
    procedure actCloneUpdate(Sender : TObject);
    procedure actCompoundCreateExecute(Sender: TObject);
    procedure actDeleteExecute (Sender : TObject );
    procedure actDeleteUpdate(Sender : TObject);
    procedure actEnumCreateExecute(Sender: TObject);
    procedure actEnumCreateUpdate(Sender : TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actExportUpdate(Sender: TObject);
    procedure actFullCollapseExecute(Sender: TObject);
    procedure actFullExpandExecute(Sender: TObject);
    procedure actIntfCreateExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actRecordCreateExecute(Sender : TObject);
    procedure actRefreshViewExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveExecute (Sender : TObject );
    procedure actEditSearchExecute(Sender : TObject);
    procedure actEditSearchUpdate(Sender : TObject);
    procedure actSaveXSDExecute(Sender : TObject);
    procedure actShowOptionsExecute(Sender: TObject);
    procedure actTreeSearchExecute(Sender : TObject);
    procedure actTreeSearchUpdate(Sender : TObject);
    procedure actTypeALiasCreateExecute(Sender : TObject);
    procedure actUpdateObjectExecute(Sender: TObject);
    procedure actUpdateObjectUpdate(Sender: TObject);
    procedure FDFind(Sender : TObject);
    procedure FDShow(Sender : TObject);
    procedure FormClose (Sender : TObject; var CloseAction : TCloseAction );
    procedure FormDropFiles(Sender : TObject; const FileNames : array of String);
    procedure FormShow(Sender: TObject);
    procedure trvSchemaSelectionChanged(Sender : TObject);
  private
    FSymbolTable : TwstPasTreeContainer;
    FStatusMessageTag : Integer;
    FCurrentFileName : string;
{$IFDEF WST_IDE}
    FProjectLibrary : TLazProjectFile;
{$ENDIF}
    FSearchArea : TSearchArea;
    FFoundNode : TTreeNode;
    FDependencyList : TObjectList;
  private
    procedure Handle_OnAppException(Sender : TObject; E : Exception);
    function FindCurrentEditor() : TSynEdit;
    function Search(const AText : string) : Boolean;
    function SearchInTree(const AText : string) : Boolean;
    function SearchInEditor(const AText : string) : Boolean;
  private
    function GetTypeNode() : TTreeNode;
    function GetInterfaceNode() : TTreeNode;
  private
    procedure ShowStatusMessage(const AMsgType : TMessageType;const AMsg : string);
    procedure RenderSymbols();
    procedure RenderSources();
    procedure RenderWSDL();
    procedure ShowDocumentation();
    procedure ShowSourceXSD();
    procedure ShowDependencies();
    procedure AddXsdImport(const AFileName : string);
    procedure OpenFile(const AFileName : string; const AContent : TStream = nil);
    procedure SaveToFile(const AFileName : string);
    function PromptAndSave() : Boolean;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy();override;
  end; 

var
  fWstTypeLibraryEdit: TfWstTypeLibraryEdit;

implementation

{$R *.lfm}

uses
  view_helper, DOM, wst_fpc_xml, XMLWrite,
  xsd_parser, wsdl_parser, source_utils, command_line_parser, generator,
  metadata_generator, generatorbase,
  binary_streamer, wst_resources_utils, xsd_generator, wsdl_generator,
  uabout, edit_helper, udm, ufrmsaveoption, ueditoptions, pparser, SynEditTypes
  {$IFDEF WST_IDE},LazIDEIntf,IDEMsgIntf, IDEExternToolIntf{$ENDIF}
  , xsd_consts, parserutils, locators;

{$IFDEF WST_IDE}
function GetCurrentProjectLibraryFile():TLazProjectFile;
var
  i, c : Integer;
begin
  Result := nil;
  c := LazarusIDE.ActiveProject.FileCount;
  for i := 0 to Pred(c) do begin
    if AnsiSameText('.wsdl',ExtractFileExt(LazarusIDE.ActiveProject.Files[i].Filename)) then begin
      Result := LazarusIDE.ActiveProject.Files[i];
      Break;
    end;
  end;
end;
{$ENDIF}

const
  DEF_FILE_NAME = 'library1';

type
  TSourceOptions = set of TComandLineOption;

function ParsePascalFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
) : TwstPasTreeContainer;overload;
const
  s_ostype =
    {$IFDEF WINDOWS}
      'WINDOWS'
    {$ELSE}
      {$IFDEF LINUX}
        'LINUX'
      {$ELSE}
        ''
      {$ENDIF}
    {$ENDIF};
    
  procedure DoNotify(const AMsgType : TMessageType; const AMsg : string);
  begin
    if ( ANotifier <> nil ) then
      ANotifier(AMsgType,AMsg);
  end;
  
var
  symName : string;
begin
  symName := ChangeFileExt(ExtractFileName(AFileName),'');
  if ( symName[Length(symName)] = '.' ) then begin
    Delete(symName,Length(symName),1);
  end;
  Result := TwstPasTreeContainer.Create();
  try
    Result.CaseSensitive := False; // Pascal is Case Insensitive !
    DoNotify(mtInfo,Format('Parsing file %s ...',[AFileName]));
    CreateWstInterfaceSymbolTable(Result);
    ParseSource(Result,AFileName,s_ostype,'');
    CreateDefaultBindingForIntf(Result);
    DoNotify(mtInfo,Format('File parsed %s .',[AFileName]));
  except
    on e : Exception do begin
      DoNotify(mtError,e.Message);
      FreeAndNil(Result);
      //raise;
    end;
  end;
end;

function ParseWsdlFile(
  const AFileName : string;
        AContent  : TStream;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
var
  locDoc : TXMLDocument;
  prsr : IParser;
  symName : string;
  locContext : IParserContext;
  locLocator : IDocumentLocator;
begin
  Result := nil;
  symName := ChangeFileExt(ExtractFileName(AFileName),'');
  if ( symName[Length(symName)] = '.' ) then begin
    Delete(symName,Length(symName),1);
  end;
  prsr := nil;
  locDoc := ReadXMLFile(AContent);
  try
    Result := TwstPasTreeContainer.Create();
    try
      Result.CaseSensitive := DM.CaseSensitive;
      Result.XsdStringMaping := DM.XsdStringMaping;
      prsr := TWsdlParser.Create(locDoc,Result,ANotifier);
      locContext := prsr as IParserContext;
      if (locContext <> nil) then begin
        locLocator := TFileDocumentLocator.Create(ExtractFilePath(AFileName));
        locContext.SetDocumentLocator(locLocator);
      end;
      prsr.Execute(pmAllTypes,symName);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    FreeAndNil(locDoc);
  end;
end;

function ParseWsdlFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
var
  locContent : TMemoryStream;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    locContent := TMemoryStream.Create();
    try
      locContent.LoadFromFile(AFileName);
      locContent.Position := 0;
      Result := ParseWsdlFile(AFileName,locContent,ANotifier);
    finally
      FreeAndNil(locContent);
    end;
  end;
end;

function ParseXsdFile(
  const AFileName : string;
        AContent  : TStream;
  const ANotifier : TOnParserMessage;
        ASymbols  : TwstPasTreeContainer
) : TwstPasTreeContainer;overload;
var
  locDoc : TXMLDocument;
  prsr : IXsdPaser;
  symName : string;
  locContext : IParserContext;
  locLocator : IDocumentLocator;
begin
  Result := nil;
  symName := ChangeFileExt(ExtractFileName(AFileName),'');
  if ( symName[Length(symName)] = '.' ) then begin
    Delete(symName,Length(symName),1);
  end;
  prsr := nil;
  locDoc := ReadXMLFile(AContent);
  try
    if (ASymbols = nil) then begin
      Result := TwstPasTreeContainer.Create();
      Result.CaseSensitive := DM.CaseSensitive;
      Result.XsdStringMaping := DM.XsdStringMaping;
    end else begin
      Result := ASymbols;
    end;
    try
      prsr := TXsdParser.Create(locDoc,Result,'',ANotifier);
      locContext := prsr as IParserContext;
      if Assigned(locContext) then begin
        locLocator := TFileDocumentLocator.Create(ExtractFilePath(AFileName));
        locContext.SetDocumentLocator(locLocator);
      end;
      prsr.ParseTypes();
    except
      if (ASymbols = nil) then
        FreeAndNil(Result);
      raise;
    end;
  finally
    FreeAndNil(locDoc);
  end;
end;

function ParseXsdFile(
  const AFileName : string;
        AContent  : TStream;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
begin
  Result := ParseXsdFile(AFileName,AContent,ANotifier,nil);
end;

function ParseXsdFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;overload;
var
  locContent : TMemoryStream;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    locContent := TMemoryStream.Create();
    try
      locContent.LoadFromFile(AFileName);
      locContent.Position := 0;
      Result := ParseXsdFile(AFileName,locContent,ANotifier);
    finally
      FreeAndNil(locContent);
    end;
  end;
end;

type TOutputType = ( otMemory, otFileSystem );
function GenerateSource(
        ASymbolTable : TwstPasTreeContainer;
        AOptions     : TSourceOptions;
  const AOutputType  : TOutputType;
  const AOutPath     : string;
  const ANotifier    : TOnParserMessage
) : ISourceManager;

  procedure Notify(const AMsg : string);
  begin
    if Assigned(ANotifier) then begin
      ANotifier(mtInfo, AMsg);
    end;
  end;
  
var
  mtdaFS: TMemoryStream;
  g : TBaseGenerator;
  mg : TMetadataGenerator;
  rsrcStrm : TMemoryStream;
begin
  Result := CreateSourceManager();
  rsrcStrm := nil;
  mtdaFS := nil;
  mg := nil;
  g := Nil;
  try
  
    if ( cloInterface in AOptions ) then begin
      Notify('Interface file generation...');
      g := TInftGenerator.Create(ASymbolTable,Result);
      if ( cloGenerateDocAsComments in AOptions ) then
        g.Options := g.Options + [goGenerateDocAsComments];
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloProxy in AOptions ) then begin
      Notify('Proxy file generation...');
      g := TProxyGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloBinder in AOptions ) then begin
      Notify('Binder file generation...');
      g := TBinderGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( cloImp in AOptions ) then begin
      Notify('Implementation file generation...');
      g := TImplementationGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( AOutputType = otFileSystem ) and ( [cloBinder,cloProxy]*AOptions  <> [] ) then begin
      Notify('Metadata file generation...');
      mtdaFS := TMemoryStream.Create();
      mg := TMetadataGenerator.Create(ASymbolTable,CreateBinaryWriter(mtdaFS));
      mg.Execute();
      //mtdaFS.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_META]));
      rsrcStrm := TMemoryStream.Create();
      mtdaFS.Position := 0;
      BinToWstRessource(UpperCase(ASymbolTable.CurrentModule.Name),mtdaFS,rsrcStrm);
      rsrcStrm.SaveToFile(AOutPath + Format('%s.%s',[ASymbolTable.CurrentModule.Name,sWST_EXTENSION]));
    end;
    
    if ( AOutputType = otFileSystem ) then begin
      Result.SaveToFile(AOutPath);
    end;
  finally
    rsrcStrm.Free();
    mg.Free();;
    mtdaFS.Free();;
    g.Free();
  end;
end;

procedure GenerateWSDL_ToStream(
        ASymbol   : TwstPasTreeContainer;
        ADest     : TStream;
  const ADestPath : string;
  const ANotifier : TOnParserMessage
);
var
  g : IGenerator;
  doc : TXMLDocument;
  locLocator : IDocumentLocator;
begin
  doc := TXMLDocument.Create();
  try
    g := TWsdlGenerator.Create(doc);
    locLocator := TFileDocumentLocator.Create(IncludeTrailingPathDelimiter(ADestPath));
    g.SetDocumentLocator(locLocator);
    if Assigned(ANotifier) then
      g.SetNotificationHandler(ANotifier);
    g.Execute(ASymbol,ASymbol.CurrentModule.Name);
    WriteXML(doc,ADest);
  finally
    FreeAndNil(doc);
  end;
end;

procedure GenerateXSD_ToStream(
        ASymbol   : TwstPasTreeContainer;
        ADest     : TStream;
  const ADestPath : string;
  const ANotifier : TOnParserMessage
);
var
  g : IGenerator;
  doc : TXMLDocument;
  locLocator : IDocumentLocator;
begin
  doc := TXMLDocument.Create();
  try
    g := TXsdGenerator.Create(doc);
    locLocator := TFileDocumentLocator.Create(IncludeLeadingPathDelimiter(ADestPath));
    g.SetDocumentLocator(locLocator);
    if Assigned(ANotifier) then
      g.SetNotificationHandler(ANotifier);
    g.Execute(ASymbol,ASymbol.CurrentModule.Name);
    WriteXML(doc,ADest);
  finally
    FreeAndNil(doc);
  end;
end;

function CreateSymbolTable(const AName : string):TwstPasTreeContainer ;
var
  mdl : TPasModule;
begin
  Result := TwstPasTreeContainer.Create();
  try
    CreateWstInterfaceSymbolTable(Result);
    mdl := TPasModule(Result.CreateElement(TPasModule,AName,Result.Package,visDefault,'',0));
    mdl.InterfaceSection := TInterfaceSection(Result.CreateElement(TInterfaceSection,'',mdl,visDefault,'',0));
    Result.Package.Modules.Add(mdl);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TfWstTypeLibraryEdit }

procedure TfWstTypeLibraryEdit.actOpenFileExecute(Sender: TObject);
begin
{$IFNDEF WST_IDE}
  OD.InitialDir := DM.Options.ReadString(ClassName(),sLAST_PATH,OD.InitialDir);
{$ENDIF WST_IDE}
  if OD.Execute() then begin
    OpenFile(OD.FileName);
  end;
end;

procedure TfWstTypeLibraryEdit.actRecordCreateExecute(Sender : TObject);
var
  e : TPasRecordType;
begin
  e := CreateRecordObject(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actRefreshViewExecute(Sender: TObject);
begin
  RenderSymbols();
end;

procedure TfWstTypeLibraryEdit.actSaveAsExecute(Sender: TObject);
begin
{$IFNDEF WST_IDE}
  SD.InitialDir := DM.Options.ReadString(ClassName(),sLAST_PATH,SD.InitialDir);
{$ENDIF WST_IDE}
  if SD.Execute() then begin
    SaveToFile(SD.FileName);
    FCurrentFileName := SD.FileName;
  end;
end;

procedure TfWstTypeLibraryEdit.actSaveExecute (Sender : TObject );
begin
  if FileExists(FCurrentFileName) then
    SaveToFile(FCurrentFileName)
  else
    actSaveAs.Execute() ;
end;

procedure TfWstTypeLibraryEdit.actEditSearchExecute(Sender : TObject);
begin
  FSearchArea := saEditor;
  FD.Execute();
end;

procedure TfWstTypeLibraryEdit.actEditSearchUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := ( PC.ActivePageIndex in [0..4] );
end;

procedure TfWstTypeLibraryEdit.actSaveXSDExecute(Sender : TObject);
var
  oldFilter, locFileName : string;
begin
  oldFilter := SD.Filter;
  SD.Filter := 'XSD files ( *.xsd )|*.xsd';
  try
    if SD.Execute() then begin
      locFileName := ChangeFileExt(SD.FileName,'.xsd');
      SaveToFile(locFileName);
      FCurrentFileName := locFileName;
    end;
  finally
    SD.Filter := oldFilter;
  end;
end;

procedure TfWstTypeLibraryEdit.actShowOptionsExecute(Sender: TObject);
var
  f : TfEditOptions;
begin
  f := TfEditOptions.Create(nil);
  try
    f.ShowModal();
  finally
    f.Release();
  end;
end;

procedure TfWstTypeLibraryEdit.actTreeSearchExecute(Sender : TObject);
begin
  FSearchArea := saTree;
  FD.Execute();
end;

procedure TfWstTypeLibraryEdit.actTreeSearchUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := ( GetTypeNode().Count > 0 );
end;

procedure TfWstTypeLibraryEdit.actTypeALiasCreateExecute(Sender : TObject);
var
  e : TPasAliasType;
begin
  e := CreateAliasType(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actUpdateObjectExecute(Sender: TObject);
var
  o : TPasElement;
  nd, nd_1, locTypeNode : TTreeNode;
begin
  nd := trvSchema.Selected;
  if Assigned(nd) and Assigned(nd.Data) then begin
    o := TPasElement(nd.Data);
    if HasEditor(o,etUpdate) and UpdateObject(o,FSymbolTable) then begin
      nd_1  := nd;
      locTypeNode := GetTypeNode();
      trvSchema.BeginUpdate();
      try
        nd := FindPainter(o).Paint(FSymbolTable,o,locTypeNode);
        nd.MoveTo(nd_1,naInsertBehind);
        FreeAndNil(nd_1);
      finally
        trvSchema.EndUpdate();
      end;
    end;
  end;
end;

procedure TfWstTypeLibraryEdit.actUpdateObjectUpdate(Sender: TObject);
var
  ok : Boolean;
  locItem : TPasElement;
begin
  ok := Assigned(trvSchema.Selected) and Assigned(trvSchema.Selected.Data);
  if ok then begin
    locItem := TPasElement(trvSchema.Selected.Data);
    if locItem.InheritsFrom(TPasModule) and (locItem <> FSymbolTable.CurrentModule) then
      ok := False
    else
      ok := ( not(locItem.InheritsFrom(TPasType)) or
              (FindModule(TPasType(locItem)) = FSymbolTable.CurrentModule)
            );
    ok := ok and HasEditor(locItem,etUpdate);
  end;
  TAction(Sender).Enabled := ok;
end;

procedure TfWstTypeLibraryEdit.FDFind(Sender : TObject);
begin
  if not Search(FD.FindText) then
    ShowMessageFmt('Unable to find "%s".',[FD.FindText]);
end;

procedure TfWstTypeLibraryEdit.FDShow(Sender : TObject);
var
  edt : TSynEdit;
begin
  FFoundNode := nil;
  edt := FindCurrentEditor();
  if Assigned(edt) and edt.SelAvail then
    FD.FindText := edt.SelText;
end;

procedure TfWstTypeLibraryEdit.FormClose (Sender : TObject; var CloseAction : TCloseAction );
begin
  PromptAndSave();
end;

procedure TfWstTypeLibraryEdit.FormDropFiles(
        Sender : TObject; 
  const FileNames : array of String
);
var
  locFileName : string;
  c, i : Integer;
begin
  c := Length(FileNames); 
  if (c > 0) then begin
    locFileName := '';
    for i := 0 to c - 1 do begin
      if FileExists(FileNames[i]) then begin
        locFileName := FileNames[i];
        Break;
      end;
    end;
    if (locFileName <> '') then
      OpenFile(locFileName);
  end;
end;

{$IFDEF WST_IDE}
procedure ShowParsing(FileName: string);
begin
{$IF lcl_fullversion < 1030000}
  IDEMessagesWindow.AddMsg(Format('Parsing %s...',[Filename]),ExtractFileDir(Filename),0);
{$ELSE}
  IDEMessagesWindow.AddCustomMessage(mluNote, 'Parsing ...', Filename);
{$ENDIF}
end;

procedure ShowParsed(FileName: string);
begin
{$IF lcl_fullversion < 1030000}
  IDEMessagesWindow.AddMsg(Format('File Parsed %s.',[Filename]),ExtractFileDir(Filename),0);
{$ELSE}
  IDEMessagesWindow.AddCustomMessage(mluNote, 'File Parsed.', Filename);
{$ENDIF}
end;
{$ENDIF WST_IDE}

procedure TfWstTypeLibraryEdit.FormShow(Sender: TObject);
var
{$IFDEF WST_IDE}
  prjFile : TLazProjectFile;
  locContent : TMemoryStream;
{$ENDIF}
  i : Integer;
begin
{$IFDEF WST_IDE}
  prjFile := GetCurrentProjectLibraryFile();
  if ( prjFile <> nil ) then begin
    locContent := TMemoryStream.Create();
    try
      locContent.LoadFromFile(prjFile.FileName);
      if ( locContent.Size > 0 ) then begin
        locContent.Position := 0;
        ShowParsing(prjFile.Filename);
        OpenFile(prjFile.Filename,locContent);
        ShowParsed(prjFile.Filename);
        FProjectLibrary := prjFile;
      end;
    finally
      FreeAndNil(locContent);
    end;
  end;
{$ENDIF}
  RenderSymbols();
  for i := 0 to AL.ActionCount-1 do begin
    if (AL.Actions[i] is TCustomAction) and
       (TCustomAction(AL.Actions[i]).Hint = '')
    then
      TCustomAction(AL.Actions[i]).Hint := TCustomAction(AL.Actions[i]).Caption;
  end;
end;

procedure TfWstTypeLibraryEdit.trvSchemaSelectionChanged(Sender : TObject);
begin
  ShowDocumentation();
  ShowSourceXSD();
  ShowDependencies();
end;

procedure TfWstTypeLibraryEdit.Handle_OnAppException(
  Sender: TObject; E: Exception
);
begin
  if (E <> nil) and E.InheritsFrom(EWstEditException) then
    MessageDlg(Self.Caption,E.Message,Dialogs.mtError,[mbOK],0)
  else
    Application.ShowException(E);
end;

function TfWstTypeLibraryEdit.FindCurrentEditor() : TSynEdit;
var
  edt : TSynEdit;
begin
  case PC.ActivePageIndex of
    0 : edt := srcInterface;
    1 : edt := srcWSDL;
    2 : edt := srcProxy;
    3 : edt := srcImp;
    4 : edt := srcBinder;
    else
      edt := nil;
  end;
  Result := edt;
end;

function TfWstTypeLibraryEdit.Search(const AText : string) : Boolean;
begin
  if ( FSearchArea = saEditor ) then
    Result := SearchInEditor(AText)
  else
    Result := SearchInTree(AText);
end;

function TfWstTypeLibraryEdit.SearchInTree(const AText : string) : Boolean;
var
  curLock : IInterface;
  searchDir : TSearchDirection;
begin
  curLock := SetCursorHourGlass();
  if ( FFoundNode <> nil ) then
    FFoundNode := FFoundNode.GetNext();
  if ( frDown in FD.Options ) then
    searchDir := sdForward
  else
    searchDir := sdbackward;
  FFoundNode := SearchItem(AText,GetTypeNode(),FFoundNode,searchDir);
  Result := ( FFoundNode <> nil );
  if Result then begin
    FFoundNode.Selected := True ;
    FFoundNode.Focused := True ;
  end;
  curLock := nil;
end;

function TfWstTypeLibraryEdit.SearchInEditor(const AText : string) : Boolean;
var
  edt : TSynEdit;
  opts : TSynSearchOptions;
begin
  Result := False;
  edt := FindCurrentEditor();
  if Assigned(edt) then begin
    opts := [];
    if not ( frDown in FD.Options ) then
      Include(opts,ssoBackwards);
    if ( frMatchCase in FD.Options ) then
      Include(opts,ssoMatchCase);
    if ( frWholeWord in FD.Options ) then
      Include(opts,ssoWholeWord);
    Result := edt.SearchReplace(AText,'',opts) <> 0;
  end;
end;

function TfWstTypeLibraryEdit.GetTypeNode(): TTreeNode;
begin
  Result := trvSchema.Items[0].GetFirstChild().Items[0];
end;

function TfWstTypeLibraryEdit.GetInterfaceNode(): TTreeNode;
begin
  Result := trvSchema.TopItem.GetFirstChild().Items[1];
end;

procedure TfWstTypeLibraryEdit.ShowStatusMessage(const AMsgType : TMessageType;const AMsg: string);
begin
  mmoLog.Lines.Add(Format('%s : %s',[MessageTypeNames[AMsgType],AMsg]));
  SB.Panels[1].Text := AMsg;
  Inc(FStatusMessageTag);
  if ( (FStatusMessageTag) > 23 ) then begin
    FStatusMessageTag := 0;
    Application.ProcessMessages();
  end;
end;

procedure TfWstTypeLibraryEdit.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TfWstTypeLibraryEdit.actAboutExecute(Sender: TObject);
var
  fa : TfAbout;
begin
  fa := TfAbout.Create(Self);
  try
    fa.ShowModal();
  finally
    fa.Release();
  end;
end;

procedure TfWstTypeLibraryEdit.actAddXsdImportExecute(Sender : TObject);
begin
{$IFNDEF WST_IDE}
   odOpenXSD.InitialDir := DM.Options.ReadString(ClassName(),sLAST_PATH,odOpenXSD.InitialDir);
{$ENDIF WST_IDE}
  odOpenXSD.Title := 'Import a schema file ...';
  if not odOpenXSD.Execute() then
    exit;
  AddXsdImport(odOpenXSD.FileName);
end;

procedure TfWstTypeLibraryEdit.actAddXsdImportUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := (FSymbolTable <> nil) and
                             (FSymbolTable.CurrentModule <> nil);
end;

procedure TfWstTypeLibraryEdit.actArrayCreateExecute(Sender : TObject);
var
  e : TPasArrayType;
begin
  e := CreateArray(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actCloneExecute(Sender : TObject);
var
  o : TPasElement;
  nd, nd_1 : TTreeNode;
  locHandler : TObjectUpdaterClass;
  locNewItem : TPasElement;
begin
  nd := trvSchema.Selected;
  if Assigned(nd) and Assigned(nd.Data) then begin
    o := TPasElement(nd.Data);
    if HasEditor(o,etClone,locHandler) then begin
      locNewItem := locHandler.CloneObject(o,FSymbolTable);
      if ( locNewItem <> nil ) then begin
        trvSchema.BeginUpdate();
        try
          nd_1 := FindPainter(locNewItem).Paint(FSymbolTable,locNewItem,GetTypeNode());   
          if ( nd_1 <> nil ) then begin
            nd_1.Expand(True);
            trvSchema.Selected := nd_1;
            trvSchema.MakeSelectionVisible();
          end;
        finally
          trvSchema.EndUpdate();
        end;
      end;  
    end;
  end; 
end;

procedure TfWstTypeLibraryEdit.actCloneUpdate(Sender : TObject);
var
  ok : Boolean;
  locItem : TPasElement;
begin
  ok := Assigned(trvSchema.Selected) and Assigned(trvSchema.Selected.Data);
  if ok then begin
    locItem := TPasElement(trvSchema.Selected.Data);
    ok := ( not(locItem.InheritsFrom(TPasType)) or
            (FindModule(TPasType(locItem)) = FSymbolTable.CurrentModule)
          ) and
          HasEditor(locItem,etClone);
  end;
  TAction(Sender).Enabled := ok;
end;

procedure TfWstTypeLibraryEdit.actCompoundCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateClassObject(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actDeleteExecute (Sender : TObject );
var
  o : TPasElement;
  nd : TTreeNode;
begin
  nd := trvSchema.Selected;
  if Assigned(nd) and Assigned(nd.Data) then begin
    o := TPasElement(nd.Data);
    if HasEditor(o,etDelete) then begin
      if ( MessageDlg(Format('Delete this object "%s" ?',[o.Name]),mtConfirmation,mbYesNo,0) = mrYes ) then begin
        DeleteObject(o,FSymbolTable);
        trvSchema.BeginUpdate();
        try
          FreeAndNil(nd);
        finally
          trvSchema.EndUpdate();
        end;
      end;
    end;
  end;
end;

procedure TfWstTypeLibraryEdit.actDeleteUpdate(Sender : TObject);
var
  ok : Boolean;
  locItem : TPasElement;
begin
  ok := Assigned(trvSchema.Selected) and Assigned(trvSchema.Selected.Data);
  if ok then begin
    locItem := TPasElement(trvSchema.Selected.Data);
    ok := ( not(locItem.InheritsFrom(TPasType)) or
            (FindModule(TPasType(locItem)) = FSymbolTable.CurrentModule)
          ) and
          HasEditor(locItem,etDelete);
  end;
  TAction(Sender).Enabled := ok;
end;

procedure TfWstTypeLibraryEdit.actEnumCreateExecute(Sender: TObject);
var
  e : TPasEnumType;
begin
  e := CreateEnum(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetTypeNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actEnumCreateUpdate(Sender : TObject);
begin
  TAction(Sender).Enabled := ( FSymbolTable <> nil );
end;

procedure TfWstTypeLibraryEdit.actExportExecute(Sender: TObject);
var
  curLok : IInterface;
  saveOpts : TComandLineOptions;
  f : TfrmSaveOptions;
begin
  f := TfrmSaveOptions.Create(nil);
  try
    if ( f.ShowModal() = mrOK ) then begin
      saveOpts := [];
      if f.edtBinder.Checked then
        Include(saveOpts,cloBinder);
      if f.edtInterface.Checked then
        Include(saveOpts,cloInterface);
      if f.edtImplementation.Checked then
        Include(saveOpts,cloImp);
      if f.edtProxy.Checked then
        Include(saveOpts,cloProxy);
      if f.edtWrappedParams.Checked then
        Include(saveOpts,cloHandleWrappedParameters);
      if f.edtDocAsComments.Checked then
        Include(saveOpts,cloGenerateDocAsComments);

      curLok := SetCursorHourGlass();
      GenerateSource(
        FSymbolTable,
        saveOpts,
        otFileSystem,
        IncludeTrailingBackslash(f.edtOutputDir.Text),
        nil
      );
      curLok := nil;
    end;
  finally
    f.Release();
  end;
end;

procedure TfWstTypeLibraryEdit.actExportUpdate(Sender: TObject);
begin
  //TAction(Sender).Enabled := Assigned(FSymbolTable) and ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 );
end;

procedure TfWstTypeLibraryEdit.actFullCollapseExecute(Sender: TObject);
begin
  trvSchema.FullCollapse();
end;

procedure TfWstTypeLibraryEdit.actFullExpandExecute(Sender: TObject);
begin
  trvSchema.FullExpand();
end;

procedure TfWstTypeLibraryEdit.actIntfCreateExecute(Sender: TObject);
var
  e : TPasClassType;
begin
  e := CreateInterface(FSymbolTable);
  if Assigned(e) then begin
    FindPainter(e).Paint(FSymbolTable,e,GetInterfaceNode());
  end;
end;

procedure TfWstTypeLibraryEdit.actNewFileExecute(Sender: TObject);
var
  res : Integer;
begin
  res := MessageDlg(Application.Title,'Save the current file before ?',mtConfirmation,mbYesNoCancel,0,mbYes);
  if ( res = mrCancel ) then begin
    Exit;
  end;
  if ( res = mrYes ) then begin
    actSave.Execute();
  end;
  FCurrentFileName := DEF_FILE_NAME;
  {$IFDEF WST_IDE}
  FProjectLibrary := nil;
  {$ENDIF}
  FreeAndNil(FSymbolTable);
  FSymbolTable := CreateSymbolTable(ExtractFileName(FCurrentFileName));
  RenderSymbols();
end;

procedure TfWstTypeLibraryEdit.RenderSymbols();
var
  objPtr : ISymbolPainter;
  nd : TTreeNode;
begin
  mmoLog.Clear();
  trvSchema.BeginUpdate();
  try
    trvSchema.Items.Clear();
    srcInterface.ClearAll();
    nd := trvSchema.Items.AddFirst(nil,'Type Library Editor');
    nd.ImageIndex := -1;
    nd.StateIndex := -1;
    nd.SelectedIndex := -1;
    if Assigned(FSymbolTable) then begin
      objPtr := FindPainter(FSymbolTable.Package);
      if Assigned(objPtr) then begin
        objPtr.Paint(FSymbolTable,FSymbolTable.Package,nd);
      end;
      RenderSources();
      RenderWSDL();
    end;
    trvSchema.Items[0].Expand(False);
    trvSchema.Items[0].Items[0].Expand(False);
  finally
    trvSchema.EndUpdate();
  end;
  ShowStatusMessage(mtInfo,'');
end;

procedure TfWstTypeLibraryEdit.RenderSources();

  procedure LoadText(const AList : TStrings; ASrc : ISourceStream);
  var
    srcItemSV : ISavableSourceStream;
    strm : TStream;
  begin
    if Supports(ASrc,ISavableSourceStream,srcItemSV) then begin
      strm := srcItemSV.GetStream();
      strm.Position := 0;
      AList.LoadFromStream(strm);
    end;
  end;

var
  srcMngr : ISourceManager;
begin
  if Assigned(FSymbolTable) then begin
    if ( FSymbolTable.CurrentModule.InterfaceSection.Declarations.Count > 0 ) then begin
      srcMngr := GenerateSource(FSymbolTable,[cloInterface,cloProxy,cloBinder,cloImp,cloGenerateDocAsComments],otMemory,'',@ShowStatusMessage);
      if Assigned(srcMngr) and ( srcMngr.GetCount() > 0 ) then begin
        LoadText(srcInterface.Lines,srcMngr.GetItem(0));
        LoadText(srcProxy.Lines,srcMngr.GetItem(1));
        LoadText(srcBinder.Lines,srcMngr.GetItem(2));
        LoadText(srcImp.Lines,srcMngr.GetItem(3));
      end;
    end else begin
      srcInterface.ClearAll();
      srcProxy.ClearAll();
      srcBinder.ClearAll();
      srcImp.ClearAll();
    end;
  end;
  ShowStatusMessage(mtInfo,'');
end;

procedure TfWstTypeLibraryEdit.RenderWSDL();
var
  mstrm : TMemoryStream;
begin
  mstrm := TMemoryStream.Create();
  try
    GenerateWSDL_ToStream(
      FSymbolTable,mstrm,ExtractFilePath(FCurrentFileName),@ShowStatusMessage
    );
    mstrm.Position := 0;
    srcWSDL.Lines.LoadFromStream(mstrm);
  finally
    FreeAndNil(mstrm);
  end;
end;

procedure TfWstTypeLibraryEdit.ShowDocumentation();
var
  docString : string;
  props : TStrings;
  elt : TPasElement;
begin
  docString := '';
  if ( trvSchema.Selected <> nil ) and
     ( trvSchema.Selected.Data <> nil ) and
     TObject(trvSchema.Selected.Data).InheritsFrom(TPasElement)
  then begin
    elt := TPasElement(trvSchema.Selected.Data);
    props := FSymbolTable.Properties.FindList(elt);
    if ( props <> nil ) then
      docString :=
        StringReplace(DecodeLineBreak(props.Values[s_documentation]),#10,sLineBreak,[rfReplaceAll]);
    if IsStrEmpty(docString) then
      docString := '< No documentation available for this item. >';
    docString := Format('%s : %s%s',[FSymbolTable.GetExternalName(elt),sLineBreak,docString]);
  end;
  edtDocumentation.Lines.Text := docString;
end;

procedure TfWstTypeLibraryEdit.ShowSourceXSD();
var
  docString : string;
  elt : TPasElement;
begin
  docString := '';
  if ( trvSchema.Selected <> nil ) and
     ( trvSchema.Selected.Data <> nil ) and
     TObject(trvSchema.Selected.Data).InheritsFrom(TPasType)
  then begin
    elt := TPasElement(trvSchema.Selected.Data);
    docString := XsdGenerateSourceForObject(elt,FSymbolTable);
  end;
  edtXSD.Lines.Text := docString;
end;

procedure TfWstTypeLibraryEdit.ShowDependencies();
var
  elt : TPasType;
begin
  if ( csDestroying in Self.ComponentState ) then
    Exit;
  if ( FDependencyList = nil ) then
    FDependencyList := TObjectList.Create(True);
  FDependencyList.Clear();
  if ( trvSchema.Selected <> nil ) and
     ( trvSchema.Selected.Data <> nil ) and
     TObject(trvSchema.Selected.Data).InheritsFrom(TPasType)
  then begin
    elt := TPasType(trvSchema.Selected.Data);
    FindDependencies(elt,FSymbolTable,FDependencyList);
  end;
  DrawDependencies(tvDependency,FDependencyList);
end;

procedure TfWstTypeLibraryEdit.AddXsdImport(const AFileName: string);
var
  i : Integer;
  locModule, locNewModule : TPasModule;
  locStream : TMemoryStream;
begin
  locStream := TMemoryStream.Create();
  try
    locStream.LoadFromFile(odOpenXSD.FileName);
    locStream.Position := 0;
    locModule := FSymbolTable.CurrentModule;
    try
      ParseXsdFile(AFileName,locStream,@ShowStatusMessage,FSymbolTable);
      locNewModule := FSymbolTable.CurrentModule;
    finally
      FSymbolTable.SetCurrentModule(locModule);
    end;
  finally
    locStream.Free();
  end;
  FSymbolTable.Properties.SetValue(locNewModule,sFILE_NAME,AFileName);

  i := FSymbolTable.CurrentModule.InterfaceSection.UsesList.IndexOf(locNewModule);
  if (i = -1) then begin
    FSymbolTable.CurrentModule.InterfaceSection.UsesList.Add(locNewModule);
    locNewModule.AddRef();
  end;
  RenderSymbols();
end;

procedure TfWstTypeLibraryEdit.OpenFile (const AFileName : string; const AContent : TStream);
var
  tmpTable : TwstPasTreeContainer;
  curLok : IInterface;
begin
  {$IFDEF WST_IDE}
  FProjectLibrary := nil;
  {$ENDIF}
  FCurrentFileName := '';
  mmoLog.Clear();
  PC.ActivePage := tsLog;
  curLok := SetCursorHourGlass();
  if SameText('.pas',ExtractFileExt(AFileName)) then begin
    tmpTable := ParsePascalFile(AFileName,@ShowStatusMessage);
  end else if SameText('.xsd',ExtractFileExt(AFileName)) then begin
    if ( AContent = nil ) then
      tmpTable := ParseXsdFile(AFileName,@ShowStatusMessage)
    else
      tmpTable := ParseXsdFile(AFileName,AContent,@ShowStatusMessage);
  end else begin
    if ( AContent = nil ) then
      tmpTable := ParseWsdlFile(AFileName,@ShowStatusMessage)
    else
      tmpTable := ParseWsdlFile(AFileName,AContent,@ShowStatusMessage);
  end;
  if Assigned(tmpTable) then begin
    if AnsiSameText('.pas',ExtractFileExt(AFileName)) then
      FCurrentFileName := ChangeFileExt(AFileName,'.wsdl')
    else
      FCurrentFileName := AFileName;
    trvSchema.Items.Clear();
    FreeAndNil(FSymbolTable);
    FSymbolTable := tmpTable;
    RenderSymbols();
    PC.ActivePage := tsInterface;
  end;
  curLok := nil;
  SB.Panels[0].Text := FCurrentFileName;
{$IFNDEF WST_IDE}
  DM.Options.WriteString(ClassName(),sLAST_PATH,ExtractFilePath(FCurrentFileName));
{$ENDIF WST_IDE}
end;

procedure TfWstTypeLibraryEdit.SaveToFile (const AFileName : string );
var
  mstrm : TMemoryStream;
begin
  mstrm := TMemoryStream.Create();
  try
    if SameText('.xsd',ExtractFileExt(AFileName)) then
      GenerateXSD_ToStream(FSymbolTable,mstrm,ExtractFilePath(FCurrentFileName),@ShowStatusMessage)
    else
      GenerateWSDL_ToStream(FSymbolTable,mstrm,ExtractFilePath(FCurrentFileName),@ShowStatusMessage);
    mstrm.SaveToFile(AFileName);
  finally
    FreeAndNil(mstrm);
  end;
{$IFNDEF WST_IDE}
  DM.Options.WriteString(ClassName(),sLAST_PATH,ExtractFilePath(AFileName));
{$ENDIF WST_IDE}
end;

function TfWstTypeLibraryEdit.PromptAndSave() : Boolean; 
var
  dlgRes : Integer;
{$IFDEF WST_IDE}
  prjFile : TLazProjectFile;
{$ENDIF}
begin
  dlgRes := MessageDlg(Self.Caption,'Save the file before exit ?',mtConfirmation,mbYesNoCancel,0);
  Result := (dlgRes <> mrCancel); 
  if Result then begin
    if ( dlgRes = mrYes ) then begin
      actSave.Execute();
    end;
{$IFDEF WST_IDE}
    if ( FProjectLibrary = nil ) then begin
      prjFile := GetCurrentProjectLibraryFile();
      if ( prjFile = nil ) then begin
        dlgRes := MessageDlg(Self.Caption,'Add this type library to the current project ?',mtConfirmation,mbYesNo,0);
        if ( dlgRes = mrYes ) then begin
          LazarusIDE.DoOpenEditorFile(FCurrentFileName,-1,-1,[ofAddToProject]);
        end;
      end;
    end;
{$ENDIF}
  end;
end;

constructor TfWstTypeLibraryEdit.Create(AOwner: TComponent);
begin
  if ( DM = nil ) then begin
    DM := TDM.Create(Application);
  end;
  inherited Create(AOwner);
  FSymbolTable := CreateSymbolTable(ExtractFileName(DEF_FILE_NAME));
  trvSchema.Images := DM.IM;
  Application.OnException := @Handle_OnAppException;
end;

destructor TfWstTypeLibraryEdit.Destroy();
begin
  FDependencyList.Free();
  trvSchema.Items.Clear();
  FreeAndNil(FSymbolTable);
  inherited Destroy();
  FreeAndNil(DM);
end;

//initialization
  //SetHeapTraceOutput('heap_trace.txt');

end.

