unit wstimportdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ActnList, logger_intf;

type

  TGenOption = (
    xgoInterface, xgoInterfaceALL,
    xgoProxy, xgoImp, xgoBinder,
    xgoWrappedParameter, xgoDocAsComments, xgoGenerateObjectCollection
  );
  TGenOptions = set of TGenOption;
  
  TOnParserMessage = procedure (const AMsgType : TMessageType; const AMsg : string) of object;
  
  { TformImport }

  TformImport = class(TForm)
    actOpenDir: TAction;
    actOpenFile: TAction;
    actOK: TAction;
    AL: TActionList;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    edtGenCollection : TCheckBox;
    edtDocAsComments : TCheckBox;
    edtAddToProject : TCheckBox;
    edtOptionIntfALL: TCheckBox;
    edtOptionIntf: TCheckBox;
    edtOptionProxy: TCheckBox;
    edtOptionBinder: TCheckBox;
    edtOptionImp: TCheckBox;
    edtInputFile: TEdit;
    edtOutputDir: TEdit;
    edtOptionWrappedParams : TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    mmoLog: TMemo;
    OD: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SDD: TSelectDirectoryDialog;
    procedure actOKExecute(Sender: TObject);
    procedure actOKUpdate(Sender: TObject);
    procedure actOpenDirExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure edtOptionIntfALLClick(Sender: TObject);
    procedure edtOptionIntfClick(Sender: TObject);
  private
    FStatusMessageTag : Integer;
    procedure ShowStatusMessage(const AMsgType : TMessageType;const AMsg : string);
  public
    function GetOptions() : TGenOptions;
  end; 

var
  formImport: TformImport;

implementation

{$R *.lfm}

uses
  DOM, XMLRead, wst_fpc_xml, pastree, pascal_parser_intf, wsdl_parser, source_utils,
  generator, metadata_generator, binary_streamer, wst_resources_utils,
  {$IFDEF WST_IDE}
  LazIDEIntf, ProjectIntf,
  {$ENDIF}
  locators,xsd_parser,generatorbase;

type
  TSourceType = xgoInterface .. xgoBinder;
  TSourceTypes = set of TSourceType;

function ParseWsdlFile(
  const AFileName : string;
  const ANotifier : TOnParserMessage
):TwstPasTreeContainer;
var
  locDoc : TXMLDocument;
  prsr : IParser;
  symName : string;
  prsrCtx : IParserContext;
begin
  Result := nil;
  if FileExists(AFileName) then begin
    symName := ChangeFileExt(ExtractFileName(AFileName),'');
    if ( symName[Length(symName)] = '.' ) then begin
      Delete(symName,Length(symName),1);
    end;
    locDoc := ReadXMLFile(AFileName);
    try
      Result := TwstPasTreeContainer.Create();
      try
        prsr := TWsdlParser.Create(locDoc,Result,ANotifier);
        prsrCtx := prsr as IParserContext;
        prsrCtx.SetDocumentLocator(TFileDocumentLocator.Create(ExtractFilePath(ExpandFileName(AFileName))));
        prsr.Execute(pmAllTypes,symName);
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      FreeAndNil(locDoc);
    end;
  end;
end;

type TOutputType = ( otMemory, otFileSystem );
function GenerateSource(
        ASymbolTable : TwstPasTreeContainer;
        AOptions     : TSourceTypes;
  const AOutputType  : TOutputType;
  const AOutPath     : string;
  const ANotifier    : TOnParserMessage;
  const AGenOptions  : TGenOptions
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
  wrappedParams : Boolean;
begin
  wrappedParams := ( xgoWrappedParameter in AGenOptions );
  Result := CreateSourceManager();
  rsrcStrm := nil;
  mtdaFS := nil;
  mg := nil;
  g := Nil;
  try

    if ( ( [xgoInterface,xgoInterfaceALL] * AOptions ) <> [] ) then begin
      Notify('Interface file generation...');
      g := TInftGenerator.Create(ASymbolTable,Result);
      if wrappedParams then
        g.Options := g.Options + [goDocumentWrappedParameter];
      if ( xgoDocAsComments in AGenOptions ) then
        g.Options := g.Options + [goGenerateDocAsComments];
      if ( xgoGenerateObjectCollection in AGenOptions ) then
        g.Options := g.Options + [goGenerateObjectCollection];
      g.Execute();
      FreeAndNil(g);
    end;

    if ( xgoProxy in AOptions ) then begin
      Notify('Proxy file generation...');
      g := TProxyGenerator.Create(ASymbolTable,Result);
      if wrappedParams then
        g.Options := g.Options + [goDocumentWrappedParameter];
      g.Execute();
      FreeAndNil(g);
    end;

    if ( xgoBinder in AOptions ) then begin
      Notify('Binder file generation...');
      g := TBinderGenerator.Create(ASymbolTable,Result);
      if wrappedParams then
        g.Options := g.Options + [goDocumentWrappedParameter];
      g.Execute();
      FreeAndNil(g);
    end;

    if ( xgoImp in AOptions ) then begin
      Notify('Implementation file generation...');
      g := TImplementationGenerator.Create(ASymbolTable,Result);
      g.Execute();
      FreeAndNil(g);
    end;

    if ( AOutputType = otFileSystem ) and ( [xgoBinder,xgoProxy]*AOptions  <> [] ) then begin
      Notify('Metadata file generation...');
      mtdaFS := TMemoryStream.Create();
      mg := TMetadataGenerator.Create(ASymbolTable,CreateBinaryWriter(mtdaFS));
      mg.Execute();
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

{ TformImport }

procedure TformImport.actOpenFileExecute(Sender: TObject);
begin
  if OD.Execute() then begin
    edtInputFile.Text := OD.FileName;
  end;
end;

procedure TformImport.edtOptionIntfALLClick(Sender: TObject);
begin
  if edtOptionIntfALL.Checked and ( not edtOptionIntf.Checked ) then
    edtOptionIntf.Checked := True;
end;

procedure TformImport.edtOptionIntfClick(Sender: TObject);
begin
  if ( not edtOptionIntf.Checked ) and edtOptionIntfALL.Checked then
    edtOptionIntfALL.Checked := False;
end;

procedure TformImport.ShowStatusMessage(const AMsgType: TMessageType;const AMsg: string);
begin
  mmoLog.Lines.Add(Format('%s : %s',[MessageTypeNames[AMsgType],AMsg]));
  Inc(FStatusMessageTag);
  if ( (FStatusMessageTag) > 23 ) then begin
    FStatusMessageTag := 0;
    Application.ProcessMessages();
  end;
end;

function TformImport.GetOptions(): TGenOptions;
begin
  Result := [];
  if edtOptionIntf.Checked then begin
    Result := Result + [xgoInterface];
    if edtOptionIntfALL.Checked then begin
      Result := Result + [xgoInterfaceALL];
    end;
  end;
  if edtOptionProxy.Checked then
    Include(Result,xgoProxy);
  if edtOptionBinder.Checked then
    Include(Result,xgoBinder);
  if edtOptionImp.Checked then
    Include(Result,xgoImp);
  if edtOptionWrappedParams.Checked then
    Include(Result,xgoWrappedParameter);
  if edtDocAsComments.Checked then
    Include(Result,xgoDocAsComments);
  if edtGenCollection.Checked then
    Include(Result,xgoGenerateObjectCollection);     
end;

procedure TformImport.actOpenDirExecute(Sender: TObject);
begin
  if SDD.Execute() then begin
    if not DirectoryExists(SDD.FileName) then
      ForceDirectories(SDD.FileName);
    edtOutputDir.Text := SDD.FileName;
  end;
end;

procedure TformImport.actOKUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FileExists(edtInputFile.Text) and
                             DirectoryExists(edtOutputDir.Text) and
                             ( ( GetOptions() - [xgoWrappedParameter,xgoDocAsComments] ) <> [] );
end;

procedure TformImport.actOKExecute(Sender: TObject);
var
  tree : TwstPasTreeContainer;
  oldCursor : TCursor;
  srcMgnr : ISourceManager;
  genOptions : TGenOptions;
  fileSet : TSourceTypes;
  destPath : string;
  {$IFDEF WST_IDE}
  i, c : Integer;
  srcItm : ISourceStream;
  openFlags : TOpenFlags;
  {$ENDIF}
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    tree := ParseWsdlFile(edtInputFile.Text,@ShowStatusMessage);
    try
      genOptions := GetOptions();
      fileSet := genOptions - [xgoWrappedParameter,xgoDocAsComments];
      destPath := IncludeTrailingPathDelimiter(edtOutputDir.Text);
      srcMgnr := GenerateSource(tree, fileSet, otFileSystem, destPath,
                                @ShowStatusMessage, genOptions);
      ShowStatusMessage(mtInfo,'');
      {$IFDEF WST_IDE}
      openFlags := []; // Could be [ofRevert] but works just fine without it.
      if edtAddToProject.Checked then
        Include(openFlags, ofAddToProject);
      c := srcMgnr.GetCount();
      for i := 0 to Pred(c) do begin
        srcItm := srcMgnr.GetItem(i);
        LazarusIDE.DoOpenEditorFile(destPath + srcItm.GetFileName(), -1, -1, OpenFlags);
      end;
      {$ENDIF}
    finally
      srcMgnr := nil;
      tree.Free();
    end;
  finally
    Screen.Cursor := oldCursor;
  end;
  ShowMessage('File parsed succefully.');
  Self.Close();
  ModalResult := mrOK;
end;

end.

