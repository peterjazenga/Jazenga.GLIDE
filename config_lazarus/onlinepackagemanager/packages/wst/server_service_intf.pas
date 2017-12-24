{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit server_service_intf;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  base_service_intf;

const
  sREMOTE_IP = 'RemoteIP';
  sREMOTE_PORT = 'RemotePort';
  sSERVICES_EXTENSIONS = 'extensions';
  
type

  IRequestBuffer = interface;
  IServerService = interface;
  IServerServiceRegistry = interface;
  IFormatterResponse = interface;
  IServiceImplementationRegistry = interface;
  IServiceImplementationFactory = interface;
  ICallControl = interface;

  IServiceExtension = interface;
  IServiceExtensionRegistry = interface;
  IObjectControl = interface;

  ICallControl = interface
    ['{7B4B7192-EE96-4B52-92C7-AE855FBC31E7}']
    procedure SetCallContext(ACallContext : ICallContext);
    function GetCallContext():ICallContext;
  end;
  
  IRequestBuffer = interface
    ['{6BF71D1F-DDC0-4432-83C6-6D50D26762C3}']
    function GetTargetService():string;
    function GetContentType():string;
    function GetContent():TStream;
    function GetResponse():TStream;
    function GetFormat() : string;
    function GetPropertyManager():IPropertyManager;
  end;
  
  IServerService = Interface
    ['{EEBF8E24-8B20-462F-AA4A-48A5C8BAE680}']
    procedure HandleRequest(ARequestBuffer : IRequestBuffer);
  End;

  TMessageStage = (
    msAfterDeserialize, msAfterSerialize, msBeforeDeserialize, msBeforeSerialize
  );
  IServiceExtension = interface
    ['{E192E6B3-7932-4D44-A8AC-135D7A0B8C93}']
    procedure ProcessMessage(
      const AMessageStage  : TMessageStage;
            ACallContext   : ICallContext;
            AMsgData       : IInterface
              { The "AMsgData" parameter actual type depends on the message state
                on correspond to :
                  - IRequestBuffer on "msBeforeDeserialize" and "msAfterSerialize"
                  - IFormatterResponse on "msAfterDeserialize", "msBeforeSerialize"
              }
    );
    function GetPropertyManager():IPropertyManager;
  end;

  IServiceExtensionRegistry = Interface
    ['{68DC78F1-E6CF-4D6B-8473-75288794769C}']
    function Find(const AName : string):IServiceExtension;
    procedure Register(
      const AName    : string;
            AFactory : IItemFactory
    );
  end;

  IServerServiceRegistry = interface
    ['{83E7BBEB-A33D-4A3E-896D-D351C2819009}']
    function Find(const AServiceName : string):IServerService;
    procedure Register(
      const AServiceName : string;
            AFactory     : IItemFactory
    );
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
  end;

  IServiceImplementationFactory = interface(IItemFactoryEx)
    ['{23A745BC-5F63-404D-BF53-55A6E64DE5BE}']
    procedure RegisterExtension(
      const AExtensionList : array of string
    ); overload;
    function GetExtension(
      out AExtensionList : string
    ) : Boolean;
    procedure RegisterExtension(
      const AExtension  : string;
      const AInitString : string
    ); overload;
  end;
  
  IServiceImplementationRegistry = Interface
    ['{0AE04033-475E-4FD5-88BD-9F816FD53A97}']
    function FindFactory(const AServiceName : string):IServiceImplementationFactory;
    function Register(
      const AServiceName : string;
            AFactory     : IServiceImplementationFactory
    ) : IServiceImplementationFactory;
  End;

  IFormatterResponse = Interface(IFormatterBase)
    ['{CA7538D4-2C16-48C2-9F39-ACE45FEBB27E}']
    procedure BeginCallResponse(Const AProcName,ATarget:string);
    procedure EndCallResponse();
    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;
    procedure BeginExceptionList(
      const AErrorCode : string;
      const AErrorMsg  : string
    );
    procedure EndExceptionList();
  End;

  IObjectControl = interface
    ['{C422C7CA-4C95-48A4-9A82-2616E619F851}']
    procedure Activate();
    procedure Deactivate();
    function CanBePooled() : Boolean;
  end;
    
  TServiceVerbMethod = procedure(AFormatter:IFormatterResponse; AContext : ICallContext) of object;
  
  { TBaseServiceBinder }

{$M+}
  TBaseServiceBinder = Class(TInterfacedObject,IServerService)
  Private
    FVerbList : TObjectList;
    FImplementationFactory : IServiceImplementationFactory;
  Protected
    procedure RegisterVerbHandler(
      const AVerb        : string;
            AVerbHandler : TServiceVerbMethod
    );
    function FindVerbHandler(const AVerb : string):TServiceVerbMethod;
    procedure HandleRequest(ARequestBuffer : IRequestBuffer);
    function GetFactory():IServiceImplementationFactory;
    function CreateCallContext():ICallContext;virtual;
    procedure DoProcessMessage(
      const AMessageStage  : TMessageStage;
            ACallContext   : ICallContext;
            AMsgData       : IInterface
    );
  Public
    constructor Create(AImplementationFactory : IServiceImplementationFactory);
    destructor Destroy();override;
    procedure Error(Const AMsg : string);overload;
    procedure Error(Const AMsg : string;Const AArgs : Array of Const);overload;
  End;
{$M-}
 
  { TBaseServiceImplementation }

  TBaseServiceImplementation = class(TSimpleFactoryItem,ICallControl)
  private
    FCallContext : ICallContext;
  protected
    procedure SetCallContext(ACallContext : ICallContext);
    function GetCallContext():ICallContext;
  End;

  { TActivableServiceImplementation }

  TActivableServiceImplementation = class(TBaseServiceImplementation,IObjectControl)
  protected
    procedure Activate();virtual;
    procedure Deactivate();virtual;
    function CanBePooled() : Boolean;virtual;
  end;

  { TImplementationFactory }

  TImplementationFactory = class(
    TSimpleItemFactoryEx,
    IInterface,
    IItemFactory,
    IItemFactoryEx,
    IServiceImplementationFactory
  )
  protected
    procedure ReleaseInstance(const AInstance : IInterface);override;
    procedure RegisterExtension(
      const AExtensionList : array of string
    ); overload;
    function GetExtension(
      out AExtensionList : string
    ) : Boolean;
    procedure RegisterExtension(
      const AExtension  : string;
      const AInitString : string
    ); overload;
  end;


  procedure HandleServiceRequest(
    ARequestBuffer   : IRequestBuffer;
    AServiceRegistry : IServerServiceRegistry = Nil
  );
  function GetFormatterRegistry():IFormatterRegistry;
  function GetServerServiceRegistry():IServerServiceRegistry;
  function GetServiceImplementationRegistry():IServiceImplementationRegistry ;
  function GetServiceExtensionRegistry():IServiceExtensionRegistry;

  procedure initialize_server_services_intf();
  procedure finalize_server_services_intf();
  
implementation
uses
  wst_consts;
  
Var
  FormatterRegistryInst : IFormatterRegistry = Nil;
  ServerServiceRegistryInst : IServerServiceRegistry = Nil;
  ServiceImplementationRegistryInst : IServiceImplementationRegistry = Nil;
  ServiceExtensionRegistryInst : IServiceExtensionRegistry = nil;


procedure HandleServiceRequest(
  ARequestBuffer   : IRequestBuffer;
  AServiceRegistry : IServerServiceRegistry
);
Var
  sr : IServerServiceRegistry;
  s : IServerService;
  svcName : string;
Begin
  Assert(Assigned(ARequestBuffer));
  If Assigned(AServiceRegistry) Then
    sr := AServiceRegistry
  Else
    sr := GetServerServiceRegistry();
  svcName := ARequestBuffer.GetTargetService();
  s := sr.Find(svcName);
  If Not Assigned(s) Then
    Raise EServiceException.CreateFmt(SERR_ServiceNotFound,[svcName]);
  s.HandleRequest(ARequestBuffer);
End;

type

  { TFormatterRegistryItem }

  TFormatterRegistryItem = class(TBaseFactoryRegistryItem)
  private
    FContentType: string;
  public
    constructor Create(
      const AName,
            AContentType : string;
      const AFactory     : IItemFactory
    );
    property ContentType : string read FContentType;
  end;
  
  TFormatterRegistry = class(TInterfacedObject,IFormatterRegistry)
  private
    FList : TObjectList;
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): TFormatterRegistryItem;
    function FindFactory(const AName: string): IItemFactory;
    function Find(const AName: string): IFormatterBase;
  private
    property Count : Integer read GetCount;
    property Item[Index:Integer] : TFormatterRegistryItem read GetItem;
  public
    constructor Create();
    destructor Destroy();override;
    procedure Register(
      const AName,
            AContentType   : string;
            AFactory       : IItemFactory
    );
  end;

  { TServerServiceRegistry }

  TServerServiceRegistry = class(TBaseFactoryRegistry,IServerServiceRegistry)
  protected
    function Find(const AServiceName : string):IServerService;
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
  end;


{ TBaseFormatterRegistryItem }

constructor TFormatterRegistryItem.Create(
  const AName,
        AContentType : string;
  const AFactory     : IItemFactory
);
begin
  inherited Create(AName,AFactory);
  FContentType := AContentType;
end;

{ TServerServiceRegistry }

function TServerServiceRegistry.Find(const AServiceName: string): IServerService;
Var
  fct : IItemFactory;
begin
  fct := FindFactory(AServiceName);
  If Assigned(fct) Then
    Result := fct.CreateInstance() as IServerService
  Else
    Result := Nil;
end;

function TFormatterRegistry.Find(const AName : string): IFormatterBase;
Var
  fct : IItemFactory;
begin
  fct := FindFactory(AName);
  if Assigned(fct) then
    Result := fct.CreateInstance() as IFormatterBase
  else
    Result := nil;
end;

function TFormatterRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFormatterRegistry.GetItem(Index: Integer) : TFormatterRegistryItem;
begin
  Result := FList[Index] as TFormatterRegistryItem;
end;

function TFormatterRegistry.FindFactory(const AName: string): IItemFactory;
var
  i , c : Integer;
  s : string;
  itm : TFormatterRegistryItem;
begin
  s := LowerCase(Trim(AName));
  c := Pred(FList.Count);
  for i := 0 to c do begin
    itm := Item[i];
    if AnsiSameText(itm.Name,s) then begin
      Result := itm.Factory;
      Exit;
    end;
  end;
  for i := 0 to c do begin
    itm := Item[i];
    if AnsiSameText(itm.ContentType,s) then begin
      Result := itm.Factory;
      Exit;
    end;
  end;
  Result := nil;
end;

constructor TFormatterRegistry.Create();
begin
  inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TFormatterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TFormatterRegistry.Register(
  const AName,
        AContentType   : string;
        AFactory       : IItemFactory
);
begin
  Assert(Assigned(AFactory));
  if not Assigned(FindFactory(AName)) then
    FList.Add(TFormatterRegistryItem.Create(AName,AContentType,AFactory));
end;

Type

  { TServiceVerbItem }

  TServiceVerbItem = class
  private
    FVerb: string;
    FVerbHandler: TServiceVerbMethod;
  public
    constructor Create(
      const AVerb        : string;
            AVerbHandler : TServiceVerbMethod
    );
    property Verb : string Read FVerb;
    property VerbHandler : TServiceVerbMethod Read FVerbHandler;
  End;

function TServerServiceRegistry.GetCount: Integer;
begin
  Result := Count;
end;

function TServerServiceRegistry.GetName(const AIndex: Integer): string;
begin
  Result := Item[AIndex].Name;
end;

{ TServiceVerbItem }

constructor TServiceVerbItem.Create(
  const AVerb: string;
        AVerbHandler: TServiceVerbMethod
);
begin
  FVerb := AVerb;
  FVerbHandler := AVerbHandler;
end;

{ TBaseServiceBinder }

procedure TBaseServiceBinder.RegisterVerbHandler(
  const AVerb        : string;
        AVerbHandler : TServiceVerbMethod
);
Var
  s : string;
begin
  Assert(Assigned(AVerbHandler));
  s := LowerCase(Trim(AVerb));
  If Not Assigned(FindVerbHandler(s)) Then
    FVerbList.Add(TServiceVerbItem.Create(s,AVerbHandler));
end;

function TBaseServiceBinder.FindVerbHandler(const AVerb: string):TServiceVerbMethod;
Var
  i : Integer;
  s : string;
begin
  s := LowerCase(Trim(AVerb));
  For i := 0 To Pred(FVerbList.Count) Do Begin
    If AnsiSameText(TServiceVerbItem(FVerbList[i]).Verb,s) Then Begin
      Result := TServiceVerbItem(FVerbList[i]).VerbHandler;
      Exit;
    End;
  End;
  Result := Nil;
end;

procedure TBaseServiceBinder.HandleRequest(ARequestBuffer: IRequestBuffer);
Var
  f : IFormatterResponse;
  s, msgFormat : string;
  m : TServiceVerbMethod;
  strm : TStream;
  cllCtx : ICallContext;
  i, j: Integer;
  hdr : THeaderBlock;
  typRegItm : TTypeRegistryItem;
begin
  s := Trim(ARequestBuffer.GetFormat());
  if ( Length(s) = 0 ) then begin
    s := ARequestBuffer.GetContentType();
  end;
  //Extract the base ContentType : type "/" subtype *( ";" parameter )
  j := Length(s);
  for i := 1 to Length(s) do begin
    if ( s[i] = ';' ) then begin
      j := ( i - 1 );
      Break;
    end;
  end;
  msgFormat := Copy(s,1,j);
  f := GetFormatterRegistry().Find(msgFormat) as IFormatterResponse;
  if not Assigned(f) then
    Error(SERR_NoSerializerFoThisType,[s]);
  try
    cllCtx := CreateCallContext();
    cllCtx.GetPropertyManager().Copy(ARequestBuffer.GetPropertyManager(),False);
    DoProcessMessage(msBeforeDeserialize,cllCtx,ARequestBuffer);
    strm := ARequestBuffer.GetContent();
    f.LoadFromStream(strm);
    f.BeginCallRead(cllCtx);
    DoProcessMessage(msAfterDeserialize,cllCtx,f);
    s := f.GetCallProcedureName();
    m := FindVerbHandler(s);
    if not Assigned(m) then
      Error(SERR_NoHandlerForThatVerb,[s]);
    m(f,cllCtx);
    for i := 0 to Pred(cllCtx.GetHeaderCount(AllHeaderDirection)) do begin
      hdr := cllCtx.GetHeader(i);
      if ( hdr.Direction = hdIn ) and ( hdr.mustUnderstand <> 0 ) and ( not hdr.Understood ) then begin
        typRegItm := GetTypeRegistry().Find(hdr.ClassName);
        if Assigned(typRegItm) then
          s := typRegItm.DeclaredName
        else
          s := hdr.ClassName;
        Error(SERR_HeaderNotUnderstood,[s]);
      end;
    end;
  except
    on e : EBaseRemoteException do begin
      f.Clear();
      f.SetSerializationStyle(ssNodeSerialization);
      if (e.FaultString = '') and (e.Message <> '') then
        e.FaultString := e.Message;
      f.BeginExceptionList(e.FaultCode,e.FaultString);
      f.EndExceptionList();
    end;
    on e : Exception do begin
      f.Clear();
      f.SetSerializationStyle(ssNodeSerialization);
      f.BeginExceptionList('Server',E.Message);
      f.EndExceptionList();
    end;
  end;
  strm := ARequestBuffer.GetResponse();
  DoProcessMessage(msBeforeSerialize,cllCtx,f);
  f.SaveToStream(strm);
  DoProcessMessage(msAfterSerialize,cllCtx,ARequestBuffer);
end;

function TBaseServiceBinder.GetFactory(): IServiceImplementationFactory;
begin
  Result := FImplementationFactory;
end;

function TBaseServiceBinder.CreateCallContext(): ICallContext;
begin
  Result := TSimpleCallContext.Create();
end;

procedure TBaseServiceBinder.DoProcessMessage(
  const AMessageStage : TMessageStage;
        ACallContext  : ICallContext;
        AMsgData      : IInterface
);
var
  s, extInitString : string;
  ls : TStringList;
  i : Integer;
  exreg : IServiceExtensionRegistry;
  se : IServiceExtension;
  pm : IPropertyManager;
begin
  exreg := GetServiceExtensionRegistry();
  if FImplementationFactory.GetExtension(s) then begin
    pm := FImplementationFactory.GetPropertyManager(sSERVICES_EXTENSIONS,True);
    ls := TStringList.Create();
    try
      ls.QuoteChar := #0;
      ls.Delimiter := PROP_LIST_DELIMITER;
      ls.DelimitedText := s;
      for i := 0 to Pred(ls.Count) do begin
        s := ls[i];
        se := exreg.Find(s);
        if Assigned(se) then begin
          extInitString := pm.GetProperty(s);
          if ( Length(extInitString) > 0 ) then
            se.GetPropertyManager().SetProperties(extInitString);
          se.ProcessMessage(AMessageStage,ACallContext,AMsgData);
        end;
      end;
    finally
      ls.Free();
    end;
  end;
end;

constructor TBaseServiceBinder.Create(AImplementationFactory : IServiceImplementationFactory);
begin
  Assert(Assigned(AImplementationFactory));
  inherited Create();
  FImplementationFactory := AImplementationFactory;
  FVerbList := TObjectList.Create(True);
end;

destructor TBaseServiceBinder.Destroy();
begin
  FVerbList.Free();
  inherited Destroy();
end;

procedure TBaseServiceBinder.Error(const AMsg: string);
begin
  Raise EServiceException.Create(AMsg);
end;

procedure TBaseServiceBinder.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EServiceException.CreateFmt(AMsg,AArgs);
end;

function GetFormatterRegistry():IFormatterRegistry;
begin
  Result := FormatterRegistryInst;
end;

function GetServerServiceRegistry():IServerServiceRegistry;
begin
  Result := ServerServiceRegistryInst;
end;

Type

  { TServiceImplementationRegistry }

  TServiceImplementationRegistry = class(TInterfacedObject,IServiceImplementationRegistry)
  private
    FList : TObjectList;
  protected
    function FindFactory(const AServiceName : string): IServiceImplementationFactory;
    function Register(
      const AServiceName : string;
            AFactory     : IServiceImplementationFactory
    ) : IServiceImplementationFactory;
  public
    constructor Create();
    destructor Destroy();override;
  End;

  { TServiceImplementationRegistryItem }

  TServiceImplementationRegistryItem = class
  private
    FFactory: IServiceImplementationFactory;
    FItemTypeInfo: string;
  public
    constructor Create(
      const AItemTypeInfo : string;
            AFactory      : IServiceImplementationFactory
    );
    property ItemTypeInfo : string Read FItemTypeInfo;
    property Factory : IServiceImplementationFactory Read FFactory;
  End;

function TServiceImplementationRegistry.FindFactory(
  const AServiceName : string
): IServiceImplementationFactory;
Var
  i : Integer;
begin
  For i := 0 To Pred(FList.Count) Do Begin
    If ( AServiceName = TServiceImplementationRegistryItem(FList[i]).ItemTypeInfo ) Then Begin
      Result := TServiceImplementationRegistryItem(FList[i]).Factory;
      Exit;
    End;
  End;
  Result := Nil;
end;

function TServiceImplementationRegistry.Register(
  const AServiceName : string;
        AFactory     : IServiceImplementationFactory
) : IServiceImplementationFactory;
begin
  Assert(Assigned(AFactory));
  if not Assigned(FindFactory(AServiceName)) then
    FList.Add(TServiceImplementationRegistryItem.Create(AServiceName,AFactory));
  Result := AFactory;
end;

constructor TServiceImplementationRegistry.Create();
begin
  FList := TObjectList.Create(True);
  inherited Create();
end;

destructor TServiceImplementationRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

{ TServiceImplementationRegistryItem }

constructor TServiceImplementationRegistryItem.Create(
  const AItemTypeInfo: string;
        AFactory: IServiceImplementationFactory
);
begin
  Assert(Assigned(AFactory));
  FItemTypeInfo := AItemTypeInfo;
  FFactory := AFactory;
end;

function GetServiceImplementationRegistry():IServiceImplementationRegistry ;
begin
  Result := ServiceImplementationRegistryInst;
end;

{ TBaseServiceImplementation }

procedure TBaseServiceImplementation.SetCallContext(ACallContext: ICallContext);
begin
  FCallContext := ACallContext;
end;

function TBaseServiceImplementation.GetCallContext(): ICallContext;
begin
  Result := FCallContext;
end;


{ TImplementationFactory }
const
  sLIST = 'list';

procedure TImplementationFactory.ReleaseInstance(const AInstance : IInterface);
var
  objCtrl : IObjectControl;
begin
  if Pooled and
     Supports(AInstance,IObjectControl,objCtrl) and
     ( not objCtrl.CanBePooled() )
  then begin
    DiscardInstance(AInstance);
  end else begin
    inherited ReleaseInstance(AInstance);
  end;
end;

procedure TImplementationFactory.RegisterExtension(
  const AExtensionList : array of string
);
var
  i : Integer;
begin
  if ( Length(AExtensionList) > 0 ) then begin
    for i := Low(AExtensionList) to High(AExtensionList) do
      RegisterExtension(AExtensionList[i],'');
  end;
end;

procedure TImplementationFactory.RegisterExtension(
  const AExtension  : string;
  const AInitString : string
);

  function IsIn(const AList, AItem : string) : Boolean;
  var
    ls : TStringList;
  begin
    ls := TStringList.Create();
    try
      ls.QuoteChar := #0;
      ls.Delimiter := PROP_LIST_DELIMITER;
      ls.DelimitedText := AList;
      Result := ( ls.IndexOf(AItem) >= 0 );
    finally
      ls.Free();
    end;
  end;

var
  pmngr : IPropertyManager;
  strBuffer, s : string;
  wasExistent : Boolean;
begin
  strBuffer := Trim(AExtension);
  if ( Length(strBuffer) > 0 ) then begin
    pmngr := GetPropertyManager(sSERVICES_EXTENSIONS,True);
    s := Trim(pmngr.GetProperty(sLIST));
    wasExistent := IsIn(s,strBuffer);
    if ( Length(s) = 0 ) or ( not wasExistent ) then begin
      if ( Length(s) = 0 ) then
        s := strBuffer
      else
        s := Format('%s;%s',[s,strBuffer]);
      pmngr.SetProperty(sLIST,s);
    end;
    s := Trim(AInitString);
    if wasExistent or ( Length(s) > 0 ) then
      pmngr.SetProperty(strBuffer,s);
  end;
end;

function TImplementationFactory.GetExtension(
  out AExtensionList : string
): Boolean;
var
  pmngr : IPropertyManager;
begin
  pmngr := GetPropertyManager(sSERVICES_EXTENSIONS,False);
  if Assigned(pmngr) then
    AExtensionList := Trim(pmngr.GetProperty(sLIST))
  else
    AExtensionList := '';
  Result := ( Length(AExtensionList) > 0 );
end;

type

  { TServiceExtensionRegistry }

  TServiceExtensionRegistry = class(TBaseFactoryRegistry,IServiceExtensionRegistry)
  protected
    function Find(const AName : string):IServiceExtension;
  End;

{ TServiceExtensionRegistry }

function TServiceExtensionRegistry.Find(const AName: string): IServiceExtension;
Var
  fct : IItemFactory;
begin
  fct := FindFactory(AName);
  If Assigned(fct) Then
    Result := fct.CreateInstance() as IServiceExtension
  Else
    Result := Nil;
end;

function GetServiceExtensionRegistry():IServiceExtensionRegistry ;
begin
  Result := ServiceExtensionRegistryInst;
end;

{ TActivableServiceImplementation }

procedure TActivableServiceImplementation.Activate();
begin

end;

procedure TActivableServiceImplementation.Deactivate();
begin

end;

function TActivableServiceImplementation.CanBePooled(): Boolean;
begin
  Result := False;
end;

procedure initialize_server_services_intf();
begin
  if ( FormatterRegistryInst = nil ) then
    FormatterRegistryInst := TFormatterRegistry.Create();
  if ( ServerServiceRegistryInst = nil ) then begin
    ServerServiceRegistryInst := TServerServiceRegistry.Create();
  end;
  if ( ServiceImplementationRegistryInst = nil ) then
    ServiceImplementationRegistryInst := TServiceImplementationRegistry.Create();
  if ( ServiceExtensionRegistryInst = nil ) then
    ServiceExtensionRegistryInst := TServiceExtensionRegistry.Create();
end;

procedure finalize_server_services_intf();
begin
  ServiceExtensionRegistryInst := nil;
  ServiceImplementationRegistryInst := Nil;
  ServerServiceRegistryInst := Nil;
  FormatterRegistryInst := Nil;
end;

initialization
  initialize_server_services_intf();
  
finalization
  finalize_server_services_intf();
  
end.
