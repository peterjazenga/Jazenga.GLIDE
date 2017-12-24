{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Base service interface }
{$INCLUDE wst_global.inc}
unit service_intf;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  base_service_intf;

Const
  sTARGET = 'target';
  
Type

  ICookieManager = interface
    ['{C04EFE37-A6BA-409E-9D9C-25836938858F}']
    function GetCount() : Integer;
    function GetName(const AIndex : Integer) : string;
    function GetValue(const AIndex : Integer) : string; overload;
    function GetValue(const AName : string) : string; overload;
    procedure SetValue(const AIndex : Integer; const AValue : string); overload;
    procedure SetValue(const AName : string; const AValue : string); overload;
  end;
  
  ITransport = Interface
    ['{AEB6677A-9620-4E7D-82A0-43E3C4C52B43}']
    function GetTransportName() : string;
    function GetPropertyManager():IPropertyManager;
    procedure SendAndReceive(ARequest,AResponse:TStream);
    function GetCookieManager() : ICookieManager;
  End;

  //The client formater interface, used to marshall parameters.
  IFormatterClient = Interface(IFormatterBase)
    ['{73746BC7-CA43-4C00-8789-71E23033C3B2}']
    procedure BeginCall(
      const AProcName,
            ATarget      : string;
            ACallContext : ICallContext
    );
    procedure EndCall();
    procedure BeginCallRead(ACallContext : ICallContext);
    function GetCallProcedureName():String;
    function GetCallTarget():String;
  End;
  
     (* This interface is used with IFormatterClient to handle messages *)
      ICallMaker = Interface
        ['{4CF7B98B-8C37-479F-AFF3-822FCCEECEC8}']
        function GetPropertyManager():IPropertyManager;
        procedure MakeCall(
          ASerializer : IFormatterClient;
          ATransport  : ITransport
        );
      End;


  (* A service protocol is defined by :
       - a marshaller
       - a call handler for that marshaller
       - and a tranport. *)
  IServiceProtocol = Interface
    ['{777FE102-0F6C-495C-9A92-528D07F1C60C}']
    function GetSerializer()  : IFormatterClient;  // the marshaller >> SOAP, XML-RPC, Binary,...
    function GetCallHandler() : ICallMaker; // Call handler   >> SOAP call handler, XML-RPC call handler, ...
    function GetTransport()   : ITransport;  // the transport  >> HTTP, TCP, named pipes, ...
      procedure SetTransport(AValue : ITransport);
  End;


  { TBaseProxy }
  (* The base class for service proxy *)
  TBaseProxy = Class(TInterfacedObject,ICallContext,IServiceProtocol)
  private
    FTarget : String;
    FProtocol : IServiceProtocol;
    FOperationsProperties : TStrings;
  private
    procedure LoadProperties();
  protected
    function GetTarget():String;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetSerializer() : IFormatterClient;
    function GetCallHandler() : ICallMaker;
    function GetTransport() : ITransport;
    procedure SetTransport(AValue : ITransport);  
    procedure MakeCall();
    class function GetServiceType() : PTypeInfo;virtual;abstract;

    // ---- BEGIN >> ICallContext implementation ----
      private
        FCallContext : ICallContext;
      protected
        procedure AddObjectToFree(const AObject : TObject);
        procedure Clear();
        function AddHeader(
          const AHeader        : THeaderBlock;
          const AKeepOwnership : Boolean
        ):Integer;overload;
        function AddHeader(
          const AHeader        : TBaseRemotable;
          const AKeepOwnership : Boolean;
          const AName          : string = ''
        ):Integer;overload;
        function GetHeaderCount(const ADirections : THeaderDirections):Integer;
        function GetHeader(const AIndex : Integer) : THeaderBlock;
        // ---- END >> ICallContext implementation ----
        procedure ClearHeaders(const ADirection : THeaderDirection);
        function GetPropertyManager():IPropertyManager;
  public
    (* This is the primary constructor! *)
    constructor Create(
      Const ATarget   : String;             // the target service
      Const AProtocol : IServiceProtocol
    );overload;virtual;
    (* A User friendly constructor *)
    constructor Create(
      Const ATarget        : String;
      Const AProtocolData  : string;
      Const ATransportData : string
    );overload;virtual;
    destructor Destroy();override;
  End;


  IFormaterQueryRegistry = Interface
    ['{037907E1-5E44-4A91-B290-CA70ACACF5E6}']
    function Find(
      Const AProtocolData : string;
      Out   ARes          : IServiceProtocol
    ):Boolean;
    procedure Register(
      Const AProtocolName        : string;
            AFormaterFactory     : IItemFactory;
            ACallHandlerFactory  : IItemFactory
    );
  End;

  ITransportRegistry = Interface
    ['{ED34F7A2-2335-4FD3-A457-2B8C4349664E}']
    function Find(
      Const ATransportData : string;
      Out   ARes           : ITransport
    ):Boolean;
    procedure Register(
      const ATransportName : string;
            AFactory       : IItemFactory
    );
  End;

  function GetFormaterRegistry():IFormaterQueryRegistry;
  function GetTransportRegistry():ITransportRegistry;

implementation
uses wst_consts,imp_utils, metadata_repository;

{ TBaseProxy }

procedure TBaseProxy.LoadProperties();
var
  pd : PPropertyData;
  i : Integer;
  sd : PService;
  opd : PServiceOperation;
  mm : IModuleMetadataMngr;
  strTransportBuffer, strFormatBuffer, strName : string;
begin
  if not Assigned(FOperationsProperties) then begin
    FOperationsProperties := TStringList.Create();
    mm := GetModuleMetadataMngr();
    sd := mm.GetServiceMetadata(GetTypeData(GetServiceType())^.IntfUnit,GetServiceType()^.Name);
    try
      Assert(Assigned(sd));
      opd := sd^.Operations;
      for i := 0 to Pred(sd^.OperationsCount) do begin
        strFormatBuffer := '';
        strTransportBuffer := '';
        pd := opd^.Properties;
        while Assigned(pd) do begin
          strName := ExtractOptionName(pd^.Name);
          if ( AnsiPos(sFORMAT + '_',pd^.Name) = 1 ) then begin
            if ( Length(strName) > 0 ) then begin
              strFormatBuffer := Format('%s%s=%s;',[strFormatBuffer,strName,pd^.Data]);
            end;
          end else if ( AnsiPos(sTRANSPORT + '_',pd^.Name) = 1 ) then begin
            if ( Length(strName) > 0 ) then begin
              strTransportBuffer := Format('%s%s=%s;',[strTransportBuffer,strName,pd^.Data]);
            end;
          end;
          pd := pd^.Next;
        end;
        if not IsStrEmpty(strFormatBuffer) then begin
          Delete(strFormatBuffer,Length(strFormatBuffer),1);
          FOperationsProperties.Values[opd^.Name + '_' + sFORMAT] := strFormatBuffer;
        end;
        if not IsStrEmpty(strTransportBuffer) then begin
          Delete(strTransportBuffer,Length(strTransportBuffer),1);
          FOperationsProperties.Values[opd^.Name + '_' + sTRANSPORT] := strTransportBuffer;
        end;
        Inc(opd);
      end;
    finally
      mm.ClearServiceMetadata(sd);
    end;
  end;
end;

function TBaseProxy.GetTarget(): String;
begin
  Result := FTarget;
end;

function TBaseProxy.GetSerializer(): IFormatterClient;
begin
  Result := FProtocol.GetSerializer();
end;

function TBaseProxy.GetCallHandler(): ICallMaker;
begin
  Result := FProtocol.GetCallHandler();
end;

function TBaseProxy.GetTransport(): ITransport;
begin
  Result := FProtocol.GetTransport();
end;

procedure TBaseProxy.SetTransport(AValue : ITransport); 
begin
  FProtocol.SetTransport(AValue);
end;

procedure TBaseProxy.MakeCall();
var
  trans : ITransport;
  frmt : IFormatterClient;
  
  procedure PrepareCall();
  var
    strBuffer, strName : string;
  begin
    LoadProperties();
    strName := frmt.GetCallProcedureName() + '_';
    strBuffer := FOperationsProperties.Values[strName + sTRANSPORT];
    if not IsStrEmpty(strBuffer) then
      trans.GetPropertyManager().SetProperties(strBuffer);
    strBuffer := FOperationsProperties.Values[strName + sFORMAT];
    if not IsStrEmpty(strBuffer) then
      frmt.GetPropertyManager().SetProperties(strBuffer);
  end;
  
begin
  trans := GetTransport();
  frmt := GetSerializer();
  PrepareCall();
  GetCallHandler().MakeCall(frmt,trans);
end;

procedure TBaseProxy.AddObjectToFree(const AObject: TObject);
begin
  FCallContext.AddObjectToFree(AObject);
end;

procedure TBaseProxy.Clear();
begin
  FCallContext.Clear();
end;

function TBaseProxy.AddHeader(
  const AHeader: THeaderBlock;
  const AKeepOwnership: Boolean
): Integer;
begin
  Result := FCallContext.AddHeader(AHeader,AKeepOwnership);
end;

function TBaseProxy.AddHeader(
  const AHeader        : TBaseRemotable;
  const AKeepOwnership : Boolean;
  const AName          : string = ''
): Integer;
begin
  Result := FCallContext.AddHeader(AHeader,AKeepOwnership,AName);
end;

function TBaseProxy.GetHeaderCount(const ADirections : THeaderDirections):Integer;
begin
  Result := FCallContext.GetHeaderCount(ADirections);
end;

function TBaseProxy.GetHeader(const AIndex: Integer): THeaderBlock;
begin
  Result := FCallContext.GetHeader(AIndex);
end;

procedure TBaseProxy.ClearHeaders(const ADirection: THeaderDirection);
begin
  FCallContext.ClearHeaders(ADirection);
end;

function TBaseProxy.GetPropertyManager(): IPropertyManager;
begin
  Result := FCallContext.GetPropertyManager();
end;

constructor TBaseProxy.Create(
  const ATarget   : String;
  const AProtocol : IServiceProtocol
);
begin
  if ( AProtocol = nil ) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['AProtocol']);
  if ( AProtocol.GetCallHandler() = nil ) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['AProtocol.GetCallHandler()']);
  if ( AProtocol.GetSerializer() = nil ) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['AProtocol.GetSerializer()']);
  if ( AProtocol.GetTransport() = nil ) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['AProtocol.GetTransport()']);

  FCallContext := TSimpleCallContext.Create();
  FTarget := ATarget;
  FProtocol := AProtocol;
  FProtocol.GetSerializer().GetPropertyManager().SetProperty(sTARGET,FTarget);
  FProtocol.GetCallHandler().GetPropertyManager().SetProperty(sTARGET,FTarget);
end;

constructor TBaseProxy.Create(
  const ATarget: string;
  const AProtocolData: string;
  const ATransportData: string
);
var
  ptcl : IServiceProtocol;
  tmpTrprt : ITransport;
begin
  if not GetFormaterRegistry().Find(AProtocolData,ptcl) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['AProtocolData']);
  if not GetTransportRegistry().Find(ATransportData,tmpTrprt) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameter,['ATransportData']);

  ptcl.SetTransport(tmpTrprt);
  Create(ATarget,ptcl);
end;

destructor TBaseProxy.Destroy();
begin
  FProtocol := Nil;
  FreeAndNil(FOperationsProperties);
  inherited Destroy();
end;

Const PROTOCOL_SEPARATOR = ':';
function ExtractProtocol( Const AProtocolName : string):String;
Var
  i : Integer;
begin
  i := Pos(PROTOCOL_SEPARATOR,AProtocolName);
  If ( i <= 0 ) Then
    i := MaxInt;
  Result := lowercase(Copy(AProtocolName,1,Pred(i)));
end;

function ExtractProtocolData(Const AProtocolPropsStr : string):String;
Var
  i : Integer;
begin
  i := Pos(PROTOCOL_SEPARATOR,AProtocolPropsStr);
  If ( i <= 0 ) Then
    i := 0;
  Result := Copy(AProtocolPropsStr,Succ(i),MaxInt);
end;

Type

  { TFormatterFactoryRegistryItem }

  { TServiceProtocol }

  TServiceProtocol = class(TInterfacedObject,IServiceProtocol)
  Private
    FFormatter     : IFormatterClient;
    FCallHandler  : ICallMaker;
    FTransport  : ITransport;
  Protected
    function GetSerializer()  : IFormatterClient;
    function GetCallHandler() : ICallMaker;
    function GetTransport()   : ITransport;
      procedure SetTransport(AValue : ITransport);
  Public
    constructor Create(
      AFormatter     : IFormatterClient;
      ACallHandler  : ICallMaker
    );
    destructor Destroy();override;
  End;

  TFormatterFactoryRegistryItem = class
  private
    FCallHandlerFactory: IItemFactory;
    FFormaterFactory: IItemFactory;
    FProtocolName: string;
  public
    constructor Create(
      Const AProtocolName        : string;
            AFormaterFactory     : IItemFactory;
            ACallHandlerFactory  : IItemFactory
    );
    destructor Destroy();override;
    property ProtocolName        : string Read FProtocolName;
    property FormaterFactory     : IItemFactory Read FFormaterFactory;
    property CallHandlerFactory : IItemFactory Read FCallHandlerFactory;
  End;

  { TFormatterRegistry }
  //Make it Threadsafe ???
  TFormatterRegistry = class(TInterfacedObject,IFormaterQueryRegistry)
  private
    FList : TObjectList;
    function IndexOf(Const AName : string ):Integer;
    //procedure Clear();
    function GetCount():Integer;
    function GetItem(const AIndex:Integer): TFormatterFactoryRegistryItem;
  protected
    function Find(
      Const AProtocolData : string;
        Out ARes          : IServiceProtocol
    ):Boolean;
    procedure Register(
      Const AProtocolName        : string;
            AFormaterFactory     : IItemFactory;
            ACallHandlerFactory  : IItemFactory
    );
  public
    constructor Create();
    destructor Destroy();override;
  End;

{ TServiceProtocol }

function TServiceProtocol.GetSerializer(): IFormatterClient;
begin
  Result := FFormatter;
end;

function TServiceProtocol.GetCallHandler(): ICallMaker;
begin
  Result := FCallHandler;
end;

function TServiceProtocol.GetTransport(): ITransport;
begin
  Result := FTransport;
end;

procedure TServiceProtocol.SetTransport(AValue: ITransport);
begin
  FTransport := AValue;
end;

constructor TServiceProtocol.Create(AFormatter: IFormatterClient;ACallHandler: ICallMaker);
begin
  FFormatter := AFormatter;
  FCallHandler := ACallHandler;
end;

destructor TServiceProtocol.Destroy();
begin
  FFormatter    := nil;
  FCallHandler  := nil;
  FTransport    := nil;
  inherited;
end;

{ TFormatterFactoryRegistryItem }

constructor TFormatterFactoryRegistryItem.Create(
  const AProtocolName: string;
  AFormaterFactory: IItemFactory; ACallHandlerFactory: IItemFactory);
begin
  FProtocolName := AProtocolName;
  FFormaterFactory := AFormaterFactory;
  FCallHandlerFactory := ACallHandlerFactory;
end;

destructor TFormatterFactoryRegistryItem.Destroy();
begin
  FFormaterFactory := nil;
  FCallHandlerFactory := nil;
  inherited Destroy();
end;

Var
  FormaterRegistryInst : IFormaterQueryRegistry = Nil;
function GetFormaterRegistry():IFormaterQueryRegistry;
begin
  If Not Assigned(FormaterRegistryInst) Then
    FormaterRegistryInst := TFormatterRegistry.Create();// Lock!!!
  Result := FormaterRegistryInst;
end;

{ TFormatterRegistry }

function TFormatterRegistry.IndexOf(const AName: string): Integer;
Var
  s : string;
begin
  s := lowercase(AName);
  
  For Result := 0 To Pred(GetCount()) Do
    If SameText(s,GetItem(Result).ProtocolName) Then
      Exit;
  Result := -1;
end;

{procedure TFormatterRegistry.Clear();
begin
  FList.Clear();
end;}

function TFormatterRegistry.GetCount(): Integer;
begin
  Result := FList.Count;
end;

function TFormatterRegistry.GetItem(const AIndex: Integer): TFormatterFactoryRegistryItem;
begin
  Result := FList[AIndex] as TFormatterFactoryRegistryItem;
end;

function TFormatterRegistry.Find(
  Const AProtocolData : string;
    Out ARes          : IServiceProtocol
): Boolean;
Var
  i : Integer;
  r : TFormatterFactoryRegistryItem;
  initData : String;
begin
  ARes := Nil;
  i := IndexOf(ExtractProtocol(AProtocolData));
  Result := ( i > -1 );
  If Result Then Begin
    initData := ExtractProtocolData(AProtocolData);
    r := GetItem(i);
    ARes := TServiceProtocol.Create(
      r.FormaterFactory.CreateInstance() as IFormatterClient,
      r.CallHandlerFactory.CreateInstance() as ICallMaker
    );
    ARes.GetSerializer().GetPropertyManager().SetProperties(initData);
    ARes.GetCallHandler().GetPropertyManager().SetProperties(initData);
  End;
end;

procedure TFormatterRegistry.Register(
  Const AProtocolName        : string;
        AFormaterFactory     : IItemFactory;
        ACallHandlerFactory  : IItemFactory
);
Var
  i : Integer;
  s : string;
begin
  Assert(Assigned(AFormaterFactory));
  Assert(Assigned(ACallHandlerFactory));
  s := ExtractProtocol(AProtocolName);
  i := IndexOf(s);
  If ( i = -1 ) Then
    FList.Add(TFormatterFactoryRegistryItem.Create(s,AFormaterFactory,ACallHandlerFactory));
end;

constructor TFormatterRegistry.Create();
begin
  FList := TObjectList.Create(True);
end;

destructor TFormatterRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

Type
  { TTransportRegistry }
  //Make it Threadsafe ???
  TTransportRegistry = class(TBaseFactoryRegistry,ITransportRegistry)
  protected
    function Find(
      Const ATransportData : string;
      Out   ARes           : ITransport
    ):Boolean;
  End;


Var
  TransportRegistryInst : ITransportRegistry = Nil;
function GetTransportRegistry():ITransportRegistry;
begin
  If Not Assigned(TransportRegistryInst) Then
    TransportRegistryInst := TTransportRegistry.Create();// Lock!!!
  Result := TransportRegistryInst;
end;

{ TTransportRegistry }

function TTransportRegistry.Find(
  const ATransportData : string;
  Out   ARes           : ITransport
): Boolean;
Var
  fct : IItemFactory;
begin
  fct := FindFactory(ExtractProtocol(ATransportData));
  If Assigned(fct) Then Begin
    ARes := fct.CreateInstance() as ITransport;
    ARes.GetPropertyManager().SetProperties(ExtractProtocolData(ATransportData));
    Result := True;
  End Else Begin
    Result := False;
  End;
end;

initialization
  TransportRegistryInst := TTransportRegistry.Create();
  FormaterRegistryInst := TFormatterRegistry.Create();
  
finalization
  FormaterRegistryInst := nil;
  TransportRegistryInst := nil;
end.
