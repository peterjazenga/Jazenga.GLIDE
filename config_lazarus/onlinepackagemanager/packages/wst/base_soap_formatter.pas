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
unit base_soap_formatter;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  {$IFDEF WST_DELPHI}xmldom, wst_delphi_xml{$ELSE}DOM{$ENDIF},
  base_service_intf;

const
  sPROTOCOL_NAME = 'SOAP';

  sXML_NS = 'xmlns';
  sXSI_NS = 'http://www.w3.org/1999/XMLSchema-instance';
  sTYPE = 'type';
  sNIL = 'nil';

  sSOAP_ENC = 'http://schemas.xmlsoap.org/soap/encoding/';
  sSOAP_ENC_ABR = 'SOAP-ENC';

  sARRAY_TYPE = 'arrayType';

  sCONTENT_TYPE = 'contenttype';
  sFORMAT = 'format';
  sSOAP_CONTENT_TYPE = 'text/xml';

  sHEADER   = 'Header';
  sENVELOPE = 'Envelope';
  sHREF     = 'href';

type

  TwstXMLDocument = {$IFDEF WST_DELPHI}wst_delphi_xml.TXMLDocument{$ELSE}TXMLDocument{$ENDIF};

  TEnumIntType = Int64;

  { ESOAPException }

  ESOAPException = class(EBaseRemoteException)
  End;

  { TStackItem }

  TStackItem = class
  private
    FAttributeFormUnqualified: Boolean;
    FElementFormUnqualified: Boolean;
    FEmbeddedScopeCount: Integer;
    FNameSpace: string;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
  protected
    function GetItemsCount : Integer;virtual;
    function GetActualNodeIfIsHRef(const ANode : TDOMNode) : TDOMNode;
  Public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    procedure SetNameSpace(const ANameSpace : string);
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property NameSpace : string Read FNameSpace;
    property ItemsCount : Integer read GetItemsCount;

    property EmbeddedScopeCount : Integer read FEmbeddedScopeCount;
    function BeginEmbeddedScope() : Integer;
    function EndEmbeddedScope() : Integer;

    function GetScopeItemNames(const AReturnList : TStrings) : Integer;virtual;

    property ElementFormUnqualified : Boolean read FElementFormUnqualified write FElementFormUnqualified;
    property AttributeFormUnqualified : Boolean read FAttributeFormUnqualified write FAttributeFormUnqualified;
  End;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    function FindNode(var ANodeName : string):TDOMNode;override;
  End;

  { TAbstractArrayStackItem }

  TAbstractArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
    FItemName : string;
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList(const ANodeName : string):TDOMNodeList;virtual;abstract;
  public
    constructor Create(
            AScopeObject : TDOMNode;
      const AScopeType   : TScopeType;
      const AItemName    : string
    );
    destructor Destroy();override;
    function FindNode(var ANodeName : string):TDOMNode;override;
  end;

  { TScopedArrayStackItem }

  TScopedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  public
    destructor Destroy();override;
  end;

  { TEmbeddedArrayStackItem }

  TEmbeddedArrayStackItem = class(TAbstractArrayStackItem)
  protected
    function CreateList(const ANodeName : string):TDOMNodeList;override;
  end;

  TSOAPEncodingStyle = ( Literal, Encoded );
  TSOAPDocumentStyle = ( RPC, Document );

{$M+}

  { TSOAPBaseFormatter }

  TSOAPBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FPropMngr : IPropertyManager;
    FContentType: string;
    FEncodingStyle: TSOAPEncodingStyle;
    FStyle: TSOAPDocumentStyle;
    FHeaderEnterCount : Integer;

    FNameSpaceCounter : Integer;
    FDoc : TwstXMLDocument;
    FStack : TObjectStack;

    FKeepedStyle : TSOAPDocumentStyle;
    FKeepedEncoding : TSOAPEncodingStyle;
    FSerializationStyle : TSerializationStyle;

    procedure InternalClear(const ACreateDoc : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}

    function NextNameSpaceCounter():Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function HasScope():Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure CheckScope();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InternalPutData(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : DOMString
    ):TDOMNode;
    function PutEnum(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutBool(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutAnsiChar(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : AnsiChar
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutWideChar(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideChar
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutInt64(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_QWORD}
    function PutUInt64(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : QWord
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_QWORD}
    function PutStr(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF WST_UNICODESTRING}
    function PutUnicodeStr(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : UnicodeString
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF WST_UNICODESTRING}
    function PutWideStr(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideString
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutFloat(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Extended
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutObj(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TObject
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutRecord(
      const AName     : string;
      const ATypeInfo : PTypeInfo;
      const AData     : Pointer
    );{$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetNodeValue(
      const ANameSpace : string;
      var AName : string;
      out AResBuffer : DOMString
    ) : Boolean;
    function GetEnum(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : TEnumIntType
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBool(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : Boolean
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetAnsiChar(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : AnsiChar
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetWideChar(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : WideChar
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    {$IFDEF FPC}
    function GetInt(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : Integer
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    {$ENDIF}
    function GetInt64(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : Int64
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_QWORD}
    function GetUInt64(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : QWord
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_QWORD}
    function GetFloat(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : Extended
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetStr(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : String
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF WST_UNICODESTRING}
    function GetUnicodeStr(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : UnicodeString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF WST_UNICODESTRING}
    function GetWideStr(
      Const ATypeInfo : PTypeInfo;
      const ANameSpace : string;
      Var   AName     : String;
      Var   AData     : WideString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetObj(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TObject
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetRecord(
      const ATypeInfo : PTypeInfo;
      var   AName     : String;
      var   AData     : Pointer
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    function GetXmlDoc():TwstXMLDocument;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(AScopeObject : TDOMNode):TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(
            AScopeObject : TDOMNode;
      const AStyle       : TArrayStyle;
      const AItemName    : string
    ):TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindAttributeByValueInNode(
      Const AAttValue : String;
      Const ANode     : TDOMNode;
      Out   AResAtt   : string
    ):boolean;
    function FindAttributeByNameInNode(
      Const AAttName     : String;
      Const ANode        : TDOMNode;
      Out   AResAttValue : string
    ):boolean;
    function FindAttributeByValueInScope(Const AAttValue : String):String;
    function FindAttributeByNameInScope(Const AAttName : String):String;
    function GetNameSpaceShortName(
      const ANameSpace        : string;
      const ACreateIfNotFound : Boolean
    ):shortstring;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function FindXMLNodeWithNamespaceInSubScope(ANameSpace, ANodeName: string): TDOMNode;
  protected
    function GetCurrentScope():String;
    function GetCurrentScopeObject():TDOMElement;
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ClearStack();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure BeginScope(
      Const AScopeName,ANameSpace : string;
      Const ANameSpaceShortName   : string ;
      Const AScopeType            : TScopeType;
      const AStyle                : TArrayStyle
    );
    function InternalBeginScopeRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AScopeType : TScopeType;
      const AStyle     : TArrayStyle;
      const AItemName  : string
    ):Integer;

    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetStyleAndEncoding(
      const AStyle : TSOAPDocumentStyle;
      const AEncoding : TSOAPEncodingStyle
    );
    procedure RestoreStyleAndEncoding();
    procedure Prepare();
    function ReadHeaders(ACallContext : ICallContext):Integer;
    function WriteHeaders(ACallContext : ICallContext):Integer;
  public
    constructor Create();override;
    destructor Destroy();override;
    function GetFormatName() : string;
    function GetPropertyManager():IPropertyManager;
    procedure Clear();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : PTypeInfo
    );
    procedure BeginArray(
      const AName         : string;
      const ATypeInfo     : PTypeInfo;
      const AItemTypeInfo : PTypeInfo;
      const ABounds       : Array Of Integer;
      const AStyle        : TArrayStyle
    );

    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    procedure AddScopeAttribute(Const AName,AValue : string);
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : PTypeInfo;
      const AStyle     : TArrayStyle;
      const AItemName  : string
    ):Integer;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;
    procedure EndScopeRead();

    procedure BeginHeader();
    procedure EndHeader();

    procedure Put(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData
    );overload;
    procedure Put(
      const ANameSpace : string;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData
    );overload;
    procedure PutScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      const AData
    );
    function Get(
      const ATypeInfo : PTypeInfo;
      var   AName     : string;
      var   AData
    ) : Boolean; overload;
    function Get(
      const ATypeInfo  : PTypeInfo;
      const ANameSpace : string;
      var   AName      : string;
      var   AData
    ) : Boolean;overload;
    procedure GetScopeInnerValue(
      const ATypeInfo : PTypeInfo;
      var   AData
    );
    function ReadBuffer(const AName : string; out AResBuffer : string) : Boolean;
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  Published
    property EncodingStyle : TSOAPEncodingStyle Read FEncodingStyle Write FEncodingStyle;
    property ContentType : string Read FContentType Write FContentType;
    property Style : TSOAPDocumentStyle Read FStyle Write FStyle;
  End;
{$M-}


  function BoolToSoapBool(const AValue : Boolean) : string;{$IFDEF USE_INLINE}inline;{$ENDIF}

  
implementation
uses 
{$IFDEF WST_DELPHI}
  XMLDoc,XMLIntf,
{$ENDIF WST_DELPHI}
{$IFDEF FPC}
  XMLWrite, XMLRead,wst_fpc_xml,
{$ENDIF FPC}
  StrUtils, imp_utils, wst_consts;


function BoolToSoapBool(const AValue : Boolean) : string;
begin
  if AValue then
    Result := 'true'
  else
    Result := 'false';
end;

{ TStackItem }

function TStackItem.GetItemsCount: Integer;
begin
  Result := GetNodeItemsCount(ScopeObject);
end;

function TStackItem.GetActualNodeIfIsHRef(const ANode: TDOMNode): TDOMNode;
var
  locAttrs : TDOMNamedNodeMap;

  function FollowIfNeeded() : TDOMNode;
  var
    locNode : TDOMNode;
    locHRefValue : DOMString;
  begin
    locNode := locAttrs.GetNamedItem(sHREF);
    if ( locNode = nil ) or ( Length(locNode.NodeValue) = 0 ) then begin
      Result := ANode;
    end else begin
      locHRefValue := locNode.NodeValue;
      if ( locHRefValue[1] = '#' ) then
        locHRefValue := Copy(locHRefValue,2,Length(locHRefValue));
      Result := SelectSingleNode(Format('//*[@id=%s]',[QuotedStr(locHRefValue)]),locNode.OwnerDocument,True);
      //ANode.OwnerDocument.GetElementById(locHRefValue);
      if ( Result = nil ) then
        raise ESOAPException.CreateFmt(SERR_NodeNotFoundByID,[locHRefValue]);
    end;
  end;

begin
  if ( ANode = nil ) then begin
    Result := nil;
  end else begin
    locAttrs := ANode.Attributes;
    if ( locAttrs <> nil ) and ( locAttrs.Length > 0 ) then
      Result := FollowIfNeeded()
    else
      Result := ANode;
  end;
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

procedure TStackItem.SetNameSpace(const ANameSpace: string);
begin
  FNameSpace := ANameSpace;
end;

function TStackItem.BeginEmbeddedScope(): Integer;
begin
  Inc(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;

function TStackItem.EndEmbeddedScope(): Integer;
begin
  if ( FEmbeddedScopeCount < 1 ) then begin
    raise Exception.Create(SERR_InvalidEmbeddedScopeOperation);
  end;
  Dec(FEmbeddedScopeCount);
  Result := FEmbeddedScopeCount;
end;

function TStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  i : Integer;
begin
  AReturnList.Clear();
  for i := 0 to Pred(GetItemsCount()) do begin
    AReturnList.Add(ScopeObject.childNodes.Item[i].nodeName);
  end;
  Result := AReturnList.Count;
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
{$IFDEF WST_DELPHI}
  Result := wst_delphi_xml.FindNode(ScopeObject,ANodeName);
{$ELSE}
  Result := ScopeObject.FindNode(ANodeName);
{$ENDIF}
  Result := GetActualNodeIfIsHRef(Result);
end;

{ TAbstractArrayStackItem }

procedure TAbstractArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList(FItemName);
  end;
end;

function TAbstractArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

constructor TAbstractArrayStackItem.Create(
        AScopeObject : TDOMNode;
  const AScopeType   : TScopeType;
  const AItemName    : string
);
begin
  inherited Create(AScopeObject,AScopeType);
  FItemName := AItemName;
end;

destructor TAbstractArrayStackItem.Destroy();
begin
  if Assigned(FItemList) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TAbstractArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise ESOAPException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  Inc(FIndex);
  ANodeName := Result.NodeName;
  Result := GetActualNodeIfIsHRef(Result);
end;

{ TSOAPBaseFormatter }

procedure TSOAPBaseFormatter.ClearStack();
begin
  while HasScope() do begin
    EndScope();
  end;
end;

function TSOAPBaseFormatter.PushStack(AScopeObject : TDOMNode) : TStackItem;
begin
  Result := FStack.Push(TObjectStackItem.Create(AScopeObject,stObject)) as TStackItem;
end;

function TSOAPBaseFormatter.PushStack(
        AScopeObject : TDOMNode;
  const AStyle       : TArrayStyle;
  const AItemName    : string
): TStackItem;
begin
  case AStyle of
    asScoped  : Result := FStack.Push(TScopedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    asEmbeded : Result := FStack.Push(TEmbeddedArrayStackItem.Create(AScopeObject,stArray,AItemName)) as TStackItem;
    else begin
      Assert(False);
      Result := nil;
    end;
  end;
end;

function TSOAPBaseFormatter.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stObject,asNone,'');
end;

function TSOAPBaseFormatter.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stArray,AStyle,AItemName);
end;

function TSOAPBaseFormatter.GetScopeItemNames(const AReturnList : TStrings) : Integer;
begin
  CheckScope();
  Result := StackTop().GetScopeItemNames(AReturnList);
end;

procedure TSOAPBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TSOAPBaseFormatter.BeginHeader();
begin
  if ( FHeaderEnterCount <= 0 ) then begin
    Inc(FHeaderEnterCount);
    Prepare();
    SetStyleAndEncoding(Document,Literal);
    BeginScope(sHEADER,sSOAP_ENV,sSOAP_ENV_ABR,stObject,asNone);
  end;
end;

procedure TSOAPBaseFormatter.EndHeader();
begin
  if ( FHeaderEnterCount > 0 ) then begin
    Dec(FHeaderEnterCount);
    RestoreStyleAndEncoding();
    EndScope();
  end;
end;

procedure TSOAPBaseFormatter.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  ReleaseDomNode(FDoc);
  FDoc := nil;
  if ACreateDoc then
    FDoc := CreateDoc();
end;

function TSOAPBaseFormatter.NextNameSpaceCounter(): Integer;
begin
  Inc(FNameSpaceCounter);
  Result := FNameSpaceCounter;
end;

function TSOAPBaseFormatter.HasScope(): Boolean;
begin
  Result := FStack.AtLeast(1);
end;

function TSOAPBaseFormatter.FindAttributeByValueInNode(
  Const AAttValue : String;
  Const ANode     : TDOMNode;
  Out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  if Assigned(ANode) and
     Assigned(ANode.Attributes) and
     ( ANode.Attributes.Length > 0 )
  then begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  end;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByNameInNode(
  const AAttName: String;
  const ANode: TDOMNode;
  Out AResAttValue: string
): boolean;
var
  i,c : Integer;
begin
  AResAttValue := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := ANode.Attributes.Length;
    if ( c > 0 ) then begin
      Dec(c);
      For i := 0 To c Do Begin
        If AnsiSameText(AAttName,ANode.Attributes.Item[i].NodeName) Then Begin
          AResAttValue := ANode.Attributes.Item[i].NodeValue;
          Result := True;
          Exit;
        End;
      End;
    end;
  End;
  Result := False;
end;

function TSOAPBaseFormatter.FindAttributeByValueInScope(const AAttValue: String): String;
Var
  tmpNode : TDOMNode;
begin
  If HasScope() Then Begin
    tmpNode := GetCurrentScopeObject();
    While Assigned(tmpNode) Do Begin
      If FindAttributeByValueInNode(AAttValue,tmpNode,Result) Then
        Exit;
      tmpNode := tmpNode.ParentNode;
    End;
  End;
  Result := '';
end;

function TSOAPBaseFormatter.FindAttributeByNameInScope(const AAttName: String): String;
var
  tmpNode : TDOMNode;
begin
  if HasScope() then begin
    tmpNode := GetCurrentScopeObject();
    while Assigned(tmpNode) do begin
      if FindAttributeByNameInNode(AAttName,tmpNode,Result) then
        Exit;
      tmpNode := tmpNode.ParentNode;
    end;
  end;
  Result := '';
end;

function TSOAPBaseFormatter.GetNameSpaceShortName(
  const ANameSpace        : string;
  const ACreateIfNotFound : Boolean
): shortstring;
begin
  Result := FindAttributeByValueInScope(ANameSpace);
  if IsStrEmpty(Result) then begin
    if ACreateIfNotFound then begin
      Result := 'ns' + IntToStr(NextNameSpaceCounter());
      AddScopeAttribute('xmlns:'+Result, ANameSpace);
    end;
  end else begin
    Result := Copy(Result,Length('xmlns:')+1,MaxInt);
  end;
end;

procedure TSOAPBaseFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error(SERR_NoScope);
end;

function ExtractNameSpaceShortName(const ANameSpaceDeclaration : string):string;
var
  i : integer;
begin
  i := AnsiPos(sXML_NS,ANameSpaceDeclaration);
  if ( i > 0 ) then begin
    Result := Copy(ANameSpaceDeclaration, (i + Length(sXML_NS) + 1 ), MaxInt );
  end else begin
    Result := '';
  end;
end;
function TSOAPBaseFormatter.FindXMLNodeWithNamespaceInSubScope(
  ANameSpace,
  ANodeName  : string
) : TDOMNode;

  function ScanNode(ANode: TDOMNode): TDOMNode;
  var
    AttrName : string;
  begin
    Result := nil;
    if FindAttributeByValueInNode(ANameSpace, ANode, AttrName) then begin
      if not IsStrEmpty(AttrName) then begin
        AttrName := ExtractNameSpaceShortName(AttrName);
        if IsStrEmpty(AttrName) then begin
          if (ANode.NodeName = ANodeName) then
            Result := ANode;
        end else begin
          if(ANode.NodeName = AttrName + ':' + ANodeName) then
            Result := ANode;
        end;
      end;
    end;
  end;

var
  locNode : TDOMNode;
begin
  locNode := GetCurrentScopeObject();
  Result := ScanNode(locNode);
  if (Result <> nil) or not(locNode.HasChildNodes) then
    exit;
  locNode := locNode.FirstChild;
  while (locNode <> nil) do begin
    Result := ScanNode(locNode);
    if (Result <> nil) then
      Break;
    locNode := locNode.NextSibling;
  end;
end;

function TSOAPBaseFormatter.InternalPutData(
  const ANameSpace : string;
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : DOMString
): TDOMNode;
Var
  namespaceLongName, namespaceShortName, strName, strNodeName, s : string;
  regItem : TTypeRegistryItem;
begin
  strNodeName := AName;
  if (Style = Document) and
     (ANameSpace <> '')
     {( ( (FSerializationStyle = ssNodeSerialization) and not(StackTop().ElementFormUnqualified) ) or
       ( (FSerializationStyle = ssAttibuteSerialization) and not(StackTop().AttributeFormUnqualified))
     )}
  then begin
    namespaceLongName := ANameSpace;
    if ( namespaceLongName <> '' ) then begin
      s := FindAttributeByValueInScope(namespaceLongName);
      if IsStrEmpty(s) then begin
        namespaceShortName := 'ns' + IntToStr(NextNameSpaceCounter());
        AddScopeAttribute('xmlns:'+namespaceShortName, namespaceLongName);
        strNodeName := namespaceShortName + ':' + strNodeName;
      end else begin
        s := ExtractNameSpaceShortName(s);
        if not IsStrEmpty(s) then
          strNodeName := s + ':' + strNodeName;
      end;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    Result := FDoc.CreateElement(strNodeName);
    Result.AppendChild(FDoc.CreateTextNode(AData));
    GetCurrentScopeObject().AppendChild(Result);
    If ( EncodingStyle = Encoded ) Then Begin
      regItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
      strName := regItem.DeclaredName;
      namespaceLongName := regItem.NameSpace;
      If Not IsStrEmpty(namespaceLongName) Then Begin
        namespaceShortName := FindAttributeByValueInScope(namespaceLongName);
        If IsStrEmpty(namespaceShortName) Then Begin
          namespaceShortName := Format('ns%d',[NextNameSpaceCounter()]);
          AddScopeAttribute(sXML_NS + ':'+namespaceShortName,namespaceLongName);
        End Else Begin
          namespaceShortName := ExtractNameSpaceShortName(namespaceShortName);//Copy(namespaceShortName,AnsiPos(':',namespaceShortName) + 1,MaxInt);
        End;
        strName := Format('%s:%s',[namespaceShortName,strName])
      End;
      namespaceShortName := GetNameSpaceShortName(sXSI_NS,True);
      if not IsStrEmpty(namespaceShortName) then
        namespaceShortName := namespaceShortName + ':';
      (Result As TDOMElement).SetAttribute(namespaceShortName + sTYPE,strName);
    End;
  end else begin
    Result := GetCurrentScopeObject();
    (Result as TDOMElement).SetAttribute(strNodeName,AData);
  end;
end;

function TSOAPBaseFormatter.PutEnum(
  const ANameSpace : string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumIntType
): TDOMNode;
begin
  Result := InternalPutData(
              ANameSpace,
              AName,
              ATypeInfo,
              GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,AData))
            );
end;

function TSOAPBaseFormatter.PutBool(
  const ANameSpace : string;
  const AName     : String;
  const ATypeInfo : PTypeInfo;
  const AData     : Boolean
): TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,BoolToSoapBool(AData));
end;

function TSOAPBaseFormatter.PutAnsiChar(
  const ANameSpace: string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: AnsiChar
) : TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,AData);
end;

function TSOAPBaseFormatter.PutWideChar(
  const ANameSpace: string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideChar
): TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,AData);
end;

function TSOAPBaseFormatter.PutInt64(
  const ANameSpace : string;
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Int64
): TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,IntToStr(AData));
end;

{$IFDEF HAS_QWORD}
function TSOAPBaseFormatter.PutUInt64(
  const ANameSpace : string;
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : QWord
): TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,IntToStr(AData));
end;
{$ENDIF HAS_QWORD}

function TSOAPBaseFormatter.PutStr(
  const ANameSpace : string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: String
):TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,AData);
end;

{$IFDEF WST_UNICODESTRING}
function TSOAPBaseFormatter.PutUnicodeStr(
  const ANameSpace: string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: UnicodeString
): TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,AData);
end;
{$ENDIF WST_UNICODESTRING}

function TSOAPBaseFormatter.PutWideStr(
  const ANameSpace: string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideString
) : TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,AData);
end;

procedure TSOAPBaseFormatter.PutObj(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

procedure TSOAPBaseFormatter.PutRecord(
  const AName     : string;
  const ATypeInfo : PTypeInfo;
  const AData     : Pointer
);
begin
  TRemotableRecordEncoder.Save(AData,Self,AName,ATypeInfo);
end;

function TSOAPBaseFormatter.PutFloat(
  const ANameSpace : string;
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
):TDOMNode;
begin
  Result := InternalPutData(ANameSpace,AName,ATypeInfo,wst_FormatFloat(ATypeInfo,AData));
end;

function TSOAPBaseFormatter.GetNodeValue(
  const ANameSpace : string;
  var AName : string;
  out AResBuffer : DOMString
): Boolean;
var
  locElt : TDOMNode;
  namespaceShortName, strNodeName, s : string;
begin
  strNodeName := AName;
  if (Style = Document) and
     (ANameSpace <> '')
     {( not(HasScope()) or
       ( ( (FSerializationStyle = ssNodeSerialization) and not(StackTop().ElementFormUnqualified) ) or
         ( (FSerializationStyle = ssAttibuteSerialization) and not(StackTop().AttributeFormUnqualified))
       )
     )}
  then begin
    if ( ANameSpace <> '' ) then begin
      {if ( ANameSpace = '' ) then
        s := StackTop().NameSpace
      else
        s := ANameSpace;}
      s := ANameSpace;
      namespaceShortName := FindAttributeByValueInScope(s);
      if not IsStrEmpty(namespaceShortName) then begin
        s := ExtractNameSpaceShortName(namespaceShortName);
        if not IsStrEmpty(s) then
          strNodeName := s + ':' + strNodeName;
      end;
    end;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName) As TDOMElement;
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;
  if (locElt = nil) and (Style = Document) then
    locElt := FindXMLNodeWithNamespaceInSubScope(ANameSpace,AName);

  Result := ( locElt <> nil );
  if Result then begin
    if locElt.HasChildNodes then
      AResBuffer := locElt.FirstChild.NodeValue
    else
      AResBuffer := locElt.NodeValue;
  end;
end;

function TSOAPBaseFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  const ANameSpace : string;
  var AName: String;
  var AData: TEnumIntType
) : Boolean;
Var
  locBuffer : DOMString;
  locStrBuffer : String;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then begin
    locStrBuffer := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetInternalPropertyName(locBuffer);
    If IsStrEmpty(locStrBuffer) Then
      AData := 0
    Else
      AData := GetEnumValue(ATypeInfo,locStrBuffer)
  end;
End;

function TSOAPBaseFormatter.GetBool(
  const ATypeInfo  : PTypeInfo;
  const ANameSpace : string;
  var   AName      : String;
  var   AData      : Boolean
) : Boolean;
Var
  locBuffer : DOMString;
  locStrBuffer : String;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then begin
    locStrBuffer := LowerCase(Trim(locBuffer));
    If IsStrEmpty(locStrBuffer) Then
      AData := False
    Else
      AData := StrToBool(locStrBuffer);
  end;
end;

function TSOAPBaseFormatter.GetAnsiChar(
  const ATypeInfo: PTypeInfo;
  const ANameSpace: string;
  var   AName: String;
  var   AData: AnsiChar
) : Boolean;
var
  tmpString : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,tmpString);
  if Result then begin
    if ( Length(tmpString) > 0 ) then
      AData := AnsiChar(tmpString[1])
    else
      AData := #0;
  end;
end;

function TSOAPBaseFormatter.GetWideChar(
  const ATypeInfo: PTypeInfo;
  const ANameSpace: string;
  var AName: String;
  var AData: WideChar
) : Boolean;
var
  tmpString : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,tmpString);
  if Result then begin
    if ( Length(tmpString) > 0 ) then
      AData := tmpString[1]
    else
      AData := #0;
  end;
end;

{$IFDEF FPC}
function TSOAPBaseFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  const ANameSpace : string;
  var   AName: String;
  var   AData: Integer
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := StrToIntDef(Trim(locBuffer),0);
end;
{$ENDIF}

function TSOAPBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var   AName     : String;
  var   AData     : Int64
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData :=StrToInt64Def(Trim(locBuffer),0);
end;

{$IFDEF HAS_QWORD}
function TSOAPBaseFormatter.GetUInt64(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var   AName     : String;
  var   AData     : QWord
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := StrToQWordDef(Trim(locBuffer),0);
end;
{$ENDIF HAS_QWORD}

function TSOAPBaseFormatter.GetFloat(
  const ATypeInfo  : PTypeInfo;
  const ANameSpace : string;
  var AName        : String;
  var AData        : Extended
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then begin
{$IFDEF HAS_FORMAT_SETTINGS}
    AData := StrToFloatDef(Trim(locBuffer),0,wst_FormatSettings);
{$ELSE}
    AData := StrToFloatDef(TranslateDotToDecimalSeperator(Trim(locBuffer)),0);
{$ENDIF HAS_FORMAT_SETTINGS}
  end;
end;

function TSOAPBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var   AName     : String;
  var   AData     : String
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := locBuffer;
end;

{$IFDEF WST_UNICODESTRING}
function TSOAPBaseFormatter.GetUnicodeStr(
  const ATypeInfo: PTypeInfo;
  const ANameSpace: string;
  var   AName: String;
  var   AData: UnicodeString
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := locBuffer;
end;
{$ENDIF WST_UNICODESTRING}

function TSOAPBaseFormatter.GetWideStr(
  const ATypeInfo: PTypeInfo;
  const ANameSpace: string;
  var   AName: String;
  var   AData: WideString
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(ANameSpace,AName,locBuffer);
  if Result then
    AData := locBuffer;
end;

function TSOAPBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TObject
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TSOAPBaseFormatter.GetRecord(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Pointer
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TRemotableRecordEncoder.Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TSOAPBaseFormatter.GetXmlDoc(): TwstXMLDocument;
begin
  Result := FDoc;
end;

function TSOAPBaseFormatter.GetCurrentScope(): String;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TSOAPBaseFormatter.GetCurrentScopeObject(): TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TSOAPBaseFormatter.StackTop(): TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TSOAPBaseFormatter.PopStack(): TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

constructor TSOAPBaseFormatter.Create();
begin
  Inherited Create();
  FContentType := sSOAP_CONTENT_TYPE;
  FStack := TObjectStack.Create();
  FDoc := CreateDoc();
end;

destructor TSOAPBaseFormatter.Destroy();
begin
  ClearStack();
  ReleaseDomNode(FDoc);
  FStack.Free();
  inherited Destroy();
end;

procedure TSOAPBaseFormatter.Clear();
begin
  InternalClear(True);
end;

procedure TSOAPBaseFormatter.BeginObject(
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH, xsiNmspcSH : string;
  mustAddAtt : Boolean;
  strNodeName : string;
begin
  typData := GetTypeRegistry().Find(ATypeInfo,False);
  If Not Assigned(typData) Then
    Error(SERR_TypeNotRegistered,[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
  mustAddAtt := False;
  if ( ATypeInfo^.Kind = tkClass ) and
     GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TAbstractSimpleRemotable) and
     HasScope()
  then
    nmspc := StackTop().NameSpace
  else
    nmspc := typData.NameSpace;
  If IsStrEmpty(nmspc) Then
    nmspcSH := 'tns'
  Else Begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    If IsStrEmpty(nmspcSH) Then Begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      If HasScope() Then
        AddScopeAttribute('xmlns:'+nmspcSH, nmspc)
      Else Begin
        mustAddAtt := True;
      End;
    End Else Begin
      nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
    End;
  End;

  if not(HasScope()) or
     ( (Style = Document) and
       not(StackTop().ElementFormUnqualified)
     )
  then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  BeginScope(strNodeName,'','',stObject,asNone);
  If mustAddAtt Then
    AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
  if ( EncodingStyle = Encoded ) then begin
    xsiNmspcSH := GetNameSpaceShortName(sXSI_NS,True);
    if not IsStrEmpty(xsiNmspcSH) then
      xsiNmspcSH := xsiNmspcSH + ':';
    AddScopeAttribute(xsiNmspcSH + sTYPE,Format('%s:%s',[GetNameSpaceShortName(typData.NameSpace,True),typData.DeclaredName]));
  end;
  StackTop().SetNameSpace(nmspc);
  StackTop().ElementFormUnqualified := trioUnqualifiedElement in typData.Options;
  StackTop().AttributeFormUnqualified := not(trioQualifiedAttribute in typData.Options);
end;

procedure TSOAPBaseFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
Var
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  i,j, k : Integer;
  strNodeName : string;
  xsiNmspcSH : string;
begin
  if ( Length(ABounds) < 2 ) then begin
    Error(SERR_InvalidArrayBounds);
  end;
  i := ABounds[0];
  j := ABounds[1];
  k := j - i + 1;
  if ( k < 0 ) then begin
    Error(SERR_InvalidArrayBounds);
  end;
  typData := GetTypeRegistry().Find(ATypeInfo,False);
  if not Assigned(typData) then begin
    Error(SERR_TypeNotRegistered,[ATypeInfo^.Name]);
  end;
  nmspc := typData.NameSpace;
  if IsStrEmpty(nmspc) then begin
    nmspcSH := 'tns'
  end else begin
    nmspcSH := FindAttributeByValueInScope(nmspc);
    if IsStrEmpty(nmspcSH) then begin
      nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
      AddScopeAttribute('xmlns:'+nmspcSH, nmspc);
    end else begin
      nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
    end;
  end;

  if ( Style = Document ) then begin
    strNodeName := nmspcSH + ':' + AName;
  end else begin
    strNodeName := AName;
  end;

  //if ( AStyle = asScoped ) then begin
    BeginScope(strNodeName,'','',stArray,AStyle);
  //end;

  if ( EncodingStyle = Encoded ) then begin
    //AddScopeAttribute(sXSI_TYPE,nmspc);
    //SOAP-ENC:arrayType="xsd:int[2]"
    {AddScopeAttribute(
      Format('%s:%s',[sSOAP_ENC_ABR,sARRAY_TYPE]) ,
      Format('%s:%s[%d]',[nmspcSH,typData.DeclaredName,k])
    );}
    xsiNmspcSH := GetNameSpaceShortName(sXSI_NS,True);
    if not IsStrEmpty(xsiNmspcSH) then
      xsiNmspcSH := xsiNmspcSH + ':';
    AddScopeAttribute(xsiNmspcSH + sTYPE,Format('%s:%s',[nmspcSH,typData.DeclaredName]));
  end;
  StackTop().SetNameSpace(nmspc);
end;

procedure TSOAPBaseFormatter.NilCurrentScope();
var
  nmspcSH : shortstring;
begin
  CheckScope();
  nmspcSH := FindAttributeByValueInScope(sXSI_NS);
  if IsStrEmpty(nmspcSH) then begin
    nmspcSH := 'ns' + IntToStr(NextNameSpaceCounter());
    AddScopeAttribute('xmlns:'+nmspcSH, sXSI_NS);
  end else begin
    nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
  end;
  GetCurrentScopeObject().SetAttribute(nmspcSH + ':' + sNIL,'true');
end;

function TSOAPBaseFormatter.IsCurrentScopeNil(): Boolean;
Var
  s,nsShortName,nilName : shortstring;
begin
  CheckScope();
  nsShortName := FindAttributeByValueInScope(sXSI_NS);
  Result := False;
  if IsStrEmpty(nsShortName) then begin
    nilName := 'nil';
  end else begin
    nsShortName := Copy(nsShortName,1 + Pos(':',nsShortName),MaxInt);
    if not IsStrEmpty(nsShortName) Then
      nsShortName := nsShortName + ':';
    nilName := nsShortName + 'nil';
  end;
  s := Trim(GetCurrentScopeObject().GetAttribute(nilName));
  if ( Length(s) > 0 ) and ( AnsiSameText(s,'true') or AnsiSameText(s,'"true"') ) then begin
    Result := True;
  end;
end;

procedure TSOAPBaseFormatter.BeginScope(
  Const AScopeName,ANameSpace : string;
  Const ANameSpaceShortName   : string;
  Const AScopeType            : TScopeType;
  const AStyle                : TArrayStyle
);
Var
  nsStr, scpStr : String;
  e : TDOMElement;
  hasNmspc, addAtt : Boolean;
begin
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    addAtt := False;
    scpStr := AScopeName;
    hasNmspc := Not IsStrEmpty(ANameSpace);
    If hasNmspc Then Begin
      nsStr := FindAttributeByValueInScope(ANameSpace);
      addAtt := IsStrEmpty(nsStr);
      If addAtt Then Begin
        If IsStrEmpty(ANameSpaceShortName) Then
          nsStr := 'ns' + IntToStr(NextNameSpaceCounter())
        Else
          nsStr := Trim(ANameSpaceShortName);
      End Else Begin
        nsStr := Copy(nsStr,Succ(AnsiPos(':',nsStr)),MaxInt);
      End;
      if not(HasScope()) or
         ( (Style = Document) {and
           not(StackTop().ElementFormUnqualified) }
         )
      then begin
        scpStr := nsStr + ':' + scpStr;
      end;
    End;

    e := FDoc.CreateElement(scpStr);
    If HasScope() Then
      GetCurrentScopeObject().AppendChild(e)
    Else
      FDoc.AppendChild(e);
    if ( AScopeType = stObject ) then begin
      PushStack(e);
    end else begin
      PushStack(e,AStyle,'');
    end;
    if hasNmspc and addAtt then begin
      e.SetAttribute('xmlns:'+nsStr,ANameSpace);
      StackTop().SetNameSpace(ANameSpace);
    end;
  end else if ( ( AScopeType = stArray ) and ( AStyle = asEmbeded ) ) then begin
    StackTop().BeginEmbeddedScope();
  end;
end;

function TSOAPBaseFormatter.InternalBeginScopeRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AScopeType : TScopeType;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
  typData : TTypeRegistryItem;
  nmspc,nmspcSH : string;
  strNodeName : string;
begin
  nmspcSH := '';
  strNodeName := AScopeName;
  if ( Style = Document ) then begin
    typData := GetTypeRegistry().Find(ATypeInfo,False);
    if not Assigned(typData) then begin
      Error(SERR_TypeNotRegistered,[IfThen(Assigned(ATypeInfo),ATypeInfo^.Name,'')]);
    end;
    if ( ATypeInfo^.Kind = tkClass ) and
       GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TAbstractSimpleRemotable) and
       HasScope()
    then
      nmspc := StackTop().NameSpace
    else
      nmspc := typData.NameSpace;
    if IsStrEmpty(nmspc) then begin
      nmspcSH := ''
    end else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if not IsStrEmpty(nmspcSH) then begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    end;
    {if IsStrEmpty(nmspcSH) then begin
      strNodeName := AScopeName
    end else begin
      if ( Pos(':',AScopeName) < 1 ) then begin
        strNodeName := nmspcSH + ':' + AScopeName
      end else begin
        strNodeName := AScopeName;
      end;
    end;}
    if not(IsStrEmpty(nmspcSH)) and
       (  not(HasScope()) or
          not(StackTop().ElementFormUnqualified)
       )
    then begin
      if ( Pos(':',AScopeName) < 1 ) then
        strNodeName := nmspcSH + ':' + AScopeName;
    end;
  end;

  stk := StackTop();
  if ( AScopeType = stObject ) or
     ( ( AScopeType = stArray ) and ( AStyle = asScoped ) )
  then begin
    locNode := stk.FindNode(strNodeName);
    if (locNode = nil) and (Style = Document) then
      locNode := FindXMLNodeWithNamespaceInSubScope(nmspc,AScopeName);
  end else begin
    locNode := stk.ScopeObject;
  end;

  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    if ( AScopeType = stObject ) then begin
      PushStack(locNode);
    end else begin
      PushStack(locNode,AStyle,AItemName);
    end;
    if ( Style = Document ) then begin
      StackTop().SetNameSpace(nmspc);
      if (AScopeType = stObject) or
         ( (AScopeType = stArray) and (AStyle = asScoped) )
      then begin
        StackTop().ElementFormUnqualified := trioUnqualifiedElement in typData.Options;
        StackTop().AttributeFormUnqualified := not(trioQualifiedAttribute in typData.Options);
      end;
    end;
    Result := StackTop().GetItemsCount();
    if ( Result = 0 ) and ( AScopeType = stArray ) then begin
      PopStack().Free();
      Result := -1;
    end;
  end;
end;

procedure TSOAPBaseFormatter.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TSOAPBaseFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

procedure TSOAPBaseFormatter.SetStyleAndEncoding(
  const AStyle: TSOAPDocumentStyle;
  const AEncoding: TSOAPEncodingStyle
);
begin
  FKeepedStyle := Style;
  FKeepedEncoding := EncodingStyle;
  Style := AStyle;
  EncodingStyle := AEncoding;
end;

procedure TSOAPBaseFormatter.RestoreStyleAndEncoding();
begin
  EncodingStyle := FKeepedEncoding;
  Style := FKeepedStyle;
end;

procedure TSOAPBaseFormatter.Prepare();
var
  locDoc : TwstXMLDocument;
begin
  locDoc := GetXmlDoc();
  if Assigned(locDoc.DocumentElement) and
     AnsiSameText(locDoc.DocumentElement.NodeName,( sSOAP_ENV_ABR + ':' + sENVELOPE ))
  then begin
    ClearStack();
    PushStack(locDoc.DocumentElement);
  end else begin
    BeginScope(sENVELOPE,sSOAP_ENV,sSOAP_ENV_ABR,stObject,asNone);
      AddScopeAttribute('xmlns:xsi',sXSI_NS);
      AddScopeAttribute('xmlns:'+sXSD, sXSD_NS);
      AddScopeAttribute('xmlns:'+sSOAP_ENC_ABR, sSOAP_ENC);
  end;
end;

function TSOAPBaseFormatter.ReadHeaders(ACallContext: ICallContext): Integer;

  function ExtractTypeInfo(ANode : TDOMElement) : TTypeRegistryItem;
  var
    j : Integer;
    ndName, nsSN, nsLN, s : string;
  begin
    ndName := ANode.NodeName;
    j := Pos(':',ndName);
    if ( j > 0 ) then
      nsSN := Copy(ndName,1,Pred(j))
    else
      nsSN := '';
    if IsStrEmpty(nsSN) then
      s := sXML_NS
    else
      s := sXML_NS + ':' + nsSN;
    if not FindAttributeByNameInNode(s,ANode,nsLN) then
      nsLN := FindAttributeByNameInScope(s);
    Result := GetTypeRegistry().FindByDeclaredName(
                Copy(ndName,Succ(j),MaxInt),
                nsLN,
                [trsoIncludeExternalSynonyms]
              );
  end;

var
  i : Integer;
  nd : TDOMElement;
  typItm : TTypeRegistryItem;
  tmpHeader : THeaderBlock;
  locName : string;
  chdLst : TDOMNodeList;
  typData : PTypeData;
  tmpObj : TBaseRemotable;
begin
  SetStyleAndEncoding(Document,Literal);
  try
    Result := StackTop().ItemsCount;
    if ( Result > 0 ) then begin
      chdLst := StackTop().ScopeObject.ChildNodes;
      try
        for i := 0 to Pred(Result) do begin
          nd := chdLst.Item[i] as TDOMElement;
          typItm := ExtractTypeInfo(nd);
          if Assigned(typItm) then begin
            if ( typItm.DataType^.Kind = tkClass ) then begin
              tmpHeader := nil;
              locName := nd.NodeName;
              typData := GetTypeData(typItm.DataType);
              if typData^.ClassType.InheritsFrom(THeaderBlock) then begin
                Get(typItm.DataType,locName,tmpHeader);
                if Assigned(tmpHeader) then begin
                  tmpHeader.Direction := hdIn;
                  ACallContext.AddHeader(tmpHeader,True);
                  tmpHeader.Name := ExtractNameFromQualifiedName(locName);
                end;
              end else if typData^.ClassType.InheritsFrom(TBaseRemotable) then begin
                tmpObj := nil;
                Get(typItm.DataType,locName,tmpObj);
                if Assigned(tmpObj) then begin
                  tmpHeader := THeaderBlockProxy.Create();
                  THeaderBlockProxy(tmpHeader).ActualObject := tmpObj;
                  THeaderBlockProxy(tmpHeader).OwnObject := True;
                  tmpHeader.Direction := hdIn;
                  ACallContext.AddHeader(tmpHeader,True);
                  tmpHeader.Name := ExtractNameFromQualifiedName(locName);
                end;
              end else begin
                Error(SERR_ExpectingRemotableObjectClass,[typItm.DataType^.Name]);
              end;
            end;
          end;
        end;
      finally
        ReleaseDomNode(chdLst);
      end;
    end;
  finally
    RestoreStyleAndEncoding();
  end;
end;

function TSOAPBaseFormatter.WriteHeaders(ACallContext : ICallContext): Integer;
var
  ptyp : PTypeInfo;
  h : THeaderBlock;
  i, c : Integer;
  regItem : TTypeRegistryItem;
begin
  Result := ACallContext.GetHeaderCount([hdOut]);
  if ( Result > 0 ) then begin
    BeginHeader();
    try
      c := ACallContext.GetHeaderCount(AllHeaderDirection);
      for i := 0 to Pred(c) do begin
        h := ACallContext.GetHeader(i);
        if ( h.Direction = hdOut ) then begin
          if h.InheritsFrom(THeaderBlockProxy) then
            ptyp := PTypeInfo(THeaderBlockProxy(h).ActualObject.ClassInfo)
          else  
            ptyp := PTypeInfo(h.ClassInfo);
          regItem := GetTypeRegistry().Find(ptyp,True);
          //Put(GetTypeRegistry().ItemByTypeInfo[ptyp].DeclaredName,ptyp,h);
          if ( regItem <> nil) then
            Put(regItem.NameSpace,h.Name,PTypeInfo(h.ClassInfo),h)
          else
            Put(h.Name,ptyp,h);
        end;
      end;
    finally
      EndHeader();
    end;
  end;
end;

procedure TSOAPBaseFormatter.EndScope();
begin
  CheckScope();
  if ( StackTop().EmbeddedScopeCount = 0 ) then begin
    FStack.Pop().Free();
  end else begin
    StackTop().EndEmbeddedScope();
  end;
end;

procedure TSOAPBaseFormatter.AddScopeAttribute(const AName, AValue: string);
begin
  CheckScope();
  GetCurrentScopeObject().SetAttribute(AName,AValue);
end;

procedure TSOAPBaseFormatter.Put(
  const ANameSpace : string;
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData
);
Var
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  wideStrData : WideString;
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  Case ATypeInfo^.Kind Of
    tkChar :
      begin
        ansiCharData := AnsiChar(AData);
        PutAnsiChar(ANameSpace,AName,ATypeInfo,ansiCharData);
      end;
    tkWChar :
      begin
        wideCharData := WideChar(AData);
        PutWideChar(ANameSpace,AName,ATypeInfo,wideCharData);
      end;
    tkInt64 :
      Begin
        int64Data := Int64(AData);
        PutInt64(ANameSpace,AName,ATypeInfo,int64Data);
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := QWord(AData);
        PutUInt64(ANameSpace,AName,ATypeInfo,uint64Data);
      End;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := AnsiString(AData);
        PutStr(ANameSpace,AName,ATypeInfo,strData);
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      Begin
        unicodeStrData := UnicodeString(AData);
        PutUnicodeStr(ANameSpace,AName,ATypeInfo,unicodeStrData);
      End;
{$ENDIF WST_UNICODESTRING}
    tkWString :
      Begin
        wideStrData := WideString(AData);
        PutWideStr(ANameSpace,AName,ATypeInfo,wideStrData);
      End;
    tkClass :
      Begin
        objData := TObject(AData);
        PutObj(AName,ATypeInfo,objData);
      End;
    tkRecord :
      begin
        PutRecord(AName,ATypeInfo,Pointer(@AData));
      end;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := Boolean(AData);
        PutBool(ANameSpace,AName,ATypeInfo,boolData);
      End;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := Boolean(AData);
          PutBool(ANameSpace,AName,ATypeInfo,boolData);
        end else begin
      {$ENDIF}
          enumData := 0;
          Case GetTypeData(ATypeInfo)^.OrdType Of
            otSByte : enumData := ShortInt(AData);
            otUByte : enumData := Byte(AData);
            otSWord : enumData := SmallInt(AData);
            otUWord : enumData := Word(AData);
            otSLong : enumData := LongInt(AData);
            otULong : enumData := LongWord(AData);
          End;
          If ( ATypeInfo^.Kind = tkInteger ) Then
            PutInt64(ANameSpace,AName,ATypeInfo,enumData)
          Else
            PutEnum(ANameSpace,AName,ATypeInfo,enumData);
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      end;
    tkFloat :
      Begin
        floatDt := 0;
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : floatDt := Single(AData);
          ftDouble : floatDt := Double(AData);
          ftExtended : floatDt := Extended(AData);
          ftCurr : floatDt := Currency(AData);
          ftComp : floatDt := Comp(AData);
        End;
        PutFloat(ANameSpace,AName,ATypeInfo,floatDt);
      End;
  End;
end;

procedure TSOAPBaseFormatter.Put(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData
);
begin
  Put('',AName,ATypeInfo,AData);
end;

procedure TSOAPBaseFormatter.PutScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  const AData
);
Var
  int64SData : Int64;
  boolData : Boolean;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  enumData : TEnumIntType;
  floatDt : Extended;
  dataBuffer : DOMString;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  CheckScope();
  Case ATypeInfo^.Kind Of
    tkChar :
      begin
        ansiCharData := AnsiChar(AData);
        dataBuffer := ansiCharData;
      end;
    tkWChar :
      begin
        wideCharData := WideChar(AData);
        dataBuffer := wideCharData;
      end;
    tkInt64 :
      begin
        int64SData := Int64(AData);
        dataBuffer := IntToStr(int64SData);
      end;
{$IFDEF HAS_QWORD}
    tkQWord :
      begin
        uint64Data := QWord(AData);
        dataBuffer := IntToStr(uint64Data);
      end;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      begin
        strData := AnsiString(AData);
        dataBuffer := strData;
      end;
    tkWString :
      begin
        wideStrData := WideString(AData);
        dataBuffer := wideStrData;
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := UnicodeString(AData);
        dataBuffer := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
    tkClass :
      begin
        raise ESOAPException.Create(SERR_InnerScopeMustBeSimpleType);
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        boolData := Boolean(AData);
        dataBuffer := BoolToSoapBool(boolData);
      end;
    {$ENDIF}
    tkInteger :
      begin
        case GetTypeData(ATypeInfo)^.OrdType of
          otSByte : enumData := ShortInt(AData);
          otUByte : enumData := Byte(AData);
          otSWord : enumData := SmallInt(AData);
          otUWord : enumData := Word(AData);
          otSLong : enumData := LongInt(AData);
          otULong : enumData := LongWord(AData);
          else
            enumData := 0;   
        end;
        dataBuffer := IntToStr(enumData);
      end;
    tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) ) then begin
          boolData := Boolean(AData);
          dataBuffer := BoolToSoapBool(boolData);
        end else begin
      {$ENDIF}
          enumData := 0;
          case GetTypeData(ATypeInfo)^.OrdType of
            otSByte : enumData := ShortInt(AData);
            otUByte : enumData := Byte(AData);
            otSWord : enumData := SmallInt(AData);
            otUWord : enumData := Word(AData);
            otSLong : enumData := LongInt(AData);
            otULong : enumData := LongWord(AData);
          end;
          dataBuffer := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,enumData))
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      end;
    tkFloat :
      begin
        floatDt := 0;
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle   : floatDt := Single(AData);
          ftDouble   : floatDt := Double(AData);
          ftExtended : floatDt := Extended(AData);
          ftCurr     : floatDt := Currency(AData);
          ftComp     : floatDt := Comp(AData);
        end;
        dataBuffer := wst_FormatFloat(ATypeInfo,floatDt);
      end;
  end;
  StackTop().ScopeObject.AppendChild(FDoc.CreateTextNode(dataBuffer));
end;

function TSOAPBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var   AName     : String;
  var   AData
) : Boolean;
Var
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
  recObject : Pointer;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  wideStrData : WideString;
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  Case ATypeInfo^.Kind Of
    tkChar :
      begin
        ansiCharData := #0;
        Result := GetAnsiChar(ATypeInfo,ANameSpace,AName,ansiCharData);
        if Result  then
          AnsiChar(AData) := ansiCharData;
      end;
    tkWChar :
      begin
        wideCharData := #0;
        Result := GetWideChar(ATypeInfo,ANameSpace,AName,wideCharData);
        if Result  then
          WideChar(AData) := wideCharData;
      end;
    tkInt64 :
      Begin
        int64Data := 0;
        Result := GetInt64(ATypeInfo,ANameSpace,AName,int64Data);
        if Result  then
          Int64(AData) := int64Data;
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := 0;
        Result := GetUInt64(ATypeInfo,ANameSpace,AName,uint64Data);
        if Result  then
          QWord(AData) := uint64Data;
      End;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := '';
        Result := GetStr(ATypeInfo,ANameSpace,AName,strData);
        if Result  then
          AnsiString(AData) := strData;
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := '';
        Result := GetUnicodeStr(ATypeInfo,ANameSpace,AName,unicodeStrData);
        if Result  then
          UnicodeString(AData) := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
    tkWString :
      begin
        wideStrData := '';
        Result := GetWideStr(ATypeInfo,ANameSpace,AName,wideStrData);
        if Result  then
          WideString(AData) := wideStrData;
      end;
    tkClass :
      Begin
        objData := TObject(AData);
        Result := GetObj(ATypeInfo,AName,objData);
        if Result  then
          TObject(AData) := objData;
      End;
    tkRecord :
      begin
        recObject := Pointer(@AData);
        Result := GetRecord(ATypeInfo,AName,recObject);
      end;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := False;
        Result := GetBool(ATypeInfo,ANameSpace,AName,boolData);
        if Result  then
          Boolean(AData) := boolData;
      End;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := False;
          Result := GetBool(ATypeInfo,ANameSpace,AName,boolData);
          if Result  then
            Boolean(AData) := boolData;
        end else begin
      {$ENDIF}
          enumData := 0;
          if ( ATypeInfo^.Kind = tkInteger ) then
            Result := GetInt64(ATypeInfo,ANameSpace,AName,enumData)
          else
            Result := GetEnum(ATypeInfo,ANameSpace,AName,enumData);
          if Result then begin
            case GetTypeData(ATypeInfo)^.OrdType of
              otSByte : ShortInt(AData) := enumData;
              otUByte : Byte(AData) := enumData;
              otSWord : SmallInt(AData) := enumData;
              otUWord : Word(AData) := enumData;
              otSLong : LongInt(AData) := enumData;
              otULong : LongWord(AData) := enumData;
            end;
          end;
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      end;
    tkFloat :
      begin
        floatDt := 0;
        Result := GetFloat(ATypeInfo,ANameSpace,AName,floatDt);
        if Result then begin
          case GetTypeData(ATypeInfo)^.FloatType of
            ftSingle : Single(AData)    := floatDt;
            ftDouble : Double(AData)    := floatDt;
            ftExtended : Extended(AData)    := floatDt;
            ftCurr : Currency(AData)    := floatDt;
{$IFDEF CPU86}
            ftComp : Comp(AData)    := floatDt;
{$ENDIF}
          end;
        end;
      end;
    else
      Result := False;
  end;
end;

function TSOAPBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  var AName : string;
  var AData
) : Boolean;
begin
  Result := Get(ATypeInfo,'',AName,AData);
end;

procedure TSOAPBaseFormatter.GetScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  var   AData
);
Var
  enumData : TEnumIntType;
  floatDt : Extended;
  dataBuffer : DOMString;
  nd : TDOMNode;
begin
  CheckScope();
  nd := StackTop().ScopeObject;
  if nd.HasChildNodes() then
    dataBuffer := nd.FirstChild.NodeValue
  else
    dataBuffer := StackTop().ScopeObject.NodeValue;
  Case ATypeInfo^.Kind Of
    tkChar       :
      begin
        if ( Length(dataBuffer) > 0 ) then
          AnsiChar(AData) := AnsiChar(dataBuffer[1])
        else
          AnsiChar(AData) := #0;
      end;
    tkWChar       :
      begin
        if ( Length(dataBuffer) > 0 ) then
          WideChar(AData) := dataBuffer[1]
        else
          WideChar(AData) := #0;
      end;
    tkInt64      : Int64(AData) := StrToInt64Def(Trim(dataBuffer),0);
{$IFDEF HAS_QWORD}
    tkQWord      : QWord(AData) := StrToQWordDef(Trim(dataBuffer),0);
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} : AnsiString(AData) := dataBuffer;
    tkWString : WideString(AData) := dataBuffer;
{$IFDEF WST_UNICODESTRING}
    tkUString : UnicodeString(AData) := dataBuffer;
{$ENDIF WST_UNICODESTRING}
    tkClass :
      begin
        raise ESOAPException.Create(SERR_InnerScopeMustBeSimpleType);
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        dataBuffer := LowerCase(Trim(dataBuffer));
        if IsStrEmpty(dataBuffer) then
          Boolean(AData) := False
        else
          Boolean(AData) := StrToBool(dataBuffer);
      end;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          dataBuffer := LowerCase(Trim(dataBuffer));
          if IsStrEmpty(dataBuffer) then
            Boolean(AData) := False
          else
            Boolean(AData) := StrToBool(dataBuffer);
        end else begin
      {$ENDIF}
          if ( ATypeInfo^.Kind = tkInteger ) then
            enumData := StrToInt64Def(Trim(dataBuffer),0)
          else
            enumData := GetEnumValue(ATypeInfo,GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetInternalPropertyName(dataBuffer));
          case GetTypeData(ATypeInfo)^.OrdType of
            otSByte : ShortInt(AData) := enumData;
            otUByte : Byte(AData)     := enumData;
            otSWord : SmallInt(AData) := enumData;
            otUWord : Word(AData)     := enumData;
            otSLong : LongInt(AData)  := enumData;
            otULong : LongWord(AData)  := enumData;
          end;
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      end;
    tkFloat :
      begin
{$IFDEF HAS_FORMAT_SETTINGS}
        floatDt := StrToFloatDef(Trim(dataBuffer),0,wst_FormatSettings);
{$ELSE}
        floatDt := StrToFloatDef(TranslateDotToDecimalSeperator(Trim(dataBuffer)),0);
{$ENDIF HAS_FORMAT_SETTINGS}
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle    : Single(AData)        := floatDt;
          ftDouble    : Double(AData)        := floatDt;
          ftExtended  : Extended(AData)      := floatDt;
          ftCurr      : Currency(AData)      := floatDt;
{$IFDEF CPU86}
          ftComp      : Comp(AData)          := floatDt;
{$ENDIF}
        end;
      end;
  end;
end;

function TSOAPBaseFormatter.ReadBuffer(const AName : string; out AResBuffer : string) : Boolean;
Var
  locElt : TDOMNode;
  namespaceShortName, strNodeName : string;
  i : Integer;
begin
  strNodeName := AName;
  if ( Style = Document ) then begin
    namespaceShortName := FindAttributeByValueInScope(StackTop().NameSpace);
    i := Pos(':',namespaceShortName);
    if ( i > 0 ) then
      namespaceShortName := Copy(namespaceShortName,i + 1,MaxInt)
    else
      namespaceShortName := '';
    if not IsStrEmpty(namespaceShortName) then
      strNodeName := namespaceShortName + ':' + strNodeName;
  end;

  if ( FSerializationStyle = ssNodeSerialization ) then begin
    locElt := StackTop().FindNode(strNodeName);
  end else begin
    locElt := GetCurrentScopeObject().GetAttributeNode(strNodeName);
  end;

  Result := ( locElt <> nil );
  if Result then
    AResBuffer := NodeToBuffer(locElt);
end;

procedure TSOAPBaseFormatter.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TSOAPBaseFormatter.LoadFromStream(AStream: TStream);
Var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := GetXmlDoc().DocumentElement;
  If Assigned(nd) Then
    PushStack(nd);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string);
begin
  Raise ESOAPException.Create(AMsg);
end;

procedure TSOAPBaseFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise ESOAPException.CreateFmt(AMsg,AArgs);
end;

function TSOAPBaseFormatter.GetFormatName() : string;
begin
  Result := sPROTOCOL_NAME;
end;

function TSOAPBaseFormatter.GetPropertyManager() : IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

procedure TSOAPBaseFormatter.WriteBuffer(const AValue: string);
var
  strm : TStringStream;
  locDoc : TwstXMLDocument;
  locNode : TDOMNode;
begin
  CheckScope();
  locDoc := nil;
  strm := TStringStream.Create(AValue);
  try
    ReadXMLFile(locDoc,strm);
    locNode := locDoc.DocumentElement.CloneNode(True {$IFDEF FPC}, StackTop().ScopeObject.OwnerDocument{$ENDIF});
    StackTop().ScopeObject.AppendChild(locNode);
  finally
    ReleaseDomNode(locDoc);
    strm.Free();
  end;
end;

{ TScopedArrayStackItem }

destructor TScopedArrayStackItem.Destroy();  
begin
  if ( FItemList <> nil ) then
    FItemList := nil;
  inherited Destroy();  
end;

function TScopedArrayStackItem.CreateList(const ANodeName : string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := ScopeObject.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

{ TEmbeddedArrayStackItem }

function TEmbeddedArrayStackItem.CreateList(const ANodeName: string): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := FilterList(ScopeObject,ANodeName);
  end else begin
    Result := nil;
  end;
end;

end.
