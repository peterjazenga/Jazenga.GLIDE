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
unit base_xmlrpc_formatter;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
{$IFDEF WST_DELPHI}
  xmldom, wst_delphi_xml, 
{$ENDIF WST_DELPHI}
{$IFDEF FPC}
  DOM, XMLWrite, XMLRead,wst_fpc_xml,
{$ENDIF FPC}
  base_service_intf;

const
  sPROTOCOL_NAME = 'XMLRPC';


  sCONTENT_TYPE = 'contenttype';
  sFORMAT = 'format';
  sXMLRPC_CONTENT_TYPE = 'text/xml';

  sDATA  = 'data';
  sFAULT = 'fault';
  sFAULT_CODE = 'faultCode';
  sFAULT_STRING = 'faultString';
  sMEMBER = 'member';
  sMETHOD_CALL  = 'methodCall';
  sMETHOD_NAME  = 'methodName';
  sMETHOD_RESPONSE  = 'methodResponse';
  sNAME = 'name';
  sPARAM = 'param';
  sPARAMS = 'params';
  sVALUE = 'value';

  XML_RPC_FALSE = '0';
  XML_RPC_TRUE  = '1';

  stXmlRpcDate    = stBase + 3;
  stSimpleContent = stXmlRpcDate + 1;

type

  TwstXMLDocument = {$IFNDEF FPC}wst_delphi_xml.TXMLDocument{$ELSE}TXMLDocument{$ENDIF};
  
  TEnumIntType = Int64;

  TXmlRpcDataType = (
    xdtString, xdtInt, xdtBoolean, xdtdouble, xdtDateTime, xdtBase64,
    xdtStruct, xdtArray
  );
  
const
  XmlRpcDataTypeNames : array[TXmlRpcDataType] of string = (
    'string', 'int', 'boolean', 'double', 'dateTime.iso8601', 'base64',
    'struct', 'array'
  );

type
  { ESOAPException }

  EXmlRpcException = class(EBaseRemoteException)
  end;

  TFoundState = ( fsNone, fsFoundNonNil, fsFoundNil );

  { TStackItem }

  TStackItem = class
  private
    FFoundState : TFoundState;
    FScopeObject: TDOMNode;
    FScopeType: TScopeType;
  protected
    function GetItemsCount() : Integer;virtual;
    procedure SetFoundState(const AFoundState : TFoundState);
  public
    constructor Create(AScopeObject : TDOMNode;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TDOMNode;virtual;abstract;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;virtual;abstract;
    property ScopeObject : TDOMNode Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property ItemsCount : Integer read GetItemsCount;
    property FoundState : TFoundState read FFoundState;

    function GetScopeItemNames(const AReturnList : TStrings) : Integer;virtual;abstract;
    procedure CreateInnerBuffer(
      const AText : DOMString;
      const ADoc  : TXMLDocument
    ); virtual;
  end;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  public
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;

    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  end;

  TBaseArrayStackItem = class(TStackItem)
  private
    FItemList : TDOMNodeList;
    FIndex : Integer;
    FIndexStack : array of Integer;
    FIndexStackIDX : Integer;
  private
    function PushIndex(const AValue : Integer) : Integer;
    function PopIndex() : Integer;
  public
    destructor Destroy();override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  end;

  { TArrayStackItem }

  TArrayStackItem = class(TBaseArrayStackItem)
  private
    FDataScope : TDOMNode;
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList():TDOMNodeList;
    function PushIndex(const AValue : Integer) : Integer;
    function PopIndex() : Integer;
  public
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;
  end;

  { TParamsArrayStackItem }

  TParamsArrayStackItem = class(TBaseArrayStackItem)
  protected
    procedure EnsureListCreated();
    function GetItemsCount() : Integer;override;
    function CreateList():TDOMNodeList;
  public
    function FindNode(var ANodeName : string):TDOMNode;override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode;override;
  end;

  { TSimpleTypeStackItem }

  TSimpleTypeStackItem = class(TStackItem)
  public
    function FindNode(var ANodeName : string):TDOMNode; override;
    function CreateBuffer(
      Const AName     : string;
      const ADataType : TXmlRpcDataType
    ):TDOMNode; override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer; override;
  end;

{$M+}

  { TXmlRpcBaseFormatter }

  TXmlRpcBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FPropMngr : IPropertyManager;
    FContentType: string;
    FDoc : TXMLDocument;
    FStack : TObjectStack;
    FSerializationStyle: TSerializationStyle;
  private
    procedure InternalClear(const ACreateDoc : Boolean);{$IFDEF USE_INLINE}inline;{$ENDIF}

    function HasScope():Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure CheckScope();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function InternalPutData(
      const AName      : string;
      const AType      : TXmlRpcDataType;
      const AData      : DOMString
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutAnsiChar(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : AnsiChar
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutWideChar(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideChar
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_QWORD}
    function PutUInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : QWord
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_QWORD}
    function PutStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF WST_UNICODESTRING}
    function PutUnicodeStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : UnicodeString
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF WST_UNICODESTRING}
    function PutWideStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideString
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Extended
    ):TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutObj(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TObject
    ); {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutRecord(
      const AName     : string;
      const ATypeInfo : PTypeInfo;
      const AData     : Pointer
    );{$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetNodeValue(
      var   AName      : string;
      out   AResBuffer : DOMString;
      const AHandleDefaultType : Boolean = False
    ) : Boolean;
    function GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumIntType
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetAnsiChar(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : AnsiChar
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetWideChar(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : WideChar
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Integer
    ) : Boolean;
    function GetInt64(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Int64
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_QWORD}
    function GetUInt64(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : QWord
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_QWORD}
    function GetFloat(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Extended
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetStr(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : String
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF WST_UNICODESTRING}
    function GetUnicodeStr(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : UnicodeString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF WST_UNICODESTRING}
    function GetWideStr(
      Const ATypeInfo : PTypeInfo;
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
    function GetXmlDoc():TXMLDocument;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(AScopeObject : TDOMNode):TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStack(
            AScopeObject : TDOMNode;
      const AStyle       : TArrayStyle;
      const AItemName    : string
    ):TStackItem;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PushStackParams(AScopeObject : TDOMNode) : TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
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
  protected
    function GetCurrentScope():String;
    function GetCurrentScopeObject():TDOMElement;{$IFDEF USE_INLINE}inline;{$ENDIF}
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
    ) : Boolean;overload;
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
  published
    property ContentType : string Read FContentType Write FContentType;
  end;
{$M-}

implementation
uses
  Imp_utils, wst_consts;

{ TStackItem }

function TStackItem.GetItemsCount(): Integer;
begin
  Result := GetNodeItemsCount(ScopeObject);
end;

procedure TStackItem.SetFoundState (const AFoundState : TFoundState );
begin
  FFoundState := AFoundState;
end;

constructor TStackItem.Create(AScopeObject: TDOMNode; AScopeType: TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

procedure TStackItem.CreateInnerBuffer(
  const AText : DOMString;
  const ADoc  : TXMLDocument
);
begin
  ScopeObject.AppendChild(ADoc.CreateTextNode(AText));
end;

{ TObjectStackItem }

function TObjectStackItem.FindNode(var ANodeName: string): TDOMNode;
var
  memberNode, tmpNode : TDOMNode;
  i : Integer;
  chilNodes : TDOMNodeList;
  nodeFound : Boolean;
begin
  Result := nil;
  if ScopeObject.HasChildNodes() then begin
    nodeFound := False;
    memberNode := ScopeObject.FirstChild;
    while ( not nodeFound ) and ( memberNode <> nil ) do begin
      if memberNode.HasChildNodes() then begin
        chilNodes := memberNode.ChildNodes;
        for i := 0 to Pred(GetNodeListCount(chilNodes)) do begin
          tmpNode := chilNodes.Item[i];
          if AnsiSameText(sNAME,tmpNode.NodeName) and
             ( tmpNode.FirstChild <> nil ) and
             AnsiSameText(ANodeName,tmpNode.FirstChild.NodeValue)
          then begin
            nodeFound := True;
            Break;
          end;
        end;
        if nodeFound then begin
        {$IFNDEF FPC}
          tmpNode := wst_delphi_xml.FindNode(memberNode,sVALUE);
        {$ELSE}
          tmpNode := memberNode.FindNode(sVALUE);
        {$ENDIF}
          if ( tmpNode <> nil ) and ( tmpNode.FirstChild <> nil ) then begin
            Result := tmpNode.FirstChild;
            Break;
          end;
        end;
      end;
      memberNode := memberNode.NextSibling;
    end;
  end;
  if ( Result <> nil ) then begin
    if Result.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
  end else begin
    SetFoundState(fsNone);
  end;
end;

function TObjectStackItem.CreateBuffer(
  const AName: String;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  memberNode, nd : TDOMNode;
begin
  memberNode := ScopeObject.OwnerDocument.CreateElement(sMEMBER);
  ScopeObject.AppendChild(memberNode);

  nd := ScopeObject.OwnerDocument.CreateElement(sNAME);
  memberNode.AppendChild(nd);
  nd.AppendChild(ScopeObject.OwnerDocument.CreateTextNode(AName));

  nd := ScopeObject.OwnerDocument.CreateElement(sVALUE);
  memberNode.AppendChild(nd);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  nd.AppendChild(Result);
end;

function TObjectStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  memberNode, tmpNode : TDOMNode;
  i : Integer;
  chilNodes : TDOMNodeList;
begin
  AReturnList.Clear();
  if ScopeObject.HasChildNodes() then begin
    memberNode := ScopeObject.FirstChild;
    while ( memberNode <> nil ) do begin
      if memberNode.HasChildNodes() then begin
        chilNodes := memberNode.ChildNodes;
        for i := 0 to Pred(GetNodeListCount(chilNodes)) do begin
          tmpNode := chilNodes.Item[i];
          if AnsiSameText(sNAME,tmpNode.NodeName) then begin
            if ( tmpNode.FirstChild <> nil ) then
              AReturnList.Add(tmpNode.FirstChild.NodeValue)
            else
              AReturnList.Add('');
            Break;
          end;
        end;
      end;
      memberNode := memberNode.NextSibling;
    end;
  end;
  Result := AReturnList.Count;
end;

{ TArrayStackItem }

procedure TArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList();
  end;
end;

function TArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

function TArrayStackItem.CreateList(): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() and ScopeObject.FirstChild.HasChildNodes() then begin
    Result := ScopeObject.FirstChild.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

function TArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise EXmlRpcException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  if Result.HasChildNodes() then begin
    if Result.FirstChild.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
    Result := Result.FirstChild;//.FirstChild;
    Inc(FIndex);
    ANodeName := Result.NodeName;
  end else begin
    raise EXmlRpcException.CreateFmt('Invalid array item : Index = %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  end;
end;

function TArrayStackItem.CreateBuffer(
  const AName: string;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  nd : TDOMNode;
begin
  if ( FDataScope = nil ) then begin
    FDataScope := ScopeObject.OwnerDocument.CreateElement(sDATA);
    ScopeObject.AppendChild(FDataScope);
  end;

  nd := FDataScope.OwnerDocument.CreateElement(sVALUE);
  FDataScope.AppendChild(nd);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  nd.AppendChild(Result);
end;

function TArrayStackItem.PushIndex(const AValue: Integer): Integer;
begin
  if ( FIndexStackIDX = Length(FIndexStack) ) then begin
    if ( Length(FIndexStack) = 0 ) then
      FIndexStackIDX := -1;
    SetLength(FIndexStack, Length(FIndexStack) + 4);
  end;
  Result := FIndex;
  Inc(FIndexStackIDX);
  FIndexStack[FIndexStackIDX] := AValue;
end;

function TArrayStackItem.PopIndex() : Integer;
begin
  if ( Length(FIndexStack) = 0 ) or ( FIndexStackIDX < 0 ) then
    raise EXmlRpcException.Create('TArrayStackItem.PopIndex() >> No saved index.');
  FIndex := FIndexStack[FIndexStackIDX];
  Dec(FIndexStackIDX);
  Result := FIndex;
end;

{ TXmlRpcBaseFormatter }

procedure TXmlRpcBaseFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

function TXmlRpcBaseFormatter.PushStack(AScopeObject : TDOMNode) : TStackItem;
begin
  Result := FStack.Push(TObjectStackItem.Create(AScopeObject,stObject)) as TStackItem;
end;

function TXmlRpcBaseFormatter.PushStack(
        AScopeObject : TDOMNode;
  const AStyle       : TArrayStyle;
  const AItemName    : string
): TStackItem;
begin
  Result := FStack.Push(TArrayStackItem.Create(AScopeObject,stArray)) as TStackItem;
end;

function TXmlRpcBaseFormatter.PushStackParams(AScopeObject: TDOMNode): TStackItem;
begin
  Result := FStack.Push(TParamsArrayStackItem.Create(AScopeObject,stArray)) as TStackItem;
end;

function TXmlRpcBaseFormatter.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stObject,asNone,'');
end;

function TXmlRpcBaseFormatter.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
begin
  Result := InternalBeginScopeRead(AScopeName,ATypeInfo,stArray,AStyle,AItemName);
end;

function TXmlRpcBaseFormatter.GetScopeItemNames(const AReturnList : TStrings) : Integer;
begin
  CheckScope();
  Result := StackTop.GetScopeItemNames(AReturnList);
end;

procedure TXmlRpcBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TXmlRpcBaseFormatter.BeginHeader();
begin
end;

procedure TXmlRpcBaseFormatter.EndHeader();
begin
end;

procedure TXmlRpcBaseFormatter.InternalClear(const ACreateDoc: Boolean);
begin
  ClearStack();
  ReleaseDomNode(FDoc);
  FDoc := nil;
  if ACreateDoc then
    FDoc := CreateDoc();
end;

function TXmlRpcBaseFormatter.HasScope(): Boolean;
begin
  Result := FStack.AtLeast(1);
end;

function TXmlRpcBaseFormatter.FindAttributeByValueInNode(
  Const AAttValue : String;
  Const ANode     : TDOMNode;
  Out   AResAtt   : string
):boolean;
Var
  i,c : Integer;
begin
  AResAtt := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttValue,ANode.Attributes.Item[i].NodeValue) Then Begin
        AResAtt := ANode.Attributes.Item[i].NodeName;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TXmlRpcBaseFormatter.FindAttributeByNameInNode(
  const AAttName: String;
  const ANode: TDOMNode;
  Out AResAttValue: string
): boolean;
var
  i,c : Integer;
begin
  AResAttValue := '';
  If Assigned(ANode) And Assigned(ANode.Attributes) Then Begin
    c := Pred(ANode.Attributes.Length);
    For i := 0 To c Do Begin
      If AnsiSameText(AAttName,ANode.Attributes.Item[i].NodeName) Then Begin
        AResAttValue := ANode.Attributes.Item[i].NodeValue;
        Result := True;
        Exit;
      End;
    End;
  End;
  Result := False;
end;

function TXmlRpcBaseFormatter.FindAttributeByValueInScope(const AAttValue: String): String;
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

function TXmlRpcBaseFormatter.FindAttributeByNameInScope(const AAttName: String): String;
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

procedure TXmlRpcBaseFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error(SERR_NoScope);
end;

function TXmlRpcBaseFormatter.InternalPutData(
  const AName      : string;
  const AType      : TXmlRpcDataType;
  const AData      : DOMString
): TDOMNode;
begin
  Result := StackTop().CreateBuffer(AName,AType).AppendChild(FDoc.CreateTextNode(AData));
end;

function TXmlRpcBaseFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumIntType
): TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetExternalPropertyName(GetEnumName(ATypeInfo,AData))
            );
end;

function TXmlRpcBaseFormatter.PutBool(
  const AName     : String;
  const ATypeInfo : PTypeInfo;
  const AData     : Boolean
) : TDOMNode;
var
  v : Char;
begin
  if AData then
    v := XML_RPC_TRUE
  else
    v := XML_RPC_FALSE;
  Result := InternalPutData(AName,xdtBoolean,v);
end;

function TXmlRpcBaseFormatter.PutAnsiChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: AnsiChar
) : TDOMNode;
begin
  Result := InternalPutData(AName,xdtString,AData);
end;

function TXmlRpcBaseFormatter.PutWideChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideChar
) : TDOMNode;
begin
  Result := InternalPutData(AName,xdtString,AData);
end;

function TXmlRpcBaseFormatter.PutInt64(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Int64
): TDOMNode;
begin
  Result := InternalPutData(AName,xdtInt,IntToStr(AData));
end;

{$IFDEF HAS_QWORD}
function TXmlRpcBaseFormatter.PutUInt64(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : QWord
): TDOMNode;
begin
  Result := InternalPutData(AName,xdtInt,IntToStr(AData));
end;
{$ENDIF HAS_QWORD}

function TXmlRpcBaseFormatter.PutStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: String
):TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              StringReplace(StringReplace(AData,'<','&lt;',[rfReplaceAll]),'&','&amp;',[rfReplaceAll])
            );
end;

{$IFDEF WST_UNICODESTRING}
function TXmlRpcBaseFormatter.PutUnicodeStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: UnicodeString
) : TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              AData//StringReplace(StringReplace(AData,'<','&lt;',[rfReplaceAll]),'&','&amp;',[rfReplaceAll])
            );
end;
{$ENDIF WST_UNICODESTRING}

function TXmlRpcBaseFormatter.PutWideStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideString
) : TDOMNode;
begin
  Result := InternalPutData(
              AName,
              xdtString,
              AData//StringReplace(StringReplace(AData,'<','&lt;',[rfReplaceAll]),'&','&amp;',[rfReplaceAll])
            );
end;

procedure TXmlRpcBaseFormatter.PutObj(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

procedure TXmlRpcBaseFormatter.PutRecord(
  const AName     : string;
  const ATypeInfo : PTypeInfo;
  const AData     : Pointer
);
begin
  TRemotableRecordEncoder.Save(AData,Self,AName,ATypeInfo);
end;

function TXmlRpcBaseFormatter.PutFloat(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
):TDOMNode;
begin
  Result := InternalPutData(AName,xdtdouble,wst_FormatFloat(ATypeInfo,AData));
end;

function TXmlRpcBaseFormatter.GetNodeValue(
  var   AName: string;
  out   AResBuffer : DOMString;
  const AHandleDefaultType : Boolean
) : Boolean;
var
  locElt : TDOMNode;
  stkTop : TStackItem;
begin
  stkTop := StackTop();
  locElt := stkTop.FindNode(AName);

  Result := ( locElt <> nil );
  if Result then begin
    if locElt.HasChildNodes then begin
      AResBuffer := locElt.FirstChild.NodeValue
    end else begin
      if ( stkTop.FoundState = fsFoundNil ) then begin
        if AHandleDefaultType then
          AResBuffer := locElt.NodeValue
        else
          AResBuffer := '';
      end else begin
        AResBuffer := locElt.NodeValue;
      end;
    end;
  end;
end;

function TXmlRpcBaseFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TEnumIntType
) : Boolean;
var
  locBuffer : DOMString;
  locStr : string;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then begin
    locStr := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetInternalPropertyName(locBuffer);
    If IsStrEmpty(locStr) Then
      AData := 0
    Else
      AData := GetEnumValue(ATypeInfo,locStr)
  end;
End;

function TXmlRpcBaseFormatter.GetBool(
  const ATypeInfo  : PTypeInfo;
  var   AName      : String;
  var   AData      : Boolean
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then
    AData := ( locBuffer = XML_RPC_TRUE );
end;

function TXmlRpcBaseFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: Integer
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then
    AData := StrToIntDef(Trim(locBuffer),0);
end;

function TXmlRpcBaseFormatter.GetAnsiChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: AnsiChar
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer,True);
  if Result then begin
    if ( Length(locBuffer) = 0 ) then
      AData := #0
    else
      AData := AnsiChar(locBuffer[1]);
  end;
end;

function TXmlRpcBaseFormatter.GetWideChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideChar
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer,True);
  if Result then begin
    if ( Length(locBuffer) = 0 ) then
      AData := #0
    else
      AData := locBuffer[1];
  end;
end;

function TXmlRpcBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Int64
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then
    AData := StrToInt64Def(Trim(locBuffer),0);
end;

{$IFDEF HAS_QWORD}
function TXmlRpcBaseFormatter.GetUInt64(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : QWord
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then
    AData := StrToQWordDef(Trim(locBuffer),0);
end;
{$ENDIF HAS_QWORD}

function TXmlRpcBaseFormatter.GetFloat(
  const ATypeInfo  : PTypeInfo;
  var AName        : String;
  var AData        : Extended
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer);
  if Result then begin
{$IFDEF HAS_FORMAT_SETTINGS}
    AData := StrToFloatDef(Trim(locBuffer),0,wst_FormatSettings);
{$ELSE}
    AData := StrToFloatDef(TranslateDotToDecimalSeperator(Trim(locBuffer)),0);
{$ENDIF HAS_FORMAT_SETTINGS}
  end;
end;

function TXmlRpcBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : String
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer,True);
  if Result then
    AData := locBuffer;
end;

{$IFDEF WST_UNICODESTRING}
function TXmlRpcBaseFormatter.GetUnicodeStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: UnicodeString
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer,True);
  if Result then
    AData := locBuffer;
end;
{$ENDIF WST_UNICODESTRING}

function TXmlRpcBaseFormatter.GetWideStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideString
) : Boolean;
var
  locBuffer : DOMString;
begin
  Result := GetNodeValue(AName,locBuffer,True);
  if Result then
    AData := locBuffer;
end;

function TXmlRpcBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TObject
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TXmlRpcBaseFormatter.GetRecord(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Pointer
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TRemotableRecordEncoder.Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TXmlRpcBaseFormatter.GetXmlDoc(): TwstXMLDocument;
begin
  Result := FDoc;
end;

function TXmlRpcBaseFormatter.GetCurrentScope(): String;
begin
  CheckScope();
  Result:= GetCurrentScopeObject().NodeName;
end;

function TXmlRpcBaseFormatter.GetCurrentScopeObject(): TDOMElement;
begin
  Result := StackTop().ScopeObject As TDOMElement;
end;

function TXmlRpcBaseFormatter.StackTop(): TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TXmlRpcBaseFormatter.PopStack(): TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

constructor TXmlRpcBaseFormatter.Create();
begin
  Inherited Create();
  FContentType := sXMLRPC_CONTENT_TYPE;
  FStack := TObjectStack.Create();
  FDoc := CreateDoc();
end;

destructor TXmlRpcBaseFormatter.Destroy();
begin
  ReleaseDomNode(FDoc);
  ClearStack();
  FStack.Free();
  inherited Destroy();
end;

procedure TXmlRpcBaseFormatter.Clear();
begin
  InternalClear(True);
end;

procedure TXmlRpcBaseFormatter.BeginObject(
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
var
  locScopeType : TScopeType;
  locClass : TClass;
begin
  locScopeType := stObject;
  if ( ATypeInfo^.Kind = tkClass ) then begin
    locClass := GetTypeData(ATypeInfo)^.ClassType;
    if locClass.InheritsFrom(TAbstractSimpleRemotable) then begin
      if locClass.InheritsFrom(TDateTimeRemotable) or
         locClass.InheritsFrom(TDateRemotable)  
      then
        locScopeType := stXmlRpcDate
      else
        locScopeType := stSimpleContent;
    end;
  end;

  BeginScope(AName,'','',locScopeType,asNone);
end;

procedure TXmlRpcBaseFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
var
  i,j, k : Integer;
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

  BeginScope(AName,'','',stArray,AStyle);
end;

procedure TXmlRpcBaseFormatter.NilCurrentScope();
begin
end;

function TXmlRpcBaseFormatter.IsCurrentScopeNil(): Boolean;
begin
  Result := False;
end;

procedure TXmlRpcBaseFormatter.BeginScope(
  Const AScopeName,ANameSpace : string;
  Const ANameSpaceShortName   : string;
  Const AScopeType            : TScopeType;
  const AStyle                : TArrayStyle
);
Var
  e : TDOMNode;
  dtType : TXmlRpcDataType;
begin
  case AScopeType of
    stXmlRpcDate      : dtType := xdtDateTime;
    stSimpleContent   : dtType := xdtString;
    stArray           : dtType := xdtArray;
    else
                        dtType := xdtStruct;
  end;
  if HasScope() then begin
    e := StackTop().CreateBuffer(AScopeName,dtType);
  end else begin
    e := FDoc.CreateElement(XmlRpcDataTypeNames[dtType]);
    FDoc.AppendChild(e);
  end;
  if ( AScopeType = stObject ) then begin
    PushStack(e);
  end else if ( AScopeType = stArray ) then begin
    PushStack(e,AStyle,'');
  end else begin
    FStack.Push(TSimpleTypeStackItem.Create(e,AScopeType));
  end;
end;

function TXmlRpcBaseFormatter.InternalBeginScopeRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo;
  const AScopeType : TScopeType;
  const AStyle     : TArrayStyle;
  const AItemName  : string
): Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.FindNode(AScopeName);
  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    if ( AScopeType = stObject ) then begin
      PushStack(locNode);
    end else begin
      PushStack(locNode,AStyle,AItemName);
    end;
    Result := StackTop().GetItemsCount();
  end;
end;

procedure TXmlRpcBaseFormatter.SetSerializationStyle(const ASerializationStyle: TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TXmlRpcBaseFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

procedure TXmlRpcBaseFormatter.EndScope();
begin
  CheckScope();
  FStack.Pop().Free();
end;

procedure TXmlRpcBaseFormatter.AddScopeAttribute(const AName, AValue: string);
begin
//  CheckScope();
  //GetCurrentScopeObject().SetAttribute(AName,AValue);
end;

procedure TXmlRpcBaseFormatter.Put(
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
        PutAnsiChar(AName,ATypeInfo,ansiCharData);
      end;
    tkWChar :
      begin
        wideCharData := WideChar(AData);
        PutWideChar(AName,ATypeInfo,wideCharData);
      end;
    tkInt64 :
      Begin
        int64Data := Int64(AData);
        PutInt64(AName,ATypeInfo,int64Data);
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := QWord(AData);
        PutUInt64(AName,ATypeInfo,uint64Data);
      End;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := AnsiString(AData);
        PutStr(AName,ATypeInfo,strData);
      End;
    tkWString :
      Begin
        wideStrData := WideString(AData);
        PutWideStr(AName,ATypeInfo,wideStrData);
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      Begin
        unicodeStrData := UnicodeString(AData);
        PutUnicodeStr(AName,ATypeInfo,unicodeStrData);
      End;
{$ENDIF WST_UNICODESTRING}
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
        PutBool(AName,ATypeInfo,boolData);
      End;
    {$ENDIF}
    tkInteger, tkEnumeration :
      Begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := Boolean(AData);
          PutBool(AName,ATypeInfo,boolData);
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
            PutInt64(AName,ATypeInfo,enumData)
          Else
            PutEnum(AName,ATypeInfo,enumData);
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      End;
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
        PutFloat(AName,ATypeInfo,floatDt);
      End;
  End;
end;

procedure TXmlRpcBaseFormatter.Put(
  const ANameSpace : string;
  const AName : String;
  const ATypeInfo : PTypeInfo; const AData
);
begin
  Put(AName,ATypeInfo,AData);
end;

procedure TXmlRpcBaseFormatter.PutScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  const AData
);
Var
  int64SData : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  boolData : Boolean;
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
        raise EXmlRpcException.Create(SERR_InnerScopeMustBeSimpleType);
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        boolData := Boolean(AData);
        if boolData then
          dataBuffer := XML_RPC_TRUE
        else
          dataBuffer := XML_RPC_FALSE;
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
          if boolData then
            dataBuffer := XML_RPC_TRUE
          else
            dataBuffer := XML_RPC_FALSE;
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
  StackTop().CreateInnerBuffer(dataBuffer,FDoc);
  //StackTop().ScopeObject.AppendChild(FDoc.CreateTextNode(dataBuffer));
end;

function TXmlRpcBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
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
        Result := GetAnsiChar(ATypeInfo,AName,ansiCharData);
        if Result then
          AnsiChar(AData) := ansiCharData;
      end;
    tkWChar :
      begin
        wideCharData := #0;
        Result := GetWideChar(ATypeInfo,AName,wideCharData);
        if Result then
          WideChar(AData) := wideCharData;
      end;
    tkInt64 :
      Begin
        int64Data := 0;
        Result := GetInt64(ATypeInfo,AName,int64Data);
        if Result then
          Int64(AData) := int64Data;
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := 0;
        Result := GetUInt64(ATypeInfo,AName,uint64Data);
        if Result then
          QWord(AData) := uint64Data;
      End;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := '';
        Result := GetStr(ATypeInfo,AName,strData);
        if Result then
          AnsiString(AData) := strData;
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := '';
        Result := GetUnicodeStr(ATypeInfo,AName,unicodeStrData);
        if Result then
          UnicodeString(AData) := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
    tkWString :
      begin
        wideStrData := '';
        Result := GetWideStr(ATypeInfo,AName,wideStrData);
        if Result then
          WideString(AData) := wideStrData;
      end;
    tkClass :
      Begin
        objData := TObject(AData);
        Result := GetObj(ATypeInfo,AName,objData);
        if Result then
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
        Result := GetBool(ATypeInfo,AName,boolData);
        if Result then
          Boolean(AData) := boolData;
      End;
    {$ENDIF}  
    tkInteger, tkEnumeration :
      Begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := False;
          Result := GetBool(ATypeInfo,AName,boolData);
          if Result then
            Boolean(AData) := boolData;
        end else begin
      {$ENDIF}      
          enumData := 0;
          if ( ATypeInfo^.Kind = tkInteger ) then
            Result := GetInt64(ATypeInfo,AName,enumData)
          else
            Result := GetEnum(ATypeInfo,AName,enumData);
          if Result then begin
            case GetTypeData(ATypeInfo)^.OrdType Of
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
      End;
    tkFloat :
      begin
        floatDt := 0;
        Result := GetFloat(ATypeInfo,AName,floatDt);
        if Result then begin
          case GetTypeData(ATypeInfo)^.FloatType of
            ftSingle : Single(AData)    := floatDt;
            ftDouble : Double(AData)    := floatDt;
            ftExtended : Extended(AData)    := floatDt;
            ftCurr : Currency(AData)    := floatDt;
{$IFDEF HAS_COMP}
            ftComp : Comp(AData)    := floatDt;
{$ENDIF}
          end;
        end;
      end;
    else
      Result := False;
  end;
end;

function TXmlRpcBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var AName : string;
  var AData
) : Boolean;
begin
  Result := Get(ATypeInfo,AName,AData);
end;

procedure TXmlRpcBaseFormatter.GetScopeInnerValue(
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
          WideChar(AData) :=dataBuffer[1]
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
        raise EXmlRpcException.Create(SERR_InnerScopeMustBeSimpleType);
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        Boolean(AData) := ( dataBuffer = XML_RPC_TRUE );
      end;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          Boolean(AData) := ( dataBuffer = XML_RPC_TRUE );
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
            otULong : LongWord(AData) := enumData;
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
{$IFDEF HAS_COMP}
          ftComp      : Comp(AData)          := floatDt;
{$ENDIF}
        end;
      end;
  end;
end;

function TXmlRpcBaseFormatter.ReadBuffer(
  const AName : string;
  out AResBuffer : string
) : Boolean;
var
  locElt : TDOMNode;
  stkTop : TStackItem;
  locName : string;
begin
  stkTop := StackTop();
  locName := AName;
  locElt := stkTop.FindNode(locName);

  Result := ( locElt <> nil );
  if Result then
    AResBuffer := NodeToBuffer(locElt);
end;

procedure TXmlRpcBaseFormatter.SaveToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc,AStream);
end;

procedure TXmlRpcBaseFormatter.LoadFromStream(AStream: TStream);
Var
  nd : TDOMNode;
begin
  InternalClear(False);
  ReadXMLFile(FDoc,AStream);
  nd := GetXmlDoc().DocumentElement;
  If Assigned(nd) Then
    PushStack(nd);
end;

procedure TXmlRpcBaseFormatter.Error(const AMsg: string);
begin
  Raise EXmlRpcException.Create(AMsg);
end;

procedure TXmlRpcBaseFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EXmlRpcException.CreateFmt(AMsg,AArgs);
end;
                 
function TXmlRpcBaseFormatter.GetFormatName() : string;
begin
  Result := sPROTOCOL_NAME;
end;

function TXmlRpcBaseFormatter.GetPropertyManager() : IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

procedure TXmlRpcBaseFormatter.WriteBuffer(const AValue: string);
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

{ TParamsArrayStackItem }

procedure TParamsArrayStackItem.EnsureListCreated();
begin
  if ( FItemList = nil ) then begin
    FItemList := CreateList();
  end;
end;

function TParamsArrayStackItem.GetItemsCount(): Integer;
begin
  EnsureListCreated();
  if Assigned(FItemList) then begin
    Result := GetNodeListCount(FItemList);
  end else begin
    Result := 0;
  end;
end;

function TParamsArrayStackItem.CreateList(): TDOMNodeList;
begin
  if ScopeObject.HasChildNodes() then begin
    Result := ScopeObject.ChildNodes;
  end else begin
    Result := nil;
  end;
end;

function TParamsArrayStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  EnsureListCreated();
  if ( FIndex >= GetNodeListCount(FItemList) ) then
    raise EXmlRpcException.CreateFmt('Index out of bound : %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  Result:= FItemList.Item[FIndex];
  if Result.HasChildNodes() then begin
    if Result.FirstChild.HasChildNodes() then
      SetFoundState(fsFoundNonNil)
    else
      SetFoundState(fsFoundNil);
    Result := Result.FirstChild.FirstChild;
    Inc(FIndex);
    ANodeName := Result.NodeName;
  end else begin
    raise EXmlRpcException.CreateFmt('Invalid array item : Index = %d; Node Name = "%s"; Parent Node = "%s"',[FIndex,ANodeName,ScopeObject.NodeName]);
  end;
end;

function TParamsArrayStackItem.CreateBuffer(
  const AName: string;
  const ADataType: TXmlRpcDataType
): TDOMNode;
var
  prmNode, valueNode : TDOMNode;
begin
  prmNode := ScopeObject.OwnerDocument.CreateElement(sPARAM);
  ScopeObject.AppendChild(prmNode);
  valueNode := ScopeObject.OwnerDocument.CreateElement(sVALUE);
  prmNode.AppendChild(valueNode);
  Result := ScopeObject.OwnerDocument.CreateElement(XmlRpcDataTypeNames[ADataType]);
  valueNode.AppendChild(Result);
end;

{ TBaseArrayStackItem }

destructor TBaseArrayStackItem.Destroy;
begin
  SetLength(FIndexStack,0);
  if Assigned(FItemList) then
    ReleaseDomNode(FItemList);
  inherited Destroy();
end;

function TBaseArrayStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  i : Integer;
  locName : string;
begin
  AReturnList.Clear();
  PushIndex(0);
  try
    locName := '';
    for i := 0 to Pred(GetItemsCount()) do begin
      FindNode(locName);
      AReturnList.Add(locName);
    end;
  finally
    PopIndex();
  end;
  Result := AReturnList.Count;
end;

function TBaseArrayStackItem.PopIndex() : Integer;
begin
  if ( Length(FIndexStack) = 0 ) or ( FIndexStackIDX < 0 ) then
    raise EXmlRpcException.Create('TArrayStackItem.PopIndex() >> No saved index.');
  Result := FIndex;
  FIndex := FIndexStack[FIndexStackIDX];
  Dec(FIndexStackIDX);
end;

function TBaseArrayStackItem.PushIndex(const AValue: Integer): Integer;
begin
  if ( FIndexStackIDX = Length(FIndexStack) ) then begin
    if ( Length(FIndexStack) = 0 ) then
      FIndexStackIDX := -1;
    SetLength(FIndexStack, Length(FIndexStack) + 4);
  end;
  Inc(FIndexStackIDX);
  Result := FIndex;
  FIndex := AValue;
  FIndexStack[FIndexStackIDX] := Result;
end;

{ TSimpleTypeStackItem }

{$WARNINGS OFF} {$HINTS OFF}
function TSimpleTypeStackItem.FindNode(var ANodeName: string): TDOMNode;
begin
  raise EXmlRpcException.CreateFmt(SERR_UnsupportedOperation,['FindNode']);
end;

function TSimpleTypeStackItem.CreateBuffer(
  const AName: string;
  const ADataType: TXmlRpcDataType
) : TDOMNode;
begin
  raise EXmlRpcException.CreateFmt(SERR_UnsupportedOperation,['CreateBuffer']);
end;

function TSimpleTypeStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
begin
  raise EXmlRpcException.CreateFmt(SERR_UnsupportedOperation,['GetScopeItemNames']);
end;
{$WARNINGS ON} {$HINTS ON}


end.
