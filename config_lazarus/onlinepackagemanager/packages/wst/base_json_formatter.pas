{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit base_json_formatter;

interface
uses
  Classes, SysUtils, TypInfo, Contnrs,
  base_service_intf,
  fpjson;

const
  sFORMAT = 'format';
  sCONTENT_TYPE = 'contenttype';
  s_json_ContentType = 'application/json';
  s_json = 'json';
  s_inner_value = 'value';
  
  s_json_code = 'code';
  s_json_error = 'error';
  s_json_id = 'id';
  s_json_message = 'message';
  s_json_method = 'method';
  s_json_name = 'name';
  s_json_params = 'params';
  s_json_result = 'result';
  s_json_version = 'version';
  s_json_rpc_version_10 = '1.0';
  s_json_rpc_version_11 = '1.1';
  
  stNilScope = stBase + 7;
  
type

  TJonRPCVersion = ( jsonRPC_10, jsonRPC_11 );
  TJsonInteger = Integer;
  TEnumIntType = Int64;

  EJsonRpcException = class(EBaseRemoteException)
  end;

  { TStackItem }

  TStackItem = class
  private
    FScopeObject: TJSONData;
    FScopeType: TScopeType;
  protected
    function GetItemCount() : Integer;virtual;
  public
    constructor Create(AScopeObject : TJSONData;AScopeType : TScopeType);
    function FindNode(var ANodeName : string):TJSONData;virtual;abstract;
    function CreateStringBuffer(
      Const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;virtual;abstract;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;virtual;abstract;
    function CreateInt64Buffer(
      Const AName  : string;
      const AValue : Int64
    ) : TJSONData;virtual; abstract;
{$IFDEF HAS_QWORD}
    function CreateUInt64Buffer(
      Const AName  : string;
      const AValue : QWord
    ) : TJSONData;virtual;
{$ENDIF HAS_QWORD}
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;virtual;abstract;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;virtual;abstract;
    function CreateObjectBuffer(const AName : string) : TJSONObject;virtual;abstract;
    function CreateArrayBuffer(const AName : string) : TJSONArray;virtual;abstract;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;virtual;abstract;
    function NilItem(AItem : TJSONData) : TJSONData;virtual;abstract;
    property ScopeObject : TJSONData Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;
    property ItemCount : Integer read GetItemCount;
  end;
  
  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  protected
    function GetDataObject() : TJSONObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(AScopeObject : TJSONObject);
    function FindNode(var ANodeName : string):TJSONData;override;
    function CreateStringBuffer(
      const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;override;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;override;
    function CreateInt64Buffer(
      Const AName  : string;
      const AValue : Int64
    ) : TJSONData; override;
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;override;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;override;
    function CreateObjectBuffer(const AName : string) : TJSONObject;override;
    function CreateArrayBuffer(const AName : string) : TJSONArray;override;
    function NilItem(AItem : TJSONData) : TJSONData;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  end;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  private
    FIndex : Integer;
  protected
    function GetDataObject() : TJSONArray;{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(AScopeObject : TJSONArray);
    function FindNode(var ANodeName : string):TJSONData;override;
    function CreateStringBuffer(
      const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;override;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;override; 
    function CreateInt64Buffer(
      Const AName  : string;
      const AValue : Int64
    ) : TJSONData; override; 
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;override;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;override;
    function CreateObjectBuffer(const AName : string) : TJSONObject;override;
    function CreateArrayBuffer(const AName : string) : TJSONArray;override;
    function NilItem(AItem : TJSONData) : TJSONData;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  end;

  { TNullStackItem }

  TNullStackItem = class(TStackItem)
  private
    procedure RaiseNotApplicable();{$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    constructor Create(AScopeObject : TJSONNull);
    function FindNode(var ANodeName : string):TJSONData;override;
    function CreateStringBuffer(
      const AName  : string;
      const AValue : TJSONStringType
    ) : TJSONData;override;
    function CreateIntBuffer(
      Const AName  : string;
      const AValue : TJsonInteger
    ) : TJSONData;override; 
    function CreateInt64Buffer(
      Const AName  : string;
      const AValue : Int64
    ) : TJSONData; override; 
    function CreateFloatBuffer(
      Const AName  : string;
      const AValue : TJSONFloat
    ) : TJSONData;override;
    function CreateBoolBuffer(
      Const AName  : string;
      const AValue : Boolean
    ) : TJSONData;override;
    function CreateObjectBuffer(const AName : string) : TJSONObject;override;
    function CreateArrayBuffer(const AName : string) : TJSONArray;override;
    function NilItem(AItem : TJSONData) : TJSONData;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  end;

  { TJsonRpcBaseFormatter }

  TJsonRpcBaseFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FPropMngr : IPropertyManager;
    FRootData : TJSONData;
    FSerializationStyle : TSerializationStyle;
    FStack : TObjectStack;
  protected
    function GetRootData() : TJSONObject;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetCurrentScope : String;
    function HasScope():Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure CheckScope();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure ClearStack();
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PushStack(const AScopeObject : TJSONData;const AScopeType : TScopeType);{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    procedure PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumIntType
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutBool(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Boolean
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutAnsiChar(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : AnsiChar
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutWideChar(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideChar
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Int64
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_QWORD}
    procedure PutUInt64(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : QWord
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_QWORD}
    procedure PutStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : String
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF WST_UNICODESTRING}
    procedure PutUnicodeStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : UnicodeString
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF WST_UNICODESTRING}
    procedure PutWideStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideString
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : Extended
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
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
    
    function GetDataBuffer(
      var AName : string;
      out AResBuffer : TJSONData
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumIntType
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    {$IFDEF FPC}
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
    function GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Integer
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    {$ENDIF}
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
  public
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
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
    property CurrentScope : String Read GetCurrentScope;

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

    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  public
    constructor Create();override;
    destructor Destroy();override;
  end;
  
  
implementation
uses 
  jsonparser, imp_utils, wst_consts;

function FindObject(AObject : TJSONObject; const AName : TJSONStringType) : TJSONData;
var
  i : Integer;
begin
  i := AObject.IndexOfName(AName);
  if ( i >= 0 ) then
    Result := AObject.Items[i]
  else
    Result := nil;
end;

{ TJsonRpcBaseFormatter }

function TJsonRpcBaseFormatter.HasScope() : Boolean;
begin
  Result := FStack.AtLeast(1);
end;

procedure TJsonRpcBaseFormatter.CheckScope();
begin
  if not HasScope() then
    Error(SERR_NoScope);
end;

procedure TJsonRpcBaseFormatter.ClearStack();
var
  i, c : Integer;
begin
  c := FStack.Count;
  for I := 1 to c do
    FStack.Pop().Free();
end;

function TJsonRpcBaseFormatter.StackTop() : TStackItem;
begin
  CheckScope();
  Result := FStack.Peek() as TStackItem;
end;

function TJsonRpcBaseFormatter.PopStack() : TStackItem;
begin
  CheckScope();
  Result := FStack.Pop() as TStackItem;
end;

procedure TJsonRpcBaseFormatter.PushStack(const AScopeObject : TJSONData; const AScopeType : TScopeType);
begin
  case AScopeType of
    stObject : FStack.Push(TObjectStackItem.Create(AScopeObject as TJSONObject));
    stArray  : FStack.Push(TArrayStackItem.Create(AScopeObject as TJSONArray));
    stNilScope  : FStack.Push(TNullStackItem.Create(AScopeObject as TJSONNull));
    else
    Assert(False);
  end;
  if not Assigned(FRootData) then
    FRootData := AScopeObject;
end;

function TJsonRpcBaseFormatter.GetRootData() : TJSONObject;
begin
  Result := TJSONObject(FRootData);
end;

procedure TJsonRpcBaseFormatter.PutEnum(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : TEnumIntType
);
begin
  StackTop().CreateStringBuffer(AName,GetEnumName(ATypeInfo,AData));
end;

procedure TJsonRpcBaseFormatter.PutBool(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : Boolean
);
begin
  StackTop().CreateBoolBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.PutAnsiChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: AnsiChar
);
begin
  StackTop().CreateStringBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.PutWideChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideChar
);
begin
  StackTop().CreateStringBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.PutInt64(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : Int64
);
begin
  StackTop().CreateInt64Buffer(AName,AData);
end;

{$IFDEF HAS_QWORD}
procedure TJsonRpcBaseFormatter.PutUInt64(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : QWord
);
begin
  StackTop().CreateUInt64Buffer(AName,AData);
end;
{$ENDIF HAS_QWORD}

procedure TJsonRpcBaseFormatter.PutStr(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : String
);
begin
  StackTop().CreateStringBuffer(AName,AData);
end;

{$IFDEF WST_UNICODESTRING}
procedure TJsonRpcBaseFormatter.PutUnicodeStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: UnicodeString
);
begin
  StackTop().CreateStringBuffer(AName,AData);
end;
{$ENDIF WST_UNICODESTRING}

procedure TJsonRpcBaseFormatter.PutWideStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideString
);
begin
  StackTop().CreateStringBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.PutFloat(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : Extended
);
begin
  StackTop().CreateFloatBuffer(AName,AData);
end;

procedure TJsonRpcBaseFormatter.PutObj(
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData : TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

procedure TJsonRpcBaseFormatter.PutRecord(
  const AName : string;
  const ATypeInfo : PTypeInfo;
  const AData : Pointer
);
begin
  TRemotableRecordEncoder.Save(AData,Self,AName,ATypeInfo);
end;

function TJsonRpcBaseFormatter.GetDataBuffer(
  var AName : string;
  out AResBuffer : TJSONData
) : Boolean;
begin
  AResBuffer := StackTop().FindNode(AName);
  Result := ( AResBuffer <> nil );
end;

function TJsonRpcBaseFormatter.GetEnum(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : TEnumIntType
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := GetEnumValue(ATypeInfo,locBuffer.AsString);
end;

function TJsonRpcBaseFormatter.GetBool(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : Boolean
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsBoolean;
end;

function TJsonRpcBaseFormatter.GetAnsiChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: AnsiChar
) : Boolean;
var
  tmpString : TJSONStringType;
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    tmpString := locBuffer.AsString;
    if ( Length(tmpString) > 0 ) then
      AData := tmpString[1]
    else
      AData := #0;
  end;
end;

function TJsonRpcBaseFormatter.GetWideChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideChar
) : Boolean;
var
  tmpString : TJSONStringType;
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result  then begin
    tmpString := locBuffer.AsString;
    if ( Length(tmpString) > 0 ) then
      AData := tmpString[1]
    else
      AData := #0;
  end;
end;

function TJsonRpcBaseFormatter.GetInt(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : Integer
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsInteger;
end;

function TJsonRpcBaseFormatter.GetInt64(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : Int64
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    if ( locBuffer.JSONType = jtNumber ) and ( TJSONNumber(locBuffer).NumberType = ntInteger ) then
      AData := locBuffer.AsInteger
    else
      AData := Round(locBuffer.AsFloat);
  end;
end;

{$IFDEF HAS_QWORD}
function TJsonRpcBaseFormatter.GetUInt64(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : QWord
) : Boolean;
var
  locBuffer : TJSONData;
  locExtData : TJSONFloat;
  tmp : QWord;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    if ( locBuffer.JSONType = jtNumber ) and ( TJSONNumber(locBuffer).NumberType = ntInteger ) then begin
      AData := locBuffer.AsInteger
    end else begin
      locExtData := locBuffer.AsFloat;
      if ( locExtData > High(Int64) ) then begin
        locExtData := locExtData - High(Int64);
        AData := High(Int64);
        tmp := Round(locExtData);
        AData := AData + tmp;
      end else begin
        AData := Round(locExtData);
      end;
    end;
  end;
end;
{$ENDIF HAS_QWORD}

function TJsonRpcBaseFormatter.GetFloat(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : Extended
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsFloat;
end;

function TJsonRpcBaseFormatter.GetStr(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : String
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsString;
end;

{$IFDEF WST_UNICODESTRING}
function TJsonRpcBaseFormatter.GetUnicodeStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: UnicodeString
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsString;
end;
{$ENDIF WST_UNICODESTRING}

function TJsonRpcBaseFormatter.GetWideStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideString
) : Boolean;
var
  locBuffer : TJSONData;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer.AsString;
end;

function TJsonRpcBaseFormatter.GetObj(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : TObject
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TJsonRpcBaseFormatter.GetRecord(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData : Pointer
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TRemotableRecordEncoder.Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

procedure TJsonRpcBaseFormatter.SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TJsonRpcBaseFormatter.GetSerializationStyle() : TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TJsonRpcBaseFormatter.GetFormatName(): string;
begin
  Result := s_json;
end;

function TJsonRpcBaseFormatter.GetPropertyManager() : IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

function TJsonRpcBaseFormatter.GetCurrentScope : string;
begin
  CheckScope();
  Result := '';
end;

procedure TJsonRpcBaseFormatter.Clear();
begin
  ClearStack();
  FreeAndNil(FRootData);
end;

procedure TJsonRpcBaseFormatter.BeginObject(
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  elt : TJSONObject;
begin
  if HasScope() then
    elt := StackTop().CreateObjectBuffer(AName)
  else
    elt := TJSONObject.Create();
  PushStack(elt,stObject);
end;

procedure TJsonRpcBaseFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
var
  i, j, k : Integer;
  locObj : TJSONData;
begin
  if ( Length(ABounds) < 2 ) then
    Error(SERR_InvalidArrayBounds);
  i := ABounds[0];
  j := ABounds[1];
  k := ( j - i + 1 );
  if ( k < 0 ) then
    Error(SERR_InvalidArrayBounds);
  if HasScope() then
    locObj := StackTop().CreateArrayBuffer(AName)
  else
    locObj := TJSONArray.Create();
  PushStack(locObj,stArray);
end;

procedure TJsonRpcBaseFormatter.NilCurrentScope();
var
  stkItem : TStackItem;
begin
  if not FStack.AtLeast(2) then
    Error(SERR_RootObjectCannotBeNIL);
  stkItem := PopStack();
  try
    PushStack(StackTop().NilItem(stkItem.ScopeObject),stNilScope);
  finally
    stkItem.Free();
  end;
end;

function TJsonRpcBaseFormatter.IsCurrentScopeNil() : Boolean;
begin
  Result := ( StackTop().ScopeType = stNilScope );
end;

procedure TJsonRpcBaseFormatter.EndScope();
begin
  FStack.Pop().Free();
end;

procedure TJsonRpcBaseFormatter.AddScopeAttribute(const AName, AValue : string);
begin
  Put(AName,TypeInfo(string),AValue);
end;

function TJsonRpcBaseFormatter.BeginObjectRead(
  var AScopeName : string;
  const ATypeInfo : PTypeInfo
) : Integer;
var
  locNode : TJSONData;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.FindNode(AScopeName);
  if not Assigned(locNode) then begin
    Result := -1;
    exit;
  end;
  case locNode.JSONType() of
    jtObject : PushStack(locNode,stObject);
    jtNull   : PushStack(locNode,stNilScope);
    else
      Error('object or Nil expected, name : %s.',[AScopeName]);
  end;
  Result := StackTop().GetItemCount();
end;

function TJsonRpcBaseFormatter.BeginArrayRead(
  var AScopeName  : string;
  const ATypeInfo : PTypeInfo;
  const AStyle    : TArrayStyle;
  const AItemName : string
): Integer;
var
  locNode : TJSONData;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.FindNode(AScopeName);
  if ( locNode <> nil ) then begin
    case locNode.JSONType() of
      jtArray : PushStack(locNode,stArray);
      jtNull   : PushStack(locNode,stNilScope);
      else
        Error('array or Nil expected, name : %s.',[AScopeName]);
    end;
    Result := StackTop().GetItemCount();
  end else begin
    Result := -1;
  end;
end;

function TJsonRpcBaseFormatter.GetScopeItemNames(const AReturnList : TStrings) : Integer;
begin
  CheckScope();
  Result := StackTop().GetScopeItemNames(AReturnList);
end;

procedure TJsonRpcBaseFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TJsonRpcBaseFormatter.BeginHeader();
begin

end;

procedure TJsonRpcBaseFormatter.EndHeader();
begin

end;

procedure TJsonRpcBaseFormatter.Put(
  const AName : String;
  const ATypeInfo : PTypeInfo;
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
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := String(AData);
        PutStr(AName,ATypeInfo,strData);
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := UnicodeString(AData);
        PutUnicodeStr(AName,ATypeInfo,unicodeStrData);
      end;
{$ENDIF WST_UNICODESTRING}
    tkWString :
      begin
        wideStrData := WideString(AData);
        PutWideStr(AName,ATypeInfo,wideStrData);
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
      begin
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
        PutFloat(AName,ATypeInfo,floatDt);
      End;
  End;
end;

procedure TJsonRpcBaseFormatter.Put(
  const ANameSpace : string;
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData
);
begin
  Put(AName,ATypeInfo,AData);
end;

procedure TJsonRpcBaseFormatter.PutScopeInnerValue(const ATypeInfo : PTypeInfo; const AData);
var
  locName : string;
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  locName := s_inner_value;
  Case ATypeInfo^.Kind Of
    tkChar :
      begin
        ansiCharData := AnsiChar(AData);
        PutAnsiChar(locName,ATypeInfo,ansiCharData);
      end;
    tkWChar :
      begin
        wideCharData := WideChar(AData);
        PutWideChar(locName,ATypeInfo,wideCharData);
      end;
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := String(AData);
        PutStr(locName,ATypeInfo,strData);
      End;
    tkWString :
      begin
        wideStrData := WideString(AData);
        PutWideStr(locName,ATypeInfo,wideStrData);
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := UnicodeString(AData);
        PutUnicodeStr(locName,ATypeInfo,unicodeStrData);
      end;
{$ENDIF WST_UNICODESTRING}
    tkInt64 :
      Begin
        int64Data := Int64(AData);
        PutInt64(locName,ATypeInfo,int64Data);
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := QWord(AData);
        PutUInt64(locName,ATypeInfo,uint64Data);
      End;
{$ENDIF HAS_QWORD}
    tkClass, tkRecord :
      begin
        raise EJsonRpcException.Create('Inner Scope value must be a "simple type" value.');
      end;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := Boolean(AData);
        PutBool(locName,ATypeInfo,boolData);
      End;
    {$ENDIF}
    tkInteger, tkEnumeration :
      begin
      {$IFDEF WST_DELPHI}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          boolData := Boolean(AData);
          PutBool(locName,ATypeInfo,boolData);
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
            PutInt64(locName,ATypeInfo,enumData)
          Else
            PutEnum(locName,ATypeInfo,enumData);
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
        PutFloat(locName,ATypeInfo,floatDt);
      End;
  End;
end;

function TJsonRpcBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  var AName : String;
  var AData
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
  WideStrData : WideString;
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
          String(AData) := strData;
      End;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      Begin
        unicodeStrData := '';
        Result := GetUnicodeStr(ATypeInfo,AName,unicodeStrData);
        if Result then
          UnicodeString(AData) := unicodeStrData;
      End;
{$ENDIF WST_UNICODESTRING}
    tkWString :
      Begin
        WideStrData := '';
        Result := GetWideStr(ATypeInfo,AName,WideStrData);
        if Result then
          WideString(AData) := WideStrData;
      End;
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
            case GetTypeData(ATypeInfo)^.OrdType of
              otSByte : ShortInt(AData)    := enumData;
              otUByte : Byte(AData)        := enumData;
              otSWord : SmallInt(AData)    := enumData;
              otUWord : Word(AData)        := enumData;
              otSLong : LongInt(AData)     := enumData;
              otULong : LongWord(AData)    := enumData;
            end;
          end;
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      End;
    tkFloat :
      Begin
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
      End;
  End;
end;

function TJsonRpcBaseFormatter.Get(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var AName : string;
  var AData
) : Boolean;
begin
  Result := Get(ATypeInfo,AName,AData);
end;

procedure TJsonRpcBaseFormatter.GetScopeInnerValue(const ATypeInfo : PTypeInfo; var AData);
var
  locName : string;
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  boolData : Boolean;
  enumData : TEnumIntType;
  floatDt : Extended;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  locName := s_inner_value;
  Case ATypeInfo^.Kind Of
    tkChar :
      begin
        ansiCharData := #0;
        GetAnsiChar(ATypeInfo,locName,ansiCharData);
        AnsiChar(AData) := ansiCharData;
      end;
    tkWChar :
      begin
        wideCharData := #0;
        GetWideChar(ATypeInfo,locName,wideCharData);
        WideChar(AData) := wideCharData;
      end;
    tkInt64 :
      Begin
        int64Data := 0;
        GetInt64(ATypeInfo,locName,int64Data);
        Int64(AData) := int64Data;
      End;
{$IFDEF HAS_QWORD}
    tkQWord :
      Begin
        uint64Data := 0;
        GetUInt64(ATypeInfo,locName,uint64Data);
        QWord(AData) := uint64Data;
      End;
{$ENDIF HAS_QWORD}
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      Begin
        strData := '';
        GetStr(ATypeInfo,locName,strData);
        String(AData) := strData;
      End;
    tkWString :
      begin
        wideStrData := '';
        GetWideStr(ATypeInfo,locName,wideStrData);
        WideString(AData) := wideStrData;
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := '';
        GetUnicodeStr(ATypeInfo,locName,unicodeStrData);
        UnicodeString(AData) := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
    tkClass, tkRecord :
      Begin
        raise EJsonRpcException.Create('Inner Scope value must be a "simple type" value.');
      End;
    {$IFDEF FPC}
    tkBool :
      Begin
        boolData := False;
        GetBool(ATypeInfo,locName,boolData);
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
          GetBool(ATypeInfo,locName,boolData);
          Boolean(AData) := boolData;
        end else begin
      {$ENDIF}
          enumData := 0;
          If ( ATypeInfo^.Kind = tkInteger ) Then
            GetInt64(ATypeInfo,locName,enumData)
          Else
            GetEnum(ATypeInfo,locName,enumData);
          Case GetTypeData(ATypeInfo)^.OrdType Of
            otSByte : ShortInt(AData)    := enumData;
            otUByte : Byte(AData)        := enumData;
            otSWord : SmallInt(AData)    := enumData;
            otUWord : Word(AData)        := enumData;
            otSLong : LongInt(AData)     := enumData;
            otULong : LongWord(AData)    := enumData;
          End;
      {$IFDEF WST_DELPHI}
        end;
      {$ENDIF}
      End;
    tkFloat :
      Begin
        floatDt := 0;
        GetFloat(ATypeInfo,locName,floatDt);
        Case GetTypeData(ATypeInfo)^.FloatType Of
          ftSingle : Single(AData)    := floatDt;
          ftDouble : Double(AData)    := floatDt;
          ftExtended : Extended(AData)    := floatDt;
          ftCurr : Currency(AData)    := floatDt;
{$IFDEF HAS_COMP}
          ftComp : Comp(AData)    := floatDt;
{$ENDIF}
        End;
      End;
  End;
end;

function TJsonRpcBaseFormatter.ReadBuffer(
  const AName : string;
  out AResBuffer : string
) : Boolean;
var
  locName : string;
  locBuffer : TJSONData;
begin
  locName := AName;
  Result := GetDataBuffer(locName,locBuffer);
  if Result then
    AResBuffer := locBuffer.AsJSON;
end;

procedure TJsonRpcBaseFormatter.WriteBuffer(const AValue: string);
var
  locStream : TStream;
  locParser : TJSONParser;
  locObject : TJSONData;
begin
  locParser := nil;
  locStream := TStringStream.Create(AValue);
  try
    locParser := TJSONParser.Create(locStream);
    locObject := locParser.Parse();
    try
      case StackTop().ScopeObject.JSONType() of
        jtObject : TJSONObject(StackTop().ScopeObject).Add('__buffer__',locObject);
        jtArray  : TJSONArray(StackTop().ScopeObject).Add(locObject);
        else
          Error('Invalid JSON buffer : Object or Array expected.');
      end;
    except
      FreeAndNil(locObject);
      raise;
    end;
  finally
    locParser.Free();
    locStream.Free();
  end;
end;

procedure TJsonRpcBaseFormatter.SaveToStream(AStream : TStream);
var
  locBuffer : UTF8String;
begin
  if Assigned(FRootData) then begin
    locBuffer := FRootData.AsJSON;
    AStream.WriteBuffer(locBuffer[1],Length(locBuffer));
  end;
end;

procedure TJsonRpcBaseFormatter.LoadFromStream(AStream : TStream);
var
  locParser : TJSONParser;
  locObject : TJSONData;
begin
  ClearStack();
  FSerializationStyle := Low(TSerializationStyle);
  locParser := TJSONParser.Create(AStream);
  try
    locObject := locParser.Parse();
    if Assigned(locObject) then begin
      FreeAndNil(FRootData); // it will be set in PushStack()
      case locObject.JSONType() of
        jtObject : PushStack(locObject,stObject);
        jtArray  : PushStack(locObject,stArray);
        else
          Error('Invalid JSON buffer : Object or Array expected.');
      end;
    end else begin
      FreeAndNil(FRootData);
    end;
  finally
    locParser.Free();
  end;
end;

procedure TJsonRpcBaseFormatter.Error(const AMsg : string);
begin
  raise EJsonRpcException.Create(AMsg);
end;

procedure TJsonRpcBaseFormatter.Error(const AMsg : string; const AArgs : array of const);
begin
  raise EJsonRpcException.CreateFmt(AMsg,AArgs);
end;

constructor TJsonRpcBaseFormatter.Create();
begin
  inherited Create();
  FStack := TObjectStack.Create();
end;

destructor TJsonRpcBaseFormatter.Destroy();
begin
  FStack.Free();
  FreeAndNil(FRootData);
  inherited Destroy();
end;

{ TStackItem }

function TStackItem.GetItemCount() : Integer;
begin
  Result := FScopeObject.Count;
end;

constructor TStackItem.Create(AScopeObject : TJSONData; AScopeType : TScopeType);
begin
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

{$IFDEF HAS_QWORD}
function TStackItem.CreateUInt64Buffer(
  const AName  : string;
  const AValue : QWord
) : TJSONData;
begin
  Result := CreateFloatBuffer(AName,AValue);
end;
{$ENDIF HAS_QWORD}

{ TObjectStackItem }

function TObjectStackItem.GetDataObject() : TJSONObject;
begin
  Result := TJSONObject(ScopeObject);
end;

constructor TObjectStackItem.Create(AScopeObject : TJSONObject);
begin
  inherited Create(AScopeObject,stObject);
end;

function TObjectStackItem.FindNode(var ANodeName : string) : TJSONData;
begin
  Result := FindObject(GetDataObject(),ANodeName);
end;

function TObjectStackItem.CreateStringBuffer(
  const AName  : string;
  const AValue : TJSONStringType
) : TJSONData;
var
  locObj : TJSONObject;
  i : Integer;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName);
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsString := AValue;
  end;
end;

function TObjectStackItem.CreateIntBuffer(
  const AName : string;
  const AValue : TJsonInteger
) : TJSONData;
var
  locObj : TJSONObject;
  i : Integer;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName);
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsInteger := AValue;
  end;
end;

function TObjectStackItem.CreateInt64Buffer(
  const AName : string;  
  const AValue : Int64
) : TJSONData;  
{$IFDEF WST_HAS_JSON_INT64}     
var
  locObj : TJSONObject;
  i : Integer;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName);
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsInt64 := AValue;
  end;   
end;
{$ELSE WST_HAS_JSON_INT64}
begin
  Result := CreateFloatBuffer(AName,AValue);
end;
{$ENDIF WST_HAS_JSON_INT64}

function TObjectStackItem.CreateFloatBuffer(
  const AName : string;
  const AValue : TJSONFloat
) : TJSONData;
var
  locObj : TJSONObject;
  i : Integer;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName);
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsFloat := AValue;
  end;
end;

function TObjectStackItem.CreateBoolBuffer(
  const AName : string;
  const AValue : Boolean
) : TJSONData;
var
  locObj : TJSONObject;
  i : Integer;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName);
  if ( Result = nil ) then begin
    i := locObj.Add(AName,AValue);
    Result := locObj.Items[i];
  end else begin
    Result.AsBoolean := AValue;
  end;
end;

function TObjectStackItem.CreateObjectBuffer(const AName : string) : TJSONObject;
var
  locObj : TJSONObject;
  locIndex : Integer;
begin
  locObj := GetDataObject();
  locIndex := locObj.IndexOfName(AName);
  if ( locIndex = -1 ) then begin
    Result := TJSONObject.Create();
    locObj.Add(AName,Result);
  end else begin
    Result := locObj.Items[locIndex] as TJSONObject;
  end;
end;

function TObjectStackItem.CreateArrayBuffer(const AName : string) : TJSONArray;
var
  locObj : TJSONObject;
begin
  locObj := GetDataObject();
  Result := FindObject(locObj,AName) as TJSONArray;
  if ( Result = nil ) then begin
    Result := TJSONArray.Create();
    locObj.Add(AName,Result);
  end;
end;

function TObjectStackItem.NilItem(AItem : TJSONData) : TJSONData;
var
  locPos : Integer;
begin
  locPos := GetDataObject().IndexOf(AItem);
  if ( locPos < 0 ) then
    raise EJsonRpcException.Create('Can not "Nil" an object not owned.');
  Result := TJSONNull.Create();
  try
    GetDataObject().Items[locPos] := Result;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TObjectStackItem.GetScopeItemNames(const AReturnList : TStrings) : Integer;
var
  i, c : Integer;
  locObj : TJSONObject;
begin
  AReturnList.Clear();
  locObj := GetDataObject();
  c := locObj.Count;
  for i := 0 to Pred(c) do begin
    AReturnList.Add(locObj.Names[i]);
  end;
  Result := AReturnList.Count;
end;

{ TArrayStackItem }

function TArrayStackItem.GetDataObject() : TJSONArray;
begin
  Result := TJSONArray(ScopeObject);
end;

constructor TArrayStackItem.Create(AScopeObject : TJSONArray);
begin
  inherited Create(AScopeObject,stArray);
end;

function TArrayStackItem.FindNode(var ANodeName : string) : TJSONData;
begin
  if ( FIndex >= GetDataObject().Count ) then
    raise EJsonRpcException.CreateFmt('Index out of bound : %d; Node Name = "%s".',[FIndex,ANodeName]);
  Result:= GetDataObject().Items[FIndex];
  Inc(FIndex);
end;

function TArrayStackItem.CreateStringBuffer(const AName : string; const AValue : TJSONStringType) : TJSONData;
begin
  Result := GetDataObject().Items[GetDataObject().Add(AValue)];
end;

function TArrayStackItem.CreateIntBuffer(const AName : string; const AValue : TJsonInteger) : TJSONData;
begin
  Result := GetDataObject().Items[GetDataObject().Add(AValue)];
end;

function TArrayStackItem.CreateInt64Buffer(
  const AName : string;  
  const AValue : Int64
) : TJSONData;  
begin
{$IFDEF WST_HAS_JSON_INT64} 
  Result := GetDataObject().Items[GetDataObject().Add(AValue)];
{$ELSE WST_HAS_JSON_INT64} 
  Result := CreateFloatBuffer(AName,AValue);
{$ENDIF WST_HAS_JSON_INT64} 
end;

function TArrayStackItem.CreateFloatBuffer(const AName : string; const AValue : TJSONFloat) : TJSONData;
begin
  Result := GetDataObject().Items[GetDataObject().Add(AValue)];
end;

function TArrayStackItem.CreateBoolBuffer(const AName : string; const AValue : Boolean) : TJSONData;
begin
  Result := GetDataObject().Items[GetDataObject().Add(AValue)];
end;

function TArrayStackItem.CreateObjectBuffer(const AName : string) : TJSONObject;
begin
  Result := TJSONObject.Create();
  try
    GetDataObject().Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TArrayStackItem.CreateArrayBuffer(const AName : string) : TJSONArray;
begin
  Result := TJSONArray.Create();
  try
    GetDataObject().Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TArrayStackItem.NilItem(AItem : TJSONData) : TJSONData;
var
  locPos : Integer;
begin
  locPos := GetDataObject().IndexOf(AItem);
  if ( locPos < 0 ) then
    raise EJsonRpcException.Create('Can not "Nil" an object not owned.');
  Result := TJSONNull.Create();
  try
    GetDataObject().Items[locPos] := Result;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TArrayStackItem.GetScopeItemNames(const AReturnList : TStrings) : Integer;
var
  i : Integer;
begin
  AReturnList.Clear();
  for i := 1 to GetDataObject().Count do
    AReturnList.Add('i');
  Result := AReturnList.Count;
end;

{ TNullStackItem }

procedure TNullStackItem.RaiseNotApplicable();
begin
  raise EJsonRpcException.Create('Operation not applicable at a NULL object.');
end;

constructor TNullStackItem.Create(AScopeObject : TJSONNull);
begin
  inherited Create(AScopeObject,stNilScope);
end;

function TNullStackItem.FindNode(var ANodeName : string) : TJSONData;
begin
  Result := nil;
end;

{$WARNINGS OFF} {$HINTS OFF}
function TNullStackItem.CreateStringBuffer(const AName : string; const AValue : TJSONStringType) : TJSONData;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.CreateIntBuffer(const AName : string; const AValue : TJsonInteger) : TJSONData;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.CreateInt64Buffer(const AName : string; const AValue : Int64) : TJSONData;  
begin
  RaiseNotApplicable();   
end;

function TNullStackItem.CreateFloatBuffer(const AName : string; const AValue : TJSONFloat) : TJSONData;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.CreateBoolBuffer(const AName : string; const AValue : Boolean) : TJSONData;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.CreateObjectBuffer(const AName : string) : TJSONObject;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.CreateArrayBuffer(const AName : string) : TJSONArray;
begin
  RaiseNotApplicable();
end;

function TNullStackItem.NilItem(AItem : TJSONData) : TJSONData;
begin
  RaiseNotApplicable();
end;
{$WARNINGS ON} {$HINTS ON}

function TNullStackItem.GetScopeItemNames(const AReturnList : TStrings) : Integer;
begin
  Result := 0;
end;

end.

