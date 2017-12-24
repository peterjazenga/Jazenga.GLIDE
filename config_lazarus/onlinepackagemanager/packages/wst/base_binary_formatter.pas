{   This file is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).
    

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit base_binary_formatter;

interface

uses
  Classes, SysUtils, Contnrs, TypInfo,
  base_service_intf, binary_streamer, wst_types;

{$DEFINE wst_binary_header}
  
const
  sBINARY_FORMAT_NAME = 'wst-binary';
  sROOT   = 'ROOT';
  sSCOPE_INNER_NAME = 'INNER_VAL';
  sFORMAT = 'format';
{$IFDEF wst_binary_header}
  sHEADER = 'HEADER';
{$ENDIF}

type
  EBinaryFormatterException = class(EServiceException)
  end;

  EBinaryException = class(EBaseRemoteException)
  end;
  
  TDataName = String;
  TDataType = (
    dtInt8U  = 1,    dtInt8S = 2,
    dtInt16U = 3,   dtInt16S = 4,
    dtInt32U = 5,   dtInt32S = 6,
    dtInt64U = 7,   dtInt64S = 8,
    dtBool   = 9,
    dtAnsiChar   = 10, dtWideChar   = 11, dtEnum     = 12,
    dtSingle     = 13, dtDouble     = 14, dtExtended = 15, dtCurrency = 16,
    dtAnsiString = 17, dtWideString = 18,
{ $IFDEF WST_UNICODESTRING}
    dtUnicodeString = 19,
{ $ENDIF WST_UNICODESTRING}
    dtObject = 30, dtArray = 31
  );
const
  dtDefaultString =
    {$IFDEF WST_UNICODESTRING}
      {$IFDEF WST_DELPHI}
        dtUnicodeString
      {$ENDIF WST_DELPHI}
      {$IFDEF FPC}
        dtAnsiString
      {$ENDIF FPC}
    {$ELSE WST_UNICODESTRING}
      dtAnsiString
    {$ENDIF WST_UNICODESTRING}
    ;

type

  PAnsiStringBuffer = ^TAnsiStringBuffer;
  PWideStringBuffer = ^TWideStringBuffer;
{ $IFDEF WST_UNICODESTRING}
  PUnicodeStringBuffer = ^TUnicodeStringBuffer;
{ $ENDIF WST_UNICODESTRING}
  PObjectBuffer = ^TObjectBuffer;
  PArrayBuffer = ^TArrayBuffer;
  PDataBuffer = ^TDataBuffer;

  TDataBuffer = Record
    Name : TDataName;
    Case DataType : TDataType of
      dtInt8S    : ( Int8S : TInt8S );
      dtInt8U    : ( Int8U : TInt8U );
      dtInt16U   : ( Int16U : TInt16U );
      dtInt16S   : ( Int16S : TInt16S );
      dtInt32U   : ( Int32U : TInt32U );
      dtInt32S   : ( Int32S : TInt32S );
      dtInt64U   : ( Int64U : TInt64U );
      dtInt64S   : ( Int64S : TInt64S );
      dtBool     : ( BoolData : TBoolData );
      dtAnsiChar : ( AnsiCharData : TAnsiCharacter; );
      dtWideChar : ( WideCharData : TWideCharacter; );
      dtEnum     : ( EnumData : TEnumData );
      dtSingle   : ( SingleData : TFloat_Single_4 );
      dtDouble   : ( DoubleData : TFloat_Double_8 );
      dtExtended   : ( ExtendedData : TFloat_Extended_10 );
      dtCurrency   : ( CurrencyData : TFloat_Currency_8 );
      
      dtAnsiString : ( AnsiStrData : PAnsiStringBuffer );
      dtWideString : ( WideStrData : PWideStringBuffer );
{ $IFDEF WST_UNICODESTRING}
      dtUnicodeString : ( UnicodeStrData : PUnicodeStringBuffer );
{ $ENDIF WST_UNICODESTRING}
      dtObject   : ( ObjectData : PObjectBuffer );
      dtArray    : ( ArrayData : PArrayBuffer );
  End;

  TAnsiStringBuffer = record
    Data : TAnsiStringData;
  end;

  TWideStringBuffer = record
    Data : TWideStringData;
  end;

{ $IFDEF WST_UNICODESTRING}
  TUnicodeStringBuffer = record
    Data : TUnicodeStringData;
  end;
{ $ENDIF WST_UNICODESTRING}



  PObjectBufferItem = ^TObjectBufferItem;
  TObjectBufferItem = Record
    Data : PDataBuffer;
    Next : PObjectBufferItem;
  End;

  TObjectBuffer = Record
    NilObject   : TBoolData;
    Count       : Integer;
    Head        : PObjectBufferItem;
    Last        : PObjectBufferItem;
    Attributes  : PObjectBuffer;
    InnerData   : PDataBuffer;
  End;

  PDataBufferList = ^TDataBufferList;
  TDataBufferList = array[0..MAX_ARRAY_LENGTH] of PDataBuffer;
  TArrayBuffer = Record
    Count : Integer;
    Items : PDataBufferList;
    Attributes  : PObjectBuffer;
  End;

  { TStackItem }

  TStackItem = class
  private
    FScopeObject: PDataBuffer;
    FScopeType: TScopeType;
  Public
    constructor Create(const AScopeObject : PDataBuffer;AScopeType : TScopeType);
    function GetItemCount():Integer;virtual;abstract;
    function Find(var AName : TDataName):PDataBuffer;virtual;abstract;
    function GetByIndex(const AIndex : Integer):PDataBuffer;virtual;abstract;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;virtual;abstract;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;virtual;abstract;
    function GetInnerBuffer():PDataBuffer;virtual;abstract;
    procedure NilCurrentScope();virtual;abstract;
    function IsCurrentScopeNil():Boolean;virtual;abstract;
    property ScopeObject : PDataBuffer Read FScopeObject;
    property ScopeType : TScopeType Read FScopeType;

    function GetScopeItemNames(const AReturnList : TStrings) : Integer;virtual;abstract;
  End;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    constructor Create(const AScopeObject : PDataBuffer);
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;override;
    function GetInnerBuffer():PDataBuffer;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  End;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  Private
    FIndex : Integer;
  Public
    constructor Create(const AScopeObject : PDataBuffer);
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;override;
    function GetInnerBuffer():PDataBuffer;overload;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  End;

  { TBaseBinaryFormatter }

  TBaseBinaryFormatter = class(TSimpleFactoryItem,IFormatterBase)
  private
    FRootData : PDataBuffer;
    FStack : TObjectStack;
    FSerializationStyle : TSerializationStyle;
    FPropMngr : IPropertyManager;
    {$IFDEF wst_binary_header}
    FHeaderEnterCount : Integer;
    {$ENDIF}
  protected
    function GetCurrentScope: String;
    function GetCurrentScopeObject():PDataBuffer;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
  protected
    function HasScope():Boolean;
    procedure CheckScope();
    procedure ClearStack();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PushStack(AScopeObject : PDataBuffer;Const AScopeType : TScopeType = stObject);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetRootData() : PDataBuffer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    procedure PutFloat(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TFloat_Extended_10
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutInt(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TInt64S
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutAnsiStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : AnsiString
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutWideStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : WideString
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{ $IFDEF WST_UNICODESTRING}
    procedure PutUnicodeStr(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : UnicodeString
    );{$IFDEF USE_INLINE}inline;{$ENDIF}
{ $ENDIF WST_UNICODESTRING}
    procedure PutEnum(
      Const AName     : String;
      Const ATypeInfo : PTypeInfo;
      Const AData     : TEnumData
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
      out AResultBuffer : PDataBuffer
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetEnum(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TEnumData
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBool(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : Boolean
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
    function GetFloat(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TFloat_Extended_10
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetInt(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : TInt64S
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
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
{$ENDIF}
    function GetAnsiStr(
      Const ATypeInfo : PTypeInfo;
      Var   AName     : String;
      Var   AData     : AnsiString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetWideStr(
      const ATypeInfo : PTypeInfo;
      var   AName     : String;
      var   AData     : WideString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{ $IFDEF WST_UNICODESTRING}
    function GetUnicodeStr(
      const ATypeInfo : PTypeInfo;
      var   AName     : String;
      var   AData     : UnicodeString
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{ $ENDIF WST_UNICODESTRING}
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

    property CurrentScope : String Read GetCurrentScope;

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
    ) : Boolean; overload;
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
  End;

  TDBGPinterProc = procedure(const AMsg:string);
  
  procedure ClearObj(const AOwner: PDataBuffer);
  procedure FreeObjectBuffer(var ABuffer : PDataBuffer); overload;
  function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
  procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
  function CreateArrayBuffer(
    const ALength   : Integer;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;
  function CreateObjBuffer(
    const ADataType : TDataType;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;
  
  procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);

implementation
uses
  imp_utils, wst_consts;
  
{$INCLUDE wst_rtl_imp.inc}

procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);
Var
  p : PObjectBufferItem;
  s : string;
  i ,j: Integer;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  s := StringOfChar(' ',ALevel);
  Case ARoot^.DataType Of
    dtInt8S   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8S) );
    dtInt8U   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8U) );
    dtInt32U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32U) );
    dtInt32S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32S) );
    dtInt64U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64U) );
    dtInt64S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64S) );
    
    dtSingle  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.SingleData) );
    dtDouble  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.DoubleData) );
    dtExtended  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.ExtendedData) );
    dtCurrency  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.CurrencyData) );
    
    dtAnsiString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.AnsiStrData^.Data );
    dtWideString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.WideStrData^.Data );
{ $IFDEF WST_UNICODESTRING}
    dtUnicodeString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.UnicodeStrData^.Data );
{ $ENDIF WST_UNICODESTRING}
    dtObject  :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ObjectData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          APrinterProc('( ' + IntToStr(ARoot^.ObjectData^.Count) + ' Objects )');
          p := ARoot^.ObjectData^.Head;
          i := ALevel + 1;
          While Assigned(p) Do Begin
            PrintObj(p^.Data,i,APrinterProc);
            p := p^.Next;
          End;
        End;
      End;
    dtArray :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ArrayData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          j := ARoot^.ArrayData^.Count;
          APrinterProc('( Objects[ '+ IntToStr(j)+ '] )');
          i := ALevel + 1;
          For j := 0 To Pred(j) Do Begin
            PrintObj(ARoot^.ArrayData^.Items^[j],i,APrinterProc);
          End;
        End;
      End;
  End;
End;

function FindObj(const AOwner: PDataBuffer; const AName : TDataName) : PDataBuffer;
Var
  p : PObjectBufferItem;
Begin
  Assert(AOwner^.DataType >= dtObject);
  Result := Nil;
   p:= AOwner^.ObjectData^.Head;
  While Assigned(p) Do Begin
    If AnsiSameText(AName,p^.Data^.Name) Then Begin
      Result := p^.Data;
      Exit;
    End;
    p := p^.Next;
  End;
End;

procedure AddObj(
  const AOwner, AChildData: PDataBuffer;
  const AIndex : Integer = -1
);
Var
  p : PObjectBufferItem;
Begin
  If ( AOwner^.DataType = dtObject ) Then Begin
    p := wst_GetMem(SizeOf(TObjectBufferItem));
    p^.Data := AChildData;
    p^.Next := Nil;
    If Assigned(AOwner^.ObjectData^.Head) Then Begin
      AOwner^.ObjectData^.Last^.Next := p;
    End Else Begin
      AOwner^.ObjectData^.Head := p;
    End;
    AOwner^.ObjectData^.Last := p;
    Inc(AOwner^.ObjectData^.Count);
  End Else If ( AOwner^.DataType = dtArray ) Then Begin
    If ( AIndex >= 0 ) And ( AIndex < AOwner^.ArrayData^.Count ) Then
      AOwner^.ArrayData^.Items^[AIndex] := AChildData
    Else
      Raise EBinaryFormatterException.CreateFmt(SERR_IndexOutOfBound,[AIndex])
  End Else Begin
    Raise EBinaryFormatterException.CreateFmt(SERR_InvalidDataTypeInContext,[IntToStr(Ord(AOwner^.DataType))])
  End;
End;

function CreateObjBuffer(
  const ADataType : TDataType;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
var
  resLen, i : Integer;
begin
  resLen := SizeOf(TDataBuffer);
  Result := wst_GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := ADataType;
    Case Result^.DataType Of
      dtAnsiString :
        Begin
          i := SizeOf(TAnsiStringBuffer);
          Result^.AnsiStrData := wst_GetMem(i);
          FillChar(Result^.AnsiStrData^,i,#0);
          Result^.AnsiStrData^.Data := '';
        End;
      dtWideString :
        begin
          i := SizeOf(TWideStringBuffer);
          Result^.WideStrData := wst_GetMem(i);
          FillChar(Result^.WideStrData^,i,#0);
          Result^.WideStrData^.Data := '';
        end;
{ $IFDEF WST_UNICODESTRING}
      dtUnicodeString :
        begin
          i := SizeOf(TUnicodeStringBuffer);
          Result^.UnicodeStrData := wst_GetMem(i);
          FillChar(Result^.UnicodeStrData^,i,#0);
          Result^.UnicodeStrData^.Data := '';
        end;
{ $ENDIF WST_UNICODESTRING}
      dtObject :
        Begin
          Result^.ObjectData := wst_GetMem(SizeOf(TObjectBuffer));
          FillChar(Result^.ObjectData^,SizeOf(TObjectBuffer),#0);
        End;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Raise;
  End;
end;

function CreateArrayBuffer(
  const ALength   : Integer;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
Var
  i, resLen : Integer;
begin
  Assert(ALength>=0);
  resLen := SizeOf(TDataBuffer);
  Result := wst_GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := dtArray;
    Result^.ArrayData := wst_GetMem(SizeOf(TArrayBuffer));
    FillChar(Result^.ArrayData^,SizeOf(TArrayBuffer),#0);
    Result^.ArrayData^.Count := ALength;
    If ( ALength > 0 ) Then Begin
      i := ALength*SizeOf(PDataBuffer);
      Result^.ArrayData^.Items := wst_GetMem(i);
      FillChar(Result^.ArrayData^.Items^[0],i,#0);
    End Else Begin
      Result^.ArrayData^.Items := Nil;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Raise;
  End;
end;

procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
Var
  p : PObjectBufferItem;
  i : TInt32S;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  i := Ord(ARoot^.DataType);
  ADest.WriteInt32S(i);
  ADest.WriteAnsiStr(ARoot^.Name);
  Case ARoot^.DataType Of
    dtInt8S  : ADest.WriteInt8S(ARoot^.Int8S);
      dtInt8U  : ADest.WriteInt8U(ARoot^.Int8U);
    dtInt16U  : ADest.WriteInt16U(ARoot^.Int16U);
      dtInt16S  : ADest.WriteInt16S(ARoot^.Int16S);
    dtInt32U  : ADest.WriteInt32U(ARoot^.Int32U);
      dtInt32S  : ADest.WriteInt32S(ARoot^.Int32S);
    dtInt64U  : ADest.WriteInt64U(ARoot^.Int64U);
      dtInt64S  : ADest.WriteInt64S(ARoot^.Int64S);
      
    dtSingle  : ADest.WriteSingle(ARoot^.SingleData);
    dtDouble  : ADest.WriteDouble(ARoot^.DoubleData);
    dtExtended  : ADest.WriteExtended(ARoot^.ExtendedData);
    dtCurrency  : ADest.WriteCurrency(ARoot^.CurrencyData);
    
    dtAnsiString  : ADest.WriteAnsiStr(ARoot^.AnsiStrData^.Data);
    dtWideString  : ADest.WriteWideStr(ARoot^.WideStrData^.Data);
{ $IFDEF WST_UNICODESTRING}
    dtUnicodeString  : ADest.WriteUnicodeStr(ARoot^.UnicodeStrData^.Data);
{ $ENDIF WST_UNICODESTRING}
    dtBool    : ADest.WriteBool(ARoot^.BoolData);
    dtAnsiChar    : ADest.WriteAnsiChar(ARoot^.AnsiCharData);
    dtWideChar    : ADest.WriteWideChar(ARoot^.WideCharData);
    dtEnum    : ADest.WriteEnum(ARoot^.EnumData);
    dtObject :
      Begin
        ADest.WriteBool(ARoot^.ObjectData^.NilObject) ;
        if not ARoot^.ObjectData^.NilObject then begin
          i := ARoot^.ObjectData^.Count;
          ADest.WriteInt32S(i);

          If ( i > 0 ) Then Begin
            p := ARoot^.ObjectData^.Head;
            For i := 1 To i Do Begin
              SaveObjectToStream(p^.Data,ADest);
              p := p^.Next;
            End;
          End;
          ADest.WriteBool(Assigned(ARoot^.ObjectData^.InnerData));
          if Assigned(ARoot^.ObjectData^.InnerData) then
            SaveObjectToStream(ARoot^.ObjectData^.InnerData,ADest);
        end;
      End;
    dtArray :
      Begin
        i := ARoot^.ArrayData^.Count;
        ADest.WriteInt32S(i);

        If ( i > 0 ) Then Begin
          For i := 0 To Pred(i) Do Begin
            SaveObjectToStream(ARoot^.ArrayData^.Items^[i],ADest);
          End;
        End;
      End;
  End;
End;

function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
Var
  i : TInt32S;
  s : string;
Begin
  Result := Nil;
  If AStoreRdr.IsAtEof() Then
    Exit;
  i := AStoreRdr.ReadInt32S();
  s := AStoreRdr.ReadAnsiStr();
  If ( TDataType(i) < dtArray ) Then
    Result := CreateObjBuffer(TDataType(i),s);
  Case TDataType(i) Of
    dtInt8S   : Result^.Int8S := AStoreRdr.ReadInt8S();
    dtInt8U   : Result^.Int8U := AStoreRdr.ReadInt8U();
    dtInt16U  : Result^.Int16U := AStoreRdr.ReadInt16U();
    dtInt16S  : Result^.Int16S := AStoreRdr.ReadInt16S();
    dtInt32U  : Result^.Int32U := AStoreRdr.ReadInt32U();
    dtInt32S  : Result^.Int32S := AStoreRdr.ReadInt32S();
    dtInt64U  : Result^.Int64U := AStoreRdr.ReadInt64U();
    dtInt64S  : Result^.Int64S := AStoreRdr.ReadInt64S();
    
    dtSingle  : Result^.SingleData := AStoreRdr.ReadSingle();
    dtDouble  : Result^.DoubleData := AStoreRdr.ReadDouble();
    dtExtended  : Result^.ExtendedData := AStoreRdr.ReadExtended();
    dtCurrency  : Result^.CurrencyData := AStoreRdr.ReadCurrency();
    
    dtAnsiString  : Result^.AnsiStrData^.Data := AStoreRdr.ReadAnsiStr();
    dtWideString  : Result^.WideStrData^.Data := AStoreRdr.ReadWideStr();
{ $IFDEF WST_UNICODESTRING}
    dtUnicodeString  : Result^.UnicodeStrData^.Data := AStoreRdr.ReadUnicodeStr();
{ $ENDIF WST_UNICODESTRING}
    dtBool    : Result^.BoolData := AStoreRdr.ReadBool();
    dtAnsiChar    : Result^.AnsiCharData := AStoreRdr.ReadAnsiChar();
    dtWideChar    : Result^.WideCharData := AStoreRdr.ReadWideChar();
    dtEnum    : Result^.EnumData := AStoreRdr.ReadEnum();
    dtObject  :
      Begin
        Result^.ObjectData^.NilObject := AStoreRdr.ReadBool();
        if not Result^.ObjectData^.NilObject then begin
          i := AStoreRdr.ReadInt32S();
          For i := 1 To i Do Begin
            AddObj(Result,LoadObjectFromStream(AStoreRdr));
          End;
          if AStoreRdr.ReadBool() then
            Result^.ObjectData^.InnerData := LoadObjectFromStream(AStoreRdr);
        end;
      end;
    dtArray  :
      Begin
        i := AStoreRdr.ReadInt32S();
        Result := CreateArrayBuffer(i,s);
        For i := 0 To Pred(i) Do Begin
          AddObj(Result,LoadObjectFromStream(AStoreRdr),i);
        End;
      End;
  End;
End;

procedure FreeObjectBuffer(var ABuffer : PObjectBuffer);overload;
var
  p,q : PObjectBufferItem;
begin
  if Assigned(ABuffer) then begin
    if Assigned(ABuffer^.Attributes) then
      FreeObjectBuffer(ABuffer^.Attributes);
    p := ABuffer^.Head;
    while Assigned(p) do begin
      q := p;
      p := p^.Next;
      ClearObj(q^.Data);
      Freemem(q^.Data);
      q^.Data := Nil;
      Freemem(q);
    end;
    if Assigned(ABuffer^.InnerData) then begin
      ClearObj(ABuffer^.InnerData);
      Freemem(ABuffer^.InnerData);
      ABuffer^.InnerData := nil;
    end;
    //ABuffer^.Head := nil;
    //ABuffer^.Last := nil;
    Freemem(ABuffer);
    ABuffer := nil;
  end;
end;

procedure FreeObjectBuffer(var ABuffer : PDataBuffer);
var
  tmpBuffer : PDataBuffer;
begin
  if ( ABuffer <> nil ) then begin
    tmpBuffer := ABuffer;
    ABuffer := nil;
    ClearObj(tmpBuffer);
    FreeMem(tmpBuffer)
  end;
end;

procedure ClearObj(const AOwner: PDataBuffer);
Var
  i , j: Integer;
  eltLen : Integer;
Begin
  AOwner^.Name := '';
  Case AOwner^.DataType Of
    dtAnsiString :
      Begin
        AOwner^.AnsiStrData^.Data := '';
        Freemem(AOwner^.AnsiStrData);
        AOwner^.AnsiStrData := Nil;
      End;
    dtWideString :
      begin
        AOwner^.WideStrData^.Data := '';
        Freemem(AOwner^.WideStrData);
        AOwner^.WideStrData := Nil;
      end;
{ $IFDEF WST_UNICODESTRING}
    dtUnicodeString :
      begin
        AOwner^.UnicodeStrData^.Data := '';
        Freemem(AOwner^.UnicodeStrData);
        AOwner^.UnicodeStrData := Nil;
      end;
{ $ENDIF WST_UNICODESTRING}
    dtObject :
      Begin
        FreeObjectBuffer(AOwner^.ObjectData);
      End;
    dtArray :
      Begin
        eltLen := SizeOf(TDataBuffer);
        For j := 0 to Pred(AOwner^.ArrayData^.Count) Do Begin
          ClearObj(AOwner^.ArrayData^.Items^[j]);
          Freemem(AOwner^.ArrayData^.Items^[j],eltLen);
          AOwner^.ArrayData^.Items^[j] := Nil;
        End;
        i := AOwner^.ArrayData^.Count * SizeOf(PDataBuffer);
        Freemem(AOwner^.ArrayData^.Items,i);
        AOwner^.ArrayData^.Items := Nil;
        FreeObjectBuffer(AOwner^.ArrayData^.Attributes);
        i := SizeOf(TArrayBuffer);
        Freemem(AOwner^.ArrayData,i);
        AOwner^.ArrayData := Nil;
      End;
  End;
End;


{ TStackItem }

constructor TStackItem.Create(const AScopeObject: PDataBuffer; AScopeType: TScopeType);
begin
  Assert(Assigned(AScopeObject));
  FScopeObject := AScopeObject;
  FScopeType := AScopeType;
end;

{ TObjectStackItem }

constructor TObjectStackItem.Create(const AScopeObject: PDataBuffer);
begin
  Inherited Create(AScopeObject,stObject);
end;

function TObjectStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ObjectData^.Count;
end;

function TObjectStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  Result := FindObj(ScopeObject,AName);
end;

function TObjectStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
Var
  p : PObjectBufferItem;
  i : Integer;
begin
  If ( AIndex >=0 ) And ( AIndex < ScopeObject^.ObjectData^.Count) Then Begin
    p := ScopeObject^.ObjectData^.Head;
    For i := 1 To AIndex Do
      p := p^.Next;
    Result := p^.Data;
  End Else
    Raise EBinaryFormatterException.CreateFmt(SERR_IndexOutOfBound,[AIndex]);
end;

function TObjectStackItem.CreateBuffer(
  Const AName     : String;
  const ADataType : TDataType
):PDataBuffer;
begin
  Result := CreateObjBuffer(ADataType,AName,ScopeObject);
end;

function TObjectStackItem.CreateInnerBuffer(const ADataType: TDataType): PDataBuffer;
begin
  Result := CreateObjBuffer(ADataType,sSCOPE_INNER_NAME,nil);
  ScopeObject^.ObjectData^.InnerData := Result;
end;

function TObjectStackItem.GetInnerBuffer(): PDataBuffer;
begin
  Result := ScopeObject^.ObjectData^.InnerData;
end;

procedure TObjectStackItem.NilCurrentScope();
begin
  Assert(ScopeObject^.ObjectData^.Count = 0);
  ScopeObject^.ObjectData^.NilObject := True;
end;

function TObjectStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result := ScopeObject^.ObjectData^.NilObject;
end;

//----------------------------------------------------------------
function TObjectStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  locBuffer : PObjectBufferItem;
begin
  AReturnList.Clear();
  if Assigned(ScopeObject) and ( ScopeObject^.ObjectData^.Count > 0 ) then begin
    locBuffer := ScopeObject^.ObjectData^.Head;
    while Assigned(locBuffer) do begin
      AReturnList.Add(locBuffer^.Data^.Name);
      locBuffer := locBuffer^.Next;
    end;
  end;
  Result := AReturnList.Count;
end;

{ TBaseBinaryFormatter }

procedure TBaseBinaryFormatter.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

procedure TBaseBinaryFormatter.PushStack(AScopeObject: PDataBuffer;const AScopeType: TScopeType);
begin
  If ( AScopeType = stObject ) Then
    FStack.Push(TObjectStackItem.Create(AScopeObject))
  Else If ( AScopeType = stArray ) Then
    FStack.Push(TArrayStackItem.Create(AScopeObject))
  Else
    Assert(False);
end;

function TBaseBinaryFormatter.StackTop(): TStackItem;
begin
  Result := FStack.Peek() as TStackItem;
end;

function TBaseBinaryFormatter.PopStack(): TStackItem;
begin
  Result := FStack.Pop() as TStackItem;
end;

function TBaseBinaryFormatter.GetRootData(): PDataBuffer;
begin
  Result := FRootData;
end;

procedure TBaseBinaryFormatter.PutFloat(
  const AName       : String;
  const ATypeInfo   : PTypeInfo;
  const AData       : TFloat_Extended_10
);
begin
  Case GetTypeData(ATypeInfo)^.FloatType Of
    ftSingle   : StackTop().CreateBuffer(AName,dtSingle)^.SingleData  := AData;
    ftDouble   : StackTop().CreateBuffer(AName,dtDouble)^.DoubleData   := AData;
    ftExtended : StackTop().CreateBuffer(AName,dtExtended)^.ExtendedData := AData;
    ftCurr     : StackTop().CreateBuffer(AName,dtCurrency)^.CurrencyData := AData;
    Else
      StackTop().CreateBuffer(AName,dtExtended)^.ExtendedData := AData;
  End;
end;

function TBaseBinaryFormatter.GetCurrentScopeObject(): PDataBuffer;
begin
  Result := StackTop().ScopeObject;
end;

procedure TBaseBinaryFormatter.SetSerializationStyle(
  const ASerializationStyle: TSerializationStyle
);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TBaseBinaryFormatter.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TBaseBinaryFormatter.HasScope(): Boolean;
begin
  Result := ( FStack.Peek <> nil );
end;

procedure TBaseBinaryFormatter.CheckScope();
begin
  If Not HasScope() Then
    Error(SERR_NoScope);
end;

function TBaseBinaryFormatter.GetCurrentScope: String;
begin
  Result := GetCurrentScopeObject()^.Name;
end;

procedure TBaseBinaryFormatter.PutInt(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : TInt64S
);
begin
  Case GetTypeData(ATypeInfo)^.OrdType Of
    otSByte : StackTop().CreateBuffer(AName,dtInt8S)^.Int8S   := AData;
    otUByte : StackTop().CreateBuffer(AName,dtInt8U)^.Int8U   := AData;
    otSWord : StackTop().CreateBuffer(AName,dtInt16S)^.Int16S := AData;
    otUWord : StackTop().CreateBuffer(AName,dtInt16U)^.Int16U := AData;
    otULong : StackTop().CreateBuffer(AName,dtInt32U)^.Int32U := AData;
    otSLong : StackTop().CreateBuffer(AName,dtInt32S)^.Int32S := AData;
  End;
end;

procedure TBaseBinaryFormatter.PutAnsiStr(
  const AName      : String;
  const ATypeInfo  : PTypeInfo;
  const AData      : AnsiString
);
begin
  StackTop().CreateBuffer(AName,dtAnsiString)^.AnsiStrData^.Data := AData;
end;

procedure TBaseBinaryFormatter.PutWideStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideString
);
begin
  StackTop().CreateBuffer(AName,dtWideString)^.WideStrData^.Data := AData;
end;

{ $IFDEF WST_UNICODESTRING}
procedure TBaseBinaryFormatter.PutUnicodeStr(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: UnicodeString
);
begin
  StackTop().CreateBuffer(AName,dtUnicodeString)^.UnicodeStrData^.Data := AData;
end;
{ $ENDIF WST_UNICODESTRING}

procedure TBaseBinaryFormatter.PutEnum(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TEnumData
);
begin
  StackTop().CreateBuffer(AName,dtEnum)^.EnumData := AData;
end;

procedure TBaseBinaryFormatter.PutBool(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: Boolean
);
begin
  StackTop().CreateBuffer(AName,dtBool)^.BoolData := AData;
end;

procedure TBaseBinaryFormatter.PutAnsiChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: AnsiChar
);
begin
  StackTop().CreateBuffer(AName,dtAnsiChar)^.AnsiCharData := AData;
end;

procedure TBaseBinaryFormatter.PutWideChar(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: WideChar
);
begin
  StackTop().CreateBuffer(AName,dtWideChar)^.WideCharData := AData;
end;

procedure TBaseBinaryFormatter.PutInt64(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: Int64
);
begin
  StackTop().CreateBuffer(AName,dtInt64S)^.Int64S := AData;
end;

{$IFDEF HAS_QWORD}
procedure TBaseBinaryFormatter.PutUInt64(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: QWord
);
begin
  StackTop().CreateBuffer(AName,dtInt64U)^.Int64U := AData;
end;
{$ENDIF HAS_QWORD}

procedure TBaseBinaryFormatter.PutObj(
  const AName: String;
  const ATypeInfo: PTypeInfo;
  const AData: TObject
);
begin
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Save(AData As TBaseRemotable, Self,AName,ATypeInfo);
end;

procedure TBaseBinaryFormatter.PutRecord(
  const AName     : string;
  const ATypeInfo : PTypeInfo;
  const AData     : Pointer
);
begin
  TRemotableRecordEncoder.Save(AData,Self,AName,ATypeInfo);
end;

function TBaseBinaryFormatter.GetDataBuffer(
  var AName: string;
  out AResultBuffer : PDataBuffer
) : Boolean;
begin
  AResultBuffer := StackTop().Find(AName);
  Result := ( AResultBuffer <> nil );
end;

function TBaseBinaryFormatter.GetEnum(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TEnumData
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.EnumData;
end;

function TBaseBinaryFormatter.GetBool(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: Boolean
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.BoolData;
end;

function TBaseBinaryFormatter.GetAnsiChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: AnsiChar
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.AnsiCharData;
end;

function TBaseBinaryFormatter.GetWideChar(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideChar
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.WideCharData;
end;

function TBaseBinaryFormatter.GetFloat(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : TFloat_Extended_10
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    case GetTypeData(ATypeInfo)^.FloatType Of
      ftSingle     : AData := locBuffer^.SingleData;
      ftDouble     : AData := locBuffer^.DoubleData;
      ftExtended   : AData := locBuffer^.ExtendedData;
      ftCurr       : AData := locBuffer^.CurrencyData;
      else
        AData := locBuffer^.ExtendedData;
    end;
  end;
end;

function TBaseBinaryFormatter.GetInt(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TInt64S
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    case GetTypeData(ATypeInfo)^.OrdType of
      otSByte : AData := locBuffer^.Int8S;
      otUByte : AData := locBuffer^.Int8U;
      otSWord : AData := locBuffer^.Int16S;
      otUWord : AData := locBuffer^.Int16U;
      otSLong : AData := locBuffer^.Int32S;
      otULong : AData := locBuffer^.Int32U;
      Else
        Assert(False);
    end;
  end;
end;

function TBaseBinaryFormatter.GetInt64(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: Int64
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int64S;
end;

{$IFDEF HAS_QWORD}
function TBaseBinaryFormatter.GetUInt64(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: QWord
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int64U;
end;
{$ENDIF HAS_QWORD}

function TBaseBinaryFormatter.GetAnsiStr(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: AnsiString
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    case locBuffer^.DataType of
      dtUnicodeString : AData := locBuffer^.UnicodeStrData^.Data;
      dtWideString    : AData := locBuffer^.WideStrData^.Data;
      else
        AData := locBuffer^.AnsiStrData^.Data;
    end;
  end;
end;

function TBaseBinaryFormatter.GetWideStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: WideString
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    case locBuffer^.DataType of
      dtAnsiString    : AData := locBuffer^.AnsiStrData^.Data;
      dtUnicodeString : AData := locBuffer^.UnicodeStrData^.Data;
      else
        AData := locBuffer^.WideStrData^.Data;
    end;
  end;
end;

{ $IFDEF WST_UNICODESTRING}
function TBaseBinaryFormatter.GetUnicodeStr(
  const ATypeInfo: PTypeInfo;
  var   AName: String;
  var   AData: UnicodeString
) : Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    case locBuffer^.DataType of
      dtAnsiString : AData := locBuffer^.AnsiStrData^.Data;
      dtWideString : AData := locBuffer^.WideStrData^.Data;
      else
        AData := locBuffer^.UnicodeStrData^.Data;
    end;
  end;
end;
{ $ENDIF WST_UNICODESTRING}

function TBaseBinaryFormatter.GetObj(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData: TObject
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TBaseRemotableClass(GetTypeData(ATypeInfo)^.ClassType).Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

function TBaseBinaryFormatter.GetRecord(
  const ATypeInfo : PTypeInfo;
  var   AName     : String;
  var   AData     : Pointer
) : Boolean;
begin
  { TODO -cEXCEPTION_SAFE : Load() should be a function ! }
  TRemotableRecordEncoder.Load(AData, Self,AName,ATypeInfo);
  Result := True;
end;

procedure TBaseBinaryFormatter.Clear();
begin
  ClearStack();
  ClearObj(FRootData);
  Freemem(FRootData);
  FRootData := CreateObjBuffer(dtObject,sROOT);
  PushStack(FRootData,stObject);
end;

procedure TBaseBinaryFormatter.BeginArray(
  const AName         : string;
  const ATypeInfo     : PTypeInfo;
  const AItemTypeInfo : PTypeInfo;
  const ABounds       : Array Of Integer;
  const AStyle        : TArrayStyle
);
var
  i, j, k : Integer;
begin
  If ( Length(ABounds) < 2 ) Then
    Raise EBinaryFormatterException.Create(SERR_InvalidArrayBounds);
  i := ABounds[0];
  j := ABounds[1];
  k := ( j - i + 1 );
  If ( k < 0 ) Then
    Raise EBinaryFormatterException.Create(SERR_InvalidArrayBounds);
  PushStack(CreateArrayBuffer(k,AName,StackTop().ScopeObject),stArray);
end;

procedure TBaseBinaryFormatter.NilCurrentScope();
begin
  CheckScope();
  StackTop().NilCurrentScope();
end;

function TBaseBinaryFormatter.IsCurrentScopeNil(): Boolean;
begin
  Result := StackTop().IsCurrentScopeNil();
end;

procedure TBaseBinaryFormatter.BeginObject(
  const AName: string;
  const ATypeInfo: PTypeInfo
);
begin
  PushStack(StackTop().CreateBuffer(AName,dtObject));
end;

procedure TBaseBinaryFormatter.EndScope();
begin
  FStack.Pop().Free();
end;

procedure TBaseBinaryFormatter.AddScopeAttribute(const AName, AValue: string);
begin
end;

function TBaseBinaryFormatter.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : PTypeInfo
): Integer;
var
  locNode : PDataBuffer;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.Find(AScopeName);
  if (locNode <> nil) then begin
    PushStack(locNode,stObject);
    Result := StackTop().GetItemCount();
  end else begin
    Result := -1;
  end;
end;

function TBaseBinaryFormatter.BeginArrayRead(
  var AScopeName  : string;
  const ATypeInfo : PTypeInfo;
  const AStyle    : TArrayStyle;
  const AItemName : string
): Integer;
var
  locNode : PDataBuffer;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.Find(AScopeName);
  if ( locNode <> nil ) then begin
    PushStack(locNode,stArray);
    Result := StackTop().GetItemCount();
  end else begin
    Result := -1;
  end;
end;

function TBaseBinaryFormatter.GetScopeItemNames(const AReturnList : TStrings) : Integer;
begin
  CheckScope();
  Result := StackTop.GetScopeItemNames(AReturnList);
end;

procedure TBaseBinaryFormatter.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TBaseBinaryFormatter.BeginHeader();
begin
{$IFDEF wst_binary_header}
  if ( FHeaderEnterCount <= 0 ) then begin
    Inc(FHeaderEnterCount);
    BeginObject(sHEADER,nil);
  end;
{$ENDIF}
end;

procedure TBaseBinaryFormatter.EndHeader();
begin
{$IFDEF wst_binary_header}
  if ( FHeaderEnterCount > 0 ) then begin
    Dec(FHeaderEnterCount);
    EndScope();
  end;
{$ENDIF}
end;

procedure TBaseBinaryFormatter.Put(const AName: String; const ATypeInfo: PTypeInfo;const AData);
Var
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWOrd;
{$ENDIF HAS_QWORD}
  ansiStrData : AnsiString;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumData;
  floatDt : TFloat_Extended_10;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
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
        ansiStrData := AnsiString(AData);
        PutAnsiStr(AName,ATypeInfo,ansiStrData);
      End;
    tkWString :
      begin
        wideStrData := WideString(AData);
        PutWideStr(AName,ATypeInfo,wideStrData);
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := UnicodeString(AData);
        PutUnicodeStr(AName,ATypeInfo,unicodeStrData);
      end;
{$ENDIF WST_UNICODESTRING}
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
      {$IFNDEF FPC}
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
            PutInt(AName,ATypeInfo,enumData)
          Else
            PutEnum(AName,ATypeInfo,enumData);
      {$IFNDEF FPC}
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

procedure TBaseBinaryFormatter.Put(
  const ANameSpace : string;
  const AName : String;
  const ATypeInfo : PTypeInfo;
  const AData
);
begin
  Put(AName,ATypeInfo,AData);
end;

procedure TBaseBinaryFormatter.PutScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  const AData
);
var
  int64SData : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWord;
{$ENDIF HAS_QWORD}
  strData : string;
  boolData : Boolean;
  enumData : TEnumData;
  floatDt : TFloat_Extended_10;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  CheckScope();
  case ATypeInfo^.Kind of
    tkChar :
      begin
        ansiCharData := AnsiChar(AData);
        StackTop().CreateInnerBuffer(dtAnsiChar)^.AnsiCharData:= ansiCharData;
      end;
    tkWChar :
      begin
        wideCharData := WideChar(AData);
        StackTop().CreateInnerBuffer(dtWideChar)^.WideCharData:= wideCharData;
      end;
    tkLString{$IFDEF FPC},tkAString{$ENDIF} :
      begin
        strData := AnsiString(AData);
        StackTop().CreateInnerBuffer(dtAnsiString)^.AnsiStrData^.Data := strData;
      end;
    tkWString :
      begin
        wideStrData := WideString(AData);
        StackTop().CreateInnerBuffer(dtWideString)^.WideStrData^.Data := wideStrData;
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := UnicodeString(AData);
        StackTop().CreateInnerBuffer(dtUnicodeString)^.UnicodeStrData^.Data := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
    tkInt64 :
      begin
        int64SData := Int64(AData);
        StackTop().CreateInnerBuffer(dtInt64S)^.Int64S := int64SData;
      end;
{$IFDEF HAS_QWORD}
    tkQWord :
      begin
        uint64Data := QWord(AData);
        StackTop().CreateInnerBuffer(dtInt64U)^.Int64U := uint64Data;
      end;
{$ENDIF HAS_QWORD}
    tkClass, tkRecord :
      begin
        raise EBinaryFormatterException.Create(SERR_InnerScopeMustBeSimpleType);
      end;
    {$IFDEF FPC}
    tkBool :
      begin
        boolData := Boolean(AData);
        StackTop().CreateInnerBuffer(dtBool)^.BoolData := boolData;
      end;
    {$ENDIF}
    tkInteger :
      begin
        enumData := 0;
        case GetTypeData(ATypeInfo)^.OrdType of
          otSByte :
            begin
              enumData := ShortInt(AData);
              StackTop().CreateInnerBuffer(dtInt8S)^.Int8S := enumData;
            end;
          otUByte :
            begin
              enumData := Byte(AData);
              StackTop().CreateInnerBuffer(dtInt8U)^.Int8U := enumData;
            end;
          otSWord :
            begin
              enumData := SmallInt(AData);
              StackTop().CreateInnerBuffer(dtInt16S)^.Int16S := enumData;
            end;
          otUWord :
            begin
              enumData := Word(AData);
              StackTop().CreateInnerBuffer(dtInt16U)^.Int16U := enumData;
            end;
          otSLong :
            begin
              enumData := LongInt(AData);
              StackTop().CreateInnerBuffer(dtInt32S)^.Int32S := enumData;
            end;
          otULong :
            begin
              enumData := LongWord(AData);
              StackTop().CreateInnerBuffer(dtInt32U)^.Int32U := enumData;
            end;
        end;
      end;
    tkEnumeration :
      begin
      {$IFNDEF FPC}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
        boolData := Boolean(AData);
        StackTop().CreateInnerBuffer(dtBool)^.BoolData := boolData;
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
          StackTop().CreateInnerBuffer(dtEnum)^.EnumData := enumData;
      {$IFNDEF FPC}
        end;
      {$ENDIF}
      end;
    tkFloat :
      begin
        floatDt := 0;
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle   :
            begin
              floatDt := Single(AData);
              StackTop().CreateInnerBuffer(dtSingle)^.SingleData := floatDt;
            end;
          ftDouble   :
            begin
              floatDt := Double(AData);
              StackTop().CreateInnerBuffer(dtDouble)^.DoubleData := floatDt;
            end;
          ftExtended :
            begin
              floatDt := Extended(AData);
              StackTop().CreateInnerBuffer(dtExtended)^.ExtendedData := floatDt;
            end;
          ftCurr     :
            begin
              floatDt := Currency(AData);
              StackTop().CreateInnerBuffer(dtExtended)^.CurrencyData := floatDt;
            end;
          ftComp     :
            begin
              floatDt := Comp(AData);
              StackTop().CreateInnerBuffer(dtCurrency)^.CurrencyData := floatDt;
            end;
          else
            StackTop().CreateInnerBuffer(dtExtended)^.ExtendedData := floatDt;
        end;
      end;
  end;
end;

function TBaseBinaryFormatter.Get(
  const ATypeInfo: PTypeInfo;
  var AName: String;
  var AData
) : Boolean;
Var
  int64Data : Int64;
{$IFDEF HAS_QWORD}
  uint64Data : QWOrd;
{$ENDIF HAS_QWORD}
  strData : AnsiString;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumData;
  floatDt : TFloat_Extended_10;
  recObject : Pointer;
  wideStrData : WideString;
{$IFDEF WST_UNICODESTRING}
  unicodeStrData : UnicodeString;
{$ENDIF WST_UNICODESTRING}
  ansiCharData : AnsiChar;
  wideCharData : WideChar;
begin
  Case ATypeInfo^.Kind Of
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
        Result := GetAnsiStr(ATypeInfo,AName,strData);
        if Result then
          AnsiString(AData) := strData;
      End;
    tkWString :
      begin
        wideStrData := '';
        Result := GetWideStr(ATypeInfo,AName,wideStrData);
        if Result then
          WideString(AData) := wideStrData;
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString :
      begin
        unicodeStrData := '';
        Result := GetUnicodeStr(ATypeInfo,AName,unicodeStrData);
        if Result then
          UnicodeString(AData) := unicodeStrData;
      end;
{$ENDIF WST_UNICODESTRING}
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
    tkInteger, tkEnumeration :
      Begin
      {$IFNDEF FPC}
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
          If ( ATypeInfo^.Kind = tkInteger ) Then
            Result := GetInt(ATypeInfo,AName,enumData)
          Else
            Result := GetEnum(ATypeInfo,AName,enumData);
          if Result then begin
            Case GetTypeData(ATypeInfo)^.OrdType Of
              otSByte : ShortInt(AData)    := enumData;
              otUByte : Byte(AData)        := enumData;
              otSWord : SmallInt(AData)    := enumData;
              otUWord : Word(AData)        := enumData;
              otSLong : LongInt(AData)     := enumData;
              otULong : LongWord(AData)    := enumData;
            End;
          end;
      {$IFNDEF FPC}
        end;
      {$ENDIF}
      End;
    tkFloat :
      Begin
        floatDt := 0;
        Result := GetFloat(ATypeInfo,AName,floatDt);
        if Result then begin
          Case GetTypeData(ATypeInfo)^.FloatType Of
            ftSingle : Single(AData)    := floatDt;
            ftDouble : Double(AData)    := floatDt;
            ftExtended : Extended(AData)    := floatDt;
            ftCurr : Currency(AData)    := floatDt;
{$IFDEF HAS_COMP}
            ftComp : Comp(AData)    := floatDt;
{$ENDIF}
          End;
        end
      end;
    else
      Result := False;
  end;
end;

function TBaseBinaryFormatter.Get(
  const ATypeInfo : PTypeInfo;
  const ANameSpace : string;
  var AName : string;
  var AData
) : Boolean;
begin
  Result := Get(ATypeInfo,AName,AData);
end;

procedure TBaseBinaryFormatter.GetScopeInnerValue(
  const ATypeInfo : PTypeInfo;
  var   AData
);
Var
  dataBuffer : PDataBuffer;
begin
  CheckScope();
  dataBuffer := StackTop().GetInnerBuffer();
  Case ATypeInfo^.Kind Of
    tkChar         : AnsiChar(AData) := dataBuffer^.AnsiCharData ;
    tkWChar        : WideChar(AData) := dataBuffer^.WideCharData ;
    tkInt64        : Int64(AData) := dataBuffer^.Int64S;
{$IFDEF HAS_QWORD}
    tkQWord        : QWord(AData) := dataBuffer^.Int64U;
{$ENDIF HAS_QWORD}

    tkLString
    {$IFDEF FPC},
    tkAString
    {$ENDIF}       :
      begin
        case dataBuffer^.DataType of
          dtUnicodeString : AnsiString(AData) := dataBuffer^.UnicodeStrData^.Data;
          dtWideString    : AnsiString(AData) := dataBuffer^.WideStrData^.Data;
          else
            AnsiString(AData) := dataBuffer^.AnsiStrData^.Data;
        end;
      end;
    tkWString      :
      begin
        case dataBuffer^.DataType of
          dtUnicodeString : WideString(AData) := dataBuffer^.UnicodeStrData^.Data;
          dtAnsiString    : WideString(AData) := dataBuffer^.AnsiStrData^.Data;
          else
            WideString(AData) := dataBuffer^.WideStrData^.Data;
        end;
      end;
{$IFDEF WST_UNICODESTRING}
    tkUString      :
      begin
        case dataBuffer^.DataType of
          dtAnsiString : UnicodeString(AData) := dataBuffer^.AnsiStrData^.Data;
          dtWideString : UnicodeString(AData) := dataBuffer^.WideStrData^.Data;
          else
            UnicodeString(AData) := dataBuffer^.UnicodeStrData^.Data;
        end;
      end;
{$ENDIF WST_UNICODESTRING}

    tkClass, tkRecord : raise EBinaryFormatterException.Create(SERR_InnerScopeMustBeSimpleType);
    {$IFDEF FPC}
    tkBool         : Boolean(AData) := dataBuffer^.BoolData;
    {$ENDIF}
    tkInteger      :
      begin
        case GetTypeData(ATypeInfo)^.OrdType Of
          otSByte : ShortInt(AData)    := dataBuffer^.Int8S;
          otUByte : Byte(AData)        := dataBuffer^.Int8U;
          otSWord : SmallInt(AData)    := dataBuffer^.Int16S;
          otUWord : Word(AData)        := dataBuffer^.Int16U;
          otSLong : LongInt(AData)     := dataBuffer^.Int32S;
          otULong : LongWord(AData)    := dataBuffer^.Int32U;
        end;
      end;
    tkEnumeration      :
      begin
      {$IFNDEF FPC}
        if ( ATypeInfo^.Kind = tkEnumeration ) and
           ( GetTypeData(ATypeInfo)^.BaseType^ = TypeInfo(Boolean) )
        then begin
          Boolean(AData) := dataBuffer^.BoolData;
        end else begin
      {$ENDIF}
          case GetTypeData(ATypeInfo)^.OrdType Of
            otSByte : ShortInt(AData)    := dataBuffer^.EnumData;
            otUByte : Byte(AData)        := dataBuffer^.EnumData;
            otSWord : SmallInt(AData)    := dataBuffer^.EnumData;
            otUWord : Word(AData)        := dataBuffer^.EnumData;
            otSLong : LongInt(AData)     := dataBuffer^.EnumData;
            otULong : LongWord(AData)    := dataBuffer^.EnumData;
          end;
      {$IFNDEF FPC}
        end;
      {$ENDIF}
      end;
    tkFloat :
      begin
        case GetTypeData(ATypeInfo)^.FloatType of
          ftSingle   : Single(AData)      := dataBuffer^.SingleData;
          ftDouble   : Double(AData)      := dataBuffer^.DoubleData;
          ftExtended : Extended(AData)    := dataBuffer^.ExtendedData;
          ftCurr     : Currency(AData)    := dataBuffer^.CurrencyData;
{$IFDEF HAS_COMP}
          else
            Comp(AData) := dataBuffer^.ExtendedData;
{$ENDIF}
        end;
      end;
  end;
end;

function TBaseBinaryFormatter.ReadBuffer (const AName : string; out AResBuffer : string) : Boolean;
Var
  locStore : IDataStore;
  bffr : PDataBuffer;
  locName : string;
  locStream : TStringStream;
begin
  locName := AName;
  Result := GetDataBuffer(locName,bffr);
  if Result then begin
    locStream := TStringStream.Create('');
    try
      locStore := CreateBinaryWriter(locStream);
      SaveObjectToStream(bffr,locStore);
      AResBuffer := locStream.DataString;
    finally
      locStream.Free();
    end;
  end;
end;

procedure TBaseBinaryFormatter.SaveToStream(AStream: TStream);
Var
  locStore : IDataStore;
begin
  locStore := CreateBinaryWriter(AStream);
  SaveObjectToStream(FRootData,locStore);
end;

procedure TBaseBinaryFormatter.LoadFromStream(AStream: TStream);
Var
  locRdr : IDataStoreReader;
  tmpRoot : PDataBuffer;
begin
  locRdr := CreateBinaryReader(AStream);
  tmpRoot := LoadObjectFromStream(locRdr);

  ClearStack();
  ClearObj(FRootData);
  Freemem(FRootData);
  FRootData := tmpRoot;
  PushStack(FRootData,stObject);
end;

procedure TBaseBinaryFormatter.Error(const AMsg: string);
begin
  Raise EBinaryFormatterException.Create(AMsg);
end;

procedure TBaseBinaryFormatter.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EBinaryFormatterException.CreateFmt(AMsg,AArgs);
end;

constructor TBaseBinaryFormatter.Create();
begin
  FRootData := CreateObjBuffer(dtObject,sROOT);
  FStack := TObjectStack.Create();
  PushStack(FRootData,stObject);
end;

destructor TBaseBinaryFormatter.Destroy();
begin
  ClearStack();
  FreeAndNil(FStack);
  ClearObj(FRootData);
  Freemem(FRootData);
  inherited Destroy();
end;

function TBaseBinaryFormatter.GetFormatName() : string;
begin
  Result := sBINARY_FORMAT_NAME;
end;

function TBaseBinaryFormatter.GetPropertyManager() : IPropertyManager;
begin
  If Not Assigned(FPropMngr) Then
    FPropMngr := TPublishedPropertyManager.Create(Self);
  Result := FPropMngr;
end;

procedure TBaseBinaryFormatter.WriteBuffer(const AValue: string);
var
  locStore : IDataStoreReader;
  bffr : PDataBuffer;
  locStream : TStringStream;
begin
  CheckScope();
  locStream := TStringStream.Create(AValue);
  try
    locStream.Position := 0;
    locStore := CreateBinaryReader(locStream);
    bffr := LoadObjectFromStream(locStore);
    AddObj(StackTop.ScopeObject,bffr);
  finally
    locStream.Free();
  end;
end;

{ TArrayStackItem }

constructor TArrayStackItem.Create(const AScopeObject: PDataBuffer);
begin
  Inherited Create(AScopeObject,stArray);
  FIndex := 0;
end;

function TArrayStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ArrayData^.Count;
end;

function TArrayStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[FIndex]
  Else
    Raise EBinaryFormatterException.CreateFmt(SERR_IndexOutOfBound,[FIndex]);
  Inc(FIndex);
end;

function TArrayStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
begin
  If ( AIndex >= 0 ) And ( AIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[AIndex]
  Else
    Raise EBinaryFormatterException.CreateFmt(SERR_IndexOutOfBound,[AIndex]);
end;

function TArrayStackItem.CreateBuffer(
  const AName     : String;
  const ADataType : TDataType
): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := CreateObjBuffer(ADataType,AName,Nil)
  Else
    Raise EBinaryFormatterException.CreateFmt(SERR_IndexOutOfBound,[FIndex]);
  ScopeObject^.ArrayData^.Items^[FIndex] := Result;
  Inc(FIndex);
end;

{$WARNINGS OFF}
function TArrayStackItem.CreateInnerBuffer(const ADataType: TDataType): PDataBuffer;
begin
  raise EBinaryFormatterException.CreateFmt(SERR_UnsupportedOperation,['TArrayStackItem.CreateInnerBuffer']);
end;

function TArrayStackItem.GetInnerBuffer(): PDataBuffer;
begin
  raise EBinaryFormatterException.CreateFmt(SERR_UnsupportedOperation,['TArrayStackItem.GetInnerBuffer']);
end;
{$WARNINGS ON}

procedure TArrayStackItem.NilCurrentScope();
begin
end;

function TArrayStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result := False;
end;

function TArrayStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  locBuffer : PDataBufferList;
  i : Integer;
begin
  AReturnList.Clear();
  if Assigned(ScopeObject) and ( ScopeObject^.ArrayData^.Count > 0 ) then begin
    locBuffer := ScopeObject^.ArrayData^.Items;
    for i := 0 to Pred(ScopeObject^.ArrayData^.Count) do begin
      AReturnList.Add(locBuffer^[i]^.Name);
    end;
  end;
  Result := AReturnList.Count;
end;

end.
