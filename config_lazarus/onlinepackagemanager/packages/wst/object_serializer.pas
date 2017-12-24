{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit object_serializer;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs,
  base_service_intf, wst_types;

type

  TObjectSerializerOption = ( osoDontDoBeginRead, osoDontDoBeginWrite );
  TObjectSerializerOptions = set of TObjectSerializerOption;
  
  ESerializerException = class(EServiceException)
  end;
  
  TPropSerializationInfo = class;
  
  TPropertyReadProc = function(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
  ) : Boolean;
  TPropertyWriteProc = procedure(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
  );
  
  { TPropSerializationInfo }

  TPropSerializationInfo = class
  private
    FExternalName : string;
    FName : string;
    FNameSpace : string;
    FPersisteType : TPropStoreType;
    FPropInfo : PPropInfo;
    FQualifiedName : Boolean;
    FReaderProc : TPropertyReadProc;
    FStyle : TSerializationStyle;
    FWriterProc : TPropertyWriteProc;
  public
    property Name : string read FName;
    property ExternalName : string read FExternalName;
    // NameSpace apply only if ( QualifiedName = True )
    property NameSpace : string read FNameSpace;
    property Style : TSerializationStyle read FStyle;
    property PersisteType : TPropStoreType read FPersisteType;
    property PropInfo : PPropInfo read FPropInfo;
    property QualifiedName : Boolean read FQualifiedName;
    property ReaderProc : TPropertyReadProc read FReaderProc;
    property WriterProc : TPropertyWriteProc read FWriterProc;
  end;
  
  { TObjectSerializer }

  TObjectSerializer = class
  private
    FSerializationInfos : TObjectList;
    FTarget : TBaseComplexRemotableClass;
    FRawPropList : PPropList;
    FOptions : TObjectSerializerOptions;
  private
    procedure Prepare(ATypeRegistry : TTypeRegistry);
    function FindInfo(const APropName : string) : TPropSerializationInfo;
    procedure UpdateExternalName(const APropName, AExtPropName : string);
  public
    constructor Create(
      ATargetClass : TBaseComplexRemotableClass;
      ATypeRegistry : TTypeRegistry
    );
    destructor Destroy();override;
    procedure Read(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );
    procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );
    property Target : TBaseComplexRemotableClass read FTarget;
    property Options : TObjectSerializerOptions read FOptions write FOptions;
  end;

  TSimpleContentObjectSerializer = class
  private
    FSerializationInfos : TObjectList;
    FTarget : TBaseComplexSimpleContentRemotableClass;
    FRawPropList : PPropList;
    FOptions : TObjectSerializerOptions;
  private
    procedure Prepare(ATypeRegistry : TTypeRegistry);
    function FindInfo(const APropName : string) : TPropSerializationInfo;
    procedure UpdateExternalName(const APropName, AExtPropName : string);
  public
    constructor Create(
      ATargetClass : TBaseComplexSimpleContentRemotableClass;
      ATypeRegistry : TTypeRegistry
    );
    destructor Destroy();override;
    procedure Read(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );
    procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );
    property Target : TBaseComplexSimpleContentRemotableClass read FTarget;
    property Options : TObjectSerializerOptions read FOptions write FOptions;
  end;

  TGetSerializerFunction = function() : TObjectSerializer of object;
  
  { TBaseComplexTypeRegistryItem }

  TBaseComplexTypeRegistryItem = class(TTypeRegistryItem)
  private
    FSerializer : TObjectSerializer;
  protected
    procedure Init(); override;
  public
    destructor Destroy();override;
    procedure RegisterExternalPropertyName(const APropName, AExtPropName : string); override;
    function GetSerializer() : TObjectSerializer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  { TSimpleContentObjectRegistryItem }

  TSimpleContentObjectRegistryItem = class(TTypeRegistryItem)
  private
    FSerializer : TSimpleContentObjectSerializer;
  protected
    procedure Init(); override;
  public
    destructor Destroy();override;
    procedure RegisterExternalPropertyName(const APropName, AExtPropName : string); override;
    procedure SetPropertyOptions(
      const APropName : string;
      const AOptions : TTypeRegistryItemOptions
    ); override;
    function GetSerializer() : TSimpleContentObjectSerializer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  end;

  { TBaseComplexRemotableInitializer }

  TBaseComplexRemotableInitializer = class(TRemotableTypeInitializer)
  public
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean;override;
    class function GetItemClass(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;override;
{$IFDEF TRemotableTypeInitializer_Initialize}
    class function Initialize(
      ATypeInfo : PTypeInfo;
      ARegistryItem : TTypeRegistryItem
    ) : Boolean;override;
{$ENDIF TRemotableTypeInitializer_Initialize}
  end;

  { TBaseComplexRemotableInitializer }

  TSimpleContentObjectRemotableInitializer = class(TRemotableTypeInitializer)
  public
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean;override;
    class function GetItemClass(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;override;
{$IFDEF TRemotableTypeInitializer_Initialize}
    class function Initialize(
      ATypeInfo : PTypeInfo;
      ARegistryItem : TTypeRegistryItem
    ) : Boolean;override;
{$ENDIF TRemotableTypeInitializer_Initialize}
  end;


implementation
uses
  wst_consts;

{$WARNINGS OFF}
function ErrorFunc(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
) : Boolean;
begin
  raise Exception.CreateFmt(SERR_NoReaderProc,[APropInfo.Name,APropInfo.PropInfo^.Name]);
end;

procedure ErrorProc(
    AObject : TObject;
    APropInfo : TPropSerializationInfo;
    AStore : IFormatterBase
);
begin
  raise Exception.CreateFmt(SERR_NoReaderProc,[APropInfo.Name,APropInfo.PropInfo^.Name]);
end;
{$WARNINGS ON}

type
  TEnumBuffer = record
    case TOrdType of
      otSByte : (ShortIntData : ShortInt);
      otUByte : (ByteData : Byte);
      otSWord : (SmallIntData : SmallInt);
      otUWord : (WordData : Word);
      otSLong : (SLongIntData : LongInt);
      otULong : (ULongIntData : LongWord);
  end;
  TFloatBuffer = record
    case TFloatType of
      ftSingle : (SingleData : Single);
      ftDouble : (DoubleData : Double);
      ftExtended : (ExtendedData : Extended);
      ftCurr : (CurrencyData : Currency);
      ftComp : (CompData : Comp);
  end;
  TFloatExtendedType = Extended;
  
//   Simple readers
{$IFDEF HAS_TKBOOL}
function BoolReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : Boolean;
begin
  locData := False;
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType,locName,locData);
  if Result then
    SetOrdProp(AObject,APropInfo.PropInfo,Ord(locData));
end;
{$ENDIF HAS_TKBOOL}

function ClassReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  objData : TObject;
  objDataCreateHere : Boolean;
begin
  locName := APropInfo.ExternalName;
  objData := GetObjectProp(AObject,APropInfo.PropInfo);
  objDataCreateHere := not Assigned(objData);
  try
    Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,objData);
    if Result and objDataCreateHere then
      SetObjectProp(AObject,APropInfo.PropInfo,objData);
  finally
    if objDataCreateHere and ( objData <> GetObjectProp(AObject,APropInfo.PropInfo) ) then
      FreeAndNil(objData);
  end;
end;

function FloatReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  propName : string;
  floatBuffer : TFloatBuffer;
  floatDt : TFloatExtendedType;
  pt : PTypeInfo;
begin
  floatDt := 0;
  floatBuffer.ExtendedData := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        Result := AStore.Get(pt,propName,floatBuffer.SingleData);
        if Result then
          floatDt := floatBuffer.SingleData;
      end;
    ftDouble :
      begin
        Result := AStore.Get(pt,propName,floatBuffer.DoubleData);
        if Result then
          floatDt := floatBuffer.DoubleData;
      end;
    ftExtended :
      begin
        Result := AStore.Get(pt,propName,floatBuffer.ExtendedData);
        if Result then
          floatDt := floatBuffer.ExtendedData;
      end;
    ftCurr :
      begin
        Result := AStore.Get(pt,propName,floatBuffer.CurrencyData);
        if Result then
          floatDt := floatBuffer.CurrencyData;
      end;
    ftComp :
      begin
        Result := AStore.Get(pt,propName,floatBuffer.CompData);
        if Result then
          floatDt := floatBuffer.CompData;
      end;
    else
      Result := False;
  end;
  if Result then
    SetFloatProp(AObject,APropInfo.PropInfo,floatDt);
end;

function IntEnumReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  propName : string;
  int64Data : Int64;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  int64Data := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    Result := AStore.Get(pt,propName,boolData);
    if Result then
      SetPropValue(AObject,propName,boolData);
  end else begin
{$ENDIF}
    enumData.ULongIntData := 0;
    Case GetTypeData(pt)^.OrdType Of
      otSByte :
        Begin
          Result := AStore.Get(pt,propName,enumData.ShortIntData);
          if Result then
            int64Data := enumData.ShortIntData;
        End;
      otUByte :
        Begin
          Result := AStore.Get(pt,propName,enumData.ByteData);
          if Result then
            int64Data := enumData.ByteData;
        End;
      otSWord :
        Begin
          Result := AStore.Get(pt,propName,enumData.SmallIntData);
          if Result then
            int64Data := enumData.SmallIntData;
        End;
      otUWord :
        Begin
          Result := AStore.Get(pt,propName,enumData.WordData);
          if Result then
            int64Data := enumData.WordData;
        End;
      otSLong:
        Begin
          Result := AStore.Get(pt,propName,enumData.SLongIntData);
          if Result then
            int64Data := enumData.SLongIntData;
        End;
      otULong :
        Begin
          Result := AStore.Get(pt,propName,enumData.ULongIntData);
          if Result then
            int64Data := enumData.ULongIntData;
        End;
      else
        Result := False;
    End;
    if Result then
      SetOrdProp(AObject,APropInfo.PropInfo,int64Data);
{$IFDEF WST_DELPHI}
  end;
{$ENDIF}
end;

function Int64Reader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : Int64;
begin
  locData := 0;
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  if Result then
    SetInt64Prop(AObject,APropInfo.PropInfo,locData);
end;

function StringReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : string;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  if Result then
    SetStrProp(AObject,APropInfo.PropInfo,locData);
end;

{$IFDEF WST_UNICODESTRING}
function UnicodeStringReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : UnicodeString;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  if Result then
    SetUnicodeStrProp(AObject,APropInfo.PropInfo,locData);
end;
{$ENDIF WST_UNICODESTRING}

function WideStringReader(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : WideString;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locName,locData);
  if Result then
    SetWideStrProp(AObject,APropInfo.PropInfo,locData);
end;

// Qualified readers
{$IFDEF HAS_TKBOOL}
function BoolReaderQualifier(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : Boolean;
begin
  locData := False;
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType,APropInfo.NameSpace,locName,locData);
  if Result then
    SetOrdProp(AObject,APropInfo.PropInfo,Ord(locData));
end;
{$ENDIF HAS_TKBOOL}

function ClassReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  objData : TObject;
  objDataCreateHere : Boolean;
begin
  locName := APropInfo.ExternalName;
  objData := GetObjectProp(AObject,APropInfo.PropInfo);
  objDataCreateHere := not Assigned(objData);
  try
    Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,objData);
    if objDataCreateHere and Result then
      SetObjectProp(AObject,APropInfo.PropInfo,objData);
  finally
    if objDataCreateHere and ( objData <> GetObjectProp(AObject,APropInfo.PropInfo) ) then
      FreeAndNil(objData);
  end;
end;

function FloatReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  propName : string;
  floatBuffer : TFloatBuffer;
  floatDt : TFloatExtendedType;
  pt : PTypeInfo;
begin
  floatDt := 0;
  floatBuffer.ExtendedData := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        Result := AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.SingleData);
        if Result then
          floatDt := floatBuffer.SingleData;
      end;
    ftDouble :
      begin
        Result := AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.DoubleData);
        if Result then
          floatDt := floatBuffer.DoubleData;
      end;
    ftExtended :
      begin
        Result := AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.ExtendedData);
        if Result then
          floatDt := floatBuffer.ExtendedData;
      end;
    ftCurr :
      begin
        Result := AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.CurrencyData);
        if Result then
          floatDt := floatBuffer.CurrencyData;
      end;
    ftComp :
      begin
        Result := AStore.Get(pt,APropInfo.NameSpace,propName,floatBuffer.CompData);
        if Result then
          floatDt := floatBuffer.CompData;
      end;
    else
      Result := False;
  end;
  if Result then
    SetFloatProp(AObject,APropInfo.PropInfo,floatDt);
end;

function Int64ReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : Int64;
begin
  locData := 0;
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  if Result then
    SetInt64Prop(AObject,APropInfo.PropInfo,locData);
end;

function IntEnumReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  propName : string;
  int64Data : Int64;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  int64Data := 0;
  propName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    Result := AStore.Get(pt,APropInfo.NameSpace,propName,boolData);
    if Result then
      SetPropValue(AObject,propName,boolData);
  end else begin
{$ENDIF}
    enumData.ULongIntData := 0;
    Case GetTypeData(pt)^.OrdType Of
      otSByte :
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ShortIntData);
          if Result then
            int64Data := enumData.ShortIntData;
        End;
      otUByte :
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ByteData);
          if Result then
            int64Data := enumData.ByteData;
        End;
      otSWord :
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.SmallIntData);
          if Result then
            int64Data := enumData.SmallIntData;
        End;
      otUWord :
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.WordData);
          if Result then
            int64Data := enumData.WordData;
        End;
      otSLong:
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.SLongIntData);
          if Result then
            int64Data := enumData.SLongIntData;
        End;
      otULong :
        Begin
          Result := AStore.Get(pt,APropInfo.NameSpace,propName,enumData.ULongIntData);
          if Result then
            int64Data := enumData.ULongIntData;
        End;
      else
        Result := False;
    End;
    if Result then
      SetOrdProp(AObject,APropInfo.PropInfo,int64Data);
{$IFDEF WST_DELPHI}
  end;
{$ENDIF}
end;

function StringReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : string;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  if Result then
    SetStrProp(AObject,APropInfo.PropInfo,locData);
end;

{$IFDEF WST_UNICODESTRING}
function UnicodeStringReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : UnicodeString;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  if Result then
    SetUnicodeStrProp(AObject,APropInfo.PropInfo,locData);
end;
{$ENDIF WST_UNICODESTRING}

function WideStringReaderQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
) : Boolean;
var
  locName : string;
  locData : WideString;
begin
  locData := '';
  locName := APropInfo.ExternalName;
  Result := AStore.Get(APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},APropInfo.NameSpace,locName,locData);
  if Result then
    SetWideStrProp(AObject,APropInfo.PropInfo,locData);
end;

//   Simple Writers
{$IFDEF HAS_TKBOOL}
procedure BoolWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locName := APropInfo.ExternalName;
  locData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
  //if ( locData <> False ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(locName,APropInfo.PropInfo^.PropType,locData);
end;
{$ENDIF HAS_TKBOOL}

procedure ClassWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Tobject;
begin
  locName := APropInfo.ExternalName;
  locData := GetObjectProp(AObject,APropInfo.PropInfo);
  if (APropInfo.PersisteType = pstAlways) or
     ( (APropInfo.PersisteType = pstOptional) and
       (locData <> nil) and
       ( not(locData.InheritsFrom(TBaseRemotable)) or
         TBaseRemotable(locData).wstHasValue()
       )
     )
  then begin
    AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
  end;
end;

procedure FloatWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  floatDt : TFloatBuffer;
  pt : PTypeInfo;
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        floatDt.SingleData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.SingleData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(prpName,pt,floatDt.SingleData);
      end;
    ftDouble :
      begin
        floatDt.DoubleData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.DoubleData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(prpName,pt,floatDt.DoubleData);
      end;
    ftExtended :
      begin
        floatDt.ExtendedData := Extended(GetFloatProp(AObject,APropInfo.PropInfo));
        if ( floatDt.ExtendedData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(prpName,pt,floatDt.ExtendedData);
      end;
    ftCurr :
      begin
        floatDt.CurrencyData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.CurrencyData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(prpName,pt,floatDt.CurrencyData);
      end;
{$IFDEF HAS_COMP}
    ftComp :
      begin
        floatDt.CompData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.CurrencyData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(prpName,pt,floatDt.CompData);
      end;
{$ENDIF}
  end;
end;

procedure IntEnumWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    boolData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
    //if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
      AStore.Put(prpName,pt,boolData);
  end else begin
{$ENDIF WST_DELPHI}
    FillChar(enumData,SizeOf(enumData),#0);
    case GetTypeData(pt)^.OrdType of
      otSByte :
        begin
          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ShortIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.ShortIntData);
        end;
      otUByte :
        begin
          enumData.ByteData := Byte(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ByteData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.ByteData);
        end;
      otSWord :
        begin
          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.SmallIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.SmallIntData);
        end;
      otUWord :
        begin
          enumData.WordData := Word(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.WordData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.WordData);
        end;
      otSLong :
        begin
          enumData.SLongIntData := LongInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.SLongIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.SLongIntData);
        end;
      otULong :
        begin
          enumData.ULongIntData := LongWord(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ULongIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(prpName,pt,enumData.ULongIntData);
        end;
    end;
{$IFDEF WST_DELPHI}
  end;
{$ENDIF WST_DELPHI}
end;

procedure Int64Writer(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locName := APropInfo.ExternalName;
  locData := GetInt64Prop(AObject,APropInfo.PropInfo);
  if ( locData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure StringWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locName := APropInfo.ExternalName;
  locData := GetStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

{$IFDEF WST_UNICODESTRING}
procedure UnicodeStringWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : UnicodeString;
begin
  locName := APropInfo.ExternalName;
  locData := GetUnicodeStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;
{$ENDIF WST_UNICODESTRING}

procedure WideStringWriter(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : WideString;
begin
  locName := APropInfo.ExternalName;
  locData := GetWideStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

// Qualified writers
{$IFDEF HAS_TKBOOL}
procedure BoolWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Boolean;
begin
  locName := APropInfo.ExternalName;
  locData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
  //if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType,locData);
end;
{$ENDIF HAS_TKBOOL}

procedure ClassWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Tobject;
begin
  locName := APropInfo.ExternalName;
  locData := GetObjectProp(AObject,APropInfo.PropInfo);
  if (APropInfo.PersisteType = pstAlways) or
     ( (APropInfo.PersisteType = pstOptional) and
       (locData <> nil) and
       ( not(locData.InheritsFrom(TBaseRemotable)) or
         TBaseRemotable(locData).wstHasValue()
       )
     )
  then begin
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
  end;
end;

procedure FloatWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  floatDt : TFloatBuffer;
  pt : PTypeInfo;
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
  case GetTypeData(pt)^.FloatType of
    ftSingle :
      begin
        floatDt.SingleData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.SingleData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.SingleData);
      end;
    ftDouble :
      begin
        floatDt.DoubleData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.DoubleData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.DoubleData);
      end;
    ftExtended :
      begin
        floatDt.ExtendedData := Extended(GetFloatProp(AObject,APropInfo.PropInfo));
        if ( floatDt.ExtendedData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.ExtendedData);
      end;
    ftCurr :
      begin
        floatDt.CurrencyData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.CurrencyData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.CurrencyData);
      end;
{$IFDEF HAS_COMP}
    ftComp :
      begin
        floatDt.CompData := GetFloatProp(AObject,APropInfo.PropInfo);
        if ( floatDt.CompData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
          AStore.Put(APropInfo.NameSpace,prpName,pt,floatDt.CompData);
      end;
{$ENDIF}
  end;
end;

procedure IntEnumWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  prpName : string;
  enumData : TEnumBuffer;
  pt : PTypeInfo;
{$IFDEF WST_DELPHI}
  boolData : Boolean;
{$ENDIF WST_DELPHI}
begin
  prpName := APropInfo.ExternalName;
  pt := APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
{$IFDEF WST_DELPHI}
  if ( pt^.Kind = tkEnumeration ) and
     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
  then begin
    boolData := Boolean(GetOrdProp(AObject,APropInfo.PropInfo));
    //if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
      AStore.Put(APropInfo.NameSpace,prpName,pt,boolData);
  end else begin
{$ENDIF WST_DELPHI}
    FillChar(enumData,SizeOf(enumData),#0);
    case GetTypeData(pt)^.OrdType of
      otSByte :
        begin
          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ShortIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ShortIntData);
        end;
      otUByte :
        begin
          enumData.ByteData := Byte(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ByteData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ByteData);
        end;
      otSWord :
        begin
          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.SmallIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.SmallIntData);
        end;
      otUWord :
        begin
          enumData.WordData := Word(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.WordData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.WordData);
        end;
      otSLong :
        begin
          enumData.SLongIntData := LongInt(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.SLongIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.SLongIntData);
        end;
      otULong :
        begin
          enumData.ULongIntData := LongWord(GetOrdProp(AObject,APropInfo.PropInfo));
          if ( enumData.ULongIntData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
            AStore.Put(APropInfo.NameSpace,prpName,pt,enumData.ULongIntData);
        end;
    end;
{$IFDEF WST_DELPHI}
  end;
{$ENDIF WST_DELPHI}
end;

procedure Int64WriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : Int64;
begin
  locName := APropInfo.ExternalName;
  locData := GetInt64Prop(AObject,APropInfo.PropInfo);
  if ( locData <> 0 ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

procedure StringWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : string;
begin
  locName := APropInfo.ExternalName;
  locData := GetStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;

{$IFDEF WST_UNICODESTRING}
procedure UnicodeStringWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : UnicodeString;
begin
  locName := APropInfo.ExternalName;
  locData := GetUnicodeStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;
{$ENDIF WST_UNICODESTRING}

procedure WideStringWriterQualified(
  AObject : TObject;
  APropInfo : TPropSerializationInfo;
  AStore : IFormatterBase
);
var
  locName : string;
  locData : WideString;
begin
  locName := APropInfo.ExternalName;
  locData := GetWideStrProp(AObject,APropInfo.PropInfo);
  if ( locData <> '' ) or ( APropInfo.PersisteType = pstAlways ) then
    AStore.Put(APropInfo.NameSpace,locName,APropInfo.PropInfo^.PropType{$IFDEF WST_DELPHI}^{$ENDIF},locData);
end;


type
  TReaderInfo = record
    Simple : TPropertyReadProc;
    Qualified : TPropertyReadProc;
  end;

  TWriterInfo = record
    Simple : TPropertyWriteProc;
    Qualified : TPropertyWriteProc;
  end;

var
{$IFDEF FPC}
  //ReaderWriterInfoMap : array[0..1] of array[TTypeKind] of TReaderWriterInfo = (
  ReaderInfoMap : array[TTypeKind] of TReaderInfo = (
     // Readers
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkUnknown
      ( Simple : @IntEnumReader; Qualified : @IntEnumReaderQualified ;) , //tkInteger
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkChar
      ( Simple : @IntEnumReader; Qualified : @IntEnumReaderQualified ;) , //tkEnumeration
      ( Simple : @FloatReader; Qualified : @FloatReaderQualified ;) , //tkFloat
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkSet
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkMethod
      ( Simple : @StringReader; Qualified : @StringReaderQualified ;) , //tkSString
      ( Simple : @StringReader; Qualified : @StringReaderQualified ;) , //tkLString
      ( Simple : @StringReader; Qualified : @StringReaderQualified ;) , //tkAString
      ( Simple : @WideStringReader; Qualified : @WideStringReaderQualified ;) , //tkWString
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkVariant
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkArray
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkRecord
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkInterface
      ( Simple : @ClassReader; Qualified : @ClassReaderQualified ;) , //tkClass
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkObject
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkWChar
      ( Simple : @BoolReader; Qualified : @BoolReaderQualifier ;) , //tkBool
      ( Simple : @Int64Reader; Qualified : @Int64ReaderQualified ;) , //tkInt64
      ( Simple : @Int64Reader; Qualified : @Int64ReaderQualified ;) , //tkQWord
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;) , //tkDynArray
      ( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)   //tkInterfaceRaw
{$IFDEF WST_TKPROCVAR}
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkProcVar
{$ENDIF WST_TKPROCVAR}
{$IFDEF WST_UNICODESTRING}
     ,( Simple : @UnicodeStringReader; Qualified : @UnicodeStringReaderQualified ;)  //tkUString
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkUChar
{$ENDIF WST_UNICODESTRING}
{$IFDEF WST_TKHELPER}
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkHelper
{$ENDIF WST_TKHELPER}
{$IFDEF WST_TKFILE}
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkFile
{$ENDIF WST_TKFILE}
{$IFDEF WST_TKCLASSREF}
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkClassRef
{$ENDIF WST_TKCLASSREF}
{$IFDEF WST_TKPOINTER}
     ,( Simple : @ErrorFunc; Qualified : @ErrorFunc ;)  //tkPointer
{$ENDIF WST_TKPOINTER}
  );

  WriterInfoMap : array[TTypeKind] of TWriterInfo = (
     // Writers
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkUnknown
      ( Simple : @IntEnumWriter; Qualified : @IntEnumWriterQualified ;) , //tkInteger
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkChar
      ( Simple : @IntEnumWriter; Qualified : @IntEnumWriterQualified ;) , //tkEnumeration
      ( Simple : @FloatWriter; Qualified : @FloatWriterQualified ;) , //tkFloat
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkSet
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkMethod
      ( Simple : @StringWriter; Qualified : @StringWriterQualified ;) , //tkSString
      ( Simple : @StringWriter; Qualified : @StringWriterQualified ;) , //tkLString
      ( Simple : @StringWriter; Qualified : @StringWriterQualified ;) , //tkAString
      ( Simple : @WideStringWriter; Qualified : @WideStringWriterQualified ;) , //tkWString
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkVariant
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkArray
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkRecord
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkInterface
      ( Simple : @ClassWriter; Qualified : @ClassWriterQualified ;) , //tkClass
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkObject
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkWChar
      ( Simple : @BoolWriter; Qualified : @BoolWriterQualified ;) , //tkBool
      ( Simple : @Int64Writer; Qualified : @Int64WriterQualified ;) , //tkInt64
      ( Simple : @Int64Writer; Qualified : @Int64WriterQualified ;) , //tkQWord
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;) , //tkDynArray
      ( Simple : @ErrorProc; Qualified : @ErrorProc ;)   //tkInterfaceRaw
{$IFDEF WST_TKPROCVAR}
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkProcVar
{$ENDIF WST_TKPROCVAR}
{$IFDEF WST_UNICODESTRING}
     ,( Simple : @UnicodeStringWriter; Qualified : @UnicodeStringWriterQualified ;)  //tkUString
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkUChar
{$ENDIF WST_UNICODESTRING}
{$IFDEF WST_TKHELPER}
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkHelper
{$ENDIF WST_TKHELPER}
{$IFDEF WST_TKFILE}
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkFile
{$ENDIF WST_TKFILE}
{$IFDEF WST_TKCLASSREF}
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkClassRef
{$ENDIF WST_TKCLASSREF}
{$IFDEF WST_TKPOINTER}
     ,( Simple : @ErrorProc; Qualified : @ErrorProc ;)  //tkPointer
{$ENDIF WST_TKPOINTER}
  );
{$ENDIF FPC}

{$IFDEF WST_DELPHI}
  ReaderInfoMap : array[TTypeKind] of TReaderInfo = (
     // Readers
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkUnknown
      ( Simple : IntEnumReader; Qualified : IntEnumReaderQualified ;) , //tkInteger
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkChar
      ( Simple : IntEnumReader; Qualified : IntEnumReaderQualified ;) , //tkEnumeration
      ( Simple : FloatReader; Qualified : FloatReaderQualified ;) , //tkFloat
      ( Simple : StringReader; Qualified : StringReaderQualified ;) , //tkString
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkSet
      ( Simple : ClassReader; Qualified : ClassReaderQualified ;) , //tkClass
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkMethod
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkWChar
      ( Simple : StringReader; Qualified : StringReaderQualified ;) , //tkLString
      ( Simple : WideStringReader; Qualified : WideStringReaderQualified ;) , //tkWString
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkVariant
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkArray
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkRecord
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) , //tkInterface
      ( Simple : Int64Reader; Qualified : Int64ReaderQualified ;) , //tkInt64
      ( Simple : ErrorFunc; Qualified : ErrorFunc ;) //tkDynArray
{$IFDEF WST_UNICODESTRING}
     ,( Simple : UnicodeStringReader; Qualified : UnicodeStringReaderQualified ;)  //tkUString
{$ENDIF WST_UNICODESTRING}
{$IFDEF WST_TKCLASSREF}
     ,( Simple : ErrorFunc; Qualified : ErrorFunc ;)  //tkClassRef
{$ENDIF WST_TKCLASSREF}
{$IFDEF WST_TKPOINTER}
     ,( Simple : ErrorFunc; Qualified : ErrorFunc ;)  //tkPointer
{$ENDIF WST_TKPOINTER}
{$IFDEF WST_TKPROCEDURE}
     ,( Simple : ErrorFunc; Qualified : ErrorFunc ;)  //tkProcedure
{$ENDIF WST_TKPROCEDURE}
    );

    WriterInfoMap : array[TTypeKind] of TWriterInfo = (
     // Writers
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkUnknown
      ( Simple : IntEnumWriter; Qualified : IntEnumWriterQualified ;) , //tkInteger
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkChar
      ( Simple : IntEnumWriter; Qualified : IntEnumWriterQualified ;) , //tkEnumeration
      ( Simple : FloatWriter; Qualified : FloatWriterQualified ;) , //tkFloat
      ( Simple : StringWriter; Qualified : StringWriterQualified ;) , //tkSString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkSet
      ( Simple : ClassWriter; Qualified : ClassWriterQualified ;) , //tkClass
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkMethod
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkWChar
      ( Simple : StringWriter; Qualified : StringWriterQualified ;) , //tkLString
      ( Simple : WideStringWriter; Qualified : WideStringWriterQualified ;) , //tkWString
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkVariant
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkArray
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkRecord
      ( Simple : ErrorProc; Qualified : ErrorProc ;) , //tkInterface
      ( Simple : Int64Writer; Qualified : Int64WriterQualified ;) , //tkInt64
      ( Simple : ErrorProc; Qualified : ErrorProc ;) //tkDynArray
{$IFDEF WST_UNICODESTRING}
     ,( Simple : UnicodeStringWriter; Qualified : UnicodeStringWriterQualified ;)  //tkUString
{$ENDIF WST_UNICODESTRING}
{$IFDEF WST_TKCLASSREF}
     ,( Simple : ErrorProc; Qualified : ErrorProc ;)  //tkClassRef
{$ENDIF WST_TKCLASSREF}
{$IFDEF WST_TKPOINTER}
     ,( Simple : ErrorProc; Qualified : ErrorProc ;)  //tkPointer
{$ENDIF WST_TKPOINTER}
{$IFDEF WST_TKPROCEDURE}
     ,( Simple : ErrorProc; Qualified : ErrorProc ;)  //tkProcedure
{$ENDIF WST_TKPROCEDURE}
    );
{$ENDIF WST_DELPHI}

{ TObjectSerializer }

procedure TObjectSerializer.Prepare(ATypeRegistry : TTypeRegistry);
var
  locObjTypeData : PTypeData;
  locTypeInfo : PTypeInfo;
  c, i : Integer;
  ppi : PPropInfo;
  cl : TClass;
  serArray : array of TPropSerializationInfo;
  serInfo : TPropSerializationInfo;
  regItem, thisRegItem : TTypeRegistryItem;
  regPropItem : TPropertyItem;
  st : TPropStoreType;
  clPL : PPropList;
  eltFormEmpty, attFormEmpty, qualifiedElt, qualifiedAtt : Boolean;
begin
  FSerializationInfos.Clear();
  locTypeInfo := PTypeInfo(Target.ClassInfo);
  locObjTypeData := GetTypeData(locTypeInfo);
  c := locObjTypeData^.PropCount;
  if ( c > 0 ) then begin
    clPL := nil;
    SetLength(serArray,c);
    try
      FillChar(Pointer(serArray)^,SizeOf(TPropSerializationInfo)*c,#0);
      cl := Target;
      thisRegItem := ATypeRegistry.ItemByTypeInfo[locTypeInfo];
      regItem := thisRegItem;
      eltFormEmpty := ([trioQualifiedElement,trioUnqualifiedElement]*regItem.Options) = [];
      attFormEmpty := ([trioQualifiedAttribute,trioUnqualifiedAttribute]*regItem.Options) = [];
      qualifiedElt := (trioQualifiedElement in regItem.Options) and not(trioUnqualifiedElement in regItem.Options);
      qualifiedElt := eltFormEmpty or qualifiedElt;
      qualifiedAtt := (trioQualifiedAttribute in regItem.Options) and not(trioUnqualifiedAttribute in regItem.Options);
      qualifiedAtt := not(attFormEmpty) and qualifiedAtt;
      if (FRawPropList = nil) then
        GetPropList(locTypeInfo,FRawPropList);
      try
        for i := 0 to Pred(c) do begin
          ppi := FRawPropList^[i];
          st := IsStoredPropClass(cl,ppi);
          if ( st in [pstAlways,pstOptional] ) then begin
            regPropItem := regItem.FindProperty(ppi^.Name,pntInternalName);
            serInfo := TPropSerializationInfo.Create();
            serArray[ppi^.NameIndex] := serInfo;
            serInfo.FName := ppi^.Name;
            serInfo.FPersisteType := st;
            serInfo.FPropInfo := ppi;
            serInfo.FNameSpace := regItem.NameSpace;
            if Target.IsAttributeProperty(ppi^.Name) then
              serInfo.FStyle := ssAttibuteSerialization
            else
              serInfo.FStyle := ssNodeSerialization;
            if ( regPropItem <> nil ) then
              serInfo.FExternalName := regPropItem.ExternalName
            else
              serInfo.FExternalName := serInfo.FName;
            if (serInfo.FStyle = ssNodeSerialization) then
              serInfo.FQualifiedName := qualifiedElt
            else
              serInfo.FQualifiedName := qualifiedAtt;
            if serInfo.QualifiedName then begin
              serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Qualified;
              serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Qualified;
            end else begin
              serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Simple;
              serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Simple;
            end;
          end;
        end;
        //Check for inherited properties declared in other namespace
        GetMem(clPL,c*SizeOf(Pointer));
        cl := cl.ClassParent;
        while ( cl <> nil ) and ( cl <> TBaseComplexRemotable ) do begin
          c := GetTypeData(PTypeInfo(cl.ClassInfo))^.PropCount;
          if ( c > 0 ) then begin
            GetPropInfos(PTypeInfo(cl.ClassInfo),clPL);
            regItem := ATypeRegistry.Find(PTypeInfo(cl.ClassInfo),True);
            if ( regItem <> nil ) then begin
              eltFormEmpty := ([trioQualifiedElement,trioUnqualifiedElement]*regItem.Options) = [];
              attFormEmpty := ([trioQualifiedAttribute,trioUnqualifiedAttribute]*regItem.Options) = [];
              qualifiedElt := (trioQualifiedElement in regItem.Options) and not(trioUnqualifiedElement in regItem.Options);
              qualifiedElt := eltFormEmpty or qualifiedElt;
              qualifiedAtt := (trioQualifiedAttribute in regItem.Options) and not(trioUnqualifiedAttribute in regItem.Options);
              qualifiedAtt := not(attFormEmpty) and qualifiedAtt;
              for i := 0 to Pred(c) do begin
                ppi := clPL^[i];
                serInfo := serArray[ppi^.NameIndex];
                if ( serInfo <> nil ) then begin
                  if (serInfo.Style = ssNodeSerialization) then begin
                    if qualifiedElt then begin
                      if not(serInfo.FQualifiedName) or (thisRegItem.NameSpace <> regItem.NameSpace) then begin
                        serInfo.FNameSpace := regItem.NameSpace;
                        serInfo.FQualifiedName := True;
                        serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Qualified;
                        serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Qualified;
                      end;
                    end else begin
                      serInfo.FNameSpace := '';
                      serInfo.FQualifiedName := False;
                      serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Simple;
                      serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Simple;
                    end;
                  end else begin
                    if qualifiedAtt then begin
                      if not(serInfo.FQualifiedName) or (thisRegItem.NameSpace <> regItem.NameSpace) then begin
                        serInfo.FNameSpace := regItem.NameSpace;
                        serInfo.FQualifiedName := True;
                        serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Qualified;
                        serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Qualified;
                      end;
                    end else begin
                      serInfo.FNameSpace := '';
                      serInfo.FQualifiedName := False;
                      serInfo.FReaderProc := ReaderInfoMap[ppi^.PropType^.Kind].Simple;
                      serInfo.FWriterProc := WriterInfoMap[ppi^.PropType^.Kind].Simple;
                    end;
                  end;
                end;
              end;
            end;
          end;
          cl := cl.ClassParent;
        end;
        // Fill the list now
        for i := 0 to Pred(Length(serArray)) do begin
          if ( serArray[i] <> nil ) then begin
            FSerializationInfos.Add(serArray[i]);
            serArray[i] := nil;
          end;
        end;
      except
        for i := 0 to Pred(locObjTypeData^.PropCount) do
          serArray[i].Free();
        raise;
      end;
    finally
      if ( clPL <> nil ) then
        FreeMem(clPL,locObjTypeData^.PropCount*SizeOf(Pointer));
      SetLength(serArray,0);
    end;
  end;
end;

function TObjectSerializer.FindInfo(const APropName: string): TPropSerializationInfo;
var
  i : Integer;
begin
  Result := nil;
  if ( FSerializationInfos.Count > 0 ) then begin
    for i := 0 to Pred(FSerializationInfos.Count) do begin
      if SameText(APropName,TPropSerializationInfo(FSerializationInfos[i]).ExternalName) then begin
        Result := TPropSerializationInfo(FSerializationInfos[i]);
        Break;
      end;
    end;
  end;
end;

procedure TObjectSerializer.UpdateExternalName(
  const APropName,
        AExtPropName : string
);
var
  itm : TPropSerializationInfo;
begin
  itm := FindInfo(APropName);
  if ( itm <> nil ) then
    itm.FExternalName := AExtPropName;
end;

constructor TObjectSerializer.Create(
  ATargetClass : TBaseComplexRemotableClass;
  ATypeRegistry : TTypeRegistry
);
begin
  Assert(ATargetClass <> nil);
  Assert(ATypeRegistry <> nil);
  FTarget := ATargetClass;
  FSerializationInfos := TObjectList.Create(True);
  Prepare(ATypeRegistry);
end;

destructor TObjectSerializer.Destroy();
begin
  if ( FRawPropList <> nil ) then
    FreeMem(FRawPropList,GetTypeData(PTypeInfo(Target.ClassInfo))^.PropCount*SizeOf(Pointer));
  FSerializationInfos.Free();
  inherited Destroy();
end;

procedure TObjectSerializer.Read(
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : Integer;
  locSerInfo : TPropSerializationInfo;
begin
  if (AObject = nil) and Target.InheritsFrom(TBaseArrayRemotable) then
    AObject := Target.Create();
  oldSS := AStore.GetSerializationStyle();
  if ( osoDontDoBeginRead in Options ) or ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      if (AObject = nil) then
        AObject := Target.Create();
      c := FSerializationInfos.Count;
      if ( c > 0 ) then begin
        for i := 0 to Pred(c) do begin
          locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
          if ( locSerInfo.Style <> AStore.GetSerializationStyle() ) then
            AStore.SetSerializationStyle(locSerInfo.Style);
          if ( not locSerInfo.ReaderProc(AObject,locSerInfo,AStore) ) and
             ( locSerInfo.PersisteType = pstAlways )
          then begin
            AStore.Error(SERR_ParamaterNotFound,[locSerInfo.ExternalName]);
          end;
        end;
      end;
    finally
      if not ( osoDontDoBeginRead in Options ) then
        AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

procedure TObjectSerializer.Save(
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : Integer;
  locSerInfo : TPropSerializationInfo;
begin
  oldSS := AStore.GetSerializationStyle();
  if not ( osoDontDoBeginWrite in Options ) then
    AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    c := FSerializationInfos.Count;
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
        if ( locSerInfo.Style <> AStore.GetSerializationStyle() ) then
          AStore.SetSerializationStyle(locSerInfo.Style);
        locSerInfo.WriterProc(AObject,locSerInfo,AStore);
      end;
    end;
  finally
    if not ( osoDontDoBeginWrite in Options ) then
      AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

{ TSimpleContentObjectSerializer }

procedure TSimpleContentObjectSerializer.Prepare(ATypeRegistry : TTypeRegistry);
var
  thisRegItem, regItem : TTypeRegistryItem;
  serArray : array of TPropSerializationInfo;
  cl : TClass;
  attFormEmpty, qualifiedAtt : Boolean;

  procedure InitPropItem(const APropInfo : PPropInfo);
  var
    regPropItem : TPropertyItem;
    serInfo : TPropSerializationInfo;
    st : TPropStoreType;
  begin
    st := IsStoredPropClass(cl,APropInfo);
    if ( st in [pstAlways,pstOptional] ) then begin
      regPropItem := regItem.FindProperty(APropInfo^.Name,pntInternalName);
      serInfo := TPropSerializationInfo.Create();
      serArray[APropInfo^.NameIndex] := serInfo;
      serInfo.FName := APropInfo^.Name;
      serInfo.FExternalName := serInfo.FName;
      serInfo.FPersisteType := st;
      serInfo.FPropInfo := APropInfo;
      //serInfo.FNameSpace := regItem.NameSpace;
      serInfo.FStyle := ssAttibuteSerialization;
      serInfo.FQualifiedName := True;
      serInfo.FNameSpace := '';
      if ( regPropItem <> nil ) then begin
        serInfo.FExternalName := regPropItem.ExternalName;
        //if not ( trioNonQualifiedName in regPropItem.Options ) then begin
        if not(attFormEmpty) and qualifiedAtt then begin
          serInfo.FNameSpace := regItem.NameSpace;
          serInfo.FQualifiedName := True;
        end;
      end;
      if serInfo.QualifiedName then begin
        serInfo.FReaderProc := ReaderInfoMap[APropInfo^.PropType^.Kind].Qualified;
        serInfo.FWriterProc := WriterInfoMap[APropInfo^.PropType^.Kind].Qualified;
      end else begin
        serInfo.FReaderProc := ReaderInfoMap[APropInfo^.PropType^.Kind].Simple;
        serInfo.FWriterProc := WriterInfoMap[APropInfo^.PropType^.Kind].Simple;
      end;
    end;
  end;

  procedure InheritedInitPropItem(const APropInfo : PPropInfo);
  var
    regPropItem : TPropertyItem;
    serInfo : TPropSerializationInfo;
  begin
    serInfo := serArray[APropInfo^.NameIndex];
    if ( serInfo <> nil ) then begin
      regPropItem := regItem.FindProperty(APropInfo^.Name,pntInternalName);
      if ( regPropItem <> nil ) then begin
        //if not ( trioNonQualifiedName in regPropItem.Options ) then begin
        if not(attFormEmpty) and qualifiedAtt then begin
          serInfo.FNameSpace := regItem.NameSpace;
          serInfo.FQualifiedName := True;
        end;
      end else begin
        if ( thisRegItem.NameSpace <> regItem.NameSpace ) then begin
          if ( serInfo.FNameSpace <> '' ) then begin
            serInfo.FNameSpace := regItem.NameSpace;
            serInfo.FQualifiedName := True;
            serInfo.FReaderProc := ReaderInfoMap[APropInfo^.PropType^.Kind].Qualified;
            serInfo.FWriterProc := WriterInfoMap[APropInfo^.PropType^.Kind].Qualified;
          end;
        end;
      end;
    end;
  end;

var
  locObjTypeData : PTypeData;
  locTypeInfo : PTypeInfo;
  c, i : Integer;
  clPL : PPropList;
begin
  FSerializationInfos.Clear();
  locTypeInfo := PTypeInfo(Target.ClassInfo);
  locObjTypeData := GetTypeData(locTypeInfo);
  c := locObjTypeData^.PropCount;
  if ( c > 0 ) then begin
    clPL := nil;
    SetLength(serArray,c);
    try
      FillChar(Pointer(serArray)^,SizeOf(TPropSerializationInfo)*c,#0);
      cl := Target;
      thisRegItem := ATypeRegistry.ItemByTypeInfo[locTypeInfo];
      regItem := thisRegItem;
      attFormEmpty := ([trioQualifiedAttribute,trioUnqualifiedAttribute]*regItem.Options) = [];
      qualifiedAtt := (trioQualifiedAttribute in regItem.Options) and not(trioUnqualifiedAttribute in regItem.Options);
      GetPropList(locTypeInfo,FRawPropList);
      try
        for i := 0 to Pred(c) do begin
          InitPropItem(FRawPropList^[i]);
        end;
        //Check for inherited properties declared in other namespace
        GetMem(clPL,c*SizeOf(Pointer));
        cl := cl.ClassParent;
        while ( cl <> nil ) and ( cl <> TBaseComplexSimpleContentRemotable ) do begin
          c := GetTypeData(PTypeInfo(cl.ClassInfo))^.PropCount;
          if ( c > 0 ) then begin
            GetPropInfos(PTypeInfo(cl.ClassInfo),clPL);
            regItem := ATypeRegistry.Find(PTypeInfo(cl.ClassInfo),True);
            if ( regItem <> nil ) then begin
              for i := 0 to Pred(c) do begin
                InheritedInitPropItem(clPL^[i]);
              end;
            end;
          end;
          cl := cl.ClassParent;
        end;
        // Fill the list now
        for i := 0 to Pred(Length(serArray)) do begin
          if ( serArray[i] <> nil ) then begin
            FSerializationInfos.Add(serArray[i]);
            serArray[i] := nil;
          end;
        end;
      except
        for i := 0 to Pred(locObjTypeData^.PropCount) do
          serArray[i].Free();
        raise;
      end;
    finally
      if ( clPL <> nil ) then
        FreeMem(clPL,locObjTypeData^.PropCount*SizeOf(Pointer));
      SetLength(serArray,0);
    end;
  end;
end;

function TSimpleContentObjectSerializer.FindInfo(const APropName: string): TPropSerializationInfo;
var
  i : Integer;
begin
  Result := nil;
  if ( FSerializationInfos.Count > 0 ) then begin
    for i := 0 to Pred(FSerializationInfos.Count) do begin
      if SameText(APropName,TPropSerializationInfo(FSerializationInfos[i]).ExternalName) then begin
        Result := TPropSerializationInfo(FSerializationInfos[i]);
        Break;
      end;
    end;
  end;
end;

procedure TSimpleContentObjectSerializer.UpdateExternalName(
  const APropName,
        AExtPropName : string
);
var
  itm : TPropSerializationInfo;
begin
  itm := FindInfo(APropName);
  if ( itm <> nil ) then
    itm.FExternalName := AExtPropName;
end;

constructor TSimpleContentObjectSerializer.Create(
  ATargetClass : TBaseComplexSimpleContentRemotableClass;
  ATypeRegistry : TTypeRegistry
);
begin
  Assert(ATargetClass <> nil);
  Assert(ATypeRegistry <> nil);
  FTarget := ATargetClass;
  FSerializationInfos := TObjectList.Create(True);
  Prepare(ATypeRegistry);
end;

destructor TSimpleContentObjectSerializer.Destroy();
begin
  if ( FRawPropList <> nil ) then
    FreeMem(FRawPropList,GetTypeData(PTypeInfo(Target.ClassInfo))^.PropCount*SizeOf(Pointer));
  FSerializationInfos.Free();
  inherited Destroy();
end;

type
  TBaseComplexSimpleContentRemotableCrack = class(TBaseComplexSimpleContentRemotable) end;

procedure TSimpleContentObjectSerializer.Read(
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : Integer;
  locSerInfo : TPropSerializationInfo;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( osoDontDoBeginRead in Options ) or ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      if not Assigned(AObject) then
        AObject := Target.Create();
      TBaseComplexSimpleContentRemotableCrack(AObject).LoadValue(AObject,AStore);
      c := FSerializationInfos.Count;
      if ( c > 0 ) then begin
        AStore.SetSerializationStyle(ssAttibuteSerialization);
        for i := 0 to Pred(c) do begin
          locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
          if ( not locSerInfo.ReaderProc(AObject,locSerInfo,AStore) ) and
             ( locSerInfo.PersisteType = pstAlways )
          then begin
            AStore.Error(SERR_ParamaterNotFound,[locSerInfo.ExternalName]);
          end;
        end;
      end;
    finally
      if not ( osoDontDoBeginRead in Options ) then
        AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

procedure TSimpleContentObjectSerializer.Save(
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  oldSS : TSerializationStyle;
  i, c : Integer;
  locSerInfo : TPropSerializationInfo;
begin
  oldSS := AStore.GetSerializationStyle();
  if not ( osoDontDoBeginWrite in Options ) then
    AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    TBaseComplexSimpleContentRemotableCrack(AObject).SaveValue(AObject,AStore);
    c := FSerializationInfos.Count;
    if ( c > 0 ) then begin
      AStore.SetSerializationStyle(ssAttibuteSerialization);
      for i := 0 to Pred(c) do begin
        locSerInfo := TPropSerializationInfo(FSerializationInfos[i]);
        locSerInfo.WriterProc(AObject,locSerInfo,AStore);
      end;
    end;
  finally
    if not ( osoDontDoBeginWrite in Options ) then
      AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

{ TBaseComplexRemotableInitializer }

class function TBaseComplexRemotableInitializer.CanHandle(ATypeInfo : PTypeInfo) : Boolean;
begin
  Result := ( ATypeInfo <> nil ) and
            ( ATypeInfo^.Kind = tkClass ) and
            GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TBaseComplexRemotable);
end;

class function TBaseComplexRemotableInitializer.GetItemClass(
  const ATypeInfo : PTypeInfo
) : TTypeRegistryItemClass;
begin
  Result := TBaseComplexTypeRegistryItem;
end;

{$IFDEF TRemotableTypeInitializer_Initialize}
class function TBaseComplexRemotableInitializer.Initialize(
  ATypeInfo : PTypeInfo;
  ARegistryItem : TTypeRegistryItem
) : Boolean;
begin
end;
{$ENDIF TRemotableTypeInitializer_Initialize}

{ TSimpleContentObjectRemotableInitializer }

class function TSimpleContentObjectRemotableInitializer.CanHandle(ATypeInfo : PTypeInfo) : Boolean;
begin
  Result := ( ATypeInfo <> nil ) and
            ( ATypeInfo^.Kind = tkClass ) and
            GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TBaseComplexSimpleContentRemotable);
end;

class function TSimpleContentObjectRemotableInitializer.GetItemClass(
  const ATypeInfo : PTypeInfo
) : TTypeRegistryItemClass;
begin
  Result := TSimpleContentObjectRegistryItem;
end;

{$IFDEF TRemotableTypeInitializer_Initialize}
class function TSimpleContentObjectRemotableInitializer.Initialize(
  ATypeInfo : PTypeInfo;
  ARegistryItem : TTypeRegistryItem
) : Boolean;
begin
end;
{$ENDIF TRemotableTypeInitializer_Initialize}

{ TBaseComplexTypeRegistryItem }

procedure TBaseComplexTypeRegistryItem.Init();
begin
  inherited Init();
  if (FSerializer = nil) then
    FSerializer := TObjectSerializer.Create(TBaseComplexRemotableClass(GetTypeData(DataType)^.ClassType),Owner);
  FSerializer.Prepare(Self.Owner);
end;

destructor TBaseComplexTypeRegistryItem.Destroy();
begin
  FSerializer.Free();
  inherited Destroy();
end;

procedure TBaseComplexTypeRegistryItem.RegisterExternalPropertyName(
  const APropName,
        AExtPropName : string
);
begin
  inherited RegisterExternalPropertyName(APropName, AExtPropName);
  GetSerializer().UpdateExternalName(APropName,AExtPropName);
end;

function TBaseComplexTypeRegistryItem.GetSerializer() : TObjectSerializer;
begin
  Result := FSerializer;
end;

{ TSimpleContentObjectRegistryItem }

procedure TSimpleContentObjectRegistryItem.Init();
begin
  inherited Init();
  if ( FSerializer <> nil ) then
    FreeAndNil(FSerializer);
  FSerializer := TSimpleContentObjectSerializer.Create(TBaseComplexSimpleContentRemotableClass(GetTypeData(DataType)^.ClassType),Owner);
end;

destructor TSimpleContentObjectRegistryItem.Destroy();
begin
  FSerializer.Free();
  inherited Destroy();
end;

procedure TSimpleContentObjectRegistryItem.RegisterExternalPropertyName(
  const APropName,
        AExtPropName : string
);
begin
  inherited RegisterExternalPropertyName(APropName, AExtPropName);
  GetSerializer().UpdateExternalName(APropName,AExtPropName);
end;

procedure TSimpleContentObjectRegistryItem.SetPropertyOptions(
  const APropName: string;
  const AOptions: TTypeRegistryItemOptions
);
begin
  inherited SetPropertyOptions(APropName,AOptions);
  Init();
end;

function TSimpleContentObjectRegistryItem.GetSerializer() : TSimpleContentObjectSerializer;
begin
  Result := FSerializer;
end;

end.
