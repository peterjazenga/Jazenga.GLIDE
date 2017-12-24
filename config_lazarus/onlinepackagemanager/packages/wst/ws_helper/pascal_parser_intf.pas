{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit pascal_parser_intf;

interface

uses
  Classes, SysUtils, Contnrs,
  pparser, pastree;

const
  sEMBEDDED_TYPE = '_E_T_';
  sIS_GROUP      = '_IS_GROUP_';
  sEXTERNAL_NAME = '_E_N_';
  sATTRIBUTE = '_ATTRIBUTE_';
  sARRAY_ITEM_NAME = 'ARRAY_ITEM_NAME';
  sARRAY_ITEM_EXT_NAME = 'ARRAY_ITEM_EXT_NAME';
  sARRAY_STYLE = 'ARRAY_STYLE';
    sARRAY_STYLE_SCOPED = 'ARRAY_STYLE_SCOPED';
    sARRAY_STYLE_EMBEDDED = 'ARRAY_STYLE_EMBEDDED';
  sARRAY_IS_COLLECTION = 'ARRAY_COLLECTION';
  sCHOICE = 'CHOICE';
  sWST_PROP_STORE_PREFIX = 'wstHas_';
  sFILE_NAME = 'FileName';
  sNS_COUNT = 'NS_Count';
  sNS_ITEM = 'NS_';
  
  sXSD_NS = 'http://www.w3.org/2001/XMLSchema';

  CASE_SENSITIVE_DEFAULT = True;

{$IF not Declared(TInterfaceSection) }
type             
  TInterfaceSection = TPasSection;
{$IFEND}
  
type
{$UNDEF wst_use_fplist}
{$IFDEF FPC}
  {$IF Defined(FPC_FULLVERSION) and (FPC_FULLVERSION>=20601)}
    {$DEFINE wst_use_fplist}
  {$IFEND}
{$ENDIF FPC}
{$IFDEF wst_use_fplist}
  TList2 = TFPList;
{$ELSE wst_use_fplist}
  TList2 = TList;
{$ENDIF wst_use_fplist}

  TElementNameKind = ( elkDeclaredName, elkName );
  TElementNameKinds = set of TElementNameKind;

  TBindingStyle = ( bsDocument, bsRPC, bsUnknown );

  TXSDStringMaping = (xsmUnicodeString, xsmString);
  
const
  BindingStyleNames : array[TBindingStyle] of string = ( 'Document', 'RPC', 'Unknown' );
  NAME_KINDS_DEFAULT = [elkDeclaredName, elkName];
  
type
  TArrayStyle = ( asScoped, asEmbeded );

  ESymbolException = class(Exception)
  end;
  { TwstBinding }

  TwstBinding = class(TPasElement)
  private
    FEasyIntf : TPasClassType;
    FAddress: string;
    FBindingStyle: TBindingStyle;
    FIntf: TPasClassType;
  private  
    procedure setEasyIntf(const AValue : TPasClassType);
  public
    constructor Create(
      const AName : string;
            AIntf : TPasClassType;
            AParent: TPasElement
    );reintroduce;
    destructor Destroy();override;
    property Intf : TPasClassType read FIntf;
    property EasyIntf : TPasClassType read FEasyIntf write setEasyIntf;
    property Address : string read FAddress write FAddress;
    property BindingStyle : TBindingStyle read FBindingStyle write FBindingStyle;
  end;
  
  { TPropertyHolder }

  TPropertyHolder = class
  private
    FObjects : TObjectList;
    FProps : TObjectList;
  private
    procedure FreeList(AObject : TObject);
  public
    constructor Create();
    destructor Destroy();override;
    procedure SetValue(AOwner : TObject; const AName, AValue : string);
    function GetValue(AOwner : TObject; const AName : string) : string;
    function HasValue(AOwner : TObject; const AName : string) : Boolean;
    function FindList(AOwner : TObject) : TStrings;
    function GetList(AOwner : TObject) : TStrings;
  end;
  
  { TwstPasTreeContainer }

  TwstPasTreeContainer = class(TPasTreeContainer)
  private
    FCurrentModule: TPasModule;
    FBindingList : TObjectList;
    FProperties : TPropertyHolder;
    FCaseSensitive : Boolean;
    FDefaultSearchNameKinds: TElementNameKinds;
    FXsdStringMaping: TXSDStringMaping;
  private
    function GetBinding(AIndex : Integer): TwstBinding;
    function GetBindingCount: Integer;
  public
    constructor Create();
    destructor Destroy();override;
    function CreateElement(
            AClass              : TPTreeElement;
      const AName               : String;
            AParent             : TPasElement;
            AVisibility         : TPasMemberVisibility;
      const ASourceFilename     : String;
            ASourceLinenumber   : Integer
    ): TPasElement;overload;override;
    function CreateArray(
      const AName              : string;
            AItemType          : TPasType;
      const AItemName,
            AItemExternalName  : string;
      const AStyle             : TArrayStyle
    ) : TPasArrayType;
    function GetArrayItemName(AArray : TPasArrayType) : string;
    function GetArrayItemExternalName(AArray : TPasArrayType) : string;
    function GetArrayStyle(AArray : TPasArrayType) : TArrayStyle;
    procedure SetArrayStyle(AArray : TPasArrayType; const AStyle : TArrayStyle);
    procedure SetArrayItemExternalName(AArray : TPasArrayType; const AExternalName : string);
    function IsCollection(AArray : TPasArrayType) : Boolean;
    procedure SetCollectionFlag(AArray : TPasArrayType; const AFlag : Boolean);
    function FindElement(const AName: String): TPasElement; overload; override;
    function FindElement(const AName: String; const ANameKinds : TElementNameKinds): TPasElement; overload;
    function FindElementNS(const AName, ANameSpace : string): TPasElement;
    function FindElementInModule(
        const AName: String;
            AModule: TPasModule;
      const ANameKinds : TElementNameKinds
    ): TPasElement; overload;
    function FindElementInModule(
        const AName: String;
            AModule: TPasModule
    ): TPasElement; overload;
    function FindModule(const AName: String): TPasModule;override;
    function IsEnumItemNameUsed(const AName : string; AModule : TPasModule) : Boolean;overload;
    function IsEnumItemNameUsed(const AName : string) : Boolean;overload;
    procedure SetCurrentModule(AModule : TPasModule);
    property CurrentModule : TPasModule read FCurrentModule;
    
    function AddBinding(const AName : string; AIntf : TPasClassType):TwstBinding;
    procedure DeleteBinding(ABinding : TwstBinding);
    function FindBinding(const AName : string):TwstBinding;overload;
    function FindBinding(const AIntf : TPasClassType; const AOrder : Integer = 0):TwstBinding;overload;
    property BindingCount : Integer read GetBindingCount;
    property Binding[AIndex : Integer] : TwstBinding read GetBinding;
    property Properties : TPropertyHolder read FProperties;
    
    procedure FreeProperties(AObject : TPasElement);
    procedure RegisterExternalAlias(AObject : TPasElement; const AExternalName : String);
    function SameName(AObject : TPasElement; const AName : string) : Boolean;
    function HasExternalName(AObject : TPasElement) : Boolean;
    function GetExternalName(AObject : TPasElement) : string;overload;
    function GetExternalName(AObject : TPasElement; const AReturnNameIfEmpty : Boolean) : string;overload;
    function GetNameSpace(AType : TPasType) : string ;
    function IsAttributeProperty(AObject : TPasVariable) : Boolean;
    procedure SetPropertyAsAttribute(AObject : TPasVariable; const AValue : Boolean);
    function IsChoiceProperty(AObject : TPasProperty) : Boolean;
    procedure SetPropertyAsChoice(AObject : TPasProperty; const AValue : Boolean);

    function IsInitNeed(AType: TPasType): Boolean;
    function IsOfType(AType: TPasType; AClass: TClass): Boolean;

    property CaseSensitive : Boolean read FCaseSensitive write FCaseSensitive default CASE_SENSITIVE_DEFAULT;
    property DefaultSearchNameKinds : TElementNameKinds
        read FDefaultSearchNameKinds
       write FDefaultSearchNameKinds
     default NAME_KINDS_DEFAULT;
    property XsdStringMaping : TXSDStringMaping read FXsdStringMaping write FXsdStringMaping;
  end;

  TPasNativeModule = class(TPasModule)
  end;
  
  TPasClassTypeClass = class of TPasClassType;
  TPasNativeSimpleContentClassType = class;

  { TPasNativeClassType }

  TPasNativeClassType = class(TPasClassType)
  private
    FExtendableType : TPasNativeSimpleContentClassType;
  public
    destructor Destroy();override;
    procedure SetExtendableType(AExtendableType : TPasNativeSimpleContentClassType);
    property ExtendableType : TPasNativeSimpleContentClassType read FExtendableType;
  end;

  TPasNativeSimpleContentClassType = class(TPasNativeClassType) end;
  TPasNativeSpecialSimpleContentClassType = class(TPasNativeSimpleContentClassType) end;

  { TPasNativeSimpleType }

  TPasNativeSimpleType = class(TPasType)
  private
    FExtendableType: TPasNativeSimpleContentClassType;
  public
    destructor Destroy();override;
    procedure SetExtendableType(AExtendableType : TPasNativeSimpleContentClassType);
    property ExtendableType : TPasNativeSimpleContentClassType read FExtendableType;
  end;
  
  TPasNativeSpecialSimpleType = class(TPasNativeSimpleType) end;

  function GetParameterIndex(
          AProcType  : TPasProcedureType;
    const AParamName : string;
    const AStartPos  : Integer = 0
  ) : Integer;
  function FindParameter(
          AProcType  : TPasProcedureType;
    const AParamName : string;
    const AStartPos  : Integer = 0
  ) : TPasArgument;
  function FindMember(AClass : TPasClassType; const AName : string) : TPasElement ; overload;
  function FindMember(AClass : TPasRecordType; const AName : string) : TPasElement ; overload;
  function FindModule(AType: TPasType) : TPasModule;
  function GetElementCount(AList : TList2; AElementClass : TPTreeElement):Integer ;
  
  function GetUltimeType(AType : TPasType) : TPasType;overload;
  function GetUltimeType(
    AType      : TPasType;
    AContainer : TwstPasTreeContainer
  ) : TPasType;overload;
  function FindActualType(
    AType      : TPasType;
    AContainer : TwstPasTreeContainer
  ) : TPasType;
  function MakeInternalSymbolNameFrom(const AName : string) : string ;

  function CreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
  function JavaCreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
  procedure CreateDefaultBindingForIntf(ATree : TwstPasTreeContainer);
  
implementation
uses 
  parserutils, wst_types, wst_consts;

const
    SIMPLE_TYPES_COUNT = 14;
      SIMPLE_TYPES : Array[0..Pred(SIMPLE_TYPES_COUNT)] Of array[0..2] of string = (
          ('integer', 'TComplexInt32SContentRemotable', 'int'),
          ('LongWord', 'TComplexInt32UContentRemotable', 'unsignedInt' ),
          ('SmallInt', 'TComplexInt16SContentRemotable', 'short'),
          ('ShortInt', 'TComplexInt8SContentRemotable', 'byte'),
          ('char', '', ''),
          ('boolean', 'TComplexBooleanContentRemotable', 'boolean'),
          ('Byte', 'TComplexInt8UContentRemotable', 'unsignedByte'),
          ('Word', 'TComplexInt16UContentRemotable', 'unsignedShort'),
          ('Longint', 'TComplexInt32SContentRemotable', 'int'),
          ('Int64', 'TComplexInt64SContentRemotable', 'long'),
          ('Qword', 'TComplexInt64UContentRemotable', 'unsignedLong'),
          ('Single', 'TComplexFloatSingleContentRemotable', 'float'),
          ('Double', 'TComplexFloatDoubleContentRemotable', 'double'),
          ('Extended', 'TComplexFloatExtendedContentRemotable', 'double')
        );
   BOXED_TYPES_COUNT = 2;
     BOXED_TYPES : Array[0..Pred(BOXED_TYPES_COUNT)] Of array[0..2] of string = (
        ('TBase64StringRemotable', 'TBase64StringExtRemotable', 'base64Binary'),
        ('TBase16StringRemotable', 'TBase16StringExtRemotable', 'hexBinary')
     );
   SPECIAL_SIMPLE_TYPES_COUNT = 5 {$IFDEF WST_UNICODESTRING} + 1 {$ENDIF WST_UNICODESTRING};
     SPECIAL_SIMPLE_TYPES : Array[0..Pred(SPECIAL_SIMPLE_TYPES_COUNT)] Of array[0..2] of string = (
          ('Currency', 'TComplexCurrencyContentRemotable', 'decimal'),
{$IFDEF WST_UNICODESTRING}
          ('UnicodeString', 'TComplexUnicodeStringContentRemotable', 'string'),
{$ENDIF WST_UNICODESTRING}
          ('string', 'TComplexStringContentRemotable', 'string'),
          ('WideString', 'TComplexWideStringContentRemotable', 'string'),
          ('AnsiChar', 'TComplexAnsiCharContentRemotable', 'string'),
          ('WideChar', 'TComplexWideCharContentRemotable', 'string')
     );
// JAVA
    JAVA_SIMPLE_TYPES_COUNT = 11;
      JAVA_SIMPLE_TYPES : Array[0..Pred(JAVA_SIMPLE_TYPES_COUNT)] Of array[0..2] of string = (
          ('char', '', ''),
          ('byte', '', ''),
          ('short', '', ''),
          ('int', '', ''),
          ('long', '', ''),
          ('float', '', ''),
          ('double', '', ''),
          ('boolean', '', ''),
          ('String', '', 'string'),
          ('String', '', 'dateTime') , //('java.util.OffsetDateTime', '', 'dateTime') ,
          //('java.time.OffsetTime', '', 'time') ,
          //('java.time.Duration', '', 'duration') ,
          ('java.math.BigDecimal', '', 'decimal')
        );


procedure JavaRegisterSimpleTypes(
  ADest      : TPasModule;
  AContainer : TwstPasTreeContainer
);
var
  i : Integer;
  splTyp : TPasNativeSimpleType;
  syb : TPasNativeSimpleContentClassType;
  s : string;
  typlst : array[0..Pred(JAVA_SIMPLE_TYPES_COUNT)] of TPasNativeSimpleType;
begin
  for i := Low(JAVA_SIMPLE_TYPES) to High(JAVA_SIMPLE_TYPES) do begin
    splTyp := TPasNativeSimpleType(
                AContainer.CreateElement(
                  TPasNativeSimpleType,JAVA_SIMPLE_TYPES[i][0],
                  ADest.InterfaceSection,visPublic,'',0
                )
              );
    ADest.InterfaceSection.Declarations.Add(splTyp);
    ADest.InterfaceSection.Types.Add(splTyp);
    typlst[i] := splTyp;
  end;
  for i := Low(JAVA_SIMPLE_TYPES) to High(JAVA_SIMPLE_TYPES) do begin
    s := JAVA_SIMPLE_TYPES[i][1];
    if not IsStrEmpty(s) then begin
      syb := AContainer.FindElementInModule(JAVA_SIMPLE_TYPES[i][1],ADest)
               as TPasNativeSimpleContentClassType;
      if not Assigned(syb) then begin
        syb := TPasNativeSimpleContentClassType(
                 AContainer.CreateElement(
                   TPasNativeSimpleContentClassType,s,
                   ADest.InterfaceSection,visDefault,'',0
                 )
               );
        ADest.InterfaceSection.Declarations.Add(syb);
        ADest.InterfaceSection.Types.Add(syb);
      end;
      typlst[i].SetExtendableType(syb);
    end;
  end;
  for i := Low(JAVA_SIMPLE_TYPES) to High(JAVA_SIMPLE_TYPES) do begin
    splTyp := typlst[i];
    if not IsStrEmpty(JAVA_SIMPLE_TYPES[i][2]) then begin
      AContainer.RegisterExternalAlias(splTyp,JAVA_SIMPLE_TYPES[i][2]);
      if ( splTyp.ExtendableType <> nil ) then begin
        AContainer.RegisterExternalAlias(splTyp.ExtendableType,JAVA_SIMPLE_TYPES[i][2]);
      end;
    end;
  end;
end;

procedure AddSystemSymbol(
  ADest      : TPasModule;
  AContainer : TwstPasTreeContainer;
  AXsdString : TXSDStringMaping
);

  procedure RegisterSpecialSimpleTypes();
  var
    i : Integer;
    splTyp : TPasNativeSpecialSimpleType;
    syb : TPasNativeSpecialSimpleContentClassType;
    s : string;
    typlst : array[0..Pred(SPECIAL_SIMPLE_TYPES_COUNT)] of TPasNativeSpecialSimpleType;
    typeOrders : array[0..Pred(SPECIAL_SIMPLE_TYPES_COUNT)] of Byte;
    typeNameArray : array[0..Pred(SPECIAL_SIMPLE_TYPES_COUNT)] of array[0..2] of string;
  begin
    typeOrders[0] := 0;
    i := 1;
{$IFDEF WST_UNICODESTRING}
    if (AXsdString = xsmString) then begin
      typeOrders[1] := 2;
      typeOrders[2] := 1;
    end else begin
      typeOrders[1] := 1;
      typeOrders[2] := 2;
    end;
    i := 3;
{$ENDIF WST_UNICODESTRING}
    for i := i to High(typeOrders) do
      typeOrders[i] := i;
    for i := Low(typeOrders) to High(typeOrders) do begin
      typeNameArray[i][0] := SPECIAL_SIMPLE_TYPES[typeOrders[i]][0];
      typeNameArray[i][1] := SPECIAL_SIMPLE_TYPES[typeOrders[i]][1];
      typeNameArray[i][2] := SPECIAL_SIMPLE_TYPES[typeOrders[i]][2];
    end;
    for i := Low(typeNameArray) to High(typeNameArray) do begin
      splTyp := TPasNativeSpecialSimpleType(AContainer.CreateElement(TPasNativeSpecialSimpleType,typeNameArray[i][0],ADest.InterfaceSection,visPublic,'',0));
      ADest.InterfaceSection.Declarations.Add(splTyp);
      ADest.InterfaceSection.Types.Add(splTyp);
      typlst[i] := splTyp;
      s := typeNameArray[i][1];
      if not IsStrEmpty(s) then begin
        syb := AContainer.FindElementInModule(s,ADest) as TPasNativeSpecialSimpleContentClassType;
        if not Assigned(syb) then begin
          syb := TPasNativeSpecialSimpleContentClassType(AContainer.CreateElement(TPasNativeSpecialSimpleContentClassType,s,ADest.InterfaceSection,visDefault,'',0));
          ADest.InterfaceSection.Declarations.Add(syb);
          ADest.InterfaceSection.Types.Add(syb);
        end;
        splTyp.SetExtendableType(syb);
      end;
    end;
    for i := Low(typeNameArray) to High(typeNameArray) do begin
      splTyp := typlst[i];
      if not IsStrEmpty(typeNameArray[i][2]) then begin
        AContainer.RegisterExternalAlias(splTyp,typeNameArray[i][2]);
        if ( splTyp.ExtendableType <> nil ) then begin
          AContainer.RegisterExternalAlias(splTyp.ExtendableType,typeNameArray[i][2]);
        end;
      end;
    end;
  end;

  procedure RegisterSimpleTypes();
  var
    i : Integer;
    splTyp : TPasNativeSimpleType;
    syb : TPasNativeSimpleContentClassType;
    s : string;
    typlst : array[0..Pred(SIMPLE_TYPES_COUNT)] of TPasNativeSimpleType;
  begin
    for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
      splTyp := TPasNativeSimpleType(AContainer.CreateElement(TPasNativeSimpleType,SIMPLE_TYPES[i][0],ADest.InterfaceSection,visPublic,'',0));
      ADest.InterfaceSection.Declarations.Add(splTyp);
      ADest.InterfaceSection.Types.Add(splTyp);
      typlst[i] := splTyp;
    end;
    for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
      s := SIMPLE_TYPES[i][1];
      if not IsStrEmpty(s) then begin
        syb := AContainer.FindElementInModule(SIMPLE_TYPES[i][1],ADest) as TPasNativeSimpleContentClassType;
        if not Assigned(syb) then begin
          syb := TPasNativeSimpleContentClassType(AContainer.CreateElement(TPasNativeSimpleContentClassType,s,ADest.InterfaceSection,visDefault,'',0));
          ADest.InterfaceSection.Declarations.Add(syb);
          ADest.InterfaceSection.Types.Add(syb);
        end;
        typlst[i].SetExtendableType(syb);
      end;
    end;
    for i := Low(SIMPLE_TYPES) to High(SIMPLE_TYPES) do begin
      splTyp := typlst[i];
      if not IsStrEmpty(SIMPLE_TYPES[i][2]) then begin
        AContainer.RegisterExternalAlias(splTyp,SIMPLE_TYPES[i][2]);
        if ( splTyp.ExtendableType <> nil ) then begin
          AContainer.RegisterExternalAlias(splTyp.ExtendableType,SIMPLE_TYPES[i][2]);
        end;
      end;
    end;
  end;
  
  procedure RegisterBoxedTypes();
  var
    i : Integer;
    nativeType : TPasNativeSpecialSimpleContentClassType;
    syb : TPasNativeSimpleContentClassType;
    s : string;
    typlst : array[0..Pred(BOXED_TYPES_COUNT)] of TPasNativeSpecialSimpleContentClassType;
  begin
    for i := Low(BOXED_TYPES) to High(BOXED_TYPES) do begin
      nativeType := TPasNativeSpecialSimpleContentClassType(AContainer.CreateElement(TPasNativeSpecialSimpleContentClassType,BOXED_TYPES[i][0],ADest.InterfaceSection,visPublic,'',0));
      ADest.InterfaceSection.Declarations.Add(nativeType);
      ADest.InterfaceSection.Types.Add(nativeType);
      typlst[i] := nativeType;
      s := BOXED_TYPES[i][1];
      if not IsStrEmpty(s) then begin
        syb := AContainer.FindElementInModule(BOXED_TYPES[i][1],ADest) as TPasNativeSpecialSimpleContentClassType;
        if not Assigned(syb) then begin
          syb := TPasNativeSpecialSimpleContentClassType(AContainer.CreateElement(TPasNativeSpecialSimpleContentClassType,s,ADest.InterfaceSection,visDefault,'',0));
          ADest.InterfaceSection.Declarations.Add(syb);
          ADest.InterfaceSection.Types.Add(syb);
        end;
        nativeType.SetExtendableType(syb);
      end;
    end;
    for i := Low(BOXED_TYPES) to High(BOXED_TYPES) do begin
      nativeType := typlst[i];
      if not IsStrEmpty(BOXED_TYPES[i][2]) then begin
        AContainer.RegisterExternalAlias(nativeType,BOXED_TYPES[i][2]);
        if ( nativeType.ExtendableType <> nil ) then begin
          AContainer.RegisterExternalAlias(nativeType.ExtendableType,BOXED_TYPES[i][2]);
        end;
      end;
    end;
  end;

begin
  RegisterSimpleTypes();
  RegisterSpecialSimpleTypes();
  RegisterBoxedTypes();
end;

function AddAlias(
        AContainer : TwstPasTreeContainer;
  const AName,
        ABaseType  : string;
        ATable     : TPasModule
) : TPasTypeAliasType;
begin
  Result := TPasTypeAliasType(AContainer.CreateElement(TPasAliasType,AName,ATable.InterfaceSection,visPublic,'',0));
  Result.DestType := AContainer.FindElementInModule(ABaseType,ATable) as TPasType;
  if Assigned(Result.DestType) then
    Result.DestType.AddRef();
  ATable.InterfaceSection.Declarations.Add(Result);
  ATable.InterfaceSection.Classes.Add(Result);
  ATable.InterfaceSection.Types.Add(Result);
end;

function AddClassDef(
        AContainer : TwstPasTreeContainer;
        ATable      : TPasModule;
  const AClassName,
        AParentName : string;
  const AClassType  : TPasClassTypeClass = nil
):TPasClassType;
var
  locClassType : TPasClassTypeClass;
begin
  if Assigned(AClassType) then begin
    locClassType := AClassType;
  end else begin
    locClassType := TPasClassType;
  end;
  Result := TPasClassType(AContainer.CreateElement(locClassType,AClassName,ATable.InterfaceSection,visDefault,'',0));
  if not IsStrEmpty(AParentName) then begin
    Result.AncestorType := AContainer.FindElementInModule(AParentName,ATable) as TPasType;
    if Assigned(Result.AncestorType) then
      Result.AncestorType.AddRef();
  end;
  ATable.InterfaceSection.Classes.Add(Result);
  ATable.InterfaceSection.Declarations.Add(Result);
  ATable.InterfaceSection.Types.Add(Result);
end;

function JavaCreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
var
  locOldNameKinds : TElementNameKinds;
begin
  locOldNameKinds := AContainer.DefaultSearchNameKinds;
  AContainer.DefaultSearchNameKinds := [elkDeclaredName,elkName];
  try
    Result := TPasNativeModule(AContainer.CreateElement(TPasNativeModule,'base_service_intf',AContainer.Package,visPublic,'',0));
    try
      AContainer.Package.Modules.Add(Result);
      AContainer.RegisterExternalAlias(Result,sXSD_NS);
      Result.InterfaceSection := TInterfaceSection(AContainer.CreateElement(TInterfaceSection,'',Result,visDefault,'',0));
      JavaRegisterSimpleTypes(Result,AContainer);
      AddClassDef(AContainer,Result,'Object','',TPasNativeClassType);
      AddAlias(AContainer,'TBaseComplexRemotable','Object',Result);
      AddAlias(AContainer,'UnicodeString','string',Result);
      AddAlias(AContainer,'token','string',Result);
      AddAlias(AContainer,'language','string',Result);
      AddAlias(AContainer,'anyURI','string',Result);
      AddAlias(AContainer,'ID','string',Result);
      AddAlias(AContainer,'base64Binary','string',Result);
      AddAlias(AContainer,'hexBinary','string',Result);
      AddAlias(AContainer,'integer','int',Result);
      AddAlias(AContainer,'nonNegativeInteger','int',Result);
      AddAlias(AContainer,'positiveInteger','int',Result);
      AddAlias(AContainer,'unsignedInt','int',Result);
      AddAlias(AContainer,'unsignedByte','int',Result);
      AddAlias(AContainer,'unsignedShort','int',Result);
      AddAlias(AContainer,'unsignedLong','long',Result);
      AddAlias(AContainer,'Currency','decimal',Result);
      AddAlias(AContainer,'date','dateTime',Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    AContainer.DefaultSearchNameKinds := locOldNameKinds;
  end;
end;

function CreateWstInterfaceSymbolTable(AContainer : TwstPasTreeContainer) : TPasModule;
var
  loc_TBaseComplexSimpleContentRemotable : TPasClassType;
  locOldNameKinds : TElementNameKinds;
begin
  locOldNameKinds := AContainer.DefaultSearchNameKinds;
  AContainer.DefaultSearchNameKinds := [elkDeclaredName,elkName];
  try
    Result := TPasNativeModule(AContainer.CreateElement(TPasNativeModule,'base_service_intf',AContainer.Package,visPublic,'',0));
    try
      AContainer.Package.Modules.Add(Result);
      AContainer.RegisterExternalAlias(Result,sXSD_NS);
      Result.InterfaceSection := TInterfaceSection(AContainer.CreateElement(TInterfaceSection,'',Result,visDefault,'',0));
      AddSystemSymbol(Result,AContainer,AContainer.XsdStringMaping);
      AddClassDef(AContainer,Result,'TBaseRemotable','',TPasNativeClassType);
        AddClassDef(AContainer,Result,'TStringBufferRemotable','TBaseRemotable',TPasNativeClassType);
        AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'anyType_Type','TBaseRemotable',TPasNativeClassType),'anyType');
        AddClassDef(AContainer,Result,'TAbstractSimpleRemotable','TBaseRemotable',TPasNativeClassType);
          AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'schema_Type','TAbstractSimpleRemotable'),'schema');
          AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'TDateRemotable','TAbstractSimpleRemotable'),'date');
          AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'TDateTimeRemotable','TAbstractSimpleRemotable'),'dateTime');
  {$IFDEF WST_HAS_TDURATIONREMOTABLE}
          AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'TDurationRemotable','TAbstractSimpleRemotable'),'duration');
  {$ENDIF WST_HAS_TDURATIONREMOTABLE}
          AContainer.RegisterExternalAlias(AddClassDef(AContainer,Result,'TTimeRemotable','TAbstractSimpleRemotable'),'time');

        AddClassDef(AContainer,Result,'TAbstractComplexRemotable','TBaseRemotable',TPasNativeClassType);
          loc_TBaseComplexSimpleContentRemotable := AddClassDef(AContainer,Result,'TBaseComplexSimpleContentRemotable','TAbstractComplexRemotable',TPasNativeClassType);
            (AContainer.FindElementInModule('TComplexInt16SContentRemotable',Result) as TPasClassType).AncestorType := loc_TBaseComplexSimpleContentRemotable;
            (AContainer.FindElementInModule('TComplexFloatDoubleContentRemotable',Result) as TPasClassType).AncestorType := loc_TBaseComplexSimpleContentRemotable;
              loc_TBaseComplexSimpleContentRemotable.AddRef();
              loc_TBaseComplexSimpleContentRemotable.AddRef();

          AddClassDef(AContainer,Result,'TBaseComplexRemotable','TAbstractComplexRemotable',TPasNativeClassType);
            AddClassDef(AContainer,Result,'THeaderBlock','TBaseComplexRemotable',TPasNativeClassType);
              AddClassDef(AContainer,Result,'TSimpleContentHeaderBlock','THeaderBlock',TPasNativeClassType);
          AddClassDef(AContainer,Result,'TBaseArrayRemotable','TAbstractComplexRemotable',TPasNativeClassType);
            AddClassDef(AContainer,Result,'TBaseObjectArrayRemotable','TBaseArrayRemotable',TPasNativeClassType);
            AddClassDef(AContainer,Result,'TBaseSimpleTypeArrayRemotable','TBaseArrayRemotable',TPasNativeClassType);
              AddClassDef(AContainer,Result,'TArrayOfStringRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfBooleanRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt8URemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt8SRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt16SRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt16URemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt32URemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt32SRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt64SRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfInt64URemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfFloatSingleRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfFloatDoubleRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfFloatExtendedRemotable','TBaseSimpleTypeArrayRemotable');
              AddClassDef(AContainer,Result,'TArrayOfFloatCurrencyRemotable','TBaseSimpleTypeArrayRemotable');

      AddAlias(AContainer,'token','string',Result);
      AddAlias(AContainer,'language','string',Result);
      AddAlias(AContainer,'anyURI','string',Result);
      AddAlias(AContainer,'ID','string',Result);
      AddAlias(AContainer,'NCName','string',Result);
      //AddAlias(AContainer,'float','Single',Result);
      AddAlias(AContainer,'integer','int',Result);
      AddAlias(AContainer,'nonNegativeInteger','LongWord',Result);
      AddAlias(AContainer,'positiveInteger','nonNegativeInteger',Result);
  {$IFNDEF WST_HAS_TDURATIONREMOTABLE}
      AddAlias(AContainer,'duration','string',Result);
  {$ENDIF WST_HAS_TDURATIONREMOTABLE}
  {$IFNDEF WST_HAS_TTIMEREMOTABLE}
      AddAlias(AContainer,'time','string',Result);
  {$ENDIF WST_HAS_TTIMEREMOTABLE}
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    AContainer.DefaultSearchNameKinds := locOldNameKinds;
  end;
end;

function GetUltimeType(
  AType      : TPasType;
  AContainer : TwstPasTreeContainer
) : TPasType;
var
  e : TPasElement;
begin
  Result := AType;
  if (Result <> nil) then begin
    while True do begin
      if Result.InheritsFrom(TPasUnresolvedTypeRef) then begin
        e := AContainer.FindElement(AContainer.GetExternalName(Result));
        if (e <> nil) and e.InheritsFrom(TPasType) then
          Result := TPasType(e);
      end;
      if Result.InheritsFrom(TPasAliasType) and
         (TPasAliasType(Result).DestType <> nil)
      then
        Result := TPasAliasType(Result).DestType
      else
        Break;
    end;
  end;
end;

function GetUltimeType(AType : TPasType) : TPasType;
begin
  Result := AType;
  if ( Result <> nil ) then begin
    while Result.InheritsFrom(TPasAliasType) and
          ( TPasAliasType(Result).DestType <> nil )
    do begin
      Result := TPasAliasType(Result).DestType;
    end;
  end;
end;

function FindActualType(
  AType      : TPasType;
  AContainer : TwstPasTreeContainer
) : TPasType;
var
  e : TPasElement;
begin
  Result := AType;
  if Result.InheritsFrom(TPasUnresolvedTypeRef) then begin
    e := AContainer.FindElement(AContainer.GetExternalName(Result));
    if (e <> nil) and e.InheritsFrom(TPasType) then
      Result := TPasType(e);
  end;
end;

function GetElementCount(AList : TList2; AElementClass : TPTreeElement):Integer ;
var
  i : Integer;
begin
  Result := 0;
  if Assigned(AList) then begin
    for i := 0 to Pred(AList.Count) do begin
      if TObject(AList[i]).InheritsFrom(AElementClass) then begin
        Inc(Result);
      end;
    end;
  end;
end;

function GetParameterIndex(
        AProcType  : TPasProcedureType;
  const AParamName : string;
  const AStartPos  : Integer
) : Integer;
var
  pl : TList2;
  i : Integer;
begin
  pl := AProcType.Args;
  if ( AStartPos >= 0 ) then
    i := AStartPos
  else
    i := 0;
  for i := i to Pred(pl.Count) do begin
    if AnsiSameText(AParamName,TPasArgument(pl[i]).Name) then begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function FindParameter(
        AProcType  : TPasProcedureType;
  const AParamName : string;
  const AStartPos  : Integer
) : TPasArgument;
var
  i : Integer;
begin
  i := GetParameterIndex(AProcType,AParamName,AStartPos);
  if ( i >= 0 ) then begin
    Result := TPasArgument(AProcType.Args[i]);
  end else begin
    Result := nil;
  end;
end;

function InternalFindMember(AMemberList : TList2; const AName : string) : TPasElement ;
var
  i : Integer;
begin
  Result := nil;
  if ( AMemberList <> nil ) then begin
    for i := 0 to Pred(AMemberList.Count) do begin
      if AnsiSameText(AName,TPasElement(AMemberList[i]).Name) then begin
        Result := TPasElement(AMemberList[i]);
      end;
    end;
  end;
end;

function FindMember(AClass : TPasClassType; const AName : string) : TPasElement ;
begin
  Result := nil;
  if ( AClass <> nil ) then begin
    Result := InternalFindMember(AClass.Members,AName);
  end;
end;

function FindMember(AClass : TPasRecordType; const AName : string) : TPasElement ;
begin
  Result := nil;
  if ( AClass <> nil ) then begin
    Result := InternalFindMember(AClass.Members,AName);
  end;
end;

function FindModule(AType: TPasType) : TPasModule;
begin
  if Assigned(AType) and
     Assigned(AType.Parent{Section}) and
     Assigned(AType.Parent.Parent{Module})
  then begin
    Result := AType.Parent.Parent as TPasModule;
  end else begin
    Result := nil;
  end;
end;

function MakeInternalSymbolNameFrom(const AName : string) : string ;
begin
  Result := ExtractIdentifier(AName);
  if IsStrEmpty(AName) then begin
    raise ESymbolException.CreateFmt(SERR_CannotMakeInternalSymbolName,[AName]);
  end;
  if IsReservedKeyWord(Result) then begin
    Result := '_' + Result;
  end;
end;

procedure CreateDefaultBindingForIntf(ATree : TwstPasTreeContainer);
var
  e : TPasElement;
  ct : TPasClassType;
  c, i : Integer;
  m : TPasModule;
  ls : TList2;
begin        
  if ( ATree.CurrentModule <> nil ) then begin
    m := ATree.CurrentModule;
    if ( m.InterfaceSection <> nil ) and 
       ( m.InterfaceSection.Declarations <> nil ) and
       ( m.InterfaceSection.Declarations.Count > 0 ) 
    then begin
      ls := m.InterfaceSection.Declarations;
      c := ls.Count;
      for i := 0 to Pred(c) do begin
        e := TPasElement(ls[i]);
        if e.InheritsFrom(TPasClassType) then begin
          ct := TPasClassType(e);
          if ( ct.ObjKind = okInterface ) and
             ( ATree.FindBinding(ct) = nil )
          then begin
            ATree.AddBinding(Format('%sBinding',[ct.Name]),ct).BindingStyle := bsRPC;
          end;
        end;
      end;
    end;
  end;
end;

{ TwstPasTreeContainer }

function TwstPasTreeContainer.GetBinding(AIndex : Integer): TwstBinding;
begin
  Result := TwstBinding(FBindingList[AIndex]);
end;

function TwstPasTreeContainer.GetBindingCount: Integer;
begin
  Result := FBindingList.Count;
end;

constructor TwstPasTreeContainer.Create();
begin
  FCaseSensitive := CASE_SENSITIVE_DEFAULT;
  FDefaultSearchNameKinds := NAME_KINDS_DEFAULT;
  FPackage := TPasPackage.Create('sample',nil);
  FBindingList := TObjectList.Create(True);
  FProperties := TPropertyHolder.Create();
end;

destructor TwstPasTreeContainer.Destroy();
begin
  FreeAndNil(FProperties);
  FreeAndNil(FBindingList);
  FreeAndNil(FPackage);
  inherited Destroy();
end;

function TwstPasTreeContainer.CreateElement(
            AClass              : TPTreeElement;
      const AName               : String;
            AParent             : TPasElement;
            AVisibility         : TPasMemberVisibility;
      const ASourceFilename     : String;
            ASourceLinenumber   : Integer
) : TPasElement;
begin
  Result := AClass.Create(AName,AParent);
  RegisterExternalAlias(Result,AName);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
  if Result.InheritsFrom(TPasModule) then begin
    FCurrentModule := Result as TPasModule;
    //Package.Modules.Add(Result);
  end;
end;

function TwstPasTreeContainer.CreateArray(
  const AName              : string;
        AItemType          : TPasType;
  const AItemName,
        AItemExternalName  : string;
  const AStyle             : TArrayStyle
) : TPasArrayType;
var
  s : string;
begin
  Result := TPasArrayType(CreateElement(TPasArrayType,AName,CurrentModule.InterfaceSection,visDefault,'',0));
  Result.ElType := AItemType;
  AItemType.AddRef();
  Properties.SetValue(Result,sARRAY_ITEM_NAME,AItemName);
  Properties.SetValue(Result,sARRAY_ITEM_EXT_NAME,AItemExternalName);
  if ( AStyle = asEmbeded ) then
    s := sARRAY_STYLE_EMBEDDED
  else
    s := sARRAY_STYLE_SCOPED;
  Properties.SetValue(Result,sARRAY_STYLE,s);
end;

function TwstPasTreeContainer.GetArrayItemName(AArray: TPasArrayType): string;
begin
  Result := Properties.GetValue(AArray,sARRAY_ITEM_NAME);
end;

function TwstPasTreeContainer.GetArrayItemExternalName(AArray: TPasArrayType): string;
begin
  Result := Properties.GetValue(AArray,sARRAY_ITEM_EXT_NAME);
end;

function TwstPasTreeContainer.GetArrayStyle(AArray: TPasArrayType): TArrayStyle;
begin
  if AnsiSameText(sARRAY_STYLE_EMBEDDED,Properties.GetValue(AArray,sARRAY_STYLE)) then
    Result := asEmbeded
  else
    Result := asScoped;
end;

procedure TwstPasTreeContainer.SetArrayStyle(
        AArray : TPasArrayType;
  const AStyle : TArrayStyle
);
begin
  if ( AStyle = asEmbeded ) then
    Properties.SetValue(AArray,sARRAY_STYLE,sARRAY_STYLE_EMBEDDED)
  else
    Properties.SetValue(AArray,sARRAY_STYLE,sARRAY_STYLE_SCOPED);
end;

procedure TwstPasTreeContainer.SetArrayItemExternalName(
        AArray : TPasArrayType;
  const AExternalName : string
);
begin
  Properties.SetValue(AArray,sARRAY_ITEM_EXT_NAME,AExternalName);
end;

function TwstPasTreeContainer.IsCollection(AArray : TPasArrayType) : Boolean;
begin
  Result := AnsiSameText('true',Properties.GetValue(AArray,sARRAY_IS_COLLECTION));
end;

procedure TwstPasTreeContainer.SetCollectionFlag(
        AArray : TPasArrayType;
  const AFlag : Boolean
);
begin
  if AFlag then
    Properties.SetValue(AArray,sARRAY_IS_COLLECTION,'true')
  else
    Properties.SetValue(AArray,sARRAY_IS_COLLECTION,'false');
end;

function TwstPasTreeContainer.FindElementInModule(
  const AName: String;
        AModule : TPasModule;
  const ANameKinds : TElementNameKinds
): TPasElement;
var
  decs : TList2;
  i, c : Integer;
begin
  Result := nil;
  if Assigned(AModule) and Assigned(AModule.InterfaceSection.Declarations) then begin
    decs := AModule.InterfaceSection.Declarations;
    c := decs.Count;
    if ( elkDeclaredName in ANameKinds ) then begin
      if FCaseSensitive then begin
        for i := 0 to Pred(c) do begin
          if AnsiSameStr(AName, GetExternalName(TPasElement(decs[i]))) then begin
            Result := TPasElement(decs[i]);
            Exit;
          end;
        end;
      end else begin
        for i := 0 to Pred(c) do begin
          if AnsiSameText(AName, GetExternalName(TPasElement(decs[i]))) then begin
            Result := TPasElement(decs[i]);
            Exit;
          end;
        end;
      end;
    end;

    if ( Result = nil ) and ( elkName in ANameKinds ) then begin
      if FCaseSensitive then begin
        for i := 0 to Pred(c) do begin
          if AnsiSameStr(AName, TPasElement(decs[i]).Name) then begin
            Result := TPasElement(decs[i]);
            Exit;
          end;
        end;
      end else begin
        for i := 0 to Pred(c) do begin
          if AnsiSameText(AName, TPasElement(decs[i]).Name) then begin
            Result := TPasElement(decs[i]);
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.FindElementInModule(
  const AName   : String;
        AModule : TPasModule
) : TPasElement;
begin
  Result := FindElementInModule(AName,AModule,DefaultSearchNameKinds);
end;

function TwstPasTreeContainer.FindElement(const AName: String): TPasElement;
begin
  Result := FindElement(AName,DefaultSearchNameKinds);
end;

function TwstPasTreeContainer.FindElement(
  const AName: String;
  const ANameKinds: TElementNameKinds
): TPasElement;
var
  i : Integer;
  mls : TList2;
  mdl : TPasModule;
begin
  Result := FindElementInModule(AName,CurrentModule,ANameKinds);
  if ( Result = nil ) then begin
    mls := Package.Modules;
    for i := 0 to Pred(mls.Count) do begin
      mdl := TPasModule(mls[i]);
      if ( CurrentModule <> mdl ) then begin
        Result := FindElementInModule(AName,mdl,ANameKinds);
        if ( Result <> nil ) then begin
          Break;
        end;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.FindModule(const AName: String): TPasModule;
var
  i , c : Integer;
  mdl : TList2;
  s : string;
begin
  Result := nil;
  s := ExtractIdentifier(AName);
  mdl := Package.Modules;
  c := mdl.Count;
  if (s = AName) then begin
    for i := 0 to Pred(c) do begin
      if SameName(TPasModule(mdl[i]),AName) then begin
        Result := TPasModule(mdl[i]);
        Break;
      end;
    end;
  end else begin
    for i := 0 to Pred(c) do begin
      if (GetExternalName(TPasModule(mdl[i])) = AName) then begin
        Result := TPasModule(mdl[i]);
        Break;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.IsEnumItemNameUsed(const AName: string;AModule: TPasModule): Boolean;
var
  i, c, j : Integer;
  elt : TPasElement;
  enumList : TList2;
  typeList : TList2;
begin
  Result := False;
  typeList := AModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to Pred(c) do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasEnumType) then begin
      enumList := TPasEnumType(elt).Values;
      for j := 0 to Pred(enumList.Count) do begin
        if AnsiSameText(AName,TPasEnumValue(enumList[j]).Name) then begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

function TwstPasTreeContainer.IsEnumItemNameUsed(const AName: string): Boolean;
begin
  Result := IsEnumItemNameUsed(AName,CurrentModule);
end;

function TwstPasTreeContainer.IsOfType(AType : TPasType; AClass : TClass) : Boolean;
var
  ut : TPasType;
begin
  Result := False;
  if Assigned(AType) then begin
    ut := AType;
    if ut.InheritsFrom(TPasUnresolvedTypeRef) then begin
      ut := FindElement(GetExternalName(ut)) as TPasType;
      if ( ut = nil ) then
        ut := AType;
    end;
    ut := GetUltimeType(ut);
    if ut.InheritsFrom(AClass) then begin
      Result := True;
    end;
  end;
end;

function TwstPasTreeContainer.IsInitNeed(AType : TPasType) : Boolean;
begin
  Result := IsOfType(AType,TPasClassType) or
            IsOfType(AType,TPasArrayType);
end;

procedure TwstPasTreeContainer.SetCurrentModule(AModule: TPasModule);
begin
  FCurrentModule := AModule;
end;

function TwstPasTreeContainer.AddBinding(const AName : string; AIntf : TPasClassType):TwstBinding;
begin
  Result := FindBinding(AName);
  if Assigned(Result) then begin
    raise Exception.CreateFmt(SERR_DuplicateBindingName,[AName]);
  end;
  Result := TwstBinding.Create(AName, AIntf, AIntf.Parent);
  FBindingList.Add(Result);
end;

procedure TwstPasTreeContainer.DeleteBinding(ABinding: TwstBinding);
begin
  FBindingList.Extract(ABinding);
  ABinding.Release();
end;

function TwstPasTreeContainer.FindBinding(const AName: string): TwstBinding;
var
  i : Integer;
begin
  for i := 0 to Pred(BindingCount) do begin
    if AnsiSameText(AName,Binding[i].Name) then begin
      Result := Binding[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TwstPasTreeContainer.FindBinding(const AIntf: TPasClassType; const AOrder : Integer): TwstBinding;
var
  i, c, ordr : Integer;
begin
  ordr := AOrder;
  c := BindingCount;
  for i := 0 to Pred(c) do begin
    Result := Binding[i];
    if ( Result.Intf = AIntf ) then begin
      if ( ordr <= 0 ) then
        Exit
      else
        Dec(ordr);
    end;
  end;
  Result := nil;
end;

procedure TwstPasTreeContainer.FreeProperties(AObject: TPasElement);

  procedure FreeClassProps(AObj : TPasClassType);
  var
    ls : TList2;
    k : Integer;
  begin
    ls := AObj.Members;
    for k := 0 to Pred(ls.Count) do begin
      FProperties.FreeList(TPasElement(ls[k]));
    end;
  end;
  
  procedure FreeRecordFields(AObj : TPasRecordType);
  var
    ls : TList2;
    k : Integer;
  begin
    ls := AObj.Members;
    for k := 0 to Pred(ls.Count) do begin
      FProperties.FreeList(TPasElement(ls[k]));
    end;
  end;
  
begin
  if Assigned(AObject) then begin
    FProperties.FreeList(AObject);
    if AObject.InheritsFrom(TPasClassType) then
      FreeClassProps(AObject as TPasClassType)
    else if AObject.InheritsFrom(TPasRecordType) then
      FreeRecordFields(AObject as TPasRecordType);
  end;
end;

procedure TwstPasTreeContainer.RegisterExternalAlias(
        AObject : TPasElement;
  const AExternalName : String
);
begin
  Properties.SetValue(AObject,sEXTERNAL_NAME,AExternalName);
end;

function TwstPasTreeContainer.SameName(
        AObject : TPasElement;
  const AName   : string
): Boolean;
begin
  Result := AnsiSameText(AName,AObject.Name) or AnsiSameText(AName,GetExternalName(AObject)) ;
end;

function TwstPasTreeContainer.HasExternalName(AObject : TPasElement) : Boolean;
begin
  Result := Properties.HasValue(AObject,sEXTERNAL_NAME);
end;

function TwstPasTreeContainer.GetExternalName(AObject: TPasElement): string;
begin
  Result := GetExternalName(AObject,True);
end;

function TwstPasTreeContainer.GetExternalName(
        AObject : TPasElement;
  const AReturnNameIfEmpty : Boolean
) : string;
begin
  Result := Properties.GetValue(AObject,sEXTERNAL_NAME);
  if IsStrEmpty(Result) and AReturnNameIfEmpty then begin
    Result := AObject.Name;
  end;
end;

function TwstPasTreeContainer.IsAttributeProperty(AObject: TPasVariable): Boolean;
begin
  Result := AnsiSameText(Properties.GetValue(AObject,sATTRIBUTE),'True');
end;

procedure TwstPasTreeContainer.SetPropertyAsAttribute(AObject: TPasVariable; const AValue: Boolean);
var
  s : string;
begin
  if AValue then
    s := 'True'
  else
    s := 'False';
  Properties.SetValue(AObject,sATTRIBUTE,s);
end;

function TwstPasTreeContainer.IsChoiceProperty(
  AObject : TPasProperty
) : Boolean;
begin
  Result := AnsiSameText(Properties.GetValue(AObject,sCHOICE),'True');
end;

procedure TwstPasTreeContainer.SetPropertyAsChoice(
        AObject : TPasProperty;
  const AValue  : Boolean
);
var
  s : string;
begin
  if AValue then
    s := 'True'
  else
    s := 'False';
  Properties.SetValue(AObject,sCHOICE,s);
end;

function TwstPasTreeContainer.FindElementNS(const AName, ANameSpace: string): TPasElement;
var
  mdl : TPasModule;
begin
  Result := nil;
  mdl := FindModule(ANameSpace);
  if Assigned(mdl) then begin
    Result := FindElementInModule(AName,mdl);
  end;
end;

function TwstPasTreeContainer.GetNameSpace(AType: TPasType): string;
begin
  if Assigned(AType) and
     Assigned(AType.Parent{Section}) and
     Assigned(AType.Parent.Parent{Module})
  then begin
    Result := GetExternalName(AType.Parent.Parent);
  end else begin
    Result := '';
  end;
end;

{ TwstBinding }

procedure TwstBinding.setEasyIntf(const AValue : TPasClassType); 
begin
  if (FEasyIntf = AValue) then 
    exit; 
  if (FEasyIntf <> nil) then
    FEasyIntf.Release();
  FEasyIntf := AValue; 
  if (FEasyIntf <> nil) then
    FEasyIntf.AddRef();  
end;

constructor TwstBinding.Create(
  const AName : string;
        AIntf : TPasClassType;
        AParent: TPasElement
);
begin
  Assert((not IsStrEmpty(AName)) and Assigned(AIntf) and ( AIntf.ObjKind = okInterface ));
  inherited Create(AName,AParent);
  FIntf := AIntf;
  FIntf.AddRef();
end;

destructor TwstBinding.Destroy();
begin
  if Assigned(FIntf) then begin
    FIntf.Release();
    FIntf := nil;
  end;
  if Assigned(FEasyIntf) then begin
    FEasyIntf.Release();
    FEasyIntf := nil;
  end;
  inherited Destroy();
end;

{ TPropertyHolder }

function TPropertyHolder.FindList(AOwner: TObject): TStrings;
var
  i : Integer;
begin
  i := FObjects.IndexOf(AOwner);
  if ( i >= 0 ) then begin
    Result := FProps[i] as TStrings;
  end else begin
    Result := nil ;
  end;
end;

function TPropertyHolder.GetList(AOwner: TObject): TStrings;
begin
  Result := FindList(AOwner);
  if ( Result = nil ) then begin
    FObjects.Add(AOwner);
    Result := TStringList.Create();
    FProps.Add(Result);
  end;
end;

procedure TPropertyHolder.FreeList(AObject: TObject);
var
  i : Integer;
begin
  i := FObjects.IndexOf(AObject);
  if ( i >= 0 ) then begin
    FObjects.Delete(i);
    FProps.Delete(i);
  end;
end;

constructor TPropertyHolder.Create();
begin
  FObjects := TObjectList.Create(False);
  FProps := TObjectList.Create(True);
end;

destructor TPropertyHolder.Destroy();
begin
  FreeAndNil(FProps);
  FreeAndNil(FObjects);
  inherited Destroy();
end;

procedure TPropertyHolder.SetValue(AOwner: TObject; const AName, AValue: string);
begin
  GetList(AOwner).Values[AName] := AValue;
end;

function TPropertyHolder.GetValue(AOwner: TObject; const AName: string): string;
var
  ls : TStrings;
begin
  ls := FindList(AOwner);
  if ( ls = nil ) then begin
    Result := '';
  end else begin
    Result := ls.Values[AName];
  end;
end;

function TPropertyHolder.HasValue(AOwner : TObject; const AName : string) : Boolean;
var
  ls : TStrings;
begin
  ls := FindList(AOwner);
  if ( ls <> nil ) and ( ls.IndexOfName(AName) > -1 ) then
    Result := True
  else
    Result := False;
end;

{ TPasNativeSimpleTypeDefinition }

destructor TPasNativeSimpleType.Destroy();
begin
  if Assigned(FExtendableType) then begin
    FExtendableType.Release();
    FExtendableType := nil
  end;
  inherited Destroy();
end;

procedure TPasNativeSimpleType.SetExtendableType(
  AExtendableType : TPasNativeSimpleContentClassType
);
begin
  if ( FExtendableType <> AExtendableType ) then begin
    if ( FExtendableType <> nil ) then begin
      FExtendableType.Release();
    end;
    FExtendableType := AExtendableType;
    if ( FExtendableType <> nil ) then
      FExtendableType.AddRef();
  end;
end;

{ TPasNativeClassType }

destructor TPasNativeClassType.Destroy();
begin
  if Assigned(FExtendableType) then begin
    FExtendableType.Release();
    FExtendableType := nil
  end;
  inherited Destroy();
end;

procedure TPasNativeClassType.SetExtendableType(AExtendableType : TPasNativeSimpleContentClassType);
begin
  if ( FExtendableType <> AExtendableType ) then begin
    if ( FExtendableType <> nil ) then begin
      FExtendableType.Release();
    end;
    FExtendableType := AExtendableType;
    if ( FExtendableType <> nil ) then
      FExtendableType.AddRef();
  end;
end;

end.

