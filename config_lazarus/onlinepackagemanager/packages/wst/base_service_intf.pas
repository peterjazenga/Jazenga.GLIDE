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
{$RANGECHECKS OFF}

unit base_service_intf;

interface

uses
  Classes, SysUtils, TypInfo, Contnrs, syncobjs, semaphore, wst_types
{$IFDEF WST_DELPHI}
  ,Windows
{$ENDIF}
  , date_utils;

const
  stBase   = 0;
  stObject = stBase + 1;
  stArray  = stBase + 2;

  sARRAY_ITEM = 'item';
  sARRAY_STYLE = 'style';

  // array style string
  sScoped  = 'scoped';
  sEmbedded = 'embedded';

type

  { standart data types defines }
  anyURI = type string;
  token = type string;
  language = type string;
  NCName = type string;
  nonNegativeInteger = type LongWord;
  positiveInteger = type nonNegativeInteger;
  float = Single;
{$IFNDEF WST_HAS_TDURATIONREMOTABLE}
  duration = type string;
{$ENDIF WST_HAS_TDURATIONREMOTABLE}
{$IFNDEF WST_HAS_TTIMEREMOTABLE}
  time = type string;
{$ENDIF WST_HAS_TTIMEREMOTABLE}

  TScopeType = Integer;
  TArrayStyle = ( asScoped, asEmbeded, asNone );
  TInstanceOption = ( ioAlwaysSerialize );
  TInstanceOptions = set of TInstanceOption;
  THeaderDirection = ( hdOut, hdIn );
  THeaderDirections = set of THeaderDirection;
const
  AllHeaderDirection = [Low(THeaderDirection)..High(THeaderDirection)];

type

  EServiceException = class(Exception) end;
  EServiceExtensionException = class(Exception) end;

  ETransportExecption = class(EServiceException)
  private
    FExtendedErrorInfo : string;
  public
    property ExtendedErrorInfo : string
        read FExtendedErrorInfo write FExtendedErrorInfo;
  end;

  EBaseRemoteException = class(EServiceException)
  private
    FFaultCode: string;
    FFaultString: string;
  Published
    property FaultCode : string Read FFaultCode Write FFaultCode;
    property FaultString : string Read FFaultString Write FFaultString;
  End;

  EServiceConfigException = class(EServiceException)
  end;

  ETypeRegistryException = class(EServiceConfigException)
  end;

  IItemFactory = Interface;
  IFormatterBase = Interface;
  IFormatterRegistry = Interface;

  TBaseRemotable = class;
  THeaderBlock = class;
  TSimpleContentHeaderBlock = class;

  //Utility interface used to configure its parent.
  IPropertyManager = Interface
    ['{A3A6B8F4-E50D-4956-B416-C642C72E4672}']
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  End;

  IItemFactory = interface
    ['{38258BC0-CBE6-437B-B104-9A62475E53AC}']
    function CreateInstance():IInterface;
  end;

  IItemFactoryEx = interface(IItemFactory)
    ['{66B77926-7E45-4780-8FFB-FB78625EDC1D}']
    procedure ReleaseInstance(const AInstance : IInterface);
    procedure DiscardInstance(const AInstance : IInterface);
    function GetPropertyManager(
      const APropertyGroup : string;
      const ACreateIfNotExists : Boolean
    ):IPropertyManager;
  end;

  IFormatterRegistry = Interface
    ['{E4D69D2A-F0A5-43E1-8C56-B47E7AB5D1AF}']
    function Find(const AFormatterName : string):IFormatterBase;
    procedure Register(
      const AFormatterName,
            AContentType   : string;
            AFactory       : IItemFactory
    );
  End;

  ICallContext = Interface
    ['{855EB8E2-0700-45B1-B852-2101023200E0}']
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
    procedure ClearHeaders(const ADirection : THeaderDirection);
    function GetPropertyManager():IPropertyManager;
  End;

  TSerializationStyle = ( ssNodeSerialization, ssAttibuteSerialization );

  IFormatterBase = Interface
    ['{2AB3BF54-B7D6-4C46-8245-133C8775E9C1}']
    function GetPropertyManager():IPropertyManager;
    function GetFormatName() : string;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    function GetCurrentScope():string;
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
    ) : Boolean; overload;
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
    //Please use this method if and _only_ if you do not have another way achieve your aim!
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);
    procedure LoadFromStream(AStream : TStream);

    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  End;

  { TSimpleCallContext }

  TSimpleCallContext = class(TInterfacedObject,ICallContext)
  private
    FHeaderList : TObjectList;
    FFreeObjectList : TObjectList;
    FPropertyManager : IPropertyManager;
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
    procedure ClearHeaders(const ADirection : THeaderDirection);
    procedure FreeHeader(AHeader : THeaderBlock);
    function GetPropertyManager():IPropertyManager;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  { TBaseRemotable }
  TBaseRemotableClass = class of TBaseRemotable;
  TBaseRemotable = class(TPersistent)
  Public
    constructor Create();virtual;
    destructor Destroy();override;
    // This will free objects and arrays properties and set them to nil.
    procedure FreeObjectProperties();virtual;

    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo
    );virtual;abstract;
    class procedure Load(
      Var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : String;
      const ATypeInfo : PTypeInfo
    );virtual;abstract;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;virtual;
    function wstHasValue() : Boolean;virtual;
  End;

  TAbstractSimpleRemotableClass = class of TAbstractSimpleRemotable;

  { TAbstractSimpleRemotable }

  TAbstractSimpleRemotable = class(TBaseRemotable) end;

  { TStringBufferRemotable }

  TStringBufferRemotable = class(TAbstractSimpleRemotable)
  private
    FData : string;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;
    property Data : string read FData write FData;
  end;

  schema_Type = class(TStringBufferRemotable) end;
  anyType_Type = class(TStringBufferRemotable) end;
  
  { TAbstractEncodedStringRemotable }

  TAbstractEncodedStringRemotable = class(TAbstractSimpleRemotable)
  private
    FBinaryData : TByteDynArray;
  private
    function GetEncodedString : string; virtual; abstract;
    procedure SetEncodedString(const AValue : string); virtual; abstract;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;
    procedure LoadFromStream(AStream : TStream);
    procedure LoadFromFile(const AFileName : string);
    procedure LoadFromBuffer(const ABuffer; const ABufferLen : Integer);
    procedure SaveToStream(AStream : TStream);
    procedure SaveToFile(const AFileName : string);
    function SaveToBuffer(var ABuffer; const ABufferLen : Integer) : Integer;
    property BinaryData : TByteDynArray read FBinaryData write FBinaryData;
    property EncodedString : string read GetEncodedString write SetEncodedString;
  end;

  { TBase64StringRemotable }

  TBase64StringRemotable = class(TAbstractEncodedStringRemotable)
  private
    function GetEncodedString : string; override;
    procedure SetEncodedString(const AValue : string); override;
  end;

  { TBase16StringRemotable }

  TBase16StringRemotable = class(TAbstractEncodedStringRemotable)
  private
    function GetEncodedString : string; override;
    procedure SetEncodedString(const AValue : string); override;
  end;

  { TBaseDateRemotable }

  TBaseDateRemotable = class(TAbstractSimpleRemotable)
  private
    FDate : TDateTimeRec;
  private
    function GetAsString: string;
    function GetOffset(const Index: Integer): Shortint;
    procedure SetAsString(const AValue: string);
    procedure SetOffset(const Index: Integer; const Value: Shortint);
    function GetDate(const AIndex : Integer) : TDateTime;
  protected
    function GetDatepart(const AIndex : Integer) : Integer;virtual;
    procedure SetDate(const AIndex : Integer; const AValue: TDateTime);virtual;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class function ToStr(const ADate : TDateTime):string;overload;
    class function ToStr(const ADate : TDateTimeRec):string;overload;virtual;abstract;
    class function Parse(const ABuffer : string):TDateTimeRec;virtual;abstract;
    class function ParseToUTC(const ABuffer : string):TDateTime;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;

    property AsDate : TDateTime index 0 read GetDate write SetDate;
    property AsUTCDate : TDateTime index 1 read GetDate write SetDate;
    property Year : Integer index 0 read GetDatepart;
    property Month : Integer index 1 read GetDatepart;
    property Day : Integer index 2 read GetDatepart;
    property HourOffset : Shortint index 0 read GetOffset write SetOffset;
    property MinuteOffset : Shortint index 1 read GetOffset write SetOffset;
    property AsString : string read GetAsString write SetAsString;
  end;

  { TDateRemotable }

  TDateRemotable = class(TBaseDateRemotable)
  public
    class function ToStr(const ADate : TDateTimeRec):string;override;
    class function Parse(const ABuffer : string):TDateTimeRec;override;
  end;

  { TDateTimeRemotable }

  TDateTimeRemotable = class(TBaseDateRemotable)
  protected
    function GetDatepart(const AIndex : Integer) : Integer;override;
  public
    class function ToStr(const ADate : TDateTimeRec):string;override;
    class function Parse(const ABuffer : string):TDateTimeRec;override;
    property Hour : Integer index 3 read GetDatepart;
    property Minute : Integer index 4 read GetDatepart;
    property Second : Integer index 5 read GetDatepart;
  end;

  { TDurationRemotable }

  TDurationRemotable = class(TAbstractSimpleRemotable)
  private
    FData : TDurationRec;
  private
    function GetAsString: string;
    function GetNegative: Boolean;
    function GetPart(AIndex: integer): PtrUInt;
    procedure SetAsString(const AValue: string);
    procedure SetNegative(const AValue: Boolean);
    procedure SetPart(AIndex: integer; const AValue: PtrUInt);
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;
    procedure Clear();

    class function Parse(const ABuffer : string) : TDurationRec;
    class function ToStr(const AValue : TDurationRec):string;

    property Negative : Boolean read GetNegative write SetNegative;
    property Year : PtrUInt index 0 read GetPart write SetPart;
    property Month : PtrUInt index 1 read GetPart write SetPart;
    property Day : PtrUInt index 2 read GetPart write SetPart;
    property Hour : PtrUInt index 3 read GetPart write SetPart;
    property Minute : PtrUInt index 4 read GetPart write SetPart;
    property Second : PtrUInt index 5 read GetPart write SetPart;
    property FractionalSecond : PtrUInt index 6 read GetPart write SetPart;
    property AsString : string read GetAsString write SetAsString;
  end;

  { TTimeRemotable }

  TTimeRemotable = class(TAbstractSimpleRemotable)
  private
    FData : TTimeRec;
  private
    function GetOffset(AIndex: integer): Shortint;
    function GetPart(AIndex: integer): Byte;
    procedure SetMilliSecond(const AValue: Word);
    procedure SetOffset(AIndex: integer; const AValue: Shortint);
    procedure SetPart(AIndex: integer; const AValue: Byte);
    function GetAsString: string;
    function GetMilliSecond: Word;
    procedure SetAsString(const AValue: string);
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;
    procedure Clear();

    class function Parse(const ABuffer : string) : TTimeRec;
    class function ToStr(const AValue : TTimeRec) : string;

    property Hour : Byte index 0 read GetPart write SetPart;
    property Minute : Byte index 1 read GetPart write SetPart;
    property Second : Byte index 2 read GetPart write SetPart;
    property MilliSecond : Word read GetMilliSecond write SetMilliSecond;
    property HourOffset : Shortint index 0 read GetOffset write SetOffset;
    property MinuteOffset : Shortint index 1 read GetOffset write SetOffset;

    property Data : TTimeRec read FData write FData;
    property AsString : string read GetAsString write SetAsString;
  end;

  TAbstractComplexRemotableClass = class of TAbstractComplexRemotable;

  { TAbstractComplexRemotable }

  TAbstractComplexRemotable = class(TBaseRemotable)
  public
    class procedure RegisterAttributeProperty(const AProperty : shortstring);virtual;
    class procedure RegisterAttributeProperties(const APropertList : array of shortstring);virtual;
    class function IsAttributeProperty(const AProperty : shortstring):Boolean;

    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
  end;

  TBaseComplexRemotableClass = class of TBaseComplexRemotable;

  { TBaseComplexRemotable }

  TBaseComplexRemotable = class(TAbstractComplexRemotable)
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;

  TRemotableRecordEncoderClass = class of TRemotableRecordEncoder;

  { TRemotableRecordEncoder }

  TRemotableRecordEncoder = class(TPersistent)
  public
    class procedure Save(
            ARecord   : Pointer;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );virtual;
    class procedure Load(
      var   ARecord   : Pointer;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );virtual;
  end;

  { TBaseComplexSimpleContentRemotable }

  TBaseComplexSimpleContentRemotable = class(TAbstractComplexRemotable)
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);virtual;abstract;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);virtual;abstract;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;
  TBaseComplexSimpleContentRemotableClass = class of TBaseComplexSimpleContentRemotable;

  { TComplexEnumContentRemotable }

  TComplexEnumContentRemotable = class(TBaseComplexSimpleContentRemotable)
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
    class function GetEnumTypeInfo() : PTypeInfo;virtual;abstract;
    function GetValueAddress() : Pointer;virtual;abstract;
  end;                    

  { TComplexInt8UContentRemotable }

  TComplexInt8UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Byte;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Byte read FValue write FValue;
  end;

  { TComplexInt8SContentRemotable }

  TComplexInt8SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: ShortInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : ShortInt read FValue write FValue;
  end;

  { TComplexInt16SContentRemotable }

  TComplexInt16SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: SmallInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : SmallInt read FValue write FValue;
  end;

  { TComplexInt16UContentRemotable }

  TComplexInt16UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Word;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Word read FValue write FValue;
  end;

  { TComplexInt32SContentRemotable }

  TComplexInt32SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: LongInt;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : LongInt read FValue write FValue;
  end;

  { TComplexInt32UContentRemotable }

  TComplexInt32UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: LongWord;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : LongWord read FValue write FValue;
  end;

  { TComplexInt64SContentRemotable }

  TComplexInt64SContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Int64;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Int64 read FValue write FValue;
  end;

  { TComplexInt64UContentRemotable }

  TComplexInt64UContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: QWord;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : QWord read FValue write FValue;
  end;

  { TComplexFloatExtendedContentRemotable }

  TComplexFloatExtendedContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Extended;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Extended read FValue write FValue;
  end;

  { TComplexFloatDoubleContentRemotable }

  TComplexFloatDoubleContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Double;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Double read FValue write FValue;
  end;

  { TComplexFloatSingleContentRemotable }

  TComplexFloatSingleContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Single;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Single read FValue write FValue;
  end;

  { TComplexCurrencyContentRemotable }

  TComplexCurrencyContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Currency;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Currency read FValue write FValue;
  end;  
    
  TComplexAnsiCharContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: AnsiChar;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : AnsiChar read FValue write FValue;
  end;

  TComplexWideCharContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: WideChar;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : WideChar read FValue write FValue;
  end;

  { TComplexStringContentRemotable }

  TComplexStringContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: string;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : string read FValue write FValue;
  end;

  { TComplexWideStringContentRemotable }

  TComplexWideStringContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Widestring;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : Widestring read FValue write FValue;
  end;

{$IFDEF WST_UNICODESTRING}
  { TComplexUnicodeStringContentRemotable }

  TComplexUnicodeStringContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: UnicodeString;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    function wstHasValue() : Boolean;override;
    property Value : UnicodeString read FValue write FValue;
  end;
{$ENDIF WST_UNICODESTRING}

  { TAbstractEncodedStringExtRemotable }

  TAbstractEncodedStringExtRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FBinaryData : TByteDynArray;
  private
    function GetEncodedString : string; virtual; abstract;
    procedure SetEncodedString(const AValue : string); virtual; abstract;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    function wstHasValue() : Boolean;override;
    procedure LoadFromStream(AStream : TStream);
    procedure LoadFromFile(const AFileName : string);
    procedure LoadFromBuffer(const ABuffer; const ABufferLen : Integer);
    procedure SaveToStream(AStream : TStream);
    procedure SaveToFile(const AFileName : string);
    function SaveToBuffer(var ABuffer; const ABufferLen : Integer) : Integer;
    property BinaryData : TByteDynArray read FBinaryData write FBinaryData;
    property EncodedString : string read GetEncodedString write SetEncodedString;
  end;

  { TBase64StringExtRemotable }

  TBase64StringExtRemotable = class(TAbstractEncodedStringExtRemotable)
  private
    function GetEncodedString : string; override;
    procedure SetEncodedString(const AValue : string); override;
  end;

  { TBase16StringExtRemotable }

  TBase16StringExtRemotable = class(TAbstractEncodedStringExtRemotable)
  private
    function GetEncodedString : string; override;
    procedure SetEncodedString(const AValue : string); override;
  end;

  { TComplexBooleanContentRemotable }

  TComplexBooleanContentRemotable = class(TBaseComplexSimpleContentRemotable)
  private
    FValue: Boolean;
  protected
    class procedure SaveValue(AObject : TBaseRemotable; AStore : IFormatterBase);override;
    class procedure LoadValue(var AObject : TObject; AStore : IFormatterBase);override;
  public
    property Value : Boolean read FValue write FValue;
  end;

  THeaderBlockClass = class of THeaderBlock;

  { THeaderBlock }

  THeaderBlock = class(TBaseComplexRemotable)
  private
    FDirection: THeaderDirection;
    FmustUnderstand: Integer;
    FName: string;
    FUnderstood: Boolean;
  private
    function HasmustUnderstand: boolean;
    procedure SetmustUnderstand(const AValue: Integer);
  protected
    function GetName: string; virtual;
    procedure SetName(const AValue: string); virtual;
  public
    property Direction : THeaderDirection read FDirection write FDirection;
    property Understood : Boolean read FUnderstood write FUnderstood;
    property Name : string read GetName write SetName;
  published
    property mustUnderstand : Integer read FmustUnderstand write SetmustUnderstand stored HasmustUnderstand;
  end;

  { TSimpleContentHeaderBlock
      Make a derived class of TSimpleContentHeaderBlock to handle a simple content
      header block.
  }
  TSimpleContentHeaderBlock = class(THeaderBlock)
  private
    FValue : string;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    property Value : string read FValue write FValue;
  end;

  { THeaderBlockProxy
      This class is used as a wrapper to allow a TBaseRemotable instance to be
      sent and received as a header block.
  }
  THeaderBlockProxy = class(THeaderBlock)
  private
    FActualObject: TBaseRemotable;
    FOwnObject: Boolean;
    FNameSet : Boolean;
  private
    procedure SetActualObject(const AValue: TBaseRemotable);
  protected
    function GetName : string; override;
    procedure SetName(const AValue: string); override;
  public
    procedure FreeObjectProperties();override;
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;

    property ActualObject : TBaseRemotable read FActualObject write SetActualObject;
    property OwnObject : Boolean read FOwnObject write FOwnObject;
  end;

  { TBaseArrayRemotable }

  TBaseArrayRemotable = class(TAbstractComplexRemotable)
  private
    FOptions : TInstanceOptions;
  protected
    class function GetItemName():string;virtual;
    class function GetStyle():TArrayStyle;virtual;
    procedure CheckIndex(const AIndex : Integer);
    function GetLength():Integer;virtual;abstract;
  public
    class function GetItemTypeInfo():PTypeInfo;virtual;abstract;
    destructor Destroy();override;

    procedure SetLength(const ANewSize : Integer);virtual;abstract;
    property Length : Integer Read GetLength;
    property Options : TInstanceOptions read FOptions write FOptions;
  end;

  TBaseArrayRemotableClass = class of TBaseArrayRemotable;

  { TBaseObjectArrayRemotable
      An implementation for array handling. The array items are "owned" by
      this class instance, so one has not to free them.
  }
  TBaseObjectArrayRemotable = class(TBaseArrayRemotable)
  Private
    FArray : Array Of TBaseRemotable;
  Protected
    function GetItem(AIndex: Integer): TBaseRemotable;
    function GetLength():Integer;override;
  Public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      Const AName     : String;
      Const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      Var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : String;
      const ATypeInfo : PTypeInfo
    );override;

    class function GetItemClass():TBaseRemotableClass;virtual;abstract;
    class function GetItemTypeInfo():PTypeInfo;override;

    constructor Create();override;
    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    procedure Exchange(const Index1,Index2 : Integer);

    procedure SetLength(Const ANewSize : Integer);override;
    Property Item[AIndex:Integer] : TBaseRemotable Read GetItem;Default;
  End;

  TBaseObjectArrayRemotableClass = class of TBaseObjectArrayRemotable;

  { TObjectCollectionRemotable
      An implementation for array handling. The array items are "owned" by
      this class instance, so one has not to free them.
  }
  TObjectCollectionRemotable = class(TBaseArrayRemotable)
  private
    FList : TObjectList;
  protected
    function GetItem(AIndex : Integer) : TBaseRemotable;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetLength() : Integer; override;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class function GetItemClass():TBaseRemotableClass;virtual;abstract;
    class function GetItemTypeInfo():PTypeInfo;override;

    constructor Create();override;
    destructor Destroy();override;
    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;

    function Add(): TBaseRemotable;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer): TBaseRemotable;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function Extract(const AIndex : Integer): TBaseRemotable;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Delete(const AIndex : Integer);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Exchange(const Index1,Index2 : Integer);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Clear();{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IndexOf(AObject : TBaseRemotable) : Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure SetLength(Const ANewSize : Integer);override;
    property Item[AIndex:Integer] : TBaseRemotable read GetItem;default;
  end;

  { TBaseSimpleTypeArrayRemotable }

  TBaseSimpleTypeArrayRemotable = class(TBaseArrayRemotable)
  protected
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );virtual;abstract;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );virtual;abstract;
  public
    class procedure Save(
            AObject   : TBaseRemotable;
            AStore    : IFormatterBase;
      const AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
    class procedure Load(
      var   AObject   : TObject;
            AStore    : IFormatterBase;
      var   AName     : string;
      const ATypeInfo : PTypeInfo
    );override;
  end;

  { TArrayOfStringRemotable }
  //  --------- Compiler Native String type !!!! ----------
  TArrayOfStringRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of String;
    function GetItem(AIndex: Integer): String;
    procedure SetItem(AIndex: Integer; const AValue: String);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    function Equal(const ACompareTo : TBaseRemotable) : Boolean;override;
    property Item[AIndex:Integer] : String read GetItem write SetItem; default;
  end;

  { TArrayOfBooleanRemotable }

  TArrayOfBooleanRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Boolean;
    function GetItem(AIndex: Integer): Boolean;
    procedure SetItem(AIndex: Integer; const AValue: Boolean);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Boolean read GetItem write SetItem; default;
  end;

  { TArrayOfInt8URemotable }

  TArrayOfInt8URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Byte;
    function GetItem(AIndex: Integer): Byte;
    procedure SetItem(AIndex: Integer; const AValue: Byte);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Byte read GetItem write SetItem; default;
  end;

  { TArrayOfInt8SRemotable }

  TArrayOfInt8SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of ShortInt;
    function GetItem(AIndex: Integer): ShortInt;
    procedure SetItem(AIndex: Integer; const AValue: ShortInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : ShortInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt16SRemotable }

  TArrayOfInt16SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of SmallInt;
    function GetItem(AIndex: Integer): SmallInt;
    procedure SetItem(AIndex: Integer; const AValue: SmallInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : SmallInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt16URemotable }

  TArrayOfInt16URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Word;
    function GetItem(AIndex: Integer): Word;
    procedure SetItem(AIndex: Integer; const AValue: Word);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Word read GetItem write SetItem; default;
  end;

  { TArrayOfInt32URemotable }

  TArrayOfInt32URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of LongWord;
    function GetItem(AIndex: Integer): LongWord;
    procedure SetItem(AIndex: Integer; const AValue: LongWord);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : LongWord read GetItem write SetItem; default;
  end;

  { TArrayOfInt32SRemotable }

  TArrayOfInt32SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of LongInt;
    function GetItem(AIndex: Integer): LongInt;
    procedure SetItem(AIndex: Integer; const AValue: LongInt);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : LongInt read GetItem write SetItem; default;
  end;

  { TArrayOfInt64SRemotable }

  TArrayOfInt64SRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Int64;
    function GetItem(AIndex: Integer): Int64;
    procedure SetItem(AIndex: Integer; const AValue: Int64);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Int64 read GetItem write SetItem; default;
  end;

  { TArrayOfInt64URemotable }

  TArrayOfInt64URemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of QWord;
    function GetItem(AIndex: Integer): QWord;
    procedure SetItem(AIndex: Integer; const AValue: QWord);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : QWord read GetItem write SetItem; default;
  end;

  { TArrayOfFloatSingleRemotable }

  TArrayOfFloatSingleRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Single;
    function GetItem(AIndex: Integer): Single;
    procedure SetItem(AIndex: Integer; const AValue: Single);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Single read GetItem write SetItem; default;
  end;

  { TArrayOfFloatDoubleRemotable }

  TArrayOfFloatDoubleRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Double;
    function GetItem(AIndex: Integer): Double;
    procedure SetItem(AIndex: Integer; const AValue: Double);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Double read GetItem write SetItem; default;
  end;

  { TArrayOfFloatExtendedRemotable }

  TArrayOfFloatExtendedRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Extended;
    function GetItem(AIndex: Integer): Extended;
    procedure SetItem(AIndex: Integer; const AValue: Extended);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Extended read GetItem write SetItem; default;
  end;

  { TArrayOfFloatCurrencyRemotable }

  TArrayOfFloatCurrencyRemotable = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Currency;
    function GetItem(AIndex: Integer): Currency;
    procedure SetItem(AIndex: Integer; const AValue: Currency);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(
            AStore : IFormatterBase;
      const AName  : String;
      const AIndex : Integer
    );override;
    procedure LoadItem(
            AStore : IFormatterBase;
      const AIndex : Integer
    );override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Currency read GetItem write SetItem; default;
  end;

  { TBaseFactoryRegistryItem }
  // Implementation helpers
  TBaseFactoryRegistryItem = class
  private
    FFactory: IItemFactory;
    FName: string;
  public
    constructor Create(
      const AName    : string;
      const AFactory : IItemFactory
    );
    destructor Destroy();override;
    property Name    : string Read FName;
    property Factory : IItemFactory Read FFactory;
  End;

  { TBaseFactoryRegistry }
  TBaseFactoryRegistry = class(TInterfacedObject,IInterface)
  private
    FList : TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TBaseFactoryRegistryItem;
  protected
    function FindFactory(const AName: string): IItemFactory;
    procedure Register(
      const AName    : string;
            AFactory : IItemFactory
    );
  protected
    property Count : Integer read GetCount;
    property Item[Index:Integer] : TBaseFactoryRegistryItem read GetItem;
  public
    constructor Create();
    destructor Destroy();override;
  End;

  { TSimpleFactoryItem }

  TSimpleFactoryItem = class(TInterfacedObject)
  public
    constructor Create();virtual;
  End;

  TSimpleFactoryItemClass = class of TSimpleFactoryItem;

  { TSimpleItemFactory }
{$TYPEINFO ON}
  TSimpleItemFactory = class(TInterfacedObject,IItemFactory)
  private
    FItemClass : TSimpleFactoryItemClass;
  protected
    function CreateInstance():IInterface;virtual;
    function GetItemClass() : TSimpleFactoryItemClass;
  public
    constructor Create(AItemClass : TSimpleFactoryItemClass);
  End;
{$TYPEINFO OFF}
  { TIntfPoolItem }

  TIntfPoolItem = class
  private
    FIntf: IInterface;
    FUsed: Boolean;
  public
    constructor Create(AIntf : IInterface; const AUsed : Boolean);
    destructor Destroy();override;
    property Intf : IInterface read FIntf;
    property Used : Boolean read FUsed write FUsed;
  end;

  TIntfPool = class
  private
    FList : TObjectList;
    FCS : TCriticalSection;
    FLock : TSemaphoreObject;
    FFactory : IItemFactory;
    FMin : Integer;
    FMax : Integer;
  private
    function CreateNew(const AUsed : Boolean) : TIntfPoolItem;
    function TryGet(const AIndex: Integer): Boolean;
  public
    constructor Create(
      const AMin, AMax : Integer;
            AFactory   : IItemFactory
    );
    destructor Destroy();override;
    function Get(const ATimeOut : Cardinal) : IInterface;
    procedure Release(const AItem : IInterface);
    procedure Discard(const AItem : IInterface);
    function GetInstancesCount: Integer;
    property Min : Integer read FMin;
    property Max : Integer read FMax;
  end;

  { TSimpleItemFactoryEx }

  TSimpleItemFactoryEx = class(TSimpleItemFactory,IItemFactory,IItemFactoryEx)
  private
    FPooled: Boolean;
    FPoolMax: Integer;
    FPoolMin: Integer;
    FPropertyNames : TStringList;
    FProperties : IInterfaceList;
    FPool : TIntfPool;
    FTimeOut: PtrUInt;
  private
    procedure PreparePool();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPooled(const AValue: Boolean);
    procedure SetPoolMax(const AValue: Integer);
    procedure SetPoolMin(const AValue: Integer);
  protected
    function CreateInstance():IInterface;override;
    procedure ReleaseInstance(const AInstance : IInterface);virtual;
    procedure DiscardInstance(const AInstance : IInterface);virtual;
    function GetPropertyManager(
      const APropertyGroup : string;
      const ACreateIfNotExists : Boolean
    ):IPropertyManager;
  public
    constructor Create(
      AItemClass         : TSimpleFactoryItemClass;
      const APropsString : string
    );overload;
    constructor Create(AItemClass : TSimpleFactoryItemClass);overload;
    destructor Destroy();override;
  published
    property PoolMax : Integer read FPoolMax write SetPoolMax;
    property PoolMin : Integer read FPoolMin write SetPoolMin;
    property Pooled : Boolean read FPooled write SetPooled;
    property TimeOut : PtrUInt read FTimeOut write FTimeOut;
  end;

  TTypeRegistryItemOption = (
    trioNonVisibleToMetadataService,
    trioUnqualifiedElement, trioQualifiedElement,
    trioUnqualifiedAttribute, trioQualifiedAttribute
  );
  TTypeRegistryItemOptions = set of TTypeRegistryItemOption;
  TTypeRegistry = class;
  TTypeRegistryItem = class;
  TTypeRegistryItemClass = class of TTypeRegistryItem;

  TRemotableTypeInitializerClass = class of TRemotableTypeInitializer;

  { TRemotableTypeInitializer }

  TRemotableTypeInitializer = class
  public
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean;virtual;
    class function GetItemClass(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;virtual;
{$IFDEF TRemotableTypeInitializer_Initialize}
    class function Initialize(
      ATypeInfo : PTypeInfo;
      ARegistryItem : TTypeRegistryItem
    ) : Boolean;virtual;abstract;
{$ENDIF TRemotableTypeInitializer_Initialize}
  end;

  TPropertyNameType = ( pntInternalName, pntExternalName );

  { TPropertyItem }

  TPropertyItem = class
  private
    FExternalName: string;
    FExtObject: TObject;
    FInternalName: string;
    FOptions: TTypeRegistryItemOptions;
  public
    property InternalName : string read FInternalName {write FInternalName};
    property ExternalName : string read FExternalName {write FExternalName};
    property ExtObject : TObject read FExtObject {write FExtObject};
    property Options : TTypeRegistryItemOptions read FOptions {write FOptions};
  end;

  { TTypeRegistryItem }

  TTypeRegistryItem = class
  private
    //FDefaultPropertyOptions: TTypeRegistryItemOptions;
    FOwner : TTypeRegistry;
    FDataType: PTypeInfo;
    FNameSpace: string;
    FDeclaredName : string;
    FOptions: TTypeRegistryItemOptions;
    FPascalSynonyms : TStrings;
    FExternalSynonyms : TStrings;
    FProperties : TObjectList;
    procedure SetOptions(AValue: TTypeRegistryItemOptions);
  protected
    procedure Init(); virtual;
  protected
    function IndexOfProp(
      const AName : string;
      const ANameType : TPropertyNameType
    ) : Integer;
  public
    constructor Create(
            AOwner        : TTypeRegistry;
            ANameSpace    : string;
            ADataType     : PTypeInfo;
      Const ADeclaredName : string = ''
    );virtual;
    destructor Destroy();override;
    function AddPascalSynonym(const ASynonym : string):TTypeRegistryItem;
    function AddExternalSynonym(const ASynonym : string):TTypeRegistryItem;
    function IsSynonym(const APascalTypeName : string):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function IsExternalSynonym(const AExternalName : string):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}

    function FindProperty(
      const AName : string;
      const ANameType : TPropertyNameType
    ) : TPropertyItem;
    procedure RegisterExternalPropertyName(const APropName, AExtPropName : string); virtual;
    function GetExternalPropertyName(const APropName : string) : string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetInternalPropertyName(const AExtPropName : string) : string;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure SetPropertyOptions(
      const APropName : string;
      const AOptions : TTypeRegistryItemOptions
    ); virtual;
    procedure AddOptions(const AOptions : TTypeRegistryItemOptions);

    procedure RegisterObject(const APropName : string; const AObject : TObject);
    function GetObject(const APropName : string) : TObject;

    property Owner : TTypeRegistry read FOwner;
    property DataType : PTypeInfo read FDataType;
    property NameSpace : string read FNameSpace;
    property DeclaredName : string read FDeclaredName;
    property Options : TTypeRegistryItemOptions read FOptions write SetOptions;
    //property DefaultPropertyOptions : TTypeRegistryItemOptions
      //read FDefaultPropertyOptions write FDefaultPropertyOptions;
  end;

  TTypeRegistrySearchOption = ( trsoIncludeExternalSynonyms );
  TTypeRegistrySearchOptions = set of TTypeRegistrySearchOption;

  { TTypeRegistry }

  TTypeRegistry = class
  private
    FList : TObjectList;
    FInitializerList : TClassList;
  private
    function GetItemClassFor(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;
{$IFDEF TRemotableTypeInitializer_Initialize}
    procedure InitializeItem(AItem : TTypeRegistryItem);
{$ENDIF TRemotableTypeInitializer_Initialize}
    function GetCount: Integer;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetItemByIndex(Index: Integer): TTypeRegistryItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetItemByTypeInfo(Index: PTypeInfo): TTypeRegistryItem;
  public
    constructor Create();
    destructor Destroy();override;
    procedure RegisterInitializer(AInitializer : TRemotableTypeInitializerClass);
    function IndexOf(Const ATypeInfo : PTypeInfo):Integer;
    function Add(AItem:TTypeRegistryItem):Integer;
    function Register(
      const ANameSpace    : string;
      const ADataType     : PTypeInfo;
      const ADeclaredName : string;
      const AOptions      : TTypeRegistryItemOptions
    ):TTypeRegistryItem;overload;
    function Register(
      Const ANameSpace    : String;
      Const ADataType     : PTypeInfo;
      Const ADeclaredName : String = ''
    ):TTypeRegistryItem;overload;
    function Find(ATypeInfo : PTypeInfo; Const AExact : Boolean):TTypeRegistryItem;overload;
    function Find(const APascalTypeName : string):TTypeRegistryItem;overload;
    function FindByDeclaredName(
      const ATypeName,
            ANameSpace : string;
      const AOptions   : TTypeRegistrySearchOptions = []
    ) : TTypeRegistryItem;
    Property Count : Integer Read GetCount;
    Property Item[Index:Integer] : TTypeRegistryItem Read GetItemByIndex;default;
    Property ItemByTypeInfo[Index:PTypeInfo] : TTypeRegistryItem Read GetItemByTypeInfo;
  end;

  TPropStoreType = ( pstNever, pstOptional, pstAlways );

  EPropertyException = class(Exception)
  end;

  { TStoredPropertyManager }

  TStoredPropertyManager = class(TInterfacedObject,IPropertyManager)
  private
    FData : TStringList;
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  protected
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  public
    constructor Create();
    destructor Destroy();override;
  end;

const
  sXSD_NS = 'http://www.w3.org/2001/XMLSchema';
  sXSD = 'xsd';
  sSOAP_ENV = 'http://schemas.xmlsoap.org/soap/envelope/';
  sSOAP_ENV_ABR = 'SOAP-ENV';
  sWST_BASE_NS_ABR = 'wst';
  sWST_BASE_NS = 'urn:wst_base';

  PROP_LIST_DELIMITER = ';';
  FIELDS_STRING = '__FIELDS__';

  function GetTypeRegistry():TTypeRegistry;
  procedure RegisterStdTypes();overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure RegisterStdTypes(ARegistry : TTypeRegistry);overload;
  procedure RegisterAttributeProperty(
    const ATypeInfo : PTypeInfo; // must be tkClass or tkRecord
    const AProperty : shortstring
  );
  procedure SetFieldSerializationVisibility(
    const ATypeInfo   : PTypeInfo; // must be tkRecord
    const AField      : shortstring;
    const AVisibility : Boolean
  );
  function GetExternalName(
    const ATypeInfo : PTypeInfo;
    const ARegistry : TTypeRegistry = nil
  ) : string;


  function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;

  procedure initialize_base_service_intf();
  procedure finalize_base_service_intf();

{$IFDEF HAS_FORMAT_SETTINGS}
var
  wst_FormatSettings : TFormatSettings;
{$ENDIF HAS_FORMAT_SETTINGS}

implementation
uses
  wst_consts, imp_utils, record_rtti, basex_encode, object_serializer, DateUtils;


type
  PObject = ^TObject;
  
  TEnumBuffer = Record
    Case TOrdType Of
      otSByte : (ShortIntData : ShortInt);
      otUByte : (ByteData : Byte);
      otSWord : (SmallIntData : SmallInt);
      otUWord : (WordData : Word);
      otSLong : (SLongIntData : LongInt);
      otULong : (ULongIntData : LongWord);
  End;
  TFloatBuffer = Record
    Case TFloatType Of
      ftSingle : (SingleData : Single);
      ftDouble : (DoubleData : Double);
      ftExtended : (ExtendedData : Extended);
      ftCurr : (CurrencyData : Currency);
      ftComp : (CompData : Comp);
  End; 
    
var
  TypeRegistryInstance : TTypeRegistry = Nil;

function GetTypeRegistry():TTypeRegistry;
begin
  If Not Assigned(TypeRegistryInstance) Then
    TypeRegistryInstance := TTypeRegistry.Create();
  Result := TypeRegistryInstance;
end;

procedure RegisterStdTypes();
begin
  RegisterStdTypes(GetTypeRegistry());
end;

procedure RegisterStdTypes(ARegistry : TTypeRegistry);
Var
  r : TTypeRegistry;
  ri : TTypeRegistryItem;
begin
  r := ARegistry;
  r.Register(sXSD_NS,TypeInfo(Integer),'int').AddPascalSynonym('Integer');
    r.Register(sXSD_NS,TypeInfo(LongWord),'unsignedInt');
    r.Register(sXSD_NS,TypeInfo(positiveInteger),'positiveInteger');
    r.Register(sXSD_NS,TypeInfo(nonNegativeInteger),'nonNegativeInteger');


  r.Register(sXSD_NS,TypeInfo(string),'string').AddPascalSynonym('string');
  r.Register(sXSD_NS,TypeInfo(AnsiString),'ansistring').AddPascalSynonym('ansistring');
  r.Register(sXSD_NS,TypeInfo(WideString),'widestring').AddPascalSynonym('widestring');
{$IFDEF WST_UNICODESTRING}
  r.Register(sXSD_NS,TypeInfo(UnicodeString),'UnicodeString').AddPascalSynonym('unicodestring');
{$ENDIF WST_UNICODESTRING}
  r.Register(sXSD_NS,TypeInfo(anyURI),'anyURI').AddPascalSynonym('anyURI');
  r.Register(sXSD_NS,TypeInfo(anyType_Type),'anyType').AddPascalSynonym('anyType_Type');
  r.Register(sXSD_NS,TypeInfo(schema_Type),'schema').AddPascalSynonym('schema_Type');
  r.Register(sXSD_NS,TypeInfo(token),'token').AddPascalSynonym('token');    
  r.Register(sXSD_NS,TypeInfo(language),'language').AddPascalSynonym('language');               

  r.Register(sXSD_NS,TypeInfo(boolean),'boolean').AddPascalSynonym('boolean');

  r.Register(sXSD_NS,TypeInfo(Byte),'unsignedByte').AddPascalSynonym('Byte');
    r.Register(sXSD_NS,TypeInfo(ShortInt),'byte').AddPascalSynonym('ShortInt');
  r.Register(sXSD_NS,TypeInfo(Word),'unsignedShort').AddPascalSynonym('Word');
    r.Register(sXSD_NS,TypeInfo(SmallInt),'short').AddPascalSynonym('SmallInt');
  r.Register(sXSD_NS,TypeInfo(Int64),'long').AddPascalSynonym('Int64');
    r.Register(sXSD_NS,TypeInfo(QWord),'unsignedLong').AddPascalSynonym('QWord');

  r.Register(sXSD_NS,TypeInfo(Single),'float').AddPascalSynonym('Single');
  r.Register(sXSD_NS,TypeInfo(Currency),'float').AddPascalSynonym('Currency');
  r.Register(sXSD_NS,TypeInfo(Comp),'float').AddPascalSynonym('Comp');
  r.Register(sXSD_NS,TypeInfo(Double),'double').AddPascalSynonym('Double');
  r.Register(sXSD_NS,TypeInfo(Extended),'decimal').AddPascalSynonym('Extended');

  r.Register(sXSD_NS,TypeInfo(TDateTimeRemotable),'dateTime').AddPascalSynonym('TDateTimeRemotable');
  r.Register(sXSD_NS,TypeInfo(TDateRemotable),'date').AddPascalSynonym('TDateRemotable');
{$IFDEF WST_HAS_TDURATIONREMOTABLE}
  r.Register(sXSD_NS,TypeInfo(TDurationRemotable),'duration').AddPascalSynonym('TDurationRemotable');
{$ELSE WST_HAS_TDURATIONREMOTABLE}
  r.Register(sXSD_NS,TypeInfo(duration),'duration').AddPascalSynonym('duration');
{$ENDIF WST_HAS_TDURATIONREMOTABLE}
{$IFDEF WST_HAS_TTIMEREMOTABLE}
  r.Register(sXSD_NS,TypeInfo(TTimeRemotable),'time').AddPascalSynonym('TTimeRemotable');
{$ELSE WST_HAS_TTIMEREMOTABLE}
  r.Register(sXSD_NS,TypeInfo(time),'time').AddPascalSynonym('time');
{$ENDIF WST_HAS_TTIMEREMOTABLE}

  ri := r.Register(sWST_BASE_NS,TypeInfo(TBaseArrayRemotable),'TBaseArrayRemotable');
  ri.Options := ri.Options + [trioNonVisibleToMetadataService];

  THeaderBlock.RegisterAttributeProperty('mustUnderstand');
  ri := r.Register(sSOAP_ENV,TypeInfo(THeaderBlock),'THeaderBlock');
  ri.Options := ri.Options + [trioNonVisibleToMetadataService,trioQualifiedAttribute];
  ri.SetPropertyOptions('mustUnderstand',[]);
  ri := r.Register(sSOAP_ENV,TypeInfo(TSimpleContentHeaderBlock));
  ri.Options := ri.Options + [trioNonVisibleToMetadataService,trioQualifiedAttribute];
  ri := r.Register(sSOAP_ENV,TypeInfo(THeaderBlockProxy));
  ri.Options := ri.Options + [trioNonVisibleToMetadataService,trioQualifiedAttribute];


  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfStringRemotable),'TArrayOfStringRemotable').AddPascalSynonym('TArrayOfStringRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfBooleanRemotable),'TArrayOfBooleanRemotable').AddPascalSynonym('TArrayOfBooleanRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt8URemotable),'TArrayOfInt8URemotable').AddPascalSynonym('TArrayOfInt8URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt8SRemotable),'TArrayOfInt8SRemotable').AddPascalSynonym('TArrayOfInt8SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt16URemotable),'TArrayOfInt16URemotable').AddPascalSynonym('TArrayOfInt16URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt16SRemotable),'TArrayOfInt16SRemotable').AddPascalSynonym('TArrayOfInt16SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt32URemotable),'TArrayOfInt32URemotable').AddPascalSynonym('TArrayOfInt32URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt32SRemotable),'TArrayOfInt32SRemotable').AddPascalSynonym('TArrayOfInt32SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt64URemotable),'TArrayOfInt64URemotable').AddPascalSynonym('TArrayOfInt64URemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfInt64SRemotable),'TArrayOfInt64SRemotable').AddPascalSynonym('TArrayOfInt64SRemotable');

  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatSingleRemotable),'TArrayOfFloatSingleRemotable').AddPascalSynonym('TArrayOfFloatSingleRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatDoubleRemotable),'TArrayOfFloatDoubleRemotable').AddPascalSynonym('TArrayOfFloatDoubleRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatExtendedRemotable),'TArrayOfFloatExtendedRemotable').AddPascalSynonym('TArrayOfFloatExtendedRemotable');
  r.Register(sWST_BASE_NS,TypeInfo(TArrayOfFloatCurrencyRemotable),'TArrayOfFloatCurrencyRemotable').AddPascalSynonym('TArrayOfFloatCurrencyRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexInt64SContentRemotable),'long').AddPascalSynonym('TComplexInt64SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt64UContentRemotable),'unsignedLong').AddPascalSynonym('TComplexInt64UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexInt32SContentRemotable),'int').AddPascalSynonym('TComplexInt32SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt32UContentRemotable),'unsignedInt').AddPascalSynonym('TComplexInt32UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexInt16SContentRemotable),'short').AddPascalSynonym('TComplexInt16SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt16UContentRemotable),'unsignedShort').AddPascalSynonym('TComplexInt16UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexInt8SContentRemotable),'byte').AddPascalSynonym('TComplexInt8SContentRemotable');
    r.Register(sXSD_NS,TypeInfo(TComplexInt8UContentRemotable),'unsignedByte').AddPascalSynonym('TComplexInt8UContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexFloatExtendedContentRemotable),'decimal').AddPascalSynonym('TComplexFloatExtendedContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexFloatDoubleContentRemotable),'double').AddPascalSynonym('TComplexFloatDoubleContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexFloatSingleContentRemotable),'Single').AddPascalSynonym('TComplexFloatSingleContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexStringContentRemotable),'string').AddPascalSynonym('TComplexStringContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexWideStringContentRemotable),'widestring').AddPascalSynonym('TComplexWideStringContentRemotable');
{$IFDEF WST_UNICODESTRING}
  r.Register(sXSD_NS,TypeInfo(TComplexUnicodeStringContentRemotable),'unicodestring').AddPascalSynonym('TComplexUnicodeStringContentRemotable');
{$ENDIF WST_UNICODESTRING}
  r.Register(sXSD_NS,TypeInfo(TComplexBooleanContentRemotable),'boolean').AddPascalSynonym('TComplexBooleanContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TComplexAnsiCharContentRemotable),'AnsiChar').AddPascalSynonym('TComplexAnsiCharContentRemotable');
  r.Register(sXSD_NS,TypeInfo(TComplexWideCharContentRemotable),'WideChar').AddPascalSynonym('TComplexWideCharContentRemotable');

  r.Register(sXSD_NS,TypeInfo(TBase64StringRemotable),'base64Binary').AddPascalSynonym('TBase64StringRemotable');
  r.Register(sXSD_NS,TypeInfo(TBase64StringExtRemotable),'base64Binary').AddPascalSynonym('TBase64StringExtRemotable');
  r.Register(sXSD_NS,TypeInfo(TBase16StringRemotable),'hexBinary').AddPascalSynonym('TBase16StringRemotable');
  r.Register(sXSD_NS,TypeInfo(TBase16StringExtRemotable),'hexBinary').AddPascalSynonym('TBase16StringExtRemotable');
end;

function GetExternalName(
  const ATypeInfo : PTypeInfo;
  const ARegistry : TTypeRegistry
) : string;
var
  locReg : TTypeRegistry;
  locRegItem : TTypeRegistryItem;
begin
  if ( ARegistry = nil ) then
    locReg := GetTypeRegistry()
  else
    locReg := ARegistry;
  locRegItem := locReg.Find(ATypeInfo,False);
  if ( locRegItem <> nil ) then
    Result := locRegItem.DeclaredName
  else
    Result := ATypeInfo^.Name;
end;

procedure SetFieldSerializationVisibility(
  const ATypeInfo   : PTypeInfo; // must be tkRecord
  const AField      : shortstring;
  const AVisibility : Boolean
);
var
  recordData : TRecordRttiDataObject;
begin
  if Assigned(ATypeInfo) and ( ATypeInfo^.Kind = tkRecord ) and
     ( not IsStrEmpty(AField) )
  then begin
    recordData := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetObject(FIELDS_STRING) as TRecordRttiDataObject;
    if Assigned(recordData) then begin
      recordData.GetField(AField)^.Visible := AVisibility;
    end else begin
      raise EServiceConfigException.CreateFmt(SERR_RecordExtendedRttiNotFound,[ATypeInfo^.Name]);
    end;
  end else begin
    raise EServiceConfigException.Create(SERR_InvalidParameters);
  end;
end;

procedure RegisterAttributeProperty(
  const ATypeInfo : PTypeInfo;
  const AProperty : shortstring
);
var
  ok : Boolean;
  recordData : TRecordRttiDataObject;
begin
  ok := False;
  if Assigned(ATypeInfo) and
     ( not IsStrEmpty(AProperty) )
  then begin
    case ATypeInfo^.Kind of
      tkClass :
        begin
          if GetTypeData(ATypeInfo)^.ClassType.InheritsFrom(TAbstractComplexRemotable) then begin
            TAbstractComplexRemotableClass(GetTypeData(ATypeInfo)^.ClassType).RegisterAttributeProperty(AProperty);
            ok := True;
          end;
        end;
      tkRecord :
        begin
          recordData := GetTypeRegistry().ItemByTypeInfo[ATypeInfo].GetObject(FIELDS_STRING) as TRecordRttiDataObject;
          if Assigned(recordData) then begin
            recordData.GetField(AProperty)^.IsAttribute := True;
            ok := True;
          end;
        end;
    end;
  end;
  if not ok then
    raise EServiceConfigException.Create(SERR_InvalidParameters);
end;

{$IFDEF FPC}
function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;
begin
  case (PropInfo^.PropProcs shr 4) and 3 of
    ptfield:
      Result := pstOptional;
    ptconst:
      begin
        if LongBool(PropInfo^.StoredProc) then
          Result := pstAlways
        else
          Result := pstNever;
      end;
    ptstatic,
    ptvirtual:
      Result := pstOptional;
  end;
end;
{$ELSE}
function IsStoredPropClass(AClass : TClass;PropInfo : PPropInfo) : TPropStoreType;
{var
  b : PByte;
begin
  if ( ( PropInfo^.StoredProc and $0FFFFFF00 ) = 0 ) then begin
    if LongBool(PropInfo^.StoredProc) then // constante
      Result := pstAlways
    else
      Result := pstNever;
  end else begin
    b := PByte(PropInfo^.StoredProc);
    Inc(b,3);
    if ( b^ < $FE ) then begin //StaticMethod
      Result := pstOptional;
    end else ( b^ > $FE ) begin Field
    end else begin // virtual method
    end;
  end;
end;}
begin
  if ( ( Cardinal(PropInfo^.StoredProc) and $0FFFFFF00 ) = 0 ) then begin
    if LongBool(PropInfo^.StoredProc) then begin
      Result := pstAlways
    end else begin
      Result := pstNever;
    end;
  end else begin
    Result := pstOptional;
  end;
end;
{$ENDIF}

{ TBaseRemotable }

constructor TBaseRemotable.Create();
begin
end;

destructor TBaseRemotable.Destroy();
begin
  FreeObjectProperties();
  inherited Destroy();
end;

procedure TBaseRemotable.FreeObjectProperties();
begin
  //Derived classes should override this method to free their object(s) and array(s).
end;

function TBaseRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
begin
  Result := ( Self = ACompareTo );
end;

function TBaseRemotable.wstHasValue() : Boolean;
begin
  Result := True;
end;

{ TBaseComplexRemotable }
Type   

  { TSerializeOptions }

  TSerializeOptions = class
  private
    FAttributeFieldList : TStringList;
  private
    FElementClass: TAbstractComplexRemotableClass;
    procedure AddAttributeField(const AAttributeField : string);
    function GetAttributeCount: Integer;
    function GetAttributeField(AIndex : Integer): string;
  public
    constructor Create(const AElementClass : TAbstractComplexRemotableClass);
    destructor Destroy();override;
    function IsAttributeField(const AField : string):Boolean;
    property ElementClass : TAbstractComplexRemotableClass read FElementClass;
    property AttributeFieldCount : Integer read GetAttributeCount;
    property AttributeField[AIndex : Integer] : string read GetAttributeField;
  end;

  { TSerializeOptionsRegistry }

  TSerializeOptionsRegistry = class
  private
    FList : TObjectList;
  private
    function GetCount: Integer;
    function GetItem(AIndex : Integer): TSerializeOptions;
    function IndexOf(const AElementClass : TAbstractComplexRemotableClass):Integer;
  public
    constructor Create();
    destructor Destroy();override;
    function RegisterClass(const AElementClass : TAbstractComplexRemotableClass):TSerializeOptions;
    function Find(const AElementClass : TAbstractComplexRemotableClass):TSerializeOptions;
    property Count : Integer read GetCount;
    property Item[AIndex : Integer] : TSerializeOptions read GetItem;
  end;

var
  SerializeOptionsRegistryInstance : TSerializeOptionsRegistry = nil;

function GetSerializeOptionsRegistry():TSerializeOptionsRegistry;
begin
  if not Assigned(SerializeOptionsRegistryInstance) then
    SerializeOptionsRegistryInstance := TSerializeOptionsRegistry.Create();
  Result := SerializeOptionsRegistryInstance;
end;

{ TSerializeOptionsRegistry }

function TSerializeOptionsRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSerializeOptionsRegistry.GetItem(AIndex : Integer): TSerializeOptions;
begin
  Result := FList[AIndex] as TSerializeOptions;
end;

function TSerializeOptionsRegistry.IndexOf(
  const AElementClass: TAbstractComplexRemotableClass
): Integer;
begin
  for Result := 0 to Pred(Count) do begin
    if ( Item[Result].ElementClass = AElementClass ) then
      Exit;
  end;
  Result := -1;
end;

constructor TSerializeOptionsRegistry.Create();
begin
  FList := TObjectList.Create(True);
end;

destructor TSerializeOptionsRegistry.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

function TSerializeOptionsRegistry.RegisterClass(
  const AElementClass: TAbstractComplexRemotableClass
): TSerializeOptions;
var
  i, j, k, c : Integer;
  ri : TSerializeOptions;
begin
  i := IndexOf(AElementClass);
  if ( i < 0 ) then begin
    c := FList.Count;
    i := FList.Add(TSerializeOptions.Create(AElementClass));
    Result := FList[i] as TSerializeOptions;
    for j := 0 to Pred(c) do begin
      ri := FList[j] as TSerializeOptions;
      if AElementClass.InheritsFrom(ri.ElementClass) then begin
        for k := 0 to Pred(ri.AttributeFieldCount) do begin
          Result.FAttributeFieldList.Add(ri.FAttributeFieldList[k]);
        end;
      end;
    end;
  end;
  Result := FList[i] as TSerializeOptions;
end;

function TSerializeOptionsRegistry.Find(const AElementClass: TAbstractComplexRemotableClass): TSerializeOptions;
var
  i : Integer;
begin
  i := IndexOf(AElementClass);
  if ( i >= 0 ) then
    Result := FList[i] as TSerializeOptions
  else
    Result := nil;
end;

{ TSerializeOptions }

procedure TSerializeOptions.AddAttributeField(const AAttributeField: string);
begin
  if ( FAttributeFieldList.IndexOf(AAttributeField) < 0 ) then
    FAttributeFieldList.Add(AAttributeField);
end;

function TSerializeOptions.GetAttributeCount: Integer;
begin
  Result := FAttributeFieldList.Count;
end;

function TSerializeOptions.GetAttributeField(AIndex : Integer): string;
begin
  Result := FAttributeFieldList[AIndex];
end;

constructor TSerializeOptions.Create(const AElementClass: TAbstractComplexRemotableClass);
begin
  FElementClass := AElementClass;
  FAttributeFieldList := TStringList.Create();
  FAttributeFieldList.Duplicates := dupIgnore;
  FAttributeFieldList.Sorted := True;
end;

destructor TSerializeOptions.Destroy();
begin
  FreeAndNil(FAttributeFieldList);
  inherited Destroy();
end;

function TSerializeOptions.IsAttributeField(const AField: string): Boolean;
begin
  Result := ( FAttributeFieldList.IndexOf(AField) >= 0 );
end;

class procedure TBaseComplexRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : String;
  const ATypeInfo  : PTypeInfo
);
{$IFDEF USE_SERIALIZE}
var
  locSerializer : TObjectSerializer;
begin
  locSerializer := TBaseComplexTypeRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then
    locSerializer.Save(AObject,AStore,AName,ATypeInfo)
  else
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
end;
{$ELSE USE_SERIALIZE}
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  int64Data : Int64;
  strData : String;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumBuffer;
  floatDt : TFloatBuffer;
  p : PPropInfo;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
  prpName : string;
begin
  oldSS := AStore.GetSerializationStyle();
  AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    propCount := GetTypeData(ATypeInfo)^.PropCount;
    if ( propCount > 0 ) then begin
      propListLen := GetPropList(ATypeInfo,propList);
      try
        ss := AStore.GetSerializationStyle();
        typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
        for i := 0 to Pred(propCount) do begin
          p := propList^[i];
          pt := p^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
          if IsStoredProp(AObject,p) then begin
            if IsAttributeProperty(p^.Name) then begin
              if ( ss <> ssAttibuteSerialization ) then
                ss := ssAttibuteSerialization;
            end else begin
              if ( ss <> ssNodeSerialization ) then
                ss := ssNodeSerialization;
            end;
            if ( ss <> AStore.GetSerializationStyle() ) then
              AStore.SetSerializationStyle(ss);
            prpName := typRegItem.GetExternalPropertyName(p^.Name);
            case pt^.Kind of
              tkInt64{$IFDEF HAS_QWORD},tkQWord{$ENDIF} :
                begin
                  int64Data := GetInt64Prop(AObject,p^.Name);
                  AStore.Put(prpName,pt,int64Data);
                end;
              tkLString{$IFDEF FPC},tkAString{$ENDIF} :
                begin
                  strData := GetStrProp(AObject,p^.Name);
                  AStore.Put(prpName,pt,strData);
                end;
              tkClass :
                begin
                  objData := GetObjectProp(AObject,p^.Name);
                  AStore.Put(prpName,pt,objData);
                end;
              {$IFDEF HAS_TKBOOL}
              tkBool :
                begin
                  boolData := Boolean(GetOrdProp(AObject,p^.Name));
                  AStore.Put(prpName,pt,boolData);
                end;
              {$ENDIF}
              tkEnumeration,tkInteger :
                begin
                {$IFDEF WST_DELPHI}
                  if ( pt^.Kind = tkEnumeration ) and
                     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                  then begin
                    boolData := Boolean(GetOrdProp(AObject,p^.Name));
                    AStore.Put(prpName,pt,boolData);
                  end else begin
                {$ENDIF}
                    FillChar(enumData,SizeOf(enumData),#0);
                    case GetTypeData(pt)^.OrdType of
                      otSByte :
                        begin
                          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ShortIntData);
                        end;
                      otUByte :
                        begin
                          enumData.ByteData := Byte(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ByteData);
                        end;
                      otSWord :
                        begin
                          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.SmallIntData);
                        end;
                      otUWord :
                        begin
                          enumData.WordData := Word(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.WordData);
                        end;
                      otSLong :
                        begin
                          enumData.SLongIntData := LongInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.SLongIntData);
                        end;
                      otULong :
                        begin
                          enumData.ULongIntData := LongWord(GetOrdProp(AObject,p^.Name));
                          AStore.Put(prpName,pt,enumData.ULongIntData);
                        end;
                    end;
                {$IFDEF WST_DELPHI}
                  end;
                {$ENDIF}
                end;
              tkFloat :
                begin
                  FillChar(floatDt,SizeOf(floatDt),#0);
                  case GetTypeData(pt)^.FloatType of
                    ftSingle :
                      begin
                        floatDt.SingleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.SingleData);
                      end;
                    ftDouble :
                      begin
                        floatDt.DoubleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.DoubleData);
                      end;
                    ftExtended :
                      begin
                        floatDt.ExtendedData := Extended(GetFloatProp(AObject,p^.Name));
                        AStore.Put(prpName,pt,floatDt.ExtendedData);
                      end;
                    ftCurr :
                      begin
                        floatDt.CurrencyData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.CurrencyData);
                      end;
{$IFDEF HAS_COMP}
                    ftComp :
                      begin
                        floatDt.CompData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(prpName,pt,floatDt.CompData);
                      end;
{$ENDIF}
                  end;
                end;
            end;
          end;
        end;
      finally
        Freemem(propList,propListLen*SizeOf(Pointer));
      end;
    end;
  finally
    AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;
{$ENDIF USE_SERIALIZE}

Type
  TFloatExtendedType = Extended;
class procedure TBaseComplexRemotable.Load(
  Var   AObject    : TObject;
        AStore     : IFormatterBase;
  var   AName      : String;
  const ATypeInfo  : PTypeInfo
);
{$IFDEF USE_SERIALIZE}
var
  locSerializer : TObjectSerializer;
begin
  locSerializer := TBaseComplexTypeRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then
    locSerializer.Read(AObject,AStore,AName,ATypeInfo)
  else
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
end;
{$ELSE USE_SERIALIZE}
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  propName : String;
  int64Data : Int64;
  strData : String;
  objData : TObject;
    objDataCreateHere : Boolean;
  boolData : Boolean;
  p : PPropInfo;
  enumData : TEnumBuffer;
  floatDt : TFloatExtendedType;
  floatBuffer : TFloatBuffer;
  persistType : TPropStoreType;
  objTypeData : PTypeData;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      If Not Assigned(AObject) Then
        AObject := Create();
      objTypeData := GetTypeData(ATypeInfo);
      propCount := objTypeData^.PropCount;
      If ( propCount > 0 ) Then Begin
        propListLen := GetPropList(ATypeInfo,propList);
        Try
          typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
          For i := 0 To Pred(propCount) Do Begin
            p := propList^[i];
            persistType := IsStoredPropClass(objTypeData^.ClassType,p);
            If ( persistType in [pstOptional,pstAlways] ) Then Begin
              pt := p^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
              propName := typRegItem.GetExternalPropertyName(p^.Name);
              if IsAttributeProperty(p^.Name) then begin
                ss := ssAttibuteSerialization;
              end else begin
                ss := ssNodeSerialization;
              end;
              if ( ss <> AStore.GetSerializationStyle() ) then
                AStore.SetSerializationStyle(ss);
              try
                Case pt^.Kind Of
                  tkInt64{$IFDEF HAS_QWORD},tkQWord{$ENDIF} :
                    begin
                      AStore.Get(pt,propName,int64Data);
                      SetInt64Prop(AObject,p^.Name,int64Data);
                    end;
                  tkLString{$IFDEF FPC}, tkAString{$ENDIF} :
                    Begin
                      AStore.Get(pt,propName,strData);
                      SetStrProp(AObject,p^.Name,strData);
                    End;
                  {$IFDEF HAS_TKBOOL}
                  tkBool :
                    Begin
                      AStore.Get(pt,propName,boolData);
                      SetOrdProp(AObject,p^.Name,Ord(boolData));
                    End;
                  {$ENDIF}
                  tkClass :
                    Begin
                      objData := GetObjectProp(AObject,p^.Name);
                      objDataCreateHere := not Assigned(objData);
                      try
                        AStore.Get(pt,propName,objData);
                        if objDataCreateHere then
                          SetObjectProp(AObject,p^.Name,objData);
                      finally
                        if objDataCreateHere and ( objData <> GetObjectProp(AObject,p^.Name) ) then
                          FreeAndNil(objData);
                      end;
                    End;
                  tkEnumeration,tkInteger :
                    Begin
                    {$IFDEF WST_DELPHI}
                      if ( pt^.Kind = tkEnumeration ) and
                         ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                      then begin
                        AStore.Get(pt,propName,boolData);
                        SetPropValue(AObject,p^.Name,boolData);
                      end else begin
                    {$ENDIF}
                        FillChar(enumData,SizeOf(enumData),#0);
                        Case GetTypeData(pt)^.OrdType Of
                          otSByte :
                            Begin
                              AStore.Get(pt,propName,enumData.ShortIntData);
                              int64Data := enumData.ShortIntData;
                            End;
                          otUByte :
                            Begin
                              AStore.Get(pt,propName,enumData.ByteData);
                              int64Data := enumData.ByteData;
                            End;
                          otSWord :
                            Begin
                              AStore.Get(pt,propName,enumData.SmallIntData);
                              int64Data := enumData.SmallIntData;
                            End;
                          otUWord :
                            Begin
                              AStore.Get(pt,propName,enumData.WordData);
                              int64Data := enumData.WordData;
                            End;
                          otSLong:
                            Begin
                              AStore.Get(pt,propName,enumData.SLongIntData);
                              int64Data := enumData.SLongIntData;
                            End;
                          otULong :
                            Begin
                              AStore.Get(pt,propName,enumData.ULongIntData);
                              int64Data := enumData.ULongIntData;
                            End;
                        End;
                        SetOrdProp(AObject,p^.Name,int64Data);
                    {$IFDEF WST_DELPHI}
                      end;
                    {$ENDIF}
                    End;
                  tkFloat :
                    Begin
                      FillChar(floatDt,SizeOf(floatBuffer),#0);
                      Case GetTypeData(pt)^.FloatType Of
                        ftSingle :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.SingleData);
                            floatDt := floatBuffer.SingleData;
                          End;
                        ftDouble :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.DoubleData);
                            floatDt := floatBuffer.DoubleData;
                          End;
                        ftExtended :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.ExtendedData);
                            floatDt := floatBuffer.ExtendedData;
                          End;
                        ftCurr :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CurrencyData);
                            floatDt := floatBuffer.CurrencyData;
                          End;
                        ftComp :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CompData);
                            floatDt := floatBuffer.CompData;
                          End;
                      End;
                      SetFloatProp(AObject,p^.Name,floatDt);
                    End;
                End;
              except
                on E : EServiceException do begin
                  if ( persistType = pstAlways ) then
                    raise;
                end;
              end;
            End;
          End;
        Finally
          Freemem(propList,propListLen*SizeOf(Pointer));
        End;
      End;
    finally
      AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;
{$ENDIF USE_SERIALIZE}

{ TBaseObjectArrayRemotable }

function TBaseObjectArrayRemotable.GetItem(AIndex: Integer): TBaseRemotable;
begin
  CheckIndex(AIndex);
  Result := FArray[AIndex];
end;

function TBaseObjectArrayRemotable.GetLength(): Integer;
begin
  Result := System.Length(FArray);
end;

class procedure TBaseObjectArrayRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : String;
  const ATypeInfo  : PTypeInfo
);
Var
  itmTypInfo : PTypeInfo;
  i, arrayLen : Integer;
  nativObj : TBaseObjectArrayRemotable;
  itm : TObject;
  itmName : string;
  styl : TArrayStyle;
begin
  if ( AObject <> nil ) then begin
    Assert(AObject.InheritsFrom(TBaseObjectArrayRemotable));
    nativObj := AObject as TBaseObjectArrayRemotable;
    arrayLen := nativObj.Length;
    styl := GetStyle();
    if ( arrayLen > 0 ) then begin
      itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
      AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),itmTypInfo,[0,Pred(arrayLen)],styl);
      try
        if ( styl = asScoped ) then begin
          itmName := GetItemName();
        end else begin
          itmName := AName;
        end;
        for i := 0 to Pred(arrayLen) do begin
          itm := nativObj.Item[i];
          AStore.Put(itmName,itmTypInfo,itm);
        end;
      finally
        AStore.EndScope();
      end;
    end else if ( styl = asScoped ) and ( ioAlwaysSerialize in nativObj.Options ) then begin
      AStore.BeginArray(
        AName, PTypeInfo(Self.ClassInfo),
        PTypeInfo(GetItemClass().ClassInfo),[0,-1],styl
      );
      try
        AStore.NilCurrentScope();
      finally
        AStore.EndScope();
      end;
    end;
  end;
end;

class procedure TBaseObjectArrayRemotable.Load(
  var   AObject   : TObject;
        AStore    : IFormatterBase;
  var   AName     : String;
  const ATypeInfo : PTypeInfo
);
Var
  i, len : Integer;
  nativObj : TBaseObjectArrayRemotable;
  s : string;
  itmTypInfo : PTypeInfo;
  itm : TBaseRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  styl := GetStyle();
  if ( styl = asScoped ) then begin
    itmName := GetItemName();
  end else begin
    itmName := AName;
  end;
  if (AObject = nil) then
    AObject := Create();
  len := AStore.BeginArrayRead(AName,ATypeInfo,styl,itmName);
  if ( len >= 0 ) then begin
    Try
      itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
      nativObj := AObject as TBaseObjectArrayRemotable;
      If ( len > 0 ) Then Begin
        s := '';
        nativObj.SetLength(len);
        For i := 0 To Pred(len) Do Begin
          itm := nativObj[i];
          AStore.Get(itmTypInfo,s,itm);
        End;
      End;
    Finally
      AStore.EndScopeRead();
    End;
  end else begin
    if ( AObject <> nil ) then
      (AObject as TBaseObjectArrayRemotable).SetLength(0);
  end;
end;

class function TBaseObjectArrayRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result:= GetItemClass().ClassInfo;
end;

constructor TBaseObjectArrayRemotable.Create();
begin
  FArray := Nil;
end;

procedure TBaseObjectArrayRemotable.Assign(Source: TPersistent);
var
  src : TBaseObjectArrayRemotable;
  i, c : Integer;
begin
  if Assigned(Source) then begin
    if Source.InheritsFrom(TBaseObjectArrayRemotable) then begin
      src := TBaseObjectArrayRemotable(Source);
      c := src.Length;
      SetLength(c);
      for i := 0 to Pred(c) do begin
        Item[i].Assign(src.Item[i]);
      end;
    end else begin
      inherited Assign(Source);
    end;
  end else begin
    SetLength(0);
  end;
end;

function TBaseObjectArrayRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
var
  i, c : Integer;
  dst : TBaseObjectArrayRemotable;
begin
  if ( Self = ACompareTo ) then begin
    Result := True;
  end else begin
    Result := ( Assigned(ACompareTo) and
                ACompareTo.InheritsFrom(TBaseObjectArrayRemotable) and
                ( Self.Length = TBaseObjectArrayRemotable(ACompareTo).Length ) and
                ( TBaseObjectArrayRemotable(ACompareTo).GetItemClass().InheritsFrom(Self.GetItemClass()) )
              ) ;
    if Result and ( Self.Length > 0 ) then begin
      dst := TBaseObjectArrayRemotable(ACompareTo);
      c := Self.Length;
      for i := 0 to Pred(c) do begin
        if not Self.Item[i].Equal(dst.Item[i]) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TBaseObjectArrayRemotable.SetLength(const ANewSize: Integer);
var
  i,oldLen : Integer;
  itmClss : TBaseRemotableClass;
begin
  oldLen := GetLength;
  if ( oldLen = ANewSize ) then
    Exit;

  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);

  if ( oldLen > ANewSize ) then begin
    for i := ANewSize to Pred(oldLen) do
      FreeAndNil(FArray[i]);
    System.SetLength(FArray,ANewSize);
  end else begin
    System.SetLength(FArray,ANewSize);
    itmClss := GetItemClass();
    for i := oldLen to Pred(ANewSize) do
      FArray[i] := itmClss.Create();
  end;
end;

procedure TBaseObjectArrayRemotable.Exchange(const Index1, Index2: Integer);
var
  tmp : TBaseRemotable;
begin
  if ( Index1 <> Index2 ) then begin
    CheckIndex(Index1);
    CheckIndex(Index2);
    tmp := FArray[Index1];
    FArray[Index1] := FArray[Index2];
    FArray[Index2] := tmp;
  end;
end;

{ TBaseFactoryRegistryItem }

constructor TBaseFactoryRegistryItem.Create(
  const AName    : string;
  const AFactory : IItemFactory
);
begin
  Assert(Assigned(AFactory));
  FName := AName;
  FFactory := AFactory;
end;

destructor TBaseFactoryRegistryItem.Destroy();
begin
  FName := '';
  FFactory := nil;
  inherited Destroy();
end;

function TBaseFactoryRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBaseFactoryRegistry.GetItem(Index: Integer): TBaseFactoryRegistryItem;
begin
  Result := FList[Index] as TBaseFactoryRegistryItem;
end;

{ TBaseFactoryRegistry }
function TBaseFactoryRegistry.FindFactory(const AName: string): IItemFactory;
Var
  i , c : Integer;
  s : string;
begin
  s := LowerCase(Trim(AName));
  c := Pred(FList.Count);
  For i := 0 To c Do Begin
    If AnsiSameText(TBaseFactoryRegistryItem(FList[i]).Name,s) Then Begin
      Result := TBaseFactoryRegistryItem(FList[i]).Factory;
      Exit;
    End;
  End;
  Result := Nil;
end;

procedure TBaseFactoryRegistry.Register(
  const AName    : string;
        AFactory : IItemFactory
);
begin
  Assert(Assigned(AFactory));
  If Not Assigned(FindFactory(AName)) Then
    FList.Add(TBaseFactoryRegistryItem.Create(AName,AFactory));
end;

constructor TBaseFactoryRegistry.Create();
begin
  inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TBaseFactoryRegistry.Destroy();
begin
  FList.Free();
  inherited Destroy();
end;

{ TSimpleItemFactory }

function TSimpleItemFactory.CreateInstance(): IInterface;
begin
  Result := FItemClass.Create() as IInterface;
end;

function TSimpleItemFactory.GetItemClass(): TSimpleFactoryItemClass;
begin
  Result := FItemClass;
end;

constructor TSimpleItemFactory.Create(AItemClass: TSimpleFactoryItemClass);
begin
  if not Assigned(AItemClass) then
    raise EServiceConfigException.CreateFmt(SERR_InvalidParameterProc,['AItemClass','TSimpleItemFactory.Create()']);
  FItemClass := AItemClass;
end;

{ TSimpleItemFactoryEx }

procedure TSimpleItemFactoryEx.PreparePool();
begin
  if ( FPool = nil ) then begin
    FPool := TIntfPool.Create(PoolMin,PoolMax,TSimpleItemFactory.Create(FItemClass));
  end;
end;

procedure TSimpleItemFactoryEx.SetPooled(const AValue: Boolean);
begin
  if ( FPooled = AValue ) then
    Exit;
  FreeAndNil(FPool);
  if AValue then begin
    if ( PoolMin < 0 ) or ( PoolMin > PoolMax ) or ( PoolMax < 1 ) then
      raise EServiceException.CreateFmt(SERR_InvalidPoolParametersArgs,[PoolMin,PoolMax]);
    PreparePool();
  end;
  FPooled := AValue;
end;

procedure TSimpleItemFactoryEx.SetPoolMax(const AValue: Integer);
begin
  if ( FPoolMax = AValue ) then
    Exit;
  if Pooled then
    raise EServiceException.Create(SERR_OperationNotAllowedOnActivePool);
  FPoolMax := AValue;
end;

procedure TSimpleItemFactoryEx.SetPoolMin(const AValue: Integer);
begin
  if ( FPoolMin = AValue ) then
    Exit;
  if Pooled then
    raise EServiceException.Create(SERR_OperationNotAllowedOnActivePool);
  FPoolMin := AValue;
end;

function TSimpleItemFactoryEx.CreateInstance(): IInterface;
begin
  if Pooled then begin
    Result := FPool.Get(TimeOut);
  end else begin
    Result := inherited CreateInstance();
  end;
end;

procedure TSimpleItemFactoryEx.ReleaseInstance(const AInstance : IInterface);
begin
  if Pooled then begin
    FPool.Release(AInstance);
  end;
end;

procedure TSimpleItemFactoryEx.DiscardInstance(const AInstance : IInterface);
begin
  if Pooled then
    FPool.Discard(AInstance);
end;

function TSimpleItemFactoryEx.GetPropertyManager(
  const APropertyGroup : string;
  const ACreateIfNotExists : Boolean
):IPropertyManager;
var
  i : Integer;
  s : string;
begin
  Result := nil;
  s := Trim(APropertyGroup);
  i := FPropertyNames.IndexOf(s);
  if ( i < 0 ) then begin
    if not ACreateIfNotExists then
      Exit;
    i := FPropertyNames.Add(s);
    if ( s = '' ) then
      FProperties.Add(TPublishedPropertyManager.Create(Self))
    else
      FProperties.Add(TStoredPropertyManager.Create());
  end;
  Result := FProperties.Get(i) as IPropertyManager;
end;

constructor TSimpleItemFactoryEx.Create(
  AItemClass         : TSimpleFactoryItemClass;
  const APropsString : string
);
begin
  inherited Create(AItemClass);
  FPropertyNames := TStringList.Create();
  FProperties := TInterfaceList.Create();
  if ( Length(APropsString) > 0 ) then begin
    GetPropertyManager('',True).SetProperties(APropsString);
  end;
end;

constructor TSimpleItemFactoryEx.Create(AItemClass: TSimpleFactoryItemClass);
begin
  Create(AItemClass,'');
end;

destructor TSimpleItemFactoryEx.Destroy();
begin
  FreeAndNil(FPropertyNames);
  FProperties := nil;
  FreeAndNil(FPool);
  inherited Destroy();
end;

{ TSimpleFactoryItem }

constructor TSimpleFactoryItem.Create();
begin
end;


{ TSimpleCallContext }

procedure TSimpleCallContext.Clear();
begin
  FHeaderList.Clear();
  FFreeObjectList.Clear();;
end;

procedure TSimpleCallContext.AddObjectToFree(const AObject: TObject);
begin
  if ( FFreeObjectList.IndexOf(AObject) < 0 ) then
    FFreeObjectList.Add(AObject);
end;

function TSimpleCallContext.AddHeader(
  const AHeader: THeaderBlock;
  const AKeepOwnership: Boolean
): Integer;
begin
  Result := FHeaderList.IndexOf(AHeader);
  if ( Result = -1 ) then
    Result := FHeaderList.Add(AHeader);
  if AKeepOwnership then
    AddObjectToFree(AHeader);
end;

function TSimpleCallContext.AddHeader(
  const AHeader        : TBaseRemotable;
  const AKeepOwnership : Boolean;
  const AName          : string = ''
) : Integer;
var
  locProxy : THeaderBlockProxy;
begin
  if ( AHeader <> nil ) then begin
    if AHeader.InheritsFrom(THeaderBlock) then begin
      if not IsStrEmpty(AName) then
        THeaderBlock(AHeader).Name := AName;
      Result := AddHeader(THeaderBlock(AHeader),AKeepOwnership);
    end else begin
      locProxy := THeaderBlockProxy.Create();
      locProxy.ActualObject := AHeader;
      locProxy.OwnObject := AKeepOwnership;
      if not IsStrEmpty(AName) then
        locProxy.Name := AName;
      Result := AddHeader(locProxy,True);
    end;
  end else begin
    locProxy := THeaderBlockProxy.Create();
    if not IsStrEmpty(AName) then
      locProxy.Name := AName;
    Result := AddHeader(locProxy,True);
  end;
end;

function TSimpleCallContext.GetHeaderCount(const ADirections : THeaderDirections):Integer;
var
  i : Integer;
begin
  if ( ADirections = [Low(THeaderDirection)..High(THeaderDirection)] ) then
    Result := FHeaderList.Count
  else begin
    Result := 0;
    for i := 0 to Pred(FHeaderList.Count) do begin
      if ( THeaderBlock(FHeaderList[i]).Direction in ADirections ) then
        Inc(Result);
    end;
  end;
end;

function TSimpleCallContext.GetHeader(const AIndex: Integer): THeaderBlock;
begin
  Result := FHeaderList[AIndex] as THeaderBlock;
end;

procedure TSimpleCallContext.ClearHeaders(const ADirection: THeaderDirection);
var
  i, c : Integer;
  h : THeaderBlock;
  fl : TObjectList;
begin
  c := FHeaderList.Count;
  if ( c > 0 ) then begin
    fl := TObjectList.Create(False);
    try
      for i := 0 to Pred(c) do begin
        h := FHeaderList[i] as THeaderBlock;
        if ( h.Direction = ADirection ) then
          fl.Add(h);
      end;
      for i := 0 to Pred(fl.Count) do
        FreeHeader(fl[i] as THeaderBlock);
    finally
      fl.Free();
    end;
  end;
end;

procedure TSimpleCallContext.FreeHeader(AHeader: THeaderBlock);
begin
  if Assigned(AHeader) then begin
    if ( FHeaderList.IndexOf(AHeader) >= 0 ) then
      FHeaderList.Remove(AHeader);
    if ( FFreeObjectList.IndexOf(AHeader) >= 0 ) then
      FHeaderList.Remove(AHeader)
    else
      AHeader.Free();
  end;
end;

function TSimpleCallContext.GetPropertyManager(): IPropertyManager;
begin
  Result := FPropertyManager;
end;

constructor TSimpleCallContext.Create();
begin
  FHeaderList := TObjectList.Create(False);
  FFreeObjectList := TObjectList.Create(True);
  FPropertyManager := TStoredPropertyManager.Create();
end;

destructor TSimpleCallContext.Destroy();
begin
  FreeAndNil(FHeaderList);
  FreeAndNil(FFreeObjectList);
  inherited Destroy();
end;

{ TTypeRegistryItem }

procedure TTypeRegistryItem.SetOptions(AValue: TTypeRegistryItemOptions);
begin
  if (FOptions = AValue) then
    Exit;
  FOptions := AValue;
  Init();
end;

procedure TTypeRegistryItem.Init();
begin

end;

function TTypeRegistryItem.IndexOfProp(
  const AName: string;
  const ANameType : TPropertyNameType
) : Integer;
var
  i : Integer;
  locName : string;
begin
  Result := -1;
  if ( FProperties <> nil ) and ( FProperties.Count > 0 ) then begin
    locName := LowerCase(AName);
    if ( ANameType = pntInternalName ) then begin
      for i := 0 to Pred(FProperties.Count) do begin
        if ( locName = LowerCase(TPropertyItem(FProperties[i]).InternalName) ) then begin
          Result := i;
          Break;
        end;
      end;
    end else begin
      for i := 0 to Pred(FProperties.Count) do begin
        if ( locName = LowerCase(TPropertyItem(FProperties[i]).ExternalName) ) then begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TTypeRegistryItem.FindProperty(
  const AName: string;
  const ANameType : TPropertyNameType
) : TPropertyItem;
var
  i : Integer;
begin
  i := IndexOfProp(AName,ANameType);
  if ( i = -1 ) then
    Result := nil
  else
    Result := TPropertyItem(FProperties[i]);
end;

constructor TTypeRegistryItem.Create(AOwner: TTypeRegistry; ANameSpace: string;
  ADataType: PTypeInfo; const ADeclaredName: string);
begin
  FOwner := AOwner;
  FNameSpace := ANameSpace;
  FDataType  := ADataType;
  FDeclaredName := Trim(ADeclaredName);
  If ( Length(FDeclaredName) = 0 ) Then
    FDeclaredName := FDataType^.Name;
end;

destructor TTypeRegistryItem.Destroy();
begin
  FreeAndNil(FProperties);
  FPascalSynonyms.Free();
  FExternalSynonyms.Free();
  inherited Destroy();
end;

function TTypeRegistryItem.AddPascalSynonym(const ASynonym: string):TTypeRegistryItem;
begin
  Result := Self;
  if AnsiSameText(ASynonym,DataType^.Name) then
    Exit;
  if not Assigned(FPascalSynonyms) then begin
    FPascalSynonyms := TStringList.Create();
    FPascalSynonyms.Add(FDataType^.Name);
  end;
  if ( FPascalSynonyms.IndexOf(ASynonym) = -1 ) then
    FPascalSynonyms.Add(AnsiLowerCase(ASynonym));
end;

function TTypeRegistryItem.AddExternalSynonym(const ASynonym: string): TTypeRegistryItem;
begin
  Result := Self;
  if AnsiSameText(ASynonym,DataType^.Name) then
    Exit;
  if not Assigned(FExternalSynonyms) then begin
    FExternalSynonyms := TStringList.Create();
    FExternalSynonyms.Add(Self.DeclaredName);
  end;
  if ( FExternalSynonyms.IndexOf(ASynonym) = -1 ) then
    FExternalSynonyms.Add(AnsiLowerCase(ASynonym));
end;

function TTypeRegistryItem.IsSynonym(const APascalTypeName: string): Boolean;
begin
  Result := AnsiSameText(APascalTypeName,DataType^.Name);
  if ( not Result ) and Assigned(FPascalSynonyms) then
    Result := ( FPascalSynonyms.IndexOf(APascalTypeName) >= 0 ) ;
end;

function TTypeRegistryItem.IsExternalSynonym(const AExternalName: string): Boolean;
begin
  Result := AnsiSameText(AExternalName,Self.DeclaredName);
  if ( not Result ) and Assigned(FExternalSynonyms) then
    Result := ( FExternalSynonyms.IndexOf(AExternalName) >= 0 ) ;
end;

procedure TTypeRegistryItem.RegisterExternalPropertyName(const APropName,AExtPropName: string);
var
  i : Integer;
  po : TPropertyItem;
begin
  i := IndexOfProp(APropName,pntInternalName);
  if ( i = -1 ) then begin
    if ( FProperties = nil ) then
      FProperties := TObjectList.Create(True);
    po := TPropertyItem.Create();
    FProperties.Add(po);
    po.FInternalName := APropName;
    //po.FOptions := Self.DefaultPropertyOptions;
  end else begin
    po := TPropertyItem(FProperties[i]);
  end;
  po.FExternalName := AExtPropName;
end;

procedure TTypeRegistryItem.RegisterObject(const APropName : string; const AObject : TObject);
var
  i : Integer;
begin
  i := IndexOfProp(APropName,pntInternalName);
  if ( i = -1 ) then begin
    RegisterExternalPropertyName(APropName,APropName);
    i := IndexOfProp(APropName,pntInternalName);
  end;
  TPropertyItem(FProperties[i]).FExtObject := AObject;
end;

function TTypeRegistryItem.GetObject(const APropName : string) : TObject;
var
  p : TPropertyItem;
begin
  p := FindProperty(APropName,pntInternalName);
  if ( p = nil ) then
    Result := nil
  else
    Result := p.ExtObject;
end;

function TTypeRegistryItem.GetExternalPropertyName(const APropName: string): string;
var
  p : TPropertyItem;
begin
  p := FindProperty(APropName,pntInternalName);
  if ( p = nil ) then
    Result := APropName
  else
    Result := p.ExternalName;
end;

function TTypeRegistryItem.GetInternalPropertyName(const AExtPropName: string): string;
var
  p : TPropertyItem;
begin
  p := FindProperty(AExtPropName,pntExternalName);
  if ( p = nil ) then
    Result := AExtPropName
  else
    Result := p.InternalName;
end;

procedure TTypeRegistryItem.SetPropertyOptions(
  const APropName: string;
  const AOptions: TTypeRegistryItemOptions
);
var
  po : TPropertyItem;
begin
  po := FindProperty(APropName,pntInternalName);
  if ( po = nil ) then begin
    RegisterExternalPropertyName(APropName,APropName);
    po := FindProperty(APropName,pntInternalName);
  end;
  po.FOptions := AOptions;
end;

procedure TTypeRegistryItem.AddOptions(
  const AOptions: TTypeRegistryItemOptions
);
begin
  Options := Options + AOptions;
end;

{ TTypeRegistry }

function TTypeRegistry.GetItemClassFor(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;
var
  i, c : Integer;
  locInitializer : TRemotableTypeInitializerClass;
begin
  Result := TTypeRegistryItem;
  c := FInitializerList.Count;
  if ( c > 0 ) then begin
    for i := Pred(c) downto 0 do begin
      locInitializer := TRemotableTypeInitializerClass(FInitializerList[i]);
      if locInitializer.CanHandle(ATypeInfo) then begin
        Result := locInitializer.GetItemClass(ATypeInfo);
        Break;
      end;
    end;
  end;
end;

{$IFDEF TRemotableTypeInitializer_Initialize}
procedure TTypeRegistry.InitializeItem(AItem : TTypeRegistryItem);
var
  i, c : Integer;
  locInitializer : TRemotableTypeInitializerClass;
begin
  c := FInitializerList.Count;
  if ( c > 0 ) then begin
    for i := Pred(c) downto 0 do begin
      locInitializer := TRemotableTypeInitializerClass(FInitializerList[i]);
      if locInitializer.CanHandle(AItem.DataType) and locInitializer.Initialize(AItem.DataType,AItem) then
        Break;
    end;
  end;
end;
{$ENDIF TRemotableTypeInitializer_Initialize}

function TTypeRegistry.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTypeRegistry.GetItemByIndex(Index: Integer): TTypeRegistryItem;
begin
  Result := FList[Index] as TTypeRegistryItem;
end;

function TTypeRegistry.GetItemByTypeInfo(Index: PTypeInfo): TTypeRegistryItem;
Var
  i : Integer;
begin
  Assert(Assigned(Index));
  i := IndexOf(Index);
  If ( i > -1 ) Then
    Result := FList[i] as TTypeRegistryItem
  Else
    Raise ETypeRegistryException.CreateFmt(SERR_TypeNotRegistered,[Index^.Name])
end;

constructor TTypeRegistry.Create();
begin
  Inherited Create();
  FList := TObjectList.Create(True);
  FInitializerList := TClassList.Create();
end;

destructor TTypeRegistry.Destroy();
begin
  FInitializerList.Free();
  FList.Free();
  inherited Destroy();
end;

procedure TTypeRegistry.RegisterInitializer(AInitializer : TRemotableTypeInitializerClass);
begin
  if ( FInitializerList.IndexOf(AInitializer) = -1 ) then
    FInitializerList.Add(AInitializer);
end;

function TTypeRegistry.IndexOf(const ATypeInfo: PTypeInfo): Integer;
var
  i : Integer;
begin
  for i := 0 to Pred(Count) do begin
    if ( ATypeInfo = Item[i].DataType ) then begin
      Result := i;
      Exit;
    end;
    {If ( ATypeInfo^.Kind = Item[Result].DataType^.Kind ) And
       AnsiSameText(ATypeInfo^.Name,Item[Result].DataType^.Name)
    Then
      Exit;}
  end;
  Result := -1;
end;

function TTypeRegistry.Add(AItem: TTypeRegistryItem): Integer;
begin
  Result := IndexOf(AItem.DataType);
  If ( Result = -1 ) Then
    Result := FList.Add(AItem)
  Else
    Raise ETypeRegistryException.CreateFmt(SERR_TypeNotRegistered,[AItem.DataType^.Name]);
end;

function TTypeRegistry.Register(
  const ANameSpace    : string;
  const ADataType     : PTypeInfo;
  const ADeclaredName : string;
  const AOptions      : TTypeRegistryItemOptions
): TTypeRegistryItem;
var
  i : Integer;
begin
  i := IndexOf(ADataType);
  if ( i = -1 ) then begin
    Result := GetItemClassFor(ADataType).Create(Self,ANameSpace,ADataType,ADeclaredName);
    Add(Result);
    Result.FOptions := Result.FOptions + AOptions;
    Result.Init();
{$IFDEF TRemotableTypeInitializer_Initialize}
    InitializeItem(Result);
{$ENDIF TRemotableTypeInitializer_Initialize}
  end else begin
    Result := Item[i];
  end;
end;

function TTypeRegistry.Register(
  const ANameSpace    : string;
  const ADataType     : PTypeInfo;
  const ADeclaredName : string
) : TTypeRegistryItem;
begin
  Result := Register(ANameSpace,ADataType,ADeclaredName,[]);
end;

function TTypeRegistry.Find(ATypeInfo: PTypeInfo; const AExact: Boolean
  ): TTypeRegistryItem;
Var
  i : Integer;
  searchClass : TClass;
begin
  Result := Nil;
  i := IndexOf(ATypeInfo);
  if ( i > -1 ) then begin
    Result := Item[i]
  end else if ( not AExact ) and Assigned(ATypeInfo) and ( ATypeInfo^.Kind = tkClass ) then begin
    searchClass := GetTypeData(ATypeInfo)^.ClassType;
    for i := Pred(Count) downto 0 do begin
      Result := Item[i];
      if ( Result.DataType^.Kind = tkClass ) and
         searchClass.InheritsFrom(GetTypeData(Result.DataType)^.ClassType)
      then begin
        Exit;
      end;
    end;
    Result := Nil;
  end;
end;

function TTypeRegistry.Find(const APascalTypeName: string): TTypeRegistryItem;
var
  i,c : Integer;
begin
  c := Count;
  for i := 0 to Pred(c) do begin
    Result := Item[i];
    if Result.IsSynonym(APascalTypeName) then
      Exit;
  end;
  Result := nil;
end;

function TTypeRegistry.FindByDeclaredName(
  const ATypeName,
        ANameSpace : string;
  const AOptions   : TTypeRegistrySearchOptions
): TTypeRegistryItem;
var
  i, c : Integer;
begin
{ The external synonym is not tested in the first loop so that the declared
  names are _first_ search for.
}
  c := Count;
  if ( c > 0 ) then begin
    for i := 0 to Pred(c) do begin
      Result := Item[i];
      if AnsiSameText(ANameSpace,Result.NameSpace) and
         AnsiSameText(ATypeName,Result.DeclaredName)
      then
        Exit;
    end;
    if ( trsoIncludeExternalSynonyms in AOptions ) then begin
      for i := 0 to Pred(c) do begin
        Result := Item[i];
        if AnsiSameText(ANameSpace,Result.NameSpace) and
           Result.IsExternalSynonym(ATypeName)
        then
          Exit;
      end;
    end;
  end;
  Result := nil;
end;


{ TBaseSimpleTypeArrayRemotable }


class procedure TBaseSimpleTypeArrayRemotable.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : string;
  const ATypeInfo  : PTypeInfo
);
var
  i, arrayLen : Integer;
  nativObj : TBaseSimpleTypeArrayRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  if Assigned(AObject) then begin
    Assert(AObject.InheritsFrom(TBaseSimpleTypeArrayRemotable));
    nativObj := AObject as TBaseSimpleTypeArrayRemotable;
    arrayLen := nativObj.Length;
    styl := GetStyle();
    if ( arrayLen > 0 ) then begin
      AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),GetItemTypeInfo(),[0,Pred(arrayLen)],styl);
      try
        if ( styl = asScoped ) then begin
          itmName := GetItemName();
        end else begin
          itmName := AName;
        end;
        for i := 0 to Pred(arrayLen) do begin
          nativObj.SaveItem(AStore,itmName,i);
        end;
      finally
        AStore.EndScope();
      end;
    end else if ( styl = asScoped ) and ( ioAlwaysSerialize in nativObj.Options ) then begin
      AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),GetItemTypeInfo(),[0,-1],styl);
      try
        AStore.NilCurrentScope();
      finally
        AStore.EndScope();
      end;
    end;
  end;
end;

class procedure TBaseSimpleTypeArrayRemotable.Load(
  var   AObject     : TObject;
        AStore      : IFormatterBase;
  var   AName       : String;
  const ATypeInfo   : PTypeInfo
);
Var
  i, len : Integer;
  nativObj : TBaseSimpleTypeArrayRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  styl := GetStyle();
  if ( styl = asScoped ) then begin
    itmName := GetItemName();
  end else begin
    itmName := AName;
  end;
  if (AObject = nil) and Self.InheritsFrom(TBaseArrayRemotable) then
    AObject := Create();
  len := AStore.BeginArrayRead(AName,ATypeInfo, GetStyle(),itmName);
  if ( len > 0 ) then begin
    try
      if not Assigned(AObject) then
        AObject := Create();
      nativObj := AObject as TBaseSimpleTypeArrayRemotable;
      if ( len >= 0 ) then begin
        nativObj.SetLength(len);
        for i := 0 to Pred(len) do begin
          nativObj.LoadItem(AStore,i);
        end;
      end;
    finally
      AStore.EndScopeRead();
    end;
  end else begin
    if ( AObject <> nil ) then
      TBaseSimpleTypeArrayRemotable(AObject).SetLength(0);
  end;
end;

{ TArrayOfStringRemotable }

function TArrayOfStringRemotable.GetItem(AIndex: Integer): String;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfStringRemotable.SetItem(AIndex: Integer;const AValue: String);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfStringRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData)
end;

procedure TArrayOfStringRemotable.SaveItem(
        AStore : IFormatterBase;
  const AName  : String;
  const AIndex : Integer
);
begin
  AStore.Put(AName,TypeInfo(String),FData[AIndex]);
end;

procedure TArrayOfStringRemotable.LoadItem(
        AStore : IFormatterBase;
  const AIndex : Integer
);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(String),sName,FData[AIndex]);
end;

class function TArrayOfStringRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(String);
end;

procedure TArrayOfStringRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfStringRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfStringRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfStringRemotable) then begin
    src := TArrayOfStringRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TArrayOfStringRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
var
  i, c : Integer;
  dst : TArrayOfStringRemotable;
begin
  if ( Self = ACompareTo ) then begin
    Result := True;
  end else begin
    Result := Assigned(ACompareTo) and
              ACompareTo.InheritsFrom(TArrayOfStringRemotable) and
              ( Self.Length = TArrayOfStringRemotable(ACompareTo).Length );
    if Result then begin
      c := Self.Length;
      dst := TArrayOfStringRemotable(ACompareTo);
      for i := 0 to Pred(c) do begin
        if ( Self.Item[i] <> dst.Item[i] ) then begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;
end;

{ TObjectCollectionRemotable }

function TObjectCollectionRemotable.GetItem(AIndex : Integer) : TBaseRemotable;
begin
  Result := TBaseRemotable(FList[AIndex]);
end;

function TObjectCollectionRemotable.GetLength() : Integer;
begin
  Result := FList.Count;
end;

class procedure TObjectCollectionRemotable.Save(
  AObject : TBaseRemotable;
  AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
Var
  itmTypInfo : PTypeInfo;
  i, arrayLen : Integer;
  nativObj : TObjectCollectionRemotable;
  itm : TObject;
  itmName : string;
  styl : TArrayStyle;
begin
  if Assigned(AObject) then begin
    Assert(AObject.InheritsFrom(TObjectCollectionRemotable));
    nativObj := AObject as TObjectCollectionRemotable;
    styl := GetStyle();
    itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
    arrayLen := nativObj.Length;
    if ( arrayLen > 0 ) then begin
      AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),itmTypInfo,[0,Pred(arrayLen)],styl);
      try
        if ( styl = asScoped ) then begin
          itmName := GetItemName();
        end else begin
          itmName := AName;
        end;
        for i := 0 to Pred(arrayLen) do begin
          itm := nativObj.Item[i];
          AStore.Put(itmName,itmTypInfo,itm);
        end;
      finally
        AStore.EndScope();
      end;
    end else if ( styl = asScoped ) and ( ioAlwaysSerialize in nativObj.Options ) then begin
      AStore.BeginArray(AName,PTypeInfo(Self.ClassInfo),itmTypInfo,[0,-1],styl);
      try
        AStore.NilCurrentScope();
      finally
        AStore.EndScope();
      end;
    end;
  end;
end;

class procedure TObjectCollectionRemotable.Load(
  var   AObject   : TObject;
        AStore    : IFormatterBase;
  var   AName     : String;
  const ATypeInfo : PTypeInfo
);
Var
  i, len : Integer;
  nativObj : TObjectCollectionRemotable;
  s : string;
  itmTypInfo : PTypeInfo;
  itm : TBaseRemotable;
  itmName : string;
  styl : TArrayStyle;
begin
  styl := GetStyle();
  if ( styl = asScoped ) then begin
    itmName := GetItemName();
  end else begin
    itmName := AName;
  end;
  len := AStore.BeginArrayRead(AName,ATypeInfo,styl,itmName);
  if ( len >= 0 ) then begin
    Try
      If Not Assigned(AObject) Then
        AObject := Create();
      itmTypInfo := PTypeInfo(GetItemClass().ClassInfo);
      nativObj := AObject as TObjectCollectionRemotable;
      If ( len > 0 ) Then Begin
        s := '';
        nativObj.Clear();
        For i := 0 To Pred(len) Do Begin
          itm := nativObj.Add();
          AStore.Get(itmTypInfo,s,itm);
        End;
      End;
    Finally
      AStore.EndScopeRead();
    End;
  end else begin
    if ( AObject <> nil ) then
      TObjectCollectionRemotable(AObject).Clear();
  end;
end;

class function TObjectCollectionRemotable.GetItemTypeInfo() : PTypeInfo;
begin
  Result := PTypeInfo(GetItemClass().ClassInfo);
end;

constructor TObjectCollectionRemotable.Create();
begin
  inherited Create();
  FList := TObjectList.Create(True);
end;

destructor TObjectCollectionRemotable.Destroy();
begin
  FreeAndNil(FList);
  inherited Destroy();
end;

procedure TObjectCollectionRemotable.Assign(Source : TPersistent);
var
  srcCol : TObjectCollectionRemotable;
  src : TBaseObjectArrayRemotable;
  i, c : Integer;
begin
  if Assigned(Source) then begin
    if Source.InheritsFrom(TObjectCollectionRemotable) then begin
      srcCol := TObjectCollectionRemotable(Source);
      c := srcCol.Length;
      FList.Clear();
      FList.Capacity := c;
      for i := 0 to Pred(c) do begin
        Add().Assign(srcCol.Item[i]);
      end;
    end else if Source.InheritsFrom(TBaseObjectArrayRemotable) then begin
      src := TBaseObjectArrayRemotable(Source);
      c := src.Length;
      FList.Clear();
      FList.Capacity := c;
      for i := 0 to Pred(c) do begin
        Add().Assign(src.Item[i]);
      end;
    end else begin
      inherited Assign(Source);
    end;
  end else begin
    FList.Clear();
  end;
end;

function TObjectCollectionRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
var
  i : Integer;
  nativeCol : TObjectCollectionRemotable;
  nativeArray : TBaseObjectArrayRemotable;
  res : Boolean;
begin
  res := False;
  if ( ACompareTo <> nil ) then begin
    if ACompareTo.InheritsFrom(TObjectCollectionRemotable) then begin
      nativeCol := TObjectCollectionRemotable(ACompareTo);
      if ( nativeCol.Length = Length ) then begin
        res := True;
        for i := 0 to Pred(Length) do begin
          if not Item[i].Equal(nativeCol[i]) then begin
            res := False;
            Break;
          end;
        end;
      end;
    end else if ACompareTo.InheritsFrom(TBaseObjectArrayRemotable) then begin
      nativeArray := TBaseObjectArrayRemotable(ACompareTo);
      if ( nativeArray.Length = Length ) then begin
        res := True;
        for i := 0 to Pred(Length) do begin
          if not Item[i].Equal(nativeArray[i]) then begin
            res := False;
            Break;
          end;
        end;
      end;
    end;
  end;
  Result := res;
end;

procedure TObjectCollectionRemotable.SetLength(const ANewSize: Integer);
var
  i,oldLen : Integer;
begin
  if ( FList = nil ) then
    Exit;
  oldLen := FList.Count;
  if ( oldLen = ANewSize ) then
    Exit;

  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidCollectionLength,[ANewSize]);

  if ( oldLen > ANewSize ) then begin
    for i := ANewSize to Pred(oldLen) do
      FList.Delete(FList.Count - 1);
  end else begin
    if ( FList.Capacity < ANewSize ) then
      FList.Capacity := ANewSize;
    for i := oldLen to Pred(ANewSize) do
      Add();
  end;
end;

function TObjectCollectionRemotable.Add() : TBaseRemotable;
begin
  Result := GetItemClass().Create();
  try
    FList.Add(Result);
  except
    Result.Free();
    raise;
  end;
end;

function TObjectCollectionRemotable.AddAt(const APosition : Integer) : TBaseRemotable;
begin
  FList.Insert(APosition,nil);
  try
    Result := GetItemClass().Create();
  except
    FList.Delete(APosition);
    raise;
  end;
  FList[APosition] := Result;
end;

function TObjectCollectionRemotable.Extract(const AIndex : Integer) : TBaseRemotable;
begin
  Result := TBaseRemotable(FList.Extract(FList[AIndex]));
end;

procedure TObjectCollectionRemotable.Delete(const AIndex : Integer);
begin
  FList.Delete(AIndex);
end;

procedure TObjectCollectionRemotable.Exchange(const Index1, Index2 : Integer);
begin
  FList.Exchange(Index1,Index2);
end;

procedure TObjectCollectionRemotable.Clear();
begin
  FList.Clear();
end;

function TObjectCollectionRemotable.IndexOf(AObject : TBaseRemotable) : Integer;
begin
  Result := FList.IndexOf(AObject);
end;

{ TBaseArrayRemotable }

class function TBaseArrayRemotable.GetItemName(): string;
var
  tri : TTypeRegistryItem;
begin
  tri := GetTypeRegistry().Find(PTypeInfo(Self.ClassInfo),False);
  if Assigned(tri) then
    Result := Trim(tri.GetExternalPropertyName(sARRAY_ITEM));
  if ( System.Length(Result) = 0 ) then
    Result := sARRAY_ITEM;
end;

class function TBaseArrayRemotable.GetStyle(): TArrayStyle;
var
  tri : TTypeRegistryItem;
begin
  tri := GetTypeRegistry().Find(PTypeInfo(Self.ClassInfo),False);
  if Assigned(tri) and AnsiSameText(sEmbedded,Trim(tri.GetExternalPropertyName(sARRAY_STYLE))) then begin
    Result := asEmbeded;
  end else begin
    Result := asScoped;
  end;
end;

procedure TBaseArrayRemotable.CheckIndex(const AIndex : Integer);
begin
  if ( AIndex < 0 ) or ( AIndex >= Length ) then
    raise EServiceException.CreateFmt(SERR_IndexOutOfBound,[AIndex]);
end;

destructor TBaseArrayRemotable.Destroy();
begin
  SetLength(0);
  inherited Destroy();
end;

{ TArrayOfBooleanRemotable }

function TArrayOfBooleanRemotable.GetItem(AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfBooleanRemotable.SetItem(AIndex: Integer;const AValue: Boolean);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfBooleanRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfBooleanRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Boolean),FData[AIndex]);
end;

procedure TArrayOfBooleanRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Boolean),sName,FData[AIndex]);
end;

class function TArrayOfBooleanRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Boolean);
end;

procedure TArrayOfBooleanRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfBooleanRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfBooleanRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfBooleanRemotable) then begin
    src := TArrayOfBooleanRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt8URemotable }

function TArrayOfInt8URemotable.GetItem(AIndex: Integer): Byte;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt8URemotable.SetItem(AIndex: Integer; const AValue: Byte);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt8URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt8URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Byte),FData[AIndex]);
end;

procedure TArrayOfInt8URemotable.LoadItem(AStore: IFormatterBase; const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Byte),sName,FData[AIndex]);
end;

class function TArrayOfInt8URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Byte);
end;

procedure TArrayOfInt8URemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt8URemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt8URemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt8URemotable) then begin
    src := TArrayOfInt8URemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt8SRemotable }

function TArrayOfInt8SRemotable.GetItem(AIndex: Integer): ShortInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt8SRemotable.SetItem(AIndex: Integer; const AValue: ShortInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt8SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt8SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(ShortInt),FData[AIndex]);
end;

procedure TArrayOfInt8SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(ShortInt),sName,FData[AIndex]);
end;

class function TArrayOfInt8SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(ShortInt);
end;

procedure TArrayOfInt8SRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt8SRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt8SRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt8SRemotable) then begin
    src := TArrayOfInt8SRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt16SRemotable }

function TArrayOfInt16SRemotable.GetItem(AIndex: Integer): SmallInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt16SRemotable.SetItem(AIndex: Integer;const AValue: SmallInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt16SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt16SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(SmallInt),FData[AIndex]);
end;

procedure TArrayOfInt16SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(SmallInt),sName,FData[AIndex]);
end;

class function TArrayOfInt16SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(SmallInt);
end;

procedure TArrayOfInt16SRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt16SRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt16SRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt16SRemotable) then begin
    src := TArrayOfInt16SRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt16URemotable }

function TArrayOfInt16URemotable.GetItem(AIndex: Integer): Word;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt16URemotable.SetItem(AIndex: Integer; const AValue: Word);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt16URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt16URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Word),FData[AIndex]);
end;

procedure TArrayOfInt16URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Word),sName,FData[AIndex]);
end;

class function TArrayOfInt16URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Word);
end;

procedure TArrayOfInt16URemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt16URemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt16URemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt16URemotable) then begin
    src := TArrayOfInt16URemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt32URemotable }

function TArrayOfInt32URemotable.GetItem(AIndex: Integer): LongWord;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt32URemotable.SetItem(AIndex: Integer;const AValue: LongWord);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt32URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt32URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(LongWord),FData[AIndex]);
end;

procedure TArrayOfInt32URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(LongWord),sName,FData[AIndex]);
end;

class function TArrayOfInt32URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongWord);
end;

procedure TArrayOfInt32URemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt32URemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt32URemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt32URemotable) then begin
    src := TArrayOfInt32URemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt32SRemotable }

function TArrayOfInt32SRemotable.GetItem(AIndex: Integer): LongInt;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt32SRemotable.SetItem(AIndex: Integer; const AValue: LongInt);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt32SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt32SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(LongInt),FData[AIndex]);
end;

procedure TArrayOfInt32SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(LongInt),sName,FData[AIndex]);
end;

class function TArrayOfInt32SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(LongInt);
end;

procedure TArrayOfInt32SRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt32SRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt32SRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt32SRemotable) then begin
    src := TArrayOfInt32SRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt64SRemotable }

function TArrayOfInt64SRemotable.GetItem(AIndex: Integer): Int64;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt64SRemotable.SetItem(AIndex: Integer; const AValue: Int64);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt64SRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt64SRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Int64),FData[AIndex]);
end;

procedure TArrayOfInt64SRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Int64),sName,FData[AIndex]);
end;

class function TArrayOfInt64SRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Int64);
end;

procedure TArrayOfInt64SRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt64SRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt64SRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt64SRemotable) then begin
    src := TArrayOfInt64SRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfInt64URemotable }

function TArrayOfInt64URemotable.GetItem(AIndex: Integer): QWord;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfInt64URemotable.SetItem(AIndex: Integer; const AValue: QWord);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfInt64URemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfInt64URemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(QWord),FData[AIndex]);
end;

procedure TArrayOfInt64URemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(QWord),sName,FData[AIndex]);
end;

class function TArrayOfInt64URemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(QWord);
end;

procedure TArrayOfInt64URemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfInt64URemotable.Assign(Source: TPersistent);
var
  src : TArrayOfInt64URemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfInt64URemotable) then begin
    src := TArrayOfInt64URemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfFloatSingleRemotable }

function TArrayOfFloatSingleRemotable.GetItem(AIndex: Integer): Single;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatSingleRemotable.SetItem(AIndex: Integer;const AValue: Single);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatSingleRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatSingleRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Single),FData[AIndex]);
end;

procedure TArrayOfFloatSingleRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Single),sName,FData[AIndex]);
end;

class function TArrayOfFloatSingleRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Single);
end;

procedure TArrayOfFloatSingleRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfFloatSingleRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfFloatSingleRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfFloatSingleRemotable) then begin
    src := TArrayOfFloatSingleRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfFloatDoubleRemotable }

function TArrayOfFloatDoubleRemotable.GetItem(AIndex: Integer): Double;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatDoubleRemotable.SetItem(AIndex: Integer;const AValue: Double);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatDoubleRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatDoubleRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Double),FData[AIndex]);
end;

procedure TArrayOfFloatDoubleRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Double),sName,FData[AIndex]);
end;

class function TArrayOfFloatDoubleRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Double);
end;

procedure TArrayOfFloatDoubleRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfFloatDoubleRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfFloatDoubleRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfFloatDoubleRemotable) then begin
    src := TArrayOfFloatDoubleRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfFloatExtendedRemotable }

function TArrayOfFloatExtendedRemotable.GetItem(AIndex: Integer): Extended;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatExtendedRemotable.SetItem(AIndex: Integer;const AValue: Extended);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatExtendedRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatExtendedRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Extended),FData[AIndex]);
end;

procedure TArrayOfFloatExtendedRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Extended),sName,FData[AIndex]);
end;

class function TArrayOfFloatExtendedRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Extended);
end;

procedure TArrayOfFloatExtendedRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfFloatExtendedRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfFloatExtendedRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfFloatExtendedRemotable) then begin
    src := TArrayOfFloatExtendedRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TArrayOfFloatCurrencyRemotable }

function TArrayOfFloatCurrencyRemotable.GetItem(AIndex: Integer): Currency;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TArrayOfFloatCurrencyRemotable.SetItem(AIndex: Integer;const AValue: Currency);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TArrayOfFloatCurrencyRemotable.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TArrayOfFloatCurrencyRemotable.SaveItem(AStore: IFormatterBase;
  const AName: String; const AIndex: Integer);
begin
  AStore.Put(AName,TypeInfo(Currency),FData[AIndex]);
end;

procedure TArrayOfFloatCurrencyRemotable.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := GetItemName();
  AStore.Get(TypeInfo(Currency),sName,FData[AIndex]);
end;

class function TArrayOfFloatCurrencyRemotable.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Currency);
end;

procedure TArrayOfFloatCurrencyRemotable.SetLength(const ANewSize: Integer);
begin
  if ( ANewSize < 0 ) then
    raise EBaseRemoteException.CreateFmt(SERR_InvalidArrayLength,[ANewSize]);
  System.SetLength(FData,ANewSize);
end;

procedure TArrayOfFloatCurrencyRemotable.Assign(Source: TPersistent);
var
  src : TArrayOfFloatCurrencyRemotable;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TArrayOfFloatCurrencyRemotable) then begin
    src := TArrayOfFloatCurrencyRemotable(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;


{ THeaderBlock }

function THeaderBlock.HasmustUnderstand: boolean;
begin
  Result := ( FmustUnderstand <> 0 );
end;

function THeaderBlock.GetName : string;
begin
  if IsStrEmpty(FName) then
    FName := GetExternalName(PTypeInfo(Self.ClassInfo));
  Result := FName;
end;

procedure THeaderBlock.SetmustUnderstand(const AValue: Integer);
begin
  if ( AValue <> 0 ) then
    FmustUnderstand := 1
  else
    FmustUnderstand := 0;
end;

procedure THeaderBlock.SetName(const AValue: string);
begin
  FName := AValue;
end;

{ TSimpleContentHeaderBlock }

class procedure TSimpleContentHeaderBlock.Save(
        AObject    : TBaseRemotable;
        AStore     : IFormatterBase;
  const AName      : String;
  const ATypeInfo  : PTypeInfo
);
var
  locSerializer : TObjectSerializer;
  locOptionChanged : Boolean;
begin
  locSerializer := TBaseComplexTypeRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then begin
    locOptionChanged := not ( osoDontDoBeginWrite in locSerializer.Options );
    if locOptionChanged then
      locSerializer.Options := locSerializer.Options + [osoDontDoBeginWrite];
    AStore.BeginObject(AName,ATypeInfo);
    try
      if ( AObject <> nil ) then
        AStore.PutScopeInnerValue(TypeInfo(string),TSimpleContentHeaderBlock(AObject).Value);
      locSerializer.Save(AObject,AStore,AName,ATypeInfo);
    finally
      AStore.EndScope();
      if locOptionChanged then
        locSerializer.Options := locSerializer.Options - [osoDontDoBeginWrite];
    end;
  end else begin
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
  end;
end;

class procedure TSimpleContentHeaderBlock.Load(
  Var   AObject    : TObject;
        AStore     : IFormatterBase;
  var   AName      : String;
  const ATypeInfo  : PTypeInfo
);
var
  locSerializer : TObjectSerializer;
  locStrBuffer : string;
begin
  locSerializer := TBaseComplexTypeRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then begin
    if not ( osoDontDoBeginRead in locSerializer.Options ) then
      locSerializer.Options := locSerializer.Options + [osoDontDoBeginRead];
    if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
      try
        if AStore.IsCurrentScopeNil() then
          Exit; // ???? FreeAndNil(AObject);
        if not Assigned(AObject) then
          AObject := locSerializer.Target.Create();
        locStrBuffer := '';
        AStore.GetScopeInnerValue(TypeInfo(string),locStrBuffer);
        TSimpleContentHeaderBlock(AObject).Value := locStrBuffer;
        locSerializer.Read(AObject,AStore,AName,ATypeInfo);
     finally
       AStore.EndScopeRead();
     end;
    end;
  end else begin
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
  end;
end;

{ THeaderBlockProxy }

procedure THeaderBlockProxy.SetActualObject(const AValue: TBaseRemotable);
var
  locObj : TObject;
begin
  if ( FActualObject <> AValue ) then begin
    if OwnObject and ( FActualObject <> nil ) then begin
      locObj := FActualObject;
      FActualObject := nil;
      locObj.Free();
    end;
    FActualObject := AValue;
  end;
end;

function THeaderBlockProxy.GetName : string;
begin
  if FNameSet then
    Result :=  inherited GetName()
  else if ( ActualObject <> nil ) then
    Result := GetExternalName(PTypeInfo(ActualObject.ClassInfo))
  else
    Result := Self.ClassName();
end;

procedure THeaderBlockProxy.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FNameSet := not IsStrEmpty(AValue);
end;

procedure THeaderBlockProxy.FreeObjectProperties();
begin
  if OwnObject then
    FreeAndNil(FActualObject);
  inherited FreeObjectProperties();
end;

class procedure THeaderBlockProxy.Save(
        AObject   : TBaseRemotable;
        AStore    : IFormatterBase;
  const AName     : string;
  const ATypeInfo : PTypeInfo
);
var
  locObj : THeaderBlockProxy;
begin
  if ( AObject <> nil ) and AObject.InheritsFrom(THeaderBlockProxy) then begin
    locObj := THeaderBlockProxy(AObject);
    if ( locObj.ActualObject <> nil ) then
      locObj.ActualObject.Save(
        locObj.ActualObject,
        AStore,
        AName,
        PTypeInfo(locObj.ActualObject.ClassInfo)
      );
  end;
end;

class procedure THeaderBlockProxy.Load(
  var AObject     : TObject;
      AStore      : IFormatterBase;
  var AName       : string;
  const ATypeInfo : PTypeInfo
);
var
  locObj : THeaderBlockProxy;
  locActualObj : TObject;
begin
  if ( AObject <> nil ) and AObject.InheritsFrom(THeaderBlockProxy) then begin
    locObj := THeaderBlockProxy(AObject);
    if ( locObj.ActualObject <> nil ) then
      locActualObj := locObj.ActualObject;
      locObj.ActualObject.Load(
        locActualObj,
        AStore,
        AName,
        PTypeInfo(locObj.ActualObject.ClassInfo)
      );
      if ( locObj.ActualObject <> locActualObj ) then
        locObj.ActualObject := TBaseRemotable(locActualObj);
  end;
end;

{ TStoredPropertyManager }

procedure TStoredPropertyManager.Error(Const AMsg: string);
begin
  raise EPropertyException.Create(AMsg);
end;

procedure TStoredPropertyManager.Error(
  Const AMsg: string;
  Const AArgs: array of const
);
begin
  raise EPropertyException.CreateFmt(AMsg,AArgs);
end;

procedure TStoredPropertyManager.SetProperty(Const AName, AValue: string);
begin
  FData.Values[AName] := AValue;
end;

procedure TStoredPropertyManager.SetProperties(Const APropsStr: string);
var
  lst : TStringList;
  i : Integer;
begin
  if ( Length(Trim(APropsStr)) = 0 ) then
    Exit;
  lst := TStringList.Create();
  try
    lst.QuoteChar := #0;
    lst.Delimiter := PROP_LIST_DELIMITER;
    lst.DelimitedText := APropsStr;
    for i := 0 to Pred(lst.Count) do
      SetProperty(lst.Names[i],lst.Values[lst.Names[i]]);
  finally
    lst.Free();
  end;
end;

function TStoredPropertyManager.GetProperty(Const AName: String): string;
begin
  Result := FData.Values[AName];
end;

function TStoredPropertyManager.GetPropertyNames(ADest: TStrings): Integer;
var
  i : Integer;
begin
  ADest.Clear();
  Result := FData.Count;
  for i := 0 to Pred(Result) do
    ADest.Add(FData.Names[i]);
end;

procedure TStoredPropertyManager.Clear();
begin
  FData.Clear();
end;

procedure TStoredPropertyManager.Copy(
        ASource      : IPropertyManager;
  Const AClearBefore : Boolean
);
var
  lst : TStringList;
  i : Integer;
  s : string;
begin
  if AClearBefore then
    Clear();
  if Assigned(ASource) then begin
    lst := TStringList.Create();
    try
      ASource.GetPropertyNames(lst);
      for i := 0 to Pred(lst.Count) do begin
        s := lst[i];
        SetProperty(s,ASource.GetProperty(s));
      end;
    finally
      lst.Free();
    end;
  end;
end;

constructor TStoredPropertyManager.Create();
begin
  FData := TStringList.Create();
end;

destructor TStoredPropertyManager.Destroy();
begin
  FreeAndNil(FData);
  inherited Destroy();
end;


{ TAbstractComplexRemotable }

class procedure TAbstractComplexRemotable.RegisterAttributeProperty(const AProperty: shortstring);
var
  ri : TSerializeOptions;
begin
  ri := GetSerializeOptionsRegistry().Find(Self);
  if not Assigned(ri) then
    ri := GetSerializeOptionsRegistry().RegisterClass(Self);
  ri.AddAttributeField(AProperty);
end;

class procedure TAbstractComplexRemotable.RegisterAttributeProperties(const APropertList: array of shortstring);
var
  i : Integer;
begin
  for i := Low(APropertList) to High(APropertList) do
    RegisterAttributeProperty(APropertList[i]);
end;

class function TAbstractComplexRemotable.IsAttributeProperty(const AProperty: shortstring): Boolean;
var
  ri : TSerializeOptions;
  pc : TClass;
  sor : TSerializeOptionsRegistry;
begin
  Result := False;
  if ( Self = TBaseComplexRemotable ) then
    Exit;
  sor := GetSerializeOptionsRegistry();
  pc := Self;
  while Assigned(pc) and pc.InheritsFrom(TBaseComplexRemotable) do begin
    ri := sor.Find(TBaseComplexRemotableClass(pc));
    if Assigned(ri) then begin
      Result := ri.IsAttributeField(AProperty);
      Exit;
    end;
    pc := pc.ClassParent;
  end;
end;

procedure TAbstractComplexRemotable.Assign(Source: TPersistent);
var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  p, sp : PPropInfo;
  selfTypeInfo : PTypeInfo;
  srcObj, dstObj : TObject;
begin
  if not Assigned(Source) then
    Exit;
  selfTypeInfo := Self.ClassInfo;
  propCount := GetTypeData(selfTypeInfo)^.PropCount;
  if ( propCount > 0 ) then begin
    propListLen := GetPropList(selfTypeInfo,propList);
    try
      for i := 0 to Pred(propCount) do begin
        p := propList^[i];
        sp := GetPropInfo(Source,p^.Name);
        if Assigned(sp) and Assigned(sp^.GetProc) and
           Assigned(p^.SetProc)
        then begin
          case p^.PropType^.Kind of
            tkInt64{$IFDEF HAS_QWORD} ,tkQWord{$ENDIF} :
              SetInt64Prop(Self,p,GetInt64Prop(Source,p^.Name));
            {$IFDEF HAS_TKBOOL}tkBool,{$ENDIF} tkEnumeration, tkInteger :
              SetOrdProp(Self,p,GetOrdProp(Source,p^.Name));
            tkLString{$IFDEF FPC}, tkAString{$ENDIF} :
              SetStrProp(Self,p,GetStrProp(Source,p^.Name));
{$IFDEF WST_UNICODESTRING}
            tkUString :
              SetUnicodeStrProp(Self,p,GetUnicodeStrProp(Source,p^.Name));
{$ENDIF WST_UNICODESTRING}
            tkClass :
              begin
                srcObj := GetObjectProp(Source,p^.Name);
                dstObj := GetObjectProp(Self,p^.Name);
                if ( not Assigned(dstObj) ) and
                   ( Assigned(srcObj) and srcObj.InheritsFrom(TAbstractComplexRemotable) )
                then begin
                  dstObj := TAbstractComplexRemotableClass(srcObj.ClassType).Create();
                  SetObjectProp(Self,p,dstObj);
                end;
                if Assigned(dstObj) then begin
                  if ( srcObj = nil ) then begin
                    FreeAndNil(dstObj);
                    SetObjectProp(Self,p,dstObj);
                  end else begin
                    if dstObj.InheritsFrom(TPersistent) and srcObj.InheritsFrom(TPersistent) then
                      TPersistent(dstObj).Assign(TPersistent(srcObj));
                  end;
                end;
              end;
            tkFloat :
              SetFloatProp(Self,p,GetFloatProp(Source,p^.Name));
          end;
        end;
      end;
    finally
      Freemem(propList,propListLen*SizeOf(Pointer));
    end;
  end;
end;

function TAbstractComplexRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  p, sp : PPropInfo;
  selfTypeInfo : PTypeInfo;
  srcObj, dstObj : TObject;
  ok : Boolean;
begin
  Result := False;
  if not Assigned(ACompareTo) then
    Exit;
  if not ACompareTo.InheritsFrom(Self.ClassType) then
    Exit;

  ok := True;
  selfTypeInfo := Self.ClassInfo;
  propCount := GetTypeData(selfTypeInfo)^.PropCount;
  if ( propCount > 0 ) then begin
    propListLen := GetPropList(selfTypeInfo,propList);
    try
      for i := 0 to Pred(propCount) do begin
        p := propList^[i];
        sp := GetPropInfo(Self,p^.Name);
        if Assigned(sp) and Assigned(sp^.GetProc) then begin
          case p^.PropType^.Kind of
            tkInt64{$IFDEF HAS_QWORD} ,tkQWord{$ENDIF} :
              ok := ( GetInt64Prop(Self,p^.Name) = GetInt64Prop(ACompareTo,p^.Name) );
            {$IFDEF HAS_TKBOOL}tkBool,{$ENDIF} tkEnumeration, tkInteger :
              ok := ( GetOrdProp(Self,p^.Name) = GetOrdProp(ACompareTo,p^.Name) );
            {$IFDEF FPC}
            tkAString,
            {$ENDIF FPC}
            {$IFDEF WST_UNICODESTRING}
            tkUString,
            {$ENDIF WST_UNICODESTRING}
            tkLString :
              ok := ( GetStrProp(Self,p^.Name) = GetStrProp(ACompareTo,p^.Name) );
            tkClass :
              begin
                if GetTypeData(p^.PropType{$IFDEF WST_DELPHI}^{$ENDIF})^.ClassType.InheritsFrom(TBaseRemotable) then begin
                  srcObj := GetObjectProp(Self,p^.Name);
                  dstObj := GetObjectProp(ACompareTo,p^.Name);
                  ok := ( Assigned(srcObj) and TBaseRemotable(srcObj).Equal(TBaseRemotable(dstObj)) ) or
                        ( ( srcObj = nil ) and ( dstObj = nil ) ) ;
                end;
              end;
            tkFloat :
              ok := ( GetFloatProp(Self,p^.Name) = GetFloatProp(ACompareTo,p^.Name) );
          end;
          if not ok then
            Break;
        end;
      end;
    finally
      Freemem(propList,propListLen*SizeOf(Pointer));
    end;
  end;
  Result := ok;
end;

{ TBaseComplexSimpleContentRemotable }

class procedure TBaseComplexSimpleContentRemotable.Save(
        AObject: TBaseRemotable;
        AStore: IFormatterBase;
  const AName: string;
  const ATypeInfo: PTypeInfo
);
{$IFDEF USE_SERIALIZE}
var
  locSerializer : TSimpleContentObjectSerializer;
begin
  locSerializer := TSimpleContentObjectRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then
    locSerializer.Save(AObject,AStore,AName,ATypeInfo)
  else
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
end;
{$ELSE USE_SERIALIZE}
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  int64Data : Int64;
  strData : String;
  objData : TObject;
  boolData : Boolean;
  enumData : TEnumBuffer;
  floatDt : TFloatBuffer;
  p : PPropInfo;
  oldSS : TSerializationStyle;
  tr : TTypeRegistry;
  regItem : TTypeRegistryItem;
  propName : string;
begin
  oldSS := AStore.GetSerializationStyle();
  AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(AObject) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    SaveValue(AObject,AStore);
    propCount := GetTypeData(ATypeInfo)^.PropCount;
    if ( propCount > 0 ) then begin
      propListLen := GetPropList(ATypeInfo,propList);
      try
        tr := GetTypeRegistry();
        regItem := tr.ItemByTypeInfo[ATypeInfo];
        AStore.SetSerializationStyle(ssAttibuteSerialization);
        for i := 0 to Pred(propCount) do begin
          p := propList^[i];
          pt := p^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
          propName := regItem.GetExternalPropertyName(p^.Name);
          if IsStoredProp(AObject,p) then begin
            case pt^.Kind of
              tkInt64{$IFDEF HAS_QWORD},tkQWord{$ENDIF} :
                begin
                  int64Data := GetOrdProp(AObject,p^.Name);
                  AStore.Put(propName,pt,int64Data);
                end;
              tkLString
              {$IFDEF FPC},tkAString{$ENDIF}
              {$IFDEF WST_UNICODESTRING}, tkUString{$ENDIF}:
                begin
                  strData := GetStrProp(AObject,p^.Name);
                  AStore.Put(propName,pt,strData);
                end;
              tkClass :
                begin
                  objData := GetObjectProp(AObject,p^.Name);
                  AStore.Put(propName,pt,objData);
                end;
              {$IFDEF HAS_TKBOOL}
              tkBool :
                begin
                  boolData := Boolean(GetOrdProp(AObject,p^.Name));
                  AStore.Put(propName,pt,boolData);
                end;
              {$ENDIF}
              tkEnumeration,tkInteger :
                begin
                {$IFDEF WST_DELPHI}
                  if ( pt^.Kind = tkEnumeration ) and
                     ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                  then begin
                    boolData := Boolean(GetOrdProp(AObject,p^.Name));
                    AStore.Put(propName,pt,boolData);
                  end else begin
                {$ENDIF}
                    FillChar(enumData,SizeOf(enumData),#0);
                    case GetTypeData(pt)^.OrdType of
                      otSByte :
                        begin
                          enumData.ShortIntData := ShortInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ShortIntData);
                        end;
                      otUByte :
                        begin
                          enumData.ByteData := Byte(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ByteData);
                        end;
                      otSWord :
                        begin
                          enumData.SmallIntData := SmallInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.SmallIntData);
                        end;
                      otUWord :
                        begin
                          enumData.WordData := Word(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.WordData);
                        end;
                      otSLong :
                        begin
                          enumData.SLongIntData := LongInt(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.SLongIntData);
                        end;
                      otULong :
                        begin
                          enumData.ULongIntData := LongWord(GetOrdProp(AObject,p^.Name));
                          AStore.Put(propName,pt,enumData.ULongIntData);
                        end;
                    end;
                {$IFDEF WST_DELPHI}
                  end;
                {$ENDIF}
                end;
              tkFloat :
                begin
                  FillChar(floatDt,SizeOf(floatDt),#0);
                  case GetTypeData(pt)^.FloatType of
                    ftSingle :
                      begin
                        floatDt.SingleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.SingleData);
                      end;
                    ftDouble :
                      begin
                        floatDt.DoubleData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.DoubleData);
                      end;
                    ftExtended :
                      begin
                        floatDt.ExtendedData := Extended(GetFloatProp(AObject,p^.Name));
                        AStore.Put(propName,pt,floatDt.ExtendedData);
                      end;
                    ftCurr :
                      begin
                        floatDt.CurrencyData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.CurrencyData);
                      end;
{$IFDEF HAS_COMP}
                    ftComp :
                      begin
                        floatDt.CompData := GetFloatProp(AObject,p^.Name);
                        AStore.Put(propName,pt,floatDt.CompData);
                      end;
{$ENDIF}
                  end;
                end;
            end;
          end;
        end;
      finally
        Freemem(propList,propListLen*SizeOf(Pointer));
      end;
    end;
  finally
    AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;
{$ENDIF USE_SERIALIZE}

class procedure TBaseComplexSimpleContentRemotable.Load(
  var AObject: TObject;
      AStore: IFormatterBase;
  var AName: string;
  const ATypeInfo: PTypeInfo
);
{$IFDEF USE_SERIALIZE}
var
  locSerializer : TSimpleContentObjectSerializer;
begin
  locSerializer := TSimpleContentObjectRegistryItem(GetTypeRegistry().ItemByTypeInfo[ATypeInfo]).GetSerializer();
  if ( locSerializer <> nil ) then
    locSerializer.Read(AObject,AStore,AName,ATypeInfo)
  else
    raise ETypeRegistryException.CreateFmt(SERR_NoSerializerFoThisType,[ATypeInfo^.Name])
end;
{$ELSE USE_SERIALIZE}
Var
  propList : PPropList;
  i, propCount, propListLen : Integer;
  pt : PTypeInfo;
  propName : String;
  int64Data : Int64;
  strData : String;
  objData : TObject;
    objDataCreateHere : Boolean;
  {$IFDEF HAS_TKBOOL}boolData : Boolean;{$ENDIF}
  p : PPropInfo;
  enumData : TEnumBuffer;
  floatDt : TFloatExtendedType;
  floatBuffer : TFloatBuffer;
  persistType : TPropStoreType;
  objTypeData : PTypeData;
  oldSS : TSerializationStyle;
  tr : TTypeRegistry;
  regItem : TTypeRegistryItem;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit; // ???? FreeAndNil(AObject);
      If Not Assigned(AObject) Then
        AObject := Create();
      LoadValue(AObject,AStore);
      objTypeData := GetTypeData(ATypeInfo);
      propCount := objTypeData^.PropCount;
      If ( propCount > 0 ) Then Begin
        propListLen := GetPropList(ATypeInfo,propList);
        Try
          tr := GetTypeRegistry();
          regItem := tr.ItemByTypeInfo[ATypeInfo];
          AStore.SetSerializationStyle(ssAttibuteSerialization);
          For i := 0 To Pred(propCount) Do Begin
            p := propList^[i];
            persistType := IsStoredPropClass(objTypeData^.ClassType,p);
            If ( persistType in [pstOptional,pstAlways] ) Then Begin
              pt := p^.PropType{$IFDEF WST_DELPHI}^{$ENDIF};
              propName := regItem.GetExternalPropertyName(p^.Name);
              try
                Case pt^.Kind Of
                  tkInt64{$IFDEF HAS_QWORD},tkQWord{$ENDIF} :
                    Begin
                      AStore.Get(pt,propName,int64Data);
                      SetOrdProp(AObject,p^.Name,int64Data);
                    End;
                  tkLString
                  {$IFDEF FPC},tkAString{$ENDIF}
                  {$IFDEF WST_UNICODESTRING}, tkUString{$ENDIF}:
                    Begin
                      AStore.Get(pt,propName,strData);
                      SetStrProp(AObject,p^.Name,strData);
                    End;
                  {$IFDEF HAS_TKBOOL}
                  tkBool :
                    Begin
                      AStore.Get(pt,propName,boolData);
                      SetOrdProp(AObject,p^.Name,Ord(boolData));
                    End;
                  {$ENDIF}
                  tkClass :
                    Begin
                      objData := GetObjectProp(AObject,p^.Name);
                      objDataCreateHere := not Assigned(objData);
                      try
                        AStore.Get(pt,propName,objData);
                        if objDataCreateHere then
                          SetObjectProp(AObject,p^.Name,objData);
                      finally
                        if objDataCreateHere then
                          FreeAndNil(objData);
                      end;
                    End;
                  tkEnumeration,tkInteger :
                    Begin
                      FillChar(enumData,SizeOf(enumData),#0);
                      Case GetTypeData(pt)^.OrdType Of
                        otSByte :
                          Begin
                            AStore.Get(pt,propName,enumData.ShortIntData);
                            int64Data := enumData.ShortIntData;
                          End;
                        otUByte :
                          Begin
                            AStore.Get(pt,propName,enumData.ByteData);
                            int64Data := enumData.ByteData;
                          End;
                        otSWord :
                          Begin
                            AStore.Get(pt,propName,enumData.SmallIntData);
                            int64Data := enumData.SmallIntData;
                          End;
                        otUWord :
                          Begin
                            AStore.Get(pt,propName,enumData.WordData);
                            int64Data := enumData.WordData;
                          End;
                        otSLong:
                          Begin
                            AStore.Get(pt,propName,enumData.SLongIntData);
                            int64Data := enumData.SLongIntData;
                          End;
                        otULong :
                          Begin
                            AStore.Get(pt,propName,enumData.ULongIntData);
                            int64Data := enumData.ULongIntData;
                          End;
                      End;
                      SetOrdProp(AObject,p^.Name,int64Data);
                    End;
                  tkFloat :
                    Begin
                      FillChar(floatDt,SizeOf(floatBuffer),#0);
                      Case GetTypeData(pt)^.FloatType Of
                        ftSingle :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.SingleData);
                            floatDt := floatBuffer.SingleData;
                          End;
                        ftDouble :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.DoubleData);
                            floatDt := floatBuffer.DoubleData;
                          End;
                        ftExtended :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.ExtendedData);
                            floatDt := floatBuffer.ExtendedData;
                          End;
                        ftCurr :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CurrencyData);
                            floatDt := floatBuffer.CurrencyData;
                          End;
                        ftComp :
                          Begin
                            AStore.Get(pt,propName,floatBuffer.CompData);
                            floatDt := floatBuffer.CompData;
                          End;
                      End;
                      SetFloatProp(AObject,p^.Name,floatDt);
                    End;
                End;
              except
                on E : EServiceException do begin
                  if ( persistType = pstAlways ) then
                    raise;
                end;
              end;
            End;
          End;
        Finally
          Freemem(propList,propListLen*SizeOf(Pointer));
        End;
      End;
    finally
      AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;
{$ENDIF USE_SERIALIZE}

{ TComplexEnumContentRemotable }

class procedure TComplexEnumContentRemotable.SaveValue(
  AObject : TBaseRemotable; 
  AStore  : IFormatterBase
);  
begin                                 
  AStore.PutScopeInnerValue(GetEnumTypeInfo(),(AObject as TComplexEnumContentRemotable).GetValueAddress()^);
end;

class procedure TComplexEnumContentRemotable.LoadValue(
  var AObject : TObject;  
      AStore  : IFormatterBase
);  
var
  locObject : TComplexEnumContentRemotable;
  locBuffer : Pointer;
begin
  locObject := AObject as TComplexEnumContentRemotable;
  locBuffer := locObject.GetValueAddress();
  AStore.GetScopeInnerValue(GetEnumTypeInfo(),locBuffer^);
end; 

{ TComplexInt32SContentRemotable }

class procedure TComplexInt32SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(LongInt),(AObject as TComplexInt32SContentRemotable).Value);
end;

class procedure TComplexInt32SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : LongInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(LongInt),i);
  (AObject as TComplexInt32SContentRemotable).Value := i;
end;

function TComplexInt32SContentRemotable.wstHasValue: Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt32UContentRemotable }

class procedure TComplexInt32UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(LongWord),(AObject as TComplexInt32UContentRemotable).Value);
end;

class procedure TComplexInt32UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : LongWord;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(LongWord),i);
  (AObject as TComplexInt32UContentRemotable).Value := i;
end;

function TComplexInt32UContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt16SContentRemotable }

class procedure TComplexInt16SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(SmallInt),(AObject as TComplexInt16SContentRemotable).Value);
end;

class procedure TComplexInt16SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : SmallInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(SmallInt),i);
  (AObject as TComplexInt16SContentRemotable).Value := i;
end;

function TComplexInt16SContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt16UContentRemotable }

class procedure TComplexInt16UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Word),(AObject as TComplexInt16UContentRemotable).Value);
end;

class procedure TComplexInt16UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Word;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Word),i);
  (AObject as TComplexInt16UContentRemotable).Value := i;
end;

function TComplexInt16UContentRemotable.wstHasValue: Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexFloatExtendedContentRemotable }

class procedure TComplexFloatExtendedContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Extended),(AObject as TComplexFloatExtendedContentRemotable).Value);
end;

class procedure TComplexFloatExtendedContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Extended;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Extended),i);
  (AObject as TComplexFloatExtendedContentRemotable).Value := i;
end;

function TComplexFloatExtendedContentRemotable.wstHasValue: Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexFloatDoubleContentRemotable }

class procedure TComplexFloatDoubleContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Double),(AObject as TComplexFloatDoubleContentRemotable).Value);
end;

class procedure TComplexFloatDoubleContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Double;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Double),i);
  (AObject as TComplexFloatDoubleContentRemotable).Value := i;
end;

function TComplexFloatDoubleContentRemotable.wstHasValue: Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexStringContentRemotable }

class procedure TComplexStringContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(string),(AObject as TComplexStringContentRemotable).Value);
end;

class procedure TComplexStringContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : string;
begin
  i := '';
  AStore.GetScopeInnerValue(TypeInfo(string),i);
  (AObject as TComplexStringContentRemotable).Value := i;
end;

function TComplexStringContentRemotable.wstHasValue: Boolean;
begin
  Result := (FValue <> '');
end;

{ TComplexWideStringContentRemotable }

class procedure TComplexWideStringContentRemotable.SaveValue(
  AObject: TBaseRemotable;
  AStore: IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(WideString),(AObject as TComplexWideStringContentRemotable).Value);
end;

class procedure TComplexWideStringContentRemotable.LoadValue(
  var AObject: TObject;
      AStore: IFormatterBase
);
var
  i : WideString;
begin
  i := '';
  AStore.GetScopeInnerValue(TypeInfo(WideString),i);
  (AObject as TComplexWideStringContentRemotable).Value := i;
end;

function TComplexWideStringContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> '');
end;

{$IFDEF WST_UNICODESTRING}
{ TComplexUnicodeStringContentRemotable }

class procedure TComplexUnicodeStringContentRemotable.SaveValue(
  AObject: TBaseRemotable;
  AStore: IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(UnicodeString),(AObject as TComplexUnicodeStringContentRemotable).Value);
end;

class procedure TComplexUnicodeStringContentRemotable.LoadValue(
  var AObject: TObject;
      AStore: IFormatterBase
);
var
  i : UnicodeString;
begin
  i := '';
  AStore.GetScopeInnerValue(TypeInfo(UnicodeString),i);
  (AObject as TComplexUnicodeStringContentRemotable).Value := i;
end;

function TComplexUnicodeStringContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> '');
end;
{$ENDIF WST_UNICODESTRING}

{ TDateRemotable }

class function TDateRemotable.ToStr(const ADate : TDateTimeRec) : string;
begin
  Result := xsd_DateTimeToStr(ADate,xdkDate);
end;

class function TDateRemotable.Parse(const ABuffer : string) : TDateTimeRec;
begin
  Result := xsd_StrToDate(ABuffer,xdkDate);
end;

{ TDateTimeRemotable }

class function TDateTimeRemotable.ToStr(const ADate: TDateTimeRec): string;
begin
  Result := xsd_DateTimeToStr(ADate,xdkDateTime);
end;

class function TDateTimeRemotable.Parse(const ABuffer: string): TDateTimeRec;
begin
  Result := xsd_StrToDate(ABuffer,xdkDateTime);
end;

function TDateTimeRemotable.GetDatepart(const AIndex: Integer): Integer;
begin
  case AIndex of
    3 : Result := HourOf(AsDate);
    4 : Result := MinuteOf(AsDate);
    5 : Result := SecondOf(AsDate);
    else
        Result := inherited GetDatepart(AIndex);
  end;
end;

{ TBaseDateRemotable }

procedure TBaseDateRemotable.SetDate(const AIndex : Integer; const AValue: TDateTime);
begin
  FDate.Date := AValue;
  if ( AIndex = 1 ) then begin
    if ( FDate.HourOffset <> 0 ) then
      FDate.Date := date_utils.IncHour(FDate.Date,FDate.HourOffset);
    if ( FDate.MinuteOffset <> 0 ) then
      FDate.Date := IncMinute(FDate.Date,FDate.MinuteOffset);
  end;
end;

class procedure TBaseDateRemotable.Save(
        AObject   : TBaseRemotable;
        AStore    : IFormatterBase;
  const AName     : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := TBaseDateRemotable(AObject).AsString;
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TBaseDateRemotable.Load(
  var AObject     : TObject;
      AStore      : IFormatterBase;
  var AName       : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := '';
  AStore.Get(TypeInfo(string),AName,buffer);
  if ( AObject = nil ) then
    AObject := Create();
  TBaseDateRemotable(AObject).AsString := buffer;
end;

procedure TBaseDateRemotable.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TBaseDateRemotable) then begin
    FDate := TBaseDateRemotable(Source).FDate;
  end else begin
    inherited Assign(Source);
  end;
end;

function TBaseDateRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
begin
  Result := ( Self = ACompareTo ) or
            ( Assigned(ACompareTo) and
              ACompareTo.InheritsFrom(TBaseDateRemotable) and
              ( Self.AsDate = TBaseDateRemotable(ACompareTo).AsDate )
            );
end;

function TBaseDateRemotable.wstHasValue() : Boolean;
begin
  Result := (FDate.Date <> 0);
end;

function TBaseDateRemotable.GetDate(const AIndex : Integer) : TDateTime;
begin
  Result := FDate.Date;
  if ( AIndex = 1 ) then begin
    if ( FDate.HourOffset <> 0 ) then
      Result := date_utils.IncHour(Result,-FDate.HourOffset);
    if ( FDate.MinuteOffset <> 0 ) then
      Result := date_utils.IncMinute(Result,-FDate.MinuteOffset);
  end;
end;

function TBaseDateRemotable.GetDatepart(const AIndex: Integer): Integer;
begin
  case AIndex of
    0 : Result := YearOf(AsDate);
    1 : Result := MonthOf(AsDate);
    2 : Result := DayOf(AsDate);
    else
      Result := 0;
  end;
end;

function TBaseDateRemotable.GetAsString: string;
begin
  Result := ToStr(FDate);
end;

function TBaseDateRemotable.GetOffset(const Index: Integer): Shortint;
begin
  if ( Index = 0 ) then
    Result := FDate.HourOffset
  else
    Result := FDate.MinuteOffset;
end;

procedure TBaseDateRemotable.SetAsString(const AValue: string);
begin
  if (AValue<>'') then
    FDate := Parse(AValue)
  else
    FillChar(FDate,SizeOf(TDateTimeRec),0);
end;

procedure TBaseDateRemotable.SetOffset(const Index: Integer; const Value: Shortint);
begin
  if ( Index = 0 ) then begin
    if ( Value >= -14 ) and ( Value <= 14 ) then
      FDate.HourOffset := Value
    else
      raise Exception.CreateFmt(SERR_InvalidHourOffetValue,[Value]);
  end else begin
    if ( Value >= -59 ) and ( Value <= 59 ) then
      FDate.MinuteOffset := Value
    else
      raise Exception.CreateFmt(SERR_InvalidMinuteOffetValue,[Value]);
  end;
end;

class function TBaseDateRemotable.ToStr(const ADate: TDateTime): string;
var
  locTemp : TDateTimeRec;
begin
  locTemp.Date := ADate;
  locTemp.HourOffset := 0;
  locTemp.MinuteOffset := 0;
  Result := ToStr(locTemp);
end;

class function TBaseDateRemotable.ParseToUTC(const ABuffer : string) : TDateTime;
begin
  Result := NormalizeToUTC(Parse(ABuffer));
end;

{ TComplexInt8SContentRemotable }

class procedure TComplexInt8SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(ShortInt),(AObject as TComplexInt8SContentRemotable).Value);
end;

class procedure TComplexInt8SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : ShortInt;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(ShortInt),i);
  (AObject as TComplexInt8SContentRemotable).Value := i;
end;

function TComplexInt8SContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt8UContentRemotable }

class procedure TComplexInt8UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Byte),(AObject as TComplexInt8UContentRemotable).Value);
end;

class procedure TComplexInt8UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Byte;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Byte),i);
  (AObject as TComplexInt8UContentRemotable).Value := i;
end;

function TComplexInt8UContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexFloatSingleContentRemotable }

class procedure TComplexFloatSingleContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Single),(AObject as TComplexFloatSingleContentRemotable).Value);
end;

class procedure TComplexFloatSingleContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Single;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Single),i);
  (AObject as TComplexFloatSingleContentRemotable).Value := i;
end;

function TComplexFloatSingleContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexCurrencyContentRemotable }

class procedure TComplexCurrencyContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Currency),(AObject as TComplexCurrencyContentRemotable).Value);   
end;

class procedure TComplexCurrencyContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Currency;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Currency),i);
  (AObject as TComplexCurrencyContentRemotable).Value := i;
end;

function TComplexCurrencyContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt64SContentRemotable }

class procedure TComplexInt64SContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Int64),(AObject as TComplexInt64SContentRemotable).Value);
end;

class procedure TComplexInt64SContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Int64;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(Int64),i);
  (AObject as TComplexInt64SContentRemotable).Value := i;
end;

function TComplexInt64SContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexInt64UContentRemotable }

class procedure TComplexInt64UContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(QWord),(AObject as TComplexInt64UContentRemotable).Value);
end;

class procedure TComplexInt64UContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : QWord;
begin
  i := 0;
  AStore.GetScopeInnerValue(TypeInfo(QWord),i);
  (AObject as TComplexInt64UContentRemotable).Value := i;
end;

function TComplexInt64UContentRemotable.wstHasValue() : Boolean;
begin
  Result := (FValue <> 0);
end;

{ TComplexBooleanContentRemotable }

class procedure TComplexBooleanContentRemotable.SaveValue(
  AObject : TBaseRemotable;
  AStore  : IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(Boolean),(AObject as TComplexBooleanContentRemotable).Value);
end;

class procedure TComplexBooleanContentRemotable.LoadValue(
  var AObject : TObject;
      AStore  : IFormatterBase
);
var
  i : Boolean;
begin
  i := False;
  AStore.GetScopeInnerValue(TypeInfo(Boolean),i);
  (AObject as TComplexBooleanContentRemotable).Value := i;
end;

{ TIntfPoolItem }

constructor TIntfPoolItem.Create(AIntf: IInterface; const AUsed: Boolean);
begin
  FIntf := AIntf as IInterface;
  FUsed := AUsed;
end;

destructor TIntfPoolItem.Destroy();
begin
  FIntf := nil;
  inherited Destroy();
end;

{ TIntfPool }

function TIntfPool.CreateNew(const AUsed : Boolean): TIntfPoolItem;
begin
  FCS.Acquire();
  try
    Result := TIntfPoolItem.Create(FFactory.CreateInstance(),AUsed);
    FList.Add(Result);
  finally
    FCS.Release();
  end;
end;

function TIntfPool.TryGet(const AIndex: Integer): Boolean;
var
  itm : TIntfPoolItem;
begin
  FCS.Acquire();
  try
    itm := TIntfPoolItem(FList[AIndex]);
    Result := not itm.Used;
    if Result then begin
      itm.Used := True;
    end;
  finally
    FCS.Release();
  end;
end;

constructor TIntfPool.Create(const AMin, AMax: Integer; AFactory: IItemFactory);
var
  i : Integer;
begin
  if not ( ( AMin >= 0 ) and ( AMax >= AMin ) and ( AFactory <> nil ) ) then
    raise Exception.CreateFmt(SERR_InvalidPoolParametersArgs,[AMin,AMax]);
  FMax := AMax;
  FMin := AMin;
  FFactory := AFactory;
  FLock := TSemaphoreObject.Create(FMax);
  FList := TObjectList.Create(True);
  FCS := TCriticalSection.Create();
  for i := 0 to Pred(AMin) do begin
    CreateNew(False);
  end;
end;

destructor TIntfPool.Destroy();
begin
  FFactory := nil;
  FreeAndNil(FCS);
  FreeAndNil(FLock);
  FreeAndNil(FList);
  inherited Destroy();
end;

function TIntfPool.Get(const ATimeOut : Cardinal): IInterface;
var
  i : Integer;
begin
  Result := nil;
  if ( FLock.WaitFor(ATimeOut) = wrSignaled ) then begin
    for i := 0 to Pred(FList.Count) do begin
      if TryGet(i) then begin
        Result := TIntfPoolItem(FList[i]).Intf;
        Break;
      end;
    end;
    if ( Result = nil ) then begin
      Result := CreateNew(True).Intf;
    end;
  end else begin
    raise EServiceException.Create(SERRE_ObjectCreationTimeOut);
  end;
end;

procedure TIntfPool.Release(const AItem: IInterface);
var
  i : Integer;
  a : IInterface;
begin
  a := AItem as IInterface;
  for i := 0 to Pred(FList.Count) do begin
    if ( TIntfPoolItem(FList[i]).Intf = a ) then begin
      TIntfPoolItem(FList[i]).Used := False;
      FLock.Release();
      Break;
    end;
  end;
end;

procedure TIntfPool.Discard(const AItem : IInterface);
var
  i : Integer;
  a : IInterface;
  itm : TIntfPoolItem;
begin
  a := AItem as IInterface;
  for i := 0 to Pred(FList.Count) do begin
    if ( TIntfPoolItem(FList[i]).Intf = a ) then begin
      itm := TIntfPoolItem(FList[i]);
      itm.FIntf := FFactory.CreateInstance() as IInterface;
      itm.Used := False;
      FLock.Release();
      Break;
    end;
  end;
end;

function TIntfPool.GetInstancesCount() : Integer;
begin
  FCS.Acquire();
  try
    Result := FList.Count;
  finally
    FCS.Release();
  end;
end;

{ TStringBufferRemotable }

class procedure TStringBufferRemotable.Save (
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  if ( AObject <> nil ) then
    buffer := TStringBufferRemotable(AObject).Data
  else
    buffer := '';
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TStringBufferRemotable.Load (
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
  locObj : TStringBufferRemotable;
begin
  AStore.ReadBuffer(AName,buffer);
  if ( AObject = nil ) then
    AObject := Create();
  locObj := AObject as TStringBufferRemotable;;
  locObj.Data := buffer;
end;

procedure TStringBufferRemotable.Assign (Source : TPersistent );
begin
  if ( Source = nil ) then begin
    FData := '';
  end else begin
    if Source.InheritsFrom(TStringBufferRemotable) then
      Self.Data := TStringBufferRemotable(Source).Data
    else
      inherited Assign(Source);
  end;
end;

function TStringBufferRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
begin
  Result := ( Self = ACompareTo ) or
            ( Assigned(ACompareTo) and
              ACompareTo.InheritsFrom(TStringBufferRemotable) and
              ( Self.Data = TStringBufferRemotable(ACompareTo).Data )
            );
end;

function TStringBufferRemotable.wstHasValue() : Boolean;
begin
  Result := (Data <> '');
end;

{ TRemotableRecordEncoder }

class procedure TRemotableRecordEncoder.Save(
        ARecord : Pointer;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  recStart, recFieldAddress : PByte;
  typData : PRecordTypeData;
  i : PtrUInt;
  pt : PTypeInfo;
  p : PRecordFieldInfo;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
  prpName : string;
  typDataObj : TObject;
begin
  oldSS := AStore.GetSerializationStyle();
  AStore.BeginObject(AName,ATypeInfo);
  try
    if not Assigned(ARecord) then begin
      AStore.NilCurrentScope();
      Exit;
    end;
    typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
    typDataObj := typRegItem.GetObject(FIELDS_STRING);
    Assert(Assigned(typDataObj),Format(SERR_IncompleteParamTypeRegistration,[AName]));
    typData := PRecordTypeData((typDataObj as TDataObject).Data);
    Assert(Assigned(typData));
    if ( typData^.FieldCount > 0 ) then begin
      recStart := PByte(ARecord);
      ss := AStore.GetSerializationStyle();
      for i := 0 to Pred(typData^.FieldCount) do begin
        p := @(typData^.Fields[i]);
        if p^.Visible then begin
          pt := p^.TypeInfo^;
          if p^.IsAttribute then begin
            if ( ss <> ssAttibuteSerialization ) then
              ss := ssAttibuteSerialization;
          end else begin
            if ( ss <> ssNodeSerialization ) then
              ss := ssNodeSerialization;
          end;
          if ( ss <> AStore.GetSerializationStyle() ) then
            AStore.SetSerializationStyle(ss);
          prpName := typRegItem.GetExternalPropertyName(p^.Name);
          recFieldAddress := recStart;
          Inc(recFieldAddress,p^.Offset);
          case pt^.Kind of
            tkInt64 : AStore.Put(prpName,pt,PInt64(recFieldAddress)^);
            {$IFDEF HAS_QWORD}
            tkQWord : AStore.Put(prpName,pt,PQWord(recFieldAddress)^);
            {$ENDIF}
            tkLString
            {$IFDEF FPC},tkAString{$ENDIF}
            {$IFDEF WST_UNICODESTRING},tkUString{$ENDIF} : AStore.Put(prpName,pt,Pointer(recFieldAddress)^);
            tkClass : AStore.Put(prpName,pt,PObject(recFieldAddress)^);
            tkRecord : AStore.Put(prpName,pt,Pointer(recFieldAddress)^);
            {$IFDEF HAS_TKBOOL}
            tkBool : AStore.Put(prpName,pt,PBoolean(recFieldAddress)^);
            {$ENDIF}
            tkEnumeration,tkInteger :
              begin
              {$IFDEF WST_DELPHI}
                if ( pt^.Kind = tkEnumeration ) and
                   ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                then begin
                  AStore.Put(prpName,pt,PBoolean(recFieldAddress)^);
                end else begin
              {$ENDIF}
                  case GetTypeData(pt)^.OrdType of
                    otSByte : AStore.Put(prpName,pt,PShortInt(recFieldAddress)^);
                    otUByte : AStore.Put(prpName,pt,PByte(recFieldAddress)^);
                    otSWord : AStore.Put(prpName,pt,PSmallInt(recFieldAddress)^);
                    otUWord : AStore.Put(prpName,pt,PWord(recFieldAddress)^);
                    otSLong : AStore.Put(prpName,pt,PLongint(recFieldAddress)^);
                    otULong : AStore.Put(prpName,pt,PLongWord(recFieldAddress)^);
                  end;
              {$IFDEF WST_DELPHI}
                end;
              {$ENDIF}
              end;
            tkFloat :
              begin
                case GetTypeData(pt)^.FloatType of
                  ftSingle   : AStore.Put(prpName,pt,PSingle(recFieldAddress)^);
                  ftDouble   : AStore.Put(prpName,pt,PDouble(recFieldAddress)^);
                  ftExtended : AStore.Put(prpName,pt,PExtended(recFieldAddress)^);
                  ftCurr     : AStore.Put(prpName,pt,PCurrency(recFieldAddress)^);
  {$IFDEF HAS_COMP}
                  ftComp     : AStore.Put(prpName,pt,PComp(recFieldAddress)^);
  {$ENDIF}
                end;
              end;
          end;
        end;
      end;
    end;
  finally
    AStore.EndScope();
    AStore.SetSerializationStyle(oldSS);
  end;
end;

class procedure TRemotableRecordEncoder.Load(
  var   ARecord : Pointer;
        AStore : IFormatterBase;
  var   AName : string;
  const ATypeInfo : PTypeInfo
);
var
  recStart, recFieldAddress : PByte;
  typData : PRecordTypeData;
  i : PtrUInt;
  pt : PTypeInfo;
  propName : String;
  p : PRecordFieldInfo;
  oldSS,ss : TSerializationStyle;
  typRegItem : TTypeRegistryItem;
  typDataObj : TObject;
begin
  oldSS := AStore.GetSerializationStyle();
  if ( AStore.BeginObjectRead(AName,ATypeInfo) >= 0 ) then begin
    try
      if AStore.IsCurrentScopeNil() then
        Exit;
      typRegItem := GetTypeRegistry().ItemByTypeInfo[ATypeInfo];
      typDataObj := typRegItem.GetObject(FIELDS_STRING);
      Assert(Assigned(typDataObj),Format(SERR_IncompleteParamTypeRegistration,[AName]));
      typData := PRecordTypeData((typDataObj as TDataObject).Data);
      Assert(Assigned(typData));
      if ( not Assigned(ARecord) ) then begin
        GetMem(ARecord,typData^.RecordSize);
        FillChar(ARecord^,typData^.RecordSize,#0);
      end;

      if ( typData^.FieldCount > 0 ) then begin
        recStart := PByte(ARecord);
        for i := 0 to Pred(typData^.FieldCount) do begin
          p := @(typData^.Fields[i]);
          if p^.Visible then begin
            pt := p^.TypeInfo^;
            propName := typRegItem.GetExternalPropertyName(p^.Name);
            if p^.IsAttribute then begin
              ss := ssAttibuteSerialization;
            end else begin
              ss := ssNodeSerialization;
            end;
            if ( ss <> AStore.GetSerializationStyle() ) then
              AStore.SetSerializationStyle(ss);
            recFieldAddress := recStart;
            Inc(recFieldAddress,p^.Offset);
            //try
              Case pt^.Kind Of
                tkInt64 : AStore.Get(pt,propName,PInt64(recFieldAddress)^);
                {$IFDEF HAS_QWORD}
                tkQWord : AStore.Get(pt,propName,PQWord(recFieldAddress)^);
                {$ENDIF}
                tkLString
                {$IFDEF FPC},tkAString{$ENDIF}
                {$IFDEF WST_UNICODESTRING},tkUString{$ENDIF} : AStore.Get(pt,propName,PPointer(recFieldAddress)^);
                {$IFDEF HAS_TKBOOL}
                tkBool : AStore.Get(pt,propName,PBoolean(recFieldAddress)^);
                {$ENDIF}
                tkClass : AStore.Get(pt,propName,PObject(recFieldAddress)^);
                tkRecord : AStore.Get(pt,propName,Pointer(recFieldAddress)^);
                tkEnumeration,tkInteger :
                  Begin
                  {$IFDEF WST_DELPHI}
                    if ( pt^.Kind = tkEnumeration ) and
                       ( GetTypeData(pt)^.BaseType^ = TypeInfo(Boolean) )
                    then begin
                      AStore.Get(pt,propName,PBoolean(recFieldAddress)^);
                    end else begin
                  {$ENDIF}
                      case GetTypeData(pt)^.OrdType Of
                        otSByte : AStore.Get(pt,propName,PShortInt(recFieldAddress)^);
                        otUByte : AStore.Get(pt,propName,PByte(recFieldAddress)^);
                        otSWord : AStore.Get(pt,propName,PSmallInt(recFieldAddress)^);
                        otUWord : AStore.Get(pt,propName,PWord(recFieldAddress)^);
                        otSLong : AStore.Get(pt,propName,PLongint(recFieldAddress)^);
                        otULong : AStore.Get(pt,propName,PLongWord(recFieldAddress)^);
                      end;
                  {$IFDEF WST_DELPHI}
                    end;
                  {$ENDIF}
                  End;
                tkFloat :
                  begin
                    case GetTypeData(pt)^.FloatType of
                      ftSingle   : AStore.Get(pt,propName,PSingle(recFieldAddress)^);
                      ftDouble   : AStore.Get(pt,propName,PDouble(recFieldAddress)^);
                      ftExtended : AStore.Get(pt,propName,PExtended(recFieldAddress)^);
                      ftCurr     : AStore.Get(pt,propName,PCurrency(recFieldAddress)^);
                      {$IFDEF HAS_COMP}
                      ftComp     : AStore.Get(pt,propName,PComp(recFieldAddress)^);
                      {$ENDIF}
                    end;
                  end;
              End;
            {except
              on E : EServiceException do begin
                if ( persistType = pstAlways ) then
                  raise;
              end;
            end;}
          end;
        end;
      end;
    finally
      AStore.EndScopeRead();
      AStore.SetSerializationStyle(oldSS);
    end;
  end;
end;

{ TBase64StringRemotable }

function TBase64StringRemotable.GetEncodedString : string;
begin
  Result := Base64Encode(Length(BinaryData),BinaryData[0]);
end;

procedure TBase64StringRemotable.SetEncodedString(const AValue : string);
begin
  BinaryData := Base64Decode(AValue,[xoDecodeIgnoreIllegalChar]);
end;

{ TBase64StringExtRemotable }

function TBase64StringExtRemotable.GetEncodedString : string;
begin
  Result := Base64Encode(Length(BinaryData),BinaryData[0]);
end;

procedure TBase64StringExtRemotable.SetEncodedString(const AValue : string);
begin
  BinaryData := Base64Decode(AValue,[xoDecodeIgnoreIllegalChar]);
end;


procedure initialize_base_service_intf();
begin
{$IFDEF HAS_FORMAT_SETTINGS}
  {$IFDEF FPC}
    wst_FormatSettings := DefaultFormatSettings;
  {$ELSE}
    GetLocaleFormatSettings(GetThreadLocale(),wst_FormatSettings);
  {$ENDIF}
  wst_FormatSettings.DecimalSeparator := '.';
  wst_FormatSettings.ThousandSeparator := #0;
{$ENDIF HAS_FORMAT_SETTINGS}

  if ( TypeRegistryInstance = nil ) then begin
    TypeRegistryInstance := TTypeRegistry.Create();
    TypeRegistryInstance.RegisterInitializer(TBaseComplexRemotableInitializer);
    TypeRegistryInstance.RegisterInitializer(TSimpleContentObjectRemotableInitializer);
  end;
  if ( SerializeOptionsRegistryInstance = nil ) then
    SerializeOptionsRegistryInstance := TSerializeOptionsRegistry.Create();
  RegisterStdTypes();
end;

procedure finalize_base_service_intf();
begin
  FreeAndNil(SerializeOptionsRegistryInstance);
  FreeAndNil(TypeRegistryInstance);
end;


{ TDurationRemotable }

function TDurationRemotable.GetAsString: string;
begin
  Result := ToStr(FData);
end;

function TDurationRemotable.GetNegative: Boolean;
begin
  Result := FData.Negative;
end;

function TDurationRemotable.GetPart(AIndex: integer): PtrUInt;
begin
  case AIndex of
    0 : Result := FData.Year;
    1 : Result := FData.Month;
    2 : Result := FData.Day;
    3 : Result := FData.Hour;
    4 : Result := FData.Minute;
    5 : Result := FData.Second;
    6 : Result := FData.FractionalSecond;
    else
        Result := 0;
  end;
end;

procedure TDurationRemotable.SetAsString(const AValue: string);
begin
  if (AValue<>'') then
    FData := Parse(AValue)
  else
    FillChar(FData,SizeOf(TDurationRec),0);
end;

procedure TDurationRemotable.SetNegative(const AValue: Boolean);
begin
  FData.Negative := AValue;
end;

procedure TDurationRemotable.SetPart(AIndex: integer; const AValue: PtrUInt);
begin
  case AIndex of
    0 : FData.Year := AValue;
    1 : FData.Month := AValue;
    2 : FData.Day := AValue;
    3 : FData.Hour := AValue;
    4 : FData.Minute := AValue;
    5 : FData.Second := AValue;
    6 : FData.FractionalSecond := AValue;
  end;
end;

class procedure TDurationRemotable.Save(
        AObject   : TBaseRemotable;
        AStore    : IFormatterBase;
  const AName     : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := TDurationRemotable(AObject).AsString;
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TDurationRemotable.Load(
  var AObject     : TObject;
      AStore      : IFormatterBase;
  var AName       : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := '';
  AStore.Get(TypeInfo(string),AName,buffer);
  if ( AObject = nil ) then
    AObject := Create();
  TDurationRemotable(AObject).AsString := buffer;
end;

procedure TDurationRemotable.Assign(Source : TPersistent);
begin
  if ( Source <> nil ) and Source.InheritsFrom(TDurationRemotable) then
    Self.FData := TDurationRemotable(Source).FData
  else
    inherited Assign(Source);
end;

function TDurationRemotable.Equal(const ACompareTo : TBaseRemotable) : Boolean;
begin
  if ( Self = ACompareTo ) then begin
    Result := True;
  end else begin
    if ( ACompareTo <> nil ) and ACompareTo.InheritsFrom(TDurationRemotable) then
      Result := ValueEquals(Self.FData,TDurationRemotable(ACompareTo).FData)
    else
      Result := inherited Equal(ACompareTo);
  end;
end;

function TDurationRemotable.wstHasValue() : Boolean;
begin
  Result := (FData.Year <> 0) or (FData.Month <> 0) or (FData.Day <> 0) or
            (FData.Hour <> 0) or (FData.Minute <> 0) or (FData.Second <> 0) or
            (FData.FractionalSecond <> 0);
end;

procedure TDurationRemotable.Clear();
begin
  FData := ZERO_DURATION;
end;

class function TDurationRemotable.Parse(const ABuffer : string) : TDurationRec;
begin
  Result := xsd_StrToDuration(ABuffer);
end;

class function TDurationRemotable.ToStr(const AValue: TDurationRec): string;
begin
  Result := xsd_DurationToStr(AValue);
end;

{ TRemotableTypeInitializer }

class function TRemotableTypeInitializer.CanHandle(ATypeInfo : PTypeInfo) : Boolean;
begin
  Result := False;
end;

class function TRemotableTypeInitializer.GetItemClass(const ATypeInfo : PTypeInfo) : TTypeRegistryItemClass;
begin
  Result := TTypeRegistryItem;
end;



{ TComplexAnsiCharContentRemotable }

class procedure TComplexAnsiCharContentRemotable.LoadValue(
  var AObject: TObject;
      AStore: IFormatterBase
);
var
  i : AnsiChar;
begin
  i := #0;
  AStore.GetScopeInnerValue(TypeInfo(AnsiChar),i);
  (AObject as TComplexAnsiCharContentRemotable).Value := i;
end;

class procedure TComplexAnsiCharContentRemotable.SaveValue(
  AObject: TBaseRemotable;
  AStore: IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(AnsiChar),(AObject as TComplexAnsiCharContentRemotable).Value);
end;

{ TComplexWideCharContentRemotable }

class procedure TComplexWideCharContentRemotable.LoadValue(
  var AObject: TObject;
      AStore: IFormatterBase
);
var
  i : WideChar;
begin
  i := #0;
  AStore.GetScopeInnerValue(TypeInfo(WideChar),i);
  (AObject as TComplexWideCharContentRemotable).Value := i;
end;

class procedure TComplexWideCharContentRemotable.SaveValue(
  AObject: TBaseRemotable;
  AStore: IFormatterBase
);
begin
  AStore.PutScopeInnerValue(TypeInfo(WideChar),(AObject as TComplexWideCharContentRemotable).Value);
end;

{ TAbstractEncodedStringRemotable }

class procedure TAbstractEncodedStringRemotable.Save(
        AObject : TBaseRemotable;
        AStore : IFormatterBase;
  const AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  if ( AObject <> nil ) then
    buffer := TAbstractEncodedStringRemotable(AObject).EncodedString
  else
    buffer := '';
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TAbstractEncodedStringRemotable.Load(
  var AObject : TObject;
      AStore : IFormatterBase;
  var AName : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := '';
  AStore.Get(TypeInfo(string),AName,buffer);
  if ( AObject = nil ) then
    AObject := Create();
  TAbstractEncodedStringRemotable(AObject).EncodedString := buffer;
end;

procedure TAbstractEncodedStringRemotable.Assign(Source: TPersistent);
begin
  if Assigned(Source) then begin
    if Source.InheritsFrom(TAbstractEncodedStringRemotable) then
      Self.BinaryData := Copy(TAbstractEncodedStringRemotable(Source).BinaryData)
    else
      inherited Assign(Source);
  end else begin
    BinaryData := nil;
  end;
end;

function TAbstractEncodedStringRemotable.Equal(const ACompareTo: TBaseRemotable): Boolean;
begin
  Result := Assigned(ACompareTo) and
            ACompareTo.InheritsFrom(TAbstractEncodedStringRemotable) and
            ( Length(Self.BinaryData) = Length(TAbstractEncodedStringRemotable(ACompareTo).BinaryData) ) and
            CompareMem(Pointer(Self.BinaryData),Pointer(TAbstractEncodedStringRemotable(ACompareTo).BinaryData),Length(Self.BinaryData));
end;

function TAbstractEncodedStringRemotable.wstHasValue() : Boolean;
begin
  Result := (Length(FBinaryData) > 0);
end;

procedure TAbstractEncodedStringRemotable.LoadFromStream(AStream: TStream);
begin
  BinaryData := LoadBufferFromStream(AStream);
end;

procedure TAbstractEncodedStringRemotable.LoadFromBuffer(
  const ABuffer;
  const ABufferLen: Integer
);
begin
  SetLength(FBinaryData,ABufferLen);
  if (ABufferLen > 0) then
    Move(ABuffer,FBinaryData[0],ABufferLen);
end;

procedure TAbstractEncodedStringRemotable.LoadFromFile(const AFileName: string);
begin
  BinaryData := LoadBufferFromFile(AFileName);
end;

procedure TAbstractEncodedStringRemotable.SaveToStream(AStream: TStream);
begin
  if ( Length(FBinaryData) > 0 ) then
    AStream.Write(FBinaryData[0],Length(FBinaryData));
end;

function TAbstractEncodedStringRemotable.SaveToBuffer(
  var ABuffer;
  const ABufferLen: Integer
) : Integer;
var
  c : Integer;
begin
  c := Length(FBinaryData);
  if (c > ABufferLen) then
    c := ABufferLen;
  if (c > 0) then
    Move(FBinaryData[0],ABuffer,c);
  Result := c;
end;

procedure TAbstractEncodedStringRemotable.SaveToFile(const AFileName: string);
var
  locStream : TFileStream;
begin
  locStream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(locStream);
  finally
    locStream.Free();
  end;
end;

{ TAbstractEncodedStringExtRemotable }

class procedure TAbstractEncodedStringExtRemotable.SaveValue(
  AObject: TBaseRemotable;
  AStore: IFormatterBase
);
var
  s : string;
begin
  s := (AObject as TAbstractEncodedStringExtRemotable).EncodedString;
  AStore.PutScopeInnerValue(TypeInfo(string),s);
end;

class procedure TAbstractEncodedStringExtRemotable.LoadValue(
  var AObject: TObject;
      AStore: IFormatterBase
);
var
  s : string;
begin
  s := '';
  AStore.GetScopeInnerValue(TypeInfo(string),s);
  (AObject as TAbstractEncodedStringExtRemotable).EncodedString := s;
end;

procedure TAbstractEncodedStringExtRemotable.Assign(Source: TPersistent);
begin
  if Assigned(Source) and Source.InheritsFrom(TAbstractEncodedStringExtRemotable) then begin
    Self.BinaryData := Copy(TAbstractEncodedStringExtRemotable(Source).BinaryData);
  end;
  inherited;
end;

function TAbstractEncodedStringExtRemotable.Equal(const ACompareTo: TBaseRemotable): Boolean;
begin
  Result := Assigned(ACompareTo) and
            ACompareTo.InheritsFrom(TAbstractEncodedStringExtRemotable) and
            ( Length(Self.BinaryData) = Length(TAbstractEncodedStringExtRemotable(ACompareTo).BinaryData) ) and
            CompareMem(Pointer(Self.BinaryData),Pointer(TAbstractEncodedStringExtRemotable(ACompareTo).BinaryData),Length(Self.BinaryData));
end;

function TAbstractEncodedStringExtRemotable.wstHasValue: Boolean;
begin
  Result := (Length(FBinaryData) > 0);
end;

procedure TAbstractEncodedStringExtRemotable.LoadFromStream(AStream: TStream);
begin
  BinaryData := LoadBufferFromStream(AStream);
end;

procedure TAbstractEncodedStringExtRemotable.LoadFromBuffer(
  const ABuffer;
  const ABufferLen: Integer
);
begin
  SetLength(FBinaryData,ABufferLen);
  if (ABufferLen > 0) then
    Move(ABuffer,FBinaryData[0],ABufferLen);
end;

procedure TAbstractEncodedStringExtRemotable.LoadFromFile(const AFileName: string);
begin
  BinaryData := LoadBufferFromFile(AFileName);
end;

procedure TAbstractEncodedStringExtRemotable.SaveToStream(AStream: TStream);
begin
  if ( Length(FBinaryData) > 0 ) then
    AStream.Write(FBinaryData[0],Length(FBinaryData));
end;

function TAbstractEncodedStringExtRemotable.SaveToBuffer(
  var ABuffer;
  const ABufferLen: Integer
) : Integer;
var
  c : Integer;
begin
  c := Length(FBinaryData);
  if (c > ABufferLen) then
    c := ABufferLen;
  if (c > 0) then
    Move(FBinaryData[0],ABuffer,c);
  Result := c;
end;

procedure TAbstractEncodedStringExtRemotable.SaveToFile(const AFileName: string);
var
  locStream : TFileStream;
begin
  locStream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(locStream);
  finally
    locStream.Free();
  end;
end;

{ TBase16StringExtRemotable }

function TBase16StringExtRemotable.GetEncodedString: string;
begin
  Result := Base16Encode(BinaryData[0],Length(BinaryData));
end;

procedure TBase16StringExtRemotable.SetEncodedString(const AValue: string);
begin
  BinaryData := Base16Decode(AValue,[xoDecodeIgnoreIllegalChar]);
end;

{ TBase16StringRemotable }

function TBase16StringRemotable.GetEncodedString: string;
begin
  Result := Base16Encode(BinaryData[0],Length(BinaryData));
end;

procedure TBase16StringRemotable.SetEncodedString(const AValue: string);
begin
  BinaryData := Base16Decode(AValue,[xoDecodeIgnoreIllegalChar]);
end;



{ TTimeRemotable }

function TTimeRemotable.GetAsString : string;
begin
  Result := ToStr(Data);
end;

function TTimeRemotable.GetMilliSecond: Word;
begin
  Result := Data.MilliSecond;
end;

function TTimeRemotable.GetOffset(AIndex: integer): Shortint;
begin
  case AIndex of
    0 : Result := Data.HourOffset;
    1 : Result := Data.MinuteOffset;
    else
        Result := 0;
  end;
end;

function TTimeRemotable.GetPart(AIndex: integer): Byte;
begin
  case AIndex of
    0 : Result := Data.Hour;
    1 : Result := Data.Minute;
    2 : Result := Data.Second;
    else
        Result := 0;
  end;
end;

procedure TTimeRemotable.SetAsString(const AValue: string);
begin
  if (AValue<>'') then
    Data := Parse(AValue)
  else
    FillChar(FData,SizeOf(TTimeRec),0);
end;

procedure TTimeRemotable.SetMilliSecond(const AValue: Word);
begin
  FData.MilliSecond := AValue;
end;

procedure TTimeRemotable.SetOffset(AIndex: integer; const AValue: Shortint);
begin
  case AIndex of
    0 : FData.HourOffset := AValue;
    1 : FData.MinuteOffset := AValue;
  end;
end;

procedure TTimeRemotable.SetPart(AIndex: integer; const AValue: Byte);
begin
  case AIndex of
    0 : FData.Hour := AValue;
    1 : FData.Minute := AValue;
    2 : FData.Second := AValue;
  end;
end;

class procedure TTimeRemotable.Save(
        AObject   : TBaseRemotable;
        AStore    : IFormatterBase;
  const AName     : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := TTimeRemotable(AObject).AsString;
  AStore.Put(AName,TypeInfo(string),buffer);
end;

class procedure TTimeRemotable.Load(
  var AObject     : TObject;
      AStore      : IFormatterBase;
  var AName       : string;
  const ATypeInfo : PTypeInfo
);
var
  buffer : string;
begin
  buffer := '';
  AStore.Get(TypeInfo(string),AName,buffer);
  if ( AObject = nil ) then
    AObject := Create();
  TTimeRemotable(AObject).AsString := buffer;
end;

procedure TTimeRemotable.Assign(Source: TPersistent);
begin
  if ( Source = nil ) then begin
    Clear();
  end else begin
    if Source.InheritsFrom(TTimeRemotable) then
      Self.Data := TTimeRemotable(Source).Data
    else if Source.InheritsFrom(TDateTimeRemotable) then
      Self.Data := DateTimeToTimeRec(TDateTimeRemotable(Source).AsUTCDate)
    else
      inherited Assign(Source);
  end;
end;

function TTimeRemotable.Equal(const ACompareTo: TBaseRemotable): Boolean;
begin
  if ( ACompareTo = nil ) then begin
    Result := ValueEquals(Data,ZERO_TIME );
  end else begin
    if ACompareTo.InheritsFrom(TTimeRemotable) then
      Result := ValueEquals(Self.Data,TTimeRemotable(ACompareTo).Data)
    else
      Result := inherited Equal(ACompareTo);
  end;
end;

function TTimeRemotable.wstHasValue: Boolean;
begin
  Result := (Data.Hour <> 0) or (Data.Minute <> 0) or (Data.Second <> 0) or
            (Data.HourOffset <> 0) or (Data.MinuteOffset <> 0);
end;

procedure TTimeRemotable.Clear();
begin
  Data := ZERO_TIME;
end;

class function TTimeRemotable.Parse(const ABuffer: string): TTimeRec;
begin
  Result := xsd_StrToTime(ABuffer);
end;

class function TTimeRemotable.ToStr(const AValue: TTimeRec): string;
begin
  Result := xsd_TimeToStr(AValue);
end;


initialization
  initialize_base_service_intf();

finalization
  finalize_base_service_intf();

end.
