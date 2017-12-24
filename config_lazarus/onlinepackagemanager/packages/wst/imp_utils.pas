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
unit imp_utils;

interface

uses
  Classes, SysUtils, TypInfo,
  wst_types, base_service_intf;

Type

  EPropertyManagerException = class(EServiceException)
  End;
  
  { TPublishedPropertyManager }

  TPublishedPropertyManager = class(TInterfacedObject,IPropertyManager)
  Private
    FParent : TObject;
    FUnknownProps : IPropertyManager;
    FHandleUnknownProps : Boolean;
  private
    procedure Error(Const AMsg:string);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  Protected
    procedure SetProperty(Const AName,AValue:string);
    procedure SetProperties(Const APropsStr:string);
    function GetProperty(Const AName:String):string;
    function GetPropertyNames(ADest : TStrings):Integer;
    procedure Clear();
    procedure Copy(ASource:IPropertyManager; Const AClearBefore : Boolean);
  Public
    constructor Create(
            AParent             : TObject;
      const AHandleUnknownProps : Boolean = False
    );
  End;

  function IsStrEmpty(Const AStr:String):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}overload;
  function IsStrEmpty(Const AStr:ShortString):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}overload;
  function GetToken(var ABuffer : string; const ADelimiter : string): string;
  function ExtractOptionName(const ACompleteName : string):string;
  function ExtractNameFromQualifiedName(const AQualifiedName : string; const ASeparator : Char = ':') : string;
  function TranslateDotToDecimalSeperator(const Value: string) : string;
  function wst_FormatFloat(
    const ATypeInfo  : PTypeInfo;
    const AData      : Extended
  ) : string;


  function LoadBufferFromFile(const AFileName : string) : TByteDynArray;
  function LoadBufferFromStream(AStream : TStream) : TByteDynArray;

{$IFNDEF WST_HAS_STRICT_DELIMITER}
  procedure SetListData(AList : TStringList; const AText : string);
{$ENDIF WST_HAS_STRICT_DELIMITER}

implementation

function IsStrEmpty(Const AStr:String):Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function IsStrEmpty(Const AStr : ShortString) : Boolean;
begin
  Result := ( Length(Trim(AStr)) = 0 );
end;

function GetToken(var ABuffer : string; const ADelimiter : string): string;
var
  locPos, locOfs, locLen : PtrInt;
  locStr             : string;
begin
  locPos := Pos(ADelimiter, ABuffer);
  locLen := Length(ADelimiter);
  locOfs := locLen - 1;
  if (IsStrEmpty(ABuffer)) or ((locPos = 0) and (Length(ABuffer) > 0)) then begin
    Result := ABuffer;
    ABuffer := '';
  end else  begin
    locStr := Copy(ABuffer, 1, locPos + locOfs);
    ABuffer := Copy(ABuffer, locPos + locLen, Length(ABuffer));
    Result := Copy(locStr, 1, Length(locStr) - locLen);
  end;
end;

function ExtractOptionName(const ACompleteName : string):string;
var
  i, c : Integer;
begin
  Result := '';
  c := Length(ACompleteName);
  for i := c downto 1 do begin
    if ( ACompleteName[i] = '_' ) then
      Break;
    Result := ACompleteName[i] + Result;
  end;
  Result := Trim(Result);
end;

function ExtractNameFromQualifiedName(const AQualifiedName : string; const ASeparator : Char) : string;
var
  sepPos : Integer;
begin
  sepPos := Pos(ASeparator,AQualifiedName);
  if ( sepPos <= 0 ) then
    sepPos := 0;
  Result := Copy(AQualifiedName,(sepPos + 1),Length(AQualifiedName));
end;

{$IFDEF HAS_FORMAT_SETTINGS}
  {$IFDEF DELPHI}
var
  DefaultFormatSettings : TFormatSettings absolute FormatSettings;
  {$DEFINE HAS_DEFAULT_FORMAT_SETTINGS}
  {$ENDIF DELPHI}
{$ENDIF HAS_FORMAT_SETTINGS}
function TranslateDotToDecimalSeperator(const Value: string) : string;
var
  i : Integer;
begin
  Result := Value;
  for i := 1 to length(Result) do begin
    if ( Result[i] = '.' ) then
      Result[i] := {$IFDEF HAS_DEFAULT_FORMAT_SETTINGS}DefaultFormatSettings.{$ENDIF}DecimalSeparator;
  end;
end;

function wst_FormatFloat(
  const ATypeInfo  : PTypeInfo;
  const AData      : Extended
) : string;
var
  s, frmt : string;
  prcsn : Integer;
  decimal : Boolean;
{$IFNDEF HAS_FORMAT_SETTINGS}
  i : PtrInt;
{$ENDIF HAS_FORMAT_SETTINGS}
begin
  decimal := False;
  case GetTypeData(ATypeInfo)^.FloatType Of
    ftCurr      :
      begin
        decimal := True;
        frmt := '##############0.####';//15.4
      end;
    ftSingle,
    ftComp      : prcsn := 7;
    ftDouble,
    ftExtended  :
      begin
{$IF Defined(FPC_HAS_TYPE_DOUBLE) OR Defined(FPC_HAS_TYPE_EXTENDED)}
        prcsn := 14;
{$ELSE}
        prcsn := 7;
{$IFEND}
      end;
    else
      prcsn := 7;
  end;
  if not decimal then
    frmt := '#.' + StringOfChar('#',prcsn) + 'E-0';
{$IFDEF HAS_FORMAT_SETTINGS}
  s := FormatFloat(frmt,AData,wst_FormatSettings);
{$ELSE}
  s := FormatFloat(frmt,AData);
  i := Pos(',',s);
  if ( i > 0 ) then
    s[i] := '.';
{$ENDIF HAS_FORMAT_SETTINGS}
  Result := s
end;

function LoadBufferFromStream(AStream : TStream) : TByteDynArray;
var
  len : Int64;
begin
  len := AStream.Size;
  SetLength(Result,len);
  if ( len > 0 ) then begin
    try
      AStream.Seek(0,soBeginning);
      AStream.Read(Result[0],len);
    except
      SetLength(Result,0);
      raise;
    end;
  end;
end;

function LoadBufferFromFile(const AFileName : string) : TByteDynArray;
var
  locStream : TStream;
begin
  locStream := TFileStream.Create(AFileName,fmOpenRead);
  try
    Result := LoadBufferFromStream(locStream);
  finally
    locStream.Free();
  end;
end;

{ TPublishedPropertyManager }

procedure TPublishedPropertyManager.Error(const AMsg: string);
begin
  Raise EPropertyManagerException.Create(AMsg);
end;

procedure TPublishedPropertyManager.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise EPropertyManagerException.CreateFmt(AMsg,AArgs);
end;

procedure TPublishedPropertyManager.SetProperty(const AName, AValue: string);
Var
  pinf : PPropInfo;
  int64Val : Int64;
begin
  pinf := GetPropInfo(FParent,AName);
  if Assigned(pinf) then begin
    if Assigned(pinf^.SetProc) then begin
      Case pinf^.PropType^.Kind of
        tkLString
        {$IFDEF WST_DELPHI},tkString{$ENDIF}
        {$IFDEF FPC},tkSString,tkAString{$ENDIF}
        {$IFDEF WST_UNICODESTRING},tkUString{$ENDIF}
        ,tkWString :
          SetStrProp(FParent,pinf,AValue);
        tkEnumeration :
          SetEnumProp(FParent,pinf,AValue);
        tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
          Begin
            If TryStrToInt64(AValue,int64Val) Then
              SetOrdProp(FParent,AName,int64Val);
          End;
        {$IFDEF FPC}
        tkBool :
          SetOrdProp(FParent,AName,Ord(StrToBool(AValue)));
        {$ENDIF}
      End;
    end;
  end else if FHandleUnknownProps then begin
    FUnknownProps.SetProperty(AName,AValue);
  end;
end;

{$IFNDEF WST_HAS_STRICT_DELIMITER}
procedure SetListData(AList : TStringList; const AText : string);
var
  i, oldPos, c : Integer;
  s : string;
begin
  c := Length(AText);
  oldPos := 1;
  s := '';
  i := 1;
  while (i <= c) do begin
    if (AText[i] = PROP_LIST_DELIMITER) then begin
      s := Copy(AText,oldPos,(i-oldPos));
      if (s <> '') then
        AList.Add(s);
      oldPos := i+1;
    end;
    i := i+1;
  end;
  if (i > oldPos) then begin                      
    s := Copy(AText,oldPos,(i-oldPos));
    if (s <> '') then
      AList.Add(s);
  end;
end;
{$ENDIF WST_HAS_STRICT_DELIMITER}

procedure TPublishedPropertyManager.SetProperties(const APropsStr: string);
var
  lst : TStringList;
  i : Integer;
begin
  If IsStrEmpty(APropsStr) Then
    Exit;
  lst := TStringList.Create();
  Try
{$IFDEF WST_HAS_STRICT_DELIMITER}
    lst.QuoteChar := #0;
    lst.Delimiter := PROP_LIST_DELIMITER;
    lst.StrictDelimiter := True;
    lst.DelimitedText := APropsStr;
{$ELSE WST_HAS_STRICT_DELIMITER}
    SetListData(lst,APropsStr);
{$ENDIF WST_HAS_STRICT_DELIMITER}
    for i := 0 to Pred(lst.Count) do
      SetProperty(lst.Names[i],lst.Values[lst.Names[i]]);
  Finally
    lst.Free();
  End;
end;

function TPublishedPropertyManager.GetProperty(const AName: String): string;
Var
  pinf : PPropInfo;
begin
  Result := '';
  pinf := GetPropInfo(FParent,AName);
  if Assigned(pinf) then begin
    if Assigned(pinf^.SetProc) then begin
      Case pinf^.PropType^.Kind of
        tkLString
        {$IFDEF WST_DELPHI},tkString{$ENDIF}
        {$IFDEF FPC},tkSString,tkAString{$ENDIF}
        {$IFDEF WST_UNICODESTRING},tkUString{$ENDIF}
        ,tkWString :
          Result := GetStrProp(FParent,pinf);
        tkEnumeration :
          Result := GetEnumProp(FParent,pinf);
        tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF} :
          Result := IntToStr(GetOrdProp(FParent,pinf));
      End;
    end;
  end else if FHandleUnknownProps then begin
    Result := FUnknownProps.GetProperty(AName);
  end;
end;

function TPublishedPropertyManager.GetPropertyNames(ADest: TStrings): Integer;
Var
  propList : PPropList;
  i, propListLen : Integer;
begin
  ADest.Clear();
  if FHandleUnknownProps then
    FUnknownProps.GetPropertyNames(ADest);
  propListLen := GetPropList(PTypeInfo(FParent.ClassInfo),propList);
  if (propListLen > 0) then begin
    Try
      For i := 0 To Pred(propListLen) Do Begin
        If ( propList^[i]^.PropType^.Kind in
             [ tkLString{$IFDEF FPC},tkSString,tkAString{$ENDIF}{$IFDEF WST_UNICODESTRING},tkUString{$ENDIF}
               ,tkWString, tkEnumeration,
               tkInteger,tkInt64{$IFDEF FPC},tkQWord{$ENDIF}
             ]
           )
        Then
          ADest.Add(propList^[i]^.Name);
      End;
    Finally
      Freemem(propList,propListLen*SizeOf(Pointer));
    End;
  end;
  Result := ADest.Count;
end;

procedure TPublishedPropertyManager.Clear();
begin

end;

procedure TPublishedPropertyManager.Copy(
        ASource: IPropertyManager;
  const AClearBefore: Boolean
);
Var
  lst : TStringList;
  i : Integer;
  s : string;
begin
  If AClearBefore Then
    Clear();
  If Assigned(ASource) Then Begin
    lst := TStringList.Create();
    Try
      ASource.GetPropertyNames(lst);
      For i := 0 To Pred(lst.Count) Do Begin
        s := lst[i];
        SetProperty(s,ASource.GetProperty(s));
      End;
    Finally
      lst.Free();
    End;
  End;
end;

constructor TPublishedPropertyManager.Create(
        AParent             : TObject;
  const AHandleUnknownProps : Boolean = False
);
begin
  Assert(Assigned(AParent));
  FParent := AParent;
  FHandleUnknownProps := AHandleUnknownProps;
  if FHandleUnknownProps then
    FUnknownProps := TStoredPropertyManager.Create();
end;


end.

