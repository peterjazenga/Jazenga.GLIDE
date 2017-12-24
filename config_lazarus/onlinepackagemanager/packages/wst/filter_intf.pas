{
    This file is part of the Web Service Toolkit
    Copyright (c) 2010 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}

unit filter_intf;

interface
uses
  SysUtils,
  wst_types, base_service_intf;

type

  EFilterException = class(Exception) end;

  IDataFilter = interface
    ['{9D9886A4-37B6-4D62-BD3B-603A8EF00A13}']
    function GetPropertyManager() : IPropertyManager;
    function GetName() : string;
    function ExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;
    function ExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;
    function GetNext() : IDataFilter;
    procedure SetNext(AItem: IDataFilter);
  end;

  IDataFilterRegistry = interface
    ['{06489785-4447-4844-965B-9A50A417B20D}']
    function Find(
      const AName : string;
      out   ARes  : IDataFilter
    ):Boolean;
    procedure Register(
      const AName    : string;
            AFactory : IItemFactory
    );
  end;

{$TYPEINFO ON}
  
  { TBaseFilter }

  TBaseFilter = class(TSimpleFactoryItem,IDataFilter)
  private
    FPropertyManager : IPropertyManager;
    FNext : IDataFilter;
  protected
    function DoExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;virtual;abstract;
    function DoExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;virtual;abstract;
  protected
    function GetPropertyManager() : IPropertyManager;
    function GetName() : string;virtual;abstract;
    function ExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;
    function ExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;
    function GetNext() : IDataFilter;
    procedure SetNext(AItem: IDataFilter);                                    
  public
    constructor Create(); override;
  end;  
{$TYPEINFO OFF}

  { TDataFilterRegistry }
  
  TDataFilterRegistry = class(TBaseFactoryRegistry,IDataFilterRegistry)
  protected
    function Find(
      const AName : string;
      out   ARes  : IDataFilter
    ):Boolean;
  end;       
  
  function GetDataFilterRegistry():IDataFilterRegistry;
  function ParseDataFilterString(AValue : string) : IDataFilter; overload;
  function ParseDataFilterString(
    AValue     : string; 
    AFilterReg : IDataFilterRegistry
  ) : IDataFilter;overload;
  function GenerateFilterString(AFilter : IDataFilter) : string;
  
implementation
uses
  classes,
  wst_consts, imp_utils;

var
  DataFilterRegistryInst : IDataFilterRegistry = nil;
function GetDataFilterRegistry():IDataFilterRegistry;
begin
  if not Assigned(DataFilterRegistryInst) then
    DataFilterRegistryInst := TDataFilterRegistry.Create();// Lock!!!
  Result := DataFilterRegistryInst;
end;


function ParseDataFilterString(AValue : string; AFilterReg : IDataFilterRegistry) : IDataFilter;
var
  locAll, locBuffer, locName, locValue : string;
  locPM : IPropertyManager;
  locFilterManager : IDataFilterRegistry;
  locHeader, locLast, locFilter : IDataFilter;
begin
  locAll := Trim(AValue);
  if IsStrEmpty(locAll) then begin
    Result := nil;
    Exit;
  end;

  if (AFilterReg = nil) then
    locFilterManager := GetDataFilterRegistry()
  else
    locFilterManager := AFilterReg;
  locHeader := nil;
  locLast := nil;
  while True do begin
    locBuffer := Trim(GetToken(locAll,'&'));
    if IsStrEmpty(locBuffer) then
      Break;
    //The filter name
    locName := Trim(GetToken(locBuffer,','));
    if not locFilterManager.Find(locName,locFilter) then
      raise ETransportExecption.CreateFmt(SERR_DataFilterNotFound,[locName]);
    locPM := locFilter.GetPropertyManager();
    while True do begin
      locName := GetToken(locBuffer,'>');
      if IsStrEmpty(locName) then
        Break;
      locValue := GetToken(locBuffer,',');
      locPM.SetProperty(locName,locValue);
    end;
    if (locHeader = nil) then begin
      locHeader := locFilter;
      locLast := locFilter;
    end else begin
      locLast.SetNext(locFilter);
    end;
    locLast := locFilter;
  end;
  Result := locHeader;
end;   


function GenerateFilterString(AFilter : IDataFilter) : string;
var
  locFilter : IDataFilter;
  locPM : IPropertyManager;
  ls : TStringList;
  locResAll, locResLine, s : string;
  i : Integer;
begin
  locResAll := '';
  if ( AFilter <> nil ) then begin
    ls := TStringList.Create();
    try
      locFilter := AFilter;
      while (locFilter <> nil) do begin
        locResLine := locFilter.GetName();
        locPM := locFilter.GetPropertyManager();
        ls.Clear();
        if ( locPM.GetPropertyNames(ls) > 0 ) then begin
          for i := 0 to Pred(ls.Count) do begin
            s := ls[i];
            locResLine := Format('%s,%s>%s',[locResLine,s,locPM.GetProperty(s)]);
          end;
        end;
        if (locResAll = '') then
          locResAll := locResLine
        else
          locResAll := locResAll + '&' + locResLine;
        locFilter := locFilter.GetNext();
      end;
    finally
      ls.Free();
    end;
  end;
  Result := locResAll;
end;   

function ParseDataFilterString(AValue : string) : IDataFilter;
begin
  Result := ParseDataFilterString(AValue,GetDataFilterRegistry());
end;

{ TBaseFilter }

function TBaseFilter.GetPropertyManager() : IPropertyManager; 
begin
  Result := FPropertyManager;
end;

function TBaseFilter.ExecuteInput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
var
  n : IDataFilter;
  r : TByteDynArray;
begin
  Result := DoExecuteInput(AData,ASize,ADataProps);
  n := GetNext();
  if (n <> nil) then begin
    r := Result;
    Result := n.ExecuteInput(r[Low(r)],Length(r),ADataProps);
  end;
end;

function TBaseFilter.ExecuteOutput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
var
  n : IDataFilter;
  r : TByteDynArray;                    
begin
  n := GetNext();
  if (n = nil) then begin
    Result := DoExecuteOutput(AData,ASize,ADataProps);
  end else begin
    r := n.ExecuteOutput(AData,ASize,ADataProps);
    Result := DoExecuteOutput(r[Low(r)],Length(r),ADataProps);
  end;                                   
end;

function TBaseFilter.GetNext() : IDataFilter; 
begin
  Result := FNext;
end;

procedure TBaseFilter.SetNext(AItem : IDataFilter); 
begin
  if (FNext <> AItem) then
    FNext := AItem;
end;

constructor TBaseFilter.Create();  
begin
  inherited;
  FPropertyManager := TPublishedPropertyManager.Create(Self,True);
end;

{ TDataFilterRegistry }

function TDataFilterRegistry.Find(
  const AName : string;
  out   ARes  : IDataFilter
): Boolean;
var
  fct : IItemFactory;
begin
  fct := FindFactory(AName);
  if Assigned(fct) then begin
    ARes := fct.CreateInstance() as IDataFilter;
    Result := True;
  end else begin
    Result := False;
  end;
end;

end.
