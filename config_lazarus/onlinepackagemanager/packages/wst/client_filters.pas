{
    This file is part of the Web Service Toolkit
    Copyright (c) 2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$INCLUDE wst_global.inc}

unit client_filters;

interface

uses
  Classes, SysUtils,
  wst_types, base_service_intf, filter_intf;

const
  LOG_FILTER_NAME = 'LogFilter';

type

  { TLogFilter }

  TLogFilter = class(TBaseFilter)
  private
    FOutputDir : string;
  protected
    function DoExecuteInput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
    function DoExecuteOutput(
      const AData;
      const ASize      : Integer;
            ADataProps : IPropertyManager
    ) : TByteDynArray;override;
    procedure SaveToFile(const AData : TByteDynArray; const AFileName : string);
  published
    property OutputDir : string read FOutputDir write FOutputDir;
  end;

  procedure RegisterLogFilter();

implementation

procedure RegisterLogFilter();
begin
  GetDataFilterRegistry().Register(
    LOG_FILTER_NAME,TSimpleItemFactory.Create(TLogFilter)
  );
end;

{ TLogFilter }

function TLogFilter.DoExecuteInput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin
  SetLength(Result,ASize);
  if (ASize > 0) then
    Move(AData,Result[0],ASize);
  SaveToFile(Result,'request.log');
end;

function TLogFilter.DoExecuteOutput(
  const AData;
  const ASize      : Integer;
        ADataProps : IPropertyManager
) : TByteDynArray;
begin
  SetLength(Result,ASize);
  if (ASize > 0) then
    Move(AData,Result[0],ASize);
  SaveToFile(Result,'response.log');
end;

procedure TLogFilter.SaveToFile(
  const AData     : TByteDynArray;
  const AFileName : string
);
var
  locFileName : string;
  locStream : TMemoryStream;
begin
  locFileName := IncludeTrailingPathDelimiter(OutputDir) + AFileName;
  locStream := TMemoryStream.Create();
  try
    if (Length(AData) > 0) then
      locStream.Write(AData[0],Length(AData));
    locStream.SaveToFile(locFileName);
  finally
    locStream.Free();
  end;
end;

end.

