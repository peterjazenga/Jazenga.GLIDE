{
    This file is part of the Web Service Toolkit
    Copyright (c) 2009-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit locators;

interface

uses
  Classes, SysUtils
{$IFDEF WST_DELPHI}
  , xmldom, wst_delphi_xml
{$ENDIF WST_DELPHI}
{$IFDEF FPC}
  , DOM, wst_fpc_xml
{$ENDIF FPC}
  ;

type

  IDocumentLocator = interface
    ['{F063700B-C0ED-4C54-9A54-C97030E80BD4}']
    function Find(
      const ADocLocation : string;
      out   ADoc : TXMLDocument
    ) : Boolean;
    function FindPath(ADocLocation : string) : string;

    function GetBasePath() : string;
    procedure SetBasePath(AValue : string);
    function Clone() : IDocumentLocator;
    function MakeRelavive(const AFileName : string) : string;
  end;

  { TFileDocumentLocator }

  TFileDocumentLocator = class(TInterfacedObject,IDocumentLocator)
  private
    FBasePath : string;
  protected
    function FindFileName(ADocLocation : string) : string;
    property BasePath : string read FBasePath;
  protected
    function Find(
      const ADocLocation : string;
      out   ADoc : TXMLDocument
    ) : Boolean;
    function FindPath(ADocLocation : string) : string;
    
    function GetBasePath() : string;
    procedure SetBasePath(AValue : string);
    function Clone() : IDocumentLocator;
    function MakeRelavive(const AFileName : string) : string;
  public
    constructor Create(const ABasePath : string);virtual;
  end;
  TFileDocumentLocatorClass = class of TFileDocumentLocator;
    
implementation

{ TFileDocumentLocator }

function TFileDocumentLocator.FindFileName(ADocLocation : string) : string; 
var
  locFileName : string;
begin
  //locFileName := BasePath + ExtractFileName(ADocLocation);
  locFileName := StringReplace(ADocLocation,'\',PathDelim,[rfIgnoreCase,rfReplaceAll]);
  locFileName := StringReplace(locFileName,'/',PathDelim,[rfIgnoreCase,rfReplaceAll]);
  
  locFileName := BasePath + locFileName;
  //locFileName := ExpandFileName(locFileName);
  if FileExists(locFileName) then
    Result := locFileName
  else
    Result := '';
end;

function TFileDocumentLocator.Find(
  const ADocLocation: string;
  out   ADoc: TXMLDocument
) : Boolean;
var
  locFileName : string;
begin
  locFileName := FindFileName(ADocLocation);
  Result := (locFileName <> '');
  if Result then
    ADoc := ReadXMLFile(locFileName);
end;

function TFileDocumentLocator.FindPath(ADocLocation : string) : string; 
begin
  Result := FindFileName(ADocLocation);
  if (Result <> '') then
    Result := ExtractFilePath(Result);
end;

function TFileDocumentLocator.GetBasePath() : string; 
begin
  Result := BasePath;
end;

procedure TFileDocumentLocator.SetBasePath(AValue : string); 
begin
  if (FBasePath <> AValue) then
    FBasePath := AValue;
end;

function TFileDocumentLocator.Clone() : IDocumentLocator; 
begin
  Result := TFileDocumentLocatorClass(Self.ClassType).Create(FBasePath) as IDocumentLocator;
end;

function TFileDocumentLocator.MakeRelavive(const AFileName: string): string;
begin
  Result := ExtractRelativePath(GetBasePath(),AFileName);
end;

constructor TFileDocumentLocator.Create(const ABasePath: string);
begin
  SetBasePath(IncludeTrailingPathDelimiter(ABasePath));
end;

end.

