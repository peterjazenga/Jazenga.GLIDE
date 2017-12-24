unit generatorbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PasTree,
  source_utils, pascal_parser_intf, parserutils;

type

  TGeneratorOption = (
    goDocumentWrappedParameter { .Net style wrapped parameters },
    goGenerateDocAsComments    { Documentation include in the XSD/WSDL schema will be generated as comments },
    goGenerateObjectCollection { Generate object "collection" instead of "array" },
    goCreateChoiceFieldsInConstructor
  );
  TGeneratorOptions = set of TGeneratorOption;

  { TBaseGenerator }

  TBaseGenerator = class
  Private
    FOptions : TGeneratorOptions;
    FSrcMngr  : ISourceManager;
    FCurrentStream : ISourceStream;
    FSymbolTable: TwstPasTreeContainer;
    FMainModule : TPasModule;
  Protected
    procedure SetCurrentStream(AStream : ISourceStream);
    procedure Indent();
    function IncIndent():Integer;
    function DecIndent():Integer;
    procedure BeginAutoIndent();
    procedure EndAutoIndent();
    procedure Write(AText : String);overload;
    procedure Write(AText : String; Const AArgs : array of const);overload;
    procedure WriteLn(AText : String);overload;
    procedure WriteLn(AText : String; Const AArgs : array of const);overload;
    procedure NewLine();

    function ExtractserviceName(AIntf : TPasElement):String;
    function GenerateExtraUses() : string;
  Public
    constructor Create(
      ASymTable : TwstPasTreeContainer;
      ASrcMngr  : ISourceManager
    );
    procedure Execute();virtual;abstract;
    property SymbolTable : TwstPasTreeContainer Read FSymbolTable;
    property SrcMngr : ISourceManager Read FSrcMngr;
    property Options : TGeneratorOptions read FOptions write FOptions;
    property MainModule : TPasModule read FMainModule;
  End;

implementation


{ TBaseGenerator }

procedure TBaseGenerator.SetCurrentStream(AStream: ISourceStream);
begin
  FCurrentStream := AStream;
end;

procedure TBaseGenerator.Indent();
begin
  FCurrentStream.Indent();
end;

function TBaseGenerator.IncIndent():Integer;
begin
  Result := FCurrentStream.IncIndent();
end;

function TBaseGenerator.DecIndent():Integer;
begin
  Result := FCurrentStream.DecIndent();
end;

procedure TBaseGenerator.BeginAutoIndent();
begin
  FCurrentStream.BeginAutoIndent();
end;

procedure TBaseGenerator.EndAutoIndent();
begin
  FCurrentStream.EndAutoIndent();
end;

procedure TBaseGenerator.Write(AText: String);
begin
  FCurrentStream.Write(AText);
end;

procedure TBaseGenerator.Write(AText: String; const AArgs: array of const);
begin
  Write(Format(AText,AArgs));
end;

procedure TBaseGenerator.WriteLn(AText: String);
begin
  Write(AText+sNEW_LINE);
end;

procedure TBaseGenerator.WriteLn(AText: String; const AArgs: array of const);
begin
  Write(AText+sNEW_LINE,AArgs);
end;

procedure TBaseGenerator.NewLine();
begin
  WriteLn('');
end;

function TBaseGenerator.ExtractserviceName(AIntf: TPasElement): String;
begin
  Result := AIntf.Name;
  If upCase(Result[1]) = 'I' Then
    Delete(Result,1,1);
end;

function TBaseGenerator.GenerateExtraUses() : string;
var
  locUsesList : TList2;
  locModule : TPasElement;
  i : Integer;
begin
  Result := '';
  locUsesList := SymbolTable.CurrentModule.InterfaceSection.UsesList;
  if (locUsesList.Count > 0) then begin
    for i := 0 to Pred(locUsesList.Count) do begin
      locModule := TPasElement(locUsesList[i]);
      Result := Result + ', ' +  locModule.Name;
    end;
    if ( Length(Result) > 0 ) then
      Delete(Result,1,2);
  end;
end;

constructor TBaseGenerator.Create(ASymTable: TwstPasTreeContainer; ASrcMngr: ISourceManager);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ASrcMngr));
  FSrcMngr :=ASrcMngr;
  FCurrentStream := Nil;
  FSymbolTable := ASymTable;
  FMainModule := FSymbolTable.CurrentModule;
end;

end.

