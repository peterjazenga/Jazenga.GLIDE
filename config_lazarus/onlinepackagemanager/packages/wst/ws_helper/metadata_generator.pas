{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2014 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit metadata_generator;

interface

uses
  Classes, SysUtils,
  pastree, pascal_parser_intf, binary_streamer;

type

  { TMetadataGenerator }

  TMetadataGenerator = class
  private
    FStream : IDataStore;
    FSymbolTable: TwstPasTreeContainer;

    procedure GenerateHeader();
    procedure GenerateIntfMetadata(AIntf : TPasClassType);
  public
    constructor Create(
      ASymTable   : TwstPasTreeContainer;
      ADstStream  : IDataStore
    );
    procedure Execute();
  end;


implementation
uses
  wst_consts;

{ TMetadataGenerator }

procedure TMetadataGenerator.GenerateHeader();
var
  c, i, k : LongInt;
  typeList : TList2;
  elt : TPasElement;
begin
  FStream.WriteAnsiStr(sWST_SIGNATURE);
  FStream.WriteAnsiStr(FSymbolTable.CurrentModule.Name);
  k := 0;
  typeList := FSymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := typeList.Count;
  for i := 0 to pred(c) do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then
      inc(k);
  end;
  FStream.WriteInt16U(k);
end;

procedure TMetadataGenerator.GenerateIntfMetadata(AIntf: TPasClassType);

  procedure WriteMethod(AMeth : TPasProcedure);

    procedure WriteParam(APrm : TPasArgument);
    begin
      FStream.WriteAnsiStr(APrm.Name);
      FStream.WriteAnsiStr(APrm.ArgType.Name);
      FStream.WriteEnum(Ord(APrm.Access));
    end;

    procedure WriteResult(ARes : TPasResultElement);
    begin
      FStream.WriteAnsiStr(ARes.Name);
      FStream.WriteAnsiStr(ARes.ResultType.Name);
      FStream.WriteEnum(Ord(argOut));
    end;

  var
    j, k : LongInt;
    argLst : TList2;
  begin
    argLst := AMeth.ProcType.Args;
    k := argLst.Count;
    FStream.WriteAnsiStr(AMeth.Name);
    if AMeth.InheritsFrom(TPasFunction) then begin
      FStream.WriteInt8U(k + 1);
    end else begin
      FStream.WriteInt8U(k);
    end;
    for j := 0 to pred(k) do begin
      WriteParam(TPasArgument(argLst[j]));
    end;
    if AMeth.InheritsFrom(TPasFunction) then begin
      WriteResult(TPasFunctionType(AMeth.ProcType).ResultEl);
    end;
  end;
  
var
  i, c : LongInt;
  mbrs : TList2;
  elt : TPasElement;
begin
  FStream.WriteAnsiStr(AIntf.Name);
  c := GetElementCount(AIntf.Members,TPasProcedure);
  FStream.WriteInt16U(c);
  mbrs := AIntf.Members;
  for i := 0 to pred(mbrs.Count) do begin
    elt := TPasElement(mbrs[i]);
    if elt.InheritsFrom(TPasProcedure) then begin
      WriteMethod(TPasProcedure(elt));
    end;
  end;
end;

constructor TMetadataGenerator.Create(ASymTable: TwstPasTreeContainer;ADstStream: IDataStore);
begin
  Assert(Assigned(ASymTable));
  Assert(Assigned(ADstStream));
  FSymbolTable := ASymTable;
  FStream := ADstStream;
end;

procedure TMetadataGenerator.Execute();
Var
  i,c : Integer;
  intf : TPasClassType;
  typeList : TList2;
  elt : TPasElement;
begin
  GenerateHeader();
  typeList := FSymbolTable.CurrentModule.InterfaceSection.Declarations;
  c := Pred(typeList.Count);
  for i := 0 to c do begin
    elt := TPasElement(typeList[i]);
    if elt.InheritsFrom(TPasClassType) and ( TPasClassType(elt).ObjKind = okInterface ) then begin
      intf := TPasClassType(elt);
      GenerateIntfMetadata(intf);
    end;
  end;
end;

end.

