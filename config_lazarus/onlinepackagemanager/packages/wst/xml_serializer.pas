{
    This file is part of the Web Service Toolkit
    Copyright (c) 2006-2015 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit xml_serializer;

interface

uses
  Classes, SysUtils, DOM,
  imp_utils, wst_fpc_xml, base_service_intf, service_intf, base_soap_formatter;

type

  IXmlFormatter = interface(IFormatterBase)
    ['{4383B991-D07A-4720-A418-572715233259}']
    procedure BeginScope(const AScopeName,ANameSpace : string);
    procedure PrepareForRead();
    function BeginScopeRead(const AScopeName, ANameSpace : string) : Integer;
  end;

  { TXmlFormatter }

  TXmlFormatter = class(TSOAPBaseFormatter,IFormatterBase,IXmlFormatter)
  private
  public
    constructor Create();override;
    procedure PrepareForRead();
    procedure BeginScope(const AScopeName,ANameSpace : string);
    function BeginScopeRead(const AScopeName, ANameSpace : string) : Integer;
  end;

implementation

{ TXmlFormatter }

constructor TXmlFormatter.Create();
begin
  inherited Create;
  Style := Document;
end;

procedure TXmlFormatter.PrepareForRead();
begin
  ClearStack();
  PushStack(GetXmlDoc().DocumentElement);
end;

procedure TXmlFormatter.BeginScope(const AScopeName, ANameSpace: string);
begin
  inherited BeginScope(AScopeName,ANameSpace,'',stObject,asNone);
end;

function TXmlFormatter.BeginScopeRead(const AScopeName, ANameSpace: string) : Integer;
var
  locNode : TDOMNode;
  stk : TStackItem;
  nmspc,nmspcSH : string;
  strNodeName : string;
begin
  if ( Style = Document ) then begin
    nmspc := ANameSpace;
    if IsStrEmpty(nmspc) then begin
      nmspcSH := ''
    end else begin
      nmspcSH := FindAttributeByValueInScope(nmspc);
      if not IsStrEmpty(nmspcSH) then begin
        nmspcSH := Copy(nmspcSH,Length('xmlns:')+1,MaxInt);
      end;
    end;
    if IsStrEmpty(nmspcSH) then begin
      strNodeName := AScopeName
    end else begin
      if ( Pos(':',AScopeName) < 1 ) then begin
        strNodeName := nmspcSH + ':' + AScopeName
      end else begin
        strNodeName := AScopeName;
      end;
    end;
  end else begin
    nmspcSH := '';
    strNodeName := AScopeName;
  end;

  stk := StackTop();
  locNode := stk.FindNode(strNodeName);
  if (locNode = nil) and (Style = Document) then
    locNode := FindXMLNodeWithNamespaceInSubScope(nmspc,AScopeName);

  if ( locNode = nil ) then begin
    Result := -1;
  end else begin
    PushStack(locNode);
    if ( Style = Document ) then begin
      StackTop().SetNameSpace(nmspc);
    end;
    Result := StackTop().ItemsCount;
  end;
end;

end.

