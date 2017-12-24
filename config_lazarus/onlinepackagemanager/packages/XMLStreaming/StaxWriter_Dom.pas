{ This file is part of the fpStax.

  Copyright (C) 2013-2015 Daniel F. Gaspary https://github.com/dgaspary

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the copyright.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit StaxWriter_Dom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StaxWriter, StaxCommon, Laz2_DOM;

type

    { TDomStaxWriter }

    TDomStaxWriter = class
    private
           FStaxWriter: TXmlStreamWriter;
           FDoc: TXMLDocument;

           FCollapseEmptyElements: boolean;
           FXmlEncoding: string;
           FXmlVersion: string;

           function GetNamespaceInfo(node: TDOMNode; out Prefix: string;
                out NsUri: string;
                out localname: string): boolean;

           function GetRepairingNamespaces: boolean;
           procedure SetRepairingNamespaces(AValue: boolean);
           procedure WriteElement(e: TDOMElement);
           procedure WriteTextNode(tn: TDOMText);
    public
          Constructor Create(XmlDoc: TXMLDocument; OutputStream: TStream);
          destructor Destroy; override;

          procedure WriteToStream;

          property RepairingNamespaces: boolean
                           read GetRepairingNamespaces
                           write SetRepairingNamespaces;

          property CollapseEmptyElements: boolean
                           read FCollapseEmptyElements
                           write FCollapseEmptyElements;

          property XmlEncoding: string read FXmlEncoding write FXmlEncoding;
          property XmlVersion: string read FXmlVersion write FXmlVersion;
    end;



procedure XmlStreamWrite(AXmlDoc: TXMLDocument; AFile: string);
procedure XmlStreamWrite(AXmlDoc: TXMLDocument; AFile, AEncoding, AXmlVersion: string);

implementation

procedure XmlStreamWrite(AXmlDoc: TXMLDocument; AFile: string);
begin
     XmlStreamWrite(AXmlDoc, AFile, AXmlDoc.Encoding, AXmlDoc.XMLVersion);
end;

procedure XmlStreamWrite(AXmlDoc: TXMLDocument; AFile, AEncoding,
  AXmlVersion: string);
var
   fs: TFileStream;
begin
     fs:=TFileStream.Create(AFile, fmCreate + fmOpenWrite);

     with TDomStaxWriter.Create(AXmlDoc, fs) do
     begin
          XmlEncoding:=AEncoding;
          XmlVersion:=AXmlVersion;

          CollapseEmptyElements:=true;

          WriteToStream;
          Free;
     end;

     fs.Free;
end;

{ TDomStaxWriter }

function TDomStaxWriter.GetNamespaceInfo(node: TDOMNode; out Prefix: string;
  out NsUri: string; out localname: string): boolean;
begin
     Prefix:=node.Prefix;
     NsUri:=node.NamespaceURI;

     Result:=(Prefix <> '');

     if Result
     then
     begin
          localname:=node.LocalName;
     end
     else
     begin
          localname:=node.NodeName;
     end;
end;

function TDomStaxWriter.GetRepairingNamespaces: boolean;
begin
     result:=FStaxWriter.RepairingNamespaces;
end;

procedure TDomStaxWriter.SetRepairingNamespaces(AValue: boolean);
begin
     FStaxWriter.RepairingNamespaces:=AValue;
end;

procedure TDomStaxWriter.WriteElement(e: TDOMElement);
var
   nsUri: string;
   prefix: string;
   localname: string;

   atts: TDOMNamedNodeMap;
   att: TDOMAttr;

   i: integer;

   node: TDOMNode;

begin
     if GetNamespaceInfo(e, prefix, nsUri, localname)
     then
     begin
          if (not e.HasChildNodes) and FCollapseEmptyElements
          then
              FStaxWriter.WriteEmptyElement(prefix, nsUri, localname)
          else
              FStaxWriter.WriteStartElement(prefix, nsUri, localname);
     end
     else
     begin
          if e.IsEmpty
          then
              FStaxWriter.WriteEmptyElement(localname)
          else
              FStaxWriter.WriteStartElement(localname);
     end;

     atts:=e.Attributes;
     for i:=0 to Pred(atts.Length) do
     begin
          att:=atts[i] as TDOMAttr;

          if GetNamespaceInfo(att, prefix, nsUri, localname)
          then
          begin
               if (prefix=TXmlConstants.XMLNS_ATTRIBUTE) and (nsUri=TXmlConstants.XMLNS_ATTRIBUTE_NS_URI)
               then
                   FStaxWriter.setPrefix(localname, att.Value)
               else
                   FStaxWriter.writeAttribute(prefix, nsUri, localname, att.Value)
          end
          else
              FStaxWriter.writeAttribute(localname, att.Value);

     end;

     if e.HasChildNodes
     then
     begin
          node:=e.FirstChild;

          while Assigned(node) do
          begin
               case node.NodeType of
                  ELEMENT_NODE: WriteElement(node as TDOMElement);
                  TEXT_NODE   : WriteTextNode(node as TDOMText);
               end;

               node:=node.NextSibling;
          end;
     end;

     FStaxWriter.WriteEndElement;
end;

procedure TDomStaxWriter.WriteTextNode(tn: TDOMText);
begin
     FStaxWriter.WriteCharacters(tn.NodeValue);
end;

constructor TDomStaxWriter.Create(XmlDoc: TXMLDocument; OutputStream: TStream);
begin
     FDoc:=XmlDoc;

     FXmlEncoding:=XML_DEFAULT_ENCODING;
     FXmlVersion:=XML_DEFAULT_VERSION;

     FStaxWriter:=TXmlStreamWriter.Create(OutputStream);
end;

destructor TDomStaxWriter.Destroy;
begin
     with FStaxWriter do
     begin
          flush;
          close;
          Free;
     end;

     inherited Destroy;
end;

procedure TDomStaxWriter.WriteToStream;
begin
     FStaxWriter.writeStartDocument(FXmlEncoding, FXmlVersion);
     WriteElement(FDoc.DocumentElement);
     FStaxWriter.writeEndDocument;
end;

end.

