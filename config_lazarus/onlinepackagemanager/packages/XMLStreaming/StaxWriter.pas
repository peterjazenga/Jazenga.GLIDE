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

unit StaxWriter;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, fgl, laz2_xmlutils, StaxFormatting;

type
    StaxString = AnsiString;

    EXMLStreamException = class(Exception);
    EStaxIllegalStateException = class(Exception);

    { TNamespaceBinding }

    TNamespaceBinding = class
         Uri,
         Prefix: string;

         constructor Create(AUri, APrefix: string);
         function Equals(Obj: TObject): boolean; override;
         function Equals(AUri, APrefix: String): boolean;
    end;


    { TNamespaceBindingList }

    TNamespaceBindingList = class(specialize TFPGList<TNamespaceBinding>)
    private
           FParent: TNamespaceBindingList;
           PrefixIndex: integer;
           procedure CreateAndAddBinding(NamespaceURI, Prefix: string;
             var NewBinding: TNamespaceBinding; AddWhenNeeded: boolean);

    public
          constructor Create(AParent: TNamespaceBindingList);
          constructor Create;

          destructor Destroy; override;

          function CreatePrefix(NamespaceURI: string): String; virtual;

          function GetBindingByURI(NamespaceURI: string; out Created: boolean;
                                   AddWhenNeeded: boolean = true): TNamespaceBinding;

          function SearchBinding(Prefix, NamespaceURI: string
             ): TNamespaceBinding;

          function GetBinding(Prefix, NamespaceURI: string; out Created: boolean;
                              CreateWhenNeeded: boolean = true): TNamespaceBinding;

          function IsBinded(Prefix, NamespaceURI: string): boolean;

          function GetPrefix(NamespaceURI: string): string;
          property Parent: TNamespaceBindingList read FParent write FParent;
    end;


    { TNameSpaceContext }

    TNameSpaceContext = class

         function GetNamespaceURI(Prefix: String): string;
         function GetPrefix(NamespaceURI: String): string;
         //function GetPrefixes(NamespaceURI: String): string;
    end;


    { TStaxScope }

    TStaxScope = class
        written: boolean;

        { TODO : List with namespaces context }
        constructor create;
    end;

    { TNamedScope }

    TNamedScope = class(TStaxScope)

    private
           FBinding: TNamespaceBinding;

    public
       Name: string;

       constructor Create(ABinding: TNamespaceBinding; AName: string);
       constructor Create(NamespaceUri, Prefix, LocalName: string);

       function GetBinding: TNamespaceBinding;
       function GetPrefix: String;

       property Binding: TNamespaceBinding read FBinding write FBinding;
    end;

    { TElementScope }

    TElementScope = class(TNamedScope)
    private
      FEmpty: boolean;
    public

       FNsList: TNamespaceBindingList;

       constructor Create(ABinding: TNamespaceBinding; AName: string);
       constructor Create(NamespaceUri, Prefix, LocalName: string);

       procedure SetParentBindingList(AList: TNamespaceBindingList);

       property BindingsList: TNamespaceBindingList read FNsList write FNsList;
       property Empty: boolean read FEmpty Write FEmpty;
    end;

    TAttributeScope = class(TNamedScope)
       Value: string;
    end;

    TNamespaceScope = class(TStaxScope)

    end;


    { TScopesList }

    TScopesList = class(specialize TFPGObjectList<TStaxScope>)

       procedure RemoveWrittenScopes(StartIndex: integer);
    end;

    { TXmlStreamWriter }

    TXmlStreamWriter = class
    private
           FDefaultNamespace: TNamespaceBinding;
           FRepairingNamespaces: boolean;
           FRootBindings: TNamespaceBindingList;

           FScopesList : TScopesList;

           FCurrentElementBinding: TNamespaceBinding;
           FParentElement: TElementScope;

           FStream: TStream;

           function ConvChars(text: string): string;
           function GetDefaultNamespace: TNamespaceBinding;

           procedure CheckElement(Prefix, NamespaceURI, LocalName: String);
           function FormatStartElement(ElementScope: TElementScope): StaxString;
           function FormatEndElement(ElementScope: TElementScope): StaxString;

           function FormatAttribute(AttScope: TAttributeScope): StaxString;


           function CreateNamedScope(Prefix, NamespaceURI, LocalName: String;
                     AScope: TNamedScope = nil): TNamedScope;
           function CreateNamedScope(Binding: TNamespaceBinding;
             LocalName: String; AScope: TNamedScope): TNamedScope;

           function CreateElementScope(Prefix, NamespaceURI, LocalName: String;
             ParentScope: TElementScope; Empty: boolean): TElementScope;

           function CreateAttributeScope(Prefix, NamespaceURI, LocalName, Value: String): TAttributeScope;
           function FormatStartElementClosing(Empty: boolean): String;
           procedure PushStartElement(Prefix, NamespaceURI,
             LocalName: String; Empty: boolean);
           function LastElementScope: TElementScope;
           procedure RaiseNamespaceUriNotBound;
           function WithinElementScope(out ElementScope: TElementScope
             ): boolean;
           function WithinElementScope: boolean;
           procedure WriteAtributeScope(AttributeScope: TAttributeScope);
           procedure WriteElementScope(ElementScope: TElementScope);
           procedure WriteElementScope_End(ElementScope: TElementScope);

           procedure WriteToStream(text: String);
    public
          constructor Create(OutputStream: TStream);
          destructor Destroy; override;

          procedure close; virtual;
          procedure flush; virtual;
          function getNamespaceContext: TNameSpaceContext; virtual; abstract;
          function getPrefix(uri: String): String; virtual;
          function getProperty(name: String): TObject; virtual; abstract;
          procedure setDefaultNamespace(uri: String); virtual;
          procedure setNamespaceContext(context: TNameSpaceContext); virtual;
          procedure setPrefix(prefix, uri: String); virtual;
          procedure writeAttribute(localName, value: String); virtual;
          procedure writeAttribute(namespaceURI, localName, value: String); virtual;
          procedure WriteAttribute(prefix, namespaceURI, localName, value: String); virtual;
          procedure writeCData(data: String); virtual; abstract;
          procedure WriteCharacters(text: array of char; start, len: integer); virtual;
          procedure WriteCharacters(text: String); virtual;
          procedure writeComment(data: String); virtual; abstract;
          procedure WriteDefaultNamespace(namespaceURI: String); virtual;
          procedure writeDTD(dtd: String); virtual; abstract;
          procedure WriteEmptyElement(localName: String); virtual;
          procedure WriteEmptyElement(namespaceURI, localName: String); virtual;
          procedure WriteEmptyElement(prefix, namespaceURI, localName: String); virtual;
          procedure writeEndDocument; virtual;

          procedure WriteEndElement; virtual;

          procedure writeEntityRef(name: String); virtual; abstract;
          procedure writeNamespace(prefix, namespaceURI: String); virtual; abstract;
          procedure writeProcessingInstruction(target: String); virtual; abstract;
          procedure writeProcessingInstruction(data, target: String); virtual; abstract;
          procedure writeStartDocument; virtual;
          procedure writeStartDocument(version: String); virtual;
          procedure writeStartDocument(encoding, version: String); virtual;

          procedure WriteStartElement(LocalName: String); virtual;
          procedure WriteStartElement(NamespaceURI, LocalName: String); virtual;
          procedure WriteStartElement(Prefix, NamespaceURI, LocalName: String); virtual;

          property RepairingNamespaces: boolean read FRepairingNamespaces write FRepairingNamespaces;
    end;


implementation

uses
    StaxCommon;

procedure RaiseNYI;
begin
     raise Exception.Create('Not Yet implemented');
end;

{ TNameSpaceContext }

function TNameSpaceContext.GetNamespaceURI(Prefix: String): string;
begin
     RaiseNYI;
end;

function TNameSpaceContext.GetPrefix(NamespaceURI: String): string;
begin
     RaiseNYI;
end;

{ TElementScope }

constructor TElementScope.Create(ABinding: TNamespaceBinding; AName: string);
begin
     inherited;

     FNsList:=nil;
end;

constructor TElementScope.Create(NamespaceUri, Prefix, LocalName: string);
begin
     inherited;

     FNsList:=nil;
end;

procedure TElementScope.SetParentBindingList(AList: TNamespaceBindingList);
begin
     if Assigned(FNsList)
     then
         FNsList.Parent:=AList
     else
         FNsList:=TNamespaceBindingList.Create(AList);
end;

constructor TNamedScope.Create(ABinding: TNamespaceBinding; AName: string);
begin
     inherited create;

     FBinding:=ABinding;

     Name:=AName;
end;

constructor TNamedScope.Create(NamespaceUri, Prefix,  LocalName: string);
begin
     Create(TNamespaceBinding.Create(NamespaceURI, Prefix),
    LocalName);
end;

function TNamedScope.GetBinding: TNamespaceBinding;
begin
     result:=FBinding;
end;

function TNamedScope.GetPrefix: String;
begin
     Result:=GetBinding.Prefix;
end;

{ TNamespaceBindingList }

constructor TNamespaceBindingList.Create(AParent: TNamespaceBindingList);
begin
     inherited create;

     FParent:=AParent;
     PrefixIndex:=0;
end;

constructor TNamespaceBindingList.Create;
begin
     Create(nil);
end;

destructor TNamespaceBindingList.Destroy;
begin
     inherited Destroy;
end;

function TNamespaceBindingList.CreatePrefix(NamespaceURI: string): String;
begin
     Result:='ns' + IntToStr(PrefixIndex);
     Inc(PrefixIndex);
end;


procedure TNamespaceBindingList.CreateAndAddBinding(NamespaceURI,
  Prefix: string; var NewBinding: TNamespaceBinding; AddWhenNeeded: boolean);
var
   vPrefix: string;
begin
     if not AddWhenNeeded
     then
         exit;

     if not Assigned(NewBinding)
     then
     begin
          if Prefix=''
          then
              vPrefix:=CreatePrefix(NamespaceURI)
          else
              vPrefix:=Prefix;

          NewBinding:=TNamespaceBinding.Create(NamespaceURI, vPrefix);
          Add(NewBinding);
     end;
end;

function TNamespaceBindingList.GetBindingByURI(NamespaceURI: string; out
  Created: boolean; AddWhenNeeded: boolean): TNamespaceBinding;
var
   i: integer;
begin
     Result:=nil;
     for i:=Pred(count) to 0 do
     begin
          if Items[i].Uri = NamespaceURI
          then
          begin
               Result:=Items[i];
               break;
          end;
     end;

     if not Assigned(Result) and Assigned(FParent)
     then
         Result:=FParent.GetBindingByURI(NamespaceURI, Created);

     Created:=not Assigned(Result);
     CreateAndAddBinding(NamespaceURI, CreatePrefix(NamespaceURI), Result, AddWhenNeeded);

     Created:=Created and Assigned(Result);
end;


function TNamespaceBindingList.SearchBinding(Prefix, NamespaceURI: string): TNamespaceBinding;
var
   i: integer;
   b: TNamespaceBinding;
begin
     Result:=nil;
     for i:=Pred(count) downto 0 do
     begin
          b:=Items[i];

          if b.Equals(NamespaceURI, Prefix)
          then
          begin
               Result:=b;
               break;
          end;
     end;
end;

function TNamespaceBindingList.GetBinding(Prefix, NamespaceURI: string; out
  Created: boolean; CreateWhenNeeded: boolean): TNamespaceBinding;
var
   i: integer;
   b: TNamespaceBinding;
begin
     Created:=false;
{     Result:=nil;
     for i:=Pred(count) downto 0 do
     begin
          b:=Items[i];

          if b.Equals(NamespaceURI, Prefix)
          then
          begin
               Result:=b;
               break;
          end;
     end;}

     Result:=SearchBinding(Prefix, NamespaceURI);

     if not Assigned(Result) and Assigned(FParent)
     then
     begin
          Result:=FParent.GetBinding(Prefix, NamespaceURI, Created, false);
          if Assigned(Result)
          then
              exit;
     end;

     Created:=not Assigned(Result);
     CreateAndAddBinding(NamespaceURI, Prefix, Result,
                         CreateWhenNeeded);
     Created:=Created and Assigned(Result);
end;

function TNamespaceBindingList.IsBinded(Prefix, NamespaceURI: string): boolean;
var
   vCreated: boolean;
begin
     Result:=(nil <> GetBinding(Prefix, NamespaceURI, vCreated));
end;

function TNamespaceBindingList.GetPrefix(NamespaceURI: string): string;
var
   b: TNamespaceBinding;
   Created: boolean; //Not used
begin
     Result:='';

     b:=GetBindingByURI(NamespaceURI, Created);

     if Assigned(b)
     then
         Result:=b.Prefix;
end;

{ TNamespaceBinding }

constructor TNamespaceBinding.Create(AUri, APrefix: string);
begin
     Uri:=AUri;
     Prefix:=APrefix;
end;

function TNamespaceBinding.Equals(Obj: TObject): boolean;
var
   b: TNamespaceBinding;
begin
     Result:=inherited Equals(Obj);

     if Result
     then
         exit;

     if Obj is TNamespaceBinding
     then
     begin
          b:=Obj as TNamespaceBinding;
          Result:=Equals(b.Uri, b.Prefix);//(self.Prefix = b.Prefix) and (self.Uri=b.Uri);
     end;
end;

function TNamespaceBinding.Equals(AUri, APrefix: String): boolean;
begin
     Result:=(Uri = AUri) and (Prefix=APrefix);
end;

{ TScopesList }

procedure TScopesList.RemoveWrittenScopes(StartIndex: integer);
var
   i: integer;
begin
     Clear;
     for i:=Count-1 downto StartIndex do
     begin
          Delete(i);
     end;
end;


{ TStaxScope }

constructor TStaxScope.create;
begin
     written:=false;
end;

{ TXmlStreamWriter }


function TXmlStreamWriter.FormatStartElement(ElementScope: TElementScope
  ): StaxString;
var
   b: TNamespaceBinding;
   vPrefix: string;
begin
     with ElementScope do
     begin
          b:=GetBinding;

          vPrefix:=b.Prefix;

          if vPrefix = TXmlConstants.XML_NS_PREFIX
          then
              vPrefix:='';

          Result:=XmlFormatElementStart(vPrefix, Name, Empty, false);
     end;
end;

function TXmlStreamWriter.FormatEndElement(ElementScope: TElementScope
  ): StaxString;
var
   vPrefix: string;
begin
     with ElementScope do
     begin
          vPrefix:=GetPrefix;

          if vPrefix = TXmlConstants.XML_NS_PREFIX
          then
              vPrefix:='';

          Result:=XmlFormatElementEnd(vPrefix, Name);
     end;
end;

function TXmlStreamWriter.FormatAttribute(AttScope: TAttributeScope
  ): StaxString;
begin
     with AttScope do
          Result:=XmlFormatAttribute(GetPrefix, Name, Value);
end;

function TXmlStreamWriter.CreateNamedScope(Prefix, NamespaceURI,
  LocalName: String; AScope: TNamedScope): TNamedScope;
begin
     Result:=CreateNamedScope(TNamespaceBinding.Create(NamespaceURI, Prefix),
         LocalName, AScope);
end;

function TXmlStreamWriter.CreateNamedScope(Binding: TNamespaceBinding;
  LocalName: String; AScope: TNamedScope): TNamedScope;
begin
     RaiseNYI;
{     if Assigned(AScope)
     then
         Result:=AScope
     else
         Result:=TNamedScope.Create;

     Result.Binding:=Binding;

     Result.Name:=LocalName;}
end;


function TXmlStreamWriter.CreateElementScope(Prefix, NamespaceURI,
             LocalName: String; ParentScope: TElementScope;
             Empty: boolean): TElementScope;
var
//   ParentScope: TElementScope;
   binding: TNamespaceBinding;

   BindingList, ParentBindingList: TNamespaceBindingList;

begin
//     ParentScope:=LastElementScope;

     Result:=TElementScope.create(NamespaceURI, Prefix, LocalName);

     Result.Empty:=Empty;

     ParentBindingList:=nil;
     if assigned(ParentScope)
     then
     begin
          ParentBindingList:=ParentScope.BindingsList;
     end;

     Result.BindingsList:=TNamespaceBindingList.Create(ParentBindingList);


     FScopesList.Add(Result);
end;

function TXmlStreamWriter.CreateAttributeScope(Prefix, NamespaceURI, LocalName,
  Value: String): TAttributeScope;
begin
{     Result:=CreateNamedScope(Prefix, NamespaceURI, LocalName, TAttributeScope.Create) as TAttributeScope;
     Result.Value:=Value;}

     Result:=TAttributeScope.Create(NamespaceURI, Prefix, LocalName);

     Result.Value:=Value;

     FScopesList.Add(Result);
end;

function TXmlStreamWriter.FormatStartElementClosing(Empty: boolean): String;
begin
     RaiseNYI;
end;

function TXmlStreamWriter.GetDefaultNamespace: TNamespaceBinding;
begin
     result:=FDefaultNamespace;
end;

procedure TXmlStreamWriter.CheckElement(Prefix, NamespaceURI, LocalName: String);
var
  ParamName: string;
begin
  ParamName:='';

      if Prefix=''
      then
          ParamName:='Prefix'
      else if NamespaceURI=''
      then
          ParamName:='NamespaceURI'
      else if LocalName=''
      then
          ParamName:='LocalName';

      if ParamName<>''
      then
      begin
           raise EXMLStreamException.Create(ParamName + ' cannot be empty.');
      end;
end;

constructor TXmlStreamWriter.Create(OutputStream: TStream);
begin
     FStream:=OutputStream;

     with TXmlConstants do
          FDefaultNamespace:=TNamespaceBinding.Create(XML_NS_URI, XML_NS_PREFIX);

     FRootBindings:=TNamespaceBindingList.Create;

     with FRootBindings do
     { TODO : Remove the debugging "IF" }

{         if Assigned(FRootBindings) and (Count>-1) and (CreatePrefix('abcd')<>'')
         then}
             Add(FDefaultNamespace);

     FParentElement:=nil;

     FCurrentElementBinding:=FDefaultNamespace;

     FScopesList:=TScopesList.Create;
end;

destructor TXmlStreamWriter.Destroy;
begin
     FScopesList.Free;
     FRootBindings.Free;

     inherited Destroy;
end;

procedure TXmlStreamWriter.close;
begin
     //RaiseNYI;
end;

procedure TXmlStreamWriter.flush;
var
   vScope: TStaxScope;
begin
     //RaiseNYI;

     vScope:=LastElementScope;

     if Assigned(vScope)
     then
     begin
          WriteElementScope(vScope as TElementScope);
     end;

     { TODO : Need to to check and write all scopes. }
end;

function TXmlStreamWriter.getPrefix(uri: String): String;
var
   e: TElementScope;
begin
{     e:=LastElementScope;

     if Assigned(e)
     then
         result:=e.GetPrefix
     else
}         Result:=FRootBindings.GetPrefix(uri);
         //result:=GetDefaultNamespace.Prefix;

end;

procedure TXmlStreamWriter.setDefaultNamespace(uri: String);
begin
     RaiseNYI;
end;

procedure TXmlStreamWriter.setNamespaceContext(context: TNameSpaceContext);
begin
     if not Assigned(context)
     then
         raise EXMLStreamException.Create('"context" Cannot be nil');
end;

procedure TXmlStreamWriter.setPrefix(prefix, uri: String);
var
  ElementScope: TElementScope;
  binding: TNamespaceBinding;
begin
     ElementScope:=LastElementScope;


     if not Assigned(ElementScope) or ElementScope.written
     then
         raise EStaxIllegalStateException.Create('Cannot Write an Attribute without an Element at scope.');

     binding:=ElementScope.BindingsList.SearchBinding(Prefix, uri);

     if Assigned(binding)
     then
         exit;

     ElementScope.BindingsList.CreateAndAddBinding(uri, prefix, binding, true);

     if Assigned(binding) {and FRepairingNamespaces} { TODO : Uncomment }
     then
         CreateAttributeScope(TXmlConstants.XMLNS_ATTRIBUTE, '', prefix, uri);
end;

procedure TXmlStreamWriter.writeAttribute(localName, value: String);
var
   binding: TNamespaceBinding;
begin
     binding:=GetDefaultNamespace;

     with binding do
          WriteAttribute(Prefix, Uri, localName, value);
end;

procedure TXmlStreamWriter.writeAttribute(namespaceURI, localName, value: String
  );
begin
     WriteAttribute('somePrefix', namespaceURI, localName, value);
end;

function TXmlStreamWriter.LastElementScope: TElementScope;
var
   i: integer;
   s: TStaxScope;
begin
     Result:=nil;

     for i:=Pred(FScopesList.Count) downto 0 do
     begin
          s:=FScopesList[i];

          if s is TElementScope
          then
          begin
               Result:=s as TElementScope;
               break;
          end;
     end;

end;

function TXmlStreamWriter.WithinElementScope: boolean;
var
   dummy: TElementScope;
begin
     Result:=WithinElementScope(dummy);
end;

function TXmlStreamWriter.WithinElementScope(out ElementScope: TElementScope): boolean;
var
   i: integer;
   scope: TStaxScope;
begin
     Result:=false;
     ElementScope:=nil;

     for i:=Pred(FScopesList.Count) downto 0 do
     begin
          scope:=FScopesList[i];

          if scope is TAttributeScope
          then
              continue;

          { TODO : Process other code node types. }

          if scope is TElementScope
          then
          begin
               Result:=not (scope as TElementScope).written;

               if Result
               then
               begin
                    ElementScope:=scope as TElementScope;
               end;

               break;
          end;
     end;

end;


procedure TXmlStreamWriter.RaiseNamespaceUriNotBound;
begin
     raise EXMLStreamException.Create('Namespace Uri not bound (Repair Namespaces: '+
     BoolToStr(FRepairingNamespaces, true) + ')');
end;

procedure TXmlStreamWriter.WriteAttribute(prefix, namespaceURI, localName,
  value: String);
var
   ElementScope, AttScope: TElementScope;
   binding: TNamespaceBinding;
   bindingCreated: boolean;
begin
     ElementScope:=LastElementScope;

     if not Assigned(ElementScope) or ElementScope.written
     then
         raise EStaxIllegalStateException.Create('Cannot Write an Attribute without an Element at scope.');


     binding:=ElementScope.BindingsList.GetBinding(Prefix, NamespaceURI, bindingCreated, true);

     if bindingCreated {and FRepairingNamespaces} { TODO : uncomment? }
     then
         CreateAttributeScope(TXmlConstants.XMLNS_ATTRIBUTE, '', binding.Prefix, NamespaceURI);

     CreateAttributeScope(prefix, namespaceURI, localName, value);
end;

procedure TXmlStreamWriter.WriteCharacters(text: array of char; start,
  len: integer);
begin

end;

function TXmlStreamWriter.ConvChars(text: string): string;
const
     cAmp = '&amp;';
     cQuote = '&quot;';
     cLt = '&lt;';
     cGt = '&gt;';
var

   i: integer;
   c: Char;

   s: string;
begin
     Result:='';

     for i:=1 to Length(text) do
     begin
          c:=text[i];

          case c of
               '&': s:=cAmp;
               '''': s:=cQuote; { TODO : Double quotes? }
               '<': s:=cLt;
               '>': s:=cGt;
               else s:=c;
          end;

          Result+=s;
     end;
end;

procedure TXmlStreamWriter.WriteCharacters(text: String);
var
   ParentScope: TElementScope;
begin
     ParentScope:=LastElementScope;

     if Assigned(ParentScope)
     then
     begin
          WriteElementScope(ParentScope);
     end;

     WriteToStream(ConvChars(text));
end;

procedure TXmlStreamWriter.WriteDefaultNamespace(namespaceURI: String);
var
   ElementScope: TElementScope;
begin
     if not WithinElementScope(ElementScope)
     then
         raise EXMLStreamException.Create('No element in scope');

     CreateAttributeScope('', '', TXmlConstants.XMLNS_ATTRIBUTE, namespaceURI);
end;

procedure TXmlStreamWriter.WriteEmptyElement(localName: String);
begin
     with GetDefaultNamespace do
          WriteEmptyElement(Prefix, Uri, LocalName);
end;

procedure TXmlStreamWriter.WriteEmptyElement(namespaceURI, localName: String);
begin
     WriteEmptyElement(getNamespaceContext.GetPrefix(NamespaceURI),
                       NamespaceURI,
                       LocalName);
end;

procedure TXmlStreamWriter.WriteEmptyElement(prefix, namespaceURI,
  localName: String);
var
   ParentScope: TElementScope;
begin
     PushStartElement(prefix, namespaceURI, localName, true);

{     CheckElement(Prefix, NamespaceURI, LocalName);

     ParentScope:=LastElementScope;

     WriteElementScope(ParentScope);

     CreateElementScope(Prefix, NamespaceURI, LocalName, ParentScope, true);}
end;

procedure TXmlStreamWriter.writeEndDocument;
var
   e: TElementScope;
begin
     e:=LastElementScope;
{     if FElementScopes.Count>0
     then
     begin
          WriteElementScope_End(FElementScopes.Last);
     end;}

     if Assigned(e)
     then
         WriteElementScope_End(e);
end;

procedure TXmlStreamWriter.WriteEndElement;
var
   scope: TElementScope;
begin
     scope:=LastElementScope;

     if not Assigned(scope)
     then
         exit;

     if not scope.written
     then
         WriteElementScope(scope);

     if not scope.Empty
     then
         WriteElementScope_End(scope);

     FScopesList.Remove(scope);
end;

procedure TXmlStreamWriter.writeStartDocument;
begin
     writeStartDocument(XML_DEFAULT_ENCODING, XML_DEFAULT_VERSION);
end;

procedure TXmlStreamWriter.writeStartDocument(version: String);
begin
     writeStartDocument(XML_DEFAULT_ENCODING, version);
end;

procedure TXmlStreamWriter.writeStartDocument(encoding, version: String);
var
   s: string;
begin
     s:=Format('<?xml version="%s" encoding="%s"?>', [version, encoding]);
     WriteToStream(s);
end;

procedure TXmlStreamWriter.WriteAtributeScope(AttributeScope: TAttributeScope);
var
   s: string;
   vPrefix: string;
begin
     if not Assigned(AttributeScope)
     then
         exit;

     with AttributeScope do
     begin
          vPrefix:=GetPrefix;
          if vPrefix = TXmlConstants.XML_NS_PREFIX
          then
              vPrefix:='';

          s:=' ' + XmlFormatAttribute(vPrefix, Name, Value);
     end;

     WriteToStream(s);
     AttributeScope.written:=true;
end;

procedure TXmlStreamWriter.PushStartElement(Prefix, NamespaceURI,
  LocalName: String; Empty: boolean);
var
   ParentScope, scope: TElementScope;
   binding: TNamespaceBinding;
   bindingCreated: boolean;
begin
     CheckElement(Prefix, NamespaceURI, LocalName);

     ParentScope:=LastElementScope;

     if Assigned(ParentScope)
     then
     begin
          WriteElementScope(ParentScope);
     end;

     scope:=CreateElementScope(Prefix, NamespaceURI, LocalName, ParentScope, Empty);

     binding:=scope.BindingsList.GetBinding(Prefix, NamespaceURI, bindingCreated, true);

     if bindingCreated {and FRepairingNamespaces} { TODO : Uncomment }
     then
         CreateAttributeScope(TXmlConstants.XMLNS_ATTRIBUTE, '', binding.Prefix, NamespaceURI);
end;

procedure TXmlStreamWriter.WriteStartElement(LocalName: String);
begin
     with GetDefaultNamespace do
          WriteStartElement(Prefix, Uri, LocalName);
end;

procedure TXmlStreamWriter.WriteStartElement(NamespaceURI, LocalName: String);
var
   binding: TNamespaceBinding;
   bindingCreated: boolean;
begin
     binding:=FRootBindings.GetBindingByURI(NamespaceURI, bindingCreated, FRepairingNamespaces);

     if not Assigned(binding) and not FRepairingNamespaces
     then
         RaiseNamespaceUriNotBound;

     WriteStartElement(binding.Prefix, NamespaceURI, LocalName);

     if bindingCreated
     then
         CreateAttributeScope(TXmlConstants.XMLNS_ATTRIBUTE, '', binding.Prefix, NamespaceURI);
end;

procedure TXmlStreamWriter.WriteStartElement(Prefix, NamespaceURI,
  LocalName: String);
begin
     PushStartElement(Prefix, NamespaceURI, LocalName, false);
end;


procedure TXmlStreamWriter.WriteElementScope(ElementScope: TElementScope);
var
   attScope: TAttributeScope;
   s: string;
   iElement, i: integer;
   scope: TStaxScope;
begin
     if not Assigned(ElementScope) or ElementScope.written
     then
         exit;

     WriteToStream(FormatStartElement(ElementScope));

     iElement:=FScopesList.IndexOf(ElementScope);

     for i:=iElement + 1 to Pred(FScopesList.Count) do
     begin
          scope:=FScopesList[i];

          if scope is TAttributeScope
          then
          begin
               WriteAtributeScope(scope as TAttributeScope);
          end
          else { TODO : Should other types be considerated?  }
          begin
               break
          end;
     end;

     if ElementScope.Empty
     then
         s:='/'
     else
         s:='';
     s+='>';

     WriteToStream(s);

     ElementScope.written:=true;
end;

procedure TXmlStreamWriter.WriteElementScope_End(ElementScope: TElementScope);
var
   s: string;
begin
     if not Assigned(ElementScope)
     then
         exit;

     WriteToStream(FormatEndElement(ElementScope));
end;

procedure TXmlStreamWriter.WriteToStream(text: String);
begin
     FStream.WriteBuffer(PChar(text)^, Length(text));
end;


end.
