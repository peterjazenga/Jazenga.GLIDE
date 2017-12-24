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


unit StaxCommon;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl;

type
    TXmlNodeType = (xntElement=1, xntAttribute, xntText, xntCDataSection,
                    xntEntityReference, xntEntity, xntProcessingInstruction,
                    xntComment, xntDocument, xntDocumentType, xntDocumentFragment,
                    xntNotation);


    {Based on XMLConstants Java class:
      http://docs.oracle.com/javase/7/docs/api/javax/xml/XMLConstants.html}
    TXmlConstants = class abstract
       const
            NULL_NS_URI = '';
            DEFAULT_NS_PREFIX = '';
            XML_NS_URI = 'http://www.w3.org/XML/1998/namespace';
            XML_NS_PREFIX = 'xml';
            XMLNS_ATTRIBUTE_NS_URI = 'http://www.w3.org/2000/xmlns/';
            XMLNS_ATTRIBUTE = 'xmlns';
    end;


    { TXmlNamespaceBinding }

    TXmlNamespaceBinding = class
      Prefix,
      NamespaceUri: string;

      constructor Create(APrefix, ANamespaceUri: string);
    end;

    TXmlNamespaceBindings = specialize TFPGObjectList<TXmlNamespaceBinding>;


const
     XML_DEFAULT_ENCODING = 'utf-8';
     XML_DEFAULT_VERSION = '1.0';


implementation

{ TXmlNamespaceBinding }

constructor TXmlNamespaceBinding.Create(APrefix, ANamespaceUri: string);
begin
     Prefix:=APrefix;
     ANamespaceUri:=NamespaceUri;
end;

end.

