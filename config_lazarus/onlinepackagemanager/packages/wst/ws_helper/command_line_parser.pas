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
unit command_line_parser;

interface

uses
  Classes, SysUtils; 

Type

  TComandLineOption = (
    cloInterface, cloProxy, cloImp, cloBinder, cloWsdl, cloXsd, cloJava,
    cloOutPutDirRelative, cloOutPutDirAbsolute, cloHandleWrappedParameters,
    cloGenerateDocAsComments, cloGenerateObjectCollection,
    cloFileRenaming, cloPrefixEnum, cloParserCaseSensitive,
    cloStringMaping, cloCreateChoiceFields
  );
  TComandLineOptions = set of TComandLineOption;

  function ParseCmdLineOptions(out AAppOptions : TComandLineOptions):Integer;
  function GetOptionArg(const AOption : TComandLineOption):string;
  
implementation
uses getopts;

Var
  OptionsArgsMAP : Array[TComandLineOption] of string;

function GetOptionArg(const AOption : TComandLineOption):string;
begin
  Result := OptionsArgsMAP[AOption];
end;

function ParseCmdLineOptions(out AAppOptions : TComandLineOptions):Integer;
var
  c : Char;
begin
  AAppOptions := [];
  c := #0;
  repeat
    c := GetOpt('u:pibo:a:wxydg:f:c:j');
    case c of
      'u' :
        begin
          Include(AAppOptions,cloInterface);
          OptionsArgsMAP[cloInterface] := OptArg;
        end;
      'p' : Include(AAppOptions,cloProxy);
      'i' : Include(AAppOptions,cloImp);
      'b' : Include(AAppOptions,cloBinder);
      'o' :
        Begin
          Include(AAppOptions,cloOutPutDirRelative);
          OptionsArgsMAP[cloOutPutDirRelative] := OptArg;
        End;
      'a' :
        Begin
          Include(AAppOptions,cloOutPutDirAbsolute);
          OptionsArgsMAP[cloOutPutDirAbsolute] := OptArg;
        End;
      'w' : Include(AAppOptions,cloWsdl);
      'x' : Include(AAppOptions,cloXsd);
      'y' : Include(AAppOptions,cloHandleWrappedParameters);
      'd' : Include(AAppOptions,cloGenerateDocAsComments);
      'g' :
        begin
          if ( Pos('A',OptArg) = 1 ) or ( Pos('C',OptArg) = 1 ) then begin
            Include(AAppOptions,cloGenerateObjectCollection);
            OptionsArgsMAP[cloGenerateObjectCollection] := OptArg;
          end else if ( Pos('E',OptArg) = 1 ) then begin
            Include(AAppOptions,cloPrefixEnum);
            OptionsArgsMAP[cloPrefixEnum] := OptArg;
          end else if ( Pos('S',OptArg) = 1 ) then begin
            Include(AAppOptions,cloStringMaping);
            OptionsArgsMAP[cloStringMaping] := OptArg;
          end else if ( Pos('F',OptArg) = 1 ) then begin
            Include(AAppOptions,cloCreateChoiceFields);
            OptionsArgsMAP[cloCreateChoiceFields] := OptArg;
          end;
        end;
      'f' :
        begin
          Include(AAppOptions,cloFileRenaming);
          OptionsArgsMAP[cloFileRenaming] := OptArg;
        end;
      'c' :
        begin
          Include(AAppOptions,cloParserCaseSensitive);
          OptionsArgsMAP[cloParserCaseSensitive] := OptArg;
        end;
      'j' : Include(AAppOptions,cloJava);
    end;
  until ( c = EndOfOptions );
  Result := OptInd;
end;

end.

