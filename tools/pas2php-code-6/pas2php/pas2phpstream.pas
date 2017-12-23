{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit Pas2PhpStream;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, Pas2PhpDefines, Pas2PhpTranslate, Pas2PhpTreeContainer, PasTree, StrUtils, SysUtils;

type

  TPas2PhpStream = class(TPas2PhpTreeContainer)
  strict private
    FStream: TStream;
    FDepth: integer;
    FIsNewLine: boolean;
  strict private
    procedure WriteStr(const AString: string);
  protected
    FPasModule: TPasModule;
  protected
    procedure NewLine;
    procedure WriteLine(const AString: string);
    procedure WriteDeclaration(const AElement: TPasElement);
    function WriteFPListTitle(const AOwner: TPasElement; const AFPList: TFPList;
      const AName: string): boolean;
  public
    procedure WritePhp(const AString: string);
    procedure WritePhpIncludeOnce(const AFileName: string);
    procedure WritePhpComment(const AString: string);
    procedure SetOutputStream(const AStream: TStream);
  end;

implementation

procedure TPas2PhpStream.SetOutputStream(const AStream: TStream);
begin
  Assert(Assigned(AStream));
  FStream := AStream;
  FDepth := 0;
  FIsNewLine := False;
  FPasModule := nil;
end;

procedure TPas2PhpStream.WriteStr(const AString: string);
begin
  if Length(AString) > 0 then begin
    FStream.Write(AString[1], Length(AString));
    FIsNewLine := False;
  end;
end;

procedure TPas2PhpStream.NewLine;
begin
  if not FIsNewLine then begin
    WriteStr(LineEnding);
    FIsNewLine := True;
  end;
end;

procedure TPas2PhpStream.WriteLine(const AString: string);
begin
  NewLine;
  WriteStr(AString);
  NewLine;
end;

procedure TPas2PhpStream.WritePhp(const AString: string);
begin
  if Length(AString) > 0 then begin
    case AString[Length(AString)] of
      '{': begin
        if FIsNewLine then begin
          WriteStr(StringOfChar(OChar.Space, FDepth * GOutputIndentCount));
        end;
        WriteStr(AString);
        NewLine;
        FDepth += 1;
      end;
      '}': begin
        WriteStr(LeftStr(AString, Length(AString) - 1));
        NewLine;
        FDepth -= 1;
        WriteStr(StringOfChar(OChar.Space, FDepth * GOutputIndentCount));
        WriteStr('}');
        NewLine;
      end;
      ';': begin
        if FIsNewLine then begin
          WriteStr(StringOfChar(OChar.Space, FDepth * GOutputIndentCount));
        end;
        WriteStr(AString);
        NewLine;
      end else begin
        if FIsNewLine then begin
          WriteStr(StringOfChar(OChar.Space, FDepth * GOutputIndentCount));
        end;
        WriteStr(AString);
      end;
    end;
  end;
end;

procedure TPas2PhpStream.WritePhpIncludeOnce(const AFileName: string);
begin
  if AFileName <> EmptyStr then begin
    WritePhp(GPhpLang.SIncludeOnce + '(' + OChar.SingleQuote + AFileName +
      OChar.SingleQuote + ')' + OChar.SemiColon);
  end;
end;

procedure TPas2PhpStream.WritePhpComment(const AString: string);
begin
  if PosSet([#10, #13], AString) = 0 then begin
    WritePhp('// ' + AString);
    NewLine;
  end else begin
    WriteLine('/*');
    WriteLine(AString);
    WriteLine('*/');
  end;
end;

procedure TPas2PhpStream.WriteDeclaration(const AElement: TPasElement);
begin
  if not (p2pNoPascalOutput in Options) and Assigned(AElement) then begin
    WritePhpComment(AElement.GetDeclaration(True) + ';');
  end;
end;

function TPas2PhpStream.WriteFPListTitle(const AOwner: TPasElement; const AFPList: TFPList;
  const AName: string): boolean;
begin
  Result := Assigned(AOwner) and Assigned(AFPList) and (AFPList.Count > 0);
  if Result then begin
    if not (p2pNoElementListTitles in Options) then begin
      WritePhpComment(AOwner.ClassName + '.' + AName + ' [' + IntToStr(AFPList.Count) + ']');
    end;
  end;
end;

end.
