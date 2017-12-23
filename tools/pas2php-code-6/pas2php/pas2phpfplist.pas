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

unit Pas2PhpFPList;

{$INCLUDE pas2php.inc}

interface

uses Classes, Pas2PhpPasElement, PasTree, SysUtils;

type

  TFPListHelper = class helper for TFPList
  strict private
    function _GetItems(const AIndex: integer): TPasElement;
  public
    function High: integer;
    function _TryFindElementByName(const AName: string; out AElement: TPasElement): boolean;
    function _FindElementByName(const AName: string): TPasElement;
  public
    property Items[const AIndex: integer]: TPasElement read _GetItems; default;
  end;

implementation

function TFPListHelper.High: integer;
begin
  Assert(Assigned(Self));
  Result := Count - 1;
end;

function TFPListHelper._GetItems(const AIndex: integer): TPasElement;
begin
  Assert(Assigned(Self));
  Result := TObject(inherited Items[AIndex]) as TPasElement;
end;

function TFPListHelper._TryFindElementByName(const AName: string; out AElement: TPasElement): boolean;
var
  LIndex: integer;
begin
  Assert(Assigned(Self));
  for LIndex := 0 to High do begin
    AElement := Items[LIndex];
    if AElement._IsName(AName) then begin
      Exit(True);
    end;
  end;
  Exit(False);
end;

function TFPListHelper._FindElementByName(const AName: string): TPasElement;
begin
  if not _TryFindElementByName(AName, Result) then begin
    Abort;
  end;
end;

end.

