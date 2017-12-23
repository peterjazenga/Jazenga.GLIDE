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

unit Pas2PhpPasClassType;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpFPList, Pas2PhpTranslate, PasTree, SysUtils;

type

  TPasClassType = PasTree.TPasClassType;

  TPasClassTypeHelper = class helper for TPasClassType
    function _FindClassMemberByName(const AName: string): TPasElement;
    procedure _FillAncestorType;
  end;

implementation

uses Pas2PhpUtils;

function TPasClassTypeHelper._FindClassMemberByName(const AName: string): TPasElement;
begin
  Assert(Assigned(Self));
  IfThenUnsupported(ClassVars.Count > 0, 'ClassVars');
  IfThenUnsupported(Interfaces.Count > 0, 'Interfaces');
  IfThenUnsupported(GenericTemplateTypes.Count > 0, 'GenericTemplateTypes');
  Result := Members._FindElementByName(AName);
end;

procedure TPasClassTypeHelper._FillAncestorType;
begin
  Assert(Assigned(Self));
  if not SameText(Name, OPasLang.STObject) and not Assigned(AncestorType) then begin
    AncestorType := TPasType.Create(OPasLang.STObject, nil);
    // TODO: Why arn't we setting the owner here?
  end;
end;

end.
