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

unit Pas2PhpPasRecordType;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpFPList, PasTree, SysUtils;

type

  TPasRecordType = PasTree.TPasRecordType;

  TPasRecordTypeHelper = class helper for TPasRecordType
    function _FindRecordMemberByName(const AName: string): TPasVariable;
  end;

implementation

function TPasRecordTypeHelper._FindRecordMemberByName(const AName: string): TPasVariable;
begin
  Assert(Assigned(Self));
  Result := Members._FindElementByName(AName) as TPasVariable;
end;

end.
