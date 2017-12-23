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

unit Pas2PhpPasConst;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpPasType, Pas2PhpTranslate, PasTree, SysUtils;

type

  TPasConstHelper = class helper for TPasConst
    function _GetPhpVarType: TPas2PhpVarType;
  end;

implementation

function TPasConstHelper._GetPhpVarType: TPas2PhpVarType;
begin
  try
    OPasLang.DecodeInteger(Value);
    Result := ptInteger;
  except
    try
      OPasLang.DecodeFloat(Value);
      Result := ptFloat;
    except
      try
        OPasLang.DecodeString(Value);
        Result := ptString;
      except
        try
          OPasLang.DecodeBoolean(Value);
          Result := ptBoolean;
        except
          Result := ptUnknown;
        end;
      end;
    end;
  end;
end;

end.
