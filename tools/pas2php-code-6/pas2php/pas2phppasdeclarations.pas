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

unit Pas2PhpPasDeclarations;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpFPList, PasTree, SysUtils;

type

  TPasDeclarations = PasTree.TPasDeclarations;

  TPasDeclarationsHelper = class helper for TPasDeclarations
    function _FindDeclarationsElementByName(const AName: string): TPasElement;
  end;

implementation

uses Pas2PhpUtils;

function TPasDeclarationsHelper._FindDeclarationsElementByName(const AName: string): TPasElement;
var
  LIndex: integer;
begin
  Assert(Assigned(Self));
  IfThenUnsupported(ExportSymbols.Count > 0, 'ExportSymbols');
  IfThenUnsupported(Properties.Count > 0, 'Properties');
  try
    Exit(Classes._FindElementByName(AName));
  except
    try
      Exit(Variables._FindElementByName(AName));
    except
      try
        Exit(Functions._FindElementByName(AName));
      except
        try
          Exit(Consts._FindElementByName(AName));
        except
          try
            Exit(Types._FindElementByName(AName));
          except
            for LIndex := 0 to Types.High do begin
              try
                Exit((Types[LIndex] as TPasEnumType).Values._FindElementByName(AName));
              except
              end;
            end;
            Abort;
          end;
        end;
      end;
    end;
  end;
end;

end.

