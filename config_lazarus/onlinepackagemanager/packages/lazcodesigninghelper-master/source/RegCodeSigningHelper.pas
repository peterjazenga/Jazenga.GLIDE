{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Registers classes with Lazarus)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-19  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit RegCodeSigningHelper;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   //SrcEditorIntf,
   ProjectIntf,
   LazIDEIntf,
   LazarusPackageIntf,
   IDEOptionsIntf,
   CodeSigningHelper.Menu,
   CodeSigningHelper.Options;

procedure RegisterCodeSigningHelper;
procedure Register;

var
   GCodeSigningHelper: TCodeSigningHelper;

implementation

procedure RegisterCodeSigningHelper;
begin
   GCodeSigningHelper.CreateMainMenuSubMenu();
   GCodeSigningHelper.AddHandlers();
end;

procedure Register;
begin
   RegisterCodeSigningHelper;
end;

initialization

   CodeSigningOptionGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
   RegisterIDEOptionsGroup(CodeSigningOptionGroup, TCodeSigningOptions);
   GCodeSigningHelper := TCodeSigningHelper.Create;

finalization
   GCodeSigningHelper.Free;
end.
