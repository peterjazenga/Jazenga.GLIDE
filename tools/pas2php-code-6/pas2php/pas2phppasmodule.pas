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

unit Pas2PhpPasModule;

{$INCLUDE pas2php.inc}

interface

uses
  Classes, Pas2PhpFPList, Pas2PhpPasDeclarations, Pas2PhpPasPackage, PasTree, SysUtils;

type

  TPasModule = PasTree.TPasModule;

  TPasModuleHelper = class helper for TPasModule
  strict private
    function __ModuleFindElementByName(const AName: string; const AInterfaceOnly: boolean): TPasElement;
  public
    function _ModuleFindElementByName(const AName: string): TPasElement;
  end;

implementation

// OLD UNUSED FUNCTION
function __PasModuleFindElementByName(const AModule: TPasModule; const AName: string;
  const AInterfaceOnly: boolean): TPasElement;
var
  LIndex: integer;
  LModule: TPasModule;
begin
  Assert(Assigned(AModule));
  try
    if AInterfaceOnly then begin
      Result := AModule.InterfaceSection._FindDeclarationsElementByName(AName);
    end else begin
      try
        Result := AModule.ImplementationSection._FindDeclarationsElementByName(AName);
      except
        Result := AModule.InterfaceSection._FindDeclarationsElementByName(AName);
      end;
    end;
  except
    for LIndex := 0 to AModule.InterfaceSection.UsesList.High do begin
      try
        LModule := (AModule.Parent as TPasPackage)._FindModuleByUnitName(
          AModule.InterfaceSection.UsesList[LIndex].Name);
        if LModule.Visibility = visDefault then begin
          LModule.Visibility := visPrivate;
          Exit(__PasModuleFindElementByName(LModule, AName, True));
        end;
      except
      end;
    end;
    Abort;
  end;
end;

function TPasModuleHelper.__ModuleFindElementByName(const AName: string;
  const AInterfaceOnly: boolean): TPasElement;
var
  LIndex: integer;
  LModule: TPasModule;
begin
  Assert(Assigned(Self));
  try
    if AInterfaceOnly then begin
      Result := InterfaceSection._FindDeclarationsElementByName(AName);
    end else begin
      try
        Result := ImplementationSection._FindDeclarationsElementByName(AName);
      except
        Result := InterfaceSection._FindDeclarationsElementByName(AName);
      end;
    end;
  except
    for LIndex := 0 to InterfaceSection.UsesList.High do begin
      try
        LModule := (Parent as TPasPackage)._FindModuleByUnitName(InterfaceSection.UsesList[LIndex].Name);
        if LModule.Visibility = visDefault then begin
          LModule.Visibility := visPrivate;
          Exit(LModule.__ModuleFindElementByName(AName, True));
        end;
      except
      end;
    end;
    Abort;
  end;
end;

function TPasModuleHelper._ModuleFindElementByName(const AName: string): TPasElement;
var
  LIndex: integer;
  LPackage: TPasPackage;
begin
  LPackage := Parent as TPasPackage;
  for LIndex := 0 to LPackage.Modules.High do begin
    LPackage.Modules[LIndex].Visibility := visDefault;
  end;
  Result := __ModuleFindElementByName(AName, False);
end;

end.


