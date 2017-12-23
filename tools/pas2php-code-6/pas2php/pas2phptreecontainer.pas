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

unit Pas2PhpTreeContainer;

{$INCLUDE pas2php.inc}

interface

uses
  Pas2PhpDefines, Pas2PhpUtils, PasTree, PParser, SysUtils;

type

  TPas2PhpTreeContainer = class(TPasTreeContainer)
  strict private
    FOptions: TPas2PhpOptions;
  public
    constructor Create(const AOptions: TPas2PhpOptions); virtual;
    destructor Destroy; override;
  public
    function CreateElement(AClass: TPTreeElement; const AName: string; AParent: TPasElement;
      AVisibility: TPasMemberVisibility; const ASourceFilename: string;
      ASourceLineNumber: integer): TPasElement;
      overload; override;
    function FindElement(const AName: string): TPasElement; override;
  public
    property Options: TPas2PhpOptions read FOptions;
  end;

implementation

constructor TPas2PhpTreeContainer.Create(const AOptions: TPas2PhpOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FPackage := TPasPackage.Create('Package', nil);
  LogMessage(OProduct.NameVersion);
  LogMessage(OProduct.Slogan);
  LogMessage(OProduct.Copyright);
  LogMessage;
end;

destructor TPas2PhpTreeContainer.Destroy;
begin
  FreeAndNil(FPackage);
  inherited Destroy;
end;

function TPas2PhpTreeContainer.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; AVisibility: TPasMemberVisibility; const ASourceFilename: string;
  ASourceLineNumber: integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFileName;
  Result.SourceLinenumber := ASourceLineNumber;
end;

function TPas2PhpTreeContainer.FindElement(const AName: string): TPasElement;
begin
  Result := nil;
end;

end.
