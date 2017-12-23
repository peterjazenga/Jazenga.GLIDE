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

unit ExampleSettings;

{$mode objfpc}{$H+}

interface


implementation

uses __RTL, FileUtil, LazFileUtils, Pas2Php;

const

  ServerPathRemote = 'http://127.0.0.1:80/';

function PascalFileName: string;
begin
  Result := ProgramDirectory + 'index.pas';
end;

function ServerPathLocal: string;
begin
  Result := CleanAndExpandDirectory(ProgramDirectory + '..' + DirectorySeparator + '..' + DirectorySeparator);
end;

initialization

  WriteLn;
  WriteLn('PAS2PHP Example');
  WriteLn('---------------');
  WriteLn;
  WriteLn('For this example to work, you need to have a HTTP server connected.');
  WriteLn('The following paths must be used, or you will have to edit this file.');
  WriteLn;
  WriteLn('Local Server Path  = ', ServerPathLocal);
  WriteLn('Remote Server Path = ', ServerPathRemote);
  WriteLn;
  WriteLn('NOTE: If you dont have a HTTP server, you may want to try my own Wascal server');
  WriteLn('which is avaliable from www.wascal.net');
  WriteLn;

  Pas2PhpTranspile(PascalFileName, ServerPathLocal, ServerPathRemote);
end.

