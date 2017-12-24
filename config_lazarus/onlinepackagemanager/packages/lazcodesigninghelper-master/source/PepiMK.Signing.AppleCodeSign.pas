{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Codesigning implementation for Apples codesign.)

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
// 2017-05-17  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
// http://wiki.lazarus.freepascal.org/Code_Signing_for_Mac_OS_X
// *****************************************************************************
   )
}

unit PepiMK.Signing.AppleCodeSign;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   PepiMK.Signing.Base;

type

   { TAppleCodeSignSigner }

   TAppleCodeSignSigner = class(TCustomFileSigner)
   protected
      procedure ConstructSignParameters(AAdder: TConstructParametersProc); override;
      procedure ConstructVerifyParameters(AAdder: TConstructParametersProc); override;
   public
      class function SupportsLazarusTargetOS(AOS: string): boolean; override;
   public
      constructor Create; override;
   end;


implementation

{ TAppleCodeSignSigner }

procedure TAppleCodeSignSigner.ConstructSignParameters(AAdder: TConstructParametersProc);
begin
   AAdder('--verbose'); // verbose
   AAdder('--deep');
   AAdder('--force'); // force, replaces existing
   AAdder('-s');
   AAdder(UTF8Decode(Certificate.Substring));
   AAdder(Filename);
end;

procedure TAppleCodeSignSigner.ConstructVerifyParameters(AAdder: TConstructParametersProc);
begin
   AAdder('--verify');
   AAdder('--verbose'); // verbose
   AAdder(Filename);
end;

class function TAppleCodeSignSigner.SupportsLazarusTargetOS(AOS: string): boolean;
begin
   Result := SameText(AOS, 'Darwin');
end;

constructor TAppleCodeSignSigner.Create;
begin
   inherited Create;
   SigningExecutable := 'codesign';
end;

end.
