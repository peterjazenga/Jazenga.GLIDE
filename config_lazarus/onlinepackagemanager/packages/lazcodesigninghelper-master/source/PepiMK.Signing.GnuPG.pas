{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(Codesigning implementation for GnuPG.)

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
// 2017-05-17  pk  10m  Completed code.
// 2017-05-17  pk  10m  Moved from old PepiMK.Authenticode.Sign.External unit.
// *****************************************************************************
   )
}

unit PepiMK.Signing.GnuPG;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   PepiMK.Signing.Base;

type
   { TGnuPGSigner }

   TGnuPGSigner = class(TCustomFileSigner)
   private
      FUseCustomKey: boolean;
   protected
      procedure ConstructSignParameters(AAdder: TConstructParametersProc); override;
      procedure ConstructVerifyParameters(AAdder: TConstructParametersProc); override;
   public
      class function SupportsLazarusTargetOS({%H-}AOS: string): boolean; override;
   public
      constructor Create; override;
      property UseCustomKey: boolean read FUseCustomKey write FUseCustomKey;
   end;

implementation

{ TGnuPGSigner }

procedure TGnuPGSigner.ConstructSignParameters(AAdder: TConstructParametersProc);
begin
   AAdder('--verbose');
   AAdder('--detach-sig');
   if UseCustomKey then begin
      case Certificate.Source of
         cscsStoreByHash:
         begin
            AAdder('--u');
            AAdder(UTF8Decode(Certificate.Hash));
         end;
         cscsStoreBySubstring:
         begin
            AAdder('--u');
            AAdder(UTF8Decode(Certificate.Substring));
         end;
      end;
   end;
   AAdder(Filename);
end;

procedure TGnuPGSigner.ConstructVerifyParameters(AAdder: TConstructParametersProc);
begin
   AAdder('--verify');
   AAdder(Filename + '.sig');
   AAdder(Filename);
end;

class function TGnuPGSigner.SupportsLazarusTargetOS(AOS: string): boolean;
begin
   Result := True;
end;

constructor TGnuPGSigner.Create;
begin
   inherited Create;
   FUseCustomKey := False;
   SigningExecutable := 'gpg';
end;

end.
