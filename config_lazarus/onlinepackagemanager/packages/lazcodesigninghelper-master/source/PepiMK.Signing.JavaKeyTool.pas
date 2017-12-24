{
   @author(Patrick Michael Kolla-ten Venne [pk] <patrick@kolla-tenvenne.de>)
   @abstract(TODO : please fill in abstract here!)

   @preformatted(
// *****************************************************************************
// Copyright: © 2017 Patrick Michael Kolla-ten Venne. All rights reserved.
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2017-05-22  pk  ---  [CCD] Updated unit header.
// *****************************************************************************
   )
}

unit PepiMK.Signing.JavaKeyTool;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF FPC}

interface

uses
   Classes,
   SysUtils,
   PepiMK.Signing.Base;

type

   { TJavaKeyToolSigner }

   TJavaKeyToolSigner = class(TCustomFileSigner)
   protected
      procedure ConstructSignParameters(AAdder: TConstructParametersProc); override;
      procedure ConstructVerifyParameters(AAdder: TConstructParametersProc); override;
   public
      class function SupportsLazarusTargetOS(AOS: string): boolean; override;
   public
      constructor Create; override;
   end;

implementation

{ TJavaKeyToolSigner }

procedure TJavaKeyToolSigner.ConstructSignParameters(AAdder: TConstructParametersProc);
begin
end;

procedure TJavaKeyToolSigner.ConstructVerifyParameters(AAdder: TConstructParametersProc);
begin

end;

class function TJavaKeyToolSigner.SupportsLazarusTargetOS(AOS: string): boolean;
begin
   Result := SameText(AOS, 'Java');
end;

constructor TJavaKeyToolSigner.Create;
begin
   inherited Create;
   SigningExecutable := 'keytool';
end;

end.
