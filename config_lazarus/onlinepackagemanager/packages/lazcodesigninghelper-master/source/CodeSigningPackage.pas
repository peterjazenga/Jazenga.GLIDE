{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodeSigningPackage;

interface

uses
   CodeSigningHelper.Options, RegCodeSigningHelper, CodeSigningHelper.Menu, 
   CodeSigningHelper.Strings, CodeSigningHelper.Options.GnuPG.Frame, 
   PepiMK.Signing.AppleCodeSign, PepiMK.Signing.Base, PepiMK.Signing.GnuPG, 
   PepiMK.Signing.MicrosoftSignTool, 
   CodeSigningHelper.ProjectOptions.MicrosoftSignTool.Frame, 
   CodeSigningHelper.ProjectOptions, 
   CodeSigningHelper.Options.AppleCodeSign.Frame, 
   CodeSigningHelper.Options.JavaKeyTool.Frame, 
   CodeSigningHelper.Options.MicrosoftSignTool.Frame, 
   PepiMK.Signing.JavaKeyTool, 
   CodeSigningHelper.ProjectOptions.AppleCodeSign.Frame, 
   CodeSigningHelper.ProjectOptions.GnuPG.Frame, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegCodeSigningHelper', @RegCodeSigningHelper.Register);
end;

initialization
  RegisterPackage('CodeSigningPackage', @Register);
end.
