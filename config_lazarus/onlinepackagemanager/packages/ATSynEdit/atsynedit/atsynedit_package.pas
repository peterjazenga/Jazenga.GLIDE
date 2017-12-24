{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atsynedit_package;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATSynEdit_Register, ATStringProc, ATStringProc_TextBuffer, 
  ATStringProc_UTF8Detect, ATStringProc_WordJump, ATStrings, ATStrings_Undo, 
  ATSynEdit, ATSynEdit_Adapters, ATSynEdit_CanvasProc, ATSynEdit_Carets, 
  ATSynEdit_Commands, ATSynEdit_Edits, ATSynEdit_Gutter, ATSynEdit_Keymap, 
  ATSynEdit_Keymap_Init, ATSynEdit_Ranges, ATSynEdit_WrapInfo, 
  ATSynEdit_Finder, ATStringProc_HtmlColor, ATStrings_Hints, ATSynEdit_Colors, 
  ATSynEdit_Export_HTML, ATSynEdit_Markers, ATSynEdit_RegExpr, 
  ATSynEdit_Adapter_Cache, ATSynEdit_Gaps, ATSynEdit_ScrollBar, 
  ATSynEdit_DimRanges, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATSynEdit_Register', @ATSynEdit_Register.Register);
end;

initialization
  RegisterPackage('atsynedit_package', @Register);
end.
