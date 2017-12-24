{
  Vampyre Imaging Library
  by Marek Mauder 
  http://imaginglib.sourceforge.net

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

{ This unit is heart of Imaging library. It contains basic functions for
  manipulating image data as well as various image file format support.}
  
unit ImagingCore;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Types, Variants,
{$IF Defined(MSWINDOWS)}
  Windows,
{$ELSEIF Defined(FPC)}
  Dos, BaseUnix, Unix,
{$ELSEIF Defined(DELPHI)}
  Posix.SysTime,
{$IFEND}
 ImagingTypes;

{$i Imaging.inc}
{$i ImagingFormats.inc}
{$i ImagingUtility.inc}
{$i ImagingIO.inc}

implementation

{$i ImagingUtility_imp.inc}
{$i ImagingFormats_imp.inc}
{$i ImagingIO_imp.inc}
{$i Imaging_imp.inc}

 
initialization 

 //======================================================= ImagingFormats
  // Initialize default sampling filter function pointers and radii
  SamplingFilterFunctions[sfNearest]    := FilterNearest;
  SamplingFilterFunctions[sfLinear]     := FilterLinear;
  SamplingFilterFunctions[sfCosine]     := FilterCosine;
  SamplingFilterFunctions[sfHermite]    := FilterHermite;
  SamplingFilterFunctions[sfQuadratic]  := FilterQuadratic;
  SamplingFilterFunctions[sfGaussian]   := FilterGaussian;
  SamplingFilterFunctions[sfSpline]     := FilterSpline;
  SamplingFilterFunctions[sfLanczos]    := FilterLanczos;
  SamplingFilterFunctions[sfMitchell]   := FilterMitchell;
  SamplingFilterFunctions[sfCatmullRom] := FilterCatmullRom;
  SamplingFilterRadii[sfNearest]    := 1.0;
  SamplingFilterRadii[sfLinear]     := 1.0;
  SamplingFilterRadii[sfCosine]     := 1.0;
  SamplingFilterRadii[sfHermite]    := 1.0;
  SamplingFilterRadii[sfQuadratic]  := 1.5;
  SamplingFilterRadii[sfGaussian]   := 1.25;
  SamplingFilterRadii[sfSpline]     := 2.0;
  SamplingFilterRadii[sfLanczos]    := 3.0;
  SamplingFilterRadii[sfMitchell]   := 2.0;
  SamplingFilterRadii[sfCatmullRom] := 2.0;
  
 //======================================================= ImagingUtility 

 InitCrcTable;
{$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(PerfFrequency);
  InvPerfFrequency := 1.0 / PerfFrequency;
{$ENDIF}

{$IF Defined(DELPHI)}
  {$IF CompilerVersion >= 23}
  FloatFormatSettings := TFormatSettings.Create('en-US');
  {$ELSE}
  GetLocaleFormatSettings(1033, FloatFormatSettings);
  {$IFEND}
{$ELSE FPC}
  FloatFormatSettings := DefaultFormatSettings;
  FloatFormatSettings.DecimalSeparator := '.';
  FloatFormatSettings.ThousandSeparator := ',';
{$IFEND}

 //======================================================= ImagingIO 
  OriginalFileIO.Open := FileOpen;
  OriginalFileIO.Close := FileClose;
  OriginalFileIO.Eof := FileEof;
  OriginalFileIO.Seek := FileSeek;
  OriginalFileIO.Tell := FileTell;
  OriginalFileIO.Read := FileRead;
  OriginalFileIO.Write := FileWrite;

  StreamIO.Open := StreamOpen;
  StreamIO.Close := StreamClose;
  StreamIO.Eof := StreamEof;
  StreamIO.Seek := StreamSeek;
  StreamIO.Tell := StreamTell;
  StreamIO.Read := StreamRead;
  StreamIO.Write := StreamWrite;

  MemoryIO.Open := MemoryOpen;
  MemoryIO.Close := MemoryClose;
  MemoryIO.Eof := MemoryEof;
  MemoryIO.Seek := MemorySeek;
  MemoryIO.Tell := MemoryTell;
  MemoryIO.Read := MemoryRead;
  MemoryIO.Write := MemoryWrite;

  ResetFileIO;
  
  
 //======================================================= Imaging
{$IFDEF MEMCHECK}
  {$IF CompilerVersion >= 18}
    System.ReportMemoryLeaksOnShutdown := True;
  {$IFEND}
{$ENDIF}
  if GlobalMetadata = nil then     GlobalMetadata := TMetadata.Create;
  if ImageFileFormats = nil then  ImageFileFormats := TList.Create;
  _InitImageFormats;
  RegisterOption(ImagingColorReductionMask, @ColorReductionMask);
  RegisterOption(ImagingLoadOverrideFormat, @LoadOverrideFormat);
  RegisterOption(ImagingSaveOverrideFormat, @SaveOverrideFormat);
  RegisterOption(ImagingMipMapFilter, @MipMapFilter);
  RegisterOption(ImagingBinaryTreshold, @BinaryTreshold);
  
finalization
  _FreeOptions;
  _FreeImageFileFormats;
  GlobalMetadata.Free;

end.

