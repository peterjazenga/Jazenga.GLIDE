{***********************************************************************
 Package pl_Barcode
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit StCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring

  StEBadBarHeight        = 'Bar Height cannot be less than one';
  StEBadBarHeightToWidth = 'BarHeightToWidth cannot be less than one';
  StEBadBarWidth         = 'Bar Width cannot be less than one';
  StEBadCountryCode      = 'Invalid Country Code';
  StEBadNumCols          = 'Invalid Number of columns';
  StEBadNumRows          = 'Invalid number of rows';
  StEBadPostalCode       = 'Invalid Postal Code';
  StEBadServiceClass     = 'Invalid Service Class';
  StEBadQuietZone        = 'Invalid Quiet Zone';
  StECodeTooLarge        = 'Code too large for barcode';
  StEGLIOutOfRange       = 'GLI value out of range';
  StEInvalidCodeword     = 'Invalid Codeword';
  StENeedBarHeight       = 'Either BarHeight or BarHeightToWidth is required';
  StENeedHorz            = 'Horizontal size needs to be specified';
  StENeedVert            = 'Vertical size needs to be specified';
  stscInvalidCharacter   = 'Invalid Character';
  stscInvalidUPCACodeLen        = 'Invalid code length (must be 11 or 12)';
  stscInvalidCheckCharacter     = 'Invalid check character';
  stscInvalidUPCECodeLen        = 'Invalid code length (must be 6)';
  stscInvalidEAN8CodeLen        = 'Invalid code length (must be 7 or 8)';
  stscInvalidEAN13CodeLen       = 'Invalid code length (must be 12 or 13)';
  stscInvalidSupCodeLen         = 'Invalid supplemental code length (must be 2 or 5)';
  stscInvalidLength             = 'POSTNET code must be 5, 9 or 11 digits';


Procedure RaiseStError(Const atext: String);
function TrimL(const S : String) : String;
function TrimLeadL(const S : String) : String;

implementation

Procedure RaiseStError(Const atext: String);
begin
end;

function TrimL(const S : String) : String;
  {-Return a string with leading and trailing white space removed.}
var
  I : Longint;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] <= ' ') do
    SetLength(Result, Pred(Length(Result)));

  I := 1;
  while (I <= Length(Result)) and (Result[I] <= ' ') do
    Inc(I);
  Dec(I);
  if I > 0 then
    System.Delete(Result, 1, I);
end;

function TrimLeadL(const S : String) : String;
  {-Return a string with leading white space removed}
begin
  Result := TrimLeft(S);
end;

end.

