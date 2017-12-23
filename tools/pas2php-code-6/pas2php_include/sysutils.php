<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('phplib.php');
include_once('strutils.php');
include_once('sysutils.php');
// EmptyStr = '';
define("EmptyStr", "");
// SBoolTrue = 'True';
define("SBoolTrue", "True");
// SBoolFalse = 'False';
define("SBoolFalse", "False");
class EAbort extends Exception {
}
class EConvertError extends Exception {
}
class EAssertionFailed extends Exception {
}
class EArgumentException extends Exception {
}
class EArgumentOutOfRangeException extends EArgumentException {
}
class EAbstractError extends Exception {
}
class EFinally extends Exception {
}
// procedure Abort;
function Abort() {
  throw new EAbort("Abort");
}
// function IntToStr(const AInteger: Integer) : string;
function IntToStr($AInteger) {
  // Result : string;
  $Result = "";
  $Result = strval($AInteger);
  return $Result;
}
// function SetDirSeparators(const AString: string) : string;
function SetDirSeparators($AString) {
  // LIndex : Integer;
  $LIndex = 0;
  // Result : string;
  $Result = "";
  $Result = $AString;
  for ($LIndex = 1, $LIndex_High_ = Length($Result); $LIndex <= $LIndex_High_; $LIndex++) {
    if ($Result[($LIndex) - 1] == "\\" || $Result[($LIndex) - 1] == "/") {
      $Result[($LIndex) - 1] = DirectorySeparator;
    }
  }
  return $Result;
}
// function TryStrToInt(AString: string;out AInteger: Integer) : Boolean;
function TryStrToInt($AString, &$AInteger) {
  // Result : Boolean;
  $Result = false;
  if (Length($AString) > 0 && $AString[(1) - 1] == "\$") {
    $AString = base_convert(MidStr($AString, 2, MaxInt), 16, 10);
  }
  $AInteger = intval($AString, 10);
  $Result = strval($AInteger) == $AString;
  return $Result;
}
// function StrToInt(const AString: string) : Integer;
function StrToInt($AString) {
  // Result : Integer;
  $Result = 0;
  if (!TryStrToInt($AString, $Result)) {
    throw new EConvertError(EmptyStr);
  }
  return $Result;
}
/*
function StrToIntDef(const AString: string;const ADefault: Integer)
                     : Integer;
*/
function StrToIntDef($AString, $ADefault) {
  // Result : Integer;
  $Result = 0;
  try {
    $Result = StrToInt($AString);
  }
  catch (Exception $exception) {
    $Result = $ADefault;
  }
  return $Result;
}
/*
function TryStrToBool(const AString: string;out ABoolean: Boolean)
                      : Boolean;
*/
function TryStrToBool($AString, &$ABoolean) {
  // Result : Boolean;
  $Result = false;
  try {
    if (SameText($AString, SBoolTrue)) {
      $ABoolean = true;
    }
    else {
      if (SameText($AString, SBoolFalse)) {
        $ABoolean = false;
      }
      else {
        $ABoolean = StrToInt($AString) != 0;
      }
    }
    $Result = true;
  }
  catch (Exception $exception) {
    $Result = false;
  }
  return $Result;
}
// function StrToBool(const AString: string) : Boolean;
function StrToBool($AString) {
  // Result : Boolean;
  $Result = false;
  if (!TryStrToBool($AString, $Result)) {
    throw new EConvertError(EmptyStr);
  }
  return $Result;
}
/*
function StrToBoolDef(const AString: string;const ADefault: Boolean)
                      : Boolean;
*/
function StrToBoolDef($AString, $ADefault) {
  // Result : Boolean;
  $Result = false;
  try {
    $Result = StrToBool($AString);
  }
  catch (Exception $exception) {
    $Result = $ADefault;
  }
  return $Result;
}
/*
function BoolToStr(const ABoolean: Boolean;const ATrue: string;
                  const AFalse: string) : string;
*/
function BoolToStr($ABoolean, $ATrue = "True", $AFalse = "False") {
  // Result : string;
  $Result = "";
  if ($ABoolean) {
    $Result = $ATrue;
  }
  else {
    $Result = $AFalse;
  }
  return $Result;
}
// function FloatToStr(const AExtended: extended) : string;
function FloatToStr($AExtended) {
  // Result : string;
  $Result = "";
  $Result = strval($AExtended);
  return $Result;
}
/*
function TryStrToFloat(const AString: string;out AExtended: extended)
                       : Boolean;
*/
function TryStrToFloat($AString, &$AExtended) {
  // Result : Boolean;
  $Result = false;
  $AExtended = floatval($AString);
  $Result = strval($AExtended) == $AString;
  return $Result;
}
// function StrToFloat(const AString: string) : extended;
function StrToFloat($AString) {
  // Result : extended;
  $Result = 0.0;
  if (!TryStrToFloat($AString, $Result)) {
    throw new EConvertError(EmptyStr);
  }
  return $Result;
}
/*
function StrToFloatDef(const AString: string;const ADefault: extended)
                       : extended;
*/
function StrToFloatDef($AString, $ADefault) {
  // Result : extended;
  $Result = 0.0;
  try {
    $Result = StrToFloat($AString);
  }
  catch (Exception $exception) {
    $Result = $ADefault;
  }
  return $Result;
}
// procedure FreeAndNil(var AVariant: variant);
function FreeAndNil(&$AVariant) {
  $AVariant = Null;
}
/*
function SameText(const AString1: string;const AString2: string)
                  : Boolean;
*/
function SameText($AString1, $AString2) {
  // Result : Boolean;
  $Result = false;
  $Result = strcasecmp($AString1, $AString2) == 0;
  return $Result;
}
// function TrimLeft(const AString: string) : string;
function TrimLeft($AString) {
  // Result : string;
  $Result = "";
  $Result = ltrim($AString);
  return $Result;
}
// function TrimRight(const AString: string) : string;
function TrimRight($AString) {
  // Result : string;
  $Result = "";
  $Result = rtrim($AString);
  return $Result;
}
/*
function AnsiCompareStr(const AString1: string;const AString2: string)
                        : Integer;
*/
function AnsiCompareStr($AString1, $AString2) {
  // Result : Integer;
  $Result = 0;
  $Result = strcmp($AString1, $AString2);
  return $Result;
}
/*
function AnsiCompareText(const AString1: string;const AString2: string)
                         : Integer;
*/
function AnsiCompareText($AString1, $AString2) {
  // Result : Integer;
  $Result = 0;
  $Result = strcasecmp($AString1, $AString2);
  return $Result;
}
// function GetEnvironmentVariable(const AName: string) : string;
function GetEnvironmentVariable($AName) {
  // Result : string;
  $Result = "";
  $Result = getenv($AName);
  return $Result;
}
// function ExtractFileDir(const AFileName: string) : string;
function ExtractFileDir($AFileName) {
  // Result : string;
  $Result = "";
  $Result = dirname(SetDirSeparators($AFileName), 1);
  return $Result;
}
// function ExtractFileName(const AFileName: string) : string;
function ExtractFileName($AFileName) {
  // Result : string;
  $Result = "";
  $Result = basename(SetDirSeparators($AFileName));
  return $Result;
}
// function FileExists(const AFileName: string) : Boolean;
function FileExists($AFileName) {
  // Result : Boolean;
  $Result = false;
  $Result = is_file($AFileName);
  return $Result;
}
// function DirectoryExists(const AFileName: string) : Boolean;
function DirectoryExists($AFileName) {
  // Result : Boolean;
  $Result = false;
  $Result = is_dir($AFileName);
  return $Result;
}
// function ExtractFileExt(const AFileName: string) : string;
function ExtractFileExt($AFileName) {
  // LPos : Integer;
  $LPos = 0;
  // Result : string;
  $Result = "";
  $Result = ExtractFileName($AFileName);
  $LPos = RPos(ExtensionSeparator, $Result);
  if ($LPos == 0) {
    $Result = EmptyStr;
  }
  else {
    $Result = MidStr($Result, $LPos, MaxInt);
  }
  return $Result;
}
// function ExtractFilePath(const AFileName: string) : string;
function ExtractFilePath($AFileName) {
  // Result : string;
  $Result = "";
  $Result = ExtractFileDir($AFileName) . DirectorySeparator;
  return $Result;
}
/*
function ChangeFileExt(const AFileName: string;const AFileExt: string)
                       : string;
*/
function ChangeFileExt($AFileName, $AFileExt) {
  // LPos : Integer;
  $LPos = 0;
  // Result : string;
  $Result = "";
  $Result = ExtractFileName($AFileName);
  $LPos = RPos(ExtensionSeparator, $Result);
  if ($LPos == 0) {
    $Result = ExtractFilePath($AFileName) . $Result . $AFileExt;
  }
  else {
    $Result = ExtractFilePath($AFileName) . MidStr($Result, 1, $LPos - 1) . $AFileExt;
  }
  return $Result;
}
// function IncludeLeadingPathDelimiter(const AFileName: string) : string;
function IncludeLeadingPathDelimiter($AFileName) {
  // Result : string;
  $Result = "";
  $Result = $AFileName;
  if (Length($Result) > 0 && !in_array($Result[(1) - 1], AllowDirectorySeparators())) {
    $Result = DirectorySeparator . $Result;
  }
  return $Result;
}
// function IncludeTrailingPathDelimiter(const AFileName: string) : string;
function IncludeTrailingPathDelimiter($AFileName) {
  // Result : string;
  $Result = "";
  $Result = $AFileName;
  if (Length($Result) > 0 && !in_array($Result[(Length($Result)) - 1], AllowDirectorySeparators())) {
    $Result = $Result . DirectorySeparator;
  }
  return $Result;
}
?>