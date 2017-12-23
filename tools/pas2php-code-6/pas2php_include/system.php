<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('p2pvariants.php');
include_once('phplib.php');
include_once('sysutils.php');
include_once('variants.php');
// MaxInt = $7fffffff;
define("MaxInt", 2147483647);
// DirectorySeparator = DIRECTORY_SEPARATOR;
define("DirectorySeparator", DIRECTORY_SEPARATOR);
// ExtensionSeparator = '.';
define("ExtensionSeparator", ".");
// DriveSeparator = ':';
define("DriveSeparator", ":");
// PathSeparator = ';';
define("PathSeparator", ";");
// LineEnding = #13#10;
define("LineEnding", "\r\n");
// MaxPathLen = 260;
define("MaxPathLen", 260);
// AllFilesMask = '*';
define("AllFilesMask", "*");
// Null = null;
define("Null", null);
// Unassigned = null;
define("Unassigned", null);
class TObject {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // constructor Create;
  function __construct() {
  }
  // destructor Destroy;  Virtual;
  function __destruct() {
  }
  // procedure Free;
  function Free() {
  }
  // function ClassName : string;
  function ClassName() {
    // Result : string;
    $Result = "";
    $Result = get_class($this);
    return $Result;
  }
}
// function string(const AVariant: variant) : string;
function string($AVariant) {
  // Result : string;
  $Result = "";
  $Result = strval($AVariant);
  return $Result;
}
// function shortstring(const AVariant: variant) : string;
function shortstring($AVariant) {
  // Result : string;
  $Result = "";
  $Result = strval($AVariant);
  return $Result;
}
// function ansistring(const AVariant: variant) : string;
function ansistring($AVariant) {
  // Result : string;
  $Result = "";
  $Result = strval($AVariant);
  return $Result;
}
// function widestring(const AVariant: variant) : string;
function widestring($AVariant) {
  // Result : string;
  $Result = "";
  $Result = strval($AVariant);
  return $Result;
}
// function char(const AInteger: Integer) : string;
function char($AInteger) {
  // Result : string;
  $Result = "";
  $Result = chr($AInteger);
  return $Result;
}
// function integer(const AVariant: variant) : Integer;
function integer($AVariant) {
  // Result : Integer;
  $Result = 0;
  $Result = intval($AVariant, 10);
  return $Result;
}
// function int64(const AVariant: variant) : Integer;
function int64($AVariant) {
  // Result : Integer;
  $Result = 0;
  $Result = intval($AVariant, 10);
  return $Result;
}
// function smallint(const AVariant: variant) : Integer;
function smallint($AVariant) {
  // Result : Integer;
  $Result = 0;
  $Result = intval($AVariant, 10);
  return $Result;
}
// function longint(const AVariant: variant) : Integer;
function longint($AVariant) {
  // Result : Integer;
  $Result = 0;
  $Result = intval($AVariant, 10);
  return $Result;
}
// function single(const AVariant: variant) : extended;
function single($AVariant) {
  // Result : extended;
  $Result = 0.0;
  $Result = floatval($AVariant);
  return $Result;
}
// function double(const AVariant: variant) : extended;
function double($AVariant) {
  // Result : extended;
  $Result = 0.0;
  $Result = floatval($AVariant);
  return $Result;
}
// function extended(const AVariant: variant) : extended;
function extended($AVariant) {
  // Result : extended;
  $Result = 0.0;
  $Result = floatval($AVariant);
  return $Result;
}
// function real(const AVariant: variant) : extended;
function real($AVariant) {
  // Result : extended;
  $Result = 0.0;
  $Result = floatval($AVariant);
  return $Result;
}
// function variant(const AVariant: variant) : variant;
function variant($AVariant) {
  // Result : variant;
  $Result = null;
  $Result = $AVariant;
  return $Result;
}
// function Assigned(const AVariant: variant) : Boolean;
function Assigned($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = !is_null($AVariant);
  return $Result;
}
// procedure Enforce(const ATrue: Boolean;const AMessage: string);
function Enforce($ATrue, $AMessage = "Enforce") {
  if (!$ATrue) {
    throw new EAssertionFailed($AMessage);
  }
}
// function Trunc(const AExtended: extended) : extended;
function Trunc($AExtended) {
  // Result : extended;
  $Result = 0.0;
  $Result = intval($AExtended, 10);
  return $Result;
}
// function SystemOrd(const AVariant: variant) : Integer;
function SystemOrd($AVariant) {
  // Result : Integer;
  $Result = 0;
  if (is_string($AVariant)) {
    $Result = ord($AVariant);
  }
  else {
    if (is_int($AVariant)) {
      $Result = $AVariant;
    }
    else {
      throw new EArgumentException("Invalid Ordinal Type");
    }
  }
  return $Result;
}
// function Pred(const AVariant: variant) : variant;
function Pred($AVariant) {
  // LString : string;
  $LString = "";
  // Result : variant;
  $Result = null;
  if (is_string($AVariant)) {
    $Result = chr(ord($AVariant[0]) - 1);
  }
  else {
    $Result = $AVariant - 1;
  }
  return $Result;
}
// function Succ(const AVariant: variant) : variant;
function Succ($AVariant) {
  // Result : variant;
  $Result = null;
  if (is_string($AVariant)) {
    $Result = chr(ord($AVariant[0]) + 1);
  }
  else {
    $Result = $AVariant + 1;
  }
  return $Result;
}
// procedure Inc(var AInteger: Integer;const AOffset: Integer);
function Inc(&$AInteger, $AOffset = 1) {
  $AInteger = $AInteger + $AOffset;
}
// procedure Dec(var AInteger: Integer;const AOffset: Integer);
function Dec(&$AInteger, $AOffset = 1) {
  $AInteger = $AInteger - $AOffset;
}
// function AllowDirectorySeparators : variant;
function AllowDirectorySeparators() {
  // Result : variant;
  $Result = null;
  $Result = VarArrayOf(array("\\", "/"));
  return $Result;
}
/*
function MidStr(const AString: string;const AOffset: Integer;
               const ALength: Integer) : string;
*/
function MidStr($AString, $AOffset, $ALength) {
  // Result : string;
  $Result = "";
  $Result = substr($AString, $AOffset - 1, $ALength);
  return $Result;
}
// function Length(const AVariant: variant) : Integer;
function Length($AVariant) {
  // Result : Integer;
  $Result = 0;
  if (is_string($AVariant)) {
    $Result = strlen($AVariant);
  }
  else {
    if (is_array($AVariant)) {
      $Result = count($AVariant);
    }
    else {
      $Result = 0;
    }
  }
  return $Result;
}
/*
procedure SetLength(var AVariant: variant;const AD1: Integer;
                   const AD2: Integer;const AD3: Integer);
*/
function SetLength(&$AVariant, $AD1, $AD2 =  - 1, $AD3 =  - 1) {
  if (is_string($AVariant)) {
    if ($AD1 < strlen($AVariant)) {
      $AVariant = substr($AVariant, 0, $AD1);
    }
    else {
      $AVariant = str_pad($AVariant, $AD1);
    }
  }
  else {
    if (is_array($AVariant)) {
      VarArraySetLength($AVariant, $AD1, $AD2, $AD3);
    }
  }
}
// function Low(const AVariant: variant) : Integer;
function Low($AVariant) {
  // Result : Integer;
  $Result = 0;
  if (is_array($AVariant)) {
    $Result = 0;
  }
  else {
    if (is_string($AVariant)) {
      $Result = 1;
    }
  }
  return $Result;
}
// function High(const AVariant: variant) : Integer;
function High($AVariant) {
  // Result : Integer;
  $Result = 0;
  $Result = Length($AVariant);
  if (is_array($AVariant)) {
    $Result = $Result - 1;
  }
  return $Result;
}
// function upcase(const AString: string) : string;
function upcase($AString) {
  // Result : string;
  $Result = "";
  $Result = strtoupper($AString);
  return $Result;
}
// function lowercase(const AString: string) : string;
function lowercase($AString) {
  // Result : string;
  $Result = "";
  $Result = strtolower($AString);
  return $Result;
}
// procedure Write;
function Write() {
  // LIndex : Integer;
  $LIndex = 0;
  // AArgs : variant;
  $AArgs = null;
  $AArgs = func_get_args();
  for ($LIndex = 0, $LIndex_High_ = count($AArgs) - 1; $LIndex <= $LIndex_High_; $LIndex++) {
    echo($AArgs[$LIndex]);
  }
}
// procedure WriteLn;
function WriteLn() {
  // LIndex : Integer;
  $LIndex = 0;
  // LArgs : variant;
  $LArgs = null;
  $LArgs = func_get_args();
  for ($LIndex = 0, $LIndex_High_ = count($LArgs) - 1; $LIndex <= $LIndex_High_; $LIndex++) {
    echo($LArgs[$LIndex]);
  }
  echo(LineEnding);
}
?>