<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('phplib.php');
include_once('sysutils.php');
// function VarIsArray(const AVariant: variant) : Boolean;
function VarIsArray($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_array($AVariant);
  return $Result;
}
// function VarIsBool(const AVariant: variant) : Boolean;
function VarIsBool($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_bool($AVariant);
  return $Result;
}
// function VarIsFloat(const AVariant: variant) : Boolean;
function VarIsFloat($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_float($AVariant);
  return $Result;
}
// function VarIsNull(const AVariant: variant) : Boolean;
function VarIsNull($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_null($AVariant);
  return $Result;
}
// function VarIsNumeric(const AVariant: variant) : Boolean;
function VarIsNumeric($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_numeric($AVariant);
  return $Result;
}
// function VarIsStr(const AVariant: variant) : Boolean;
function VarIsStr($AVariant) {
  // Result : Boolean;
  $Result = false;
  $Result = is_string($AVariant);
  return $Result;
}
/*
function VarToStrDef(const AVariant: variant;const ADefault: string)
                     : string;
*/
function VarToStrDef($AVariant, $ADefault) {
  // Result : string;
  $Result = "";
  if (VarIsNull($AVariant)) {
    $Result = $ADefault;
  }
  else {
    $Result = strval($AVariant);
  }
  return $Result;
}
// function VarToStr(const AVariant: variant) : string;
function VarToStr($AVariant) {
  // Result : string;
  $Result = "";
  $Result = VarToStrDef($AVariant, EmptyStr);
  return $Result;
}
// function VarArrayOf(const AVariant: variant) : variant;
function VarArrayOf($AVariant) {
  // Result : variant;
  $Result = null;
  $Result = $AVariant;
  return $Result;
}
/*
function VarArrayLowBound(const AVariant: variant;const ADim: Integer)
                          : Integer;
*/
function VarArrayLowBound($AVariant, $ADim) {
  // Result : Integer;
  $Result = 0;
  Enforce($ADim == 1);
  $Result = 0;
  return $Result;
}
/*
function VarArrayHighBound(const AVariant: variant;const ADim: Integer)
                           : Integer;
*/
function VarArrayHighBound($AVariant, $ADim) {
  // Result : Integer;
  $Result = 0;
  Enforce($ADim == 1);
  $Result = count($AVariant) - 1;
  return $Result;
}
// procedure VarArrayRedim(var AVariant: variant;const AHigh: Integer);
function VarArrayRedim(&$AVariant, $AHigh) {
  // LIndex : Integer;
  $LIndex = 0;
  for ($LIndex = count($AVariant) - 1, $LIndex_High_ = $AHigh + 1; $LIndex >= $LIndex_High_; $LIndex--) {
    unset($AVariant[$LIndex]);
  }
  for ($LIndex = count($AVariant), $LIndex_High_ = $AHigh; $LIndex <= $LIndex_High_; $LIndex++) {
    $AVariant[$LIndex] = Null;
  }
}
?>