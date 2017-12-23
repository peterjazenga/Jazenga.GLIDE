<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('classes.php');
include_once('dateutils.php');
include_once('fileutil.php');
include_once('httpdefs.php');
include_once('p2pcgi.php');
include_once('sysutils.php');
// function SessionGetString(const AName: string) : string;
function SessionGetString($AName) {
  // Result : string;
  $Result = "";
  $Result = $_SESSION[$AName];
  if (!is_string($Result)) {
    throw new EArgumentException("Unable to find \"" . $AName . "\".");
  }
  return $Result;
}
/*
function SessionGetStringDef(const AName: string;const ADefault: string)
                             : string;
*/
function SessionGetStringDef($AName, $ADefault = "") {
  // Result : string;
  $Result = "";
  try {
    $Result = SessionGetString($AName);
  }
  catch (Exception $exception) {
    $Result = $ADefault;
  }
  return $Result;
}
// procedure SessionSetString(const AName: string;const AValue: string);
function SessionSetString($AName, $AValue) {
  $_SESSION[$AName] = $AValue;
}
/*
procedure SessionSetFromCgiDef(const AName: string;
                              const ADefault: string);
*/
function SessionSetFromCgiDef($AName, $ADefault = "") {
  SessionSetString($AName, CgiGetParamDef($AName, $ADefault));
}
?>