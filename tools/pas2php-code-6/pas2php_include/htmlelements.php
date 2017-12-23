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
// function EscapeHTML(const AString: string) : string;
function EscapeHTML($AString) {
  // Result : string;
  $Result = "";
  $Result = htmlspecialchars($AString);
  return $Result;
}
// function UnescapeHTML(const AString: string) : string;
function UnescapeHTML($AString) {
  // Result : string;
  $Result = "";
  $Result = htmlspecialchars_decode($AString);
  return $Result;
}
?>