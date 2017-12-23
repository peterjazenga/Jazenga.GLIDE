<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('phplib.php');
// function HttpEncode(const AString: string) : string;
function HttpEncode($AString) {
  // Result : string;
  $Result = "";
  $Result = urlencode($AString);
  return $Result;
}
// function HttpDecode(const AString: string) : string;
function HttpDecode($AString) {
  // Result : string;
  $Result = "";
  $Result = urldecode($AString);
  return $Result;
}
?>