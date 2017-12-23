<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('phplib.php');
// function DecodeStringBase64(const AString: string) : string;
function DecodeStringBase64($AString) {
  // Result : string;
  $Result = "";
  $Result = base64_decode($AString);
  return $Result;
}
// function EncodeStringBase64(const AString: string) : string;
function EncodeStringBase64($AString) {
  // Result : string;
  $Result = "";
  $Result = base64_encode($AString);
  return $Result;
}
?>