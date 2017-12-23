<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('sysutils.php');
/*
function AnsiCompare(const A: string;const B: string;
                    const ACaseSensitive: Boolean) : Integer;
*/
function AnsiCompare($A, $B, $ACaseSensitive) {
  // Result : Integer;
  $Result = 0;
  if ($ACaseSensitive) {
    $Result = AnsiCompareStr($A, $B);
  }
  else {
    $Result = AnsiCompareText($A, $B);
  }
  return $Result;
}
/*
function AnsiSame(const A: string;const B: string;
                 const ACaseSensitive: Boolean) : Boolean;
*/
function AnsiSame($A, $B, $ACaseSensitive) {
  // Result : Boolean;
  $Result = false;
  if ($ACaseSensitive) {
    $Result = AnsiSameStr($A, $B);
  }
  else {
    $Result = AnsiSameText($A, $B);
  }
  return $Result;
}
?>