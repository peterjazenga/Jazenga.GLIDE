<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('p2pvariants.php');
include_once('strutils.php');
include_once('sysutils.php');
include_once('variants.php');
// function AnsiSplit(const AStr: string;const ADelimiter: Char) : variant;
function AnsiSplit($AStr, $ADelimiter) {
  // LPos : Integer;
  $LPos = 0;
  // LEnd : Integer;
  $LEnd = 0;
  // Result : variant;
  $Result = null;
  $Result = VarArrayOf(array());
  $LPos = 1;
  do {
    $LEnd = PosEx($ADelimiter, $AStr, $LPos);
    if ($LEnd == 0) {
      $LEnd = Length($AStr) + 1;
    }
    VarArraySetLength($Result, VarArrayLength($Result) + 1);
    $Result[VarArrayHigh($Result)] = MidStr($AStr, $LPos, $LEnd - $LPos);
    $LPos = $LEnd + 1;
  }
  while(!($LPos > Length($AStr)));
  return $Result;
}
// function AnsiJoin(const AArray: variant;const ADelimiter: Char) : string;
function AnsiJoin($AArray, $ADelimiter) {
  // LIndex : Integer;
  $LIndex = 0;
  // Result : string;
  $Result = "";
  $Result = EmptyStr;
  if (VarArrayLength($AArray) > 0) {
    $Result = $AArray[0];
    for ($LIndex = 1, $LIndex_High_ = VarArrayHigh($AArray); $LIndex <= $LIndex_High_; $LIndex++) {
      $Result = $Result . $ADelimiter . $AArray[$LIndex];
    }
  }
  return $Result;
}
?>