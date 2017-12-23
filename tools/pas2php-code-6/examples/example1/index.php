<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('..\..\pas2php.php');
include_once('system.php');
// procedure Main;
function Main() {
  // LIndex : Integer;
  $LIndex = 0;
  // LSum : Integer;
  $LSum = 0;
  $LSum = 0;
  for ($LIndex = 0, $LIndex_High_ = 10; $LIndex <= $LIndex_High_; $LIndex++) {
    Inc($LSum, $LIndex);
    WriteLn($LIndex, "</BR>");
  }
  WriteLn("Sum = ", $LSum);
}
Main();
?>