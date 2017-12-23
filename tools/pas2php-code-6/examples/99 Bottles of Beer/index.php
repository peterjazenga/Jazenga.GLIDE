<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('..\..\pas2php.php');
include_once('system.php');
// BOTTLESSTART = 99;
define("BOTTLESSTART", 99);
// BOTTLESEND = 1;
define("BOTTLESEND", 1);
// LineBreak = '<BR>';
define("LineBreak", "<BR>");
// bottles : Integer;
$bottles = 0;
// procedure Main;
function Main() {
  global $bottles;
  for ($bottles = BOTTLESSTART, $bottles_High_ = BOTTLESEND; $bottles >= $bottles_High_; $bottles--) {
    if ($bottles > 1) {
      Write($bottles, " bottles of beer on the wall, ", $bottles, " bottles of beer." . LineBreak);
      Write("Take one down, pass it around, ");
      Write($bottles - 1, " bottles of beer on the wall." . LineBreak);
      Write(LineBreak);
    }
    else {
      Write("1 bottle of beer on the wall, one bottle of beer." . LineBreak);
      Write("Take one down, pass it around, no more bottles of beer on the wall" . LineBreak);
      Write(LineBreak);
      Write("No more bottles of beer on the wall, no more bottles of beer." . LineBreak);
      Write("Go to the store and buy some more, 99 bottles of beer on the wall." . LineBreak);
    }
  }
}
Main();
?>