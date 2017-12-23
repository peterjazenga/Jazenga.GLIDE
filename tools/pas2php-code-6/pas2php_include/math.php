<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('phplib.php');
// function Power(const A: extended;const B: extended) : extended;
function Power($A, $B) {
  // Result : extended;
  $Result = 0.0;
  $Result = pow($A, $B);
  return $Result;
}
?>