<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('httpdefs.php');
include_once('p2psysutils.php');
include_once('sysutils.php');
include_once('variants.php');
@define("vcmStrict", 0);
@define("vcmLoose", 1);
@define("vcmCaseSensitive", 2);
@define("vcmCaseInsensitive", 3);
/*
function VarGetItem(const AVariant: variant;const AIndex: Integer)
                    : variant;
*/
function VarGetItem($AVariant, $AIndex) {
  // Result : variant;
  $Result = null;
  if (VarIsStr($AVariant)) {
    $Result = $AVariant[$AIndex - 1];
  }
  else {
    $Result = $AVariant[$AIndex];
  }
  return $Result;
}
/*
procedure VarSetItem(var AVariant: variant;const AIndex: Integer;
                    const AItem: variant);
*/
function VarSetItem(&$AVariant, $AIndex, $AItem) {
  if (VarIsStr($AVariant)) {
    $AVariant[$AIndex - 1] = $AItem;
  }
  else {
    $AVariant[$AIndex] = $AItem;
  }
}
// function VarArrayLow(const AArray: variant) : Integer;
function VarArrayLow($AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = 0;
  return $Result;
}
// function VarArrayHigh(const AArray: variant) : Integer;
function VarArrayHigh($AArray) {
  // Result : Integer;
  $Result = 0;
  if (VarIsArray($AArray)) {
    $Result = VarArrayHighBound($AArray, 1);
  }
  else {
    $Result =  - 1;
  }
  return $Result;
}
// function VarArrayLength(const AArray: variant) : Integer;
function VarArrayLength($AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = VarArrayHigh($AArray) + 1;
  return $Result;
}
// procedure VarArraySetLength1(var AArray: variant;const ALength: Integer);
function VarArraySetLength1(&$AArray, $ALength) {
  if (!VarIsArray($AArray)) {
    $AArray = VarArrayOf(array());
  }
  VarArrayRedim($AArray, $ALength - 1);
}
/*
procedure VarArraySetLength(var AArray: variant;const AD1: Integer;
                           const AD2: Integer;const AD3: Integer);
*/
function VarArraySetLength(&$AArray, $AD1, $AD2 =  - 1, $AD3 =  - 1) {
  // I : Integer;
  $I = 0;
  // J : Integer;
  $J = 0;
  VarArraySetLength1($AArray, $AD1);
  if ($AD2 >= 0) {
    for ($I = 0, $I_High_ = $AD1 - 1; $I <= $I_High_; $I++) {
      VarArraySetLength1($AArray[$I], $AD2);
    }
    if ($AD3 >= 0) {
      for ($I = 0, $I_High_ = $AD1 - 1; $I <= $I_High_; $I++) {
        for ($J = 0, $J_High_ = $AD2 - 1; $J <= $J_High_; $J++) {
          VarArraySetLength1($AArray[$I][$J], $AD2);
        }
      }
    }
  }
}
/*
function VarArrayGetItem(var AArray: variant;const AIndex: Integer;
                        const ADefault: variant) : variant;
*/
function VarArrayGetItem(&$AArray, $AIndex, $ADefault) {
  // Result : variant;
  $Result = null;
  if ($AIndex < 0 || $AIndex > VarArrayHigh($AArray)) {
    $Result = $ADefault;
  }
  else {
    $Result = $AArray[$AIndex];
  }
  return $Result;
}
// function VarIsSameType(const A: variant;const B: variant) : Boolean;
function VarIsSameType($A, $B) {
  // Result : Boolean;
  $Result = false;
  $Result = gettype($A) == gettype($B);
  return $Result;
}
/*
function VarIsSame(const A: variant;const B: variant;
                  const AMode: TVarCompareMode) : Boolean;
*/
function VarIsSame($A, $B, $AMode) {
  // Result : Boolean;
  $Result = false;
  switch ($AMode) {
    case vcmStrict: {
      $Result = VarIsSameType($A, $B) && $A == $B;
      break;
    }
    case vcmLoose: {
      $Result = $A == $B;
      break;
    }
    case vcmCaseInsensitive: {
      $Result = AnsiSame(VarToStr($A), VarToStr($B), false);
      break;
    }
    case vcmCaseSensitive: {
      $Result = AnsiSame(VarToStr($A), VarToStr($B), true);
      break;
    }
  }
  return $Result;
}
/*
function VarArrayIndex(const AValue: variant;const AArray: variant;
                      const AMode: TVarCompareMode) : Integer;
*/
function VarArrayIndex($AValue, $AArray, $AMode) {
  // Result : Integer;
  $Result = 0;
  for ($Result = 0, $Result_High_ = VarArrayHigh($AArray); $Result <= $Result_High_; $Result++) {
    if (VarIsSame($AValue, $AArray[$Result], $AMode)) {
      return($Result);
    }
  }
  $Result =  - 1;
  return $Result;
}
/*
function VarArrayMatch(const AValue: variant;const AArray: variant;
                      const AMode: TVarCompareMode) : Boolean;
*/
function VarArrayMatch($AValue, $AArray, $AMode) {
  // LIndex : Integer;
  $LIndex = 0;
  // Result : Boolean;
  $Result = false;
  for ($LIndex = 0, $LIndex_High_ = VarArrayHigh($AArray); $LIndex <= $LIndex_High_; $LIndex++) {
    if (VarIsSame($AValue, $AArray[$LIndex], $AMode)) {
      return(true);
    }
  }
  $Result = false;
  return $Result;
}
/*
function VarArrayMatchStr(const AValue: variant;const AArray: variant)
                          : Boolean;
*/
function VarArrayMatchStr($AValue, $AArray) {
  // Result : Boolean;
  $Result = false;
  $Result = VarArrayMatch($AValue, $AArray, vcmCaseSensitive);
  return $Result;
}
/*
function VarArrayMatchText(const AValue: variant;const AArray: variant)
                           : Boolean;
*/
function VarArrayMatchText($AValue, $AArray) {
  // Result : Boolean;
  $Result = false;
  $Result = VarArrayMatch($AValue, $AArray, vcmCaseInsensitive);
  return $Result;
}
/*
function VarArrayIndexStr(const AValue: variant;const AArray: variant)
                          : Integer;
*/
function VarArrayIndexStr($AValue, $AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = VarArrayIndex($AValue, $AArray, vcmCaseSensitive);
  return $Result;
}
/*
function VarArrayIndexText(const AValue: variant;const AArray: variant)
                           : Integer;
*/
function VarArrayIndexText($AValue, $AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = VarArrayIndex($AValue, $AArray, vcmCaseInsensitive);
  return $Result;
}
/*
function VarArrayGetValue(const AArray: variant;const AName: string)
                          : string;
*/
function VarArrayGetValue($AArray, $AName) {
  // LIndex : Integer;
  $LIndex = 0;
  // LPos : Integer;
  $LPos = 0;
  // LString : string;
  $LString = "";
  // Result : string;
  $Result = "";
  $Result = EmptyStr;
  for ($LIndex = 0, $LIndex_High_ = VarArrayLength($AArray) - 1; $LIndex <= $LIndex_High_; $LIndex++) {
    $LString = $AArray[$LIndex];
    $LPos = PosEx("=", $LString);
    if ($LPos > 0) {
      if (SameText(MidStr($LString, 1, $LPos - 1), $AName)) {
        $Result = MidStr($LString, $LPos + 1, MaxInt);
      }
    }
  }
  return $Result;
}
/*
function VarArrayGetValueHttp(const AArray: variant;const AName: string)
                              : string;
*/
function VarArrayGetValueHttp($AArray, $AName) {
  // Result : string;
  $Result = "";
  $Result = HttpDecode(VarArrayGetValue($AArray, $AName));
  return $Result;
}
?>