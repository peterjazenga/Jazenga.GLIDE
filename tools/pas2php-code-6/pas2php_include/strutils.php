<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('p2pvariants.php');
include_once('phplib.php');
include_once('sysutils.php');
include_once('variants.php');
// function ReverseString(const AStr: string) : string;
function ReverseString($AStr) {
  // Result : string;
  $Result = "";
  $Result = strrev($AStr);
  return $Result;
}
// function AnsiReverseString(const AStr: string) : string;
function AnsiReverseString($AStr) {
  // Result : string;
  $Result = "";
  $Result = strrev($AStr);
  return $Result;
}
// function AnsiSameStr(const A: string;const B: string) : Boolean;
function AnsiSameStr($A, $B) {
  // Result : Boolean;
  $Result = false;
  $Result = strcmp($A, $B) == 0;
  return $Result;
}
// function AnsiSameText(const A: string;const B: string) : Boolean;
function AnsiSameText($A, $B) {
  // Result : Boolean;
  $Result = false;
  $Result = strcasecmp($A, $B) == 0;
  return $Result;
}
/*
function ReplaceText(const AStr: string;const AFrom: string;
                    const ATo: string) : string;
*/
function ReplaceText($AStr, $AFrom, $ATo) {
  // Result : string;
  $Result = "";
  $Result = str_ireplace($AFrom, $ATo, $AStr);
  return $Result;
}
/*
function ReplaceStr(const AStr: string;const AFrom: string;
                   const ATo: string) : string;
*/
function ReplaceStr($AStr, $AFrom, $ATo) {
  // Result : string;
  $Result = "";
  $Result = str_replace($AFrom, $ATo, $AStr);
  return $Result;
}
/*
function AnsiReplaceText(const AStr: string;const AFrom: string;
                        const ATo: string) : string;
*/
function AnsiReplaceText($AStr, $AFrom, $ATo) {
  // Result : string;
  $Result = "";
  $Result = str_ireplace($AFrom, $ATo, $AStr);
  return $Result;
}
/*
function AnsiReplaceStr(const AStr: string;const AFrom: string;
                       const ATo: string) : string;
*/
function AnsiReplaceStr($AStr, $AFrom, $ATo) {
  // Result : string;
  $Result = "";
  $Result = str_replace($AFrom, $ATo, $AStr);
  return $Result;
}
// function RPos(const ASub: string;const AStr: string) : Integer;
function RPos($ASub, $AStr) {
  // Result : Integer;
  $Result = 0;
  $Result = strrpos($AStr, $ASub, 0);
  if (is_bool($Result)) {
    $Result = 0;
  }
  else {
    $Result = $Result + 1;
  }
  return $Result;
}
/*
function RPosEx(const ASub: string;const AStr: string;
               const AIdx: Integer) : Integer;
*/
function RPosEx($ASub, $AStr, $AIdx) {
  // Result : Integer;
  $Result = 0;
  $Result = strrpos($AStr, $ASub, $AIdx - 1);
  if (is_bool($Result)) {
    $Result = 0;
  }
  else {
    $Result = $Result + 1;
  }
  return $Result;
}
/*
function IfThen(const ABoolean: Boolean;const A: string;const B: string)
                : string;
*/
function IfThen($ABoolean, $A, $B) {
  // Result : string;
  $Result = "";
  if ($ABoolean) {
    $Result = $A;
  }
  else {
    $Result = $B;
  }
  return $Result;
}
/*
function AnsiIndexStr(const AStr: string;const AArray: variant)
                      : Integer;
*/
function AnsiIndexStr($AStr, $AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = VarArrayIndexStr($AStr, $AArray);
  return $Result;
}
/*
function AnsiIndexText(const AStr: string;const AArray: variant)
                       : Integer;
*/
function AnsiIndexText($AStr, $AArray) {
  // Result : Integer;
  $Result = 0;
  $Result = VarArrayIndexText($AStr, $AArray);
  return $Result;
}
/*
function AnsiMatchStr(const AStr: string;const AArray: variant)
                      : Boolean;
*/
function AnsiMatchStr($AStr, $AArray) {
  // Result : Boolean;
  $Result = false;
  $Result = VarArrayMatchStr($AStr, $AArray);
  return $Result;
}
/*
function AnsiMatchText(const AStr: string;const AArray: variant)
                       : Boolean;
*/
function AnsiMatchText($AStr, $AArray) {
  // Result : Boolean;
  $Result = false;
  $Result = VarArrayMatchText($AStr, $AArray);
  return $Result;
}
// function AnsiLeftStr(const AStr: string;const ALen: Integer) : string;
function AnsiLeftStr($AStr, $ALen) {
  // Result : string;
  $Result = "";
  $Result = MidStr($AStr, 1, $ALen);
  return $Result;
}
// function AnsiRightStr(const AStr: string;const ALen: Integer) : string;
function AnsiRightStr($AStr, $ALen) {
  // Result : string;
  $Result = "";
  $Result = MidStr($AStr, Length($AStr) - $ALen + 1, $ALen);
  return $Result;
}
// function AnsiStartsStr(const ASub: string;const AStr: string) : Boolean;
function AnsiStartsStr($ASub, $AStr) {
  // Result : Boolean;
  $Result = false;
  $Result = AnsiSameStr($ASub, AnsiLeftStr($AStr, Length($ASub)));
  return $Result;
}
// function AnsiEndsStr(const ASub: string;const AStr: string) : Boolean;
function AnsiEndsStr($ASub, $AStr) {
  // Result : Boolean;
  $Result = false;
  $Result = AnsiSameStr($ASub, AnsiRightStr($AStr, Length($ASub)));
  return $Result;
}
// function AnsiStartsText(const ASub: string;const AStr: string) : Boolean;
function AnsiStartsText($ASub, $AStr) {
  // Result : Boolean;
  $Result = false;
  $Result = AnsiSameText($ASub, AnsiLeftStr($AStr, Length($ASub)));
  return $Result;
}
// function AnsiEndsText(const ASub: string;const AStr: string) : Boolean;
function AnsiEndsText($ASub, $AStr) {
  // Result : Boolean;
  $Result = false;
  $Result = AnsiSameText($ASub, AnsiRightStr($AStr, Length($ASub)));
  return $Result;
}
/*
function StuffString(const AStr: string;const APos: Integer;
                    const ALen: Integer;const ASub: string) : string;
*/
function StuffString($AStr, $APos, $ALen, $ASub) {
  // Result : string;
  $Result = "";
  $Result = MidStr($AStr, 1, $APos - 1) . $ASub . MidStr($AStr, $APos + $ALen, MaxInt);
  return $Result;
}
/*
function PosEx(const ASubString: variant;const AString: string;
              const AOffset: Integer) : Integer;
*/
function PosEx($ASubString, $AString, $AOffset = 1) {
  // Result : Integer;
  $Result = 0;
  $Result = strpos($AString, $ASubString, $AOffset - 1);
  if (is_bool($Result)) {
    $Result = 0;
  }
  else {
    $Result = $Result + 1;
  }
  return $Result;
}
/*
function PosSet(const ASubString: variant;const AString: string)
                : Integer;
*/
function PosSet($ASubString, $AString) {
  // Result : Integer;
  $Result = 0;
  $Result = PosSetEx($ASubString, $AString, 1);
  return $Result;
}
/*
function PosSetEx(const ASubString: variant;const AString: string;
                 const AOffset: Integer) : Integer;
*/
function PosSetEx($ASubString, $AString, $AOffset) {
  // LIndex : Integer;
  $LIndex = 0;
  // Result : Integer;
  $Result = 0;
  $Result = 0;
  if (VarIsStr($ASubString)) {
    for ($LIndex = 1, $LIndex_High_ = Length($ASubString); $LIndex <= $LIndex_High_; $LIndex++) {
      $Result = PosEx(VarToStr($ASubString[$LIndex]), $AString, $AOffset);
      if ($Result > 0) {
        break;
      }
    }
  }
  else {
    if (VarIsArray($ASubString)) {
      for ($LIndex = 0, $LIndex_High_ = VarArrayHigh($ASubString); $LIndex <= $LIndex_High_; $LIndex++) {
        $Result = PosEx(VarToStr($ASubString[$LIndex]), $AString, $AOffset);
        if ($Result > 0) {
          break;
        }
      }
    }
    else {
      throw new EArgumentException("Invalid SubString.");
    }
  }
  return $Result;
}
?>