<?PHP
// 
// Converted From Pascal By Pas2Php v0.4
// Converts Standard Pascal to PHP
// http://www.wascal.net/pas2php/
// Pas2Php v0.4 is Copyright (C) 2014 Derek John Evans
// 
include_once('system.php');
include_once('httpdefs.php');
include_once('phplib.php');
include_once('strutils.php');
include_once('sysutils.php');
class CStringList extends TObject {
  var $Items = null;
  function GetCount() {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    $Result = 0;
    $Result = VarArrayLength($this->Items);
    return $Result;
  }
  function SetCount($ACount) {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    VarArraySetLength($this->Items, $ACount);
  }
  function Add($AStr) {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    $this->SetCount($this->GetCount() + 1);
    $this->Items[$this->GetCount() - 1] = $AStr;
  }
  function AddHttp($AStr) {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    $LPos = 0;
    $LEqu = 0;
    $LEnd = 0;
    $LLine = '';
    $LPos = 1;
    do {
      $LEnd = PosEx("&", $AStr, $LPos);
      if ($LEnd == 0) {
        $LEnd = Length($AStr) + 1;
      }
      $this->SetCount($this->GetCount() + 1);
      $LLine = Pas2PhpCopy($AStr, $LPos, $LEnd - $LPos);
      $LEqu = Pas2PhpPos("=", $LLine);
      if ($LEqu > 0) {
        $this->Items[$this->GetCount() - 1] = Pas2PhpCopy($LLine, 1, $LEqu - 1) . "=" . urldecode(Pas2PhpCopy($LLine, $LEqu + 1, MaxInt));
      }
      $LPos = $LEnd + 1;
    }
    while(!($LPos > Length($AStr)));
  }
  function GetText() {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    $LIdx = 0;
    $Result = '';
    $Result = '';
    for ($LIdx = 0, $LIdx_End = $this->GetCount() - 1; $LIdx <= $LIdx_End; $LIdx++) {
      $Result = $Result . '' . $this->Items[$LIdx];
    }
    return $Result;
  }
  function GetValue($AName, $ADefault) {
    global $GQueryList, $GPageTop, $GPageMid, $GPageBot;
    $LIdx = 0;
    $LPos = 0;
    $Result = '';
    $Result = $ADefault;
    for ($LIdx = 0, $LIdx_End = $this->GetCount() - 1; $LIdx <= $LIdx_End; $LIdx++) {
      $LPos = Pas2PhpPos("=", $this->Items[$LIdx]);
      if ($LPos > 0) {
        if (SameText(Pas2PhpCopy($this->Items[$LIdx], 1, $LPos - 1), $AName)) {
          $Result = Pas2PhpCopy($this->Items[$LIdx], $LPos + 1, MaxInt);
          break;
        }
      }
    }
    return $Result;
  }
}
?>