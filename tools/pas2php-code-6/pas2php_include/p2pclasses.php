<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('classes.php');
include_once('httpdefs.php');
include_once('p2pcontnrs.php');
include_once('p2psysutils.php');
include_once('strutils.php');
include_once('sysutils.php');
class CStringList extends TObject {
  function __get($name) {
    // Count : Integer;
    if (strcasecmp($name, "Count") === 0) {
      return $this->GetCount();
    }
    // CaseSensitive : Boolean;
    if (strcasecmp($name, "CaseSensitive") === 0) {
      return $this->FCaseSensitive;
    }
    // Delimiter : Char;
    if (strcasecmp($name, "Delimiter") === 0) {
      return $this->FDelimiter;
    }
    // NameValueSeparator : Char;
    if (strcasecmp($name, "NameValueSeparator") === 0) {
      return $this->FNameValueSeparator;
    }
    return parent::__get($name);
  }
  function __set($name, $value) {
    // Count : Integer;
    if (strcasecmp($name, "Count") === 0) {
      return $this->SetCount($value);
    }
    // CaseSensitive : Boolean;
    if (strcasecmp($name, "CaseSensitive") === 0) {
      return $this->FCaseSensitive = $value;
    }
    // Delimiter : Char;
    if (strcasecmp($name, "Delimiter") === 0) {
      return $this->FDelimiter = $value;
    }
    // NameValueSeparator : Char;
    if (strcasecmp($name, "NameValueSeparator") === 0) {
      return $this->FNameValueSeparator = $value;
    }
    return parent::__set($name, $value);
  }
  // FItems : Array of string;
  var $FItems = array();
  // FNameValueSeparator : Char;
  var $FNameValueSeparator = "";
  // FDelimiter : Char;
  var $FDelimiter = "";
  // FCaseSensitive : Boolean;
  var $FCaseSensitive = false;
  // constructor Create;
  function __construct() {
    parent::__construct();
    $this->FNameValueSeparator = "=";
    $this->FDelimiter = "\n";
    $this->FCaseSensitive = false;
  }
  // function GetCount : Integer;
  function GetCount() {
    // Result : Integer;
    $Result = 0;
    $Result = Length($this->FItems);
    return $Result;
  }
  // procedure SetCount(const ACount: Integer);
  function SetCount($ACount) {
    SetLength($this->FItems, $ACount);
  }
  // procedure Clear;
  function Clear() {
    SetLength($this->FItems, 0);
  }
  // function Get(const AIndex: Integer) : string;
  function Get($AIndex) {
    // Result : string;
    $Result = "";
    $Result = $this->FItems[$AIndex];
    return $Result;
  }
  // function GetName(const AIndex: Integer) : string;
  function GetName($AIndex) {
    // LPos : Integer;
    $LPos = 0;
    // Result : string;
    $Result = "";
    $Result = $this->FItems[$AIndex];
    $LPos = PosEx($this->FNameValueSeparator, $Result);
    if ($LPos > 0) {
      $Result = AnsiLeftStr($Result, $LPos - 1);
    }
    return $Result;
  }
  // function GetValueFromIndex(const AIndex: Integer) : string;
  function GetValueFromIndex($AIndex) {
    // LPos : Integer;
    $LPos = 0;
    // Result : string;
    $Result = "";
    $Result = $this->FItems[$AIndex];
    $LPos = PosEx($this->FNameValueSeparator, $Result);
    if ($LPos > 0) {
      $Result = MidStr($Result, $LPos + 1, MaxInt);
    }
    else {
      $Result = EmptyStr;
    }
    return $Result;
  }
  // function GetText : string;
  function GetText() {
    // LIndex : Integer;
    $LIndex = 0;
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    for ($LIndex = 0, $LIndex_High_ = High($this->FItems); $LIndex <= $LIndex_High_; $LIndex++) {
      $Result = $Result . $this->FItems[$LIndex] . LineEnding;
    }
    return $Result;
  }
  // function GetValue(const AName: string) : string;
  function GetValue($AName) {
    // LIndex : Integer;
    $LIndex = 0;
    // Result : string;
    $Result = "";
    $LIndex = $this->IndexOfName($AName);
    if ($LIndex < 0) {
      $Result = EmptyStr;
    }
    else {
      $Result = $this->GetValueFromIndex($LIndex);
    }
    return $Result;
  }
  // function IndexOf(const AString: string) : Integer;
  function IndexOf($AString) {
    // Result : Integer;
    $Result = 0;
    for ($Result = 0, $Result_High_ = High($this->FItems); $Result <= $Result_High_; $Result++) {
      if (AnsiSame($AString, $this->FItems[$Result], $this->FCaseSensitive)) {
        return($Result);
      }
    }
    $Result =  - 1;
    return $Result;
  }
  // function IndexOfName(const AName: string) : Integer;
  function IndexOfName($AName) {
    // Result : Integer;
    $Result = 0;
    for ($Result = 0, $Result_High_ = High($this->FItems); $Result <= $Result_High_; $Result++) {
      if (AnsiSame($AName, $this->GetName($Result), $this->FCaseSensitive)) {
        return($Result);
      }
    }
    $Result =  - 1;
    return $Result;
  }
  // function Add(const AString: string) : Integer;
  function Add($AString) {
    // Result : Integer;
    $Result = 0;
    SetLength($this->FItems, Length($this->FItems) + 1);
    $Result = High($this->FItems);
    $this->FItems[$Result] = $AString;
    return $Result;
  }
  // procedure Delete(AIndex: Integer);
  function Delete($AIndex) {
    Enforce($AIndex >= 0 && $AIndex < Length($this->FItems));
    while ($AIndex < High($this->FItems)) {
      $this->FItems[$AIndex] = $this->FItems[$AIndex + 1];
      $AIndex = $AIndex + 1;
    }
    SetLength($this->FItems, Length($this->FItems) - 1);
  }
  // procedure Remove(const AItem: string);
  function Remove($AItem) {
    $this->Delete($this->IndexOf($AItem));
  }
  // procedure RemoveName(const AName: string);
  function RemoveName($AName) {
    $this->Delete($this->IndexOfName($AName));
  }
  // procedure Put(const AIndex: Integer;const AString: string);
  function Put($AIndex, $AString) {
    $this->FItems[$AIndex] = $AString;
  }
  // procedure SetValue(const AName: string;const AValue: string);
  function SetValue($AName, $AValue) {
    // LIndex : Integer;
    $LIndex = 0;
    // LPos : Integer;
    $LPos = 0;
    for ($LIndex = 0, $LIndex_High_ = High($this->FItems); $LIndex <= $LIndex_High_; $LIndex++) {
      $LPos = PosEx($this->FNameValueSeparator, $this->FItems[$LIndex]);
      if ($LPos > 0) {
        if (SameText($AName, AnsiLeftStr($this->FItems[$LIndex], $LPos - 1))) {
          $this->FItems[$LIndex] = $AName . $this->FNameValueSeparator . $AValue;
          return($Result);
        }
      }
    }
    $this->Add($AName . $this->FNameValueSeparator . $AValue);
  }
}
class CStringListEx extends CStringList {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure SetValues(const AValues: Array of string);
  function SetValues($AValues) {
    // LIndex : Integer;
    $LIndex = 0;
    $LIndex = 0;
    while ($LIndex < High($AValues)) {
      $this->SetValue($AValues[$LIndex], $AValues[$LIndex + 1]);
      $LIndex = $LIndex + 2;
    }
  }
  // procedure AddHttp(const AHttp: string);
  function AddHttp($AHttp) {
    // LPos : Integer;
    $LPos = 0;
    // LEqu : Integer;
    $LEqu = 0;
    // LEnd : Integer;
    $LEnd = 0;
    // LLine : string;
    $LLine = "";
    $LPos = 1;
    do {
      $LEnd = PosEx("&", $AHttp, $LPos);
      if ($LEnd == 0) {
        $LEnd = Length($AHttp) + 1;
      }
      $LLine = MidStr($AHttp, $LPos, $LEnd - $LPos);
      $LEqu = PosEx("=", $LLine);
      if ($LEqu > 0) {
        $this->Add(HttpDecode(AnsiLeftStr($LLine, $LEqu - 1)) . "=" . HttpDecode(MidStr($LLine, $LEqu + 1, MaxInt)));
      }
      $LPos = $LEnd + 1;
    }
    while(!($LPos > Length($AHttp)));
  }
}
class CComponent extends TObject {
  function __get($name) {
    // Name : string;
    if (strcasecmp($name, "Name") === 0) {
      return $this->GetName();
    }
    // Value : string;
    if (strcasecmp($name, "Value") === 0) {
      return $this->GetValue();
    }
    // Owner : CComponent;
    if (strcasecmp($name, "Owner") === 0) {
      return $this->GetOwner();
    }
    return parent::__get($name);
  }
  function __set($name, $value) {
    // Name : string;
    if (strcasecmp($name, "Name") === 0) {
      return $this->SetName($value);
    }
    // Value : string;
    if (strcasecmp($name, "Value") === 0) {
      return $this->SetValue($value);
    }
    // Owner : CComponent;
    if (strcasecmp($name, "Owner") === 0) {
      return $this->SetOwner($value);
    }
    return parent::__set($name, $value);
  }
  // FOwner : CComponent;
  var $FOwner = null;
  // FObjects : CObjectList;
  var $FObjects = null;
  // FName : string;
  var $FName = "";
  // FValue : string;
  var $FValue = "";
  // function GetName : string;  Virtual;
  function GetName() {
    // Result : string;
    $Result = "";
    $Result = $this->FName;
    return $Result;
  }
  // function GetOwner : CComponent;  Virtual;
  function GetOwner() {
    // Result : CComponent;
    $Result = null;
    $Result = $this->FOwner;
    return $Result;
  }
  // function GetValue : string;  Virtual;
  function GetValue() {
    // Result : string;
    $Result = "";
    $Result = $this->FValue;
    return $Result;
  }
  // procedure SetName(const AName: string);  Virtual;
  function SetName($AName) {
    $this->FName = $AName;
  }
  // procedure SetOwner(const AOwner: CComponent);  Virtual;
  function SetOwner($AOwner) {
    if (Assigned($this->FOwner)) {
      $this->FOwner->FObjects->Remove($this);
    }
    $this->FOwner = $AOwner;
    if (Assigned($this->FOwner)) {
      $this->FOwner->FObjects->Add($this);
    }
  }
  // procedure SetValue(const AValue: string);  Virtual;
  function SetValue($AValue) {
    $this->FValue = $AValue;
  }
/*
constructor Create(const AOwner: CComponent;const AValue: string;
                  const AName: string);
*/
  function __construct($AOwner, $AValue = "", $AName = "") {
    parent::__construct();
    $this->FObjects = new CObjectList();
    $this->SetName($AName);
    $this->SetValue($AValue);
    $this->SetOwner($AOwner);
    $this->DoCreate();
  }
  // destructor Destroy;  Override;
  function __destruct() {
    FreeAndNil($this->FObjects);
    parent::__destruct();
  }
  // function GetComponents(const AIndex: Integer) : CComponent;
  function GetComponents($AIndex) {
    // Result : CComponent;
    $Result = null;
    $Result = $this->FObjects->GetItem($AIndex);
    return $Result;
  }
  // function ComponentCount : Integer;
  function ComponentCount() {
    // Result : Integer;
    $Result = 0;
    $Result = $this->FObjects->Count;
    return $Result;
  }
  // procedure DoCreate;  Virtual;
  function DoCreate() {
  }
/*
function Spawn(const AValue: string;const AName: string) : CComponent
              ;  Virtual;
*/
  function Spawn($AValue = "", $AName = "") {
    // Result : CComponent;
    $Result = null;
    $Result = new CComponent($this, $AValue, $AName);
    return $Result;
  }
}
?>