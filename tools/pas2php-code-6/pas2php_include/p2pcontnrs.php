<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('sysutils.php');
// A = True;
define("A", true);
class CObjectList extends TObject {
  function __get($name) {
    // Count : Integer;
    if (strcasecmp($name, "Count") === 0) {
      return $this->GetCount();
    }
    return parent::__get($name);
  }
  function __set($name, $value) {
    // Count : Integer;
    if (strcasecmp($name, "Count") === 0) {
      return $this->SetCount($value);
    }
    return parent::__set($name, $value);
  }
  // FFreeObjects : Boolean;
  var $FFreeObjects = false;
  // FItems : Array of TObject;
  var $FItems = array();
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
  // constructor Create;
  function __construct() {
    parent::__construct();
    $this->FFreeObjects = true;
  }
  // destructor Destroy;  Override;
  function __destruct() {
    $this->Clear();
    parent::__destruct();
  }
  // procedure Clear;
  function Clear() {
    // LIndex : Integer;
    $LIndex = 0;
    if ($this->FFreeObjects) {
      for ($LIndex = High($this->FItems), $LIndex_High_ = 0; $LIndex >= $LIndex_High_; $LIndex--) {
        FreeAndNil($this->FItems[$LIndex]);
      }
    }
    SetLength($this->FItems, 0);
  }
  // function GetItem(const AIndex: Integer) : TObject;
  function GetItem($AIndex) {
    // Result : TObject;
    $Result = null;
    $Result = $this->FItems[$AIndex];
    return $Result;
  }
  // function IndexOf(const AObject: TObject) : Integer;
  function IndexOf($AObject) {
    // Result : Integer;
    $Result = 0;
    for ($Result = 0, $Result_High_ = High($this->FItems); $Result <= $Result_High_; $Result++) {
      if ($this->FItems[$Result] == $AObject) {
        return($Result);
      }
    }
    $Result =  - 1;
    return $Result;
  }
  // procedure Add(const AObject: TObject);
  function Add($AObject) {
    SetLength($this->FItems, Length($this->FItems) + 1);
    $this->FItems[High($this->FItems)] = $AObject;
  }
  // procedure Delete(AIndex: Integer);
  function Delete($AIndex) {
    Enforce($AIndex >= 0 && $AIndex < Length($this->FItems));
    if ($this->FFreeObjects) {
      FreeAndNil($this->FItems[$AIndex]);
    }
    while ($AIndex < High($this->FItems)) {
      $this->FItems[$AIndex] = $this->FItems[$AIndex + 1];
      $AIndex = $AIndex + 1;
    }
    SetLength($this->FItems, Length($this->FItems) - 1);
  }
  // procedure Remove(const AItem: TObject);
  function Remove($AItem) {
    // LIndex : Integer;
    $LIndex = 0;
    $LIndex = $this->IndexOf($AItem);
    if ($LIndex !=  - 1) {
      $this->Delete($LIndex);
    }
  }
  // procedure SetItem(const AIndex: Integer;const AObject: TObject);
  function SetItem($AIndex, $AObject) {
    $this->FItems[$AIndex] = $AObject;
  }
}
?>