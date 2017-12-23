<?PHP
// 
// Converted From Pascal By Pas2Php v0.8
// Converts Object Pascal to PHP
// http://www.wascal.net/?page=pas2php
// Pas2Php v0.8 is Copyright (C) 2014-2016 Derek John Evans
// 
include_once('system.php');
include_once('classes.php');
include_once('htmlelements.php');
include_once('p2pcgi.php');
include_once('p2pclasses.php');
include_once('strutils.php');
include_once('sysutils.php');
// tnA = 'A';
define("tnA", "A");
// tnBUTTON = 'BUTTON';
define("tnBUTTON", "BUTTON");
// tnCENTER = 'CENTER';
define("tnCENTER", "CENTER");
// tnFORM = 'FORM';
define("tnFORM", "FORM");
// tnH1 = 'H1';
define("tnH1", "H1");
// tnH2 = 'H2';
define("tnH2", "H2");
// tnH3 = 'H3';
define("tnH3", "H3");
// tnIMG = 'IMG';
define("tnIMG", "IMG");
// tnINPUT = 'INPUT';
define("tnINPUT", "INPUT");
// tnP = 'P';
define("tnP", "P");
// tnTABLE = 'TABLE';
define("tnTABLE", "TABLE");
// tnTD = 'TD';
define("tnTD", "TD");
// tnTEXTAREA = 'TEXTAREA';
define("tnTEXTAREA", "TEXTAREA");
// tnTH = 'TH';
define("tnTH", "TH");
// tnTR = 'TR';
define("tnTR", "TR");
// anBorder = 'border';
define("anBorder", "border");
// anCellPadding = 'cellpadding';
define("anCellPadding", "cellpadding");
// anCellSpacing = 'cellspacing';
define("anCellSpacing", "cellspacing");
// anClass = 'class';
define("anClass", "class");
// anChecked = 'checked';
define("anChecked", "checked");
// anDeclare = 'declare';
define("anDeclare", "declare");
// anDefer = 'defer';
define("anDefer", "defer");
// anDisabled = 'disabled';
define("anDisabled", "disabled");
// anMethod = 'method';
define("anMethod", "method");
// anMultiple = 'multiple';
define("anMultiple", "multiple");
// anName = 'name';
define("anName", "name");
// anReadOnly = 'readonly';
define("anReadOnly", "readonly");
// anSelected = 'selected';
define("anSelected", "selected");
// anSrc = 'src';
define("anSrc", "src");
// anStyle = 'style';
define("anStyle", "style");
// anType = 'type';
define("anType", "type");
// anValue = 'value';
define("anValue", "value");
// anWidth = 'width';
define("anWidth", "width");
// itCheckBox = 'checkbox';
define("itCheckBox", "checkbox");
// itHidden = 'hidden';
define("itHidden", "hidden");
// itSubmit = 'submit';
define("itSubmit", "submit");
// itEdit = 'edit';
define("itEdit", "edit");
class CBuzzHtml extends CComponent {
  function __get($name) {
    // TagName : string;
    if (strcasecmp($name, "TagName") === 0) {
      return $this->GetTagName();
    }
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // FAttributes : CStringListEx;
  var $FAttributes = null;
  // function GetTagName : string;  Virtual;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = $this->ClassName();
    return $Result;
  }
/*
constructor Create(const AOwner: CComponent;const AValue: string;
                  const AName: string);
*/
  function __construct($AOwner = null, $AValue = "", $AName = "") {
    $this->FAttributes = new CStringListEx();
    parent::__construct($AOwner, $AValue, $AName);
    $this->SetAttribute(anClass, $this->ClassName());
  }
/*
function GetAttribute(const AName: string;const ADefault: string)
                      : string;
*/
  function GetAttribute($AName, $ADefault = "") {
    // LIndex : Integer;
    $LIndex = 0;
    // Result : string;
    $Result = "";
    $LIndex = $this->FAttributes->IndexOfName($AName);
    if ($LIndex < 0) {
      $Result = $ADefault;
    }
    else {
      $Result = $this->FAttributes->GetValueFromIndex($LIndex);
    }
    return $Result;
  }
  // procedure RemoveAttribute(const AName: string);
  function RemoveAttribute($AName) {
    try {
      $this->FAttributes->Delete($this->FAttributes->IndexOfName($AName));
    }
    catch (Exception $exception) {
    }
  }
  // procedure SetAttribute(const AName: string;const AValue: string);
  function SetAttribute($AName, $AValue) {
    $this->FAttributes->SetValue($AName, $AValue);
  }
  // procedure SetAttributes(const AAttributes: Array of string);
  function SetAttributes($AAttributes) {
    $this->FAttributes->SetValues($AAttributes);
  }
  // procedure SetValueFromCgi(const ADefault: string);
  function SetValueFromCgi($ADefault) {
    $this->Value = CgiGetParamDef($this->Name, $ADefault);
  }
  // function GetInnerHtml : string;  Virtual;
  function GetInnerHtml() {
    // LIndex : Integer;
    $LIndex = 0;
    // Result : string;
    $Result = "";
    $Result = $this->GetTagHtml();
    for ($LIndex = 0, $LIndex_High_ = $this->ComponentCount() - 1; $LIndex <= $LIndex_High_; $LIndex++) {
      $Result = $Result . $this->GetComponents($LIndex)->GetOuterHtml();
    }
    return $Result;
  }
  // function GetOuterHtml : string;  Virtual;
  function GetOuterHtml() {
    // Result : string;
    $Result = "";
    $Result = $this->GetTagOpen() . $this->GetInnerHtml() . $this->GetTagClose();
    return $Result;
  }
  // function GetTagAttributes : string;  Virtual;
  function GetTagAttributes() {
    // LIndex : Integer;
    $LIndex = 0;
    // LPos : Integer;
    $LPos = 0;
    // LString : string;
    $LString = "";
    // LName : string;
    $LName = "";
    // LValue : string;
    $LValue = "";
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    for ($LIndex = 0, $LIndex_High_ = $this->FAttributes->Count - 1; $LIndex <= $LIndex_High_; $LIndex++) {
      $LString = $this->FAttributes->Get($LIndex);
      $LPos = PosEx("=", $LString);
      if ($LPos > 0) {
        $LName = AnsiLeftStr($LString, $LPos - 1);
        $LValue = MidStr($LString, $LPos + 1, MaxInt);
        if (AnsiMatchText($LName, array(anChecked, anSelected, anDisabled, anMultiple, anReadOnly))) {
          if (StrToBoolDef($LValue, false)) {
            $Result = $Result . " " . $LName;
          }
        }
        else {
          $Result = $Result . " " . $LName . "=\"" . EscapeHTML($LValue) . "\"";
        }
      }
    }
    return $Result;
  }
  // function GetTagClose : string;  Virtual;
  function GetTagClose() {
    // Result : string;
    $Result = "";
    $Result = "</" . $this->GetTagName() . ">";
    return $Result;
  }
  // function GetTagHtml : string;  Virtual;
  function GetTagHtml() {
    // Result : string;
    $Result = "";
    $Result = $this->Value;
    return $Result;
  }
  // function GetTagOpen : string;  Virtual;
  function GetTagOpen() {
    // Result : string;
    $Result = "";
    $Result = "<" . $this->GetTagName() . $this->GetTagAttributes() . ">";
    return $Result;
  }
}
class CBuzzItem extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetInnerHtml : string;  Override;
  function GetInnerHtml() {
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    return $Result;
  }
  // function GetTagClose : string;  Override;
  function GetTagClose() {
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    return $Result;
  }
}
class CBuzzHtmlAttrName extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure SetName(const AName: string);  Override;
  function SetName($AName) {
    $this->SetAttribute(anName, $AName);
    parent::SetName($AName);
  }
}
class CBuzzItemAttrName extends CBuzzItem {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure SetName(const AName: string);  Override;
  function SetName($AName) {
    $this->SetAttribute(anName, $AName);
    parent::SetName($AName);
  }
}
class CBuzzItemAttrNameValue extends CBuzzItemAttrName {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure SetValue(const AValue: string);  Override;
  function SetValue($AValue) {
    $this->SetAttribute(anValue, $AValue);
    parent::SetValue($AValue);
  }
}
class CBuzzHtmlRaw extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagOpen : string;  Override;
  function GetTagOpen() {
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    return $Result;
  }
  // function GetTagClose : string;  Override;
  function GetTagClose() {
    // Result : string;
    $Result = "";
    $Result = EmptyStr;
    return $Result;
  }
}
class CBuzzText extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagHtml : string;  Override;
  function GetTagHtml() {
    // Result : string;
    $Result = "";
    $Result = EscapeHTML(parent::GetTagHtml());
    return $Result;
  }
}
class CBuzzTextA extends CBuzzText {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnA;
    return $Result;
  }
}
class CBuzzTextP extends CBuzzText {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnP;
    return $Result;
  }
}
class CBuzzTextH1 extends CBuzzText {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnH1;
    return $Result;
  }
}
class CBuzzTextH2 extends CBuzzText {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnH2;
    return $Result;
  }
}
class CBuzzTextH3 extends CBuzzText {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnH3;
    return $Result;
  }
}
class CBuzzCenter extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnCENTER;
    return $Result;
  }
}
class CBuzzTable extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure DoCreate;  Override;
  function DoCreate() {
    $this->SetAttributes(array(anCellPadding, "0", anCellSpacing, "0", anBorder, "0"));
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnTABLE;
    return $Result;
  }
/*
function Spawn(const AValue: string;const AName: string) : CComponent
              ;  Override;
*/
  function Spawn($AValue = "", $AName = "") {
    // Result : CComponent;
    $Result = null;
    $Result = new CBuzzTableRow($this, $AValue, $AName);
    return $Result;
  }
}
class CBuzzTableRow extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnTR;
    return $Result;
  }
/*
function Spawn(const AValue: string;const AName: string) : CComponent
              ;  Override;
*/
  function Spawn($AValue = "", $AName = "") {
    // Result : CComponent;
    $Result = null;
    $Result = new CBuzzTableCell($this, $AValue, $AName);
    return $Result;
  }
}
class CBuzzTableHead extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnTH;
    return $Result;
  }
}
class CBuzzTableCell extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnTD;
    return $Result;
  }
}
class CBuzzImage extends CBuzzItem {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnIMG;
    return $Result;
  }
}
class CBuzzTextArea extends CBuzzHtmlAttrName {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnTEXTAREA;
    return $Result;
  }
}
class CBuzzInput extends CBuzzItemAttrNameValue {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnINPUT;
    return $Result;
  }
  // procedure SetType(const AType: string);  Virtual;
  function SetType($AType) {
    $this->SetAttribute(anType, $AType);
  }
}
class CBuzzHidden extends CBuzzInput {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure DoCreate;  Override;
  function DoCreate() {
    $this->SetType(itHidden);
  }
}
class CBuzzEdit extends CBuzzInput {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure DoCreate;  Override;
  function DoCreate() {
    $this->SetType(itEdit);
  }
}
class CBuzzCheckBox extends CBuzzInput {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure DoCreate;  Override;
  function DoCreate() {
    $this->SetType(itCheckBox);
  }
  // function GetTagAttributes : string;  Override;
  function GetTagAttributes() {
    // LValue : string;
    $LValue = "";
    // Result : string;
    $Result = "";
    if (StrToBoolDef($this->Value, false)) {
      $this->SetAttribute(anChecked, "1");
    }
    else {
      $this->RemoveAttribute(anChecked);
    }
    $LValue = $this->Value;
    try {
      $this->Value = "1";
      $Result = parent::GetTagAttributes();
      throw new EFinally();
    }
    catch (Exception $exception) {
      $this->Value = $LValue;
      if (!($exception instanceof EFinally)) throw $exception;
    }
    return $Result;
  }
}
class CBuzzSubmit extends CBuzzInput {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // procedure DoCreate;  Override;
  function DoCreate() {
    $this->SetAttribute(anType, itSubmit);
  }
}
class CBuzzButton extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnBUTTON;
    return $Result;
  }
}
class CBuzzForm extends CBuzzHtml {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function GetTagName : string;  Override;
  function GetTagName() {
    // Result : string;
    $Result = "";
    $Result = tnFORM;
    return $Result;
  }
}
class CBuzzValueList extends CBuzzTable {
  function __get($name) {
    return parent::__get($name);
  }
  function __set($name, $value) {
    return parent::__set($name, $value);
  }
  // function AddRow(const ACaption: string) : CBuzzTableCell;
  function AddRow($ACaption) {
    // LRow : CComponent;
    $LRow = null;
    // Result : CBuzzTableCell;
    $Result = null;
    $LRow = $this->Spawn();
    new CBuzzValueListName($LRow, $ACaption);
    $Result = new CBuzzValueListValue($LRow);
    return $Result;
  }
/*
function AddEdit(const ACaption: string;const AValue: string;
                const AName: string) : CBuzzEdit;
*/
  function AddEdit($ACaption, $AValue, $AName) {
    // Result : CBuzzEdit;
    $Result = null;
    $Result = new CBuzzEdit($this->AddRow($ACaption), $AValue, $AName);
    return $Result;
  }
/*
function AddCheckBox(const ACaption: string;const AValue: string;
                    const AName: string) : CBuzzCheckBox;
*/
  function AddCheckBox($ACaption, $AValue, $AName) {
    // Result : CBuzzCheckBox;
    $Result = null;
    $Result = new CBuzzCheckBox($this->AddRow($ACaption), $AValue, $AName);
    return $Result;
  }
/*
function AddSubmit(const ACaption: string;const AValue: string;
                  const AName: string) : CBuzzSubmit;
*/
  function AddSubmit($ACaption, $AValue, $AName) {
    // Result : CBuzzSubmit;
    $Result = null;
    $Result = new CBuzzSubmit($this->AddRow($ACaption), $AValue, $AName);
    return $Result;
  }
}
class CBuzzValueListName extends CBuzzTableCell {
}
class CBuzzValueListValue extends CBuzzTableCell {
}
?>