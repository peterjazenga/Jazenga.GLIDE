{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit P2PBuzz;

interface

uses
  Classes, HtmlElements, P2PCgi, P2PClasses, StrUtils, SysUtils;

const

  tnA = 'A';
  tnBUTTON = 'BUTTON';
  tnCENTER = 'CENTER';
  tnFORM = 'FORM';
  tnH1 = 'H1';
  tnH2 = 'H2';
  tnH3 = 'H3';
  tnIMG = 'IMG';
  tnINPUT = 'INPUT';
  tnP = 'P';
  tnTABLE = 'TABLE';
  tnTD = 'TD';
  tnTEXTAREA = 'TEXTAREA';
  tnTH = 'TH';
  tnTR = 'TR';

  anBorder = 'border';
  anCellPadding = 'cellpadding';
  anCellSpacing = 'cellspacing';
  anClass = 'class';
  anChecked = 'checked';
  anDeclare = 'declare';
  anDefer = 'defer';
  anDisabled = 'disabled';
  anMethod = 'method';
  anMultiple = 'multiple';
  anName = 'name';
  anReadOnly = 'readonly';
  anSelected = 'selected';
  anSrc = 'src';
  anStyle = 'style';
  anType = 'type';
  anValue = 'value';
  anWidth = 'width';

  itCheckBox = 'checkbox';
  itHidden = 'hidden';
  itSubmit = 'submit';
  itEdit = 'edit';

type

  CBuzzHtml = class(CComponent)
  strict private
    FAttributes: CStringListEx;
  protected
    function GetTagName: string; virtual;
  public
    constructor Create(const AOwner: CComponent = nil; const AValue: string = ''; const AName: string = '');
  public
    function GetAttribute(const AName: string; const ADefault: string = ''): string;
    procedure RemoveAttribute(const AName: string);
    procedure SetAttribute(const AName, AValue: string);
    procedure SetAttributes(const AAttributes: array of string);
    procedure SetValueFromCgi(const ADefault: string);
  public
    function GetInnerHtml: string; virtual;
    function GetOuterHtml: string; virtual;
    function GetTagAttributes: string; virtual;
    function GetTagClose: string; virtual;
    function GetTagHtml: string; virtual;
    function GetTagOpen: string; virtual;
  published
    property TagName: string read GetTagName;
  end;

  CBuzzItem = class(CBuzzHtml)
    function GetInnerHtml: string; override;
    function GetTagClose: string; override;
  end;

  CBuzzHtmlAttrName = class(CBuzzHtml)
    procedure SetName(const AName: string); override;
  end;

  CBuzzItemAttrName = class(CBuzzItem)
    procedure SetName(const AName: string); override;
  end;

  CBuzzItemAttrNameValue = class(CBuzzItemAttrName)
    procedure SetValue(const AValue: string); override;
  end;

  CBuzzHtmlRaw = class(CBuzzHtml)
    function GetTagOpen: string; override;
    function GetTagClose: string; override;
  end;

  CBuzzText = class(CBuzzHtml)
    function GetTagHtml: string; override;
  end;

  CBuzzTextA = class(CBuzzText)
    function GetTagName: string; override;
  end;

  CBuzzTextP = class(CBuzzText)
    function GetTagName: string; override;
  end;

  CBuzzTextH1 = class(CBuzzText)
    function GetTagName: string; override;
  end;

  CBuzzTextH2 = class(CBuzzText)
    function GetTagName: string; override;
  end;

  CBuzzTextH3 = class(CBuzzText)
    function GetTagName: string; override;
  end;

  CBuzzCenter = class(CBuzzHtml)
    function GetTagName: string; override;
  end;

  CBuzzTable = class(CBuzzHtml)
    procedure DoCreate; override;
    function GetTagName: string; override;
    function Spawn(const AValue: string = ''; const AName: string = ''): CComponent; override;
  end;

  CBuzzTableRow = class(CBuzzHtml)
    function GetTagName: string; override;
    function Spawn(const AValue: string = ''; const AName: string = ''): CComponent; override;
  end;

  CBuzzTableHead = class(CBuzzHtml)
    function GetTagName: string; override;
  end;

  CBuzzTableCell = class(CBuzzHtml)
    function GetTagName: string; override;
  end;

  CBuzzImage = class(CBuzzItem)
    function GetTagName: string; override;
  end;

  CBuzzTextArea = class(CBuzzHtmlAttrName)
    function GetTagName: string; override;
  end;

  CBuzzInput = class(CBuzzItemAttrNameValue)
    function GetTagName: string; override;
    procedure SetType(const AType: string); virtual;
  end;

  CBuzzHidden = class(CBuzzInput)
    procedure DoCreate; override;
  end;

  CBuzzEdit = class(CBuzzInput)
    procedure DoCreate; override;
  end;

  CBuzzCheckBox = class(CBuzzInput)
    procedure DoCreate; override;
    function GetTagAttributes: string; override;
  end;

  CBuzzSubmit = class(CBuzzInput)
    procedure DoCreate; override;
  end;

  CBuzzButton = class(CBuzzHtml)
    function GetTagName: string; override;
  end;

  CBuzzForm = class(CBuzzHtml)
    function GetTagName: string; override;
  end;

  CBuzzValueList = class(CBuzzTable)
  protected
    function AddRow(const ACaption: string): CBuzzTableCell;
  public
    function AddEdit(const ACaption: string; const AValue, AName: string): CBuzzEdit;
    function AddCheckBox(const ACaption: string; const AValue, AName: string): CBuzzCheckBox;
    function AddSubmit(const ACaption: string; const AValue, AName: string): CBuzzSubmit;
  end;

  CBuzzValueListName = class(CBuzzTableCell)
  end;

  CBuzzValueListValue = class(CBuzzTableCell)
  end;

implementation

constructor CBuzzHtml.Create(const AOwner: CComponent; const AValue, AName: string);
begin
  FAttributes := CStringListEx.Create;
  inherited Create(AOwner, AValue, AName);
  SetAttribute(anClass, ClassName);
end;

procedure CBuzzHtml.SetValueFromCgi(const ADefault: string);
begin
  Value := CgiGetParamDef(Name, ADefault);
end;

function CBuzzHtml.GetTagName: string;
begin
  Result := ClassName;
end;

procedure CBuzzHtml.SetAttribute(const AName, AValue: string);
begin
  FAttributes.SetValue(AName, AValue);
end;

procedure CBuzzHtml.SetAttributes(const AAttributes: array of string);
begin
  FAttributes.SetValues(AAttributes);
end;

procedure CBuzzHtml.RemoveAttribute(const AName: string);
begin
  try
    FAttributes.Delete(FAttributes.IndexOfName(AName));
  except
  end;
end;

function CBuzzHtml.GetAttribute(const AName, ADefault: string): string;
var
  LIndex: integer;
begin
  LIndex := FAttributes.IndexOfName(AName);
  if LIndex < 0 then begin
    Result := ADefault;
  end else begin
    Result := FAttributes.GetValueFromIndex(LIndex);
  end;
end;

function CBuzzHtml.GetTagAttributes: string;
var
  LIndex, LPos: integer;
  LString, LName, LValue: string;
begin
  Result := EmptyStr;
  for LIndex := 0 to FAttributes.Count - 1 do begin
    LString := FAttributes.Get(LIndex);
    LPos := Pos('=', LString);
    if LPos > 0 then begin
      LName := AnsiLeftStr(LString, LPos - 1);
      LValue := Copy(LString, LPos + 1, MaxInt);
      if AnsiMatchText(LName, [anChecked, anSelected, anDisabled, anMultiple, anReadonly]) then begin
        if StrToBoolDef(LValue, False) then begin
          Result := Result + ' ' + LName;
        end;
      end else begin
        Result := Result + ' ' + LName + '="' + EscapeHTML(LValue) + '"';
      end;
    end;
  end;
end;

function CBuzzHtml.GetTagHtml: string;
begin
  Result := Value;
end;

function CBuzzHtml.GetTagOpen: string;
begin
  Result := '<' + GetTagName + GetTagAttributes + '>';
end;

function CBuzzHtml.GetTagClose: string;
begin
  Result := '</' + GetTagName + '>';
end;

function CBuzzHtml.GetInnerHtml: string;
var
  LIndex: integer;
begin
  Result := GetTagHtml;
  for LIndex := 0 to ComponentCount - 1 do begin
    Result := Result + (GetComponents(LIndex) as CBuzzHtml).GetOuterHtml;
  end;
end;

function CBuzzHtml.GetOuterHtml: string;
begin
  Result := GetTagOpen + GetInnerHtml + GetTagClose;
end;

function CBuzzItem.GetInnerHtml: string;
begin
  Result := EmptyStr;
end;

function CBuzzItem.GetTagClose: string;
begin
  Result := EmptyStr;
end;

procedure CBuzzHtmlAttrName.SetName(const AName: string);
begin
  SetAttribute(anName, AName);
  inherited SetName(AName);
end;

procedure CBuzzItemAttrName.SetName(const AName: string);
begin
  SetAttribute(anName, AName);
  inherited SetName(AName);
end;

procedure CBuzzItemAttrNameValue.SetValue(const AValue: string);
begin
  SetAttribute(anValue, AValue);
  inherited SetValue(AValue);
end;

function CBuzzHtmlRaw.GetTagOpen: string;
begin
  Result := EmptyStr;
end;

function CBuzzHtmlRaw.GetTagClose: string;
begin
  Result := EmptyStr;
end;

function CBuzzText.GetTagHtml: string;
begin
  Result := EscapeHTML(inherited GetTagHtml);
end;

function CBuzzTextA.GetTagName: string;
begin
  Result := tnA;
end;

function CBuzzTextP.GetTagName: string;
begin
  Result := tnP;
end;

function CBuzzTextH1.GetTagName: string;
begin
  Result := tnH1;
end;

function CBuzzTextH2.GetTagName: string;
begin
  Result := tnH2;
end;

function CBuzzTextH3.GetTagName: string;
begin
  Result := tnH3;
end;

function CBuzzCenter.GetTagName: string;
begin
  Result := tnCENTER;
end;

function CBuzzTable.GetTagName: string;
begin
  Result := tnTABLE;
end;

procedure CBuzzTable.DoCreate;
begin
  SetAttributes([anCellPadding, '0', anCellSpacing, '0', anBorder, '0']);
end;

function CBuzzTable.Spawn(const AValue, AName: string): CComponent;
begin
  Result := CBuzzTableRow.Create(Self, AValue, AName);
end;

function CBuzzTableRow.GetTagName: string;
begin
  Result := tnTR;
end;

function CBuzzTableRow.Spawn(const AValue, AName: string): CComponent;
begin
  Result := CBuzzTableCell.Create(Self, AValue, AName);
end;

function CBuzzTableHead.GetTagName: string;
begin
  Result := tnTH;
end;

function CBuzzTableCell.GetTagName: string;
begin
  Result := tnTD;
end;

function CBuzzImage.GetTagName: string;
begin
  Result := tnIMG;
end;

function CBuzzTextArea.GetTagName: string;
begin
  Result := tnTEXTAREA;
end;

function CBuzzInput.GetTagName: string;
begin
  Result := tnINPUT;
end;

procedure CBuzzInput.SetType(const AType: string);
begin
  SetAttribute(anType, AType);
end;

procedure CBuzzHidden.DoCreate;
begin
  SetType(itHidden);
end;

procedure CBuzzEdit.DoCreate;
begin
  SetType(itEdit);
end;

procedure CBuzzCheckBox.DoCreate;
begin
  SetType(itCheckBox);
end;

function CBuzzCheckBox.GetTagAttributes: string;
var
  LValue: string;
begin
  if StrToBoolDef(Value, False) then begin
    SetAttribute(anChecked, '1');
  end else begin
    RemoveAttribute(anChecked);
  end;
  LValue := Value;
  try
    Value := '1';
    Result := inherited GetTagAttributes;
  finally
    Value := LValue;
  end;
end;

procedure CBuzzSubmit.DoCreate;
begin
  SetAttribute(anType, itSubmit);
end;

function CBuzzButton.GetTagName: string;
begin
  Result := tnBUTTON;
end;

function CBuzzForm.GetTagName: string;
begin
  Result := tnFORM;
end;

function CBuzzValueList.AddRow(const ACaption: string): CBuzzTableCell;
var
  LRow: CComponent;
begin
  LRow := Spawn;
  CBuzzValueListName.Create(LRow, ACaption);
  Result := CBuzzValueListValue.Create(LRow);
end;

function CBuzzValueList.AddEdit(const ACaption: string; const AValue, AName: string): CBuzzEdit;
begin
  Result := CBuzzEdit.Create(AddRow(ACaption), AValue, AName);
end;

function CBuzzValueList.AddCheckBox(const ACaption: string; const AValue, AName: string): CBuzzCheckBox;
begin
  Result := CBuzzCheckBox.Create(AddRow(ACaption), AValue, AName);
end;

function CBuzzValueList.AddSubmit(const ACaption: string; const AValue, AName: string): CBuzzSubmit;
begin
  Result := CBuzzSubmit.Create(AddRow(ACaption), AValue, AName);
end;

end.
