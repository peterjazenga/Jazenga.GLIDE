{  lrRxControls unit

  Copyright (C) 2005-2016 Lagunov Aleksey alexs@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit lrRxControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LR_Class, LRDialogControls, Graphics, LResources,
  rxlookup, DB, rxDateRangeEditUnit;

type

  { TlrRxDBLookupComboBox }

  TlrRxDBLookupComboBox = class(TlrVisualControl)
  private
    FKeyField:string;
    FListField:string;
    FListSource:string;
    function GetKeyField: string;
    function GetListField: string;
    function GetListSource: string;
    function GetText: string;
    procedure SetKeyField(AValue: string);
    procedure SetListField(AValue: string);
    procedure SetListSource(AValue: string);
    procedure RxDBLookupComboBox1CloseUp(Sender: TObject; SearchResult:boolean);
    procedure SetText(AValue: string);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    procedure AfterLoad;override;
    procedure AfterCreate;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property KeyField:string read GetKeyField write SetKeyField;
    property ListField:string read GetListField write SetListField;
    property ListSource:string read GetListSource write SetListSource;
    property Color;
    property Enabled;
    property Text:string read GetText write SetText;
    property OnClick;
  end;

  { TlrRxDateEdit }

  TlrRxDateEdit = class(TlrVisualControl)
  private
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Date:TDateTime read GetDate write SetDate;
    property OnClick;
  end;

  { TlrSelectPeriodControl }

  TlrSelectPeriodControl  = class(TlrVisualControl)
  private
    function GetFlat: Boolean;
    function GetHalfYear: word;
    function GetMonth: word;
    function GetOptions: TRxDateRangeEditOptions;
    function GetPeriod: TDateTime;
    function GetPeriodEnd: TDateTime;
    function GetQuarter: word;
    function GetYear: word;
    procedure SetFlat(AValue: Boolean);
    procedure SetHalfYear(AValue: word);
    procedure SetMonth(AValue: word);
    procedure SetOptions(AValue: TRxDateRangeEditOptions);
    procedure SetPeriod(AValue: TDateTime);
    procedure SetQuarter(AValue: word);
    procedure SetYear(AValue: word);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property AutoSize;
    property Quarter:word read GetQuarter write SetQuarter;
    property HalfYear:word read GetHalfYear write SetHalfYear;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Year:word read GetYear write SetYear;
    property Month:word read GetMonth write SetMonth;
    property Period:TDateTime read GetPeriod write SetPeriod;
    property PeriodEnd:TDateTime read GetPeriodEnd;
    property Options:TRxDateRangeEditOptions read GetOptions write SetOptions default [reoMonth];
  end;

implementation
uses DBPropEdits, LR_DBRel, LR_Utils, PropEdits, rxduallist, rxstrutils, rxtooledit,
  LCLIntf, LCLType, Forms, LR_DBComponent;

{$R lrrxdbdialogcontrols_img.res}

procedure DoRegsiterControl(var cmpBMP:TBitmap; lrClass:TlrVisualControlClass);
begin
  if not assigned(cmpBMP) then
  begin
    cmpBMP := TBitmap.Create;
    cmpBMP.LoadFromResourceName(0, lrClass.ClassName);
    frRegisterObject(lrClass, cmpBMP, lrClass.ClassName, nil, otlUIControl, nil);
  end;
end;

var
  lrBMP_LRDBLookupComboBox : TBitmap = nil;
  lrBMP_LRRxDateEdit : TBitmap = nil;
  lrBMP_LRSelectPeriod : TBitmap = nil;

procedure InitLRComp;
begin
  DoRegsiterControl(lrBMP_LRDBLookupComboBox, TlrRxDBLookupComboBox);
  DoRegsiterControl(lrBMP_LRRxDateEdit, TlrRxDateEdit);
  DoRegsiterControl(lrBMP_lrSelectPeriod, TlrSelectPeriodControl);
end;
{
var
  lrBMP_SelectClient:TBitmap = nil;
}
{ TlrRxDateEdit }

function TlrRxDateEdit.GetDate: TDateTime;
begin
  Result:=TRxDateEdit(FControl).Date;
end;

procedure TlrRxDateEdit.SetDate(AValue: TDateTime);
begin
  TRxDateEdit(FControl).Date:=AValue;
end;

procedure TlrRxDateEdit.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrRxDateEdit.CreateControl: TControl;
begin
  Result:=TRxDateEdit.Create(nil);
end;

constructor TlrRxDateEdit.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrRxDateEdit';
  AutoSize:=true;
end;

procedure TlrRxDateEdit.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
end;

procedure TlrRxDateEdit.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
end;

{ TlrRxDBLookupComboBox }

function TlrRxDBLookupComboBox.GetKeyField: string;
begin
  Result:=FKeyField;
end;

function TlrRxDBLookupComboBox.GetListField: string;
begin
  Result:=FListField;
end;

function TlrRxDBLookupComboBox.GetListSource: string;
begin
  Result:=FListSource;
end;

function TlrRxDBLookupComboBox.GetText: string;
begin
  Result:=TRxDBLookupCombo(FControl).Value;
end;

procedure TlrRxDBLookupComboBox.SetKeyField(AValue: string);
begin
  if FKeyField=AValue then Exit;
  FKeyField:=AValue;
  TRxDBLookupCombo(FControl).LookupField:=AValue;
end;

procedure TlrRxDBLookupComboBox.SetListField(AValue: string);
begin
  if FListField=AValue then Exit;
  FListField:=AValue;
  TRxDBLookupCombo(FControl).LookupDisplay:=AValue;
end;

procedure TlrRxDBLookupComboBox.SetListSource(AValue: string);
var
  D:TDataSet;
begin
  if FListSource=AValue then Exit;
  FListSource:=AValue;

  D:=frFindComponent(nil, AValue) as TDataSet;
  if Assigned(D) then
  begin
    TRxDBLookupCombo(FControl).LookupSource:=frGetDataSource(OwnerForm, D);
  end;
end;

procedure TlrRxDBLookupComboBox.RxDBLookupComboBox1CloseUp(Sender: TObject;
  SearchResult: boolean);
begin
{  if Assigned(TDBLookupComboBox(FControl).ListSource) and Assigned(TDBLookupComboBox(FControl).ListSource.DataSet) then
    TDBLookupComboBox(FControl).ListSource.DataSet.Locate(TDBLookupComboBox(FControl).KeyField, TDBLookupComboBox(FControl).KeyValue, []);
}
end;

procedure TlrRxDBLookupComboBox.SetText(AValue: string);
begin
  TRxDBLookupCombo(FControl).Value:=AValue;
end;

procedure TlrRxDBLookupComboBox.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Name) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Name);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrRxDBLookupComboBox.CreateControl: TControl;
begin
  Result:=TRxDBLookupCombo.Create(nil);
  TRxDBLookupCombo(Result).DisplayAllFields:=true;
  TRxDBLookupCombo(Result).OnClosePopup:=@RxDBLookupComboBox1CloseUp;
end;

procedure TlrRxDBLookupComboBox.AfterLoad;
var
  D:TDataSet;
  i:integer;
begin
  inherited AfterLoad;

  D:=frFindComponent(nil, FListSource) as TDataSet;
  if Assigned(D) then
  begin
    if Assigned(D.Owner) then
    begin
      try
        TRxDBLookupCombo(FControl).LookupSource:=frGetDataSource(D.Owner, D);
      finally
      end;
    end
    else
    begin
      for i:=0 to OwnerPage.Objects.Count-1 do
      begin
        if TfrObject(OwnerPage.Objects[i]) is TLRDataSetControl then
        begin
          if TLRDataSetControl(OwnerPage.Objects[i]).DataSet = D then
          begin
           TRxDBLookupCombo(FControl).LookupSource:=TLRDataSetControl(OwnerPage.Objects[i]).lrDataSource;
           break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TlrRxDBLookupComboBox.AfterCreate;
begin
  inherited AfterCreate;
  TRxDBLookupCombo(FControl).OnChange:=FControl.OnClick;
  FControl.OnClick:=nil;
end;

constructor TlrRxDBLookupComboBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName:='lrRxDBLookupComboBox';
end;

procedure TlrRxDBLookupComboBox.LoadFromXML(XML: TLrXMLConfig;
  const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  KeyField:=XML.GetValue(Path+'KeyField/Value'{%H-}, '');
  ListField:=XML.GetValue(Path+'ListField/Value'{%H-}, '');
  FListSource:=XML.GetValue(Path+'ListSource/Value'{%H-}, '');
end;

procedure TlrRxDBLookupComboBox.SaveToXML(XML: TLrXMLConfig; const Path: String
  );
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'KeyField/Value'{%H-}, FKeyField);
  XML.SetValue(Path+'ListField/Value'{%H-}, FListField);
  XML.SetValue(Path+'ListSource/Value'{%H-}, FListSource);
end;

procedure TlrRxDBLookupComboBox.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TlrRxDBLookupComboBox then
  begin
    KeyField:=TlrRxDBLookupComboBox(Source).KeyField;
    ListSource:=TlrRxDBLookupComboBox(Source).ListSource;
    ListField:=TlrRxDBLookupComboBox(Source).ListField;
  end;
end;

{ TlrSelectPeriodControl }

function TlrSelectPeriodControl.GetFlat: Boolean;
begin
  Result:=TRxDateRangeEdit(Control).Flat;
end;

function TlrSelectPeriodControl.GetHalfYear: word;
begin
  Result:=TRxDateRangeEdit(Control).HalfYear;
end;

function TlrSelectPeriodControl.GetMonth: word;
begin
  Result:=TRxDateRangeEdit(Control).Month;
end;

function TlrSelectPeriodControl.GetOptions: TRxDateRangeEditOptions;
begin
  Result:=TRxDateRangeEdit(Control).Options;
end;

function TlrSelectPeriodControl.GetPeriod: TDateTime;
begin
  Result:=TRxDateRangeEdit(Control).Period;
end;

function TlrSelectPeriodControl.GetPeriodEnd: TDateTime;
begin
  Result:=TRxDateRangeEdit(Control).PeriodEnd;
end;

function TlrSelectPeriodControl.GetQuarter: word;
begin
  Result:=TRxDateRangeEdit(Control).Quarter;
end;

function TlrSelectPeriodControl.GetYear: word;
begin
  Result:=TRxDateRangeEdit(Control).Year;
end;

procedure TlrSelectPeriodControl.SetFlat(AValue: Boolean);
begin
  TRxDateRangeEdit(Control).Flat:=AValue;
end;

procedure TlrSelectPeriodControl.SetHalfYear(AValue: word);
begin
  TRxDateRangeEdit(Control).HalfYear:=AValue;
end;

procedure TlrSelectPeriodControl.SetMonth(AValue: word);
begin
  TRxDateRangeEdit(Control).Month:=AValue;
end;

procedure TlrSelectPeriodControl.SetOptions(AValue: TRxDateRangeEditOptions);
begin
  TRxDateRangeEdit(Control).Options:=AValue;
end;

procedure TlrSelectPeriodControl.SetPeriod(AValue: TDateTime);
begin
  TRxDateRangeEdit(Control).Period:=AValue;
end;

procedure TlrSelectPeriodControl.SetQuarter(AValue: word);
begin
  TRxDateRangeEdit(Control).Quarter:=AValue;
end;

procedure TlrSelectPeriodControl.SetYear(AValue: word);
begin
  TRxDateRangeEdit(Control).Year:=AValue;
end;

procedure TlrSelectPeriodControl.PaintDesignControl;
begin
  inherited PaintDesignControl;
end;

function TlrSelectPeriodControl.CreateControl: TControl;
begin
  Result:=TRxDateRangeEdit.Create(nil);
  TRxDateRangeEdit(Result).AutoSize:=false;
end;

constructor TlrSelectPeriodControl.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName:='lrSelectPeriod';
  AutoSize:=false;
end;

procedure TlrSelectPeriodControl.LoadFromXML(XML: TLrXMLConfig;
  const Path: String);
var
  S: String;
begin
  inherited LoadFromXML(XML, Path);
  Flat:=XML.GetValue(Path+'Flat/Value'{%H-}, false);
  S:=XML.GetValue(Path+'Options/Value','');
  if S<>'' then
    RestoreProperty('Options',S)
  else
    Options:=[];
end;

procedure TlrSelectPeriodControl.SaveToXML(XML: TLrXMLConfig; const Path: String
  );
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Flat/Value'{%H-}, Flat);
  XML.SetValue(Path+'Options/Value', GetSaveProperty('Options'));
end;

type
  { TlrDBLookupComboBoxListSourceProperty }

  TlrDBLookupComboBoxListSourceProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TlrDBLookupComboBoxFiledsProperty }

  TlrDBLookupComboBoxFiledsProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TlrLookupDisplayProperty }

  TlrLookupDisplayProperty = class(TlrDBLookupComboBoxFiledsProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{ TlrLookupDisplayProperty }

function TlrLookupDisplayProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=inherited GetAttributes + [paDialog]
end;

procedure TlrLookupDisplayProperty.Edit;
var
  DualListDialog1: TDualListDialog;
  Cmp1:TlrRxDBLookupComboBox;

procedure DoInitFill;
var
  i,j:integer;
  LookupDisplay:string;
begin
  LookupDisplay:=Cmp1.ListField;

  if LookupDisplay<>'' then
  begin
    StrToStrings(LookupDisplay, DualListDialog1.List2, ';');
    for i:=DualListDialog1.List1.Count-1 downto 0 do
    begin
      j:=DualListDialog1.List2.IndexOf(DualListDialog1.List1[i]);
      if j>=0 then
        DualListDialog1.List1.Delete(i);
    end;
  end;
end;

function DoFillDone:string;
var
  i:integer;
begin
  for i:=0 to DualListDialog1.List2.Count-1 do
    Result:=Result + DualListDialog1.List2[i]+';';
  if Result<>'' then
    Result:=Copy(Result, 1, Length(Result)-1);
end;

procedure DoSetCaptions;
begin
  DualListDialog1.Label1Caption:='All fields';
  DualListDialog1.Label2Caption:='Fields is LookupDisplay';
  DualListDialog1.Title:='Fill fields in LookupDisplay property';
end;

begin
  Cmp1:=nil;

  if GetComponent(0) is TlrRxDBLookupComboBox then
    Cmp1:=TlrRxDBLookupComboBox(GetComponent(0));

  DualListDialog1:=TDualListDialog.Create(Application);
  try
    DoSetCaptions;
    FillValues(DualListDialog1.List1 as TStringList);
    DoInitFill;
    if DualListDialog1.Execute then
    begin
      if Assigned(Cmp1) then
        Cmp1.ListField:=DoFillDone
    end;
  finally
    FreeAndNil(DualListDialog1);
  end;
end;

procedure TlrDBLookupComboBoxFiledsProperty.FillValues(const Values: TStringList
  );
var
  L:TlrRxDBLookupComboBox;
begin
  if (GetComponent(0) is TlrRxDBLookupComboBox) then
  begin
    L:=GetComponent(0) as TlrRxDBLookupComboBox;
    if Assigned(TRxDBLookupCombo(L.Control).LookupSource) then
      frGetFieldNames(TfrTDataSet(TRxDBLookupCombo(L.Control).LookupSource.DataSet) , Values);
  end;
end;

procedure TlrDBLookupComboBoxListSourceProperty.FillValues(
  const Values: TStringList);
begin
  if (GetComponent(0) is TlrRxDBLookupComboBox) then
    frGetComponents(nil, TDataSet, Values, nil);
end;

initialization
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TlrRxDBLookupComboBox, 'ListSource', TlrDBLookupComboBoxListSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TlrRxDBLookupComboBox, 'KeyField', TlrDBLookupComboBoxFiledsProperty);
  RegisterPropertyEditor(TypeInfo(string), TlrRxDBLookupComboBox, 'ListField', TlrLookupDisplayProperty);

finalization
  if Assigned(lrBMP_LRDBLookupComboBox) then
    FreeAndNil(lrBMP_LRDBLookupComboBox);

  if Assigned(lrBMP_LRRxDateEdit) then
    FreeAndNil(lrBMP_LRRxDateEdit);

  if Assigned(lrBMP_LRSelectPeriod) then
    FreeAndNil(lrBMP_LRSelectPeriod);
end.

