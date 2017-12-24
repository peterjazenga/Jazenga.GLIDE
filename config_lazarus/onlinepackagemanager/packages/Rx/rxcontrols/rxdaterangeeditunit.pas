{ rxDateRangeEditUnit unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
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

unit rxDateRangeEditUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Controls, Buttons, StdCtrls, Spin;

type
  TRxDateRangeEditOption = (reoMonth, reoQuarter, reoHalfYear);
  TRxDateRangeEditOptions = set of TRxDateRangeEditOption;

type

  { TRxCustomDateRangeEdit }

  TRxCustomDateRangeEdit = class(TCustomControl)
  private
    FFlat: Boolean;
    FLockCount:integer;
    FOnEditChange: TNotifyEvent;
    FOnEditClick: TNotifyEvent;
    FOnEditEnter: TNotifyEvent;
    FOnEditExit: TNotifyEvent;
    FOptions: TRxDateRangeEditOptions;
    FsbDecYear: TSpeedButton;
    FsbDecMonth: TSpeedButton;
    FsbIncYear: TSpeedButton;
    FsbIncMonth: TSpeedButton;
    FEditYear: TSpinEdit;
    FEditMonth: TComboBox;
    procedure DoIncMonth(Sender: TObject);
    procedure DoIncYear(Sender: TObject);
    procedure DoDecMonth(Sender: TObject);
    procedure DoDecYear(Sender: TObject);
    function GetHalfYear: word;
    function GetMonth: word;
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
    procedure InternalOnEditChange(Sender: TObject);
    procedure InternalOnEditClick(Sender: TObject);
    procedure InternalOnEditEnter(Sender: TObject);
    procedure InternalOnEditExit(Sender: TObject);
    procedure Lock;
    procedure UnLock;
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure FillMonthNames;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure EditChange; virtual;
    procedure EditClick; virtual;
    procedure EditEnter; virtual;
    procedure EditExit; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Quarter:word read GetQuarter write SetQuarter;
    property HalfYear:word read GetHalfYear write SetHalfYear;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Year:word read GetYear write SetYear;
    property Month:word read GetMonth write SetMonth;
    property Period:TDateTime read GetPeriod write SetPeriod;
    property PeriodEnd:TDateTime read GetPeriodEnd;
    property Options:TRxDateRangeEditOptions read FOptions write SetOptions default [reoMonth];
    property OnChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnEnter: TNotifyEvent read FOnEditEnter write FOnEditEnter;
    property OnExit: TNotifyEvent read FOnEditExit write FOnEditExit;
  end;

type
  TRxDateRangeEdit = class(TRxCustomDateRangeEdit)
  published
    property Align;
    property Anchors;
    property Autosize default True;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property Color;
    property Constraints;
    property Cursor;
    property Enabled;
    property Flat;
    property Hint;
    property Month;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property Year;

    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation
uses rxdateutil, rxconst;

{ TRxCustomDateRangeEdit }

procedure TRxCustomDateRangeEdit.DoIncMonth(Sender: TObject);
var
  i:integer;
begin
  if FEditMonth.ItemIndex>=0 then
  begin
    i:=PtrInt(FEditMonth.Items.Objects[FEditMonth.ItemIndex]);

    if I in [17, 18] then
    begin
      if i = 18 then
      begin
        i:=17;
        Year:=Year + 1;
      end
      else
        i:=18;
    end
    else
    if i in [13..16] then
    begin
      inc(i);
      if i> 16 then
      begin
        i:=13;
        Year:=Year + 1;
      end;
    end
    else
    begin
      inc(i);
      if i > 12 then
      begin
        i:=1;
        Year:=Year + 1;
      end;
    end;
    FEditMonth.ItemIndex := i - 1;
  end
  else
    FEditMonth.ItemIndex := 0;
  InternalOnEditChange(Self);
end;

procedure TRxCustomDateRangeEdit.DoIncYear(Sender: TObject);
begin
  FEditYear.Value:=FEditYear.Value + 1;
end;

procedure TRxCustomDateRangeEdit.DoDecMonth(Sender: TObject);
var
  i:integer;
begin
  if FEditMonth.ItemIndex>=0 then
  begin
    i:=PtrInt(FEditMonth.Items.Objects[FEditMonth.ItemIndex]);

    if I in [17, 18] then
    begin
      if i = 18 then
      begin
        i:=17;
        Year:=Year - 1;
      end
      else
        i:=18;
    end
    else
    if i in [13..16] then
    begin
      Dec(i);
      if i> 13 then
      begin
        i:=16;
        Year:=Year - 1;
      end;
    end
    else
    begin
      Dec(i);
      if i < 1 then
      begin
        i:=12;
        Year:=Year - 1;
      end;
    end;
    FEditMonth.ItemIndex := i - 1;
  end
  else
    FEditMonth.ItemIndex := 0;
  InternalOnEditChange(Self);
end;

procedure TRxCustomDateRangeEdit.DoDecYear(Sender: TObject);
begin
  FEditYear.Value:=FEditYear.Value - 1;
end;

function TRxCustomDateRangeEdit.GetHalfYear: word;
var
  i:integer;
begin
  Result:=0;
  if reoHalfYear in FOptions then
  begin
    i:=PtrInt(FEditMonth.Items.Objects[FEditMonth.ItemIndex]);
    if i in [17..18] then
      Result:=i - 16;
  end
end;

function TRxCustomDateRangeEdit.GetMonth: word;
var
  i:integer;
begin
  Result:=0;
  if (reoMonth in FOptions) or (FOptions = []) then
  begin
    i:=PtrInt(FEditMonth.Items.Objects[FEditMonth.ItemIndex]);
    if i in [1..12] then
      Result:=i;
  end
end;

function TRxCustomDateRangeEdit.GetPeriod: TDateTime;
begin
  Result:=EncodeDate(Year, Month, 1);
end;

function TRxCustomDateRangeEdit.GetPeriodEnd: TDateTime;
begin
  Result:=EncodeDate(Year, Month, DaysPerMonth(Year, Month));
end;

function TRxCustomDateRangeEdit.GetQuarter: word;
var
  i:integer;
begin
  Result:=0;
  if reoQuarter in FOptions then
  begin
    i:=PtrInt(FEditMonth.Items.Objects[FEditMonth.ItemIndex]);
    if i in [13..16] then
      Result:=i - 12;
  end
end;

function TRxCustomDateRangeEdit.GetYear: word;
begin
  Result:=FEditYear.Value;
end;

procedure TRxCustomDateRangeEdit.SetFlat(AValue: Boolean);
begin
  if FFlat=AValue then Exit;
  FFlat:=AValue;
  FsbDecMonth.Flat:=FFlat;
  FsbDecYear.Flat:=FFlat;
  FsbIncMonth.Flat:=FFlat;
  FsbIncYear.Flat:=FFlat;
end;

procedure TRxCustomDateRangeEdit.SetHalfYear(AValue: word);
begin

end;

procedure TRxCustomDateRangeEdit.SetMonth(AValue: word);
begin
  if (AValue>0) and (AValue < 13) then
    FEditMonth.ItemIndex:=AValue-1;
end;

procedure TRxCustomDateRangeEdit.SetOptions(AValue: TRxDateRangeEditOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  FillMonthNames;
end;

procedure TRxCustomDateRangeEdit.SetPeriod(AValue: TDateTime);
var
  Y, M, D: word;
begin
  DecodeDate(AValue, Y, M, D);
  FEditMonth.ItemIndex:=M-1;
  FEditYear.Value:=Y;
end;

procedure TRxCustomDateRangeEdit.SetQuarter(AValue: word);
begin

end;

procedure TRxCustomDateRangeEdit.SetYear(AValue: word);
begin
  FEditYear.Value:=AValue;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditChange(Sender: TObject);
begin
  if FLockCount = 0 then
    EditChange;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditClick(Sender: TObject);
begin
  EditClick;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditEnter(Sender: TObject);
begin
  EditEnter;
end;

procedure TRxCustomDateRangeEdit.InternalOnEditExit(Sender: TObject);
begin
  EditExit;
end;

procedure TRxCustomDateRangeEdit.Lock;
begin
  Inc(FLockCount);
end;

procedure TRxCustomDateRangeEdit.UnLock;
begin
  if FLockCount > 0 then
    Dec(FLockCount)
  else
    InternalOnEditChange(Self);
end;

class function TRxCustomDateRangeEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 80 + 70 + 23 * 4;
  Result.CY := 23;
end;

procedure TRxCustomDateRangeEdit.FillMonthNames;
var
  i, k: Integer;
begin
  FEditMonth.Items.BeginUpdate;
  FEditMonth.Items.Clear;
  if (reoMonth in FOptions) or (FOptions = []) then
  begin
    for i:=1 to 12 do
    begin
      k:=FEditMonth.Items.Add(DefaultFormatSettings.LongMonthNames[i]);
      FEditMonth.Items.Objects[K]:=TObject(Pointer(i));
    end;
  end;

  if (reoQuarter in FOptions) or (FOptions = []) then
  begin
    k:=FEditMonth.Items.Add(sFirstQuarter);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(13));
    k:=FEditMonth.Items.Add(sSecondQuarter);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(14));
    k:=FEditMonth.Items.Add(sThirdQuarter);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(15));
    k:=FEditMonth.Items.Add(sFourthQuarter);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(16));
  end;

  if (reoHalfYear in FOptions) or (FOptions = []) then
  begin
    k:=FEditMonth.Items.Add(sFirstHalfOfYear);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(17));
    k:=FEditMonth.Items.Add(sSecondHalfOfYear);
    FEditMonth.Items.Objects[K]:=TObject(Pointer(18));
  end;
  FEditMonth.ItemIndex:=0;
  FEditMonth.Items.EndUpdate;
end;

procedure TRxCustomDateRangeEdit.SetAutoSize(AValue: Boolean);
begin
  if AutoSize = AValue then
    Exit;
  inherited SetAutosize(AValue);
  FEditMonth.AutoSize := AValue;
  FEditYear.AutoSize := AValue;
end;

procedure TRxCustomDateRangeEdit.EditChange;
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TRxCustomDateRangeEdit.EditClick;
begin
  if Assigned(FOnEditClick) then FOnEditClick(Self);
end;

procedure TRxCustomDateRangeEdit.EditEnter;
begin
  if Assigned(FOnEditEnter) then FOnEditEnter(Self);
end;

procedure TRxCustomDateRangeEdit.EditExit;
begin
  if Assigned(FOnEditExit) then FOnEditExit(Self);
end;

constructor TRxCustomDateRangeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLockCount:=0;
  FOptions:=[reoMonth];

  FEditYear:=TSpinEdit.Create(Self);
  FEditMonth:=TComboBox.Create(Self);
  FEditMonth.Style:=csDropDownList;
  FEditMonth.DropDownCount:=12;
  FEditYear.Width:=70;
  FEditMonth.Width:=100;

  FsbDecYear:=TSpeedButton.Create(Self);
  FsbDecMonth:=TSpeedButton.Create(Self);
  FsbIncYear:=TSpeedButton.Create(Self);
  FsbIncMonth:=TSpeedButton.Create(Self);

  FsbDecYear.OnClick:=@DoDecYear;
  FsbDecMonth.OnClick:=@DoDecMonth;
  FsbIncYear.OnClick:=@DoIncYear;
  FsbIncMonth.OnClick:=@DoIncMonth;


  FEditYear.Parent:=Self;
  FsbDecYear.Parent:=Self;
  FsbDecMonth.Parent:=Self;
  FsbIncYear.Parent:=Self;
  FsbIncMonth.Parent:=Self;
  FEditMonth.Parent:=Self;

  FsbDecYear.Caption:='<<';
  FsbDecMonth.Caption:='<';
  FsbIncYear.Caption:='>>';
  FsbIncMonth.Caption:='>';

  FsbDecYear.Left:=0;
  FsbDecMonth.Left:=23;
  FEditMonth.Left:=46;
  FEditYear.Left:=126;
  FsbIncMonth.Left:=206;
  FsbIncYear.Left:=229;


  ControlStyle := ControlStyle + [csNoFocus];


  FsbDecYear.Align:=alLeft;
  FsbDecMonth.Align:=alLeft;
  FsbIncYear.Align:=alRight;
  FsbIncMonth.Align:=alRight;

  FEditYear.Align:=alRight;
  FEditMonth.Align:=alClient;

  FEditYear.MaxValue:=9999;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  FillMonthNames;

  SetPeriod(Now);
  AutoSize := True;

  FEditMonth.OnChange:=@InternalOnEditChange;
  FEditYear.OnChange:=@InternalOnEditChange;

  FEditMonth.OnClick:=@InternalOnEditClick;
  FEditYear.OnClick:=@InternalOnEditClick;

  FEditMonth.OnEnter:=@InternalOnEditEnter;
  FEditYear.OnEnter:=@InternalOnEditEnter;

  FEditMonth.OnExit:=@InternalOnEditExit;
  FEditYear.OnExit:=@InternalOnEditExit;
end;

destructor TRxCustomDateRangeEdit.Destroy;
begin
  inherited Destroy;
end;

end.

