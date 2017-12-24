{ RxDBGridPrintGrid unit

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

unit RxDBGridFooterTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, rxdbgrid, Graphics, Grids, Controls;

type

  { TRxDBGridFooterTools }

  TRxDBGridFooterTools = class(TRxDBGridAbstractTools)
  private
    FFooterColor: TColor;
    FFooterRowCount: integer;
    procedure SetFooterColor(AValue: TColor);
    procedure SetFooterRowCount(AValue: integer);
  protected
    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;
    function MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer):boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FooterRowCount:integer read FFooterRowCount write SetFooterRowCount default 1;
    property FooterColor:TColor read FFooterColor write SetFooterColor default clYellow;
  end;

implementation
uses Forms, Dialogs, rxdbgridfootertools_setup, rxdconst;

{ TRxDBGridFooterTools }

procedure TRxDBGridFooterTools.SetFooterRowCount(AValue: integer);
begin
  if FFooterRowCount=AValue then Exit;
  FFooterRowCount:=AValue;
end;

procedure TRxDBGridFooterTools.SetFooterColor(AValue: TColor);
begin
  if FFooterColor=AValue then Exit;
  FFooterColor:=AValue;
end;

function TRxDBGridFooterTools.DoExecTools: boolean;
begin
  Result:=false;
  if (RxDBGrid = nil) or (RxDBGrid.DataSource = nil) or (RxDBGrid.DataSource.Dataset = nil) then
    Exit;

  if RxDBGrid.FooterOptions.Active then
    RxDBGrid.FooterOptions.Active:=false
  else
  begin
    if RxDBGrid.FooterOptions.RowCount = 0 then
      RxDBGrid.FooterOptions.RowCount:=FFooterRowCount;

    if RxDBGrid.FooterOptions.Color = clNone then
      RxDBGrid.FooterOptions.Color:=FFooterColor;
    RxDBGrid.FooterOptions.Active:=true
  end;
end;

function TRxDBGridFooterTools.DoSetupTools: boolean;
begin
  RxDBGridFooterTools_SetupForm:=TRxDBGridFooterTools_SetupForm.Create(Application);
  RxDBGridFooterTools_SetupForm.InitData(RxDBGrid);
  Result:=RxDBGridFooterTools_SetupForm.ShowModal = mrOk;
  if Result then
  begin
    RxDBGridFooterTools_SetupForm.SetData;
    RxDBGrid.CalcStatTotals;
  end;
  RxDBGridFooterTools_SetupForm.Free;
end;

type
  THackRxDBGrid = class(TRxDBGrid);

function TRxDBGridFooterTools.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer): boolean;
begin
  Result:=(Y > THackRxDBGrid(RxDBGrid).GCache.ClientHeight - (RxDBGrid.DefaultRowHeight * RxDBGrid.FooterOptions.RowCount));
  if Result and (ssDouble in Shift) then
    DoSetupTools;
end;

constructor TRxDBGridFooterTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaption:=sRxDBGridToolsCaption;
  FToolsEvents:=[rxteMouseDown];
  ShowSetupForm:=false;
  FFooterColor:=clYellow;
  FFooterRowCount:=1;
end;

destructor TRxDBGridFooterTools.Destroy;
begin
  inherited Destroy;
end;

end.

