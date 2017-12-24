{  AutoPanel unit

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

unit RxAutoPanel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LCLType, ExtCtrls;
type

  TPlacement = packed record
    Left, Top, Width, Height: Integer;
  end;
  
  PIntArray = ^TRectArray;
  TRectArray = array[0..4096] of TPlacement;

  TAutoPanel = class(TPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
    pWidth :Integer;
    pHeight:Integer;
    FAutoChildPosLeft : Boolean;
    FAutoChildPosTop : Boolean;
    FAutoChildWidth : Boolean;
    FAutoChildHeight : Boolean;
    PCtrlsCoordArr:PIntArray;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Resize; override;
  published
    { Published declarations }
    property AutoChildPosLeft : Boolean read FAutoChildPosLeft write FAutoChildPosLeft default False;
    property AutoChildPosTop : Boolean read FAutoChildPosTop write FAutoChildPosTop default False;
    property AutoChildWidth : Boolean  read FAutoChildWidth write FAutoChildWidth default False;
    property AutoChildHeight : Boolean read FAutoChildHeight write FAutoChildHeight default False;

    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Caption;
    property Color;
    property Font;
    //property Locked;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;

    property Anchors;
    property AutoSize;
    //property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property FullRepaint;
    //property ParentBiDiMode;

    //property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
  end;

//procedure Register;

implementation

//--------------------------------------
constructor TAutoPanel.Create(AOwner: TComponent);
begin
 inherited;
 FAutoChildPosLeft := False;
 FAutoChildPosTop := False;
 FAutoChildWidth := False;
 FAutoChildHeight := False;
 pWidth := -1;
 pHeight := -1;
 PCtrlsCoordArr := nil;
end;


destructor TAutoPanel.Destroy;
begin
 inherited;
 FreeMem(PCtrlsCoordArr);
end;

procedure TAutoPanel.Loaded;
var i:Integer;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then Exit;

  if (pWidth = -1) and (pHeight = -1) then begin
   GetMem(PCtrlsCoordArr, ControlCount * sizeof(TRect));
   for  i := 0 to ControlCount - 1 do begin
      PCtrlsCoordArr^[i].Left := Controls[i].Left;
      PCtrlsCoordArr^[i].Top := Controls[i].Top;
      PCtrlsCoordArr^[i].Width := Controls[i].Width;
      PCtrlsCoordArr^[i].Height := Controls[i].Height;
   end;
   pWidth := Width;
   pHeight := Height;
  end;
end;

procedure TAutoPanel.Resize;
var I:Integer;
begin
  inherited;
  if (csDesigning in ComponentState) then Exit;
  if not (AutoChildPosLeft or AutoChildWidth or AutoChildPosTop or AutoChildHeight) then Exit;
  try
    for  i := 0 to ControlCount - 1 do
    begin
      if(AutoChildPosLeft = true) then
        if (AutoChildWidth = true) then
        begin
          Controls[i].Left := MulDiv (PCtrlsCoordArr^[i].Left,Width,pWidth);
          Controls[i].Width :=  MulDiv (PCtrlsCoordArr^[i].Width,Width,pWidth);
        end
        else
          Controls[i].Left := Round(
             PCtrlsCoordArr^[i].Left * Width / pWidth  +
             ((PCtrlsCoordArr^[i].Width) * Width / pWidth -
             (PCtrlsCoordArr^[i].Width))/2
            );

      if(AutoChildPosTop = true) then
        if (AutoChildHeight = true) then
        begin
          Controls[i].Top := MulDiv (PCtrlsCoordArr^[i].Top,Height,pHeight);
          Controls[i].Height := MulDiv (PCtrlsCoordArr^[i].Height,Height,pHeight);
        end
        else
          Controls[i].Top := Round(
             PCtrlsCoordArr^[i].Top * Height / pHeight +
             ((PCtrlsCoordArr^[i].Height)  * Height / pHeight -
             (PCtrlsCoordArr^[i].Height))/2
            );
    end;
  finally
  end;
end;
//--------------------------------------

end.
