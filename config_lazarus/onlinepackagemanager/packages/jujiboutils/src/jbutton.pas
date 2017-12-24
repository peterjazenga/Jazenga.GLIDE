{ TJButton

  Copyright (C) 2012 Julio Jiménez Borreguero
  Contact: jujibo at gmail dot com

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file license-jujiboutils.txt and COPYING.LGPL, included in this distribution,
  for details about the license.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

unit JButton;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  JLabel;

type

  { TJButton }

  TJButton = class(TCustomPanel)
  private
    FImage: TImage;
    FJLabel: TJLabel;
    //OldBevel: TPanelBevel;
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetName(const Value: TComponentName); override;
    procedure JLabel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure JLabel1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure JLabel1MouseClick(Sender: TObject);
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
  published
    { Published declarations }
    property LCaption: TJLabel read FJLabel;
    property Image: TImage read FImage;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    //property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I jbutton_icon.lrs}
  RegisterComponents('Jujibo', [TJButton]);
end;

{ TJButton }

procedure TJButton.SetName(const Value: TComponentName);
var
  ChangeText: boolean;
begin
  inherited SetName(Value);
  if (csDesigning in ComponentState) and (FJLabel.Caption = FJLabel.Name) then
    FJLabel.Caption := Value;
end;

procedure TJButton.JLabel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  //OldBevel:= BevelOuter;
  BevelOuter := bvLowered;
end;

procedure TJButton.JLabel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  BevelOuter := bvRaised; //OldBevel;
end;

procedure TJButton.JLabel1MouseClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    Click;
end;

constructor TJButton.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Width := 65;
  Height := 65;
  ControlStyle := ControlStyle - [csSetCaption];    // No caption
  // Image
  FImage := TImage.Create(Self);
  FImage.Name := 'Image';
  FImage.Parent := Self;
  FImage.SetSubComponent(True);
  FImage.Align := alClient;
  FImage.ControlStyle := FImage.ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  // JLabel
  FJLabel := TJLabel.Create(Self);
  FJLabel.Name := 'JLabel';
  FJLabel.Parent := Self;
  FJLabel.ParentFont := False;
  FJLabel.SetSubComponent(True);
  FJLabel.ControlStyle := FJLabel.ControlStyle + [csNoDesignSelectable] -
    [csDoubleClicks];
  FJLabel.Alignment := taCenter;
  FJLabel.Layout := tlCenter;
  FJLabel.Align := alClient;
  FJLabel.OnMouseDown := @JLabel1MouseDown;
  FJLabel.OnMouseUp := @JLabel1MouseUp;
  FJLabel.OnClick := @JLabel1MouseClick;
end;

end.
