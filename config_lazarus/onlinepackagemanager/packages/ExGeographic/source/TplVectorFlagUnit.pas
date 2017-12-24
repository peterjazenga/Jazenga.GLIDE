{**********************************************************************
 Package pl_ExGeographic
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit TplVectorFlagUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,Controls, SysUtils,
  flagcustomunit,flagtype;
  
type

TplVectorFlag = class(TCustomFlag)
  protected
    class function GetControlClassDefaultSize: TPoint;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StyleChanged(Sender: TObject);
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDrag;
    property ShowHint;
    property Visible;
  end;

implementation

constructor TplVectorFlag.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ControlStyle := ControlStyle + [csReplicatable];
end;

destructor TplVectorFlag.Destroy;
begin
  inherited Destroy;
end;

procedure TplVectorFlag.Paint;
begin
  inherited Paint;
end;

procedure TplVectorFlag.StyleChanged(Sender: TObject);
begin
  If (Parent <> nil) and (Visible or (csDesigning in ComponentState))
   and Parent.HandleAllocated
  then
    Invalidate;
end;


class function TplVectorFlag.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=99;
  Result.Y:=66;
end;



end.

