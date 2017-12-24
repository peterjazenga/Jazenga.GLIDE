{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplLabelUnit;

interface
uses
  LCLIntf, LCLType, LMessages, Messages,
  Classes, Graphics, Controls, Forms, StdCtrls,
  plUtils;

 type
 THTTPorMAIL = (hmHTTP, hmMAIL);

 TplLabel = class(TCustomLabel)
  private
    FTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure Paint; override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
  //  property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property Color Default clNone;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
   // property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

 TplURLLabel = class(TplLabel)
  private
    FHTTPorMail: THTTPorMail;
    FColStd : TColor;
    FColPre : TColor;
    procedure SetHTTPorMail(const value : THTTPorMail);
    procedure SetColStd(const Value: TColor);
    procedure SetColPre(const Value: TColor);
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
  public
    constructor Create( AOwner : TComponent); override;
    procedure OpenObject(sObjectPath : PChar);
  published
    property HTTPorMail : THTTPorMAIL read FHTTPorMail write SetHTTPorMail default hmHTTP;
    property URLColStd: TColor read FColStd write SetColStd default clBlue;
    property URLColPre: TColor read FColPre write SetColPre default clRed;
    property Caption;
    property WordWrap;
    property Align;
    property Font;
    property AutoSize;
    property Alignment;
    property ParentFont;
    property Transparent;
    property Hint;
    property ShowHint;
  end;

implementation
uses SysUtils;

//=========================== TplLabel =================================

procedure TplLabel.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Invalidate;
end;

procedure TplLabel.Paint;
begin
  if FTransparent then Color := clNone;
  inherited paint;
end;

//=========================== TplURLLabel ================================
const
   HTTP = 'http:\\';
   MAIL = 'mailto:';

constructor TplURLLabel.Create( AOwner : TComponent);
begin
   inherited Create(AOwner);

   cursor := crHandPoint;
   FColStd := clBlue;
   FColPre := clRed;
   font.color := FColStd;
   font.Style := [fsUnderline];
end;

procedure TplURLLabel.SetHTTPorMail(const value : THTTPorMail);
begin
   if FHTTPorMail <> Value then
   begin
      FHTTPorMail := Value;
      Invalidate;
   end;
end;

procedure TplURLLabel.SetColStd(const Value: TColor);
begin
   if FColStd <> Value then
   begin
     FColStd := Value;
     font.color := FColStd;
     Invalidate;
   end;
end;

procedure TplURLLabel.SetColPre(const Value: TColor);
begin
   if FColPre <> Value then
   begin
     FColPre := Value;
     Invalidate;
   end;
end;

procedure TplURLLabel.OpenObject(sObjectPath : PChar);
begin
    OpenDocument(sObjectPath); { *Converted from ShellExecute* }
end;

procedure TplURLLabel.Click;
var
  TempString : array[0..79] of char;
  szApp : string;
begin
   Inherited Click;
   if FHTTPorMail = hmHTTP then
      szApp := HTTP else szApp := MAIL;
      
   StrPCopy(TempString,szApp+Caption);
   OpenObject(TempString);
end;

procedure TplURLLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
              X, Y: Integer);
begin
   inherited MouseDown(Button,Shift,X,Y);
   if Button = mbLeft then font.color := URLColPre;
end;

procedure TplURLLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
              X, Y: Integer);
begin
   inherited MouseUp(Button,Shift,X,Y);
   font.color := URLColStd;
end;

end.
