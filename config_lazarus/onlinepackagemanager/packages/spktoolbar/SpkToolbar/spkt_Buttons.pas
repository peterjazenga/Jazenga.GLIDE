unit spkt_Buttons;

{$mode delphi}
{.$Define EnhancedRecordSupport}

(*******************************************************************************
*                                                                              *
*  Plik: spkt_Buttons.pas                                                      *
*  Opis: Modu³ zawieraj¹cy komponenty przycisków dla toolbara.                 *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*******************************************************************************)

interface

uses
  Graphics, Classes, Controls, Menus, ActnList, Math,
     Dialogs, ImgList, Forms,
     SpkGUITools, SpkGraphTools, SpkMath,
     spkt_Const, spkt_BaseItem, spkt_Exceptions, spkt_Tools;

type TSpkButtonState = (bsIdle,
                        bsBtnHottrack, bsBtnPressed,
                        bsDropdownHottrack, bsDropdownPressed);
     TSpkMouseButtonElement = (beNone, beButton, beDropdown);
     TSpkButtonKind = (bkButton, bkButtonDropdown, bkDropdown);

type TSpkBaseButton = class;

     TSpkButtonActionLink = class(TActionLink)
     private
     protected
       FClient : TSpkBaseButton;

       procedure AssignClient(AClient: TObject); override;
       function IsOnExecuteLinked: Boolean; override;
       procedure SetCaption(const Value: string); override;
       procedure SetEnabled(Value: Boolean); override;
       procedure SetImageIndex(Value: integer); override;
       procedure SetVisible(Value: Boolean); override;
       procedure SetOnExecute(Value: TNotifyEvent); override;
     public
      function IsCaptionLinked: Boolean; override;
      function IsEnabledLinked: Boolean; override;
      function IsImageIndexLinked: Boolean; override;
      function IsVisibleLinked: Boolean; override;
     end;


     { TSpkBaseButton }

     TSpkBaseButton = class abstract(TSpkBaseItem)
     private
       FMouseHoverElement : TSpkMouseButtonElement;
       FMouseActiveElement : TSpkMouseButtonElement;
     protected
       FCaption : string;
       FOnClick : TNotifyEvent;

       FActionLink : TSpkButtonActionLink;

       FButtonState : TSpkButtonState;

       FButtonRect : T2DIntRect;
       FDropdownRect : T2DIntRect;

       FButtonKind : TSpkButtonKind;
       FDropdownMenu : TPopupMenu;

     // *** Obs³uga rysowania ***

     /// <summary>Zadaniem metody w odziedziczonych klasach jest obliczenie
     /// rectów przycisku i menu dropdown w zale¿noœci od FButtonState</summary>
       procedure CalcRects; virtual; abstract;

       function GetDropdownPoint : T2DIntPoint; virtual; abstract;

     // *** Obs³uga akcji ***

       procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); virtual;
       procedure DoActionChange(Sender: TObject);
       procedure Click; virtual;
       function GetDefaultCaption: String; virtual;

     // *** Gettery i settery ***

       procedure SetEnabled(const Value : boolean); override;
       procedure SetDropdownMenu(const Value : TPopupMenu);
       procedure SetRect(const Value: T2DIntRect); override;
       procedure SetCaption(const Value : string);
       procedure SetAction(const Value : TBasicAction); virtual;
       procedure SetButtonKind(const Value : TSpkButtonKind);
       function GetAction: TBasicAction;

       property ButtonKind : TSpkButtonKind read FButtonKind write SetButtonKind;
       property DropdownMenu : TPopupMenu read FDropdownMenu write SetDropdownMenu;

     public
       constructor Create(AOwner : TComponent); override;
       destructor Destroy; override;

       procedure MouseLeave; override;
       procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); override;
       procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
       procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); override;

       function GetRootComponent: TComponent;

     published
       property Caption : string read FCaption write SetCaption;
       property Action : TBasicAction read GetAction write SetAction;
       property OnClick : TNotifyEvent read FOnClick write FOnClick;
     end;


     { TSpkLargeButton }

     TSpkLargeButton = class(TSpkBaseButton)
     private
       FLargeImageIndex: TImageIndex;
       procedure FindBreakPlace(s: string; out Position: integer; out Width: integer);
       procedure SetLargeImageIndex(const Value: TImageIndex);
     protected
       procedure CalcRects; override;
       function GetDropdownPoint : T2DIntPoint; override;
     public
       constructor Create(AOwner: TComponent); override;
       procedure Draw(ABuffer: TBitmap; ClipRect: T2DIntRect); override;
       function GetWidth: integer; override;
       function GetTableBehaviour: TSpkItemTableBehaviour; override;
       function GetGroupBehaviour: TSpkItemGroupBehaviour; override;
       function GetSize: TSpkItemSize; override;
     published
       property LargeImageIndex: TImageIndex read FLargeImageIndex write SetLargeImageIndex default -1;
       property ButtonKind;
       property DropdownMenu;
     end;


     { TSpkSmallButton }

     TSpkSmallButton = class(TSpkBaseButton)
     private
       FImageIndex: TImageIndex;
       FTableBehaviour: TSpkItemTableBehaviour;
       FGroupBehaviour: TSPkItemGroupBehaviour;
       FHideFrameWhenIdle: boolean;
       FShowCaption: boolean;
       procedure ConstructRects(out BtnRect, DropRect: T2DIntRect);
       procedure SetImageIndex(const Value: TImageIndex);
       procedure SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
       procedure SetHideFrameWhenIdle(const Value: boolean);
       procedure SetTableBehaviour(const Value: TSpkItemTableBehaviour);
       procedure SetShowCaption(const Value: boolean);
     protected
       procedure CalcRects; override;
       function GetDropdownPoint: T2DIntPoint; override;
     public
       constructor Create(AOwner: TComponent); override;
       procedure Draw(ABuffer: TBitmap; ClipRect: T2DIntRect); override;
       function GetWidth: integer; override;
       function GetTableBehaviour: TSpkItemTableBehaviour; override;
       function GetGroupBehaviour: TSpkItemGroupBehaviour; override;
       function GetSize: TSpkItemSize; override;
     published
       property ShowCaption: boolean read FShowCaption write SetShowCaption;
       property TableBehaviour: TSpkItemTableBehaviour read FTableBehaviour write SetTableBehaviour;
       property GroupBehaviour: TSpkItemGroupBehaviour read FGroupBehaviour write SetGroupBehaviour;
       property HideFrameWhenIdle: boolean read FHideFrameWhenIdle write SetHideFrameWhenIdle;
       property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
       property ButtonKind;
       property DropdownMenu;
     end;

implementation

uses
  LCLType, LCLIntf, LCLProc, SysUtils, spkt_Pane, spkt_Appearance;

{ TSpkButtonActionLink }

procedure TSpkButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := TSpkBaseButton(AClient);
end;

function TSpkButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
            Assigned(FClient) and
            (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TSpkButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
            Assigned(FClient) and
           (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TSpkButtonActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@TSpkBaseButton(FClient).OnClick = @Action.OnExecute);
end;

function TSpkButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := (inherited IsImageIndexLinked) and
    (
      ((FClient is TSpkSmallButton)
        and (TSpkSmallButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex))
      or
      ((FClient is TSpkLargeButton)
        and (TSpkLargeButton(FClient).LargeImageIndex = (Action as TCustomAction).ImageIndex))
    );
end;

function TSpkButtonActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
            Assigned(FClient) and
            (FClient.Visible = (Action as TCustomAction).Visible);
end;

procedure TSpkButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TSpkButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TSpkButtonActionLink.SetImageIndex(Value: integer);
begin
  if IsImageIndexLinked then begin
    if (FClient is TSpkSmallButton) then
      (TSpkSmallButton(FClient)).ImageIndex := Value;
    if (FClient is TSpkLargeButton) then
      (TSpkLargeButton(FClient)).LargeImageIndex := Value;
  end;
end;

procedure TSpkButtonActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.OnClick := Value;
end;

procedure TSpkButtonActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

{ TSpkBaseButton }

procedure TSpkBaseButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Caption = '') or (Self.Caption = GetDefaultCaption) then
         Self.Caption := Caption;
      if not CheckDefaults or (Self.Enabled = True) then
         Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Visible = True) then
         Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.OnClick) then
         Self.OnClick := OnExecute;
      if self is TSpkSmallButton then begin
        if not CheckDefaults or (TSpkSmallButton(self).ImageIndex < 0) then
          TSpkSmallButton(self).ImageIndex := ImageIndex;
      end;
      if self is TSpkLargeButton then begin
        if not CheckDefaults or (TSpkLargeButton(self).LargeImageIndex < 0) then
          TSpkLargeButton(Self).LargeImageIndex := ImageIndex;
      end;
    end;
end;

constructor TSpkBaseButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FCaption:=GetDefaultCaption;
  FButtonState:=bsIdle;
  FButtonKind:=bkButton;
  {$IFDEF EnhancedRecordSupport}
  FButtonRect:=T2DIntRect.Create(0, 0, 1, 1);
  FDropdownRect:=T2DIntRect.Create(0, 0, 1, 1);
  {$ELSE}
  FButtonRect.Create(0, 0, 1, 1);
  FDropdownRect.Create(0, 0, 1, 1);
  {$ENDIF}
  FMouseHoverElement:=beNone;
  FMouseActiveElement:=beNone;
end;

destructor TSpkBaseButton.Destroy;
begin
  FreeAndNil(FActionLink);
  inherited Destroy;
end;

procedure TSpkBaseButton.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(self)
end;

procedure TSpkBaseButton.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TSpkBaseButton.GetAction: TBasicAction;
begin
if assigned(FActionLink) then
   result:=FActionLink.Action else
   result:=nil;
end;

function TSpkBaseButton.GetDefaultCaption: String;
begin
  result := 'Button';
end;

function TSpkBaseButton.GetRootComponent: TComponent;
var
  pane: TSpkBaseItem;
  tab: TSpkBaseItem;
begin
  result := nil;
  if Collection <> nil then
    pane := TSpkBaseItem(Collection.RootComponent)
  else
    exit;
  if (pane <> nil) and (pane.Collection <> nil) then
    tab := TSpkBaseItem(pane.Collection.RootComponent)
  else
    exit;
  if (tab <> nil) and (tab.Collection <> nil) then
    result := tab.Collection.RootComponent;
end;

procedure TSpkBaseButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
if FEnabled then
   begin
   // Przyciski reaguj¹ tylko na lewy przycisk myszy
   if Button <> mbLeft then
      exit;

   if FMouseActiveElement = beButton then
      begin
      if FButtonState<>bsBtnPressed then
         begin
         FButtonState:=bsBtnPressed;
         if assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
         end;
      end else
   if FMouseActiveElement = beDropdown then
      begin
      if FButtonState<>bsDropdownPressed then
         begin
         FButtonState:=bsDropdownPressed;
         if assigned(FToolbarDispatch) then
            FToolbarDispatch.NotifyVisualsChanged;
         end;
      end else
   if FMouseActiveElement = beNone then
      begin
      if FMouseHoverElement = beButton then
         begin
         FMouseActiveElement:=beButton;

         if FButtonState<>bsBtnPressed then
            begin
            FButtonState:=bsBtnPressed;
            if FToolbarDispatch<>nil then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end else
      if FMouseHoverElement = beDropdown then
         begin
         FMouseActiveElement:=beDropdown;

         if FButtonState<>bsDropdownPressed then
            begin
            FButtonState:=bsDropdownPressed;
            if FToolbarDispatch<>nil then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end;
      end;
   end
else
   begin
   FMouseHoverElement:=beNone;
   FMouseActiveElement:=beNone;
   if FButtonState<>bsIdle then
      begin
      FButtonState:=bsIdle;

      if assigned(FToolbarDispatch) then
         FToolbarDispatch.NotifyVisualsChanged;
      end;
   end;
end;

procedure TSpkBaseButton.MouseLeave;
begin
if FEnabled then
   begin
   if FMouseActiveElement = beNone then
      begin
      if FMouseHoverElement = beButton then
         begin
         // Placeholder, gdyby zasz³a potrzeba obs³ugi tego zdarzenia
         end else
      if FMouseHoverElement = beDropdown then
         begin
         // Placeholder, gdyby zasz³a potrzeba obs³ugi tego zdarzenia
         end;
      end;

   if FButtonState<>bsIdle then
      begin
      FButtonState:=bsIdle;
      if assigned(FToolbarDispatch) then
         FToolbarDispatch.NotifyVisualsChanged;
      end;
   end
else
   begin
   FMouseHoverElement:=beNone;
   FMouseActiveElement:=beNone;
   if FButtonState<>bsIdle then
      begin
      FButtonState:=bsIdle;

      if assigned(FToolbarDispatch) then
         FToolbarDispatch.NotifyVisualsChanged;
      end;
   end;
end;

procedure TSpkBaseButton.MouseMove(Shift: TShiftState; X, Y: Integer);

var NewMouseHoverElement : TSpkMouseButtonElement;

begin
if FEnabled then
   begin
   {$IFDEF EnhancedRecordSupport}
   if FButtonRect.Contains(T2DIntPoint.Create(X,Y)) then
   {$ELSE}
   if FButtonRect.Contains(X,Y) then
   {$ENDIF}
      NewMouseHoverElement:=beButton else
   if (FButtonKind = bkButtonDropdown) and
      {$IFDEF EnhancedRecordSupport}
      (FDropdownRect.Contains(T2DIntPoint.Create(X,Y))) then
      {$ELSE}
      (FDropdownRect.Contains(X,Y)) then
      {$ENDIF}
      NewMouseHoverElement:=beDropdown else
      NewMouseHoverElement:=beNone;

   if FMouseActiveElement = beButton then
      begin
      if (NewMouseHoverElement = beNone) and (FButtonState<>bsIdle) then
         begin
         FButtonState:=bsIdle;
         if FToolbarDispatch<>nil then
            FToolbarDispatch.NotifyVisualsChanged;
         end else
      if (NewMouseHoverElement = beButton) and (FButtonState<>bsBtnPressed) then
         begin
         FButtonState:=bsBtnPressed;
         if FToolbarDispatch<>nil then
            FToolbarDispatch.NotifyVisualsChanged;
         end;
      end else
   if FMouseActiveElement = beDropdown then
      begin
      if (NewMouseHoverElement = beNone) and (FButtonState<>bsIdle) then
         begin
         FButtonState:=bsIdle;
         if FToolbarDispatch<>nil then
            FToolbarDispatch.NotifyVisualsChanged;
         end else
      if (NewMouseHoverElement = beDropdown) and (FButtonState<>bsDropdownPressed) then
         begin
         FButtonState:=bsDropdownPressed;
         if FToolbarDispatch<>nil then
            FToolbarDispatch.NotifyVisualsChanged;
         end;
      end else
   if FMouseActiveElement = beNone then
      begin
      // Z uwagi na uproszczon¹ obs³ugê myszy w przycisku, nie ma potrzeby
      // informowaæ poprzedniego elementu o tym, ¿e mysz opuœci³a jego obszar.

      if NewMouseHoverElement = beButton then
         begin
         if FButtonState<>bsBtnHottrack then
            begin
            FButtonState:=bsBtnHottrack;
            if FToolbarDispatch<>nil then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end else
      if NewMouseHoverElement = beDropdown then
         begin
         if FButtonState<>bsDropdownHottrack then
            begin
            FButtonState:=bsDropdownHottrack;
            if FToolbarDispatch<>nil then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end;
      end;

   FMouseHoverElement:=NewMouseHoverElement;
   end
else
   begin
   FMouseHoverElement:=beNone;
   FMouseActiveElement:=beNone;
   if FButtonState<>bsIdle then
      begin
      FButtonState:=bsIdle;

      if assigned(FToolbarDispatch) then
         FToolbarDispatch.NotifyVisualsChanged;
      end;
   end;
end;

procedure TSpkBaseButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

var ClearActive : boolean;
  DropPoint: T2DIntPoint;

begin
if FEnabled then
   begin
   // Przyciski reaguj¹ tylko na lewy przycisk myszy
   if Button <> mbLeft then
      exit;

   ClearActive:=not(ssLeft in Shift);

   if FMouseActiveElement = beButton then
      begin
      // Zdarzenie zadzia³a tylko wtedy, gdy przycisk myszy zosta³ puszczony nad
      // przyciskiem
      if FMouseHoverElement = beButton then
         begin
         if FButtonKind in [bkButton, bkButtonDropdown] then
            begin
              Click;
              FButtonState:=bsBtnHottrack;
              if assigned(FToolbarDispatch) then
                 FToolbarDispatch.NotifyVisualsChanged;
            end else
         if FButtonKind = bkDropdown then
            begin
            if assigned(FDropdownMenu) then
               begin
               DropPoint:=FToolbarDispatch.ClientToScreen(GetDropdownPoint);
               FDropdownMenu.Popup(DropPoint.x, DropPoint.y);
               FButtonState:=bsBtnHottrack;
               if assigned(FToolbarDispatch) then
                  FToolbarDispatch.NotifyVisualsChanged;
               end;
            end;
         end;
      end else
   if FMouseActiveElement = beDropDown then
      begin
      // Zdarzenie zadzia³a tylko wtedy, gdy przycisk myszy zosta³ puszczony nad
      // przyciskiem DropDown

      if FMouseHoverElement = beDropDown then
         begin
         if assigned(FDropdownMenu) then
            begin
            DropPoint:=FToolbarDispatch.ClientToScreen(GetDropdownPoint);
            FDropdownMenu.Popup(DropPoint.x, DropPoint.y);
            FButtonState:=bsBtnHottrack;
            if assigned(FToolbarDispatch) then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end;
      end;

   if (ClearActive) and (FMouseActiveElement<>FMouseHoverElement) then
      begin
      // Z uwagi na uproszczon¹ obs³ugê, nie ma potrzeby informowaæ poprzedniego
      // elementu o tym, ¿e mysz opuœci³a jego obszar.

      if FMouseHoverElement = beButton then
         begin
         if FButtonState<>bsBtnHottrack then
            begin
            FButtonState:=bsBtnHottrack;
            if assigned(FToolbarDispatch) then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end else
      if FMouseHoverElement = beDropdown then
         begin
         if FButtonState<>bsDropdownHottrack then
            begin
            FButtonState:=bsDropdownHottrack;
            if assigned(FToolbarDispatch) then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end else
      if FMouseHoverElement = beNone then
         begin
         if FButtonState <> bsIdle then
            begin
            FButtonState:=bsIdle;
            if assigned(FToolbarDispatch) then
               FToolbarDispatch.NotifyVisualsChanged;
            end;
         end;
      end;

   if ClearActive then
      begin
      FMouseActiveElement:=beNone;
      end;
   end
else
   begin
   FMouseHoverElement:=beNone;
   FMouseActiveElement:=beNone;
   if FButtonState<>bsIdle then
      begin
      FButtonState:=bsIdle;

      if assigned(FToolbarDispatch) then
         FToolbarDispatch.NotifyVisualsChanged;
      end;
   end;
end;

procedure TSpkBaseButton.SetAction(const Value: TBasicAction);
begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    if FActionLink = nil then
      FActionLink := TSpkButtonActionLink.Create(self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
  end;
end;

procedure TSpkBaseButton.SetButtonKind(const Value: TSpkButtonKind);
begin
  FButtonKind := Value;
  if Assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetDropdownMenu(const Value: TPopupMenu);
begin
  FDropdownMenu := Value;
  if Assigned(FToolbarDispatch) then
     FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkBaseButton.SetEnabled(const Value: boolean);
begin
  inherited;
  if not FEnabled then
  begin
    // Jeœli przycisk zosta³ wy³¹czony, zostaje natychmiast prze³¹czony
    // w stan Idle i zerowane s¹ elementy aktywne i pod mysz¹. Jeœli zosta³
    // w³¹czony, jego stan zmieni siê podczas pierwszej akcji myszy.

    FMouseHoverElement := beNone;
    FMouseActiveElement := beNone;

    if FButtonState <> bsIdle then
    begin
      FButtonState := bsIdle;
      if Assigned(FToolbarDispatch) then
        FToolbarDispatch.NotifyVisualsChanged;
    end;
  end;
end;

procedure TSpkBaseButton.SetRect(const Value: T2DIntRect);
begin
  inherited;
  CalcRects;
end;

{ TSpkLargeButton }

procedure TSpkLargeButton.CalcRects;
begin
 {$IFDEF EnhancedRecordSupport}
  if FButtonKind = bkButtonDropdown then
  begin
    FButtonRect := T2DIntRect.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom - LargeButtonDropdownFieldSize);
    FDropdownRect := T2DIntRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize, FRect.Right, FRect.Bottom);
    //FDropdownRect := T2DIntRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize + 1, FRect.Right, FRect.Bottom);
  end else
  begin
    FButtonRect := FRect;
    FDropdownRect := T2DIntRect.Create(0, 0, 0, 0);
  end;
 {$ELSE}
  if FButtonKind = bkButtonDropdown then
  begin
    FButtonRect.Create(FRect.Left, FRect.Top, FRect.Right, FRect.Bottom - LargeButtonDropdownFieldSize);
    FDropdownRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize, FRect.Right, FRect.Bottom);
//    FDropdownRect.Create(FRect.Left, FRect.Bottom - LargeButtonDropdownFieldSize + 1, FRect.Right, FRect.Bottom);
  end else
  begin
    FButtonRect := FRect;
    FDropdownRect.Create(0, 0, 0, 0);
  end;
 {$ENDIF}
end;

constructor TSpkLargeButton.Create(AOwner: TComponent);
begin
  inherited;
  FLargeImageIndex := -1;
end;

procedure TSpkLargeButton.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);
var
  fontColor, frameColor: TColor;
  gradientFromColor, gradientToColor: TColor;
  innerLightColor, innerDarkColor: TColor;
  gradientKind: TBackgroundKind;
  x: Integer;
  y: Integer;
  delta: Integer;
  cornerRadius: Integer;
  imgList: TImageList;
  txtHeight: Integer;
  breakPos, breakWidth: Integer;
  s: String;
  P: T2DIntPoint;
  drawBtn: Boolean;
begin
  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  if (FRect.Width < 2*LargeButtonRadius) or (FRect.Height < 2*LargeButtonRadius) then
    exit;

  delta := FAppearance.Element.HotTrackBrightnessChange;
  case FAppearance.Element.Style of
    esRounded:
      cornerRadius := LargeButtonRadius;
    esRectangle:
      cornerRadius := 0;
  end;

  // Prepare text color
  fontColor := clNone;
  case FButtonState of
    bsIdle:
      fontColor := FAppearance.Element.IdleCaptionColor;
    bsBtnHottrack,
    bsDropdownHottrack:
      fontColor := FAppearance.Element.HotTrackCaptionColor;
    bsBtnPressed,
    bsDropdownPressed:
      fontColor := FAppearance.ELement.ActiveCaptionColor;
  end;
  if not FEnabled then
    fontColor := TColorTools.ColorToGrayscale(fontColor);

  // Dropdown button
  // Draw full rect, otherwise the DropDownRect will contain the full gradient
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState in [bsBtnHottrack, bsBtnPressed]) then
    begin
      frameColor := TColorTools.Brighten(FAppearance.Element.HotTrackFrameColor, delta);
      innerLightColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerLightColor, delta);
      innerDarkColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerDarkColor, delta);
      gradientFromColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientFromColor, delta);
      gradientToColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientToColor, delta);
      gradientKind := FAppearance.Element.HotTrackGradientType;
    end else
    if (FButtonState = bsDropdownHottrack) then
    begin
      frameColor := FAppearance.Element.HotTrackFrameColor;
      innerLightColor := FAppearance.Element.HotTrackInnerLightColor;
      innerDarkColor := FAppearance.Element.HotTrackInnerDarkColor;
      gradientFromColor := FAppearance.Element.HotTrackGradientFromColor;
      gradientToColor := FAppearance.Element.HotTrackGradientToColor;
      gradientKind := FAppearance.Element.HotTrackGradientType;
    end else
    if (FButtonState = bsDropdownPressed) then
    begin
      frameColor := FAppearance.Element.ActiveFrameColor;
      innerlightColor := FAppearance.Element.ActiveInnerLightColor;
      innerDarkColor := FAppearance.Element.ActiveInnerDarkColor;
      gradientFromColor := FAppearance.Element.ActiveGradientFromColor;
      gradientToColor := FAppearance.Element.ActiveGradientToColor;
      gradientKind := FAppearance.Element.ActiveGradientType;
    end else
      drawBtn := false;

    if drawBtn then begin
      TButtonTools.DrawButton(
        ABuffer,
        FRect,
        frameColor,
        innerLightColor,
        innerDarkColor,
        gradientFromColor,
        gradientToColor,
        gradientKind,
        false,
        false,
        false,
        false,
        cornerRadius,
        ClipRect
      );
    end;
  end;

  // Button (Background and frame)
  drawBtn := true;
  if FButtonState = bsBtnHottrack then
  begin
    frameColor := FAppearance.Element.HotTrackFrameColor;
    innerLightColor := FAppearance.Element.HotTrackInnerLightColor;
    innerDarkColor := FAppearance.Element.HotTrackInnerDarkColor;
    gradientFromColor := FAppearance.Element.HotTrackGradientFromColor;
    gradientToColor := FAppearance.Element.HotTrackGradientToColor;
    gradientKind := FAppearance.Element.HotTrackGradientType;
  end else
  if FButtonState = bsBtnPressed then
  begin
    frameColor := FAppearance.Element.ActiveFrameColor;
    innerDarkColor := FAppearance.Element.ActiveInnerDarkColor;
    innerLightColor := FAppearance.Element.ActiveInnerLightColor;
    gradientFromColor := FAppearance.Element.ActiveGradientFromColor;
    gradientToColor := FAppearance.Element.ActiveGradientToColor;
    gradientKind := FAppearance.Element.ActiveGradientType;
  end else
  if (FButtonState in [bsDropdownHotTrack, bsDropdownPressed]) then
  begin
    frameColor := TColorTools.Brighten(FAppearance.Element.HotTrackFrameColor, delta);
    innerDarkColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerDarkColor, delta);
    innerLightColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerLightColor, delta);
    gradientFromColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientFromColor, delta);
    gradientToColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientToColor, delta);
    gradientKind := FAppearance.Element.HotTrackGradientType;
  end else
    drawBtn := false;

  if drawBtn then
  begin
    TButtonTools.DrawButton(
      ABuffer,
      FButtonRect,       // draw button part only
      frameColor,
      innerLightColor,
      innerDarkColor,
      gradientFromColor,
      gradientToColor,
      gradientKind,
      false,
      false,
      false,
      FButtonKind = bkButtonDropdown,
      cornerRadius,
      ClipRect
    );
  end;

  // Dropdown button - draw horizontal dividing line
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState in [bsDropdownHotTrack, bsBtnHotTrack]) then
      frameColor := FAppearance.element.HotTrackFrameColor
    else
    if (FButtonState in [bsDropDownPressed, bsBtnPressed]) then
      frameColor := FAppearance.Element.ActiveFrameColor
    else
      drawBtn := false;
    if drawBtn then
      TGuiTools.DrawHLine(
        ABuffer,
        FDropDownRect.Left,
        FDropDownRect.Right,
        FDropDownRect.Top,
        frameColor,
        ClipRect
     );
  end;

  // Icon
  if not FEnabled and (FDisabledLargeImages <> nil) then
    imgList := FDisabledLargeImages
  else
    imgList := FLargeImages;

  if (imgList <> nil) and (FLargeImageIndex >= 0) and (FLargeImageIndex < imgList.Count) then
  begin
    P := {$IFDEF EnhancedRecordSupport}T2DIntPoint.Create{$ELSE}Create2DIntPoint{$ENDIF}(
      FButtonRect.Left + (FButtonRect.Width - imgList.Width) div 2,
      FButtonRect.Top + LargeButtonBorderSize + LargeButtonGlyphMargin
    );
    TGUITools.DrawImage(
      ABuffer.Canvas,
      imgList,
      FLargeImageIndex,
      P,
      ClipRect
    );
  end;

  // Text
  ABuffer.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
  ABuffer.Canvas.Font.Color := fontColor;

  if FButtonKind = bkButton then
    FindBreakPlace(FCaption, breakPos, breakWidth)
  else
    breakPos := 0;
  txtHeight := ABuffer.Canvas.TextHeight('Wy');

  if breakPos > 0 then
  begin
    s := Copy(FCaption, 1, breakPos - 1);
    x := FRect.Left + (FRect.Width - ABuffer.Canvas.Textwidth(s)) div 2;
    y := FRect.Top + LargeButtonCaptionTopRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, s, fontColor, ClipRect);

    s := Copy(FCaption, breakPos+1, Length(FCaption) - breakPos);
    x := FRect.Left + (FRect.Width - ABuffer.Canvas.Textwidth(s)) div 2;
    y := FRect.Top + LargeButtonCaptionButtomRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, s, fontColor, ClipRect);
  end else
  begin
    // Tekst nie z³amany
    x := FButtonRect.Left + (FButtonRect.Width - ABuffer.Canvas.Textwidth(FCaption)) div 2;
    y := FRect.Top + LargeButtonCaptionTopRail - txtHeight div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, FCaption, FontColor, ClipRect);
  end;

  // Chevron
  ABuffer.Canvas.Font.Charset := DEFAULT_CHARSET;
  ABuffer.Canvas.Font.Name := 'Marlett';
  ABuffer.Canvas.Font.Style := [];
  ABuffer.Canvas.Font.Orientation := 0;

  if FButtonKind = bkDropdown then
  begin
    x := FButtonRect.Left + (FButtonRect.width - ABuffer.Canvas.Textwidth('u')) div 2;
    y := FButtonRect.bottom - ABuffer.Canvas.Textheight('u') - LargeButtonChevronHMargin;
    TGUITools.DrawText(ABuffer.Canvas, x, y, 'u', FontColor, ClipRect);
  end else
  if FButtonKind = bkButtonDropdown then
  begin
    x := FDropdownRect.Left + (FDropdownRect.width - ABuffer.Canvas.Textwidth('u')) div 2;
    y := FDropdownRect.bottom - ABuffer.Canvas.Textheight('u') - LargeButtonChevronHMargin;
    TGUITools.DrawText(ABuffer.Canvas, x, y, 'u', FontColor, ClipRect);
  end;
end;

procedure TSpkLargeButton.FindBreakPlace(s: string; out Position: integer; out Width: integer);
var
  i: integer;
  Bitmap: TBitmap;
  BeforeWidth, AfterWidth: integer;
begin
  Position := -1;
  Width := -1;

  if FToolbarDispatch=nil then
     exit;
  if FAppearance=nil then
     exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if Bitmap=nil then
    exit;

  Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);

  Width := Bitmap.Canvas.TextWidth(FCaption);

  for i := 1 to Length(s) do
    if s[i] = ' ' then
    begin
      if i > 1 then
        BeforeWidth := Bitmap.Canvas.TextWidth(Copy(s, 1, i-1))
      else
        BeforeWidth := 0;

      if i < Length(s) then
        AfterWidth := Bitmap.Canvas.TextWidth(Copy(s, i+1, Length(s)-i))
      else
        AfterWidth := 0;

      if (Position = -1) or (Max(BeforeWidth, AfterWidth) < Width) then
      begin
        Width := Max(BeforeWidth, AfterWidth);
        Position := i;
      end;
    end;
end;

function TSpkLargeButton.GetDropdownPoint: T2DIntPoint;
begin
  {$IFDEF EnhancedRecordSupport}
  case FButtonKind of
    bkDropdown       : Result := T2DIntPoint.Create(FButtonRect.left, FButtonRect.Bottom+1);
    bkButtonDropdown : Result := T2DIntPoint.Create(FDropdownRect.left, FDropdownRect.Bottom+1);
  else
    Result := T2DIntPoint.Create(0,0);
  end;
  {$ELSE}
  case FButtonKind of
    bkDropdown       : Result.Create(FButtonRect.left, FButtonRect.Bottom+1);
    bkButtonDropdown : Result.Create(FDropdownRect.left, FDropdownRect.Bottom+1);
  else
    Result.Create(0,0);
  end;
  {$ENDIF}
end;

function TSpkLargeButton.GetGroupBehaviour: TSpkItemGroupBehaviour;
begin
  Result := gbSingleItem;
end;

function TSpkLargeButton.GetSize: TSpkItemSize;
begin
  Result := isLarge;
end;

function TSpkLargeButton.GetTableBehaviour: TSpkItemTableBehaviour;
begin
  Result := tbBeginsColumn;
end;

function TSpkLargeButton.GetWidth: integer;
var
  GlyphWidth: integer;
  TextWidth: integer;
  Bitmap: TBitmap;
  BreakPos, RowWidth: integer;
begin
  Result := -1;

  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  Bitmap := FToolbarDispatch.GetTempBitmap;
  if Bitmap = nil then
    exit;

  // Glyph
  if FLargeImages <> nil then
    GlyphWidth := 2 * LargeButtonGlyphMargin + FLargeImages.Width
  else
    GlyphWidth := 0;

  // Text
  if FButtonKind = bkButton then
  begin
    // £amiemy etykietê
    FindBreakPlace(FCaption,BreakPos,RowWidth);
    TextWidth := 2 * LargeButtonCaptionHMargin + RowWidth;
  end else
  begin
    // Nie ³amiemy etykiety
    Bitmap.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
    TextWidth := 2 * LargeButtonCaptionHMargin + Bitmap.Canvas.TextWidth(FCaption);
  end;

  Result := Max(LargeButtonMinWidth, Max(GlyphWidth, TextWidth));
end;

procedure TSpkLargeButton.SetLargeImageIndex(const Value: TImageIndex);
begin
  FLargeImageIndex := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

{ TSpkSmallButton }

procedure TSpkSmallButton.CalcRects;
var
  RectVector: T2DIntVector;
begin
  ConstructRects(FButtonRect, FDropdownRect);
 {$IFDEF EnhancedRecordSupport}
  RectVector := T2DIntVector.Create(FRect.Left, FRect.Top);
 {$ELSE}
  RectVector.Create(FRect.Left, FRect.Top);
 {$ENDIF}
  FButtonRect := FButtonRect + RectVector;
  FDropdownRect := FDropdownRect + RectVector;
end;

procedure TSpkSmallButton.ConstructRects(out BtnRect, DropRect: T2DIntRect);

var BtnWidth : integer;
    DropdownWidth: Integer;
    Bitmap : TBitmap;
    TextWidth: Integer;
  AdditionalPadding: Boolean;

begin
{$IFDEF EnhancedRecordSupport}
BtnRect:=T2DIntRect.Create(0, 0, 0, 0);
DropRect:=T2DIntRect.Create(0, 0, 0, 0);
{$ELSE}
BtnRect.Create(0, 0, 0, 0);
DropRect.Create(0, 0, 0, 0);
{$ENDIF}

if not(assigned(FToolbarDispatch)) then
   exit;
if not(assigned(FAppearance)) then
   exit;

Bitmap:=FToolbarDispatch.GetTempBitmap;
if not(assigned(Bitmap)) then
   exit;

// *** Niezale¿nie od rodzaju, musi byæ miejsce dla ikony i/lub tekstu ***

BtnWidth:=0;
AdditionalPadding:=false;

// Ikona
if FImageIndex<>-1 then
   begin
   BtnWidth:=BtnWidth + SmallButtonPadding + SmallButtonGlyphWidth;
   AdditionalPadding:=true;
   end;

// Tekst
if FShowCaption then
   begin
   Bitmap.Canvas.Font.assign(FAppearance.Element.CaptionFont);
   TextWidth:=Bitmap.Canvas.TextWidth(FCaption);

   BtnWidth:=BtnWidth + SmallButtonPadding + TextWidth;
   AdditionalPadding:=true;
   end;

// Padding za tekstem lub ikon¹
if AdditionalPadding then
   BtnWidth:=BtnWidth + SmallButtonPadding;

// Szerokoœæ zawartoœci przycisku musi wynosiæ co najmniej SMALLBUTTON_MIN_WIDTH
BtnWidth := Max(SmallButtonMinWidth, BtnWidth);

// *** Dropdown ***
case FButtonKind of
     bkButton: begin
               // Lewa krawêdŸ przycisku
               if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
                  BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth else
                  BtnWidth:=BtnWidth + SmallButtonBorderWidth;

               // Prawa krawêdŸ przycisku
               if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
                  BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth else
                  BtnWidth:=BtnWidth + SmallButtonBorderWidth;

               {$IFDEF EnhancedRecordSupport}
               BtnRect:=T2DIntRect.Create(0, 0, BtnWidth - 1, SpkLayoutSizes.PANE_ROW_HEIGHT - 1);
               DropRect:=T2DIntRect.Create(0, 0, 0, 0);
               {$ELSE}
               BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
               DropRect.Create(0, 0, 0, 0);
               {$ENDIF}
               end;
     bkButtonDropdown: begin
                       // Lewa krawêdŸ przycisku
                       if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
                          BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth else
                          BtnWidth:=BtnWidth + SmallButtonBorderWidth;

                       // Prawa krawêdŸ przycisku
                       BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth;

                       // Lewa krawêdŸ i zawartoœæ pola dropdown
                       DropdownWidth := SmallButtonHalfBorderWidth + SmallButtonDropdownWidth;

                       // Prawa krawêdŸ pola dropdown
                       if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
                          DropdownWidth:=DropdownWidth + SmallButtonHalfBorderWidth else
                          DropdownWidth:=DropdownWidth + SmallButtonBorderWidth;

                       {$IFDEF EnhancedRecordSupport}
                       BtnRect:=T2DIntRect.Create(0, 0, BtnWidth - 1, PaneRowHeightT - 1);
                       DropRect:=T2DIntRect.Create(BtnRect.right+1,
                                                   0,
                                                   BtnRect.right+DropdownWidth,
                                                   PaneRowHeight - 1);
                       {$ELSE}
                       BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
                       DropRect.Create(BtnRect.right+1,  0,
                           BtnRect.right+DropdownWidth, PaneRowHeight - 1);
                       {$ENDIF}
                       end;
     bkDropdown: begin
                 // Lewa krawêdŸ przycisku
                 if FGroupBehaviour in [gbContinuesGroup, gbEndsGroup] then
                    BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth else
                    BtnWidth:=BtnWidth + SmallButtonBorderWidth;

                 // Prawa krawêdŸ przycisku
                 if (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) then
                    BtnWidth:=BtnWidth + SmallButtonHalfBorderWidth else
                    BtnWidth:=BtnWidth + SmallButtonBorderWidth;

                 // Dodatkowy obszar na dropdown + miejsce na œrodkow¹ krawêdŸ,
                 // dla kompatybilnoœci wymiarów z dkButtonDropdown
                 BtnWidth:=BtnWidth + SmallButtonBorderWidth + SmallButtonDropdownWidth;

                 {$IFDEF EnhancedRecordSupport}
                 BtnRect:=T2DIntRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
                 DropRect:=T2DIntRect.Create(0, 0, 0, 0);
                 {$ELSE}
                 BtnRect.Create(0, 0, BtnWidth - 1, PaneRowHeight - 1);
                 DropRect.Create(0, 0, 0, 0);
                 {$ENDIF}
                 end;
end;
end;

constructor TSpkSmallButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FTableBehaviour := tbContinuesRow;
  FGroupBehaviour := gbSingleItem;
  FHideFrameWhenIdle := false;
  FShowCaption := true;
end;

procedure TSpkSmallButton.Draw(ABuffer: TBitmap; ClipRect: T2DIntRect);
var
  fontColor: TColor;
  frameColor, innerLightColor, innerDarkColor: TColor;
  gradientFromColor, gradientToColor: TColor;
  gradientKind: TBackgroundKind;
  P: T2DIntPoint;
  x, y: Integer;
  delta: Integer;
  cornerRadius: Integer;
  imgList: TImageList;
  drawBtn: Boolean;
begin
  if (FToolbarDispatch = nil) or (FAppearance = nil) then
    exit;

  if (FRect.Width < 2*SmallButtonRadius) or (FRect.Height < 2*SmallButtonRadius) then
    exit;

  delta := FAppearance.Element.HotTrackBrightnessChange;
  case FAppearance.Element.Style of
    esRounded:
      cornerRadius := SmallButtonRadius;
    esRectangle:
      cornerRadius := 0;
  end;

  // Button (Background and frame)
  drawBtn := true;
  if (FButtonState = bsIdle) and (not FHideFrameWhenIdle) then
  begin
    frameColor := FAppearance.Element.IdleFrameColor;
    innerLightColor := FAppearance.Element.IdleInnerLightColor;
    innerDarkColor := FAppearance.Element.IdleInnerDarkColor;
    gradientFromColor := FAppearance.Element.IdleGradientFromColor;
    gradientToColor := FAppearance.Element.IdleGradientToColor;
    gradientKind := FAppearance.Element.IdleGradientType;
  end else
  if FButtonState = bsBtnHottrack then
  begin
    frameColor := FAppearance.Element.HotTrackFrameColor;
    innerLightColor := FAppearance.Element.HotTrackInnerLightColor;
    innerDarkColor := FAppearance.Element.HotTrackInnerDarkColor;
    gradientFromColor := FAppearance.Element.HotTrackGradientFromColor;
    gradientToColor := FAppearance.Element.HotTrackGradientToColor;
    gradientKind := FAppearance.Element.HotTrackGradientType;
  end else
  if FButtonState = bsBtnPressed then
  begin
    frameColor := FAppearance.Element.ActiveFrameColor;
    innerDarkColor := FAppearance.Element.ActiveInnerDarkColor;
    innerLightColor := FAppearance.Element.ActiveInnerLightColor;
    gradientFromColor := FAppearance.Element.ActiveGradientFromColor;
    gradientToColor := FAppearance.Element.ActiveGradientToColor;
    gradientKind := FAppearance.Element.ActiveGradientType;
  end else
  if (FButtonState in [bsDropdownHotTrack, bsDropdownPressed]) then
  begin
    frameColor := TColorTools.Brighten(FAppearance.Element.HotTrackFrameColor, delta);
    innerDarkColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerDarkColor, delta);
    innerLightColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerLightColor, delta);
    gradientFromColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientFromColor, delta);
    gradientToColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientToColor, delta);
    gradientKind := FAppearance.Element.HotTrackGradientType;
  end else
    drawBtn := false;

  if drawBtn then
  begin
    TButtonTools.DrawButton(
      ABuffer,
      FButtonRect,       // draw button part only
      frameColor,
      innerLightColor,
      innerDarkColor,
      gradientFromColor,
      gradientToColor,
      gradientKind,
      (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]),
      (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]) or (FButtonKind = bkButtonDropdown),
      false,
      false,
      cornerRadius,
      ClipRect
    );
  end;

  // Icon
  if not FEnabled and (FDisabledImages <> nil) then
    imgList := FDisabledImages
  else
    imgList := FImages;

  if (imgList <> nil) and (FImageIndex >= 0) and (FImageIndex < imgList.Count) then
  begin
    if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
      x := FButtonRect.Left + SmallButtonHalfBorderWidth + SmallButtonPadding
    else
      x := FButtonRect.Left + SmallButtonBorderWidth + SmallButtonPadding;
    y := FButtonRect.top + (FButtonRect.height - imgList.Height) div 2;
    P := {$IFDEF EnhancedRecordSupport}T2DIntPoint.Create{$ELSE}Create2DIntPoint{$ENDIF}(x, y);
    TGUITools.DrawImage(
      ABuffer.Canvas,
      imgList,
      FImageIndex,
      P,
      ClipRect
    );
  end;

  // Prepare font and chevron color
  fontColor := clNone;
  case FButtonState of
    bsIdle:
      fontColor := FAppearance.Element.IdleCaptionColor;
    bsBtnHottrack,
    bsDropdownHottrack:
      fontColor := FAppearance.Element.HotTrackCaptionColor;
    bsBtnPressed,
    bsDropdownPressed:
      fontColor := FAppearance.ELement.ActiveCaptionColor;
  end;
  if not FEnabled then
    fontColor := TColorTools.ColorToGrayscale(fontColor);

  // Text
  if FShowCaption then
  begin
    ABuffer.Canvas.Font.Assign(FAppearance.Element.CaptionFont);
    ABuffer.Canvas.Font.Color := fontColor;

    if (FGroupBehaviour in [gbContinuesGroup, gbEndsGroup]) then
      x := FButtonRect.Left + SmallButtonHalfBorderWidth
    else
      x := FButtonRect.Left + SmallButtonBorderWidth;

    if FImageIndex <> -1 then
      x := x + 2 * SmallButtonPadding + SmallButtonGlyphWidth
    else
      x := x + SmallButtonPadding;
    y := FButtonRect.Top + (FButtonRect.Height - ABuffer.Canvas.TextHeight('Wy')) div 2;

    TGUITools.DrawText(ABuffer.Canvas, x, y, FCaption, fontColor, ClipRect);
  end;

  // Dropdown button
  if FButtonKind = bkButtonDropdown then
  begin
    drawBtn := true;
    if (FButtonState = bsIdle) and (not FHideFrameWhenIdle) then
    begin
      frameColor := FAppearance.Element.IdleFrameColor;
      innerLightColor := FAppearance.Element.IdleInnerLightColor;
      innerDarkColor := FAppearance.Element.IdleInnerDarkColor;
      gradientFromColor := FAppearance.Element.IdleGradientFromColor;
      gradientToColor := FAppearance.Element.IdleGradientToColor;
      gradientKind := FAppearance.Element.IdleGradientType;
    end else
    if (FButtonState in [bsBtnHottrack, bsBtnPressed]) then
    begin
      frameColor := TColorTools.Brighten(FAppearance.Element.HotTrackFrameColor, delta);
      innerLightColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerLightColor, delta);
      innerDarkColor := TColorTools.Brighten(FAppearance.Element.HotTrackInnerDarkColor, delta);
      gradientFromColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientFromColor, delta);
      gradientToColor := TColorTools.Brighten(FAppearance.Element.HotTrackGradientToColor, delta);
      gradientKind := FAppearance.Element.HotTrackGradientType;
    end else
    if (FButtonState = bsDropdownHottrack) then
    begin
      frameColor := FAppearance.Element.HotTrackFrameColor;
      innerLightColor := FAppearance.Element.HotTrackInnerLightColor;
      innerDarkColor := FAppearance.Element.HotTrackInnerDarkColor;
      gradientFromColor := FAppearance.Element.HotTrackGradientFromColor;
      gradientToColor := FAppearance.Element.HotTrackGradientToColor;
      gradientKind := FAppearance.Element.HotTrackGradientType;
    end else
    if (FButtonState = bsDropdownPressed) then
    begin
      frameColor := FAppearance.Element.ActiveFrameColor;
      innerlightColor := FAppearance.Element.ActiveInnerLightColor;
      innerDarkColor := FAppearance.Element.ActiveInnerDarkColor;
      gradientFromColor := FAppearance.Element.ActiveGradientFromColor;
      gradientToColor := FAppearance.Element.ActiveGradientToColor;
      gradientKind := FAppearance.Element.ActiveGradientType;
    end else
      drawBtn := false;

    if drawBtn then begin
      TButtonTools.DrawButton(
        ABuffer,
        FDropdownRect,
        frameColor,
        innerLightColor,
        innerDarkColor,
        gradientFromColor,
        gradientToColor,
        gradientKind,
        true,
        (FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup]),
        false,
        false,
        cornerRadius,
        ClipRect
      );
    end;
  end;

  // Chevron
  ABuffer.Canvas.Font.Charset := DEFAULT_CHARSET;
  ABuffer.Canvas.Font.Name := 'Marlett';
  ABuffer.Canvas.Font.Style := [];
  ABuffer.Canvas.Font.Orientation := 0;

  if FButtonKind = bkDropdown then
  begin
    if FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup] then
      x := FButtonRect.Right - SmallButtonHalfBorderWidth - (SmallButtonDropdownWidth + ABuffer.Canvas.Textwidth('u')) div 2 + 1
    else
      x := FButtonRect.Right - SmallButtonBorderWidth - (SmallButtonDropdownWidth + ABuffer.Canvas.Textwidth('u')) div 2 + 1;
    y := FButtonRect.top + (FButtonRect.height - ABuffer.Canvas.Textheight('u')) div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, 'u', fontColor, ClipRect);
  end else
  if FButtonKind = bkButtonDropdown then
  begin
    if FGroupBehaviour in [gbBeginsGroup, gbContinuesGroup] then
      x := FDropdownRect.Right - SmallButtonHalfBorderWidth - (SmallButtonDropdownWidth + ABuffer.Canvas.Textwidth('u')) div 2 + 1
    else
      x := FDropdownRect.Right - SmallButtonBorderWidth - (SmallButtonDropdownWidth + ABuffer.Canvas.Textwidth('u')) div 2 + 1;
    y := FDropdownRect.top + (FDropdownRect.Height - ABuffer.Canvas.Textheight('u')) div 2;
    TGUITools.DrawText(ABuffer.Canvas, x, y, 'u', FontColor, ClipRect);
  end;
end;

function TSpkSmallButton.GetDropdownPoint: T2DIntPoint;
begin
 {$IFDEF EnhancedRecordSupport}
  if FButtonKind in [bkButtonDropdown, bkDropdown] then
    Result := T2DIntPoint.Create(FButtonRect.Left, FButtonRect.Bottom+1)
  else
    Result := T2DIntPoint.Create(0,0);
 {$ELSE}
  if FButtonKind in [bkButtonDropdown, bkDropdown] then
    Result.Create(FButtonRect.Left, FButtonRect.Bottom+1)
  else
    Result.Create(0,0);
 {$ENDIF}
end;

function TSpkSmallButton.GetGroupBehaviour: TSpkItemGroupBehaviour;
begin
  Result := FGroupBehaviour;
end;

function TSpkSmallButton.GetSize: TSpkItemSize;
begin
  Result := isNormal;
end;

function TSpkSmallButton.GetTableBehaviour: TSpkItemTableBehaviour;
begin
  Result := FTableBehaviour;
end;

function TSpkSmallButton.GetWidth: integer;
var
  BtnRect, DropRect: T2DIntRect;
begin
  Result := -1;

  if FToolbarDispatch = nil then
    exit;
  if FAppearance = nil then
    exit;

  ConstructRects(BtnRect, DropRect);

  if FButtonKind = bkButtonDropdown then
    Result := DropRect.Right+1
  else
    Result := BtnRect.Right+1;
end;

procedure TSpkSmallButton.SetGroupBehaviour(const Value: TSpkItemGroupBehaviour);
begin
  FGroupBehaviour := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetHideFrameWhenIdle(const Value: boolean);
begin
  FHideFrameWhenIdle := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyVisualsChanged;
end;

procedure TSpkSmallButton.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex:=Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetShowCaption(const Value: boolean);
begin
  FShowCaption := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;

procedure TSpkSmallButton.SetTableBehaviour(const Value: TSpkItemTableBehaviour);
begin
  FTableBehaviour := Value;
  if Assigned(FToolbarDispatch) then
    FToolbarDispatch.NotifyMetricsChanged;
end;


end.
