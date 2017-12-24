unit SpkToolbar;

{$mode delphi}

{.$DEFINE EnhancedRecordSupport}
{.$DEFINE DELAYRUNTIMER}

 //Translation from Polish into English by Raf20076, Poland, 2016
//I do my best but if you find any mistakes in English comments
//please correct it.

(*******************************************************************************
*                                                                              *
*  File: SpkToolbar.pas                                                        *
*  Description: Main toolbar component                                         *
*  Copyright: (c) 2009 by Spook.                                               *
*  License:   Modified LGPL (with linking exception, like Lazarus LCL)         *
'             See "license.txt" in this installation                           *
*                                                                              *
*                                                                              *
*******************************************************************************)

interface

uses
  LCLType, LMessages, Graphics, SysUtils, Controls, Classes, Math, Dialogs,
  Forms, Types, SpkGraphTools, SpkGUITools, SpkMath, ExtCtrls,
  spkt_Appearance, spkt_BaseItem, spkt_Const, spkt_Dispatch, spkt_Tab,
  spkt_Pane, spkt_Types;

type
  { Type describes regions of the toolbar which are used during handling
    of interaction with the mouse }
  TSpkMouseToolbarElement = (teNone, teToolbarArea, teTabs, teTabContents);

  TSpkTabChangingEvent = procedure(Sender: TObject; OldIndex, NewIndex: integer;
    var Allowed: boolean) of object;

type
  TSpkToolbar = class;

  { Dispatcher class which is used for safe accepting of information
    and requests from sub-elements. }

  TSpkToolbarDispatch = class(TSpkBaseToolbarDispatch)
  private
    { Toolbar component which is accepting information and
      requests from sub-elements }
    FToolbar: TSpkToolbar;
  protected

  public
    // *******************
    // *** Constructor ***
    // *******************

    //Constructor
    constructor Create(AToolbar: TSpkToolbar);

    // ******************************************************************
    // *** Implementation of abstract methods TSpkBaseToolbarDispatch ***
    // ******************************************************************

    { Method (NotifyAppearanceChanged) called when a content of the
      object of the appearance changes
      The object of the appearance contains colours and fonts used
      to draw the toolbar }
    procedure NotifyAppearanceChanged; override;

    { Method (NotifyItemsChanged) called when list of the sub-elements
      of one of toolbar elements changes }
    procedure NotifyItemsChanged; override;

    { Method (NotifyMetricsChanged) called when the size and position (metric)
      of one of toolbar elements change }
    procedure NotifyMetricsChanged; override;

    { Method (NotifyVisualsChanged) called when the appearance of one of
      toolbar elements changes
      if the toolbar element however doesn't need rebuilding of metrics }
    procedure NotifyVisualsChanged; override;

    { Method (GetTempBitmap) requests for suppporting bitmap delivered by toolbar
      For example, used to calculate the size of rendered text }
    function GetTempBitmap: TBitmap; override;

    { Method (ClientToScreen) converts the toolbar coordinates to screen coordinates
      For example, used to unfold popup menu }
    function ClientToScreen(Point: T2DIntPoint): T2DIntPoint; override;
  end;

  //Extended toolbar inspired by Microsoft Fluent UI

  { TSpkToolbar }

  TSpkToolbar = class(TCustomControl)
  private

    { Instance of dispatcher object
      Dispatcher is transfered to toolbar elements }
    FToolbarDispatch: TSpkToolbarDispatch;

    { Buffer bitmap to which toolbar is drawn }
    FBuffer: TBitmap;

    { Supporting bitmap is sent when toolbar elements request it }
    FTemporary: TBitmap;
   {$IFDEF DELAYRUNTIMER}
    FDelayRunTimer: TTimer;
   {$ENDIF}

    { Array of Rects of "handles" of tabs }
    FTabRects: array of T2DIntRect;

    { Cliprect region of "handles" of tabs }
    FTabClipRect: T2DIntRect;

    { ClipRect of region content of tab }
    FTabContentsClipRect: T2DIntRect;

    { The element over which the mouse pointer is }
    FMouseHoverElement: TSpkMouseToolbarElement;

    { The element over which the mouse pointer is and in which a mouse
      button is pressed }
    FMouseActiveElement: TSpkMouseToolbarElement;

    { The mouse pointer is now on the "handle" of tab }
    FTabHover: integer;

    { Flag which informs about validity of metrics of toolbar and its elements }
    FMetricsValid: boolean;

    { Flag which informs about validity of buffer content }
    FBufferValid: boolean;

    { Flag FInternalUpdating allows to block the validation of metrics and buffer
      when component is rebuilding its content
      The flag is switched on and off internally by component }
    FInternalUpdating: boolean;

    { Flag FUpdating allows to block the validation of metrics and buffer
      when user is rebuilding content of the component.
      FUpdating is controlled by user }
    FUpdating: boolean;

    { Quick selection of different appearances }
    FStyle: TSpkStyle;

    FOnTabChanging: TSpkTabChangingEvent;
    FOnTabChanged: TNotifyEvent;

   {$IFDEF DELAYRUNTIMER}
    procedure DelayRunTimer(Sender: TObject);
   {$ENDIF}

  protected

    { Instance of the Appearance object storing colours and fonts used during
      rendering of the component }
    FAppearance: TSpkToolbarAppearance;

    { Tabs of the toolbar }
    FTabs: TSpkTabs;

    { Index of the selected tab }
    FTabIndex: integer;

    { Imagelist of the small pictures of toolbar elements }
    FImages: TImageList;

    { Image list of the small pictures in the state "disabled".
      If the list is not assigned, small "disabled" pictures will be generated
      automatically }
    FDisabledImages: TImageList;

    { Imagelist of the large pictures of toolbar elements }
    FLargeImages: TImageList;

    { Image list of the large pictures in the state "disabled".
      If the list is not assigned, large "disabled" pictures will be generated
      automatically }
    FDisabledLargeImages: TImageList;

    function DoTabChanging(OldIndex, NewIndex: integer): boolean;

    // *****************************************************
    // *** Management of the metric and the buffer state ***
    // *****************************************************

    { Method switches flags FMetricsValid and FBufferValid off }
    procedure SetMetricsInvalid;

    { Method swiches flag FBufferValid off }
    procedure SetBufferInvalid;

    { Method validates toolbar metrics and toolbar elements }
    procedure ValidateMetrics;

    { Method validates the content of the buffer }
    procedure ValidateBuffer;

    { Method switches on the mode of internal rebuilding
      and swiches flag FInternalUpdating on }
    procedure InternalBeginUpdate;

    { Method switches on the mode of internal rebuilding
      and swiches the flag FInternalUpdating off}
    procedure InternalEndUpdate;

    // ************************************************
    // *** Covering of methods from derived classes ***
    // ************************************************

    { The Change of component size }
    procedure DoOnResize; override;
    procedure EraseBackground(DC: HDC); override;

    { Method called when mouse pointer left component region }
    procedure MouseLeave;

    { Method called when mouse button is pressed }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    { Method called when mouse pointer is moved over component }
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

    { Method called when the mouse button is released }
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    { Method called when the whole component has finished loading from LFM file }
    procedure Loaded; override;

    { Method called when component becomes the owner of other component,
      or one of its sub-components is released }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // ******************************************
    // *** Handling of mouse events for tabs  ***
    // ******************************************

    { Method called when mouse pointer left the region of tab "handles" }
    procedure TabMouseLeave;

    { Method called when the mouse button is pressed
      and at the same time the mouse pointer is over the region of tabs }
    procedure TabMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);

    { Method called when the mouse will move over the region of tab "handles" }
    procedure TabMouseMove(Shift: TShiftState; X, Y: integer);

    { Method called when one of the mouse buttons is released
      and at the same time the region of tabs was active element of toolbar }
    procedure TabMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer);

    // *********************
    // *** Extra support ***
    // *********************

    { Metchod checks if at least one of the tabs is switched on by flag Visible }
    function AtLeastOneTabVisible: boolean;

    // ****************
    // *** Messages ***
    // ****************

    { Message is received when mouse left the region of component }
    procedure CMMouseLeave(var msg: TLMessage); message CM_MOUSELEAVE;

    // **************************
    // *** Designtime and LFM ***
    // **************************

    {Method gives back elements which will be saved as sub-elements of component }
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    { Method allows for saving and reading additional properties of component }
    procedure DefineProperties(Filer: TFiler); override;

    // ***************************
    // *** Getters and setters ***
    // ***************************

    {Getter for property Height }
    function GetHeight: integer;

    { Setter for property Appearance }
    procedure SetAppearance(const Value: TSpkToolbarAppearance);

    { Getter for property Color }
    function GetColor: TColor;

    { Setter for property Color }
    procedure SetColor(const Value: TColor);

    { Setter for property TabIndex }
    procedure SetTabIndex(const Value: integer);

    { Setter for property Images }
    procedure SetImages(const Value: TImageList);

    { Setter for property DisabledImages }
    procedure SetDisabledImages(const Value: TImageList);

    { Setter for property LargeImages }
    procedure SetLargeImages(const Value: TImageList);

    { Setter for property DisabledLargeImages }
    procedure SetDisabledLargeImages(const Value: TImageList);

    { Setter for toolbar style, i.e. quick selection of new appearance theme }
    procedure SetStyle(const Value: TSpkStyle);

  public

    // *************************
    // *** Dispatcher events ***
    // *************************

    { Reaction to change of toolbar elements structure }
    procedure NotifyItemsChanged;

    { Reaction to change of toolbar elements metric }
    procedure NotifyMetricsChanged;

    { Reaction to change of toolbar elements appearance }
    procedure NotifyVisualsChanged;

    { Reaction to change of content of toolbar class appearance }
    procedure NotifyAppearanceChanged;

    { Method gives back the instance of supporting bitmap }
    function GetTempBitmap: TBitmap;

    // **********************************
    // *** Constructor and Destructor ***
    // **********************************

    { Constructor }
    constructor Create(AOwner: TComponent); override;

    { Destructor }
    destructor Destroy; override;

    // ***************
    // *** Drawing ***
    // ***************

    { Method draws the content of the component }
    procedure Paint; override;

    { Method enforces the rebuilding of metrics and buffer }
    procedure ForceRepaint;

    { Method swiches over the component in update mode of the content
      by switching on flag FUpdating }
    procedure BeginUpdate;

    { Method switches off the update mode of the content
      by switching off flag FUpdating }
    procedure EndUpdate;

    // ****************
    // *** Elements ***
    // ****************

    { Method called when one of the tabs is released
      You cannot call method FreeingTab from code (by writing it in code)
      It's called internally and its purpuse is to update internal list of tabs }
    procedure FreeingTab(ATab: TSpkTab);

    // **********************
    // *** Access to tabs ***
    // **********************

    { Property gives accesss to tabs in runtime mode
      To edit tabs in designtime mode use proper editor
      Savings and readings from LFM is done manually }
    property Tabs: TSpkTabs read FTabs;

  published

    { Component background color }
    property Color: TColor read GetColor write SetColor default clSkyBlue;

    { Appearance style - don't move after Appearance! }
    property Style: TSpkStyle read FStyle write SetStyle default spkOffice2007Blue;

    { Object containing attributes of toolbar appearance }
    property Appearance: TSpkToolbarAppearance read FAppearance write SetAppearance;

    { Height of toolbar (read-only) }
    property Height: integer read GetHeight;

    { Index of active tab }
    property TabIndex: integer read FTabIndex write SetTabIndex;

    { ImageList with the small pictures }
    property Images: TImageList read FImages write SetImages;

    { ImageList with the small pictures in state "disabled" }
    property DisabledImages: TImageList read FDisabledImages write SetDisabledImages;

    { ImageList with the large pictures }
    property LargeImages: TImageList read FLargeImages write SetLargeImages;

    { ImageList with the large pictures in state "disabled" }
    property DisabledLargeImages: TImageList
      read FDisabledLargeImages write SetDisabledLargeImages;

    { Events called before and after another tab is selected }
    property OnTabChanging: TSpkTabChangingEvent
      read FOnTabChanging write FOnTabChanging;
    property OnTabChanged: TNotifyEvent read FOnTabChanged write FOnTabChanged;
  end;

implementation

uses
  LCLIntf, Themes;

{ TSpkToolbarDispatch }

function TSpkToolbarDispatch.ClientToScreen(Point: T2DIntPoint): T2DIntPoint;
begin
  {$IFDEF EnhancedRecordSupport}
  if FToolbar <> nil then
    Result := FToolbar.ClientToScreen(Point)
  else
    Result := T2DIntPoint.Create(-1, -1);
  {$ELSE}
  if FToolbar <> nil then
    Result := FToolbar.ClientToScreen(Point)
  else
    Result.Create(-1, -1);
  {$ENDIF}
end;

constructor TSpkToolbarDispatch.Create(AToolbar: TSpkToolbar);
begin
  inherited Create;
  FToolbar := AToolbar;
end;

function TSpkToolbarDispatch.GetTempBitmap: TBitmap;
begin
  if FToolbar <> nil then
    Result := FToolbar.GetTempBitmap
  else
    Result := nil;
end;

procedure TSpkToolbarDispatch.NotifyAppearanceChanged;
begin
  if FToolbar <> nil then
    FToolbar.NotifyAppearanceChanged;
end;

procedure TSpkToolbarDispatch.NotifyMetricsChanged;
begin
  if FToolbar <> nil then
    FToolbar.NotifyMetricsChanged;
end;

procedure TSpkToolbarDispatch.NotifyItemsChanged;
begin
  if FToolbar <> nil then
    FToolbar.NotifyItemsChanged;
end;

procedure TSpkToolbarDispatch.NotifyVisualsChanged;
begin
  if FToolbar <> nil then
    FToolbar.NotifyVisualsChanged;
end;

{ TSpkToolbar }

function TSpkToolbar.AtLeastOneTabVisible: boolean;

var
  i: integer;
  TabVisible: boolean;

begin
  Result := FTabs.Count > 0;
  if Result then
  begin
    TabVisible := False;
    i := FTabs.Count - 1;
    while (i >= 0) and not (TabVisible) do
    begin
      TabVisible := FTabs[i].Visible;
      Dec(i);
    end;
    Result := Result and TabVisible;
  end;
end;

procedure TSpkToolbar.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TSpkToolbar.CMMouseLeave(var msg: TLMessage);
begin
  inherited;
  MouseLeave;
end;

constructor TSpkToolbar.Create(AOwner: TComponent);
var
  DesignDPI: Integer;
begin
  inherited Create(AOwner);

  // Initialization of inherited property
  inherited Align := alTop;
  //todo: not found in lcl
  //inherited AlignWithMargins:=true;

  if AOwner is TForm then
    DesignDPI := TForm(AOwner).DesignTimeDPI
  else
    DesignDPI := ScreenInfo.PixelsPerInchX;

  SpkInitLayoutConsts(DesignDPI);
  inherited Height := ToolbarHeight;

  //inherited Doublebuffered:=true;

  // Initialization of internal data fields
  FToolbarDispatch := TSpkToolbarDispatch.Create(self);

  FBuffer := TBitmap.Create;
  FBuffer.PixelFormat := pf24bit;

  FTemporary := TBitmap.Create;
  FTemporary.Pixelformat := pf24bit;

  setlength(FTabRects, 0);

  {$IFDEF EnhancedRecordSupport}
  FTabClipRect := T2DIntRect.Create(0, 0, 0, 0);
  FTabContentsClipRect := T2DIntRect.Create(0, 0, 0, 0);
  {$ELSE}
  FTabClipRect.Create(0, 0, 0, 0);
  FTabContentsClipRect.Create(0, 0, 0, 0);
  {$ENDIF}

  FMouseHoverElement := teNone;
  FMouseActiveElement := teNone;

  FTabHover := -1;

  // Initialization of fields
  FAppearance := TSpkToolbarAppearance.Create(FToolbarDispatch);

  FTabs := TSpkTabs.Create(self);
  FTabs.ToolbarDispatch := FToolbarDispatch;
  FTabs.Appearance := FAppearance;

  FTabIndex := -1;
  Color := clSkyBlue;

 {$IFDEF DELAYRUNTIMER}
  FDelayRunTimer := TTimer.Create(nil);
  FDelayRunTimer.Interval := 36;
  FDelayRunTimer.Enabled := False;
  FDelayRunTimer.OnTimer := DelayRunTimer
 {$ENDIF}
end;

{$IFDEF DELAYRUNTIMER}
procedure TSpkToolbar.DelayRunTimer(Sender: TObject);
begin
  SetMetricsInvalid;
  SetBufferInvalid;
  invalidate;
  FDelayRunTimer.Enabled := False;
end;
{$ENDIF}

procedure TSpkToolbar.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Tabs', FTabs.ReadNames, FTabs.WriteNames, True);
end;

destructor TSpkToolbar.Destroy;
begin
  // Release the fields
  FTabs.Free;

  FAppearance.Free;

  // Release the internal fields
  FTemporary.Free;
  FBuffer.Free;

  FToolbarDispatch.Free;

 {$IFDEF DELAYRUNTIMER}
  FDelayRunTimer.Free;
 {$ENDIF}

  inherited Destroy;
end;

procedure TSpkToolbar.EndUpdate;
begin
  FUpdating := False;

  ValidateMetrics;
  ValidateBuffer;
  Repaint;
end;

procedure TSpkToolbar.ForceRepaint;
begin
  SetMetricsInvalid;
  SetBufferInvalid;
  Repaint;
end;

procedure TSpkToolbar.FreeingTab(ATab: TSpkTab);
begin
  FTabs.RemoveReference(ATab);
end;

procedure TSpkToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: integer;
begin
  inherited;

  if FTabs.Count > 0 then
    for i := 0 to FTabs.Count - 1 do
      Proc(FTabs.Items[i]);
end;

function TSpkToolbar.GetColor: TColor;
begin
  Result := inherited Color;
end;

function TSpkToolbar.GetHeight: integer;
begin
  Result := inherited Height;
end;

function TSpkToolbar.GetTempBitmap: TBitmap;
begin
  Result := FTemporary;
end;

procedure TSpkToolbar.InternalBeginUpdate;
begin
  FInternalUpdating := True;
end;

procedure TSpkToolbar.InternalEndUpdate;
begin
  FInternalUpdating := False;

  //After internal changes the metrics and buffers are refreshed
  ValidateMetrics;
  ValidateBuffer;
  Repaint;
end;

procedure TSpkToolbar.Loaded;
begin
  inherited;

  InternalBeginUpdate;

  if FTabs.ListState = lsNeedsProcessing then
  begin
    FTabs.ProcessNames(self.Owner);
  end;

  InternalEndUpdate;

  //The process of internal update always refreshes metrics and buffer at the end
  //and draws component
end;

procedure TSpkToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  inherited MouseDown(Button, Shift, X, Y);

  //It is possible that the other mouse button was pressed
  //In this situation active object receives next notification
  if FMouseActiveElement = teTabs then
  begin
    TabMouseDown(Button, Shift, X, Y);
  end
  else
  if FMouseActiveElement = teTabContents then
  begin
    if FTabIndex <> -1 then
      FTabs[FTabIndex].MouseDown(Button, Shift, X, Y);
  end
  else
  if FMouseActiveElement = teToolbarArea then
  begin
    //Placeholder if there will be need to use this event
  end
  else
  //If there is no active element, the active element will be one
  //which is now under the mouse
  if FMouseActiveElement = teNone then
  begin
    if FMouseHoverElement = teTabs then
    begin
      FMouseActiveElement := teTabs;
      TabMouseDown(Button, Shift, X, Y);
    end
    else
    if FMouseHoverElement = teTabContents then
    begin
      FMouseActiveElement := teTabContents;
      if FTabIndex <> -1 then
        FTabs[FTabIndex].MouseDown(Button, Shift, X, Y);
    end
    else
    if FMouseHoverElement = teToolbarArea then
    begin
      FMouseActiveElement := teToolbarArea;
      //Placeholder if there will be need to use this event
    end;
  end;
end;

procedure TSpkToolbar.MouseLeave;
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  //MouseLeave has no chance to be called for active object
  //because when the mouse button is pressed every mouse move is transfered
  //as MouseMove. If the mouse left from component region then
  //MouseLeave will be called just after MouseUp but MouseUp cleans the
  //active object
  if FMouseActiveElement = teNone then
  begin
    //If there is no active element, the elements under mouse will be supported
    if FMouseHoverElement = teTabs then
    begin
      TabMouseLeave;
    end
    else
    if FMouseHoverElement = teTabContents then
    begin
      if FTabIndex <> -1 then
        FTabs[FTabIndex].MouseLeave;
    end
    else
    if FMouseHoverElement = teToolbarArea then
    begin
      //Placeholder if there will be need to use this event
    end;
  end;

  FMouseHoverElement := teNone;
end;

procedure TSpkToolbar.MouseMove(Shift: TShiftState; X, Y: integer);
var
  NewMouseHoverElement: TSpkMouseToolbarElement;
  MousePoint: T2DIntVector;
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  inherited MouseMove(Shift, X, Y);

  //Checking which element is under the mouse
  {$IFDEF EnhancedRecordSupport}
  MousePoint := T2DIntVector.Create(x, y);
  {$ELSE}
  MousePoint.Create(x, y);
  {$ENDIF}

  if FTabClipRect.Contains(MousePoint) then
    NewMouseHoverElement := teTabs
  else
  if FTabContentsClipRect.Contains(MousePoint) then
    NewMouseHoverElement := teTabContents
  else
  if (X >= 0) and (Y >= 0) and (X < self.Width) and (Y < self.Height) then
    NewMouseHoverElement := teToolbarArea
  else
    NewMouseHoverElement := teNone;

  //If there is an active element then it has exlusiveness for messages
  if FMouseActiveElement = teTabs then
  begin
    TabMouseMove(Shift, X, Y);
  end
  else
  if FMouseActiveElement = teTabContents then
  begin
    if FTabIndex <> -1 then
      FTabs[FTabIndex].MouseMove(Shift, X, Y);
  end
  else
  if FMouseActiveElement = teToolbarArea then
  begin
    //Placeholder if there will be need to use this event
  end
  else
  if FMouseActiveElement = teNone then
  begin
    //If element changes under the mouse, then previous element will be informed
    //that mouse is leaving its region
    if NewMouseHoverElement <> FMouseHoverElement then
    begin
      if FMouseHoverElement = teTabs then
      begin
        TabMouseLeave;
      end
      else
      if FMouseHoverElement = teTabContents then
      begin
        if FTabIndex <> -1 then
          FTabs[FTabIndex].MouseLeave;
      end
      else
      if FMouseHoverElement = teToolbarArea then
      begin
        //Placeholder if there will be need to use this event
      end;
    end;

    //Element under mouse receives MouseMove
    if NewMouseHoverElement = teTabs then
    begin
      TabMouseMove(Shift, X, Y);
    end
    else
    if NewMouseHoverElement = teTabContents then
    begin
      if FTabIndex <> -1 then
        FTabs[FTabIndex].MouseMove(Shift, X, Y);
    end
    else
    if NewMouseHoverElement = teToolbarArea then
    begin
      //Placeholder if there will be need to use this event
    end;
  end;

  FMouseHoverElement := NewMouseHoverElement;
end;

procedure TSpkToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ClearActive: boolean;
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  inherited MouseUp(Button, Shift, X, Y);

  ClearActive := not (ssLeft in Shift) and not (ssMiddle in Shift) and not (ssRight in Shift);

  //If there is an active element then it has exlusiveness for messages
  if FMouseActiveElement = teTabs then
  begin
    TabMouseUp(Button, Shift, X, Y);
  end
  else
  if FMouseActiveElement = teTabContents then
  begin
    if FTabIndex <> -1 then
      FTabs[FTabIndex].MouseUp(Button, Shift, X, Y);
  end
  else
  if FMouseActiveElement = teToolbarArea then
  begin
    //Placeholder if there will be need to use this event
  end;

  //If the last mouse button is released and mouse doesn't locate over
  //the active object, it must additionally call MouseLeave for active one
  //and MouseMove for object being under mouse
  if ClearActive and (FMouseActiveElement <> FMouseHoverElement) then
  begin
    if FMouseActiveElement = teTabs then
      TabMouseLeave
    else
    if FMouseActiveElement = teTabContents then
    begin
      if FTabIndex <> -1 then
        FTabs[FTabIndex].MouseLeave;
    end
    else
    if FMouseActiveElement = teToolbarArea then
    begin
      //Placeholder if there will be need to use this event
    end;

    if FMouseHoverElement = teTabs then
      TabMouseMove(Shift, X, Y)
    else
    if FMouseHoverElement = teTabContents then
    begin
      if FTabIndex <> -1 then
        FTabs[FTabIndex].MouseMove(Shift, X, Y);
    end
    else
    if FMouseHoverElement = teToolbarArea then
    begin
      //Placeholder if there will be need to use this event
    end;
  end;

  //MouseUp swiches off active object, when all mouse buttons were released
  if ClearActive then
    FMouseActiveElement := teNone;
end;

procedure TSpkToolbar.Notification(AComponent: TComponent; Operation: TOperation);
var
  Tab: TSpkTab;
  Pane: TSpkPane;
  Item: TSpkBaseItem;
begin
  inherited;

  if Operation <> opRemove then
    exit;

  if AComponent is TSpkTab then
  begin
    FreeingTab(AComponent as TSpkTab);
  end
  else
  if AComponent is TSpkPane then
  begin
    Pane := AComponent as TSpkPane;
    if (Pane.Parent <> nil) and (Pane.Parent is TSpkTab) then
    begin
      Tab := Pane.Parent as TSpkTab;
      Tab.FreeingPane(Pane);
    end;
  end
  else
  if AComponent is TSpkBaseItem then
  begin
    Item := AComponent as TSpkBaseItem;
    if (Item.Parent <> nil) and (Item.Parent is TSpkPane) then
    begin
      Pane := Item.Parent as TSpkPane;
      Pane.FreeingItem(Item);
    end;
  end;
end;

procedure TSpkToolbar.NotifyAppearanceChanged;
begin
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.NotifyMetricsChanged;
begin
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.NotifyItemsChanged;
var
  OldTabIndex: integer;
begin
  OldTabIndex := FTabIndex;
  // Fixed TabIndex when you need it
  if not (AtLeastOneTabVisible) then
    FTabIndex := -1
  else
  begin
    FTabIndex := max(0, min(FTabs.Count - 1, FTabIndex));

    //I know that at least one tab is visible (from previous condition)
    //so below loop will finish
    while not (FTabs[FTabIndex].Visible) do
      FTabIndex := (FTabIndex + 1) mod FTabs.Count;
  end;
  FTabHover := -1;

  if DoTabChanging(OldTabIndex, FTabIndex) then
  begin
    SetMetricsInvalid;

    if not (FInternalUpdating or FUpdating) then
      Repaint;

    if Assigned(FOnTabChanged) then
      FOnTabChanged(self);
  end
  else
    FTabIndex := OldTabIndex;

end;

procedure TSpkToolbar.NotifyVisualsChanged;
begin
  SetBufferInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.Paint;
begin
  //If the rebuilding process (internal or by user) is running now
  //then validation of metrics and buffer is not running, however
  //the buffer is drawn in a shape what was remembered before rebuilding process
  if not (FInternalUpdating or FUpdating) then
  begin
    if not (FMetricsValid) then
      ValidateMetrics;
    if not (FBufferValid) then
      ValidateBuffer;
  end;
  self.canvas.draw(0, 0, FBuffer);
end;

procedure TSpkToolbar.DoOnResize;
begin
  inherited Height := ToolbarHeight;

 {$IFDEF DELAYRUNTIMER}
  FDelayRunTimer.Enabled := False;
  FDelayRunTimer.Enabled := True;
 {$ELSE}
  SetMetricsInvalid;
  SetBufferInvalid;
 {$ENDIF}

  if not (FInternalUpdating or FUpdating) then
    invalidate;

  inherited;
end;

procedure TSpkToolbar.EraseBackground(DC: HDC);
begin
  // The correct implementation is doing nothing
  if ThemeServices.ThemesEnabled then
    inherited;   // wp: this calls FillRect!
  // "inherited" removed in case of no themes to fix issue #0025047 (flickering
  // when using standard windows theme or when manifest file is off)
end;

procedure TSpkToolbar.SetBufferInvalid;
begin
  FBufferValid := False;
end;

procedure TSpkToolbar.SetColor(const Value: TColor);
begin
  inherited Color := Value;
  SetBufferInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.SetDisabledImages(const Value: TImageList);
begin
  FDisabledImages := Value;
  FTabs.DisabledImages := Value;
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.SetDisabledLargeImages(const Value: TImageList);
begin
  FDisabledLargeImages := Value;
  FTabs.DisabledLargeImages := Value;
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.SetImages(const Value: TImageList);
begin
  FImages := Value;
  FTabs.Images := Value;
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.SetLargeImages(const Value: TImageList);
begin
  FLargeImages := Value;
  FTabs.LargeImages := Value;
  SetMetricsInvalid;

  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.SetStyle(const Value: TSpkStyle);
begin
  FStyle := Value;
  FAppearance.Reset(FStyle);
  ForceRepaint;
end;

function TSpkToolbar.DoTabChanging(OldIndex, NewIndex: integer): boolean;
begin
  Result := True;
  if Assigned(FOnTabChanging) then
    FOnTabChanging(Self, OldIndex, NewIndex, Result);
end;

procedure TSpkToolbar.SetMetricsInvalid;
begin
  FMetricsValid := False;
  FBufferValid := False;
end;

procedure TSpkToolbar.SetTabIndex(const Value: integer);
var
  OldTabIndex: integer;
begin
  OldTabIndex := FTabIndex;

  if not (AtLeastOneTabVisible) then
    FTabIndex := -1
  else
  begin
    FTabIndex := max(0, min(FTabs.Count - 1, Value));

    //I know that at least one tab is visible (from previous condition)
    //so below loop will finish
    while not (FTabs[FTabIndex].Visible) do
      FTabIndex := (FTabIndex + 1) mod FTabs.Count;
  end;
  FTabHover := -1;

  if DoTabChanging(OldTabIndex, FTabIndex) then
  begin
    SetMetricsInvalid;
    if not (FInternalUpdating or FUpdating) then
      Repaint;
    if Assigned(FOnTabChanged) then
      FOnTabChanged(self);
  end
  else
    FTabIndex := OldTabIndex;
end;

procedure TSpkToolbar.TabMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  SelTab: integer;
  TabRect: T2DIntRect;
  i: integer;
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  SelTab := -1;
  if AtLeastOneTabVisible then
    for i := 0 to FTabs.Count - 1 do
      if FTabs[i].Visible then
      begin
        if FTabClipRect.IntersectsWith(FTabRects[i], TabRect) then
             {$IFDEF EnhancedRecordSupport}
          if TabRect.Contains(T2DIntPoint.Create(x, y)) then
             {$ELSE}
            if TabRect.Contains(x, y) then
             {$ENDIF}
              SelTab := i;
      end;

  //If any tab was clicked but one (not being selected) then change selection
  if (Button = mbLeft) and (SelTab <> -1) and (SelTab <> FTabIndex) then
  begin
    if DoTabChanging(FTabIndex, SelTab) then
    begin
      FTabIndex := SelTab;
      SetMetricsInvalid;
      Repaint;
      if Assigned(FOnTabChanged) then
        FOnTabChanged(self);
    end;
  end;
end;

procedure TSpkToolbar.TabMouseLeave;
begin
  //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  if FTabHover <> -1 then
  begin
    FTabHover := -1;
    SetBufferInvalid;
    Repaint;
  end;
end;

procedure TSpkToolbar.TabMouseMove(Shift: TShiftState; X, Y: integer);
var
  NewTabHover: integer;
  TabRect: T2DIntRect;
  i: integer;
begin
 //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  NewTabHover := -1;
  if AtLeastOneTabVisible then
    for i := 0 to FTabs.Count - 1 do
      if FTabs[i].Visible then
      begin
        if FTabClipRect.IntersectsWith(FTabRects[i], TabRect) then
             {$IFDEF EnhancedRecordSupport}
          if TabRect.Contains(T2DIntPoint.Create(x, y)) then
             {$ELSE}
            if TabRect.Contains(x, y) then
             {$ENDIF}
              NewTabHover := i;
      end;

  if NewTabHover <> FTabHover then
  begin
    FTabHover := NewTabHover;
    SetBufferInvalid;
    Repaint;
  end;
end;

procedure TSpkToolbar.TabMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
 //During rebuilding procees the mouse is ignored
  if FInternalUpdating or FUpdating then
    exit;

  if (FTabIndex > -1) then
    FTabs[FTabIndex].ExecOnClick;


  //Tabs don't need MouseUp
end;

procedure TSpkToolbar.SetAppearance(const Value: TSpkToolbarAppearance);
begin
  FAppearance.Assign(Value);

  SetBufferInvalid;
  if not (FInternalUpdating or FUpdating) then
    Repaint;
end;

procedure TSpkToolbar.ValidateBuffer;

  procedure DrawBackgroundColor;
  begin
    FBuffer.Canvas.Brush.Color := Color;
    FBuffer.Canvas.Brush.Style := bsSolid;
    FBuffer.Canvas.FillRect(Rect(0, 0, self.Width, self.Height));
  end;

  procedure DrawBody;
  var
    FocusedAppearance: TSpkToolbarAppearance;
    i: integer;
  begin
    //Loading appearance of selected tab
    //or FToolbarAppearance if selected tab has no set OverrideAppearance
    if (FTabIndex <> -1) and (FTabs[FTabIndex].OverrideAppearance) then
      FocusedAppearance := FTabs[FTabIndex].CustomAppearance
    else
      FocusedAppearance := FAppearance;

    TGuiTools.DrawRoundRect(FBuffer.Canvas,
                          {$IFDEF EnhancedRecordSupport}
      T2DIntRect.Create(0, ToolbarTabCaptionsHeight, self.Width - 1, self.Height - 1),
                          {$ELSE}
      Create2DIntRect(0, ToolbarTabCaptionsHeight, self.Width - 1, self.Height - 1),
                          {$ENDIF}
      ToolbarCornerRadius,
      FocusedAppearance.Tab.GradientFromColor,
      FocusedAppearance.Tab.GradientToColor,
      FocusedAppearance.Tab.GradientType);
    TGuiTools.DrawAARoundCorner(FBuffer,
                              {$IFDEF EnhancedRecordSupport}
      T2DIntPoint.Create(0, ToolbarTabCaptionsHeight),
                              {$ELSE}
      Create2DIntPoint(0, ToolbarTabCaptionsHeight),
                              {$ENDIF}
      ToolbarCornerRadius,
      cpLeftTop,
      FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawAARoundCorner(FBuffer,
                              {$IFDEF EnhancedRecordSupport}
      T2DIntPoint.Create(self.Width - ToolbarCornerRadius, ToolbarTabCaptionsHeight),
                              {$ELSE}
      Create2DIntPoint(self.Width - ToolbarCornerRadius, ToolbarTabCaptionsHeight),
                              {$ENDIF}
      ToolbarCornerRadius,
      cpRightTop,
      FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawAARoundCorner(FBuffer,
                              {$IFDEF EnhancedRecordSupport}
      T2DIntPoint.Create(0, self.Height - ToolbarCornerRadius),
                              {$ELSE}
      Create2DIntPoint(0, self.Height - ToolbarCornerRadius),
                              {$ENDIF}
      ToolbarCornerRadius,
      cpLeftBottom,
      FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawAARoundCorner(FBuffer,
                              {$IFDEF EnhancedRecordSupport}
      T2DIntPoint.Create(self.Width - ToolbarCornerRadius, self.Height - ToolbarCornerRadius),
                              {$ELSE}
      Create2DIntPoint(self.Width - ToolbarCornerRadius, self.Height - ToolbarCornerRadius),
                              {$ENDIF}
      ToolbarCornerRadius,
      cpRightBottom,
      FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawVLine(FBuffer, 0, ToolbarTabCaptionsHeight +
      ToolbarCornerRadius, self.Height - ToolbarCornerRadius,
      FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawHLine(FBuffer, ToolbarCornerRadius, self.Width - ToolbarCornerRadius,
      self.Height - 1, FocusedAppearance.Tab.BorderColor);
    TGuiTools.DrawVLine(FBuffer, self.Width - 1, ToolbarTabCaptionsHeight +
      ToolbarCornerRadius, self.Height - ToolbarCornerRadius,
      FocusedAppearance.Tab.BorderColor);

    if not (AtLeastOneTabVisible) then
    begin

      //If there are no tabs then the horizontal line will be drawn
      TGuiTools.DrawHLine(FBuffer, ToolbarCornerRadius, self.Width -
        ToolbarCornerRadius, ToolbarTabCaptionsHeight, FocusedAppearance.Tab.BorderColor);
    end
    else
    begin
      //If there are tabs then the place will be left for them
      //Last visible tab is looked for
      i := FTabs.Count - 1;
      while not (FTabs[i].Visible) do
        Dec(i);

      //Only right part, the rest will be drawn with tabs
      if FTabRects[i].Right < self.Width - ToolbarCornerRadius - 1 then
        TGuiTools.DrawHLine(FBuffer, FTabRects[i].Right + 1, self.Width -
          ToolbarCornerRadius, ToolbarTabCaptionsHeight, FocusedAppearance.Tab.BorderColor);
    end;
  end;

  procedure DrawTabs;
  var
    i: integer;
    TabRect: T2DIntRect;
    CurrentAppearance: TSpkToolbarAppearance;
    FocusedAppearance: TSpkToolbarAppearance;

    procedure DrawTabText(index: integer; AFont: TFont; AOverrideTextColor: TColor = clNone);
    var
      x, y: integer;
      TabRect: T2DIntRect;
      clr: TColor;
    begin
      TabRect := FTabRects[index];

      FBuffer.canvas.font.Assign(AFont);
      if AOverrideTextColor <> clNone then
        clr := AOverrideTextColor else
        clr := AFont.Color;
      x := TabRect.left + (TabRect.Width - FBuffer.Canvas.textwidth(
        FTabs[index].Caption)) div 2;
      y := TabRect.top + (TabRect.Height - FBuffer.Canvas.Textheight('Wy')) div 2;

      TGuiTools.DrawText(FBuffer.Canvas,
        x,
        y,
        FTabs[index].Caption,
        clr,
        FTabClipRect);
    end;

    procedure DrawTab(index: integer;
      Border, GradientFrom, GradientTo: TColor);
    var
      TabRect: T2DIntRect;
      TabRegion: HRGN;
      TmpRegion, TmpRegion2: HRGN;
    begin
      //Note!! Tabs cover one pixel of toolbar region, because
      // the they must draw edge, which fits in with region edge
      TabRect := FTabRects[index];

      //Middle rectangle
      TabRegion := CreateRectRgn(
        TabRect.Left + TabCornerRadius - 1,
        TabRect.Top + TabCornerRadius,
        TabRect.Right - TabCornerRadius + 1 + 1,
        TabRect.Bottom + 1
      );

      //Top part with top convex curves
      TmpRegion := CreateRectRgn(
        TabRect.Left + 2 * TabCornerRadius - 1,
        TabRect.Top,
        TabRect.Right - 2 * TabCornerRadius + 1 + 1,
        TabRect.Top + TabCornerRadius
      );
      CombineRgn(TabRegion, TabRegion, TmpRegion, RGN_OR);
      DeleteObject(TmpRegion);

      TmpRegion := CreateEllipticRgn(
        TabRect.Left + TabCornerRadius - 1,
        TabRect.Top,
        TabRect.Left + 3 * TabCornerRadius,
        TabRect.Top + 2 * TabCornerRadius + 1
      );
      CombineRgn(TabRegion, TabRegion, TmpRegion, RGN_OR);
      DeleteObject(TmpRegion);

      TmpRegion := CreateEllipticRgn(
        TabRect.Right - 3 * TabCornerRadius + 2,
        TabRect.Top,
        TabRect.Right - TabCornerRadius + 3,
        TabRect.Top + 2 * TabCornerRadius + 1
      );
      CombineRgn(TabRegion, TabRegion, TmpRegion, RGN_OR);
      DeleteObject(TmpRegion);

      //Bottom part with bottom convex curves
      TmpRegion := CreateRectRgn(
        TabRect.Left,
        TabRect.Bottom - TabCornerRadius,
        TabRect.Right + 1,
        TabRect.Bottom + 1
      );

      TmpRegion2 := CreateEllipticRgn(
        TabRect.Left - TabCornerRadius,
        TabRect.Bottom - 2 * TabCornerRadius + 1,
        TabRect.Left + TabCornerRadius + 1,
        TabRect.Bottom + 2
      );
      CombineRgn(TmpRegion, TmpRegion, TmpRegion2, RGN_DIFF);
      DeleteObject(TmpRegion2);

      TmpRegion2 := CreateEllipticRgn(
        TabRect.Right - TabCornerRadius + 1,
        TabRect.Bottom - 2 * TabCornerRadius + 1,
        TabRect.Right + TabCornerRadius + 2,
        TabRect.Bottom + 2
      );
      CombineRgn(TmpRegion, TmpRegion, TmpRegion2, RGN_DIFF);
      DeleteObject(TmpRegion2);

      CombineRgn(TabRegion, TabRegion, TmpRegion, RGN_OR);
      DeleteObject(TmpRegion);

      TGUITools.DrawRegion(FBuffer.Canvas,
        TabRegion,
        TabRect,
        GradientFrom,
        GradientTo,
        bkVerticalGradient);

      DeleteObject(TabRegion);

      // Frame
      TGuiTools.DrawAARoundCorner(FBuffer,
                                {$IFDEF EnhancedRecordSupport}
        T2DIntPoint.Create(TabRect.left, TabRect.bottom - TabCornerRadius + 1),
                                {$ELSE}
        Create2DIntPoint(TabRect.left, TabRect.bottom - TabCornerRadius + 1),
                                {$ENDIF}
        TabCornerRadius,
        cpRightBottom,
        Border,
        FTabClipRect);

      TGuiTools.DrawAARoundCorner(FBuffer,
                                {$IFDEF EnhancedRecordSupport}
        T2DIntPoint.Create(TabRect.right - TabCornerRadius + 1, TabRect.bottom - TabCornerRadius + 1),
                                {$ELSE}
        Create2DIntPoint(TabRect.right - TabCornerRadius + 1, TabRect.bottom - TabCornerRadius + 1),
                                {$ENDIF}
        TabCornerRadius,
        cpLeftBottom,
        Border,
        FTabClipRect);

      TGuiTools.DrawVLine(FBuffer,
        TabRect.left + TabCornerRadius - 1,
        TabRect.top + TabCornerRadius,
        TabRect.Bottom - TabCornerRadius + 1,
        Border,
        FTabClipRect);

      TGuiTools.DrawVLine(FBuffer,
        TabRect.Right - TabCornerRadius + 1,
        TabRect.top + TabCornerRadius,
        TabRect.Bottom - TabCornerRadius + 1,
        Border,
        FTabClipRect);

      TGuiTools.DrawAARoundCorner(FBuffer,
                                {$IFDEF EnhancedRecordSupport}
        T2DIntPoint.Create(TabRect.Left + TabCornerRadius - 1, 0),
                                {$ELSE}
        Create2DIntPoint(TabRect.Left + TabCornerRadius - 1, 0),
                                {$ENDIF}
        TabCornerRadius,
        cpLeftTop,
        Border,
        FTabClipRect);

      TGuiTools.DrawAARoundCorner(FBuffer,
                                {$IFDEF EnhancedRecordSupport}
        T2DIntPoint.Create(TabRect.Right - 2 * TabCornerRadius + 2, 0),
                                {$ELSE}
        Create2DIntPoint(TabRect.Right - 2 * TabCornerRadius + 2, 0),
                                {$ENDIF}
        TabCornerRadius,
        cpRightTop,
        Border,
        FTabClipRect);

      TGuiTools.DrawHLine(FBuffer,
        TabRect.Left + 2 * TabCornerRadius - 1,
        TabRect.Right - 2 * TabCornerRadius + 2,
        0,
        Border,
        FTabClipRect);
    end;

    procedure DrawBottomLine(index: integer;
      Border: TColor);
    var
      TabRect: T2DIntRect;
    begin
      TabRect := FTabRects[index];

      TGUITools.DrawHLine(FBuffer,
        TabRect.left,
        TabRect.right,
        TabRect.bottom,
        Border,
        FTabClipRect);
    end;

  var
    delta: Integer;
  begin
    //I assume that the tabs size is reasonable

    //Loading appearance of selected now tab (her appearance, if
    //its flag - OverrideAppearance is switched on otherwise
    //FToolbarAppearance
    if (FTabIndex <> -1) and (FTabs[FTabIndex].OverrideAppearance) then
      FocusedAppearance := FTabs[FTabIndex].CustomAppearance
    else
      FocusedAppearance := FAppearance;

    if FTabs.Count > 0 then
      for i := 0 to FTabs.Count - 1 do
        if FTabs[i].Visible then
        begin
          // Is there any sense to draw?
          if not (FTabClipRect.IntersectsWith(FTabRects[i])) then
            continue;

          //Loading appearance of now drawn tab
          if (FTabs[i].OverrideAppearance) then
            CurrentAppearance := FTabs[i].CustomAppearance
          else
            CurrentAppearance := FAppearance;

          if CurrentAppearance.Tab.GradientType = bkSolid then
            delta := 0 else
            delta := 50;

          TabRect := FTabRects[i];

          // Tab is drawn
          if i = FTabIndex then      // active tab
          begin
            if i = FTabHover then
            begin
              DrawTab(i,
                CurrentAppearance.Tab.BorderColor,
                TColorTools.Brighten(TColorTools.Brighten(
                  CurrentAppearance.Tab.GradientFromColor, delta), delta),
                CurrentAppearance.Tab.GradientFromColor);
            end
            else
            begin
              DrawTab(i,
                CurrentAppearance.Tab.BorderColor,
                TColorTools.Brighten(
                  CurrentAppearance.Tab.GradientFromColor, delta),
                CurrentAppearance.Tab.GradientFromColor);
            end;

            DrawTabText(i, CurrentAppearance.Tab.TabHeaderFont);
          end
          else
          begin                     // inactive tab
            if i = FTabHover then
            begin
              DrawTab(i,
                TColorTools.Shade(
                  self.Color, CurrentAppearance.Tab.BorderColor, delta),
                TColorTools.Shade(self.color,
                  TColorTools.Brighten(CurrentAppearance.Tab.GradientFromColor, delta), 50),
                TColorTools.Shade(
                  self.color, CurrentAppearance.Tab.GradientFromColor, 50) );
            end;

            // Bottom line
            //Warning!! Irrespective of tab , the appearance will be drawn
            //with color now selected tab
            DrawBottomLine(i, FocusedAppearance.Tab.BorderColor);

            // Text
            DrawTabText(i, CurrentAppearance.Tab.TabHeaderFont,
              CurrentAppearance.Tab.InactiveTabHeaderFontColor);
          end;
        end;
  end;

  procedure DrawTabContents;
  begin
    if FTabIndex <> -1 then
      FTabs[FTabIndex].Draw(FBuffer, FTabContentsClipRect);
  end;

begin
  if FInternalUpdating or FUpdating then
    exit;
  if FBufferValid then
    exit;

  // ValidateBuffer could be called only when metrics is calulated
  //Method assumes that buffer has proper sizes and all rects of toolbar and
  //sub-elements are correctly calculated

  // *** Component background ***
  DrawBackgroundColor;

  // *** The toolbar background is generated ***
  DrawBody;

  // *** Tabs ***
  DrawTabs;

  // *** Tabs content ***
  DrawTabContents;

  // Buffer is correct
  FBufferValid := True;
end;

procedure TSpkToolbar.ValidateMetrics;
var
  i: integer;
  x: integer;
  TabWidth: integer;
  TabAppearance: TSpkToolbarAppearance;
begin
  if FInternalUpdating or FUpdating then
    exit;
  if FMetricsValid then
    exit;

  FBuffer.Free;
  FBuffer := TBitmap.Create;
  FBuffer.SetSize(self.Width, self.Height);

  // *** Tabs ***

  // Cliprect of tabs (containg top frame of component)
{$IFDEF EnhancedRecordSupport}
  FTabClipRect := T2DIntRect.Create(
    ToolbarCornerRadius,
    0,
    self.Width - ToolbarCornerRadius - 1,
    ToolbarTabCaptionsHeight);
{$ELSE}
  FTabClipRect.Create(
    ToolbarCornerRadius,
    0,
    self.Width - ToolbarCornerRadius - 1,
    ToolbarTabCaptionsHeight);
{$ENDIF}

  // Rects of tabs headings (containg top frame of component)
  Setlength(FTabRects, FTabs.Count);
  if FTabs.Count > 0 then
  begin
    x := ToolbarCornerRadius;
    for i := 0 to FTabs.Count - 1 do
      if FTabs[i].Visible then
      begin
        // Loading appearance of tab
        if FTabs[i].OverrideAppearance then
          TabAppearance := FTabs[i].CustomAppearance
        else
          TabAppearance := FAppearance;
        FBuffer.Canvas.font.Assign(TabAppearance.Tab.TabHeaderFont);

        TabWidth := 2 +  // Frame
          2 * TabCornerRadius +
          // Curves
          2 * ToolbarTabCaptionsTextHPadding +
          // Internal margins
          max(ToolbarMinTabCaptionWidth,
          FBuffer.Canvas.TextWidth(FTabs.Items[i].Caption));
        // Breadth of text

        FTabRects[i].Left := x;
        FTabRects[i].Right := x + TabWidth - 1;
        FTabRects[i].Top := 0;
        FTabRects[i].Bottom := ToolbarTabCaptionsHeight;

        x := FTabRects[i].right + 1;
      end
      else
      begin
          {$IFDEF EnhancedRecordSupport}
        FTabRects[i] := T2DIntRect.Create(-1, -1, -1, -1);
          {$ELSE}
        FTabRects[i].Create(-1, -1, -1, -1);
          {$ENDIF}
      end;
  end;

  // *** Panes ***

  if FTabIndex <> -1 then
  begin
    // Rect of tab region
   {$IFDEF EnhancedRecordSupport}
    FTabContentsClipRect := T2DIntRect.Create(ToolbarBorderWidth + TabPaneLeftPadding,
      ToolbarTabCaptionsHeight + ToolbarBorderWidth + TabPaneTopPadding,
      self.Width - 1 - ToolbarBorderWidth - TabPaneRightPadding,
      self.Height - 1 - ToolbarBorderWidth - TabPaneBottomPadding);
   {$ELSE}
    FTabContentsClipRect.Create(ToolbarBorderWidth + TabPaneLeftPadding,
      ToolbarTabCaptionsHeight + ToolbarBorderWidth + TabPaneTopPadding,
      self.Width - 1 - ToolbarBorderWidth - TabPaneRightPadding,
      self.Height - 1 - ToolbarBorderWidth - TabPaneBottomPadding);
   {$ENDIF}

    FTabs[FTabIndex].Rect := FTabContentsClipRect;
  end;

  FMetricsValid := True;
end;

end.
