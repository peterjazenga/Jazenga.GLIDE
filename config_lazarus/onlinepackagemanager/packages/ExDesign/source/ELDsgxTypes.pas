{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxTypes;

{$mode objfpc}{$H+}

interface

uses

  LCLType, LCLIntf, Graphics, ExtCtrls,
  SysUtils, Classes,
  LMessages, Controls, Forms,
  ELDsgxConsts, ELDsgxResources;

const
  MaxPixelCount = 32767;


type
  TplBytes = Pointer;
  IntPtr = Pointer;

type

  PCaptionChar = PChar;
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;



  TCMButtonPressed = packed record
    Msg: Cardinal;
    Index: Integer;     { clx has Index and Control switched }
    Control: TControl;
    Result: Longint;
  end;

  THintString = string;
  THintStringList = TStringList;


  TInputKey = (ikAll, ikArrows, ikChars, ikButton, ikTabs, ikEdit, ikNative{, ikNav, ikEsc});
  TInputKeys = set of TInputKey;

  TplRGBTriple = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;

const
  NullHandle = 0;
  // (rom) deleted fbs constants. They are already in JvConsts.pas.

type
  TTimerProc = procedure(hwnd: THandle; Msg: Cardinal; idEvent: Cardinal; dwTime: Cardinal);

type

  TplPersistent = class(TComponent)
  private
    FOwner: TPersistent;
    function _GetOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); reintroduce; virtual;

    function GetNamePath: string; override;
    property Owner: TPersistent read _GetOwner;
  end;


  TplPropertyChangeEvent = procedure(Sender: TObject; const PropName: string) of object;

  TplPersistentProperty = class(TplPersistent)
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnChangingProperty: TplPropertyChangeEvent;
    FOnChangedProperty: TplPropertyChangeEvent;
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure ChangedProperty(const PropName: string); virtual;
    procedure ChangingProperty(const PropName: string); virtual;
    procedure SetUpdateState(aUpdating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChangedProperty: TplPropertyChangeEvent read FOnChangedProperty write FOnChangedProperty;
    property OnChangingProperty: TplPropertyChangeEvent read FOnChangingProperty write FOnChangingProperty;
  end;

  TplRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers,
    hkPerformanceData, hkCurrentConfig, hkDynData);
  TplRegKeys = set of TplRegKey;

  // base JVCL Exception class to derive from
  EJVCLException = class(Exception);

  TplLinkClickEvent = procedure(Sender: TObject; Link: string) of object;
  //  TOnRegistryChangeKey = procedure(Sender: TObject; RootKey: HKEY; Path: string) of object;
  //  TAngle = 0..360;
  TplOutputMode = (omFile, omStream);
  //  TLabelDirection = (sdLeftToRight, sdRightToLeft); // JvScrollingLabel

  TplDoneFileEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer; Url: string) of object;
  TplDoneStreamEvent = procedure(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string) of object;
  TplHTTPProgressEvent = procedure(Sender: TObject; UserData, Position: Integer; TotalSize: Integer; Url: string; var Continue: Boolean) of object;
  TplFTPProgressEvent = procedure(Sender: TObject; Position: Integer; Url: string) of object;


  TplClipboardCommand = (caCopy, caCut, caPaste, caClear, caUndo);
  TplClipboardCommands = set of TplClipboardCommand;


  TCMForceSize = record
    Msg: Cardinal;
    NewSize: TSmallPoint;
    Sender: TControl;
    Result: Longint;
  end;

  PJvRGBArray = ^TplRGBArray;
  TplRGBArray = array [0..MaxPixelCount] of TplRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0..MaxPixelCount] of TRGBQuad;
  PRGBPalette = ^TRGBPalette;
  TRGBPalette = array [Byte] of TRGBQuad;


  TplErrorEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
  TplWaveLocation = (frFile, frResource, frRAM);

  TplPopupPosition = (ppNone, ppForm, ppApplication);

  TplProgressEvent = procedure(Sender: TObject; Current, Total: Integer) of object;
  TplNextPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TplBitmapStyle = (bsNormal, bsCentered, bsStretched);


  TplGradientStyle = (grFilled, grEllipse, grHorizontal, grVertical, grPyramid, grMount);

  TplParentEvent = procedure(Sender: TObject; ParentWindow: THandle) of object;

  TplDiskRes = (dsSuccess, dsCancel, dsSkipfile, dsError);
  TplDiskStyle = (idfCheckFirst, idfNoBeep, idfNoBrowse, idfNoCompressed, idfNoDetails,
    idfNoForeground, idfNoSkip, idfOemDisk, idfWarnIfSkip);
  TplDiskStyles = set of TplDiskStyle;
  TplDeleteStyle = (idNoBeep, idNoForeground);
  TplDeleteStyles = set of TplDeleteStyle;

  TplNotifyParamsEvent = procedure(Sender: TObject; Params: Pointer) of object;

  TplFileInfoRec = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;

  TplAnimation = (anLeftRight, anRightLeft, anRightAndLeft, anLeftVumeter, anRightVumeter);
  TplAnimations = set of TplAnimation;

  TplTriggerKind =
    (tkOneShot, tkEachSecond, tkEachMinute, tkEachHour, tkEachDay, tkEachMonth, tkEachYear);


  TplFourCC = array [0..3] of AnsiChar;
  PJvAniTag = ^TplAniTag;
  TplAniTag = packed record
    ckID: TplFourCC;
    ckSize: Longint;
  end;

  TplAniHeader = packed record
    dwSizeof: Longint;
    dwFrames: Longint;
    dwSteps: Longint;
    dwCX: Longint;
    dwCY: Longint;
    dwBitCount: Longint;
    dwPlanes: Longint;
    dwJIFRate: Longint;
    dwFlags: Longint;
  end;

  TplChangeColorEvent = procedure(Sender: TObject; Foreground, Background: TColor) of object;

  TplLayout = (lTop, lCenter, lBottom);
  TplBevelStyle = (bsShape, bsLowered, bsRaised);

  {for OnLoseFocus the AFocusControl argument will point at the control that
   receives focus while for OnGetFocus it is the control that lost the focus}
  TplFocusChangeEvent = procedure(const ASender: TObject;
    const AFocusControl: TWinControl) of object;


  TTickCount = Cardinal;

  {**** string handling routines}
  TSetOfChar = TSysCharSet;
  TCharSet = TSysCharSet;

  TDateOrder = (doMDY, doDMY, doYMD);
  TDayOfWeekName = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
  TDaysOfWeek = set of TDayOfWeekName;

const
  DefaultDateOrder = doDMY;

  CenturyOffset: Byte = 60;
  NullDate: TDateTime = 0; {-693594}

type
  // JvDriveCtrls / JvLookOut
  TplImageSize = (isSmall, isLarge);
  TplImageAlign = (iaLeft, iaCentered);

  TplDriveType = (dtUnknown, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TplDriveTypes = set of TplDriveType;

  // Defines how a property (like a HotTrackFont) follows changes in the component's normal Font
  TplTrackFontOption = (
    hoFollowFont,  // makes HotTrackFont follow changes to the normal Font
    hoPreserveCharSet,  // don't change HotTrackFont.Charset
    hoPreserveColor,    // don't change HotTrackFont.Color
    hoPreserveHeight,   // don't change HotTrackFont.Height (affects Size as well)
    hoPreserveName,     // don't change HotTrackFont.Name
    hoPreservePitch,    // don't change HotTrackFont.Pitch
    hoPreserveStyle);   // don't change HotTrackFont.Style
  TplTrackFontOptions = set of TplTrackFontOption;

const
  DefaultTrackFontOptions = [hoFollowFont, hoPreserveColor, hoPreserveStyle];
  DefaultHotTrackColor = $00D2BDB6;
  DefaultHotTrackFrameColor = $006A240A;

type
  // from JvListView.pas
  TplSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TplListViewColumnSortEvent = procedure(Sender: TObject; Column: Integer; var AMethod: TplSortMethod) of object;

  // from JvOfficeColorPanel.pas
  TplAddInControlSiteInfo = record
    AddInControl: TControl;
    BoundsRect: TRect;
    SiteInfoData: TObject;
  end;

  TplClickColorType =
    (cctColors, cctNoneColor, cctDefaultColor, cctCustomColor, cctAddInControl, cctNone);
  TplHoldCustomColorEvent = procedure(Sender: TObject; AColor: TColor) of object;
  TplColorQuadLayOut = (cqlNone, cqlLeft, cqlRight, cqlClient);
  TplGetAddInControlSiteInfoEvent = procedure(Sender: TControl; var ASiteInfo: TplAddInControlSiteInfo) of object;

  // from JvColorProvider.pas
  TColorType = (ctStandard, ctSystem, ctCustom);

  TDefColorItem = record
    Value: TColor;
    Constant: string;
    Description: string;
  end;

const
  ColCount = 20;
  StandardColCount = 40;
  SysColCount = 30;

  ColorValues: array [0 .. ColCount - 1] of TDefColorItem = (
    (Value: clBlack;      Constant: 'clBlack';      Description: RsClBlack),
    (Value: clMaroon;     Constant: 'clMaroon';     Description: RsClMaroon),
    (Value: clGreen;      Constant: 'clGreen';      Description: RsClGreen),
    (Value: clOlive;      Constant: 'clOlive';      Description: RsClOlive),
    (Value: clNavy;       Constant: 'clNavy';       Description: RsClNavy),
    (Value: clPurple;     Constant: 'clPurple';     Description: RsClPurple),
    (Value: clTeal;       Constant: 'clTeal';       Description: RsClTeal),
    (Value: clGray;       Constant: 'clGray';       Description: RsClGray),
    (Value: clSilver;     Constant: 'clSilver';     Description: RsClSilver),
    (Value: clRed;        Constant: 'clRed';        Description: RsClRed),
    (Value: clLime;       Constant: 'clLime';       Description: RsClLime),
    (Value: clYellow;     Constant: 'clYellow';     Description: RsClYellow),
    (Value: clBlue;       Constant: 'clBlue';       Description: RsClBlue),
    (Value: clFuchsia;    Constant: 'clFuchsia';    Description: RsClFuchsia),
    (Value: clAqua;       Constant: 'clAqua';       Description: RsClAqua),
    (Value: clWhite;      Constant: 'clWhite';      Description: RsClWhite),
    (Value: clMoneyGreen; Constant: 'clMoneyGreen'; Description: RsClMoneyGreen),
    (Value: clSkyBlue;    Constant: 'clSkyBlue';    Description: RsClSkyBlue),
    (Value: clCream;      Constant: 'clCream';      Description: RsClCream),
    (Value: clMedGray;    Constant: 'clMedGray';    Description: RsClMedGray)
  );

  //added by dejoy (2005-04-20)
  StandardColorValues: array [0 .. StandardColCount - 1] of TDefColorItem = (
    (Value: $00000000;    Constant: 'clBlack';          Description: RsClBlack),
    (Value: $00003399;    Constant: 'clBrown';          Description: RsClBrown),
    (Value: $00003333;    Constant: 'clOliveGreen';     Description: RsClOliveGreen),
    (Value: $00003300;    Constant: 'clDarkGreen';      Description: RsClDarkGreen),
    (Value: $00663300;    Constant: 'clDarkTeal';       Description: RsClDarkTeal),
    (Value: $00800000;    Constant: 'clDarkBlue';       Description: RsClDarkBlue),
    (Value: $00993333;    Constant: 'clIndigo';         Description: RsClIndigo),
    (Value: $00333333;    Constant: 'clGray80';         Description: RsClGray80),

    (Value: $00000080;    Constant: 'clDarkRed';        Description: RsClDarkRed),
    (Value: $000066FF;    Constant: 'clOrange';         Description: RsClOrange),
    (Value: $00008080;    Constant: 'clDarkYellow';     Description: RsClDarkYellow),
    (Value: $00008000;    Constant: 'clGreen';          Description: RsClGreen),
    (Value: $00808000;    Constant: 'clTeal';           Description: RsClTeal),
    (Value: $00FF0000;    Constant: 'clBlue';           Description: RsClBlue),
    (Value: $00996666;    Constant: 'clBlueGray';       Description: RsClBlueGray),
    (Value: $00808080;    Constant: 'clGray50';         Description: RsClGray50),

    (Value: $000000FF;    Constant: 'clRed';            Description: RsClRed),
    (Value: $000099FF;    Constant: 'clLightOrange';    Description: RsClLightOrange),
    (Value: $0000CC99;    Constant: 'clLime';           Description: RsClLime),
    (Value: $00669933;    Constant: 'clSeaGreen';       Description: RsClSeaGreen),
    (Value: $00999933;    Constant: 'clAqua';           Description: RsClAqua),
    (Value: $00FF6633;    Constant: 'clLightBlue';      Description: RsClLightBlue),
    (Value: $00800080;    Constant: 'clViolet';         Description: RsClViolet),
    (Value: $00999999;    Constant: 'clGray40';         Description: RsClGray40),

    (Value: $00FF00FF;    Constant: 'clPink';           Description: RsClPink),
    (Value: $0000CCFF;    Constant: 'clGold';           Description: RsClGold),
    (Value: $0000FFFF;    Constant: 'clYellow';         Description: RsClYellow),
    (Value: $0000FF00;    Constant: 'clBrightGreen';    Description: RsClBrightGreen),
    (Value: $00FFFF00;    Constant: 'clTurquoise';      Description: RsClTurquoise),
    (Value: $00F0CAA6;    Constant: 'clSkyBlue';        Description: RsClSkyBlue),
    (Value: $00663399;    Constant: 'clPlum';           Description: RsClPlum),
    (Value: $00C0C0C0;    Constant: 'clGray25';         Description: RsClGray25),

    (Value: $00CC99FF;    Constant: 'clRose';           Description: RsClRose),
    (Value: $0099CCFF;    Constant: 'clTan';            Description: RsClTan),
    (Value: $0099FFFF;    Constant: 'clLightYellow';    Description: RsClLightYellow),
    (Value: $00CCFFCC;    Constant: 'clLightGreen';     Description: RsClLightGreen),
    (Value: $00FFFFCC;    Constant: 'clLightTurquoise'; Description: RsClLightTurquoise),
    (Value: $00FFCC99;    Constant: 'clPaleBlue';       Description: RsClPaleBlue),
    (Value: $00FF99CC;    Constant: 'clLavender';       Description: RsClLavender),
    (Value: $00FFFFFF;    Constant: 'clWhite';          Description: RsClWhite)
  );

  SysColorValues: array [0 .. SysColCount - 1] of TDefColorItem = (
    (Value: clScrollBar;           Constant: 'clScrollBar';           Description: RsClScrollBar),
    (Value: clBackground;          Constant: 'clBackground';          Description: RsClBackground),
    (Value: clActiveCaption;       Constant: 'clActiveCaption';       Description: RsClActiveCaption),
    (Value: clInactiveCaption;     Constant: 'clInactiveCaption';     Description: RsClInactiveCaption),
    (Value: clMenu;                Constant: 'clMenu';                Description: RsClMenu),
    (Value: clWindow;              Constant: 'clWindow';              Description: RsClWindow),
    (Value: clWindowFrame;         Constant: 'clWindowFrame';         Description: RsClWindowFrame),
    (Value: clMenuText;            Constant: 'clMenuText';            Description: RsClMenuText),
    (Value: clWindowText;          Constant: 'clWindowText';          Description: RsClWindowText),
    (Value: clCaptionText;         Constant: 'clCaptionText';         Description: RsClCaptionText),
    (Value: clActiveBorder;        Constant: 'clActiveBorder';        Description: RsClActiveBorder),
    (Value: clInactiveBorder;      Constant: 'clInactiveBorder';      Description: RsClInactiveBorder),
    (Value: clAppWorkSpace;        Constant: 'clAppWorkSpace';        Description: RsClAppWorkSpace),
    (Value: clHighlight;           Constant: 'clHighlight';           Description: RsClHighlight),
    (Value: clHighlightText;       Constant: 'clHighlightText';       Description: RsClHighlightText),
    (Value: clBtnFace;             Constant: 'clBtnFace';             Description: RsClBtnFace),
    (Value: clBtnShadow;           Constant: 'clBtnShadow';           Description: RsClBtnShadow),
    (Value: clGrayText;            Constant: 'clGrayText';            Description: RsClGrayText),
    (Value: clBtnText;             Constant: 'clBtnText';             Description: RsClBtnText),
    (Value: clInactiveCaptionText; Constant: 'clInactiveCaptionText'; Description: RsClInactiveCaptionText),
    (Value: clBtnHighlight;        Constant: 'clBtnHighlight';        Description: RsClBtnHighlight),
    (Value: cl3DDkShadow;          Constant: 'cl3DDkShadow';          Description: RsCl3DDkShadow),
    (Value: cl3DLight;             Constant: 'cl3DLight';             Description: RsCl3DLight),
    (Value: clInfoText;            Constant: 'clInfoText';            Description: RsClInfoText),
    (Value: clInfoBk;              Constant: 'clInfoBk';              Description: RsClInfoBk),

    (Value: clGradientActiveCaption;   Constant: 'clGradientActiveCaption';  Description: RsGradientActiveCaption),
    (Value: clGradientInactiveCaption; Constant: 'clGradientInactiveCaption';Description: RsGradientInactiveCaption),
    (Value: clHotLight;                Constant: 'clHotLight';               Description: RsHotLight),
    (Value: clMenuBar;                 Constant: 'clMenuBar';                Description: RsMenuBar),
    (Value: clMenuHighlight;           Constant: 'clMenuHighlight';          Description: RsMenuHighlight)
  );

type
  TplSizeRect = packed record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TplMessage = packed record
    Msg: Integer;
    case Integer of
    0:
     (
      WParam: Integer;
      LParam: Integer;
      Result: Integer;
     );
    1:
     (
      WParamLo: Word;
      WParamHi: Word;
      LParamLo: Word;
      LParamHi: Word;
      ResultLo: Word;
      ResultHi: Word;
     );
    2:
     ( // WM_NOPARAMS
      Unused: array[0..3] of Word;
      Handled: LongBool;  // "Result"
     );
    3:
     ( // WM_SCROLL
      Pos: Integer;         // WParam
      ScrollCode: Integer;  // LParam
     );
    4:
     ( // WM_TIMER
      TimerID: Integer;     // WParam
      TimerProc: TTimerProc;// LParam
     );
    5:
     ( // WM_MOUSEACTIVATE
      TopLevel: HWND;       // WParam
      HitTestCode: Word;    // LParamLo
      MouseMsg: Word;       // LParamHi
     );
    6:
     ( // WM_MOUSE(WHEEL) | WM_MOVE
      case Integer of
      0:
       ( // WM_MOUSE
        Keys: Integer;     // WParam
        // LParam: Pos | (XPos, YPos)
        case Integer of
        0:
         (
          Position: TSmallPoint;
         );
        1:
         (
          XPos: Smallint;
          YPos: Smallint;
         )
       );
      1:
       ( // WM_MOUSEWHEEL
        WheelDelta: Integer; // WParam
       );
     );
    7:
     ( // WM_ACTIVATE
      Active: Word; { WA_INACTIVE, WA_ACTIVE, WA_CLICKACTIVE } // WParamLo
      Minimized: WordBool;  // WParamHi
      ActiveWindow: HWND;   // LParam
     );

    8:
     ( // WM_COMMAND
      ItemID: Word;         // WParamLo
      NotifyCode: Word;     // WParamHi
      Ctl: HWND;            // LParam
     );
    9:
     ( // WM_GETICON
      BigIcon: LongBool;
     );
    10:
     ( // CM_(FOCUS|CONTROL)CHANGED  | CM_HINTSHOW
      Reserved: Integer;      // WParam
      case Integer of
        0:
         ( // CM_(CONTROL)CHANGED
          Child: TControl;    // LParam
         );
        1:
         ( // CM_FOCUSCHANGED | CM_FORCESIZE }
          Sender: TControl;   // LParam
         );
        2:
         ( //CM_HINTSHOW
          HintInfo: PHintInfo;
         )
     );
    11:
     ( // CM_CONTROLLISTCHANGE | CM_(CONTROL)CHANGED (| CM_BUTTONPRESSED for clx)
      Control: TControl;    // WParam
      case Integer of
        0:
         ( // CM_(CONTROL)CHANGED
          Inserting: LongBool;    // LParam
         );
        1: // CM_BUTTONPRESSED (clx)
         (
          Index: Integer;
         )
     );
    12:
     ( // CM_HINTSHOWPAUSE
      WasActive: LongBool;
      Pause: PInteger;
     );
    13:
     ( // WM_KEY
      CharCode: Word;
      NotUsed: Word;
      KeyData: Integer;
     );
    14:
     ( // WM_GETTEXT
      TextMax: Integer;
      Text: PChar
     );
    15:
     ( // WM_ERASEBKGND | WM_PAINT
      DC: HDC;
     );
    16:
     ( // WM_KILLFOCUS
      FocusedWnd: HWND;
     );
    17:
     (
      NewSize: TSmallPoint; //CM_FORCESIZE wParam
     );
    18:
     ( { alternative naming for VCL CM_BUTTONPRESSED }
      GroupIndex: Integer;
      Button: TControl;
     );
  end;


implementation

{ TplPersistent }
constructor TplPersistent.Create(AOwner: TPersistent);
begin
  if AOwner is TComponent then
    inherited Create(AOwner as TComponent)
  else
    inherited Create(nil);
  SetSubComponent(True);

  FOwner := AOwner;
end;

type
  TPersistentAccessProtected = class(TPersistent);

function TplPersistent.GetNamePath: string;
var
  S: string;
  lOwner: TPersistent;
begin
  Result := inherited GetNamePath;
  lOwner := GetOwner;   //Resturn Nested NamePath
  if (lOwner <> nil)
    and ( (csSubComponent in TComponent(lOwner).ComponentStyle)
         or (TPersistentAccessProtected(lOwner).GetOwner <> nil)
        )
   then
  begin
    S := lOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TplPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TplPersistent._GetOwner: TPersistent;
begin
  Result := GetOwner;
end;

{ TplPersistentProperty }

procedure TplPersistentProperty.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TplPersistentProperty.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TplPersistentProperty.ChangedProperty(const PropName: string);
begin
  if Assigned(FOnChangedProperty) then
    FOnChangedProperty(Self, PropName);
end;

procedure TplPersistentProperty.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TplPersistentProperty.ChangingProperty(const PropName: string);
begin
  if Assigned(FOnChangingProperty) then
    FOnChangingProperty(Self, PropName);
end;

procedure TplPersistentProperty.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TplPersistentProperty.SetUpdateState(aUpdating: Boolean);
begin
  if aUpdating then
    Changing
  else
    Changed;
end;

end.

