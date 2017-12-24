
{**********************************************************************
 Package pl_ExControls.pkg
 From PilotLogic Software House(http://www.pilotlogic.com/)
 This unit is part of CodeTyphon Studio
***********************************************************************}

unit TplSpiderGraphUnit;

interface

uses
  Messages, SysUtils, Classes, Controls,
  StdCtrls, Graphics, LCLtype, Types, Math, LCLIntf,
  Forms;

const
  GRADIENT_FILL_RECT_V = 1;
  GRADIENT_FILL_RECT_H = 2;

type

  TQSummitArray = array of TPoint;
  TQSingleArray = array of single;
  TQBackGround = (bgTransparent, bgColored, bgTopBottom, bgBottomTop, bgLeftToRight, bgRightToLeft);
  TQTitlePosition = (qtpTop, qtpBottom);
  TQBorderStyle = (bsNone, bsFlat, bs3D);
  TQHighlightMode = (hmShowPoints, hmWidened, hmColorLine, hmFlashLine);


  GRADIENT_RECT = record
    UpperLeft: cardinal;
    LowerRight: cardinal;
  end;

  TRIVERTEX = packed record
    X, Y: DWORD;
    Red, Green, Blue, Alpha: word;
  end;

  TQLinesInfoBox = record
    box: TStaticText;
    defined: boolean;
  end;

  TQMouseLineEvt = procedure(Sender: TObject; const lineIndex: integer) of object;

  TplSpiderGraph = class;                                   // Forward class declaration, needed by both TQSpiderAxe & TQSGLine.

  TQSpiderAxe = class                                       // Axes definition
  private
    angle,                                                  // angle of this axe compared to zero
    fMin,                                                   // axes min ...
    fMax: single;                                  // ... and max values;
    fIndex: integer;                                 // index of this axe in the SpiderGraph instance axes array.
    fAxeOwner: TplSpiderGraph;                           // The graph upon which this axe will show.
    fAutoSizeMin,                                           // shall min and max's values be computed ...
    fAutoSizeMax: boolean;                                 // ... internally or will they be provided ?
    procedure SetAutoSizeMin(Value: boolean);
    procedure SetAutoSizeMax(Value: boolean);
    procedure SetAutoSzBoth(Value: boolean);
    function GetAutoSzBoth: boolean;
    procedure SetMin(Value: single);
    procedure SetMax(Value: single);
    constructor CreateOwned(anOwner: TplSpiderGraph);
  public
    Caption: string;
    destructor Destroy; override;
    property autoSizeMin: boolean read fAutoSizeMin write SetAutoSizeMin;
    property autoSizeMax: boolean read fAutoSizeMax write SetAutoSizeMax;
    property autoSized: boolean read GetAutoSzBoth write SetAutoSzBoth;
    property MinValue: single read fMin write SetMin;
    property MaxValue: single read fMax write SetMax;
  end;


  TQSGLine = class                                          // Lines descriptor
  private
    fLnOwner: TplSpiderGraph;                            // The graph upon which this line will show.
    fValues: TQSingleArray;                            // Line values {Array of Single}
    fPoints: array of TPoint;                          // Coordinates of each value
    fHasValues: boolean;                                  // False until values are sent
    fPenWidthMem,                                           // Storage needed to reset the boolean below (after highlighting)
    fPenWidth: word;                                     // Default pen width for this line.
    fShow: boolean;                                  // If false, this line won't be drawn.
    fShowPointsMem,                                         // Storage needed to reset the boolean below (after highlighting)
    fShowPoints: boolean;                                  // In normal state, or when highlighted
    fColorMem,                                              // Like above. The line color, when not highlighted
    fColor: TColor;                                   // The line color to be drawn.
    constructor CreateOwned(anOwner: TplSpiderGraph);
    procedure SetPenWidth(Value: word);
    procedure SetShowPoints(Value: boolean);
    procedure SetValues(const vals: TQSingleArray);
    procedure SetColor(Value: TColor);
  public
    Caption: string;                                   // Line caption
    property color: TColor read fColor write SetColor;
    property penWidth: word read fPenWidth write SetPenWidth;
    property values: TQSingleArray read fValues write SetValues;
    property showPoints: boolean read fShowPoints write SetShowPoints;
    property Visible: boolean read fShow write fShow;
    procedure Hide;
    procedure Show;
    destructor Destroy; override;
  end;


  TplSpiderGraph = class(TCustomControl)
  private
    fBackGround: TQBackGround;                          // transparent, colored, gradient, ...
    fBackgroundColor,                                       // background color, if single color
    fBackGStartColor,                                       // gradient, if background shows a gradient
    fBackGFinalColor: TColor;                              // gradient, if background shows a gradient
    fUsfHeight,                                             // width and height of the component, without the border
    fUsfWidth: integer;
    fBorderStyle: TQBorderStyle;                         // component's border : none, flat or 3DLight
    fBorderSize: integer;                               // internal mem of its size (0<size<2), depending on above
    fGraphRect: TRect;                                 // the rectangle where the graph itself will be drawn
    fGraphCenter: TPoint;                                // the graph center's coordinates
    fAxes: array of TQSpiderAxe;                  // Axes of the graph ;
    fAxesCount: integer;                               // Number of axes; At least 3
    fAxesLen: integer;                               // physical length of axes, in bytes
    fAxesMMdefined: boolean;                              // Each axe max and min, to be recomputed or not
    fAxesColor: TColor;                                // Axes and their polygon border color;
    fAxesAutoSized: boolean;                               // See setter (SetAxesSizeState();) for more...
    fSummits: TQSummitArray;                         // The axes summits(+1), for polyline() ;
    fSummitsPolygon: TQSummitArray;                         // The axes summits, for polygone() ;
    fAngle: single;                                // Angle between two axes;
    fAxesCapFramed: boolean;                               // Angle between top and an axe ; Depends upon the number of axes...
    fPolygonFill: boolean;                               // Fill axes polygon area ?
    fPolygonColor: TColor;                                // If yes, with what color ?
    fTitleCaption: string;                                // Title to dispaly
    fTitlePosition: TQTitlePosition;                      // At top or bottom
    fTitleRect: TRect;                                 // Needed to place the graph rect
    fLines: array of TQSGLine;                     // Lines values collection
    fLinesCount: integer;                               // Logically, at least 1;
    fMultiLines: boolean;                               // := fLineCount > 1;
    fDfltPenWidth: word;                                  // Lines width;
    fShowLnPoints: boolean;                               // Lines points. USed to setup new lines.
    fHighlightColor: TColor;                                // Color to be used to highlight line(s)
    fLinesCpnBmp: TBitmap;                               // The box where lines captions and their colors will be drawn
    fShowLinesCpn: boolean;                               // Is this box to be drawn ?
    fLinesCpnBmpdef: boolean;
    // Forces re-drawing of the bitmap above each time something in the layout changes
    fLinesCpnBmpTrp: boolean;                               // Lines captions box background transparent or not
    fLinesCpnBmpClr: TColor;                                // Lines captions box backgroud color, if not transparent

    fLinesInfoBoxes: array of TQLinesInfoBox;               // The box showing lines values, if tracking mouseMoves
    fMBoxParent: TWinControl;                           // Will be used to draw the mouse info boxes on the graph or on the whole form
    fShowMouseBox: boolean;                               // Shall the popup box (above) be shown ?
    fMBoxBackColor: TColor;                                // Back color is shared by all these info boxes;
    fMBoxForColor: TColor;                                // Possibly common to all these info boxes ... or ignored (see below);
    fMBoxUsesLnClr: boolean;                               // If True, fMBoxForColor above will be = to it's line's color;

    fTrackMsMoves: boolean;                               // Mouse moves global interruptor
    fMouseOnLine: boolean;                               // Memory witness to know if things have changed between two moves
    fMouseLineIndex: integer;                               // The index of the line the mouse is over
    fMouseEnterLine: TQMouseLineEvt;                        // Event to be raised
    fMouseExitLine: TQMouseLineEvt;                        // Event to be raised

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure InitSummits;
    procedure DefineDrawSpace;
    function GetLinePoint(iMin, iMax, position, iAngle: single): TPoint;
    function ResizeRect(aRect: Trect; hm: integer): Trect;
    procedure SetBorderStyle(Value: TQBorderStyle);
    procedure SetBackGround(Value: TQBackGround);
    procedure SetBackGroundColor(Value: TColor);
    procedure SetBackGStartColor(Value: TColor);
    procedure SetBackGFinalColor(Value: TColor);
    procedure SetAxesColor(Value: TColor);
    procedure SetAxesCount(Value: integer);
    procedure SetAxesFramed(Value: boolean);
    procedure SetAxesSizeState(Value: boolean);
    procedure SetPolygonFill(Value: boolean);
    procedure SetPolygonColor(Value: TColor);
    function GetMinValue(axeIx: integer; var minVal: single; cnd: boolean = False): TIntegerDynArray;
    function GetMaxValue(axeIx: integer; var maxVal: single; cnd: boolean = False): TIntegerDynArray;
    procedure SetTitleCaption(Value: string);
    procedure SetTitlePosition(Value: TQTitlePosition);
    function GetAxesByIndex(ix: integer): TQSpiderAxe;
    function GetCaptionRect(aString: string; aCanvas: TCanvas; anAngle: single;
                            destinationRect: TRect; inflateBy, Xbase, Ybase: integer): TRect;
    procedure DefineLinesInfoBox(lineIx: integer);
    function GetLinesCaptionHeight: integer;
    procedure SetLinesCount(Value: integer);
    function GetLinesByIndex(ix: integer): TQSGLine;
    procedure FlashLine(targets: TIntegerDynArray);
    procedure SetDfltPenWidth(Value: word);
    procedure SetShowMouseBox(Value: boolean);
    procedure SetMBoxForColor(Value: TColor);
    procedure SetMBoxBackColor(Value: TColor);
    procedure SetMBoxUsesLnClr(Value: boolean);
    procedure SetShowLnPoints(Value: boolean);
    procedure SetTrackMsMoves(Value: boolean);
    procedure SetShowLinesCpn(Value: boolean);
    procedure SetLinesCpnBmpTrp(Value: boolean);
    procedure SetLinesCpnBmpClr(Value: TColor);
    procedure ResetLinesAppearance;
    procedure MouseTrack(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure SetMBoxParent(Value: TWinControl);

  public
    axesFont: TFont;
    titleFont: TFont;
    linesCpnFont: TFont;
    highlightMode: set of TQHighlightMode;

    property axes[ix: integer]: TQSpiderAxe read GetAxesByIndex;
    property linesCount: integer read fLinesCount write SetLinesCount;
    property line: TQSGLine  Index 0 read GetLinesByIndex;
    property Lines[ix: integer]: TQSGLine read GetLinesByIndex;
    property mBoxParent: TWinControl read fMBoxParent write SetMBoxParent;

    function GetBestLineByArea(maxWanted: boolean = True): TIntegerDynArray;
    function GetBestLineByAxe(axeIx: integer; maxWanted: boolean = True): TIntegerDynArray;
    procedure HighlightLineByCrit(criteria: integer; maxWanted: boolean = True);
    procedure HighlightLineByIndex(indexArray: TIntegerDynArray); overload;
    procedure HighlightLineByIndex(index: integer); overload;
    function AddLine(const vals: TQSingleArray): integer;
    function RemoveLine(lineIndex: integer): boolean;
    function Save2File(fileName: TFileName; asBMP: boolean = True): boolean;
    function Copy2Bitmap(var aBitmap: TBitmap): boolean;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Anchors;
    property polygonFill: boolean read fPolygonFill write SetPolygonFill;
    property polygonColor: TColor read fPolygonColor write SetPolygonColor;
    property axesColor: TColor read fAxesColor write SetAxesColor;
    property axesCount: integer read fAxesCount write SetAxesCount default 7;
    property axesCaptionsFramed: boolean read fAxesCapFramed write SetAxesFramed;
    property axesAutoSized: boolean read fAxesAutoSized write SetAxesSizeState;
    property borderStyle: TQBorderStyle read fBorderStyle write SetBorderStyle;
    property backGround: TQBackGround read fBackGround write SetBackGround;
    property backGroundColor: TColor read fBackgroundColor write SetBackgroundColor;
    property backGStartColor: TColor read fBackGStartColor write SetBackGStartColor;
    property backGFinalColor: TColor read fBackGFinalColor write SetBackGFinalColor;
    property titleCaption: string read fTitleCaption write SetTitleCaption;
    property titlePosition: TQTitlePosition read fTitlePosition write SetTitlePosition;
    property defaultPenWidth: word read fDfltPenWidth write SetDfltPenWidth;
    property showMouseBox: boolean read fShowMouseBox write SetShowMouseBox;
    property mBoxForColor: TColor read fMBoxForColor write SetMBoxForColor;
    property mBoxBackColor: TColor read fMBoxBackColor write SetMBoxBackColor;
    property mBoxUsesLnColor: boolean read fMBoxUsesLnClr write SetMBoxUsesLnClr;
    property showLinesPoints: boolean write SetShowLnPoints;
    property showLinesCaption: boolean read fShowLinesCpn write SetShowLinesCpn;
    property linesBoxTransparent: boolean read fLinesCpnBmpTrp write SetLinesCpnBmpTrp;
    property linesBoxColor: TColor read fLinesCpnBmpClr write SetLinesCpnBmpClr;
    property highlightColor: TColor read fHighlightColor write fHighlightColor;
    property trackMouseMoves: boolean read fTrackMsMoves write SetTrackMsMoves;
    property OnMouseEnterLine: TQMouseLineEvt read fMouseEnterLine write fMouseEnterLine;
    property OnMouseExitLine: TQMouseLineEvt read fMouseExitLine write fMouseExitLine;
  end;

function GradientFill(aCanvas: TCanvas; pVertex: Pointer; dwNumVertex: DWORD; pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWord;

const
  HC_NONE = -1;
  HC_AREA = -2;

implementation

function GradientFill(aCanvas: TCanvas; pVertex: Pointer; dwNumVertex: DWORD; pMesh: Pointer; dwNumMesh, dwMode: DWORD): DWord;
begin
  //Nothing;
end;

//====================== TQSpiderAxe =====================================
constructor TQSpiderAxe.CreateOwned(anOwner: TplSpiderGraph);
begin
  inherited Create;
  fAxeOwner := anOwner;
  fAutoSizeMin := anOwner.fAxesAutoSized;
  fAutoSizeMax := anOwner.fAxesAutoSized;
  fIndex := -1;
  fMax := 100;
  fMin := 0.0;
end;

destructor TQSpiderAxe.Destroy;
begin
  fAxeOwner := nil;
  inherited;
end;

// Axes autoSize min and max general behaviour :

// If set to False : Nothing is touched. This axe's min wil stay what it
//   is at that moment (a value you did set, the default one, or the last one
//   computed. However, this value may change if you send another line later,
//   with values below the actual axe's min (or obviously, if fMin is
//   explicitely set Through "axes[i].minValue := ").
// If set to True : fMin will be computed now, and each time a new line is
//   added to the graph.
//   min & max are then set to 1/twentiest above and bellow the actual range of this axe.

// note : Most of these properties rely on the graph's .GetMinValue(); function
//   to retrieve the actual min or max value of an axe.
//   GetMinValue(); return the min if at least one line has values, or the actual
//   fMin property of this axe otherwise.


procedure TQSpiderAxe.SetAutoSizeMin(Value: boolean);
var
  highestVal, lowestVal: single;
begin
  self.fAutoSizeMin := Value;
  if Value then
  begin
    fAxeOwner.GetMinValue(self.fIndex, lowestVal, False);
    fAxeOwner.GetMaxValue(self.fIndex, highestVal, False);
    if (lowestVal = 0.0) and (highestVal = 0.0) then
      self.fMin := -1
    else
      self.fMin := lowestVal - ((Abs(highestVal) + Abs(lowestVal)) / 20);
  end;
end;

procedure TQSpiderAxe.SetAutoSizeMax(Value: boolean);
var
  highestVal, lowestVal: single;
begin
  self.fAutoSizeMax := Value;
  if Value then
  begin
    fAxeOwner.GetMinValue(self.fIndex, lowestVal, False);
    fAxeOwner.GetMaxValue(self.fIndex, highestVal, False);
    if (lowestVal = 0.0) and (highestVal = 0.0) then
      self.fMax := 1
    else
      self.fMax := highestVal + ((Abs(highestVal) + Abs(lowestVal)) / 20);
  end;
end;

function TQSpiderAxe.GetAutoSzBoth: boolean;
  // Care ! Result is True if at least one of the two possibilities is True,
  // in opposite to the "both" setter. To explicitely check wether BOTH are
  // True, both values have to be read directly :
  // (If (myGraph.Axe[i].autoSizeMin) AND (myGraph.Axe[i].autoSizeMax) Then ...)
begin
  Result := (fAutoSizeMin or fAutoSizeMax);
end;

procedure TQSpiderAxe.SetAutoSzBoth(Value: boolean);
begin
  self.SetAutoSizeMin(Value);
  self.SetAutoSizeMax(Value);
end;

// The two following properties are "requests", not "demands" : If you try to set
// a "min" above the actual lowest lines value for this axe, or siblingly try to set
// a "max" bellow the actual highest lines value for this axe, the request will be
// ignored.

procedure TQSpiderAxe.SetMin(Value: single);
var
  lowVal: single;
begin
  lowVal := Value;
  fAutoSizeMin := False;
  fAxeOwner.GetMinValue(self.fIndex, lowVal, True);
  if Value <= lowVal then
    self.fMin := Value;
end;

procedure TQSpiderAxe.SetMax(Value: single);
var
  highVal: single;
begin
  highVal := Value;
  fAutoSizeMax := False;
  fAxeOwner.GetMaxValue(self.fIndex, highVal, True);
  if Value >= highVal then
    self.fMax := Value;
end;

//======================== TQSGLine =================================================
// Sizes the two internal arrays. anOwner will allow accesses
// to some needed owner's values, like axesCount, linesCount, aso;
constructor TQSGLine.CreateOwned(anOwner: TplSpiderGraph);
begin
  inherited Create;
  fLnOwner := anOwner;
  SetLength(fValues, fLnOwner.fAxesCount);
  SetLength(fPoints, fLnOwner.fAxesCount + 1);                    // Prepares polyline (ie. Polygon not filled)
  fHasValues := False;
  Caption := ' ';
  Color := clRed;                                           // Red is the default color for lines ;
  fPenWidth := flnOwner.defaultPenWidth;
  fPenWidthMem := flnOwner.defaultPenWidth;
  fShowPoints := flnOwner.fShowLnPoints;
  fShow := True;
  fShowPointsMem := flnOwner.fShowLnPoints;
  fLnOwner.fLinesCpnBmpdef := False;                              // tells owner that the box showing lines caption and ...
end;                                                              // ... the corresponding colors has to be copmputed again.

destructor TQSGLine.Destroy;
begin
  if Assigned(fLnOwner) then
    fLnOwner.fLinesCpnBmpdef := False;
  SetLength(fValues, 0);
  SetLength(fPoints, 0);
  fLnOwner := nil;
  Caption := '';
  inherited;
end;

procedure TQSGLine.SetPenWidth(Value: word);
begin
  if (Value < 1) then
    Value := 1;
  self.fPenWidth := Value;
  self.fPenWidthMem := Value;
end;

procedure TQSGLine.SetValues(const vals: TQSingleArray);
// Accepts less values than axes       -> the lasts ones will default to Axe[i].min ;
// Don't accept  more values than axes -> the lasts ones provided (exeeding axeCount) will be ignored ;
// Moreover, if values sent are bellow the corresponding axe current value,
// this axe's min will be reseted to this new min; Siblingly for Max.
// If an axe has its autosize property to true, a boolean will be set,
// to force the recomputing before next Paint;
var
  i, loopTmp: integer;
  minVal, maxVal: single;
begin
  // Which is the smallest : [vals]'s length or fAxesCount ?
  loopTmp := Min(fLnOwner.fAxesCount, Length(vals));

  // values storage, and control of max and min.
  for i := 0 to loopTmp - 1 do
  begin
    self.fValues[i] := vals[i];
    minVal := vals[i];
    maxVal := vals[i];
    fLnOwner.GetMinValue(i, minVal, fLnOwner.fAxes[i].autoSizeMin);                  // Get the min
    fLnOwner.GetMaxValue(i, maxVal, fLnOwner.fAxes[i].autoSizeMax);                  // and the max

    if (vals[i] <= minVal) then
      fLnOwner.fAxes[i].fMin := vals[i];
    if (vals[i] >= maxVal) then
      fLnOwner.fAxes[i].fMax := vals[i];
    // If needed, forces recomputing of max and min values;
    if fLnOwner.fAxes[i].autoSized then
      fLnOwner.fAxesMMdefined := False;
  end;

  // Finalization : If some axes values haven't been provided, set them to the
  // lowest value of the corresponding axes...
  for i := loopTmp to (fLnOwner.fAxesCount - 1) do
    self.fValues[i] := fLnOwner.fAxes[i].fMin;
  self.fHasValues := True;
end;

procedure TQSGLine.SetShowPoints(Value: boolean);
begin
  self.fShowPoints := Value;
  self.fShowPointsMem := Value;
end;

procedure TQSGLine.SetColor(Value: TColor);
// Filters received color to ensure that the color finally used by the system
// is the same than the one stored by the component : In some cases the system
// may use a color "close to" the one requested, instead of the exact one
// requested. But the graph uses the color below the pointer to detect mouse
// moves over a line, comparing the one read with the ones stored, so that we
// need to make sure they do correspond.
var
  r, g, b: byte;
begin
  // don't know if the conversion to R,G,B and back is usefull or not ...
  r := Red(Value);
  g := Green(Value);
  b := Blue(Value);

  //=====================================================================================
  //Self.fColor    := GetNearestColor(self.fLnOwner.Canvas.Handle, RGBtoColor(r,g,b));
  //=====================================================================================

  Self.fColor := RGBtoColor(r, g, b);
  self.fColorMem := Self.fColor;
end;

procedure TQSGLine.Hide;
begin
  self.fShow := False;
  self.fLnOwner.Invalidate;
end;

procedure TQSGLine.Show;
begin
  self.fShow := True;
  self.fLnOwner.Invalidate;
end;

//======================= TplSpiderGraph ================================================================

procedure TplSpiderGraph.MouseTrack(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  i, lnIx: integer;
  xP, yP: integer;      // X and Y of the parent
  aString: string;
begin
  // Ignores mouse moves outside the graphRect area (title, line captions box)
  // or if the graph shall not track mouse moves
  if not (fTrackMsMoves) or (Y > self.fGraphRect.Bottom - 2) or (Y < self.fGraphRect.top + 2) then
    Exit;

  // If the mouse info boxe has to be drawn outside the bounds of the
  // graph itself, X and Y have to be related to the whole form.
  // ( fMBoxParent can be Self or the graph's parent form)
  if Self.fMBoxParent = Self then
  begin
    xP := 0;
    yP := 0;
  end
  else
  begin
    xP := self.Left;
    yP := self.Top;
  end;

  lnIx := -1;
  aString := '';
  for i := 0 to fLinesCount - 1 do
    if ((Canvas.Pixels[X, Y] = fLines[i].color) or (Canvas.Pixels[X + 1, Y] = fLines[i].color) or
      (Canvas.Pixels[X - 1, Y] = fLines[i].color) or (Canvas.Pixels[X, Y + 1] = fLines[i].color) or
      (Canvas.Pixels[X, Y - 1] = fLines[i].color)) and fLines[i].fHasValues then
      lnIx := i;

  // Here above, the checking of "fLines[i].fHasValues" is needed because the loop returns the last "i" whose
  // line's color equals the color below the mouse. But if the line count has been set to more lines
  // than needed, the exceeding ones have been created with clRed as color (default one). and if the line
  // actually bellow the mouse is red too, then the loop will return the last "i" instead of the first one.
  // Checking fHasValues avoids that (a "break" could have do the trick too)

  // -1- If mouse isn't on a line, was it above before ? If yes, we've just "exited" one line.
  if lnIx < 0 then
  begin
    if (fMouseOnLine) then                                  // exiting a line
    begin
      fMouseOnLine := False;
      fLinesInfoBoxes[fMouseLineIndex].box.Hide;
      Invalidate;
      if Assigned(fMouseExitLine) then
        fMouseExitLine(Self, fMouseLineIndex);
      fMouseLineIndex := lnIx;
    end;
    // -2- Otherwise, mouse is actually upon a line. Question becomes :
    //     Was it on a line before ? If no, we've just entered a line ;
    //     If yes, was it upon another line (implying to first raise a mouseExit before the new mouseEnter) ?
  end
  else if (fMouseOnLine) and (lnIx <> fMouseLineIndex) then
  begin
    fLinesInfoBoxes[fMouseLineIndex].box.Hide;
    Invalidate;
    if Assigned(fMouseExitLine) then
      fMouseExitLine(Self, fMouseLineIndex);
    if (fShowMouseBox) then
    begin
      if not (fLinesInfoBoxes[lnIx].defined) then
        DefineLinesInfoBox(lnIx);
      fLinesInfoBoxes[lnIx].box.Top := Y + yP;
      fLinesInfoBoxes[lnIx].box.Left := X + xP + 15;
      fLinesInfoBoxes[lnIx].box.Show;
    end;
    fMouseLineIndex := lnIx;
    if Assigned(fMouseEnterLine) then
      fMouseEnterLine(Self, fMouseLineIndex);
  end
  else
  begin
    fLinesInfoBoxes[lnIx].box.Top := Y + yP;
    fLinesInfoBoxes[lnIx].box.Left := X + xP + 15;
    if not (fMouseOnLine) then
    begin
      if (fShowMouseBox) then
      begin
        if not (fLinesInfoBoxes[lnIx].defined) then
          DefineLinesInfoBox(lnIx);
        fLinesInfoBoxes[lnIx].box.Show;
      end;
      fMouseOnLine := True;
      fMouseLineIndex := lnIx;
      if Assigned(fMouseEnterLine) then
        fMouseEnterLine(Self, fMouseLineIndex);
    end;
  end;
end;


function TplSpiderGraph.ResizeRect(aRect: Trect; hm: integer): Trect;
  // aRect : The rect to be resized; hm : "How much ?".
  // returns aRect decreased by hm in any direction (or inflated, if hm < 0)
begin
  Result.Left := aRect.Left + hm;
  Result.Right := aRect.Right - hm;
  Result.Top := aRect.Top + hm;
  Result.Bottom := aRect.Bottom - hm;
end;


function TplSpiderGraph.GetLinesCaptionHeight: integer;
  // Builds fLinesCpnBmp, the TBitmap showing lines captions + color
  // and returns its height;
var
  maxLineLen, maxWidth, iTmp, i, j, txtWidth, lineCnt, lineHgt, lineLen: integer;
  xClr, yClr, xCap, yCap, linePtr: integer;
  arCpnDescr: array of integer;                          // will store the number of captions to draw, line by line

begin
  Result := 0;
  if (fLinesCount = 0) or (not (fLines[0].fHasValues)) then
    Exit;
  try
    if assigned(fLinesCpnBmp) and (fLinesCpnBmpdef) then
    begin
      Result := fLinesCpnBmp.Height;
      Exit;
    end
    else
    begin
      if Assigned(fLinesCpnBmp) then
        fLinesCpnBmp.Free;
      fLinesCpnBmp := TBitmap.Create;
      SetLength(arCpnDescr, 1);

      fLinesCpnBmp.Canvas.Font := linesCpnFont;
      fLinesCpnBmp.Transparent := fLinesCpnBmpTrp;
      fLinesCpnBmp.TransparentColor := fLinesCpnBmpClr;
      fLinesCpnBmp.TransparentMode := tmFixed;
      fLinesCpnBmp.Canvas.Brush.Color := clGray;
      fLinesCpnBmp.canvas.FrameRect(Rect(1, 1, fLinesCpnBmp.Width - 2, fLinesCpnBmp.Height - 2));
      fLinesCpnBmp.Canvas.Brush.Color := fLinesCpnBmpClr;

      maxLineLen := 0;
      maxWidth := Self.Width - fBorderSize - 4;       // -4 preserves 2 pixels on both sides
      lineCnt := 1;
      arCpnDescr[0] := 0;
      lineHgt := fLinesCpnBmp.Canvas.TextHeight('Ip') + 2;
      lineLen := 0;

      // -1- Counting lines
      for i := 0 to self.fLinesCount - 1 do
      begin
        txtWidth := fLinesCpnBmp.canvas.TextWidth(fLines[i].Caption) + 28; // the colored rectangle, some space and the caption
        iTmp := lineLen + txtWidth;
        if iTmp <= maxWidth then
        begin
          lineLen := iTmp;
          arCpnDescr[lineCnt - 1] := arCpnDescr[lineCnt - 1] + 1;
          if lineLen > maxLineLen then
            maxLineLen := lineLen;
        end
        else
        begin
          Inc(lineCnt);
          SetLength(arCpnDescr, lineCnt);
          lineLen := txtWidth;
          arCpnDescr[lineCnt - 1] := 1;                  // new line to display, and so, at least one caption (the current one)
        end;
      end;

      // -2- Drawing
      fLinesCpnBmp.Height := lineHgt * lineCnt + 3;
      fLinesCpnBmp.Width := maxLineLen;

      xClr := 12;
      yClr := ((lineHgt - 5) div 2) + 1;
      xCap := 25;
      yCap := 2;
      linePtr := 0;

      for i := 0 to Length(arCpnDescr) - 1 do
      begin
        for j := 0 to arCpnDescr[i] - 1 do
        begin
          with fLinesCpnBmp do
          begin
            Canvas.pen.Color := clGray;
            Canvas.Brush.Color := fLines[linePtr].Color;
            Canvas.Rectangle(xClr, yClr, xClr + 10, yClr + 5);
            Canvas.Brush.Color := fLinesCpnBmpClr;
            Canvas.Pen.Color := clBlack;
            Canvas.TextOut(xCap, yCap, fLines[linePtr].Caption);
          end;
          xClr := xClr + fLinesCpnBmp.canvas.TextWidth(fLines[linePtr].Caption) + 28;
          xCap := xCap + fLinesCpnBmp.canvas.TextWidth(fLines[linePtr].Caption) + 28;
          Inc(linePtr);
        end;
        xClr := 12;
        yClr := yClr + lineHgt;
        xCap := 25;
        yCap := yCap + lineHgt;
      end;
      fLinesCpnBmp.Canvas.Brush.Color := clGray;
      fLinesCpnBmp.canvas.FrameRect(Rect(0, 0, fLinesCpnBmp.Width, fLinesCpnBmp.Height));
      fLinesCpnBmpdef := True;
    end;
    Result := fLinesCpnBmp.Height;
  except;
    Result := 0;
  end;
end;


procedure TplSpiderGraph.DefineDrawSpace;
var
  oldFont: TFont;
  capHgt: integer;
begin
  // width & height
  case self.fBorderStyle of
    bsNone:
    begin
      fBorderSize := 0;
      fUsfWidth := Width;
      fUsfHeight := Height;
      fGraphRect := ClientRect;
    end;
    bsFlat:
    begin
      fBorderSize := 1;
      fUsfWidth := Width - 2;
      fUsfHeight := Height - 2;
      fGraphRect := ResizeRect(ClientRect, 1);
    end;
    else
    begin         //bs3D
      fBorderSize := 2;
      fUsfWidth := Width - 4;
      fUsfHeight := Height - 4;
      fGraphRect := ResizeRect(ClientRect, 2);
    end;
  end; {Case}

  // Graph's title
  if self.titleCaption <> '' then
  begin
    oldFont := canvas.Font;
    canvas.Font := TitleFont;
    case fTitlePosition of
      qtpTop:
      begin
        fTitleRect.Top := self.fBorderSize + 4;
        fTitleRect.Left := (self.fUsfWidth - canvas.TextWidth(titleCaption)) div 2;
        fTitleRect.Right := fTitleRect.Left + canvas.TextWidth(titleCaption);
        fTitleRect.Bottom := fTitleRect.Top + canvas.TextHeight(titleCaption);
      end;
      else
      begin   // qtpBottom
        fTitleRect.Left := (self.fUsfWidth - canvas.TextWidth(titleCaption)) div 2;
        fTitleRect.Right := fTitleRect.Left + canvas.TextWidth(titleCaption);
        fTitleRect.Bottom := self.Height - fBorderSize - 4;
        fTitleRect.Top := fTitleRect.Bottom - canvas.TextHeight(titleCaption);
      end;
    end;{Case}
    canvas.Font := oldFont;
  end;

  // Graph's drawSpace
  if titlecaption = '' then
  begin
    fGraphCenter.X := fBorderSize + (fUsfWidth div 2);
    fGraphCenter.Y := fBorderSize + (fUsfHeight div 2);
    fGraphRect.Left := fBorderSize;
    fGraphRect.Top := fBorderSize;
    fGraphRect.Right := self.Width - (fBorderSize shl 1);
    fGraphRect.Bottom := self.Height - (fBorderSize shl 1);
  end
  else
  begin
    case titlePosition of
      qtpTop:
      begin
        fGraphRect.Left := fBorderSize;
        fGraphRect.Top := fTitleRect.Top + fTitleRect.Bottom;
        fGraphRect.Right := self.Width - (fBorderSize shl 1);
        fGraphRect.Bottom := self.Height - (fBorderSize shl 1);
        fGraphCenter.X := (fGraphRect.Right - fGraphRect.Left) div 2;
        fGraphCenter.Y := fGraphRect.Top + ((fGraphRect.Bottom - fGraphRect.Top) div 2);
      end;
      else
      begin               // ie. case qtpBottom
        //             - (fBorderSize shl 1) : self.height still contains both boders.
        fGraphRect.Bottom := self.Height - (fBorderSize shl 1) -
          (fTitleRect.Bottom - fTitleRect.Top);
        fGraphRect.Top := fBorderSize;
        fGraphRect.Left := fBorderSize;
        fGraphRect.Right := self.Width - (fBorderSize shl 1);
        fGraphCenter.X := (fGraphRect.Right - fGraphRect.Left) shr 1;
        fGraphCenter.Y := (fGraphRect.Bottom - fGraphRect.Top) shr 1;
      end;
    end;{case}
  end;

  // Lines captions : drawn at opposite side of title.
  // Kept unchanged : fGraphRect.Left; fGraphRect.Right; fGraphCenter.X;
  if fShowLinesCpn then
  begin
    capHgt := GetLinesCaptionHeight;                        // Computes the caption rect and returns its height
    case titlePosition of
      qtpTop:
      begin
        fGraphRect.Bottom := fGraphRect.Bottom - capHgt;
        fGraphCenter.Y := fGraphRect.Top + ((fGraphRect.Bottom - fGraphRect.Top) div 2);
      end;
      else
      begin
        fGraphRect.Top := fGraphRect.Top + capHgt;
        fGraphCenter.Y := fGraphRect.Top + ((fGraphRect.Bottom - fGraphRect.Top) div 2);
      end;
    end;{case}
  end;
end;


procedure TplSpiderGraph.InitSummits;
var
  i: integer;
  baseAngle: single;
  gwd, ght: integer;
begin
  if Length(self.fAxes) < self.fAxesCount then
    SetLength(fAxes, fAxesCount);
  SetLength(self.fSummits, self.fAxesCount + 1);                 // Trick, to allow polyline instead of polygone, see help.
  Self.fAngle := 360 / self.fAxesCount;
  baseAngle := 0;

  gwd := fGraphRect.Right - fGraphRect.Left;
  ght := fGraphRect.Bottom - fGraphRect.Top;
  i := Min(gwd, ght);
  fAxesLen := (i div 20) * 9;                             // 8/10° du rayon

  for i := 0 to self.fAxesCount - 1 do
  begin
    Self.fSummits[i].X := trunc(fGraphCenter.x + fAxesLen * cos((baseAngle - 90) * pi / 180));
    Self.fSummits[i].Y := trunc(fGraphCenter.y + fAxesLen * sin((baseAngle - 90) * pi / 180));
    // Corrects most visible approximations ...
    if (baseAngle = 45) then
      fSummits[i].Y := fSummits[i].Y + 1
    else if (baseAngle = 225) then
      fSummits[i].Y := fSummits[i].Y + 1
    else if ((baseAngle = 90) or (baseAngle = 270)) then
      fSummits[i].Y := fGraphCenter.Y
    else if (baseAngle = 180) then
      fSummits[i].X := fGraphCenter.X;
    fAxes[i].Angle := baseAngle;                            // axes
    baseAngle := baseAngle + fAngle;
  end;
  Self.fSummits[self.fAxesCount].X := Self.fSummits[0].X;
  Self.fSummits[self.fAxesCount].Y := Self.fSummits[0].Y;
  SetLength(fSummitsPolygon, fAxesCount);
  fSummitsPolygon := fSummits;
end;

// Returns the rectangle that will hold this axe's caption(optionally including it's shape)
// Base for work = fsummit[i].X,fsummit[i].Y  (Xbase,Ybase)
// Captions for angles 0, 90, 180, 270 are considered special cases ;
// Others depend of the quandrant they're into.
// Basically, and to make things not too complicated, we turn like the niddles of a watch,
// and consider in turn each axe's summit to be the reference angle to draw the caption.
// Moreover, we constantly control that the resulting rectangle doesn't excess the graph limits.

function TplSpiderGraph.GetCaptionRect(aString: string; aCanvas: TCanvas; anAngle: single;
  destinationRect: TRect; inflateBy, Xbase, Ybase: integer): TRect;

var
  strWidth, strHeight: integer;

begin
  Result.Top := inflateBy;
  Result.Left := inflateBy;
  strWidth := aCanvas.TextWidth(aString);
  strHeight := aCanvas.TextHeight(aString);
  Result.Right := Result.Left + strWidth;
  Result.Bottom := Result.Top + strHeight;
  ResizeRect(Result, inflateBy);
  try
    strWidth := strWidth + (inflateBy shl 1);
    strHeight := strHeight + (inflateBy shl 1);

    if anAngle = 0 then                               // central-top caption : center horizontaly and check .Top ;
    begin
      // top  = summit.Y + rectResized + 2, but can't exces graphrect.top
      // left = summit.X (-1/2 * caption.Width) but can't exces graphrect.left;
      Result.Top := (Max(destinationRect.Top, Ybase - strHeight - 2));
      Result.Left := (Max(destinationRect.Left, Xbase - (strWidth shr 1)));
    end
    else
    if anAngle < 90 then                              //1st quadrant : reference = bottom-left corner of caption
    begin
      // top = summit.Y + rectResized.Height - 2 ou .top de graphrect
      // left = summit.X -1/2 de caption ou .left de graphrect;
      Result.Top := Ybase - 2 - strHeight;
      Result.Left := Xbase + 2;
      if (Result.Left + strWidth) > destinationRect.Right then
        Result.left := destinationRect.Right - strWidth;
    end
    else
    if anAngle = 90 then                              // central-left caption : centered vertically ;
    begin                                             // and check the right margin.
      Result.Top := Ybase - (strHeight shr 1);
      Result.Left := Xbase + 2;
      if (Result.Left + strWidth) > destinationRect.Right then
        Result.Left := destinationRect.Right - strWidth;
    end
    else
    if anAngle < 180 then                             //2nd quadrant : reference = Top-left corner of caption
    begin
      Result.Top := Ybase + 2;
      Result.Left := Xbase + 2;
      if (Result.Left + strWidth) > destinationRect.Right then
        Result.Left := destinationRect.Right - strWidth;
    end
    else
    if anAngle = 180 then                             // central-bottom caption : centered horizontally ;
    begin                                             // and check the bottom margin.
      Result.Top := Ybase + 2;
      Result.Left := Xbase - (strWidth shr 1);
      if (Result.Top + strHeight) > destinationRect.Bottom then
        Result.Top := destinationRect.Bottom - strHeight;
    end
    else
    if anAngle < 270 then                             // 3rd quadrant : reference = Top-right corner of caption
    begin
      Result.Top := Ybase + 2;
      Result.Left := Xbase - 2 - strWidth;
      if Result.Left < destinationRect.left then
        Result.Left := destinationRect.left;
    end
    else
    if anAngle = 270 then                             // central-left caption
    begin
      Result.Top := Ybase - (strHeight shr 1);
      Result.Left := Xbase - 2 - strWidth;
      if Result.Left < destinationRect.left then
        Result.Left := destinationRect.left;
    end
    else                                          // Last quadrant;  270 < anAngle < 359
    begin
      Result.Top := Ybase - 2 - strHeight;
      Result.Left := Xbase - 2 - strWidth;
      if Result.Left < destinationRect.left then
        Result.Left := destinationRect.left;
    end;
    Result.Right := Result.Left + strWidth;
    Result.Bottom := Result.Top + strHeight;
  except;
  end;
end;

// returns the TPoint coordinates of lines points.
function TplSpiderGraph.GetLinePoint(iMin, iMax, position, iAngle: single): TPoint;
var
  posNorm, posPct, posDist: single;
  axeRange: extended;
begin
  try
    axeRange := (iMax - iMin);
    if axeRange = 0 then
    begin
      Result.X := fGraphCenter.x;
      Result.Y := fGraphCenter.y;
      exit;
    end;
    posNorm := position - iMin;
    posPct := posNorm / axeRange;
    posDist := fAxesLen * posPct;
    Result.X := trunc(fGraphCenter.x + posDist * cos((iAngle - 90) * pi / 180));  // NIH
    Result.Y := trunc(fGraphCenter.y + posDist * sin((iAngle - 90) * pi / 180));
  except;
  end;
end;


constructor TplSpiderGraph.Create(aOwner: TComponent);
var
  i: integer;
  anAxe: TQSpiderAxe;

begin
  inherited Create(aOwner);
  Parent := TWinControl(aOwner);
  SetBounds(0, 0, 200, 200);
  fAxesCount := 7;
  fAxesColor := clNavy;
  axesFont := TFont.Create;
  fAxesAutoSized := False;

  SetLength(self.fAxes, self.fAxesCount);
  for i := 0 to fAxesCount - 1 do
  begin
    anAxe := TQSpiderAxe.CreateOwned(Self);
    anAxe.fMin := 0;
    anAxe.fMax := 100;
    anAxe.Angle := 0;
    anAxe.fIndex := i;
    anAxe.Caption := IntToStr(i);
    fAxes[i] := anAxe;
  end;

  fGraphRect := Self.ClientRect;
  fGraphCenter.X := Self.Width div 2;
  fGraphCenter.Y := Self.Height div 2;
//  DoubleBuffered := True;

  fBorderStyle := bsNone;
  fBorderSize := 0;
  fBackGround := bgTransparent;
  fBackgroundColor := clWhite;
  fBackGStartColor := RGBToColor(230, 230, 230);
  fBackGFinalColor := clWhite;

  fTitleCaption := '';
  fTitlePosition := qtpTop;
  titlefont := TFont.Create;
  titlefont.Color := clBlue;
  titleFont.Style := [fsBold];
  titleFont.Size := 12;
  self.fPolygonFill := False;
  self.fPolygonColor := RGBToColor(230, 255, 255);

  fDfltPenWidth := 2;
  fShowLnPoints := False;
  fLinesCount := 1;                                       // A graph has allways at least one line (even if this line is "empty")
  fMultilines := False;
  SetLength(fLines, 1);
  fLines[0] := TQSGLine.CreateOwned(Self);

  linesCpnFont := TFont.Create;
  fLinesCpnBmpClr := clWhite;
  fLinesCpnBmpDef := False;
  fShowLinesCpn := False;
  // lines information window (Mouse track result)
  fShowMouseBox := True;
  fMBoxBackColor := clInfoBk;
  fMBoxForColor := clBlack;
  fMBoxUsesLnClr := False;
  SetLength(fLinesInfoBoxes, 1);
  fMBoxParent := Self;                                       // Prepares the future Boxes parent
  fLinesInfoBoxes[0].Box := TStaticText.Create(Self);
  fLinesInfoBoxes[0].Box.Parent := Self;
  with fLinesInfoBoxes[0].Box do
  begin
    Hide;
    Parent := Self;
    AutoSize := False;
    BorderStyle := sbsSingle;
    Caption := '';
    Color := mBoxBackColor;
  end;
  fLinesInfoBoxes[0].defined := False;

  fTrackMsMoves := True;
  fMouseOnLine := False;
  HighlightMode := [hmShowPoints, hmFlashLine];
  fHighlightColor := clYellow;
  fMouseLineIndex := -1;
  Self.OnMouseMove := @MouseTrack;

  Self.DefineDrawSpace;
  Self.InitSummits;

  parent := nil;                                            // see above ...
end;


destructor TplSpiderGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to linesCount - 1 do
    fLines[i].Free;
  if Assigned(fLinesCpnBmp) then
    fLinesCpnBmp.Free;
  inherited;
end;


procedure TplSpiderGraph.Paint;
var
  i, ix0, ix1: integer;
  oldBkMode: integer;
  oldFont: TFont;
  vert: array[0..1] of TRIVERTEX;
  gRect: GRADIENT_RECT;
  aRect: TRect;
  gDir: integer;
  aClr: TColor;
  aPoint: TPoint;
  lc, pw: integer;

begin
  // -1- Initializing margins and summits
  DefineDrawSpace;
  InitSummits;

  // -2- (if needed) max and min
  if not (fAxesMMdefined) then
  begin
    for i := 0 to Self.fAxesCount - 1 do
    begin
      fAxes[i].SetAutoSizeMin(fAxes[i].fAutoSizeMin);
      fAxes[i].SetAutoSizeMax(fAxes[i].fAutoSizeMax);
    end;
    fAxesMMdefined := True;
  end;

  // -3- Component's border
  case self.fBorderStyle of
    bsFlat:
    begin
      aClr := self.Canvas.Brush.Color;
      self.Canvas.Brush.Color := clGray;
      self.Canvas.FrameRect(Self.ClientRect);
      self.Canvas.Brush.Color := aClr;
    end;
    bs3D:
    begin
      aClr := self.Canvas.Brush.Color;
      self.Canvas.Brush.Color := clWhite;
      aRect := Self.ClientRect;
      aRect.Left := Self.ClientRect.Left + 1;
      aRect.Top := Self.ClientRect.Top + 1;
      self.Canvas.FrameRect(aRect);

      self.Canvas.Brush.Color := RGBtoColor(172, 168, 153);
      aRect := Self.ClientRect;
      aRect.Right := Self.ClientRect.Right - 1;
      aRect.Bottom := Self.ClientRect.Bottom - 1;
      self.Canvas.FrameRect(aRect);
      self.Canvas.Brush.Color := aClr;
    end;
  end;{Case}
  // -4- Drawing the background
  if fBackGround > bgcolored then
  begin
    case fBackGround of
      bgTopBottom:
      begin
        gDir := GRADIENT_FILL_RECT_V;
        ix0 := 0;
        ix1 := 1;
      end;
      bgBottomTop:
      begin
        gDir := GRADIENT_FILL_RECT_V;
        ix0 := 1;
        ix1 := 0;
      end;
      bgLeftToRight:
      begin
        gDir := GRADIENT_FILL_RECT_H;
        ix0 := 0;
        ix1 := 1;
      end;
      else
      begin
        gDir := GRADIENT_FILL_RECT_H;
        ix0 := 1;
        ix1 := 0;
      end;
    end;{case}
    vert[0].x := self.fBorderSize;
    vert[0].y := self.fBorderSize;
    vert[ix0].Red := RED(self.fBackGStartColor) shl 8;
    vert[ix0].Green := GREEN(self.fBackGStartColor) shl 8;
    vert[ix0].Blue := BLUE(self.fBackGStartColor) shl 8;
    vert[ix0].Alpha := $0000;
    vert[1].x := self.Width - self.fBorderSize;
    vert[1].y := self.Height - self.fBorderSize;
    vert[ix1].Red := RED(self.fBackGFinalColor) shl 8;
    vert[ix1].Green := GREEN(self.fBackGFinalColor) shl 8;
    vert[ix1].Blue := BLUE(self.fBackGFinalColor) shl 8;
    vert[ix1].Alpha := $0000;
    gRect.UpperLeft := 0;
    gRect.LowerRight := 1;

    { TODO : Stefanos fix this 9999 }
    //==================================================================
    // GradientFill(Self.Canvas, @vert,2,@gRect,1,gDir);
    //==================================================================

    aClr := self.Canvas.Brush.Color;
    canvas.Brush.Color := self.fBackgroundColor;
    canvas.FillRect(ResizeRect(Self.ClientRect, fBorderSize));
    self.Canvas.Brush.Color := aClr;

  end
  else
  if fBackGround = bgColored then
  begin
    aClr := self.Canvas.Brush.Color;
    canvas.Brush.Color := self.fBackgroundColor;
    canvas.FillRect(ResizeRect(Self.ClientRect, fBorderSize));
    self.Canvas.Brush.Color := aClr;
  end;

  // -5- Axes
  // Linking the axes summits can be done two ways :
  // -> using Canvas.Polygon() will fill the polygon with the brush's color ;
  // -> Otherwise, D6's help suggests using Canvas.Polyline() to avoid
  //    this filling, passing the first point two times (as first and last point);
  with canvas do
  begin
    Pen.Width := 1;
    Pen.Color := self.fAxesColor;
    // summits
    if Self.fPolygonFill then
    begin
      aClr := self.Canvas.Brush.Color;
      Canvas.Brush.Color := fPolygonColor;
      Polygon(Self.fSummitsPolygon);
      self.Canvas.Brush.Color := aClr;
    end
    else
      Polyline(Self.fSummits);
    // axes
    for i := 0 to Length(self.fSummits) - 1 do
    begin
      moveto(fGraphCenter.x, fGraphCenter.y);
      lineTo(Self.fSummits[i].X, Self.fSummits[i].Y);
    end;
  end;

  // -6- Title
  if (self.titleCaption <> '') then
  begin
    oldFont := canvas.Font;
    canvas.Font := TitleFont;
    OldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.TextOut(fTitleRect.Left, fTitleRect.Top, titleCaption);
    SetBkMode(Canvas.Handle, OldBkMode);
    canvas.Font := oldFont;
  end;

  // -7- Axes' captions
  oldBkMode := SetBkMode(Canvas.Handle, TRANSPARENT);
  oldFont := canvas.Font;
  canvas.Font := axesFont;
  for i := 0 to fAxesCount - 1 do
  begin
    if (self.fAxes[i].Caption <> '') then
    begin
      aRect := GetCaptionRect(fAxes[i].Caption, self.Canvas, fAxes[i].angle, fGraphRect, 2,
        fSummits[i].X, fSummits[i].Y);
      SetBkMode(Canvas.Handle, TRANSPARENT);
      Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, fAxes[i].Caption);
      if fAxesCapFramed then
      begin
        aClr := Canvas.Brush.Color;
        Canvas.Brush.Color := canvas.Font.Color;
        Canvas.FrameRect(aRect);
        Canvas.Brush.Color := aClr;
      end;
    end;
  end;
  SetBkMode(Canvas.Handle, OldBkMode);
  canvas.Font := oldFont;

  // -8- Lines
  for lc := 0 to self.fLinesCount - 1 do
  begin
    if (not (fLines[lc].fHasValues)) or (not (fLines[lc].fshow)) then
      Continue;
    for i := 0 to fAxesCount - 1 do
    begin
      aPoint := GetLinePoint(fAxes[i].fMin, fAxes[i].fmax, fLines[lc].fValues[i], fAxes[i].angle);
      fLines[lc].fPoints[i].X := aPoint.X;
      fLines[lc].fPoints[i].Y := aPoint.Y;
    end;
    // polyline finalization : linking last and first values;
    // (The "fPoints" array has soon been sized in the lines constructor to fAxesCount +1)
    fLines[lc].fPoints[fAxesCount].X := fLines[lc].fPoints[0].X;
    fLines[lc].fPoints[fAxesCount].Y := fLines[lc].fPoints[0].Y;
    // HighLight management : the current line's pen width, showPoints, aso, has been set earlier :
    // either by the TQLine instance constructor, or by the programmer at run-time.
    canvas.Pen.Width := fLines[lc].fPenWidth;
    canvas.Pen.Color := fLines[lc].color;
    canvas.Polyline(fLines[lc].fPoints);
    canvas.Pen.Width := 2;
    if fLines[lc].fShowPoints then
    begin
      if fLines[lc].penWidth < 4 then
        pw := 4
      else
        pw := fLines[lc].penWidth;
      for i := 0 to fAxesCount - 1 do
        Canvas.Ellipse(fLines[lc].fPoints[i].X - pw,
          fLines[lc].fPoints[i].Y - pw,
          fLines[lc].fPoints[i].X + pw,
          fLines[lc].fPoints[i].Y + pw);
    end;
  end;
  canvas.Pen.Width := 1;

  // -9- lines captions with their colors :
  if fShowLinesCpn and (GetLinesCaptionHeight > 0) then
  begin
    case titlePosition of
      qtpTop: Canvas.Draw((self.Width - fLinesCpnBmp.Width) div 2,
          self.Height - fLinesCpnBmp.Height - fBorderSize - 2,
          fLinesCpnBmp);
      else
        Canvas.Draw((self.Width - fLinesCpnBmp.Width) div 2,
          fBorderSize + 2,
          fLinesCpnBmp);
    end;{case}
  end;
end;


procedure TplSpiderGraph.Resize;
begin
  inherited;
  self.fLinesCpnBmpdef := False;
  Invalidate;
end;

procedure TplSpiderGraph.SetBorderStyle(Value: TQBorderStyle);
begin
  Self.fBorderStyle := Value;
  defineDrawSpace;
  Invalidate;
end;

procedure TplSpiderGraph.SetBackGround(Value: TQBackGround);
begin
  fBackGround := Value;
  Invalidate;
end;

procedure TplSpiderGraph.SetBackGroundColor(Value: TColor);
begin
  fBackgroundColor := Value;
  invalidate;
end;

procedure TplSpiderGraph.SetAxesColor(Value: TColor);
begin
  fAxesColor := Value;
  invalidate;
end;

procedure TplSpiderGraph.SetBackGStartColor(Value: TColor);
begin
  Self.fBackGStartColor := Value;
  invalidate;
end;

procedure TplSpiderGraph.SetBackGFinalColor(Value: TColor);
begin
  Self.fBackGFinalColor := Value;
  invalidate;
end;

// Care : Changing axesCount erases stored lines...
// May seam boring, but look : The line values setter stores only
// as many values as there is axes.
// values in excess have been rejected, missing ones have been set to a
// "Min" which is no longer good.
procedure TplSpiderGraph.SetAxesCount(Value: integer);
var
  i: integer;
  anAxe: TQSpiderAxe; //PQSpiderAxe;

begin
  for i := 0 to fAxesCount - 1 do
  begin
    anAxe := fAxes[i];
    anAxe.Free;
  end;
  SetLength(self.fAxes, 0);

  if Value < 3 then
    Self.fAxesCount := 3
  else
    Self.fAxesCount := Value;

  SetLength(self.fAxes, fAxesCount);

  for i := 0 to fAxesCount - 1 do
  begin
    anAxe := TQSpiderAxe.CreateOwned(Self);
    anAxe.fMin := 0;
    anAxe.fMax := 100;
    anAxe.Angle := 0;
    anAxe.fIndex := i;
    anAxe.Caption := IntToStr(i);
    fAxes[i] := anAxe;
  end;

  // reset lines collection
  for i := 0 to fLinesCount - 1 do
    if Assigned(fLines[i]) then
      fLines[i].Free;
  SetLength(fLines, 0);
  self.fMultilines := False;
  self.fLinesCount := 1;
  SetLength(fLines, 1);
  fLines[0] := TQSGLine.CreateOwned(Self);

  // and their corresponding infoBoxes
  for i := 0 to Length(fLinesInfoBoxes) - 1 do
    if (Assigned(fLinesInfoBoxes[i].box)) then
    begin
      fLinesInfoBoxes[i].box.Free;
      fLinesInfoBoxes[i].box := nil;
    end;

  SetLength(fLinesInfoBoxes, 1);
  fLinesInfoBoxes[0].Box := TStaticText.Create(Self);
  with fLinesInfoBoxes[0].Box do
  begin
    Hide;
    Parent := Self.fMBoxParent;
    AutoSize := False;
    BorderStyle := sbsSingle;
    Caption := '';
    Color := mBoxBackColor;
  end;
  fLinesInfoBoxes[0].defined := False;
  fLinesCpnBmpdef := False;

  Self.InitSummits;
  invalidate;
end;

// The computing of the new min and max are done in the axes autosize
// setter. If lines are added to the graph later, the lines values
// setter will request a new computed at he next paint.
procedure TplSpiderGraph.SetAxesSizeState(Value: boolean);

var
  i: integer;
begin
  self.fAxesAutoSized := Value;
  for i := 0 to self.fAxesCount - 1 do
    fAxes[i].autoSized := Value;
end;

procedure TplSpiderGraph.SetTitleCaption(Value: string);
begin
  self.fTitleCaption := Value;
  DefineDrawSpace;
  invalidate;
end;

procedure TplSpiderGraph.SetTitlePosition(Value: TQTitlePosition);
begin
  self.fTitlePosition := Value;
  if self.titleCaption <> '' then
    invalidate;
end;

function TplSpiderGraph.GetAxesByIndex(ix: integer): TQSpiderAxe;//PQSpiderAxe;
begin
  Result := self.faxes[ix];
end;

procedure TplSpiderGraph.SetAxesFramed(Value: boolean);
begin
  self.fAxesCapFramed := Value;
  invalidate;
end;

procedure TplSpiderGraph.SetPolygonFill(Value: boolean);
begin
  Self.fPolygonFill := Value;
  Invalidate;
end;

procedure TplSpiderGraph.SetPolygonColor(Value: TColor);
begin
  Self.fPolygonColor := Value;
  Invalidate;
end;

// axeIx  : index of the axe to be searched ;
// value  : the lowest value ;
// cnd    : "candidate" : minVal will be returned if no line has values
// result : array of  indexe(s) of the line(s) with the lowest value for this axe.

// If no line has yet been defined, GetMinValue will return either the actual fMin,
// (cnd=False) or minVal (cnd=True);

// Works in two passes, because the min value is not known until we've searched the
// entire lines collection. (or could have work in one pass, resetting the result
// array at each new lowest value.)

function TplSpiderGraph.GetMinValue(axeIx: integer; var minVal: single;
  cnd: boolean = False): TIntegerDynArray;

var
  i: integer;
  mVal: single;
begin
  setLength(Result, 0);
  mVal := Infinity;                                         // <=> High(single);
  try
    // -1- Searches the lowest value :
    for i := 0 to self.fLinesCount - 1 do
      if fLines[i].fHasValues then
        if {(IsInfinite(mVal)) or}  fLines[i].fValues[axeIx] < mVal then
          mVal := fLines[i].fValues[axeIx];

    // -2- If no line has values, returns either this axes'min, or the value received,
    //     else searches the index of the corresponding line(s) :
    if IsInfinite(mVal) then
    begin
      if not (cnd) then
        minVal := self.fAxes[axeIx].fMin;
    end
    else
    begin
      if cnd then
        minVal := mVal
      else
        minVal := self.fAxes[axeIx].fMin;
      for i := 0 to self.fLinesCount - 1 do
        if fLines[i].fValues[axeIx] = mVal then
        begin
          setLength(Result, Length(Result) + 1);
          Result[length(Result) - 1] := i;
        end;
    end;
  except;
  end;
end;


// axeIx  : index of the axe to be searched ;
// value  : the highest value ;
// cnd    : "candidate" : minVal will be returned if no line has values
// result : array of  indexe(s) of the line(s) with the highest value for this axe.

// If no line has yet been defined, GetMaxValue will return either the actual fMax,
// (cnd=False) or maxVal (cnd=True);

function TplSpiderGraph.GetMaxValue(axeIx: integer; var maxVal: single;
  cnd: boolean = False): TIntegerDynArray;

var
  i: integer;
  mVal: single;
begin
  setLength(Result, 0);
  mVal := NegInfinity;                                      // <=> Low(single);
  try
    // -1- Searches the highest value :
    for i := 0 to self.fLinesCount - 1 do
      if fLines[i].fHasValues then
        if fLines[i].fValues[axeIx] > mVal then
          mVal := fLines[i].fValues[axeIx];

    // -2- If no line has values, returns either this axes'max, or the value received,
    //     else searches the index of the corresponding line(s) :
    if IsInfinite(mVal) then
    begin
      if not (cnd) then
        maxVal := self.fAxes[axeIx].fMax;
    end
    else
    begin
      if cnd then
        maxVal := mVal
      else
        maxVal := self.fAxes[axeIx].fMax;
      for i := 0 to self.fLinesCount - 1 do
        if fLines[i].fValues[axeIx] = mVal then
        begin
          setLength(Result, Length(Result) + 1);
          Result[length(Result) - 1] := i;
        end;
    end;
  except;
  end;
end;

// makes a copy of the whole component into a bitmap, and saves it on the
// disk, either as .bmp or .jpg  (asBmp = "copy asBMP or not)".
// The resulting bmp is in high resolution (pf32bit). To obtain less heavy files,
// use the "Copy2Bitmap()" function instead of this one;
function TplSpiderGraph.Save2File(fileName: TFileName; asBMP: boolean = True): boolean;

var
  aBitmap: TBitmap;
begin
  Result := True;
  aBitmap := TBitmap.Create;
  try
    try
      aBitmap.PixelFormat := pf32bit;
      aBitmap.Width := Self.Width;
      aBitmap.Height := Self.Height;
      BitBlt(aBitmap.Canvas.Handle, 0, 0, aBitmap.Width, aBitmap.Height,
        Self.Canvas.Handle, 0, 0, SRCCOPY);
      case AsBmp of
        True: aBitmap.SaveToFile(fileName);
        False: with TJPEGImage.Create do
          begin
            Assign(aBitmap);
            SaveToFile(FileName);
            Free;
          end;
      end; {case}
    finally
      aBitmap.Free;
    end;
  except
    Result := False;
  end;
end;


// Fills aBitmap with the picture of the component.
// Allows you to setup the bitmap the way you want (PixelFormat, aso)
// (but the dimenssions which are set to the graph's ones before copying).
function TplSpiderGraph.Copy2Bitmap(var aBitmap: TBitmap): boolean;
begin
  Result := Assigned(aBitmap);
  if not (Result) then
    exit;
  try
    aBitmap.Width := Self.Width;
    aBitmap.Height := Self.Height;
    BitBlt(aBitmap.Canvas.Handle, 0, 0, aBitmap.Width, aBitmap.Height,
      Self.Canvas.Handle, 0, 0, SRCCOPY);
  except
    Result := False;
  end;
end;

// Lines info boxes are the ones which appear next to the mouse pointer, and
// display the lines values, when mouseTracking is enabled.
// They are build once only, to avoid CPU time consuming at each mouse move
// (and obviously rebuild if any setting changes).
procedure TplSpiderGraph.DefineLinesInfoBox(lineIx: integer);

{ ------------------------  Local  ------------------------ }
  function ResizeCaption(aString: string; len: integer): string;
  var
    i: integer;
  begin
    Result := aString;
    for i := 0 to (len - Length(aString)) do
      Result := Result + ' ';
  end;

  function CenterLine(aString: string; len: integer): string;
  var
    i, target: integer;
  begin
    Result := aString;
    target := len - Length(aString);
    for i := 0 to (target div 2) do
      Result := ' ' + Result + ' ';
    for i := 0 to (target mod 2) - 1 do
      Result := Result + ' ';
  end;

  { ------------------------ \Local\ ------------------------ }

var
  oldFont: TFont;
  tslAxesCap, tslLnsVals: TStringList;
  i, axMax, lineLen, lnMax: integer;
  bxTitle: string;

begin
  try
    fLinesInfoBoxes[lineIx].box.Font.Name := 'Courier New';
    fLinesInfoBoxes[lineIx].box.Font.Size := 8;
  except;
  end;
  fLinesInfoBoxes[lineIx].box.color := mBoxBackColor;
  if mBoxUsesLnColor then
    fLinesInfoBoxes[lineIx].box.Font.Color := fLines[lineIx].Color
  else
    fLinesInfoBoxes[lineIx].box.Font.Color := mBoxForColor;

  axMax := 0;
  lnMax := 0;
  tslAxesCap := TStringList.Create;
  tslLnsVals := TStringList.Create;
  tslAxesCap.Capacity := self.fAxesCount + 1;
  tslLnsVals.Capacity := self.fAxesCount + 1;

  for i := 0 to self.fAxesCount - 1 do
  begin
    tslAxesCap.Add(fAxes[i].Caption);
    tslLnsVals.Add(FloatToStrF(fLines[lineIx].values[i], ffGeneral, 7, 2));
    axMax := Max(axMax, Length(fAxes[i].Caption));
    lnMax := Max(lnMax, Length(tslLnsVals.Strings[i]));
  end;

  // resizes lines
  for i := 0 to Self.fAxesCount - 1 do
    tslAxesCap.Strings[i] := ' ' + ResizeCaption(tslAxesCap.Strings[i], axMax) + ': ' + tslLnsVals.Strings[i];
  if fLines[lineIx].Caption = '' then
    bxTitle := 'Line N° ' + IntToStr(lineIx)
  else
    bxTitle := fLines[lineIx].Caption;
  lineLen := Max(axMax + lnMax + 5, Length(bxTitle) + 2);                    //"1 + axMax + 3 + lnMax + 1";

  // Builds title
  bxTitle := CenterLine(bxTitle, lineLen);

  // Fill this boxe's caption
  fLinesInfoBoxes[lineIx].box.Caption := bxTitle;
  for i := 0 to self.fAxesCount - 1 do
    fLinesInfoBoxes[lineIx].box.Caption := fLinesInfoBoxes[lineIx].box.Caption + #13#10 + tslAxesCap.Strings[i];

  oldFont := canvas.Font;
  canvas.Font := fLinesInfoBoxes[lineIx].box.Font;
  bxTitle := '';                                            // re-used (no longer needed for the title);
  fLinesInfoBoxes[lineIx].box.Height := (Canvas.TextHeight('Ip') + 2) * (fAxesCount + 2);
  fLinesInfoBoxes[lineIx].box.Width := Canvas.TextWidth(ResizeCaption(bxTitle, lineLen));
  canvas.Font := oldFont;
  tslAxesCap.Free;
  tslLnsVals.Free;

  // decides which is the parent : The graph or a form ?
  if Assigned(self.fMBoxParent) then
    fLinesInfoBoxes[lineIx].box.Parent := self.fMBoxParent;

  fLinesInfoBoxes[lineIx].defined := True;
end;


procedure TplSpiderGraph.SetLinesCount(Value: integer);
var
  i: integer;
begin
  // Free previous lines
  for i := 0 to self.fLinesCount - 1 do
    fLines[i].Free;
  SetLength(fLines, 0);

  // corresponding infoBoxes
  for i := 0 to Length(fLinesInfoBoxes) - 1 do
    if Assigned(fLinesInfoBoxes[i].box) then
      fLinesInfoBoxes[i].box.Free;

  // Define new ones
  if Value < 2 then
  begin
    self.fMultilines := False;
    self.fLinesCount := 1;
    SetLength(fLines, 1);
    fLines[0] := TQSGLine.CreateOwned(Self);
  end
  else
  begin
    self.fMultilines := True;
    self.fLinesCount := Value;
    SetLength(fLines, Value);
    for i := 0 to Value - 1 do
      fLines[i] := TQSGLine.CreateOwned(Self);
  end;
  // corresponding boxes
  SetLength(fLinesInfoBoxes, self.fLinesCount);
  for i := 0 to self.fLinesCount - 1 do
  begin
    fLinesInfoBoxes[i].Box := TStaticText.Create(Self);
    with fLinesInfoBoxes[i].Box do
    begin
      Hide;
      Parent := Self.fMBoxParent;
      AutoSize := False;
      BorderStyle := sbsSingle;
      Caption := '';
      Color := mBoxBackColor;
    end;
    fLinesInfoBoxes[i].defined := False;
  end;
  fLinesCpnBmpdef := False;
end;


// Returns a pointer to the specified line object
// CARE : If accessed before the lineCount has been set, or with an index above
// lineCount -1, will return a NIL pointer, and your app will show the classical
// eAccessViolation message ...
function TplSpiderGraph.GetLinesByIndex(ix: integer): TQSGLine;

begin
  if (ix >= fLinesCount) or (ix < 0) then
    Result := nil
  else
    Result := fLines[ix];                    // D6 help : dynmic assignment of arrays alloweded.
end;

function TplSpiderGraph.RemoveLine(lineIndex: integer): boolean;
var
  i: integer;
begin
  Result := True;
  try
    if lineIndex > fLinesCount - 1 then
      exit;

    // Free fLines[lineIndex]
    if Assigned(fLines[lineIndex]) then
      fLines[lineIndex].Free;
    for i := lineIndex to fLinesCount - 2 do
      fLines[i] := fLines[i + 1];
    SetLength(fLines, Length(fLines) - 1);

    // Free corresponding infoBox
    if Assigned(fLinesInfoBoxes[lineIndex].box) then
      fLinesInfoBoxes[lineIndex].box.Free;
    for i := lineIndex to fLinesCount - 2 do
      fLinesInfoBoxes[i] := fLinesInfoBoxes[i + 1];
    SetLength(fLinesInfoBoxes, Length(fLinesInfoBoxes) - 1);

    Dec(fLinesCount);
    fLinesCpnBmpdef := False;
    Invalidate;
  except;
    Result := False;
  end;
end;

function TplSpiderGraph.AddLine(const vals: TQSingleArray): integer;
begin
  try
    // add one line to the collection
    if (fLinesCount = 1) and (fLines[0].fHasValues = False) then
    begin
      if Assigned(fLines[0]) then
        fLines[0].Free;
      SetLength(fLines, 0);
      fLinesCount := 0;
      if Assigned(fLinesInfoBoxes[0].box) then
        fLinesInfoBoxes[0].box.Free;
    end;
    SetLength(fLines, Length(fLines) + 1);
    fLinesCount := Length(fLines);
    fMultilines := fLinesCount > 1;
    Result := fLinesCount - 1;
    fLines[Result] := TQSGLine.CreateOwned(Self);
    fLines[Result].values := vals;
    // prepares the corresponding infoBox
    SetLength(fLinesInfoBoxes, fLinesCount);
    fLinesInfoBoxes[Result].Box := TStaticText.Create(Self);
    with fLinesInfoBoxes[Result].Box do
    begin
      Hide;
      Parent := Self.fMBoxParent;
      AutoSize := False;
      BorderStyle := sbsSingle;
      Caption := '';
      Color := mBoxBackColor;
    end;
    fLinesInfoBoxes[Result].defined := False;
    fLinesCpnBmpdef := False;
    invalidate;
  except;
    Result := -1;
  end;
end;


procedure TplSpiderGraph.SetDfltPenWidth(Value: word);
var
  i: integer;
begin
  if (Value < 1) or (Value > 255) then
    Value := 1;
  self.fDfltPenWidth := Value;
  for i := 0 to Self.fLinesCount - 1 do
  begin
    self.fLines[i].penWidth := Value;
  end;
  invalidate;
end;

procedure TplSpiderGraph.SetShowLnPoints(Value: boolean);
var
  i: integer;
begin
  Self.fShowLnPoints := Value;
  for i := 0 to self.fLinesCount - 1 do
    self.fLines[i].showPoints := Value;
  invalidate;
end;

procedure TplSpiderGraph.SetShowLinesCpn(Value: boolean);
begin
  self.fShowLinesCpn := Value;
  Invalidate;
end;

procedure TplSpiderGraph.SetLinesCpnBmpTrp(Value: boolean);
begin
  self.fLinesCpnBmpTrp := Value;
  self.fLinesCpnBmpdef := False;
  invalidate;
end;

procedure TplSpiderGraph.SetLinesCpnBmpClr(Value: TColor);
begin
  self.fLinesCpnBmpClr := Value;
  self.fLinesCpnBmpdef := False;
  Invalidate;
end;


procedure TplSpiderGraph.SetMBoxParent(Value: TWinControl);
// Allows the mouse info boxes to be displayed on the whole form area,
// instead of the graph only;
var
  i: integer;
begin
  if (Value = nil) or not (Value is TForm) then
    Value := Self;
  Self.fMBoxParent := Value;
  for i := 0 to Self.fLinesCount - 1 do
    if Assigned(fLinesInfoBoxes[i].box) then
      fLinesInfoBoxes[i].box.Parent := Value;
end;

procedure TplSpiderGraph.SetTrackMsMoves(Value: boolean);
// If set to False, ensure that there is no box actually visible
var
  i: integer;
begin
  Self.ftrackMsMoves := Value;
  if not (Value) then
    for i := 0 to Self.fLinesCount - 1 do
      if (fLinesInfoBoxes[i].box.Visible) then
      begin
        fLinesInfoBoxes[i].box.Hide;
        Invalidate;
      end;
end;

procedure TplSpiderGraph.SetShowMouseBox(Value: boolean);
// If set to False, ensure that there is no box actually visible
var
  i: integer;
begin
  Self.fShowMouseBox := Value;
  if not (Value) then
    for i := 0 to Self.fLinesCount - 1 do
      if (fLinesInfoBoxes[i].box.Visible) then
      begin
        fLinesInfoBoxes[i].box.Hide;
        Invalidate;
      end;
end;

procedure TplSpiderGraph.SetMBoxForColor(Value: TColor);
var
  i: integer;
begin
  self.fMBoxForColor := Value;
  for i := 0 to Length(fLinesInfoBoxes) - 1 do
    fLinesInfoBoxes[i].defined := False;                  // set .defined to false to have the box computed again
end;

procedure TplSpiderGraph.SetMBoxBackColor(Value: TColor);
var
  i: integer;
begin
  self.fMBoxBackColor := Value;
  for i := 0 to Length(fLinesInfoBoxes) - 1 do
    fLinesInfoBoxes[i].defined := False;                  // set .defined to false to have the box computed again
end;

procedure TplSpiderGraph.SetMBoxUsesLnClr(Value: boolean);
var
  i: integer;
begin
  self.fMBoxUsesLnClr := Value;
  for i := 0 to Length(fLinesInfoBoxes) - 1 do
    fLinesInfoBoxes[i].defined := False;                  // set .defined to false to have the box computed again
end;


function TplSpiderGraph.GetBestLineByArea(maxWanted: boolean = True): TIntegerDynArray;
  // NIH : Polygon area computing can be find anywhere on the web...
  // Returns an array of integers containing the indexes of the line(s)
  // with the widest area. return[0] is worth -1 if a probleme occured
  // or no line has values.
  // length(return) = fLinesCount if all areas are strictly equal...

var
  i, lc, iTmp: integer;
  arBestArea: array of real;
  bestArea: real;
  valuesRead: boolean;
begin
  SetLength(Result, 1);
  Result[0] := -1;
  SetLength(arBestArea, fLinesCount);
  valuesRead := False;

  if maxWanted then
    bestArea := 0
  else
    bestArea := Infinity;

  try
    // searches best area
    for lc := 0 to fLinesCount - 1 do
    begin
      iTmp := 0;
      for i := 0 to fAxesCount - 1 do
      begin
        if fLines[lc].fHasValues then
          valuesRead := True;
        iTmp := iTmp + fLines[lc].fPoints[i].X * fLines[lc].fPoints[i + 1].Y -
          fLines[lc].fPoints[i + 1].X * fLines[lc].fPoints[i].Y;
        arBestArea[lc] := abs(iTmp / 2);
      end;
      if maxWanted then
      begin
        if arBestArea[lc] > bestArea then
          bestArea := arBestArea[lc];
      end
      else
      if arBestArea[lc] < bestArea then
        bestArea := arBestArea[lc];
    end;

    // populates result
    if (bestArea <= 0) or not (valuesRead) then
      Exit;
    SetLength(Result, 0);                                      // if still there, resize the result, to ease next step.
    for lc := 0 to fLinesCount - 1 do
    begin
      if arBestArea[lc] = bestArea then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[length(Result) - 1] := lc;
      end;
    end;
    if Length(Result) = 0 then                                // useless (?)
    begin
      SetLength(Result, 1);
      Result[0] := -1;
    end;
  except;
  end;
end;

function TplSpiderGraph.GetBestLineByAxe(axeIx: integer; maxWanted: boolean = True): TIntegerDynArray;
var
  lc: integer;
  bestLine: real;
  valuesRead: boolean;
begin
  SetLength(Result, 1);
  Result[0] := -1;

  if (axeIx > (self.fAxesCount - 1)) or (axeIx < 0) then
    Exit;
  try
    if maxWanted then
      bestLine := fAxes[axeIx].fMin
    else
      bestLine := fAxes[axeIx].fMax;

    valuesRead := False;                                    // There can be lines (due to the lineCount setter) ...
    // searches the best(s) line(s)                         // ... but these may not have yet values. So, check it.
    for lc := 0 to fLinesCount - 1 do
      if fLines[lc].fHasValues then
      begin
        valuesRead := True;
        if maxWanted then
        begin
          if fLines[lc].values[axeIx] > bestLine then
            bestLine := fLines[lc].values[axeIx];
        end
        else
        if fLines[lc].values[axeIx] < bestLine then
          bestLine := fLines[lc].values[axeIx];
      end;

    // populates result
    if not (valuesRead) then
      Exit;                           // No line has values, get "outta here"
    SetLength(Result, 0);                                    // If still there, resize the result, to ease next step.
    for lc := 0 to fLinesCount - 1 do
    begin
      if fLines[lc].values[axeIx] = bestLine then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[length(Result) - 1] := lc;
      end;
    end;

    if Length(Result) = 0 then                              // useless (?)
    begin
      SetLength(Result, 1);
      Result[0] := -1;
    end;
  except;
  end;
end;


procedure TplSpiderGraph.ResetLinesAppearance;
var
  i: integer;
begin
  for i := 0 to self.fLinesCount - 1 do
  begin
    fLines[i].fPenWidth := fLines[i].fPenWidthMem;
    fLines[i].fShowPoints := fLines[i].fShowPointsMem;
    fLines[i].fColor := fLines[i].fColorMem;
    fLines[i].fShow := True;                          // Anycase ...
  end;
end;


procedure TplSpiderGraph.FlashLine(targets: TIntegerDynArray);
// "Flashes" the line(s). At end, they are in their original state.
var
  i, lc, tgtCount: integer;
  gtc: cardinal;
begin
  if Length(targets) < 1 then
    exit;
  tgtCount := Length(targets) - 1;
  for i := 0 to 7 do
  begin
    for lc := 0 to tgtCount do
      fLines[targets[lc]].fColor := highlightColor;
    gtc := GetTickCount + 50;
    Invalidate;
    repeat
      Application.ProcessMessages;
    until
      GetTickCount > gtc;

    for lc := 0 to tgtCount do
      fLines[targets[lc]].fColor := fLines[targets[lc]].fColorMem;
    gtc := GetTickCount + 50;
    Invalidate;
    repeat
      Application.ProcessMessages;
    until
      GetTickCount > gtc;
  end;
end;


procedure TplSpiderGraph.HighlightLineByCrit(criteria: integer; maxWanted: boolean = True);
// rq :  Const HC_NONE=-1; HC_AREA=-2;
// => :  -2  <  criteria  <  self.fAxesCount-1 ;
var
  arBests: TIntegerDynArray;
begin
  if (criteria < HC_AREA) or (criteria > fAxesCount - 1)      // axes indexes are ZERO-BASED.
  then
    criteria := HC_NONE;
  SetLength(arBests, 0);
  ResetLinesAppearance;
  case criteria of
    HC_NONE:
    begin
      Invalidate;
      Exit;
    end;
    HC_AREA: arBests := GetBestLineByArea(maxWanted);
    else
      arBests := GetBestLineByAxe(criteria, maxWanted);
  end; {case}

  if arBests[0] <> -1 then
    Self.HighlightLineByIndex(arBests);
end;


procedure TplSpiderGraph.HighlightLineByIndex(index: integer);
var
  wArr: TIntegerDynArray;
begin
  SetLength(wArr, 1);
  wArr[0] := index;
  self.HighlightLineByIndex(wArr);
end;

procedure TplSpiderGraph.HighlightLineByIndex(indexArray: TIntegerDynArray);
var
  i, wCnt: integer;
  wArr: TIntegerDynArray;
begin
  try
    ResetLinesAppearance();
    if (Length(indexArray) = 0) or (Length(indexArray) > fLinesCount) then
      exit;
    // Rejects invalid indexes. As long as ResetLinesAppearance(); has been called,
    // an eventual reset request (throught (index=-1) ) has soon been applied.
    // We can then discard any index outside the 0..flinesCount-1 range
    wCnt := 0;
    SetLength(wArr, length(indexArray));
    for i := 0 to length(indexArray) - 1 do
      if (indexArray[i] >= 0) and ((indexArray[i] < fLinesCount)) then
      begin
        wArr[wCnt] := indexArray[i];
        Inc(wCnt);
      end;
    if wCnt = 0 then
      exit
    else
      SetLength(wArr, wCnt);
    Dec(wCnt);                                                // will then be used bellow as an <=> for "length(wArr)-1"
    if hmShowPoints in self.HighlightMode then
    begin
      for i := 0 to fLinesCount - 1 do
        fLines[i].fShowPoints := False;
      for i := 0 to wCnt do
        fLines[wArr[i]].fShowPoints := True;
    end;
    if hmWidened in self.HighlightMode then
    begin
      for i := 0 to fLinesCount - 1 do
        fLines[i].fPenWidth := self.fDfltPenWidth;
      for i := 0 to wCnt do
        fLines[wArr[i]].fPenWidth := 2 * self.fDfltPenWidth;
    end;

    // Then, the eventual flashing, but not in the loop;
    if hmFlashLine in self.HighlightMode then
      FlashLine(indexArray);
    // Then the color, if requested;
    for i := 0 to wCnt do
      if hmColorLine in self.HighlightMode then
        fLines[wArr[i]].fColor := highlightColor;
    // finaly, repaint the whole thing.
  finally
    Invalidate;
  end;
end;


end.
