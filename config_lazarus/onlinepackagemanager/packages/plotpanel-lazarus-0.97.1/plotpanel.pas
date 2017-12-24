unit Plotpanel;

{$mode objfpc}{$H+}
{
 *
 *  File    :		 PlotPanel.pas (version 0.97)
 *  Purpose :		 A Plot component for Lazarus to replace
 *			         more or less the Chart-component known from Delphi
 *  Author  :		 Marien van Westen <m.c.van.westen@pl.hanze.nl>
 *  Created :		 Development started: 25 nov 2001  (for Delphi)
 *  Modified:	   November 2008
 *  Description: Playing with Delphi Open Edition I discovered that a
 *		           lot of my programs used the Chart-component wich isn't
 *		           included in the Delphi Open Edition.
 *		           Inspired by the MathPanel component by Mirko Patrizi I
 *		           started writing this component.
 *		           This component fits perfectly with Delphi.
 *		           It allows up to 8 layers of graphs, and by using the
 *		           Freeze-method it is possible to produce flickerfree
 *		           animated plots.
 *
 *		 
 *  Copyright (C) 2008	Marien van Westen
 *
 *  You are free to use and modify this code under the terms of
 *  the GNU Lesser General Public Licence version 3 or later.
 *  
 *
 Version 0.91
 - added
     property PlotBMP (thank you  Dr. Luis A. Fernández)
 Version 0.95
 - added
    draw labels at X and Y axis
    cured several bugs
    changed license from GPL to LGPL
 Version 0.96
 - added
    ClearData of single layer
    destroy Xmarksfont and Ymarksfont to prevent memory leak (thank you Jorge Solla)
    OnXMarksWrite and OnYMarksWrite events (see demo for its use. Also thanks to Jorge Solla)
 Version 0.97
 - Some small changes to make ik work with Lazarus 09.28.x	
 }



interface

  uses Interfaces, LResources, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, Math;

type
  TPlotMode = ( pmDot, pmLine, pmBar );

  Txyc = record
    x : Extended;
    y : Extended;
    c : TColor;
  end;

  TLayerOpt = record
     PlotMode :TPlotMode;
     PenWidth :Integer;
  end;

  // Axis draw event
  TWriteAxisEvt = procedure(Sender: TCustomPanel; Value:extended;var AxisText:String; var AFont:TFont; var EventHandled:boolean) of object;

  TPlotPanel = class(TCustomPanel)
  private
    // ***************************************************************
    //		PRIVATE VARIABLES
    // ***************************************************************
    LastXpixDrawn : Array [0..7] of Extended;
    LastYpixDrawn : Array [0..7] of Extended;
    //
    FOnXMarksWrite: TWriteAxisEvt;  // X Axis draw event
    FOnYMarksWrite: TWriteAxisEvt;  // Y Axis draw event
    //
    Xqnt : Extended;
    Yqnt : Extended;
    R2Width : integer;
    R2Height : integer;
    //
    fBackColor : Tcolor;
    fGridColor : Tcolor;
    //
    fMargin : integer;
    fMarginTop : integer;
    fMarginLeft : integer;
    fMarginRight:Integer;
    fMarginBottom: Integer;

    fYStart : Extended;
    fXStart : Extended;
    //
    fXMin : Extended;
    fXMax : Extended;
    fXMarks : Boolean;
    fXMarksFont : TFont;
    fXLog : Boolean;
    fXInterval : Extended;
  	fXLabel : String;
    //
    fYMin : Extended;
    fYMax : Extended;
    fYMarks : Boolean;
    fYMarksFont : TFont;
    fYLog : Boolean;
    fYInterval : Extended;
  	fYLabel: String;
    //
    fPlotMode : TPlotMode;
    fPlotPen : TPen;
    fRPlot, fRBorder : Trect;
    fClr : Boolean;
    fDataLength : Array [0..7] of Integer ;
    Datarray : Array[0..7] of Array of Txyc;
    fPlot : TBitmap;
    fBorder:TBitmap;
    fHeight : Integer;
    fFreeze : Boolean;
    FTitle : String;
    FLayerOpt : Array [0..7] of TLayerOpt;
    FLayerOptions : Boolean;
    // ***************************************************************
    //		ACCESS METHODS
    // ***************************************************************
    Procedure SetBackColor( value : Tcolor );
    Procedure SetGridColor( value : Tcolor );
    //
    Procedure SetMargin( value : integer );
    //
    Procedure SetXMin( value : Extended );
    Procedure SetXMax( value : Extended );
    Procedure SetXMarks( value : boolean );
    Procedure SetXMarksFont( value : Tfont );
    Procedure SetXInterval( value : Extended );
    Procedure SetXLog(const Value: Boolean);
   	Procedure SetXLabel(value: String);
    //
    Procedure SetYMin( value : Extended );
    Procedure SetYMax( value : Extended );
    Procedure SetYMarks( value : boolean );
    Procedure SetYMarksFont( value : Tfont );
    Procedure SetYInterval( value : Extended );
    Procedure SetYLog(const Value: Boolean);
  	Procedure SetYLabel(value: String);
    //
    Procedure SetPlotMode( value : TPlotMode );
    Procedure SetPlotPen(value : TPen);
    Procedure SetTitle(value : String);
    Procedure PlotData( x, y : Extended; colour : Tcolor;Layer:Integer );
    //
    procedure DrawHorizontalGridLinesLog(ACanvas:TCanvas);
    procedure DrawVerticalGridLines(ACanvas:TCanvas);	     // Y-axis (vertical) lines
    procedure DrawVerticalGridLinesLog(ACanvas:TCanvas);
    procedure DrawHorizontalGridLines(ACanvas:TCanvas);      // X-axis (horizontal) lines
    procedure WriteXaxisValues(ACanvas:TCanvas);
    procedure WriteYaxisValues(ACanvas:TCanvas);
    procedure WriteXaxisLogValues(ACanvas:TCanvas);
    procedure WriteYaxisLogValues(ACanvas:TCanvas);
    //
    Procedure AdjustR2Dimension;
    procedure Teken(Sender:TObject);
    Procedure SetLayerOptions(value:Boolean);
   // procedure VerticalTextOut(x,y:Integer;CanvasText:String;TextCanvas:TCanvas);

  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create( aOwner : Tcomponent ); override;
    Destructor Destroy; override;

    procedure Paint; override;
    //
    Procedure ClearData;overload;
	Procedure ClearData(Layer:Integer);overload;
    Procedure AddXY( x, y : Extended; colour : Tcolor; Layer:Integer );overload;
    Procedure AddXY( x, y : Extended); overload;
    Procedure AutoScale(Layer:Integer);
    Procedure Freeze(_Boolean:Boolean);
    Procedure HideLayer(Layer:Integer);
    Procedure UnHideLayer(Layer:Integer);
    Function ConvertS2W(X,Y : Integer; var WX,WY : Extended): Boolean;
    Procedure LayerOptions(Layer:Integer;PlotMode:TPlotMode;PenWidth:Integer);
  published
    { Published declarations }
    Property GridColor : Tcolor read fGridColor write SetGridColor;
    Property BackColor : Tcolor read fBackColor write SetBackColor;
    Property Margin : integer read fMargin write SetMargin;
    //
    Property PlotMode : TPlotMode read fPlotMode write SetPlotMode;
    Property PlotPen : TPen read fPlotPen write SetPlotPen;
    Property Title : String read FTitle write SetTitle;
    Property LayerOption : Boolean read FLayerOptions write SetLayerOptions;
    Property PlotBMP : TBitmap read fBorder;
    Property XMin : Extended read fXMin write SetXMin;
    Property XMax : Extended read fXMax write SetXMax;
    property XScaleLog : Boolean read fXLog write SetXLog;
    Property XMarks : boolean read fXMarks write SetXMarks;
    Property XMarksFont : Tfont read fXMarksFont write SetXMarksFont;
    Property XInterval : Extended read fXInterval write SetXInterval;
    Property XLabel: String read fXLabel write SetXLabel;
    //
    Property YMin : Extended read fYMin write SetYMin;
    Property YMax : Extended read fYMax write SetYMax;
    property YScaleLog : Boolean read fYLog write SetYLog;
    Property YMarks : boolean read fYMarks write SetYMarks;
    Property YMarksFont : Tfont read fYMarksFont write SetYMarksFont;
    Property YInterval : Extended read fYInterval write SetYInterval;
    Property YLabel: String read fYLabel write SetYLabel;
    //
    property Align;
    //property Alignment;
    property Anchors;
    property AutoSize;
   // property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
   // property ChildSizing;
   // property ClientHeight;
   // property ClientWidth;
    property Color;
    property Constraints;
    //property DragMode;
    property Enabled;
    property Font;
    //property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //property TabOrder;
    //property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    //property OnStartDrag;

     // Axis drawing events
    property OnXMarksWrite: TWriteAxisEvt read FOnXMarksWrite write FOnXMarksWrite;
    property OnYMarksWrite: TWriteAxisEvt read FOnYMarksWrite write FOnYMarksWrite;

  end;

procedure Register;

implementation

//////////////////////////////////////////////////////////////////////////////
//
//     X - Axis
//
/////////////////////////////////////////////////////////////////////////////



Procedure TPlotPanel.SetXMin( value : Extended );
begin
     fXMin := value;
     if fXMin>=fXMax then fXMax:=(fXMin+1);
     Teken(nil);
end;

//
Procedure TPlotPanel.SetXMax( value : Extended );
begin
     fXMax := value;
     if fXMin>=fXMax then fXMin:=(fXMax-1);
     Teken(nil);
end;

//
Procedure TPlotPanel.SetXMarks( value : boolean );
begin
     fXMarks := value;
     Teken(nil);
end;

//
Procedure TPlotPanel.SetXMarksFont( value : Tfont );
begin
     fXMarksFont.Assign( value );
     Teken(nil);
end;

//
Procedure TPlotPanel.SetXInterval( value : Extended );
begin
  if value=0 then value:=1;
  fXInterval:=value;
  Teken(nil);
end;


procedure TPlotPanel.SetXLog(const Value: Boolean);
begin
  fXLog:=Value;
  Teken(nil);
end;

Procedure TPlotPanel.SetXLabel( value : String );
begin
     fXLabel := value ;
     Teken(nil);
end;


//////////////////////////////////////////////////////////////////////////////
//
//     Y - Axis
//
/////////////////////////////////////////////////////////////////////////////



Procedure TPlotPanel.SetYMin( value : Extended );
begin
     fYMin := value;
     if fYMin>=fYMax then fYMax:=(fYMin+1);
     Teken(nil);
end;

//
Procedure TPlotPanel.SetYMax( value : Extended );
begin
     fYMax := value;
     if fYMin>=fYMax then fYMin:=(fYMax-1);
     Teken(nil);
end;

//
Procedure TPlotPanel.SetYMarks( value : boolean );
begin
     fYMarks := value;
     Teken(nil);
end;

//
Procedure TPlotPanel.SetYMarksFont( value : Tfont );
begin
     fYMarksFont.Assign( value );
     Teken(nil);
end;

//
Procedure TPlotPanel.SetYInterval( value : Extended );
begin
  if value=0 then value:=1;
  fYInterval:=value;
  Teken(nil);
end;


procedure TPlotPanel.SetYLog(const Value: Boolean);
begin
  fYLog:=Value;
  Teken(nil);
end;

Procedure TPlotPanel.SetYLabel( value : String );
begin
     fYLabel := value ;
     Teken(nil);
end;

//////////////////////////////////////////////////////////////////////////////
//
//   PlotPanel	Create and Destroy
//
/////////////////////////////////////////////////////////////////////////////


// 
Constructor TPlotPanel.Create( aOwner :TComponent);
var
  i : Integer;
begin
     inherited Create( AOwner );
     Parent:=TWinControl(AOwner);
     // Global panel default settings
     Height :=200;
     Width  :=300;
     Caption :=' ';
     // Graph-backgound and axis colors
     fBackColor := clWhite;
     fGridColor := clBlack;
     fPlotMode:=pmLine;
     // Default distances between graph-edges and panel borders  
     fMargin := 15;
     fMarginTop := fMargin;
     fMarginLeft:= fMargin;
     fMarginBottom:=fMargin;
     fMarginRight:=fMargin;
     // Numerical starting ranges
     fXMin := 0;
     fXMax := 10;
     fYMin := -1;
     fYMax := 1;
     fXLog:=False;
     fYLog:=False;
     // Values referred to the axis aspects
     fXMarks := false;
     fXMarksFont := Tfont.Create;
     fXInterval := 1;
     fYMarks := false;
     fYMarksFont := Tfont.Create;
     fYInterval := 1;
     fXMarksFont.OnChange:=@Teken;
     FYMarksFont.OnChange:=@Teken;
     //
     fPlotPen:=TPen.Create;
     fPlotPen.OnChange:=@Teken;
     fLayerOptions:=False;

     //
     for i:=0 to 7 do
       begin
       fLayerOpt[i].PlotMode:=fPlotMode;
       fLayerOpt[i].PenWidth:=1;
       fDataLength[i]:=1;
       SetLength(Datarray[i],1);
       end;
     fPlot:=TBitmap.Create;
     fBorder:=TBitmap.Create;
     Teken(nil);
end;


// 
Destructor TPlotPanel.Destroy;
begin
   if assigned (FXMarksFont) then
   begin
     fXMarksFont.Destroy;
     fXMarksFont:=nil;
   end;

   if assigned (FYMarksFont) then
   begin
     fYMarksFont.Destroy;
     fYMarksFont:=nil;
   end;

  if assigned (fPlotPen) then
  begin
	   fPlotPen.Free;
	   fPlotPen:=nil;
  end;
     //
     if assigned (fPlot) then
     begin
	fPlot.Free;
	fPlot:=nil;
     end;
     //
     if assigned (fBorder) then
     begin
	fBorder.Free;
	fBorder:=nil;
     end;
     //
     inherited Destroy;
end;


//////////////////////////////////////////////////////////////////////////////
//
//   Colors, Margins, Pen, Plotmode, Title
//
/////////////////////////////////////////////////////////////////////////////

Procedure TPlotPanel.SetBackColor( value : Tcolor );
begin
     fBackColor := value;
     Teken(nil);
end;

//
Procedure TPlotPanel.SetGridColor( value : Tcolor );
begin
     fGridColor := value;
     Teken(nil);
end;

//
Procedure TPlotPanel.SetMargin( value : integer );
begin
     fMargin := value;
     Teken(nil);
end;


Procedure TPlotPanel.SetPlotPen( value : TPen );
begin
     fPlotPen.Assign( value );
     Teken(nil);
end;

//
Procedure TPlotPanel.SetTitle( value : String );
begin
     FTitle := value ;
     Teken(nil);
end;

//
Procedure TPlotPanel.SetPlotMode( value : TPlotMode );
begin
     fPlotMode := value;
     Teken(nil);
end;

//
Procedure TPLotPanel.SetLayerOptions(value:Boolean);
begin
 if value=false then fLayerOptions:=False;
end;



//////////////////////////////////////////////////////////////////////////////
//
//   Linear Grid Lines
//
/////////////////////////////////////////////////////////////////////////////



procedure TPlotPanel.DrawVerticalGridLines(ACanvas:TCanvas);
var
 X : Extended;
begin
// Y-axis (vertical) lines
     if ((fXMax-fXMin)/fXInterval)>(Width/8) then
       fXInterval:=8*(fXMax-fXMin)/Width;
     FXstart:=fXInterval*Round(0.4999+(fXMin/fXInterval));
     X:=FXstart;
     repeat

       ACanvas.moveTo( Round((X-fXMin)*Xqnt)+FMarginLeft,
			    fMarginTop);
       ACanvas.lineto( Round((X-fXMin)*Xqnt)+FMarginLeft,
			    height-fMarginBottom);
       X:=X +fXInterval;
     until X>fXMax*1.000001;  // something goes wrong with rounding ???
end;

procedure TPlotPanel.DrawHorizontalGridLines(ACanvas:TCanvas);
var
 Y : Extended;
begin
  // X-axis (horizontal) lines
     if ((fYMax-fYMin)/fYInterval)>(Height/8) then
       fYInterval:=8*(fYMax-fYMin)/Height;
     FYstart:=fYInterval*Round(0.4999+(fYMin/fYInterval));
     Y:=FYstart;
     repeat
       ACanvas.moveTo( fMarginLeft ,
		       Height-FMarginBottom-Round((Y-fYMin)*Yqnt));
       ACanvas.lineto( width -fMarginRight ,
		       Height-FMarginBottom-Round((Y-fYMin)*Yqnt));
       Y:=Y +fYInterval;
     until Y>fYMax*1.000001;
end;


//////////////////////////////////////////////////////////////////////////////
//
//   Logaritmic axis
//
/////////////////////////////////////////////////////////////////////////////



procedure TPlotPanel.DrawHorizontalGridLinesLog(ACanvas:TCanvas);
var
 Y, YS, F : Extended;
 i : Integer;
begin
// Horizontal grid lines logaritmic
      // Startingpoint at power of 10
     YS :=20;
     if fYMax<=0 then fYMax:=1;
     if fYMin<=0 then fYMin:=0.1;
     repeat
       YS:=YS-1;
     until ((YS<=log10(fYMin)) or (YS<-20));

      Y:=power(10,YS);
      F:=Y;

     repeat
       i:=0;
       repeat
       inc(i);
       if (log10(fYMax)-log10(fYMin)>4) then inc(i);
       if Y>=fYMin then
	 begin
	 ACanvas.moveTo( FMarginLeft, height-fMarginBottom-Round((log10(Y)-log10(fYMin))*Yqnt));
	 ACanvas.lineto( width-FMarginRight, height-fMarginBottom-Round((log10(Y)-Log10(fYMin))*Yqnt));
	 end;
       Y:=f*i;
       if (log10(fYMax)-log10(fYMin)<0.64) then Y:=f*(1+i/2);
       if (log10(fYMax)-log10(fYMin)<0.45) then Y:=f*(1+i/5);
       if (log10(fYMax)-log10(fYMin)<0.302) then Y:=f*(1+i/10); // oktaaf
       if (log10(fYMax)-log10(fYMin)<0.15) then Y:=f*(1+i/20);
     //  if (log10(fXMax)-log10(fXMin)>4) then i:=i+2 else inc(i);
       until (Y>=10*f) or (Y>fYMax);
       f:=f*10;
       Y:=f;
     until (Y>fYMax);
end;

procedure TPlotPanel.WriteYaxisLogValues(ACanvas:TCanvas);
var
 Y ,YS, F : Extended;
 YStr : string;
 i, NewPos, LastPos :Integer;
 evtHandled : Boolean;
 MyFont : TFont;
begin
   // ***********************
   // WRITES Y AXIS VALUES	 Logaritmic!!!!

   // Sets Y-axis color and font
   ACanvas.Brush.Color := Color;
   ACanvas.Font.Assign( fYMarksFont );
   MyFont:=TFont.Create;
   MyFont.Assign(fYMarksFont);
   LastPos:=0;
   // Startingpoint at power of 10
     YS :=20;
     repeat
       YS:=YS-1;
     until (YS<=log10(fYMin)) or (YS<-20);

   Y:=power(10,YS);
   F:=Y;
   repeat
      evtHandled:=False;
         // Handle custom drawing
         if assigned(FOnYMarksWrite) then
         begin
              // Call user handler
              FOnYMarksWrite(self,Y,YStr,MyFont,evtHandled);
              ACanvas.Font.Assign(MyFont);
         end;

         // If not handled by user then draw default
         if (evtHandled=False) then
         begin
           if Y<1000
              then YStr := FormatFloat('0.##', Y)
              else YStr := FormatFloat('0.##e-0', Y);
         end;

     if (Y>=fYMin) then
	   ACanvas.TextOut( fMarginLeft-ACanvas.TextWidth(Ystr)-4,
		      -3+Height-FMarginBottom-ACanvas.TextHeight(Ystr) div 2-Round((log10(Y)-log10(fYMin))*Yqnt),
		      YStr );

     i:=0;
     repeat
       inc(i);
       if (log10(fYMax)-log10(fYMin)>4) then inc(i);
       Y:=F*i;
       if (log10(fYMax)-log10(fYMin)<0.64) then y:=f*(1+i/2);
       if (log10(fYMax)-log10(fYMin)<0.45) then Y:=f*(1+i/5);
       if (log10(fYMax)-log10(fYMin)<0.302) then Y:=f*(1+i/10); // oktaaf
       if (log10(fYMax)-log10(fYMin)<0.15) then Y:=f*(1+i/20);
      // x:=f*i;
       if Y<1000
       then YStr := FormatFloat('0.##', Y)
       else YStr := FormatFloat('0.##e-0', Y);

       NewPos:= Round((log10(Y)-log10(fYMin))*Yqnt);
       if (Y>=fYMin)and (Y<fYMax) and(NewPos-LastPos>ACanvas.TextHeight(Ystr))and (not odd(i)) then
	  begin
	  ACanvas.TextOut( fMarginLeft-ACanvas.TextWidth(Ystr)-4,
		      -3+Height-FMarginBottom-ACanvas.TextHeight(Ystr) div 2-NewPos,
		      YStr );
    LastPos:=NewPos;
	  end;

     until (Y>=10*f) or (log10(Y)>=log10(fYMax));
     f:=f*10;
     Y:=f;
     until log10(Y)>=log10(fYMax);
     MyFont.Free;
end;
//

//
procedure TPlotPanel.DrawVerticalGridLinesLog(ACanvas:TCanvas);
var
 X, XS, F : Extended;
 i,n : Integer;
begin
// Y-axis (vertical) lines logaritmic
      // Startingpoint at power of 10
     XS :=20;
     if fXMax<=0 then fXMax:=1;
     if fXMin<=0 then fXMin:=0.1;
     repeat
       XS:=XS-1;
     until ((XS<=log10(fXMax)) or (XS<-20));
      XS:=XS+1;
      x:=power(10,XS);
       f:=x;
     repeat
       i:=0;
       n:=5;
       if (log10(fXMax)-log10(fXMin)<4) then n:=10;
       if (log10(fXMax)-log10(fXMin)<0.6) then n:=40;
       if (log10(fXMax)-log10(fXMin)<0.5) then n:=100;
       if (log10(fXMax)-log10(fXMin)<0.4) then n:=200;
       i:=0;
       repeat
       x:=(n-i)*f/n;
       if x>fXMin then
	 begin
	 ACanvas.moveTo( Round((log10(X)-log10(fXMin))*Xqnt)+FMarginLeft, fMarginTop);
	 ACanvas.lineto( Round((log10(X)-Log10(fXMin))*Xqnt)+FMarginLeft, height-fMarginBottom);
	 end;
       inc(i)
       until (i=n) or (X<fXMin);
       f:=f/10;
       x:=f;
     until (X<fXMin);
end;

procedure TPlotPanel.WriteXaxisLogValues(ACanvas:TCanvas);
var
 X ,XS, F : Extended;
 XStr : string;
 i,n, NewPos, LastPos :Integer;
 evtHandled : Boolean;
 MyFont : TFont;
begin
   // ***********************
   // WRITES X AXIS VALUES	 Logaritmic!!!!

   // Sets X-axis color and font
   ACanvas.Brush.Color := Color;
   ACanvas.Font.Assign( fXMarksFont );
   MyFont:=TFont.Create;
   MyFont.Assign(fXMarksFont);
   LastPos:=Self.Width;
   // Startingpoint at power of 10
     Xs :=20;
     repeat
       XS:=XS-1;
     until (XS<=log10(fXMax)) or (XS<-20);
     XS:=XS+1;
   x:=power(10,XS);
   f:=x;
   repeat
      evtHandled:=False;
      // Handle custom drawing
      if assigned(FOnXMarksWrite) then
         begin
         // Call user handler
            FOnXMarksWrite(self,X,xStr,MyFont,evtHandled);
            ACanvas.Font.Assign(MyFont);
         end;

      // If not handled by user then draw default
      if (evtHandled=False) then
         begin
         if X<1000
	          then XStr := FormatFloat('0.##', X)
	          else XStr := FormatFloat('0.##e-0', X);
         end;

     n:=5;
     if (log10(fXMax)-log10(fXMin)<4) then n:=10;
     if (log10(fXMax)-log10(fXMin)<0.6) then n:=40;
     if (log10(fXMax)-log10(fXMin)<0.5) then n:=100;
     if (log10(fXMax)-log10(fXMin)<0.4) then n:=200;
     i:=0;
     repeat
       x:=(n-i)*f/n;
       if X<1000
          then XStr := FormatFloat('0.##', X)
          else XStr := FormatFloat('0.##e-0', X);

       NewPos:= Round((log10(X)-(log10(fXMin)))*Xqnt);
       if (x>=fXMin) and (x<fXMax) and (NewPos+1+((ACanvas.TextWidth(Xstr) div 2))<LastPos) and (not odd(i)) then
	  begin
	  ACanvas.TextOut((-ACanvas.TextWidth(Xstr) div 2)+ NewPos+FMarginLeft ,
				       height - fMarginBottom +3,
				       XStr );
	  LastPos:=NewPos-1-(ACanvas.TextWidth(Xstr) div 2);
	  end;
     inc(i);
     until (i=n) or (log10(X)<=log10(fXMin));
     f:=f/10;
     x:=f;
     until X<fXMin;
    MyFont.Free;
end;

procedure TPlotPanel.WriteXaxisValues(ACanvas:TCanvas);
var
 X : Extended;
 XStr : string;
 evtHandled : Boolean;
 MyFont : Tfont;
begin
   // ***********************
   // WRITES X AXIS VALUES
   // Sets X-axis color
   ACanvas.Brush.Color := Color;
   ACanvas.Font.Assign( fXMarksFont );
   MyFont:=TFont.Create;
   MyFont.Assign(fXMarksFont);
   // Write X values
   X:=FXstart;
   repeat
     evtHandled:=False;
     // Handle custom drawing
     if assigned(FOnXMarksWrite) then
        begin
        // Call user handler
         FOnXMarksWrite(self,X,xStr,MyFont,evtHandled);
         ACanvas.Font.Assign(MyFont);
        end;
        // If not handled by user then draw default
     if (evtHandled=False) then
         begin
         if X<1000
	          then XStr := FormatFloat('0.##', X)
	          else XStr := FormatFloat('0.##e-0', X);
         end;

     ACanvas.TextOut((-ACanvas.TextWidth(Xstr) div 2)+ Round((X-fXMin)*Xqnt)+FMarginLeft ,
		      height - fMarginBottom +3,
		      XStr );
     X:=X +fXInterval;
  until X>fXMax*1.000001;
  MyFont.Free;
end;

procedure TPlotPanel.WriteYaxisValues(ACanvas:TCanvas);
var
 Y : Extended;
 YStr : string;
 evtHandled : Boolean;
 Myfont: TFont;
begin
   // ***********************
   // WRITES Y AXIS VALUES
   // Sets Y-axis color
   ACanvas.Brush.Color := Color;
   ACanvas.Font.Assign(fYMarksFont);
  // ACanvas.Font.Assign( fYMarksFont );
   MyFont:=TFont.Create;
   MyFont.Assign(fYMarksFont);
   // Writes Y values
   Y:=FYStart;
   repeat
     evtHandled:=False;
     // Handle custom drawing
     if assigned(FOnYMarksWrite) then
         begin
         // Call user handler
            FOnYMarksWrite(self,Y,YStr,MyFont,evtHandled);
            ACanvas.Font.Assign(MyFont);
         end;

    // If not handled by user then draw default
     if (evtHandled=False) then
        begin
        if Y<1000
           then YStr := FormatFloat('0.##', Y)
           else YStr := FormatFloat('0.##e-0', Y);
        end;

     ACanvas.TextOut( fMarginLeft-ACanvas.TextWidth(Ystr)-4,
	      -3+Height-FMarginBottom-ACanvas.TextHeight(Ystr) div 2-Round((Y-fYMin)*Yqnt),
		      YStr );

     Y:=Y +fYInterval;
  until Y>fYMax*1.000001;
  MyFont.Free;
end;

//
Procedure TPlotPanel.AdjustR2Dimension;
begin
     // Width and Height (pixels) of the cartesian area
     R2Width  := width - ( fMarginLeft+fMarginRight );
     R2Height := height - ( fMarginTop+fMarginBottom );
     // Pixels per numeric unit
     if fXMax-fXMin=0 then fXMax:=fXMax+2e-16;
     if fYMax-fYMin=0 then fYMax:=fYMax+3e-16;
     if fXLog = True
	then
	  begin
	  if fXMax<=0 then fXMax:=1;
	  if fXMin<=0 then fXMin:=0.1;
	  Xqnt := R2Width / (log10(fXMax) - log10(fXMin));
	  end
	else
	  begin
	  Xqnt := R2Width / (fXMax - fXMin);
	  end;
     if fYLog = True
	then
	  begin
	  if fYMax<=0 then fYMax:=1;
	  if fYMin<=0 then fYMin:=0.1;
	  Yqnt := R2Height /(log10(fYMax) - log10(fYMin));
	  end
	else
	  begin
	  Yqnt := R2Height / (fYMax - fYMin);
	  end;
end;

//------------------------------------------------
//
//   Here the real drawing of the plot is done
//
//------------------------------------------------


//
Procedure TPlotPanel.Paint;

begin
  inherited Paint;
  Teken(nil);
end;

//
Procedure TPlotPanel.Teken(Sender:TObject);
Var
   i,j : Integer;
   x,y : Integer;
   tmp : Boolean;
   ACanvas : TCanvas;
   Bmp : TBitmap;
begin
     //
     if (csDesigning in ComponentState) then ACanvas:=Canvas
					else ACanvas:=fBorder.Canvas;
     // Adjust Margins
     fMarginRight:=fMargin;
     if Length(fTitle)<>0 then fMarginTop:=fMargin+Canvas.TextHeight(FTitle)+2
		   else fMarginTop:=fMargin;
     if fXMarks    then fMarginBottom:=fMargin+Canvas.TextHeight('0.00')+2
		   else fMarginBottom:=fMargin;
     if Length(fXLabel)<>0 then fMarginBottom:=fMarginBottom+Canvas.TextHeight(FXLabel);
     if fYMarks    then fMarginLeft:=fMargin+Canvas.TextWidth('0.000')+2
		   else fMarginLeft:=fMargin;
     if Length(fYLabel)<>0 then fMarginLeft:=fMarginLeft+Canvas.TextHeight(FYLabel);
     // Refresh proportional value
     AdjustR2Dimension;
     // Draws the window for the graph
     if ACanvas=fBorder.Canvas then
	begin
	fBorder.Width:=Width-2*BevelWidth;
	fBorder.Height:=Height-2*BevelWidth;
	end;

     fPlot.Width:=Width;
     fPlot.Height:=Height;
     // fRBorder is the whole visible area of the Panel (with Border)
     fRBorder.Left:=2*BevelWidth;
     fRBorder.Top:=2*BevelWidth;
     fRBorder.Right:=Width-2*BevelWidth;
     fRBorder.Bottom:=Height-2*BevelWidth;
     ACanvas.Brush.Color:=Color;
     ACanvas.FillRect(fRBorder);

     // fRPlot is the part where the plotting is done (without Border)
     fRPlot.Left := fMarginLeft;
     fRPlot.Top := fMarginTop;
     fRPlot.Right := Width-fMarginRight;
     fRPlot.Bottom :=Height-fMarginBottom;
     fHeight:=fRPlot.Bottom;

     ACanvas.Brush.Color := fBackColor;
     ACanvas.Pen.Color := fGridColor;
     ACanvas.Pen.Width:=1;
     ACanvas.FillRect( fRPlot );
     ACanvas.Rectangle( fRPlot);
     // Draws cartesian axis


     if fYLog=true
	then  DrawHorizontalGridLinesLog(ACanvas)	 // X-axis (horizontal) lines
	else  DrawHorizontalGridLines(ACanvas);      // X-axis (horizontal) lines
     if fXLog=true
	then  DrawVerticalGridLinesLog(ACanvas)        // Y-axis (vertical) lines
	else  DrawVerticalGridLines(ACanvas);	     // Y-axis (vertical) lines
    //
    if FTitle<>'' Then
       begin
       ACanvas.Brush.Color := Color;
       ACanvas.Font.Assign( Font); //fTitleFont );
       ACanvas.Textout((width-ACanvas.TextWidth(FTitle)) div 2,
			BevelWidth*2+fMargin div 2,
			FTitle);
       end;
    //

    If fXMarks then
       if fXLog=True
	  then WriteXaxisLogValues(ACanvas)
	  else WriteXaxisValues(ACanvas);	    // WRITES X AXIS VALUES
    if FXLabel<>'' Then
       begin
       ACanvas.Brush.Color := Color;
       ACanvas.Font.Assign(fXMarksFont); //fTitleFont );
       ACanvas.Textout((width-ACanvas.TextWidth(fXLabel)) div 2,
			height-(BevelWidth*2+ACanvas.TextHeight(fXLabel)+(fMargin div 2)),
			fXLabel);
       end;
	  
    If fYMarks then			    // WRITES Y AXIS VALUES
       if fYLog=True
	  then WriteYaxisLogValues(ACanvas)
	  else WriteYaxisValues(ACanvas);
    if fYLabel<>'' Then
       begin
       ACanvas.Brush.Color := Color;
       ACanvas.Font.Assign(fYMarksFont); //fTitleFont );
       Bmp:=Tbitmap.Create;
       try
         x:=BevelWidth*2 +(fMargin div 2);
         y:=(height-ACanvas.TextWidth(fYLabel)) div 2;
         Bmp.Canvas.Font:=fYMarksFont;
         Bmp.Width:=aCanvas.TextWidth(fYLabel);
         Bmp.Height:=aCanvas.TextHeight(fYLabel);
         Bmp.Canvas.Brush.Color:=ACanvas.Brush.Color;
         Bmp.Canvas.TextOut(0,0,fYLabel);
         for i:=0 to Bmp.Width-1 do
            for j:=0 to Bmp.Height-1 do
               ACanvas.Pixels[x+j,y+Bmp.Width-i]:=Bmp.Canvas.Pixels[i,j];
       finally
       Bmp.Free;
       end;
       end;

    if (csDesigning in ComponentState) then Exit;

    fPlot.Canvas.CopyRect(fRPlot,ACanvas,fRPlot);
    // Plot eventuele data
      tmp:=fFreeze;
      fFreeze:=true;
	for j:=0 to 7 do
	  begin
	  if fDataLength[j]>1 then
	     begin
	     fClr:=True;
	     for i:=1 to fDataLength[j]-1 do
	       PlotData(Datarray[j,i].X, Datarray[j,i].Y, Datarray[j,i].c,j);
	     end;
	   end;
     fFreeze:=tmp;
     fBorder.Canvas.CopyRect(fRPlot,fPlot.Canvas,fRPlot);
    if not fFreeze then Canvas.CopyRect(fRBorder,fBorder.Canvas,fRBorder);
  //
end;
//
// The following procedure did work in a test program, but not in PlotPanel not clear why it doesn't work
// had to put the code in the Teken routine. Not very nice

{procedure TPlotPanel.VerticalTextOut(x,y:Integer;CanvasText:String; TextCanvas:TCanvas);
var
 Bmp : TBitmap;
 i,j : Integer;
begin
  Bmp:=Tbitmap.Create;
  try
    Bmp.Canvas.Font:=TextCanvas.Font;
    Bmp.Width:=bmp.Canvas.TextWidth(Text);
    Bmp.Height:=bmp.Canvas.TextHeight(Text);
    Bmp.Canvas.Brush.Color:=TextCanvas.Brush.Color;
    Bmp.Canvas.TextOut(0,0,Text);
    for i:=0 to Bmp.Width-1 do
      for j:=0 to Bmp.Height-1 do
        TextCanvas.Pixels[x+j,y+Bmp.Width-i]:=Bmp.Canvas.Pixels[i,j];
  finally
  Bmp.Free;
  end;
end; }
//
Procedure TPlotPanel.Freeze(_Boolean:Boolean);
begin
if _Boolean then fFreeze:=true
	 else begin
	      fFreeze:=False;
	      fBorder.Canvas.CopyRect(fRPlot,fPlot.Canvas,fRPlot);
	      Canvas.CopyRect(fRBorder,fBorder.Canvas,fRBorder);
	      end;
end;

//
procedure TPlotPanel.ClearData;
var
  i : Integer;
begin
     fClr:=True;
     for i:=0 to 7 do
       begin
       fDataLength[i]:= 1;
       Datarray[i]:=Copy(Datarray[i],0,1);
       SetLength(Datarray[i],1);
       end;
     Teken(nil);
end;

//
procedure TPlotPanel.ClearData(Layer:Integer);
var
  i : Integer;
begin
     fClr:=True;
     fDataLength[Layer]:= 1;
     Datarray[Layer]:=Copy(Datarray[Layer],0,1);
     SetLength(Datarray[Layer],1);
     Teken(nil);
end;

//
Procedure TPlotPanel.AutoScale(Layer:Integer);
var
  i : Integer;
  x, y, Range,f : Extended;
begin
   if fDataLength[Layer]>2 then
	 begin
	   fXMin:=Datarray[Layer,1].X;
	   fXMax:=fXMin*1.001;
	   fYMin:=Datarray[Layer,1].Y;
	   fYMax:=fYMin*1.001;

	   if Datarray[Layer,2].X > fXMin then fXMax:=Datarray[Layer,2].x;
	   if Datarray[Layer,2].x < fXMin then begin
					   fXMax:=fXMin;
					   fXMin:=Datarray[Layer,2].x;
					 end;
	   if Datarray[Layer,2].y > fYMin then fYMax:=Datarray[Layer,2].y;
	   if Datarray[Layer,2].y < fYMin then begin
					   fYMax:=fYMin;
					   fYMin:=Datarray[Layer,2].y;
					 end;
	   for i:=3 to fDataLength[Layer]-1 do
	     begin
	     x:=Datarray[Layer,i].x;
	     y:=Datarray[Layer,i].y;
	     if x>fXMax then fXMax:=x;
	     if x<fXMin then fXMin:=x;
	     if y>fYMax then fYMax:=y;
	     if y<fYMin then fYMin:=y;
	     end;
	 if fXLog=True then
	    begin
	      fXMax:=power(10,fXMax);
	      fXMin:=power(10,fXMin);
	    end;
	 if fYLog=True then
	    begin
	      fYMax:=power(10,fYMax);
	      fYMin:=power(10,fYMin);
	    end;
	 Range:=fXMax-fXMin;
	 f:=1;
	 While Range>100 do
	    begin
	    Range:=Range/10;
	    f:=f/10;
	    end;
	 While Range<10 do
	    begin
	    Range:=Range*10;
	    f:=f*10;
	    end;
	 fXInterval:=Round(0.4999+Range/10)/f;

	 Range:=fYMax-fYMin;
	 f:=1;
	 While Range>100 do
	    begin
	    Range:=Range/10;
	    f:=f/10;
	    end;
	 While Range<10 do
	    begin
	    Range:=Range*10;
	    f:=f*10;
	    end;
	 fYInterval:=Round(0.4999+Range/10)/f;
     end;
     Teken(nil);
end ;

//
Procedure TPlotPanel.AddXY( x, y : Extended; colour : Tcolor; Layer:Integer );
begin
      if fXLog=True then if x<=0 then x:=1.e-50;
      if fXLog=True then x:=log10(x);
      if fYLog=True then if y<=0 then y:=1.e-50;
      if fYLog=True then y:=log10(y);
      Layer:=Layer and 7;
      if fDatalength[Layer]=1 then fClr:=True;
      Inc(fDataLength[Layer]);
      SetLength(Datarray[Layer],fDataLength[Layer]);
      Datarray[Layer,fDataLength[Layer]-1].X:=x;
      Datarray[Layer,fDataLength[Layer]-1].y:=y;
      Datarray[Layer,fDataLength[Layer]-1].c:=colour;
      Plotdata(x,y,colour,Layer);
      if not fFreeze then Canvas.CopyRect(fRBorder,fBorder.Canvas,fRBorder);
end;

//
Procedure TPlotPanel.AddXY( x, y : Extended);
var
  Colour: TColor;
  Layer: Integer;
begin
    if fXLog=True then if x<=0 then x:=1.e-50;
    if fXLog=True then x:=log10(x);
    if fYLog=True then if y<=0 then y:=1.e-50;
    if fYLog=True then y:=log10(y);
    Layer:=0;
    Colour:=fPlotPen.Color;
    FLayerOptions:=False;
    if fDatalength[Layer]=1 then fClr:=True;
      Inc(fDataLength[Layer]);
      SetLength(Datarray[Layer],fDataLength[Layer]);
      Datarray[Layer,fDataLength[Layer]-1].X:=x;
      Datarray[Layer,fDataLength[Layer]-1].y:=y;
      Datarray[Layer,fDataLength[Layer]-1].c:=colour;
      Plotdata(x,y,Colour,Layer);
      if not fFreeze then Canvas.CopyRect(fRBorder,fBorder.Canvas,fRBorder);
end;

//
Procedure TPlotPanel.PlotData( x, y : Extended; colour : Tcolor;Layer:Integer );
var
  Punt : TRect;
  l,b : Integer;
  s, s2 : Integer;
  XOG, YOG : Extended;
begin
      l:=fMarginLeft;
      b:=Height-fMarginBottom;
      s:=fPlotPen.Width div 2;
      s2:=fPlotPen.Width;
      if fXLog=True then XOG:=log10(fXMin) else XOG:=fXMin;
      if fYLog=True then YOG:=log10(fYMin) else YOG:=fYMin;
      fPlot.Canvas.Pen:=fPlotPen;
      fPlot.Canvas.Pen.Color:=colour;
      if FLayerOptions=True then
	 begin
	 fPlotMode:=fLayerOpt[Layer].PlotMode;
	 if fPlotMode=pmDot then
	     begin
	     s := (fLayerOpt[Layer].PenWidth) div 2 ;
	     s2:=  fLayerOpt[Layer].PenWidth;
	     end
	 else fPlot.Canvas.Pen.Width:=fLayerOpt[Layer].PenWidth;
	 end;

      Case fPlotMode of
      pmDot:
	   begin
	      fPlot.Canvas.Brush.Color := colour;
	      Punt.Left:=Round(-s+l+(x - XOG)* Xqnt);
	      Punt.Bottom:=+s+b - Round((y - YOG)* Yqnt);
	      Punt.Right:=-s+s2+l+ Round((x - XOG)* Xqnt);
	      Punt.Top:=b +s-s2 - Round((y - YOG)* Yqnt);
	      fPlot.Canvas.FillRect(Punt);
	      fPlot.Canvas.Brush.Color := fBackColor;
	   end;
      pmLine:
	   begin
	     if fClr then
		begin
		  fPlot.Canvas.MoveTo(l+Round((x - XOG)* Xqnt),
				      b - Round((y - YOG)* Yqnt));
		  fClr:=False;
		end
		else
		begin
		fPlot.Canvas.MoveTo( Round(LastXpixDrawn[Layer]),
				     Round(LastYpixDrawn[Layer]));
		fplot.Canvas.LineTo(l+Round((x - XOG)* Xqnt) ,
				    Round(b-(y-YOG)* Yqnt));
		end;
	     LastXpixDrawn[Layer] :=l+(x - XOG)* Xqnt;
	     LastYpixDrawn[Layer] :=b -(y - YOG)* Yqnt;
	   end; //pmLine
      pmBar:
	   begin
	      fPlot.Canvas.MoveTo(l+Round((x - XOG)* Xqnt) ,
				  b - Round(-YOG* Yqnt));
	      fPlot.Canvas.LineTo(l+Round((x - XOG)* Xqnt),
				  b - Round((y-YOG)*Yqnt));
	   end;
       end; // case
     if not fFreeze then fBorder.Canvas.CopyRect(fRPlot,fPlot.Canvas,fRPlot);
end;

//
Procedure TPlotPanel.HideLayer(Layer:Integer);
begin
  Layer:=Layer and 7;
  fDataLength[Layer]:= 1;
  Teken(nil);
end;

//
Procedure TPlotPanel.UnHideLayer(Layer:Integer);
begin
  Layer:=Layer and 7;
  fDataLength[Layer]:= Length(Datarray[Layer]);
  Teken(nil);
end;

//
Function TPlotPanel.ConvertS2W(X,Y : Integer; var WX,WY : Extended): Boolean;
begin
 Result:=True;
 X:=X-fMarginLeft;
 Y:=Height-fMarginBottom-Y;
 if fXLog=true
    then WX:=power(10,X/Xqnt)*fXMin
    else WX:=fXMin + X/Xqnt;
 if fYLog=true
    then WY:=power(10,Y/Yqnt)*fYMin
    else WY:=fYMin + Y/Yqnt;
 if WX<fXMin then Result:=False;
 if WX>fXMax then Result:=False;
 if WY<fYMin then Result:=False;
 if WY>fYMax then Result:=False;
end;

Procedure TPlotPanel.LayerOptions(Layer:Integer;PlotMode:TPlotMode;PenWidth:Integer);
begin
 FLayerOptions:=True;
 Layer:=Layer and 7;
 fLayerOpt[Layer].PlotMode:=Plotmode;
 fLayerOpt[Layer].PenWidth:=PenWidth;
end;

//
procedure Register;
begin
  RegisterComponents('Extra', [TPlotPanel]);
end;

initialization
{$I plotpanel.lrs}


end.





