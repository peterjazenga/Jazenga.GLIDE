{
 /***************************************************************************
                               RingChart.pp
                               --------
                 Component Library Extended Controls

 ***************************************************************************/

 *****************************************************************************
 * These files are distributed under the Library GNU General Public License  *
 * (see the file COPYING.LPGL) with the following modification:              *
 *                                                                           *
 * As a special exception, the copyright holders of this library give you    *
 * permission to link this library with independent modules to produce an    *
 * executable, regardless of the license terms of these independent modules, *
 * and to copy and distribute the resulting executable under terms of your   *
 * choice, provided that you also meet, for each linked independent module,  *
 * the terms and conditions of the license of that module. An independent    *
 * module is a module which is not derived from or based on this library. If *
 * you modify this library, you may extend this exception to your version of *
 * the library, but you are not obligated to do so. If you do not wish to do *
 * so, delete this exception statement from your version.                    *
 *                                                                           *
 * If you didn't receive a copy of the file COPYING.LGPL, contact:           *
 *     Free Software Foundation, Inc.,                                       *
 *     675 Mass Ave                                                          *
 *     Cambridge, MA  02139                                                  *
 *     USA                                                                   *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Salvatore Coppola
  N.B. Structured like the LCL TChart
}

unit RingChart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LCLProc, LCLIntf, LCLType, Controls, Graphics, Dialogs,
  LResources, GraphMath, ExtCtrls;

type

  TLegendPosition=(lpInside, lpOutside, lpNone);
  TKindLabel=(klPercentage, klValue, klBoth, klNone);
  
  TSector =class(TCollectionItem)
  public
    FSName: string;
    FValue: double;
    FColor: TColor;
  end;

  { TCustomRingChart }
  
  TCustomRingChart = class(TGraphicControl)
  private
    bx,by:integer;
    FUpdateCount: Integer;
    FSectors: TCollection;
    FDepth: byte;
    FLegend:TLegendPosition;
    FKindofLabel:TKindLabel;
    FRing:boolean;
    procedure SetDepth(Value:byte);
    procedure SetLegend(Value:TLegendPosition);
    procedure SetLabel(Value:TKindLabel);
    procedure SetRing(Value:boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function AddSector(const SName: string; Value: double; AColor: TColor): TSector;
    function GetSector(SId: integer): TSector;
    function ChangeSector(SId: integer; const SName: string; Value: double; AColor: TColor):boolean;
    function DeleteSector(SId: integer):boolean;
    function SectorCount: Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Depth: byte read FDepth write SetDepth default 5;
    property Legend: TLegendPosition read FLegend write SetLegend;
    property KindofLabel: TKindLabel read FKindofLabel write SetLabel;
    property Ring:boolean read FRing write SetRing;
  end;


  { TRingChart }

  TRingChart = class(TCustomRingChart)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses Ellipse;

procedure Register;
begin
  RegisterComponents('Misc',[TRingChart]);
end;

procedure TCustomRingChart.SetDepth(Value:byte);
begin
  if FDepth=Value then exit;
  FDepth:=Value;
  Invalidate;
end;

procedure TCustomRingChart.SetLegend(Value:TLegendPosition);
begin
  if FLegend=Value then exit;
  FLegend:=Value;
  Invalidate;
end;

procedure TCustomRingChart.SetLabel(Value:TKindLabel);
begin
  if FKindofLabel=Value then exit;
  FKindofLabel:=Value;
  Invalidate;
end;

procedure TCustomRingChart.SetRing(Value:boolean);
begin
  if FRing=Value then exit;
  FRing:=Value;
  Invalidate;
end;

constructor TCustomRingChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WIN32}
  bx:=GetSystemMetrics(SM_CXEDGE);
  by:=GetSystemMetrics(SM_CYEDGE);
  {$ELSE WIN32}
  bx:=2;//GetSystemMetrics(SM_CXEDGE);
  by:=2;//GetSystemMetrics(SM_CYEDGE);
  {$ENDIF WIN32}
  FSectors:=TCollection.Create(TSector);
  FDepth:=5;
  FLegend:=lpInside;
  FKindofLabel:=klPercentage;
  FRing:=true;
  SetInitialBounds(0,0,150,120);
end;

destructor TCustomRingChart.Destroy;
begin
  FSectors.Destroy;
  inherited Destroy;
end;

function TCustomRingChart.AddSector(const SName: string; Value: double;
  AColor: TColor): TSector;
begin
  BeginUpdate;
  Try
    Result:=TSector(FSectors.Add);
    Result.FsName:=SName;
    Result.FValue:=Value;
    Result.FColor:=AColor;
  finally
    EndUpdate;
  end;
end;

function TCustomRingChart.ChangeSector(SId: integer; const SName: string; Value: double; AColor: TColor):boolean;
var Sect:TSector;
begin
  Result:=false;
  Sect:=GetSector(SId);
  if Sect=nil then exit;
  Sect.FSName:=SName;
  Sect.FValue:=Value;
  Sect.FColor:=AColor;
  Invalidate;
  Result:=true;
end;

function TCustomRingChart.DeleteSector(SId: integer):boolean;
begin
  Result:=false;
  if SId>SectorCount-1 then exit;
  FSectors.Delete(SId);
  Invalidate;
  Result:=true;
end;

function TCustomRingChart.GetSector(SId: integer): TSector;
begin
  if SId<=SectorCount-1 then
    Result:=TSector(FSectors.Items[SId])
  else
    Result:=nil;
end;

procedure TCustomRingChart.Paint;
const sx=16; sy=8;
      rhoLabel=0.95;
      dalfa=15;
var a,b:integer;//  x^2/a^2+y^2/b^2=1
    ahole,bhole:integer;//  x^2/a^2+y^2/b^2=1
    ts: TSector;
    i,roundtmp1,roundtmp2: integer;
    ListSectors:TStringList;
    strPer100_Angolo, strItemLabel:string;
    Per100:string;
    AngoloF,AngoloInizialeF:double;
    NameWidth, LabelHeigth, LegendWidth, ItemWidth:integer;
    p0:TPoint;
    Teta_e,ro_e:double;//_e sta per ellisse;
    Rect:TRect;
    p100:double;
    CenterEllipsePoint,StartPoint,EndPoint:TFloatPoint;
    OtherList:TStringList;

procedure GetSectorsFan(Sectors:TCollection;var Result:TStringList);
var totale:double;
    i:integer;
    ts: TSector;
begin
  Result.Clear;
  totale:=0;
  for i:=0 to Sectors.Count-1 do begin
    ts:=TSector(FSectors.Items[i]);
    totale:=totale+ts.FValue;
  end;
  for i:=0 to Sectors.Count-1 do begin
    ts:=TSector(FSectors.Items[i]);
    Result.Add(ts.FSName+#9+FloatToStr(ts.FValue/totale));
  end;
end;

begin
  inherited Paint;
  ListSectors:=TStringList.Create;
  OtherList:=TStringList.Create;
  GetSectorsFan(FSectors,ListSectors);
  a:=(Width-2*bx)div 2;
  b:=(Height-2*by-FDepth)div 2;
  LabelHeigth:=Canvas.TextHeight('A');
  if FLegend=lpOutside then begin
    NameWidth:=0;
    for i:=0 to ListSectors.Count-1 do begin
      strPer100_Angolo:=ListSectors.Strings[i];
      strItemLabel:=copy(strPer100_Angolo,1,pos(#9,strPer100_Angolo)-1);
      ItemWidth:=Canvas.TextWidth(strItemLabel);
      if ItemWidth>NameWidth then
        NameWidth:=ItemWidth;
    end;
    LegendWidth:=2*bx+sx+NameWidth+LabelHeigth;
    if LegendWidth>a then
      LegendWidth:=a;
    a:=(Width-LegendWidth-2*bx)div 2;
  end;

  p0.x:=bx;
  p0.y:=by;

  if FKindofLabel<>klNone then begin
    a:=a-LabelHeigth;
    b:=b-LabelHeigth;
  end;

//  Legenda
  Canvas.Brush.Color:=clNone;
  if FLegend=lpOutside then
    Canvas.Rectangle(Width-2*bx-LegendWidth,2*by,Width-2*bx,2*by+ListSectors.Count*(LabelHeigth+sy div 2)+2*Canvas.Pen.Width);

  Canvas.Pen.Style:=psSolid;
  if a<=0 then
    a:=1;
  if b<=0 then
    b:=1;

  Canvas.Brush.Color:=clGray;

  if FDepth>0 then
    Canvas.EllipseC(p0.x+a,p0.y+b+FDepth,a,b);
  Canvas.Brush.Color:=clNone;
  if FSectors.Count=0 then
    Canvas.Ellipse(p0.x,p0.y,p0.x+2*a,p0.y+2*b);

  CenterEllipsePoint.X:=p0.x+a;
  CenterEllipsePoint.Y:=p0.y+b;
  AngoloInizialeF:=0;
  AngoloF:=0;
  StartPoint.X:=CenterEllipsePoint.X+a;
  StartPoint.Y:=CenterEllipsePoint.Y;

  p100:=0.0;
  for i:=0 to FSectors.Count-1 do begin
    ts:=TSector(FSectors.Items[i]);
    Canvas.Brush.Color:=ts.FColor;
    strPer100_Angolo:=ListSectors.Strings[i];
    delete(strPer100_Angolo,1,pos(#9,strPer100_Angolo));//elimina FSName
    p100:=p100+StrToFloat(strPer100_Angolo);
    AngoloF:=EllipseGetAnglePercentageArea(a,b,p100);
    EndPoint:=EllipsePoint(CenterEllipsePoint,a,b,AngoloF);
    if i=FSectors.Count-1 then begin
      EndPoint.X:=CenterEllipsePoint.X+a;
      EndPoint.Y:=CenterEllipsePoint.Y;
    end;
    if StartPoint<>EndPoint then
      Canvas.Pie(p0.x,p0.y,p0.x+2*a,p0.y+2*b,round(StartPoint.X),round(StartPoint.Y),round(EndPoint.X),round(EndPoint.Y));
    if FLegend=lpOutside then begin
      Canvas.Rectangle(Width-LegendWidth-bx+(LabelHeigth div 2),
                       2*by+i*(LabelHeigth+sy div 2)+(LabelHeigth div 2),
                       Width-LegendWidth-bx+sx+(LabelHeigth div 2),
                       2*by+i*(LabelHeigth +sy div 2)+(LabelHeigth div 2)+sy);
      Canvas.Brush.Color:=clCaptionText;
      Canvas.TextOut(Width-LegendWidth+sx+(LabelHeigth div 2),sy div 2+i*(LabelHeigth+sy div 2)+(LabelHeigth div 2),ts.FSName);
    end;
    OtherList.Add(FloatToStr((AngoloInizialeF+AngoloF)/2));
    AngoloInizialeF:=AngoloF;
    StartPoint:=EndPoint;
  end;

  if FRing then begin
    Canvas.Brush.Color:=clDkGray;
    ahole:=a div 5;
    bhole:=b div 5;
    if ahole<=0 then
      ahole:=1;
    if bhole<=0 then
      bhole:=1;
    Canvas.EllipseC(p0.x+a, p0.y+b,ahole,bhole);
    if FDepth>0 then
      Canvas.Arc(    p0.x+a-ahole, p0.y+b-bhole+FDepth,p0.x+a+ahole,p0.y+b+bhole,dalfa*16,(180-2*dalfa)*16);
  end;

  for i:=0 to FSectors.Count-1 do begin  //separate "for cicle" to avoid graphic sovrapposition
    ts:=TSector(FSectors.Items[i]);
    Canvas.Brush.Color:=ts.FColor;
    strPer100_Angolo:=ListSectors.Strings[i];
    delete(strPer100_Angolo,1,pos(#9,strPer100_Angolo));//elimina FSName
    Per100:=FormatFloat('0.00',StrToFloat(strPer100_Angolo)*100);
    Teta_e:=StrToFloat(OtherList[i]);
    ro_e:=EllipseRho(a,b,Teta_e);

    if FLegend=lpInside then begin
      roundtmp1:=round(ro_e/2*cos(Teta_e));
      roundtmp2:=round(ro_e/2*sin(Teta_e));
      Canvas.Rectangle(p0.x+a+roundtmp1-(sx div 2),p0.y+b-roundtmp2-(sy div 2),
                       p0.x+a+roundtmp1+(sx div 2),p0.y+b-roundtmp2+(sy div 2));
      Canvas.Brush.Color:=clCaptionText;
      Canvas.TextOut(p0.x+a+roundtmp1-(sx div 2),p0.y+b-roundtmp2+sy,ts.FSName);
    end;

    Canvas.Brush.Color:=ts.FColor;
    roundtmp1:=round(rhoLabel*ro_e*cos(Teta_e));
    roundtmp2:=round(rhoLabel*ro_e*sin(Teta_e));
    case FKindofLabel of
      klPercentage:begin
        ItemWidth:=Canvas.TextWidth(Per100+'%');
        Rect.Left:=p0.x+a+roundtmp1-(ItemWidth div 2);
        Rect.Top:=p0.y+b-roundtmp2-(LabelHeigth div 2);
        Rect.Right:=Rect.Left+2*bx+ItemWidth;
        Rect.Bottom:=Rect.Top+2*by+LabelHeigth;
        Canvas.Rectangle(Rect);
        Canvas.TextOut(Rect.Left+bx,Rect.Top+(LabelHeigth div 2)-2*by,Per100+'%');
      end;
      klValue:begin
        ItemWidth:=Canvas.TextWidth(FloatToStr(ts.FValue));
        Rect.Left:=p0.x+a+roundtmp1-(ItemWidth div 2);
        Rect.Top:=p0.y+b-roundtmp2-(LabelHeigth div 2);
        Rect.Right:=Rect.Left+2*bx+ItemWidth;
        Rect.Bottom:=Rect.Top+2*by+LabelHeigth;
        Canvas.Rectangle(Rect);
        Canvas.TextOut(Rect.Left+bx,Rect.Top+(LabelHeigth div 2)-2*by,FloatToStr(ts.FValue));
      end;
      klBoth:begin
        ItemWidth:=Canvas.TextWidth(FloatToStr(ts.FValue));
        NameWidth:=ItemWidth;
        if Canvas.TextWidth(Per100+'%')>ItemWidth then
          ItemWidth:=Canvas.TextWidth(Per100+'%');
        Rect.Left:=p0.x+a+roundtmp1-(ItemWidth div 2);
        Rect.Top:=p0.y+b-roundtmp2-LabelHeigth;
        Rect.Right:=Rect.Left+2*bx+ItemWidth;
        Rect.Bottom:=Rect.Top+2*by+2*LabelHeigth+2;
        Canvas.Rectangle(Rect);
        Canvas.TextOut(Rect.Left+bx+((Rect.Right-Rect.Left-NameWidth)div 2),Rect.Top+(LabelHeigth div 2)-2*by,FloatToStr(ts.FValue));
        NameWidth:=Canvas.TextWidth(Per100+'%');
        Canvas.TextOut(Rect.Left+((Rect.Right-Rect.Left-NameWidth)div 2),2+Rect.Top+(LabelHeigth div 2)+LabelHeigth-2*by,Per100+'%');
      end;
      klNone:begin
      end;
    end;
  end;

  Canvas.TextOut(bx,by,Caption);
  Canvas.Pen.Style:=psSolid;
  ListSectors.Free;
  OtherList.Free;
end;

procedure TCustomRingChart.Clear;
begin
  FSectors.Clear;
  Invalidate;
end;

procedure TCustomRingChart.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomRingChart.EndUpdate;
begin
  if FUpdateCount=0 then
    raise Exception.Create('TCustomRingChart.EndUpdate');
  Dec(FUpdateCount);
  If FUpdateCount=0 then
    Invalidate;
end;

function TCustomRingChart.SectorCount: Integer;
begin
  Result:=FSectors.Count;
end;

initialization
{$i ringchart.lrs}

end.
