{**********************************************************************                   
 Package pl_Astronomy.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit mooncomp;

interface

uses

  messages,
  Lmessages,
  graphics,
  classes,
  controls,
  extctrls,
  sysutils,
  IntfGraphics,LCLIntf, LCLType,
  Moonmath,
  moonAPI;

  {$r moonimages.res }            { The File containing ALL bitmaps }

type
  TMoonSize=(ms64,ms32,ms16);
  TMoonStyle=(msClassic,msColor);
  TRotate=(rot_none,rot_90,rot_180,rot_270);

  TMoon=class(TImage)
  private
    F_Align: TAlign;
    FBMP : TBitmap;
    FMaxWidth,FMaxHeight: integer;
    FMoonSize: TMoonSize;
    FAngle: extended;
    FDate: TDateTime;
    FDateChanged: boolean;
    FIcon: TIcon;
    FRotate: TRotate;
    fApollo: boolean;
    FApolloDate: TDateTime;
    FStyle: TMoonStyle;
    procedure Set_Size(Value:TMoonSize);
    procedure SetDate(value:TDateTime);
    procedure SetRotate(value:TRotate);
    procedure SetStyle(value:TMoonStyle);
    procedure DoNothing(value:TPicture);
    procedure DoNothingIcon(value:TIcon);
  protected
    procedure SetBitmap;
    procedure Draw_Moon(acanvas:TCanvas; offset_x,offset_y,radius,apollo_x,apollo_y:integer);
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    function GetIcon:TIcon;
    Function GetIconResString(asize:TMoonSize;astyle:TMoonStyle):string;
    Function GetBkIconResString(asize:TMoonSize):string;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Align: TAlign read F_Align default alNone;
    property MoonSize:TMoonSize read FMoonSize write Set_Size;
    property Date: TDateTime read FDate write SetDate stored FDateChanged;
    property Picture write donothing stored false;
    property Icon:TIcon read GetIcon write donothingIcon stored false;
    property Rotation:TRotate read FRotate write SetRotate;
    property ShowApollo11:boolean read fApollo write FApollo;
    property MoonStyle:TMoonStyle read fStyle write SetStyle;
    end;

implementation

{ TODO : Stefanos fix this 9999, Rotation rot_90,rot_270 gives ERROR}

procedure rotate_bitmap(source:TBitmap; rotate:TRotate);
var
  tempimage: TBitmap;
  w,h,i,j: integer;
  S_IntfImage,T_IntfImage:TLazIntfImage;
begin

  tempimage:=NIL;
  try
    tempimage:=TBitmap.Create;
    tempimage.assign(source);
    h:=source.height-1;
    w:=source.width-1;

    case rotate of
      rot_none:begin end;

      rot_90: begin
           {
               S_IntfImage:=source.CreateIntfImage;
               T_IntfImage:=tempimage.CreateIntfImage;
               for i:=0 to w do
                 for j:=0 to h do
                  begin

                   S_IntfImage.Pixels[i,h-j]:=T_IntfImage.Pixels[j,i];

                  end;
               source.LoadFromIntfImage(S_IntfImage);
               S_IntfImage.free;
               T_IntfImage.free;

               }
               end;

      rot_180: begin
                source.canvas.copyrect( rect(w,h,0,0),
                                        tempimage.canvas,
                                        rect(0,0,w,h));
               end;

      rot_270: begin
                {
                  S_IntfImage:=source.CreateIntfImage;
                  T_IntfImage:=tempimage.CreateIntfImage;
                  for i:=0 to w do
                   for j:=0 to h do
                   begin

                     S_IntfImage.Pixels[w-i,j]:=T_IntfImage.Pixels[j,i];

                   end;
                 source.LoadFromIntfImage(S_IntfImage);
                 S_IntfImage.free;
                 T_IntfImage.free;
                 }
               end;

      end;

  finally
    tempimage.free;
    end;
  end;


const

  size_moon:array[TMoonSize,0..6] of integer=
    ((64,64,28,31,28,41,29),
     (32,32,14,15,14,20,15),
     (16,16,7,7,7,9,7));   { max_x,max_y,offset_y,offset_x,radius,xApollo,yApollo }

Function TMoon.GetIconResString(asize:TMoonSize;astyle:TMoonStyle):string;
 begin
   result:='';
   if astyle=msClassic then
   Case asize of
     ms64:result:='MOON_64';
     ms32:result:='MOON_32';
     ms16:result:='MOON_16';
   end;

   if astyle=msColor then
   Case asize of
     ms64:result:='MOON_COLOR_64';
     ms32:result:='MOON_COLOR_32';
     ms16:result:='MOON_COLOR_16';
   end;
 end;

Function TMoon.GetBkIconResString(asize:TMoonSize):string;
 begin
   result:='';
   Case asize of
     ms64:result:='MOON_BW_64';
     ms32:result:='MOON_BW_32';
     ms16:result:='MOON_BW_16';
   end;
 end;

constructor TMoon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBMP := TBitmap.Create;  {Note dynamic allocation of the pointer}
  SetDate(now);
  FDateChanged:=false;
  ficon:=TIcon.Create;
  Set_Size(ms64);
  f_align:=alNone;
  fApollo:=true;
  FApolloDate:=EncodeDate(1969,7,20)+EncodeTime(20,17,43,0);
  end;

procedure TMoon.SetBitmap;
begin

   case FStyle of
    msClassic: FBMP.LoadFromResourceName(hInstance, GetIconResString(FMoonSize,msClassic));
    msColor:   FBMP.LoadFromResourceName(hInstance, GetIconResString(FMoonSize,msColor));
    end;


  Self.Picture.Graphic := FBMP as TGraphic;
  draw_moon(self.canvas,size_moon[FMoonSize,3],
    size_moon[FMoonSize,2],size_moon[FMoonSize,4],
    size_moon[FMoonSize,5],size_moon[FMoonSize,6]);
  rotate_bitmap(self.picture.bitmap,frotate);
  end;

procedure TMoon.WMSize(var Message: TLMSize);
begin
  inherited;
  if (csDesigning in ComponentState) then begin
    Width := FMaxWidth;
    Height := FMaxHeight;
    end;
  end;

procedure TMoon.Set_Size(Value:TMoonSize);
begin
  FMoonSize:=value;
  FMaxHeight:=size_moon[FMoonSize,0];
  FMaxWidth:=size_moon[FMoonSize,1];
  Self.Height := FMaxHeight;
  Self.Width := FMaxWidth;
  setbitmap;
  end;

procedure TMoon.Draw_Moon(acanvas:TCanvas; offset_x,offset_y,radius,apollo_x,apollo_y:integer);
var
  y,radius2: integer;
  xm,scale: extended;
  xmax,xmin:integer;
begin

(* FAngle = 0   -> New Moon
   FAngle = 90  -> First Quarter
   FAngle = 180 -> Full Moon
   FAngle = 270 -> Last Quarter *)

  if fApollo and (FApolloDate<fdate) then begin
    acanvas.pixels[apollo_x,apollo_y]:=clRed;
    end;
  acanvas.brush.color:=clBlack;
  radius2:=radius*radius;
  scale:=cos_d(fangle);
  for y:=0 to radius do begin
    xm:=sqrt(radius2-y*y);
    xmax:=round(xm);
    xmin:=round(xm*scale);
    if fangle<180 then begin
      xmax:=offset_x-xmax-1;
      xmin:=offset_x-xmin;
      end
    else begin
      xmax:=offset_x+xmax+1;
      xmin:=offset_x+xmin;
      end;
    acanvas.moveto(xmin,y+offset_y);
    acanvas.lineto(xmax,y+offset_y);
    acanvas.moveto(xmin,-y+offset_y);
    acanvas.lineto(xmax,-y+offset_y);
    end;
  end;

procedure TMoon.SetDate(Value: TDateTime);
begin
  FDate:=Value;
  FAngle:=put_in_360(moon_phase_angle(Value));
  setbitmap;
  FDateChanged:=true;
  end;

procedure TMoon.SetRotate(value:TRotate);
begin
  if frotate<>value then begin
    frotate:=value;
    setbitmap;
    end;
  end;

procedure TMoon.SetStyle(value:TMoonStyle);
begin
  if fstyle<>value then begin
    fstyle:=value;
    setbitmap;
    end;
  end;

procedure TMoon.DoNothing(value:TPicture);
begin
end;

procedure TMoon.DoNothingIcon(value:TIcon);
begin
end;

destructor TMoon.Destroy;
begin
  FBMP.free;
  ficon.free;
  inherited destroy;
  end;

function TMoon.GetIcon:TIcon;
var
  IconSizeX : integer;
  IconSizeY : integer;
  AndMask : TBitmap;
  XOrMask : TBitmap;
  IconInfo : TIconInfo;
  Size: TMoonSize;
begin
  AndMask:=NIL;
  XOrMask:=NIL;
  try
    {Get the icon size}
    IconSizeX := GetSystemMetrics(SM_CXICON);
    IconSizeY := GetSystemMetrics(SM_CYICON);

    Size:=ms32;
    if false then
    else if (IconSizeX=16) and (IconSizeY=16) then
      Size:=ms16
    else if (IconSizeX=32) and (IconSizeY=32) then
      Size:=ms32
    else if (IconSizeX=64) and (IconSizeY=64) then
      size:=ms64
    else
      (* ??? *);

    {Create the "And" mask}
    AndMask := TBitmap.Create;
    AndMask.Monochrome := true;
    AndMask.Width := IconSizeX;
    AndMask.Height := IconSizeY;

    FBMP.LoadFromResourceName(hInstance, GetBkIconResString(Size));

    AndMask.canvas.copyrect(Rect(0,0,size_moon[Size,0],size_moon[Size,1]),
                             FBMP.canvas,
                             Rect(0,0,size_moon[Size,0],size_moon[Size,1]));

    {Create the "XOr" mask}
    XOrMask := TBitmap.Create;
    XOrMask.Width := IconSizeX;
    XOrMask.Height := IconSizeY;

    {Draw on the "XOr" mask}

     case FStyle of
       msClassic: FBMP.LoadFromResourceName(hInstance, GetIconResString(Size,msClassic));
       msColor:   FBMP.LoadFromResourceName(hInstance, GetIconResString(Size,msColor));
      end;

    XOrMask.canvas.copyrect(Rect(0,0,size_moon[Size,0],size_moon[Size,1]),
                            FBMP.canvas,
                            Rect(0,0,size_moon[Size,0],size_moon[Size,1]));

    draw_moon(XOrMask.Canvas,size_moon[Size,3],
      size_moon[Size,2],size_moon[Size,4],
      size_moon[Size,5],size_moon[Size,6]);

    rotate_bitmap(XOrMask,frotate);
    rotate_bitmap(AndMask,frotate);

    (*Create a icon *)

    IconInfo.fIcon := true;
    IconInfo.xHotspot := 0;
    IconInfo.yHotspot := 0;
    IconInfo.hbmMask := AndMask.Handle;
    IconInfo.hbmColor := XOrMask.Handle;

      {
    FIcon.Handle := CreateIconIndirect(IconInfo);
              ct9999 }

    FIcon.Create;
    FIcon.SetHandles(IconInfo.hbmColor,IconInfo.hbmMask);
    result := FIcon;
  finally
    AndMask.Free;
    XOrMask.Free;
    end;
  end;

end.

