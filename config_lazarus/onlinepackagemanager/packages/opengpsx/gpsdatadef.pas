unit gpsdatadef;

interface


uses
  //Windows,
  Classes, ExtCtrls, Contnrs, Graphics, SysUtils,
  Controls, Messages, Forms, geocompute;

type

  THozAlignment = (haLeft, haRight, haCenter);
  TVerAlignment = (vaTop, vaBottom, vaCenter);

  TRotationText = record
    FaceName : string;
    Color : longint;
    Height : longint;
    Weight : TFontStyle;
    Text : string;
    RotationAngle : double;
    HozAlignment : THozAlignment;
    VerAlignment : TVerAlignment;
  end;

  //Keep GPS data. this class needed by TGPSSkyPlot.
  //Use TList to store TGSVInfo one by one.
  TGSVInfo = class(TObject)
  private
    FPrn : string;
    FAzimuth : double;
    FElevation : double;
    FImgX, FImgY : integer;
    FSNR : integer;
    FMessageNumber : integer;
    FMessageCount : integer;
    FSatsInView : integer;
    FSatsInMessage : integer;
    FFirstSat : boolean;
    FLastSat  : boolean;
  public
    property X : integer read FImgX write FImgX;
    property Y : integer read FImgY write FImgY;
    property PRN : string read FPrn write FPrn;
    property SNR : integer read FSNR write FSNR;
    property Azimuth : double read FAzimuth write FAzimuth;
    property Elevation : double read FElevation write FElevation;

    //The next two properties declared to improve TGPSSkyplot displaying.
    property FirstSat : boolean read FFirstSat write FFirstSat;
    property LastSat : boolean read FLastSat write FLastSat;

    property MessageNumber : integer read FMessageNumber write FMessageNumber;
    property MessageCount : integer read FMessageCount write FMessageCount;
    property SatsInView : integer read FSatsInView write FSatsInView;
    property SatsInMessage : integer read FSatsInMessage write FSatsInMessage;
  end;

  TGSVInfos = class(TObjectList)
  private
  public
    constructor Create(AOwner:TComponent);
    destructor Destroy; override;
    function AddGSV(AGSV:TGSVInfo):longint;
    procedure DeleteGSV(Index:longint;AGSV:TGSVInfo);
    procedure UpdateGSV(AGSV:TGSVInfo;ANewGSV:TGSVInfo);
    procedure InsertGSV(Index:longint;AGSV:TGSVInfo);
  end;

  TGeoCoordinate = class (TPersistent)
  private
    FLatitude  : double;
    FLongitude : double;
  public
    procedure Assign(Source : TPersistent); override;
  published
    property Latitude : double read FLatitude write FLatitude;
    property Longitude : double read FLongitude write FLongitude;
  end;

  TUTMGridCoordinate = class (TPersistent)
  private
    FNorthing : double;
    FEasting  : double;
    FUTM : TUTMProjection;
    procedure SetUTMProjection(const Value : TUTMProjection);
  protected
  public
    procedure Assign(Source : TPersistent); override;
    constructor Create;
    destructor  Destroy; override ;
  published
    property Northing : double read FNorthing write FNorthing;
    property Easting  : double read FEasting write FEasting;
    property UTM : TUTMProjection read FUTM write SetUTMProjection;
  end;

implementation
{TUTMGridCoordinate}
constructor TUTMGridCoordinate.Create;
begin
  inherited Create;
  FUTM := TUTMProjection.Create;
end;

destructor TUTMGridCoordinate.Destroy;
begin
  FUTM.Free;
  inherited;
end;

procedure TUTMGridCoordinate.SetUTMProjection(const Value : TUTMProjection);
begin
  FUTM := Value;
end;

procedure TUTMGridCoordinate.Assign(Source : TPersistent);
begin
  if (Source is TUTMProjection) then
  with TUTMGridCoordinate(Source) do
  begin
    Self.Northing := Northing;
    Self.Easting := Easting;
    Self.UTM := UTM;
  end
  else
    inherited;
end;

{TGeoCoordinate}
procedure TGeoCoordinate.Assign(Source : TPersistent);
begin
  if Source is TGeoCoordinate then
  with TGeoCoordinate(Source) do
  begin
    Self.Latitude := Latitude;
    Self.Longitude := Longitude;
  end
  else
    inherited; //raises an exception
end;

{TGSVInfos}
constructor TGSVInfos.Create(AOwner:TComponent);
begin
  inherited Create;
  OwnsObjects := true;
end;

destructor TGSVInfos.Destroy;
begin
  inherited Destroy;
end;

function TGSVInfos.AddGSV(AGSV:TGSVInfo):longint;
begin
  result := inherited Add(AGSV);
end;

procedure TGSVInfos.DeleteGSV(Index:longint;AGSV:TGSVInfo);
begin
  inherited Delete(Index);
end;

procedure TGSVInfos.UpdateGSV(AGSV:TGSVInfo;ANewGSV:TGSVInfo);
begin
  AGSV.Azimuth := ANewGSV.Azimuth;
  AGSV.Elevation := ANewGSV.Elevation;
  AGSV.FirstSat := ANewGSV.FirstSat;
  AGSV.LastSat := ANewGSV.LastSat;
  AGSV.MessageCount := ANewGSV.MessageCount;
  AGSV.MessageNumber := ANewGSV.MessageNumber;
  AGSV.PRN := ANewGSV.PRN;
  AGSV.SatsInMessage := ANewGSV.SatsInMessage;
  AGSV.SatsInView := ANewGSV.SatsInView;
  AGSV.SNR := ANewGSV.SNR;
  AGSV.X := ANewGSV.X;
  AGSV.Y := ANewGSV.Y;
end;

procedure TGSVInfos.InsertGSV(Index:longint;AGSV:TGSVInfo);
begin
  inherited Insert(Index, AGSV);
end;


end.



