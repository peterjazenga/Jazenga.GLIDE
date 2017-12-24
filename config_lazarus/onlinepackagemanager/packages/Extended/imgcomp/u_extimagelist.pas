unit u_extimagelist;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses Graphics,
{$IFDEF TNT}
  TntExtCtrls,
{$ELSE}
  ExtCtrls,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ELSE}
  Windows,
{$ENDIF}
  ImgList,
  Classes, U_ExtImage;

{$IFDEF VERSIONS}
const
  gVer_TExtImageList: T_Version = (Component: 'Composant TExtDBImageList';
    FileUnit: 'U_ExtDBImageList';
    Owner: 'Matthieu Giroux';
    Comment:
    'Gestion de liste d''images dans les données.';
    BugsStory : '0.9.9.0 : Testing.' + #13#10 +
                '0.9.0.0 : Non testée.';
    UnitType: 3;
    Major: 0; Minor: 9; Release: 9; Build: 0);

{$ENDIF}

type
  { TExtDBImageList }

  { TExtImageList }

  TExtImageList = class(TExtImage)
  private
    FImages: TCustomImageList;
    FImageIndex: integer;
    procedure p_SetImages(const Value: TCustomImageList);
  protected
    procedure p_setImageIndex ( const AValue : Integer ); virtual;
    procedure SetOrder; virtual;
    procedure p_SetImage; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Filename;
  published
    property ImageIndex : Integer read FImageIndex write p_setImageIndex default -1;
    property Images: TCustomImageList read FImages write p_SetImages;
  end;


implementation

uses SysUtils;

{ TExtImageList }


constructor TExtImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex:=-1;
end;

procedure TExtImageList.p_setImageIndex(const AValue: Integer);
begin
  if AValue <> FImageIndex Then
    Begin
     FImageIndex:=AValue;
     p_SetImage;
    end;
end;

procedure TExtImageList.SetOrder;
begin

end;

procedure TExtImageList.p_SetImage;
begin
  if assigned(FImages)
   Then
    Begin
      if FImageIndex >= 0 Then
        begin
          FImages.GetBitmap(FImageIndex, Picture.Bitmap);
        end
       Else
       Begin
        Picture.Bitmap.Assign(nil);
       end;
    end;
end;

procedure TExtImageList.p_SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value Then
    Begin
     FImages := Value;
     p_SetImage;
    end;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion(gVer_TExtImageList);
{$ENDIF}
end.
