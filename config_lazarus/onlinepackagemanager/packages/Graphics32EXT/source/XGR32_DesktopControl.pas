
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_DesktopControl;

interface

uses

  LCLIntf, LCLType, LMessages,
  Messages, Classes, Graphics, Controls, SysUtils;

type
   TGRDesktopCanvas = class(TCanvas)
   private
     DC : HDC;
     function GetWidth:Integer;
     function GetHeight:Integer;
   public
     constructor Create;
     destructor Destroy; override;
   published
     property Width: Integer read GetWidth;
     property Height: Integer read GetHeight;
   end;

  TGRDesktopControl = class(TControl)
  private
    FCanvas: TGRDesktopCanvas;
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; Virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TGRDesktopCanvas read FCanvas;
  end;
  

implementation

{ TGRDesktopCanvas }
function TGRDesktopCanvas.GetWidth:Integer;
begin
   Result:=GetDeviceCaps(Handle,HORZRES) ;
end;

function TGRDesktopCanvas.GetHeight:Integer;
begin
   Result:=GetDeviceCaps(Handle,VERTRES) ;
end;

constructor TGRDesktopCanvas.Create;
begin
   inherited Create;
   DC := GetDC(0) ;
   Handle := DC;
end;

destructor TGRDesktopCanvas.Destroy;
begin
   Handle := 0;
   ReleaseDC(0, DC) ;
   inherited Destroy;
end;

{ TGRDesktopControl }
constructor TGRDesktopControl.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TGRDesktopCanvas.Create;
  SetBounds(0,0,Canvas.Width, Canvas.Height);
end;

destructor TGRDesktopControl.Destroy; 
begin
	FreeAndNil(FCanvas);
  inherited;
end;

function TGRDesktopControl.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := False;
end;

function TGRDesktopControl.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
	Result := (NewWidth = Canvas.Width) and (NewHeight = Canvas.Height);
end;

procedure TGRDesktopControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
	inherited SetBounds(0,0,Canvas.Width, Canvas.Height);
end;

end.
