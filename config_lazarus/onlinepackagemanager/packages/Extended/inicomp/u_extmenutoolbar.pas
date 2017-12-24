unit u_extmenutoolbar;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
  Classes, SysUtils, ComCtrls, Menus,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  menutbar;


{$IFDEF VERSIONS}
const
    gVer_TExtMenuToolBar : T_Version = ( Component : 'Composant TExtMenuToolBar' ;
                                               FileUnit : 'u_extmenutoolbar' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Barre de menu avec bouton de click.' ;
                                               BugsStory : '0.9.9.0 : Testing.' + #13#10
                                                         + '0.9.0.2 : Making comments.' + #13#10
                                                         + '0.9.0.1 : Optimising.' + #13#10
                                                         + '0.9.0.0 : Gestion en place et testée.';
                                               UnitType : 3 ;
                                               Major : 0 ; Minor : 9 ; Release : 9 ; Build : 0 );

{$ENDIF}

type
  { TExtMenuToolBar }

  TExtMenuToolBar = class(TMenuToolBar)
  private
    {$IFNDEF FPC}
    ExtMenuToolbar_ResInstance : THandle;
    {$ENDIF}
    FButtonGet: TToolButton;
    FAutoDrawDisabled : Boolean;
    FOnClickCustomize : TNotifyEvent;
  protected
    procedure DoOnMenuCreating; override;
    procedure DoOnMenuCreated; override;
    procedure WindowGet ( AObject : TObject ); virtual;
    procedure SetCustomizeImage; virtual;
  public
    procedure p_setAutoDrawDisabled ( AValue: Boolean ); virtual;
    constructor Create(TheOwner: TComponent); override;
    procedure Loaded ; override;
    property ButtonGet: TToolButton read FButtonGet;
  published
    property AutoDrawDisabled : Boolean read FAutoDrawDisabled write FAutoDrawDisabled default True;
    property OnClickCustomize: TNotifyEvent read FOnClickCustomize write FOnClickCustomize;
  end;

implementation

uses   {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
 Controls, Graphics,
{$IFDEF FPC}
     LResources,
{$ENDIF}
     Dialogs;

const MenuToolbar_TExtMenuToolBar = 'TExtMenuToolBar' ;
      MenuToolbar_TagCustomizeButton = 1000;

{ TExtMenuToolBar }

// procedure TExtMenuToolBar.WindowGet
// OnClick Event of the FBUttonGet Button
// AObject : The notify event sender
procedure TExtMenuToolBar.WindowGet(AObject: TObject);
begin
  if assigned ( FOnClickCustomize ) Then
    FOnClickCustomize ( AObject );
end;

// procedure TExtMenuToolBar.Loaded
// draw the Disabled Images when AutoDrawDisabled set to true

procedure TExtMenuToolBar.Loaded;
begin
  inherited;
  SetCustomizeImage;
  p_setAutoDrawDisabled ( FAutoDrawDisabled );
end;


// procedure TExtMenuToolBar.p_setAutoDrawDisabled
// Setting AutoDrawDisabled
// draw the Disabled Images when AutoDrawDisabled set to true
// AValue : The new value of AutoDrawDisabled
procedure TExtMenuToolBar.p_setAutoDrawDisabled(AValue: Boolean);
var lbmp_Bitmap : TBitmap;
     i : Integer;
begin
  FAutoDrawDisabled := AValue;
  if  assigned( DisabledImages )
  and assigned( Images)
  and FAutoDrawDisabled Then
  Begin
    DisabledImages.Clear;
    lbmp_Bitmap := TBitmap.Create;
    for i := 0 to Images.Count -1 do
      Begin
        Images.GetBitmap( I, lbmp_Bitmap);
        lbmp_Bitmap.Monochrome:=True;
        DisabledImages.AddMasked(lbmp_Bitmap,lbmp_Bitmap.Canvas.Pixels[lbmp_Bitmap.Width-1, lbmp_Bitmap.Height -1]);
      End;
    {$IFNDEF FPC}
    lbmp_Bitmap.Dormant;
    {$ENDIF}
    lbmp_Bitmap.Free;
  end;
end;

// constructor TExtMenuToolBar.Create
// Initing the  TExtMenuToolBar
// TheOwner : The owner of the component
constructor TExtMenuToolBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FButtonGet := nil;
  FAutoDrawDisabled := True;
  {$IFNDEF FPC}
  ExtMenuToolbar_ResInstance := 0;
  {$ENDIF}
end;

/// Procedure SetMenu
// Creating the ToolButtons when setting the menu
procedure TExtMenuToolBar.DoOnMenuCreating;
begin
  if assigned ( FButtonGet ) Then
    Begin
      FButtonGet.Parent := nil;
    end;
  inherited;
end;
procedure TExtMenuToolBar.SetCustomizeImage;
{$IFNDEF FPC}
var lbmp_Bitmap : TBitmap;
{$ENDIF}
begin
  // create customize button once only
  if assigned(FButtonGet)
  and(FButtonGet.ImageIndex=-1)
  and assigned(Images) Then
    Begin
{$IFDEF FPC}
      Images.AddLazarusResource(MenuToolbar_TExtMenuToolBar,clNone);
{$ELSE}
      if ( ExtMenuToolbar_ResInstance = 0 ) Then
        ExtMenuToolbar_ResInstance:= FindResourceHInstance(HInstance);
      lbmp_Bitmap := TBitmap.Create;
      with lbmp_Bitmap do
       Begin
        LoadFromResourceName(ExtMenuToolbar_ResInstance, MenuToolbar_TExtMenuToolBar );
        Images.AddMasked(lbmp_Bitmap,Canvas.Pixels [ Width - 1, Height - 1 ]);
        Dormant;
        Destroy;
       End;
{$ENDIF}
      FButtonGet.ImageIndex:= Images.Count - 1;
    end;
End;

procedure TExtMenuToolBar.DoOnMenuCreated;
begin
  if not ( csDesigning in ComponentState ) Then
    Begin
      // create customize button once only
      if not assigned ( FButtonGet ) Then
        Begin
          FButtonGet:= TToolButton.Create(Self);
          with FButtonGet do
           Begin
            Name := 'Button_' + Name + '_Customize' ;
            Tag:= MenuToolbar_TagCustomizeButton;
            Caption:= GS_TOOLBARMENU_Personnaliser;
            OnClick:= WindowGet;
            SetCustomizeImage;
            Style:= tbsButton;
            Visible:=True;
           End;
        end;
      // At the end
      if ControlCount > 0 Then
        with Controls [ ControlCount - 1 ] do
          if Width > Height
           Then FButtonGet.Left  := Left + Width
           else FButtonGet.Top   := Top  + Height ;
      FButtonGet.Parent := Self;
    end;
  inherited DoOnMenuCreated;
  p_setAutoDrawDisabled ( FAutoDrawDisabled );
end;

initialization
{$IFDEF FPC}
  // lazarus Resources file
  {$I u_extmenutoolbar.lrs}
{$ENDIF}
{$IFDEF VERSIONS}
  // Versioning
  p_ConcatVersion ( gVer_TExtMenuToolBar );
{$ENDIF}
end.

