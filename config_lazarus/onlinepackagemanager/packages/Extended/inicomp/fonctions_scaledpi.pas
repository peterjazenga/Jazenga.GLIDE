unit fonctions_scaledpi;

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
  Graphics, Forms,
  {$IFDEF VERSIONS}
    fonctions_version,
  {$ENDIF}
  Controls, Classes, sysutils;

Const
  FromDPI=8;//Screen.MenuFont.Size de la conception
  SPACING = 'Spacing';
  SCALE_NODE_HEIGHT = 'DefaultNodeHeight';
  SCALE_SEARCH_WIDTH = 'SearchWidth';
  SCALE_GLYPH_SIZE  = 'GlyphSize';
  {$IFDEF VERSIONS}
  gver_fonctions_scaledpi : T_Version = ( Component : 'Fonctions d''adaptation de fontes' ;
                                     FileUnit : 'fonctions_scaledpi' ;
              			                 Owner : 'Matthieu Giroux' ;
              			                 Comment : 'Adapt forms and controls to system.' ;
              			                 BugsStory :  'Version 1.0.1.2 : More components to scale.' + #13#10 +
                                                              'Version 1.0.1.1 : Never scale on design.' + #13#10 +
                                                              'Version 1.0.1.0 : Adding interfaces for classes.' + #13#10 +
                                                              'Version 1.0.0.0 : OK on linux with ini.' + #13#10 +
                                                              'Version 0.9.9.0 : OK on windows.' + #13#10 +
                                                              'Version 0.9.0.0 : To test.';
              			                 UnitType : 1 ;
              			                 Major : 1 ; Minor : 0 ; Release : 1 ; Build : 2 );

{$ENDIF}

procedure p_addtoSoftware;
procedure HighDPI;
procedure ScaleDPIControl(const Control: TComponent;const ANewEchelle:Extended);
function Scale(const Valeur:Integer;const ANewEchelle:Extended):Integer;
procedure ScaleFormShow(const Control: TCustomForm;const ANewEchelle:Extended);
procedure ScaleFormCreate(const Control: TCustomForm;const ANewEchelle:Extended);
function fb_CalculateScale ( var AEchelle : Extended ):Boolean;
procedure FormInScreen(const Control: TCustomForm);


type
  INoAdaptComponent = interface
   ['{4D071431-71E6-4B55-9AD2-B815D34A8379}']
  End;

  ISpecialAdaptComponent = interface
    ['{4D071431-71E6-4B55-9AD2-B815D34A8379}']
    procedure ScaleComponent ( const NewScale : Extended );
  End;

  { TDMAdaptForms }

  TDMAdaptForms = class(TDataModule)
    procedure ApplicationActivate ( Sender : TObject );
  private
    { private declarations }
  published
    { published declarations }
    constructor Create ( AOwner : TComponent ); override;

  end;

var
  gb_AdaptFormsToOS : Boolean = True;
  ge_FontsScale:Extended=1;

implementation

uses fonctions_proprietes, math, typinfo, Grids;

var ge_OldApplicationActivate : TNotifyEvent = nil;
    DMAdaptForms: TDMAdaptForms = nil;

// Optional Application.OnActivate Event ( not using FormAdapt )
procedure TDMAdaptForms.ApplicationActivate ( Sender : TObject );
Begin
  if Assigned(ge_OldApplicationActivate) Then
    ge_OldApplicationActivate ( Sender );
  HighDPI;
end;

// no lfm ( not using FormAdapt )
constructor TDMAdaptForms.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  if (ClassType <> TDataModule) and
     not (csDesigning in ComponentState) then
    begin
    if OldCreateOrder then
      DoCreate;
    end;
end;

// calculate scale from system ( windows only )
function fb_CalculateScale ( var AEchelle : Extended ):Boolean;
var
  LNewEchelle : Extended;
Begin
  if not gb_AdaptFormsToOS Then
  Begin
    Result := False;
    Exit;
  end;
  if Screen.MenuFont.Size = 0
    Then LNewEchelle:=FromDPI
    Else LNewEchelle:=Screen.MenuFont.Size;
  {WriteLn(IntToStr(Screen.SystemFont.Size));
  WriteLn(IntToStr(Screen.HintFont.Size));
  WriteLn(IntToStr(Screen.IconFont.Size));
  WriteLn(IntToStr(Screen.MenuFont.Size));}

  LNewEchelle:=LNewEchelle/FromDPI*AEchelle;
  Result := LNewEchelle<>AEchelle;
  AEchelle:=LNewEchelle;
End;

// Scale all Applications forms
procedure HighDPI;
var
  i: integer;
begin
  if fb_CalculateScale ( ge_FontsScale ) then
    for i:=0 to Screen.FormCount-1 do
      ScaleFormShow(Screen.Forms[i],ge_FontsScale);
end;

// Scale a value
function Scale(const Valeur:Integer;const ANewEchelle:Extended):Integer;
Var
  Ext:Extended;
begin
  Ext:=Valeur*ANewEchelle;
  if Ext>=0 then
    Result:=Trunc(Ext+0.5)
  else
    Result:=Trunc(Ext-0.5);
end;

// scale on Form Show
procedure ScaleFormShow(const Control: TCustomForm;const ANewEchelle:Extended);
var
  i: integer;
Begin
  with Control do
  if not ( csDesigning in ComponentState ) //never change designing
   Then
    Begin
     {$IFDEF FPC}BeginUpdateBounds;{$ENDIF}

     Font.Size:=Scale(Font.Size,ANewEchelle);
     for i:=0 to Control.ComponentCount-1 do
      ScaleDPIControl(Control.Components[i],ANewEchelle);
     {$IFDEF FPC}EndUpdateBounds;{$ENDIF}
    End;
end;
// scale on form create
procedure ScaleFormCreate(const Control: TCustomForm;const ANewEchelle:Extended);
Begin
  with Control do
   Begin
     // resize if possible
     Width  := Min ( Screen.{$IFDEF WINDOWS}WorkAreaWidth{$ELSE}Width{$ENDIF}, Scale(Width,ANewEchelle));
     // resize if possible
     Height  := Min ( Screen.{$IFDEF WINDOWS}WorkAreaHeight{$ELSE}Height{$ENDIF}, Scale(Height,ANewEchelle));

   End;
  // form can be out of screen
  FormInScreen(Control);
end;

// form can be out of screen
procedure FormInScreen(const Control: TCustomForm);
Begin
  with Control do
   Begin
     // form can be out of screen
     if Width > Screen.{$IFDEF WINDOWS}WorkAreaWidth{$ELSE}Width{$ENDIF} Then
       Width   := Screen.{$IFDEF WINDOWS}WorkAreaWidth{$ELSE}Width{$ENDIF};
     if Height > Screen.{$IFDEF WINDOWS}WorkAreaHeight{$ELSE}Height{$ENDIF} Then
       Height  := Screen.{$IFDEF WINDOWS}WorkAreaHeight{$ELSE}Height{$ENDIF};
     if left + Width > Screen.{$IFDEF WINDOWS}WorkAreaWidth{$ELSE}Width{$ENDIF} Then
       Left   := Max ( 0, Screen.{$IFDEF WINDOWS}WorkAreaWidth{$ELSE}Width{$ENDIF} - Width );
     if Top + Height > Screen.{$IFDEF WINDOWS}WorkAreaHeight{$ELSE}Height{$ENDIF} Then
       Top    := Max ( 0, Screen.{$IFDEF WINDOWS}WorkAreaHeight{$ELSE}Height{$ENDIF} - Height );
   End;
end;

// scale a font
procedure ScaleFont(const Control: TObject;const ANewEchelle:Extended);
var  AFont : TFont;
Begin
  if ( Control is TCustomForm )
  or not fb_getComponentBoolProperty(Control,'ParentFont', False ) Then
   Begin
    AFont := TFont ( fobj_getComponentObjectProperty ( Control, 'Font' ));
    if assigned ( AFont ) then
     Begin
      AFont.Size:=Scale(AFont.Size,ANewEchelle);
     end;
   end;
end;

procedure p_ScaleChildControls(const Control: TComponent;const ANewEchelle:Extended);
var WinControl: TWinControl;
    ChildControl : TControl;
    i: integer;
Begin
  if (Control is TWinControl) Then
  begin
    WinControl:=TWinControl(Control);
    for i:=0 to WinControl.ControlCount-1 do
     Begin
      ChildControl:= WinControl.Controls[i];
      if not ( ChildControl.Owner is TCustomForm ) then
       ScaleDPIControl(ChildControl,ANewEchelle);
     end;
  end;
end;

// scale a control
procedure ScaleDPIControl(const Control: TComponent;const ANewEchelle:Extended);
var
  i: integer;
  AColumn : TCollection;
  AItem   : TCollectionItem;
begin
  if  {$IFNDEF FPC}Supports{$ENDIF}( Control {$IFDEF FPC}is{$ELSE},{$ENDIF} INoAdaptComponent)
  or ( csDesigning in Control.ComponentState ) Then  //never change designing
   Exit;
  if Control is TControl Then
  with Control as TControl do
   begin
    with Constraints do
     begin
      MaxHeight:=Scale(MaxHeight,ANewEchelle);
      MaxWidth:=Scale(MaxWidth,ANewEchelle);
      MinHeight:=Scale(MinHeight,ANewEchelle);
      MinWidth:=Scale(MinWidth,ANewEchelle);
     end;

    if not ( Control is TCustomForm )
    and    ( Align   <> alClient    ) then
      begin
        if not (Align in [alTop,alBottom]) then
        begin
          if not (akRight in Anchors) and (Align<>alRight) then
            Left:=Scale(Left,ANewEchelle);
          if ([akRight,akLeft]*Anchors<>[akRight,akLeft]) then
            Width:=Scale(Width,ANewEchelle);
        end;
        if not (Align in [alLeft,alRight])then
        begin
          if (not (akBottom in Anchors))and(Align<>alBottom) then
            Top:=Scale(Top,ANewEchelle);
          if [akBottom,akTop]*Anchors<>[akBottom,akTop] then
            Height:=Scale(Height,ANewEchelle);
        end;
      end;

    if assigned ( GetPropInfo ( Control, SCALE_NODE_HEIGHT )) Then
      SetPropValue(Control, SCALE_NODE_HEIGHT, Scale ( GetPropValue (Control, SCALE_NODE_HEIGHT ), ANewEchelle));
    if assigned ( GetPropInfo ( Control, SCALE_SEARCH_WIDTH )) Then
      SetPropValue(Control, SCALE_SEARCH_WIDTH, Scale ( GetPropValue (Control, SCALE_SEARCH_WIDTH ), ANewEchelle));
    if assigned ( GetPropInfo ( Control, SPACING ))
    and ( PropType(Control,SPACING) in [tkInteger{$IFDEF FPC},tkQWord{$ENDIF},tkInt64] ) Then
     Begin
      I := Scale ( GetPropValue (Control, SPACING ), ANewEchelle);
      if i < 0
       Then
        Begin
         I := Max ( -Width + 4, I );  // It is inside and text before picture spacing
         SetPropValue(Control, SPACING, i );
        end;
     end;
    if assigned ( GetPropInfo ( Control, SCALE_GLYPH_SIZE ))
    and ( PropType(Control,SCALE_GLYPH_SIZE) in [tkInteger{$IFDEF FPC},tkQWord{$ENDIF},tkInt64] ) Then
      SetPropValue(Control, SCALE_GLYPH_SIZE, Scale ( GetPropValue (Control, SCALE_GLYPH_SIZE ), ANewEchelle));

    if ( Control is TCustomGrid )
    and assigned ( GetPropInfo ( Control, CST_PROPERTY_COLUMNS )) Then
     Begin
       AColumn := GetObjectProp ( Control, CST_PROPERTY_COLUMNS ) as TCollection;
       with AColumn do
       for i := 0 to Count - 1 do
        Begin
          AItem := Items [ i ];
          ScaleFont( AItem,ANewEchelle);
          if assigned ( GetPropInfo ( AItem, CST_PROPERTY_WIDTH )) Then
            SetPropValue ( AItem, CST_PROPERTY_WIDTH,
                           Scale ( GetPropValue ( AItem, CST_PROPERTY_WIDTH )
                                  , ANewEchelle ));
        end;
     end;
  end;

  ScaleFont(Control,ANewEchelle);

  if {$IFNDEF FPC}Supports{$ENDIF}( Control {$IFDEF FPC}is{$ELSE},{$ENDIF} ISpecialAdaptComponent) Then
   ( Control as ISpecialAdaptComponent ).ScaleComponent(ANewEchelle);

  p_ScaleChildControls(Control,ANewEchelle);
end;

// main optinal procedure ( not using FormAdapt )
procedure p_addtoSoftware;
Begin
  ge_OldApplicationActivate := Application.OnActivate;
  if DMAdaptForms = nil Then
    DMAdaptForms := TDMAdaptForms.Create(Application);
  Application.OnActivate    := TNotifyEvent ( fmet_getComponentMethodProperty ( DMAdaptForms, 'ApplicationActivate' ) );
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gver_fonctions_scaledpi );
{$ENDIF}
end.
