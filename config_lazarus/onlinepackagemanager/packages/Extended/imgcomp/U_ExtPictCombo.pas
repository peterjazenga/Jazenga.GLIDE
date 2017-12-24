{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtPictCombo :                                        }
{             Objet de choix de couleur                               }
{             qui permet de personnalisé la couleur du titre          }
{             de l'onglet actif                                       }
{             10 Mars 2006                                            }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit U_ExtPictCombo;

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}
{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}

interface

uses
{$IFDEF FPC}
  LCLIntf, LCLType, lMessages,
{$ELSE}
  Windows, Variants, Messages,
{$ENDIF}
{$IFDEF TNT}
   TntStdCtrls,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls,
  StdCtrls,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  u_extcomponent, ImgList, U_ExtMapImageIndex ;

const
{$IFDEF VERSIONS}
    gVer_TExtPictCombo : T_Version = ( Component : 'Composant TExtPictCombo' ;
                                               FileUnit : 'U_ExtPictCombo' ;
                                               Owner : 'Matthieu Giroux' ;
                                               Comment : 'Choisir une image dans une liste.' ;
                                               BugsStory : '1.0.0.0 : MyLabel unset correctly.' + #13#10 +
                                                           '0.9.9.1 : Tested more.' + #13#10 +
                                                           '0.9.9.0 : Tested more.' + #13#10 +
                                                           '0.9.0.0 : Tested and optimised.' + #13#10 +
                                                           '0.8.0.0 : Not tested.';
                                               UnitType : 3 ;
                                               Major : 1 ; Minor : 0 ; Release : 0 ; Build : 0 );


{$ENDIF}
    CST_PICT_COMBO_DEFAULT_STYLE       = csOwnerDrawFixed ;
type

  { TExtPictCombo }

  TExtPictCombo = class(TCustomComboBox, IFWComponent, IFWComponentEdit)
    private
      FImages : TCustomImageList;
      FMapImages : TExtMapImages;
      FBeforeEnter, FBeforeExit : TNotifyEvent;
      FLabel : {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} ;
      FNotifyOrder : TNotifyEvent;
      FOldColor ,
      FColorFocus ,
      FColorReadOnly,
      FColorEdit ,
      FColorLabel : TColor;
      FReadOnly   ,
      FAlwaysSame : Boolean;
      FValue : String;
      procedure p_SetImages ( const Value : TCustomImageList );
      procedure p_SetImagesMap ( const Value : TExtMapImages );
      procedure p_setLabel ( const alab_Label : TLabel);
      procedure WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF}); message {$IFDEF FPC}LM_PAINT{$ELSE}WM_PAINT{$ENDIF};
      procedure CNCommand(var TheMessage: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF}); message {$IFDEF FPC}CN_COMMAND{$ELSE}WM_COMMAND{$ENDIF};
    protected
    { Protected declarations }
      procedure DrawAnImage( const AIndex : Longint; const ARect: TRect; var novorect : TRect ); virtual ;
      procedure p_SetValue(const AValue: String); virtual ;
    protected
      function GetReadOnly: Boolean; virtual;
      procedure SetReadOnly(Value: Boolean); virtual;
      procedure Notification(AComponent: TComponent;
                Operation: TOperation); override;
    public
    { Public declarations }
      constructor Create(AOwner: TComponent); override;
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure SetOrder ; virtual;
      procedure Loaded; override;
      procedure Change; override;
      procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
      function Focused : Boolean; override ;
    published
    { Published declarations }
      property Images : TCustomImageList read FImages write p_SetImages ;
      property ImagesMap : TExtMapImages read FMapImages write p_SetImagesMap ;
      property Value : String read FValue write p_SetValue;
      property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored True default False;
    // Visuel
      property FWBeforeEnter : TnotifyEvent read FBeforeEnter write FBeforeEnter stored False;
      property FWBeforeExit  : TnotifyEvent read FBeforeExit  write FBeforeExit stored False ;
      property ColorLabel : TColor read FColorLabel write FColorLabel default CST_LBL_SELECT ;
      property ColorFocus : TColor read FColorFocus write FColorFocus default CST_EDIT_SELECT ;
      property ColorEdit : TColor read FColorEdit write FColorEdit default CST_EDIT_STD ;
      property ColorReadOnly : TColor read FColorReadOnly write FColorReadOnly default CST_EDIT_READ ;
      property MyLabel : {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} read FLabel write p_setLabel;
      property AlwaysSame : Boolean read FAlwaysSame write FAlwaysSame default true;
      property OnOrder : TNotifyEvent read FNotifyOrder write FNotifyOrder;
    // Propriétés gardées
      property Style default csOwnerDrawFixed;
      property AutoComplete;
      property AutoDropDown;
{$IFDEF DELPHI}
      property AutoCloseUp;
      property BevelEdges;
      property BevelInner;
      property BevelKind;
      property BevelOuter;
      property ImeMode;
      property ImeName;
{$ENDIF}
      property Anchors;
      property BiDiMode;
      property Color;
      property Constraints;
    {$IFNDEF FPC}
    property Ctl3D;
    property ParentCtl3D;
    {$ENDIF}
    property DragCursor;
    property DragKind;
    property Items;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses fonctions_proprietes,
     fonctions_components,
     fonctions_images;


{ TExtPictCombo }

constructor TExtPictCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Style := csOwnerDrawFixed;
  FValue := '' ;

  //Visuel
  FReadOnly   := False;
  FAlwaysSame := True;
  FColorLabel := CST_LBL_SELECT;
  FColorEdit  := CST_EDIT_STD;
  FColorFocus := CST_EDIT_SELECT;
  FColorReadOnly := CST_EDIT_READ;
end;

procedure TExtPictCombo.SetOrder;
begin
  if assigned ( FNotifyOrder ) then
    FNotifyOrder ( Self );
end;

procedure TExtPictCombo.p_SetImages(const Value: TCustomImageList);
begin
  FImages:= Value;
end;

procedure TExtPictCombo.p_setLabel(const alab_Label: TLabel);
begin
  p_setMyLabel ( FLabel, alab_Label, Self );
end;

function TExtPictCombo.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;


procedure TExtPictCombo.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TExtPictCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove Then
    Exit;
  if AComponent = FLabel     then FLabel := nil;
  if AComponent = FMapImages then FMapImages := nil;
  if AComponent = FImages    then FImages := nil;
end;

procedure TExtPictCombo.DoEnter;
begin
  if GetReadOnly Then Exit;
  if assigned ( FBeforeEnter ) Then
    FBeforeEnter ( Self );
  // Si on arrive sur une zone de saisie, on met en valeur son {$IFDEF TNT}TTntLabel{$ELSE}TLabel{$ENDIF} par une couleur
  // de fond bleu et son libellé en marron (sauf si le libellé est sélectionné
  // avec la souris => cas de tri)
  p_setLabelColorEnter ( FLabel, FColorLabel, FAlwaysSame );
  p_setCompColorEnter  ( Self, FColorFocus, FAlwaysSame );
  inherited DoEnter;
end;

procedure TExtPictCombo.DoExit;
begin
  if assigned ( FBeforeExit ) Then
    FBeforeExit ( Self );
  inherited DoExit;
  p_setLabelColorExit ( FLabel, FAlwaysSame );
  p_setCompColorExit ( Self, FOldColor, FAlwaysSame );

end;

procedure TExtPictCombo.Loaded;
begin
  inherited Loaded;
  FOldColor := Color;
  if  FAlwaysSame
   Then
    Color := gCol_Edit ;
end;

procedure TExtPictCombo.WMPaint(var Message: {$IFDEF FPC}TLMPaint{$ELSE}TWMPaint{$ENDIF});
Begin
  p_setCompColorReadOnly ( Self,FColorEdit,FColorReadOnly, FAlwaysSame, ReadOnly );
{  if ItemIndex >= 0 Then
    DrawAnImage(ItemIndex,Message.PaintStruct.rcPaint,Message.PaintStruct.rcPaint);}
  inherited;
End;

procedure TExtPictCombo.CNCommand(var TheMessage: {$IFDEF FPC}TLMCommand{$ELSE}TWMCommand{$ENDIF});
begin
  if GetReadOnly Then Exit;
  inherited;
end;

procedure TExtPictCombo.DrawAnImage ( const AIndex : Longint; const ARect: TRect; var novorect : TRect );
var
   FImage:TBitmap;
Begin
 if AIndex < FImages.Count
  Then
    Begin
      FImage := TBitmap.Create;
      FImages.GetBitmap(AIndex,FImage);
      novorect:= rect(arect.Left+4, arect.Top+1, arect.bottom - arect.Top - 2, arect.bottom - arect.Top- 2);
      if FImage.Width  < novorect.Right  Then novorect.Right  := FImage.Width;
      if FImage.Height < novorect.Bottom Then novorect.Bottom := FImage.Height;
      p_ChangeTailleBitmap(FImage,novorect.Right,novorect.Bottom,True);
      Canvas.Draw ( novorect.Left+ ( arect.bottom - arect.Top - FImage.Width  ) div 2 - 1,
                    novorect.Top + ( arect.bottom - arect.Top - FImage.Height ) div 2 - 1,FImage);
      {$IFNDEF FPC}
      FImage.Dormant;
      {$ENDIF}
      FImage.FreeImage;
      FImage.Free;
    end;
 novoRect := rect(ARect.Left + arect.bottom - arect.Top + 6, arect.top, arect.right - 5, arect.bottom);
end;

procedure TExtPictCombo.DrawItem(Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
      novorect: trect;
      format : Uint ;
begin
    with Canvas do
    begin
      FillRect(ARect);
      novorect:=ARect;
      if  assigned ( FImages ) Then
       Begin
        if assigned ( FMapImages ) then
         Begin
          if ( Index < FMapImages.Columns.Count ) Then
            DrawAnImage ( FMapImages.Columns.Items[Index].ImageIndex, ARect, novorect );
         End
        Else
           DrawAnImage ( Index, ARect, novorect );
       End;
      format := DT_SINGLELINE or DT_NOPREFIX;
      if ( BiDiMode = bdLeftToRight )
      or (( BiDiMode = bdRightToLeftReadingOnly ) and not DroppedDown ) Then
        format := format or DT_LEFT
      Else
        format := format or DT_RIGHT ;
      if BiDiMode <> bdRightToLeftNoAlign Then
        format := format or DT_VCENTER ;
      if  ( Index < Items.Count ) Then
        DrawText(Canvas.Handle, Pchar ( Items [ Index] ), Length(Items [ Index]), novoRect, format );
    end;
end;

procedure TExtPictCombo.p_SetImagesMap(const Value: TExtMapImages);
begin
 FMapImages := Value;

end;

procedure TExtPictCombo.Change;
begin
   if  assigned ( FMapImages ) Then
    Begin
     if ( itemindex >= 0 )
     and ( itemindex < FMapImages.Columns.Count )
      Then
        p_SetValue(FMapImages.Columns.Items[ItemIndex].Value)
    End
   Else
     p_SetValue(IntToStr(ItemIndex))
end;

procedure TExtPictCombo.p_SetValue(const AValue: String);
begin
 if FValue <> AValue then
   Begin
    FValue:=AValue;
    if assigned ( FMapImages )
     Then ItemIndex:=FMapImages.IndexOf(FValue)
     Else ItemIndex:=Items.IndexOf(FValue);
    Invalidate;
   end;
end;


function TExtPictCombo.Focused: Boolean;
begin
  Result := csFocusing in ControlState ;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtPictCombo   );
{$ENDIF}
end.
