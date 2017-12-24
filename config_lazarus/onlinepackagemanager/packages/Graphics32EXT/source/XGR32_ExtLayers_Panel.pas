
{**********************************************************************
 Package pl_Graphics32EXT.pkg
 this unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit XGR32_ExtLayers_Panel;

interface

uses
  LCLIntf, LCLType, LMessages,
  SysUtils, Graphics,Controls, StdCtrls, ExtCtrls, Classes, Dialogs, Math, Forms,
  GR32_LowLevel, XGR32_Blendmodes,
  GR32, GR32_Image, GR32_Layers,
  GR32_RangeBars,XGR32_ExtLayers;

type
  // Mark process stage -- on the layer or on the mask
  TLayerProcessStage = (lpsNone, lpsLayer, lpsMask);
  TArrangeLayerMode = (almBringToFront,almSendToBack,almUpOneLevel,almDownOneLevel);

TExLayers32Panel = class;

TExLayerPanel = class(TObject)
  private
    // Draw layer image stage
    procedure LayerImagePaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
    procedure SetLayerMasterAlpha(const Alpha: Byte);
    procedure SetBlendModeIndex(const Index: Integer);
  protected
    FPerentWinControl   : TWinControl;      //visual owner for all panels
    FParentList         : TList; //owner for TExLayerPanel
    FLayerImage         : TImage32;
    FMaskImage          : TImage32;
    FStageImage         : TImage32;
    FEyeImage           : TImage32;
    FMainPanel          : TPanel;
    FLayerImageHolder   : TPanel;
    FMaskImageHolder    : TPanel;
    FStageHolder        : TPanel;
    FEyeHolder          : TPanel;
    FLayerNameLabel     : TLabel;
    FBlendModeIndex     : Integer;
    FBlendModeEvent     : TPixelCombineEvent;
    FLayerProcessStage  : TLayerProcessStage;
    FLayerMasterAlpha   : Byte;
    FLayer              : TExtBitmapLayer;
    FLastAlphaChannelBmp: TBitmap32;
    FIsSelected         : Boolean;
    FIsLayerVisible     : Boolean;
    FIsEnabledMask      : Boolean;
    FisBackround        : Boolean;
    FBackRoundImage     : TBitmap32; // if panel hold Image backround
    procedure SetImage32(ABitmap: TBitmap32; AImage: TImage32; APanel: TPanel);
    procedure LayerPixelBlend(F: TColor32; var B: TColor32; M: TColor32); virtual;
    Function  GetIndex:Integer;
  public
    constructor Create(aParentList:TList; aPerentWinControl: TWinControl; aLayer: TExtBitmapLayer); virtual;
    destructor Destroy; override;
    procedure  Assign(Value:TExLayerPanel);
    procedure  SetLayerName(const Name: string);
    procedure  SetAsBackRoundImage(abmp:TBitmap32);
    procedure  UpdateLayerThumbnail;
    procedure  UpdateMaskThumbnail;
    procedure  UpdateLayerVisibleState;
    procedure  ShowThumbnailByRightOrder;

    //......................................
    property PerentWinControl : TWinControl        read FPerentWinControl;
    property ParentList       : TList              read FParentList;
    property Layer            : TExtBitmapLayer    read FLayer;
    property MaskImage        : TImage32           read FMaskImage;
    property IsBackround      : Boolean            read FisBackround       write FisBackround;
    property IsSelected       : Boolean            read FisSelected        write FisSelected;
    property IsLayerVisible   : Boolean            read FIsLayerVisible    write FIsLayerVisible;
    property IsEnabledMask    : Boolean            read FIsEnabledMask     write FIsEnabledMask;
    property BlendModeIndex   : Integer            read FBlendModeIndex    write SetBlendModeIndex;
    property Index            : Integer            read GetIndex;
    property LayerProcessStage: TLayerProcessStage read FLayerProcessStage write FLayerProcessStage;
    property LayerMasterAlpha : Byte               read FLayerMasterAlpha  write SetLayerMasterAlpha;
  end;

TExLayers32Panel = class(TScrollBox)
  private
    FPanelsList:TList;
    FImgView                  : TImgView32;
    FSelectedPanel            : TExLayerPanel;
    FBlendModeViewer          : TComboBox;
    FMasterAlphaViewer        : TGaugeBar;
    FDrawing                  : Boolean;
    FStartPoint               : TPoint;
    FOnSelectedPanelChange    : TNotifyEvent;
    procedure SetImgView(val:TImgView32);
    procedure SetSelPanel(val:TExLayerPanel);
    Function  GetSelectedLayer:TExtBitmapLayer;
    procedure UpdateAllPanels;
    procedure MainPanelClick(Sender: TObject);
    procedure EyeHolderClick(Sender: TObject);
    procedure LayerImageHolderClick(Sender: TObject);
    procedure MaskImageHolderClick(Sender: TObject);
    procedure ConnectEventToLayerPanel(LayerPanel: TExLayerPanel);
    procedure AddLayerPanelToList(AItem: Pointer);
    procedure InserTExLayerPanelToList(const Index: Integer; const AItem: Pointer);
    procedure ApplyMask;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  Clear;
    procedure  ClearExeptSelected;
    procedure  ClearOnlyPanels;

    function  CreateBlankPanelWithNoLayer:TExLayerPanel;
    function  CreateBlankPanel:TExLayerPanel;
    function  CreateNewPanelFromLayer(aLayer:TExtBitmapLayer):TExLayerPanel;
    Procedure BuildLayersFromImgView(const ClrType:integer=0);
    procedure EnableMask;
    procedure DeletePanel(const val:integer); Overload;
    procedure DeletePanel(aLayerPanel:TExLayerPanel); Overload;
    procedure DeleteSelectedLayerPanel;
    procedure DeleteSelectedLayer;
    procedure ArrangeSelectedLayer(const ArrangeMode: TArrangeLayerMode);
    Procedure UnSelectAll;
    function  GeTExLayerPanel(const Index: Integer): TExLayerPanel; Overload;
    function  GeTExLayerPanel(aLayer:TExtBitmapLayer): TExLayerPanel; Overload;
    procedure SetSelectedPanel(const Val:integer);Overload;
    procedure SetSelectedPanel(aLayer:TExtBitmapLayer);Overload;
    procedure ShowAllLayerPanels;
    procedure HideAllLayerPanels;
    Procedure UpdateBackroundPanel;

    procedure SeTExLayerPanelName(var LayerPanel: TExLayerPanel);
    procedure SetSelectedPanelName(const aName:string);

    procedure LayerMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure LayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure LayerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);

    //-----
    Property PanelsList           : TList           read FPanelsList;
    property SelectedLayer        : TExtBitmapLayer read GetSelectedLayer;
    property SelectedPanel        : TExLayerPanel   read FSelectedPanel       write SetSelPanel;

 published

    property ImgView              : TImgView32      read FImgView             write SetImgView;
    property BlendModeViewer      : TComboBox       read FBlendModeViewer     write FBlendModeViewer;
    property MasterAlphaViewer    : TGaugeBar       read FMasterAlphaViewer   write FMasterAlphaViewer;
    property OnSelectedPanelChange: TNotifyEvent    read FOnSelectedPanelChange write FOnSelectedPanelChange;

end;

procedure DrawStage(Stage: TBitmap32);

implementation

{$R XGR32_ExtLayers_Panel.res}

const
  MAIN_PANEL_HEIGHT : Integer = 42;//42
  IMAGE_HOLDER_WIDTH: Integer = 40;//40;
  IMAGE_WIDTH       : Integer = 32;//32;
  IMAGE_HEIGHT      : Integer = 32;//32;
  PANEL_STAGE_WIDTH : Integer = 24;
  IMAGE_STAGE_WIDTH : Integer = 16;
  IMAGE_STAGE_HEIGHT: Integer = 16;



//== Necessary Methods =========================================================

function IntToByte(const I: Integer): Byte;
begin
  if   I > 255
  then Result := 255
  else
  if   I < 0
  then Result := 0
  else Result := I;
end; { IntToByte }

procedure GetAlphaChannelBitmap(const Source, Dest: TBitmap32);
var
  a   : Cardinal;
  i   : Integer;
  s, d: PColor32;
begin
  if   Dest.DrawMode <> dmOpaque
  then Dest.DrawMode := dmOpaque;

  Dest.SetSize(Source.Width, Source.Height);
  Dest.FillRect(0, 0, Dest.Width, Dest.Height, clBlack32);
  s := @Source.Bits[0];
  d := @Dest.Bits[0];
  for i := 0 to Dest.Width * Dest.Height - 1 do
  begin
    a  := s^ shr 24 and $FF;
    d^ := $FF000000 or (a shl 16) or (a shl 8) or a;
    Inc(s);
    Inc(d);
  end;
end; { GetAlphaChannelBitmap }

procedure ReplaceAlphaChannelWithMask(const Dest, Mask: TBitmap32);
var
  i           : Integer;
  da          : Cardinal;
  mr, mg, mb  : Byte;
  dr, dg, db  : Integer;
  DBits, MBits: PColor32;
begin
  if   (Dest.Width <> Mask.Width) or (Dest.Height <> Mask.Height)
  then Exit;

  DBits := @Dest.Bits[0];
  MBits := @Mask.Bits[0];
  for i := 0 to Dest.Width * Dest.Height - 1 do
  begin
    mr     := MBits^ shr 16 and $FF;
    mg     := MBits^ shr  8 and $FF;
    mb     := MBits^        and $FF;
    da     := (mr + mg + mb) div 3;
    dr     := DBits^ shr 16 and $FF;
    dg     := DBits^ shr  8 and $FF;
    db     := DBits^        and $FF;
    DBits^ := (da shl 24) or (dr shl 16) or (dg shl 8) or db;
    Inc(DBits);
    Inc(MBits);
  end;
end; { ReplaceAlphaChannelWithMask }

procedure ChangeAlphaChannelBySubMask(const Dest, Source, Mask: TBitmap32);
var
  i, Intensity, Delta: Integer;
  r, g, b            : Integer;
  OldOpacity         : Integer;
  NewOpacity         : Cardinal;
  s, d, m            : PColor32;
begin
  if  (Dest.Width = Source.Width) and (Dest.Height = Source.Height)
  and (Mask.Width = Source.Width) and (Mask.Height = Source.Height)  then
  begin
    s := @Source.Bits[0];
    d := @Dest.Bits[0];
    m := @Mask.Bits[0];
    for i := 0 to Dest.Width * Dest.Height - 1 do
    begin
      Intensity  := m^        and $FF;
      r          := d^ shr 16 and $FF;
      g          := d^ shr 8  and $FF;
      b          := d^        and $FF;
      OldOpacity := s^        and $FF;
      Delta      := $FF - Intensity;
      NewOpacity := IntToByte(OldOpacity - Delta);
      d^         := (NewOpacity shl 24) or (r shl 16) or (g shl 8) or b;
      Inc(s);
      Inc(d);
      Inc(m);
    end;
  end;
end; { ChangeAlphaChannelBySubMask }

procedure DrawStage(Stage: TBitmap32);
const
  Colors: array [0..1] of TColor32 = ($00FFFFFF, $00B0B0B0);
var
  W, I, J, Parity: Integer;
  Line1, Line2   : TArrayOfColor32; // a buffer for a couple of scanlines
begin
  with Stage do
  begin
    W := Width;
    SetLength(Line1, W);
    SetLength(Line2, W);

    for I := 0 to W - 1 do
    begin
      Parity   := I shr 2 and $1;
      Line1[I] := Colors[Parity];
      Line2[I] := Colors[1 - Parity];
    end;

    for J := 0 to Height - 1 do
    begin
      Parity := J shr 2 and $1;
      
      if   Boolean(Parity) then
        MoveLongword(Line1[0], ScanLine[J]^, W) else
        MoveLongword(Line2[0], ScanLine[J]^, W);
    end;
  end;
end; { DrawStage }

procedure ScaleImage32(const Source: TBitmap32; const AImage: TImage32; const Width, Height: Integer);
var
  SW, SH, Scale: Real;
  OK           : Boolean;
begin
  if   AImage.AutoSize
  then AImage.AutoSize := False;

  if   AImage.ScaleMode <> smStretch
  then AImage.ScaleMode := smStretch;

  Scale := 0.0;
  OK    := False;

  if (Source.Width <= Width) and (Source.Height <= Height) then
  begin
    AImage.Width  := Source.Width;
    AImage.Height := Source.Height;
    OK            := False;
  end
  else
  if (Source.Width > Width) and (Source.Height > Height) then
  begin
    SW := Source.Width  / Width;
    SH := Source.Height / Height;

    if   SW > SH
    then Scale := SW
    else Scale := SH;

    OK := True;
  end
  else
  if (Source.Width > Width) and (Source.Height <= Height) then
  begin
    Scale := Source.Width / Width;;
    OK    := True;
  end
  else
  if (Source.Width <= Width) and (Source.Height > Height) then
  begin
    Scale := Source.Height / Height;;
    OK    := True;
  end;

  if OK then
  begin
    AImage.Width  := Round(Source.Width  / Scale);
    AImage.Height := Round(Source.Height / Scale);
  end;

  AImage.Bitmap.Assign(Source);
end;

procedure CenterImage32InPanel(const APanel: TPanel; const AImage: TImage32);
var
  s: string;
begin
  if  (APanel.Width  >= AImage.Width)
  and (APanel.Height >= AImage.Height) then
  begin
    AImage.Left := APanel.Width  div 2 - AImage.Width  div 2 - 1;
    AImage.Top  := APanel.Height div 2 - AImage.Height div 2 - 1;
  end;
end;

procedure SetTransparency(const Bmp: TBitmap32; const TransparentColor: TColor32);
var
  i      : Integer;
  p      : PColor32;
  r, g, b: Byte;
begin
  Bmp.DrawMode := dmBlend;
  p := @Bmp.Bits[0];
  for i := 0 to Bmp.Width * Bmp.Height - 1 do
  begin
    if   p^ = TransparentColor
    then p^ := p^ and $00FFFFFF;
    Inc(p);
  end;
end;

//=========================================================================
//=================== TExLayers32Panel ===================================
//=========================================================================

constructor TExLayers32Panel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  self.VertScrollBar.Tracking:=true;
  self.VertScrollBar.Smooth:=true;
  FPanelsList:=TList.Create;

  FImgView            := nil;
  FBlendModeViewer    := nil;
  FMasterAlphaViewer  := nil;

end;

destructor TExLayers32Panel.Destroy;
begin

  OnSelectedPanelChange:=nil;
  FSelectedPanel := nil;
  clear;
  FPanelsList.Clear;
  FPanelsList.Free;
  inherited Destroy;
end;


Procedure TExLayers32Panel.BuildLayersFromImgView(const ClrType:integer=0);
 var i:integer;
     xl:TCustomLayer;
     lp:TExLayerPanel;
 begin

  if Assigned(FImgView)=false then exit;

  case ClrType of
   0:Clear;
   1:ClearExeptSelected;
   2:ClearOnlyPanels;
  end;

  HideAllLayerPanels;
  
  UpdateBackroundPanel;

  if Assigned(FImgView) then
   for I := 0 to FImgView.Layers.count - 1 do
    begin
      xl:=nil;
      xl:=fImgView.Layers[i];
      if (xl is TExtBitmapLayer)=true then
       begin
         lp:=CreateNewPanelFromLayer(TExtBitmapLayer(xl));
       end;
    end;

  ShowAllLayerPanels;
  UpdateAllPanels;
 end;


Procedure TExLayers32Panel.UpdateBackroundPanel;
var lp:TExLayerPanel;
 begin
   if FPanelsList.Count<1 then exit;

   lp:=TExLayerPanel(FPanelsList.Items[0]);
   if lp<>nil then
     if lp.IsBackround then lp.UpdateLayerThumbnail;
 end;

procedure TExLayers32Panel.SetImgView(val:TImgView32);
 var lp:TExLayerPanel;
begin
  FImgView := val;
  FSelectedPanel     := nil;
  FBlendModeViewer   := nil;
  FMasterAlphaViewer := nil;
  FDrawing := False;
  //..............................
  lp:=self.CreateBlankPanelWithNoLayer;
  lp.SetAsBackRoundImage(FImgView.Bitmap);
  lp.SetLayerName('Backround');

end;

procedure TExLayers32Panel.SetSelectedPanel(const Val:integer);
var lp: TExLayerPanel;
 begin
   if (val <0) or (val>FPanelsList.count-1) then exit;
   lp:=TExLayerPanel(FPanelsList.Items[Val]);
   if lp=nil then exit;
   lp.IsSelected:=true;
   SelectedPanel :=lp;
 end;

procedure TExLayers32Panel.SetSelectedPanel(aLayer:TExtBitmapLayer);
 var Lp: TExLayerPanel;
begin
  if alayer=nil then
   begin
     SetSelectedPanel(0);
   end else
   begin
    lp:=GeTExLayerPanel(alayer);
    if lp=nil then exit;
    lp.IsSelected:=true;
    SelectedPanel :=lp;
   end;
end;

procedure TExLayers32Panel.SetSelPanel(val:TExLayerPanel);
 begin
  unselectAll;
  FSelectedPanel:=val;

  if FSelectedPanel<>nil then FSelectedPanel.IsSelected:=true;

  UpdateAllPanels;
  if Assigned(FOnSelectedPanelChange) then FOnSelectedPanelChange(SelectedPanel);
 end;

Function  TExLayers32Panel.GetSelectedLayer:TExtBitmapLayer;
 begin
   result:=nil;
   if FSelectedPanel<>nil then result:=FSelectedPanel.Layer;     
 end;

procedure  TExLayers32Panel.Clear;
 var i:integer;
     LayerPanel : TExLayerPanel;
     Lyr        : TExtBitmapLayer;
     isSel:boolean;
 begin
  if FPanelsList.Count<1 then exit;

    for i := FPanelsList.Count - 1 downto 0 do
     begin
       LayerPanel := TExLayerPanel(FPanelsList.Items[i]);

       if LayerPanel.IsBackround=false then
        begin
         LayerPanel.FMainPanel.Visible := False;
         Lyr:=LayerPanel.Layer;

         if Lyr<>nil then
           if Assigned(FImgView) then
             FImgView.Layers.Delete(Lyr.Index);

         FPanelsList.Delete(i);
        end;
     end;
 end;

procedure  TExLayers32Panel.ClearExeptSelected;
 var i:integer;
     LayerPanel : TExLayerPanel;
     Lyr        : TExtBitmapLayer;
     isSel:boolean;
 begin
  if FPanelsList.Count<1 then exit;

    for i := FPanelsList.Count - 1 downto 0 do
     begin
       LayerPanel := TExLayerPanel(FPanelsList.Items[i]);

       if (LayerPanel.IsBackround=false) then        
       if (LayerPanel.IsSelected=false) then
          begin
           LayerPanel.FMainPanel.Visible := False;
           Lyr:=LayerPanel.Layer;

           if Lyr<>nil then
            if Assigned(FImgView) then
              FImgView.Layers.Delete(Lyr.Index);

           FPanelsList.Delete(i);
        end;
     end;
 end;

procedure  TExLayers32Panel.ClearOnlyPanels;
 var i:integer;
     LayerPanel : TExLayerPanel;
     isSel:boolean;
 begin
  if FPanelsList.Count<1 then exit;

    for i := FPanelsList.Count - 1 downto 0 do
     begin
       LayerPanel := TExLayerPanel(FPanelsList.Items[i]);

       if LayerPanel.IsBackround=false then
        begin
         LayerPanel.FMainPanel.Visible := False;
         FPanelsList.Delete(i);
        end;
     end;
 end;

procedure TExLayers32Panel.DeleteSelectedLayerPanel;
 begin
   DeletePanel(fSelectedPanel);
 end;

procedure TExLayers32Panel.DeletePanel(aLayerPanel:TExLayerPanel);
 begin
   if aLayerPanel=nil then exit;
   DeletePanel(aLayerPanel.Index);
 end;

procedure TExLayers32Panel.DeletePanel(const val:integer);
var
  Ni: Integer;
  LayerPanel : TExLayerPanel;
  Lyr        : TExtBitmapLayer;
  isSel:boolean;
begin

  if (val<0) or (Val>FPanelsList.Count-1) then exit;

  Ni:=val;
  isSel:=false;

  if FPanelsList.Count > 0 then
  begin
       LayerPanel := TExLayerPanel(FPanelsList.Items[Ni]);
      if LayerPanel.IsBackround=true then exit; //DOT Delete Backround LayerPanel

       isSel:=LayerPanel.IsSelected;
       Lyr:=LayerPanel.Layer;
       LayerPanel.FMainPanel.Visible := False;

       if Lyr<>nil then
        if Assigned(FImgView) then
         FImgView.Layers.Delete(Lyr.Index);

      // LayerPanel.Free;
       FPanelsList.Delete(ni);
       LayerPanel := nil;
  end;

  if isSel=false then exit;

  //....... set SelectedPanel ............
  
  if FPanelsList.Count-1>Ni then Ni:=Ni-1;
  if Ni<0 then begin SelectedPanel:=nil; exit; end;

  if FPanelsList.Count > 0 then
  begin
    LayerPanel            := TExLayerPanel(FPanelsList.Items[Ni-1]);
    LayerPanel.IsSelected := True;
    SelectedPanel        := LayerPanel;
    if  Assigned(FBlendModeViewer)
     then FBlendModeViewer.ItemIndex := SelectedPanel.BlendModeIndex;

    if   Assigned(FMasterAlphaViewer)
     then FMasterAlphaViewer.Position := SelectedPanel.LayerMasterAlpha;

    UpdateAllPanels;
  end;
   
end;

function  TExLayers32Panel.CreateBlankPanelWithNoLayer:TExLayerPanel;
var
  LayerPanel         : TExLayerPanel;
  Index              : Integer;
begin
  LayerPanel := TExLayerPanel.Create(FPanelsList, self, nil);
  AddLayerPanelToList(LayerPanel);
  Self.Update;
  result:=LayerPanel;
end;

function TExLayers32Panel.CreateBlankPanel:TExLayerPanel;
var
  NewLayer: TExtBitmapLayer;
  LayerPanel         : TExLayerPanel;
  Index              : Integer;
begin
  result:=nil;

  if NOT Assigned(FImgView) then exit;

  NewLayer := TExtBitmapLayer.Create(FImgView.Layers);
  LayerPanel := TExLayerPanel.Create(FPanelsList, self, NewLayer);
  AddLayerPanelToList(LayerPanel);
  Self.Update;
  result:=LayerPanel;
end;

function  TExLayers32Panel.CreateNewPanelFromLayer(aLayer:TExtBitmapLayer):TExLayerPanel;
   var Lp  : TExLayerPanel;
 begin
  lp := TExLayerPanel.Create(FPanelsList,self, aLayer);
   FPanelsList.Add(lp);
   SeTExLayerPanelName(lp);
   ConnectEventToLayerPanel(lp);
  result:=lp;
 end;

procedure TExLayers32Panel.EnableMask;
begin
  if SelectedPanel.IsEnabledMask = False then
  begin
    GetAlphaChannelBitmap(SelectedPanel.Layer.Bitmap, SelectedPanel.FLastAlphaChannelBmp);
    with SelectedPanel do
    begin
      FMaskImage.Bitmap.Clear(clWhite32);
      LayerProcessStage := lpsMask;
      IsEnabledMask     := True;
      ShowThumbnailByRightOrder;
    end;

    UpdateAllPanels;
  end;
end;

procedure TExLayers32Panel.DeleteSelectedLayer;
var
  LayerName  : string;
  ModalResult: TModalResult;
begin
  if FPanelsList.Count > 0 then
  begin
    case SelectedPanel.LayerProcessStage of
      lpsLayer:
        begin
          if FPanelsList.Count > 1 then
          begin
            LayerName := '"' + SelectedPanel.FLayerNameLabel.Caption + '"';
            DeleteSelectedLayerPanel;
          end;
        end;
      lpsMask:
        begin
          ModalResult := MessageDlg('Apply mask to layer before removing?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
          case ModalResult of
            mrYes:
              begin
                // Applying the mask.
                SelectedPanel.LayerProcessStage := lpsLayer;
                SelectedPanel.IsEnabledMask     := False;
                SelectedPanel.ShowThumbnailByRightOrder;
                UpdateAllPanels;
                GetAlphaChannelBitmap(SelectedPanel.Layer.Bitmap, SelectedPanel.FLastAlphaChannelBmp);
              end;
            mrNo:
              begin
                // Discarding the mask.
                SelectedPanel.LayerProcessStage := lpsLayer;
                SelectedPanel.IsEnabledMask     := False;
                SelectedPanel.ShowThumbnailByRightOrder;
                ReplaceAlphaChannelWithMask(SelectedPanel.Layer.Bitmap, SelectedPanel.FLastAlphaChannelBmp);
                SelectedPanel.Layer.Changed;
                UpdateAllPanels;
                SelectedPanel.UpdateLayerThumbnail;
              end;
          end;
        end;
    end;
  end;
end;

procedure TExLayers32Panel.ArrangeSelectedLayer(const ArrangeMode: TArrangeLayerMode);
var
  CurIndex,CurLayIndex, NewIndex: Integer;
begin
  HideAllLayerPanels;  
  CurIndex := FSelectedPanel.Index;
  CurLayIndex := FSelectedPanel.Layer.Index;

  case ArrangeMode of
    almBringToFront:
      begin
        NewIndex := FPanelsList.Count - 1;
        SelectedPanel.Layer.BringToFront;
      end;

    almSendToBack:
      begin
        NewIndex := 1;
        SelectedPanel.Layer.SendToBack;
      end;

    almUpOneLevel:
      begin
        NewIndex := CurIndex + 1;
        if NewIndex > FPanelsList.Count - 1 then NewIndex := FPanelsList.Count - 1;

        if CurLayIndex<FImgView.Layers.Count-2 then SelectedPanel.Layer.Index := CurLayIndex+1;
      end;

    almDownOneLevel:
      begin
        NewIndex := CurIndex - 1;
        if   NewIndex < 0 then NewIndex := 0;

        if CurLayIndex>0 then SelectedPanel.Layer.Index := CurLayIndex-1;
      end;

    
  end;

  SelectedPanel.Layer.Bitmap.Changed;

  if NewIndex>0 then FPanelsList.Move(CurIndex, NewIndex);

  ShowAllLayerPanels;
end;

procedure TExLayers32Panel.LayerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if Assigned(SelectedPanel.Layer) and Assigned(SelectedPanel) then
  begin
    P := (Sender as TImgView32).ControlToBitmap(Point(X, Y));
    case SelectedPanel.LayerProcessStage of
      lpsLayer:
        begin
          if   SelectedPanel.IsEnabledMask
          then ReplaceAlphaChannelWithMask(SelectedPanel.Layer.Bitmap, SelectedPanel.FLastAlphaChannelBmp);

          SelectedPanel.Layer.Bitmap.MoveTo(P.X, P.Y);
          SelectedPanel.Layer.Bitmap.LineToS(P.X, P.Y);

          if SelectedPanel.IsEnabledMask then
          begin
            SelectedPanel.FLastAlphaChannelBmp.MoveTo(P.X, P.Y);
            SelectedPanel.FLastAlphaChannelBmp.LineToS(P.X, P.Y);
            ApplyMask;
          end
          else SelectedPanel.Layer.Changed;
        end;
      lpsMask:
        begin
          SelectedPanel.FMaskImage.Bitmap.MoveTo(P.X, P.Y);
          SelectedPanel.FMaskImage.Bitmap.LineToS(P.X, P.Y);
          ApplyMask;
        end;
    end;
    FDrawing := True;
  end
  else FDrawing := False;
end;

procedure TExLayers32Panel.LayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
var
  P: TPoint;
begin
  if Assigned(SelectedPanel.Layer) and Assigned(SelectedPanel) then
  begin
    if FDrawing then
    begin
      P := (Sender as TImgView32).ControlToBitmap(Point(X, Y));
      case SelectedPanel.LayerProcessStage of
        lpsLayer:
          begin
            if   SelectedPanel.IsEnabledMask
            then ReplaceAlphaChannelWithMask(SelectedPanel.Layer.Bitmap, SelectedPanel.FLastAlphaChannelBmp);

            SelectedPanel.Layer.Bitmap.LineToS(P.X, P.Y);

            if SelectedPanel.IsEnabledMask then
            begin
              SelectedPanel.FLastAlphaChannelBmp.LineToS(P.X, P.Y);
              ApplyMask;
            end
            else SelectedPanel.Layer.Bitmap.Changed;
          end;
        lpsMask:
          begin
            SelectedPanel.FMaskImage.Bitmap.LineToS(P.X, P.Y);
            ApplyMask;
          end;
      end;
    end;
  end
end;

procedure TExLayers32Panel.LayerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if FDrawing then
  begin
    if Assigned(SelectedPanel) then
    begin
      case SelectedPanel.LayerProcessStage of
        lpsLayer: SelectedPanel.UpdateLayerThumbnail;
        lpsMask : SelectedPanel.UpdateMaskThumbnail;
      end;
    end;
    FDrawing := False;
  end;
end; 

procedure TExLayers32Panel.UpdateAllPanels;
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
  if FPanelsList.Count > 0 then
  begin
    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);

      case LayerPanel.IsSelected of
        True:
          begin
             LayerPanel.FMainPanel.BevelInner      := bvLowered;
             LayerPanel.FMainPanel.Color           := clred;

             LayerPanel.FLayerNameLabel.Color      := clred;
             LayerPanel.FLayerNameLabel.Font.Color := clred;

            case LayerPanel.LayerProcessStage of
              lpsLayer: LayerPanel.FStageImage.Bitmap.LoadFromResourceName(hInstance, 'XGR32_PAINTBRUSH');
              lpsMask : LayerPanel.FStageImage.Bitmap.LoadFromResourceName(hInstance, 'XGR32_MASK');
            end;

            SetTransparency(LayerPanel.FStageImage.Bitmap, clWhite32);
            
            if not LayerPanel.FStageImage.Visible then LayerPanel.FStageImage.Visible := True;

          end;

        False:
          begin
            LayerPanel.FMainPanel.BevelInner      := bvRaised;

            if LayerPanel.IsBackround then
             begin
               LayerPanel.FMainPanel.Color           := clblue;
               LayerPanel.FLayerNameLabel.Font.Color := clblue;
             end else
             begin
               LayerPanel.FMainPanel.Color           := clBtnFace;
               LayerPanel.FLayerNameLabel.Font.Color := clBlack;
             end;

            if LayerPanel.FStageImage.Visible then LayerPanel.FStageImage.Visible := False;
          end;
      end;

    end;
  end;
end;


procedure TExLayers32Panel.MainPanelClick(Sender: TObject);
var
  i, CurIndex: Integer;
  LayerPanel : TExLayerPanel;
begin
  // Get current index.
  for i := 0 to FPanelsList.Count - 1 do
  begin
    LayerPanel :=TExLayerPanel(FPanelsList.Items[i]);
    if (Sender = LayerPanel.FMainPanel)
    or (Sender = LayerPanel.FLayerNameLabel)
    or (Sender = LayerPanel.FLayerImage)
    or (Sender = LayerPanel.FLayerImageHolder)
    or (Sender = LayerPanel.FMaskImage)
    or (Sender = LayerPanel.FMaskImageHolder) then
    begin
      CurIndex := i;
      Break;
    end;
  end;

  if CurIndex = FSelectedPanel.Index then Exit;

  if FPanelsList.Count > 0 then
  begin
    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel            := TExLayerPanel(FPanelsList.Items[i]);
      LayerPanel.IsSelected := False;
    end;

    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);
      if (Sender = LayerPanel.FMainPanel)
      or (Sender = LayerPanel.FLayerNameLabel)
      or (Sender = LayerPanel.FLayerImage)
      or (Sender = LayerPanel.FLayerImageHolder)
      or (Sender = LayerPanel.FMaskImage)
      or (Sender = LayerPanel.FMaskImageHolder) then
      begin
        LayerPanel.IsSelected := True;

        SelectedPanel:= LayerPanel;
        UpdateAllPanels;

        if Assigned(FBlendModeViewer)
          then FBlendModeViewer.ItemIndex := SelectedPanel.BlendModeIndex;

        if Assigned(FMasterAlphaViewer)
          then FMasterAlphaViewer.Position := SelectedPanel.LayerMasterAlpha;

        Break;
      end;
    end;
  end;
end;

procedure TExLayers32Panel.EyeHolderClick(Sender: TObject);
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
  if FPanelsList.Count > 0 then
  begin
    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);
      if (Sender = LayerPanel.FEyeImage)
      or (Sender = LayerPanel.FEyeHolder) then
      begin
        if LayerPanel.IsLayerVisible  then
            LayerPanel.IsLayerVisible := False else
            LayerPanel.IsLayerVisible := True;

        LayerPanel.UpdateLayerVisibleState;

        if LayerPanel.IsLayerVisible then
         begin
           LayerPanel.FEyeImage.Bitmap.LoadFromResourceName(hInstance, 'XGR32_EYE');
           SetTransparency(LayerPanel.FEyeImage.Bitmap, clWhite32);
         end else
         begin
           LayerPanel.FEyeImage.Bitmap.Clear($00000000);
         end;
        //========================================================================
        //=========================================================================
        //========================================================================
        //=========================================================================
        if LayerPanel.Layer<>nil then
         begin
           LayerPanel.FLayer.Bitmap.Changed;
           LayerPanel.Layer.Visible:=LayerPanel.IsLayerVisible;
          end;
          
        Break;
      end;
    end;
  end;
end;

procedure TExLayers32Panel.LayerImageHolderClick(Sender: TObject);
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
  if FPanelsList.Count > 0 then
  begin
    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);
      if (Sender = LayerPanel.FLayerImage)
      or (Sender = LayerPanel.FLayerImageHolder) then
      begin
        if LayerPanel.LayerProcessStage <> lpsLayer then
        begin
          FSelectedPanel:=LayerPanel;
          LayerPanel.LayerProcessStage := lpsLayer;
          UpdateAllPanels;
        end;
        Break;
      end;
    end;

    MainPanelClick(Sender);
  end;
end;

procedure TExLayers32Panel.MaskImageHolderClick(Sender: TObject);
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
  if FPanelsList.Count > 0 then
  begin
    for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);
      if (Sender = LayerPanel.FMaskImage)
      or (Sender = LayerPanel.FMaskImageHolder) then
      begin
        if LayerPanel.LayerProcessStage <> lpsMask then
        begin
          FSelectedPanel:=LayerPanel;
          LayerPanel.LayerProcessStage := lpsMask;
          UpdateAllPanels;
        end;
        Break;
      end;
    end;
    MainPanelClick(Sender);
  end;
end;

procedure TExLayers32Panel.ConnectEventToLayerPanel(LayerPanel: TExLayerPanel);
begin
  LayerPanel.FMainPanel.OnClick        := @MainPanelClick;
  LayerPanel.FLayerNameLabel.OnClick   := @MainPanelClick;
  LayerPanel.FEyeImage.OnClick         := @EyeHolderClick;
  LayerPanel.FEyeHolder.OnClick        := @EyeHolderClick;
  LayerPanel.FLayerImage.OnClick       := @LayerImageHolderClick;
  LayerPanel.FLayerImageHolder.OnClick := @LayerImageHolderClick;
  LayerPanel.FMaskImage.OnClick        := @MaskImageHolderClick;
  LayerPanel.FMaskImageHolder.OnClick  := @MaskImageHolderClick;
end;

procedure TExLayers32Panel.AddLayerPanelToList(AItem: Pointer);
var  Lp : TExLayerPanel;
begin
  UnSelectAll;

  FPanelsList.Add(AItem);
  Lp := TExLayerPanel(FPanelsList.Items[FPanelsList.Count - 1]);
  SelectedPanel := Lp;
  SeTExLayerPanelName(Lp);
  ConnectEventToLayerPanel(Lp);

  HideAllLayerPanels;
  ShowAllLayerPanels;  

  if Assigned(FBlendModeViewer) then FBlendModeViewer.ItemIndex := SelectedPanel.BlendModeIndex;
  if Assigned(FMasterAlphaViewer)then FMasterAlphaViewer.Position := SelectedPanel.LayerMasterAlpha;
end;

Procedure TExLayers32Panel.UnSelectAll;
 var
  i          : Integer;
  LayerPanel : TExLayerPanel;
 begin
   for i := 0 to FPanelsList.Count - 1 do
    begin
      LayerPanel:= TExLayerPanel(FPanelsList.Items[i]);
      LayerPanel.IsSelected := False;
    end;
 end;

procedure TExLayers32Panel.SeTExLayerPanelName(var LayerPanel: TExLayerPanel);
 var ss:string;
 begin
   if LayerPanel=nil then exit;
   ss:= 'Layer' + IntToStr(LayerPanel.Index+1);

   if LayerPanel.Layer<>nil then
    if LayerPanel.Layer.Name='' then
       LayerPanel.Layer.Name:=ss else
       ss:=LayerPanel.Layer.Name;


   LayerPanel.SetLayerName(ss);
 end;

procedure TExLayers32Panel.SetSelectedPanelName(const aName:string);
 begin
   if SelectedPanel=nil then exit;

   SelectedPanel.SetLayerName(aName);
   if SelectedPanel.Layer<>nil then SelectedPanel.Layer.Name:=aName;
 end;

procedure TExLayers32Panel.InserTExLayerPanelToList(const Index: Integer; const AItem: Pointer);
  var LayerPanel : TExLayerPanel;
begin
  UnSelectAll;

  FPanelsList.Insert(Index, AItem);
  LayerPanel     := TExLayerPanel(FPanelsList.Items[Index]);
  FSelectedPanel:=LayerPanel;
  SeTExLayerPanelName(LayerPanel);
  ConnectEventToLayerPanel(LayerPanel);
  UpdateAllPanels;
  HideAllLayerPanels;
  ShowAllLayerPanels;


  SelectedPanel := LayerPanel;

  if   Assigned(FBlendModeViewer)
  then FBlendModeViewer.ItemIndex := SelectedPanel.BlendModeIndex;

  if   Assigned(FMasterAlphaViewer)
  then FMasterAlphaViewer.Position := SelectedPanel.LayerMasterAlpha;
end;

procedure TExLayers32Panel.HideAllLayerPanels;
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
  if FPanelsList.Count<1 then exit;

  for i := FPanelsList.Count - 1 downto 0 do
    begin
      LayerPanel := TExLayerPanel(FPanelsList.Items[i]);
      layerPanel.FMainPanel.Hide;
    end;

end;

procedure TExLayers32Panel.ShowAllLayerPanels;
var
  i         : Integer;
  LayerPanel: TExLayerPanel;
begin
 if FPanelsList.Count<1 then exit;

 for i := FPanelsList.Count - 1 downto 0 do
  begin
      LayerPanel                 := TExLayerPanel(FPanelsList.Items[i]);
      LayerPanel.FMainPanel.Left := 0;
      LayerPanel.FMainPanel.Top  := (FPanelsList.Count - 1 - i) * LayerPanel.FMainPanel.Height;
      LayerPanel.FMainPanel.Show;
   end;

end;  

procedure TExLayers32Panel.ApplyMask;
begin
  if Assigned(SelectedPanel.Layer) and Assigned(SelectedPanel) then
  begin
    ChangeAlphaChannelBySubMask(SelectedPanel.Layer.Bitmap,
                                SelectedPanel.FLastAlphaChannelBmp,
                                SelectedPanel.FMaskImage.Bitmap);
    SelectedPanel.Layer.Changed;
  end;
end;

function  TExLayers32Panel.GeTExLayerPanel(const Index: Integer): TExLayerPanel;
 begin
   Result:=nil;
   if (index<0) or (Index>FPanelsList.count) then exit;
   Result := TExLayerPanel(FPanelsList.items[Index]);
 end;

function  TExLayers32Panel.GeTExLayerPanel(aLayer:TExtBitmapLayer): TExLayerPanel;
 var i:integer;
     Lp:TExLayerPanel;
 begin
    Result:=nil;

    for I := 0 to FPanelsList.Count - 1 do
      begin
       Lp:=TExLayerPanel(FPanelsList.items[i]);
       if Lp.Layer=aLayer then begin result:=lp; exit; end;
      end;
 end;

//==============================================================================
//== TExLayerPanel Class =========================================================
//==============================================================================

constructor TExLayerPanel.Create(aParentList:TList; aPerentWinControl: TWinControl; ALayer: TExtBitmapLayer);
begin
  inherited Create;
  FPerentWinControl     :=aPerentWinControl;
  fParentList           :=aParentList;
  FLayer                := ALayer;
  FisSelected           := false;
  FIsLayerVisible       := True;
  FBlendModeIndex       := 0;
  FBlendModeEvent       := @BlendMode.NormalBlend;
  FLayerProcessStage    := lpsLayer;
  FIsEnabledMask        := False;
  FLayerMasterAlpha     := 255;

  fBackRoundImage:=nil;
  IsBackround:= false;

{ Create FMainPanel  }

  FMainPanel := TPanel.Create(aPerentWinControl);
  with FMainPanel do
  begin
    Parent     := aPerentWinControl;
    Align      := alTop;
    AutoSize   := False;
    Height     := MAIN_PANEL_HEIGHT;
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    BevelWidth := 1;
    ParentColor:=false;
    Color      := clBackground;
    ShowHint   := False;
    Visible    := False;
  end;

{ Create FLayerImageHolder }

  FLayerImageHolder := TPanel.Create(FMainPanel);
  with FLayerImageHolder do
  begin
    Parent     := FMainPanel;
    Align      := alLeft;
    AutoSize   := False;
    Width      := IMAGE_HOLDER_WIDTH;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
    BevelWidth := 1;
    Cursor     := crHandPoint;
    Visible    := True;
  end;

{ Create FLayerImage }

  FLayerImage := TImage32.Create(FLayerImageHolder);
  with FLayerImage do
  begin
    Parent                := FLayerImageHolder;
    Width                 := IMAGE_WIDTH;
    Height                := IMAGE_HEIGHT;
    Bitmap.DrawMode       := dmCustom;
    Bitmap.OnPixelCombine := @BlendMode.NormalBlend;
    AutoSize              := False;
    ScaleMode             := smStretch;
    Cursor                := crHandPoint;
    Hint                  := 'Layer thumbnail';
    ShowHint              := True;
    Visible               := True;
  end;

  // Force TImage32 to call the OnPaintStage event instead of performming
  // default action.

  with FLayerImage.PaintStages[0]^ do
  begin
     if   Stage = PST_CLEAR_BACKGND
     then Stage := PST_CUSTOM;
  end;
  FLayerImage.OnPaintStage := @LayerImagePaintStage;

{ Create FMaskImageHolder }

  FMaskImageHolder := TPanel.Create(FMainPanel);
  with FMaskImageHolder do
  begin
    Parent     := FMainPanel;
    Align      := alLeft;
    AutoSize   := False;
    Width      := IMAGE_HOLDER_WIDTH;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
    BevelWidth := 1;
    Cursor     := crHandPoint;
    Visible    := False;
  end;

{ Create FMaskImage }

  FMaskImage := TImage32.Create(FMaskImageHolder);
  with FMaskImage do
  begin
    Parent          := FMaskImageHolder;
    Width           := IMAGE_WIDTH;
    Height          := IMAGE_HEIGHT;
    AutoSize        := False;
    ScaleMode       := smStretch;
    Bitmap.DrawMode := dmOpaque;
    Scale           := 1;
    Cursor          := crHandPoint;
    Hint            := 'Layer mask thumbnail';
    ShowHint        := True;
    Visible         := True; 

    Bitmap.PenColor := clBlack32;

    if FLayer=nil then
       Bitmap.SetSize(0,0) else
       Bitmap.SetSize(FLayer.Bitmap.Width, FLayer.Bitmap.Height);

    Bitmap.FillRect(0, 0, Bitmap.Width, Bitmap.Height, clWhite32);

  end;
  SetImage32(FMaskImage.Bitmap, FMaskImage, FMaskImageHolder);

{ Create FStageHolder }

  FStageHolder := TPanel.Create(FMainPanel);
  with FStageHolder do
  begin
    Parent     := FMainPanel;
    Align      := alLeft;
    AutoSize   := False;
    Width      := PANEL_STAGE_WIDTH;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
    BevelWidth := 1;
    Left       := 0;
    Top        := 0;
    Visible    := True;
  end;

{ Create FStageImage }

  FStageImage := TImage32.Create(FStageHolder);
  with FStageImage do
  begin
    Parent         := FStageHolder;
    AutoSize       := False;
    ScaleMode      := smStretch;
    Width          := IMAGE_STAGE_WIDTH;
    Height         := IMAGE_STAGE_HEIGHT;
    Top            := (FStageHolder.Height - FStageImage.Height) div 2;
    Left           := (FStageHolder.Width  - FStageImage.Width)  div 2;
    // Indicate the process stage is layer.
    Bitmap.LoadFromResourceName(hInstance, 'XGR32_PAINTBRUSH');
    SetTransparency(Bitmap, clWhite32);
    Visible := True;
  end;

{ Create FEyeHolder }

  FEyeHolder := TPanel.Create(FMainPanel);
  with FEyeHolder do
  begin
    Parent     := FMainPanel;
    Align      := alLeft;
    AutoSize   := False;
    Cursor     := crHandPoint;
    Width      := PANEL_STAGE_WIDTH;
    BevelInner := bvRaised;
    BevelOuter := bvLowered;
    BevelWidth := 1;
    Visible    := True;
  end;

{ Create FEyeImage }

  FEyeImage := TImage32.Create(FEyeHolder);
  with FEyeImage do
  begin
    Parent    := FEyeHolder;
    AutoSize  := False;
    ScaleMode := smStretch;
    Cursor    := crHandPoint;
    Width     := IMAGE_STAGE_WIDTH;
    Height    := IMAGE_STAGE_HEIGHT;
    Top       := (FEyeHolder.Height - Height) div 2;
    Left      := (FEyeHolder.Width  - Width)  div 2;
    // Indicate the layer is visible.
    Bitmap.LoadFromResourceName(hInstance, 'XGR32_EYE');
    SetTransparency(Bitmap, clWhite32);
    Visible := True;
  end;

{ Create FLayerNameLabel }

  FLayerNameLabel := TLabel.Create(FMainPanel);
  with FLayerNameLabel do
  begin
    Parent     := FMainPanel;
    Align      := alNone;
    Left       := FEyeHolder.Width + FStageHolder.Width +
                  FLayerImageHolder.Width + FMaskImageHolder.Width + 10;
    Top        := (FMainPanel.Height - Height) div 2;
    Caption    := '';
    ParentColor:=false;
    Font.Color := clWhite;
    ShowHint   := True;
    Visible    := True;
  end;

  // Store the alpha channel by a bitmap32.
  FLastAlphaChannelBmp          := TBitmap32.Create;
  FLastAlphaChannelBmp.DrawMode := dmOpaque;
  
  if fLayer<>nil then
   begin
    GetAlphaChannelBitmap(fLayer.Bitmap, FLastAlphaChannelBmp);
    FLayer.Bitmap.DrawMode := dmCustom;
    FLayer.Bitmap.OnPixelCombine := @LayerPixelBlend;
   end;
  //................................................... 
  UpdateLayerThumbnail;
  if fLayer<>nil then SetImage32(fLayer.Bitmap, FLayerImage, FLayerImageHolder);
  ShowThumbnailByRightOrder;

end;

destructor TExLayerPanel.Destroy;
begin
  FLayerImage.Free;
  FMaskImage.Free;
  FStageImage.Free;
  FLayerImageHolder.Free;
  FMaskImageHolder.Free;
  FStageHolder.Free;
  FMainPanel.Free;
  FLayerNameLabel.Free;
  FLastAlphaChannelBmp.Free;
  FLayer := nil;
  inherited Destroy;
end;

Function  TExLayerPanel.GetIndex:Integer;
 var i:integer;
 begin
   result:=fParentList.IndexOf(self);
 end;


procedure TExLayerPanel.Assign(Value:TExLayerPanel);
 begin
   if Value=nil then exit;
   
 end;

procedure TExLayerPanel.LayerImagePaintStage(Sender: TObject; Buffer: TBitmap32; StageNum: Cardinal);
begin
  DrawStage(Buffer);
end;

procedure TExLayerPanel.SetLayerMasterAlpha(const Alpha: Byte);
begin
  if FIsLayerVisible then
  begin
    if FLayerMasterAlpha <> Alpha then
    begin
      FLayerMasterAlpha  := Alpha;
      FLayer.Bitmap.MasterAlpha := Alpha;
      FLayer.Bitmap.Changed;
    end;
  end;
end;

procedure TExLayerPanel.SetBlendModeIndex(const Index: Integer);
begin
  if FBlendModeIndex <> Index then
  begin
    FBlendModeIndex := Index;
    FBlendModeEvent := GetBlendMode(FBlendModeIndex);
    FLayer.Bitmap.OnPixelCombine := FBlendModeEvent;
    FLayer.Bitmap.Changed;
  end;
end;

procedure TExLayerPanel.SetImage32(ABitmap: TBitmap32; AImage: TImage32; APanel: TPanel);
begin
  ScaleImage32(ABitmap, AImage, IMAGE_WIDTH, IMAGE_HEIGHT);
  CenterImage32InPanel(APanel, AImage);
end;

procedure TExLayerPanel.SetLayerName(const Name: string);
begin
  FLayerNameLabel.Caption := Name;
  FLayerNameLabel.Hint    := Name;
end;

procedure TExLayerPanel.SetAsBackRoundImage(abmp:TBitmap32);
 begin
  fBackRoundImage:=abmp;
  flayer:=nil;
  FisBackround:=true;
 end;

procedure TExLayerPanel.UpdateLayerThumbnail;
begin
  if FLayer<>nil then
   SetImage32(FLayer.Bitmap, FLayerImage, FLayerImageHolder) else

   if fBackRoundImage<>nil then SetImage32(fBackRoundImage, FLayerImage, FLayerImageHolder)

end;

procedure TExLayerPanel.UpdateMaskThumbnail;
begin
  SetImage32(FMaskImage.Bitmap, FMaskImage, FMaskImageHolder);
end;

procedure TExLayerPanel.UpdateLayerVisibleState;
begin
  if FisBackround then exit;

  if  FIsLayerVisible then
    FLayer.Bitmap.MasterAlpha := FLayerMasterAlpha else
    FLayer.Bitmap.MasterAlpha := 0;

  FLayer.Bitmap.Changed;
end;

procedure TExLayerPanel.ShowThumbnailByRightOrder;
begin
  FEyeHolder.Left           := 1;
  FStageHolder.Left         := FEyeHolder.Left + FEyeHolder.Width;
  FLayerImageHolder.Left    := FStageHolder.Left + FStageHolder.Width;
  FMaskImageHolder.Left     := FLayerImageHolder.Left + FLayerImageHolder.Width;
  FEyeHolder.Visible        := True;
  FStageHolder.Visible      := True;
  FLayerImageHolder.Visible := True;
  if   FIsEnabledMask
  then FMaskImageHolder.Visible := True
  else FMaskImageHolder.Visible := False;
end;

procedure TExLayerPanel.LayerPixelBlend(F: TColor32; var B: TColor32; M: TColor32);
begin
  FBlendModeEvent(F, B, M);
end;


end.
