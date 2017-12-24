unit fonctions_images;

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses Forms,
{$IFDEF FPC}
  LCLIntf, zstream,
{$ELSE}
  Windows,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  DB, Graphics, Classes,
  ImgList, fonctions_string,
{$IFDEF BGRA}
  bgrabitmap,bgrabitmaptypes,
{$ELSE}
  ImagingTypes,
  ImagingComponents,
  Imaging,
{$ENDIF}
  Controls, Dialogs ;

const CST_EXTENSION_JPEG           = '.jpg' ;
      CST_EXTENSION_GIF            = '.gif' ;
{$IFDEF VERSIONS}
  gVer_fonctions_images : T_Version = ( Component : 'Gestion des images' ; FileUnit : 'fonctions_images' ;
                        			             Owner : 'Matthieu Giroux' ;
                        			              Comment : 'Chargement des icônes et bitmap ( vérifier des erreurs éventuelles avec Memproof ).' + #13#10 + 'Gestion des images.' ;
                        			              BugsStory : 'Version 1.0.3.3 : Resize Bug.' + #13#10 +
                        			                	  'Version 1.0.3.2 : Lazarus 1.6 windows.' + #13#10 +
                        			                	  'Version 1.0.3.1 : Lazarus 1.6.' + #13#10 +
                        			                	  'Version 1.0.3.0 : Using BGRACustomBitmap.' + #13#10 +
                        			                	  'Version 1.0.2.1 : Errors on BGRABitmap debug.' + #13#10 +
                        			                	  'Version 1.0.2.0 : Introducing BGRABitmap.' + #13#10 +
                        			                	  'Version 1.0.1.5 : try finally added.' + #13#10 +
                        			                	  'Version 1.0.1.4 : Stream resizing.' + #13#10 +
                        			                	  'Version 1.0.1.3 : No Bitmap bug.' + #13#10 +
                        			                	  'Version 1.0.1.2 : UTF 8.' + #13#10 +
                        			                	  'Version 1.0.1.1 : Improving p_ChangeTailleBitmap.' + #13#10 +
                        			                	  'Version 1.0.1.0 : Testing and saving to file.' + #13#10 +
                        			                	  'Version 1.0.0.5 : Testing Imaging.' + #13#10 +
                        			                	  'Version 1.0.0.4 : Bug couleur transparente en noir dans les imagelist.' + #13#10 +
                        			                	  'Version 1.0.0.3 : Handle à 0 après FreeImage et create des TBitmap.' + #13#10 +
                        			                	  'Version 1.0.0.2 : Suppression du RealeaseHanlde après FreeImage.' + #13#10 +
                        			                	  'Version 1.0.0.1 : Meilleure gestion des images, problèmes de rafraichissement.' + #13#10 +
                        			                	  'Version 1.0.0.0 : La gestion est en place.' + #13#10 + 'Il faut utiliser les fonctions et vérifier les erreurs éventuellement produites avec Memproof.';
                        			              UnitType : 1 ;
                        			              Major : 1 ; Minor : 0 ; Release : 3 ; Build : 3 );

{$ENDIF}

var gby_CompressionJpeg : Byte = 68;

type EFileImageError = EFilerError;

// Charge un icône ou un bitmap dans un champ bitmap si le bitmap est assez grand par rapport à ai_Taille
// aod_ChargerImage : Chargement du fichier image
// aF_FieldImage    : Champ image à enregistrer
// ai_Taille        : La taille à modifier : -1 pour ne pas modifier
// ab_MontreMessage : Interaction avec l'utilisateur
// adxb_Image       : Bitmap de visualisation
// Sortie           : Enregistré ou non

function fb_ChargeIcoBmp ( const aod_ChargerImage : TOpenDialog ;
                           const adat_DataSet     : TDataSet    ;
                           const aF_FieldImage    : TField      ;
                           const ai_Taille        : Integer     ;
                           const ab_MontreMessage : Boolean     ;
                           const adxb_Image       : TBitmap     ) : Boolean ;

procedure p_DestroyBitmapWithoutMemoryLeak ( const Abitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} );
procedure p_ClearBitmapWithoutMemoryLeak ( const Abitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} );
function  fb_ImageFieldToFile ( const field : TField ; const afile: String; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False ) : Boolean;
function  fb_FiletoImageField ( const afile: String; const field : TField ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False ) : Boolean;
function  fci_StreamToCustomImage ( const Stream : TStream ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True ) : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
function  fb_StreamToFile ( const Stream : TStream ; const afile : String; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True ; const ab_ShowError : Boolean = False ) : Boolean;
procedure p_ImageToStream ( const lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const Stream : TStream ; const Extension : String='JPG'; const FileName : String=''; const ACompress : Boolean=False; const ACompression :
 {$IFDEF BGRA}Tcompressionlevel = clmax {$ELSE}TDDSCompresion=dcDXT5{$ENDIF} ; const ab_ShowError : Boolean = False);
procedure p_ImageFieldToStream ( const field : TField ; const ast_memory_stream: tMemoryStream ; const ab_ShowError : Boolean = False );
procedure p_ImageFileToField ( const afile: String; const field : TField ; const ab_ShowError : Boolean = False );
procedure p_StreamToField ( const astream: TStream; const field : TField ; const ab_ShowError : Boolean = False );
procedure p_FieldToImage ( const field : TField ; const Image : TBitmap ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False );
function  fci_GetCustomImage :{$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
function  fci_BitmapToCustomImage ( const ab_Bitmap : TBitmap ):{$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
procedure p_ImageToStreamJpeg ( const abb_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const Stream : TStream );
procedure p_FileToStream ( const afile : String; const Stream : TStream ; const ab_ShowError : Boolean = False );
procedure p_FreeCustomImage ( var abb_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF} );
procedure p_StreamToImage ( const stream: tStream; const Image : TBitmap ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False );
function fci_FileToCustomImage ( const afile : String; const ab_ShowError : Boolean = False ):{$IFDEF BGRA}TBGRABitmap{$ELSE}TImageData{$ENDIF};
procedure p_FileToBitmap ( const afile : String; const abmp_Image : TBitmap ; const ab_ShowError : Boolean = False );
procedure p_FileToImage ( const afile : String; const Image : TPicture ; const ab_ShowError : Boolean = False );
procedure p_ChangeTailleBitmap ( const abmp_BitmapOrigine : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF};
                                 const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ; const ab_KeepProportion : Boolean = False );
function fPoi_FromAlignToCoord ( const AWidthIn, AHeightIn, ASurfaceWidth, ASurfaceHeight : Integer ; AAlign : TAlign ) : TPoint ;
procedure p_DrawImageFromList ( const ACanvas : TCanvas; const AImages : TCustomImageList ; const AImageIndex : Integer ; const X : Integer = 0 ; const Y : Integer = 0 );
procedure p_DrawImageFromListToBitmap ( const ABitmap : TBitmap; const AImages : TCustomImageList ; const AImageIndex : Integer; const AColor : TColor; const X : Integer = 0 ; const Y : Integer = 0 );
procedure p_DrawEventualImageFromListToBitmap ( const ABitmap : TBitmap; const AImages : TCustomImageList ; const AImageIndex, AWidth, AHeight : Integer; const AColor : TColor; const AAlign : TAlign = alLeft );
function fb_ResizeImage ( var Fdata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ):Boolean;
function fb_ResizeCustomImage ( var Fdata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ; const ab_KeepProportion : Boolean = True ):Boolean;
function fs_SeparateTextFromWidth ( ASText : String; const ANeededWidth : Integer; const Acanvas : TCanvas; const Ach_separator : String = ' ' ) : TStringAArray ;

function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const ab_AjouteBitmap      ,
                                        ab_ImageDefaut       : Boolean     ;
                                  const aIma_ImagesMenus     : TCustomImageList  ;
                                  const ai_CompteurImageDef : Integer     ) : Integer ; overload ;
// Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const aIma_ImagesMenus     : TCustomImageList  ) : Integer ; overload ;
function fb_SaveBitmaptoFile(const abmp_Bitmap : TBitmap; const afile: String): Boolean;
// Transforme un bitmap en tout petit bitmap
// Entrée : Le Bitmap source
// Sortie : Le petit bitmap
procedure p_RecuperePetitBitmap ( const abmp_BitmapOrigine : TBitmap );

// Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aGra_IconAChanger   : Image à changer
// aGra_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_AssignDBImage ( const aFie_FieldImage     : TField   ;
                        		const aGra_IconAChanger   : TGraphic ;
                        		const aGra_DefaultPicture : TGraphic ) : Boolean ;

function fb_FichierIcoBmpVersBitmap ( const as_Fichier : String; const aBmp_Sortie : TBitmap ) : Boolean ;
// Transformation d'un champ bitmap en TIcon
// abmp_Bitmap        : Image bitmap
// aico_Destination   : Image retour icon

procedure p_BitmapVersIco ( const aBmp_Bitmap : TBitmap ; const aIco_Destination : TIcon );
procedure p_FillBitmap ( const ABitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} ; const AColor : TColor );
procedure p_SetAndFillBitmap ( const ABitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF}; const AWidth, AHeight : Integer; const AColor : TColor );
procedure p_SetFontColor ( const acom_component : TObject ; const acol_couleur : TColor );

implementation
uses
{$IFDEF FPC}
     LCLType,
{$ELSE}
     JclGraphics, fonctions_system,
{$ENDIF}
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  {$IFDEF BGRA}
  FPWriteJpeg,
   FPImage,
  {$ENDIF}
  {$IFDEF FPC}
  LazFileUtils,
  {$ENDIF}
  strutils, Math,
  fonctions_proprietes,
  SysUtils;

// fill acanvas with acolor
procedure p_SetAndFillBitmap ( const ABitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} ; const AWidth, AHeight : Integer; const AColor : TColor );
Begin
  ABitmap.Width :=AWidth;
  ABitmap.Height:=AHeight;
  p_FillBitmap ( ABitmap, AColor );
End;

// fill acanvas with acolor
procedure p_FillBitmap ( const ABitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} ; const AColor : TColor );
Begin
  with ABitmap.Canvas do
   Begin
     Brush.Color:=AColor;
     FillRect( Rect ( 0, 0, ABitmap.Width, ABitmap.Height ));
   end;
End;


// Transformation d'un champ bitmap en TIcon
// abmp_Bitmap        : Image bitmap
// aico_Destination   : Image retour icon

procedure p_BitmapVersIco ( const aBmp_Bitmap : TBitmap ; const aIco_Destination : TIcon );
var
  lii_IconInfo : TIconInfo;
{$IFDEF FPC}
  lpi_IconInfo : PIconInfo;
{$ENDIF}
begin
// Le bitmap doit être transparent
  aBmp_Bitmap.Transparent := True ;
  aIco_Destination.Modified := False ;
  with aIco_Destination do
    if Handle <> 0 Then
      Begin
        ReleaseHandle ;
        Handle := 0 ;
      End ;
  if aBmp_Bitmap.Empty
   Then Exit; // quiting when bitmap empty
 {Crée un icon}
  lii_IconInfo.fIcon := true; // C'est un icône
  lii_IconInfo.xHotspot := 0; // Valeurs par défaut : connaîs pas
  lii_IconInfo.yHotspot := 0;
  lii_IconInfo.hbmMask := aBmp_Bitmap.MaskHandle; // Masque de transparence
  lii_IconInfo.hbmColor := aBmp_Bitmap.Handle; // Bitmap
{$IFDEF FPC}
  lpi_IconInfo := @lii_IconInfo ;
  aIco_Destination.Handle := CreateIconIndirect(lpi_IconInfo); /// Création de l'icône
{$ELSE}
  aIco_Destination.Handle := CreateIconIndirect(lii_IconInfo); /// Création de l'icône
{$ENDIF}
  aIco_Destination.Palette := aBmp_Bitmap.Palette ; // récupère la Palette
// l'icône est transparent
  aIco_Destination.Transparent := True ;
  // Midifications faites
  aIco_Destination.PaletteModified := True ;
  aIco_Destination.Modified := True ;

end;

// Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aGra_IconAChanger   : Image à changer
// aGra_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_AssignDBImage ( const aFie_FieldImage     : TField   ;
                        		const  aGra_IconAChanger    : TGraphic ;
                        		const aGra_DefaultPicture : TGraphic ) : Boolean ;
var lBmp_Bitmap : TBitmap ;
begin
  Result := False ; // Pas d'affectation
  try
//    if not aGra_IconAChanger.Empty Then
      Begin
       if ( aGra_IconAChanger is TIcon )
       and (( aGra_IconAChanger as TIcon ).Handle <> 0 ) Then
         Begin
           ( aGra_IconAChanger as TIcon ).ReleaseHandle ;
           ( aGra_IconAChanger as TIcon ).Handle := 0 ;
         End
       else
         if  ( aGra_IconAChanger is TBitmap )
         and ( not ( aGra_IconAChanger as TBitmap ).Handle <> 0 ) Then
           Begin
             p_ClearBitmapWithoutMemoryLeak( aGra_IconAChanger as TBitmap );
             try
               ( aGra_IconAChanger as TBitmap ).Canvas.Refresh ;
             Except
             End;
           End ;
      End ;
    if  Assigned ( aFie_FieldImage )
    and not aFie_FieldImage.IsNull
    and (   aFie_FieldImage.IsBlob )
     then
      Begin
        aGra_IconAChanger.Transparent := True ;
        if aGra_IconAChanger is TIcon
         Then
          begin
            Result := True ;
            lBmp_Bitmap := TBitmap.Create ;
            lBmp_Bitmap.Handle := 0 ;
          // affectation
            try
              lBmp_Bitmap.Transparent := True ;
              lBmp_Bitmap.Assign ( aFie_FieldImage );
              lBmp_Bitmap.Transparent := True ;
{$IFDEF DELPHI}
              p_BitmapVersIco ( lBmp_Bitmap, aGra_IconAChanger as TIcon );
{$ENDIF}
            except
            End;
            p_DestroyBitmapWithoutMemoryLeak(lBmp_Bitmap);
          end
         else
          begin
          // affectation
            ( aGra_IconAChanger as TBitmap ).Assign ( aFie_FieldImage );
            Result := True ;
          end ;
        aGra_IconAChanger.Transparent := True ;
        aGra_IconAChanger.Modified := True ;
      End
    else
      if assigned ( aGra_DefaultPicture ) // Image par défaut
      and aGra_IconAChanger.Empty
      and ( aGra_IconAChanger.ClassType = aGra_DefaultPicture.ClassType ) // Image par défaut
       Then
        begin
          aGra_IconAChanger.Assign ( aGra_DefaultPicture );
          aGra_IconAChanger.Transparent := True ;
          aGra_IconAChanger.Modified := True ;
          Result := True ;
        end;
  finally
  end;
end ;

  Function Ico2Bmp (Ico : TIcon) : TBitmap;
  Begin
    Result := TBitmap.Create;
    Result.Handle := 0 ;
    Result.Width := Ico.Width;
    Result.Height := Ico.Height;
    Result.Canvas.CopyMode := cmSrcCopy;
    Result.Canvas.Draw(0, 0, Ico);
  end;
  // Transformation d'un champ image en TPersistent ( Faire un assign sur TIcon ensuite )
// aFie_FieldImage     : Champ image
// aIco_IconAChanger   : Image à changer
// aIco_DefaultPicture : Image si rien
// Sortie : A-t-on affecté ?

function fb_FichierIcoBmpVersBitmap ( const as_Fichier : String; const aBmp_Sortie : TBitmap ) : Boolean;
var lIco_Icon : TIcon ;
begin
  Result := False ;
  try
    if lowercase ( ExtractFileExt ( as_Fichier )) = '.ico'
     Then
      Begin                     
        Result := True ;
        lIco_Icon := TIcon.Create ;
        try
          lIco_Icon  .LoadFromFile   ( as_Fichier );
          aBmp_Sortie.Width := lIco_Icon.Width;
          aBmp_Sortie.Height := lIco_Icon.Height;
          aBmp_Sortie.Canvas.CopyMode := cmSrcCopy;
          aBmp_Sortie.Canvas.Draw(0, 0, lIco_Icon);
  //        aBmp_Sortie.Handle := IconToBitmap ( lIco_Icon.Handle ) ;
          aBmp_Sortie.Modified := True ;
          lIco_Icon.ReleaseHandle ;
        finally
          lIco_Icon.Destroy ;
        end;
      End
     Else
      Begin
        aBmp_Sortie.LoadFromFile ( as_Fichier );
      End ;
    aBmp_Sortie.Transparent := True ;
  except
      MessageDlg(GS_IMAGE_MAUVAISE_IMAGE, mtWarning, [mbOk], 0);
  End ;
end ;

// Transforme un bitmap en tout petit bitmap
// Entrée : Le Bitmap source
// Sortie : Le petit bitmap
procedure p_ChangeTailleBitmap ( const abmp_BitmapOrigine : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF}; const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ; const ab_KeepProportion : Boolean = False );

var
  lrec_Rectangle      : TRect ;  // Nouvelle taille
  lbmp_Tempo          : TBitmap ;
  li_Size             : Longint ;
  lb_Continue         : Boolean;
Begin
  if  ( ali_newWidth  <= 0 )
  and ( ali_newHeight <= 0 )
   Then exit;
  lbmp_Tempo := TBitmap.Create ; // Création petit bitmap
  lbmp_Tempo.Handle := 0 ;
  lrec_Rectangle.Left := 0 ;
  lrec_Rectangle.Top  := 0 ;
  lb_Continue := False;
  with abmp_BitmapOrigine do
    if   ( Width  > 0 )
    and  ( Height > 0 ) Then
     if not ab_KeepProportion
      Then
       Begin
        lb_Continue := True;
        lbmp_Tempo.Width   := ali_newWidth ;
        lbmp_Tempo.Height  := ali_newWidth ;
        lrec_Rectangle.Right  := ali_newWidth ;
        lrec_Rectangle.Bottom := ali_newWidth ;
        lbmp_Tempo.Canvas.StretchDraw ( lrec_Rectangle, abmp_BitmapOrigine );
        abmp_BitmapOrigine.Width   := ali_newWidth ;
        abmp_BitmapOrigine.Height  := ali_newWidth ;
       End
      else
       Begin
         if  ( ali_newWidth > 0 )
         and ( ali_newWidth <  Width )
         // doit-on retailler en longueur ?
         and (( ali_newHeight <= 0 ) or ( Width / ali_newWidth >= Height / ali_newHeight ))
          Then
           Begin
             lb_Continue := True;
             li_Size := ( ali_newWidth * Height ) div Width;
             lbmp_Tempo.Width   := ali_newWidth ;
             lbmp_Tempo.Height  := li_Size ;
             lrec_Rectangle.Right  := ali_newWidth ;
             lrec_Rectangle.Bottom := li_Size ;
             lbmp_Tempo.Canvas.StretchDraw ( lrec_Rectangle, abmp_BitmapOrigine );
             abmp_BitmapOrigine.Width   := ali_newWidth ;
             abmp_BitmapOrigine.Height  := li_Size ;
           End
         else
           if  ( ali_newHeight > 0 )
           and ( ali_newHeight <  Height ) Then
             Begin
               lb_Continue := True;
               li_Size := ( ali_newHeight * Width ) div Height ;
               lbmp_Tempo.Width   := li_Size ;
               lbmp_Tempo.Height  := ali_newHeight ;
               lrec_Rectangle.Right  := li_Size ;
               lrec_Rectangle.Bottom := ali_newHeight ;
               lbmp_Tempo.Canvas.StretchDraw ( lrec_Rectangle, abmp_BitmapOrigine );
               abmp_BitmapOrigine.Width   := li_Size ;
               abmp_BitmapOrigine.Height  :=ali_newHeight ;
             End ;
      End ;
//   writeln(IntToStr(li_Size) + ' '+ IntToStr(lbmp_Tempo.Height) + ' '  + IntToStr(lbmp_Tempo.Width) + ' ' + IntToStr(ali_newHeight) + ' ' + IntToStr(ali_newWidth) + ' ' + IntToStr(abmp_BitmapOrigine.Width) + ' ' + IntToStr(abmp_BitmapOrigine.Height));
  if lb_Continue Then
    Begin
      {$IFDEF FPC}
      lbmp_Tempo.TransparentMode := tmAuto ;
      {$ENDIF}
      lbmp_Tempo.TransparentColor := clBlack ;
      lbmp_Tempo.Transparent := True ;
      {$IFDEF FPC}
      abmp_BitmapOrigine.Clear;
      {$ENDIF}

      // 2004-10-20 : MAJ destruction bitmap
      p_ClearBitmapWithoutMemoryLeak ( abmp_BitmapOrigine );
      abmp_BitmapOrigine.Assign ( lbmp_Tempo );
    end;
  p_DestroyBitmapWithoutMemoryLeak(lbmp_Tempo);
end ;

procedure p_RecuperePetitBitmap ( const abmp_BitmapOrigine : TBitmap );

Begin
  p_ChangeTailleBitmap ( abmp_BitmapOrigine, 16 );
end ;
function fb_SaveBitmaptoFile(const abmp_Bitmap : TBitmap; const afile: String): Boolean;
var lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  Result:=False;
  lid_imagedata:=fci_BitmapToCustomImage(abmp_Bitmap);
  try
    {$IFDEF BGRA}
    lid_imagedata.SaveToFile( afile );
    {$ELSE}
    SaveImageToFile( afile, lid_imagedata);
    {$ENDIF}
    Result:=True;
  finally
    p_FreeCustomImage(lid_imagedata);
  end;
end; // Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const ab_AjouteBitmap      ,
                                        ab_ImageDefaut       : Boolean     ;
                                  const aIma_ImagesMenus     : TCustomImageList  ;
                                  const ai_CompteurImageDef : Integer     ) : Integer ;
Begin
  Result := -1 ;
  // Peut ajouter un bitmap
  if ab_AjouteBitmap
  and not aBmp_Picture.Empty
   Then
    Begin
      // Récupère le bitmap en petit
      p_RecuperePetitBitmap ( aBmp_Picture );
      // La couleur de transparence doit être celle du bitmap
      aIma_ImagesMenus.BkColor := aBmp_Picture.TransparentColor ;
      // Ajoute dans l'image liste
      aIma_ImagesMenus.AddMasked ( aBmp_Picture, aBmp_Picture.TransparentColor );
      // Libère l'image temporaire
      p_ClearBitmapWithoutMemoryLeak(aBmp_Picture);
      // Numero de l'image ajoutée
      Result := aIma_ImagesMenus.Count - 1 ;
    End
    // Sinon image par défaut de l'image liste
   Else
    if ab_ImageDefaut
     Then
      Result := ai_CompteurImageDef ;
End ;

//from image align to TPoint
function fPoi_FromAlignToCoord ( const AWidthIn, AHeightIn, ASurfaceWidth, ASurfaceHeight : Integer ; AAlign : TAlign ) : TPoint ;
Begin
  if (   ( ASurfaceWidth  > AWidthIn  )
      or ( ASurfaceHeight > AHeightIn ))
  and ( AAlign <> alNone ) Then
   Begin
    if ASurfaceWidth < AWidthIn Then
      case AAlign of
       alTop : AAlign:=alNone;
       alBottom : AAlign:=alNone;
       alClient : AAlign:=alLeft;
      End;
    if ASurfaceHeight < AHeightIn Then
      case AAlign of
       alLeft : AAlign:=alNone;
       alRight : AAlign:=alNone;
       alClient : AAlign:=alTop;
      End;
      case AAlign of
       alNone:
        begin
         Result.X := 0;
         Result.Y := 0;
        end;
       alTop :
       begin
        Result.X := (ASurfaceWidth-AWidthIn) div 2;
        Result.Y := 0;
       end;
       alBottom :
       begin
        Result.X := (ASurfaceWidth-AWidthIn) div 2;
        Result.Y := ASurfaceHeight - AHeightIn;
       end;
       alLeft :
       begin
        Result.X := 0;
        Result.Y := ( ASurfaceHeight - AHeightIn ) div 2;
       end;
       alRight :
       begin
        Result.X := ASurfaceWidth-AWidthIn;
        Result.Y := ( ASurfaceHeight - AHeightIn ) div 2;
       end;
       alClient :
       begin
        Result.X := (ASurfaceWidth-AWidthIn) div 2;
        Result.Y := ( ASurfaceHeight - AHeightIn ) div 2;
       end;
      End
   end
  Else
   Begin
     Result.X := 0;
     Result.Y := 0;
   end;

end;

// Peindre une image d'une liste d'images
// ACanvas : À peindre
procedure p_DrawImageFromList ( const ACanvas : TCanvas; const AImages : TCustomImageList ; const AImageIndex : Integer ; const X : Integer = 0 ; const Y : Integer = 0 );
var  ABitmap : TBitmap;
Begin
  ABitmap := TBitmap.Create;
  AImages.GetBitmap ( aimageIndex, ABitmap );
  ACanvas.Draw ( X, Y, Abitmap );
  p_DestroyBitmapWithoutMemoryLeak(ABitmap);
end;

// draw ABitmap from imagelist
procedure p_DrawImageFromListToBitmap ( const ABitmap : TBitmap; const AImages : TCustomImageList ; const AImageIndex : Integer ; const AColor : TColor; const X : Integer = 0 ; const Y : Integer = 0 );
Begin
  with ABitmap do
   Begin
     p_FillBitmap ( ABitmap, AColor );
     p_DrawImageFromList ( Canvas, AImages, AImageIndex, X, Y );
     Modified := True;
   end;
end;

// draw image from imagelist if exists
procedure p_DrawEventualImageFromListToBitmap ( const ABitmap : TBitmap; const AImages : TCustomImageList ; const AImageIndex, AWidth, AHeight : Integer; const AColor : TColor; const AAlign : TAlign = alLeft );
var ACoord : TPoint;
Begin
  if  ( AImageIndex <> -1 )
  and ( AImages <> nil ) Then
     Begin
      ACoord := fPoi_FromAlignToCoord ( AImages.Width, AImages.Height, AWidth, AHeight, AAlign );
      ABitmap.Width :=Max(AWidth ,AImages.Width);
      ABitmap.Height:=Max(AHeight,AImages.Height);
      p_DrawImageFromListToBitmap ( ABitmap, AImages, AImageIndex, AColor, ACoord.X, ACoord.Y ) ;
      p_ChangeTailleBitmap(ABitmap,AWidth,AHeight,True);
     end
   Else
    ABitmap.Assign(nil);
end;

// Clear ABitmap Without Memory Leak
procedure p_ClearBitmapWithoutMemoryLeak ( const Abitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} );
Begin
  with Abitmap do
   Begin
     {$IFDEF DELPHI}
     Dormant ;
     {$ENDIF}
     FreeImage ;
     Handle := 0 ;
   end;
end;
// Destroy ABitmap Without Memory Leak
procedure p_DestroyBitmapWithoutMemoryLeak ( const Abitmap : {$IFDEF FPC}TCustomBitmap{$ELSE}TBitmap{$ENDIF} );
Begin
  try
    p_ClearBitmapWithoutMemoryLeak ( Abitmap );
  finally
    Abitmap.Destroy;
  end;
end;

// Ajoute une image bmp dans une imagelist et efface le handle
//  aBmp_Picture : L'image
// ab_AjouteBitmap : Ajoute l'image
// ab_ImageDefaut  : Ajoute l'image par défaut
// aIma_ImagesMenus : Liste d'iamges
// ai_CompteurImageDef : Compteur d'image par défaut
function fi_AjouteBmpAImages  (   const aBmp_Picture         : TBitmap     ;
                                  const aIma_ImagesMenus     : TCustomImageList  ) : Integer ;

//var lIco_Icon : TIcon ;
Begin
  Result := -1 ;
  // Peut ajouter un bitmap
  if aBmp_Picture.Handle <> 0
   Then
    Begin
      // Récupère le bitmap en petit
      p_RecuperePetitBitmap ( aBmp_Picture );
      // La couleur de transparence doit être celle du bitmap
      aIma_ImagesMenus.BkColor := aBmp_Picture.TransparentColor ;
      // Ajoute dans l'image liste
      aIma_ImagesMenus.AddMasked ( aBmp_Picture , aBmp_Picture.TransparentColor );
      // Libère l'image temporaire
      p_ClearBitmapWithoutMemoryLeak(aBmp_Picture);
      // Spécififique àl'xpbar
      aIma_ImagesMenus.BkColor := clBackground ;
      // Numero de l'image ajoutée
      Result := aIma_ImagesMenus.Count - 1 ;
    End ;
End ;

// Charge un icône ou un bitmap dans un champ bitmap si le bitmap est assez grand par rapport à ai_Taille
// aod_ChargerImage : Chargement du fichier image
// aF_FieldImage    : Champ image à enregistrer
// ai_Taille        : La taille à modifier : -1 pour ne pas modifier
// ab_MontreMessage : Interaction avec l'utilisateur
// adxb_Image       : Bitmap de visualisation
// Sortie           : Enregistré ou non

function fb_ChargeIcoBmp ( const aod_ChargerImage : TOpenDialog ;
                           const adat_DataSet     : TDataSet    ;
                           const aF_FieldImage    : TField      ;
                           const ai_Taille        : Integer     ;
                           const ab_MontreMessage : Boolean     ;
                           const adxb_Image       : TBitmap     ) : Boolean ;
var lRec_Taille32  : TRect ;
    LBmp_Tempo     ,
    LBmp_Tempo2    : TBitmap ;
begin
  Result := False ;
  If aod_ChargerImage.Execute then
    begin
      if assigned ( adxb_Image )
       Then
        adxb_Image.Modified := False ;
      LBmp_Tempo := TBitmap.Create ;
      LBmp_Tempo.Handle := 0 ;
      fb_FichierIcoBmpVersBitmap ( aod_ChargerImage.FileName, LBmp_Tempo );
      // L'image n'est pas à la bonne taille
      if  ( ai_Taille         >  0  )
      and (    ( LBmp_Tempo.Width  <> ai_Taille )
           or  ( LBmp_Tempo.Height <> ai_Taille ))
       Then
        Begin
      // L'image est petite alors erreur
          if ( LBmp_Tempo.Width  < ai_Taille )
          or ( LBmp_Tempo.Height < ai_Taille )
           Then
            Begin
              if ab_MontreMessage
               Then
                MessageDlg ( GS_IMAGE_MAUVAISE_TAILLE, mtWarning, [mbOk], 0);
              Exit ;
            End
           Else
            if  (    ( LBmp_Tempo.Width  = LBmp_Tempo.Height )
      // L'image va être déformée alors avertissement
                 or  ( ab_MontreMessage and ( MessageDlg ( GS_IMAGE_DEFORMATION, mtWarning, [mbOk,mbCancel], 0) = mrOK )))
             Then
              Begin
              // Création du bitmap de conversion
		LBmp_Tempo2 := TBitmap.Create ;
		LBmp_Tempo2.Handle := 0 ;
                LBmp_Tempo2.Assign ( LBmp_Tempo );
                lRec_Taille32.Left   := 0 ;
                lRec_Taille32.Top    := 0 ;
                lRec_Taille32.Right  := ai_Taille ;
                lRec_Taille32.Bottom := ai_Taille ;
                LBmp_Tempo.Transparent := False ;

              // Conversion
                LBmp_Tempo2.Canvas.StretchDraw ( lRec_Taille32, LBmp_Tempo );
                LBmp_Tempo2.Height := ai_Taille ;
                LBmp_Tempo2.Width  := ai_Taille ;
                LBmp_Tempo2.Modified := True ;

                p_ClearBitmapWithoutMemoryLeak(LBmp_Tempo);
		LBmp_Tempo.Assign ( LBmp_Tempo2 );
		LBmp_Tempo.Transparent := True ;
		Result := True ;
                p_DestroyBitmapWithoutMemoryLeak(LBmp_Tempo2);
              End ;


        End
       Else
        Begin
         Result := True ;
        End ;
      if Result
       Then
         Begin
          if assigned ( adxb_Image )
           Then
            Begin
            // 2004-10-20 : MAJ destruction bitmap
              p_ClearBitmapWithoutMemoryLeak(adxb_Image);
              adxb_Image.Assign ( aF_FieldImage );
              adxb_Image.Modified := True ;
            End ;
          adat_DataSet.Edit ;
          aF_FieldImage.Assign ( LBmp_Tempo );
          adat_DataSet.Post ;
         end;
      p_DestroyBitmapWithoutMemoryLeak(LBmp_Tempo);
    end;
End ;


// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_FieldToImage ( const field : TField ; const Image : TBitmap ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False );
var l_c_memory_stream: TMemoryStream;
begin
  if not ( field.IsNull ) then
    try
      l_c_memory_stream:= TMemoryStream.Create;
      try
        ( field as tBlobField ).SaveToStream ( l_c_memory_stream );
      Except
        On E:Exception do
         if ab_ShowError Then
            ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FIELD_IMAGE+CST_ENDOFLINE+E.ClassName);
      end;
      l_c_memory_stream.Position:=0;
      p_StreamToImage ( l_c_memory_stream, Image, ali_newWidth, ali_newHeight, ab_KeepProportion, ab_ShowError );
    Finally
      l_c_memory_stream.Destroy;
    End;

end;

// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_ImageFileToField ( const afile: String; const field : TField ; const ab_ShowError : Boolean = False );
var l_c_memory_stream: TMemoryStream;
begin
  if FileExistsUTF8 ( afile ) then
    try
      l_c_memory_stream:= TMemoryStream.Create;
      p_FileToStream(afile,l_c_memory_stream, ab_ShowError);
      l_c_memory_stream.Position:=0;
      p_StreamToField ( l_c_memory_stream, field, ab_ShowError );
    Finally
      l_c_memory_stream.Destroy;
    End;

end;

// Procédure de transfert d'un champ vers un fichier
// field : Le champ image
// afile : La destination
function fb_ImageFieldToFile ( const field : TField ; const afile: String; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False ) : Boolean;
var l_c_memory_stream: tMemoryStream;
begin

  if not FileExistsUTF8 ( afile )
  and ( field is TBlobField )
  and (( field as TBlobField ).BlobSize > 0 ) then
    try
      l_c_memory_stream := TMemoryStream.Create();
      p_ImageFieldToStream ( field, l_c_memory_stream );
      Result := fb_StreamToFile ( l_c_memory_stream, afile, ali_newWidth, ali_newHeight, ab_KeepProportion, ab_ShowError );
    Finally
      l_c_memory_stream.Destroy;
    End
  Else
    Result := False;

end;


// Procédure de transfert d'un champ vers un fichier
// field : Le champ image
// afile : La destination
function fb_FileToImageField ( const afile: String; const field : TField ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True; const ab_ShowError : Boolean = False ) : Boolean;
var l_c_memory_stream: tMemoryStream;
begin
  if FileExistsUTF8 ( afile )
  and ( field is TBlobField )
  and (( field as TBlobField ).BlobSize > 0 ) then
    try
      l_c_memory_stream := TMemoryStream.Create();
      p_FileToStream ( afile, l_c_memory_stream, ab_ShowError );
      Result := fb_StreamToFile ( l_c_memory_stream, afile, ali_newWidth, ali_newHeight, ab_KeepProportion, ab_ShowError );
    Finally
      l_c_memory_stream.Destroy;
    End
  Else
    Result := False;
end;

// Procédure de transfert d'un champ vers un fichier
// field : Le champ image
// afile : La destination
procedure p_ImageFieldToStream ( const field : TField ; const ast_memory_stream: tMemoryStream ; const ab_ShowError : Boolean = False );
begin
  if not field.IsNull Then
  try
    ( field as tBlobField ).SaveToStream ( ast_memory_stream );
  Except
    On E:Exception do
     if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_STREAM_IMAGE+CST_ENDOFLINE+E.ClassName);
  end;
end;

// Procédure de transfert d'un champ vers une image
// field : Le champ image
// Image : La destination
procedure p_StreamToField ( const astream: TStream; const field : TField ; const ab_ShowError : Boolean = False );
begin
  if ( field is TBlobField ) then
    try
      ( field as TBlobField ).LoadFromStream ( astream );
    Except
      On E:Exception do
       Begin
         if ab_ShowError Then
            ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_STREAM_FIELD+CST_ENDOFLINE+E.ClassName+E.Message);
         raise EFileImageError.Create(GS_CHARGEMENT_IMPOSSIBLE_STREAM_FIELD);
       end;

    end;

end;

procedure p_StreamToImage ( const stream: tStream; const Image : TBitmap ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True ; const ab_ShowError : Boolean = False );
var lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  {$IFDEF BGRA}
  lid_imagedata:=nil;
  if DetectFileFormat ( Stream )>ifUnknown
  {$ELSE}
  Finalize ( lid_imagedata );
  if DetermineStreamFormat ( Stream )>''
  {$ENDIF}
   Then
    try
      lid_imagedata := fci_StreamToCustomImage  ( Stream, ali_newWidth, ali_newHeight, ab_KeepProportion );
      if  ( lid_imagedata.Width  > 0 )
      and ( lid_imagedata.Height > 0 ) Then
        Begin
         {$IFDEF BGRA}
         Image.Width:=lid_imagedata.Width;
         Image.Height:=lid_imagedata.Height;
         lid_imagedata.Draw(Image.Canvas, 0, 0, True);
         {$ELSE}
         ConvertDataToBitmap( lid_imagedata, Image );
         {$ENDIF}
        end
       Else
        p_ClearBitmapWithoutMemoryLeak ( Image );
    Finally
      p_FreeCustomImage (lid_imagedata);
    end;
end;
procedure p_FreeCustomImage ( var abb_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF} );
Begin
  {$IFDEF BGRA}FreeAndNil{$ELSE}FreeImage{$ENDIF}(abb_imagedata);
end;

procedure p_ImageToStreamJpeg ( const abb_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const Stream : TStream);
{$IFDEF BGRA}
var    AWriter:TFPWriterJpeg;
Begin
  AWriter:=TFPWriterJpeg.Create;
  AWriter.CompressionQuality:=gby_compressionJpeg;
  try
    abb_imagedata.SaveToStream(Stream,AWriter);
  finally
    AWriter.Destroy;
  end;
{$ELSE}
Begin
  SetOption(ImagingJpegQuality,gby_compressionJpeg);
  SaveImageToStream( 'JPG', Stream, abb_imagedata);
{$ENDIF}
end;
function  fci_BitmapToCustomImage ( const ab_Bitmap : TBitmap ):{$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
Begin
  if ab_Bitmap.Handle=0 Then
   Begin
{$IFDEF BGRA}
    Result:=nil;
{$ELSE}
    Finalize(Result);
{$ENDIF}
    Exit;
   end;
  Result:=fci_GetCustomImage;
  {$IFDEF BGRA}
  Result.Assign ( ab_bitmap );
  {$ELSE}
  ConvertBitmapToData( ab_Bitmap, Result );
  {$ENDIF}
end;

function  fci_GetCustomImage :{$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
Begin
  {$IFDEF BGRA}
  Result:=nil;
  {$ELSE}
  Finalize ( Result );
  {$ENDIF}
  {$IFDEF BGRA}
  Result:=TBGRABitmap.Create;
  {$ELSE}
  InitImage(Result);
  {$ENDIF}
end;

procedure p_FileToStream ( const afile : String; const Stream : TStream ; const ab_ShowError : Boolean = False );
var lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  lid_imagedata:=fci_GetCustomImage;
  try
    try
      {$IFDEF BGRA}
      lid_imagedata.LoadFromFile  ( afile );
      {$ELSE}
      LoadImageFromFile  ( afile, lid_imagedata );
      {$ENDIF}
      p_ImageToStreamJpeg ( lid_imagedata, Stream );
    Except
      On E:Exception do
       Begin
        if ab_ShowError Then
          ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE+CST_ENDOFLINE+E.ClassName);
        raise EFileImageError.Create(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE);
       end;
    end;
  Finally
    p_FreeCustomImage (lid_imagedata);
  end;
end;
procedure p_ImageToStream ( const lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const Stream : TStream ; const Extension : String='JPG'; const FileName : String=''; const ACompress : Boolean=False; const ACompression :
 {$IFDEF BGRA}Tcompressionlevel = clmax {$ELSE}TDDSCompresion=dcDXT5{$ENDIF} ; const ab_ShowError : Boolean = False);
{$IFDEF BGRA}
var   AWriter:TFPCustomImageWriter;
      AImageType : TBGRAImageFormat;
{$ENDIF}
begin
  try
    {$IFDEF BGRA}
    AImageType:= SuggestImageFormat(FileName);
    if AImageType > ifUnknown Then
     Begin
      Awriter:=CreateBGRAImageWriter(AImageType,True);
      if ACompress Then
       p_SetComponentProperty(AWriter,'CompressionLevel',ACompression);
      try
        lid_imagedata.SaveToStream(Stream,AWriter);
      finally
        AWriter.Destroy;
      end;
     end;
    {$ELSE}
    SaveImageToStream( Extension, Stream, lid_imagedata);
    {$ENDIF}
  Except
    On E:Exception do
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE+CST_ENDOFLINE+E.ClassName);
  end;
end;
function fb_StreamToFile ( const Stream : TStream ; const afile : String; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True ; const ab_ShowError : Boolean = False ) : Boolean;
var lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  Result := False;
  try
    if ( Stream.Size = 0 ) then
      Exit;
    Stream.Position := 0;
    lid_imagedata := fci_StreamToCustomImage  ( Stream, ali_newWidth, ali_newHeight, ab_KeepProportion );
    try
      {$IFDEF BGRA}
      lid_imagedata.SaveToFile( afile );
      {$ELSE}
      SaveImageToFile( afile, lid_imagedata);
      {$ENDIF}
    Finally
      p_FreeCustomImage (lid_imagedata);
    end;
    Result := True;
  Except
    On E:Exception do
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE+CST_ENDOFLINE+E.ClassName);
  end;
end;

function fci_StreamToCustomImage ( const Stream : TStream ; const ali_newWidth : Longint = 0; const ali_newHeight : Longint = 0; const ab_KeepProportion : Boolean = True ) : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  Result:=fci_GetCustomImage;
  Stream.Position := 0;
  if ( Stream.Size = 0 ) then
    Exit;
  {$IFDEF BGRA}
  Result.LoadFromStream( Stream );
  {$ELSE}
  LoadImageFromStream  ( Stream, Result );
  {$ENDIF}
  if ( Result.Height = 0 )
  or (( ali_newHeight = 0 ) and ( ali_newWidth = 0 ))
  or ( Result.Width  = 0 ) then
    Exit;
  fb_ResizeCustomImage(Result, ali_newWidth, ali_newHeight, ab_KeepProportion );
end;

/////////////////////////////////////////////////////////////////////////
// procedure p_FileToBitmap
// setting some image file to Bitmap object
// afile : The file image
// abmp_Image : Bitmap object to set
// ab_ShowError : Error showing
/////////////////////////////////////////////////////////////////////////
procedure p_FileToBitmap ( const afile : String; const abmp_Image : TBitmap ; const ab_ShowError : Boolean = False );
var lid_imagedata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
begin
  lid_imagedata:=fci_FileToCustomImage ( afile, ab_ShowError );
  {$IFDEF BGRA}
  if assigned(lid_imagedata) Then
  {$ELSE}
  if lid_imagedata.Size > 0 Then
  {$ENDIF}
   try
    {$IFDEF BGRA}
    abmp_Image.Assign(lid_imagedata.Bitmap);
    {$ELSE}
    ConvertDataToBitmap( lid_imagedata, abmp_Image );
    {$ENDIF}
   Finally
    p_FreeCustomImage (lid_imagedata);
   end;
end;

/////////////////////////////////////////////////////////////////////////
// procedure p_FileToBitmap
// setting some image file to Bitmap object
// afile : The file image
// abmp_Image : Bitmap object to set
// ab_ShowError : Error showing
/////////////////////////////////////////////////////////////////////////
function fci_FileToCustomImage ( const afile : String; const ab_ShowError : Boolean = False ):{$IFDEF BGRA}TBGRABitmap{$ELSE}TImageData{$ENDIF};
begin
  {$IFDEF BGRA}
  Result:=nil;
  {$ELSE}
  Finalize ( Result );
  InitImage(Result);
  {$ENDIF}

  try
    {$IFDEF BGRA}
    Result:=TBGRABitmap.Create(afile, False,[loJpegQuick]);
    {$ELSE}
    LoadImageFromFile  ( afile, Result );;
    {$ENDIF}
  Except
    On E:Exception do
     Begin
      if ab_ShowError Then
        ShowMessage(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE+CST_ENDOFLINE+afile);
      raise EFileImageError.Create(GS_CHARGEMENT_IMPOSSIBLE_FILE_IMAGE+CST_ENDOFLINE+afile);
     end;
  end;
end;

function fb_ResizeImage ( var Fdata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ):Boolean;
var Fdata2 : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF};
Begin
  Fdata2:=Fdata;  // to destroy
  {$IFDEF BGRA}
  Fdata:=Fdata.Resample( ali_newWidth, ali_newHeight );
  {$ELSE}
  Result := ResizeImage ( Fdata, ali_newWidth, ali_newHeight, rfBicubic );
  {$ENDIF}
  p_FreeCustomImage (Fdata2);
  Result := True;

end;

function fb_ResizeCustomImage ( var Fdata : {$IFDEF BGRA}TBGRACustomBitmap{$ELSE}TImageData{$ENDIF}; const ali_newWidth : Longint ; const ali_newHeight : Longint = 0 ; const ab_KeepProportion : Boolean = True ):Boolean;
var
    li_ImageWidth,
    li_ImageHeight,
    li_Size : Longint ;
begin
  li_ImageWidth  := Fdata.Width;
  li_ImageHeight := Fdata.Height;
  // let the system doing some thinks
  Result := False;
  //Resizing
  if  (( ali_newWidth  < li_ImageWidth ) or ( ali_newHeight < li_ImageHeight ))
  and ( li_ImageHeight > 0 )
  and ( li_ImageWidth > 0 )
  and ( not ab_KeepProportion )
    Then  Result := fb_ResizeImage ( Fdata, ali_newWidth, ali_newHeight )
    else
     Begin
       if  ( ali_newWidth > 0 )
       and ( ali_newWidth <  li_ImageWidth )
       // doit-on retailler en longueur ?
       and (( ali_newHeight = 0 ) or ( li_ImageWidth / ali_newWidth > li_ImageHeight / ali_newHeight ))
        Then
         Begin
           li_Size := ( ali_newWidth * li_ImageHeight ) div li_ImageWidth;
           Result := fb_ResizeImage ( Fdata, ali_newWidth, li_Size );
         End
       else
         if  ( ali_newHeight > 0 )
         and ( ali_newHeight <  li_ImageHeight ) Then
           Begin
             li_Size := ( ali_newHeight * li_ImageWidth ) div li_ImageHeight ;
             Result := fb_ResizeImage ( Fdata, li_Size, ali_newHeight );
           End ;
    End ;
end;

procedure p_FileToImage ( const afile : String; const Image : TPicture ; const ab_ShowError : Boolean = False );
begin
  p_FileToBitmap ( afile, Image.Bitmap, ab_ShowError );
  Image.Bitmap.Canvas.Refresh;
end;

function fs_SeparateTextFromWidth ( ASText : String; const ANeededWidth : Integer; const Acanvas : TCanvas; const Ach_separator : String = ' ' ) : TStringAArray ;
var Apos, J, i : Integer;
    lb_notfound : Boolean ;
    lchar : Char;
Begin
  Apos := 1;
  j    := 0;
  Finalize(Result);
  for i := 1 to length ( Ach_separator ) do
   Begin
     lchar := Ach_separator[i];
     if pos( lchar, ASText ) > 0 then
       Begin
         while  (posex(lchar, ASText, Apos + 1 ) > 0)
            and ( ANeededWidth > ACanvas.TextWidth(copy(ASText,1,posex(lchar, ASText, Apos + 1 )))) do
          Begin
           Apos:=posex(lchar, ASText, Apos +1 );
          end;
         if Apos = 0 Then
           break;
         if Apos > 1 Then
          Begin
           SetLength(Result,high ( Result ) + 2);
           Result [ high ( Result )] := copy(ASText,1,Apos-1);
           inc ( j );
          end;
         ASText:=copy(ASText,Apos+1,Length(ASText)-Apos);
         Apos := 1;
         if ANeededWidth < ACanvas.TextWidth(ASText) Then
           Break;
       end;
   end;
  SetLength(Result,high ( Result ) + 2);
  Result [ high ( Result )] := ASText;
End;

// assigne une couleur à un tcomponent ayant la propriété font
// acom_component   : le composant
// acol_couleur     : la couleur à affecter
procedure p_SetFontColor ( const acom_component : TObject ; const acol_couleur : TColor );
var
  lfon_font : TObject ;
begin
  lfon_font := fobj_getComponentObjectProperty ( acom_component, 'Font' );
  if assigned ( lfon_font )
  and ( lfon_font is TFont )
   Then
    ( lfon_font as TFont ).Color := acol_couleur ;
End ;

initialization
  {$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_fonctions_images );
  {$ENDIF}
end.
