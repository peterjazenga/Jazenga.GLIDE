unit U_FormAdapt;
// Unité de la Version 2 du projet FormMain
// La version 1 TFormMain n'est pas sa fenêtre parente

// Le module crée des propriété servant à la gestion du fichier INI
// Il gère la déconnexion
// Il gère la gestion des touches majuscules et numlock
// Il gère les forms enfants
// créé par Matthieu Giroux en décembre 2007

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

interface

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

uses
{$IFDEF SFORM}
  CompSuperForm,
{$ENDIF}
{$IFDEF FPC}
  LCLIntf, LCLType,
{$ELSE}
  Windows, OleDb,
{$ENDIF}
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
{$IFDEF TNT}
  TNTForms,
{$ENDIF}
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  fonctions_ini, IniFiles,
  fonctions_scaledpi;

{$IFDEF VERSIONS}
  const
    gVer_TFormAdapt : T_Version = (  Component : 'Fonts Adapting Form' ;
                                       FileUnit : 'U_FormAdapt' ;
                                       Owner : 'Matthieu Giroux' ;
                                       Comment : 'Fiche principale deuxième version.' ;
                                       BugsStory : '1.1.0.0 : oncreate event, ini procedures.' + #13#10 +
                                                   '1.0.0.0 : Adapting Fonts.';
                                       UnitType : 3 ;
                                       Major : 1 ; Minor : 1 ; Release : 0 ; Build : 0 );

{$ENDIF}
type
  { TF_FormAdapt }

  TF_FormAdapt = class({$IFDEF SFORM}TSuperForm{$ELSE}{$IFDEF TNT}TTntForm{$ELSE}TForm{$ENDIF}{$ENDIF})
  private
    FScale:Extended;
    FOldCreate : TNotifyEvent;
    procedure FormAdaptCreate(AForm: TObject);
  protected
    procedure DoShow; override;
  public
    { Déclarations publiques }
    // Constructeur et destructeur
    constructor Create(AOwner: TComponent); override;
    property Scale : Extended read FScale;
  end;

var ge_GlobalScaleForm : Extended = 1;

const INI_SCALE = 'Scale' ;

procedure p_writeScaleToIni ( const AIniFile : TIniFile );
procedure p_ReadScaleFromIni  ( const AIniFile : TIniFile );

implementation

uses fonctions_erreurs, TypInfo,
   fonctions_system;

procedure p_writeScaleToIni ( const AIniFile : TIniFile );
Begin
  AIniFile.WriteFloat(INISEC_PAR,INI_SCALE,ge_GlobalScaleForm);
end;

procedure p_ReadScaleFromIni  ( const AIniFile : TIniFile );
Begin
  ge_GlobalScaleForm := AIniFile.ReadFloat(INISEC_PAR,INI_SCALE,ge_GlobalScaleForm);
end;

{ TF_FormAdapt }

constructor TF_FormAdapt.Create(AOwner: TComponent);
begin
  inherited;
  if not ( csDesigning in ComponentState ) Then
   Begin
    FOldCreate := OnCreate;
    OnCreate:=FormAdaptCreate;
   end;
end;
procedure TF_FormAdapt.FormAdaptCreate(AForm: TObject);
begin
  FScale:=1;
  if not ( Owner is TCustomForm ) Then
   Begin
     p_ReadScaleFromIni  ( f_GetMemIniFile );
     if ge_GlobalScaleForm <> 1
     Then ScaleFormCreate(Self,ge_GlobalScaleForm)
     else if fb_CalculateScale ( FScale )
      Then ScaleFormCreate(Self,FScale)
      Else FormInScreen( self );
   End
   Else FormInScreen( self );
  FScale:=1;
  if Assigned(FOldCreate) Then
    FOldCreate ( Self );
end;

procedure TF_FormAdapt.DoShow;
begin
  if not ( csDesigning in ComponentState )
  and ( FScale = 1 ) Then
   Begin
     if ge_GlobalScaleForm <> 1 Then
      Begin
        if ge_GlobalScaleForm <> FScale
          Then ScaleFormShow(Self,ge_GlobalScaleForm);
        FScale:=ge_GlobalScaleForm;
      end
     else if fb_CalculateScale ( FScale ) Then
      ScaleFormShow(Self,FScale);
   End;
  inherited;
end;


initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_TFormAdapt );
{$ENDIF}
end.
