{-----------------------------------------------------------------------}
{                                                                       }
{           Subprogram Name:                                            }
{           Purpose:          Ancestromania                             }
{           Source Language:  Francais                                  }
{           Auteurs :                                                   }
{           André Langlet (Main), Matthieu Giroux (LAZARUS),            }
{           Philippe Cazaux-Moutou (Old Ancestro GPL)                   }
{                                                                       }
{-----------------------------------------------------------------------}
{                                                                       }
{           Description:                                                }
{           Ancestromania est un Logiciel Libre                         }
{                                                                       }
{-----------------------------------------------------------------------}
{                                                                       }
{           Revision History                                            }
{           v#    ,Date       ,Author Name            ,Description      }
{                                                                       }
{-----------------------------------------------------------------------}

unit u_form_working;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Forms, Controls, StdCtrls,
  {$IFDEF VERSIONS}
    fonctions_version,
  {$ENDIF}
  {$IFNDEF FPC}
   JvExControls, JvXPCore, JvXPButtons,
  {$ENDIF}
  ExtCtrls,
  U_FormAdapt,
  u_buttons_appli, Graphics,
  u_buttons_defs, Classes;

{$IFDEF VERSIONS}
const
    gVer_F_Working : T_Version = ( Component : 'Wait Window' ;
       			                 FileUnit : 'u_form_working' ;
       			                 Owner : 'Matthieu Giroux' ;
       			                 Comment : 'While working wait.' ;
      			                 BugsStory : 'Version 0.1.1.0 : Simplify'+#10
                                                   + 'Version 0.1.0.0 : From other software' ;
			                 UnitType : CST_TYPE_UNITE_FICHE ;
			                 Major : 0 ; Minor : 1 ; Release : 1 ; Build : 0 );
{$ENDIF}

type

  TFWorking = class(TF_FormAdapt)
    Panel1: TPanel;
    PleaseWait: TLabel;
    Panel2: TPanel;
    BtnCancel: TFWCancel;
    PanCancel: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);

  private
    HFiche,HLabel:integer;
    procedure doDesactive;

  public
    procedure doInit(const sTexte:string;const Annuler:boolean=false);
  end;

var gF_Working : TFWorking;

implementation

{$IFDEF FPC}{$R *.lfm}{$ELSE}{$R *.DFM}{$ENDIF}

uses sysutils,
     fonctions_graphic_dialogs,
     fonctions_dialogs;

{ TFWorking }

procedure TFWorking.doDesactive;
begin
  Hide;
  screen.cursor := crDefault;

end;

procedure TFWorking.doInit(const sTexte: string;const Annuler:boolean=false);
//var
//  i,l:integer;
begin
  BtnCancel.Enabled:=true;
  PanCancel.Visible:=Annuler;
 { i:=0;
  l:=Pos(#10,sTexte);
  while l>0 do
  begin
    inc(i);
    l:=PosEx(#10,sTexte,l+1);
  end;
  if Annuler
   then self.ClientHeight:=HFiche+i*PleaseWait.Height
   else self.ClientHeight:=HFiche+i*PleaseWait.Height-PanCancel.Height;}
  screen.cursor := crHourGlass;
  Caption:=sTexte;
  PleaseWait.Caption:=sTexte;
  Show;
  Update;// needed to show message
end;

procedure TFWorking.FormDestroy(Sender: TObject);
begin
  doDesactive;
  close;
  gF_Working:=nil;
end;

procedure TFWorking.FormCreate(Sender: TObject);
var
  aForm:TForm;
begin
  HFiche:=self.ClientHeight;
  HLabel:=PleaseWait.Height;
  aForm:= Screen.ActiveForm;
  if aForm=nil then
    if Assigned(Application.MainForm) then
      aForm:=Application.MainForm;
  if aForm<>nil then
  begin
    Position:=poDesigned;
    Left:=aForm.Left+(aForm.Width-Width)div 2;
    Top:=aForm.Top+(aForm.Height-Height)div 2;
  end;
end;

procedure TFWorking.BtnCancelClick(Sender: TObject);
begin
  gb_btnCancel:=true;
  BtnCancel.Enabled:=false;

end;

initialization
{$IFDEF VERSIONS}
  p_ConcatVersion ( gVer_F_Working );
{$ENDIF}
 ge_ShowMessage := TMessage({$IFNDEF FPC}@{$ENDIF}MyGraphicShowMessage);
 ge_ShowWorking := {$IFNDEF FPC}@{$ENDIF}MyGraphicShowWorking;
 ge_CloseWorking :={$IFNDEF FPC}@{$ENDIF}MyGraphicCloseWorking;

end.
