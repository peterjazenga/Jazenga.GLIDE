{-----------------------------------------------------------------------}
{                                                                       }
{           Subprogram Name:                                            }
{           Purpose:          Ancestromania                             }
{           Source Language:  Francais                                  }
{           Auteurs :                      }
{           Philippe Cazaux-Moutou                                       }
{                                                                       }
{-----------------------------------------------------------------------}
{                                                                       }
{           Description:                                                }
{           Ancestromania est un Logiciel Libre                         }
{                                                                       }
{-----------------------------------------------------------------------}
{                                                                       }
{           Revision History                                            }
{           Date 03/01/2009      ,Author Name Andr? Langlet           }
{           Description      }
{           Ajout image et affichage dans memo                          }
{-----------------------------------------------------------------------}

unit u_form_msg;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  StdCtrls,Forms,ExtCtrls,
  Controls,Classes,Dialogs,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  u_buttons_appli,
  u_buttons_defs,
  U_ExtImage,
  U_FormAdapt,
  Menus,
  Graphics;


{$IFDEF VERSIONS}
const
    gVer_F_Msg : T_Version = ( Component : 'Message dialog Window' ;
       			                 FileUnit : 'u_form_working' ;
       			                 Owner : 'Matthieu Giroux' ;
       			                 Comment : 'Ask or tell anything.' ;
      			                 BugsStory : 'Version 0.1.4.0 : Retry button.'+#10
                                                   + 'Version 0.1.3.0 : Adding copy of showed text.'+#10
                                                   + 'Version 0.1.2.0 : Simplify more.'+#10
                                                   + 'Version 0.1.1.0 : Simplify.'+#10
                                                   + 'Version 0.1.0.0 : From other software';
			                 UnitType : CST_TYPE_UNITE_FICHE ;
			                 Major : 0 ; Minor : 1 ; Release : 4 ; Build : 0 );
{$ENDIF}

type

  { TFMsg }

  TFMsg=class(TF_FormAdapt)
    Image: TExtImage;
    lbMsg: TLabel;
    Panel1: TPanel;
    PanelButtons: TPanel;
    p_Msg: TPanel;
    p_Main: TPanel;
    procedure FormShow(Sender: TObject);
    procedure lbMsgClick(Sender: TObject);
  private
    FModalCancel,FModalOK:Integer;
    fButtons:TMsgDlgButtons;
    fDlgType:TMsgDlgType;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
  public
    AOwner : TControl;
    property Buttons:TMsgDlgButtons read fButtons write fButtons;
    property DlgType:TMsgDlgType read fDlgType write fDlgType;
    procedure InitMessage;
  end;

implementation

uses
     {$IFDEF FPC}
     unite_messages,
     {$ELSE}
     unite_messages_delphi,
     {$ENDIF}
     Clipbrd,
     Sysutils;

const CST_IMAGE_Warning = 'mwarning';
      CST_IMAGE_Information = 'minfo';
      CST_IMAGE_Confirmation = 'mconfirm';
      CST_IMAGE_Error = 'merror';


{$IFDEF FPC}{$R *.lfm}{$ELSE}{$R *.DFM}{$ENDIF}

procedure TFMsg.InitMessage;
var
  k,p,TotalWidth:integer;
  procedure PutInBtn(const numBtn:integer;const aText:string;const aResult:word);//MG2013
  var
    btn:TFWButton;
  begin
    btn := TFWButton.Create(Self);
    with btn do
     Begin
       Parent := PanelButtons;
       Align:=alRight;
       case aResult of
         mrYes,mrOK,mrYesToAll,mrAll     : p_Load_Buttons_Appli (Glyph,CST_FWOK,btn);
         mrNo,mrCancel,mrAbort,mrNoToAll : p_Load_Buttons_Appli (Glyph,CST_FWCANCEL,btn);
         mrRetry                         : p_Load_Buttons_Appli (Glyph,CST_FWREFRESH,btn);
         {$IFDEF FPC}
         mrClose                         : p_Load_Buttons_Appli (Glyph,CST_FWCLOSE,btn);
         {$ENDIF}
        End;
       GlyphSize := Height - 2 ;
       Caption:=aText;
       ModalResult:=aResult;
       if numBtn=1//si c'est le premier
        Then
         Begin
           Default:=True;
           FModalOK:=AResult;
         end;
       if numBtn=4-k //c'est le dernier (pas d'inconvénient s'il est aussi le premier)
        Then
          Begin
            Cancel:=True;
            FModalCancel:=AResult;
          end;
       Width := Canvas.TextWidth(aText)+GlyphSize+12;
       inc ( TotalWidth, Width );
       visible:=true;
      end;
  end;

  Procedure PlaceType(const imagefile,libelle:String);
  begin
    Caption:=libelle;
    p_Load_Buttons_Appli ( Image.Picture, imagefile, Image );
  end;

begin
  k:=0;
  TotalWidth := 20;
  if (mbYes in fButtons) then inc(k);
  if (mbYesToAll in fButtons) then inc(k);
  if (mbNo in fButtons) then inc(k);
  if (mbNoToAll in fButtons) then inc(k);
  if (mbOk in fButtons) then inc(k);
  if (mbCancel in fButtons) then inc(k);
  if (mbRetry in fButtons) then inc(k);
  if (mbIgnore in fButtons) then inc(k);

  if (k>0) then
  begin
    p:=0;//numéro du btn de départ (-1)

    if (mbYes in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_Yes,mrYes);
    end;

    if (mbYesToAll in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_YesToAll,mrYesToAll);
    end;

    if (mbNo in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_No,mrNo);
    end;

    if (mbNoToAll in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_NoToAll,mrNoToAll);
    end;

    if (mbOk in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_OK,mrOk);
    end;

    if (mbCancel in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_Cancel,mrCancel);
    end;

    if (mbIgnore in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_Ignore,mrIgnore);
    end;
    if (mbRetry in fButtons) then
    begin
      inc(p);
      putInBtn(p,gs_Retry,mrRetry);
    end;
    inc ( TotalWidth, 4 );
  end;

  with p_Main do
   if ClientWidth < TotalWidth Then
     ClientWidth := TotalWidth;

  case fDlgType of
    mtWarning: PlaceType(CST_IMAGE_Warning,gs_Warning);
    mtInformation: PlaceType(CST_IMAGE_Information,gs_Information);
    mtConfirmation: PlaceType(CST_IMAGE_Confirmation,gs_Confirmation);
  else
    PlaceType(CST_IMAGE_Error,gs_Error);
  end;
  lbMsg.Hint:=gs_Press_ctrl_c_to_copy_text;
end;

procedure TFMsg.KeyDown(var Key:Word;Shift:TShiftState);
begin
  if (ssAlt in Shift)and(Key=VK_F4) then Key:=0;
  if (ssCtrl in Shift)and(LowerCase(Chr(Key))='c') then Clipboard.AsText:=lbMsg.Caption;
  if key = VK_ESCAPE Then
    Begin
      ModalResult:=FModalCancel;
    end;
  if key = VK_RETURN Then
    Begin
      ModalResult:=FModalOK;
    end;
  Inherited;
end;

// procedure TFMsg.FormShow
// resizing form works only on show
procedure TFMsg.FormShow(Sender: TObject);
var  RectMonitor:TRect;

begin

  if (AOwner=nil)and Assigned(Application.MainForm)
   then
     AOwner:=Application.MainForm;

  if AOwner<>nil then
    begin
      Position:=poDesigned;
      Left:=AOwner.Left+(AOwner.Width-Width)div 2;
      Top:=AOwner.Top+(AOwner.Height-Height)div 2;
      RectMonitor:=Monitor.WorkareaRect;

      if (Top+Height)>RectMonitor.Bottom then
        Top:=RectMonitor.Bottom-Height;
      if Top<RectMonitor.Top then
        Top:=RectMonitor.Top;

      if (Left+Width)>RectMonitor.Right then
        Left:=RectMonitor.Right-Width;
      if Left<RectMonitor.Left then
        Left:=RectMonitor.Left;
    end;
  BringToFront;
end;

procedure TFMsg.lbMsgClick(Sender: TObject);
begin
  with lbMsg do
   if Color = clBtnFace
    Then Color:=clNone
    Else Begin
          Clipboard.AsText:=Caption;
          Color := clBtnFace;
         end;
end;


{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_F_Msg );
{$ENDIF}
end.

