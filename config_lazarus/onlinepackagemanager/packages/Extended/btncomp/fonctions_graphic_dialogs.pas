unit fonctions_graphic_dialogs;

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Graphics,
  Dialogs,
  Classes, SysUtils,
  Controls,
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  u_form_working,fonctions_dialogs;

{$IFDEF VERSIONS}
const
    gVer_fonctions_dialogs : T_Version = ( Component : 'Fonctions IHM de dialogue' ;
       			                 FileUnit : 'fonctions_dialogs' ;
       			                 Owner : 'Matthieu Giroux' ;
       			                 Comment : 'Fonctions IHM servant au dialogue avec l''utiisateur.' ;
      			                 BugsStory : 'Version 1.0.1.1 : Testing on lazarus 1.4.4.' + #13#10
                                           + 'Version 1.0.1.0 : Simplifying.' + #13#10
                                           + 'Version 1.0.0.0 : Tested.' ;
			                 UnitType : CST_TYPE_UNITE_FONCTIONS ;
			                 Major : 1 ; Minor : 0 ; Release : 1 ; Build : 1 );
{$ENDIF}

procedure MyGraphicShowWorking(const sText:string;const Cancel:boolean=false);//AL
procedure MyGraphicCloseWorking;

function MyGraphicShowMessage ( const Msg:string;
                         const amb_Buttons : TMyMsgDlgButtons;
                         const ADlgType:TMyMsgDlgType;
                         const proprio:TComponent;
                         const StyleLb:TMyFontStyles):Word;
function MyGraphicMessageDlg  ( const Msg:string;
                         const ADlgType:TMsgDlgType=mtError;
                         const AButtons:TMsgDlgButtons=[mbOK];
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word; overload;
function MyGraphicMessageDlg  ( const Title,Msg:string;
                         const ADlgType:TMsgDlgType=mtError;
                         const AButtons:TMsgDlgButtons=[mbOK];
                         const Help : Integer = 0;
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word; overload;
function AMessageDlg   ( const Msg:string;
                         const ADlgType:TMsgDlgType;
                         const AButtons:TMsgDlgButtons;
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word;


var gF_Working:TFWorking=nil;
    gb_btnCancel:boolean;

implementation

uses u_form_msg,
     Forms;


procedure MyGraphicShowWorking(const sText:string;const Cancel:boolean=false);
begin
  if not Assigned(gF_Working) then
    gF_Working:=TFWorking.create(Application);
  gb_btnCancel:=False;
  gF_Working.doInit(sText,Cancel);
end;

procedure MyGraphicCloseWorking;
begin
  // some problems at closing software on linux
  FreeAndNil(gF_Working)
end;


function MyGraphicMessageDlg  ( const Msg:string;
                         const ADlgType:TMsgDlgType=mtError;
                         const AButtons:TMsgDlgButtons=[mbOK];
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word; overload;
Begin
  MyGraphicCloseWorking;
  Result := AMessageDlg( Msg, ADlgType, AButtons, proprio, StyleLb);
end;

function MyGraphicShowMessage ( const Msg:string;
                         const amb_Buttons : TMyMsgDlgButtons;
                         const ADlgType:TMyMsgDlgType;
                         const proprio:TComponent;
                         const StyleLb:TMyFontStyles):Word;
Begin
  MyGraphicCloseWorking;
  {$IFDEF FPC}
  Result := AMessageDlg( Msg, TMsgDlgType(ADlgType), TMsgDlgButtons(Longword(Word(amb_Buttons))), TControl(proprio), TFontStyles(LongWord(Byte((StyleLb)))));
  {$ELSE}
  Result := AMessageDlg( Msg, TMsgDlgType(ADlgType), TMsgDlgButtons(amb_Buttons), TControl(proprio), TFontStyles(StyleLb));
  {$ENDIF}
end;

function CreateMessageDlg(const Msg:string;const ADlgType:TMsgDlgType;const AButtons:TMsgDlgButtons;const proprio:TControl=nil;const StyleLb:TFontStyles=[fsBold]):TFMsg;
begin
  Result:=TFMsg.create(proprio);
  with Result do
   Begin
    lbMsg.Caption:=Msg;
    lbMsg.Font.Style := StyleLb;
    Buttons:=AButtons;
    DlgType:=ADlgType;
    AOwner:=proprio;
    InitMessage;
   end;

end;

function AMessageDlg   ( const Msg:string;
                         const ADlgType:TMsgDlgType;
                         const AButtons:TMsgDlgButtons;
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word;
var lf_MessageDlg : TFMsg;
Begin
 lf_MessageDlg := CreateMessageDlg(Msg, ADlgType, AButtons, proprio, StyleLb);
 try
   result:=lf_MessageDlg.ShowModal;
 finally
   lf_MessageDlg.Destroy;
 end;
end;

function MyGraphicMessageDlg  ( const Title,Msg:string;
                         const ADlgType:TMsgDlgType=mtError;
                         const AButtons:TMsgDlgButtons=[mbOK];
                         const Help : Integer = 0;
                         const proprio:TControl=nil;
                         const StyleLb:TFontStyles=[fsBold]):Word; overload;
var lf_MessageDlg : TFMsg;
Begin
 lf_MessageDlg := CreateMessageDlg(Msg, ADlgType, AButtons, proprio, StyleLb);
 lf_MessageDlg.Caption:=Title;
 lf_MessageDlg.HelpContext:=Help;
 try
   result:=lf_MessageDlg.ShowModal;
 finally
   lf_MessageDlg.Destroy;
 end;
end;

initialization
{$IFDEF VERSIONS}
  // adding optional version infos
  p_ConcatVersion ( gVer_fonctions_dialogs );
{$ENDIF}
end.
