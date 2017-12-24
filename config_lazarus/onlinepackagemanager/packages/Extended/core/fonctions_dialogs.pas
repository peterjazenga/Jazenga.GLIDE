unit fonctions_dialogs;

{$I ..\dlcompilers.inc}
{$I ..\extends.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes, SysUtils;

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


type
  TMyMsgDlgType    = (mmtWarning, mmtError, mmtInformation, mmtConfirmation,
                    mmtCustom);
  TMyMsgDlgBtn     = (mmbYes, mmbNo, mmbOK, mmbCancel, mmbAbort, mmbRetry, mmbIgnore,
                    mmbAll, mmbNoToAll, mmbYesToAll, mmbHelp, mmbClose);
  TMyMsgDlgButtons = set of TMyMsgDlgBtn;
  TMyFontStyle = (mfsBold, mfsItalic, mfsUnderline, mfsStrikeOut);
  TMyFontStyles = set of TMyFontStyle;
  TMessage = function ( const Msg:string;
                        const amb_Buttons : TMyMsgDlgButtons;
                        const ADlgType:TMyMsgDlgType;
                        const proprio:TComponent;
                        const Style : TMyFontStyles):Word;
  TCloseWorking = procedure;
  TShowWorking = procedure (const sText:string;const Cancel:boolean=false);

var ge_ShowMessage : TMessage = nil ;
 ge_ShowWorking : TShowWorking = nil ;
 ge_CloseWorking : TCloseWorking = nil ;

const
  mmbYesNoCancel = [mmbYes, mmbNo, mmbCancel];
  mmbYesNo = [mmbYes, mmbNo];
  mmbOKCancel = [mmbOK, mmbCancel];
  mmbAbortRetryIgnore = [mmbAbort, mmbRetry, mmbIgnore];



procedure doShowWorking(const sText:string;const Cancel:boolean=false);//AL
procedure doCloseWorking;

function MyShowMessage ( const Msg:string;
                         const ammb_Buttons : TMyMsgDlgButtons =[mmbOK] ;
                         const ADlgType:TMyMsgDlgType=mmtWarning;
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word;
function MyMessageDlg  ( const Msg:string;
                         const ADlgType:TMyMsgDlgType=mmtError;
                         const AButtons:TMyMsgDlgButtons=[mmbOK];
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word; overload;
function MyMessageDlg  ( const Title,Msg:string;
                         const ADlgType:TMyMsgDlgType=mmtError;
                         const AButtons:TMyMsgDlgButtons=[mmbOK];
                         const Help : Integer = 0;
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word; overload;


var gb_btnCancel:boolean;

implementation

uses fonctions_objects;


procedure doShowWorking(const sText:string;const Cancel:boolean=false);
begin
  if Assigned(ge_ShowWorking) then
    ge_ShowWorking ( sText, Cancel );
end;

procedure doCloseWorking;
begin
  if Assigned(ge_CloseWorking) then
    ge_CloseWorking;
end;


function MyMessageDlg  ( const Msg:string;
                         const ADlgType:TMyMsgDlgType=mmtError;
                         const AButtons:TMyMsgDlgButtons=[mmbOK];
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word; overload;
Begin
  if Assigned(ge_ShowMessage) then
    Result := ge_ShowMessage( Msg, AButtons, ADlgType,proprio,StyleLb);
end;

function MyShowMessage ( const Msg:string;
                         const ammb_Buttons : TMyMsgDlgButtons =[mmbOK] ;
                         const ADlgType:TMyMsgDlgType=mmtWarning;
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word;
Begin
  if Assigned(ge_ShowMessage) then
    Result := ge_ShowMessage( Msg, ammb_Buttons, ADlgType, proprio,StyleLb);
end;


function MyMessageDlg  ( const Title,Msg:string;
                         const ADlgType:TMyMsgDlgType=mmtError;
                         const AButtons:TMyMsgDlgButtons=[mmbOK];
                         const Help : Integer = 0;
                         const proprio:TComponent=nil;
                         const StyleLb:TMyFontStyles=[mfsBold]):Word; overload;
Begin
  if Assigned(ge_ShowMessage) then
    Result := ge_ShowMessage( Msg, AButtons, ADlgType, proprio,StyleLb);
end;

initialization
{$IFDEF VERSIONS}
  // adding optional version infos
  p_ConcatVersion ( gVer_fonctions_dialogs );
{$ENDIF}
end.
