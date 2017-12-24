{**********************************************************************
                PilotLogic Software House.
    
 Package pl_LockBox.pkg 
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

{$I LockBox.inc}

unit alllockboxreg;

{$MODE Delphi}

  {-LockBox About Box and component registration}

interface

uses
  LCLIntf, LCLType, LMessages,
  Messages,
  Dialogs,
  TypInfo, lresources, PropEdits, ComponentEditors,
  StdCtrls,
  Graphics,
  ExtCtrls,
  Controls,
  Forms,
  SysUtils,
  Classes;


procedure Register;

implementation
 {$R alllockboxreg.res}
uses
  LbClass, LbAsym, LbRSA, LbDSA, LbKeyEd1, LbKeyEd2,
  LbConst;



{ == component registration ================================================ }
procedure Register;
begin
  RegisterComponentEditor(TLbSymmetricCipher, TLbSymmetricKeyEditor);
  RegisterComponentEditor(TLbRSA, TLbRSAKeyEditor);
  RegisterComponentEditor(TLbRSASSA, TLbRSAKeyEditor);

  (* RegisterComponentEditor(TLbDSA, TLbDSAKeyEditor); *)

  RegisterComponents('LockBox',
                     [TLbBlowfish,
                      TLbDES,
                      TLb3DES,
                      TLbRijndael,
                      TLbRSA,
                      TLbMD5,
                      TLbSHA1,
                      TLbDSA, 
                      TLbRSASSA]
                      );
end;

end.
