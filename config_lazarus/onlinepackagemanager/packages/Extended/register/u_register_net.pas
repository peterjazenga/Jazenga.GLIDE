unit u_register_net;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
{$IFDEF FPC}
     LResources,
     unite_messages,
{$ELSE}
     unite_messages_delphi,
{$ENDIF}
     u_netupdate, u_mailssendbutton;

procedure Register;
Begin
  RegisterComponents(CST_PALETTE_COMPOSANTS_INVISIBLE, [TNetUpdate]);
  RegisterComponents(CST_PALETTE_COMPOSANTS, [TExtSendMails]);
End;

{$IFDEF FPC}
initialization
  {$I *.lrs}
{$ENDIF}
end.

