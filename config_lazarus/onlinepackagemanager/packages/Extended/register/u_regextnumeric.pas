unit u_regextnumeric;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
  Classes,
  SysUtils;

procedure Register;

implementation

uses U_ExtNumEdits,
  {$IFDEF FPC}
     unite_messages,
  {$ELSE}
    unite_messages_delphi,
  {$ENDIF}
  {$IFDEF FPC}
     lresources,
  {$ENDIF}
  U_ExtOperation,
  U_ExtOperate,
  U_ExtConstOperation,
  U_ExtMultiOperation;

procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS_DB, [TExtDBNumEdit]);
  RegisterComponents(CST_PALETTE_COMPOSANTS   , [TExtNumEdit,TExtOperation,TExtCOperation,TExtMOperation]);
  RegisterComponents(CST_PALETTE_COMPOSANTS_INVISIBLE, [TExtOperate,TExtOperateForm]);
end;

{$IFDEF FPC}
initialization
  {$i U_ExtNumEdits.lrs}
  {$i U_ExtOperations.lrs}
{$ENDIF}
end.

