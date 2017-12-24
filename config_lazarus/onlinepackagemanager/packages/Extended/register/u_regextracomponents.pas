unit u_regextracomponents;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
  Classes,
{$IFDEF FPC}
  lresources,
{$ENDIF}
  SysUtils;

procedure Register;

implementation

uses
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
  {$IFDEF FPC}
  ComponentEditors, dbpropedits, PropEdits,
  {$ELSE}
   DBReg, Designintf,
  {$ENDIF}
  u_framework_components,
  u_framework_dbcomponents,
  U_ExtComboInsert;

procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS_DB, [TFWDBComboBox,
                                                TExtDBComboInsert,
                                                TFWDBDateEdit, TFWDBDateTimePicker,
                                                TFWDBEdit,TFWDBFileEdit,
                                                TFWDBLookupCombo,
                                                TFWDBMemo,
                                                TFWDBSpinEdit]);
  RegisterComponents(CST_PALETTE_COMPOSANTS   , [TFWComboBox, TFWDateEdit,
                                                TFWDateTimePicker, TFWEdit,TFWFileEdit,TFWGrid,
                                                TFWLabel,TFWMemo,
                                                TFWSpinEdit]);

end;

{$IFDEF FPC}
initialization
  {$i u_extsearchedit.lrs}
  {$i U_ExtComboInsert.lrs}
{$ENDIF}
end.

