unit u_regsbbuttons;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
{$IFDEF FPC}
  lresources,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes;

procedure Register;

implementation

uses u_buttons_speed;

procedure Register;
begin
  RegisterComponents('SBButtons', [TSBDate,TSBMDate,TSBRefresh,TSBClose,TSBNext,TSBPrior,TSBLoad,TSBTrash,TSBConfig,
                                   {$IFDEF GROUPVIEW}TSBBasket,TSBInSelect,TSBInAll,TSBOutSelect,TSBOutAll,{$ENDIF}
                                   TSBOK,TSBInsert,TSBInit,TSBDelete,TSBMDelete,TSBDocument,TSBCancel,TSBQuit,TSBErase,
                                   TSBSaveAs,TSBAdd,TSBMAdd,TSBImport,TSBExport,TSBPrint,TSBPreview,TSBCopy]);
End ;



initialization
{$IFDEF FPC}
  {$I u_regsbbuttons.lrs}
{$ENDIF}

end.

