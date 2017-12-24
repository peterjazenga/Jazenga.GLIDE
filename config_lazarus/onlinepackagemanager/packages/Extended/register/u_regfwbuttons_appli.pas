{*********************************************************************}
{                                                                     }
{                                                                     }
{             Matthieu Giroux                                         }
{             TExtNumEdit  :                                       }
{             Composant edit de nombre              }
{             TExtDBNumEdit :                                       }
{             Composant dbedit de nombre }
{             22 Avril 2006                                           }
{                                                                     }
{                                                                     }
{*********************************************************************}

unit u_regfwbuttons_appli;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

uses
{$IFDEF FPC}
  lresources,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes;

procedure Register;

implementation

uses
    u_buttons_appli,
  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
    u_buttons_defs;

procedure Register;
begin
  RegisterComponents(CST_PALETTE_BOUTONS, [
                                   TFWRefresh,TFWDate,TFWFolder,
                                   TFWNext,TFWPrior,TFWLoad,TFWTrash,TFWConfig, TFWClose, TFWCancel, TFWOK,
                                   {$IFDEF GROUPVIEW}TFWBasket,TFWInSelect,TFWInAll,TFWOutSelect,TFWOutAll,
                                   TFWInSelectV,TFWInAllV,TFWOutSelectV,TFWOutAllV,{$ENDIF}
                                   TFWInsert,TFWInit,TFWAdd,TFWDelete,TFWDocument,
                                   TFWQuit,TFWErase,TFWImport,TFWExport,TFWPrint,TFWPreview,TFWCopy,
                                   TFWSaveAs,TFWSearch,TFWZoomIn,TFWZoomOut,TFWMedia]);
End ;

{$IFNDEF MEMBUTTONS}
{$IFDEF FPC}
initialization
{$I u_buttons_appli.lrs}
{$ENDIF}
{$ENDIF}
end.
