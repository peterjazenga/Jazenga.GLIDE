unit u_regimagecomponents;

{$IFDEF FPC}
{$mode Delphi}
{$ENDIF}


{$I ..\DLCompilers.inc}
{$I ..\extends.inc}

interface

uses
  {$IFDEF FPC}
  lresources,
  {$ENDIF}
  Classes;

procedure Register;

implementation

uses  {$IFDEF FPC}
  unite_messages,
  {$ELSE}
  unite_messages_delphi,
  {$ENDIF}
     U_ExtDBNavigator,
     u_extsearchedit,
   {$IFDEF FPC}
     dbpropedits, PropEdits,U_ExtImages, U_ExtDBImages,
{$ELSE}
     DBReg, Designintf,
{$ENDIF}
     U_ExtDBImage, U_ExtDBImageList, U_ExtImage,
     U_ExtPictCombo, U_ExtDBPictCombo, U_ExtMapImageIndex,
     u_extdbgrid, u_buttons_defs,
     u_extimagelist;

procedure Register;
begin
  RegisterComponents(CST_PALETTE_COMPOSANTS_INVISIBLE, [TExtMapImages]);
  RegisterComponents(CST_PALETTE_COMPOSANTS_DB, [TExtDBImage,{$IFDEF FPC}TExtDBImages,{$ENDIF}TExtDBImageList,
                                                 TExtDBGrid,TExtDBPictCombo]);
  RegisterComponents(CST_PALETTE_BOUTONS   , [TFWButton]);
  RegisterComponents(CST_PALETTE_COMPOSANTS   , [TExtImage,{$IFDEF FPC}TExtImages,{$ENDIF}TExtImageList,
                                                TExtPictCombo]);
  RegisterPropertyEditor ( TypeInfo(string), TExtDBNavigator, 'SortField', {$IFDEF FPC}TFieldProperty{$ELSE}TDataFieldProperty{$ENDIF});
  RegisterPropertyEditor ( TypeInfo(string), TExtSearchDBEdit, 'FieldKey'   , {$IFDEF FPC}TFieldProperty{$ELSE}TDataFieldProperty{$ENDIF});
end;

{$IFDEF FPC}
initialization
  {$I u_regimagecomponents.lrs}
  {$i U_ExtPictCombo.lrs}
  {$i U_ExtDBImageList.lrs}
  {$i U_ExtImageList.lrs}
  {$i u_extmapimageindex.lrs}
  {$i u_extimage.lrs}
  {$i u_extimages.lrs}
  {$i u_extdbgrid.lrs}
{$ENDIF}
end.

