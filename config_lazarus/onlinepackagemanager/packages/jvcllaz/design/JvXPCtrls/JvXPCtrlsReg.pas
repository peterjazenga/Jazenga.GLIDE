unit JvXPCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

uses
  ComponentEditors, PropEdits, GraphPropEdits, ImgList,
  JvDsgnConsts, JvXPCore, JvXPPropertyEditors,
  JvXPBar, JvXPContainer, JvXPButtons, JvXPCheckCtrls;

procedure Register;
begin
  RegisterComponents(RsPaletteXPControls, [
    TJvXPBar,
    TJvXPContainer,
    TJvXPButton,
    TJvXPToolButton,
    TJvXPCheckbox,
    TJvXPStyleManager
  ]);

  {
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvXPBarItem, 'ImageIndex',
    TJvXPItemImageIndexProperty);
  }

  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvXPBarItem, 'ImageIndex',
    TImageIndexPropertyEditor);

  RegisterComponentEditor(TJvXPBar, TJvXPBarItemEditor);
end;

initialization
  {$I ../resource/JvXPCtrlsLaz.lrs}
  
end.

