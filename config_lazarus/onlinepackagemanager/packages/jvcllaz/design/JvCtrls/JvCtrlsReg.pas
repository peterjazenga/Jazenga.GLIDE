unit JvCtrlsReg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure Register;

implementation

{$R ../../resource/JvHTControlsReg.res}

uses
  Classes, JvDsgnConsts,
  JvHtControls, {JvDBHTLabel,} JvHint, JvHTHintForm,
  PropEdits, Controls;

procedure Register;
begin
  RegisterComponents(RsPaletteLabel, [TJvHTLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvHTListBox, TJvHTComboBox]);
  RegisterComponents(RsPaletteNonVisual, [TJvHint]);

  RegisterPropertyEditor(TypeInfo(TCaption), TJvHTLabel, 'Caption', TJvHintProperty);

  (*
  RegisterComponents('JvHTControls', [TJvHTLabel, TJvHTComboBox, TJvHTListBox,
    TJvDBHTLabel, TJvHint]);
    *)
end;

end.

