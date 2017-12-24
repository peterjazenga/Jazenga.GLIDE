unit RegisterSpkToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazarusPackageIntf, SpkToolbar, PropEdits, ComponentEditors,
  SpkToolbarEditor, spkt_Buttons, spkt_Checkboxes, spkt_Pane, spkt_Tab, spkt_Appearance,
  LResources;

procedure Register;

implementation

uses
  ImgList;

procedure RegisterUnitSpkToolbar;
begin
  RegisterComponents('SpkToolbar', [TSpkToolbar]);
end;

procedure RegisterUnitSpkt_Buttons;
begin
  RegisterNoIcon([TSpkLargeButton, TSpkSmallButton]);
end;

procedure RegisterUnitSpkt_Checkboxes;
begin
  RegisterNoIcon([TSpkCheckbox, TSpkRadioButton]);
end;

procedure RegisterUnitSpkt_Pane;
begin
  RegisterNoIcon([TSpkPane]);
end;

procedure RegisterUnitSpkt_Tab;
begin
  RegisterNoIcon([TSpkTab]);
end;

procedure Register;
begin
  RegisterUnit('SpkToolbar', @RegisterUnitSpkToolbar);
  RegisterUnit('spkt_Buttons', @RegisterUnitSpkt_Buttons);
  RegisterUnit('spkt_Checkboxes', @RegisterUnitSpkt_Checkboxes);
  RegisterUnit('spkt_Pane', @RegisterUnitSpkt_Pane);
  RegisterUnit('spkt_Tab', @RegisterUnitSpkt_Tab);

  RegisterComponentEditor(TSpkToolbar, TSpkToolbarEditor);
  RegisterPropertyEditor(TypeInfo(TSpkToolbarAppearance), TSpkToolbar,
    'Appearance', TSpkToolbarAppearanceEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TSpkLargeButton, '',
    TSpkImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TSpkSmallButton, '',
    TSpkImageIndexPropertyEditor);
  //todo: register Caption Editor
end;

initialization
{$I SpkToolbar.lrs}

end.

