unit JvPageCompsReg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, SysUtils;

procedure Register;

implementation

uses
  JvDsgnConsts, JvNavigationPane;

procedure Register;
begin
  RegisterComponents(RsPaletteNavPane, [TJvNavigationPane, TJvNavIconButton,
    TJvNavPanelButton, TJvNavPanelHeader, TJvNavPanelDivider, TJvOutlookSplitter,
    TJvNavPaneStyleManager, TJvNavPaneToolPanel]);
end;

initialization
  {$I ..\..\resource\JvNavigationPaneLaz.lrs}

end.

