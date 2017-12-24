program rx_ext_test_harness;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, rxnew, rx_ext_test_case_1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

