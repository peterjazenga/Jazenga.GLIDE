unit delphi_init_com;

interface
uses ActiveX;

implementation

initialization
  CoInitialize(nil);

finalization
  CoUninitialize();
end.
