unit virtualghfdbtreesreg;

// This unit is an addendum to VirtualTrees.pas and contains code of design time editors as well as
// for theirs and the tree's registration.

interface


uses
  {$IFDEF LCL}
    LCLProc, LCLType, LMessages,
  {$ELSE}
    Windows,
  {$ENDIF}
  Classes,
  virtualtrees, VTHeaderPopup;


procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  TypInfo, VirtualDBTreeEx, VirtualGHFDBTreeEx;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('Virtual Controls', [TVirtualGHFDBTreeEx]);
end;

//----------------------------------------------------------------------------------------------------------------------

end.