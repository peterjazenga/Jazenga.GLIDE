unit wst_delphi_rtti_utils;

interface
uses SysUtils, TypInfo;


  function GetEnumNameCount(AEnumTypeInfo: PTypeInfo): Integer;

implementation

function GetEnumNameCount(AEnumTypeInfo: PTypeInfo): Integer;
var
  T: PTypeData;
begin
  T :=  GetTypeData(AEnumTypeInfo);
  Result := ( T^.MaxValue - T^.MinValue ) + 1;
end;

end.
