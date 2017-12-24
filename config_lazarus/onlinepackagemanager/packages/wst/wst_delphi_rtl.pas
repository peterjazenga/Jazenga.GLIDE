{
    This file is part of the Web Service Toolkit
    Copyright (c) 2008 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{$INCLUDE wst_global.inc}
unit wst_delphi_rtl;

interface
uses
  wst_types;
  
{$IFDEF WST_DELPHI}
  function InterlockedExchange(var Target: Pointer; Value: Pointer): Integer; stdcall;
{$ENDIF WST_DELPHI}

implementation
{$IFDEF WST_DELPHI}
uses
  Windows;
{$ENDIF WST_DELPHI}

{$IFDEF WST_DELPHI}
function InterlockedExchange(var Target: Pointer; Value: Pointer): Integer;
begin
  Windows.InterlockedExchange(PtrInt(Target),PtrInt(Value));
end;
{$ENDIF WST_DELPHI}

end.