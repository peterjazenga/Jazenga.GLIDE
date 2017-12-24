{
    This file is part of the Web Service Toolkit
    Copyright (c) 2007 by Inoussa OUEDRAOGO

    This file is provide under modified LGPL licence
    ( the files COPYING.modifiedLGPL and COPYING.LGPL).


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit common_gui_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls;
  
  function FindItem(const ACaption : string; AList : TListItems) : TListItem ;

implementation

function FindItem(const ACaption : string; AList : TListItems) : TListItem ;
var
  i : Integer;
begin
  for i := 0 to Pred(AList.Count) do begin
    if AnsiSameText(ACaption,AList[i].Caption) then begin
      Result := AList[i];
      Exit;
    end;
  end;
  Result := nil;
end;


end.

