unit jdbutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

function FieldIsEditable(Field: TField): boolean;

implementation

function FieldIsEditable(Field: TField): boolean;
begin
  Result := (Field <> nil) and (not Field.Calculated) and
    (Field.DataType <> ftAutoInc) and (not Field.Lookup);
end;

end.

