{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit P2PContnrs;

interface

uses SysUtils;

const
  A = True;

type

  CObjectList = class
  strict private
    FFreeObjects: boolean;
    FItems: array of TObject;
  protected
    function GetCount: integer;
    procedure SetCount(const ACount: integer);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Clear;
    function GetItem(const AIndex: integer): TObject;
    function IndexOf(const AObject: TObject): integer;
    procedure Add(const AObject: TObject);
    procedure Delete(AIndex: integer);
    procedure Remove(const AItem: TObject);
    procedure SetItem(const AIndex: integer; const AObject: TObject);
  published
    property Count: integer read GetCount write SetCount;
  end;

implementation

constructor CObjectList.Create;
begin
  inherited Create;
  FFreeObjects := True;
end;

destructor CObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function CObjectList.GetCount: integer;
begin
  Result := Length(FItems);
end;

procedure CObjectList.SetCount(const ACount: integer);
begin
  SetLength(FItems, ACount);
end;

procedure CObjectList.Clear;
var
  LIndex: integer;
begin
  if FFreeObjects then begin
    for LIndex := High(FItems) downto 0 do begin
      FreeAndNil(FItems[LIndex]);
    end;
  end;
  SetLength(FItems, 0);
end;

function CObjectList.GetItem(const AIndex: integer): TObject;
begin
  Result := FItems[AIndex];
end;

procedure CObjectList.SetItem(const AIndex: integer; const AObject: TObject);
begin
  FItems[AIndex] := AObject;
end;

procedure CObjectList.Add(const AObject: TObject);
begin
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := AObject;
end;

function CObjectList.IndexOf(const AObject: TObject): integer;
begin
  for Result := 0 to High(FItems) do begin
    if FItems[Result] = AObject then begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure CObjectList.Delete(AIndex: integer);
begin
  Assert((AIndex >= 0) and (AIndex < Length(FItems)));
  if FFreeObjects then begin
    FreeAndNil(FItems[AIndex]);
  end;
  while AIndex < High(FItems) do begin
    FItems[AIndex] := FItems[AIndex + 1];
    AIndex := AIndex + 1;
  end;
  SetLength(FItems, Length(FItems) - 1);
end;

procedure CObjectList.Remove(const AItem: TObject);
var
  LIndex: integer;
begin
  LIndex := IndexOf(AItem);
  if LIndex <> -1 then begin
    Delete(LIndex);
  end;
end;

end.
