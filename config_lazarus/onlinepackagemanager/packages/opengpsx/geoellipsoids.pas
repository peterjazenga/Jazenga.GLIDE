{ <GeoEllipsoids>

  Copyright (C) 2010 Prajuab Riabroy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit geoellipsoids;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils;

type

  TGeoEllipsoidType = (geWGS84, geGRS80, geWGS72, geAustraliean_1965, geKrasovsky_1940,
                       geInternational_1924, geClake_1880, geClake_1866, geAiry_1830,
                       geBessel_1841, geEverest_1830, geHayford_1909, geNorth_American_1927,
                       geNAD_83);

  TEllipsoidCollectionItem = class(TCollectionItem)
  private
   FEllipsoidName : String;
   FMajorAxis : double;
   FInvFlat : double;
  protected
    function GetDisplayName : string; override;
  public
    procedure Assign(Source : TPersistent); override;
  published
    property EllipsoidName : string read FEllipsoidName write FEllipsoidName;
    property MajorAxis : double read FMajorAxis write FMajorAxis;
    property InvFlattening : double read FInvFlat write FInvFlat;
  end;

  TEllipsoids = class (TPersistent)
  private
    FEllipCollectionProp : TCollection;
    procedure SetEllipsoidCollectionProp(const value : TCollection);
  public
    constructor Create(AOwner : TComponent);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
  published
    property Ellipsoids : TCollection
             read FEllipCollectionProp
             write SetEllipsoidCollectionProp;
  end;

  TEllipsoid = class(TPersistent)
  private
    FEllipName : string;
    FMajorAxis : double;
    FInvFlat : double;
  public
    procedure Assign(Source : TPersistent); override;
    constructor Create ;
    destructor  Destroy ; override ;
  published
    property EllipsoidName : string read FEllipName write FEllipName;
    property MajorAxis : double read FMajorAxis write FMajorAxis;
    property InvFlattening : double read FInvFlat write FInvFlat;
  end;
{
  TEllipsoidList = class (TStringList)
    private

    protected
      function GetEllipsoid(idx : Integer ) : TEllipsoid ;
    public
      constructor Create ;
      destructor  Destroy ; override ;
      procedure   AddEx (const Name : string; const MajorAxisA : double; const InvFlat : double);
      function    FindEx(const Name : string): TEllipsoid ;
  end;
}
//var
  //EllipsoidList : TEllipsoidCollection;
  //Ellipsoids : TEllipsoidList;

implementation

{TEllipsoidCollectionItem}
procedure TEllipsoidCollectionItem.Assign(Source : TPersistent);
begin
 if (Source is TEllipsoidCollectionItem) then
 begin
   FEllipsoidName := TEllipsoidCollectionItem(Source).EllipsoidName;
   FMajorAxis := TEllipsoidCollectionItem(Source).MajorAxis;
   FInvFlat := TEllipsoidCollectionItem(Source).InvFlattening;
 end
 else
   inherited;
end;
function TEllipsoidCollectionItem.GetDisplayName : string;
begin
   result := FEllipsoidName;
   inherited GetDisplayName;
end;

procedure TEllipsoids.Assign(Source : TPersistent);
begin
  if Source is TEllipsoids then
  with TEllipsoids(Source) do
  begin
    //This actually assigns
    Self.Ellipsoids := Ellipsoids;
  end
  else
    inherited; //raises an exception
end;

constructor TEllipsoids.Create(AOwner : TComponent);
begin
  inherited Create;
  FEllipCollectionProp := TCollection.Create(TEllipsoidCollectionItem);
end;

procedure TEllipsoids.SetEllipsoidCollectionProp(const Value : TCollection);
begin
  FEllipCollectionProp.Assign(Value);
end;

destructor TEllipsoids.Destroy;
begin
  FEllipCollectionProp.Free;
  inherited;
end;

{TEllipsoid}
constructor TEllipsoid.Create;
begin
  inherited create;
end;

destructor TEllipsoid.Destroy;
begin
  inherited Destroy;
end;

procedure TEllipsoid.Assign(Source : TPersistent);
begin
  if Source is TEllipsoid then
  with TEllipsoid(Source) do
  begin
    Self.EllipsoidName := TEllipsoid(Source).EllipsoidName;
    Self.MajorAxis := TEllipsoid(Source).MajorAxis;
    Self.InvFlattening := TEllipsoid(Source).InvFlattening;
  end
  else
    inherited; //raises an exception
end;
{
//TEllipsoidList
constructor TEllipsoidList.Create ;
begin
  inherited Create;
  Sorted := True ;
  AddEx('WGS84', 6378137, 298.257223563);
  AddEx('GRS80', 6378137, 298.257222101);
  AddEx('WGS72', 6378135, 298.260);
  AddEx('Australian 1965', 6378160, 298.250);
  AddEx('Krasovsky 1940', 6378245, 298.3);
  AddEx('International (1924)', 6378388, 297);
  AddEx('Clake 1880', 6378249.1, 293.460);
  AddEx('Clarke 1866', 6378206.4, 294.980);
  AddEx('Airy 1830', 6377563.4, 299.320);
  AddEx('Bessel 1841', 6377397.2, 299.150);
  AddEx('Everest 1830', 6377276.345, 300.8017);
  AddEx('Hayford 1909', 6378388.0, 296.999362081575);
  AddEx('North American 1927', 6378206.4, 294.978698213898);
  AddEx('NAD 83', 6378137, 298.257223563);
end ;


destructor TEllipsoidList.Destroy ;
var
  i : Integer ;
begin
 for i:= Count -1 downto 0 do
 begin
   if Assigned( Objects[i] ) then
      TEllipsoidList( Objects[i] ).Free ;
  end ;
  inherited Destroy;
end ;

procedure TEllipsoidList.AddEx(const Name          : string ;
                               const MajorAxisA     : double ;
                               const InvFlat : double);
var
  e : TEllipsoid ;
  pos : Integer ;
begin
  if Find(Name, pos ) then begin
    if Assigned(Objects[pos]) then
      TEllipsoid(Objects[pos]).Free ;
  end ;
  pos := Add(Name) ;

  e := TEllipsoid.Create ;
  with e do
  begin
    EllipsoidName := Name;
    MajorAxis     := MajorAxisA;
    InvFlattening := InvFlat;
  end ;
  Objects[pos] := e;

end;

function TEllipsoidList.FindEx(const Name : String): TEllipsoid ;
var
  pos : Integer ;
begin
  if Find( Name, pos ) then
    Result := TEllipsoid(Objects[pos])
  else
    Result := nil ;
end ;

function TEllipsoidList.GetEllipsoid(idx : Integer) : TEllipsoid ;
begin
  Result := TEllipsoid( Objects[ idx ] ) ; ;
end ;
}
Initialization
  //Ellipsoids := TEllipsoidList.Create;

Finalization
  //Ellipsoids.Free;
end.

