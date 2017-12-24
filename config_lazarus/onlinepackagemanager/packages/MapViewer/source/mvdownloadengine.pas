{ Map Viewer Download Engine Synapse

  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit mvDownloadEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
Type

  { TCustomDownloadEngine }

  TCustomDownloadEngine = class(TComponent)
  public
    procedure DownloadFile(const Url: string; str: TStream); virtual;
  end;

implementation

{ TCustomDownloadEngine }

procedure TCustomDownloadEngine.DownloadFile(const Url: string; str: TStream);
begin

end;

end.

