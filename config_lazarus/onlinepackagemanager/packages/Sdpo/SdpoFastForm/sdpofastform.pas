{ SdpoFastForm v0.1.7

  CopyRight (C) 2007-2010 Paulo Malheiros

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at paulo.malheiros@fe.up.pt
}
unit SdpoFastForm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, LCLType, Forms,
{$IFDEF LINUX}
  xlib,
  {$ifdef LCLGTK2}
  gtk2, gdk2, gdk2x, x, gdk2pixbuf, glib2;
  {$endif}
  {$ifdef LCLGTK}
  gtk, gdk, glib;
  {$endif}
{$ELSE}
  IntfGraphics, GraphType, LCLProc, LCLIntf;
{$ENDIF}


const
  FFPoNone = 0;
  FFPoCenter = 1;
  FFPoMouse = 2;
  FFPoSelectable = 3;
  FFItNormal = 0;
  FFItShared = 1;
  FFItFastest = 2;

type
  TFFPosition=(
    poNone,
    poCenter,
    poMouse,
    poSelectable);
    
const
  FFPositionConsts: array[TFFPosition] of LongWord =(
    FFPoNone,
    FFPoCenter,
    FFPoMouse,
    FFPoSelectable);
    
type
  TFFImageType=(
    itNormal,
    itShared,
    itFastest);

const
  FFImageTypeConsts: array[TFFImageType] of LongWord =(
    FFItNormal,
    FFItShared,
    FFItFastest);
    
{$IFDEF LINUX}
{$I sdpofastformlinux.inc}
{$ELSE}
{$I sdpofastformwindows.inc}
{$ENDIF}


procedure Register;
begin
  RegisterComponents('5dpo',[TSdpoFastForm]);
end;

initialization
{$i TSdpoFastForm.lrs}

end.    

