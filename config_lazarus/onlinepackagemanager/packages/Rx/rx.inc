{ global include module

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}

{$H+}

{$DEFINE RX_D3}
{$DEFINE RX_D4}
{$DEFINE HASVARIANT}
{.$DEFINE ENABLE_Child_Defs}
{$DEFINE NEW_STYLE_TITLE_ALIGNMENT_RXDBGRID}
{.$DEFINE RXDBGRID_OPTIONS_WO_CANCEL_ON_EXIT}
{.$DEFINE USED_BiDi}
{$DEFINE DEFAULT_POPUP_CALENDAR}
{.$DEFINE FIX_BUG_FieldNo}

{.DEFINE USE_TRXXPManifest}
{.$DEFINE USE_TRxAppIcon}

{.$DEFINE USE_DEPRECATES}

{$if FPC_FULLVERSION<20701}
  {$DEFINE NoAutomatedBookmark}
  {$DEFINE RX_USE_LCL_DEVEL}
{$endif}

{$IFDEF NoAutomatedBookmark}
{$ELSE}
{$ENDIF}

{.$DEFINE OLD_fpSPREADSHEET}
