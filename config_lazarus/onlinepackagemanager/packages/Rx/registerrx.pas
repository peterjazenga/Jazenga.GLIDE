{ registerrx unit

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

unit registerrx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LazarusPackageIntf;

const
  RxCtrllPageName = 'RX Controls';
  RxToolsPageName = 'RX Tools';

procedure Register;

implementation
uses
  PropEdits, rxfolderlister, rxduallist, RxHistoryNavigator,
  rxcurredit, rxswitch, rxdice, rxtoolbar,
  {$IFDEF USE_TRXXPManifest}
  rxxpman,
  {$ENDIF}
  rxpagemngr,
  {$IFDEF USE_TRxAppIcon}
  RxAppIcon,
  {$ENDIF}
  Dialogs, ComponentEditors, DBPropEdits, rxctrls, RxAutoPanel, rxpickdate, rxtooledit, rxclock,
  rxspin, RxTimeEdit, rxDateRangeEditUnit, RxAboutDialog, RxViewsPanel, RxMDI;

{$R rx.res}

{$IFDEF USE_TRxAppIcon}
procedure RegisterRxAppIcon;
begin
  RegisterComponents(RxCtrllPageName,[TRxAppIcon]);
end;
{$ENDIF}
{$IFDEF USE_TRXXPManifest}
procedure RegisterRxXPMan;
begin
  RegisterComponents(RxCtrllPageName,[TRXXPManifest]);
end;
{$ENDIF}

procedure RegisterPageMngr;
begin
  RegisterComponents(RxCtrllPageName,[TPageManager]);
end;


procedure RegisterDualList;
begin
  RegisterComponents(RxCtrllPageName,[TDualListDialog]);
end;

procedure RegisterCurrEdit;
begin
  RegisterComponents(RxCtrllPageName,[TCurrencyEdit]);
end;

procedure RegisterRXSwitch;
begin
  RegisterComponents(RxCtrllPageName,[TRxSwitch]);
end;

procedure RegisterRXDice;
begin
  RegisterComponents(RxCtrllPageName,[TRxDice]);
end;

procedure RegisterFolderLister;
begin
  RegisterComponents(RxCtrllPageName,[TFolderLister]);
end;

procedure RegisterRxToolBar;
begin
  RegisterComponents(RxCtrllPageName,[TToolPanel]);
end;

procedure RegisterRxCtrls;
begin
  RegisterComponents(RxCtrllPageName,[TRxLabel, TSecretPanel, TRxSpeedButton, TRxRadioGroup]);
end;


procedure RegisterAutoPanel;
begin
  RegisterComponents(RxCtrllPageName,[TAutoPanel]);
end;

procedure RegisterPickDate;
begin
  RegisterComponents(RxCtrllPageName,[TRxCalendarGrid]);
end;

procedure RegisterToolEdit;
begin
  RegisterComponents(RxCtrllPageName,[TRxDateEdit]);
end;

procedure RegisterRxClock;
begin
  RegisterComponents(RxCtrllPageName,[TRxClock]);
end;

procedure RegisterRxSpin;
begin
  RegisterComponents(RxCtrllPageName,[TRxSpinButton, TRxSpinEdit]);
end;

procedure RegisterRxTimeEdit;
begin
  RegisterComponents(RxCtrllPageName,[TRxTimeEdit]);
end;

procedure RegisterRxAboutDialog;
begin
  RegisterComponents(RxCtrllPageName,[TRxAboutDialog]);
end;

procedure RegisterRxViewsPanel;
begin
  RegisterComponents(RxCtrllPageName,[TRxViewsPanel]);
end;

procedure RegisterRxMDI;
begin
  RegisterComponents(RxCtrllPageName,[TRxMDICloseButton, TRxMDIPanel, TRxMDITasks]);
end;

procedure RegisterRxHistoryNavigator;
begin
  RegisterComponents(RxToolsPageName,[TRxHistoryNavigator]);
end;

procedure RegisterrxDateRangeEditUnit;
begin
  RegisterComponents(RxCtrllPageName,[TRxDateRangeEdit]);
end;

procedure Register;
begin
  //RX
  RegisterUnit('rxfolderlister', @RegisterFolderLister);
  RegisterUnit('rxduallist', @RegisterDualList);
  RegisterUnit('rxcurredit', @RegisterCurrEdit);
  RegisterUnit('rxswitch', @RegisterRXSwitch);
  RegisterUnit('rxdice', @RegisterRXDice);
  {$IFDEF USE_TRXXPManifest}
  RegisterUnit('RxXPMan', @RegisterRxXPMan);
  {$ENDIF}
  RegisterUnit('rxPageMngr', @RegisterPageMngr);
  RegisterUnit('rxtoolbar', @RegisterRxToolBar);
  {$IFDEF USE_TRxAppIcon}
  RegisterUnit('rxappicon', @RegisterRxAppIcon);
  {$ENDIF}
  RegisterUnit('rxctrls', @RegisterRxCtrls);
  RegisterUnit('RxAutoPanel', @RegisterAutoPanel);
  RegisterUnit('rxpickdate', @RegisterPickDate);
  RegisterUnit('rxtooledit', @RegisterToolEdit);
  RegisterUnit('rxclock', @RegisterRxClock);
  RegisterUnit('rxspin', @RegisterRxSpin);
  RegisterUnit('RxTimeEdit', @RegisterRxTimeEdit);
  RegisterUnit('RxAboutDialog', @RegisterRxAboutDialog);
  RegisterUnit('RxViewsPanel', @RegisterRxViewsPanel);
  RegisterUnit('RxHistoryNavigator', @RegisterRxHistoryNavigator);
  RegisterUnit('RxMDI', @RegisterRxMDI);
  RegisterUnit('rxDateRangeEditUnit', @RegisterrxDateRangeEditUnit);

end;

//initialization
//  {$i rx.lrs}
end.
