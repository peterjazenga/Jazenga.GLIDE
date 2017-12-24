{ rxconst unit

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

unit rxconst;

interface

{$I RX.INC}


const
  RX_VERSION = $0002004B;  { 2.75 }


resourcestring
  sBrowse               = 'Browse';
  sDefaultFilter        = 'All files (*.*)|*.*';
  sDateDlgTitle         = 'Select a Date';
  sNextYear             = 'Next Year|';
  sNextMonth            = 'Next Month|';
  sPrevYear             = 'Previous Year|';
  sPrevMonth            = 'Previous Month|';
  sNotImplemented       = 'Function not yet implemented';
  sFileNotExec          = 'File specified is not an executable file, dynamic-link library, or icon file';
  sLoadLibError         = 'Could not load ''%s'' library';
  sDetails              = 'Details';
  sWindowsIcoFiles      = 'Windows Ico files (*.ico)|*.ico|All files (*.*)|*.*';
  sToCurDate            = 'Set current date';

  //TDualListDialog
  SDualListSrcCaption   = 'Source';
  SDualListDestCaption  = 'Destination';
  SDualListCaption      = 'Dual list dialog';

  //TToolPanelSetupForm
  sToolPanelSetup       = 'Tool panel setup';
  sVisibleButtons       = 'Visible buttons';
  sOptions              = 'Options';
  sAvaliableButtons     = 'Avaliable buttons';
  sShowCaption          = 'Show caption';
  sToolBarStyle         = 'Tool bar style';
  sToolBarStyle1        = 'Standard';
  sToolBarStyle2        = 'Windows XP';
  sToolBarStyle3        = 'Native';
  sFlatButtons          = 'Flat buttons';
  sTransparent          = 'Transparent';
  sShowHint             = 'Show hint';
  sButtonAlign          = 'Button align';
  sButtonAlign1         = 'None';
  sButtonAlign2         = 'Left';
  sButtonAlign3         = 'Right';
//  sGTKWidgetSet         = 'GTK widget set';
//  sGTK2WidgetSet        = 'GTK 2 widget set';
//  sWin32_64WidgetSet    = 'Win32/Win64 widget set';
//  sWinCEWidgetSet       = 'WinCE widget set';
//  sCarbonWidgetSet      = 'Carbon widget set';
//  sQTWidgetSet          = 'QT widget set';
//  sFpGUIWidgetSet       = 'FpGUI widget set';
//  sOtherGUIWidgetSet    = 'Other gui';
  sWidget               = 'Widget : ';
  sAppVersion           = 'Version : ';
  sLCLVersion           = 'LCL Version: ';
  sFpcVersion           = 'FPC version : ';
  sTargetCPU            = 'Target CPU : ';
  sTargetOS             = 'Target OS : ';
  sBuildDate            = 'Build date : ';
  sAbout                = 'About';
  sGeneral              = 'General';
  sLicense              = 'License';
  SOutOfRange           = 'Out of range %d %d %d %d';

  { TRxHistoryNavigator }
  sHistoryDesc          = 'History - "%s"';

  { RxCloseFormValidator }
  sCloseValidError      = 'Error. Expected values...';
  sReqValue             = 'Error. Expected value for field %s.';
  sExptControlNotFound  = 'Control not found in validate %s.';

  { RxMDI }
  sCloseWindows         = 'Close window';
  sCloseAllExceptThis   = 'Close all except this';
  sCloseAllWindows      = 'Close all windows';

  { TRxDateRangeEdit }
  sFirstQuarter         = 'First quarter';
  sSecondQuarter        = 'Second quarter';
  sThirdQuarter         = 'Third quarter';
  sFourthQuarter        = 'Fourth quarter';
  sFirstHalfOfYear      = 'First half of year';
  sSecondHalfOfYear     = 'Second half of year';

  { TFolderLister }
  sFolderListerErr      = '%s. Not assigned property MenuItem';

  { TConfigValues }
  sVariableIsNotString  = 'Variable %s is not string';
  sVariableIsNotInteger = 'Variable %s is not integer';
  sVariableIsNotFloat   = 'Variable %s is not float';
  sVariableIsNotDT      = 'Variable %s is not date/time';
  sVariableIsNotBoolean = 'Variable %s is not boolean';

implementation

end.
