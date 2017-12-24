unit uconfigform;
{ LazAutoUpdater Tray Updater

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

  An example of using LazAutoUpdate as a silent updater

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
{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, Buttons,
  StdCtrls, EditBtn;

type

  { Tconfigform }

  Tconfigform = class(TForm)
    cmd_DeleteAppProfile: TButton;
    cmd_SaveChanges: TBitBtn;
    chk_Update: TCheckBox;
    cmb_IntervalDate: TComboBox;
    cmb_IntervalHour: TComboBox;
    cmb_IntervalDay: TComboBox;
    cmd_Close: TBitBtn;
    cmd_NewAppProfile: TButton;
    cmb_AppProfile: TComboBox;
    cmb_IntervalType: TComboBox;
    edt_INIPath: TEdit;
    edt_AppPath: TFileNameEdit;
    edt_AppVersion: TEdit;
    edt_SFUpdatesDirectory: TEdit;
    edt_ZipPath: TEdit;
    edt_SFProjectName: TEdit;
    grp_updateinterval: TGroupBox;
    grp_configapp: TGroupBox;
    lbl_LastUpdated: TLabel;
    lbl_AppVersion: TLabel;
    lbl_AppProfile: TLabel;
    lbl_AppPath: TLabel;
    lbl_INIPath: TLabel;
    lbl_IntervalDate: TLabel;
    lbl_SFUpdatesDirectory: TLabel;
    lbl_IntervalType: TLabel;
    lbl_SFUpdatesDirectory2: TLabel;
    lbl_IntervalDay: TLabel;
    lbl_ZipPath: TLabel;
    lbl_SFProjectName: TLabel;
    procedure chk_UpdateClick(Sender: TObject);
    procedure cmb_AppProfileClick(Sender: TObject);
    procedure cmb_AppProfileCloseUp(Sender: TObject);
    procedure cmd_CloseClick(Sender: TObject);
    procedure cmd_DeleteAppProfileClick(Sender: TObject);
    procedure cmd_NewAppProfileClick(Sender: TObject);
    procedure cmd_SaveChangesClick(Sender: TObject);
    procedure cmb_IntervalTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure DoInitialiseCombobox;
    procedure DoDisplayProfile(AIndex: integer);
    procedure DoSaveAllChanges;
    procedure DoEnableDisableIntervalCombos;
  public
    { public declarations }
    bDetailsChanged: boolean;
  end;

var
  configform: Tconfigform;

implementation

uses
  umainform;

{$R *.lfm}

{ Tconfigform }

procedure Tconfigform.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  Caption := 'Configure ' + Application.Title;
end;

procedure Tconfigform.DoSaveAllChanges;
begin
  with mainform.AppRecArray[cmb_AppProfile.ItemIndex] do
  begin
    AppPath := edt_AppPath.Text;
    AppVersion := edt_AppVersion.Text;
    INIPath := edt_INIPath.Text;
    ZipPath := edt_ZipPath.Text;
    SFProjectName := edt_SFProjectName.Text;
    SFUpdatesDirectory := edt_SFUpdatesDirectory.Text;
    IntervalType := cmb_IntervalType.ItemIndex;
    IntervalDay := cmb_IntervalDay.ItemIndex;
    IntervalDate := cmb_IntervalDate.ItemIndex;
    IntervalHour := cmb_IntervalHour.ItemIndex;
    Update := chk_Update.Checked;
    bDetailsChanged := True;
  end;
end;

procedure Tconfigform.DoDisplayProfile(AIndex: integer);
begin
  with mainform.AppRecArray[AIndex] do
  begin
    edt_AppPath.Text := AppPath;
    edt_AppVersion.Text := AppVersion;
    edt_INIPath.Text := INIPath;
    edt_ZipPath.Text := ZipPath;
    edt_SFProjectName.Text := SFProjectName;
    edt_SFUpdatesDirectory.Text := SFUpdatesDirectory;
    cmb_IntervalType.ItemIndex := IntervalType;
    cmb_IntervalDay.ItemIndex := IntervalDay;
    cmb_IntervalDate.ItemIndex := IntervalDate;
    cmb_IntervalHour.ItemIndex := IntervalHour;
    chk_Update.Checked := Update;
    lbl_LastUpdated.Caption :=
      Format('Last successful update was %s',
      [FormatDateTime('c', LastCheckDateTime)]);
    DoEnableDisableIntervalCombos;
  end;
end;

procedure Tconfigform.DoEnableDisableIntervalCombos;
begin
  case cmb_IntervalType.ItemIndex of
    0:
    begin
      cmb_IntervalDay.Enabled := False;
      cmb_IntervalDate.Enabled := False;
    end;
    1:
    begin
      cmb_IntervalDay.Enabled := True;
      cmb_IntervalDate.Enabled := False;
    end;
    2:
    begin
      cmb_IntervalDay.Enabled := False;
      cmb_IntervalDate.Enabled := True;
    end;
  end;
end;

procedure Tconfigform.cmd_SaveChangesClick(Sender: TObject);
begin
  DoSaveAllChanges;
  if (MessageDlg(Application.Title, 'Changes saved OK.  Close window now?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    cmd_Close.Click;
end;

procedure Tconfigform.cmd_CloseClick(Sender: TObject);
begin
  mainform.iCurrentRecIndex := cmb_AppProfile.ItemIndex;
  mainform.INI.WriteInteger('ProgramInfo', 'CurrentProfileIndex',
    mainform.iCurrentRecIndex);
  mainform.INI.UpdateFile;
end;

procedure Tconfigform.chk_UpdateClick(Sender: TObject);
begin
  mainform.AppRecArray[cmb_AppProfile.ItemIndex].Update := chk_Update.Checked;
  //  mainform.LongTimerArray[cmb_AppProfile.ItemIndex].Enabled := chk_Update.Checked;
end;

procedure Tconfigform.cmb_AppProfileClick(Sender: TObject);
begin
  DoSaveAllChanges;
  DoDisplayProfile(cmb_AppProfile.ItemIndex);
  mainform.iCurrentRecIndex := cmb_AppProfile.ItemIndex;

end;

procedure Tconfigform.cmb_AppProfileCloseUp(Sender: TObject);
begin
  DoDisplayProfile(cmb_AppProfile.ItemIndex);
  mainform.iCurrentRecIndex := cmb_AppProfile.ItemIndex;
end;

procedure Tconfigform.cmd_DeleteAppProfileClick(Sender: TObject);
// Make a temp copy of AppRecArray
// Copy over all but the deleted profile into the temp copy
// Copy the temp copy back to AppRecArray
// Delete the INIFile section
var
  TempAppRecArray: array of TAppRec;
  i, iLastIndex: integer;
begin
  if High(mainform.AppRecArray) = 0 then
  begin
    MessageDlg(Application.Title,
      'You cannot delete the only profile left',
      mtError, [mbOK], 0);
    exit;
  end;

  // Delete INI section
  mainform.INI.EraseSection(
    mainform.AppRecArray[cmb_AppProfile.ItemIndex].AppPrettyName);

  SetLength(TempAppRecArray, High(mainform.AppRecArray)); // one less
  iLastIndex := -1;
  for i := Low(mainform.AppRecArray) to High(mainform.AppRecArray) do
    if (i <> cmb_AppProfile.ItemIndex) then
    begin
      Inc(iLastIndex);
      TempAppRecArray[iLastIndex] := mainform.AppRecArray[i];
    end;
  SetLength(mainform.AppRecArray, High(TempAppRecArray) + 1);
  mainform.AppRecArray := TempAppRecArray;

  DoInitialiseCombobox;
  cmb_AppProfile.ItemIndex := 0;
  mainform.iCurrentRecIndex := 0;
  mainform.INI.WriteInteger('ProgramInfo', 'CurrentProfileIndex',
    mainform.iCurrentRecIndex);
  mainform.INI.UpdateFile;
  bDetailsChanged := True;
  mainform.DoWriteAppRecArrayIntoINI;
end;

procedure Tconfigform.cmd_NewAppProfileClick(Sender: TObject);
var
  szProfileName: string;
  iLastIndex: integer;
begin
  if InputQuery('New Profile', 'New Profile', False, szProfileName) then
  begin
    iLastIndex := High(mainform.AppRecArray);
    SetLength(mainform.AppRecArray, iLastIndex + 2);
    iLastIndex := High(mainform.AppRecArray);
    mainform.AppRecArray[iLastIndex] := mainform.AppRecArray[iLastIndex - 1];
    mainform.AppRecArray[iLastIndex].AppPrettyName := szProfileName;
    DoInitialiseCombobox;
    cmb_AppProfile.ItemIndex := iLastIndex;
    mainform.iCurrentRecIndex := iLastIndex;
    mainform.INI.WriteInteger('ProgramInfo', 'CurrentProfileIndex',
      mainform.iCurrentRecIndex);
    mainform.INI.UpdateFile;
    bDetailsChanged := True;
    mainform.DoWriteAppRecArrayIntoINI;
  end;
end;

procedure Tconfigform.cmb_IntervalTypeChange(Sender: TObject);
begin
  DoEnableDisableIntervalCombos;
end;

procedure Tconfigform.DoInitialiseCombobox;
var
  i: integer;
begin
  if (Length(mainform.AppRecArray) = 0) then
  begin
    ShowMessage('There are no profiles to configure!');
    ModalResult := mrOk;
  end;
  cmb_AppProfile.Clear;
  with mainform do
  begin
    for i := Low(AppRecArray) to High(AppRecArray) do
    begin
      cmb_AppProfile.Items.Add(AppRecArray[i].AppPrettyName);
    end;
  end;
  mainform.iCurrentRecIndex :=
    mainform.INI.ReadInteger('ProgramInfo', 'CurrentProfileIndex', 0);
  cmb_AppProfile.ItemIndex := mainform.iCurrentRecIndex;
  DoDisplayProfile(mainform.iCurrentRecIndex);
end;

procedure Tconfigform.FormShow(Sender: TObject);
begin
  DoInitialiseCombobox;
  bDetailsChanged := False;
end;

end.
