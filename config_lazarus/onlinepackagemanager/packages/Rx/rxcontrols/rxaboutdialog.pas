{ RxAboutDialog

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
unit RxAboutDialog;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  TRxAboutDialogOption = (radHelpButton, radLicenseTab, radShowImageLogo);
  TRxAboutDialogOptions = set of TRxAboutDialogOption;
  { TRxAboutDialog }

  TRxAboutDialog = class(TComponent)
  private
    FAdditionalInfo: TStrings;
    FApplicationTitle: string;
    FCaption: string;
    FLicenseFileName: string;
    FOptions: TRxAboutDialogOptions;
    FPicture: TPicture;
    procedure SetAdditionalInfo(const AValue: TStrings);
    procedure SetPicture(const AValue: TPicture);
    procedure SetRxAboutDialogOptions(const AValue: TRxAboutDialogOptions);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property Options:TRxAboutDialogOptions read FOptions write SetRxAboutDialogOptions;
    property ApplicationTitle:string read FApplicationTitle write FApplicationTitle;
    property LicenseFileName:string read FLicenseFileName write FLicenseFileName;
    property Caption:string read FCaption write FCaption;
    property Picture: TPicture read FPicture write SetPicture;
    property AdditionalInfo:TStrings read FAdditionalInfo write SetAdditionalInfo;
  end;


implementation
uses rxAboutFormUnit, ButtonPanel, rxconst;

{ TRxAboutDialog }

procedure TRxAboutDialog.SetRxAboutDialogOptions(
  const AValue: TRxAboutDialogOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

procedure TRxAboutDialog.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  FPicture.Assign(AValue);
end;

procedure TRxAboutDialog.SetAdditionalInfo(const AValue: TStrings);
begin
  FAdditionalInfo.Assign(AValue);
end;

constructor TRxAboutDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FCaption:=sAbout;
  FAdditionalInfo:= TStringList.Create;
end;

destructor TRxAboutDialog.Destroy;
begin
  FAdditionalInfo.Free;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited Destroy;
end;

procedure TRxAboutDialog.Execute;
var
  rxAboutFormForm: TrxAboutFormForm;
begin
  rxAboutFormForm:=TrxAboutFormForm.Create(Application);
  rxAboutFormForm.Caption:=FCaption;
  if radLicenseTab in FOptions then
    rxAboutFormForm.LoadLicense(FLicenseFileName)
  else
    rxAboutFormForm.TabSheet3.TabVisible:=false;

  if radHelpButton in FOptions then
    rxAboutFormForm.ButtonPanel1.ShowButtons:=rxAboutFormForm.ButtonPanel1.ShowButtons + [pbHelp]
  else
    rxAboutFormForm.ButtonPanel1.ShowButtons:=rxAboutFormForm.ButtonPanel1.ShowButtons - [pbHelp];

  if FApplicationTitle <> '' then
    rxAboutFormForm.lblAppTitle.Caption:=FApplicationTitle;

  if radShowImageLogo in FOptions then
  begin
    rxAboutFormForm.Image1.Picture.Assign(Picture);
  end
  else
  begin
  end;

  rxAboutFormForm.Memo2.Lines.Assign(FAdditionalInfo);

  try
    rxAboutFormForm.ShowModal;
  finally
    rxAboutFormForm.Free;
  end;
end;

end.
