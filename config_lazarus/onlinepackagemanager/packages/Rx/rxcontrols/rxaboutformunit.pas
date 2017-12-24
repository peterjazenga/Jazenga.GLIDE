{ RxAboutForm

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

unit rxAboutFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ButtonPanel, RxVersInfo;

type

  { TrxAboutFormForm }

  TrxAboutFormForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Image1: TImage;
    lblAppTitle: TLabel;
    lblBuildDate: TLabel;
    lblFPCVersion: TLabel;
    lblLCLVersion: TLabel;
    lblTargCPU: TLabel;
    lblTargetOS: TLabel;
    lblVersion: TLabel;
    lblWidgetName: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    RxVersionInfo1: TRxVersionInfo;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure LoadLicense(AFileName:string);
  end; 

implementation
uses rxconst, LazFileUtils, LazUTF8;

{$R *.lfm}

{ TrxAboutFormForm }

procedure TrxAboutFormForm.FormCreate(Sender: TObject);
begin
  lblAppTitle.Caption:=RxVersionInfo1.ProductName;
  if lblAppTitle.Caption = '' then
    lblAppTitle.Caption:=Application.Title;

  PageControl1.ActivePageIndex:=0;
  Memo1.Text:='';
  lblWidgetName.Caption:=RxVersionInfo1.WidgetName;
  lblVersion.Caption:=sAppVersion + RxVersionInfo1.FileLongVersion;
  lblLCLVersion.Caption:=sLCLVersion + LCLVersion;
  lblFPCVersion.Caption:=sFpcVersion + {$I %FPCVERSION%};
  lblTargCPU.Caption:=sTargetCPU + {$I %FPCTARGETCPU%};
  lblTargetOS.Caption:=sTargetOS + {$I %FPCTARGETOS%};
  lblBuildDate.Caption:=sBuildDate + {$I %DATE%};
  TabSheet1.Caption:=sGeneral;
  TabSheet2.Caption:=sDetails;
  TabSheet3.Caption:=sLicense;
  Memo2.Color:=TabSheet1.Color;
end;


procedure TrxAboutFormForm.LoadLicense(AFileName: string);
begin
  if FileExistsUTF8(AFileName) then
    Memo1.Lines.LoadFromFile(UTF8ToSys(AFileName));
end;

end.

