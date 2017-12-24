unit umemoform;

{ LazAutoUpdate Pack system

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

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
  SysUtils, LazUTF8,LazFileUtils,FileUtil, Forms, Dialogs, StdCtrls,
  Buttons;

type
  TMemoAction = (maShowWhatsNew, maShowCode);
  { TMemoForm }

  TMemoForm = class(TForm)
    cmd_Close: TBitBtn;
    memo_whatsnew: TMemo;
    cmd_SaveAsAndClose: TBitBtn;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure cmd_CloseClick(Sender: TObject);
    procedure cmd_SaveAsAndCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    szWhatsNewPath, szCodeText: string;
    MemoAction: TMemoAction;
  end;

var
  MemoForm: TMemoForm;

implementation

uses umainform;

{$R *.lfm}

{ TMemoForm }

procedure TMemoForm.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
end;

procedure TMemoForm.cmd_CloseClick(Sender: TObject);
begin
  case MemoAction of
    maShowWhatsNew:
      memo_whatsnew.Lines.SaveToFile(szWhatsNewPath);
    maShowCode:
      begin
      memo_whatsnew.Lines.SaveToFile(mainform.ProfileRec.ProfileName + '.txt');
      ShowMessageFmt('Code saved as %s', [ProgramDirectory +
        mainform.ProfileRec.ProfileName + '.txt']);
      end;
    end;
end;

procedure TMemoForm.cmd_SaveAsAndCloseClick(Sender: TObject);
var
  sz: string;
begin
  case MemoAction of
    maShowWhatsNew:
      begin
      SelectDirectoryDialog1.InitialDir := ExtractFileDir(szWhatsNewPath);
      if SelectDirectoryDialog1.Execute then
        szWhatsNewPath := AppendPathDelim(SelectDirectoryDialog1.Filename) +
          'whatsnew.txt';
      memo_whatsnew.Lines.SaveToFile(szWhatsNewPath);
      end;
    maShowCode:
      begin
      SelectDirectoryDialog1.InitialDir := mainform.ProfileRec.OutDir;
      if SelectDirectoryDialog1.Execute then
        begin
        sz := AppendPathDelim(SelectDirectoryDialog1.Filename) +
          mainform.ProfileRec.ProfileName + '.txt';
        memo_whatsnew.Lines.SaveToFile(sz);
        ShowMessageFmt('Code saved as %s', [sz]);
        end;
      end;
    end;
end;

procedure TMemoForm.FormShow(Sender: TObject);
begin
  memo_whatsnew.Lines.Clear;
  case MemoAction of
    maShowWhatsNew:
      begin
      if FileExistsUTF8(szWhatsNewPath) then
        memo_whatsnew.Lines.LoadFromFile(szWhatsNewPath);
      Caption := 'Contents of ' + szWhatsNewPath;
      end;
    maShowCode:
      begin
      memo_whatsnew.Clear;
      memo_whatsnew.Append(szCodeText);
      // memo_whatsnew.Lines.AddText(szCodeText);
      Caption := 'Code';
      end;
    end;
end;

end.
