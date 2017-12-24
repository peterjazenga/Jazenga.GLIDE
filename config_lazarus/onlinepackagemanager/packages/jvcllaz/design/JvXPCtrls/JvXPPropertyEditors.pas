{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPPropertyEditors.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvXPPropertyEditors.pas 10610 2006-05-19 13:35:08Z elahn $

{$MODE DELPHI}

unit JvXPPropertyEditors;

interface

uses
  Classes, SysUtils,
  PropEdits, ComponentEditors, LazarusPackageIntf,
  LCLIntf, LCLProc, LCLType, Contnrs, TypInfo,
  Forms, Graphics;

type

  TJvXPBarItemEditor = class(TDefaultEditor)
  private
    procedure RestoreDefaultColors;
    {procedure RestoreDefaultFonts;}
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  JvDsgnConsts, JvXPBar;

resourcestring
  RsItemEditorEllipsis = 'Item editor...';
  RsDefaultColorItem = 'Restore default colors';
  //RsDefaultFontsItem = 'Restore default fonts';

type
  TCustomWinXPBar = class(TJvXPCustomWinXPBar);


//=== { TJvXPBarItemEditor } =================================================

procedure TJvXPBarItemEditor.Edit;
var
  Hook: TPropertyEditorHook;
  TheXPBar: TJvXPBar;
begin
  GetHook(Hook);
  TheXPBar := GetComponent as TJvXPBar;
  EditCollection(TheXPBar, TheXPBar.Items, 'Items');
  if Assigned(Hook) then Hook.Modified(self);
end;

procedure TJvXPBarItemEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;                  // 'Item Editor...'
    1: RestoreDefaultColors;  // 'Restore Default Colors'
 //   2: RestoreDefaultFonts;   // 'Restore Default Fonts'
  end;
end;

function TJvXPBarItemEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := RsItemEditorEllipsis;
    1: Result := RsDefaultColorItem;
  //  2: Result := RsDefaultFontsItem;
  end;
end;

function TJvXPBarItemEditor.GetVerbCount: Integer;
begin
  Result := 2; //3;
end;

procedure TJvXPBarItemEditor.RestoreDefaultColors;
var
  def_colors: TJvXPBarColors;
begin
  def_colors := TJvXPBarColors.Create;
  try
    with TCustomWinXPBar(Component) do
    begin
      Colors.Assign(def_colors);
      Font.Color := dxColor_FontColorXP;
      HeaderFont.Color := dxColor_HeaderFontColorXP;
      Invalidate;
      if csDesigning in ComponentState then
        TCustomForm(Owner).Designer.Modified;
    end;
  finally
    def_colors.Free;
  end;
end;

{ Not working correctly }
{
procedure TJvXPBarItemEditor.RestoreDefaultFonts;
begin
  with TCustomWinXPBar(Component) do
  begin
    ParentFont := True;
    Invalidate;
    if csDesigning in ComponentState then
      TCustomForm(Owner).Designer.Modified;
  end;
end;
}

end.

