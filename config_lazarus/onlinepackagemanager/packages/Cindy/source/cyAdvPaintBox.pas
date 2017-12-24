{   Component(s):
    tcyAdvPaintBox

    Description:
    A PaintBox that paint a graphic (Bitmap, jpeg etc ...)

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}
unit cyAdvPaintBox;

{$MODE Delphi}

interface

uses cyClasses, cyPaintbox, LCLIntf, LCLType, LMessages, Themes, ExtCtrls, Graphics, classes, Messages, Controls;

type
  TcyAdvPaintBox = class(TcyCustomPaintBox)
  private
    FWallpaper: TcyBgPicture;
    procedure SetWallpaper(const Value: TcyBgPicture);
  protected
    procedure DrawBackground(aRect: TRect); override;
    procedure WallpaperChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    property Wallpaper: TcyBgPicture read FWallpaper write SetWallpaper;
    property Align;
    property Anchors;
//    property Color;    
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
//    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // Herited from TcyCustomPaintBox :
    property Degrade;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
  end;

implementation

uses Types;

constructor TcyAdvPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FWallpaper := TcyBgPicture.Create(self);
  FWallpaper.OnChange := WallpaperChanged;
end;

destructor TcyAdvPaintBox.Destroy;
begin
  FWallpaper.Free;
  inherited Destroy;
end;

procedure TcyAdvPaintBox.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TcyAdvPaintBox.DrawBackground(aRect: TRect);
begin
  Inherited;
  cyDrawBgPicture(Canvas, aRect, FWallpaper);
end;

procedure TcyAdvPaintBox.SetWallpaper(const Value: TcyBgPicture);
begin
  FWallpaper := Value;
end;

end.

