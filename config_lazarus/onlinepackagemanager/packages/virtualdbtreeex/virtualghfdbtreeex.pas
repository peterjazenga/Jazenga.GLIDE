{-----------------------------------------------------------}
{----Purpose : VT With Groupheader and Footer.              }
{    By      : Ir. G.W. van der Vegt                        }
{    For     : Fun                                          }
{    Module  : VirtualGHFStringTree.pas                     }
{    Depends : VT 3.2.1                                     }
{-----------------------------------------------------------}
{ ddmmyyyy comment                                          }
{ -------- -------------------------------------------------}
{ 05062002-Initial version.                                 }
{         -Footer Min/Maxwidth linked to VT Header.         }
{ 06062002-Implemented Min/MaxWdith width for groupheader.  }
{         -Set Fulldrag of THeadControls to False to prevent}
{          strange width problems when dragging exceeds     }
{          MaxWidth.                                        }
{         -Corrected some bugs.                             }
{         -Started on documentation.                        }
{-----------------------------------------------------------}
{ nr.    todo                                               }
{ ------ -------------------------------------------------- }
{ 1.     Scan for missing 3.2.1 properties.                 }
{-----------------------------------------------------------}

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{
@abstract(Extends a TVirtualStringTree with a GroupHeader and Footer Control. )
@author(G.W. van der Vegt <wvd_vegt@knoware.nl>)
@created(Juli 05, 2002)
@lastmod(Juli 06, 2002)
This unit contains a TVirtualStringTree Descendant that can
be linked to two THeaderControls that will act as a
GroupHeader and a Footer. The Component takes care of the
synchronized resizing of the columns, sections and controls.
<P>
Just drop a TVirtualGHFDBTreeEx and up to two
THeaderControls and link them to the TVirtualGHFDBTreeEx.
Then add columns to both THeaderControls and Columns to
the TVirtualGHFDBTreeEx's Header.
<P>
The Tag Value of the TVirtualGHFDBTreeEx Header Columns is
used to group the columns and link them to a GroupHeader's
Section.
}

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

unit virtualghfdbtreeex;

interface

uses
  {$IFDEF LCL}
    LCLProc, LCLType, LMessages,
  {$ELSE}
    Windows,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualDBTreeEx, VirtualTrees, StdCtrls, Comctrls;

type
  { This TVirtualStringTree Descendant allows one to
    attach a GroupHeader and or Footer to a
    TVirtualStringTree.
    <P>
    Both GroupHeader and Footer are THeaderControl's.
    TVirtualGHFDBTreeEx takes care of the synchronized
    resizing of all three components.
  }

  TVirtualGHFDBTreeEx = class(TVirtualDBTreeEx)
  private
    FGroupHeader: THeaderControl;
    FFooter: THeaderControl;
    function GetFooter: THeaderControl;
    function GetGroupHeader: THeaderControl;
    procedure SetFooter(const Value: THeaderControl);
    procedure SetGroupHeader(const Value: THeaderControl);
  protected
    { Description<P>
      Used for TreeOption in VirtualTreeView.        }
    function GetOptionsClass: TTreeOptionsClass; override;
    { Description<P>
      Sets the name of the Component and renames the GroupHeader
      and Footer controls accordingly. Do not call it directly
      but use the name property instead.
      <P>
      Parameters<P>
      NewName :   The new name of the Component. }
    procedure SetName(const NewName: TComponentName); override;

    { Description<P>
      Responds to Resizing the component by re-aligning
      the GroupHeader and Footer controls accordingly. Do
      not call directly.
      <P>
      Parameters<P>
      Message :   The WM_SIZE Message.        }
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    { Description<P>
      Responds to Moving the component by re-aligning
      the GroupHeader and Footer controls accordingly. Do
      not call directly.
      <P>
      Parameters<P>
      Message :   The WM_MOVE Message.        }
    procedure WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF}); message {$IFDEF FPC}LM_MOVE{$ELSE}WM_MOVE{$ENDIF};

    { Description<P>
      Called when the component's loading is finished. It
      will re-align the GroupHeader and Footer controls.
      Do not call directly. }
    procedure Loaded; override;

    { Description<P>
      Internally used to Resize the GroupHeader and Footer controls and
      update the MinWidth and Maxwith properties of both GroupHeader and Footer's
      Sections. Do not call directly. }
    procedure MyResize;

    { Description<P>
      Internally used to Resize the Columns and Sections.
      Do not call directly. }
    procedure ReAlignAll;

    { Description<P>
      Internally used to trap Column Resizing. Attached to
      the TVirtualStringTree's OnColumnResize Event.}
    procedure MyOnColumnResize(Sender: TVTHeader;
      Column: TColumnIndex);

    { Description<P>
      Internally used to trap Footer Section Resizing. Attached to
      the Footer's OnSectionTrack Event.}
    procedure MyOnFooterSectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; AWidth: Integer; State: TSectionTrackState);

    { Description<P>
      Internally used to trap Group Header Section Resizing. Attached to
      the GroupHeader's OnSectionTrack Event.}
    procedure MyOnGroupHeaderSectionTrack(HeaderControl: TCustomHeaderControl;
      Section: THeaderSection; AWidth: Integer; State: TSectionTrackState);
  public
    { Description<P>
      Standard Constructor.
      <P>
      Parameters<P>
      AOwner : The owning Component.  }
    constructor Create(AOwner: TComponent); override;

    { Description<P>
      Standard Destructor. }
    destructor Destroy; override;
  published
    { Get/Sets the Footer value of the TVirtualGHFDBTreeEx.
      The number of sections must be equal to the number of
      columns in the TVirtualGHFDBTreeEx. MinWidth and MaxWidth
      are derived from the Header Columns.
      <P>
      Note<P>
      The Footer is renamed on basis of the TVirtualGHFDBTreeEx's
      name to prevent problems with determining which THeaderControl belongs
      to which TVirtualGHFDBTreeEx.}
    property Footer: THeaderControl read GetFooter write SetFooter;

    { Get/Sets the GroupHeader value of the TVirtualGHFDBTreeEx.
      The Tag property of the HeaderColumn is used to determine
      which Header Columns belong to which GroupHeader Section.
      A group consists of adjacent Header Columns. The rightmost
      group(s) may be empty if their Indexes aren't used as the Tag Value
      in any Header Column. MinWidth and MaxWidth are derived from
      the Header Columns.
      <P>
      Note<P>
      The GroupHeader is renamed on basis of the TVirtualGHFDBTreeEx's
      name to prevent problems with determining which THeaderControl belongs
      to which TVirtualGHFDBTreeEx.}
    property GroupHeader: THeaderControl read GetGroupHeader write SetGroupHeader;

    //Default Properties:

  end;

implementation

function LineHeight(Canvas: TCanvas): Integer;
var
  tm           : {$IFDEF FPC}TLCLTextMetric{$ELSE}TEXTMETRIC{$ENDIF};
begin
  with Canvas do
    begin
      GetTextmetrics({$IFNDEF FPC}handle, {$ENDIF}tm);
      Result := {$IFDEF FPC}tm.Height + tm.Ascender{$ELSE}tm.tmHeight + tm.tmExternalLeading{$ENDIF};
    end;
end;

{ TVirtualGHFDBTreeEx }

constructor TVirtualGHFDBTreeEx.Create(AOwner: TComponent);
begin
  inherited;

//Preset some settings so it looks like a R/O Memo
  DefaultNodeHeight := 13;                                                      //Measured against a plain TMemo so it looks equal to it.
  Header.Height := 17;                                                          //Measured against a TListView;

  with TStringTreeOptions(TreeOptions) do
    begin
      PaintOptions := PaintOptions + [toShowRoot, toShowTreeLines, toShowButtons, toHideFocusRect];
      SelectionOptions := SelectionOptions + [toFullRowSelect];
    end;

  Parent := TWinControl(AOwner);
  Header.Options := Header.Options + [hoVisible] - [hoRestrictDrag, hoDrag];
  BorderWidth := 0;
  {$IFNDEF FPC}
  BevelKind := bkNone;
  {$ENDIF}
  BorderStyle := bsNone;

  FGroupHeader := nil;
  FFooter := nil;

  OnColumnResize := MyOnColumnResize;

  Header.Height := Round(2.6 * lineHeight(Canvas));                             //34

  Hint := '';
end;

destructor TVirtualGHFDBTreeEx.Destroy;
begin
{
  If Assigned(FGroupHeader) then
    begin
      FGroupHeader.Parent:=nil;
      FGroupHeader.Free;
    end;
  If Assigned(FFooter) then
    begin
      FFooter.Parent:=nil;
      FFooter.Free;
    end;
   }
  inherited;
end;

function TVirtualGHFDBTreeEx.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;

procedure TVirtualGHFDBTreeEx.MyResize;
begin
  if Assigned(FGroupHeader) then
    begin
      FGroupHeader.Left := Left;
      FGroupHeader.Width := Width;
      FGroupHeader.Height := Round(1.6 * lineHeight(FGroupHeader.Canvas));      //1.9 = 25, 1.8 = 23, was 1.6 = 21
      FGroupHeader.Top := Top - Round(1.6 * lineHeight(FGroupHeader.Canvas));
    end;

  if Assigned(FFooter) then
    begin
      FFooter.Left := Left;
      FFooter.Width := Width;
      FFooter.Top := Self.Top + Self.Height;
      FFooter.Height := Round(2.6 * lineHeight(FFooter.Canvas));                //34
    end;
end;

procedure TVirtualGHFDBTreeEx.MyOnColumnResize(Sender: TVTHeader;
  Column: TColumnIndex);
var
  ndx,
    w,
    i          : Integer;
begin
  ndx := Sender.Columns[Column].Tag;

  if not (assigned(GroupHeader)) or (ndx >= GroupHeader.Sections.Count) then Exit;

//Get Width of Group Header's columns
  w := 0;
  for i := 0 to Pred(Header.Columns.Count) do
    if (Sender.Columns[i].Tag = ndx) then Inc(w, Header.Columns[i].Width);
  GroupHeader.Sections[ndx].Width := w;
  GroupHeader.Update;

  if not (assigned(Footer)) or (Header.Columns.Count <> Footer.Sections.Count) then Exit;

  for i := 0 to Pred(Header.Columns.Count) do
    begin
      Footer.Sections[i].MinWidth := Header.Columns[i].MinWidth;
      Footer.Sections[i].MaxWidth := Header.Columns[i].MaxWidth;
      Footer.Sections[i].Width := Header.Columns[i].Width;
    end;

  Footer.Update;
end;

procedure TVirtualGHFDBTreeEx.ReAlignAll;
var
  i, j         : TColumnIndex;
  MinW, MaxW   : Integer;
begin
  if assigned(GroupHeader) then
    begin
    //Loop through the Groupheaders Columns to calculated the Total MinWidth and MaxWidth
      for i := 0 to Pred(GroupHeader.Sections.Count) do
        begin
          MinW := -1;
          MaxW := -1;

          for j := 0 to Pred(Header.Columns.Count) do
            if (i = Header.Columns[j].Tag) then
              begin
                Inc(MinW, Header.Columns[j].MinWidth);
                Inc(MaxW, Header.Columns[j].MaxWidth);
              end;

          if (MinW <> -1) and (MaxW <> -1) then
            begin
              GroupHeader.Sections[i].MinWidth := MinW + 1;
              GroupHeader.Sections[i].MaxWidth := MaxW + 1;
            end;
        end;
    end;

//Loop through the Header Columns to copy their MinWidth and MaxWidth to the Footer
  if Assigned(Footer) then
    begin
      Footer.Sections.BeginUpdate;
      for i := 0 to Pred(Header.Columns.Count) do
        begin
          Footer.Sections[i].MinWidth := Header.Columns[i].MinWidth;
          Footer.Sections[i].MaxWidth := Header.Columns[i].MaxWidth;
        end;
      Footer.Sections.EndUpdate;
    end;

//Resize Every Column.
  for i := 0 to Pred(Header.Columns.Count) do
    MyOnColumnResize(Header, i);
end;

procedure TVirtualGHFDBTreeEx.MyOnFooterSectionTrack(HeaderControl: TCustomHeaderControl;
  Section: THeaderSection; AWidth: Integer; State: TSectionTrackState);
begin
//Strange Effects when MaxWidth is Exceeded during Drag. Width seem to be Starting at zero again.
//  OutputDebugString(PChar(IntToStr(Width)));
  Header.Columns[Section.Index].Width := Width;
  HeaderControl.Repaint;
  MyOnColumnResize(Header, Section.Index);
end;

procedure TVirtualGHFDBTreeEx.MyOnGroupHeaderSectionTrack(HeaderControl: TCustomHeaderControl;
  Section: THeaderSection; AWidth: Integer; State: TSectionTrackState);
var
  d, i, gr, grw, sw: Integer;
  found        : Boolean;
  v, mid, sid  : Integer;
begin
//Strange Effects when MaxWidth is Exceeded during Drag. Width seem to be Starting at zero again.
  gr := Section.Index;

  found := False;
  grw := 0;
  for i := 0 to Pred(Header.Columns.Count) do
    if (Header.Columns[i].Tag = gr) then
      begin
        Inc(grw, Header.Columns[i].Width);
        Found := True;
      end;

  Section.Width := Width;

//Prevent Resizing when there are no columns for this GroupHeader Section.
  if not Found then exit;

//  OutputDebugString(PChar(IntToStr(Width) + '+' + IntToStr(grw) + '+' + IntToStr(gr)));

  sw := Width;
  d := Abs(grw - sw);

//Now loop and Increment either the smallest or Decrement the largest column
//until the sizes match.
  Header.Columns.BeginUpdate;
  repeat
    found := false;

    if (d > 0) then
      begin
        if (grw - sw) > 0 then
          begin
          //Find largest
            v := -1;
            mid := 0;
            for i := 0 to Pred(Header.Columns.Count) do
              if (Header.Columns[i].Tag = gr) and (v < Header.Columns[i].Width) then
                begin
                  v := Header.Columns[i].Width;
                  mid := i;
                end;

            if (Header.Columns[mid].Width > Header.Columns[mid].MinWidth) then
              begin
                Header.Columns[mid].Width := Header.Columns[mid].Width - 1;
                Dec(d);
                Dec(grw);
                found := True;
              end;
          end
        else
          begin
          //Find smallest
            v := maxint;
            sid := 0;
            for i := 0 to Pred(Header.Columns.Count) do
              if (Header.Columns[i].Tag = gr) and (v > Header.Columns[i].Width) then
                begin
                  v := Header.Columns[i].Width;
                  sid := i;
                end;

            if (Header.Columns[sid].Width < Header.Columns[sid].MaxWidth) then
              begin
                Header.Columns[sid].Width := Header.Columns[sid].Width + 1;
                Dec(d);
                Inc(grw);
                found := True;
              end;
          end;
      end
  until (d = 0) or not found;
  Header.Columns.EndUpdate;
  Update;

//Prevent Resizing when there's no Footer
  if not (assigned(Footer)) or (Header.Columns.Count <> Footer.Sections.Count) then Exit;

  Footer.Sections.BeginUpdate;
  for i := 0 to Pred(Header.Columns.Count) do
    Footer.Sections[i].Width := Header.Columns[i].Width;
  Footer.Sections.EndUpdate;
  Footer.Update;
end;

procedure TVirtualGHFDBTreeEx.SetName(const NewName: TComponentName);
begin
  inherited;

//Rename the headercontrols so we can see to who they belong.
//Makes it ieasier to find the newly dropped ones with their default names.
  if Assigned(FGroupHeader) then FGroupHeader.Name := NewName + '_GroupHeader';
  if Assigned(FFooter) then FFooter.Name := NewName + '_Footer';

//Update after the components name is changed won't hurt.
  MyResize;
  ReAlignAll;
end;

procedure TVirtualGHFDBTreeEx.Loaded;
begin
  inherited;

//We need an update after the component is loaded to align everything.
  MyResize;
  ReAlignAll;
end;

function TVirtualGHFDBTreeEx.GetGroupHeader: THeaderControl;
begin
  Result := FGroupHeader;
end;

procedure TVirtualGHFDBTreeEx.SetGroupHeader(
  const Value: THeaderControl);
begin
  if Assigned(Value) then
    begin
    //Make sure to change some properties so we don't get problems.
      FGroupHeader := Value;
      FGroupHeader.Align := alNone;
      FGroupHeader.DoubleBuffered := True;

    //Prevent Strange Effects when MaxWidth is Exceeded during Drag. Width seem to be Starting at zero again.
      {$IFNDEF FPC}
      FGroupHeader.FullDrag := False;
      {$ENDIF}
      FGroupHeader.OnSectionTrack := MyOnGroupHeaderSectionTrack;

    //Rename the headercontrol so we can see to who they belong.
    //Makes it easier to find the newly dropped ones with their default names.
      FGroupHeader.Name := Name + '_GroupHeader';

    //Re-arrange all when adding or removing a header
      MyResize;
      ReAlignAll;
    end
  else
    begin
      FGroupHeader.OnSectionTrack := nil;
      FGroupHeader := Value;
    end;
end;

function TVirtualGHFDBTreeEx.GetFooter: THeaderControl;
begin
  Result := FFooter;
end;

procedure TVirtualGHFDBTreeEx.SetFooter(const Value: THeaderControl);
begin
  if Assigned(Value) then
    begin
    //Make sure to change some properties so we don't get problems.
      FFooter := Value;
      FFooter.Align := alNone;
      FFooter.DoubleBuffered := True;

    //Strange Effects when MaxWidth is Exceeded during Drag. Width seem to be Starting at zero again.
      {$IFNDEF FPC}
      FGroupHeader.FullDrag := False;
      {$ENDIF}
      FFooter.OnSectionTrack := MyOnFooterSectionTrack;

    //Rename the headercontrols so we can see to who they belong.
    //Makes it easier to find the newly dropped ones with their default names.
      FFooter.Name := Name + '_Footer';

    //Re-arrange all when adding or removing a footer
      MyResize;
      ReAlignAll;
    end
  else
    begin
      FFooter.OnSectionTrack := nil;
      FFooter := Value;
    end;
end;

procedure TVirtualGHFDBTreeEx.WMMove(var Message: {$IFDEF FPC}TLMMove{$ELSE}TWMMove{$ENDIF});
begin
  inherited;

//Re-arrange all when moving the component.
  MyResize;
  ReAlignAll;
end;

procedure TVirtualGHFDBTreeEx.WMSize(var Message: TWMSize);
begin
  inherited;

//Re-arrange all when sizing the component.
  MyResize;
  ReAlignAll;
end;

end.
