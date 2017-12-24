{ TAboutLazAutoUpdate and TAboutBox Component License

  Copyright (C) 2014 Gordon Bamber minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit aboutlazautoupdateunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, Forms, Graphics, LResources, SysUtils,
  ExtCtrls, StdCtrls, StrUtils, Buttons, PropEdits;

const
  C_DEFAULTLICENSEFORMWIDTH = 500;
  C_DEFAULTLICENSEFORMWIDTH_LINUX = C_DEFAULTLICENSEFORMWIDTH + 100;
  C_DEFAULTLICENSEFORMHEIGHT = 400;
  C_DEFAULTLICENSEFORMHEIGHT_LINUX = C_DEFAULTLICENSEFORMHEIGHT + 50;

type
  TLicenseType = (abNone, abGPL, abLGPL, abMIT, abModifiedGPL, abProprietry);

  TAboutbox = class(TComponent)
  private
    { Private declarations }
    fDialog: TForm;
    fBackgroundbitmap: TBitMap;
    fBackgroundResourceName: string;
    fDescription: TStrings;
    fDialogTitle, fVersion, fAuthorname, fAuthorEmail, fOrganisation,
    fComponentName: string;
    fDialogHeight, fDialogWidth: integer;
    fStretchBackground: boolean;
    fFont: TFont;
    fColor: TColor;
    fLicenseType: TLicenseType;
    procedure SetBackgroundBitmap(const AValue: TBitMap);
    procedure SetDescriptionStrings(const AValue: TStrings);
    procedure SetFont(const AValue: TFont);
    procedure ShowLicense(Sender: TObject);
    procedure SetDialogTitle(const AValue: string);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure ShowDialog;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    // Set these properties in your component Constructor method
    property BackGround: TBitMap read fBackgroundbitmap write SetBackgroundBitmap;
    property BackgroundResourceName: string
      read fBackgroundResourceName write fBackgroundResourceName;
    property Description: TStrings read fDescription write SetDescriptionStrings;
    property Title: string read fDialogTitle write SetDialogTitle;
    property Height: integer read fDialogHeight write fDialogHeight;
    property Width: integer read fDialogWidth write fDialogWidth;
    property Font: TFont read fFont write SetFont;
    property BackGroundColor: TColor read fColor write fColor;
    property StretchBackground: boolean read fStretchBackground
      write fStretchBackground default False;
    property Version: string read fVersion write fVersion;
    property Authorname: string read fAuthorname write fAuthorname;
    property Organisation: string read fOrganisation write fOrganisation;
    property AuthorEmail: string read fAuthorEmail write fAuthorEmail;
    property ComponentName: string read fComponentName write fComponentName;
    property LicenseType: TLicenseType read fLicenseType write fLicenseType;
  end;

  TAboutLazAutoUpdate = class(TComponent)
  private
    { Private declarations }
    fAboutBox: tAboutBox;
    procedure SetMyComponentName(const Avalue: string);
    procedure SetAboutBoxWidth(const AValue: integer);
    procedure SetAboutBoxHeight(const AValue: integer);
    procedure SetAboutBoxDescription(const AValue: string);
    procedure SetAboutBoxFontName(const AValue: string);
    procedure SetAboutBoxFontSize(const AValue: integer);
    procedure SetAboutBoxBitmap(const AValue: TBitmap);
    procedure SetAboutBoxBackgroundColor(const AValue: TColor);
    procedure SetAboutBoxTitle(const AValue: string);

    procedure SetAboutBoxVersion(const AValue: string);
    procedure SetAboutBoxAuthorname(const AValue: string);
    procedure SetAboutBoxOrganisation(const AValue: string);
    procedure SetAboutBoxAuthorEmail(const AValue: string);
    procedure SetAboutBoxBackgroundResourceName(const AValue: string);
    procedure SetAboutBoxLicenseType(const AValue: string);
    procedure SetAboutBoxStretchBackgroundImage(const AValue: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override; // Constructor must be public
    destructor Destroy; override; // Destructor must be public

    // Set these (hidden) properties in your inherited component's Create procedure
    property AboutBoxComponentName: string write SetMyComponentName;
    property AboutBoxWidth: integer write SetAboutBoxWidth;
    property AboutBoxHeight: integer write SetAboutBoxHeight;
    property AboutBoxDescription: string write SetAboutBoxDescription;
    property AboutBoxFontName: string write SetAboutBoxFontName;
    property AboutBoxFontSize: integer write SetAboutBoxFontSize;
    property AboutBoxBackgroundColor: TColor write SetAboutBoxBackgroundColor;
    property AboutBoxTitle: string write SetAboutBoxTitle;

    property AboutBoxVersion: string write SetAboutBoxVersion;
    property AboutBoxAuthorname: string write SetAboutBoxAuthorname;
    property AboutBoxOrganisation: string write SetAboutBoxOrganisation;
    property AboutBoxAuthorEmail: string write SetAboutBoxAuthorEmail;
    property AboutBoxLicenseType: string write SetAboutBoxLicenseType;
    property AboutBoxBackgroundResourceName: string
      write SetAboutBoxBackgroundResourceName;
    property AboutBoxStretchBackgroundImage: boolean
      write SetAboutBoxStretchBackgroundImage;
  published
    // The clickable 'About' property will automaticcally appear in any component
    // descended from TAboutLazAutoUpdate

    // About this component...
    property About: tAboutBox read fAboutBox write fAboutBox;
  end;

  // Declaration for the 'About' property editor
  TAboutPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

{For i8n if required}
resourcestring
  rs_Componentname = 'Component name';
  rs_About = 'About';
  rs_License = 'License';
  rs_By = 'By';
  rs_For = 'For';
  rs_DatafileMissing = 'Resource datafile license.lrs is missing';
  rs_LicenseTextError = 'There is something wrong with the Licence text';
  rs_AboutBoxError = 'Subcomponent TAboutBox Error';


implementation

{ TABoutBox}

constructor TAboutbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBackgroundbitmap := TBitMap.Create;
  fDescription := TStringList.Create;
  fFont := TFont.Create;
  fColor := clDefault;
  fLicenseType := abNone;
  fComponentName := rs_Componentname;
  fDialogTitle := rs_About + ' ' + fComponentName;

  fDialogWidth := 320;
  fDialogHeight := 280;
  fVersion := '1.0.0.1';
  fLicenseType := abNone;
end;

destructor TAboutbox.Destroy;
begin
  FreeAndNil(fFont);
  FreeAndNil(fDescription);
  FreeAndNil(fBackgroundbitmap);
  inherited Destroy;
end;

procedure TAboutbox.SetDialogTitle(const AValue: string);
begin
  if AnsiContainsText(fDialogTitle, rs_About) then
    fDialogTitle := AValue
  else
    fDialogTitle := rs_About + ' ' + Avalue;
end;

procedure TAboutbox.ShowDialog;
var
  OKbutton, LicenseButton: TBitBtn;
  lbl_Description: TLabel;
  img_BackGround: TImage;
  sz: string;
  iCount: integer;
  r: TLResource;
begin
  fDialog := TForm.CreateNew(nil);
    try  //.. finally FreeAndNil everything
    with fDialog do
      begin
      // Set Dialog properties
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fDialogTitle;
      formstyle := fsSystemStayOnTop;
      color := fColor;
      font := fFont;
      if (fDialogHeight > 0) then
        Height := fDialogHeight
      else
        Height := 240;
      if (fDialogWidth > 0) then
        Width := fDialogWidth
      else
        Width := 320;

      // Create a background image
      img_BackGround := Timage.Create(fDialog);
      with img_BackGround do
        // Set img_BackGround properties
        begin
        Align := alClient;
        Stretch := fStretchBackground;
        // Bitmap assigned?
        if Assigned(fBackgroundbitmap) then
          Picture.Assign(fBackgroundbitmap);
        // Resource file?
        r := LazarusResources.Find(fBackgroundResourceName);
        if r <> nil then
          img_BackGround.Picture.LoadFromLazarusResource(fBackgroundResourceName);
        SendToBack;
        parent := fDialog;
        end;

      // Create a BitBtn button
      okbutton := TBitBtn.Create(fDialog);
      // Set BitBtn properties
      with okButton do
        begin
        Kind := bkClose;
        left := (fDialog.Width div 2) - Width div 2;
        top := fDialog.Height - Height - 10;
        ParentFont := False;
        parent := fDialog;
        end;

      // Create a License Button
      LicenseButton := TBitBtn.Create(fDialog);
      if (fLicenseType <> abNone) then
        // Put it on the right
        begin
        LicenseButton.Top := OKButton.Top;
        LicenseButton.Caption := rs_License + '...';
        LicenseButton.left := Width - LicenseButton.Width - 10;
        LicenseButton.OnClick := @ShowLicense;
        LicenseButton.ParentFont := False;
        LicenseButton.Parent := fDialog;
        end;


      // Create a label control
      lbl_Description := Tlabel.Create(fDialog);
      // Set label properties
      with lbl_Description do
        begin
        left := 8;
        Top := 30;
        Width := fDialog.Width - 8;
        Height := fDialog.Height - 30;
        Autosize := False;
        ParentFont := True;
        Alignment := taCenter;
        end;

      // Build up Label text
      sz := '';
      // Component name
      if fComponentName <> '' then
        sz += fComponentName + LineEnding;
      // Author name (+Email)
      if fAuthorname <> '' then
        sz += rs_By + ': ' + fAuthorname + LineEnding;
      if fAuthorEmail <> '' then
        sz += ' (' + fAuthorEmail + ')' + LineEnding
      else
        sz += LineEnding;

      sz += LineEnding;

      // Version
      if fVersion <> '' then
        sz += 'Version: ' + fVersion + LineEnding;
      // License
      case fLicenseType of
        abGPL: sz += rs_License + ': GPL' + LineEnding;
        abLGPL: sz += rs_License + ': LGPL' + LineEnding;
        abMIT: sz += rs_License + ': M.I.T.' + LineEnding;
        abModifiedGPL: sz += rs_License + ': Modified GPL' + LineEnding;
        abProprietry: sz += rs_License + ': Proprietry' + LineEnding;
        end;
      if fOrganisation <> '' then
        sz += rs_For + ': ' + fOrganisation + LineEnding;
      if fDescription.Count > 0 then
        begin
        sz += LineEnding;
        for iCount := 1 to fDescription.Count do
          sz += fDescription[iCount - 1] + LineEnding;
        end;

      lbl_Description.Caption := sz;
      lbl_Description.parent := fDialog;
      // Display the dialog modally
      ShowModal;
      end;
    finally
    // Free all resources
    FreeAndNil(img_BackGround);
    FreeAndNil(lbl_Description);
    FreeAndNil(LicenseButton);
    FreeAndNil(okbutton);
    end;
end;

procedure TAboutbox.ShowLicense(Sender: TObject);
// Triggered by License button Click
var
  sLicenseString: string;
  theList: TStringList;
  f: integer;
  LicenceForm: TForm;
  lblText: TLabel;
  closebuttton: TBitBtn;
  r: TLResource;
  szLicenseFile: string;
begin
  // Quit early?
  if fLicenseType = abNone then
    Exit;

  // Set to resource name in license.lrs
  case fLicenseType of
    abNone: szLicenseFile := '';
    abGPL: szLicenseFile := 'gpl.txt';
    abLGPL: szLicenseFile := 'lgpl.txt';
    abMIT: szLicenseFile := 'mit.txt';
    abModifiedgpl: szLicenseFile := 'modifiedgpl.txt';
    end;


  // Use a string list to split the text file into lines
  theList := TStringList.Create;
  // Create a window, label and close button on-the-fly
  LicenceForm := TForm.Create(nil);
  lblText := TLabel.Create(LicenceForm);
  closebuttton := TBitBtn.Create(LicenceForm);
  // Load up the text into variable 'sLicenseString'
  sLicenseString := LineEnding + LineEnding + fComponentName + LineEnding;
    try
      try
        begin
        // Load license text from resource string
        r := LazarusResources.Find(szLicenseFile);
        if r = nil then
          raise Exception.Create(rs_DatafileMissing);
        thelist.Add(r.Value);
        for f := 0 to TheList.Count - 1 do
          sLicenseString += TheList[f] + LineEnding;
        end;
      except
      On e: Exception do
        MessageDlg(rs_AboutBoxError,
          rs_LicenseTextError, mtError, [mbOK], 0);
      end;

    // Replace boilerplate text if possible
    sLicenseString := AnsiReplaceText(sLicenseString, '<year>',
{$I %DATE%}
      );
    sLicenseString := AnsiReplaceText(sLicenseString, '<name of author>', fAuthorname);
    sLicenseString := AnsiReplaceText(sLicenseString, '<contact>',
      '(' + fAuthorEmail + ')');
    sLicenseString := AnsiReplaceText(sLicenseString, '<copyright holders>',
      fOrganisation);

    // Make up the form window and controls
    with LicenceForm do
      begin
      // Form
      {$IFDEF WINDOWS}
      // More compact GUI?
      Width := C_DEFAULTLICENSEFORMWIDTH;
      Height := C_DEFAULTLICENSEFORMHEIGHT;
      {$ELSE WINDOWS}
      Width := C_DEFAULTLICENSEFORMWIDTH_LINUX;
      Height := C_DEFAULTLICENSEFORMHEIGHT_LINUX;
      {$ENDIF}
      // autosize:=true;
      // If you enable autosize, the button placement goes awry!

      // The Modified GPL has an extra clause
      if (szLicenseFile = 'modifiedgpl.txt') or
        (Pos('As a special exception', sLicenseString) > 0) then
        Height := Height + 100;
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fComponentName + ': Licensing';
      formstyle := fsSystemStayOnTop;

      // Label
      lblText.Align := alClient;
      lblText.Alignment := taCenter;
      lblText.Caption := sLicenseString;
      lblText.Parent := LicenceForm;

      // Close Button
      closebuttton.Kind := bkClose;
      closebuttton.left := (Width div 2) - closebuttton.Width div 2;
      closebuttton.top := Height - closebuttton.Height - 10;
      closebuttton.parent := LicenceForm;
      // Show modally over the existing modal form
      PopupParent := TForm(Sender);
      ShowModal;
      end;
    finally
    // Free up all component created resources from memory
    FreeAndNil(theList);
    FreeAndNil(lblText);
    FreeAndNil(closebuttton);
    FreeAndNil(LicenceForm);
    end;
end;

procedure TAboutbox.SetBackgroundBitmap(const AValue: TBitMap);
begin
  if Assigned(AValue) then
    fBackgroundbitmap.Assign(AValue);
end;

procedure TAboutbox.SetDescriptionStrings(const AValue: TStrings);
begin
  if Assigned(AValue) then
    fDescription.Assign(Avalue);
end;

procedure TAboutbox.SetFont(const AValue: TFont);
begin
  if Assigned(AValue) then
    fFont.Assign(AValue);
end;

{ TAboutLazAutoUpdate }

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAboutbox),
    TAboutLazAutoUpdate, 'About', TAboutPropertyEditor);
end;

procedure TAboutPropertyEditor.Edit;
// Communicate with the component properties
var
  AAboutBox: TAboutBox;
begin
  AAboutBox := TAboutBox(GetObjectValue(TAboutBox));
  AABoutBox.ShowDialog;
end;

function TAboutPropertyEditor.GetAttributes: TPropertyAttributes;
  // Show the ellipsis
begin
  Result := [paDialog, paReadOnly];
end;

// Sets for AboutBox dialog properties
procedure TAboutLazAutoUpdate.SetMyComponentName(const Avalue: string);
begin
  fAboutBox.ComponentName := AValue;
  fAboutBox.Title := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxWidth(const AValue: integer);
begin
  fAboutBox.Width := Avalue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxHeight(const AValue: integer);
begin
  fAboutBox.Height := Avalue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxDescription(const AValue: string);
begin
  fAboutBox.Description.Clear;
  fAboutBox.Description.Add(AValue);
end;

procedure TAboutLazAutoUpdate.SetAboutBoxFontName(const AValue: string);
begin
  fAboutBox.Font.Name := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxFontSize(const AValue: integer);
begin
  if (AValue > 6) then
    fAboutBox.Font.Size := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxTitle(const AValue: string);
begin
  fAboutBox.Title := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxBitmap(const AValue: TBitmap);
begin
  if Assigned(Avalue) then
    fAboutBox.Assign(AValue);
end;

procedure TAboutLazAutoUpdate.SetAboutBoxBackgroundColor(const AValue: TColor);
begin
  fAboutBox.BackGroundColor := AValue;
  ;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxVersion(const AValue: string);
begin
  fAboutBox.Version := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxAuthorname(const AValue: string);
begin
  fAboutBox.Authorname := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxOrganisation(const AValue: string);
begin
  fAboutBox.Organisation := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxAuthorEmail(const AValue: string);
begin
  fAboutBox.AuthorEmail := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxBackgroundResourceName(const AValue: string);
begin
  fAboutBox.BackgroundResourceName := AValue;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxLicenseType(const AValue: string);
begin
  case Upcase(AValue) of
    'GPL': fAboutBox.LicenseType := abGPL;
    'LGPL': fAboutBox.LicenseType := abLGPL;
    'MIT': fAboutBox.LicenseType := abMIT;
    'MODIFIEDGPL': fAboutBox.LicenseType := abModifiedGPL;
    'PROPRIETRY': fAboutBox.LicenseType := abProprietry;
    else
      fAboutBox.LicenseType := abNone;
    end;
end;

procedure TAboutLazAutoUpdate.SetAboutBoxStretchBackgroundImage(const AValue: boolean);
begin
  fAboutBox.StretchBackground := AValue;
end;

// End Sets

constructor TAboutLazAutoUpdate.Create(AOwner: TComponent);
var
  TempImage: TPicture;
  r: TLResource;
begin
  // Inherit default properties
  inherited Create(AOwner);
  // Use tAboutBox as a subcomponent
  fAboutBox := tAboutBox.Create(nil);
  with fAboutBox do
    begin
    SetSubComponent(True);  // Tell the IDE to store the modified properties
    // Default of TAboutLazAutoUpdate values override TAbouBox.Create defaults
    ComponentName := 'TAboutLazAutoUpdate';
    Description.Add('This is to demonstrate'); //TStrings
    Description.Add('the use of TAboutLazAutoUpdate'); //TStrings
    Description.Add('Set its properties in your Constructor'); //TStrings
    Width := 320; //Integer
    Height := 280; //Integer
    // Set any Font properties or subproperties here
    // Font.Name := 'Arial';
    Font.Color := clNavy;
    Font.Size := 10;
    // BackGroundColor shows if no BackGround image is set
    BackGroundColor := clWindow;
    Version := '1.0.0.0';
    AuthorName := 'Gordon Bamber';
    AuthorEmail := 'minesadorada@charcodelvalle.com';
    Organisation := 'Public Domain';

    //Types available: abNone, abGPL, abLGPL, abMIT, abModifiedGPL, abProprietry
    LicenseType := abLGPL;

    // BackGround image is optional
    // It must be in a resouce file in the initialization section

    //== How to set a background image to your About dialog --
    // The BackGround property is a TBitmap
    // Use a Temporary TPicture to load a JPG.
    // NOTE a PNG file will create an error when your component is used in an application!
    r := LazarusResources.Find(fAboutBox.BackgroundResourceName);
    if r <> nil then
      begin
      TempImage := TPicture.Create;
      // .lrs file is in the initialization section
      TempImage.LoadFromLazarusResource(fAboutBox.BackgroundResourceName);
      BackGround.Assign(TempImage.Bitmap);
      TempImage.Free;
      StretchBackground := fAboutBox.StretchBackground; //Boolean
      end;
    end;
end;

destructor TAboutLazAutoUpdate.Destroy;
begin
  FreeAndNil(fAboutBox);
  inherited Destroy;
end;

initialization
{$I license.lrs}

end.
