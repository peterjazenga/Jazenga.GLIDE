unit U_ExtImages;

interface

{$i ..\extends.inc}
{$IFDEF FPC}
{$Mode Delphi}
{$ENDIF}

uses
{$IFDEF VERSIONS}
  fonctions_version,
{$ENDIF}
  Classes, U_ExtMapImageIndex,
  U_ExtImage, U_ExtCustomImages,
  u_framework_components,
  Controls,
  IniFiles,
{$IFDEF TNT}
     TntExtCtrls, TntStdCtrls,
{$ELSE}
     ExtCtrls, StdCtrls,
{$ENDIF}
{$IFDEF FPC}
  lmessages,
{$ENDIF}
  u_extcomponent;

const
  CST_DEFAULT_PANEL_WIDTH = 150;
  CST_DEFAULT_PANEL_HEIGHT = 100;
  CST_DEFAULT_PER_LINE = 4;
  CST_INI_DIRECTORY = 'Dir';
  CST_INI_EXT_OUT   = 'Out';
  CST_INI_MEMOS     = 'Memos';
{$IFDEF VERSIONS}
  gVer_TExtImages : T_Version = ( Component : 'Composant TExtImages' ;
                                  FileUnit : 'U_ExtImages' ;
                                  Owner : 'Matthieu Giroux' ;
                                  Comment : 'Présentation d''images de tous types avec résumé intégré dans le répertoire.' ;
                                  BugsStory : '1.0.1.0 : Scrolling and working. SubDirs working.' + #13#10 +
                                              '1.0.0.0 : Scrolling and working. SubDirs not working.' + #13#10 +
                                              '0.9.0.0 : Creating from TExtImage.';
                                  UnitType : 3 ;
                                  Major : 1 ; Minor : 0 ; Release : 1 ; Build : 0 );

{$ENDIF}

type
  TExtPanelImage = class;
  TExtCollectionImages = class;

 { TExtMapImageIndex }

  { TExtPanelImage }

  TExtPanelImage = class(TExtCustomPanelImage)
  private
   s_Path : String;
   FImageForm : TExtImage;
  protected
    procedure OnClickImage ( Sender : TObject ); override;
    procedure EraseMemo; override;
  public
   property Path : String read s_Path write s_Path;
  End;

  TExtPanelImageClass = class of TExtPanelImage;

  { TExtCollectionImages }

  TExtCollectionImages = class(TExtCustomCollectionImages)
  private
  public
  End;

{ TExtImages }

   TExtImages = class( TExtCustomImages )
     private
      FLastToUse : Boolean;
      FSubDirs ,
      FWorking,
      FAutoLoad,
      FShowErrors : Boolean ;
      FErasedPos : Integer;
      FDirectories : TStringList;
      FCurrentFile,
      FDirectory : String;
      FIniFileMemos : TIniFile;
      FErasedImages : TStringList;
      procedure p_SetSubDirs ( const Value : Boolean );
     protected
       FGoingToDirectory : Boolean;
       procedure SaveMemoToIni(const ai_memo: Integer); virtual;
       procedure LoadMemoIni ( const ai_memo : Integer ; const as_ext : String); virtual;
       procedure AddDownPanel; override;
       function CreateImagesMap : TExtCustomCollectionImages; override;
       procedure ReSizeImages(var Message: TLMSize); override;
       procedure AddDirectory(const as_Directory: String; ab_SubDirs: Boolean); virtual;
       procedure GotoDirectory(const as_Directory,as_DirectoryToGo: String; ab_SubDirs: Boolean); virtual;
       procedure p_SetDirectory ( const as_value : String ); virtual;
       procedure AddFile (  const as_SourceFile : String ;const ab_Top : Boolean =False); virtual;
       function  DeleteImagesTopPanel:Boolean; override;
       procedure DeleteImagesDownPanel; override;
       function  DeleteLinesTopPanel:Boolean; override;
       procedure DeleteLinesDownPanel; override;
       function  AddImagesDownPanel:Boolean; override;
       function  AddLinesTopPanel:Boolean; override;
       function  NotAddLines:Boolean; virtual;
       procedure SetVisible(Value: Boolean); override;
      public
       constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
       procedure CreateSomeLine; override;
       procedure DestroyWnd; override;
       procedure Loaded; override;
       procedure DeleteIniMemos; virtual;
       procedure DeleteIni; virtual;
       procedure DeleteFromIni; virtual;
       procedure ShowDirectory; virtual;
       procedure DestroyComponents; override;
       property IniFileMemos : TIniFile read FIniFileMemos write FIniFileMemos;
       property Directories : TStringList read FDirectories;
       property ErasedImages : TStringList read FErasedImages;
       property CurrentFile : String read FCurrentFile;
     published
      property AutoLoad : Boolean read FAutoLoad write FAutoLoad default True ;
      property SubDirs : Boolean read FSubDirs write p_SetSubDirs default True ;
      property Directory : String read FDirectory write p_SetDirectory ;
      property ShowErrors : Boolean read FShowErrors write FShowErrors default True ;
     end;


implementation

uses
{$IFDEF FPC}
    unite_messages,
    LazFileUtils,
{$ELSE}
     unite_messages_delphi,
{$ENDIF}
     fonctions_ini,
     Dialogs,
     fonctions_string,
     fonctions_system,
     sysutils,
     StrUtils,
     fonctions_file;

{ TExtPanelImage }

// procedure TExtPanelImage.OnClickImage
// Shows a form with image inside
procedure TExtPanelImage.OnClickImage(Sender: TObject);
begin
  inherited OnClickImage(Sender);
  FImageForm := TExtImage.Create(Form);
  FImageForm.Parent := Form;
  FImageForm.Align  := alClient;
  FImageForm.Stretch:=true;
  FImageForm.Proportional:=true;
  FImageForm.LoadFromFile(s_Path);
  Form.Show;
end;

// procedure TExtPanelImage.EraseMemo;
// delete memo from ini
procedure TExtPanelImage.EraseMemo;
begin
  inherited;
  with (Collection as TExtCustomCollectionImages).Component as TExtImages do
   if IniFileActive
   and assigned (FIniFileMemos) Then
     FIniFileMemos.EraseSection(copy(s_Path,1,posex('.',s_Path,length(s_path)-6)-1));
end;

{ TExtImages }

// constructor TExtImages.Create
// initing SubDirs
constructor TExtImages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDirectories :=TStringList.Create;
  FErasedImages:=TStringList.Create;
  FSubDirs:=True;
  FLastToUse := False;
  FIniFileMemos := nil;
  FShowErrors:=True;
  FAutoLoad:=True;
  FErasedPos := 0;
  FGoingToDirectory := False;
  FWorking := False;
end;

// destroy
// destroying
destructor TExtImages.Destroy;
begin
  inherited Destroy;
  FIniFileMemos.Free;
  FDirectories .Destroy;
  FErasedImages.Destroy;
end;

// procedure TExtImages.SaveMemoToIni
// save a memo to ini
procedure TExtImages.SaveMemoToIni ( const ai_memo : Integer );
Begin
  with Columns [ ai_memo ] as TExtPanelImage do
    if Trim(Memo.Lines.Text) > '' Then
      SauveTStringsDansIni ( FIniFileMemos, copy(s_Path,1,posex('.',s_Path,length(s_path)-6)-1), Memo.Lines );
end;

// procedure TExtImages.DestroyWnd;
// saving on destroy
procedure TExtImages.DestroyWnd;
var li_i : Integer;
begin
  if IniFileActive Then
    Begin
      f_GetMemIniFile();
      if Assigned(FIniFileMemos) Then
        Begin
          // Erasing all before adding all
         for li_i := 0 to Columns.Count - 1 do
          with Columns [ li_i ] as TExtPanelImage do
            FIniFileMemos.EraseSection(copy(s_Path,1,posex('.',s_Path,length(s_path)-6)-1));

         // adding all without extension saving
         for li_i := 0 to Columns.Count - 1 do
          SaveMemoToIni ( li_i );

        end;
      // fonction_init.FIniFile saving
      p_IniWriteSectionStr(Owner.Name,Name+CST_INI_DIRECTORY,FDirectory);
      // Saving
      fb_iniWriteFile( FIniFile );
      fb_iniWriteFile( FIniFileMemos );
    end;
  inherited;
end;

// procedure TExtImages.Loaded;
// Loads the directory
procedure TExtImages.Loaded;
begin
  if IniFileActive Then
   Begin
     f_GetMemIniFile();
     FIniFileMemos.Free;
     FIniFileMemos := TInifile.create ( fs_getIniDir () + fs_GetSoftName () + CST_INI_MEMOS + CST_EXTENSION_INI);
   end;
  inherited Loaded;
  if IniFileActive Then
    Begin
      FDirectory := f_IniReadSectionStr(Owner.Name,Name+CST_INI_DIRECTORY,FDirectory);
    end;
  if not ( csDesigning in ComponentState)
  and ( FDirectory > '' )
  and FAutoLoad then
    ShowDirectory;
end;


// procedure TExtImages.DeleteIniMemos
// delete memos ini file
procedure TExtImages.DeleteIniMemos;
var LSIniFile : String;
Begin
  LSIniFile := fs_getIniDir () + fs_GetSoftName () + CST_INI_MEMOS + CST_EXTENSION_INI;
  if FileExistsUTF8(LSIniFile)
   Then DeleteFileUTF8(LSIniFile);
end;

// procedure TExtImages.DeleteIni;
// delete ini file
procedure TExtImages.DeleteIni;
var LSIniFile : String;
Begin
  LSIniFile := f_getUserIniPath;
  if FileExistsUTF8(LSIniFile)
   Then DeleteFileUTF8(LSIniFile);
end;

//procedure TExtImages.DeleteFromIni;
// delete component ini section
procedure TExtImages.DeleteFromIni;
begin
  with f_GetMemIniFile do
    Begin
      EraseSection(Owner.Name+'.'+Name+CST_INI_EXT_OUT);
      EraseSection(Owner.Name);
    end;
end;

// procedure TExtImages.AddDirectory
// Recursive load directory
procedure TExtImages.AddDirectory(const as_Directory: String; ab_SubDirs : Boolean );
var lstl_Files : TStringList;
    li_i : Integer;
begin
  lstl_Files := TStringList.Create;
  try
    if fb_FindFiles ( lstl_Files, as_Directory, True, True, True, CST_FILTER_ALL ) Then
      Begin
       lstl_Files.Sort;
       with Columns do
        for li_i := 0 to lstl_Files.count-1 do
         try
            if NotAddLines
             Then Break;
            if FSubDirs and DirectoryExistsUTF8 ( lstl_Files [ li_i ] ) Then
             AddDirectory ( lstl_Files [ li_i ], ab_SubDirs )
            Else
             if not FGoingToDirectory then
              Begin
               AddFile(lstl_Files [ li_i ]);
               FCurrentFile := lstl_Files [ li_i ];
              end;
         finally
         end;

      end;
  finally
    lstl_Files.Destroy;
  end;
end;

// procedure TExtImages.GotoDirectory
// Find a file and reload directory
procedure TExtImages.GotoDirectory(const as_Directory,
  as_DirectoryToGo: String; ab_SubDirs: Boolean);
var lstl_Files : TStringList;
    ls_dirname : String;
    li_i : Integer;
begin
  lstl_Files := TStringList.Create;
  FGoingToDirectory := True;
  try
  if (length(as_Directory) <= length ( as_DirectoryToGo )) Then
   Begin
    if fb_FindFiles ( lstl_Files, as_Directory, True, length(as_Directory) < length ( as_DirectoryToGo ), False, CST_FILTER_ALL ) Then
     Begin
      lstl_Files.Sort;
      if posex ( DirectorySeparator,  as_DirectoryToGo, length ( as_Directory ) + 1 ) > 0
       Then ls_dirname := copy ( as_DirectoryToGo, length ( as_Directory ) + 1, posex ( DirectorySeparator,  as_DirectoryToGo, length ( as_Directory ) + 1 ) - length ( as_Directory ))
       Else  ls_dirname := copy ( as_DirectoryToGo, length ( as_Directory ) + 1,  length ( as_DirectoryToGo ) - length ( as_Directory ));
      with Columns do
       for li_i := 0 to lstl_Files.count-1 do
        if DirectoryExistsUTF8(as_directory+lstl_Files [ li_i ])
        Then
          Begin
           if lstl_Files [ li_i ] = ls_dirname Then
            try
               if FSubDirs and DirectoryExistsUTF8 (  as_directory + lstl_Files [ li_i ] ) Then
                Begin
                  if FGoingToDirectory
                   Then GotoDirectory( as_directory + lstl_Files [ li_i ] + DirectorySeparator, as_DirectoryToGo, ab_SubDirs)
                   Else AddDirectory ( as_directory + lstl_Files [ li_i ] + DirectorySeparator, ab_SubDirs );
                end;
            finally
            end;
          end
       else if not FGoingToDirectory Then
           Begin
             AddFile ( as_directory + lstl_Files [ li_i ]);
             FCurrentFile := as_directory + lstl_Files [ li_i ];
             if NotAddLines Then
                Exit;
           end
        Else
          if ( as_directory + lstl_Files [ li_i ] = FCurrentFile ) Then
           FGoingToDirectory := False;
     end
    Else
     if length ( as_Directory ) = length ( as_DirectoryToGo ) Then
      FGoingToDirectory := False;
     if as_Directory < as_DirectoryToGo Then
      Begin
       li_i := posex(DirectorySeparator,as_DirectoryToGo,Length(as_Directory)+1);
       if posex(DirectorySeparator,as_DirectoryToGo,li_i+1) > 0
        Then GotoDirectory(as_Directory+copy(as_DirectoryToGo,posex(DirectorySeparator,as_DirectoryToGo,Length(as_Directory)+1), posex(DirectorySeparator,as_DirectoryToGo,li_i+1)),as_DirectoryToGo,ab_SubDirs)
        Else GotoDirectory(as_DirectoryToGo,as_DirectoryToGo,ab_SubDirs);
      End;
   end;
  finally
    lstl_Files.Destroy;
    FGoingToDirectory := False;
  end;
end;

// procedure TExtImages.ShowDirectory;
// Destroy and load directory
procedure TExtImages.ShowDirectory;
var lstl_Files : TStringList;
    li_i : Integer;

begin
  if FWorking
  or not Visible
   Then
    Exit;
  try
    FWorking := True;
    DestroyComponents;
    AddDirectory(IncludeTrailingPathDelimiter(FDirectory),FSubDirs);
  finally
    FWorking := False;
  end;
end;

// procedure TExtImages.DestroyComponents
// Reinit at destroying
procedure TExtImages.DestroyComponents;
begin
  inherited;
  FLastToUse := False;
end;

// procedure TExtImages.SetVisible
// Loads on Visible at true
procedure TExtImages.SetVisible(Value: Boolean);
begin
  inherited;
  if  not ( csLoading in ComponentState )
  and Visible and FAutoLoad Then
   ShowDirectory;
end;

// procedure TExtImages.p_SetDirectory
// Set and load Directory
procedure TExtImages.p_SetDirectory(const as_value: String);
begin
  if  ( FDirectory <> as_value ) Then
    Begin
      FDirectory:=as_value;
      if (FDirectory > '') and FAutoLoad Then
        ShowDirectory;
    end;
end;

// procedure TExtImages.LoadMemoIni
// loads a memo from ini
procedure TExtImages.LoadMemoIni ( const ai_memo : Integer ; const as_ext : String );
Begin
  with Columns [ ai_memo ] as TExtPanelImage do
    LitTstringsDeIni ( FIniFileMemos, copy(s_Path,1,posex(as_ext,s_Path,length(s_Path)-6)-1), Memo.Lines );
end;

// Add an image file to columns and panels
procedure TExtImages.AddFile(const as_SourceFile: String;const ab_Top : Boolean =False);
var li_iposext : Integer;
    ls_ext : String;
    lb_continue : boolean;
Begin
  if not fileexistsUTF8 ( as_SourceFile ) Then
   Exit;
  // unused extensions verify
  if not IsValidFile ( as_SourceFile ) Then
    Exit;
  // must keep li_iposext until except
  li_iposext := posex ( '.', as_SourceFile, Length(as_SourceFile)-6);
  if li_iposext = 0 Then
   Exit;
  ls_ext := copy ( as_SourceFile, li_iposext, Length(as_SourceFile) - li_iposext + 1 );
//  ShowMessage(as_SourceFile);
  Begin
    // do we need to create a column ?
    if not FLastToUse Then
     Columns.Add;
    with Columns, Items [ Count - 1 ] as TExtPanelImage do
      try
         // Adding or not adding : That is the question
        Image.ShowErrors:=False;
        Image.LoadFromFile ( as_SourceFile );
        If ab_Top
         Then Panel.Left := 0
         Else Panel.Left := CounterPerLine*PanelWidth+5;

        If Image.Picture.Bitmap.Empty Then
          // adding for next
           FLastToUse := True
         Else
          // Adding really
           Begin
             FLastToUse := False;
             s_Path := as_SourceFile;
             if IniFileActive
             and assigned(FIniFileMemos)
              Then
               LoadMemoIni ( Count - 1, ls_ext );
           end;

      Except
        On E:Exception do
         Begin
          FLastToUse := True;
         end;
      end;
  end;
end;

// procedure DeleteImagesDown
// Have got ErasedFiles to Delete when down
// When there is one line only
function TExtImages.DeleteImagesTopPanel:Boolean;
begin
  if  ( Columns.Count > VisibleLines )  Then
   Begin
    Result:=True;
    FErasedImages.Add((Columns [0] as TExtPanelImage).Path);
    Columns.Delete(0);
    Inherited; // Have Really deleted
   end
  Else
   Result:=False;
end;

// procedure DeleteImagesUp
// Have got Directories and CurrentFile to Delete when up
// When there is one line only
procedure TExtImages.DeleteImagesDownPanel;
begin
  if  ( Columns.Count > VisibleLines )  Then
   Begin
    Columns.Delete(Columns.Count-1);
    Inherited; // Have Really deleted
   end;
end;

// procedure DeleteLinesDown
// Have got ErasedFiles to Delete when down
// Deleting one line of lines only
function TExtImages.DeleteLinesTopPanel:Boolean;
var li_i : Integer;
    LParent : TWinControl;
begin
  with Columns do
  if (Count > 0)
  and (  ((ImagesPerLine > 0 ) and (Count/ImagesPerLine*PanelHeight>Self.Height+PanelHeight*VisibleLines))
      or ((ImagesPerLine = 0 ) and (Count*PanelWidth >Self.Width +PanelWidth *VisibleLines)))
  Then
   Begin
    // Testing if can delete lines
    Result := True;
    /// deleting
      with Items [0] do
       Begin
         // Panel of the line
         LParent := Panel.Parent;
         //Deleting Panel of line
         for li_i := 0 to Count - 1 do
          with Items [0] as TExtPanelImage do
          Begin
            // Deleting one line only
            if LParent <> Panel.Parent
             Then Break;
            // Adding erased image to future added image
            FErasedImages.Add(Path);
            if IniFileActive Then
              SaveMemoToIni ( 0 );
            Delete(0);// deleting and destroying
            // Increasing and decreasing counters
            Counter:=Counter-1;
          end;
         // Destroying panel of line
         LParent.Destroy;
         Inherited; // Have Really deleted
       end;
   end
  Else
   Result := False;
end;

// procedure DeleteLinesUp
// Have got Directories and CurrentFile to Delete when up
// Deleting one line of lines only
procedure TExtImages.DeleteLinesDownPanel;
var li_i : Integer;
    LParent : TWinControl;
begin
  with Columns do
  // Testing if should delete lines
   if Count > VisibleLines * ImagesPerLine Then
    with Items [Count - 1] do
     Begin
       // Panel of the line
       LParent := Panel.Parent;
       /// deleting
       while Count > 1 do
        with Items [Count - 1] as TExtPanelImage do
        Begin
          // Deleting one line only
          if LParent <> Panel.Parent
           Then
             Break;
          // The next file will be the erased image
          if ExtractFileDir(FCurrentFile)<>ExtractFileDir((Items[Count - 2] as TExtPanelImage).s_Path)
           Then // Not working fully
            with FDirectories do
             Begin
               li_i := IndexOf(ExtractFileDir(FCurrentFile));
               if li_i > -1 Then
                Delete(li_i);
             end;
          if IniFileActive Then
            SaveMemoToIni ( Count - 1 );
          Delete(Count - 1);

        // Decreasing counter
          Counter:=Counter-1;
        end;
       if Count > 0
        Then FCurrentFile := (Items [Count - 1] as TExtPanelImage).s_Path
        Else FCurrentFile := '';
       // Will add a line next
       CounterPerLine:=ImagesPerLine;
       // Destroying panel of line
       LParent.Destroy;
       Inherited; // Have Really deleted
     end;
end;

// procedure TExtImages.p_SetSubDirs
// autoload on setting subdirs property
procedure TExtImages.p_SetSubDirs(const Value: Boolean);
begin
  if FSubDirs <> Value Then
   Begin
    FSubDirs := Value;
    if FAutoLoad Then
      ShowDirectory;
   end;
end;

// procedure TExtImages.ReSizeImages
// inherited method from CustomImages.
procedure TExtImages.ReSizeImages(var Message: TLMSize);
begin
  if not ( csDesigning in ComponentState)
  and ( FDirectory > '' )
  and FAutoLoad
  and not FWorking Then
   Inherited;
end;

// procedure TExtImages.AddDownPanel
// adding at bottom
procedure TExtImages.AddDownPanel;
var li_i : Integer;
    li_count : Integer;
begin
  with Columns do
   Begin
    li_count := FErasedImages.Count-1;
    for li_i := li_Count downto li_Count-ImagesPerLine+1 do
     Begin
      if li_i = -1 then Break;
      AddFile(FErasedImages[li_i]);
      FErasedImages.Delete(li_i);
     end;
    if Count > 0 Then
      PanelLine:=Items[Count-1].Panel.Parent as TPanel;
   end;
End;

// function TExtImages.AddImagesDownPanel
// adding at bottom
// Result : True if done
function TExtImages.AddImagesDownPanel:Boolean;
begin
  if  ( FErasedImages.Count > 0 )
  Then
    Begin
      Result:=True;
      FLastToUse := False;
      AddDownPanel;
    end
  Else
   Result:=False;
  Inherited;
end;

// function TExtImages.NotAddLines
// adding or not adding lines ?
// Result = true if not add lines
function TExtImages.NotAddLines:Boolean;
begin
  with Columns do
  Result :=  (( ImagesPerLine = 0 ) and ( Count*PanelWidth  > Self.Width ))
          or (( ImagesPerLine > 0 ) and ( Count/ImagesPerLine*PanelHeight > Self.Height )  and (CounterPerLine = ImagesPerLine));
end;

// function TExtImages.AddLinesTopPanel
// Adding lines
// Result = true if done
function TExtImages.AddLinesTopPanel:Boolean;
begin
  with Columns do
  if  ( FErasedImages.Count > 0 )
  Then
    Begin
     FLastToUse := False;
     CreatePanelLine;
     PanelLine.Top:=0;
     CounterPerLine := 0;
     AddDownPanel;
     Result := True;
    end
   Else
    Result := False;
  Inherited;
end;

// procedure TExtImages.CreateSomeLine
// restoring last image scrolled
procedure TExtImages.CreateSomeLine;
{var lstl_Files : TStringList;
    li_i, li_j : Integer;
    LCurrentDir : String;
    lb_break : Boolean;}
begin
  with Columns do
  if (FCurrentFile>'')
  and (    ((ImagesPerLine>0) and (VertScrollBar.ClientSize-VertScrollBar.Position<Self.Height+PanelHeight*VisibleLines))
        or ((ImagesPerLine=0) and (HorzScrollBar.ClientSize-HorzScrollBar.Position<Self.Width +PanelWidth *VisibleLines))) Then
    GoToDirectory ( IncludeTrailingPathDelimiter(FDirectory), ExtractFilePath ( FCurrentFile ), FSubDirs );
{  lstl_Files := TStringList.Create;
  with Columns do
  lstl_Files.Text :=IntToStr(ImagesPerLine)+' '+IntToStr(HorzScrollBar.ClientSize)+' '+IntToStr(VertScrollBar.Position)+' '+IntToStr(Self.Height)+' '+IntToStr(PanelHeight)+' '+IntToStr(VisibleLines);
  lstl_Files.SaveToFile ( GetAppDir + 'es.txt' );
  lb_break := False;
  lstl_Files := TStringList.Create;
  try
    while not lb_break and (FDirectories.Count > 0) and fb_FindFiles ( lstl_Files, FDirectories [ 0 ], True, True, True, CST_FILTER_ALL ) do
     Begin
       lstl_Files.Sort;
       with Columns do
        Begin
          LCurrentDir := FDirectories [ 0 ];
          FDirectories.Delete ( 0 );
          for li_i := 0 to lstl_Files.count-1 do
           if lstl_Files  [ li_i ] = FCurrentFile Then
            Begin
              li_j := li_i;
              while li_j < lstl_Files.count do
               Begin
                 if (( ImagesPerLine = 0 ) and ( PanelLine.Left - Self.HorzScrollBar.Position > Self.Width ))
                 or (( ImagesPerLine > 0 ) and ( PanelLine.Top  - Self.VertScrollBar.Position > Self.Height + PanelHeight ) and (CounterPerLine=ImagesPerLine))
                  Then
                   Begin
                     FDirectories.Add(LCurrentDir);
                     FCurrentFile := lstl_Files  [ li_j ];
                     lb_break := True;
                     Break;
                   end;
                  if FSubDirs and DirectoryExistsUTF8 ( lstl_Files [ li_j ] )
                   Then AddDirectory ( lstl_Files [ li_j ], FSubDirs )
                   Else AddFile(lstl_Files [ li_j ]);
                  inc ( li_j );
               end;
             Break;
           End;

          Inherited; // We create only now
        end;
     end;
  finally
    lstl_Files.Destroy;
  end; }
end;

// function TExtImages.CreateImagesMap
// creating Images Collection
function TExtImages.CreateImagesMap: TExtCustomCollectionImages;
begin
  Result := TExtCollectionImages.Create(Self,TExtPanelImage);
  // adding editing component
  if csDesigning in ComponentState Then
    Result.Add;
end;

{$IFDEF VERSIONS}
initialization
  p_ConcatVersion ( gVer_TExtImages );
{$ENDIF}
end.
