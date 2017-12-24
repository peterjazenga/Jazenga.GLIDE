unit FindFolderFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Buttons,
  ComCtrls;

type

  TSearchType=(tst_folder,
               tst_file);

  { TFrmFindFolder }

  TFrmFindFolder = class(TForm)
    btnFindFolders: TButton;
    edtRootDir: TEdit;
    lblItemsFound: TLabel;
    lblInitialSearchDir: TLabel;
    lstFolders: TListBox;
    btnSelectRootFolder: TSpeedButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    procedure btnFindFoldersClick(Sender: TObject);
    procedure btnSelectRootFolderClick(Sender: TObject);
    procedure lstFoldersDblClick(Sender: TObject);
  private
    FSelectedItem: string;
  public
    SearchCriteria: string;
    SearchType:TSearchType;
    property SelectedItem:string read FSelectedItem;
  end;

function ShowFindFolderDlg(var InitialRootDir:string;_searchcriteria,_caption:string;_SearchType:TSearchType):string;
function AllFilesOfDrive(_path,_mask:string;_FileList:TStrings;_StatusBar:TStatusBar):boolean;

implementation

{$R *.lfm}


procedure AllDirectoriesOfPath(const _Path:string;_SearchMask: string;_DirList:TStrings;_StatusBar:TStatusBar); // get all directories of path <_path> into the Dirlist.
var
  _SearchRec : TSearchRec;

  function AddToList(const _Rec : TSearchRec;_searchmask:string):boolean;
  begin
    result:=false;
    if _Rec.Attr and faDirectory<>faDirectory then exit;  // we only want directories here
    if _Rec.Attr and faSymLink=faSymLink then exit;
    if _Rec.name='' then exit;                            // empty name is not allowed.
    if pos('.',_Rec.name)=1 then exit;                    // '..' and '.' we also don't want in the list.
    if _DirList.IndexOf(IncludeTrailingPathDelimiter(_Path)+_Rec.Name)>-1 then exit;          // check if the entry is not already in the list.
    if pos(_searchmask,_Rec.Name)<>0 then _DirList.add(IncludeTrailingPathDelimiter(_Path)+_Rec.Name); // add it.
    result:=true;
  end;

begin
  if not assigned(_DirList) then exit;
  try
    if not SysUtils.FindFirst(IncludeTrailingPathDelimiter(_Path)+'*', faDirectory or fasymlink , _SearchRec) = 0 then exit;
    AddToList(_SearchRec,_SearchMask);
    while SysUtils.FindNext(_SearchRec) = 0 do begin
      if not AddToList(_SearchRec,_searchMask) then continue;
      _StatusBar.simpletext:=IncludeTrailingPathDelimiter(_Path)+_SearchRec.Name;
      AllDirectoriesOfPath(IncludeTrailingPathDelimiter(_Path)+_SearchRec.Name,_SearchMask,_DirList,_StatusBar);
      Application.ProcessMessages;
    end;
  finally
    SysUtils.FindClose(_SearchRec);
  end;
end;


function AllFilesOfDrive(_path,_mask:string;_FileList:TStrings;_StatusBar:TStatusBar):boolean;
var
_search: TSearchRec;
begin
  result:=false;
  if not assigned(_FileList) then exit;
  _mask:=lowercase(_mask);
  _path:=IncludeTrailingPathDelimiter(_path);
  // find all files
  try
    if SysUtils.FindFirst(_path+_mask, faAnyFile, _search) = 0 then
    begin
      repeat
        // add the files to the listbox
        if trim(_search.Name)<>'' then _FileList.Add(_path + _search.Name);
      until (SysUtils.FindNext(_search) <> 0);
    end;

    Application.ProcessMessages;
    Result := not (Application.Terminated);
    if not Result then exit;

    // Subdirectories/ Unterverzeichnisse
    if SysUtils.FindFirst(_path + '*.*', faDirectory, _search) = 0 then
    begin
      repeat
        if ((_search.Attr and faDirectory) = faDirectory) and
           (trim(_search.Name)<>'') and
           (_search.Name[1] <> '.') then AllFilesOfDrive(_path + _search.Name + '\',_mask,_FileList,_StatusBar);
        if assigned(_StatusBar) then _StatusBar.simpletext:=IncludeTrailingPathDelimiter(_Path)+_Search.Name;
      until (SysUtils.FindNext(_search) <> 0);
    end;
    result:=true;
  finally
    SysUtils.FindClose(_search);
  end;
end;



function ShowFindFolderDlg(var InitialRootDir:string;_searchcriteria,_caption:string;_SearchType:TSearchType):string;
var
_FrmFindFolder: TFrmFindFolder;
begin
  result:='';
  Screen.Cursor:=crDefault;
  _FrmFindFolder:=TFrmFindFolder.create(nil);
  try
    _FrmFindFolder.caption:=_caption;
    _FrmFindFolder.SearchCriteria:=_searchcriteria;
    _FrmFindFolder.SearchType:=_SearchType;
    _FrmFindFolder.edtRootDir.text:=IncludeTrailingPathDelimiter(InitialRootDir);
    _FrmFindFolder.showmodal;
    if trim(_FrmFindFolder.SelectedItem)<>'' then begin
      case _SearchType of
        tst_folder:result:=IncludeTrailingPathDelimiter(_FrmFindFolder.SelectedItem);
        tst_file:result  :=_FrmFindFolder.SelectedItem;
      end;
    end;
    InitialRootDir:=_FrmFindFolder.edtRootDir.text;
  finally
    _FrmFindFolder.free;
  end;
end;

{ TFrmFindFolder }

procedure TFrmFindFolder.btnFindFoldersClick(Sender: TObject);
var
_DirList:TStrings;
_path:string;
begin
  btnFindFolders.Enabled:=false;
  edtRootDir.enabled:=false;
  btnSelectRootFolder.enabled:=false;
  _DirList:=TStringList.create;
  try
    _path:=edtRootDir.text;
    case SearchType of
      tst_folder:AllDirectoriesOfPath(_path,searchcriteria,_DirList,StatusBar1);
      tst_file:AllFilesOfDrive(_path,searchcriteria,_DirList,StatusBar1);
    end;
    lstFolders.Items.assign(_DirList);
    if _DirList.count=0 then StatusBar1.SimpleText:=Format('No folder with search criteria <%s> in <%s> found.',[searchcriteria,edtRootDir.text])
                        else StatusBar1.SimpleText:='Search finished.';
  finally
    _DirList.free;
    btnFindFolders.Enabled:=true;
    edtRootDir.enabled:=true;
    btnSelectRootFolder.enabled:=true;
  end;
end;

procedure TFrmFindFolder.btnSelectRootFolderClick(Sender: TObject);
begin
  if not SelectDirectoryDialog1.Execute then exit;
  edtRootDir.text:=SelectDirectoryDialog1.FileName;
end;

procedure TFrmFindFolder.lstFoldersDblClick(Sender: TObject);
begin
  FSelectedItem := lstFolders.Items[lstFolders.ItemIndex];
  close;
end;



end.

