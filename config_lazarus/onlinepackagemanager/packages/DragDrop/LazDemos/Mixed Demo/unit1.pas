unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DropTarget, DragDrop, DragDropFile, Forms,
  Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Types, DropSource
  , Shellapi
  , LCLIntf
  , LazFileUtils
  , lazutf8
  , activex
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    DataFormatAdapter_File: TDataFormatAdapter;
    DataFormatAdapter_Stream: TDataFormatAdapter;
    DropDummy1: TDropDummy;
    DropEmptyTarget1: TDropEmptyTarget;
    DropFileSource1: TDropFileSource;
    DropFileTarget1: TDropFileTarget;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    Label10: TLabel;
    Label9: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    Panel3: TPanel;
    Panel4: TPanel;
    Shape10: TShape;
    Shape9: TShape;
    procedure DropDummy1DragOver(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    procedure DropDummy1Enter(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    procedure DropEmptyTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    procedure DropEmptyTarget1Enter(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    procedure DropFileSource1GetDragImage(Sender: TObject;
      const DragSourceHelper: IDragSourceHelper; var Handled: boolean);
    procedure DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Longint);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListView1Resize(Sender: TObject);
    procedure LV_InsertFiles(strPath: string; ListView: TListView; ImageList: TImageList);
  private
    { private declarations }
    test_path:string;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

function FileSizeFormat(bytes:Double):string;


const MAX_DATA = 30000000;

implementation

{$R *.lfm}

{ TForm1 }

function FileSizeFormat(bytes:Double):string;
begin
  // below 1024 bytes
  if bytes < 1024 then
    Result:=floattostr(bytes) + 'b'
  // kilobytes; 1kb = 1024bytes
  else if (bytes >= 1024) and (bytes < 1024*1024) then
    Result:=FormatFloat('0.00', bytes/1024)+'kb'
  // megabytes; 1mb = 1024*1024bytes
  else if (bytes >= 1024*1024) and (bytes < 1024*1024*1024) then
    Result:=FormatFloat('0.00', bytes/1024/1024)+'mb'
  // gigabytes; 1gb = 1024*1024*1024bytes
  else if (bytes >= 1024*1024*1024) and (bytes < 1024*1024*1024*1024) then
    Result:=FormatFloat('0.00', bytes/1024/1024/1024)+'gb'
  // for everything above, we show it in terrabytes
  else
    Result:=FormatFloat('0.00', bytes/1024/1024/1024/1024)+'tb';
end;

procedure TForm1.LV_InsertFiles(strPath: string; ListView: TListView; ImageList: TImageList);
var
  i, ii: Integer;
  Icon_: TIcon;
  SearchRec: TSearchRec;
  ListItem: TListItem;
  FileInfo: SHFILEINFOW;
  Filetime:TDateTime;
  SortList:TStringlist;
  SortListItem:TStringlist;
begin
  screen.cursor:=crHourGlass;
  // Assign a Imagelist to the ListView
  ListView.SmallImages := ImageList1;
  // Show Listview in Report Style and add 2 Columns
  ListView.ViewStyle := vsReport;
  ListView.Columns.Clear;
  ListView.Items.Clear;
  // Name
  ListView.Columns.Add;
  ListView.Columns[ ListView.Columns.Count-1].Width:=250;
  ListView.Columns[ ListView.Columns.Count-1].Caption:='Filename';
  // Beschreibung
  ListView.Columns.Add;
  ListView.Columns[ ListView.Columns.Count-1].Width:=250;
  ListView.Columns[ ListView.Columns.Count-1].Caption:='Description';
  // Datum
  ListView.Columns.Add;
  ListView.Columns[ ListView.Columns.Count-1].Width:=220;
  ListView.Columns[ ListView.Columns.Count-1].Caption:='Date';
  // Size
  ListView.Columns.Add;
  ListView.Columns[ ListView.Columns.Count-1].Width:=150;
  ListView.Columns[ ListView.Columns.Count-1].Caption:='Size';

  // Create a temporary TIcon
  Icon_ := TIcon.Create;
  ListView.Items.BeginUpdate;
  SortList:=TStringlist.Create;
  SortList.Clear;
  SortList.Sorted:=true;
  SortListItem:=TStringlist.Create;
  SortListItem.Delimiter:=';';
  SortListItem.StrictDelimiter:=true;
  try
    // search for the first file
    i := FindFirstUTF8(strPath + '*.*', faAnyFile, SearchRec);
    while i = 0 do
    begin
      with ListView do
      begin
        // On directories and volumes
        if ((SearchRec.Attr and FaDirectory <> FaDirectory) and
          (SearchRec.Attr and FaVolumeId <> FaVolumeID)) then
        begin
          // big Icon
//          if SHGetFileInfoW(PWideChar( Widestring(strPath + SearchRec.Name)), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_DISPLAYNAME or SHGFI_TYPENAME) <> 0 then begin
          // small Icon
          if SHGetFileInfoW(PWideChar( Widestring(strPath + SearchRec.Name)), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME or SHGFI_TYPENAME) <> 0 then begin

            Icon_.Handle := FileInfo.hIcon;
            ii:= ImageList.AddIcon(Icon_);
            DestroyIcon(FileInfo.hIcon);

            Filetime:=FileDatetoDateTime( SearchRec.Time);

            SortList.add(
              Formatdatetime('yyyymmddhhnnss',Filetime) + ';' +
              UTF16toUTF8(FileInfo.szDisplayName) + ';' +
              inttostr(ii) + ';' +
              FileInfo.szTypeName + ';' +
              Formatdatetime('dd.mm.yyyy hh:nn:ss',Filetime) + ';' +
              format('%10s',[FileSizeFormat( SearchRec.size)])
            );
          end;
        end;
      end;
      i := FindNextUTF8(SearchRec);
    end;

    SortList.Sort;
    for i := 0 to SortList.Count-1 do begin
       SortListItem.DelimitedText:=SortList[ SortList.Count - 1 - i];

       ListItem := ListView.Items.Add;
       Listitem.Caption := SortListItem[1];
       ListItem.ImageIndex := strtoint(SortListItem[2]);
       ListItem.SubItems.Add( SortListItem[3]);
       ListItem.SubItems.Add( SortListItem[4]);
       ListItem.SubItems.Add( SortListItem[5]);
    end;

  finally
    FINDCLOSEutf8( SearchRec);
    Icon_.Free;
    ListView.Items.EndUpdate;
    SortList.free;
    SortListItem.free;
  end;
  screen.cursor:=crdefault;
end;

procedure TForm1.DropFileTarget1Drop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Longint);
var
  liste:Tstringlist;
  i, k:integer;
begin
  liste:=Tstringlist.create;
  liste.Clear;
  for i := 0 to DropFileTarget1.Files.Count - 1 do begin
     liste.add( '>>> ' +extractfilename(DropFileTarget1.Files[i]));
  end;

  if QuestionDlg('Delete Files', 'Delete?'+#13+#13+liste.Text, mtWarning, [mrYes, 'Yes', mrNo, 'N0', 'IsDefault'], 1)= mrYes then begin
    for i := 0 to DropFileTarget1.Files.Count - 1 do begin
      if not deletefile( DropFileTarget1.Files[i]) then
        Showmessage('Couldn''t delete File:'+#13+#13+liste[i]);
    end;
    for k := liste.Count-1 to 0 do begin
      for i := 0 to ListView1.Items.Count - 1 do begin
        if ListView1.Items[i].Caption = liste[k] then begin
         ListView1.Items.Delete( i);
         break;
        end;
      end;
    end;
  end;
  liste.free;
  LV_InsertFiles(test_path +PathDelim, ListView1, ImageList1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  test_path:=extractfilepath(application.exename) + 'Demo';
  if not directoryexists( test_path ) then
    mkdir(  test_path );
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LV_InsertFiles(test_path +PathDelim, ListView1, ImageList1);
  ListView1Resize(Self);
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
begin
  OpenURL( test_path +PathDelim + ListView1.Selected.Caption);
end;

procedure TForm1.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  (*
  ** Wait for user to move mouse before we start the drag/drop.
  *)

  // Note:
  // Due to some internal mouse message juggling inside TListView we will not
  // get the MouseDown event until the mouse is either moved or the mouse button
  // is released.
  // Remember this when it appears that DragDetectPlus isn't working...
  if (ListView1.SelCount > 0) and (DragDetectPlus(TWinControl(Sender))) then
  begin
    // Delete anything from a previous drag.
    DropFileSource1.Files.Clear;

    // Fill DropSource1.Files with selected files from ListView1.
    for i := 0 to ListView1.Items.Count-1 do
      if (ListView1.items.Item[i].Selected) then
        DropFileSource1.Files.Add( test_path + PathDelim + ListView1.items.Item[i].Caption);

    // Start the drag operation.
    DropFileSource1.Execute;
  end;
end;

procedure TForm1.ListView1Resize(Sender: TObject);
begin
  if ListView1.Columns.Count = 4 then
    ListView1.Columns[ 0].Width:=ListView1.Width - 5 - ListView1.Columns[ 1].Width - ListView1.Columns[ 2].Width - ListView1.Columns[ 3].Width;
end;

procedure TForm1.DropDummy1DragOver(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Longint);
begin
  Effect:=DROPEFFECT_COPY;
end;

procedure TForm1.DropDummy1Enter(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Longint);
begin
  Effect:=DROPEFFECT_COPY;
end;

procedure TForm1.DropEmptyTarget1Drop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Longint);
var
  i, x: integer;
  Stream: IStream;
  filename : string;
  StatStg: TStatStg;
  Total, BufferSize, Chunk, Size: longInt;
  Buffer: AnsiString;
  p: PAnsiChar;
  F1: TextFile;
  MyList : TStringlist;
begin
  if TFileDataFormat(DataFormatAdapter_File.DataFormat).Files.Count > 0 then begin
    for i := 0 to TFileDataFormat(DataFormatAdapter_File.DataFormat).Files.Count-1 do begin
      filename := UTF8toansi(TFileDataFormat(DataFormatAdapter_File.DataFormat).Files[i]);
      if (not(fileexists(test_path +PathDelim + extractfilename(filename)))) or
         (
           (fileexists(test_path +PathDelim + extractfilename(filename))=true ) and
           (QuestionDlg('File exists', 'Replace existing File?'+#13+extractfilename(filename), mtWarning, [mrYes, 'Yes', mrNo, 'No', 'IsDefault'], 1)= mrYes)
         )
      then begin
        copyfile(PChar(filename), PChar(test_path +PathDelim + extractfilename(filename)), false);
      end;
    end;
    LV_InsertFiles(test_path +PathDelim, ListView1, ImageList1);
  end else
  // Transfer the file names and contents from the data format.
  if (TVirtualFileStreamDataFormat(DataFormatAdapter_Stream.DataFormat).FileNames.Count > 0) then begin
    try
      // Note: Since we can actually drag and drop from and onto ourself, we
      // can't clear the ListView until the data has been read from the listview
      // (by the source) and inserted into it again (by the target). To
      // accomplish this, we add the dropped items to the list first and then
      // delete the old items afterwards.
      // Another, and more common, approach would be to reject or disable drops
      // onto ourself while we are performing drag/drop operations.
      for i := 0 to TVirtualFileStreamDataFormat(DataFormatAdapter_Stream.DataFormat).FileNames.Count-1 do begin
        filename := UTF8toansi(TVirtualFileStreamDataFormat(DataFormatAdapter_Stream.DataFormat).FileNames[i]);

        if (not(fileexists(test_path +PathDelim + filename))) or
           (
             (fileexists(test_path +PathDelim + filename)=true ) and
             (QuestionDlg('File exists', 'Replace existing File?'+#13+extractfilename(filename), mtWarning, [mrYes, 'Yes', mrNo, 'No', 'IsDefault'], 1)= mrYes)
           )
        then begin
          // Get data stream from source.
          Stream := TVirtualFileStreamDataFormat(DataFormatAdapter_Stream.DataFormat).FileContentsClipboardFormat.GetStream(i);
          if (Stream <> nil) then
          begin
            // Read data from stream.
            Stream.Stat(StatStg, STATFLAG_NONAME);
            Total := StatStg.cbSize;

            // Assume that stream is at EOF, so set it to BOF.
            // See comment in TCustomSimpleClipboardFormat.DoSetData (in
            // DragDropFormats.pas) for an explanation of this.
            Stream.Seek(0, STREAM_SEEK_SET, qword( PLargeint(nil)^));

            // If a really big hunk of data has been dropped on us we display a
            // small part of it since there isn't much point in trying to display
            // it all in the limited space we have available.
            // Additionally, it would be *really* bad for performce if we tried to
            // allocated a too big buffer and read sequentially into it. Tests has
            // shown that allocating a 10Mb buffer and trying to read data into it
            // in 1Kb chunks takes several minutes, while the same data can be
            // read into a 32Kb buffer in 1Kb chunks in seconds. The Windows
            // explorer uses a 1 Mb buffer, but that's too big for this demo.
            // The above tests were performed using the AsyncSource demo.
            BufferSize := Total;
            if (BufferSize > MAX_DATA) then
              BufferSize := MAX_DATA;

            SetLength(Buffer, BufferSize);

            // dem Pointer Speicher zuweisen
            p := PAnsiChar(Buffer);
            Stream.Read(p, Buffersize, @Size);
            AssignFile(F1, test_path +PathDelim + filename);
            Rewrite(F1);
            for x := 1 to buffersize  do
              Write(F1, buffer[x]);
            CloseFile(F1);


            LV_InsertFiles(test_path +PathDelim, ListView1, ImageList1);
            ListView1Resize(self);
          end;
        end;
      end;
    finally
      screen.cursor := crdefault;
    end;
  end else begin
    try
      // Save a reference to the data object for later use. We need it to fetch
      // data from the drop source when the user selects an item from the list.
      //MyList := TStringlist.Create;
      //GetFileListFromObj( TCustomDropTarget(Sender).DataObject, MyList);
      //for x  := 0 to MyList.Count - 1 do
      //   Memo1.lines.add( MyList[x]);
      //MyList.free;
    finally
      screen.cursor := crdefault;
    end;
  end;
  Shape10.Brush.Color:=clwhite;
end;

procedure TForm1.DropEmptyTarget1Enter(Sender: TObject;
  ShiftState: TShiftState; APoint: TPoint; var Effect: Longint);
begin
  with TVirtualFileStreamDataFormat(DataFormatAdapter_Stream.DataFormat) do begin
    if not(
            FileContentsClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject)
            and
            (
              AnsiFileGroupDescriptorClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject)
              or
              UnicodeFileGroupDescriptorClipboardFormat.HasValidFormats(DropEmptyTarget1.DataObject)
            )
          ) then begin
        Effect := DROPEFFECT_NONE;
     end;
  end;
end;

procedure TForm1.DropFileSource1GetDragImage(Sender: TObject;
  const DragSourceHelper: IDragSourceHelper; var Handled: boolean);
var
  Pt: TPoint;
begin
  GetCursorPos(Pt);
  Handled := Succeeded(DragSourceHelper.InitializeFromWindow(Listview1.Handle, Pt, TCustomDropSource(Sender) as IDataObject));
end;

end.

