{**********************************************************************
 Package pl_OpenGL.pkg
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit OpenGL_Textures;

{$mode DELPHI}

interface

uses
  Classes,SysUtils,LCLtype, LCLIntf, Graphics, IntfGraphics, LazTGA,
  GL,GLu;

function LoadTexture(Filename: String; var Texture: GLuint; LoadFromRes : Boolean): Boolean;

implementation


{------------------------------------------------------------------}
{  Create the Texture                                              }
{------------------------------------------------------------------}
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
var  Texture : GLuint;
begin
  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);

  result :=Texture;
end;


{------------------------------------------------------------------}
{  Load BMP textures                                               }
{------------------------------------------------------------------}

function LoadBMPTexture(Filename: String; var Texture : GLuint; LoadFromResource : Boolean) : Boolean;
var
  FileHeader: BITMAPFILEHEADER;
  InfoHeader: BITMAPINFOHEADER;
  Palette: array of RGBQUAD;
  BitmapFile: TFileStream;
  BitmapLength: LongWord;
  PaletteLength: LongWord;
  ReadBytes: LongWord;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  I : Integer;
  Width, Height : Integer;
  pData : Pointer;

  // used for loading from resource
  ResHandle : THandle;
  MemHandle : THandle;
  MemStream : TMemoryStream;
  ResPtr    : PByte;
  ResSize   : Longint;
begin

  if LoadFromResource then // Load from resource
  begin
    try
      ResHandle := FindResource(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'bmp');
      if ResHandle = 0 then
      begin

        MessageBox(0, PChar('File not found in resource - ' + Filename), PChar('BMP Texture'), MB_OK);
        result :=FALSE;
        Exit;
      end;
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);

      MemStream.ReadBuffer(FileHeader, SizeOf(FileHeader));  // FileHeader
      MemStream.ReadBuffer(InfoHeader, SizeOf(InfoHeader));  // InfoHeader
      PaletteLength := InfoHeader.biClrUsed;
      SetLength(Palette, PaletteLength);
      MemStream.ReadBuffer(Palette, PaletteLength);          // Palette

      Width := InfoHeader.biWidth;
      Height := InfoHeader.biHeight;

      BitmapLength := InfoHeader.biSizeImage;
      if BitmapLength = 0 then
        BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;

      GetMem(pData, BitmapLength);
      MemStream.ReadBuffer(pData^, BitmapLength);            // Bitmap Data
      MemStream.Free;
    except
      MessageBox(0, PChar('Error reading file from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end
  else
  begin   // Load image from file

    BitmapFile:= TFileStream.Create(Filename,fmOpenRead);
    ReadBytes:=BitmapFile.Read(FileHeader, SizeOf(FileHeader));
    ReadBytes:=BitmapFile.Read(InfoHeader, SizeOf(InfoHeader));

    // Get palette
    PaletteLength := InfoHeader.biClrUsed;
    SetLength(Palette, PaletteLength);

    ReadBytes:=BitmapFile.Read(Palette, PaletteLength);
    if (ReadBytes <> PaletteLength) then begin
      MessageBox(0, PChar('Error reading palette'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;

    Width  := InfoHeader.biWidth;
    Height := InfoHeader.biHeight;
    BitmapLength := InfoHeader.biSizeImage;

    if BitmapLength = 0 then
      BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;

    // Get the actual pixel data
    GetMem(pData, BitmapLength);
    ReadBytes:=BitmapFile.Read( pData^, BitmapLength);
    if (ReadBytes <> BitmapLength) then
    begin
      MessageBox(0, PChar('Error reading bitmap data'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;

    BitmapFile.Free;
  end;

  //===================================================================
  // Bitmaps are stored BGR and not RGB, so swap the R and B bytes.
  for I :=0 to Width * Height - 1 do
  begin
    Front := Pointer(ptrint(pData) + I*3);
    Back := Pointer(ptrint(pData) + I*3 + 2);
    Temp := Front^;
    Front^ := Back^;
    Back^ := Temp;
  end;

  Texture :=CreateTexture(Width, Height, GL_RGB, pData);
  FreeMem(pData);
  result :=TRUE;
end;


{------------------------------------------------------------------}
{  Load JPEG textures                                              }
{------------------------------------------------------------------}
function LoadJPGTexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  iBMP:TLazIntfImage;
  JPG : TJPEGImage;
  C : LongWord;
  Line : ^LongWord;

  // used for loading from resource
  ResHandle : THandle;
  MemHandle : THandle;
  MemStream : TMemoryStream;
  ResPtr    : PByte;
  ResSize   : Longint;
begin
  JPG:=TJPEGImage.Create;

  if LoadFromResource then // Load from resource
  begin
    try
      ResHandle := FindResource(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'JPEG');
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);
      JPG.LoadFromStream(MemStream);
      MemStream.Free;
    except
      MessageBox(0, PChar('Couldn''t load JPG Resource - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end
  else
  begin
    try
      JPG.LoadFromFile(Filename);
    except
      MessageBox(0, PChar('Couldn''t load JPG - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end;

  Width :=JPG.Width;
  Height :=JPG.Height;
  SetLength(Data, Width*Height);

  //========================================
  iBMP:=JPG.CreateIntfImage;

  For H:=0 to Height-1 do
  Begin
    Line :=iBMP.GetDataLineStart(Height-H-1);   // flip JPEG
    For W:=0 to Width-1 do
    Begin   // No Transparent Color
      c:=Line^ and $FFFFFF; // Need to do a color swap
      Data[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;
      inc(Line);
    End;
  End;
  FreeAndNil(iBMP);
  //======================================

  JPG.free;

  Texture :=CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result :=TRUE;
end;

{------------------------------------------------------------------}
{  Load GIF textures                                               }
{------------------------------------------------------------------}

function LoadGIFTexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  iBMP:TLazIntfImage;
  GIF : TGIFImage;
  C : LongWord;
  Line : ^LongWord;

  // used for loading from resource
  ResHandle : THandle;
  MemHandle : THandle;
  MemStream : TMemoryStream;
  ResPtr    : PByte;
  ResSize   : Longint;
begin
  GIF:=TGIFImage.Create;

  if LoadFromResource then // Load from resource
  begin
    try
      ResHandle := FindResource(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'GIF');
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);
      GIF.LoadFromStream(MemStream);
      MemStream.Free;
    except
      MessageBox(0, PChar('Couldn''t load JPG Resource - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end
  else
  begin
    try
      GIF.LoadFromFile(Filename);
    except
      MessageBox(0, PChar('Couldn''t load JPG - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end;

  Width :=GIF.Width;
  Height :=GIF.Height;
  SetLength(Data, Width*Height);

  //========================================
  iBMP:=GIF.CreateIntfImage;

  For H:=0 to Height-1 do
  Begin
    Line :=iBMP.GetDataLineStart(Height-H-1);   // flip JPEG
    For W:=0 to Width-1 do
    Begin   // No Transparent Color
      c:=Line^ and $FFFFFF; // Need to do a color swap
      Data[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;
      inc(Line);
    End;
  End;
  FreeAndNil(iBMP);
  //======================================

  GIF.free;

  Texture :=CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result :=TRUE;
end;


{------------------------------------------------------------------}
{  Loads TGA textures                                              }
{------------------------------------------------------------------}
function LoadTGATexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  iBMP:TLazIntfImage;
  TGA : TTGAImage;
  C : LongWord;
  Line : ^LongWord;

  // used for loading from resource
  ResHandle : THandle;
  MemHandle : THandle;
  MemStream : TMemoryStream;
  ResPtr    : PByte;
  ResSize   : Longint;
begin
  TGA:=TTGAImage.Create;

  if LoadFromResource then // Load from resource
  begin
    try
      ResHandle := FindResource(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'TGA');
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);
      TGA.LoadFromStream(MemStream);
      MemStream.Free;
    except
      MessageBox(0, PChar('Couldn''t load TGA Resource - "'+ Filename +'"'), PChar('TGA Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end
  else
  begin
    try
      TGA.LoadFromFile(Filename);
    except
      MessageBox(0, PChar('Couldn''t load TGA - "'+ Filename +'"'), PChar('TGA Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end;


  Width :=TGA.Width;
  Height :=TGA.Height;
  SetLength(Data, Width*Height);

  //========================================
  iBMP:=TGA.CreateIntfImage;

  For H:=0 to Height-1 do
  Begin
    Line :=iBMP.GetDataLineStart(Height-H-1);
    For W:=0 to Width-1 do
    Begin    // With Transparent Color
      c:=Line^;
      {$IFDEF WINDOWS}
       Data[W+(H*Width)] :=((c and $FF) shl 16) or ((c and $FF0000) shr 16) or c and $FF00FF00;
      {$ELSE}
       Data[W+(H*Width)] :=c;
      {$ENDIF}
      inc(Line);
    End;
  End;
  FreeAndNil(iBMP);
  //======================================

  TGA.free;

  Texture :=CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result :=TRUE;
end;

{------------------------------------------------------------------}
{  Load PNG textures                                               }
{------------------------------------------------------------------}

function LoadPNGTexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  iBMP:TLazIntfImage;
  PNG : TPortableNetworkGraphic;
  C : LongWord;
  Line : ^LongWord;
  // used for loading from resource
  ResHandle : THandle;
  MemHandle : THandle;
  MemStream : TMemoryStream;
  ResPtr    : PByte;
  ResSize   : Longint;
begin
  PNG:=TPortableNetworkGraphic.Create;

  if LoadFromResource then // Load from resource
  begin
    try
      ResHandle := FindResource(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'PNG');
      MemHandle := LoadResource(hInstance, ResHandle);
      ResPtr    := LockResource(MemHandle);
      MemStream := TMemoryStream.Create;
      ResSize := SizeOfResource(hInstance, ResHandle);
      MemStream.SetSize(ResSize);
      MemStream.Write(ResPtr^, ResSize);
      FreeResource(MemHandle);
      MemStream.Seek(0, 0);
      PNG.LoadFromStream(MemStream);
      MemStream.Free;
    except
      MessageBox(0, PChar('Couldn''t load JPG Resource - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end
  else
  begin
    try
      PNG.LoadFromFile(Filename);
    except
      MessageBox(0, PChar('Couldn''t load JPG - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      result :=FALSE;
      Exit;
    end;
  end;

  Width :=PNG.Width;
  Height :=PNG.Height;
  SetLength(Data, Width*Height);

  //========================================
  iBMP:=PNG.CreateIntfImage;

  For H:=0 to Height-1 do
  Begin
    Line :=iBMP.GetDataLineStart(Height-H-1);   // flip JPEG
    For W:=0 to Width-1 do
    Begin    // With Transparent Color
      c:=Line^;
      {$IFDEF WINDOWS}
       Data[W+(H*Width)] :=((c and $FF) shl 16) or ((c and $FF0000) shr 16) or c and $FF00FF00;
      {$ELSE}
       Data[W+(H*Width)] :=c;
      {$ENDIF}
      inc(Line);
    End;
  End;
  FreeAndNil(iBMP);
  //======================================

  PNG.free;

  Texture :=CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result :=TRUE;
end;



{------------------------------------------------------------------}
{  Determines file type and sends to correct function              }
{------------------------------------------------------------------}

function LoadTexture(Filename: String; var Texture : GLuint; LoadFromRes : Boolean) : Boolean;
begin
  if copy(filename, length(filename)-3, 4) = '.bmp' then
    LoadBMPTexture(Filename, Texture, LoadFromRes);
  if copy(filename, length(filename)-3, 4) = '.png' then
    LoadPNGTexture(Filename, Texture, LoadFromRes);
  if copy(filename, length(filename)-3, 4) = '.jpg' then
    LoadJPGTexture(Filename, Texture, LoadFromRes);
  if copy(filename, length(filename)-3, 4) = '.gif' then
    LoadGIFTexture(Filename, Texture, LoadFromRes);
  if copy(filename, length(filename)-3, 4) = '.tga' then
    LoadTGATexture(Filename, Texture, LoadFromRes);
end;


end.

