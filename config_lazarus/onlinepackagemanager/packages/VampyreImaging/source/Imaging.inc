
type
  { Default Imaging excepton class }
  EImagingError = class(Exception);
  { Raised when function receives bad image (not passed TestImage).}
  EImagingBadImage = class(Exception)
  public
    constructor Create;
  end;

  { Dynamic array of TImageData records }
  TDynImageDataArray = array of TImageData;


{ ------------------------------------------------------------------------
                       Low Level Interface Functions
  ------------------------------------------------------------------------}

{ General Functions }

{ Initializes image (all is set to zeroes). Call this for each image
  before using it (before calling every other function) to be sure there
  are no random-filled bytes (which would cause errors later).}
procedure InitImage(var Image: TImageData);
{ Creates empty image of given dimensions and format. Image is filled with
  transparent black color (A=0, R=0, G=0, B=0).}
function NewImage(Width, Height: LongInt; Format: TImageFormat; var Image: TImageData): Boolean;
{ Returns True if given TImageData record is valid.}
function TestImage(const Image: TImageData): Boolean;
{ Frees given image data. Ater this call image is in the same state
  as after calling InitImage. If image is not valid (dost not pass TestImage
  test) it is only zeroed by calling InitImage.}
procedure FreeImage(var Image: TImageData);
{ Call FreeImage() on all images in given dynamic array and sets its
  length to zero.}
procedure FreeImagesInArray(var Images: TDynImageDataArray);
{ Returns True if all TImageData records in given array are valid. Returns False
  if at least one is invalid or if array is empty.}
function TestImagesInArray(const Images: TDynImageDataArray): Boolean;
{ Checks given file for every supported image file format and if
  the file is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If file is not in any of the supported formats empty string is returned.}
function DetermineFileFormat(const FileName: string): string;
{ Checks given stream for every supported image file format and if
  the stream is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If stream is not in any of the supported formats empty string is returned.}
function DetermineStreamFormat(Stream: TStream): string;
{ Checks given memory for every supported image file format and if
  the memory is in one of them returns its string identifier
  (which can be used in LoadFromStream/LoadFromMem type functions).
  If memory is not in any of the supported formats empty string is returned.}
function DetermineMemoryFormat(Data: Pointer; Size: LongInt): string;
{ Checks that an apropriate file format is supported purely from inspecting
  the given file name's extension (not contents of the file itself).
  The file need not exist.}
function IsFileFormatSupported(const FileName: string): Boolean;
{ Enumerates all registered image file formats. Descriptive name,
  default extension, masks (like '*.jpg,*.jfif') and some capabilities
  of each format are returned. To enumerate all formats start with Index at 0 and
  call EnumFileFormats with given Index in loop until it returns False (Index is
  automatically increased by 1 in function's body on successful call).}
function EnumFileFormats(var Index: LongInt; var Name, DefaultExt, Masks: string;
  var CanSaveImages, IsMultiImageFormat: Boolean): Boolean;

{ Loading Functions }

{ Loads single image from given file.}
function LoadImageFromFile(const FileName: string; var Image: TImageData): Boolean;
{ Loads single image from given stream. If function fails stream position
  is not changed.}
function LoadImageFromStream(Stream: TStream; var Image: TImageData): Boolean;
{ Loads single image from given memory location.}
function LoadImageFromMemory(Data: Pointer; Size: LongInt; var Image: TImageData): Boolean;
{ Loads multiple images from given file.}
function LoadMultiImageFromFile(const FileName: string;
  var Images: TDynImageDataArray): Boolean;
{ Loads multiple images from given stream. If function fails stream position
  is not changed.}
function LoadMultiImageFromStream(Stream: TStream;
  var Images: TDynImageDataArray): Boolean;
{ Loads multiple images from given memory location.}
function LoadMultiImageFromMemory(Data: Pointer; Size: LongInt;
  var Images: TDynImageDataArray): Boolean;

{ Saving Functions }

{ Saves single image to given file.}
function SaveImageToFile(const FileName: string; const Image: TImageData): Boolean;
{ Saves single image to given stream. If function fails stream position
  is not changed. Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveImageToStream(const Ext: string; Stream: TStream;
  const Image: TImageData): Boolean;
{ Saves single image to given memory location. Memory must be allocated and its
  size is passed in Size parameter in which number of written bytes is returned.
  Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveImageToMemory(const Ext: string; Data: Pointer; var Size: LongInt;
  const Image: TImageData): Boolean;
{ Saves multiple images to given file. If format supports
  only single level images and there are multiple images to be saved,
  they are saved as sequence of files img000.jpg, img001.jpg ....).}
function SaveMultiImageToFile(const FileName: string;
  const Images: TDynImageDataArray): Boolean;
{ Saves multiple images to given stream. If format supports
  only single level images and there are multiple images to be saved,
  they are saved one after another to the stream. If function fails stream
  position is not changed. Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveMultiImageToStream(const Ext: string; Stream: TStream;
  const Images: TDynImageDataArray): Boolean;
{ Saves multiple images to given memory location. If format supports
  only single level images and there are multiple images to be saved,
  they are saved one after another to the memory. Memory must be allocated and
  its size is passed in Size parameter in which number of written bytes is returned.
  Ext identifies desired image file format (jpg, png, dds, ...).}
function SaveMultiImageToMemory(const Ext: string; Data: Pointer;
  var Size: LongInt; const Images: TDynImageDataArray): Boolean;

{ Manipulation Functions }

{ Creates identical copy of image data. Clone should be initialized
  by InitImage or it should be vaild image which will be freed by CloneImage.}
function CloneImage(const Image: TImageData; var Clone: TImageData): Boolean;
{ Converts image to the given format.}
function ConvertImage(var Image: TImageData; DestFormat: TImageFormat): Boolean;
{ Flips given image. Reverses the image along its horizontal axis - the top
  becomes the bottom and vice versa.}
function FlipImage(var Image: TImageData): Boolean;
{ Mirrors given image. Reverses the image along its vertical axis — the left
  side becomes the right and vice versa.}
function MirrorImage(var Image: TImageData): Boolean;
{ Resizes given image to new dimensions. Nearest, bilinear, or bicubic filtering
  can be used. Input Image must already be created - use NewImage to create new images.}
function ResizeImage(var Image: TImageData; NewWidth, NewHeight: LongInt;
  Filter: TResizeFilter): Boolean;
{ Swaps SrcChannel and DstChannel color or alpha channels of image.
  Use ChannelRed, ChannelBlue, ChannelGreen, ChannelAlpha constants to
  identify channels.}
function SwapChannels(var Image: TImageData; SrcChannel, DstChannel: LongInt): Boolean;
{ Reduces the number of colors of the Image. Currently MaxColors must be in
  range <2, 4096>. Color reduction works also for alpha channel. Note that for
  large images and big number of colors it can be very slow.
  Output format of the image is the same as input format.}
function ReduceColors(var Image: TImageData; MaxColors: LongInt): Boolean;
{ Generates mipmaps for image. Levels is the number of desired mipmaps levels
  with zero (or some invalid number) meaning all possible levels.}
function GenerateMipMaps(const Image: TImageData; Levels: LongInt;
  var MipMaps: TDynImageDataArray): Boolean;
{ Maps image to existing palette producing image in ifIndex8 format.
  Pal must be allocated to at least Entries * SizeOf(TColor32Rec) bytes.
  As resulting image is in 8bit indexed format Entries must be lower or
  equal to 256.}
function MapImageToPalette(var Image: TImageData; Pal: PPalette32;
  Entries: LongInt): Boolean;
{ Splits image into XChunks x YChunks subimages. Default size of each chunk is
  ChunkWidth x ChunkHeight. If PreserveSize si True chunks at the edges of
  the image are also ChunkWidth x ChunkHeight sized and empty space is filled
  with optional Fill pixels. After calling this function XChunks contains number of
  chunks along x axis and YChunks along y axis. To access chunk [X, Y] use this
  index: Chunks[Y * XChunks + X].}
function SplitImage(var Image: TImageData; var Chunks: TDynImageDataArray;
  ChunkWidth, ChunkHeight: LongInt; var XChunks, YChunks: LongInt;
  PreserveSize: Boolean; Fill: Pointer = nil): Boolean;
{ Creates palette with MaxColors based on the colors of images in Images array.
  Use it when you want to convert several images to indexed format using
  single palette for all of them. If ConvertImages is True images in array
  are converted to indexed format using resulting palette. if it is False
  images are left intact and only resulting palatte is returned in Pal.
  Pal must be allocated to have at least MaxColors entries.}
function MakePaletteForImages(var Images: TDynImageDataArray; Pal: PPalette32;
  MaxColors: LongInt; ConvertImages: Boolean): Boolean;
{ Rotates image by Angle degrees counterclockwise. All angles are allowed.}
procedure RotateImage(var Image: TImageData; Angle: Single);

{ Drawing/Pixel functions }

{ Copies rectangular part of SrcImage to DstImage. No blending is performed -
  alpha is simply copied to destination image. Operates also with
  negative X and Y coordinates.
  Note that copying is fastest for images in the same data format
  (and slowest for images in special formats).}
function CopyRect(const SrcImage: TImageData; SrcX, SrcY, Width, Height: LongInt;
  var DstImage: TImageData; DstX, DstY: LongInt): Boolean;
{ Fills given rectangle of image with given pixel fill data. Fill should point
  to the pixel in the same format as the given image is in.}
function FillRect(var Image: TImageData; X, Y, Width, Height: LongInt; FillColor: Pointer): Boolean;
{ Replaces pixels with OldPixel in the given rectangle by NewPixel.
  OldPixel and NewPixel should point to the pixels in the same format
  as the given image is in.}
function ReplaceColor(var Image: TImageData; X, Y, Width, Height: LongInt;
  OldColor, NewColor: Pointer): Boolean;
{ Stretches the contents of the source rectangle to the destination rectangle
  with optional resampling. No blending is performed - alpha is
  simply copied/resampled to destination image. Note that stretching is
  fastest for images in the same data format (and slowest for
  images in special formats).}
function StretchRect(const SrcImage: TImageData; SrcX, SrcY, SrcWidth,
  SrcHeight: LongInt; var DstImage: TImageData; DstX, DstY, DstWidth,
  DstHeight: LongInt; Filter: TResizeFilter): Boolean;
{ Copies pixel of Image at [X, Y] to memory pointed at by Pixel. Doesn't
  work with special formats.}
procedure GetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
{ Copies pixel from memory pointed at by Pixel to Image at position [X, Y].
  Doesn't work with special formats.}
procedure SetPixelDirect(const Image: TImageData; X, Y: LongInt; Pixel: Pointer);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to 32 bit ARGB. Works for all image formats (except special)
  so it is not very fast.}
function GetPixel32(const Image: TImageData; X, Y: LongInt): TColor32Rec;
{ Procedure for setting pixel colors. Input 32 bit ARGB color is translated to
  native format and then written to Image. Works for all image formats (except special)
  so it is not very fast.}
procedure SetPixel32(const Image: TImageData; X, Y: LongInt; const Color: TColor32Rec);
{ Function for getting pixel colors. Native pixel is read from Image and
  then translated to FP ARGB. Works for all image formats (except special)
  so it is not very fast.}
function GetPixelFP(const Image: TImageData; X, Y: LongInt): TColorFPRec;
{ Procedure for setting pixel colors. Input FP ARGB color is translated to
  native format and then written to Image. Works for all image formats (except special)
  so it is not very fast.}
procedure SetPixelFP(const Image: TImageData; X, Y: LongInt; const Color: TColorFPRec); 

{ Palette Functions }

{ Allocates new palette with Entries ARGB color entries.}
procedure NewPalette(Entries: LongInt; var Pal: PPalette32);
{ Frees given palette.}
procedure FreePalette(var Pal: PPalette32);
{ Copies Count palette entries from SrcPal starting at index SrcIdx to
  DstPal at index DstPal.}
procedure CopyPalette(SrcPal, DstPal: PPalette32; SrcIdx, DstIdx, Count: LongInt);
{ Returns index of color in palette or index of nearest color if exact match
  is not found. Pal must have at least Entries color entries.}
function FindColor(Pal: PPalette32; Entries: LongInt; Color: TColor32): LongInt;
{ Creates grayscale palette where each color channel has the same value.
  Pal must have at least Entries color entries.}
procedure FillGrayscalePalette(Pal: PPalette32; Entries: LongInt);
{ Creates palette with given bitcount for each channel.
  2^(RBits + GBits + BBits) should be equl to Entries. Examples:
  (3, 3, 2) will create palette with all possible colors of R3G3B2 format
  and (8, 0, 0) will create palette with 256 shades of red.
  Pal must be allocated to at least Entries * SizeOf(TColor32Rec) bytes.}
procedure FillCustomPalette(Pal: PPalette32; Entries: LongInt; RBits, GBits,
  BBits: Byte; Alpha: Byte = $FF);
{ Swaps SrcChannel and DstChannel color or alpha channels of palette.
  Use ChannelRed, ChannelBlue, ChannelGreen, ChannelAlpha constants to
  identify channels. Pal must be allocated to at least
  Entries * SizeOf(TColor32Rec) bytes.}
procedure SwapChannelsOfPalette(Pal: PPalette32; Entries, SrcChannel,
  DstChannel: LongInt);

{ Options Functions }

{ Sets value of integer option specified by OptionId parameter.
  Option Ids are constans starting ImagingXXX.}
function SetOption(OptionId, Value: LongInt): Boolean;
{ Returns value of integer option specified by OptionId parameter. If OptionId is
  invalid, InvalidOption is returned. Option Ids are constans
  starting ImagingXXX.}
function GetOption(OptionId: LongInt): LongInt;
{ Pushes current values of all options on the stack. Returns True
  if successfull (max stack depth is 8 now). }
function PushOptions: Boolean;
{ Pops back values of all options from the top of the stack. Returns True
  if successfull (max stack depth is 8 now). }
function PopOptions: Boolean;

{ Image Format Functions }

{ Returns short information about given image format.}
function GetImageFormatInfo(Format: TImageFormat; out Info: TImageFormatInfo): Boolean;
{ Returns size in bytes of Width x Height area of pixels. Works for all formats.}
function GetPixelsSize(Format: TImageFormat; Width, Height: LongInt): LongInt;

{ IO Functions }

{ User can set his own file IO functions used when loading from/saving to
  files by this function.}
procedure SetUserFileIO(OpenProc: TOpenProc; CloseProc: TCloseProc; EofProc: TEofProc; SeekProc:
  TSeekProc; TellProc: TTellProc; ReadProc: TReadProc; WriteProc: TWriteProc);
{ Sets file IO functions to Imaging default.}
procedure ResetFileIO;

{ Raw Image IO Functions }

procedure ReadRawImageFromFile(const FileName: string; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageFromStream(Stream: TStream; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageFromMemory(Data: Pointer; DataSize: Integer; Width, Height: Integer;
  Format: TImageFormat; var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);
procedure ReadRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  var Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);

procedure WriteRawImageToFile(const FileName: string; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageToStream(Stream: TStream; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageToMemory(Data: Pointer; DataSize: Integer; const Image: TImageData;
  Offset: Integer = 0; RowLength: Integer = 0);
procedure WriteRawImageRect(Data: Pointer; Left, Top, Width, Height: Integer;
  const Image: TImageData; Offset: Integer = 0; RowLength: Integer = 0);

{ Convenience/helper Functions }

procedure ResizeImageToFit(const SrcImage: TImageData; FitWidth, FitHeight: Integer;
  Filter: TResizeFilter; var DestImage: TImageData);


{ ------------------------------------------------------------------------
                           Other Imaging Stuff
  ------------------------------------------------------------------------}

type
  { Set of TImageFormat enum.}
  TImageFormats = set of TImageFormat;

  { Record containg set of IO functions internaly used by image loaders/savers.}
  TIOFunctions = record
    Open: TOpenProc;
    Close: TCloseProc;
    Eof: TEofProc;
    Seek: TSeekProc;
    Tell: TTellProc;
    Read: TReadProc;
    Write: TWriteProc;
  end;
  PIOFunctions = ^TIOFunctions;

type
  TFileFormatFeature = (
    ffLoad,
    ffSave,
    ffMultiImage,
    ffReadOnSave,
    ffProgress,
    ffReadScanlines);

  TFileFormatFeatures = set of TFileFormatFeature;

  TMetadata = class;

  { Base class for various image file format loaders/savers which
    descend from this class. If you want to add support for new image file
    format the best way is probably to look at TImageFileFormat descendants'
    implementations that are already part of Imaging.}
{$TYPEINFO ON}
  TImageFileFormat = class
  private
    FExtensions: TStringList;
    FMasks: TStringList;
    function GetCanLoad: Boolean;
    function GetCanSave: Boolean;
    function GetIsMultiImageFormat: Boolean;
    { Does various checks and actions before LoadData method is called.}
    function PrepareLoad(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstFrame: Boolean): Boolean;
    { Processes some actions according to result of LoadData.}
    function PostLoadCheck(var Images: TDynImageDataArray; LoadResult: Boolean): Boolean;
    { Helper function to be called in SaveData methods of descendants (ensures proper
      index and sets FFirstIdx and FLastIdx for multi-images).}
    function PrepareSave(Handle: TImagingHandle; const Images: TDynImageDataArray;
      var Index: LongInt): Boolean;
    { Returns file open mode used for saving images. Depends on defined Features.}
    function GetSaveOpenMode: TOpenMode;
  protected
    FName: string;
    FFeatures: TFileFormatFeatures;
    FSupportedFormats: TImageFormats;
    FFirstIdx, FLastIdx: LongInt;
    FMetadata: TMetadata;
    { Descendants must override this method and define file format name and
      capabilities.}
    procedure Define; virtual;
    { Defines filename masks for this image file format. AMasks should be
      in format '*.ext1,*.ext2,umajo.*'.}
    procedure AddMasks(const AMasks: string);
    function GetFormatInfo(Format: TImageFormat): TImageFormatInfo;
    { Returns set of TImageData formats that can be saved in this file format
      without need for conversion.}
    function GetSupportedFormats: TImageFormats; virtual;
    { Method which must be overrided in descendants if they' are be capable
      of loading images. Images are already freed and length is set to zero
      whenever this method gets called. Also Handle is assured to be valid
      and contains data that passed TestFormat method's check.}
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstFrame: Boolean): Boolean; virtual;
    { Method which must be overriden in descendants if they are be capable
      of saving images. Images are checked to have length >0 and
      that they contain valid images. For single-image file formats
      Index contain valid index to Images array (to image which should be saved).
      Multi-image formats should use FFirstIdx and FLastIdx fields to
      to get all images that are to be saved.}
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; virtual;
    { This method is called internaly by MakeCompatible when input image
      is in format not supported by this file format. Image is clone of
      MakeCompatible's input and Info is its extended format info.}
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); virtual;
    { Returns True if given image is supported for saving by this file format.
      Most file formats don't need to override this method. It checks
      (in this base class) if Image's format is in SupportedFromats set.
      But you may override it if you want further checks
      (proper widht and height for example).}
    function IsSupported(const Image: TImageData): Boolean; virtual;
  public
    constructor Create(AMetadata: TMetadata = nil); virtual;
    destructor Destroy; override;

    { Loads images from file source.}
    function LoadFromFile(const FileName: string; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Loads images from stream source.}
    function LoadFromStream(Stream: TStream; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Loads images from memory source.}
    function LoadFromMemory(Data: Pointer; Size: LongInt;
      var Images: TDynImageDataArray; OnlyFirstLevel: Boolean = False): Boolean;

    { Saves images to file. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images (for example SaveToFile saves sequence of
      files img000.jpg, img001.jpg ....).}
    function SaveToFile(const FileName: string; const Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Saves images to stream. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images.}
    function SaveToStream(Stream: TStream; const Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean = False): Boolean;
    { Saves images to memory. If format supports only single level images and
      there are multiple images to be saved, they are saved as sequence of
      independent images. Data must be already allocated and their size passed
      as Size parameter, number of written bytes is then returned in the same
      parameter.}
    function SaveToMemory(Data: Pointer; var Size: LongInt;
      const Images: TDynImageDataArray; OnlyFirstLevel: Boolean = False): Boolean;

    { Makes Image compatible with this file format (that means it is in one
      of data formats in Supported formats set). If input is already
      in supported format then Compatible just use value from input
      (Compatible := Image) so must not free it after you are done with it
      (image bits pointer points to input image's bits).
      If input is not in supported format then it is cloned to Compatible
      and concerted to one of supported formats (which one dependeds on
      this file format). If image is cloned MustBeFreed is set to True
      to indicated that you must free Compatible after you are done with it.}
    function MakeCompatible(const Image: TImageData; var Compatible: TImageData;
      out MustBeFreed: Boolean): Boolean;
    { Returns True if data located in source identified by Handle
      represent valid image in current format.}
    function TestFormat(Handle: TImagingHandle): Boolean; virtual;
    { Resturns True if the given FileName matches filter for this file format.
      For most formats it just checks filename extensions.
      It uses filename masks in from Masks property so it can recognize
      filenames like this 'umajoXXXumajo.j0j' if one of themasks is
      'umajo*umajo.j?j'.}
    function TestFileName(const FileName: string): Boolean;
    { Descendants use this method to check if their options (registered with
      constant Ids for SetOption/GetOption interface or accessible as properties
      of descendants) have valid values and make necessary changes.}
    procedure CheckOptionsValidity; virtual;

    { Description of this format.}
    property Name: string read FName;
    { Indicates whether images in this format can be loaded.}
    property CanLoad: Boolean read GetCanLoad;
    { Indicates whether images in this format can be saved.}
    property CanSave: Boolean read GetCanSave;
    { Indicates whether images in this format can contain multiple image levels.}
    property IsMultiImageFormat: Boolean read GetIsMultiImageFormat;
    { List of filename extensions for this format.}
    property Extensions: TStringList read FExtensions;
    { List of filename masks that are used to associate filenames
      with TImageFileFormat descendants. Typical mask looks like
      '*.bmp' or 'texture.*' (supports file formats which use filename instead
      of extension to identify image files).}
    property Masks: TStringList read FMasks;
    { Set of TImageFormats supported by saving functions of this format. Images
      can be saved only in one those formats.}
    property SupportedFormats: TImageFormats read GetSupportedFormats;
  end;
{$TYPEINFO OFF}

  { Class reference for TImageFileFormat class}
  TImageFileFormatClass = class of TImageFileFormat;

  { Physical resolution unit.}
  TResolutionUnit = (
    ruSizeInMicroMeters, // value is pixel size in micrometers
    ruDpi,               // value is pixels/dots per inch
    ruDpm,               // value is pixels/dots per meter
    ruDpcm               // value is pixels/dots per centimeter
  );

  { Class for storage of single metadata item.}
  TMetadataItem = class
  public
    Id: string;
    ImageIndex: Integer;
    Value: Variant;
  end;

  { Metadata manager class.}
  TMetadata = class
  private
    FLoadMetaItems: TStringList;
    FSaveMetaItems: TStringList;
    procedure AddMetaToList(List: TStringList; const Id: string; const Value: Variant; ImageIndex: Integer);
    procedure ClearMetaList(List: TStringList);
    function GetMetaById(const Id: string): Variant;
    function GetMetaByIdMulti(const Id: string; ImageIndex: Integer): Variant;
    function GetMetaCount: Integer;
    function GetMetaByIdx(Index: Integer): TMetadataItem;
    function GetSaveMetaById(const Id: string): Variant;
    function GetSaveMetaByIdMulti(const Id: string; ImageIndex: Integer): Variant;
    procedure TranslateUnits(ResolutionUnit: TResolutionUnit; var XRes, YRes: Single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddMetaItem(const Id: string; const Value: Variant; ImageIndex: Integer = 0);
    procedure AddMetaItemForSave(const Id: string; const Value: Variant; ImageIndex: Integer = 0);
    function HasMetaItem(const Id: string; ImageIndex: Integer = 0): Boolean;
    function HasMetaItemForSave(const Id: string; ImageIndex: Integer = 0): Boolean;

    procedure ClearMetaItems;
    procedure ClearMetaItemsForSave;
    function GetMetaItemName(const Id: string; ImageIndex: Integer): string;
    { Copies loaded meta items to items-for-save stack. Use this when you want to
      save metadata that have been just loaded (e.g. resaving image in
      different file format but keeping the metadata).}
    procedure CopyMetaItems;

    function GetPhysicalPixelSize(ResUnit: TResolutionUnit; var XSize,
      YSize: Single; MetaForSave: Boolean = False; ImageIndex: Integer = 0): Boolean;
    procedure SetPhysicalPixelSize(ResUnit: TResolutionUnit; XSize, YSize: Single;
      MetaForSave: Boolean = False; ImageIndex: Integer = 0);

    property MetaItems[const Id: string]: Variant read GetMetaById;
    property MetaItemsMulti[const Id: string; ImageIndex: Integer]: Variant read GetMetaByIdMulti;
    { Number of loaded metadata items.}
    property MetaItemCount: Integer read GetMetaCount;
    property MetaItemsByIdx[Index: Integer]: TMetadataItem read GetMetaByIdx;
    property MetaItemsForSave[const Id: string]: Variant read GetSaveMetaById;
    property MetaItemsForSaveMulti[const Id: string; ImageIndex: Integer]: Variant read GetSaveMetaByIdMulti;
  end;

const
  { Metadata item id constants }

  { Physical size of one pixel in micrometers. Type of value is Float.}
  SMetaPhysicalPixelSizeX = 'PhysicalPixelSizeX';
  SMetaPhysicalPixelSizeY = 'PhysicalPixelSizeY';
  { Delay for frame of animation (how long it should stay visible) in milliseconds.
    Type of value is Integer.}
  SMetaFrameDelay = 'FrameDelay';
  { Number of times animation should be looped (0 = infinite looping). Type is Int. }
  SMetaAnimationLoops = 'AnimationLoops';
  { Gamma correction value. Type is Float.}
  SMetaGamma = 'Gamma';
  { Exposure value for HDR etc. Type is Float.}
  SMetaExposure = 'Exposure';
  { EXIF image metadata raw blob.}
  SMetaExifBlob = 'ExifBlob';
  { XMP image metadata raw blob.}
  SMetaXmpBlob  = 'XmpBlob';
  { IPTC image metadata raw blob.}
  SMetaIptcBlob = 'IptcBlob';

var
  GlobalMetadata: TMetadata;

{ Returns symbolic name of given format.}
function GetFormatName(Format: TImageFormat): string;
{ Returns string with information about given Image.}
function ImageToStr(const Image: TImageData): string;
{ Returns Imaging version string in format 'Major.Minor.Patch'.}
function GetVersionStr: string;
{ If Condition is True then TruePart is retured, otherwise FalsePart is returned.}
function IffFormat(Condition: Boolean; const TruePart, FalsePart: TImageFormat): TImageFormat;

{ Registers new option so it can be used by SetOption and GetOption functions.
  Returns True if registration was succesful - that is Id is valid and is
  not already taken by another option.}
function RegisterOption(OptionId: LongInt; Variable: PLongInt): Boolean;

{ Registers new image loader/saver so it can be used by LoadFrom/SaveTo
  functions.}
procedure RegisterImageFileFormat(AClass: TImageFileFormatClass);
{ Returns image format loader/saver according to given extension
  or nil if not found.}
function FindImageFileFormatByExt(const Ext: string): TImageFileFormat;
{ Returns image format loader/saver according to given filename
  or nil if not found.}
function FindImageFileFormatByName(const FileName: string): TImageFileFormat;
{ Returns image format loader/saver based on its class
  or nil if not found or not registered.}
function FindImageFileFormatByClass(AClass: TImageFileFormatClass): TImageFileFormat;
{ Returns number of registered image file format loaders/saver.}
function GetFileFormatCount: LongInt;
{ Returns image file format loader/saver at given index. Index must be
  in range [0..GetFileFormatCount - 1] otherwise nil is returned.}
function GetFileFormatAtIndex(Index: LongInt): TImageFileFormat;
{ Returns filter string for usage with open and save picture dialogs
  which contains all registered image file formats.
  Set OpenFileFilter to True if you want filter for open dialog
  and to False if you want save dialog filter (formats that cannot save to files
  are not added then).
  For open dialog filter for all known graphic files
  (like All(*.jpg;*.png;....) is added too at the first index.}
function GetImageFileFormatsFilter(OpenFileFilter: Boolean): string;
{ Returns file extension (without dot) of image format selected
  by given filter index. Used filter string is defined by GetImageFileFormatsFilter
  function. This function can be used with save dialogs (with filters created
  by GetImageFileFormatsFilter) to get the extension of file format selected
  in dialog quickly. Index is in range 1..N (as FilterIndex property
  of TOpenDialog/TSaveDialog)}
function GetFilterIndexExtension(Index: LongInt; OpenFileFilter: Boolean): string;
{ Returns filter index of image file format of file specified by FileName. Used filter
  string is defined by GetImageFileFormatsFilter function.
  Returned index is in range 1..N (as FilterIndex property of TOpenDialog/TSaveDialog)}
function GetFileNameFilterIndex(const FileName: string; OpenFileFilter: Boolean): LongInt;

{ Returns current IO functions.}
function GetIO: TIOFunctions;
{ Raises EImagingError with given message.}
procedure RaiseImaging(const Msg: string; const Args: array of const); overload;
procedure RaiseImaging(const Msg: string); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

const
  SImagingLibTitle = 'Vampyre Imaging Library';

