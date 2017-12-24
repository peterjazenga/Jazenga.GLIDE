{**********************************************************************
 Package pl_ExDesign
 This unit is part of CodeTyphon Studio (http://www.pilotlogic.com/)
***********************************************************************}

unit ELDsgxClip;

{$mode objfpc}{$H+}

interface

uses

  LCLProc, LCLType, LResources, LCLIntf, LMessages, Classes;

type
  TplDesignComponentClipboard = class(TObject)
  protected
    Stream: TMemoryStream;
    FParentComponent: TComponent;
    procedure Close;
    procedure Open;
    procedure ReadError(Reader: TReader; const Msg: string; var Handled: Boolean);
  public
    constructor Create(ParentComponent: TComponent);

    function GetComponent: TComponent;
    procedure CloseRead;
    procedure CloseWrite;
    procedure OpenRead;
    procedure OpenWrite;
    procedure SetComponent(InComponent: TComponent);
  end;

function DesignLoadComponentFromBinaryStream(InStream: TStream;
  InComponent: TComponent; InOnError: TReaderError): TComponent;
procedure DesignSaveComponentToBinaryStream(InStream: TStream; InComponent: TComponent);
procedure DesignCopyStreamFromClipboard(InFmt: Cardinal; InS: TStream);
procedure DesignCopyStreamToClipboard(InFmt: Cardinal; InS: TStream);


implementation

uses
  SysUtils, Clipbrd,
  ELDsgxUtils;

var
  CF_COMPONENTSTREAM: UINT;

procedure DesignSaveComponentToBinaryStream(InStream: TStream; InComponent: TComponent);
var
  MS: TMemoryStream;
  SZ: Int64;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(InComponent);
    MS.Position := 0;
    SZ := MS.Size;
    InStream.Write(SZ, SizeOf(SZ));
    InStream.CopyFrom(MS, SZ);
  finally
    MS.Free;
  end;
end;

function DesignLoadComponentFromBinaryStream(InStream: TStream;
  InComponent: TComponent; InOnError: TReaderError): TComponent;
var
  MS: TMemoryStream;
  SZ: Int64;
begin
  InStream.Read(SZ, SizeOf(SZ));
  MS := TMemoryStream.Create;
  try
    MS.CopyFrom(InStream, SZ);
    MS.Position := 0;
    with TReader.Create(MS, 4096) do
    try
      Parent := InComponent;
      OnError := InOnError;
      Result := ReadRootComponent(nil);
    finally
      Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure DesignCopyStreamToClipboard(InFmt: Cardinal; InS: TStream);
var
  HMem: THandle;
  PMem: Pointer;
begin
  Clipboard.Open;
  Clipboard.AddFormat( InFmt, InS);
  Clipboard.Close;

end;

procedure DesignCopyStreamFromClipboard(InFmt: Cardinal; InS: TStream);
var
  HMem: THandle;
  PMem: Pointer;
begin
  Clipboard.GetFormat(InFmt, InS);
end;

//=== { TplDesignComponentClipboard } ========================================

procedure TplDesignComponentClipboard.Close;
begin
  Stream.Free;
  Clipboard.Close;
end;

procedure TplDesignComponentClipboard.CloseRead;
begin
  Close;
end;

procedure TplDesignComponentClipboard.CloseWrite;
begin
  DesignCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
  Close;
end;

constructor TplDesignComponentClipboard.Create(ParentComponent: TComponent);
begin
  inherited Create;

  FParentComponent := ParentComponent;
end;

function TplDesignComponentClipboard.GetComponent: TComponent;
begin
  if Stream.Position < Stream.Size then
    Result := DesignLoadComponentFromBinaryStream(Stream, FParentComponent,
    TReaderError( @ReadError))
  else
    Result := nil;
end;

procedure TplDesignComponentClipboard.Open;
begin
  Clipboard.Open;
  Stream := TMemoryStream.Create;
end;

procedure TplDesignComponentClipboard.OpenRead;
begin
  Open;
  DesignCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
end;

procedure TplDesignComponentClipboard.OpenWrite;
begin
  Open;
end;

procedure TplDesignComponentClipboard.ReadError(Reader: TReader;
  const Msg: string; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TplDesignComponentClipboard.SetComponent(InComponent: TComponent);
begin
  DesignSaveComponentToBinaryStream(Stream, InComponent);
end;

initialization
  { The following string should not be localized }
  CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');


end.
