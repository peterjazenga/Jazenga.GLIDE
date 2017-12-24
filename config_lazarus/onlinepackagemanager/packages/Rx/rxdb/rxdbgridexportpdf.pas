{ RxDBGridExportPdf unit

  Copyright (C) 2005-2017 Lagunov Aleksey alexs75@yandex.ru and Lazarus team
  original conception from rx library for Delphi (c)

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit RxDBGridExportPdf;

{$mode objfpc}{$H+}

interface

{$IF (FPC_FULLVERSION >= 30101)}
uses
  Classes, SysUtils, DB, rxdbgrid, rxvclutils, Graphics, fpPDF, contnrs,  fpparsettf,
  fpTTF;

type

  TRxDBGridExportPdfOption = (repExportTitle,
    repExportColors,
    repExportFooter,
    repOverwriteExisting,
    repExportImages
    );
  TRxDBGridExportPdfOptions = set of TRxDBGridExportPdfOption;

  { TPdfExportOptions }

  TPdfExportOptions = class(TPersistent)
  private
    FOwner: TPersistent;
    FOptions: TPDFOptions;
    FPaperOrientation: TPDFPaperOrientation;
    FPaperType: TPDFPaperType;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent);
  published
    property PaperType:TPDFPaperType read FPaperType write FPaperType default ptA4;
    property PaperOrientation:TPDFPaperOrientation read FPaperOrientation write FPaperOrientation default ppoPortrait;
    property Options:TPDFOptions read FOptions write FOptions;
  end;

type
  TRxDBGridExportPDF = class;
  TExportFonts = class;

  { TExportFontItem }

  TExportFontItem = class
  private
    FFontColor: TColor;
    FFontName: string;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FOwner:TExportFonts;
    FDefaultFont: boolean;
    //
    FPdfFont:integer;
    FTTFFontInfo: TFPFontCacheItem;
    function GetBold: boolean;
    function GetItalic: boolean;
    procedure SetFontSize(AValue: Integer);
  public
    constructor Create(AOwner:TExportFonts; AFontName:string; AFontStyle: TFontStyles);
    destructor Destroy; override;
    procedure Activate;
    property FontStyle: TFontStyles read FFontStyle;
    property FontSize:Integer read FFontSize write SetFontSize;
    property FontColor:TColor read FFontColor write FFontColor;
    property Bold:boolean read GetBold;
    property Italic:boolean read GetItalic;
    property DefaultFont:boolean read FDefaultFont;
    property FontName:string read FFontName;
  end;

  { TExportFonts }

  TExportFonts = class
  private
    FDefaultFontBold: TExportFontItem;
    FDefaultFontNormal: TExportFontItem;
    FOwner:TRxDBGridExportPDF;
    FList:TFPList;
    function GetCount: integer;
    function GetItem(Index: integer): TExportFontItem;
  public
    constructor Create(AOwner:TRxDBGridExportPDF);
    destructor Destroy; override;
    procedure Clear;
    function AddItem(AFontName: string; AFontStyle:TFontStyles = []): TExportFontItem;
    function FindItem(AFontName: string; AFontStyle:TFontStyles = []):TExportFontItem;
    property DefaultFontNormal:TExportFontItem read FDefaultFontNormal;
    property DefaultFontBold:TExportFontItem read FDefaultFontBold;
    property Count:integer read GetCount;
    property Item[Index:integer]:TExportFontItem read GetItem;
  end;


  { TRxDBGridExportPDF }

  TRxDBGridExportPDF = class(TRxDBGridAbstractTools)
  private
    FPageMargin: TRxPageMargin;
    FPageHeight:integer;
    FPageWidth:integer;

    FAuthorPDF: string;
    FFileName: string;
    FOpenAfterExport: boolean;
    FOptions: TRxDBGridExportPdfOptions;
    FProducerPDF: string;
    FPdfOptions:TPdfExportOptions;
    FCurPage: TPDFPage;
    FTitleColor: TColor;
    FWorkPages:TFPList;
    FWorkPagesNeedCount:integer;

    FFontItems:TExportFonts;

    function GetPdfOptions: TPdfExportOptions;
    procedure SetPageMargin(AValue: TRxPageMargin);
    procedure SetPdfOptions(AValue: TPdfExportOptions);
    function ActivateFont(AFont:TFont; AOwnerFont:TFont):TExportFontItem;
  protected
    FPDFDocument:TPDFDocument;
    FCurSection: TPDFSection;
    FDataSet:TDataSet;
    FPosY : integer;

    procedure InitFonts;
    procedure DoSetupDocHeader;
    procedure DoSetupFonts;
    //
    procedure WriteTextRect(AExportFont:TExportFontItem; X, Y, W, H:integer; AText:string; ATextAlign:TAlignment);
    procedure DrawRect(X, Y, W, H: integer; ABorderColor, AFillColor: TColor);
    procedure DrawImage(X, Y, W, H: integer; ABmp:TBitmap; ATextAlign:TAlignment);

    procedure StartNewPage;

    procedure DoExportPage;
    procedure DoExportTitle;
    procedure DoExportBody;
    procedure DoExportFooter;
    procedure DoSaveDocument;

    function DoExecTools:boolean;override;
    function DoSetupTools:boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName:string read FFileName write FFileName;
    property Options:TRxDBGridExportPdfOptions read FOptions write FOptions;
    property PdfOptions:TPdfExportOptions read GetPdfOptions write SetPdfOptions;
    property OpenAfterExport:boolean read FOpenAfterExport write FOpenAfterExport default false;
    property AuthorPdf:string read FAuthorPDF write FAuthorPDF;
    property ProducerPdf:string read FProducerPDF write FProducerPDF;
    property PageMargin:TRxPageMargin read FPageMargin write SetPageMargin;
    property TitleColor:TColor read FTitleColor write FTitleColor default clSilver;
  end;

  {$ENDIF}
implementation

{$IF (FPC_FULLVERSION >= 30101)}
uses Grids, rxdconst, FileUtil, Forms, Controls, LCLIntf, LazFileUtils, FPReadBMP, RxDBGridExportPdfSetupUnit;

const
  cInchToMM = 25.4;
function ConvetUnits(AUnits:TPDFFloat):TPDFFloat; inline;
begin
  Result := (AUnits * cInchToMM) / gTTFontCache.DPI;
end;

function ColorToDdfColor(C:TColor):TARGBColor;
var
  A:array [1..4] of byte absolute C;
begin
  if C = clWindow then
    Result:=clWhite
  else
    Result:={A[1] shl 24 +} A[1] shl 16 + A[2] shl 8 + A[3];
end;

type
  THackExDBGrid = class(TRxDBGrid);

{ TExportFonts }

function TExportFonts.GetCount: integer;
begin
  Result:=FList.Count;
end;

function TExportFonts.GetItem(Index: integer): TExportFontItem;
begin
  Result:=TExportFontItem(FList[Index]);
end;

constructor TExportFonts.Create(AOwner: TRxDBGridExportPDF);
begin
  inherited Create;
  FOwner:=AOwner;
  FList:=TFPList.Create;
end;

destructor TExportFonts.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TExportFonts.Clear;
var
  I: Integer;
begin
  for I:=0 to FList.Count-1 do
    TExportFontItem(FList[i]).Free;
  FList.Clear;
end;

function TExportFonts.AddItem(AFontName: string; AFontStyle: TFontStyles
  ): TExportFontItem;
var
  S1, S2, S3: String;
begin
  Result:=FindItem(AFontName, AFontStyle);
  if Assigned(Result) then exit;

  Result:=TExportFontItem.Create(Self, AFontName, AFontStyle);

  S1:=ExtractFileDir(Result.FTTFFontInfo.FileName);
  S2:=ExtractFileName(Result.FTTFFontInfo.FileName);
  S3:=AFontName;

  FOwner.FPDFDocument.FontDirectory:=S1;

  Result.FPdfFont:=FOwner.FPDFDocument.AddFont(S2, S3);
end;

function TExportFonts.FindItem(AFontName: string; AFontStyle: TFontStyles
  ): TExportFontItem;
var
  K: TExportFontItem;
  i: Integer;
begin
  Result:=nil;

  if AFontName = 'default' then
  begin
    if Graphics.fsBold in AFontStyle then
      Result:=FDefaultFontBold
    else
      Result:=FDefaultFontNormal;
  end
  else
  begin
    for i:=0 to FList.Count-1 do
    begin
      K:=TExportFontItem(FList[i]);
      if (K.FontName = AFontName) and (K.FontStyle = AFontStyle) then
      begin
        Result:=K;
        exit;
      end
    end;
  end;
end;

{ TExportFontItem }

function TExportFontItem.GetBold: boolean;
begin
  Result:=Graphics.fsBold in FFontStyle;
end;

function TExportFontItem.GetItalic: boolean;
begin
  Result:=Graphics.fsItalic in FFontStyle;
end;

procedure TExportFontItem.SetFontSize(AValue: Integer);
begin
  if AValue = 0 then
    FFontSize:=10
  else
    FFontSize:=AValue;
end;

constructor TExportFontItem.Create(AOwner: TExportFonts; AFontName: string;
  AFontStyle: TFontStyles);
begin
  inherited Create;
  FOwner:=AOwner;
  FOwner.FList.Add(Self);
  FFontStyle:=AFontStyle;
  FFontName:=AFontName;
  FTTFFontInfo:=gTTFontCache.Find(AFontName, Graphics.fsBold in AFontStyle, Graphics.fsItalic in AFontStyle);
  if not Assigned(FTTFFontInfo) then
    raise Exception.CreateFmt('fpTTF:in gTTFontCache not found font "%s" info.', [AFontName]);
end;

destructor TExportFontItem.Destroy;
begin
  inherited Destroy;
end;

procedure TExportFontItem.Activate;
begin
  FOwner.FOwner.FCurPage.SetFont(FPdfFont, FontSize);
  FOwner.FOwner.FCurPage.SetColor(ColorToDdfColor(FontColor), false);
end;

{ TPdfExportOptions }

procedure TPdfExportOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TPdfExportOptions then
  begin
    TPdfExportOptions(Dest).FOptions := FOptions;
    TPdfExportOptions(Dest).FPaperOrientation:=FPaperOrientation;
    TPdfExportOptions(Dest).FPaperType:=FPaperType;
  end
  else
  inherited AssignTo(Dest);
end;

constructor TPdfExportOptions.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner:=AOwner;
  FPaperType:=ptA4;
  FPaperOrientation:=ppoPortrait;
end;

{ TRxDBGridExportSpreadSheet }

function TRxDBGridExportPDF.GetPdfOptions: TPdfExportOptions;
begin
  Result:=FPdfOptions;
end;

procedure TRxDBGridExportPDF.SetPageMargin(AValue: TRxPageMargin);
begin
  FPageMargin.Assign(AValue);
end;

procedure TRxDBGridExportPDF.SetPdfOptions(AValue: TPdfExportOptions);
begin
  FPdfOptions.Assign(AValue);
end;

function TRxDBGridExportPDF.ActivateFont(AFont: TFont; AOwnerFont: TFont
  ): TExportFontItem;
begin
  Result:=FFontItems.FindItem(AFont.Name, AFont.Style);
{  if not Assigned(Result) then
    Result:=SelectFont(AOwnerFont);
  if not Assigned(Result) then
    Result:=FFontItems.FDefaultFontNormal;
}
  if Assigned(Result) then
  begin
    Result.FontSize:=AFont.Size;
    Result.FontColor:=AFont.Color;
    Result.Activate
  end
  else
    raise Exception.CreateFmt('Font "%s" not found', [AFont.Name]);
end;

procedure TRxDBGridExportPDF.WriteTextRect(AExportFont: TExportFontItem; X, Y,
  W, H: integer; AText: string; ATextAlign: TAlignment);
var
  FTW, FTH, FTH1, FTH2: Single;
  X1: TPDFFloat;
  Y1, fX, fY: TPDFFloat;
  fW, fH: Extended;
begin

  fX := ConvetUnits(X);
  fY := ConvetUnits(Y);
  fW := ConvetUnits(W);
  fH := ConvetUnits(H);

  //Calc text width
  FTW:=ConvetUnits(AExportFont.FTTFFontInfo.TextWidth(AText, AExportFont.FontSize));
  //Calc text height
  FTH1 := AExportFont.FTTFFontInfo.FontData.CapHeight * AExportFont.FontSize * gTTFontCache.DPI / (72 * AExportFont.FTTFFontInfo.FontData.Head.UnitsPerEm);
  FTH2 := Abs(AExportFont.FTTFFontInfo.FontData.Descender) * AExportFont.FontSize * gTTFontCache.DPI / (72 * AExportFont.FTTFFontInfo.FontData.Head.UnitsPerEm);
  FTH :=  (FTH1 * 25.4) / gTTFontCache.DPI + (FTH2 * 25.4) / gTTFontCache.DPI;


  case ATextAlign of
    taLeftJustify:
      begin
        Y1:=fY - FTH2;
        X1:=fX + ConvetUnits(constCellPadding);
      end;
    taRightJustify:
      begin
        Y1:=fY - FTH2;
        X1:=fX + fW - FTW - ConvetUnits(constCellPadding);
        if X1 < fX then
          X1:=fX;
      end;
    taCenter:
      begin
        Y1:=fY - FTH2;
        X1:=fX + fW / 2 - FTW / 2 - ConvetUnits(constCellPadding);
        if X1 < fX then
          X1:=fX;
      end;
  end;

  FCurPage.WriteText(X1, Y1 + fH, AText);
end;

procedure TRxDBGridExportPDF.DrawRect(X, Y, W, H: integer; ABorderColor,
  AFillColor: TColor);
var
  fX, fY, fW, fH: Extended;
begin
  if (AFillColor = clNone) and (ABorderColor = clNone) then exit;

  if ABorderColor <> clNone then
    FCurPage.SetColor(ColorToDdfColor(ABorderColor), true);

  if (AFillColor <> clNone) and (repExportColors in FOptions) then
    FCurPage.SetColor(ColorToDdfColor(AFillColor), false);

  fW:= ConvetUnits(W);
  fH:= ConvetUnits(H);
  fX:= ConvetUnits(X);
  fY:= ConvetUnits(Y) + fH;

  FCurPage.DrawRect(fX, fY, fW, fH, 1, (AFillColor <> clNone) and (repExportColors in FOptions), (ABorderColor <> clNone));
end;

procedure TRxDBGridExportPDF.DrawImage(X, Y, W, H: integer; ABmp: TBitmap;
  ATextAlign: TAlignment);
var
  S:TMemoryStream;
  IDX: Integer;
  fW, fH, fX, fY, X1, Y1, fW1, fH1: TPDFFloat;
begin
  S:=TMemoryStream.Create;
  try
    ABmp.SaveToStream(S);
    S.Position:=0;
    IDX := FPDFDocument.Images.AddFromStream(S, TFPReaderBMP, False);
    fW1 := ConvetUnits(FPDFDocument.Images[IDX].Width);
    fH1 := ConvetUnits(FPDFDocument.Images[IDX].Height);
    fX:=ConvetUnits(X);
    fY:=ConvetUnits(Y + constCellPadding);
    fW:=ConvetUnits(W);
    fH:=ConvetUnits(H);

    case ATextAlign of
      taLeftJustify:
        begin
          Y1:=fY;
          X1:=fX + ConvetUnits(constCellPadding);
        end;
      taRightJustify:
        begin
          Y1:=fY;
          X1:=fX + fW - fW1 - ConvetUnits(constCellPadding);
          if X1 < fX then
            X1:=fX;
        end;
      taCenter:
        begin
          Y1:=fY;
          X1:=fX + fW / 2 - fW1 / 2 - ConvetUnits(constCellPadding);
          if X1 < fX then
            X1:=fX;
        end;
    end;

    Y1:=Y1 + fW1;
    FCurPage.DrawImage(X1, Y1, fW1, fH1, IDX);  // left-bottom coordinate of image

  finally
    S.Free;
  end;
end;

procedure TRxDBGridExportPDF.StartNewPage;
var
  P: TPDFPage;
  i: Integer;
begin
  FWorkPages.Clear;
  for i:=0 to FWorkPagesNeedCount - 1 do
  begin
    P := FPDFDocument.Pages.AddPage;
    P.PaperType := FPdfOptions.PaperType;
    //P.UnitOfMeasure := uomPixels;
    P.UnitOfMeasure := uomMillimeters; //normal work only whis mm ??
    FCurSection.AddPage(P);
    FWorkPages.Add(P);
  end;

  FPosY:=FPageMargin.Top;
end;

procedure TRxDBGridExportPDF.DoExportTitle;
var
  i, X, CP, K, KY, TH1, X1, W1, WNext: Integer;
  C, FStartCol: TRxColumn;
  CT: TRxColumnTitle;
  H: LongInt;
  KL: TMLCaptionItem;
begin
  X:=FPageWidth + FPageMargin.Right;
  H:=THackExDBGrid(FRxDBGrid).RowHeights[0];
  CP:=-1;
  FCurPage:=nil;
  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
        FStartCol:=C;
      end;

      CT:=C.Title as TRxColumnTitle;
      if CT.CaptionLinesCount > 0 then
      begin
        KY:=FPosY;
        for K:=0 to CT.CaptionLinesCount - 1 do
        begin
          TH1:=CT.CaptionLine(K).Height * RxDBGrid.DefaultRowHeight;
          if K < CT.CaptionLinesCount-1 then
          begin

            if i < FRxDBGrid.Columns.Count-1 then
              WNext:=FRxDBGrid.Columns[i+1].Width
            else
              WNext:=0;

            if (not Assigned(CT.CaptionLine(K).Next)) or (X + C.Width + WNext > FPageWidth - FPageMargin.Right) then
            begin
              KL:=CT.CaptionLine(K);
              X1:=X;
              W1:=C.Width;
              while Assigned(KL.Prior) and (KL.Col <> FStartCol) do
              begin
                KL:=KL.Prior;
                X1:=X1 - KL.Col.Width;
                W1:=W1 + KL.Col.Width;
              end;

              DrawRect(X1, KY, W1, TH1, FRxDBGrid.BorderColor, FTitleColor);
              WriteTextRect(ActivateFont(C.Title.Font, FRxDBGrid.TitleFont), X1, KY, W1, TH1, CT.CaptionLine(K).Caption, C.Title.Alignment);
            end;
            KY:=KY + TH1;
          end
          else
          begin
            DrawRect(X, KY, C.Width, FPosY + H - KY, FRxDBGrid.BorderColor, FTitleColor);
            WriteTextRect(ActivateFont(C.Title.Font, FRxDBGrid.TitleFont), X, KY, C.Width, FPosY + H - KY, CT.CaptionLine(K).Caption, C.Title.Alignment);
          end;
        end;
      end
      else
      begin
        DrawRect(X, FPosY, C.Width, H, FRxDBGrid.BorderColor, FTitleColor);
        WriteTextRect(ActivateFont(C.Title.Font, FRxDBGrid.TitleFont), X, FPosY, C.Width, H, C.Title.Caption, C.Title.Alignment);
      end;
      X:=X + C.Width;
    end;
  end;

  Inc(FPosY, H); // DefaultRowHeight);
end;

procedure TRxDBGridExportPDF.DoExportBody;
procedure DoWriteRow;
var
  i, X, CP: Integer;
  C: TRxColumn;
  B: TBitmap;
  AImageIndex: LongInt;
begin
  X:=FPageWidth + FPageMargin.Right;
  CP:=-1;
  FCurPage:=nil;


  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
      end;

      DrawRect(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, FRxDBGrid.BorderColor, C.Color);

      if Assigned(C.Field) then
      begin
        if (repExportImages in FOptions) and Assigned(C.ImageList) then
        begin
          AImageIndex := StrToIntDef(C.KeyList.Values[C.Field.AsString], C.NotInKeyListIndex);
          if (AImageIndex > -1) and (AImageIndex < C.ImageList.Count) then
          begin
            B:=TBitmap.Create;
            try
              B.Width:=C.ImageList.Width;
              B.Height:=C.ImageList.Height;
              B.Canvas.Brush.Color:=clWhite;
              B.Canvas.FillRect(0, 0, B.Width, B.Height);

              C.ImageList.StretchDraw(B.Canvas, AImageIndex, Rect(0, 0,  B.Width, B.Height));
              DrawImage(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, B, C.Alignment);
            finally
              B.Free
            end;
          end
        end
        else
          WriteTextRect(ActivateFont(C.Font, FRxDBGrid.Font), X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, C.Field.DisplayText, C.Alignment);
      end;

      X:=X + C.Width;
    end;
  end;
end;

begin
  while not FDataSet.EOF do
  begin
    DoWriteRow;
    FDataSet.Next;
    Inc(FPosY, FRxDBGrid.DefaultRowHeight);
    if FPosY > FPageHeight - FPageMargin.Bottom then
      exit;
  end;
end;

procedure TRxDBGridExportPDF.DoSetupFonts;
//Find default font name
function DefFontName:string;
const
  DefFontNames : array [1..3] of string =
     ('Liberation Sans', 'Arial', 'FreeSans');
var
  i: Integer;
begin
  for i:=1 to 3 do
    if Assigned(gTTFontCache.Find(DefFontNames[i], false, false)) then
    begin
      Result:=DefFontNames[i];
      exit;
    end;
  raise Exception.Create('Not found Sans font');
end;

var
  i: Integer;
  sDefFontName:string;
begin
  InitFonts;
  sDefFontName:=DefFontName;
  FFontItems.FDefaultFontNormal:=FFontItems.AddItem(sDefFontName, []);
  FFontItems.FDefaultFontBold:=FFontItems.AddItem(sDefFontName, [Graphics.fsBold]);

  for i:=0 to FRxDBGrid.Columns.Count-1 do
  begin
    if FRxDBGrid.Columns[i].Font.Name <> 'default' then
      FFontItems.AddItem(FRxDBGrid.Columns[i].Font.Name, FRxDBGrid.Columns[i].Font.Style);

    if FRxDBGrid.Columns[i].Footer.Font.Name <> 'default' then
      FFontItems.AddItem(FRxDBGrid.Columns[i].Footer.Font.Name, FRxDBGrid.Columns[i].Footer.Font.Style);

    if FRxDBGrid.Columns[i].Title.Font.Name <> 'default' then
      FFontItems.AddItem(FRxDBGrid.Columns[i].Title.Font.Name, FRxDBGrid.Columns[i].Title.Font.Style);
  end;
end;

procedure TRxDBGridExportPDF.DoExportFooter;

procedure WriteFooterRow(AFooterRow:Integer);
var
  i, X, CP: Integer;
  S: String;
  C: TRxColumn;
begin
  X:=FPageWidth + FPageMargin.Right;
  CP:=-1;
  FCurPage:=nil;

  for i:=0 to FRxDBGrid.Columns.Count - 1 do
  begin
    C:=FRxDBGrid.Columns[i];
    if C.Visible then
    begin
      if X + C.Width > FPageWidth - FPageMargin.Right then
      begin
        Inc(CP);
        FCurPage:=TPDFPage(FWorkPages[CP]);
        X:=FPageMargin.Left;
      end;

      DrawRect(X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, FRxDBGrid.BorderColor, FRxDBGrid.FooterOptions.Color);

      if FRxDBGrid.FooterOptions.RowCount = 1 then
        S:=C.Footer.DisplayText
      else
      begin
        if C.Footers.Count > AFooterRow then
          S:=C.Footers[AFooterRow].DisplayText
        else
          S:='';
      end;

      if (S<>'') then
        WriteTextRect(ActivateFont(C.Footer.Font, FRxDBGrid.Font), X, FPosY, C.Width, FRxDBGrid.DefaultRowHeight, S, C.Footer.Alignment);

      X:=X + C.Width;
    end;
  end;
  Inc(FPosY, FRxDBGrid.DefaultRowHeight);
end;

var
  j: Integer;
begin
  if FRxDBGrid.FooterOptions.RowCount = 1 then
    WriteFooterRow(1)
  else
  begin
    for j:=0 to FRxDBGrid.FooterOptions.RowCount-1 do
    begin
      if FPosY > FPageHeight - FPageMargin.Bottom then
        StartNewPage;
      WriteFooterRow(j);
    end;
  end;
end;

procedure TRxDBGridExportPDF.DoSetupDocHeader;
var
  W, i: Integer;
  C: TRxColumn;
begin
  FPDFDocument.Infos.Title := Application.Title;
  FPDFDocument.Infos.Author := FAuthorPDF;
  FPDFDocument.Infos.Producer := FProducerPDF;
  FPDFDocument.Infos.ApplicationName := ApplicationName;
  FPDFDocument.Infos.CreationDate := Now;

  FPDFDocument.Options:=FPdfOptions.FOptions;
  FPDFDocument.DefaultOrientation:=FPdfOptions.PaperOrientation;

  //calc need count pages for all columns
  FWorkPagesNeedCount:=1;
  if FPdfOptions.FPaperType <> ptCustom then
  begin
    if FPdfOptions.PaperOrientation = ppoPortrait then
    begin
      FPageWidth := PDFPaperSizes[FPdfOptions.FPaperType, 1];
      FPageHeight := PDFPaperSizes[FPdfOptions.FPaperType, 0];
    end
    else
    begin
      FPageWidth := PDFPaperSizes[FPdfOptions.FPaperType, 0];
      FPageHeight := PDFPaperSizes[FPdfOptions.FPaperType, 1];
    end;

    W:=FPageWidth + FPageMargin.Right;
    FWorkPagesNeedCount:=0;
    for i:=0 to FRxDBGrid.Columns.Count - 1 do
    begin
      C:=FRxDBGrid.Columns[i];
      if C.Visible then
      begin
        if W + C.Width > FPageWidth - FPageMargin.Right then
        begin
          Inc(FWorkPagesNeedCount);
          W:=FPageMargin.Left;
        end;
        W:=W + C.Width;
      end;
    end;
  end;
end;

procedure TRxDBGridExportPDF.DoExportPage;
begin
  StartNewPage;

  if repExportTitle in FOptions then
    DoExportTitle;
  DoExportBody;
end;

function TRxDBGridExportPDF.DoExecTools: boolean;
var
  P: TBookMark;
begin
  Result:=false;
  FDataSet:=FRxDBGrid.DataSource.DataSet;
  FDataSet.DisableControls;
  {$IFDEF NoAutomatedBookmark}
  P:=FDataSet.GetBookmark;
  {$ELSE}
  P:=FDataSet.Bookmark;
  {$ENDIF}

  FPDFDocument:=TPDFDocument.Create(nil);
  FFontItems:=TExportFonts.Create(Self);
  FWorkPages:=TFPList.Create;
  try
    DoSetupFonts;
    DoSetupDocHeader;
    FPDFDocument.StartDocument;
    FCurSection := FPDFDocument.Sections.AddSection; // we always need at least one section
    FDataSet.First;
    repeat
      DoExportPage;
    until FDataSet.EOF;

    if repExportFooter in FOptions then
    begin
      if FPosY > FPageHeight - FPageMargin.Bottom then
        StartNewPage;

      DoExportFooter;
    end;

    //DoTest; //!!!!

    DoSaveDocument;
    Result:=true;
  finally
    {$IFDEF NoAutomatedBookmark}
    FDataSet.GotoBookmark(P);
    FDataSet.FreeBookmark(P);
    {$ELSE}
    FDataSet.Bookmark:=P;
    {$ENDIF}
    FDataSet.EnableControls;

    FreeAndNil(FWorkPages);
    FreeAndNil(FPDFDocument);
    FreeAndNil(FFontItems);
  end;

  if Result and FOpenAfterExport then
    OpenDocument(FileName);
end;

function TRxDBGridExportPDF.DoSetupTools: boolean;
begin
  RxDBGridExportPdfSetupForm:=TRxDBGridExportPdfSetupForm.Create(Application);
  RxDBGridExportPdfSetupForm.FileNameEdit1.FileName:=FileName;
  RxDBGridExportPdfSetupForm.cbOpenAfterExport.Checked:=FOpenAfterExport;
  RxDBGridExportPdfSetupForm.cbExportColumnHeader.Checked:=repExportTitle in FOptions;
  RxDBGridExportPdfSetupForm.cbExportColumnFooter.Checked:=repExportFooter in FOptions;
  RxDBGridExportPdfSetupForm.cbExportCellColors.Checked:=repExportColors in FOptions;
  RxDBGridExportPdfSetupForm.CheckBox6.Checked:=repExportImages in FOptions;
  RxDBGridExportPdfSetupForm.ColorBox1.Selected:=FTitleColor;

  RxDBGridExportPdfSetupForm.RadioGroup1.ItemIndex:=Ord(FPdfOptions.PaperOrientation = ppoLandscape);
  RxDBGridExportPdfSetupForm.ComboBox1.ItemIndex:=Ord(FPdfOptions.PaperType)-1;

  RxDBGridExportPdfSetupForm.CheckBox1.Checked:=poOutLine in FPdfOptions.Options;
  RxDBGridExportPdfSetupForm.CheckBox2.Checked:=poCompressText in FPdfOptions.Options;
  RxDBGridExportPdfSetupForm.CheckBox3.Checked:=poCompressFonts in FPdfOptions.Options;
  RxDBGridExportPdfSetupForm.CheckBox4.Checked:=poCompressImages in FPdfOptions.Options;
  RxDBGridExportPdfSetupForm.CheckBox5.Checked:=poUseRawJPEG in FPdfOptions.Options;

  Result:=RxDBGridExportPdfSetupForm.ShowModal = mrOk;
  if Result then
  begin
    FileName:=RxDBGridExportPdfSetupForm.FileNameEdit1.FileName;
    FOpenAfterExport:=RxDBGridExportPdfSetupForm.cbOpenAfterExport.Checked;
    FTitleColor:=RxDBGridExportPdfSetupForm.ColorBox1.Selected;

    if  RxDBGridExportPdfSetupForm.cbExportColumnHeader.Checked then
      FOptions:=FOptions + [repExportTitle]
    else
      FOptions:=FOptions - [repExportTitle];

    if RxDBGridExportPdfSetupForm.cbExportColumnFooter.Checked then
      FOptions:=FOptions + [repExportFooter]
    else
      FOptions:=FOptions - [repExportFooter];

    if RxDBGridExportPdfSetupForm.cbExportCellColors.Checked then
      FOptions:=FOptions + [repExportColors]
    else
      FOptions:=FOptions - [repExportColors];

    if RxDBGridExportPdfSetupForm.CheckBox6.Checked then
      FOptions:=FOptions + [repExportImages]
    else
      FOptions:=FOptions - [repExportImages];


    if RxDBGridExportPdfSetupForm.RadioGroup1.ItemIndex = 0 then
     FPdfOptions.PaperOrientation:=ppoPortrait
    else
      FPdfOptions.PaperOrientation:=ppoLandscape;

    FPdfOptions.PaperType:=TPDFPaperType(RxDBGridExportPdfSetupForm.ComboBox1.ItemIndex+1);

    if RxDBGridExportPdfSetupForm.CheckBox1.Checked then
      FPdfOptions.Options:=FPdfOptions.Options + [poOutLine]
    else
      FPdfOptions.Options:=FPdfOptions.Options - [poOutLine];

    if RxDBGridExportPdfSetupForm.CheckBox2.Checked then
      FPdfOptions.Options:=FPdfOptions.Options + [poCompressText]
    else
      FPdfOptions.Options:=FPdfOptions.Options - [poCompressText];

    if RxDBGridExportPdfSetupForm.CheckBox3.Checked then
      FPdfOptions.Options:=FPdfOptions.Options + [poCompressFonts]
    else
      FPdfOptions.Options:=FPdfOptions.Options - [poCompressFonts];

    if RxDBGridExportPdfSetupForm.CheckBox4.Checked then
      FPdfOptions.Options:=FPdfOptions.Options + [poCompressImages]
    else
      FPdfOptions.Options:=FPdfOptions.Options - [poCompressImages];

    if RxDBGridExportPdfSetupForm.CheckBox5.Checked then
      FPdfOptions.Options:=FPdfOptions.Options + [poUseRawJPEG]
    else
      FPdfOptions.Options:=FPdfOptions.Options - [poUseRawJPEG];

  end;
  RxDBGridExportPdfSetupForm.Free;
end;

procedure TRxDBGridExportPDF.DoSaveDocument;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FFileName,fmCreate);
  try
    FPDFDocument.SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TRxDBGridExportPDF.InitFonts;
var
  FontDirList: TStringList;

procedure CreateFontDirList;
{$IFDEF WINDOWS}
var
  s: String;
{$ENDIF}
begin
 {$IFDEF WINDOWS}
  s := SHGetFolderPathUTF8(20); // CSIDL_FONTS = 20
  if s <> '' then
    FontDirList.Add(s);
 {$ENDIF}
 {$IFDEF linux}
  //tested on Fedora 24
  FontDirList.Add('/usr/share/cups/fonts/');
  FontDirList.Add('/usr/share/fonts/');
  FontDirList.Add('/usr/share/wine/fonts/');
  FontDirList.Add('/usr/local/lib/X11/fonts/');
  FontDirList.Add(GetUserDir + '.fonts/');
 {$ENDIF}
end;
begin
  FontDirList := TStringList.Create;
  CreateFontDirList;
  if gTTFontCache.Count = 0 then
  begin
    gTTFontCache.BuildFontCacheIgnoresErrors:=true;
    CreateFontDirList;
    gTTFontCache.SearchPath.Assign(FontDirList);
    FreeAndNil(FontDirList);
    gTTFontCache.BuildFontCache;
  end;
end;

constructor TRxDBGridExportPDF.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageMargin:=TRxPageMargin.Create;
  FPdfOptions:=TPdfExportOptions.Create(Self);
  FTitleColor:=clSilver;

  FCaption:=sToolsExportPDF;
  FOpenAfterExport:=false;
end;

destructor TRxDBGridExportPDF.Destroy;
begin
  FreeAndNil(FPdfOptions);
  FreeAndNil(FPageMargin);
  inherited Destroy;
end;

{$ENDIF}
end.

