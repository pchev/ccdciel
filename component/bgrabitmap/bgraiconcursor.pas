// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ @abstract(Reading/writing of icon/cursor files (with multiple sizes))

  ICO and CUR files contain multiple scaled versions of the same image.
  This unit allows to edit them. }
unit BGRAIconCursor;

{$mode objfpc}{$H+}
{$i bgrabitmap.inc}

interface

uses
  BGRAClasses, SysUtils, BGRAMultiFileType, BGRABitmapTypes;

type
  { Image entry within an icon or cursor }
  TBGRAIconCursorEntry = class(TMultiFileEntry)
  protected
    FWidth,FHeight,FBitDepth: integer;
    FExtension: string;
    FContent: TStream;
    FHotSpot: TPoint;
    function GetName: utf8string; override;
    procedure SetName({%H-}AValue: utf8string); override;
    function GetExtension: utf8string; override;
    function GetFileSize: int64; override;
  public
    constructor Create(AContainer: TMultiFileContainer; AExtension: string; AInfo: TQuickImageInfo; AContent: TStream);
    class function TryCreate(AContainer: TMultiFileContainer; AContent: TStream): TBGRAIconCursorEntry; static;
    destructor Destroy; override;
    function CopyTo(ADestination: TStream): int64; override;
    function GetStream: TStream; override;
    function GetBitmap: TBGRACustomBitmap;
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property BitDepth: integer read FBitDepth;
    property HotSpot: TPoint read FHotSpot write FHotSpot;
  end;

  { An icon or cursor, with multiple images, one for each size and depth }
  TBGRAIconCursor = class(TMultiFileContainer)
  private
    function GetBitDepthAt(AIndex: integer): integer;
    function GetHeightAt(AIndex: integer): integer;
    function GetHotSpotAtAt(AIndex: integer): TPoint;
    function GetWidthAt(AIndex: integer): integer;
    procedure SetFileType(AValue: TBGRAImageFormat);
    procedure SetHotSpotAt(AIndex: integer; AValue: TPoint);
  protected
    FFileType : TBGRAImageFormat;
    FLoading : boolean;
    function CreateEntry(AName: utf8string; AExtension: utf8string;
      AContent: TStream): TMultiFileEntry; override;
    function ExpectedMagic: Word;
    procedure Init; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AFileType: TBGRAImageFormat); overload;
    procedure Assign(Source: TPersistent); override;
    function Add(ABitmap: TBGRACustomBitmap; ABitDepth: integer; AOverwrite: boolean = false): integer; overload;
    function Add(AContent: TStream; AOverwrite: boolean = false; AOwnStream: boolean = true): integer; overload;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(ADestination: TStream); override;
    function GetBitmap(AIndex: integer): TBGRACustomBitmap;
    function GetBestFitIndex(AWidth,AHeight: integer): integer;
    function GetBestFitBitmap(AWidth,AHeight: integer): TBGRACustomBitmap;
    function IndexOf(AWidth,AHeight,ABitDepth: integer): integer; overload;
    property FileType: TBGRAImageFormat read FFileType write SetFileType;
    property Width[AIndex: integer]: integer read GetWidthAt;
    property Height[AIndex: integer]: integer read GetHeightAt;
    property BitDepth[AIndex: integer]: integer read GetBitDepthAt;
    property HotSpot[AIndex: integer]: TPoint read GetHotSpotAtAt write SetHotSpotAt;
  end;

function BGRADitherIconCursor(ABitmap: TBGRACustomBitmap; ABitDepth: integer; ADithering: TDitheringAlgorithm): TBGRACustomBitmap;
function BGRABitDepthIconCursor(ABitmap: TBGRACustomBitmap): integer;

implementation

uses BGRAWinResource, BGRAUTF8, BGRAReadPng, BGRAReadBMP, FPWriteBMP, BGRAPalette, BGRAWritePNG,
  BGRAColorQuantization{$IFDEF BGRABITMAP_USE_LCL}, Graphics{$ENDIF};

function BGRADitherIconCursor(ABitmap: TBGRACustomBitmap; ABitDepth: integer;
  ADithering: TDitheringAlgorithm): TBGRACustomBitmap;
var
  frameMask, temp: TBGRACustomBitmap;
  quantizer: TBGRAColorQuantizer;
  maskQuantizer: TBGRAColorQuantizer;

  x,y: integer;
  psrc,pdest: PBGRAPixel;
begin
  if ABitDepth <= 0 then
    raise exception.Create('Invalid bit depth');

  if ABitDepth <= 24 then
  begin
    if ABitDepth = 1 then
    begin
      quantizer := TBGRAColorQuantizer.Create([BGRABlack,BGRAWhite,BGRAPixelTransparent],false,3);
      result := quantizer.GetDitheredBitmap(ADithering, ABitmap);
      quantizer.Free;
    end
    else
    begin
      frameMask := ABitmap.GetMaskFromAlpha;
      maskQuantizer := TBGRAColorQuantizer.Create([BGRABlack,BGRAWhite],false,2);
      temp := maskQuantizer.GetDitheredBitmap(ADithering, frameMask);
      frameMask.Free;
      frameMask := temp;
      maskQuantizer.Free;

      result := ABitmap.Duplicate;
      result.ReplaceTransparent(BGRABlack);
      result.AlphaFill(255);

      if ABitDepth <= 8 then
      begin
        quantizer := TBGRAColorQuantizer.Create(result,acFullChannelInPalette, 1 shl ABitDepth);
        temp := quantizer.GetDitheredBitmap(daFloydSteinberg, result);
        result.free;
        result := temp;
        quantizer.Free;
      end;

      result.ApplyMask(frameMask);
      frameMask.Free;
    end;
  end else
    result := ABitmap.Duplicate;

  if Assigned(ABitmap.XorMask) then
  begin
    result.NeedXorMask;
    for y := 0 to ABitmap.XorMask.Height-1 do
    begin
      psrc := ABitmap.XorMask.ScanLine[y];
      pdest := result.XorMask.ScanLine[y];
      for x := 0 to ABitmap.XorMask.Width-1 do
      begin
        if ((psrc^.red shl 1)+(psrc^.green shl 2)+psrc^.blue >= 128*(1+2+4)) then
          pdest^ := BGRA(255,255,255,0);
        inc(psrc);
        inc(pdest);
      end;
    end;
  end;
end;

function BGRABitDepthIconCursor(ABitmap: TBGRACustomBitmap): integer;
var pal: TBGRAPalette;
  p: PBGRAPixel;
  n: integer;

  function BlackAndWhite: boolean;
  var
    i: Integer;
  begin
    if pal.Count > 2 then result := false
    else
    begin
      for i := 0 to pal.Count-1 do
        if (pal.Color[i] <> BGRAWhite) and (pal.Color[i] <> BGRABlack) then
          exit(false);
      result := true;
    end;
  end;

begin
  pal := TBGRAPalette.Create;
  p := ABitmap.Data;
  n := ABitmap.NbPixels;
  while (n > 0) and (pal.Count < 257) do
  begin
    if p^.alpha = 0 then
    begin
      if pal.Count < 257 then pal.AddColor(BGRABlack);
    end else
    if p^.alpha = 255 then
    begin
      if pal.Count < 257 then pal.AddColor(p^);
    end else
    begin
      pal.Free;
      exit(32);
    end;
    inc(p);
    dec(n);
  end;
  if pal.Count > 256 then result := 24 else
  if pal.Count > 16 then result := 8 else
  if (pal.Count > 2) or not BlackAndWhite then result := 4 else
    result := 1;
  pal.Free;
end;

{ TBGRAIconCursorEntry }

constructor TBGRAIconCursorEntry.Create(AContainer: TMultiFileContainer; AExtension: string; AInfo: TQuickImageInfo;
  AContent: TStream);
begin
  inherited Create(AContainer);
  FExtension:= AExtension;
  FWidth := AInfo.Width;
  FHeight:= AInfo.Height;

  // 16 bit per channel is not relevant for icon depth
  if AInfo.ColorDepth >= 24 then
  begin
    if AInfo.AlphaDepth >= 8 then
      FBitDepth := 32
    else
      FBitDepth := 24;
  end else
    FBitDepth := AInfo.ColorDepth;

  FContent := AContent;
end;

class function TBGRAIconCursorEntry.TryCreate(
  AContainer: TMultiFileContainer; AContent: TStream): TBGRAIconCursorEntry;
var
  format: TBGRAImageFormat;
  imageInfo: TQuickImageInfo;
  tempStream: TMemoryStream;
  reader: TBGRAImageReader;
  bmp: TBGRACustomBitmap;
  maskLine: packed array of byte;
  maskStride: integer;
  psrc: PBGRAPixel;
  maskBit: byte;
  maskPos,x,y: integer;
  headerSize, dataSize: integer;
begin
  AContent.Position:= 0;
  format := DetectFileFormat(AContent);
  case format of
  ifBmp:
    begin
      reader := TBGRAReaderBMP.Create;
      bmp := BGRABitmapFactory.Create;
      try
        AContent.Position := 0;
        imageInfo := reader.GetQuickInfo(AContent);
        if (imageInfo.width <= 0) or (imageInfo.height <= 0) or
           (imageInfo.width > 256) or (imageInfo.height > 256) then
          raise exception.Create('Invalid image size');
        AContent.Position := 0;
        //load bitmap to build mask
        bmp.LoadFromStream(AContent);
        maskStride := ((bmp.Width+31) div 32)*4;

        tempStream := TMemoryStream.Create;
        //BMP header is not stored in icon/cursor
        AContent.Position:= sizeof(TBitMapFileHeader);
        tempStream.CopyFrom(AContent, AContent.Size - sizeof(TBitMapFileHeader));
        AContent.Free;

        //fix height
        tempStream.Position := 0;
        headerSize := LEtoN(tempStream.ReadDWord);
        if headerSize = sizeof(TOS2BitmapHeader) then // OS/2 1.x
        begin
          tempStream.Position := 6;
          tempStream.WriteWord(NtoLE(word(bmp.Height*2))); //include mask size
        end else
        begin
          tempStream.Position := 8;
          tempStream.WriteDWord(NtoLE(LongWord(bmp.Height*2))); //include mask size
          if headerSize >= 20+4 then
          begin
            tempStream.Position:= 20;
            dataSize := LEtoN(tempStream.ReadDWord);
            if dataSize <> 0 then
            begin //if data size is supplied, include mask size
              inc(dataSize, maskStride*bmp.Height);
              tempStream.Position:= 20;
              tempStream.WriteDWord(NtoLE(dataSize));
            end;
          end;
        end;

        //build mask
        tempStream.Position := tempStream.Size;
        setlength(maskLine, maskStride);
        for y := bmp.Height-1 downto 0 do
        begin
          maskBit := $80;
          maskPos := 0;
          psrc := bmp.ScanLine[y];
          fillchar(maskLine[0], length(maskLine), 0);
          for x := 0 to bmp.Width-1 do
          begin
            if psrc^.alpha = 0 then
              maskLine[maskPos] := maskLine[maskPos] or maskBit;
            maskBit := maskBit shr 1;
            if maskBit = 0 then
            begin
              maskBit := $80;
              inc(maskPos);
            end;
            inc(psrc);
          end;
          tempStream.WriteBuffer(maskLine[0], length(maskLine));
        end;

        result := TBGRAIconCursorEntry.Create(AContainer, 'dib', imageInfo, tempStream);
      finally
        bmp.Free;
        reader.Free;
      end;
    end;
  ifPng:
    begin
      reader := TBGRAReaderPNG.Create;
      imageInfo := reader.GetQuickInfo(AContent);
      reader.Free;
      result := TBGRAIconCursorEntry.Create(AContainer, 'png', imageInfo, AContent);

    end;
  ifUnknown, ifLazPaint {a headerless bmp can be confused for a headerless lzp}:
    begin
      //assume headerless BMP
      AContent.Position := 0;
      reader := TBGRAReaderBMP.Create;
      imageInfo := reader.GetQuickInfo(AContent);
      imageInfo.Height:= imageInfo.Height div 2; //mask size is included
      reader.Free;
      if (imageInfo.width <= 0) or (imageInfo.height <= 0) or
         (imageInfo.width > 256) or (imageInfo.height > 256) then
        raise exception.Create('Invalid image size');
      result := TBGRAIconCursorEntry.Create(AContainer, 'dib', imageInfo, AContent);
    end;
  else
    raise exception.Create(SuggestImageExtension(format) + ' format is not handled');
  end;
end;

destructor TBGRAIconCursorEntry.Destroy;
begin
  FContent.Free;
  inherited Destroy;
end;

function TBGRAIconCursorEntry.CopyTo(ADestination: TStream): int64;
begin
  if FContent.Size = 0 then
  begin
    result := 0;
    exit;
  end;

  FContent.Position := 0;
  result := ADestination.CopyFrom(FContent, FContent.Size);
end;

function TBGRAIconCursorEntry.GetStream: TStream;
begin
  Result:= FContent;
end;

function TBGRAIconCursorEntry.GetBitmap: TBGRACustomBitmap;
var reader: TBGRAImageReader;
begin
  if Extension = 'dib' then
  begin
    reader := TBGRAReaderBMP.Create;
    TBGRAReaderBMP(reader).Subformat := bsfHeaderlessWithMask;
  end else
    reader := TBGRAReaderPNG.create;

  result := BGRABitmapFactory.Create;
  FContent.Position := 0;
  try
    result.LoadFromStream(FContent, reader);
  except on ex: Exception do
    begin
      result.Free;
      reader.Free;
      raise ex;
    end;
  end;
  reader.Free;

  result.HotSpot := HotSpot;
end;

function TBGRAIconCursorEntry.GetName: utf8string;
begin
  result := IntToStr(FWidth)+'x'+IntToStr(FHeight)+'x'+IntToStr(FBitDepth);
end;

procedure TBGRAIconCursorEntry.SetName(AValue: utf8string);
begin
  raise exception.Create('Name cannot be changed');
end;

function TBGRAIconCursorEntry.GetExtension: utf8string;
begin
  result := FExtension;
end;

function TBGRAIconCursorEntry.GetFileSize: int64;
begin
  result := FContent.Size;
end;

{ TBGRAIconCursor }

function TBGRAIconCursor.GetBitDepthAt(AIndex: integer): integer;
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  result := TBGRAIconCursorEntry(Entry[AIndex]).BitDepth;
end;

function TBGRAIconCursor.GetHeightAt(AIndex: integer): integer;
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  result := TBGRAIconCursorEntry(Entry[AIndex]).Height;
end;

function TBGRAIconCursor.GetHotSpotAtAt(AIndex: integer): TPoint;
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  result := TBGRAIconCursorEntry(Entry[AIndex]).HotSpot;
end;

function TBGRAIconCursor.GetWidthAt(AIndex: integer): integer;
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  result := TBGRAIconCursorEntry(Entry[AIndex]).Width;
end;

procedure TBGRAIconCursor.SetFileType(AValue: TBGRAImageFormat);
begin
  if FFileType=AValue then Exit;
  if not (AValue in [ifIco,ifCur,ifUnknown]) then
    raise exception.Create('Allowed formats: ICO, CUR or unknown');
  FFileType:=AValue;
end;

procedure TBGRAIconCursor.SetHotSpotAt(AIndex: integer; AValue: TPoint);
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  TBGRAIconCursorEntry(Entry[AIndex]).HotSpot := AValue;
end;

function TBGRAIconCursor.CreateEntry(AName: utf8string;
  AExtension: utf8string; AContent: TStream): TMultiFileEntry;
begin
  AExtension := UTF8LowerCase(AExtension);
  if (AExtension <> 'png') and (AExtension <> 'dib') then
    raise exception.Create('The only supported extensions are PNG and DIB');

  result := TBGRAIconCursorEntry.TryCreate(self, AContent);
  if result.Extension <> AExtension then
  begin
    result.Free;
    raise exception.Create(AExtension + ' file extension expected but ' + result.Extension + ' found');
  end;

  if result.Name <> AName then
  begin
    result.Free;
    raise exception.Create('"' + AName + '" dimension expected but "' + result.Name + '" found');
  end;
end;

function TBGRAIconCursor.ExpectedMagic: Word;
begin
  case FFileType of
  ifIco: result := ICON_OR_CURSOR_FILE_ICON_TYPE;
  ifCur: result := ICON_OR_CURSOR_FILE_CURSOR_TYPE;
  else
    raise exception.Create('Invalid icon/cursor type');
  end;
end;

procedure TBGRAIconCursor.Init;
begin
  inherited Init;
  FFileType:= ifUnknown;
end;

procedure TBGRAIconCursor.AssignTo(Dest: TPersistent);
{$IFDEF BGRABITMAP_USE_LCL}
var
  temp: TMemoryStream;
{$ENDIF}
begin
  {$IFDEF BGRABITMAP_USE_LCL}
  if Dest is TCustomIcon then
  begin
    temp := TMemoryStream.Create;
    try
      SaveToStream(temp);
      temp.Position:= 0;
      TCustomIcon(Dest).LoadFromStream(temp);
    finally
      temp.Free;
    end;
  end else
  {$ENDIF}
    inherited AssignTo(Dest);
end;

constructor TBGRAIconCursor.Create(AFileType: TBGRAImageFormat);
begin
  if not (AFileType in [ifIco,ifCur,ifUnknown]) then
    raise exception.Create('Allowed formats: ICO, CUR or unknown');

  Init;
  FFileType := AFileType;
end;

function TBGRAIconCursor.Add(ABitmap: TBGRACustomBitmap; ABitDepth: integer;
  AOverwrite: boolean): integer;
var stream, temp: TStream;
  writer: TFPWriterBMP;
  bmpXOR: TBGRACustomBitmap;
  y: Integer;
  psrcMask, pdest: PBGRAPixel;
  bitAndMask: array of byte;
  bitAndMaskPos: integer;
  bitAndMaskBit: byte;
  bitAndMaskRowSize, x: integer;
  palette: TBGRAPalette;
  writerPng: TBGRAWriterPNG;

begin
  stream := TMemoryStream.Create;
  try
    //PNG format is advised from 256 on but does not handle XOR
    if ((ABitmap.Width >= 256) or (ABitmap.Height >= 256)) and (ABitDepth >= 8) and
        ((ABitmap.XorMask = nil) or ABitmap.XorMask.IsZero) then
    begin
      writerPng := TBGRAWriterPNG.Create;
      try
        writerPng.WordSized := false;
        if ABitDepth = 8 then
        begin
          writerPng.Indexed := true;
          writerpng.UseAlpha := ABitmap.HasTransparentPixels;
        end else
        begin
          writerPng.Indexed := false;
          writerpng.UseAlpha := (ABitDepth = 32);
        end;
        ABitmap.SaveToStream(stream, writerPng);
      finally
        writerPng.Free;
      end;
      result := Add(stream, AOverwrite, true);
      stream := nil;
    end else
    if ((ABitmap.XorMask = nil) or ABitmap.XorMask.IsZero) and
      (not ABitmap.HasTransparentPixels or (ABitDepth = 32)) then
    begin
      writer := TFPWriterBMP.Create;
      writer.BitsPerPixel := ABitDepth;
      try
        if not ABitmap.UsePalette and (ABitDepth < 24) then
        begin
          palette := TBGRAPalette.Create(ABitmap);
          try
            palette.AssignTo(ABitmap);
          finally
            palette.Free;
          end;
          ABitmap.SaveToStream(stream, writer);
          ABitmap.UsePalette:= false;
        end
        else
          ABitmap.SaveToStream(stream, writer);
      finally
        writer.Free;
      end;
      result := Add(stream, AOverwrite, true);
      stream := nil;
    end else
    begin
      bmpXOR := BGRABitmapFactory.Create(ABitmap);
      try
        bitAndMaskRowSize := ((bmpXOR.Width+31) div 32)*4;
        setlength(bitAndMask, bitAndMaskRowSize*bmpXOR.Height);
        for y := bmpXOR.Height-1 downto 0 do
        begin
          if assigned(ABitmap.XorMask) then
            psrcMask := ABitmap.XorMask.ScanLine[y]
          else
            psrcMask := nil;
          pdest := bmpXOR.ScanLine[y];
          bitAndMaskPos := (bmpXOR.Height-1-y)*bitAndMaskRowSize;
          bitAndMaskBit:= $80;
          for x := bmpXOR.Width-1 downto 0 do
          begin
            //xor mask is either 100% or 0%
            if assigned(psrcMask) and ((psrcMask^.red <> 0) or (psrcMask^.green <> 0) or (psrcMask^.blue <> 0)) then
            begin
              pdest^ := psrcMask^;
              pdest^.alpha := 255;
              bitAndMask[bitAndMaskPos] := bitAndMask[bitAndMaskPos] or bitAndMaskBit;
            end else
            if pdest^.alpha = 0 then
            begin
              bitAndMask[bitAndMaskPos] := bitAndMask[bitAndMaskPos] or bitAndMaskBit;
              if ABitDepth <= 24 then //if we cannot save alpha, replace with black.
              begin                   //mask will task care of making it transparent
                pdest^ := BGRABlack;
              end;
            end;

            bitAndMaskBit := bitAndMaskBit shr 1;
            if bitAndMaskBit = 0 then
            begin
              bitAndMaskBit := $80;
              inc(bitAndMaskPos);
            end;
            if assigned(psrcMask) then inc(psrcMask);
            inc(pdest);
          end;
        end;
        bmpXOR.InvalidateBitmap;

        if ABitDepth < 24 then
        begin
          palette := TBGRAPalette.Create(bmpXor);
          palette.AssignTo(bmpXor);
          palette.Free;
        end;

        temp := TMemoryStream.Create;
        try
          writer := TFPWriterBMP.Create;
          writer.BitsPerPixel := ABitDepth;
          try
            bmpXOR.SaveToStream(temp, writer);
            //write double height to include mask
            temp.Position := 22;
            temp.WriteDWord(NtoLE(LongWord(bmpXOR.Height*2)));
            //go after the file header
            temp.Position := 14;
            //copy bitmap without header
            stream.CopyFrom(temp, temp.Size-temp.Position);
          finally
            writer.Free;
          end;
        finally
          temp.Free;
        end;
        //write mask
        stream.WriteBuffer(bitAndMask[0],length(bitAndMask));
        result := Add(stream, AOverwrite, true);
        stream := nil;
      finally
        bmpXOR.Free;
      end;
    end;

  finally
    stream.Free;
  end;
end;

function TBGRAIconCursor.Add(AContent: TStream; AOverwrite: boolean;
  AOwnStream: boolean): integer;
var
  index,i: Integer;
  newEntry: TBGRAIconCursorEntry;
  contentCopy: TMemoryStream;
begin
  if not AOwnStream then
  begin
    AContent.Position:= 0;
    contentCopy := TMemoryStream.Create;
    contentCopy.CopyFrom(AContent, AContent.Size);
    newEntry := TBGRAIconCursorEntry.TryCreate(self, contentCopy);
  end else
    newEntry := TBGRAIconCursorEntry.TryCreate(self, AContent);

  index := IndexOf(newEntry.Name, newEntry.Extension);
  if index <> -1 then
  begin
    if AOverwrite then
      Delete(index)
    else
    begin
      newEntry.Free;
      raise Exception.Create('Duplicate entry');
    end;
  end else if not FLoading then
  begin
    for i := 0 to Count-1 do
      if ((Width[i] < newEntry.Width) and (Height[i] < newEntry.Height)) or
         ((Width[i] = newEntry.Width) and (Height[i] = newEntry.Height) and (BitDepth[i] < newEntry.BitDepth)) then
      begin
        index := i;
        break;
      end;
  end;
  result := AddEntry(newEntry, index);
end;

procedure TBGRAIconCursor.LoadFromStream(AStream: TStream);
var header: TGroupIconHeader;
  dir: packed array of TIconFileDirEntry;
  startPos: int64;
  entryContent: TMemoryStream;
  entryIndex, i: integer;
begin
  FLoading:= true;
  try
    startPos := AStream.Position;
    AStream.ReadBuffer({%H-}header, sizeof(header));
    header.SwapIfNecessary;
    if header.Reserved <> 0 then
      raise exception.Create('Invalid file format');
    if FileType = ifUnknown then
    begin
      case header.ResourceType of
      ICON_OR_CURSOR_FILE_ICON_TYPE: FFileType := ifIco;
      ICON_OR_CURSOR_FILE_CURSOR_TYPE: FFileType := ifCur;
      end;
    end;
    if header.ResourceType <> ExpectedMagic then
      raise exception.Create('Invalid resource type');
    Clear;
    setlength(dir, header.ImageCount);
    AStream.ReadBuffer(dir[0], sizeof(TIconFileDirEntry)*length(dir));
    for i := 0 to high(dir) do
    begin
      AStream.Position:= LEtoN(dir[i].ImageOffset) + startPos;
      entryContent := TMemoryStream.Create;
      entryContent.CopyFrom(AStream, LEtoN(dir[i].ImageSize));
      entryIndex := Add(entryContent, false, true);
      if ((dir[i].Width = 0) and (Width[entryIndex] < 256)) or
         ((dir[i].Width > 0) and (Width[entryIndex] <> dir[i].Width)) or
         ((dir[i].Height = 0) and (Height[entryIndex] < 256)) or
         ((dir[i].Height > 0) and (Height[entryIndex] <> dir[i].Height)) then
          raise Exception.Create('Inconsistent image size');
      if FFileType = ifCur then
        TBGRAIconCursorEntry(Entry[entryIndex]).HotSpot := Point(LEtoN(dir[i].HotSpotX),LEtoN(dir[i].HotSpotY));
    end;
  finally
    FLoading:= false;
  end;
end;

procedure TBGRAIconCursor.SaveToStream(ADestination: TStream);
var header: TGroupIconHeader;
  i: integer;
  accSize: LongWord;
  dir: packed array of TIconFileDirEntry;
  contentSize: LongWord;
begin
  if Count = 0 then
    raise exception.Create('File cannot be empty');
  if FileType = ifUnknown then
    raise exception.Create('You need to specify the file type');
  header.ImageCount:= Count;
  header.Reserved := 0;
  header.ResourceType:= ExpectedMagic;
  header.SwapIfNecessary;
  accSize := sizeof(header) + sizeof(TIconFileDirEntry)*Count;
  setlength(dir, Count);
  for i := 0 to Count-1 do
  begin
    if Width[i] >= 256
    then dir[i].Width := 0
    else dir[i].Width := Width[i];

    if Height[i] >= 256
    then dir[i].Height := 0
    else dir[i].Height := Height[i];

    if BitDepth[i] < 8 then
      dir[i].Colors := 1 shl BitDepth[i]
    else
      dir[i].Colors := 0;
    dir[i].Reserved := 0;
    case FFileType of
    ifCur: begin dir[i].HotSpotX:= NtoLE(Word(HotSpot[i].X)); dir[i].HotSpotY := NtoLE(Word(HotSpot[i].Y)); end;
    ifIco: begin dir[i].BitsPerPixel:= NtoLE(Word(BitDepth[i])); dir[i].Planes := NtoLE(Word(1)); end;
    else dir[i].Variable:= 0;
    end;
    dir[i].ImageOffset := LEtoN(accSize);
    contentSize:= Entry[i].FileSize;
    dir[i].ImageSize := NtoLE(contentSize);
    inc(accSize,contentSize);
  end;

  ADestination.WriteBuffer(header, sizeof(header));
  ADestination.WriteBuffer(dir[0], sizeof(TIconFileDirEntry)*length(dir));
  for i := 0 to Count-1 do
    if Entry[i].CopyTo(ADestination) <> Entry[i].FileSize then
        raise exception.Create('Unable to write data in stream');
end;

function TBGRAIconCursor.GetBitmap(AIndex: integer): TBGRACustomBitmap;
begin
  if (AIndex < 0) or (AIndex >= Count) then raise ERangeError.Create('Index out of bounds');
  result := TBGRAIconCursorEntry(Entry[AIndex]).GetBitmap;
end;

function TBGRAIconCursor.GetBestFitIndex(AWidth, AHeight: integer): integer;
var bestIndex: integer;
  bestSizeDiff: integer;
  bestBPP: integer;
  sizeDiff, i: integer;
begin
  bestBPP := 0;
  bestSizeDiff := high(integer);
  bestIndex := -1;
  for i := 0 to Count-1 do
  begin
    sizeDiff := abs(AWidth-Width[i])+abs(AHeight-Height[i]);
    if (sizeDiff < bestSizeDiff) or
      ((sizeDiff = bestSizeDiff) and (BitDepth[i] > bestBPP)) then
    begin
      bestIndex := i;
      bestSizeDiff:= sizeDiff;
      bestBPP:= BitDepth[i];
    end;
  end;
  exit(bestIndex);
end;

function TBGRAIconCursor.GetBestFitBitmap(AWidth, AHeight: integer): TBGRACustomBitmap;
var bestIndex: integer;
begin
  bestIndex := GetBestFitIndex(AWidth, AHeight);
  if bestIndex = -1 then
    raise Exception.Create('No bitmap found')
  else
    result := GetBitmap(bestIndex);
end;

function TBGRAIconCursor.IndexOf(AWidth, AHeight, ABitDepth: integer): integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if (Width[i] = AWidth) and (Height[i] = AHeight) and (BitDepth[i] = ABitDepth) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TBGRAIconCursor.Assign(Source: TPersistent);
{$IFDEF BGRABITMAP_USE_LCL}
var
  temp: TMemoryStream;
{$ENDIF}
begin
  {$IFDEF BGRABITMAP_USE_LCL}
  if Source is TCustomIcon then
  begin
    temp := TMemoryStream.Create;
    try
      TCustomIcon(Source).SaveToStream(temp);
      temp.Position:= 0;
      LoadFromStream(temp);
    finally
      temp.Free;
    end;
  end else
  {$ENDIF}
    inherited Assign(Source);
end;

end.

