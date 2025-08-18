// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Implementation of BGRABitmap for fpGUI }
unit BGRAfpGUIBitmap;
{ It should NOT be added to the **uses** clause. }

{$mode objfpc}{$H+}

interface

{$i bgrabitmap.inc}

uses
  SysUtils, BGRAClasses, BGRAGraphics, BGRABitmapTypes, BGRADefaultBitmap,
  {$IFDEF BGRABITMAP_USE_LAZFREETYPE}BGRAFreeType, EasyLazFreeType, LazFreeTypeFontCollection,{$ENDIF}
  BGRACanvas;

type

  { Implementation of TBGRABitmap for fpGUI }
  TBGRAfpGUIBitmap = class(TBGRADefaultBitmap)
  private
    FPseudoCanvas: TBGRACanvas;
    function GetPseudoCanvas: TBGRACanvas;
    function GetBitmapTransparent: boolean;
    procedure SetBitmapTransparent(AValue: boolean);
  protected
    procedure RebuildBitmap; override;
    function CreateDefaultFontRenderer: TBGRACustomFontRenderer; override;
    function LoadFromRawImage(ARawImage: TRawImage; DefaultOpacity: byte;
      AlwaysReplaceAlpha: boolean=False; {%H-}RaiseErrorOnInvalidPixelFormat: boolean
      =True): boolean; override;
    procedure Init; override;
    procedure FreeData; override;
    procedure ReallocData; override;
    procedure FreeBitmap; override;
    procedure NotAvailable;
  public
    destructor Destroy; override;
    procedure AssignToBitmap(ADestination: TBitmap);
    {$IFDEF BGRABITMAP_USE_LAZFREETYPE}
    class procedure AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean = false); static;
    class procedure AddFreeTypeFontFile(AFilename: string; AUTF8: boolean = false); static;
    class procedure AddFreeTypeFontStream(AStream: TStream; AOwned: boolean); static;
    {$ENDIF}
    procedure Draw(ACanvas: TCanvas; x, y: integer; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; {%H-}Opaque: boolean=True); override;
    procedure Draw(ACanvas: TGUICanvas; x, y: integer; {%H-}Opaque: boolean=True); overload;
    procedure Draw(ACanvas: TGUICanvas; Rect: TRect; {%H-}Opaque: boolean=True); overload;
    procedure GetImageFromCanvas({%H-}CanvasSource: TCanvas; {%H-}x, {%H-}y: integer); override; //not available
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure TakeScreenshot({%H-}ARect: TRect); override; //not available
    procedure TakeScreenshotOfPrimaryMonitor; override; //not available
    procedure LoadFromDevice({%H-}DC: HDC); override; //not available
    procedure LoadFromDevice({%H-}DC: HDC; {%H-}ARect: TRect); override; //not available
    property BitmapTransparent: boolean read GetBitmapTransparent write SetBitmapTransparent;
    property Canvas: TBGRACanvas read GetPseudoCanvas;
  end;

implementation

{ TBGRAfpGUIBitmap }

function TBGRAfpGUIBitmap.GetBitmapTransparent: boolean;
begin
  result := FBitmap.Transparent;
end;

function TBGRAfpGUIBitmap.GetPseudoCanvas: TBGRACanvas;
begin
  if FPseudoCanvas = nil then
  begin
    FPseudoCanvas := TBGRACanvas.Create(self);
    FPseudoCanvas.AntialiasingMode := amOff;
  end;
  result := FPseudoCanvas;
end;

procedure TBGRAfpGUIBitmap.SetBitmapTransparent(AValue: boolean);
begin
  if FBitmap.Transparent <> AValue then
  begin
    FBitmap.Transparent:= AValue;
    InvalidateBitmap;
  end;
end;

procedure TBGRAfpGUIBitmap.RebuildBitmap;
var pmask, pmaskline: PByte;
  pdata: PBGRAPixel;
  raw: TRawImage;
  x,y,bit,masklinesize,curmaskbyte: UInt32or64;
begin
  if FBitmap.Transparent then
  begin
    raw := FBitmap.RawImage;
    masklinesize := ((Width+31) div 32)*4;
    pmaskline := FBitmap.RawImage.MaskData;
    pdata := raw.ImageData;
    for y := 0 to Height-1 do
      begin
        pmask:= pmaskline;
        bit := $80;
        curmaskbyte := 0;
        for x := Width-1 downto 0 do
        begin
          if pdata^.alpha >= $80 then
            curmaskbyte := curmaskbyte or bit;
          bit := bit shr 1;
          if bit = 0 then
          begin
            bit := $80;
            pmask^ := curmaskbyte;
            inc(pmask);
            curmaskbyte := 0;
          end;
          inc(pdata);
        end;
        if bit <> $80 then
          pmask^ := curmaskbyte;
        inc(pmaskline, masklinesize);
      end;
  end;
  FBitmap.RawImage.UpdateImage;
end;

function TBGRAfpGUIBitmap.CreateDefaultFontRenderer: TBGRACustomFontRenderer;
begin
  {$IFDEF BGRABITMAP_USE_LAZFREETYPE}
  result := TBGRAFreeTypeFontRenderer.Create;
  {$ELSE}
  raise Exception.Create('LazFreeType not available');
  {$ENDIF}
end;

function TBGRAfpGUIBitmap.LoadFromRawImage(ARawImage: TRawImage;
  DefaultOpacity: byte; AlwaysReplaceAlpha: boolean;
  RaiseErrorOnInvalidPixelFormat: boolean): boolean;
var
  lineSize: integer;
  y: Integer;
begin
  if (ARawImage.Width <> Width) or
    (ARawImage.Height <> Height) then
    raise Exception.Create('Bitmap size is inconsistent');

  lineSize := Width*sizeof(TBGRAPixel);
  for y := 0 to Height-1 do
    move(ARawImage.ScanLine[y]^, ScanLine[y]^, lineSize);
  if AlwaysReplaceAlpha then AlphaFill(DefaultOpacity);
  result := true;
end;

procedure TBGRAfpGUIBitmap.Init;
begin
  inherited Init;
  FBitmap := TBitmap.Create;
  FontAntialias:= true;
end;

procedure TBGRAfpGUIBitmap.ReallocData;
begin
  FBitmap.Width := Width;
  FBitmap.Height:= Height;
  FDataByte := PByte(FBitmap.RawImage.ImageData);
  InvalidateBitmap;
  FScanPtr := nil;
end;

procedure TBGRAfpGUIBitmap.FreeData;
begin
  //nothing
end;

procedure TBGRAfpGUIBitmap.FreeBitmap;
begin
  //nothing
end;

procedure TBGRAfpGUIBitmap.NotAvailable;
begin
  raise exception.Create('Function not available with fpGUI');
end;

destructor TBGRAfpGUIBitmap.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FPseudoCanvas);
  inherited Destroy;
end;

procedure TBGRAfpGUIBitmap.AssignToBitmap(ADestination: TBitmap);
begin
  ADestination.Assign(Bitmap);
end;

{$IFDEF BGRABITMAP_USE_LAZFREETYPE}class procedure TBGRAfpGUIBitmap.AddFreeTypeFontFolder(ADirectory: string; AUTF8: boolean);
begin
  if AUTF8 then ADirectory:= Utf8ToAnsi(ADirectory);
  EasyLazFreeType.FontCollection.AddFolder(ADirectory);

end;{$ENDIF}

{$IFDEF BGRABITMAP_USE_LAZFREETYPE}class procedure TBGRAfpGUIBitmap.AddFreeTypeFontFile(AFilename: string; AUTF8: boolean);
begin
  if AUTF8 then AFilename:= Utf8ToAnsi(AFilename);
  EasyLazFreeType.FontCollection.AddFile(AFilename);
end;{$ENDIF}

{$IFDEF BGRABITMAP_USE_LAZFREETYPE}class procedure TBGRAfpGUIBitmap.AddFreeTypeFontStream(AStream: TStream; AOwned: boolean);
begin
  EasyLazFreeType.FontCollection.AddStream(AStream, AOwned);
  if AOwned then AStream.Free;
end;{$ENDIF}

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean);
begin
  Draw(ACanvas.GUICanvas, x, y, Opaque);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean);
begin
  Draw(ACanvas.GUICanvas, Rect, Opaque);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TGUICanvas; x, y: integer;
  Opaque: boolean);
begin
  BitmapTransparent := not Opaque;
  ACanvas.DrawImage(x,y, Bitmap.RawImage);
end;

procedure TBGRAfpGUIBitmap.Draw(ACanvas: TGUICanvas; Rect: TRect;
  Opaque: boolean);
begin
  BitmapTransparent := not Opaque;
  ACanvas.StretchDraw(rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, Bitmap.RawImage);
end;

procedure TBGRAfpGUIBitmap.GetImageFromCanvas(CanvasSource: TCanvas; x,
  y: integer);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var temp: TBGRAfpGUIBitmap;
begin
  temp := TBGRAfpGUIBitmap.Create(AWidth,AHeight);
  move(AData^, temp.Data^, temp.NbPixels*sizeof(TBGRAPixel));
  if ALineOrder <> temp.LineOrder then temp.VerticalFlip;
  temp.Draw(ACanvas, Rect, False);
  temp.Free;
end;

procedure TBGRAfpGUIBitmap.DataDrawOpaque(ACanvas: TCanvas; Rect: TRect;
  AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
var temp: TBGRAfpGUIBitmap;
begin
  temp := TBGRAfpGUIBitmap.Create(AWidth,AHeight);
  move(AData^, temp.Data^, temp.NbPixels*sizeof(TBGRAPixel));
  if ALineOrder <> temp.LineOrder then temp.VerticalFlip;
  temp.Draw(ACanvas, Rect, True);
  temp.Free;
end;

procedure TBGRAfpGUIBitmap.TakeScreenshot(ARect: TRect);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.TakeScreenshotOfPrimaryMonitor;
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.LoadFromDevice(DC: HDC);
begin
  NotAvailable;
end;

procedure TBGRAfpGUIBitmap.LoadFromDevice(DC: HDC; ARect: TRect);
begin
  NotAvailable;
end;

end.

