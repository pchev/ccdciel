// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Computation of text effects based on a text mask }
unit BGRACustomTextFX;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAPhongTypes, BGRAGrayscaleMask;

const DefaultOutlineWidth = 3;

type
  { @abstract(Computes text effects based on a grayscale mask.)

    This class computes at the pixel level the following effects: drop shadow, outline
    and multi-color text (on color per letter) }

  TBGRACustomTextEffect = class
  private
    function GetBounds: TRect;
    function GetMaskHeight: integer;
    class function GetOutlineWidth: integer; static;
    function GetShadowBounds(ARadius: integer): TRect;
    function GetMaskWidth: integer;
    function GetTextHeight: integer;
    function GetTextWidth: integer;
    procedure SetShadowQuality(AValue: TRadialBlurType);
  protected
    FShadowQuality: TRadialBlurType;
    FTextMask: TGrayscaleMask;
    FShadowRadius: integer;
    FOutlineMask, FShadowMask : TGrayscaleMask;
    FShadingMask: TBGRACustomBitmap;
    FShadingAltitude: integer;
    FShadingRounded: boolean;
    FTextSize: TSize;
    FOffset: TPoint;
    function DrawMaskMulticolored(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; const AColors: array of TBGRAPixel): TRect;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; AColor: TBGRAPixel): TRect; overload;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; ATexture: IBGRAScanner): TRect; overload;
    function InternalDrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
    procedure Init(AMask: TGrayscaleMask; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
  public
    constructor Create(AMask: TBGRACustomBitmap; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
    constructor Create(AMask: TGrayscaleMask; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);

    {$IFNDEF BGRABITMAP_CORE}procedure ApplySphere;{$ENDIF}
    {$IFNDEF BGRABITMAP_CORE}procedure ApplyVerticalCylinder;{$ENDIF}
    {$IFNDEF BGRABITMAP_CORE}procedure ApplyHorizontalCylinder;{$ENDIF}
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect; overload;

    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; AAlign: TAlignment; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; AAlign: TAlignment; ARounded: Boolean = true): TRect; overload;

    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel): TRect; overload;
    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect; overload;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel): TRect; overload;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    destructor Destroy; override;
    property TextMask: TGrayscaleMask read FTextMask;
    property TextMaskOffset: TPoint read FOffset;
    property Width: integer read GetTextWidth; deprecated;
    property Height: integer read GetTextHeight; deprecated;
    property MaskWidth: integer read GetMaskWidth;
    property MaskHeight: integer read GetMaskHeight;
    property TextSize: TSize read FTextSize;
    property TextWidth: integer read GetTextWidth;
    property TextHeight: integer read GetTextHeight;
    property Bounds: TRect read GetBounds;
    property ShadowBounds[ARadius: integer]: TRect read GetShadowBounds;
    property ShadowQuality: TRadialBlurType read FShadowQuality write SetShadowQuality;
    class property OutlineWidth: integer read GetOutlineWidth;
  end;

implementation

uses Math, BGRAGradientScanner, BGRAFilterBlur;

procedure BGRACustomReplace(var Destination: TBGRACustomBitmap; Temp: TObject); overload;
begin
  Destination.Free;
  Destination := Temp as TBGRACustomBitmap;
end;

procedure BGRACustomReplace(var Destination: TGrayscaleMask; Temp: TObject); overload;
begin
  Destination.Free;
  Destination := Temp as TGrayscaleMask;
end;

{ TBGRACustomTextEffect }

function TBGRACustomTextEffect.GetBounds: TRect;
begin
  if FTextMask = nil then
    result := EmptyRect else
  with TextMaskOffset do
    result := rect(X,Y,X+FTextMask.Width,Y+FTextMask.Height);
end;

function TBGRACustomTextEffect.GetMaskHeight: integer;
begin
  if FTextMask = nil then
    result := 0
  else
    result := FTextMask.Height;
end;

class function TBGRACustomTextEffect.GetOutlineWidth: integer; static;
begin
  result := DefaultOutlineWidth;
end;

function TBGRACustomTextEffect.GetShadowBounds(ARadius: integer): TRect;
begin
  result := Bounds;
  if (ARadius > 0) and not result.IsEmpty then
    result.Inflate(ARadius, ARadius);
end;

function TBGRACustomTextEffect.GetMaskWidth: integer;
begin
  if FTextMask = nil then
    result := 0
  else
    result := FTextMask.Width;
end;

function TBGRACustomTextEffect.GetTextHeight: integer;
begin
  result := FTextSize.cy;
end;

function TBGRACustomTextEffect.GetTextWidth: integer;
begin
  result := FTextSize.cx;
end;

procedure TBGRACustomTextEffect.SetShadowQuality(AValue: TRadialBlurType);
begin
  if FShadowQuality=AValue then Exit;
  FShadowQuality:=AValue;
  FreeAndNil(FShadowMask);
end;

function TBGRACustomTextEffect.DrawMaskMulticolored(ADest: TBGRACustomBitmap;
  AMask: TCustomUniversalBitmap; X, Y: Integer; const AColors: array of TBGRAPixel
  ): TRect;
var
  scan: TBGRASolidColorMaskScanner;
  xb,yb,startX,numColor: integer;
  p0,p: PByte;
  emptyCol, nextCol: boolean;
begin
  if (AMask = nil) or (length(AColors)=0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if (length(AColors)=1) then
  begin
    result := DrawMask(ADest,AMask,X,Y,AColors[0]);
    exit;
  end;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColors[0]);
  numColor := 0;
  startX := -1;
  p0 := AMask.DataByte;
  for xb := 0 to AMask.Width-1 do
  begin
    p := p0;

    if startX=-1 then
    begin
      emptyCol := true;
      for yb := AMask.Height-1 downto 0 do
      begin
        if (p^ <> 0) then
        begin
          emptyCol := false;
          break;
        end;
        inc(p, AMask.Width);
      end;

      if not emptyCol then
      begin
        if startX=-1 then
          startX := xb;
      end else
      begin
        if startX<>-1 then
        begin
          ADest.FillRect(X+startX,Y,X+xb,Y+AMask.Height,scan,dmDrawWithTransparency);
          inc(numColor);
          if numColor = length(AColors) then
            numColor := 0;
          scan.Color := AColors[numColor];
          startX := -1;
        end;
      end;

    end else
    begin
      emptyCol := true;
      nextCol := true;
      for yb := AMask.Height-1 downto 0 do
      begin
        if (p^ <> 0) then
        begin
          emptyCol := false;
          if ((p-1)^ <> 0) then
          begin
            nextCol := false;
            break;
          end;
        end;
        inc(p, AMask.Width);
      end;
      if nextCol or emptyCol then
      begin
        ADest.FillRect(X+startX,Y,X+xb,Y+AMask.Height,scan,dmDrawWithTransparency);
        inc(numColor);
        if numColor = length(AColors) then
          numColor := 0;
        scan.Color := AColors[numColor];
        if emptyCol then startX := -1
         else startX := xb;
      end;
    end;

    inc(p0);
  end;
  if startX<>-1 then
    ADest.FillRect(X+startX,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRACustomTextEffect.DrawMask(ADest: TBGRACustomBitmap;
  AMask: TCustomUniversalBitmap; X, Y: Integer; AColor: TBGRAPixel): TRect;
var
  scan: TBGRACustomScanner;
begin
  if (AMask = nil) or (AColor.alpha = 0) then
  begin
    result := EmptyRect;
    exit;
  end;
  scan := TBGRASolidColorMaskScanner.Create(AMask,Point(-X,-Y),AColor);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRACustomTextEffect.DrawMask(ADest: TBGRACustomBitmap;
  AMask: TCustomUniversalBitmap; X, Y: Integer; ATexture: IBGRAScanner): TRect;
var
  scan: TBGRACustomScanner;
begin
  if (AMask = nil) or (ATexture = nil) then
  begin
    result := EmptyRect;
    exit;
  end;
  scan := TBGRATextureMaskScanner.Create(AMask,Point(-X,-Y),ATexture);
  ADest.FillRect(X,Y,X+AMask.Width,Y+AMask.Height,scan,dmDrawWithTransparency);
  scan.Free;
  result := rect(X,Y,X+AMask.Width,Y+AMask.Height);
end;

function TBGRACustomTextEffect.InternalDrawShaded(ADest: TBGRACustomBitmap; X,
  Y: integer; Shader: TCustomPhongShading; Altitude: integer;
  AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
var
  WithMargin,GrayMap: TGrayscaleMask;
  HeightMap: TBGRACustomBitmap;
  p: PBGRAPixel;
  n,maxv: integer;
  v,blurRadius: single;
  iBlurRadius: integer;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;

  if (FShadingMask <> nil) and ((FShadingAltitude <> Altitude) or (FShadingRounded <> ARounded)) then
    FreeAndNil(FShadingMask);

  if FShadingMask = nil then
  begin
    FShadingRounded := ARounded;
    FShadingAltitude := Altitude;

    if ARounded then blurRadius := Altitude
      else blurRadius := Altitude*0.5;

    iBlurRadius := ceil(blurRadius);

    WithMargin := TGrayscaleMask.Create(FTextMask.Width+iBlurRadius*2, FTextMask.Height+iBlurRadius*2,BGRABlack);
    WithMargin.PutImage(iBlurRadius, iBlurRadius, FTextMask, dmSet);
    if (iBlurRadius <> blurRadius) and (blurRadius < 3) then
      GrayMap := BGRAFilterBlur.FilterBlurRadial(WithMargin,
        round(blurRadius*10), round(blurRadius*10), rbPrecise) as TGrayscaleMask
    else
      GrayMap := BGRAFilterBlur.FilterBlurRadial(WithMargin, iBlurRadius, iBlurRadius,
        rbFast) as TGrayscaleMask;
    HeightMap := BGRABitmapFactory.Create;
    HeightMap.SetSize(GrayMap.Width, GrayMap.Height);
    GrayMap.Draw(HeightMap, 0, 0);
    GrayMap.Free;

    p := HeightMap.Data;
    maxv := 0;
    for n := HeightMap.NbPixels-1 downto 0 do
    begin
      if p^.green > maxv then
        maxv := p^.green;
      inc(p);
    end;

    if maxv > 0 then
    begin
      p := HeightMap.Data;
      for n := HeightMap.NbPixels-1 downto 0 do
      begin
        v := p^.green/maxv;
        if ARounded then
        begin
          if v <= 0.5 then
            v := v*v*2 else
            v := 1-(1-v)*(1-v)*2;
        end;
        p^ := MapHeightToBGRA( v, p^.alpha);
        inc(p);
      end;
    end;

    HeightMap.ApplyMask(WithMargin);
    WithMargin.Free;
    BGRACustomReplace(HeightMap, HeightMap.GetPart( rect(iBlurRadius, iBlurRadius,
                                 HeightMap.Width-iBlurRadius, HeightMap.Height-iBlurRadius) ) );
    FShadingMask := HeightMap;
  end;

  inc(X, FOffset.X);
  Inc(Y, FOffset.Y);
  if ATexture <> nil then
    Shader.DrawScan(ADest,FShadingMask,Altitude,X,Y, ATexture)
  else
    Shader.Draw(ADest,FShadingMask,Altitude,X,Y, AColor);
  result := rect(X,Y, X+FShadingMask.Width,Y+FShadingMask.Height);
end;

procedure TBGRACustomTextEffect.Init(AMask: TGrayscaleMask;
  AMaskOwner: boolean; AWidth, AHeight: integer; AOffset: TPoint);
begin
  FTextSize := Size(AWidth,AHeight);
  FOffset := AOffset;
  if not AMaskOwner then
    FTextMask := AMask.Duplicate
  else
    FTextMask := AMask;
  FShadowQuality:= rbFast;
end;

function TBGRACustomTextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := Draw(ADest,X-TextSize.cx,Y,AColor);
  taCenter: result := Draw(ADest,X-TextSize.cx div 2,Y,AColor);
  else result := Draw(ADest,X,Y,AColor);
  end;
end;

function TBGRACustomTextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := Draw(ADest,X-TextSize.cx,Y,ATexture);
  taCenter: result := Draw(ADest,X-TextSize.cx div 2,Y,ATexture);
  else result := Draw(ADest,X,Y,ATexture);
  end;
end;

function TBGRACustomTextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel;
  ARounded: Boolean): TRect;
begin
  result := InternalDrawShaded(ADest,X,Y,Shader,Altitude,AColor,nil,ARounded);
end;

function TBGRACustomTextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner;
  ARounded: Boolean): TRect;
begin
  result := InternalDrawShaded(ADest,X,Y,Shader,Altitude,BGRAPixelTransparent,ATexture,ARounded);
end;

function TBGRACustomTextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel;
  AAlign: TAlignment; ARounded: Boolean): TRect;
begin
  Case AAlign of
  taLeftJustify: result := DrawShaded(ADest,X,Y,Shader,Altitude,AColor,ARounded);
  taRightJustify: result := DrawShaded(ADest,X-TextSize.cx,Y,Shader,Altitude,AColor,ARounded);
  taCenter: result := DrawShaded(ADest,X-TextSize.cx div 2,Y,Shader,Altitude,AColor,ARounded);
  else
    result := EmptyRect;
  end;
end;

function TBGRACustomTextEffect.DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer;
  Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner;
  AAlign: TAlignment; ARounded: Boolean): TRect;
begin
  Case AAlign of
  taLeftJustify: result := DrawShaded(ADest,X,Y,Shader,Altitude,ATexture,ARounded);
  taRightJustify: result := DrawShaded(ADest,X-TextSize.cx,Y,Shader,Altitude,ATexture,ARounded);
  taCenter: result := DrawShaded(ADest,X-TextSize.cx div 2,Y,Shader,Altitude,ATexture,ARounded);
  else
    result := EmptyRect;
  end;
end;

constructor TBGRACustomTextEffect.Create(AMask: TBGRACustomBitmap; AMaskOwner: boolean; AWidth,
  AHeight: integer; AOffset: TPoint);
var
  grayMask: TGrayscaleMask;
begin
  grayMask := TGrayscaleMask.Create(AMask, cGreen);
  if AMaskOwner then AMask.Free;
  Init(grayMask, true, AWidth, AHeight, AOffset);
end;

constructor TBGRACustomTextEffect.Create(AMask: TGrayscaleMask;
  AMaskOwner: boolean; AWidth, AHeight: integer; AOffset: TPoint);
begin
  Init(AMask, AMaskOwner, AWidth, AHeight, AOffset);
end;

{$IFNDEF BGRABITMAP_CORE}procedure TBGRACustomTextEffect.ApplySphere;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRACustomReplace(FTextMask, FTextMask.FilterSphere);
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}procedure TBGRACustomTextEffect.ApplyVerticalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRACustomReplace(FTextMask, FTextMask.FilterCylinder);
end;{$ENDIF}

{$IFNDEF BGRABITMAP_CORE}procedure TBGRACustomTextEffect.ApplyHorizontalCylinder;
begin
  if FTextMask = nil then exit;
  FreeAndNil(FOutlineMask);
  FreeAndNil(FShadowMask);
  FShadowRadius := 0;
  BGRACustomReplace(FTextMask,FTextMask.RotateCW);
  BGRACustomReplace(FTextMask,FTextMask.FilterCylinder);
  BGRACustomReplace(FTextMask,FTextMask.RotateCCW);
end;{$ENDIF}

function TBGRACustomTextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel): TRect;
begin
  result := DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

function TBGRACustomTextEffect.Draw(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner): TRect;
begin
  result := DrawMask(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

function TBGRACustomTextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X,
  Y: integer; const AColors: array of TBGRAPixel): TRect;
begin
  result := DrawMaskMulticolored(ADest,FTextMask,X+FOffset.X,Y+FOffset.Y,AColors);
end;

function TBGRACustomTextEffect.DrawMulticolored(ADest: TBGRACustomBitmap; X,
  Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawMulticolored(ADest,X-TextSize.cx,Y,AColors);
  taCenter: result := DrawMulticolored(ADest,X-TextSize.cx div 2,Y,AColors);
  else result := DrawMulticolored(ADest,X,Y,AColors);
  end;
end;

function TBGRACustomTextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel): TRect;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.Negative;
  end;
  result := DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,AColor);
end;

function TBGRACustomTextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner): TRect;
begin
  if (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := EmptyRect;
    exit;
  end;
  if FOutlineMask = nil then
  begin
    FOutlineMask := FTextMask.FilterContour;
    FOutlineMask.Negative;
  end;
  result := DrawMask(ADest,FOutlineMask,X+FOffset.X,Y+FOffset.Y,ATexture);
end;

function TBGRACustomTextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawOutline(ADest,X-TextSize.cx,Y,AColor);
  taCenter: result := DrawOutline(ADest,X-TextSize.cx div 2,Y,AColor);
  else result := DrawOutline(ADest,X,Y,AColor);
  end;
end;

function TBGRACustomTextEffect.DrawOutline(ADest: TBGRACustomBitmap; X, Y: integer;
  ATexture: IBGRAScanner; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawOutline(ADest,X-TextSize.cx,Y,ATexture);
  taCenter: result := DrawOutline(ADest,X-TextSize.cx div 2,Y,ATexture);
  else result := DrawOutline(ADest,X,Y,ATexture);
  end;
end;

function TBGRACustomTextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel): TRect;
begin
  if AColor.alpha = 0 then exit(EmptyRect);
  if (Radius <= 0) or (FTextMask = nil) or (FTextMask.Width = 0) or (FTextMask.Height = 0) then
  begin
    result := Draw(ADest,X,Y,AColor);
    exit;
  end;
  if (FShadowRadius <> Radius) or (FShadowMask = nil) then
  begin
    FShadowRadius := Radius;
    FreeAndNil(FShadowMask);
    FShadowMask := TGrayscaleMask.Create(FTextMask.Width+Radius*2,FTextMask.Height+Radius*2, 0);
    FShadowMask.PutImage(Radius,Radius,FTextMask,dmSet);
    BGRACustomReplace(FShadowMask, BGRAFilterBlur.FilterBlurRadial(FShadowMask, Radius, Radius, ShadowQuality));
  end;
  Inc(X,FOffset.X-Radius);
  Inc(Y,FOffset.Y-Radius);
  DrawMask(ADest,FShadowMask,X,Y,AColor);
  result := rect(X,Y,X+FShadowMask.Width,Y+FShadowMask.Height);
end;

function TBGRACustomTextEffect.DrawShadow(ADest: TBGRACustomBitmap; X, Y,
  Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect;
begin
  Case AAlign of
  taRightJustify: result := DrawShadow(ADest,X-TextSize.cx,Y,Radius,AColor);
  taCenter: result := DrawShadow(ADest,X-TextSize.cx div 2,Y,Radius,AColor);
  else result := DrawShadow(ADest,X,Y,Radius,AColor);
  end;
end;

destructor TBGRACustomTextEffect.Destroy;
begin
  FShadowMask.free;
  FTextMask.Free;
  FOutlineMask.Free;
  FShadingMask.Free;
  inherited Destroy;
end;

end.

