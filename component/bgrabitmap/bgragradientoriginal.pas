// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Original that renders a gradient. Originals can be used in a layered image }
unit BGRAGradientOriginal;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner,
  BGRASVG, BGRASVGShapes, BGRASVGType;

type
  TBGRAColorInterpolation = BGRAGradientScanner.TBGRAColorInterpolation;
  TBGRAGradientRepetition = BGRAGradientScanner.TBGRAGradientRepetition;
  TBGRAGradientOriginalDiff = BGRALayerOriginal.TBGRAOriginalStorageDiff;

  { Original for rendering a gradient }
  TBGRALayerGradientOriginal = class(TBGRALayerCustomOriginal)
  private
    function GetColorByIndex(AIndex: integer): TBGRAPixel;
    function GetColorCount: integer;
    function GetColorPositionByIndex(AIndex: integer): single;
    function GetEndColor: TBGRAPixel;
    function GetIsOpaque: boolean;
    function GetStartColor: TBGRAPixel;
    procedure SetColorByIndex(AIndex: integer; AValue: TBGRAPixel);
    procedure SetColorCount(AValue: integer);
    procedure SetColorInterpolation(AValue: TBGRAColorInterpolation);
    procedure SetColorPositionByIndex(AIndex: integer; AValue: single);
    procedure SetEndColor(AValue: TBGRAPixel);
    procedure SetFocalPoint(AValue: TPointF);
    procedure SetFocalRadius(AValue: Single);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetOrigin(AValue: TPointF);
    procedure SetRadius(AValue: Single);
    procedure SetRepetition(AValue: TBGRAGradientRepetition);
    procedure SetStartColor(AValue: TBGRAPixel);
    procedure SetXAxis(AValue: TPointF);
    procedure SetYAxis(AValue: TPointF);
  protected
    FColors: array of TBGRAPixel;
    FColorPositions: array of single;
    FGradientType: TGradientType;
    FOrigin,FXAxis,FYAxis,FFocalPoint: TPointF;
    FOriginBackup,FXAxisBackup, FYAxisBackup: TPointF;
    FRadius,FFocalRadius: single;
    FColorInterpolation: TBGRAColorInterpolation;
    FRepetition: TBGRAGradientRepetition;
    FUpdateCount: integer;
    FUpdateDiff: TBGRAGradientOriginalDiff;
    function GetAverageColor: TBGRAPixel;
    function GetComputedRadius: single;
    function GetComputedYAxis: TPointF;
    function GetComputedFocalPoint: TPointF;
    function GetComputedFocalRadius: single;
    procedure OnMoveOrigin({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveXAxisNeg({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveYAxis({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveFocalPoint({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnMoveFocalRadius({%H-}ASender: TObject; {%H-}APrevCoord, ANewCoord: TPointF; {%H-}AShift: TShiftState);
    procedure OnStartMove({%H-}ASender: TObject; {%H-}AIndex: integer; {%H-}AShift: TShiftState);
    procedure BeginUpdate;
    procedure EndUpdate;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject; override;
    function AddToSVGDefs(const AMatrix: TAffineMatrix; ADefs: TSVGDefine): TObject;
    function IsInfiniteSurface: boolean; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean); overload; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix; ADraft: boolean; ADrawMode: TDrawMode); overload;
    function CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean = false): TBGRACustomScanner;
    procedure ConfigureEditor(AEditor: TBGRAOriginalEditor); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    class function CanConvertToSVG: boolean; override;
    property ComputedYAxis: TPointF read GetComputedYAxis;
    property ComputedRadius: single read GetComputedRadius;
    property ComputedFocalPoint: TPointF read GetComputedFocalPoint;
    property ComputedFocalRadius: single read GetComputedFocalRadius;
    procedure Transform(AMatrix: TAffineMatrix);
    procedure AssignExceptGeometry(AOther: TBGRALayerGradientOriginal);
    procedure FitGeometry(const ABox: TAffineBox);
    procedure SetColors(AStartColor, AEndColor: TBGRAPixel);
    procedure SetColors(const AColors: array of TBGRAPixel; ASpreadColorsEqually: boolean = true);
    procedure SetColors(const AColors: array of TBGRAPixel; const APositions: Array of single);
    procedure AddColor(AColor: TBGRAPixel; APreviousColorPosition: single = EmptySingle);
    procedure ApplyOpacity(AOpacity: byte);
    function Equals(Obj: TObject): boolean; override;

    property StartColor: TBGRAPixel read GetStartColor write SetStartColor;
    property EndColor: TBGRAPixel read GetEndColor write SetEndColor;
    property Color[AIndex: integer]: TBGRAPixel read GetColorByIndex write SetColorByIndex;
    property ColorPosition[AIndex: integer]: single read GetColorPositionByIndex write SetColorPositionByIndex;
    property ColorCount: integer read GetColorCount write SetColorCount;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property GradientType: TGradientType read FGradientType write SetGradientType;   //default gtLinear
    property Origin: TPointF read FOrigin write SetOrigin;
    property XAxis: TPointF read FXAxis write SetXAxis;
    property YAxis: TPointF read FYAxis write SetYAxis;
    property FocalPoint: TPointF read FFocalPoint write SetFocalPoint;     //default Origin
    property Radius: Single read FRadius write SetRadius;                  //default 1
    property FocalRadius: Single read FFocalRadius write SetFocalRadius;   //default 0
    property ColorInterpolation: TBGRAColorInterpolation read FColorInterpolation write SetColorInterpolation;
    property Repetition: TBGRAGradientRepetition read FRepetition write SetRepetition;
    property IsOpaque: boolean read GetIsOpaque;

  end;

implementation

uses BGRATransform, BGRABlend, math;

{ TBGRALayerGradientOriginal }

function TBGRALayerGradientOriginal.GetComputedRadius: single;
begin
  if FRadius = EmptySingle then result := 1 else result := FRadius;
end;

function TBGRALayerGradientOriginal.GetAverageColor: TBGRAPixel;
begin
  result := MergeBGRAWithGammaCorrection(StartColor, 1, EndColor, 1);
end;

function TBGRALayerGradientOriginal.GetColorByIndex(AIndex: integer
  ): TBGRAPixel;
begin
  if (AIndex < 0) or (AIndex >= ColorCount) then
     raise ERangeError.Create('Index out of range');
  result := FColors[AIndex];
end;

function TBGRALayerGradientOriginal.GetColorCount: integer;
begin
  result := length(FColors);
end;

function TBGRALayerGradientOriginal.GetColorPositionByIndex(AIndex: integer
  ): single;
begin
  if (AIndex < 0) or (AIndex >= ColorCount) then
     raise ERangeError.Create('Index out of range');
  result := FColorPositions[AIndex];
end;

function TBGRALayerGradientOriginal.GetEndColor: TBGRAPixel;
begin
  result := FColors[high(FColors)];
end;

function TBGRALayerGradientOriginal.GetIsOpaque: boolean;
var
  xLen, yLen, focalLen: Single;
  focalCoord, u, v: TPointF;
begin
  result := (StartColor.alpha = 255) and (EndColor.alpha = 255);
  if result and (GradientType = gtRadial) and not FocalPoint.IsEmpty and
    not Origin.IsEmpty and not XAxis.IsEmpty then
  begin
    u := XAxis - Origin;
    v := ComputedYAxis - Origin;
    xLen := VectLen(u);
    yLen := VectLen(v);
    if (xLen = 0) or (yLen = 0) then
      result := false
    else
    begin
      focalCoord := PointF((FocalPoint - Origin)**u/sqr(xLen),
                           (FocalPoint - Origin)**v/sqr(yLen));
      focalLen := VectLen(focalCoord);
      if (focalLen + ComputedFocalRadius + 0.01 >= ComputedRadius) and not
        (ComputedFocalRadius > focalLen + ComputedRadius + 0.01) then
        result := false;
    end;
  end;
end;

function TBGRALayerGradientOriginal.GetStartColor: TBGRAPixel;
begin
  result := FColors[0];
end;

procedure TBGRALayerGradientOriginal.SetColorByIndex(AIndex: integer;
  AValue: TBGRAPixel);
begin
  if (AIndex < 0) or (AIndex >= ColorCount) then
     raise ERangeError.Create('Index out of range');
  BeginUpdate;
  FColors[AIndex] := AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColorCount(AValue: integer);
var
  prevCount, i: Integer;
begin
  if AValue < 2 then
    raise ERangeError.Create('There must be at least 2 colors');
  if AValue = ColorCount then exit;
  prevCount := ColorCount;
  BeginUpdate;
  setlength(FColors, AValue);
  setlength(FColorPositions, AValue);
  if AValue > prevCount then
  begin
    for i := prevCount to AValue - 1 do
    begin
      FColors[i] := FColors[prevCount - 1];
      FColorPositions[i] := FColorPositions[prevCount - 1];
    end;
  end else
  if AValue < prevCount then
  begin
    FColorPositions[AValue-1] := 1;
  end;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColorInterpolation(
  AValue: TBGRAColorInterpolation);
begin
  if FColorInterpolation=AValue then Exit;
  BeginUpdate;
  FColorInterpolation:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColorPositionByIndex(AIndex: integer;
  AValue: single);
begin
  if (AIndex < 0) or (AIndex >= ColorCount) then
     raise ERangeError.Create('Index out of range');
  if (AIndex = 0) and (AValue <> 0) then
     raise ERangeError.Create('First position must be 0');
  if (AIndex = ColorCount-1) and (AValue <> 1) then
     raise ERangeError.Create('Last position must be 1');
  BeginUpdate;
  FColorPositions[AIndex] := AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetEndColor(AValue: TBGRAPixel);
begin
  if EndColor.EqualsExactly(AValue) then Exit;
  BeginUpdate;
  FColors[high(FColors)]:= AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetFocalPoint(AValue: TPointF);
begin
  if FFocalPoint=AValue then Exit;
  BeginUpdate;
  FFocalPoint:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetFocalRadius(AValue: Single);
begin
  if FFocalRadius=AValue then Exit;
  BeginUpdate;
  FFocalRadius:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetGradientType(AValue: TGradientType);
begin
  if FGradientType=AValue then Exit;
  BeginUpdate;
  FGradientType:=AValue;
  if FGradientType in [gtLinear,gtReflected] then FYAxis := EmptyPointF;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetOrigin(AValue: TPointF);
begin
  if FOrigin=AValue then Exit;
  BeginUpdate;
  FOrigin:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetRadius(AValue: Single);
begin
  if FRadius=AValue then Exit;
  BeginUpdate;
  FRadius:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetRepetition(
  AValue: TBGRAGradientRepetition);
begin
  if FRepetition=AValue then Exit;
  BeginUpdate;
  FRepetition:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetStartColor(AValue: TBGRAPixel);
begin
  if StartColor.EqualsExactly(AValue) then Exit;
  BeginUpdate;
  FColors[0]:= AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetXAxis(AValue: TPointF);
begin
  if FXAxis=AValue then Exit;
  BeginUpdate;
  FXAxis:=AValue;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetYAxis(AValue: TPointF);
begin
  if FYAxis=AValue then Exit;
  BeginUpdate;
  FYAxis:=AValue;
  EndUpdate;
end;

function TBGRALayerGradientOriginal.GetComputedYAxis: TPointF;
var
  u: TPointF;
begin
  if isEmptyPointF(FYAxis) then
  begin
    u := FXAxis - FOrigin;
    result := FOrigin + PointF(-u.y,u.x)
  end
  else
    result := FYAxis;
end;

function TBGRALayerGradientOriginal.GetComputedFocalPoint: TPointF;
begin
  if isEmptyPointF(FFocalPoint) then result := FOrigin else result := FFocalPoint;
end;

function TBGRALayerGradientOriginal.GetComputedFocalRadius: single;
begin
  if FFocalRadius = EmptySingle then result := 0 else result := FFocalRadius;
end;

procedure TBGRALayerGradientOriginal.OnMoveOrigin(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  delta: TPointF;
begin
  BeginUpdate;
  delta := ANewCoord-APrevCoord;
  FOrigin.Offset(delta);
  FXAxis.Offset(delta);
  FYAxis.Offset(delta);
  FFocalPoint.Offset(delta);
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.OnMoveXAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  m: TAffineMatrix;
  c: TPointF;
begin
  BeginUpdate;
  if not (ssAlt in AShift) or (GradientType in [gtLinear,gtReflected]) then
  begin
    if not isEmptyPointF(FYAxis) and not isEmptyPointF(FYAxisBackup) then
    begin
      m := AffineMatrixScaledRotation(FXAxisBackup, ANewCoord, FOrigin);
      FYAxis := m*FYAxisBackup;
    end;
  end else
    if isEmptyPointF(FYAxis) then FYAxis := ComputedYAxis;

  if (GradientType = gtLinear) and (ssShift in AShift) then
  begin
    c := (FOriginBackup+FXAxisBackup)*0.5;
    m := AffineMatrixScaledRotation(FXAxisBackup, ANewCoord, c);
    FOrigin := m*FOriginBackup;
  end
  else
    FOrigin := FOriginBackup;

  FXAxis := ANewCoord;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.OnMoveXAxisNeg(ASender: TObject;
  APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
var
  delta, c: TPointF;
  m: TAffineMatrix;
begin
  BeginUpdate;
  delta := ANewCoord-APrevCoord;

  if (GradientType = gtLinear) and (ssShift in AShift) then
  begin
    c := (FOriginBackup+FXAxisBackup)*0.5;
    m := AffineMatrixScaledRotation(FOriginBackup, (FOrigin+delta), c);
    FXAxis := m*FXAxisBackup;
  end
  else
    FXAxis := FXAxisBackup;

  FOrigin.Offset(delta);
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.OnMoveYAxis(ASender: TObject; APrevCoord,
  ANewCoord: TPointF; AShift: TShiftState);
var
  m: TAffineMatrix;
begin
  BeginUpdate;
  if not (ssAlt in AShift) or (GradientType in [gtLinear,gtReflected]) then
  begin
    if not isEmptyPointF(FXAxis) then
    begin
      m := AffineMatrixScaledRotation(FYAxisBackup, ANewCoord, FOrigin);
      FXAxis := m*FXAxisBackup;
    end;
  end;
  FYAxis := ANewCoord;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.OnMoveFocalPoint(ASender: TObject;
  APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
begin
  FocalPoint := ANewCoord;
end;

procedure TBGRALayerGradientOriginal.OnMoveFocalRadius(ASender: TObject;
  APrevCoord, ANewCoord: TPointF; AShift: TShiftState);
var refLen: single;
  u, focalOrig: TPointF;
begin
  BeginUpdate;
  focalOrig := ComputedFocalPoint;
  if isEmptyPointF(focalOrig) or isEmptyPointF(FOrigin) or isEmptyPointF(FXAxis) then exit;
  refLen := VectLen(FOrigin-FXAxis);
  if refLen = 0 then exit;

  u := (FOrigin-FXAxis)*(1/refLen);
  FFocalRadius := u ** (ANewCoord-focalOrig) / refLen - 0.1;
  if FFocalRadius < 0 then FFocalRadius:= 0;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.OnStartMove(ASender: TObject;
  AIndex: integer; AShift: TShiftState);
begin
  FOriginBackup := FOrigin;
  FXAxisBackup := FXAxis;
  FYAxisBackup := ComputedYAxis;
end;

procedure TBGRALayerGradientOriginal.BeginUpdate;
begin
  if DiffExpected and (FUpdateCount = 0) then
    FUpdateDiff := TBGRAGradientOriginalDiff.Create(self);
  inc(FUpdateCount);
end;

procedure TBGRALayerGradientOriginal.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      if Assigned(FUpdateDiff) then
        FUpdateDiff.ComputeDifference(self);
      NotifyChange(FUpdateDiff);
      FUpdateDiff := nil;
    end;
  end;
end;

constructor TBGRALayerGradientOriginal.Create;
begin
  inherited Create;
  setLength(FColors, 2);
  FColors[0] := BGRABlack;
  FColors[1] := BGRAWhite;
  setlength(FColorPositions, 2);
  FColorPositions[0] := 0;
  FColorPositions[1] := 1;
  FGradientType := gtLinear;
  FColorInterpolation:= ciStdRGB;
  FRepetition := grPad;
  FRadius := EmptySingle;
  FFocalRadius := EmptySingle;
  FFocalPoint := EmptyPointF;
  FOrigin := PointF(0,0);
  FXAxis := EmptyPointF;
  FYAxis := EmptyPointF;
end;

destructor TBGRALayerGradientOriginal.Destroy;
begin
  FUpdateDiff.Free;
  inherited Destroy;
end;

function TBGRALayerGradientOriginal.ConvertToSVG(const AMatrix: TAffineMatrix; out AOffset: TPoint): TObject;
var
  svg: TBGRASVG;
  def: TSVGDefine;
  grad: TSVGGradient;
  r: TSVGRectangle;
begin
  AOffset:= Point(0, 0);
  svg := TBGRASVG.Create(640, 480, cuPixel);  // potentially infinite
  result := svg;
  def := svg.Content.AppendDefine;
  grad := AddToSVGDefs(AMatrix, def) as TSVGGradient;
  r := svg.Content.AppendRect(0, 0, 100, 100, cuPercent);
  if Assigned(grad) then
  begin
    grad.ID := 'grad1';
    r.fill:= 'url(#grad1)';
  end else
    r.fillColor := AverageColor;
end;

function TBGRALayerGradientOriginal.AddToSVGDefs(const AMatrix: TAffineMatrix;
  ADefs: TSVGDefine): TObject;
const ApproxCount = 16;
  MaxReflectRepeatCount = 8;
var
  grad: TSVGGradient;
  colors: TBGRASimpleGradient;
  tOrigin, tXAxis, tYAxis, tFocalPoint, reflectedXAxis, repeatedXAxis: TPointF;
  gt: TGradientType;

  procedure AddColorStops(AOffset, AFactor: single; AIncludeStart: boolean);
  var i, i0: integer;
  begin
    if (Repetition <> grSine) and (ColorInterpolation in [ciStdRGB, ciLinearRGB])  then
    begin
      if AFactor >= 0 then
      begin
        if AIncludeStart then
          grad.Content.AppendStop(StartColor, AOffset, false);
        grad.Content.AppendStop(EndColor, AOffset + AFactor*1, false);
      end else
      begin
        grad.Content.AppendStop(EndColor, AOffset + AFactor*1, false);
        if AIncludeStart then
          grad.Content.AppendStop(StartColor, AOffset, false);
      end;
    end else
    begin
      colors := TBGRASimpleGradient.CreateAny(ColorInterpolation, StartColor,EndColor, Repetition);
      try
        if AIncludeStart then i0 := 0 else i0 := 1;
        if AFactor >= 0 then
        begin
          for i := i0 to ApproxCount do
            grad.Content.AppendStop(colors.GetColorAtF(i/ApproxCount), AOffset + AFactor*i/ApproxCount, false);
        end else
          for i := ApproxCount downto i0 do
            grad.Content.AppendStop(colors.GetColorAtF(i/ApproxCount), AOffset + AFactor*i/ApproxCount, false);
      finally
        colors.Free;
      end;
    end;
  end;

var j: integer;
  m: TAffineMatrix;
  radialScale: Single;
  fp, u, v: TPointF;
  lenU, lenV: Single;

begin
  m := AffineMatrixTranslation(0.5, 0.5) * AMatrix;
  tOrigin := m * Origin;
  tXAxis := m * XAxis;
  tYAxis := m * ComputedYAxis;
  tFocalPoint := m * ComputedFocalPoint;
  gt := GradientType;
  if (GradientType = gtReflected) and (Repetition = grReflect) then
    gt := gtLinear; // same as linear in this case
  case gt of
  gtLinear:
    grad := ADefs.Content.AppendLinearGradient(tOrigin.X,tOrigin.Y,tXAxis.X,tXAxis.Y,cuCustom);
  gtReflected:
  begin
    if Repetition <> grPad then j := MaxReflectRepeatCount else j := 1;
    reflectedXAxis := tOrigin - j*(tXAxis - tOrigin);
    repeatedXAxis := tOrigin + j*(tXAxis - tOrigin);
    grad := ADefs.Content.AppendLinearGradient(reflectedXAxis.X,reflectedXAxis.Y,
      repeatedXAxis.X,repeatedXAxis.Y,cuCustom);
  end;
  gtDiamond, gtRadial: // diamond approximated by radial
    begin
      u := tXAxis - tOrigin;
      v := tYAxis - tOrigin;
      lenU := u.Length;
      lenV := v.Length;
      radialScale := (lenU + lenV)/2;
      if radialScale = 0 then
        grad := ADefs.Content.AppendRadialGradient(tOrigin.X,tOrigin.Y,0,
          tOrigin.X,tOrigin.Y,0, cuCustom)
      else if (lenU = lenV) and (u**v = 0) then
        grad := ADefs.Content.AppendRadialGradient(tOrigin.X,tOrigin.Y,radialScale*ComputedRadius,
          tFocalPoint.X,tFocalPoint.Y,radialScale*ComputedFocalRadius, cuCustom)
      else
      begin
        if lenU = 0 then lenU := 1;
        if lenV = 0 then lenV := 1;
        fp := PointF((tFocalPoint - tOrigin) ** u / sqr(lenU),
          (tFocalPoint - tOrigin)**v / sqr(lenV));
        tFocalPoint := tOrigin + (fp.x * radialScale / lenU) * u + (fp.y * radialScale / lenV) * v;
        grad := ADefs.Content.AppendRadialGradient(tOrigin.X,tOrigin.Y,radialScale*ComputedRadius,
          tFocalPoint.X,tFocalPoint.Y,radialScale*ComputedFocalRadius, cuCustom);
        grad.gradientMatrix[cuPixel] :=
          AffineMatrix((1 / radialScale)*u, (1 / radialScale)*v, tOrigin) *
          AffineMatrixTranslation(-tOrigin.X, -tOrigin.Y);
      end;
    end;
  gtAngular: exit(nil); // not implemented
  else
    raise Exception.Create('Unknown gradient type');
  end;
  case Repetition of
  grPad: grad.spreadMethod := ssmPad;
  grReflect: grad.spreadMethod := ssmReflect;
  grRepeat, grSine: grad.spreadMethod := ssmRepeat;
  end;
  if gt = gtReflected then
  begin
    if Repetition <> grPad then
    begin
      for j := -MaxReflectRepeatCount+1 to 0 do
        AddColorStops(0.5 + j/MaxReflectRepeatCount*0.5, -0.5/MaxReflectRepeatCount, true);
      for j := 0 to MaxReflectRepeatCount-1 do
        AddColorStops(0.5 + j*0.5/MaxReflectRepeatCount, 0.5/MaxReflectRepeatCount, j > 0);
    end else
    begin
      AddColorStops(0.5, -0.5, true);
      AddColorStops(0.5, 0.5, false);
    end;
  end else
    AddColorStops(0, 1, true);
  if ColorInterpolation = ciStdRGB then
    grad.colorInterpolation := sciStdRGB
    else grad.colorInterpolation := sciLinearRGB;
  result := grad;
end;

function TBGRALayerGradientOriginal.IsInfiniteSurface: boolean;
begin
  Result:= true;
end;

procedure TBGRALayerGradientOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
begin
  Render(ADest,AMatrix,ADraft,dmSet);
end;

procedure TBGRALayerGradientOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean; ADrawMode: TDrawMode);
var
  grad: TBGRACustomScanner;
  temp: TBGRABitmap;
begin
  if (ADrawMode in[dmDrawWithTransparency, dmLinearBlend, dmSetExceptTransparent]) and
    IsOpaque then ADrawMode := dmSet;

  if ADraft and (ADest.ClipRect.Width*ADest.ClipRect.Height > 512*512) then
  begin
    temp := TBGRABitmap.Create(0,0);
    temp.SetSize(min(400,ADest.Width),min(400,ADest.Height));
    Render(temp, AffineMatrixScale(temp.Width/ADest.Width,
                                   temp.Height/ADest.Height)*AMatrix, ADraft);
    ADest.StretchPutImage(rect(0,0,ADest.Width,Adest.Height),temp, ADrawMode);
    temp.Free;
  end else
  begin
    grad := CreateScanner(AMatrix, ADraft);
    if ADraft then
      ADest.FillRect(ADest.ClipRect, grad,ADrawMode)
      else ADest.FillRect(ADest.ClipRect, grad,ADrawMode, daFloydSteinberg);
    grad.Free;
  end;
end;

function TBGRALayerGradientOriginal.CreateScanner(AMatrix: TAffineMatrix; ADraft: boolean): TBGRACustomScanner;
var
  colors: TBGRACustomGradient;
  grad: TBGRAGradientScanner;
begin
  if isEmptyPointF(FOrigin) or isEmptyPointF(FXAxis) then exit(nil);

  if ColorCount = 2 then
    colors := TBGRASimpleGradient.CreateAny(FColorInterpolation,
           StartColor, EndColor, FRepetition)
  else
    colors := TBGRAMultiGradient.CreateAny(FColors,
           FColorPositions, FColorInterpolation,
           FRepetition);
  if ADraft then
    colors := TBGRABufferedGradient.Create(colors, true, FRepetition = grPad, 1024);

  if FGradientType = gtRadial then
  begin
    grad := TBGRAGradientScanner.Create(FOrigin,FXAxis,ComputedYAxis,ComputedFocalPoint,ComputedRadius,ComputedFocalRadius);
  end else
    grad := TBGRAGradientScanner.Create(FGradientType, FOrigin,FXAxis,ComputedYAxis);

  grad.SetGradient(colors, true);
  grad.Transform := AMatrix;

  exit(grad);
end;

procedure TBGRALayerGradientOriginal.ConfigureEditor(
  AEditor: TBGRAOriginalEditor);
var
  originPoint: Integer;
begin
  if not isEmptyPointF(FOrigin) then
  begin
    AEditor.AddStartMoveHandler(@OnStartMove);

    if not isEmptyPointF(FXAxis) and (FGradientType = gtLinear) then
      originPoint := AEditor.AddPoint((FOrigin + FXAxis)*0.5, @OnMoveOrigin, true)
    else originPoint := AEditor.AddPoint(FOrigin, @OnMoveOrigin, true);

    if not isEmptyPointF(FXAxis) then
    begin
      if not isEmptyPointF(FXAxis) and (FGradientType = gtLinear) then
      begin
        AEditor.AddArrow((FOrigin + FXAxis)*0.5, FXAxis, @OnMoveXAxis);
        AEditor.AddArrow((FOrigin + FXAxis)*0.5, FOrigin, @OnMoveXAxisNeg);
      end
      else AEditor.AddArrow(FOrigin, FXAxis, @OnMoveXAxis);

      if FGradientType in[gtDiamond, gtRadial, gtAngular] then
        AEditor.AddArrow(FOrigin, ComputedYAxis, @OnMoveYAxis);
    end;
    if FGradientType = gtRadial then
    begin
      AEditor.AddPoint(ComputedFocalPoint, @OnMoveFocalPoint, false, originPoint);
      AEditor.AddArrow(ComputedFocalPoint, ComputedFocalPoint - (FXAxis - FOrigin) * (ComputedFocalRadius + 0.1), @OnMoveFocalRadius, false);
    end;
  end;
end;

function TBGRALayerGradientOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
begin
  result := ADestRect;
end;

procedure TBGRALayerGradientOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  i: Integer;
begin
  FColors := AStorage.ColorArray['colors'];
  FColorPositions := AStorage.FloatArray['color-positions'];
  if FColorPositions = nil then
  begin
    setlength(FColorPositions, length(FColors));
    if length(FColorPositions) > 1 then
    begin
      for i := 0 to high(FColorPositions) do
        FColorPositions[i] := i / high(FColorPositions);
    end else
      FColorPositions[0] := 0;
  end;


  case AStorage.RawString['gradient-type'] of
  'reflected': FGradientType := gtReflected;
  'radial': FGradientType := gtRadial;
  'diamond': FGradientType := gtDiamond;
  'angular': FGradientType := gtAngular;
  else {'linear'} FGradientType := gtLinear;
  end;

  FOrigin := AStorage.PointF['origin'];
  FXAxis := AStorage.PointF['x-axis'];
  FYAxis := AStorage.PointF['y-axis'];
  FFocalPoint := AStorage.PointF['focal-point'];

  FRadius := AStorage.Float['radial'];
  FFocalRadius := AStorage.Float['focal-radius'];

  case AStorage.RawString['color-interpolation'] of
  'RGB': FColorInterpolation:= ciLinearRGB;
  'HSL': FColorInterpolation:= ciLinearHSLAuto;
  'HSL+': FColorInterpolation:= ciLinearHSLPositive;
  'HSL-': FColorInterpolation:= ciLinearHSLNegative;
  'GSB': FColorInterpolation:= ciGSBAuto;
  'GSB+': FColorInterpolation:= ciGSBPositive;
  'GSB-': FColorInterpolation:= ciGSBNegative;
  else {'sRGB'} FColorInterpolation:= ciStdRGB;
  end;

  case AStorage.RawString['repetition'] of
  'repeat': FRepetition:= grRepeat;
  'reflect': FRepetition:= grReflect;
  'sine': FRepetition := grSine;
  else {'pad'} FRepetition:= grPad;
  end;
end;

procedure TBGRALayerGradientOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  gtStr, ciStr: String;
begin
  AStorage.ColorArray['colors'] := FColors;
  AStorage.FloatArray['color-positions'] := FColorPositions;

  case FGradientType of
  gtReflected: gtStr := 'reflected';
  gtRadial: gtStr := 'radial';
  gtDiamond: gtStr := 'diamond';
  gtAngular: gtStr := 'angular';
  else {gtLinear} gtStr := 'linear';
  end;
  AStorage.RawString['gradient-type'] := gtStr;

  AStorage.PointF['origin'] := FOrigin;
  AStorage.PointF['x-axis'] := FXAxis;

  if FGradientType in[gtRadial,gtDiamond,gtAngular] then
    AStorage.PointF['y-axis'] := FYAxis
  else
    AStorage.RemoveAttribute('y-axis');

  if FGradientType = gtRadial then
  begin
    AStorage.Float['radius'] := FRadius;
    AStorage.Float['focal-radius'] := FFocalRadius;
    AStorage.PointF['focal-point'] := FFocalPoint;
  end else
  begin
    AStorage.RemoveAttribute('radius');
    AStorage.RemoveAttribute('focal-radius');
  end;

  case FColorInterpolation of
  ciLinearRGB: ciStr := 'RGB';
  ciLinearHSLAuto: ciStr := 'HSL';
  ciLinearHSLPositive: ciStr := 'HSL+';
  ciLinearHSLNegative: ciStr := 'HSL-';
  ciGSBAuto: ciStr := 'GSB';
  ciGSBPositive: ciStr := 'GSB+';
  ciGSBNegative: ciStr := 'GSB-';
  else {ciStdRGB} ciStr := 'sRGB';
  end;
  AStorage.RawString['color-interpolation'] := ciStr;

  case FRepetition of
  grRepeat: AStorage.RawString['repetition'] := 'repeat';
  grReflect: AStorage.RawString['repetition'] := 'reflect';
  grSine: AStorage.RawString['repetition'] := 'sine';
  else {grPad} AStorage.RawString['repetition'] := 'pad';
  end;
end;

class function TBGRALayerGradientOriginal.StorageClassName: RawByteString;
begin
  result := 'gradient';
end;

class function TBGRALayerGradientOriginal.CanConvertToSVG: boolean;
begin
  Result:= true;
end;

procedure TBGRALayerGradientOriginal.Transform(AMatrix: TAffineMatrix);
begin
  BeginUpdate;
  if not isEmptyPointF(FOrigin) then FOrigin := AMatrix*FOrigin;
  if not isEmptyPointF(FXAxis) then FXAxis := AMatrix*FXAxis;
  if not isEmptyPointF(FYAxis) then FYAxis := AMatrix*FYAxis;
  if not isEmptyPointF(FFocalPoint) then FFocalPoint := AMatrix*FFocalPoint;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.AssignExceptGeometry(
  AOther: TBGRALayerGradientOriginal);
begin
  if (GradientType = AOther.GradientType) and
    (StartColor.EqualsExactly(AOther.StartColor)) and
    (EndColor.EqualsExactly(AOther.EndColor)) and
    (ColorInterpolation = AOther.ColorInterpolation) and
    (Repetition = AOther.Repetition) then exit;
  BeginUpdate;
  GradientType := AOther.GradientType;
  StartColor:= AOther.StartColor;
  EndColor:= AOther.EndColor;
  ColorInterpolation:= AOther.ColorInterpolation;
  Repetition:= AOther.Repetition;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.FitGeometry(const ABox: TAffineBox);
begin
  BeginUpdate;
  if GradientType = gtLinear then
  begin
    Origin := ABox.TopLeft;
    XAxis := ABox.BottomRight;
  end else
  begin
    Origin := (ABox.TopLeft + ABox.BottomRight)*0.5;
    if GradientType = gtReflected then
      XAxis := ABox.BottomRight
    else
    begin
      XAxis := (ABox.TopRight + ABox.BottomRight)*0.5;
      YAxis := (ABox.BottomLeft + ABox.BottomRight)*0.5;
    end;
  end;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColors(AStartColor,
  AEndColor: TBGRAPixel);
begin
  if (AStartColor = StartColor) and (AEndColor = EndColor) and (ColorCount = 2) then exit;
  BeginUpdate;
  ColorCount := 2;
  StartColor := AStartColor;
  EndColor := AEndColor;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.ApplyOpacity(AOpacity: byte);
var
  cStart, cEnd: TBGRAPixel;
begin
  cStart := StartColor;
  cStart.alpha := BGRABlend.ApplyOpacity(cStart.alpha, AOpacity);
  cEnd := EndColor;
  cEnd.alpha := BGRABlend.ApplyOpacity(cEnd.alpha, AOpacity);
  SetColors(cStart, cEnd);
end;

function TBGRALayerGradientOriginal.Equals(Obj: TObject): boolean;
var
  other: TBGRALayerGradientOriginal;
begin
  if Obj is TBGRALayerGradientOriginal then
  begin
    other := TBGRALayerGradientOriginal(Obj);
    result := StartColor.EqualsExactly(other.StartColor) and
              EndColor.EqualsExactly(other.EndColor) and
              (GradientType = other.GradientType) and
              (Origin = other.Origin) and
              (XAxis = other.XAxis) and
              ((GradientType in[gtLinear, gtReflected]) or
               (YAxis = other.YAxis)) and
              ((GradientType <> gtRadial) or
               ((FocalPoint = other.FocalPoint) and
                (FocalRadius = other.FocalRadius))) and
              (ColorInterpolation = other.ColorInterpolation) and
              (Repetition = other.Repetition);
  end else
    Result:=inherited Equals(Obj);
end;

procedure TBGRALayerGradientOriginal.AddColor(AColor: TBGRAPixel; APreviousColorPosition: single);
var
  prevCount: Integer;
begin
  prevCount := ColorCount;
  BeginUpdate;
  setlength(FColors, prevCount+1);
  setlength(FColorPositions, prevCount+1);
  FColors[prevCount] := AColor;
  FColorPositions[prevCount] := FColorPositions[prevCount - 1];
  if APreviousColorPosition <> EmptySingle then
  begin
    FColorPositions[prevCount - 1] := APreviousColorPosition;
    if FColorPositions[prevCount] < APreviousColorPosition then
      FColorPositions[prevCount] := APreviousColorPosition;
  end;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColors(
  const AColors: array of TBGRAPixel; ASpreadColorsEqually: boolean);
var
  i: Integer;
begin
  if length(AColors) < 2 then
    exception.Create('At least two colors expected');
  BeginUpdate;
  ColorCount := length(AColors);
  for i := 0 to high(AColors) do
    FColors[i] := AColors[i];
  if ASpreadColorsEqually then
  begin
    for i := 0 to high(AColors) do
      FColorPositions[i] := i / (length(AColors) - 1);
  end;
  EndUpdate;
end;

procedure TBGRALayerGradientOriginal.SetColors(
  const AColors: array of TBGRAPixel; const APositions: array of single);
var
  i: Integer;
begin
  if length(AColors) < 2 then
    exception.Create('At least two colors expected');
  if length(APositions) <> length(AColors) then
    exception.Create('Too many color positions');
  if (APositions[0] <> 0) or (APositions[high(APositions)] <> 1) then
    exception.Create('Positions must start at 0 and end at 1');
  BeginUpdate;
  ColorCount := length(AColors);
  for i := 0 to high(AColors) do
  begin
    FColors[i] := AColors[i];
    FColorPositions[i] := APositions[i];
  end;
  EndUpdate;
end;

initialization

  RegisterLayerOriginal(TBGRALayerGradientOriginal);

end.
