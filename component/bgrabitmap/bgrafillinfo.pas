// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Generic class to hold a shape to be filled as well as the implementation
  for basic shapes }
unit BGRAFillInfo;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes;

const
  AntialiasPrecision = 16;
  AntialiasPrecisionShift = 4;

type
  TDensity = word;
  PDensity = ^TDensity;

type
  { Abstract class to provide fill information for any shape }
  TFillShapeInfo = class(TBGRACustomFillInfo)
    protected
      FPointInsideInter : ArrayOfTIntersectionInfo;
      //compute intersections. the array must be big enough
      procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //sort from left to right
      procedure SortIntersection(var inter: ArrayOfTIntersectionInfo; nbInter: integer); virtual;
      procedure InternalQuickSortIntersection(inter0: pointer; idxL, idxH: Integer); virtual;
      //apply non-zero winding rule. it can change the number of intersections
      procedure ConvertFromNonZeroWinding(var inter: ArrayOfTIntersectionInfo; var nbInter: integer); virtual;
      //returns maximum of intersection per line
      function NbMaxIntersection: integer; virtual;

    public
      destructor Destroy; override;

      //returns true if the same segment number can be curved
      function SegmentsCurved: boolean; override;

      //returns integer bounds
      function GetBounds: TRect; override;

      //check if the point is inside the filling zone
      function IsPointInside(x,y: single; windingMode: boolean): boolean; override;

      //create an array that will contain computed intersections.
      //you may augment, in this case, use CreateIntersectionInfo for new items
      function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
      function CreateIntersectionInfo: TIntersectionInfo; override; //creates a single info
      procedure FreeIntersectionArray(var inter: ArrayOfTIntersectionInfo); override;

      //fill a previously created array of intersections with actual intersections at the current y coordinate.
      //nbInter gets the number of computed intersections
      procedure ComputeAndSort(cury: single; var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean); override;

      //can be called after ComputeAndSort or ComputeIntersection to determine the current horizontal slice
      //so that it can be checked if the intermediates scanlines can be skipped
      function GetSliceIndex: integer; override;

  end;

  { Fill information for an ellipse }
  TFillEllipseInfo = class(TFillShapeInfo)
  private
    FX, FY, FRX, FRY: single;
    FSliceIndex: integer;
    function GetCenter: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x, y, rx, ry: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    function GetSliceIndex: integer; override;
    property Center: TPointF read GetCenter;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { Fill information for the border of an ellipse }
  TFillBorderEllipseInfo = class(TFillShapeInfo)
  private
    FInnerBorder, FOuterBorder: TFillEllipseInfo;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x, y, rx, ry, w: single);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    function GetSliceIndex: integer; override;
    property InnerBorder: TFillEllipseInfo read FInnerBorder;
    property OuterBorder: TFillEllipseInfo read FOuterBorder;
  end;

  { Fill information for a round rectangle }
  TFillRoundRectangleInfo = class(TFillShapeInfo)
  private
    FX1, FY1, FX2, FY2, FRX, FRY: single;
    FOptions: TRoundRectangleOptions;
    function GetBottomRight: TPointF;
    function GetTopLeft: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x1, y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean = true);
    function SegmentsCurved: boolean; override;
    function GetBounds: TRect; override;
    property TopLeft: TPointF read GetTopLeft;
    property BottomRight: TPointF read GetBottomRight;
    property RadiusX: single read FRX;
    property RadiusY: single read FRY;
  end;

  { Fill information for rectangle }
  TFillRectangleInfo = class(TFillShapeInfo)
  private
    FX1, FY1, FX2, FY2: single;
    function GetBottomRight: TPointF;
    function GetTopLeft: TPointF;
  protected
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    WindingFactor: integer;
    constructor Create(x1, y1, x2, y2: single; APixelCenteredCoordinates: boolean = true);
    function GetBounds: TRect; override;
    property TopLeft: TPointF read GetTopLeft;
    property BottomRight: TPointF read GetBottomRight;
  end;

  { Fill information for the border of a round rectangle }
  TFillBorderRoundRectInfo = class(TFillShapeInfo)
  protected
    FInnerBorder, FOuterBorder: TFillRoundRectangleInfo;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(x1, y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean = true);
    function GetBounds: TRect; override;
    function SegmentsCurved: boolean; override;
    destructor Destroy; override;
    property InnerBorder: TFillRoundRectangleInfo read FInnerBorder;
    property OuterBorder: TFillRoundRectangleInfo read FOuterBorder;
  end;

  PCustomPointRecord = ^TCustomPointRecord;
  { Base record to describe a point in a polygon to be filled }
  TCustomPointRecord = record
    originalIndex: integer;
    slope: single;
    empty: boolean;
    next: integer;
    winding: integer;
    includeStartingPoint,includeEndingPoint: boolean;
    data: pointer;
    case boolean of
    false: (x,y,x2,y2: single);
    true: (coord,coord2: TPointF);
  end;

  { Abstract class to provide fill information for a polygon }
  TCustomFillPolyInfo = class(TFillShapeInfo)
  private
    function GetNbPoints: integer;
  protected
    FPoints: array of TCustomPointRecord;
    FSegmentsDataCreated: boolean;
    FBoundsF: TRectF;
    function NbMaxIntersection: integer; override;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding, ANumSegment: integer; {%H-}dy: single; {%H-}AData: pointer); virtual;
    procedure InitPoints(const points: array of TPointF);
    procedure CreateSegmentsData; virtual;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    destructor Destroy; override;
    function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
    function CreateSegmentData({%H-}numPt, {%H-}nextPt: integer; {%H-}ASeg: PCustomPointRecord): pointer; virtual;
    procedure FreeSegmentData(data: pointer); virtual;
    function GetBounds: TRect; override;
    function GetBoundsF: TRectF;
    property NbPoints: integer read GetNbPoints;
  end;

  { Horizontal strip of a polygon being filled }
  TPolySlice = record
    y1,y2: single;
    segments: array of record
                id: integer;
                custom: PCustomPointRecord;
              end;
    nbSegments: integer;
  end;

  { Fill information for a polygon }
  TFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    FSlices:   array of TPolySlice;
    FCurSlice: integer;
    FMaxIntersection: integer;
    function NbMaxIntersection: integer; override;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    function GetSliceIndex: integer; override;
  end;

  POnePassRecord = ^TOnePassRecord;
  { Linked list of coordinates for one-pass drawing }
  TOnePassRecord = record
                id: integer;
                custom: PCustomPointRecord;
                next: POnePassRecord;
                nextWaiting: POnePassRecord;
                nextDrawing: POnePassRecord;
            end;

  { Fill information for a polygon assuming that queries are always in increasying Y. }
  TOnePassFillPolyInfo = class(TCustomFillPolyInfo)
  private
    procedure InsertionSortByY;
    function PartitionByY(left, right: integer): integer;
    procedure QuickSortByY(left, right: integer);
    procedure SortByY;
  protected
    FOnePass: array of TOnePassRecord;
    FSortedByY: array of POnePassRecord;
    FFirstWaiting, FFirstDrawing: POnePassRecord;
    FShouldInitializeDrawing: boolean;
    FSliceIndex: integer;
    procedure ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
    function CreateIntersectionArray: ArrayOfTIntersectionInfo; override;
    function GetSliceIndex: integer; override;
  end;

  { Fill information for a simple polygons }
  TSimpleFillPolyInfo = class(TCustomFillPolyInfo)
  protected
    procedure ComputeIntersection(cury: single; var inter: ArrayOfTIntersectionInfo;
      var nbInter: integer); override;
  public
    constructor Create(const points: array of TPointF; APixelCenteredCoordinates: boolean = true);
  end;

procedure AddDensity(dest: PDensity; start,count: integer; value : word); inline;
function DivByAntialiasPrecision(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision256(value: UInt32or64): UInt32or64; inline;
function DivByAntialiasPrecision65536(value: UInt32or64): UInt32or64; inline;
procedure ComputeAliasedRowBounds(x1,x2: single; minx,maxx: integer; out ix1,ix2: integer);

function IsPointInPolygon(const points: ArrayOfTPointF; point: TPointF; windingMode: boolean): boolean;
function IsPointInEllipse(x,y,rx,ry: single; point: TPointF): boolean;
function IsPointInRoundRectangle(x1, y1, x2, y2, rx, ry: single; point: TPointF): boolean;
function IsPointInRectangle(x1, y1, x2, y2: single; point: TPointF): boolean;

function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  bmpDest: TBGRACustomBitmap): boolean; overload;
function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  clip: TRect): boolean; overload;

implementation

uses Math;

function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  bmpDest: TBGRACustomBitmap): boolean;
begin
  result := BGRAShapeComputeMinMax(AShape, minx,miny,maxx,maxy, bmpDest.ClipRect);
end;

function BGRAShapeComputeMinMax(AShape: TBGRACustomFillInfo; out minx, miny, maxx, maxy: integer;
  clip: TRect): boolean;
var bounds: TRect;
begin
  result := true;
  bounds := AShape.GetBounds;

  if (bounds.Right <= bounds.left) or (bounds.bottom <= bounds.top) then
  begin
    result := false;
    exit;
  end;

  miny := bounds.top;
  maxy := bounds.bottom - 1;
  minx := bounds.left;
  maxx := bounds.right - 1;

  if minx < clip.Left then
    minx := clip.Left;
  if maxx < clip.Left then
    result := false;

  if maxx > clip.Right - 1 then
    maxx := clip.Right- 1;
  if minx > clip.Right - 1 then
    result := false;

  if miny < clip.Top then
    miny := clip.Top;
  if maxy < clip.Top then
    result := false;

  if maxy > clip.Bottom - 1 then
    maxy := clip.Bottom - 1;
  if miny > clip.Bottom - 1 then
    result := false;
end;

procedure ComputeAliasedRowBounds(x1,x2: single; minx,maxx: integer; out ix1,ix2: integer);
begin
  ix1 := trunc(x1);
  if frac(x1)>0.5 then inc(ix1)
  else if frac(x1)<=-0.5 then dec(ix1);
  ix2 := trunc(x2)-1;
  if frac(x2)>0.5 then inc(ix2)
  else if frac(x2)<=-0.5 then dec(ix2);
  if ix1 < minx then ix1 := minx;
  if ix2 >= maxx then ix2 := maxx;
end;

function IsPointInPolygon(const points: ArrayOfTPointF; point: TPointF
  ; windingMode: boolean): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TSimpleFillPolyInfo.Create(points);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,windingMode);
  info.free;
end;

function IsPointInEllipse(x, y, rx, ry: single; point: TPointF): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TFillEllipseInfo.Create(x,y,rx,ry);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,false);
  info.free;
end;

function IsPointInRoundRectangle(x1, y1, x2, y2, rx, ry: single; point: TPointF
  ): boolean;
var info: TBGRACustomFillInfo;
begin
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,[]);
  result := info.IsPointInside(point.x+0.5,point.y+0.5,false);
  info.free;
end;

function IsPointInRectangle(x1, y1, x2, y2: single; point: TPointF): boolean;
begin
  with point do
    result := (((x1<x) and (x2>x)) or ((x1>x) and (x2<x))) and
              (((y1<y) and (y2>y)) or ((y1>y) and (y2<y)));
end;

procedure AddDensity(dest: PDensity; start,count: integer; value: word);
var valueValue: LongWord;
    lastAdd: integer;
begin
  if count=0 then exit;
  inc(dest,start);
  if start and 1 = 1 then
  begin
    inc(dest^, value);
    inc(dest);
    dec(count);
  end;
  lastAdd := count and 1;
  count := count shr 1;
  if count > 0 then
  begin
    valueValue := value+(value shl 16);
    while count > 0 do
    begin
      inc(plongword(dest)^, valueValue);
      inc(dest,2);
      dec(count);
    end;
  end;
  if lastAdd <> 0 then
    inc(dest^, value);
end;

function DivByAntialiasPrecision(value: UInt32or64): UInt32or64;
begin             //
  result := value shr AntialiasPrecisionShift;// div AntialiasPrecision;
end;

function DivByAntialiasPrecision256(value: UInt32or64): UInt32or64;
begin             //
  result := value shr (AntialiasPrecisionShift+8);// div (256*AntialiasPrecision);
end;

function DivByAntialiasPrecision65536(value: UInt32or64): UInt32or64;
begin             //
  result := value shr (AntialiasPrecisionShift+16);//div (65536*AntialiasPrecision);
end;

{ TFillRectangleInfo }

function TFillRectangleInfo.GetBottomRight: TPointF;
begin
  result := PointF(FX2-0.5,FY2-0.5);
end;

function TFillRectangleInfo.GetTopLeft: TPointF;
begin
  result := PointF(FX1-0.5,FY1-0.5);
end;

function TFillRectangleInfo.NbMaxIntersection: integer;
begin
  Result:= 2;
end;

procedure TFillRectangleInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin
  if (cury >= FY1) and (cury <= FY2) then
  begin
    inter[nbinter].interX := FX1;
    inter[nbinter].winding := -windingFactor;
    inter[nbinter].numSegment := 0;
    Inc(nbinter);
    inter[nbinter].interX := FX2;
    inter[nbinter].winding := +windingFactor;
    inter[nbinter].numSegment := 1;
    Inc(nbinter);
  end;
end;

constructor TFillRectangleInfo.Create(x1, y1, x2, y2: single;
  APixelCenteredCoordinates: boolean);
var
  temp: Single;
begin
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if APixelCenteredCoordinates then
  begin
    FX1  := x1 + 0.5;
    FY1  := y1 + 0.5;
    FX2  := x2 + 0.5;
    FY2  := y2 + 0.5;
  end else
  begin
    FX1 := x1;
    FY1 := y1;
    FX2 := x2;
    FY2 := y2;
  end;
  WindingFactor := 1;
end;

function TFillRectangleInfo.GetBounds: TRect;
begin
  result := rect(floor(fx1),floor(fy1),floor(fx2)+1,floor(fy2)+1);
end;

{ TFillShapeInfo }

function TFillShapeInfo.GetBounds: TRect;
begin
  Result := rect(0, 0, 0, 0);
end;


function TFillShapeInfo.IsPointInside(x, y: single; windingMode: boolean
  ): boolean;
var
  i,nbInter: integer;
begin
  if FPointInsideInter = nil then
    FPointInsideInter := CreateIntersectionArray;
  ComputeAndSort(y,FPointInsideInter,nbInter,windingMode);
  i := 0;
  while i+1 < nbInter do
  begin
    if (FPointInsideInter[i].interX < x) and (FPointInsideInter[i+1].interX > x) then
    begin
      result := true;
      FreeIntersectionArray(FPointInsideInter);
      exit;
    end;
    inc(i,2);
  end;
  result := false;
end;

function TFillShapeInfo.NbMaxIntersection: integer;
begin
  Result := 0;
end;

destructor TFillShapeInfo.Destroy;
begin
  FreeIntersectionArray(FPointInsideInter);
  inherited Destroy;
end;

function TFillShapeInfo.SegmentsCurved: boolean;
begin
  result := false;
end;

function TFillShapeInfo.CreateIntersectionInfo: TIntersectionInfo;
begin
  result := TIntersectionInfo.Create;
end;

procedure TFillShapeInfo.FreeIntersectionArray(
  var inter: ArrayOfTIntersectionInfo);
var
  i: Integer;
begin
  for i := 0 to high(inter) do
    inter[i].free;
  inter := nil;
end;

{$hints off}
procedure TFillShapeInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin

end;
{$hints on}

procedure TFillShapeInfo.SortIntersection(var inter: ArrayOfTIntersectionInfo; nbInter: integer);
var
  i,j,k: Integer;
  tempInter: TIntersectionInfo;
begin
  if nbInter > 10 then
    InternalQuickSortIntersection(@inter[0], 0, nbInter-1);
  for i := 1 to nbinter - 1 do
  begin
    j := i;
    while (j > 0) and (inter[i].interX < inter[j-1].interX) do dec(j);
    if j <> i then
    begin
      tempInter := inter[i];
      for k := i-1 downto j do
        inter[k+1] := inter[k];
      inter[j]  := tempInter;
    end;
  end;
end;

procedure TFillShapeInfo.InternalQuickSortIntersection(inter0: pointer;
      idxL, idxH: Integer);
const Stride = sizeof(pointer);
      MinSub = 10;
type PIntersectionInfo = ^TIntersectionInfo;
var
  ls,hs : Integer;
  li,hi : Integer;
  mi    : Integer;
  ms    : Integer;
  pb    : PByte;
  tempInfo: TIntersectionInfo;
  m: Single;
begin
  pb:=PByte(inter0);
  li:=idxL;
  hi:=idxH;
  mi:=(li+hi) div 2;
  ls:=li*Stride;
  hs:=hi*Stride;
  ms:=mi*Stride;
  m := PIntersectionInfo(pb+ms)^.interX;
  repeat
    while PIntersectionInfo(pb+ls)^.interX < m do begin
      inc(ls, Stride);
      inc(li);
    end;
    while m < PIntersectionInfo(pb+hs)^.interX do begin
      dec(hs, Stride);
      dec(hi);
    end;
    if ls <= hs then begin
      tempInfo := PIntersectionInfo(pb+ls)^;
      PIntersectionInfo(pb+ls)^ := PIntersectionInfo(pb+hs)^;
      PIntersectionInfo(pb+hs)^ := tempInfo;
      inc(ls, Stride); inc(li);
      dec(hs, Stride); dec(hi);
    end;
  until ls>hs;
  if hi>=idxL+MinSub-1 then InternalQuickSortIntersection(inter0, idxL, hi);
  if li+MinSub-1<=idxH then InternalQuickSortIntersection(inter0, li, idxH);
end;

procedure TFillShapeInfo.ConvertFromNonZeroWinding(var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var windingSum,prevSum,i,nbAlternate: integer;
    tempInfo: TIntersectionInfo;
begin
  windingSum := 0;
  nbAlternate := 0;
  for i := 0 to nbInter-1 do
  begin
    prevSum := windingSum;
    inc(windingSum, inter[i].winding);
    if (windingSum = 0) xor (prevSum = 0) then
    begin
      if nbAlternate<>i then
      begin
        tempInfo := inter[nbAlternate];
        inter[nbAlternate] := inter[i];
        inter[i] := tempInfo;
      end;
      inc(nbAlternate);
    end;
  end;
  nbInter := nbAlternate;
end;

procedure TFillShapeInfo.ComputeAndSort(cury: single;
  var inter: ArrayOfTIntersectionInfo; out nbInter: integer; windingMode: boolean);
begin
  nbInter := 0;
  ComputeIntersection(cury,inter,nbInter);
  if nbInter < 2 then exit;
  SortIntersection(inter,nbInter);
  if windingMode then ConvertFromNonZeroWinding(inter,nbInter);
end;

function TFillShapeInfo.GetSliceIndex: integer;
begin
  result := 0;
end;

function TFillShapeInfo.CreateIntersectionArray: ArrayOfTIntersectionInfo;
var
  i: Integer;
begin
  setlength(result, NbMaxIntersection);
  for i := 0 to high(result) do
    result[i] := CreateIntersectionInfo;
end;

function ComputeWinding(y1,y2: single): integer;
begin
    if y2 > y1 then result := 1 else
    if y2 < y1 then result := -1 else
      result := 0;
end;

type
  arrayOfSingle = array of single;

procedure InsertionSortSingles(var a: arrayOfSingle);
var i,j: integer;
    temp: single;
begin
  for i := 1 to high(a) do
  begin
    Temp := a[i];
    j := i;
    while (j>0) and (a[j-1]> Temp) do
    begin
      a[j] := a[j-1];
      dec(j);
    end;
    a[j] := Temp;
  end;
end;

function PartitionSingles(var a: arrayOfSingle; left,right: integer): integer;

  procedure Swap(idx1,idx2: integer); inline;
  var temp: single;
  begin
    temp := a[idx1];
    a[idx1] := a[idx2];
    a[idx2] := temp;
  end;

var pivotIndex: integer;
    pivotValue: single;
    storeIndex: integer;
    i: integer;

begin
  pivotIndex := left + random(right-left+1);
  pivotValue := a[pivotIndex];
  swap(pivotIndex,right);
  storeIndex := left;
  for i := left to right-1 do
    if a[i] <= pivotValue then
    begin
      swap(i,storeIndex);
      inc(storeIndex);
    end;
  swap(storeIndex,right);
  result := storeIndex;
end;

procedure QuickSortSingles(var a: arrayOfSingle; left,right: integer);
var pivotNewIndex: integer;
begin
  if right > left+9 then
  begin
    pivotNewIndex := PartitionSingles(a,left,right);
    QuickSortSingles(a,left,pivotNewIndex-1);
    QuickSortSingles(a,pivotNewIndex+1,right);
  end;
end;

procedure SortSingles(var a: arrayOfSingle);
begin
  if length(a) < 10 then InsertionSortSingles(a) else
  begin
    QuickSortSingles(a,0,high(a));
    InsertionSortSingles(a);
  end;
end;

procedure RemoveSingleDuplicates(var a: arrayOfSingle; var nb: integer);
var i,idx: integer;
begin
  idx := 0;
  for i := 1 to nb-1 do
  begin
    if a[i] <> a[idx] then
    begin
      inc(idx);
      a[idx] := a[i];
    end;
  end;
  nb := idx+1;
end;

function BinarySearchSingle(value: single; var a: arrayOfSingle; left,right: integer): integer;
var pivotIndex: integer;
    pivotValue: single;
begin
  pivotIndex := (left+right) div 2;
  pivotValue := a[pivotIndex];
  if value = pivotValue then
    result := pivotIndex else
  if value < pivotValue then
  begin
    if pivotIndex = left then result := left else
      result := BinarySearchSingle(value, a, left,pivotIndex-1);
  end else
  begin
    if pivotIndex = right then result := right+1 else
      result := BinarySearchSingle(value, a, pivotIndex+1, right);
  end;
end;

{ TCustomFillPolyInfo }

constructor TCustomFillPolyInfo.Create(const points: array of TPointF; APixelCenteredCoordinates: boolean);
var
  cur, first, i, j: integer;
  p, pNext: PCustomPointRecord;
  tempCoord: TPointF;
  tempBool: Boolean;

begin
  InitPoints(points);
  FSegmentsDataCreated:= false;
  if FPoints=nil then
  begin
    FBoundsF := EmptyRectF;
    exit;
  end;

  //look for empty points, correct coordinate and successors
  cur   := -1;
  first := -1;
  p := @FPoints[0];
  for i := 0 to high(FPoints) do
  begin
    if not isEmptyPointF(p^.coord) then
    begin
      p^.empty := False;
      if APixelCenteredCoordinates then
        p^.coord.Offset(0.5,0.5);
      if cur <> -1 then
        FPoints[cur].next := i;
      if first = -1 then
        first := i;
      cur := i;
    end
    else
    begin
      if (first <> -1) and (cur <> first) then
        FPoints[cur].next := first;

      p^.empty := True;
      p^.next  := -1;
      cur   := -1;
      first := -1;
    end;
    inc(p);
  end;
  if (first <> -1) and (cur <> first) then
    FPoints[cur].next := first;

  FBoundsF := RectF(FPoints[0].coord,FPoints[0].coord);

  p := @FPoints[0];
  for i := 0 to high(FPoints) do
  begin
    if not p^.empty then
    begin
      if p^.x < FBoundsF.Left then FBoundsF.Left := p^.x else
      if p^.x > FBoundsF.Right then FBoundsF.Right := p^.x;
      if p^.y < FBoundsF.Top then FBoundsF.Top := p^.y else
      if p^.y > FBoundsF.Bottom then FBoundsF.Bottom := p^.y;
    end;
    if p^.next <> -1 then
    begin
      pNext := @FPoints[p^.next];
      p^.coord2 := pNext^.coord;
    end;
    inc(p);
  end;

  //compute slopes
  p := @FPoints[0];
  for i := 0 to high(FPoints) do
  begin
    if not p^.empty then
    begin
      p^.winding := ComputeWinding(p^.y, p^.y2);
      if p^.winding<>0 then
        p^.slope := (p^.x2 - p^.x) / (p^.y2 - p^.y)
      else
        p^.slope := EmptySingle;
    end
    else
      p^.slope := EmptySingle;
    inc(p);
  end;

  //check if end points are included
  p := @FPoints[0];
  for i := 0 to high(FPoints) do
  begin
    if not p^.empty then
    begin
      j := p^.next;
      pNext := @FPoints[j];
      if p^.winding > 0 then
        p^.includeEndingPoint := pNext^.winding < 0
      else if p^.winding < 0 then
        p^.includeEndingPoint := pNext^.winding >= 0
      else
        p^.includeStartingPoint := false;

      if pNext^.winding > 0 then
        pNext^.includeStartingPoint := true
      else if pNext^.winding < 0 then
        pNext^.includeStartingPoint := p^.winding <> 0;
    end;
    inc(p);
  end;

  //flip vertically to have always top to bottom
  p := @FPoints[0];
  for i := 0 to high(FPoints) do
  begin
    if p^.winding < 0 then
    begin
      tempCoord := p^.coord;
      p^.coord := p^.coord2;
      p^.coord2 := tempCoord;
      tempBool := p^.includeStartingPoint;
      p^.includeStartingPoint := p^.includeEndingPoint;
      p^.includeEndingPoint := tempBool;
    end;
    inc(p);
  end;
end;

destructor TCustomFillPolyInfo.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(FPoints) do
    freemem(FPoints[i].data);
  inherited Destroy;
end;

function TCustomFillPolyInfo.CreateIntersectionArray: ArrayOfTIntersectionInfo;
var
  i: Integer;
begin
  CreateSegmentsData;
  setlength(result, NbMaxIntersection);
  for i := 0 to high(result) do
    result[i] := nil;
end;

function TCustomFillPolyInfo.CreateSegmentData(numPt, nextPt: integer;
  ASeg: PCustomPointRecord): pointer;
begin
  result := nil;
end;

procedure TCustomFillPolyInfo.FreeSegmentData(data: pointer);
begin
  freemem(data);
end;

function TCustomFillPolyInfo.GetBounds: TRect;
begin
  with FBoundsF do
    result := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
end;

function TCustomFillPolyInfo.GetBoundsF: TRectF;
begin
  result := FBoundsF;
end;

function TCustomFillPolyInfo.GetNbPoints: integer;
begin
  result := length(FPoints);
end;

function TCustomFillPolyInfo.NbMaxIntersection: integer;
begin
  Result := length(FPoints);
end;

procedure TCustomFillPolyInfo.SetIntersectionValues(AInter: TIntersectionInfo;
  AInterX: Single; AWinding, ANumSegment: integer; dy: single; AData: pointer);
begin
  AInter.SetValues( AInterX, AWinding, ANumSegment );
end;

procedure TCustomFillPolyInfo.InitPoints(const points: array of TPointF);
const
  minDist = 0.00390625; //1 over 256

var
  i, first, nbP: integer;

  function PointAlmostEqual(const p1,p2: TPointF): boolean;
  begin
    result := (abs(p1.x-p2.x) < minDist) and (abs(p1.y-p2.y) < minDist);
  end;

  procedure EndOfSubPolygon;
  begin
    //if there is a subpolygon
    if first<>-1 then
    begin
      //last point is the same as first point?
      if (nbP >= first+2) and PointAlmostEqual(FPoints[nbP-1].coord,FPoints[first].coord) then
        dec(nbP); //remove superfluous looping point

      if (nbP <= first+2) then //are there only one or two points?
      begin
        //remove subpolygon because we need at least a triangle
        nbP := first;
        first := -1;
      end;

    end;
  end;

begin
  setlength(FPoints, length(points));
  nbP := 0;
  first := -1;
  for i := 0 to high(points) do
  if isEmptyPointF(points[i]) then
  begin
    EndOfSubPolygon;
    if first<>-1 then
    begin
      FPoints[nbP].originalIndex := i;
      FPoints[nbP].coord := EmptyPointF;
      inc(nbP);
      first := -1;
    end;
  end else
  if (first=-1) or not PointAlmostEqual(FPoints[nbP-1].coord,points[i]) then
  begin
    if first = -1 then first := nbP;
    FPoints[nbP].originalIndex := i;
    FPoints[nbP].coord := points[i];
    inc(nbP);
  end;
  EndOfSubPolygon;
  //if last point was a subpolygon delimiter (EmptyPointF) then removes it
  if (nbP > 0) and isEmptyPointF(FPoints[nbP-1].coord) then dec(nbP);

  setlength(FPoints, nbP);
end;

procedure TCustomFillPolyInfo.CreateSegmentsData;
var
  i: Integer;
  p: PCustomPointRecord;
begin
  if FSegmentsDataCreated then exit;
  FSegmentsDataCreated := true;
  if FPoints<>nil then
  begin
    p := @FPoints[0];
    for i := 0 to high(FPoints) do
    begin
      if not p^.empty and (p^.slope <> EmptySingle) then
      begin
        if p^.winding < 0 then
          p^.data := CreateSegmentData(p^.next,i, p)
        else
          p^.data := CreateSegmentData(i,p^.next, p);
      end;
      inc(p);
    end;
  end;
end;

{ TFillPolyInfo }

function TFillPolyInfo.NbMaxIntersection: integer;
begin
  Result:= FMaxIntersection;
end;

procedure TFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  j: integer;
  cust: PCustomPointRecord;
  pInter: PIntersectionInfo;
begin
  if length(FSlices)=0 then exit;

  while (cury < FSlices[FCurSlice].y1) and (FCurSlice > 0) do dec(FCurSlice);
  while (cury > FSlices[FCurSlice].y2) and (FCurSlice < high(FSlices)) do inc(FCurSlice);

  pInter := @inter[nbInter];
  with FSlices[FCurSlice] do
  if (cury >= y1) and (cury < y2) then
  begin
    for j := 0 to nbSegments-1 do
    begin
      cust := segments[j].custom;
      if pInter^ = nil then pInter^ := CreateIntersectionInfo;
      SetIntersectionValues(pInter^, (cury - cust^.y) * cust^.slope + cust^.x,
                            cust^.winding, segments[j].id, cury - cust^.y, cust^.data );
      Inc(nbinter);
      inc(pInter);
    end;
  end;
end;

constructor TFillPolyInfo.Create(const points: array of TPointF; APixelCenteredCoordinates: boolean);
  function AddSeg(numSlice: integer): integer;
  begin
    result := FSlices[numSlice].nbSegments;
    if length(FSlices[numSlice].segments)=FSlices[numSlice].nbSegments then
      setlength(FSlices[numSlice].segments,FSlices[numSlice].nbSegments*2+2);
    inc(FSlices[numSlice].nbSegments);
  end;

var
  yList: array of single;
  nbYList: integer;
  ya,yb,temp: single;
  sliceStart,sliceEnd,idxSeg: integer;
  i,j,idSeg: integer;

begin
  inherited Create(points, APixelCenteredCoordinates);

  //slice
  nbYList:= length(FPoints)*2;
  setlength(YList, nbYList);
  for i := 0 to high(FPoints) do
  begin
    YList[i*2] := FPoints[i].y;
    YList[i*2+1] := FPoints[i].y2;
  end;

  SortSingles(YList);
  RemoveSingleDuplicates(YList, nbYList);

  setlength(FSlices, nbYList-1);
  for i := 0 to high(FSlices) do
  begin
    FSlices[i].y1 := YList[i];
    FSlices[i].y2 := YList[i+1];
    FSlices[i].nbSegments := 0;
  end;

  idSeg := 0;
  for j := 0 to high(FPoints) do
  begin
    if FPoints[j].slope<>EmptySingle then
    begin
      ya := FPoints[j].y;
      yb := FPoints[j].y2;
      if yb < ya then
      begin
        temp := ya;
        ya := yb;
        yb := temp;
      end;
      sliceStart := BinarySearchSingle(ya,YList,0,nbYList-1);
      sliceEnd := BinarySearchSingle(yb,YList,0,nbYList-1);
      if sliceEnd > high(FSlices) then sliceEnd := high(FSlices);
      for i := sliceStart to sliceEnd do
      begin
        if ((FPoints[j].y < FSlices[i].y2) and
           (FPoints[j].y2 > FSlices[i].y1)) or
           ((FPoints[j].y2 < FSlices[i].y2) and
           (FPoints[j].y > FSlices[i].y1)) then
        begin
          idxSeg := AddSeg(i);
          with FSlices[i].segments[idxSeg] do
          begin
            inc(idSeg);
            id := idSeg;
            custom:= @FPoints[j];
          end;
        end;
      end;
    end;
  end;

  FCurSlice := 0;
  FMaxIntersection:= 0;
  for i := 0 to high(FSlices) do
    if FSlices[i].nbSegments > FMaxIntersection then
      FMaxIntersection:= FSlices[i].nbSegments;
end;

function TFillPolyInfo.GetSliceIndex: integer;
begin
  Result:= FCurSlice;
end;

{ TOnePassFillPolyInfo }

function TOnePassFillPolyInfo.PartitionByY(left,right: integer): integer;

  procedure Swap(idx1,idx2: integer); inline;
  var temp: POnePassRecord;
  begin
    temp := FSortedByY[idx1];
    FSortedByY[idx1] := FSortedByY[idx2];
    FSortedByY[idx2] := temp;
  end;

var pivotIndex: integer;
    pivotValue: single;
    storeIndex: integer;
    i: integer;

begin
  pivotIndex := left + random(right-left+1);
  pivotValue := FSortedByY[pivotIndex]^.custom^.y;
  swap(pivotIndex,right);
  storeIndex := left;
  for i := left to right-1 do
    if FSortedByY[i]^.custom^.y <= pivotValue then
    begin
      swap(i,storeIndex);
      inc(storeIndex);
    end;
  swap(storeIndex,right);
  result := storeIndex;
end;

procedure TOnePassFillPolyInfo.QuickSortByY(left,right: integer);
var pivotNewIndex: integer;
begin
  if right > left+9 then
  begin
    pivotNewIndex := PartitionByY(left,right);
    QuickSortByY(left,pivotNewIndex-1);
    QuickSortByY(pivotNewIndex+1,right);
  end;
end;

procedure TOnePassFillPolyInfo.InsertionSortByY;
var i,j: integer;
    tempValue: single;
    tempPtr: POnePassRecord;
begin
  for i := 1 to high(FSortedByY) do
  begin
    tempPtr := FSortedByY[i];
    TempValue := tempPtr^.custom^.y;
    j := i;
    while (j>0) and (FSortedByY[j-1]^.custom^.y > TempValue) do
    begin
      FSortedByY[j] := FSortedByY[j-1];
      dec(j);
    end;
    FSortedByY[j] := tempPtr;
  end;
end;

procedure TOnePassFillPolyInfo.SortByY;
var i,nbSorted: integer;
begin
  setlength(FSortedByY, length(FPoints));
  nbSorted := 0;
  for i := 0 to high(FSortedByY) do
    if not FPoints[i].empty then
    begin
      FSortedByY[nbSorted] := @FOnePass[i];
      inc(nbSorted);
    end;
  setlength(FSortedByY,nbSorted);
  if length(FSortedByY) < 10 then InsertionSortByY else
  begin
    QuickSortByY(0,high(FSortedByY));
    InsertionSortByY;
  end;
end;

procedure TOnePassFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  p,pprev,pnext: POnePassRecord;
{  t: TextFile;
  i: Integer; }
  pCust: PCustomPointRecord;
  pInter: PIntersectionInfo;
begin
  FShouldInitializeDrawing := true;

  p := FFirstWaiting;
  while p <> nil do
  begin
    if (cury >= p^.custom^.y) then
    begin
      if cury <= p^.custom^.y2+1 then
      begin
        p^.nextDrawing := FFirstDrawing;
        FFirstDrawing := p;
        inc(FSliceIndex);
      end;
    end
      else break;
    p := p^.nextWaiting;
  end;
  FFirstWaiting:= p;

  p := FFirstDrawing;
  pprev := nil;
  pInter := @inter[nbInter];
  while p <> nil do
  begin
    pnext := p^.nextDrawing;
    pCust := p^.custom;
    if (((cury > pCust^.y) and (cury < pCust^.y2)) or
       (pCust^.includeStartingPoint and (cury = pCust^.y)) or
       (pCust^.includeEndingPoint and (cury = pCust^.y2))) then
    begin
      if pInter^ = nil then pInter^ := CreateIntersectionInfo;
      SetIntersectionValues(pInter^, (cury - pCust^.y)*pCust^.slope + pCust^.x,
                   pCust^.winding, p^.id, cury - pCust^.y, pCust^.data);
      inc(nbinter);
      inc(pInter);
    end else
    if (cury > pCust^.y2+1) then
    begin
      if pprev <> nil then
        pprev^.nextDrawing := pnext
      else
        FFirstDrawing:= pnext;
      p := pnext;
      Inc(FSliceIndex);
      continue;
    end;
    pprev := p;
    p := pnext;
  end;
{  if odd(nbInter) then
  begin
    assignfile(t, 'polygon.dump');
    rewrite(t);
    writeln(t,'Polygon tested at ',cury);
    for i := 0 to NbPoints-1 do
      if isEmptyPointF(FPoints[i]) then write(t,'] [') else
      write(t,FPoints[i].x, ',', FPoints[i].y,'  ');
    writeln(t);
    writeln(t,'Drawing');
    p := FFirstDrawing;
    while p <> nil do
    begin
      if ((p^.winding > 0) and
        (((cury > p^.y1) and (cury < p^.y2)) or
         (p^.includeStartingPoint and (cury = p^.y1)) or
         (p^.includeEndingPoint and (cury = p^.y2)))) or
        ((p^.winding < 0) and
       (((cury > p^.y1) and (cury < p^.y2)) or
         (p^.includeStartingPoint and (cury = p^.y2)) or
         (p^.includeEndingPoint and (cury = p^.y1)))) then
         write(t,'* ') else write(t,'- ');

      writeln(t,p^.x1,',',p^.y1,'  ',p^.x2,',',p^.y2,'  ',p^.winding,' ',BoolToStr(p^.includeEndingPoint,'end incl','end not incl'));
      p := p^.nextDrawing;
    end;
    closefile(t);

    raise exception.Create('Even intersections expected');
  end;   }
end;

constructor TOnePassFillPolyInfo.Create(const points: array of TPointF; APixelCenteredCoordinates: boolean);
var i,j: integer;
  p: POnePassRecord;
begin
  inherited create(points, APixelCenteredCoordinates);

  FShouldInitializeDrawing := true;
  setlength(FOnePass, length(FPoints));
  for i := 0 to high(FPoints) do
  if not FPoints[i].empty then
  begin
    p := @FOnePass[i];
    p^.id := i;
    j := FPoints[i].next;
    p^.next := @FOnePass[j];
    p^.custom:= @FPoints[i];
  end;

  SortByY;
  FSliceIndex := 0;
end;

function TOnePassFillPolyInfo.CreateIntersectionArray: ArrayOfTIntersectionInfo;
var i: integer;
  p,pprev: POnePassRecord;
begin
  if FShouldInitializeDrawing then
  begin
    FShouldInitializeDrawing := false;
    FFirstWaiting:= nil;
    pprev := nil;
    for i := 0 to high(FSortedByY) do
    begin
      p := FSortedByY[i];
      if p^.custom^.slope <> EmptySingle then
      begin
        if pprev <> nil then
          pprev^.nextWaiting:= p
        else
          FFirstWaiting := p;
        pprev := p;
      end;
    end;
  end;
  result := inherited CreateIntersectionArray;
end;

function TOnePassFillPolyInfo.GetSliceIndex: integer;
begin
  Result:= FSliceIndex;
end;

{ TSimpleFillPolyInfo }

procedure TSimpleFillPolyInfo.ComputeIntersection(cury: single;
  var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var i: integer;
  p: PCustomPointRecord;
  pInter: PIntersectionInfo;
begin
  if FPoints = nil then exit;
  p := @FPoints[0];
  pInter := @inter[nbInter];
  for i := 0 to high(FPoints) do
  begin
    if (p^.winding <> 0) and
     ( ((cury > p^.y) and (cury < p^.y2)) or
       (p^.includeStartingPoint and (cury = p^.y)) or
       (p^.includeEndingPoint and (cury = p^.y2)) ) then
    begin
      if pInter^ = nil then pInter^ := CreateIntersectionInfo;
      SetIntersectionValues(pInter^, (cury - p^.y)*p^.slope + p^.x, p^.winding, i, cury - p^.y, p^.data);
      inc(nbinter);
      inc(pInter);
    end;
    inc(p);
  end;
end;

constructor TSimpleFillPolyInfo.Create(const points: array of TPointF; APixelCenteredCoordinates: boolean);
begin
  inherited Create(points, APixelCenteredCoordinates);
end;

{ TFillEllipseInfo }

constructor TFillEllipseInfo.Create(x, y, rx, ry: single);
begin
  FX  := x + 0.5;
  FY  := y + 0.5;
  FRX := abs(rx);
  FRY := abs(ry);
  WindingFactor := 1;
  FSliceIndex:= -1;
end;

function TFillEllipseInfo.GetBounds: TRect;
begin
  Result := rect(floor(fx - frx), floor(fy - fry), ceil(fx + frx), ceil(fy + fry));
end;

function TFillEllipseInfo.SegmentsCurved: boolean;
begin
  Result:= true;
end;

function TFillEllipseInfo.GetSliceIndex: integer;
begin
  Result:= FSliceIndex;
end;

function TFillEllipseInfo.GetCenter: TPointF;
begin
  result := PointF(FX-0.5,FY-0.5);
end;

function TFillEllipseInfo.NbMaxIntersection: integer;
begin
  Result := 2;
end;

procedure TFillEllipseInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  d: single;
begin
  if (FRY <= 0) or (FRX <= 0) then exit;
  d := sqr((cury - FY) / FRY);
  if d < 1 then
  begin
    d := sqrt(1 - d) * FRX;
    inter[nbinter].SetValues( FX - d, -windingFactor, 0);
    Inc(nbinter);
    inter[nbinter].SetValues( FX + d, windingFactor, 1);
    Inc(nbinter);
    FSliceIndex := 0;
  end else
  begin
    if cury < FY then
      FSliceIndex:= -1
    else
      FSliceIndex:= 1;
  end;
end;

{ TFillBorderEllipseInfo }

constructor TFillBorderEllipseInfo.Create(x, y, rx, ry, w: single);
begin
  if rx < 0 then
    rx := -rx;
  if ry < 0 then
    ry := -ry;
  FOuterBorder := TFillEllipseInfo.Create(x, y, rx + w / 2, ry + w / 2);
  if (rx > w / 2) and (ry > w / 2) then
  begin
    FInnerBorder := TFillEllipseInfo.Create(x, y, rx - w / 2, ry - w / 2);
    FInnerBorder.WindingFactor := -1;
  end
  else
    FInnerBorder := nil;
end;

function TFillBorderEllipseInfo.GetBounds: TRect;
begin
  Result := FOuterBorder.GetBounds;
end;

function TFillBorderEllipseInfo.SegmentsCurved: boolean;
begin
  Result:= FOuterBorder.SegmentsCurved;
  if FInnerBorder <> nil then result := result or FInnerBorder.SegmentsCurved;
end;

function TFillBorderEllipseInfo.NbMaxIntersection: integer;
begin
  Result := 4;
end;

procedure TFillBorderEllipseInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin
  FOuterBorder.ComputeIntersection(cury, inter, nbInter);
  if FInnerBorder <> nil then
    FInnerBorder.ComputeIntersection(cury, inter, nbInter);
end;

destructor TFillBorderEllipseInfo.Destroy;
begin
  FOuterBorder.Free;
  if FInnerBorder <> nil then
    FInnerBorder.Free;
  inherited Destroy;
end;

function TFillBorderEllipseInfo.GetSliceIndex: integer;
begin
  Result:= FOuterBorder.GetSliceIndex;
end;

{ TFillRoundRectangleInfo }

constructor TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean);
var
  temp: Single;
begin
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if APixelCenteredCoordinates then
  begin
    FX1  := x1 + 0.5;
    FY1  := y1 + 0.5;
    FX2  := x2 + 0.5;
    FY2  := y2 + 0.5;
  end else
  begin
    FX1 := x1;
    FY1 := y1;
    FX2 := x2;
    FY2 := y2;
  end;
  FRX := abs(rx);
  FRY := abs(ry);
  if 2*FRX > x2-x1 then FRX := (x2-x1)/2;
  if 2*FRY > y2-y1 then FRY := (y2-y1)/2;
  FOptions:= options;
  WindingFactor := 1;
end;

function TFillRoundRectangleInfo.SegmentsCurved: boolean;
begin
  if (not (rrTopLeftSquare in FOptions) and not (rrTopLeftBevel in FOptions)) or
     (not (rrTopRightSquare in FOptions) and not (rrTopRightBevel in FOptions)) or
     (not (rrBottomRightSquare in FOptions) and not (rrBottomRightBevel in FOptions)) or
     (not (rrBottomLeftSquare in FOptions) and not (rrBottomLeftBevel in FOptions)) then
     result := true else result := false;
end;

function TFillRoundRectangleInfo.GetBounds: TRect;
begin
  result := rect(floor(fx1),floor(fy1),floor(fx2)+1,floor(fy2)+1);
end;

function TFillRoundRectangleInfo.GetBottomRight: TPointF;
begin
  result := PointF(FX2-0.5,FY2-0.5);
end;

function TFillRoundRectangleInfo.GetTopLeft: TPointF;
begin
  result := PointF(FX1-0.5,FY1-0.5);
end;

function TFillRoundRectangleInfo.NbMaxIntersection: integer;
begin
  result := 2;
end;

procedure TFillRoundRectangleInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
var
  d,d2: single;
begin
  if (cury >= FY1) and (cury <= FY2) then
  begin
    if cury < FY1+FRY then
    begin
      d := abs((cury - (FY1+FRY)) / FRY);
      if d > 1 then d2 := 0
      else d2 := sqrt(1 - sqr(d)) * FRX;

      if rrTopLeftSquare in FOptions then
        inter[nbinter].interX := FX1 else
      if rrTopLeftBevel in FOptions then
        inter[nbinter].interX := FX1 + d*FRX
      else
        inter[nbinter].interX := FX1 + FRX - d2;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);

      if rrTopRightSquare in FOptions then
        inter[nbinter].interX := FX2 else
      if rrTopRightBevel in FOptions then
        inter[nbinter].interX := FX2 - d*FRX
      else
        inter[nbinter].interX := FX2 - FRX + d2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end else
    if cury > FY2-FRY then
    begin
      d := abs((cury - (FY2-FRY)) / FRY);
      if d > 1 then d2 := 0
      else d2 := sqrt(1 - sqr(d)) * FRX;

      if rrBottomLeftSquare in FOptions then
        inter[nbinter].interX := FX1 else
      if rrBottomLeftBevel in FOptions then
        inter[nbinter].interX := FX1 + d*FRX
      else
        inter[nbinter].interX := FX1 + FRX - d2;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);

      if rrBottomRightSquare in FOptions then
        inter[nbinter].interX := FX2 else
      if rrBottomRightBevel in FOptions then
        inter[nbinter].interX := FX2 - d*FRX
      else
        inter[nbinter].interX := FX2 - FRX + d2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end else
    begin
      inter[nbinter].interX := FX1;
      inter[nbinter].winding := -windingFactor;
      inter[nbinter].numSegment := 0;
      Inc(nbinter);
      inter[nbinter].interX := FX2;
      inter[nbinter].winding := +windingFactor;
      inter[nbinter].numSegment := 1;
      Inc(nbinter);
    end;
  end;
end;

{ TFillBorderRoundRectInfo }

constructor TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; APixelCenteredCoordinates: boolean);
var rdiff: single;
  temp: Single;
begin
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;

  if rx < 0 then
    rx := -rx;
  if ry < 0 then
    ry := -ry;
  if 2*rx > x2-x1 then rx := (x2-x1)/2;
  if 2*ry > y2-y1 then ry := (y2-y1)/2;
  rdiff := w*(sqrt(2)-1);
  FOuterBorder := TFillRoundRectangleInfo.Create(x1-w/2,y1-w/2,x2+w/2,y2+w/2, rx+rdiff, ry+rdiff, options, APixelCenteredCoordinates);
  if (abs(x2-x1) > w) and (abs(y2-y1) > w) then
  begin
    if (rx-rdiff <= 0) or (ry-rdiff <= 0) then
      FInnerBorder := TFillRoundRectangleInfo.Create(x1+w/2, y1+w/2, x2-w/2, y2-w/2, 0,0, options, APixelCenteredCoordinates)
    else
      FInnerBorder := TFillRoundRectangleInfo.Create(x1+w/2, y1+w/2, x2-w/2, y2-w/2, rx-rdiff, ry-rdiff, options, APixelCenteredCoordinates);
    FInnerBorder.WindingFactor := -1;
  end
  else
    FInnerBorder := nil;
end;

function TFillBorderRoundRectInfo.GetBounds: TRect;
begin
  result := FOuterBorder.GetBounds;
end;

function TFillBorderRoundRectInfo.SegmentsCurved: boolean;
begin
  Result:= FOuterBorder.SegmentsCurved;
  if FInnerBorder <> nil then result := result or FInnerBorder.SegmentsCurved;
end;

function TFillBorderRoundRectInfo.NbMaxIntersection: integer;
begin
  Result := 4;
end;

procedure TFillBorderRoundRectInfo.ComputeIntersection(cury: single;
      var inter: ArrayOfTIntersectionInfo; var nbInter: integer);
begin
  FOuterBorder.ComputeIntersection(cury, inter, nbInter);
  if FInnerBorder <> nil then
    FInnerBorder.ComputeIntersection(cury, inter, nbInter);
end;

destructor TFillBorderRoundRectInfo.Destroy;
begin
  FOuterBorder.Free;
  FInnerBorder.Free;
  inherited Destroy;
end;

initialization

  Randomize;

end.

