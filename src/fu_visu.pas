unit fu_visu;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

interface

uses Graphics, cu_fits, math, UScaleDPI, Classes, SysUtils, FileUtil, TAGraph,
  TASeries, TAChartUtils, u_translation, u_hints, LCLType,
  u_global, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin, ComCtrls;

type

  { Tf_visu }

  Tf_visu = class(TFrame)
    BtnClipRange: TSpeedButton;
    BtnClipping: TSpeedButton;
    BtnInvert: TSpeedButton;
    BtnFlipHorz: TSpeedButton;
    BtnFlipVert: TSpeedButton;
    BtnShowImage: TSpeedButton;
    BtnZoom05: TSpeedButton;
    BtnBullsEye: TSpeedButton;
    HistGraph: TChart;
    HistGraphAreaSeries1: TAreaSeries;
    Gamma: TFloatSpinEdit;
    HistGraphMaxLine: TConstantLine;
    HistGraphMinLine: TConstantLine;
    Panel1: TPanel;
    BtnZoomAdjust: TSpeedButton;
    HistBar: TPanel;
    HistBarLeft: TPanel;
    HistBarRight: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BtnZoom2: TSpeedButton;
    BtnZoom1: TSpeedButton;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelNoDisplay: TPanel;
    SpinEditMin: TFloatSpinEdit;
    SpinEditMax: TFloatSpinEdit;
    Splitter1: TSplitter;
    Title: TLabel;
    TimerRedraw: TTimer;
    TimerMinMax: TTimer;
    procedure BtnBullsEyeClick(Sender: TObject);
    procedure BtnClippingClick(Sender: TObject);
    procedure BtnFlipHorzClick(Sender: TObject);
    procedure BtnFlipVertClick(Sender: TObject);
    procedure BtnClipRangeClick(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure BtnShowImageClick(Sender: TObject);
    procedure BtnZoomClick(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure GammaChange(Sender: TObject);
    procedure HistGraphMouseEnter(Sender: TObject);
    procedure HistGraphMouseLeave(Sender: TObject);
    procedure histminmaxClick(Sender: TObject);
    procedure HistogramMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HistogramMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HistogramMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HistBarLeftResize(Sender: TObject);
    procedure SpinEditMaxChange(Sender: TObject);
    procedure SpinEditMinChange(Sender: TObject);
    procedure TimerMinMaxTimer(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
  private
    { private declarations }
    Fhist:Thistogram;
    Fmaxh, Fmaxp, Fsum: integer;
    FimageC, FimageMin, FimageMax : double;
    FisFloatingPoint, FisFlipped, Finitialized: boolean;
    FimgMin, FimgMax: double;
    FHistStart,FHistStop,FHistStep: integer;
    FBullsEye, LockSpinEdit, FClipping, FInvert: Boolean;
    FZoom: double;
    StartUpd,Updmax,HistogramAdjusted, LockHistogram: boolean;
    LockRedraw: boolean;
    FRedraw: TNotifyEvent;
    FonZoom: TNotifyEvent;
    FShowHistogramPos: TNotifyStr;
    FShowLastImage: TNotifyEvent;
    procedure SetZoom(value: double);
    function  GetFlipHorz: boolean;
    function  GetFlipVert: boolean;
    procedure SetLimit(SetLevel:boolean);
    procedure PlotHistogram;
    function GetHistBarPosition: integer;
    procedure SetHistBarPosition(value:integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure DrawHistogram(f: TFits; SetLevel,ResetCursor: boolean);
    property Zoom: double read FZoom write SetZoom;
    property ImgMin: double read FimgMin write FimgMin;
    property ImgMax: double read FimgMax write FimgMax;
    property BullsEye: boolean read FBullsEye;
    property Clipping: boolean read FClipping;
    property Invert: boolean read FInvert;
    property FlipHorz: boolean read GetFlipHorz;
    property FlipVert: boolean read GetFlipVert;
    property HistBarPosition: integer read GetHistBarPosition write SetHistBarPosition ;
    property onZoom: TNotifyEvent read FonZoom write FonZoom;
    property onRedraw: TNotifyEvent read FRedraw write FRedraw;
    property onShowHistogramPos: TNotifyStr read FShowHistogramPos write FShowHistogramPos;
    property onShowLastImage: TNotifyEvent read FShowLastImage write FShowLastImage;
  end;

implementation

{$R *.lfm}

{ Tf_visu }

constructor Tf_visu.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 BtnClipping.Flat:=true;
 BtnZoom05.Flat:=true;
 BtnBullsEye.Flat:=true;
 BtnZoomAdjust.Flat:=true;
 BtnZoom2.Flat:=true;
 BtnZoom1.Flat:=true;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 Finitialized:=false;
 ImgMax:=high(word);
 ImgMin:=0;
 FimageC:=1;
 HistogramAdjusted:=false;
 LockHistogram:=false;
 StartUpd:=false;
 Updmax:=false;
 FBullsEye:=false;
 FClipping:=false;
 FInvert:=false;
 LockSpinEdit:=true;
 LockRedraw:=false;
 FisFlipped:=true;
end;

destructor  Tf_visu.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_visu.SetLang;
begin
  Title.Caption:=rsVisualisatio;
  HistGraph.Hint:=Format(rsHistogramOfT, [crlf]);
  BtnZoomAdjust.Hint:=rsZoomToAdjust;
  HistBar.Hint:=rsImageLuminos;
  BtnZoom2.Hint:=rsZoomTwoTime;
  BtnZoom1.Hint:=rsZoomToOrigin;
  BtnZoom05.Hint:=rsZoomToHalfSi;
  BtnBullsEye.Hint:=rsShowBullsEye;
  BtnClipping.Hint:=rsShowHighligh;
  Gamma.Hint:=rsGammaOfTheIm;
  BtnInvert.Hint:=rsInvertImageD;
  BtnFlipHorz.Hint:=rsFlipTheImageH;
  BtnFlipVert.Hint:=rsFlipTheImageV;
  BtnShowImage.Hint:=rsShowLastCapt;
  BtnClipRange.Hint:=rsHistogramFul;
end;

procedure Tf_visu.SetLimit(SetLevel:boolean);
var histp: integer;
begin
  if SetLevel and (not HistogramAdjusted) then begin
    histp:=HistBarPosition;
    HistLevel(histp,Fsum,FHistStart,Fmaxp,Fhist,FimgMin,FimgMax);
  end;
  // adjust spinedit for data range
  LockHistogram:=true;
  if FisFloatingPoint then begin
    SpinEditMin.DecimalPlaces:=3;
    SpinEditMax.DecimalPlaces:=3;
    if abs(FimageMax-FimageMin)<=10 then begin
      SpinEditMin.Increment:=0.01;
      SpinEditMax.Increment:=0.01;
    end
    else begin
      SpinEditMin.Increment:=1;
      SpinEditMax.Increment:=1;
    end;
  end
  else begin
    SpinEditMin.DecimalPlaces:=0;
    SpinEditMax.DecimalPlaces:=0;
    if abs(FimageMax-FimageMin)<=255 then begin
      SpinEditMin.Increment:=1;
      SpinEditMax.Increment:=1;
    end
    else begin
      SpinEditMin.Increment:=10;
      SpinEditMax.Increment:=10;
    end;
  end;
  // histogram is always 0-65535, show real pixel value in the spinedit
  SpinEditMin.minValue:=FimageMin;
  SpinEditMin.maxValue:=FimageMax;
  SpinEditMax.minValue:=FimageMin;
  SpinEditMax.maxValue:=FimageMax;
  // scale from 0-65535 to image min-max
  SpinEditMin.Value:=FimageMin+FImgMin/FimageC;
  SpinEditMax.Value:=FimageMin+FimgMax/FimageC;
  LockHistogram:=false;
end;

procedure Tf_visu.DrawHistogram(f: TFits; SetLevel,ResetCursor: boolean);
var i: integer;
begin
try
LockSpinEdit:=true;
if ResetCursor then HistogramAdjusted:=false;
FisFloatingPoint:=f.HeaderInfo.floatingpoint;
FisFlipped:=f.HeaderInfo.roworder<>bottomup;
FimageC:=f.imageC;
FimageMin:=f.imageMin;
FimageMax:=f.imageMax;
for i:=0 to high(word) do Fhist[i]:=f.Histogram[i];
HistStats(Fhist,Fmaxh,Fmaxp,Fsum,FHistStart,FHistStop);
if Fmaxh=0 then exit;
if Fmaxp>(FHistStart+(FHistStop-FHistStart)/10) then Fmaxp:=0; // peak is probably not sky background
SetLimit(SetLevel);
PlotHistogram;

finally
  LockSpinEdit:=false;
end;
end;

procedure Tf_visu.PlotHistogram;
var i,j,r: integer;
    x: double;
begin
HistGraphAreaSeries1.Clear;
if BtnClipRange.Down then begin
  r:=FHistStop-FHistStart;
  FHistStep:=max(1,r div 256);
  for i:=0 to (r div FHistStep)-1 do begin
    x:=0;
    for j:=0 to FHistStep-1 do
      x:=x+Fhist[FHistStart+i*FHistStep+j];
    HistGraphAreaSeries1.Add(ln(x+1));
  end;
  HistGraphMinLine.Position:=max(0,(FimgMin-FHistStart)/FHistStep);
  HistGraphMaxLine.Position:=min(r/FHistStep,(FimgMax-FHistStart)/FHistStep);
  HistGraph.Extent.XMin:=0;
  HistGraph.Extent.XMax:=r/FHistStep;
  HistGraph.Extent.UseXMax:=true;
  HistGraph.Extent.UseXMin:=true;
end
else begin
  FHistStep:=255;
  for i:=0 to 255 do begin
    x:=0;
    for j:=0 to FHistStep do
      x:=x+Fhist[i*FHistStep+j];
    HistGraphAreaSeries1.Add(ln(x+1));
  end;
  HistGraphMinLine.Position:=FimgMin/FHistStep;
  HistGraphMaxLine.Position:=FimgMax/FHistStep;
  HistGraph.Extent.UseXMax:=false;
  HistGraph.Extent.UseXMin:=false;
end;
Finitialized:=true;
end;

procedure Tf_visu.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).Width>TPanel(Target).Height then begin
        panel2.Align:=alRight;
     end else begin
        panel2.Align:=alTop;
     end;
  end;
end;

procedure Tf_visu.FrameResize(Sender: TObject);
var btnw: integer;
begin
  if Parent is TPanel then begin
     if TPanel(Parent).Width>TPanel(Parent).Height then begin
        panel2.Align:=alRight;
     end else begin
        panel2.Align:=alTop;
     end;
     SpinEditMax.Left:=Panel3.Width-SpinEditMax.Width;
     btnw:=(Panel4.ClientWidth-DoScaleX(3)) div 4;
     gamma.Width:=2*btnw+DoScaleX(1);
     BtnFlipVert.Width:=btnw;
     BtnZoomAdjust.Width:=btnw;
     BtnZoom2.Width:=btnw;
     BtnZoom1.Width:=btnw;
     BtnZoom05.Width:=btnw;
     BtnBullsEye.Width:=btnw;
     BtnClipping.Width:=btnw;
     BtnInvert.Width:=btnw;
     BtnFlipHorz.Width:=btnw;
     BtnClipRange.Width:=btnw;
     BtnShowImage.Top:=Panel6.top+BtnClipRange.top;
  end;
end;

procedure Tf_visu.HistBarLeftResize(Sender: TObject);
begin
  HistogramAdjusted:=false;
  SetLimit(true);
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.SetZoom(value: double);
begin
 if FZoom<>value then begin
    FZoom:=value;
    if FZoom=0 then BtnZoomAdjust.Down:=true
    else if FZoom=0.5 then BtnZoom05.Down:=true
    else if FZoom=1 then BtnZoom1.Down:=true
    else if FZoom=2 then BtnZoom2.Down:=true;
 end;
end;

procedure Tf_visu.BtnZoomClick(Sender: TObject);
begin
  case TSpeedButton(sender).tag of
    0: FZoom:=0;
    1: FZoom:=0.5;
    2: FZoom:=1;
    3: FZoom:=2;
  end;
  if Assigned(FonZoom) then FonZoom(self);
end;

procedure Tf_visu.GammaChange(Sender: TObject);
begin
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.BtnBullsEyeClick(Sender: TObject);
begin
  FBullsEye:=not FBullsEye;
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnClippingClick(Sender: TObject);
begin
  FClipping:=BtnClipping.Down;
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

function Tf_visu.GetFlipHorz: boolean;
begin
  result:=BtnFlipHorz.Down;
end;

function Tf_visu.GetFlipVert: boolean;
begin
  result:=FisFlipped xor BtnFlipVert.Down;
end;

procedure Tf_visu.BtnFlipHorzClick(Sender: TObject);
begin
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnFlipVertClick(Sender: TObject);
begin
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnClipRangeClick(Sender: TObject);
begin
  PlotHistogram;
end;

procedure Tf_visu.BtnInvertClick(Sender: TObject);
begin
  FInvert:=BtnInvert.Down;
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnShowImageClick(Sender: TObject);
begin
  if assigned(FShowLastImage) then FShowLastImage(self);
end;

procedure Tf_visu.histminmaxClick(Sender: TObject);
begin
  ImgMin:=0;
  ImgMax:=high(word);
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.HistogramMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var xpos,dx1,dx2: double;
    pt: TDoublePoint;
begin
  if not Finitialized then exit;
  pt:=HistGraph.ImageToGraph(point(X,Y));
  if BtnClipRange.Down then begin
    xpos:=FHistStart+pt.X*FHistStep;
  end
  else begin
    xpos:=pt.X*FHistStep;
  end;
  dx1:=abs(ImgMin-xpos);
  dx2:=abs(ImgMax-xpos);
  if dx1=dx2 then begin
    Updmax:=(xpos>=ImgMax);
  end
  else begin
    Updmax:=dx2<dx1;
  end;
  StartUpd:=true;
end;

procedure Tf_visu.HistogramMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var xpos,val: double;
    pt: TDoublePoint;
    txt: string;
begin
if not Finitialized then exit;
if StartUpd then begin
  pt:=HistGraph.ImageToGraph(point(X,Y));
  if BtnClipRange.Down then begin
    xpos:=max(0,min(MAXWORD,FHistStart+pt.X*FHistStep));
  end
  else begin
    xpos:=max(0,min(MAXWORD,pt.X*FHistStep));
  end;
  if Updmax then
    HistGraphMaxLine.Position:=pt.X
  else
    HistGraphMinLine.Position:=pt.X;
  val:=FimageMin+xpos/FimageC;
  if FisFloatingPoint then
    txt:=FormatFloat(f3,val)
  else
    txt:=FormatFloat(f0,round(val));
  if Assigned(FShowHistogramPos) then FShowHistogramPos(txt);
end;
end;

procedure Tf_visu.HistGraphMouseEnter(Sender: TObject);
begin
  SpinEditMin.Visible:=true;
  SpinEditMax.Visible:=true;
end;

procedure Tf_visu.HistGraphMouseLeave(Sender: TObject);
begin
  SpinEditMin.Visible:=false;
  SpinEditMax.Visible:=false;
end;

procedure Tf_visu.HistogramMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var pt: TDoublePoint;
    xpos: double;
begin
  if not Finitialized then exit;
  pt:=HistGraph.ImageToGraph(point(X,Y));
  if BtnClipRange.Down then begin
    xpos:=max(0,min(MAXWORD,FHistStart+pt.X*FHistStep));
  end
  else begin
    xpos:=max(0,min(MAXWORD,pt.X*FHistStep));
  end;
  if Updmax then begin
    ImgMax:=xpos;
    ImgMax:=max(ImgMax,ImgMin);
  end
  else begin
    ImgMin:=xpos;
    ImgMin:=min(ImgMin,ImgMax);
  end;
  StartUpd:=false;
  HistogramAdjusted:=true;
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.SpinEditMaxChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  SpinEditMin.maxValue:=min(FimageMax,SpinEditMax.Value);
  if not LockHistogram then HistogramAdjusted:=true;
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.SpinEditMinChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  SpinEditMax.minValue:=max(FimageMin,SpinEditMin.Value);
  if not LockHistogram then HistogramAdjusted:=true;
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.TimerMinMaxTimer(Sender: TObject);
begin
  TimerMinMax.Enabled:=false;
  // scale from image min-max to 0-65535
  FImgMin:=round(FimageC*(SpinEditMin.Value-FimageMin));
  FImgMax:=round(FimageC*(SpinEditMax.Value-FimageMin));
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.TimerRedrawTimer(Sender: TObject);
begin
  TimerRedraw.Enabled:=false;
  if LockRedraw then begin
    TimerRedraw.Enabled:=true;
  end
  else begin
    try
     LockRedraw:=true;
     if Assigned(FShowHistogramPos) then FShowHistogramPos('');
     if Assigned(FRedraw) then FRedraw(self);
    finally
     LockRedraw:=false;
    end;
  end;
end;

function Tf_visu.GetHistBarPosition: integer;
begin
  // return splitter position in range 0..100
  result:=round(100*HistBarLeft.Width/(HistBar.ClientWidth-Splitter1.Width));
end;

procedure Tf_visu.SetHistBarPosition(value:integer);
begin
 // set splitter position from value in range 0..100
 HistBarLeft.Width:=max(min(round(value*(HistBar.ClientWidth-Splitter1.Width)/100),HistBar.ClientWidth-Splitter1.Width),1);
end;


end.

