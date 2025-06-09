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
  TASeries, TAChartUtils, u_translation, u_hints, LCLType, u_utils,
  u_global, Forms, Controls, ExtCtrls, StdCtrls, Buttons, SpinEx, ComCtrls;

type

  { Tf_visu }

  Tf_visu = class(TFrame)
    BtnClipRange: TSpeedButton;
    BtnClipping: TSpeedButton;
    BtnInvert: TSpeedButton;
    BtnFlipHorz: TSpeedButton;
    BtnFlipVert: TSpeedButton;
    BtnPinVisu: TSpeedButton;
    BtnShowImage: TSpeedButton;
    BtnZoom05: TSpeedButton;
    BtnBullsEye: TSpeedButton;
    cbHistRange: TComboBox;
    HistGraph: TChart;
    HistGraphAreaSeries1: TAreaSeries;
    Gamma: TFloatSpinEditEx;
    HistGraphMaxLine: TConstantLine;
    HistGraphMinLine: TConstantLine;
    LabelPos: TLabel;
    Panel1: TPanel;
    BtnZoomAdjust: TSpeedButton;
    HistBar: TPanel;
    HistBarLeft: TPanel;
    HistBarCenter: TPanel;
    HistBarRight: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BtnZoom2: TSpeedButton;
    BtnZoom1: TSpeedButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelNoDisplay: TPanel;
    BtnZoomHist: TSpeedButton;
    SpinEditMin: TFloatSpinEditEx;
    SpinEditMax: TFloatSpinEditEx;
    SplitterMin: TSplitter;
    SplitterMax: TSplitter;
    TimerResize: TTimer;
    Title: TLabel;
    TimerRedraw: TTimer;
    TimerMinMax: TTimer;
    procedure BtnBullsEyeClick(Sender: TObject);
    procedure BtnClippingClick(Sender: TObject);
    procedure BtnFlipHorzClick(Sender: TObject);
    procedure BtnFlipVertClick(Sender: TObject);
    procedure BtnClipRangeClick(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure BtnPinVisuClick(Sender: TObject);
    procedure BtnShowImageClick(Sender: TObject);
    procedure BtnZoomClick(Sender: TObject);
    procedure BtnZoomHistClick(Sender: TObject);
    procedure cbHistRangeChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure GammaChange(Sender: TObject);
    procedure HistBarCenterClick(Sender: TObject);
    procedure HistBarLeftClick(Sender: TObject);
    procedure HistBarRightClick(Sender: TObject);
    procedure HistGraphMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure histminmaxClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure SpinEditMaxChange(Sender: TObject);
    procedure SpinEditMinChange(Sender: TObject);
    procedure SplitterMaxMoved(Sender: TObject);
    procedure SplitterChangeBounds(Sender: TObject);
    procedure SplitterMinMoved(Sender: TObject);
    procedure TimerMinMaxTimer(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
    procedure TimerResizeTimer(Sender: TObject);
  private
    { private declarations }
    Fhist:Thistogram;
    Fmaxh, Fmaxp, Fsum: integer;
    FimageC, FimageMin, FimageMax, FdataMin, FdataMax, Fmean, Fsd : double;
    FisFloatingPoint, FisFlipped, Finitialized: boolean;
    FimgMin, FimgMax: double;
    FHistStart,FHistStop,FZoomStart,FZoomStop: integer;
    FBullsEye, LockSpinEdit, LockSpinInit, LockHistbar, FClipping, FInvert, FZoomCurrentRange: Boolean;
    FZoom: double;
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
    procedure PanelVisuClose(Sender: TObject; var CloseAction: TCloseAction);
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
 BtnClipping.Flat:=true;
 BtnZoom05.Flat:=true;
 BtnBullsEye.Flat:=true;
 BtnZoomAdjust.Flat:=true;
 BtnZoom2.Flat:=true;
 BtnZoom1.Flat:=true;
 BtnFlipHorz.Flat:=true;
 BtnFlipVert.Flat:=true;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 Finitialized:=false;
 ImgMax:=high(word);
 ImgMin:=0;
 FimageC:=1;
 Fmaxh:=0;
 FBullsEye:=false;
 FClipping:=false;
 FInvert:=false;
 LockSpinEdit:=true;
 LockSpinInit:=false;
 LockHistbar:=false;
 LockRedraw:=false;
 FisFlipped:=true;
 FZoomCurrentRange:=false;
end;

destructor  Tf_visu.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_visu.SetLang;
begin
  Title.Caption:=rsVisualisatio;
  cbHistRange.Items[0]:=rsDataRange;
  cbHistRange.Items[1]:=rsLow;
  cbHistRange.Items[2]:=rsMedium;
  cbHistRange.Items[3]:=rsHigh;
  cbHistRange.Items[4]:=rsVeryHigh;
  cbHistRange.Items[5]:=rsManual;
  HistGraph.Hint:=rsHistogramOfT;
  BtnZoomAdjust.Hint:=rsZoomToAdjust;
  HistBar.Hint:=rsClickAndMove;
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
  BtnPinVisu.Hint:=rsDetachTheGra;
  BtnZoomHist.Hint:=rsZoomTheHisto;
  cbHistRange.Hint:=rsPredefinedHi;
  SpinEditMin.Hint:=rsTheLowerLimi;
  SpinEditMax.Hint:=rsTheUpperLimi;
end;

procedure Tf_visu.SetLimit(SetLevel:boolean);
begin
  if SetLevel and (Fmaxh>0) then begin
    case cbHistRange.ItemIndex of
      0 : begin  // data range
            FimgMin:=FdataMin;
            FimgMax:=FdataMax;
          end;
      1 : begin  // low
            FimgMin:=FdataMin;
            FimgMax:=FdataMin + 0.5*(FdataMax-FdataMin);
          end;
      2 : begin  // medium
            FimgMin:=Fmean;
            FimgMax:=Fmean + 0.2*(FdataMax-Fmean);
          end;
      3 : begin  // high
            FimgMin:=Fmean;
            FimgMax:=Fmean + 0.1*(FdataMax-Fmean);
          end;
      4 : begin  // very high
            FimgMin:=Fmean;
            FimgMax:=Fmean + 3*Fsd;
          end;
      else begin  // manual
            // do not change previous setting
          end;
    end;
    if FimgMax<=FimgMin then
      FimgMax:=FimgMin+1;
    FImgMin:=(FImgMin-FimageMin)*FimageC;
    FImgMax:=(FImgMax-FimageMin)*FimageC;
  end;
  // adjust spinedit for data range
  LockSpinInit:=true;  // setting decimalplaces trigger onchange
  if FisFloatingPoint then begin
    if abs(FimageMax-FimageMin)<=1 then begin
      SpinEditMin.DecimalPlaces:=4;
      SpinEditMax.DecimalPlaces:=4;
      SpinEditMin.Increment:=0.001;
      SpinEditMax.Increment:=0.001;
    end
    else if abs(FimageMax-FimageMin)<=10 then begin
      SpinEditMin.DecimalPlaces:=3;
      SpinEditMax.DecimalPlaces:=3;
      SpinEditMin.Increment:=0.01;
      SpinEditMax.Increment:=0.01;
    end
    else begin
      SpinEditMin.DecimalPlaces:=1;
      SpinEditMax.DecimalPlaces:=1;
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
  LockSpinInit:=false;
  SpinEditMin.Value:=FimageMin+FImgMin/FimageC;
  SpinEditMax.Value:=FimageMin+FimgMax/FimageC;
end;

procedure Tf_visu.DrawHistogram(f: TFits; SetLevel,ResetCursor: boolean);
var i,iterations: integer;
begin
try
if not setlevel then exit;
LockSpinEdit:=true;
FisFloatingPoint:=f.HeaderInfo.floatingpoint;
FisFlipped:=f.HeaderInfo.roworder<>bottomup;
FimageC:=f.imageC;
FimageMin:=f.imageMin;
FimageMax:=f.imageMax;
FdataMin:=f.HeaderInfo.dmin;
FdataMax:=f.HeaderInfo.dmax;
f.stdev2(4,Fmean,Fsd,iterations);
Fmean:=FimageMin+Fmean/FimageC;
Fsd:=FimageMin+Fsd/FimageC;
for i:=0 to high(word) do Fhist[i]:=f.Histogram[i];
HistStats(Fhist,Fmaxh,Fmaxp,Fsum,FHistStart,FHistStop);
FZoomStart:=FHistStart;
FZoomStop:=FHistStop;
if Fmaxh=0 then exit;
SetLimit(SetLevel);
PlotHistogram;
finally
  LockSpinEdit:=false;
end;
end;

procedure Tf_visu.PlotHistogram;
var i,r: integer;
    x,t: double;
begin
HistGraphAreaSeries1.Clear;
if BtnClipRange.Down then begin
  if FZoomCurrentRange then begin
    t:=abs(SpinEditMax.Value-SpinEditMin.Value)/3;
    x:=max(FimageMin,SpinEditMin.Value-t);
    FZoomStart:=round(FimageC*(x-FimageMin));
    x:=min(FimageMax,SpinEditMax.Value+t);
    FZoomStop:=round(FimageC*(x-FimageMin));
  end
  else begin
    FZoomStart:=FHistStart;
    FZoomStop:=FHistStop;
  end;
  r:=FZoomStop-FZoomStart;
  for i:=0 to r-1  do begin
    x:=Fhist[FZoomStart+i];
    HistGraphAreaSeries1.Add(ln(x+1));
  end;
  HistGraphMinLine.Position:=max(0,(FimgMin-FZoomStart));
  HistGraphMaxLine.Position:=min(r,(FimgMax-FZoomStart));
  HistGraph.Extent.XMin:=0;
  HistGraph.Extent.XMax:=r;
  HistGraph.Extent.UseXMax:=true;
  HistGraph.Extent.UseXMin:=true;
end
else begin
  for i:=0 to 65535 do begin
    x:=Fhist[i];
    HistGraphAreaSeries1.Add(ln(x+1));
  end;
  HistGraphMinLine.Position:=FimgMin;
  HistGraphMaxLine.Position:=FimgMax;
  HistGraph.Extent.UseXMax:=false;
  HistGraph.Extent.UseXMin:=false;
end;
FZoomCurrentRange:=false;
Finitialized:=true;
SpinEditMaxChange(nil);
SpinEditMinChange(nil);
end;

procedure Tf_visu.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).Width>TPanel(Target).Height then begin
        width:=DoScaleX(440);
        height:=DoScaleY(127);
        panel2.Align:=alRight;
     end else begin
        width:=DoScaleX(262);
        height:=DoScaleY(254);
        panel2.Align:=alTop;
     end;
  end;
end;

procedure Tf_visu.FrameResize(Sender: TObject);
begin
  if Parent is TPanel then begin
     if TPanel(Parent).Width>TPanel(Parent).Height then begin
        width:=DoScaleX(440);
        height:=DoScaleY(127);
        panel2.Align:=alRight;
     end else begin
        width:=DoScaleX(262);
        height:=DoScaleY(254);
        panel2.Align:=alTop;
     end;
  end;
end;

procedure Tf_visu.Panel1Resize(Sender: TObject);
begin
  TimerResize.Enabled:=true;
  if panel1.Width>panel1.Height then begin
    panel2.Align:=alRight;
  end else begin
    panel2.Align:=alTop;
  end;
end;

procedure Tf_visu.TimerResizeTimer(Sender: TObject);
begin
TimerResize.Enabled:=false;
SpinEditMinChange(Sender);
SpinEditMaxChange(Sender);
SplitterMax.Left:=HistBarRight.left-1;
SplitterMin.Left:=HistBarLeft.left+1;
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

procedure Tf_visu.BtnZoomHistClick(Sender: TObject);
begin
  BtnClipRange.Down:=true;
  FZoomCurrentRange:=true;
  PlotHistogram;
  SplitterMax.Left:=HistBarRight.left-1;
  SplitterMin.Left:=HistBarLeft.left+1;
end;

procedure Tf_visu.cbHistRangeChange(Sender: TObject);
begin
  SetLimit(true);
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
  FZoomCurrentRange:=false;
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

procedure Tf_visu.SpinEditMaxChange(Sender: TObject);
var ma:double;
    p:integer;
begin
  if LockSpinInit then exit;
  if Fmaxh=0 then exit;
  LockHistbar:=true;
  p:=SplitterMax.Left;
  if BtnClipRange.Down then
    ma:=(round(FimageC*(SpinEditMax.Value-FimageMin))-FZoomStart)/(FZoomStop-FZoomStart)
  else
    ma:=(SpinEditMax.Value-FimageMin)/FimageMax;
  p:=round(1+HistBar.ClientWidth-ma*HistBar.ClientWidth-SplitterMax.Width);
  if HistBarRight.Width<>p then HistBarRight.Width:=p;
  LockHistbar:=false;
  if LockSpinEdit then exit;
  SpinEditMin.maxValue:=min(FimageMax,SpinEditMax.Value);
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.SpinEditMinChange(Sender: TObject);
var mi:double;
    p:integer;
begin
  if LockSpinInit then exit;
  if Fmaxh=0 then exit;
  LockHistbar:=true;
  if BtnClipRange.Down then
    mi:=(round(FimageC*(SpinEditMin.Value-FimageMin))-FZoomStart)/(FZoomStop-FZoomStart)
  else
    mi:=(SpinEditMin.Value-FimageMin)/FimageMax;
  p:=round(mi*HistBar.ClientWidth+1);
  if HistBarLeft.Width<>p then HistBarLeft.Width:=p;
  LockHistbar:=false;
  if LockSpinEdit then exit;
  SpinEditMax.minValue:=max(FimageMin,SpinEditMin.Value);
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.TimerMinMaxTimer(Sender: TObject);
begin
  TimerMinMax.Enabled:=false;
  // scale from image min-max to 0-65535
  FImgMin:=round(FimageC*(SpinEditMin.Value-FimageMin));
  FImgMax:=round(FimageC*(SpinEditMax.Value-FimageMin));
  if BtnClipRange.Down then begin
    HistGraphMinLine.Position:=max(0,(FimgMin-FZoomStart));
    HistGraphMaxLine.Position:=min(FZoomStop-FZoomStart,(FimgMax-FZoomStart));
  end
  else begin
    HistGraphMinLine.Position:=FimgMin;
    HistGraphMaxLine.Position:=FimgMax;
  end;
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

procedure Tf_visu.SplitterMaxMoved(Sender: TObject);
var p,ma: double;
begin
  p:=(SplitterMax.Left-1)/HistBar.ClientWidth;
  if BtnClipRange.Down then
    ma:=FimageMin+(FZoomStart + p*(FZoomStop-FZoomStart))/FimageC
  else
    ma:=FimageMin + p*(FimageMax-FimageMin);
  LockSpinEdit:=true;
  SpinEditMax.Value:=ma;
  LockSpinEdit:=false;
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.SplitterMinMoved(Sender: TObject);
var p,mi: double;
begin
  p:=(SplitterMin.Left-1)/HistBar.ClientWidth;
  if BtnClipRange.Down then
    mi:=FimageMin+(FZoomStart + p*(FZoomStop-FZoomStart))/FimageC
  else
    mi:=FimageMin + p*FimageMax;
  LockSpinEdit:=true;
  SpinEditMin.Value:=mi;
  LockSpinEdit:=false;
  TimerMinMax.Enabled:=false;
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.HistBarLeftClick(Sender: TObject);
begin
  HistBarLeft.Width:=max(1,HistBarLeft.Width-10);
  SplitterMinMoved(Sender);
end;

procedure Tf_visu.HistBarCenterClick(Sender: TObject);
var p: TPoint;
begin
  p:=mouse.CursorPos;
  p:=HistBarCenter.ScreenToClient(p);
  if p.x<(HistBarCenter.Width div 2) then begin
    HistBarLeft.Width:=min(HistBar.Width-1,HistBarLeft.Width+10);
    SplitterMinMoved(Sender);
  end
  else begin
    HistBarRight.Width:=min(HistBar.Width-1,HistBarRight.Width+10);
    SplitterMaxMoved(Sender);
  end;
end;

procedure Tf_visu.HistBarRightClick(Sender: TObject);
begin
  HistBarRight.Width:=max(1,HistBarRight.Width-10);
  SplitterMaxMoved(Sender);
end;

procedure Tf_visu.SplitterChangeBounds(Sender: TObject);
var p,mi: double;
    txt,fmt: string;
begin
  p:=(TSplitter(Sender).Left-1)/HistBar.ClientWidth;
  if BtnClipRange.Down then
    mi:=FimageMin+(FZoomStart + p*(FZoomStop-FZoomStart))/FimageC
  else
    mi:=FimageMin + p*FimageMax;
  case SpinEditMax.DecimalPlaces of
    0: fmt:=f0;
    1: fmt:=f1;
    2: fmt:=f2;
    3: fmt:=f3;
    4: fmt:=f4;
    else fmt:=f1;
  end;
  txt:=rsHistogram+': '+FormatFloat(fmt,mi);
  if LabelPos.Visible then
    LabelPos.Caption:=txt
  else
    if Assigned(FShowHistogramPos) then
       FShowHistogramPos(txt);
end;

procedure Tf_visu.HistGraphMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var xpos,val: double;
    pt: TDoublePoint;
    txt,fmt: string;
begin
  if not Finitialized then exit;
  pt:=HistGraph.ImageToGraph(point(X,Y));
  if BtnClipRange.Down then begin
    xpos:=FZoomStart+pt.X;
  end
  else begin
    xpos:=pt.X
  end;
  val:=FimageMin+xpos/FimageC;
  case SpinEditMax.DecimalPlaces of
    0: fmt:=f0;
    1: fmt:=f1;
    2: fmt:=f2;
    3: fmt:=f3;
    4: fmt:=f4;
    else fmt:=f1;
  end;
  txt:=rsHistogram+': '+FormatFloat(fmt,val);
  if LabelPos.Visible then
    LabelPos.Caption:=txt
  else
    if Assigned(FShowHistogramPos) then
       FShowHistogramPos(txt);
end;

procedure Tf_visu.BtnPinVisuClick(Sender: TObject);
var f: TForm;
    x,y,w,h: integer;
begin
  if Panel1.Parent is Tf_visu then begin
   x:=config.GetValue('/Visu/PosX',-1);
   y:=config.GetValue('/Visu/PosY',-1);
   w:=config.GetValue('/Visu/PosW',-1);
   h:=config.GetValue('/Visu/PosH',-1);
   f:=TForm.Create(self);
   f.FormStyle:=fsStayOnTop;
   f.OnClose:=@PanelVisuClose;
   if w>0 then
     f.Width:=w
   else
     f.Width:=DoScaleX(500);
   if h>0 then
     f.Height:=h
   else
     f.Height:=DoScaleY(200);
   f.Caption:=rsVisualisatio;
   labelpos.Visible:=true;
   Panel1.Parent:=f;
   if (x>0)and(y>0) then begin
     f.Left:=x;
     f.Top:=y;
   end
   else
     FormPos(f,mouse.CursorPos.x,mouse.CursorPos.y);
   f.Show;
  end
  else if Panel1.Parent is TForm then
   TForm(Panel1.Parent).Close;
end;

procedure Tf_visu.PanelVisuClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  config.SetValue('/Visu/PosX',TForm(Sender).Left);
  config.SetValue('/Visu/PosY',TForm(Sender).Top);
  config.SetValue('/Visu/PosW',TForm(Sender).Width);
  config.SetValue('/Visu/PosH',TForm(Sender).Height);
  labelpos.Visible:=false;
  CloseAction:=caFree;
  Panel1.Parent:=self;
end;

end.

