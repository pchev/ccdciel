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

uses Graphics, cu_fits, math, UScaleDPI, Classes, SysUtils, FileUtil, u_translation, u_hints,
  u_global, Forms, Controls, ExtCtrls, StdCtrls, Buttons, SpinEx, ComCtrls;

type

  { Tf_visu }

  Tf_visu = class(TFrame)
    BtnClipping: TSpeedButton;
    BtnInvert: TSpeedButton;
    BtnFlipHorz: TSpeedButton;
    BtnFlipVert: TSpeedButton;
    BtnShowImage: TSpeedButton;
    BtnZoom05: TSpeedButton;
    BtnBullsEye: TSpeedButton;
    Gamma: TFloatSpinEditEx;
    Histogram: TImage;
    Panel1: TPanel;
    BtnZoomAdjust: TSpeedButton;
    Panel2: TPanel;
    Panel3: TPanel;
    BtnZoom2: TSpeedButton;
    BtnZoom1: TSpeedButton;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelNoDisplay: TPanel;
    SpinEditMin: TFloatSpinEditEx;
    SpinEditMax: TFloatSpinEditEx;
    Title: TLabel;
    TimerRedraw: TTimer;
    TimerMinMax: TTimer;
    HistBar: TTrackBar;
    procedure BtnBullsEyeClick(Sender: TObject);
    procedure BtnClippingClick(Sender: TObject);
    procedure BtnFlipHorzClick(Sender: TObject);
    procedure BtnFlipVertClick(Sender: TObject);
    procedure BtnInvertClick(Sender: TObject);
    procedure BtnShowImageClick(Sender: TObject);
    procedure BtnZoomClick(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure GammaChange(Sender: TObject);
    procedure histminmaxClick(Sender: TObject);
    procedure HistogramMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HistogramMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure HistogramMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinEditMaxChange(Sender: TObject);
    procedure SpinEditMinChange(Sender: TObject);
    procedure TimerMinMaxTimer(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
    procedure HistBarChange(Sender: TObject);
  private
    { private declarations }
    Fhist:Thistogram;
    Fmaxh, Fmaxp, Fsum: integer;
    FimageC, FimageMin, FimageMax : double;
    FisFloatingPoint, Finitialized: boolean;
    FimgMin, FimgMax: double;
    FBullsEye, LockSpinEdit, FClipping, FInvert, FFlipVert, FFlipHorz: Boolean;
    FZoom: double;
    StartUpd,Updmax: boolean;
    XP: integer;
    FRedraw: TNotifyEvent;
    FonZoom: TNotifyEvent;
    FRedrawHistogram: TNotifyEvent;
    FShowHistogramPos: TNotifyStr;
    FShowLastImage: TNotifyEvent;
    procedure SetZoom(value: double);
    procedure SetFlipHorz(value:boolean);
    procedure SetFlipVert(value:boolean);
    procedure SetLang;
    procedure SetLimit(SetLevel:boolean);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure DrawHistogram(hist:Thistogram; SetLevel,isFloatingPoint: boolean; iC,iMin,iMax: double);
    property Zoom: double read FZoom write SetZoom;
    property ImgMin: double read FimgMin write FimgMin;
    property ImgMax: double read FimgMax write FimgMax;
    property BullsEye: boolean read FBullsEye;
    property Clipping: boolean read FClipping;
    property Invert: boolean read FInvert;
    property FlipHorz: boolean read FFlipHorz write SetFlipHorz;
    property FlipVert: boolean read FFlipVert write SetFlipVert;
    property onZoom: TNotifyEvent read FonZoom write FonZoom;
    property onRedraw: TNotifyEvent read FRedraw write FRedraw;
    property onRedrawHistogram: TNotifyEvent read FRedrawHistogram write FRedrawHistogram;
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
 {$ifdef lclgtk2}
   HistBar.top:=-((HistBar.Height-Panel5.Height) div 2);
 {$else}
   HistBar.Top:=0;
   HistBar.Height:=Panel5.ClientHeight;
 {$endif}
 SetLang;
 Finitialized:=false;
 ImgMax:=high(word);
 ImgMin:=0;
 FBullsEye:=false;
 FClipping:=false;
 FInvert:=false;
 FFlipVert:=false;
 FFlipHorz:=false;
 LockSpinEdit:=true;
 with Histogram.Picture.Bitmap do begin
   Width:=Histogram.Width;
   Height:=Histogram.Height;
   Canvas.Brush.Color:=clBlack;
   Canvas.Pen.Color:=clBlack;
   Canvas.FillRect(0,0,Width,Height);
 end;
end;

destructor  Tf_visu.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_visu.SetLang;
begin
  Title.Caption:=rsVisualisatio;
  Histogram.Hint:=Format(rsHistogramOfT, [crlf]);
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
end;

procedure Tf_visu.SetLimit(SetLevel:boolean);
var hval: double;
    i,sum,slh,shh,lh,hh: integer;

begin
  hval:=(101-power(10,HistBar.Position/100))/100;
  slh:=round((1-hval)*Fsum); lh:=0;
  shh:=round(hval*Fsum); hh:=0;
  sum:=0;
  for i:=0 to high(word) do begin
    sum:=sum+Fhist[i];
    if i>(0.7*Fmaxp) then begin
      if (lh=0) and (sum>=slh) then lh:=i;
      if (hh=0) and (sum>=shh) then hh:=i;
    end;
  end;
  if SetLevel then begin
    if hval=1 then begin
      FImgMin:=0;
      FImgMax:=high(word);
    end
    else begin
      FimgMin:=lh;
      FImgMax:=hh;
    end;
  end;
  // adjust spinedit for data range
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
end;

procedure Tf_visu.DrawHistogram(hist:Thistogram; SetLevel,isFloatingPoint: boolean; iC,iMin,iMax: double);
var i,j,h,hd2,l: integer;
    hc: integer;
    sh: double;
begin
try
LockSpinEdit:=true;
FimageC:=iC;
FimageMin:=iMin;
FimageMax:=iMax;
FisFloatingPoint:=isFloatingPoint;
Fmaxh:=0;
Fsum:=0;
for i:=0 to high(word) do begin
  Fhist[i]:=hist[i];
  Fsum:=Fsum+hist[i];
  if hist[i]>Fmaxh then begin
      Fmaxh:=hist[i];
      Fmaxp:=i;
  end;
end;
if Fmaxh=0 then exit;
if Fmaxp>(high(word) div 10) then Fmaxp:=0; // peak is probably not sky background

SetLimit(SetLevel);

Histogram.Picture.Bitmap.Width:=Histogram.Width;
Histogram.Picture.Bitmap.Height:=Histogram.Height;
with Histogram.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  sh:=height/ln(Fmaxh);
  Canvas.Pen.Color:=clWhite;
  hd2:=0;
  for i:=0 to 255 do begin
    hc:=0;
    for j:=0 to 255 do
       if Fhist[255*i+j]>hc then hc:=Fhist[255*i+j];
    h:=trunc(ln(hc)*sh);
    if (Histogram.Width=128) then begin
      if ((i mod 2)=0) then begin
        h:=(h+hd2) div 2;
        l:=i div 2;
        Canvas.Line(l,Height,l,Height-h);
      end else begin
        hd2:=h;
      end;
    end
    else Canvas.Line(i,Height,i,Height-h);
  end;
  Canvas.Pen.Color:=clRed;
  i:=round(ImgMin/255);
  Canvas.Line(i,0,i,Height);
  Canvas.Pen.Color:=clGreen;
  i:=round(ImgMax/255-1);
  Canvas.Line(i,0,i,Height);
  Finitialized:=true;
end;
finally
  LockSpinEdit:=false;
end;
end;

procedure Tf_visu.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).Width>TPanel(Target).Height then begin
        Panel3.Constraints.MaxWidth:=256;
        Panel3.Constraints.MinWidth:=256;
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
        Panel3.Constraints.MaxWidth:=128;
        Panel3.Constraints.MinWidth:=128;
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
  if Assigned(FRedrawHistogram) then FRedrawHistogram(self);
end;

procedure Tf_visu.FrameResize(Sender: TObject);
begin
  if Parent is TPanel then begin
     if TPanel(Parent).Width>TPanel(Parent).Height then begin
        Panel3.Constraints.MaxWidth:=256;
        Panel3.Constraints.MinWidth:=256;
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
        Panel3.Constraints.MaxWidth:=128;
        Panel3.Constraints.MinWidth:=128;
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
  if Assigned(FRedrawHistogram) then FRedrawHistogram(self);
end;

procedure Tf_visu.HistBarChange(Sender: TObject);
begin
  SetLimit(true);
  TimerRedraw.Enabled:=false;
  TimerRedraw.Enabled:=true;
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
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnBullsEyeClick(Sender: TObject);
begin
  FBullsEye:=not FBullsEye;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnClippingClick(Sender: TObject);
begin
  FClipping:=BtnClipping.Down;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.SetFlipHorz(value:boolean);
begin
  FFlipHorz:=value;
  BtnFlipHorz.Down:=value;
end;

procedure Tf_visu.SetFlipVert(value:boolean);
begin
  FFlipVert:=value;
  BtnFlipVert.Down:=value;
end;

procedure Tf_visu.BtnFlipHorzClick(Sender: TObject);
begin
  FFlipHorz:=BtnFlipHorz.Down;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnFlipVertClick(Sender: TObject);
begin
  FFlipVert:=BtnFlipVert.Down;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.BtnInvertClick(Sender: TObject);
begin
  FInvert:=BtnInvert.Down;
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
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.HistogramMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var dx1,dx2: double;
begin
  if not Finitialized then exit;
  dx1:=abs(ImgMin/255-X);
  dx2:=abs(ImgMax/255-X);
  if dx1=dx2 then begin
    Updmax:=X>=(ImgMax/255);
  end
  else begin
    Updmax:=dx2<dx1;
  end;
  if Updmax then XP:=round(ImgMax) else XP:=round(ImgMin);
  StartUpd:=true;
end;

procedure Tf_visu.HistogramMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xpos,val: double;
    txt: string;
begin
if not Finitialized then exit;
if StartUpd then begin
  with Histogram.Picture.Bitmap do begin
    Canvas.Pen.Color:=clWhite;
    Canvas.Pen.Mode:=pmXor;
    Canvas.Line(XP,0,XP,Height);
    Canvas.Line(X,0,X,Height);
    XP:=X;
    if Updmax then xpos:=min(high(word),X*255)
              else xpos:=max(0,X*255);
    val:=FimageMin+xpos/FimageC;
    if FisFloatingPoint then
      txt:=FormatFloat(f3,val)
    else
      txt:=FormatFloat(f0,round(val));
    if Assigned(FShowHistogramPos) then FShowHistogramPos(txt);
  end;
end else begin
  SpinEditMin.Visible:=(y<SpinEditMin.Height)and(x<SpinEditMin.Width);
  SpinEditMax.Visible:=(y<SpinEditMax.Height)and(x>SpinEditMax.Left);
end;
end;

procedure Tf_visu.HistogramMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Finitialized then exit;
  if Updmax then begin
    ImgMax:=min(high(word),X*255);
    ImgMax:=max(ImgMax,ImgMin);
  end
  else begin
    ImgMin:=max(0,X*255);
    ImgMin:=min(ImgMin,ImgMax);
  end;
  StartUpd:=false;
  TimerRedraw.Enabled:=true;
end;

procedure Tf_visu.SpinEditMaxChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  SpinEditMin.maxValue:=min(FimageMax,SpinEditMax.Value);
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.SpinEditMinChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  SpinEditMax.minValue:=max(FimageMin,SpinEditMin.Value);
  TimerMinMax.Enabled:=true;
end;

procedure Tf_visu.TimerMinMaxTimer(Sender: TObject);
begin
  TimerMinMax.Enabled:=false;
  // scale from image min-max to 0-65535
  FImgMin:=round(FimageC*(SpinEditMin.Value-FimageMin));
  FImgMax:=round(FimageC*(SpinEditMax.Value-FimageMin));
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.TimerRedrawTimer(Sender: TObject);
begin
  TimerRedraw.Enabled:=false;
  if Assigned(FShowHistogramPos) then FShowHistogramPos('');
  if Assigned(FRedraw) then FRedraw(self);
end;

end.

