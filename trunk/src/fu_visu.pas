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

uses Graphics, cu_fits, math, UScaleDPI, Classes, SysUtils, FileUtil,
  Forms, Controls, ExtCtrls, StdCtrls, Buttons, Spin;

type

  { Tf_visu }

  Tf_visu = class(TFrame)
    BtnClipping: TSpeedButton;
    BtnZoom05: TSpeedButton;
    BtnBullsEye: TSpeedButton;
    Histogram: TImage;
    Panel1: TPanel;
    hist1: TSpeedButton;
    hist2: TSpeedButton;
    histminmax: TSpeedButton;
    BtnLinear: TRadioButton;
    BtnLog: TRadioButton;
    BtnSqrt: TRadioButton;
    BtnZoomAdjust: TSpeedButton;
    Panel2: TPanel;
    Panel3: TPanel;
    BtnZoom2: TSpeedButton;
    BtnZoom1: TSpeedButton;
    SpinEditMin: TSpinEdit;
    SpinEditMax: TSpinEdit;
    StaticText1: TStaticText;
    Timer1: TTimer;
    procedure BtnBullsEyeClick(Sender: TObject);
    procedure BtnClippingClick(Sender: TObject);
    procedure BtnZoomClick(Sender: TObject);
    procedure BtnIttChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure hist1Click(Sender: TObject);
    procedure hist2Click(Sender: TObject);
    procedure histminmaxClick(Sender: TObject);
    procedure HistogramMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HistogramMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure HistogramMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinEditMaxChange(Sender: TObject);
    procedure SpinEditMinChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FimgMin, FimgMax: double;
    FBullsEye, LockSpinEdit, FClipping: Boolean;
    FZoom: double;
    StartUpd,Updmax: boolean;
    XP: integer;
    l1,h1,l2,h2: integer;
    FRedraw: TNotifyEvent;
    FonZoom: TNotifyEvent;
    FRedrawHistogram: TNotifyEvent;
    procedure SetZoom(value: double);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure DrawHistogram(hist:Thistogram; SetLevel: boolean);
    property Zoom: double read FZoom write SetZoom;
    property ImgMin: double read FimgMin write FimgMin;
    property ImgMax: double read FimgMax write FimgMax;
    property BullsEye: boolean read FBullsEye;
    property Clipping: boolean read FClipping;
    property onZoom: TNotifyEvent read FonZoom write FonZoom;
    property onRedraw: TNotifyEvent read FRedraw write FRedraw;
    property onRedrawHistogram: TNotifyEvent read FRedrawHistogram write FRedrawHistogram;
  end;

implementation

{$R *.lfm}

{ Tf_visu }

constructor Tf_visu.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 ImgMax:=high(word);
 ImgMin:=0;
 FBullsEye:=false;
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

procedure Tf_visu.DrawHistogram(hist:Thistogram; SetLevel: boolean);
var i,j,maxh,h,hd2,l: integer;
    sum,sl1,sh1,sl2,sh2,hc: integer;
    sh: double;
begin
try
LockSpinEdit:=true;
maxh:=0;
sum:=0;
for i:=0 to high(word) do begin
  sum:=sum+hist[i];
  if hist[i]>maxh then
      maxh:=hist[i];
end;
if maxh=0 then exit;
sl1:=round(0.050*sum); l1:=0;
sh1:=round(0.950*sum); h1:=0;
sl2:=round(0.100*sum); l2:=0;
sh2:=round(0.900*sum); h2:=0;
sum:=0;
for i:=0 to high(word) do begin
  sum:=sum+hist[i];
  if (l1=0) and (sum>=sl1) then l1:=i;
  if (l2=0) and (sum>=sl2) then l2:=i;
  if (h1=0) and (sum>=sh1) then h1:=i;
  if (h2=0) and (sum>=sh2) then h2:=i;
end;
if SetLevel then begin
  if hist1.Down then begin
    FimgMin:=l1;
    FImgMax:=h1;
  end;
  if hist2.Down then begin
    FImgMin:=l2;
    FImgMax:=h2;
  end;
end;
SpinEditMin.Value:=round(FImgMin);
SpinEditMax.Value:=round(FimgMax);
Histogram.Picture.Bitmap.Width:=Histogram.Width;
Histogram.Picture.Bitmap.Height:=Histogram.Height;
with Histogram.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  sh:=height/ln(maxh);
  Canvas.Pen.Color:=clWhite;
  hd2:=0;
  for i:=0 to 255 do begin
    hc:=0;
    for j:=0 to 255 do
       if hist[255*i+j]>hc then hc:=hist[255*i+j];
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
end;
finally
  Application.ProcessMessages;
  LockSpinEdit:=false;
end;
end;

procedure Tf_visu.BtnIttChange(Sender: TObject);
begin
  if Assigned(FRedraw) then FRedraw(self);
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

procedure Tf_visu.hist1Click(Sender: TObject);
begin
FimgMin:=l1;
FImgMax:=h1;
if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.hist2Click(Sender: TObject);
begin
FImgMin:=l2;
FImgMax:=h2;
if Assigned(FRedraw) then FRedraw(self);
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

procedure Tf_visu.BtnBullsEyeClick(Sender: TObject);
begin
  FBullsEye:=not FBullsEye;
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.BtnClippingClick(Sender: TObject);
begin
  FClipping:=BtnClipping.Down;
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.histminmaxClick(Sender: TObject);
begin
  ImgMin:=0;
  ImgMax:=high(word);
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.HistogramMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var dx1,dx2: double;
begin
  dx1:=abs(ImgMin/255-X);
  dx2:=abs(ImgMax/255-X);
  Updmax:=dx2<dx1;
  if Updmax then XP:=round(ImgMax) else XP:=round(ImgMin);
  StartUpd:=true;
end;

procedure Tf_visu.HistogramMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if StartUpd then begin
  with Histogram.Picture.Bitmap do begin
    Canvas.Pen.Color:=clWhite;
    Canvas.Pen.Mode:=pmXor;
    Canvas.Line(XP,0,XP,Height);
    Canvas.Line(X,0,X,Height);
    XP:=X;
  end;
end else begin
  SpinEditMin.Visible:=(y<24)and(x<60);
  SpinEditMax.Visible:=(y<24)and(x>195);
end;
end;

procedure Tf_visu.HistogramMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Updmax then ImgMax:=min(high(word),X*255)
            else ImgMin:=max(0,X*255);
  StartUpd:=false;
  histminmax.Down:=true;
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.SpinEditMaxChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  timer1.Enabled:=true;
end;

procedure Tf_visu.SpinEditMinChange(Sender: TObject);
begin
  if LockSpinEdit then exit;
  timer1.Enabled:=true;
end;

procedure Tf_visu.Timer1Timer(Sender: TObject);
begin
  timer1.Enabled:=false;
  histminmax.Down:=true;
  ImgMax:=SpinEditMax.Value;
  ImgMin:=SpinEditMin.Value;
  if Assigned(FRedraw) then FRedraw(self);
end;

end.

