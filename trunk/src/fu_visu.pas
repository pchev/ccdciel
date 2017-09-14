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
    BtnZoom05: TSpeedButton;
    BtnBullsEye: TSpeedButton;
    Histogram: TImage;
    Panel1: TPanel;
    hist98: TSpeedButton;
    hist99: TSpeedButton;
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
    procedure BtnZoomClick(Sender: TObject);
    procedure BtnIttChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure hist98Click(Sender: TObject);
    procedure hist99Click(Sender: TObject);
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
    FBullsEye, LockSpinEdit: Boolean;
    FZoom: double;
    StartUpd,Updmax: boolean;
    XP: integer;
    l98,h98,l99,h99: integer;
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
 ImgMax:=255;
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
var i,maxh,h,h2,l,sum,sl98,sh98,sl99,sh99: integer;
    sh: double;
begin
try
LockSpinEdit:=true;
maxh:=0;
sum:=0;
for i:=0 to 255 do begin
  sum:=sum+hist[i];
  if hist[i]>maxh then
      maxh:=hist[i];
end;
if maxh=0 then exit;
sl98:=round(0.005*sum); l98:=0;
sh98:=round(0.995*sum); h98:=0;
sl99:=round(0.002*sum); l99:=0;
sh99:=round(0.998*sum); h99:=0;
sum:=0;
for i:=0 to 255 do begin
  sum:=sum+hist[i];
  if (l98=0) and (sum>=sl98) then l98:=i;
  if (l99=0) and (sum>=sl99) then l99:=i;
  if (h98=0) and (sum>=sh98) then h98:=i;
  if (h99=0) and (sum>=sh99) then h99:=i;
end;
if SetLevel then begin
  if hist98.Down then begin
    FimgMin:=l98;
    FImgMax:=h98;
  end;
  if hist99.Down then begin
    FImgMin:=l99;
    FImgMax:=h99;
  end;
end;
SpinEditMin.Value:=round(FImgMin*256);
SpinEditMax.Value:=round(FimgMax*256);
Histogram.Picture.Bitmap.Width:=Histogram.Width;
Histogram.Picture.Bitmap.Height:=Histogram.Height;
with Histogram.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  sh:=height/ln(maxh);
  Canvas.Pen.Color:=clWhite;
  h2:=0;
  for i:=0 to 255 do begin
    h:=trunc(ln(hist[i])*sh);
    if (Histogram.Width=128) then begin
      if ((i mod 2)=0) then begin
        h:=(h+h2) div 2;
        l:=i div 2;
        Canvas.Line(l,Height,l,Height-h);
      end else begin
        h2:=h;
      end;
    end
    else Canvas.Line(i,Height,i,Height-h);
  end;
  Canvas.Pen.Color:=clRed;
  i:=round(ImgMin);
  Canvas.Line(i,0,i,Height);
  Canvas.Pen.Color:=clGreen;
  i:=round(ImgMax-1);
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

procedure Tf_visu.hist98Click(Sender: TObject);
begin
FimgMin:=l98;
FImgMax:=h98;
if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.hist99Click(Sender: TObject);
begin
FImgMin:=l99;
FImgMax:=h99;
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

procedure Tf_visu.histminmaxClick(Sender: TObject);
begin
  ImgMin:=0;
  ImgMax:=255;
  if Assigned(FRedraw) then FRedraw(self);
end;

procedure Tf_visu.HistogramMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var dx1,dx2: double;
begin
  dx1:=abs(ImgMin-X);
  dx2:=abs(ImgMax-X);
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
  if Updmax then ImgMax:=min(255,X)
            else ImgMin:=max(0,X);
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
  ImgMax:=SpinEditMax.Value/256;
  ImgMin:=SpinEditMin.Value/256;
  if Assigned(FRedraw) then FRedraw(self);
end;

end.

