unit fu_starprofile;

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

uses BGRABitmap, BGRABitmapTypes,
  u_global, u_utils, math, UScaleDPI, fu_preview,
  fu_focuser, Graphics, Classes, SysUtils, FPImage, cu_fits, FileUtil, TAGraph,
  TAFuncSeries, TASeries, TASources, Forms, Controls, StdCtrls, ExtCtrls;

const maxhist=50;

type

  { Tf_starprofile }

  Tf_starprofile = class(TFrame)
    ChkAutofocus: TCheckBox;
    ChkFocus: TCheckBox;
    FitSourceL: TListChartSource;
    FitSourceR: TListChartSource;
    graph: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelFWHM: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    profile: TImage;
    LabelHFD: TLabel;
    LabelImax: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    VcChart: TChart;
    VcChartL: TFitSeries;
    VcChartR: TFitSeries;
    procedure ChkAutofocusChange(Sender: TObject);
    procedure ChkFocusChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure graphDblClick(Sender: TObject);
  private
    { private declarations }
    FFindStar: boolean;
    FStarX,FStarY,FValMax: double;
    FFocusStart,FFocusStop: TNotifyEvent;
    FAutoFocusStop,FAutoFocusStart: TNotifyEvent;
    FonFocusIN, FonFocusOUT, FonAbsolutePosition: TNotifyEvent;
    FonMsg: TNotifyMsg;
    Fpreview:Tf_preview;
    Ffocuser:Tf_focuser;
    emptybmp:Tbitmap;
    histfwhm, histimax: array[0..maxhist] of double;
    maxfwhm,maximax: double;
    Fhfd,Ffwhm,Ffwhmarcsec,FLastHfd,FSumHfd,Fsnr,FMinSnr,FminPeak:double;
    curhist,FfocuserSpeed,FnumHfd,FPreFocusPos: integer;
    focuserdirection,terminated,FirstFrame: boolean;
    FAutofocusResult: boolean;
    ahfd: array of double;
    aminhfd,amaxhfd:double;
    afmpos,aminpos:integer;
    procedure msg(txt:string);
    function  getRunning:boolean;
    procedure PlotProfile(img:Timaw16; c,vmin,bg: double; s:integer);
    procedure PlotHistory;
    procedure ClearGraph;
    procedure doAutofocusVcurve;
    procedure doAutofocusMean;
    procedure doAutofocusIterative;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure GetHFD(img:Timaw16; c,vmin: double; x,y,ri: integer; var bg: double; out xc,yc,hfd,star_fwhm,valmax: double);
    procedure FindStarPos(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer; out xc,yc,ri:integer; out vmax,bg: double);
    procedure FindBrightestPixel(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax,starwindow2: integer; out xc,yc:integer; out vmax: double);
    procedure ShowProfile(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer; focal:double=-1; pxsize:double=-1);
    procedure Autofocus(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer);
    procedure InitAutofocus;
    property AutofocusRunning: boolean read getRunning;
    property FindStar : boolean read FFindStar write FFindStar;
    property HFD:double read Fhfd;
    property SNR:double read Fsnr;
    property ValMax: double read FValMax;
    property PreFocusPos: integer read FPreFocusPos write FPreFocusPos;
    property StarX: double read FStarX write FStarX;
    property StarY: double read FStarY write FStarY;
    property AutofocusResult: boolean read FAutofocusResult write FAutofocusResult;
    property preview:Tf_preview read Fpreview write Fpreview;
    property focuser:Tf_focuser read Ffocuser write Ffocuser;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onFocusStart: TNotifyEvent read FFocusStart write FFocusStart;
    property onFocusStop: TNotifyEvent read FFocusStop write FFocusStop;
    property onAutoFocusStart: TNotifyEvent read FAutoFocusStart write FAutoFocusStart;
    property onAutoFocusStop: TNotifyEvent read FAutoFocusStop write FAutoFocusStop;
    property onFocusIN: TNotifyEvent read FonFocusIN write FonFocusIN;
    property onFocusOUT: TNotifyEvent read FonFocusOUT write FonFocusOUT;
    property onAbsolutePosition: TNotifyEvent read FonAbsolutePosition write FonAbsolutePosition;
  end;

implementation

{$R *.lfm}

{ Tf_starprofile }

procedure Tf_starprofile.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
 if Target is TPanel then begin
    if TPanel(Target).Width>TPanel(Target).Height then begin
       Panel1.ChildSizing.ControlsPerLine:=2;
       Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    end else begin
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    end;
 end;
end;

procedure Tf_starprofile.ChkFocusChange(Sender: TObject);
begin
 if ChkAutofocus.Checked then begin
   ChkFocus.Checked:=false;
   exit;
 end;
 if ChkFocus.Checked then begin
    if Assigned(FFocusStart) then FFocusStart(self);
 end else begin
   if Assigned(FFocusStop) then FFocusStop(self);
 end;
end;

procedure Tf_starprofile.ChkAutofocusChange(Sender: TObject);
begin
 if ChkFocus.Checked then begin
    ChkAutofocus.Checked:=false;
    exit;
 end;
 if ChkAutofocus.Checked then begin
    if Assigned(FAutoFocusStart) then FAutoFocusStart(self);
 end else begin
   terminated:=true;
   if Assigned(FAutoFocusStop) then FAutoFocusStop(self);
 end;
end;

function  Tf_starprofile.getRunning:boolean;
begin
 result:=ChkAutofocus.Checked;
end;

procedure Tf_starprofile.InitAutofocus;
begin
 FnumHfd:=0;
 FSumHfd:=0;
 FMinSnr:=99999;
 FminPeak:=9999999;
 terminated:=false;
 FirstFrame:=true;
 FAutofocusResult:=false;
 FfocuserSpeed:=AutofocusMaxSpeed;
 focuser.FocusSpeed:=FfocuserSpeed;
 focuserdirection:=AutofocusMoveDir;
 FPreFocusPos:=focuser.FocusPosition;
 AutofocusMeanStep:=afmStart;
 if focuserdirection=FocusDirOut then
    AutofocusVcStep:=vcsStartL
  else
    AutofocusVcStep:=vcsStartR;
 case AutofocusMode of
   afVcurve   : msg('Autofocus start Vcurve');
   afMean     : msg('Autofocus start Dynamic curve');
   afIterative: msg('Autofocus start Iterative focus');
 end;
end;

procedure Tf_starprofile.FrameResize(Sender: TObject);
begin
 if Parent is TPanel then begin
    if TPanel(Parent).Width>TPanel(Parent).Height then begin
       Panel1.ChildSizing.ControlsPerLine:=2;
       Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    end else begin
        Panel1.ChildSizing.ControlsPerLine:=99;
        Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
    end;
 end;
end;

procedure Tf_starprofile.graphDblClick(Sender: TObject);
begin
 curhist:=0;
 maxfwhm:=0;
 maximax:=0;
end;

procedure Tf_starprofile.msg(txt:string);
begin
 if assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_starprofile.ClearGraph;
begin
 profile.Picture.Bitmap.Width:=profile.Width;
 profile.Picture.Bitmap.Height:=profile.Height;
 with profile.Picture.Bitmap do begin
   Canvas.Brush.Color:=clBlack;
   Canvas.Pen.Color:=clBlack;
   Canvas.Pen.Mode:=pmCopy;
   Canvas.FillRect(0,0,Width,Height);
 end;
 graph.Picture.Bitmap.Width:=graph.Width;
 graph.Picture.Bitmap.Height:=graph.Height;
 with graph.Picture.Bitmap do begin
   Canvas.Brush.Color:=clBlack;
   Canvas.Pen.Color:=clBlack;
   Canvas.Pen.Mode:=pmCopy;
   Canvas.FillRect(0,0,Width,Height);
 end;
end;

constructor Tf_starprofile.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 ScaleDPI(Self);
 emptybmp:=Tbitmap.Create;
 emptybmp.SetSize(1,1);
 FFindStar:=false;
 curhist:=0;
 maxfwhm:=0;
 maximax:=0;
 FPreFocusPos:=-1;
 focuserdirection:=FocusDirIn;
 FLastHfd:=MaxInt;
 LabelHFD.Caption:='-';
 LabelFWHM.Caption:='-';
 LabelImax.Caption:='-';
 ClearGraph;
end;

destructor  Tf_starprofile.Destroy;
begin
 emptybmp.Free;
 inherited Destroy;
end;

function double_star(img:Timaw16; c,vmin : double;ri, x,y : integer):boolean; // double star detection based difference bright_spot and center_of_gravity

var SumVal,SumValX,SumValY,val,valmax,bg, Xg, Yg: double;
     i,j : integer;
begin
  // New background from corner values
  bg:=0;
  for i:=-ri+1 to ri do {calculate average background at the square boundaries of region of interest}
  begin
    bg:=bg+Img[0,y+ri,x+i];{top line, left to right}
    bg:=bg+Img[0,y+i,x+ri];{right line, top to bottom}
    bg:=bg+Img[0,y-ri,x-i];{bottom line, right to left}
    bg:=bg+Img[0,y-i,x-ri];{right line, bottom to top}
  end;
  bg:=bg/(8*ri);
  bg:=vmin+bg/c;

  SumVal:=0;
  SumValX:=0;
  SumValY:=0;
  valmax:=0;
  for i:=-ri to ri do
    for j:=-ri to ri do
    begin
      val:=vmin+Img[0,y+j,x+i]/c-bg;
      if val<0 then val:=0;
      if val>valmax then valmax:=val;
      SumVal:=SumVal+val;
      SumValX:=SumValX+val*(i);
     SumValY:=SumValY+val*(j);
    end;
  Xg:=SumValX/SumVal;
  Yg:=SumValY/SumVal;
  if ((Xg*Xg)+(Yg*Yg))>0.3 then result:=true {0.3 is experimental factor. Double star, too much unbalance between bright spot and centre of gravity}
    else
    result:=false;
end;{double star detection}

procedure Tf_starprofile.FindBrightestPixel(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax,starwindow2: integer; out xc,yc:integer; out vmax: double);
// brightest 3x3 pixels in area s*s centered on x,y of image Img of size xmax,ymax
var i,j,rs,xm,ym: integer;
            val :double;
begin
 rs:= s div 2;
 if (x-rs)<3 then x:=rs+3;
 if (x+rs)>(xmax-3) then x:=xmax-rs-3;
 if (y-rs)<3 then y:=rs+3;
 if (y+rs)>(ymax-3) then y:=ymax-rs-3;

 vmax:=0;
 xm:=0;
 ym:=0;

 // try with double star exclusion
 for i:=-rs to rs do
   for j:=-rs to rs do begin
     val:=(Img[0,y+j-1 ,x+i-1]+Img[0,y+j-1 ,x+i]+Img[0,y+j-1 ,x+i+1]+
           Img[0,y+j ,x+i-1]+Img[0,y+j ,x+i]+Img[0,y+j ,x+i+1]+
           Img[0,y+j+1 ,x+i-1]+Img[0,y+j+1 ,x+i]+Img[0,y+j+1 ,x+i+1])/9;

     Val:=vmin+Val/c;
     if Val>vmax then
     begin
       if double_star(img,c,vmin,starwindow2, x+i,y+j)=false then
       begin
         vmax:=Val;
         xm:=i;
         ym:=j;
       end;
     end;
 end;

 // if we not find anything repeat with only max value
 if vmax=0 then
   for i:=-rs to rs do
     for j:=-rs to rs do begin
       val:=(Img[0,y+j-1 ,x+i-1]+Img[0,y+j-1 ,x+i]+Img[0,y+j-1 ,x+i+1]+
             Img[0,y+j ,x+i-1]+Img[0,y+j ,x+i]+Img[0,y+j ,x+i+1]+
             Img[0,y+j+1 ,x+i-1]+Img[0,y+j+1 ,x+i]+Img[0,y+j+1 ,x+i+1])/9;

       Val:=vmin+Val/c;
       if Val>vmax then
       begin
         vmax:=Val;
         xm:=i;
         ym:=j;
       end;
   end;

 xc:=x+xm;
 yc:=y+ym;

end;

procedure Tf_starprofile.FindStarPos(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer; out xc,yc,ri:integer; out vmax,bg: double);
// center of gravity in area s*s centered on x,y of image Img of size xmax,ymax
var i,j,rs: integer;
    SumVal,SumValX,SumValY: double;
    val,xg,yg:double;
    b:TBGRABitmap;
    p: PBGRAPixel;
    xm,ym,tol:integer;
    imin,xx,yy: double;
begin
  vmax:=0;
  bg:=0;
  rs:=s div 2;
  if (x-rs)<1 then x:=rs+1;
  if (x+rs)>(xmax-1) then x:=xmax-rs-1;
  if (y-rs)<1 then y:=rs+1;
  if (y+rs)>(ymax-1) then y:=ymax-rs-1;
  // find brightest point position, max and min values
  vmax:=0;
  imin:=MaxInt;
  SumVal:=0;
  for i:=-rs to rs do
    for j:=-rs to rs do begin
      Val:=vmin+Img[0,y+j,x+i]/c;
      SumVal:=SumVal+val;
      if val<imin then imin:=val;
      if Val>vmax then begin
           vmax:=Val;
           xm:=j;
           ym:=i;
      end;
    end;
  xm:=rs+xm;
  ym:=rs+ym;
  if imin=vmax then imin:=val-1;
  // average background
  bg:=0;
  for i:=-rs+1 to rs do {calculate average background at the square boundaries of region of interest}
  begin
    bg:=bg+Img[0,y+rs,x+i];{top line, left to right}
    bg:=bg+Img[0,y+i,x+rs];{right line, top to bottom}
    bg:=bg+Img[0,y-rs,x-i];{bottom line, right to left}
    bg:=bg+Img[0,y-i,x-rs];{right line, bottom to top}
  end;
  bg:=vmin+bg/(8*rs)/c;

  // copy to bitmap, scale min=0 to max=250
  b:=TBGRABitmap.Create(s+1,s+1);
  for i:=-rs to rs do  begin
   p := b.Scanline[i+rs];
   for j:=-rs to rs do begin
     val:=vmin+Img[0,y+j,x+i]/c;
     val:=(val-imin)*250/(vmax-imin);
     p^.red:=round(val);
     p^.green:=p^.red;
     p^.blue:=p^.red;
     p^.alpha:=255;
     inc(p);
   end;
  end;
  b.InvalidateBitmap;
  // fill values above average, connected to brightest pixel
  tol:=round((vmax-bg)*250/(vmax-imin));
  if tol<1 then tol:=1;
  if tol>245 then tol:=245;
  b.FloodFill(xm,ym,BGRAWhite,fmSet,tol);
  // center of gravity of filled values
  SumVal:=0;
  SumValX:=0;
  SumValY:=0;
  ri:=0;
  for i:=-rs to rs do begin
   p := b.Scanline[i+rs];
   for j:=-rs to rs do begin
     val:=p^.red;
     if val>254 then begin
       SumVal:=SumVal+1;
       SumValX:=SumValX+(i);
       SumValY:=SumValY+(j);
     end;
     inc(p);
   end;
  end;
  if SumVal>0 then begin
    Xg:=SumValX/SumVal;
    Yg:=SumValY/SumVal;
  end
  else begin
    xg:=0; yg:=0;
  end;
  // radius of interest
  for i:=-rs to rs do begin
   xx:=i-xg;
   p := b.Scanline[i+rs];
   for j:=-rs to rs do begin
     val:=p^.red;
     if val>254 then begin
       yy:=j-yg;
       ri:=max(ri,2+ceil(sqrt(xx*xx+yy*yy)));
     end;
     inc(p);
   end;
  end;
  if ri=0 then ri:=rs;
  if ri<3 then ri:=3;
  xc:=round(x+Xg);
  yc:=round(y+Yg);
  b.free;
end;

procedure Tf_starprofile.GetHFD(img:Timaw16; c,vmin: double; x,y,ri: integer; var bg: double; out xc,yc,hfd,star_fwhm,valmax: double);
var i,j: integer;
    SumVal,SumValX,SumValY,SumValR: double;
    Xg,Yg: double;
    r:double;
    val,bg_average,bg_standard_deviation, pixel_counter: double;

    function value_subpixel(x1,y1:double):double; {calculate image pixel value on subpixel level}
    // see: https://www.ap-i.net/mantis/file_download.php?file_id=817&type=bug 
    var
      x_trunc,y_trunc: integer;
      x_frac,y_frac : double;
    begin
      x_trunc:=trunc(x1);
      y_trunc:=trunc(y1);

      x_frac :=frac(x1);
      y_frac :=frac(y1);
      result:= Img[0,y_trunc ,x_trunc ] * (1-x_frac)*(1-y_frac);{pixel left top, 1}
      result:=result + Img[0,y_trunc ,x_trunc+1] * ( x_frac)*(1-y_frac);{pixel right top, 2}
      result:=result + Img[0,y_trunc+1,x_trunc ] * (1-x_frac)*( y_frac);{pixel left bottom, 3}
      result:=result + Img[0,y_trunc+1,x_trunc+1] * ( x_frac)*( y_frac);{pixel right bottom, 4}
    end;

begin
// x,y must be the star center, ri the radius of interest, bg the mean image value computed by FindStarPos
hfd:=-1;
star_fwhm:=-1;
bg_average:=0;
for i:=-ri+1 to ri do {calculate average background at the square boundaries of region of interest}
begin
  bg_average:=bg_average+Img[0,y+ri,x+i];{top line, left to right}
  bg_average:=bg_average+Img[0,y+i,x+ri];{right line, top to bottom}
  bg_average:=bg_average+Img[0,y-ri,x-i];{bottom line, right to left}
  bg_average:=bg_average+Img[0,y-i,x-ri];{right line, bottom to top}
end;
bg_average:=bg_average/(8*ri);

bg_standard_deviation:=0;
for i:=-ri+1 to ri do {calculate standard deviation background at the square boundaries of region of interest}
begin
  bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Img[0,y+ri,x+i]);{top line, left to right}
  bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Img[0,y+i,x+ri]);{right line, top to bottom}
  bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Img[0,y-ri,x-i]);{bottom line, right to left}
  bg_standard_deviation:=bg_standard_deviation+sqr(bg_average-Img[0,y-i,x-ri]);{left line, bottom to top}
end;
bg_standard_deviation:=sqrt(0.0001+bg_standard_deviation/(8*ri))/c;

bg:=vmin+bg_average/c;

// Get center of gravity whithin radius of interest
SumVal:=0;
SumValX:=0;
SumValY:=0;
valmax:=0;
for i:=-ri to ri do
 for j:=-ri to ri do begin
   val:=vmin+Img[0,y+j,x+i]/c-bg;
   if val<0 then val:=0;
   if val>valmax then valmax:=val;
   begin
     SumVal:=SumVal+val;
     SumValX:=SumValX+val*(i);
     SumValY:=SumValY+val*(j);
   end;
 end;
Xg:=SumValX/SumVal;
Yg:=SumValY/SumVal;
xc:=x+Xg;
yc:=y+Yg;

// Get HFD
SumVal:=0;
SumValR:=0;
pixel_counter:=0;
Fsnr:=valmax/sqrt(valmax+2*bg);
if Fsnr>3 then
begin
  for i:=-ri to ri do
    for j:=-ri to ri do
    begin
      Val:=vmin+value_subpixel(xc+i,yc+j)/c-bg;
      if val<0 then val:=0;{required?}
      r:=sqrt(i*i+j*j);
      if val>((3*bg_standard_deviation)) then {3 * sd should be signal }
      begin
        SumVal:=SumVal+Val;
        SumValR:=SumValR+Val*r;
        if val>=valmax*0.5 then pixel_counter:=pixel_counter+1;{how many pixels are above half maximum for FWHM}
      end;
    end;
    Sumval:=Sumval+0.00001;{prevent divide by zero}
    hfd:=2*SumValR/SumVal;
    hfd:=max(0.7,hfd); // minimum value for a star size of 1 pixel
    star_fwhm:=2*sqrt(pixel_counter/pi);{calculate from surface (by counting pixels above half max) the diameter equals FWHM }
  end;
end;

procedure Tf_starprofile.PlotProfile(img:Timaw16; c,vmin,bg: double; s:integer);
var i,j,i0,x1,x2,y1,y2:integer;
    xs,ys: double;
    txt:string;
begin
if (StarX<0)or(StarY<0)or(s<0) then exit;
// labels
LabelHFD.Caption:=FormatFloat(f1,Fhfd);
LabelImax.Caption:=FormatFloat(f0,FValMax);
if Ffwhm>0 then begin
  txt:=FormatFloat(f1,Ffwhm);
  if Ffwhmarcsec>0 then txt:=txt+'/'+FormatFloat(f1,Ffwhmarcsec)+'"';
  LabelFWHM.Caption:=txt;
end
else
  LabelFWHM.Caption:='-';
if curhist>maxhist then
  for i:=0 to maxhist-1 do begin
    histfwhm[i]:=histfwhm[i+1];
    histimax[i]:=histimax[i+1];
    curhist:=maxhist;
  end;
histfwhm[curhist]:=Fhfd;
histimax[curhist]:=FValMax;
if histfwhm[curhist] > maxfwhm then maxfwhm:=histfwhm[curhist];
if histimax[curhist] > maximax then maximax:=histimax[curhist];
// Star profile
profile.Picture.Bitmap.Width:=profile.Width;
profile.Picture.Bitmap.Height:=profile.Height;
with profile.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  if FValMax>0 then begin
    Canvas.Pen.Color:=clRed;
    xs:=Width/s;
    ys:=Height/(1.05*FValMax);
    j:=trunc(FStarY);
    i0:=trunc(FStarX)-(s div 2);
    x1:=0;
    y1:=Height-trunc((vmin+(img[0,j,i0]/c)-bg)*ys);
    for i:=0 to s-1 do begin
      x2:=trunc(i*xs);
      y2:=trunc((vmin+(img[0,j,i0+i]/c)-bg)*ys);
      y2:=Height-y2;
      Canvas.Line(x1,y1,x2,y2);
      x1:=x2;
      y1:=y2;
    end;
  end;
end;
// History graph
graph.Picture.Bitmap.Width:=graph.Width;
graph.Picture.Bitmap.Height:=graph.Height;
if FValMax>0 then with graph.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  xs:=Width/maxhist;
  ys:=Height/maxfwhm;
  Canvas.Pen.Color:=clRed;
  for i:=0 to curhist-1 do begin
    Canvas.Line( trunc(i*xs),
                 Height-trunc(histfwhm[i]*ys),
                 trunc((i+1)*xs),
                 Height-trunc(histfwhm[i+1]*ys));
  end;
  ys:=Height/maximax;
  Canvas.Pen.Color:=clLime;
  for i:=0 to curhist-1 do begin
    Canvas.Line( trunc(i*xs),
                 Height-trunc(histimax[i]*ys),
                 trunc((i+1)*xs),
                 Height-trunc(histimax[i+1]*ys));
  end;
end;
inc(curhist);
end;

procedure Tf_starprofile.PlotHistory;
var i:integer;
    xs,ys: double;
begin
if curhist>maxhist then
  for i:=0 to maxhist-1 do begin
    histfwhm[i]:=histfwhm[i+1];
    histimax[i]:=histimax[i+1];
    curhist:=maxhist;
  end;
histfwhm[curhist]:=Fhfd;
histimax[curhist]:=FValMax;
if histfwhm[curhist] > maxfwhm then maxfwhm:=histfwhm[curhist];
if histimax[curhist] > maximax then maximax:=histimax[curhist];
// History graph
graph.Picture.Bitmap.Width:=graph.Width;
graph.Picture.Bitmap.Height:=graph.Height;
if FValMax>0 then with graph.Picture.Bitmap do begin
  Canvas.Brush.Color:=clBlack;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.FillRect(0,0,Width,Height);
  xs:=Width/maxhist;
  ys:=Height/maxfwhm;
  Canvas.Pen.Color:=clRed;
  for i:=0 to curhist-1 do begin
    Canvas.Line( trunc(i*xs),
                 Height-trunc(histfwhm[i]*ys),
                 trunc((i+1)*xs),
                 Height-trunc(histfwhm[i+1]*ys));
  end;
  ys:=Height/maximax;
  Canvas.Pen.Color:=clLime;
  for i:=0 to curhist-1 do begin
    Canvas.Line( trunc(i*xs),
                 Height-trunc(histimax[i]*ys),
                 trunc((i+1)*xs),
                 Height-trunc(histimax[i+1]*ys));
  end;
end;
inc(curhist);
end;

procedure Tf_starprofile.ShowProfile(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer; focal:double=-1; pxsize:double=-1);
var bg: double;
  xg,yg: double;
  xm,ym,ri: integer;
begin
 if (x<0)or(y<0)or(s<0) then exit;

 if s>=(xmax div 2) then s:=xmax div 2;
 if s>=(ymax div 2) then s:=ymax div 2;

 FindStarPos(img,c,vmin,x,y,s,xmax,ymax,xm,ym,ri,FValMax,bg);
 if FValMax=0 then exit;

 GetHFD(img,c,vmin,xm,ym,ri,bg,xg,yg,Fhfd,Ffwhm,FValMax);
 if (Ffwhm>0)and(focal>0)and(pxsize>0) then begin
   Ffwhmarcsec:=Ffwhm*3600*rad2deg*arctan(pxsize/1000/focal);
 end
 else Ffwhmarcsec:=-1;

 // Plot result
 if (Fhfd>0) then begin
   FFindStar:=true;
   FStarX:=round(xg);
   FStarY:=round(yg);
   PlotProfile(img,c,vmin,bg,s);
   PlotHistory;
 end else begin
   FFindStar:=false;
   LabelHFD.Caption:='-';
   LabelFWHM.Caption:='-';
   LabelImax.Caption:='-';
   ClearGraph;
 end;
end;

procedure Tf_starprofile.Autofocus(img:Timaw16; c,vmin: double; x,y,s,xmax,ymax: integer);
var bg,star_fwhm: double;
  xg,yg: double;
  xm,ym,ri: integer;
begin
  if (x<0)or(y<0)or(s<0) then begin
    msg('Autofocus canceled because no star was selected.');
    FAutofocusResult:=false;
    ChkAutofocus.Checked:=false;
    exit;
  end;
  FindStarPos(img,c,vmin,x,y,s,xmax,ymax,xm,ym,ri,FValMax,bg);
  if FValMax=0 then begin
     msg('Autofocus canceled because no star was found.');
     FAutofocusResult:=false;
     ChkAutofocus.Checked:=false;
     exit;
  end;
  GetHFD(img,c,vmin,xm,ym,ri,bg,xg,yg,Fhfd,star_fwhm,FValMax);
  // process this measurement
  if (Fhfd<=0) then begin
    msg('Autofocus canceled because the HFD cannot be measured');
    FAutofocusResult:=false;
    ChkAutofocus.Checked:=false;
    exit;
  end;
  // sum of multiple exposures
  if (AutofocusNearNum>1)and
    ((Fhfd<(AutofocusNearHFD+1))or(AutofocusMode=afVcurve))and
    (not((AutofocusVcStep=vcsCheckL)or(AutofocusVcStep=vcsCheckR)))and
    (not FirstFrame)and
    (not terminated)
    then begin
    FFindStar:=true;
    FStarX:=round(xg);
    FStarY:=round(yg);
    PlotProfile(img,c,vmin,bg,s);
    FSumHfd:=FSumHfd+Fhfd;
    FMinSnr:=min(FMinSnr,Fsnr);
    FminPeak:=min(FminPeak,FValMax);
    inc(FnumHfd);
    msg('Autofocus mean frame '+inttostr(FnumHfd)+'/'+inttostr(AutofocusNearNum)+', hfd='+FormatFloat(f1,Fhfd)+' peak:'+FormatFloat(f1,FValMax)+' snr:'+FormatFloat(f1,Fsnr));
    if FnumHfd>=AutofocusNearNum then begin  // mean of measurement
      Fhfd:=FSumHfd/FnumHfd;
      FnumHfd:=0;
      FSumHfd:=0;
      Fsnr:=FMinSnr;
      FValMax:=FminPeak;
      FMinSnr:=99999;
      FminPeak:=999999;
    end
    else begin
      exit;
    end;
  end;
  // plot progress
  FFindStar:=true;
  FStarX:=round(xg);
  FStarY:=round(yg);
  Ffwhm:=-1;
  PlotProfile(img,c,vmin,bg,s);
  // check low snr
  if fsnr<AutofocusMinSNR then begin
    msg('Autofocus canceled because of low SNR, POS='+focuser.Position.Text+' HFD='+FormatFloat(f1,Fhfd)+' PEAK:'+FormatFloat(f1,FValMax)+' SNR:'+FormatFloat(f1,Fsnr));
    FAutofocusResult:=false;
    ChkAutofocus.Checked:=false;
    exit;
  end;
  // check focus result
  if terminated then begin
    if Fhfd<=AutofocusTolerance then FAutofocusResult:=true;
    msg('Autofocus finished, POS='+focuser.Position.Text+' HFD='+FormatFloat(f1,Fhfd)+' PEAK:'+FormatFloat(f1,FValMax)+' SNR:'+FormatFloat(f1,Fsnr));
    if not FAutofocusResult then begin
       msg('Autofocus final HFD is higher than '+FormatFloat(f1,AutofocusTolerance));
    end;
    ChkAutofocus.Checked:=false;
    exit;
  end;
  msg('Autofocus running, hfd='+FormatFloat(f1,Fhfd)+' peak:'+FormatFloat(f1,FValMax)+' snr:'+FormatFloat(f1,Fsnr));
  // do focus and continue
  case AutofocusMode of
    afVcurve   : doAutofocusVcurve;
    afMean     : doAutofocusMean;
    afIterative: doAutofocusIterative;
  end;
  FirstFrame:=false;
end;

procedure Tf_starprofile.doAutofocusVcurve;
var newpos,delta,meanhfd:double;
begin
 case AutofocusVcStep of
   vcsStartL: begin
              // move to curve start position to clear backlash
              newpos:=AutofocusVc[0,1];
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              if AutofocusVcTemp<>NullCoord then newpos:=newpos+focuser.TempOffset(AutofocusVcTemp,FocuserTemp);
              // check in range
              if newpos<FocuserPositionMin then newpos:=FocuserPositionMin;
              focuser.FocusPosition:=round(newpos);
              msg('Clear focuser backlash');
              FonAbsolutePosition(self);
              wait(1);
              // move back to start focus position
              newpos:=AutofocusVcpiL+(AutofocusStartHFD/AutofocusVcSlopeL);
              if newpos<AutofocusVc[0,1] then begin
                 msg('Start focus HFD is outside of current V curve, please decrease the start HFD value');
                 ChkAutofocus.Checked:=false;
                 exit;
              end;
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              if AutofocusVcTemp<>NullCoord then newpos:=newpos+focuser.TempOffset(AutofocusVcTemp,FocuserTemp);
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to start position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsNearL;
              wait(1);
             end;
   vcsStartR: begin
              // move to curve end position to clear backlash
              newpos:=AutofocusVc[AutofocusVcNum,1];
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              if AutofocusVcTemp<>NullCoord then newpos:=newpos+focuser.TempOffset(AutofocusVcTemp,FocuserTemp);
              // check in range
              if newpos>FocuserPositionMax then newpos:=FocuserPositionMax;
              focuser.FocusPosition:=round(newpos);
              msg('Clear focuser backlash');
              FonAbsolutePosition(self);
              wait(1);
              // move back to start focus position
              newpos:=AutofocusVcpiR+(AutofocusStartHFD/AutofocusVcSlopeR);
              if newpos>AutofocusVc[AutofocusVcNum,1] then begin
                 msg('Start focus HFD is outside of current V curve, please decrease the start HFD value');
                 ChkAutofocus.Checked:=false;
                 exit;
              end;
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              if AutofocusVcTemp<>NullCoord then newpos:=newpos+focuser.TempOffset(AutofocusVcTemp,FocuserTemp);
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to start position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsNearR;
              wait(1);
             end;
   vcsNearL: begin
              // compute near focus position
              delta:=(AutofocusStartHFD-Fhfd)/AutofocusVcSlopeL;
              newpos:=AutofocusVcpiL+(AutofocusNearHFD/AutofocusVcSlopeL)+delta;
              // move to near focus position
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to near focus '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsCheckL;
              AutofocusVcCheckNum:=0;
              AutofocusVcCheckHFDsum:=0;
              wait(1);
             end;
   vcsNearR: begin
              // compute near focus position
              delta:=(AutofocusStartHFD-Fhfd)/AutofocusVcSlopeR;
              newpos:=AutofocusVcpiR+(AutofocusNearHFD/AutofocusVcSlopeR)+delta;
              // move to near focus position
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to near focus '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsCheckR;
              AutofocusVcCheckNum:=0;
              AutofocusVcCheckHFDsum:=0;
              wait(1);
             end;
   vcsCheckL:begin
              inc(AutofocusVcCheckNum);
              AutofocusVcCheckHFDsum:=AutofocusVcCheckHFDsum+Fhfd;
              meanhfd:=AutofocusVcCheckHFDsum/AutofocusVcCheckNum;
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeL)+AutofocusVcPID/2;
              msg('Autofocus measurement '+IntToStr(AutofocusVcCheckNum)+' : HFD='+FormatFloat(f3,meanhfd)+' position='+IntToStr(round(newpos)));
              if AutofocusVcCheckNum>=AutofocusNearNum then begin
                AutofocusVcStep:=vcsFocusL;
                doAutofocusVcurve;
              end;
             end;
   vcsCheckR:begin
              inc(AutofocusVcCheckNum);
              AutofocusVcCheckHFDsum:=AutofocusVcCheckHFDsum+Fhfd;
              meanhfd:=AutofocusVcCheckHFDsum/AutofocusVcCheckNum;
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeR)-AutofocusVcPID/2;
              msg('Autofocus measurement'+IntToStr(AutofocusVcCheckNum)+' : HFD='+FormatFloat(f3,meanhfd)+' position='+IntToStr(round(newpos)));
              if AutofocusVcCheckNum>=AutofocusNearNum then begin
                AutofocusVcStep:=vcsFocusR;
                doAutofocusVcurve;
              end;
             end;
   vcsFocusL:begin
              // move to focus
              meanhfd:=AutofocusVcCheckHFDsum/AutofocusVcCheckNum;
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeL)+AutofocusVcPID/2;
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to focus position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              terminated:=true;
              wait(1);
             end;
   vcsFocusR:begin
              // move to focus
              meanhfd:=AutofocusVcCheckHFDsum/AutofocusVcCheckNum;
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeR)-AutofocusVcPID/2;
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to focus position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              terminated:=true;
              wait(1);
             end;
 end;
end;

procedure Tf_starprofile.doAutofocusMean;
var i,k,step: integer;
    VcpiL,VcpiR,al,bl,rl,ar,br,rr: double;
    p:array of TDouble2;
  procedure ResetPos;
  begin
    k:=AutofocusMeanNumPoint div 2;
    focuser.FocusSpeed:=AutofocusMeanMovement*(k+1);
    if AutofocusMoveDir=FocusDirIn then begin
      onFocusOUT(self);
      Wait(1);
      focuser.FocusSpeed:=AutofocusMeanMovement;
      onFocusIN(self)
    end
    else begin
      onFocusIN(self);
      Wait(1);
      focuser.FocusSpeed:=AutofocusMeanMovement;
      onFocusOUT(self);
    end;
    Wait(1);
  end;
begin
  case AutofocusMeanStep of
    afmStart: begin
              if not odd(AutofocusMeanNumPoint) then
                inc(AutofocusMeanNumPoint);
              if AutofocusMeanNumPoint<5 then AutofocusMeanNumPoint:=5;
              SetLength(ahfd,AutofocusMeanNumPoint);
              // set initial position
              k:=AutofocusMeanNumPoint div 2;
              focuser.FocusSpeed:=AutofocusMeanMovement*(k+1);
              if AutofocusMoveDir=FocusDirIn then begin
                onFocusOUT(self);
                Wait(1);
                focuser.FocusSpeed:=AutofocusMeanMovement;
                onFocusIN(self)
              end
              else begin
                onFocusIN(self);
                Wait(1);
                focuser.FocusSpeed:=AutofocusMeanMovement;
                onFocusOUT(self);
              end;
              Wait(1);
              afmpos:=-1;
              aminhfd:=9999;
              amaxhfd:=-1;
              AutofocusMeanStep:=afmMeasure;
              end;
    afmMeasure: begin
              // store hfd
              inc(afmpos);
              ahfd[afmpos]:=Fhfd;
              if Fhfd<aminhfd then begin
                aminhfd:=Fhfd;
                aminpos:=afmpos;
              end;
              if Fhfd>amaxhfd then begin
                amaxhfd:=Fhfd;
              end;
              // increment position
              if AutofocusMoveDir=FocusDirIn then
                onFocusIN(self)
              else
                onFocusOUT(self);
              wait(1);
              if afmpos=(AutofocusMeanNumPoint-1) then AutofocusMeanStep:=afmEnd;
              end;
    afmEnd: begin
              // check measure validity
              if (aminpos<2)or((AutofocusMeanNumPoint-aminpos-1)<2) then begin
                 msg('Not enough points in or out of focus position,');
                 msg('Try to start with a better position or increase the movement.');
                 ResetPos;
                 terminated:=true;
                 exit;
              end;
              if (amaxhfd<(3*aminhfd)) then begin
                 msg('Too small HFD difference,');
                 msg('Try to increase the number of point or the movement.');
                 ResetPos;
                 terminated:=true;
                 exit;
              end;
              // compute focus
              k:=aminpos;
              // left part
              SetLength(p,k);
              for i:=0 to k-1 do begin
                p[i,1]:=i+1;
                p[i,2]:=ahfd[i];
              end;
              LeastSquares(p,al,bl,rl);
              VcpiL:=-bl/al;
              // right part
              k:=AutofocusMeanNumPoint-k-1;
              SetLength(p,k);
              for i:=0 to k-1 do begin
                p[i,1]:=aminpos+2+i;
                p[i,2]:=ahfd[aminpos+1+i];
              end;
              LeastSquares(p,ar,br,rr);
              VcpiR:=-br/ar;
              // focus position
              step:=round(AutofocusMeanMovement*(VcpiL+VcpiR)/2);
              focuser.FocusSpeed:=step+AutofocusMeanMovement;
              if AutofocusMoveDir=FocusDirIn then begin
                onFocusOUT(self);
                wait(1);
                focuser.FocusSpeed:=AutofocusMeanMovement;
                onFocusIN(self);
              end
              else begin
                onFocusIN(self);
                wait(1);
                focuser.FocusSpeed:=AutofocusMeanMovement;
                onFocusOUT(self)
              end;
              wait(1);
              terminated:=true;
              end;
  end;
end;

procedure Tf_starprofile.doAutofocusIterative;
begin
  if Fhfd>FLastHfd then begin  // reverse direction
    if FfocuserSpeed=AutofocusMinSpeed  then begin
      // we reach focus, go back one step and terminate
      focuserdirection:=not focuserdirection;
      terminated:=true;
    end else begin
      if Fhfd<=AutofocusNearHFD then begin
         FfocuserSpeed:=max(FfocuserSpeed div 2,AutofocusMinSpeed);   // divide speed by 2
         focuser.FocusSpeed:=FfocuserSpeed; // set new speed
      end;
      focuserdirection:=not focuserdirection;
    end;
  end;
  if focuserdirection=FocusDirIn
     then begin
        msg('Autofocus focus in by '+inttostr(FfocuserSpeed));
        FonFocusIN(self);
      end
     else begin
       msg('Autofocus focus out by '+inttostr(FfocuserSpeed));
       FonFocusOUT(self);
     end;
  FLastHfd:=Fhfd;
end;

end.

