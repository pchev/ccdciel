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

uses  u_modelisation, u_global,
  Graphics, Classes, SysUtils, FPImage, cu_fits,
  FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

const maxhist=50;

type

  { Tf_starprofile }

  Tf_starprofile = class(TFrame)
    focus: TCheckBox;
    graph: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelFWHM: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    profile: TImage;
    LabelHFD: TLabel;
    LabelImax: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure focusChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure graphDblClick(Sender: TObject);
  private
    { private declarations }
    FFindStar: boolean;
    FStarX,FStarY: double;
    FFocusStart,FFocusStop: TNotifyEvent;
    emptybmp:Tbitmap;
    histfwhm, histimax: array[0..maxhist] of double;
    maxfwhm,maximax: double;
    curhist: integer;
    procedure ClearGraph;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ShowProfile(img:Timaw16; c,min: double; x,y,s,xmax,ymax: integer; focal:double=-1; pxsize:double=-1);
    property FindStar : boolean read FFindStar;
    property StarX: double read FStarX write FStarX;
    property StarY: double read FStarY write FStarY;
    property onFocusStart: TNotifyEvent read FFocusStart write FFocusStart;
    property onFocusStop: TNotifyEvent read FFocusStop write FFocusStop;
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

procedure Tf_starprofile.focusChange(Sender: TObject);
begin
 if focus.Checked then begin
    if Assigned(FFocusStart) then FFocusStart(self);
 end else begin
   if Assigned(FFocusStop) then FFocusStop(self);
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
 emptybmp:=Tbitmap.Create;
 emptybmp.SetSize(1,1);
 FFindStar:=false;
 curhist:=0;
 maxfwhm:=0;
 maximax:=0;
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

procedure Tf_starprofile.ShowProfile(img:Timaw16; c,min: double; x,y,s,xmax,ymax: integer; focal:double=-1; pxsize:double=-1);
var Val,ValMax: double;
  i,j,rs,i0,x1,x2,y1,y2,xm,ym: integer;
  bg,snr,r,Xg,Yg,fxg,fyg,SumVal,SumValX,SumValY,SumValR,xs,ys,hfd,fwhm,fwhmarcsec:double;
  txt: string;
  simg:TPiw16;
  imgdata: Tiw16;
  PSF:TPSF;
begin
 rs:= s div 2;
 if (x-s)<1 then x:=s+1;
 if (x+s)>(xmax-1) then x:=xmax-s-1;
 if (y-s)<1 then y:=s+1;
 if (y+s)>(ymax-1) then y:=ymax-s-1;
 // Center on brightest pixel
 ValMax:=0;
 for i:=-rs to rs do
   for j:=-rs to rs do begin
     Val:=min+Img[0,y+j,x+i]/c;
     if Val>ValMax then begin
          ValMax:=Val;
          xm:=i;
          ym:=j;
     end;
   end;
 if ValMax=0 then exit;
 x:=x+xm;
 y:=y+ym;
 // Get center of gravity
 SumVal:=0;
 SumValX:=0;
 SumValY:=0;
 for i:=-rs to rs do
   for j:=-rs to rs do begin
     Val:=min+Img[0,y+j,x+i]/c;
     SumVal:=SumVal+Val;
     SumValX:=SumValX+Val*i;
     SumValY:=SumValY+Val*j;
   end;
 Xg:=SumValX/SumVal;
 Yg:=SumValY/SumVal;
 x:=trunc(x+Xg);
 y:=trunc(y+Yg);
 fxg:=frac(x+Xg);
 fyg:=frac(y+Yg);
 // Get HFD
 hfd:=-1;
 SumVal:=0;
 SumValR:=0;
 bg:=min+((Img[0,y-rs,x-rs]+Img[0,y-rs,x+rs]+Img[0,y+rs,x-rs]+Img[0,y+rs,x+rs]) div 4)/c;
 valmax:=valmax-bg;
 snr:=valmax/bg;
 if snr>2 then begin
   for i:=-rs to rs do
     for j:=-rs to rs do begin
       Val:=min+Img[0,y+j,x+i]/c-bg;
       xs:=i-fxg;
       ys:=j-fyg;
       r:=sqrt(xs*xs+ys*ys);
       if val>(1.1*bg) then begin
         SumVal:=SumVal+Val;
         SumValR:=SumValR+Val*r;
       end;
     end;
   hfd:=2*SumValR/SumVal;
 end;
 // Get gaussian psf
 setlength(imgdata,s,s);
 simg:=addr(imgdata);
 for i:=0 to s-1 do
   for j:=0 to s-1 do begin
     x1:=x+i-rs;
     y1:=y+j-rs;
     if (x1>0)and(x1<xmax)and(y1>0)and(y1<ymax) then
        imgdata[i,j]:=trunc(min+img[0,y1,x1]/c)
     else imgdata[i,j]:=trunc(min);
   end;
 ModeliseEtoile(simg,s,TGauss,lowPrecision,LowSelect,0,PSF);
 if psf.Flux>0 then begin
   fwhm:=PSF.Sigma;
   if (focal>0)and(pxsize>0) then begin
     fwhmarcsec:=fwhm*3600*rad2deg*arctan(pxsize/1000/focal);
   end
   else fwhmarcsec:=-1;
 end
 else fwhm:=-1;
 // Plot result
 if (hfd>0) then begin
   FFindStar:=true;
   FStarX:=round(x+fxg);
   FStarY:=round(y+fyg);
   LabelHFD.Caption:=FormatFloat(f1,hfd);
   LabelImax.Caption:=FormatFloat(f0,ValMax);
   if fwhm>0 then begin
     txt:=FormatFloat(f1,fwhm);
     if fwhmarcsec>0 then txt:=txt+'/'+FormatFloat(f1,fwhmarcsec)+'"';
     LabelFWHM.Caption:=txt;
   end;
   if curhist>maxhist then
     for i:=0 to maxhist-1 do begin
       histfwhm[i]:=histfwhm[i+1];
       histimax[i]:=histimax[i+1];
       curhist:=maxhist;
     end;
   histfwhm[curhist]:=hfd;
   histimax[curhist]:=ValMax;
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
     if ValMax>0 then begin
       Canvas.Pen.Color:=clRed;
       xs:=Width/s;
       ys:=Height/(ValMax+bg);
       j:=trunc(FStarY);
       i0:=trunc(FStarX)-(s div 2);
       x1:=0;
       y1:=Height-trunc((min+(img[0,j,i0]-bg)/c)*ys);
       for i:=0 to s-1 do begin
         x2:=trunc(i*xs);
         y2:=trunc((min+(img[0,j,i0+i]-bg)/c)*ys);
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
   if ValMax>0 then with graph.Picture.Bitmap do begin
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
 end else begin
   FFindStar:=false;
   LabelHFD.Caption:='-';
   LabelFWHM.Caption:='-';
   LabelImax.Caption:='-';
   ClearGraph;
 end;
end;

end.

