unit fu_starprofile;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses  u_modelisation, u_global,
  Graphics, Classes, SysUtils, FPImage, cu_fits,
  FileUtil, Forms, Controls, StdCtrls, ExtCtrls;

const maxhist=20;

type

  { Tf_starprofile }

  Tf_starprofile = class(TFrame)
    focus: TCheckBox;
    graph: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    profile: TImage;
    LabelFWHM: TLabel;
    LabelImax: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure focusChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure graphDblClick(Sender: TObject);
    procedure StaticText1StartDrag(Sender: TObject; var DragObject: TDragObject
      );
  private
    { private declarations }
    FFindStar: boolean;
    FStarX,FStarY: double;
    FFocusStart,FFocusStop: TNotifyEvent;
    emptybmp:Tbitmap;
    histfwhm, histimax: array[0..maxhist] of double;
    maxfwhm,maximax,curflux: double;
    curhist,curx,cury: integer;
    procedure ClearGraph;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ShowProfile(img:Timaw16; c,min: double; x,y,s,xmax,ymax: integer);
    property FindStar : boolean read FFindStar;
    property StarX: double read FStarX write FStarX;
    property StarY: double read FStarY write FStarY;
    property onFocusStart: TNotifyEvent read FFocusStart write FFocusStart;
    property onFocusStop: TNotifyEvent read FFocusStop write FFocusStop;
  end;

implementation

{$R *.lfm}

{ Tf_starprofile }

procedure Tf_starprofile.StaticText1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  DragObject := TDragObject.Create(self as TControl);
end;

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
 curx:=0;
 cury:=0;
 maxfwhm:=0;
 maximax:=0;
 LabelFWHM.Caption:='-';
 LabelImax.Caption:='-';
 ClearGraph;
end;

destructor  Tf_starprofile.Destroy;
begin
 emptybmp.Free;
 inherited Destroy;
end;

procedure Tf_starprofile.ShowProfile(img:Timaw16; c,min: double; x,y,s,xmax,ymax: integer);
var simg:TPiw16;
  imgdata: Tiw16;
  Valeur,ValMax: integer;
  i,j,xx,yy,x1,x2,y1,y2: integer;
  PSF:TPSF;
  xs,ys,vx:double;
begin
 ValMax:=0;
 for i:=0 to s-1 do
   for j:=0 to s-1 do
      if ((x+i)>0)and((x+i)<xmax)and((y+j)>0)and((y+j)<ymax) then begin
         Valeur:=Img[0,y+j,x+i];
         if Valeur>ValMax then begin
              ValMax:=Valeur;
              xx:=i;
              yy:=j;
         end;
      end;
 if ValMax=0 then exit;
 vx:=min+ValMax/c;
 x:=x+xx - s div 2;
 y:=y+yy - s div 2;
 setlength(imgdata,s,s);
 simg:=addr(imgdata);
 for i:=0 to s-1 do
   for j:=0 to s-1 do begin
     xx:=x+i;
     yy:=y+j;
     if (xx>0)and(xx<xmax)and(yy>0)and(y<ymax) then
        imgdata[i,j]:=trunc(min+img[0,yy,xx]/c)
     else imgdata[i,j]:=trunc(min);
   end;
 ModeliseEtoile(simg,s,TGauss,lowPrecision,LowSelect,0,PSF);
 if PSF.Flux>0 then begin
   FFindStar:=true;
   FStarX:=x+psf.X;
   FStarY:=y+psf.Y;
   LabelFWHM.Caption:=FormatFloat(f1,PSF.Sigma);
   LabelImax.Caption:=FormatFloat(f0,PSF.IntensiteMax);
   curflux:=PSF.Flux;
   curx:=x;
   cury:=y;
   if curhist>maxhist then
     for i:=0 to maxhist-1 do begin
       histfwhm[i]:=histfwhm[i+1];
       histimax[i]:=histimax[i+1];
       curhist:=maxhist;
     end;
   histfwhm[curhist]:=PSF.Sigma;
   histimax[curhist]:=PSF.IntensiteMax;
   if histfwhm[curhist] > maxfwhm then maxfwhm:=histfwhm[curhist];
   if histimax[curhist] > maximax then maximax:=histimax[curhist];
   profile.Picture.Bitmap.Width:=profile.Width;
   profile.Picture.Bitmap.Height:=profile.Height;
   with profile.Picture.Bitmap do begin
     Canvas.Brush.Color:=clBlack;
     Canvas.Pen.Color:=clBlack;
     Canvas.Pen.Mode:=pmCopy;
     Canvas.FillRect(0,0,Width,Height);
     if PSF.IntensiteMax>0 then begin
       Canvas.Pen.Color:=clRed;
       j:=trunc(PSF.Y);
       xs:=Width/s;
       ys:=Height/vx;
       x1:=0;
       y1:=Height-trunc(imgdata[0,j]*ys);
       for i:=1 to s-1 do begin
         x2:=trunc(i*xs);
         y2:=imgdata[i,j];
         y2:=Height-trunc(y2*ys);
         Canvas.Line(x1,y1,x2,y2);
         x1:=x2;
         y1:=y2;
       end;
     end;
   end;
   graph.Picture.Bitmap.Width:=graph.Width;
   graph.Picture.Bitmap.Height:=graph.Height;
   if PSF.IntensiteMax>0 then with graph.Picture.Bitmap do begin
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
   LabelFWHM.Caption:='-';
   LabelImax.Caption:='-';
   ClearGraph;
 end;
 setlength(imgdata,0,0);
end;

end.

