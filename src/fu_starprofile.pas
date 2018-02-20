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

uses BGRABitmap, BGRABitmapTypes, u_global, u_utils, math, UScaleDPI,
  fu_preview, fu_focuser, Graphics, Classes, SysUtils, FPImage, cu_fits,
  FileUtil, TAGraph, TAFuncSeries, TASeries, TASources, Forms, Controls,
  StdCtrls, ExtCtrls, Buttons, LCLType;

const maxhist=50;

type

  { Tf_starprofile }

  Tf_starprofile = class(TFrame)
    FitSourceL: TListChartSource;
    FitSourceR: TListChartSource;
    graph: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelFWHM: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelGraph: TPanel;
    PanelFWHM: TPanel;
    Panel6: TPanel;
    profile: TImage;
    LabelHFD: TLabel;
    LabelImax: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ChkFocus: TSpeedButton;
    ChkAutofocus: TSpeedButton;
    BtnMeasureImage: TSpeedButton;
    PtSourceL: TListChartSource;
    PtSourceR: TListChartSource;
    StaticText1: TStaticText;
    TimerHideGraph: TTimer;
    VcChart: TChart;
    VcChartL: TFitSeries;
    VcChartPtL: TLineSeries;
    VcChartPtR: TLineSeries;
    VcChartR: TFitSeries;
    VcChartRegL: TLineSeries;
    VcChartRegR: TLineSeries;
    procedure ChkAutofocusChange(Sender: TObject);
    procedure ChkFocusChange(Sender: TObject);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure graphDblClick(Sender: TObject);
    procedure BtnMeasureImageClick(Sender: TObject);
    procedure TimerHideGraphTimer(Sender: TObject);
  private
    { private declarations }
    FFindStar: boolean;
    FStarX,FStarY,FValMax: double;
    FFocusStart,FFocusStop: TNotifyEvent;
    FAutoFocusStop,FAutoFocusStart: TNotifyEvent;
    FonFocusIN, FonFocusOUT, FonAbsolutePosition: TNotifyEvent;
    FonMeasureImage: TNotifyEvent;
    FonMsg: TNotifyMsg;
    Fpreview:Tf_preview;
    Ffocuser:Tf_focuser;
    emptybmp:Tbitmap;
    histfwhm, histimax: array[0..maxhist] of double;
    maxfwhm,maximax: double;
    Fhfd,Ffwhm,Ffwhmarcsec,FLastHfd,Fsnr,FMinSnr,FminPeak:double;
    FhfdList: array of double;
    curhist,FfocuserSpeed,FnumHfd,FPreFocusPos,FnumGraph: integer;
    focuserdirection,terminated,FirstFrame: boolean;
    FAutofocusResult: boolean;
    ahfd: array of double;
    aminhfd,amaxhfd:double;
    afmpos,aminpos:integer;
    procedure msg(txt:string);
    function  getRunning:boolean;
    procedure PlotProfile(f: TFits; bg: double; s:integer);
    procedure PlotHistory;
    procedure ClearGraph;
    procedure doAutofocusVcurve;
    procedure doAutofocusDynamic;
    procedure doAutofocusIterative;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ShowProfile(f: TFits; x,y,s: integer; focal:double=-1; pxsize:double=-1);
    procedure Autofocus(f: TFits; x,y,s: integer);
    procedure InitAutofocus;
    procedure ChkFocusDown(value:boolean);
    procedure ChkAutoFocusDown(value:boolean);
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
    property onMeasureImage: TNotifyEvent read FonMeasureImage write FonMeasureImage;
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

procedure Tf_starprofile.ChkFocusDown(value:boolean);
begin
  ChkFocus.Down:=value;
  ChkFocusChange(ChkFocus);
end;

procedure Tf_starprofile.ChkAutoFocusDown(value:boolean);
begin
 ChkAutofocus.Down:=value;
 ChkAutofocusChange(ChkAutofocus);
end;

procedure Tf_starprofile.ChkFocusChange(Sender: TObject);
begin
 if ChkAutofocus.Down then begin
   ChkFocus.Down:=false;
   exit;
 end;
 if ChkFocus.Down then begin
    if Assigned(FFocusStart) then FFocusStart(self);
 end else begin
   if Assigned(FFocusStop) then FFocusStop(self);
 end;
end;

procedure Tf_starprofile.ChkAutofocusChange(Sender: TObject);
begin
 if ChkFocus.Down then begin
    ChkAutofocus.Down:=false;
    exit;
 end;
 if ChkAutofocus.Down then begin
    if Assigned(FAutoFocusStart) then FAutoFocusStart(self);
 end else begin
   terminated:=true;
   if Assigned(FAutoFocusStop) then FAutoFocusStop(self);
 end;
end;

function  Tf_starprofile.getRunning:boolean;
begin
 result:=ChkAutofocus.Down;
end;

procedure Tf_starprofile.InitAutofocus;
var i: integer;
begin
 FnumHfd:=0;
 FnumGraph:=0;
 SetLength(FhfdList,AutofocusNearNum);
 FMinSnr:=99999;
 FminPeak:=9999999;
 terminated:=false;
 FirstFrame:=true;
 FAutofocusResult:=false;
 FPreFocusPos:=focuser.FocusPosition;
 focuserdirection:=AutofocusMoveDir;
 PtSourceL.Clear;
 FitSourceL.Clear;
 PanelFWHM.Visible:=false;
 PanelGraph.Visible:=true;
 case AutofocusMode of
   afVcurve   : begin
                // plot curve in graph
                if AutofocusMode=afVcurve then begin
                  for i:=0 to AutofocusVcNum do begin
                    FitSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2]);
                  end;
                end;
                msg('Autofocus start Vcurve');
                if focuserdirection=FocusDirOut then
                   AutofocusVcStep:=vcsStartL
                 else
                   AutofocusVcStep:=vcsStartR;
                end;
   afDynamic  : begin
                msg('Autofocus start Dynamic curve');
                AutofocusDynamicStep:=afdStart;
                end;
   afIterative: begin
                msg('Autofocus start Iterative focus');
                FfocuserSpeed:=AutofocusMaxSpeed;
                focuser.FocusSpeed:=FfocuserSpeed;
                end;
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

procedure Tf_starprofile.BtnMeasureImageClick(Sender: TObject);
begin
  if assigned(FonMeasureImage) then FonMeasureImage(self);
end;

procedure Tf_starprofile.TimerHideGraphTimer(Sender: TObject);
begin
 TimerHideGraph.Enabled:=false;
 PanelFWHM.Visible:=true;
 PanelGraph.Visible:=false;
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

procedure Tf_starprofile.PlotProfile(f: TFits; bg: double; s:integer);
var i,j,i0,x1,x2,y1,y2,rs:integer;
    xs,ys: double;
    txt:string;
begin
if (FStarX<0)or(FStarY<0)or(s<0) then exit;
try
// labels
LabelHFD.Caption:=FormatFloat(f1,Fhfd);
if FValMax>1 then
   LabelImax.Caption:=FormatFloat(f0,FValMax)
else
   LabelImax.Caption:=FormatFloat(f3,FValMax);
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
    rs:=s div 2;
    if (FStarX-rs)<0 then rs:=round(FStarX);
    if (FStarX+rs)>(img_Width-1) then rs:=img_Width-1-round(FStarX);
    if (FStarY-rs)<0 then rs:=round(FStarY);
    if (FStarY+rs)>(img_Height-1) then rs:=img_Height-1-round(FStarY);
    if rs<=0 then exit;
    s:=2*rs;
    Canvas.Pen.Color:=clRed;
    xs:=Width/s;
    ys:=Height/(1.05*FValMax);
    j:=trunc(FStarY);
    i0:=trunc(FStarX)-(s div 2);
    x1:=0;
    y1:=Height-trunc((f.imageMin+(f.image[0,j,i0]/f.imageC)-bg)*ys);
    for i:=0 to s-1 do begin
      x2:=trunc(i*xs);
      y2:=trunc((f.imageMin+(f.image[0,j,i0+i]/f.imageC)-bg)*ys);
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
except
  on E: Exception do begin
    msg('PlotProfile :'+ E.Message);
  end;
end;
end;

procedure Tf_starprofile.PlotHistory;
var i:integer;
    xs,ys: double;
begin
try
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
except
  on E: Exception do begin
    msg('PlotHistory :'+ E.Message);
  end;
end;
end;

procedure Tf_starprofile.ShowProfile(f: TFits; x,y,s: integer; focal:double=-1; pxsize:double=-1);
var bg,bgdev: double;
  xg,yg: double;
  xm,ym,ri: integer;
begin
 if (x<0)or(y<0)or(s<0) then exit;

 try

 if s>=(f.HeaderInfo.naxis1 div 2) then s:=f.HeaderInfo.naxis1 div 2;
 if s>=(f.HeaderInfo.naxis2 div 2) then s:=f.HeaderInfo.naxis2 div 2;

 f.FindStarPos(x,y,s,xm,ym,ri,FValMax,bg,bgdev);
 if FValMax=0 then exit;

 f.GetHFD(xm,ym,ri,bg,bgdev,xg,yg,Fhfd,Ffwhm,FValMax,Fsnr);
 if (Ffwhm>0)and(focal>0)and(pxsize>0) then begin
   Ffwhmarcsec:=Ffwhm*3600*rad2deg*arctan(pxsize/1000/focal);
 end
 else Ffwhmarcsec:=-1;

 // Plot result
 if (Fhfd>0) then begin
   FFindStar:=true;
   FStarX:=round(xg);
   FStarY:=round(yg);
   PlotProfile(f,bg,s);
   PlotHistory;
 end else begin
   FFindStar:=false;
   LabelHFD.Caption:='-';
   LabelFWHM.Caption:='-';
   LabelImax.Caption:='-';
   ClearGraph;
 end;
 except
   on E: Exception do begin
     msg('ShowProfile :'+ E.Message);
   end;
 end;
end;

procedure Tf_starprofile.Autofocus(f: TFits; x,y,s: integer);
var bg,bgdev,star_fwhm,focuspos,tempcomp: double;
  xg,yg: double;
  xm,ym,ri: integer;
begin
  if (x<0)or(y<0)or(s<0) then begin
    msg('Autofocus canceled because no star was selected.');
    FAutofocusResult:=false;
    ChkAutofocusDown(false);
    exit;
  end;
  f.FindStarPos(x,y,s,xm,ym,ri,FValMax,bg,bgdev);
  if FValMax=0 then begin
     msg('Autofocus canceled because no star was found.');
     FAutofocusResult:=false;
     ChkAutofocusDown(false);
     exit;
  end;
  f.GetHFD(xm,ym,ri,bg,bgdev,xg,yg,Fhfd,star_fwhm,FValMax,Fsnr);
  // process this measurement
  if (Fhfd<=0) then begin
    msg('Autofocus canceled because the HFD cannot be measured');
    FAutofocusResult:=false;
    ChkAutofocusDown(false);
    exit;
  end;
  // sum of multiple exposures
  if (AutofocusNearNum>1)and
    ((Fhfd<(AutofocusNearHFD+1))or(AutofocusMode=afVcurve)or(AutofocusMode=afDynamic))and
    (not((AutofocusVcStep=vcsCheckL)or(AutofocusVcStep=vcsCheckR)))and
    (not FirstFrame)and
    (not terminated)
    then begin
    FFindStar:=true;
    FStarX:=round(xg);
    FStarY:=round(yg);
    PlotProfile(f,bg,s);
    FhfdList[FnumHfd]:=Fhfd;
    FMinSnr:=min(FMinSnr,Fsnr);
    FminPeak:=min(FminPeak,FValMax);
    inc(FnumHfd);
    msg('Autofocus mean frame '+inttostr(FnumHfd)+'/'+inttostr(AutofocusNearNum)+', hfd='+FormatFloat(f1,Fhfd)+' peak:'+FormatFloat(f1,FValMax)+' snr:'+FormatFloat(f1,Fsnr));
    if FnumHfd>=AutofocusNearNum then begin  // mean of measurement
      Fhfd:=SMedian(FhfdList);
      FnumHfd:=0;
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
  PlotProfile(f,bg,s);
  if not FirstFrame then begin
    inc(FnumGraph);
    if AutofocusMode=afVcurve then begin
      PtSourceL.Add(focuser.FocusPosition,Fhfd,'',clGreen);
    end
    else
    if (not terminated) then begin
      PtSourceL.Add(FnumGraph,Fhfd,'',clGreen);
      FitSourceL.Add(FnumGraph,Fhfd);
    end;
  end;
  // check low snr
  if fsnr<AutofocusMinSNR then begin
    msg('Autofocus canceled because of low SNR, POS='+focuser.Position.Text+' HFD='+FormatFloat(f1,Fhfd)+' PEAK:'+FormatFloat(f1,FValMax)+' SNR:'+FormatFloat(f1,Fsnr));
    FAutofocusResult:=false;
    ChkAutofocusDown(false);
    exit;
  end;
  // check focus result
  if terminated then begin
    if Fhfd<=AutofocusTolerance then FAutofocusResult:=true;
    msg('Autofocus finished, POS='+focuser.Position.Text+' HFD='+FormatFloat(f1,Fhfd)+' PEAK:'+FormatFloat(f1,FValMax)+' SNR:'+FormatFloat(f1,Fsnr)+' TEMP:'+FormatFloat(f1,FocuserTemp));
    if FAutofocusResult then begin
      // adjust slippage offset with current result
      if AutofocusSlippageCorrection then begin
        if AutofocusVcTemp<>NullCoord then
           tempcomp:=focuser.TempOffset(AutofocusVcTemp,FocuserTemp)
        else
           tempcomp:=0;
        focuspos:=focuser.FocusPosition;
        focuspos:=focuspos-(CurrentFilterOffset-AutofocusVcFilterOffset);
        focuspos:=focuspos-tempcomp;
        AutofocusSlippageOffset:=round(focuspos-(AutofocusVcpiL+AutofocusVcpiR)/2);
        config.SetValue('/StarAnalysis/AutofocusSlippageOffset',AutofocusSlippageOffset);
        msg('Focuser slippage offset set to '+inttostr(AutofocusSlippageOffset));
      end;
    end
    else begin
       msg('Autofocus final HFD is higher than '+FormatFloat(f1,AutofocusTolerance));
    end;
    ChkAutofocusDown(false);
    exit;
  end;
  msg('Autofocus running, hfd='+FormatFloat(f1,Fhfd)+' peak:'+FormatFloat(f1,FValMax)+' snr:'+FormatFloat(f1,Fsnr));
  // do focus and continue
  case AutofocusMode of
    afVcurve   : doAutofocusVcurve;
    afDynamic  : doAutofocusDynamic;
    afIterative: doAutofocusIterative;
  end;
  FirstFrame:=false;
end;

procedure Tf_starprofile.doAutofocusVcurve;
var newpos,delta,meanhfd,tempcomp:double;
begin
 if not ChkAutofocus.Down then exit;
 case AutofocusVcStep of
   vcsStartL: begin
              // temperature compensation
              if AutofocusVcTemp<>NullCoord then
                 tempcomp:=focuser.TempOffset(AutofocusVcTemp,FocuserTemp)
              else
                 tempcomp:=0;
              // move to curve start position to clear backlash
              newpos:=AutofocusVc[0,1];
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
              // check in range
              if newpos<FocuserPositionMin then newpos:=FocuserPositionMin;
              focuser.FocusPosition:=round(newpos);
              msg('Clear focuser backlash');
              FonAbsolutePosition(self);
              wait(1);
              if not ChkAutofocus.Down then exit;
              // move back to start focus position
              newpos:=AutofocusVcpiL+(AutofocusStartHFD/AutofocusVcSlopeL);
              if newpos<AutofocusVc[0,1] then begin
                 msg('Start focus HFD is outside of current V curve, please decrease the start HFD value');
                 ChkAutofocusDown(false);
                 exit;
              end;
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to start position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsNearL;
              wait(1);
             end;
   vcsStartR: begin
              // temperature compensation
              if AutofocusVcTemp<>NullCoord then
                 tempcomp:=focuser.TempOffset(AutofocusVcTemp,FocuserTemp)
              else
                 tempcomp:=0;
              // move to curve end position to clear backlash
              newpos:=AutofocusVc[AutofocusVcNum,1];
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
              // check in range
              if newpos>FocuserPositionMax then newpos:=FocuserPositionMax;
              focuser.FocusPosition:=round(newpos);
              msg('Clear focuser backlash');
              FonAbsolutePosition(self);
              wait(1);
              if not ChkAutofocus.Down then exit;
              // move back to start focus position
              newpos:=AutofocusVcpiR+(AutofocusStartHFD/AutofocusVcSlopeR);
              if newpos>AutofocusVc[AutofocusVcNum,1] then begin
                 msg('Start focus HFD is outside of current V curve, please decrease the start HFD value');
                 ChkAutofocusDown(false);
                 exit;
              end;
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
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
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
              // move to near focus position
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to near focus '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsCheckL;
              AutofocusVcCheckNum:=0;
              SetLength(AutofocusVcCheckHFDlist,0);
              wait(1);
             end;
   vcsNearR: begin
              // compute near focus position
              delta:=(AutofocusStartHFD-Fhfd)/AutofocusVcSlopeR;
              newpos:=AutofocusVcpiR+(AutofocusNearHFD/AutofocusVcSlopeR)+delta;
              // correct for filter offset
              newpos:=newpos+(CurrentFilterOffset-AutofocusVcFilterOffset);
              // correct for temperature
              newpos:=newpos+tempcomp;
              // correct for slippage
              if AutofocusSlippageCorrection then newpos:=newpos+AutofocusSlippageOffset;
              // move to near focus position
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to near focus '+focuser.Position.Text);
              FonAbsolutePosition(self);
              AutofocusVcStep:=vcsCheckR;
              AutofocusVcCheckNum:=0;
              SetLength(AutofocusVcCheckHFDlist,0);
              wait(1);
             end;
   vcsCheckL:begin
              inc(AutofocusVcCheckNum);
              dec(FnumGraph);
              SetLength(AutofocusVcCheckHFDlist,AutofocusVcCheckNum);
              AutofocusVcCheckHFDlist[AutofocusVcCheckNum-1]:=Fhfd;
              meanhfd:=SMedian(AutofocusVcCheckHFDlist);
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeL)+AutofocusVcPID/2;
              msg('Autofocus measurement '+IntToStr(AutofocusVcCheckNum)+' : HFD='+FormatFloat(f3,meanhfd)+' position='+IntToStr(round(newpos)));
              if AutofocusVcCheckNum>=AutofocusNearNum then begin
                AutofocusVcStep:=vcsFocusL;
                doAutofocusVcurve;
              end;
             end;
   vcsCheckR:begin
              inc(AutofocusVcCheckNum);
              dec(FnumGraph);
              SetLength(AutofocusVcCheckHFDlist,AutofocusVcCheckNum);
              AutofocusVcCheckHFDlist[AutofocusVcCheckNum-1]:=Fhfd;
              meanhfd:=SMedian(AutofocusVcCheckHFDlist);
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeR)-AutofocusVcPID/2;
              msg('Autofocus measurement'+IntToStr(AutofocusVcCheckNum)+' : HFD='+FormatFloat(f3,meanhfd)+' position='+IntToStr(round(newpos)));
              if AutofocusVcCheckNum>=AutofocusNearNum then begin
                AutofocusVcStep:=vcsFocusR;
                doAutofocusVcurve;
              end;
             end;
   vcsFocusL:begin
              // move to focus
              inc(FnumGraph);
              meanhfd:=SMedian(AutofocusVcCheckHFDlist);
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeL)+AutofocusVcPID/2;
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to focus position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              terminated:=true;
              wait(1);
             end;
   vcsFocusR:begin
              // move to focus
              inc(FnumGraph);
              meanhfd:=SMedian(AutofocusVcCheckHFDlist);
              newpos:=focuser.FocusPosition-(meanhfd/AutofocusVcSlopeR)-AutofocusVcPID/2;
              focuser.FocusPosition:=round(newpos);
              msg('Autofocus move to focus position '+focuser.Position.Text);
              FonAbsolutePosition(self);
              terminated:=true;
              wait(1);
             end;
 end;
end;

procedure Tf_starprofile.doAutofocusDynamic;
var i,k,step: integer;
    VcpiL,VcpiR,al,bl,rl,r2,ar,br,rr: double;
    p:array of TDouble2;
  procedure ResetPos;
  begin
    k:=AutofocusDynamicNumPoint div 2;
    focuser.FocusSpeed:=AutofocusDynamicMovement*(k);
    if AutofocusMoveDir=FocusDirIn then begin
      onFocusOUT(self);
      Wait(1);
      focuser.FocusSpeed:=AutofocusDynamicMovement;
      onFocusIN(self)
    end
    else begin
      onFocusIN(self);
      Wait(1);
      focuser.FocusSpeed:=AutofocusDynamicMovement;
      onFocusOUT(self);
    end;
    Wait(1);
  end;
begin
  case AutofocusDynamicStep of
    afdStart: begin
              if not odd(AutofocusDynamicNumPoint) then
                inc(AutofocusDynamicNumPoint);
              if AutofocusDynamicNumPoint<5 then AutofocusDynamicNumPoint:=5;
              SetLength(ahfd,AutofocusDynamicNumPoint+1);
              // set initial position
              k:=AutofocusDynamicNumPoint div 2;
              if AutofocusMoveDir=FocusDirIn then begin
                focuser.FocusSpeed:=AutofocusDynamicMovement*(k+1);
                onFocusOUT(self);
                Wait(1);
                focuser.FocusSpeed:=AutofocusDynamicMovement;
                onFocusIN(self)
              end
              else begin
                focuser.FocusSpeed:=AutofocusDynamicMovement*(k+1);
                onFocusIN(self);
                Wait(1);
                focuser.FocusSpeed:=AutofocusDynamicMovement;
                onFocusOUT(self);
              end;
              Wait(1);
              afmpos:=0;
              aminhfd:=9999;
              amaxhfd:=-1;
              AutofocusDynamicStep:=afdMeasure;
              end;
    afdMeasure: begin
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
              if afmpos=(AutofocusDynamicNumPoint) then begin
                // last point, process measurements
                AutofocusDynamicStep:=afdEnd;
                doAutofocusDynamic;
                exit
              end;
              // increment position
              if AutofocusMoveDir=FocusDirIn then
                onFocusIN(self)
              else
                onFocusOUT(self);
              wait(1);
              end;
    afdEnd: begin
              // check measure validity
              if (aminpos<2)or((AutofocusDynamicNumPoint-aminpos)<2) then begin
                 msg('Not enough points in or out of focus position,');
                 msg('Try to start with a better position or increase the movement.');
                 ResetPos;
                 terminated:=true;
                 exit;
              end;
              if (amaxhfd<(2*aminhfd)) then begin
                 msg('Too small HFD difference,');
                 msg('Try to increase the number of point or the movement.');
                 ResetPos;
                 terminated:=true;
                 exit;
              end;
              // compute focus
              k:=aminpos-1;
              // left part
              SetLength(p,k);
              for i:=1 to k do begin
                p[i-1,1]:=i;
                p[i-1,2]:=ahfd[i];
              end;
              LeastSquares(p,al,bl,rl);
              VcpiL:=-bl/al;
              // right part
              k:=AutofocusDynamicNumPoint-aminpos;
              SetLength(p,k);
              for i:=1 to k do begin
                p[i-1,1]:=aminpos+i;
                p[i-1,2]:=ahfd[aminpos+i];
              end;
              LeastSquares(p,ar,br,rr);
              VcpiR:=-br/ar;
              // focus quality, mean of both side
              r2:=(rr*rr+rl*rl)/2;
              msg('Focus quality = '+FormatFloat(f3,r2));
              // focus position
              step:=round(AutofocusDynamicMovement*(AutofocusDynamicNumPoint-(VcpiL+VcpiR)/2));
              focuser.FocusSpeed:=step+AutofocusDynamicMovement;
              if AutofocusMoveDir=FocusDirIn then begin
                onFocusOUT(self);
                wait(1);
                focuser.FocusSpeed:=AutofocusDynamicMovement;
                onFocusIN(self);
              end
              else begin
                onFocusIN(self);
                wait(1);
                focuser.FocusSpeed:=AutofocusDynamicMovement;
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

