unit cu_autoguider_internal;

{$mode objfpc}{$H+}

{
Copyright (C) 2022 Patrick Chevalley & Han Kleijn

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

uses cu_autoguider, u_global, u_utils, math, fu_internalguider, indiapi, simpletimer, CTimer,
  u_translation, Graphics, Forms, Classes, SysUtils, ExtCtrls;

type
  star_position=record x1,y1,x2,y2,flux: double; end;//for internal guider
  star_position_array= array of star_position;//for internal guider
  TSpectroTarget = record
     RA,DEC: double;
     valid: boolean;
  end;


  T_autoguider_internal = class(T_autoguider)
  private
    InternalguiderInitialize,InternalCalibrationInitialize,GuideLogFileOpen, solar_tracking  : boolean;
    pulseRA,pulseDEC,GuideFrameCount, InternalguiderCalibrationDirection,InternalguiderCalibrationStep,
    CalibrationDuration,Calflip,CalCount,Calnrtest,CalDecBacklash,frame_size,Binning,BacklashStep: integer;
    driftX,driftY,driftRA,driftDec,moveRA,moveDEC, Guidethecos,old_moveRA,old_moveDEC,  paEast, paNorth,
    pulsegainEast,pulsegainWest,pulsegainNorth,pulsegainSouth,Calthecos,Orthogonality,Caltheangle,CaldriftOld, ditherX,ditherY,
    GuideStartTime,LogSNR,LogFlux,mean_hfd,CalNorthDec1,CalNorthDec2,CalEastRa1,CalEastRa2,CurrentHFD,MinimumDrift : double;
    LastDecSign: double;
    SameDecSignCount,LastBacklashDuration: integer;
    LastBacklash,FirstDecDirectionChange: boolean;
    north, south    : integer;
    xy_trend : xy_guiderlist;{fu_internalguider}
    xy_array,xy_array_old : star_position_array;//internal guider for measure drift
    GuideLog: TextFile;
    FPaused, FSettling, FSettlingInRange,PulseGuiding,OffsetFromTarget: boolean;
    InternalguiderCalibratingMeridianFlip : boolean;
    FSettleStartTime, FSettleTime: double;
    TimerWaitPulseGuiding: TSimpleTimer;
    FRecoveringCamera: boolean;
    FRecoveringCameraCount: integer;
    FSpectroTarget: TSpectroTarget;
    function  measure_drift(var initialize: boolean; out drX,drY :double) : integer;
    function angular_distance(a1,a2:double):double;
    Procedure StartGuideExposure;
    procedure InternalguiderStartAsync(Data: PtrInt);
    function  WaitPulseGuiding(pulse:longint): boolean;
    procedure SetStatus(aStatus: string ; aState: TAutoguiderState);
    Procedure InitLog;
    Procedure WriteLog( buf : string);
    Procedure CloseLog;
    procedure TimerWaitPulseGuidingTimer(const Sender: TObject);
    function SelectSpectroTarget: boolean;
  protected
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure Terminate;
    procedure StarLostTimerTimer(Sender: TObject); override;
    procedure StartSettle;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean; settle:boolean=true); override;
    procedure Dither(pixel:double; raonly:boolean; waittime:double); override;
    function GetLockPosition(out x,y:double):boolean; override;
    procedure SetLockPosition(x,y: double); override;
    function SpectroSetTarget(TargetRa,TargetDec: double):boolean; override;
    procedure InternalguiderLoop;
    procedure InternalguiderStart;
    procedure InternalguiderStop;
    procedure InternalguiderRecoverCamera;
    procedure InternalguiderCalibrate;
    procedure InternalguiderCalibrateBacklash;
    procedure InternalAutoguiding;
    procedure InternalCalibration;
    procedure BacklashCalibration;
    procedure InternalguiderCaptureDark;
    procedure ParameterChange(txt: string);
    Procedure StartGuideExposureAsync(Data: PtrInt);
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
    procedure ShowImgInfo;
    procedure NewImageReceived;
  end;

implementation

const
   nrpointsTrend=50; //number of trend points plotted
   maxreverse=3; // wait 3 declination pulse in same direction after a reversal
var
  oldtickcount: qword=0;


procedure mad_median(list: array of double;leng :integer;out mad,median :double);{calculate mad and median without modifying the data}
var  {idea from https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/}
  i        : integer;
  list2: array of double;
begin
  setlength(list2,leng);
  for i:=0 to leng-1 do list2[i]:=list[i];{copy magn offset data}
  median:=Smedian(list2,leng);
  for i:=0 to leng-1 do list2[i]:=abs(list[i] - median);{fill list2 with offsets}
  mad:=Smedian(list2,leng); //median absolute deviation (MAD)
  list2:=nil;
end;


procedure get_best_mean(list: array of double; leng : integer; out mean : double);{Remove outliers from polulation using MAD. }
var  {idea from https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/}
  i,count         : integer;
  median, mad     : double;

begin
 if leng=1 then begin mean:=list[0];exit end
 else
 if leng=2 then begin mean:=(list[0]+list[1])/2;exit end;
 mad_median(list,leng,mad,median);{calculate mad and median without modifying the data}
 count:=0;
 mean:=0;
 for i:=0 to leng-1 do
   if abs(list[i]-median)<1.50*1.4826*mad then {offset less the 1.5*sigma.}
   begin
     mean:=mean+list[i];{Calculate mean. This gives a little less noise then calculating median again. Note weighted mean gives poorer result and is not applied.}
     inc(count);
   end;
 if count>0 then  mean:=mean/count;  {mean without using outliers}
end;


procedure rotate2(rot,x,y :double;out  x2,y2:double);{rotate a vector point CCW}
var
  sin_rot, cos_rot :double;
begin
  sincos(rot, sin_rot, cos_rot);
  x2:=x * cos_rot - y*sin_rot;
  y2:=x * sin_rot + y*cos_rot;
end;

Constructor T_autoguider_internal.Create ;
begin
  inherited Create;
  FAutoguiderType:=agINTERNAL;
  FStatus:=rsInternal;
  FState:=GUIDER_IDLE;
  FRunning:=true;
  FPaused:=false;
  FSettling:=false;
  PulseGuiding:=false;
  OffsetFromTarget:=false;
  FSettlePix:=1;
  FSettleTmin:=5;
  FSettleTmax:=30;
  InitLog;
  StopInternalguider:=false;
  InternalguiderRunning:=false;
  InternalguiderGuiding:=false;
  InternalguiderCalibrating:=false;
  InternalguiderCalibratingBacklash:=false;
  InternalguiderCapturingDark:=false;
  InternalguiderCalibratingMeridianFlip:=false;
  frame_size:=999999;
  TimerWaitPulseGuiding:=TSimpleTimer.Create(nil);
  TimerWaitPulseGuiding.Enabled:=false;
  TimerWaitPulseGuiding.OnTimer:=@TimerWaitPulseGuidingTimer;
  FRecoveringCamera:=false;
  FRecoveringCameraCount:=0;
  FSpectroTarget.RA:=NullCoord;
  FSpectroTarget.DEC:=NullCoord;
  FSpectroTarget.valid:=false;
end;

Destructor T_autoguider_internal.Destroy;
begin
  CloseLog;
  TimerWaitPulseGuiding.Free;
  inherited Destroy;
end;

Procedure T_autoguider_internal.Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False);
begin
  // this not use the thread, connect() is called only to destroy
  start;
end;

procedure T_autoguider_internal.Disconnect;
begin
  // Not used
end;

procedure T_autoguider_internal.Execute;
begin
  // this not use the thread, just exit to destroy
end;

procedure T_autoguider_internal.Terminate;
begin
  // destroy
  Free;
end;

Procedure T_autoguider_internal.ProcessEvent(txt:string);
begin
  // event from thread, not used
end;

procedure T_autoguider_internal.ConnectGear;
begin
  // Not used
end;

procedure T_autoguider_internal.Shutdown;
begin
  // Not used
end;

procedure T_autoguider_internal.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
  FSettlePix:=pixel;
  FSettleTmin:=mintime;
  FSettleTmax:=maxtime;
end;

function T_autoguider_internal.WaitBusy(maxwait:integer=5):boolean;
var endt: TDateTime;
begin
result:=false;
try
  endt:=now+maxwait/secperday;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if terminated then break;
    if CancelAutofocus then break;
    if FStopGuiding or StopInternalguider then break;
    if FState<>GUIDER_BUSY then break;
  end;
  result:=(FState<>GUIDER_BUSY) or FStopGuiding;
except
end;
end;

function T_autoguider_internal.WaitGuiding(maxwait:integer=5):boolean;
var endt: TDateTime;
    n: integer;
begin
result:=false;
try
  endt:=now+maxwait/secperday;
  n:=0;
  while now<endt do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if terminated then break;
    if CancelAutofocus then break;
    if FStopGuiding or StopInternalguider then break;
    if FState=GUIDER_GUIDING then break;
    inc(n);
    if ((n mod 150)=0) and assigned(FonShowMessage) then
        FonShowMessage('Waiting for autoguider to start...');
  end;
  result:=(FState=GUIDER_GUIDING);
except
end;
end;

function T_autoguider_internal.WaitDithering(maxwait:integer=5):boolean;
begin
  { #todo : dither }
  result:=true;
end;

procedure T_autoguider_internal.Calibrate;
begin
  Guide(true,true);
end;

procedure T_autoguider_internal.Guide(onoff:boolean; recalibrate:boolean=false);
begin
  if onoff then begin
    if recalibrate then InternalguiderCalibrate;
    InternalguiderStart;
  end
  else begin
     InternalguiderStop;
  end;
end;

function T_autoguider_internal.GetLockPosition(out x,y:double):boolean;
begin
  x:=finternalguider.LockX;
  y:=finternalguider.LockY;
  result:=true;
end;

procedure T_autoguider_internal.SetLockPosition(x,y:double);
begin
  finternalguider.LockX:=x;
  finternalguider.LockY:=y;
end;

procedure T_autoguider_internal.StartSettle;
begin
  if InternalguiderGuiding then begin
    FSettling:=true;
    FSettlingInRange:=false;
    FSettleTime:=MaxDouble;
    FSettleStartTime:=now;
    SetStatus('Settling',GUIDER_BUSY);
    WriteLog('INFO: SETTLING STATE CHANGE, Settling started');
  end;
end;

procedure T_autoguider_internal.Pause(onoff:boolean; settle:boolean=true);
begin
  if onoff then begin
    if FState=GUIDER_GUIDING then begin
      FPaused:=true;
      SetStatus('Paused',GUIDER_IDLE);
      WriteLog('INFO: Server received PAUSE');

    end;
  end else begin
    FPaused:=false;
    if InternalguiderGuiding then begin
      WriteLog('INFO: Server received RESUME');
      StartSettle;
    end;
  end;
end;

procedure T_autoguider_internal.Dither(pixel:double; raonly:boolean; waittime:double);
var dra,ddec,mflipcorr: double;
begin
  if ((InternalguiderGuiding) and (not InternalguiderInitialize) and  (not solar_tracking)) then begin
    dra:=(2*random-1)*pixel; // in pixel
    if raonly then
      ddec:=0
    else
      ddec:=(2*random-1)*pixel;
    if mount.isGem and ((mount.PierSide=pierWest) <> (pos('E',finternalguider.pier_side)>0)) then // Did a meridian flip occur since calibration.
      mflipcorr:=180 // A meridian flip occurred
    else
      mflipcorr:=0;
    rotate2(((finternalguider.PA+mflipcorr)*pi/180),dra,ddec, ditherX,ditherY);{rotate a vector point, counter clockwise}
    FDithering:=true;
    Finternalguider.OffsetX:=ditherX; // show in spectro offset
    Finternalguider.OffsetY:=ditherY;
    WriteLog('INFO: DITHER by '+FormatFloat(f3,ditherX)+', '+FormatFloat(f3,ditherY));
    StartSettle;
  end;
end;

procedure T_autoguider_internal.StarLostTimerTimer(Sender: TObject);
begin
  { #todo : star lost }
end;

procedure T_autoguider_internal.SetStatus(aStatus: string ; aState: TAutoguiderState);
begin
  FStatus:=aStatus;
  FState:=aState;
  if assigned(FonStatusChange) then FonStatusChange(self);
end;

Procedure T_autoguider_internal.InitLog;
begin
  try
     Filemode:=2;
     AssignFile(GuideLog,slash(LogDir)+'CCDciel_GuideLog_'+FormatDateTime('yyyy-mm-dd_hhnnss',now)+'.txt');
     Rewrite(GuideLog);
     WriteLn(GuideLog,'CCDciel '+ccdciel_version+'-'+RevisionStr+', Log version 2.5. Log enabled at '+FormatDateTime('YYYY-MM-DD HH:NN:SS',now));
     WriteLn(GuideLog, '');
     Flush(GuideLog);
     GuideLogFileOpen:=true;
  except
  {$I-}
     GuideLogFileOpen:=false;
     CloseFile(GuideLog);
     IOResult;
  {$I+}
  end;
end;

Procedure T_autoguider_internal.CloseLog;
begin
  try
    if GuideLogFileOpen then begin
      WriteLn(GuideLog,'Log closed at '+FormatDateTime('YYYY-MM-DD HH:NN:SS',now));
      GuideLogFileOpen:=false;
      CloseFile(GuideLog);
    end;
  except
    {$I-}
    IOResult;
    {$I+}
  end;
end;

Procedure T_autoguider_internal.WriteLog( buf : string);
begin
  try
     if GuideLogFileOpen then begin
       WriteLn(GuideLog,buf);
       Flush(GuideLog);
     end;
  except
    {$I-}
    on E: Exception do begin
      GuideLogFileOpen:=false;
      msg('Error writing guide log file: '+ E.Message,1);
      CloseFile(GuideLog);
    end;
    {$I+}
  end;
end;

function  T_autoguider_internal.measure_drift(var initialize:boolean; out drX,drY :double) : integer;// ReferenceX,Y indicates the total drift, drX,drY to drift since previouse call. Arrays old_xy_array,xy_array are for storage star positions
var
  i,fitsx,fitsy,stepsize,xsize,ysize,star_counter,star_counter2,counter,len,maxSNRstar,ix,iy: integer;
  hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr,flux,fluxratio,min_SNR,min_HFD,maxSNR,maxSNRhfd,margin,y,mhfd,peak : double;
  x1,y1,bg1,bgdev1,fwhm1,vmax1,snr1,flux1: double;
  GuideLock: boolean;
  drift_arrayX,drift_arrayY : array of double;
  starx,stary,frx,fry,frw,frh: integer;
const
    searchA=28;//square search area
    overlap=6;
    maxstars=1000;
begin
  result:=1;// Assume no stars detected
  star_counter:=0;
  stepsize:=searchA-overlap;//some overlap

  // for guide log
  LogSNR:=0;
  LogFlux:=0;

  FGuideBmp.Canvas.Pen.Color:=clYellow;
  FGuideBmp.Canvas.Pen.Mode:=pmMerge;
  FGuideBmp.Canvas.Pen.Style:=psSolid;
  FGuideBmp.Canvas.Pen.Width:=1;

  xsize:=guidefits.HeaderInfo.naxis1;// width image
  ysize:=guidefits.HeaderInfo.naxis2;// height image


  if initialize then
  begin
    setlength(xy_array,maxstars);
    ditherx:=0;// dither offset
    dithery:=0;
    if OffsetFromTarget then
      // set by target, do not clear
      OffsetFromTarget:=false
    else begin
      Finternalguider.OffsetX:=ditherX; // reset if not set by target position
      Finternalguider.OffsetY:=ditherY;
    end;
  end;

  GuideLock:=finternalguider.SpectroFunctions and finternalguider.GuideLock;

  min_SNR:=finternalguider.minSNR;//make local to reduce some CPU load
  min_HFD:=finternalguider.minHFD;//make local to reduce some CPU load
  maxSNR:=0;
  maxSNRstar:=0;
  maxSNRhfd:=0;
  if xsize<800 then
    margin:=2*DitherPixel+10
  else if xsize<1200 then
    margin:=2*DitherPixel+50
  else
    margin:=2*DitherPixel+100;

  if initialize then
  begin
   if GuideLock then begin
    vmax1:=0;
    if (finternalguider.GuideLockNextX>0)and(finternalguider.GuideLockNextY>0) then begin
       // search star near specified position
       fitsx:=round(finternalguider.GuideLockNextX);
       fitsy:=ysize-round(finternalguider.GuideLockNextY);
    end
    else begin
      // search unique star near slit position
      fitsx:=round(finternalguider.LockX);
      fitsy:=ysize-round(finternalguider.LockY);
    end;
    if finternalguider.GuideLockNextX>-2 then begin
      // can be set to -10 by SpectroSetTarget, in this case we must skip SearchWinMin to go directly with SearchWinMax
      guidefits.FindStarPos2(fitsx,fitsy,finternalguider.SearchWinMin,xc,yc,vmax,bg,bgdev);
      guidefits.GetHFD2(round(xc),round(yc),finternalguider.SearchWinMin,x1,y1,bg1,bgdev1,hfd1,fwhm1,vmax1,snr1,flux1);
    end;
    if (vmax1=0)or(snr1<Finternalguider.MinSNR) then begin
      // if not found try with larger aperture
      guidefits.FindStarPos2(fitsx,fitsy,finternalguider.SearchWinMax,xc,yc,vmax,bg,bgdev);
      guidefits.GetHFD2(round(xc),round(yc),finternalguider.SearchWinMin,x1,y1,bg1,bgdev1,hfd1,fwhm1,vmax1,snr1,flux1);
    end;
    if (vmax1=0)or(snr1<Finternalguider.MinSNR) then begin
      // if still not found search brightest star in image
      guidefits.FindBrightestPixel(xsize div 2,ysize div 2,2*min(xsize,ysize) div 3,starwindow div 2,ix,iy,vmax,true);
      guidefits.FindStarPos2(ix,iy,finternalguider.SearchWinMax,xc,yc,vmax,bg,bgdev);
      guidefits.GetHFD2(round(xc),round(yc),finternalguider.SearchWinMin,x1,y1,bg1,bgdev1,hfd1,fwhm1,vmax1,snr1,flux1);
    end;
    if vmax>0 then begin
        if vmax1>0 then
          CurrentHFD:=hfd1
        else
          CurrentHFD:=0;
        setlength(xy_array,1);
        if (InternalguiderCalibrating or InternalguiderCalibratingBacklash) then begin
          // for calibration the reference is the star position
          xy_array[0].x1:=xc;
          xy_array[0].y1:=yc;
        end
        else begin
          // for guiding use the slit position as reference
          xy_array[0].x1:=fitsx;
          xy_array[0].y1:=fitsy;
        end;
        // current position of star, to be moved on slit
        xy_array[0].x2:=xc;
        xy_array[0].y2:=yc;
        xy_array[0].flux:=1;
        setlength(xy_array_old,1);
        star_counter:=1;
        // Mark star area
        hfd1:=finternalguider.SearchWinMin/2/3;
        FGuideBmp.Canvas.Frame(trunc(1+xc*GuideImgPixRatio-hfd1*3),trunc(1+yc-hfd1*3),trunc(1+xc*GuideImgPixRatio+hfd1*3),trunc(1+yc+hfd1*3));
        WriteLog('INFO: Star(s)='+inttostr(star_counter)+', HFD='+floattostrF(CurrentHFD,FFgeneral,3,3));
        msg(inttostr(star_counter)+' guide stars used, HFD='+floattostrF(CurrentHFD,FFgeneral,3,3),3);
        finternalguider.Info:=IntToStr(star_counter)+' star, Intensity: '+FormatFloat(f1,vmax+bg)+', HFD='+floattostrF(CurrentHFD,FFgeneral,3,3);
     end;
   end
   else begin
    // Divide the image in square areas. Try to detect a star in each area. Store the star position and flux in the xy_array
    mean_hfd:=0;
    peak:=0;
    fitsy:=stepsize div 2;
    repeat
      fitsx:=stepsize div 2;
      repeat
        guidefits.GetHFD3(fitsX,fitsY,searchA,true{autocenter},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);//find a star in this segment. Auto center is true

        if ((snr>Min_SNR) and (hfd1>Min_HFD) and (abs(fitsX-xc)<stepsize div 2) and (abs(fitsY-yc)<stepsize div 2) and (star_counter<maxstars))  then //detection and no other area closer
        begin // star in this area
          xy_array[star_counter].x1:=xc;//store initial measured position for recovering if star is lost
          xy_array[star_counter].y1:=yc;

          xy_array[star_counter].x2:=xc;//store measured star position
          xy_array[star_counter].y2:=yc;
          xy_array[star_counter].flux:=flux;
          mean_hfd:=mean_hfd+hfd1;

          if (vmax+bg)>peak then peak:=vmax+bg;

          // for single star detection
          if (snr>maxSNR)and(xc>margin)and(xc<(xsize-margin))and(yc>margin)and(yc<(ysize-margin)) then begin
            maxSNR:=snr;
            maxSNRhfd:=hfd1;
            maxSNRstar:=star_counter;
          end;

          inc(star_counter);

          // max value for guide log
          LogSNR:=max(LogSNR,snr);
          LogFlux:=max(LogFlux,flux);

          // Annotate the star
          FGuideBmp.Canvas.Frame(trunc(1+xc*GuideImgPixRatio-hfd1*3),trunc(1+yc-hfd1*3),trunc(1+xc*GuideImgPixRatio+hfd1*3),trunc(1+yc+hfd1*3));
        end;

        inc(fitsx,stepsize);
      until fitsx>=xsize-1+stepsize div 2;;
      inc(fitsy,stepsize);
    until fitsy>=ysize-1+stepsize div 2;

    if star_counter>0 then
    begin
      mean_hfd:=mean_hfd/star_counter;
      CurrentHFD:=mean_hfd;

      if ((frame_size<(ysize-1)) and (frame_size<(xsize-1)) and (not (InternalguiderCalibrating or InternalguiderCalibratingBacklash))) then //filter out stars available in the frame
      begin
        starx:=round(xy_array[maxSNRstar].x1); // brightest star position
        stary:=round(xy_array[maxSNRstar].y1);
        frw:=frame_size; // camera frame size
        frh:=frame_size;
        frx:=starx-(frame_size div 2); // camera frame position
        if FCamera.VerticalFlip then
          fry:=ysize-stary-(frame_size div 2)
        else
          fry:=stary-(frame_size div 2);

        frx:=min(xsize-1-frw,max(0,frx)); // Keep frame within sensor area
        fry:=min(ysize-1-frh,max(0,fry));

        star_counter2:=0;
        for i:=0 to star_counter-1 do
        begin
          if FCamera.VerticalFlip then y:=ysize-xy_array[i].y1 else  y:=xy_array[i].y1;
          if  ((xy_array[i].x1>frx+margin) and
               (xy_array[i].x1<frx+frw-margin) and
               (y>fry+margin) and
               (y<fry+frh-margin)) then //use only the stars in the frame including the bright star
          begin
            xy_array[star_counter2]:=xy_array[i];//sort out the stars near the brightest star

            xy_array[star_counter2].x1:=xy_array[i].x1-frx; // new starcenter in small frame
            xy_array[star_counter2].y1:=xy_array[i].y1+fry+frh-ysize;
            xy_array[star_counter2].x2:=xy_array[star_counter2].x1;
            xy_array[star_counter2].y2:=xy_array[star_counter2].y1;
            xy_array[star_counter2].flux:=xy_array[star_counter2].flux;
            if FCamera.CameraInterface=INDI then
            begin
              // INDI frame in unbinned pixel
              frx:=frx*FCamera.BinX;
              fry:=fry*FCamera.BinX;
              frw:=frw*FCamera.BinY;
              frh:=frh*FCamera.BinY;
            end;
            inc(star_counter2);
          end;
        end;
        star_counter:=star_counter2;

        FCamera.SetFrame(frx,fry,frw,frh);//set frame area around the brightest star
        GuideImgCx:=0;
        GuideImgCy:=0;
      end;//filter out stars
    end;

    if star_counter>0 then
    begin
      setlength(xy_array,star_counter);
      setlength(xy_array_old,star_counter);//for later

      WriteLog('INFO: Star(s)='+inttostr(star_counter)+', HFD='+floattostrF(mean_hfd,FFgeneral,3,3));
      msg(inttostr(star_counter)+' guide stars used, HFD='+floattostrF(mean_hfd,FFgeneral,3,3),3);
      finternalguider.Info:=IntToStr(star_counter)+' stars, HFD: '+FormatFloat(f1,mean_hfd)+', SNR: '+FormatFloat(f0,maxSNR)+', PEAK: '+FormatFloat(f0,peak);
    end //stars found
    else
    begin //no star(s) found
      setlength(xy_array,0);
      setlength(xy_array_old,0);
    end;
   end;
  end
  else
  begin //second, third ... call
   if GuideLock then begin
     // single star slit guiding
     if not (InternalguiderCalibrating or InternalguiderCalibratingBacklash) then begin
       // adjust reference slit position if the user make modification while guiding
       xy_array[0].x1:=finternalguider.LockX;
       xy_array[0].y1:=ysize-finternalguider.LockY;
     end;
     xy_array_old[0].x1:=xy_array[0].x1;
     xy_array_old[0].y1:=xy_array[0].y1;
     // search star near previous position
     guidefits.FindStarPos2(round(xy_array_old[0].x2),round(xy_array_old[0].y2),finternalguider.SearchWinMin,xc,yc,vmax,bg,bgdev);
     if vmax=0 then
       guidefits.FindStarPos2(round(xy_array_old[0].x2),round(xy_array_old[0].y2),finternalguider.SearchWinMax,xc,yc,vmax,bg,bgdev);
     if vmax>0 then begin
       // star found
       xy_array[0].x2:=xc;
       xy_array[0].y2:=yc;
       xy_array[0].flux:=1;
       star_counter:=1;
       finternalguider.GuideLockNextX:=round(xc); // in case of recovery restart
       finternalguider.GuideLockNextY:=ysize-round(yc);
       // Mark star area
       hfd1:=finternalguider.SearchWinMin/2/3;
       FGuideBmp.Canvas.Frame(trunc(1+xc*GuideImgPixRatio-hfd1*3),trunc(1+yc-hfd1*3),trunc(1+xc*GuideImgPixRatio+hfd1*3),trunc(1+yc+hfd1*3));
       finternalguider.Info:=IntToStr(star_counter)+' star, Intensity: '+FormatFloat(f1,vmax);
     end
     else begin
       // star lost
       xy_array[0].flux:=0;
     end;
   end
   else begin
    // multi star guiding
    mhfd:=0;
    peak:=0;
    for i:=0 to length(xy_array_old)-1 do
    begin
      if xy_array_old[i].flux<>0 then // Previouse dection, keep tracking this star while it drifts away
      begin //try first within a small area
        guidefits.GetHFD3(round(xy_array_old[i].x2),round(xy_array_old[i].y2),round(mean_hfd*3.5){smaller search area},true{autocenter},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);
       if snr<1 then // no detection, look wider
        guidefits.GetHFD3(round(xy_array_old[i].x2),round(xy_array_old[i].y2),searchA{area},true{autocenter},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false) // use a larger search area
      end
      else // try in the initial area
        guidefits.GetHFD3(round(xy_array_old[i].x1),round(xy_array_old[i].y1),searchA,true{autocenter},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);// find a star in the orginal segment

      if ((snr>max(min_SNR-10,6)) and (hfd1>Min_HFD)) then // star detection
      begin // star in this area
        xy_array[i].x2:=xc;
        xy_array[i].y2:=yc;
        xy_array[i].flux:=flux;
        inc(star_counter);
        mhfd:=mhfd+hfd1;

        if (vmax+bg)>peak then peak:=vmax+bg;

        // max value for guide log
        LogSNR:=max(LogSNR,snr);
        LogFlux:=max(LogFlux,flux);

        // Mark star area
        FGuideBmp.Canvas.Frame(trunc(1+xc*GuideImgPixRatio-hfd1*3),trunc(1+yc-hfd1*3),trunc(1+xc*GuideImgPixRatio+hfd1*3),trunc(1+yc+hfd1*3));
      end
      else
      begin //Star lost temporary
        xy_array[i].flux:=0;
      end;
    end;
    if star_counter>0 then begin
      mhfd:=mhfd/star_counter;
      finternalguider.Info:=IntToStr(star_counter)+' stars, HFD: '+FormatFloat(f1,mhfd)+', SNR: '+FormatFloat(f0,LogSNR)+', PEAK: '+FormatFloat(f0,peak);
    end;
   end;
  end;
  if star_counter<1 then
  begin
    msg('No stars detected!',0);
    initialize:=true;// Return initialize=true for fresh restart next call.
    FCamera.ResetFrame;
    finternalguider.GuideLockNextX:=-1;
    finternalguider.GuideLockNextY:=-1;
    exit;
  end;

  // calculate movement in each area
  counter:=0;
  if ((initialize=false) and (length(xy_array_old)>0)) then //not empthy, second round or later
  begin
    len:=length(xy_array_old);
    setlength(drift_arrayX,len);
    setlength(drift_arrayY,len);
    for i:=0 to len-1 do
    begin
      fluxratio:=xy_array_old[i].flux/(xy_array[i].flux+0.001);
      if  ((fluxratio>0.5) and (fluxratio<2)) then //star flux is similar
      begin
        drift_arrayX[counter]:=xy_array[i].x2 - xy_array_old[i].x1+ditherX; //drift in pixels relative to initial measurement x1,y1
        drift_arrayY[counter]:=xy_array[i].y2 - xy_array_old[i].y1+ditherY;
        inc(counter);
      end;
    end;
    if counter/star_counter<0.5 then  // second round and less the 50% of stars are detected
      msg('Guider, warning lost track or exposure time changed!',0); //more then 7.5 pixels drift in one cycle

    // Remove outliers and calculate mean drift in X and Y.
    get_best_mean(drift_arrayX,counter {length},drX );
    get_best_mean(drift_arrayY,counter {length},drY );
  end;

  for i:=0 to length(xy_array_old)-1 do // copy xy_array to xy_array_old
      xy_array_old[i]:=xy_array[i];
  initialize:=false;// success, first data collected
  result:=0; // good result
end;


procedure T_autoguider_internal.InternalguiderLoop;
begin
  SetStatus('Looping Exposures',GUIDER_IDLE);
  StopInternalguider:=false;
  InternalguiderRunning:=true;
  Finternalguider.Info:='';
  Finternalguider.ButtonLoop.enabled:=false;
  Finternalguider.ButtonCalibrate.enabled:=false;
  Finternalguider.ButtonGuide.enabled:=false;
  Finternalguider.ButtonDark.enabled:=false;
  if InternalguiderCapturingDark then begin
    FGuideFits.SetBPM(bpm,0,0,0,0);
    FGuideFits.DarkOn:=false;
    FGuideFits.FlatOn:=false;
    FCamera.AddFrames:=true;
    FCamera.StackNum:=12;
    FCamera.SaveFrames:=false;
    FCamera.AlignFrames:=false;
    FCamera.StackOperation:=1;
    FCamera.StackAllow8bit:=true;
    FCamera.StackUseDark:=false;
    FCamera.StackUseFlat:=false;
    FCamera.StackDebayer:=false;
  end
  else begin
    FGuideFits.SetBPM(bpm,0,0,0,0);
    FGuideFits.DarkOn:=true;
    FGuideFits.FlatOn:=false;
    FCamera.AddFrames:=false;
    FCamera.StackNum:=-1;
    FCamera.StackAllow8bit:=false;
    FCamera.SaveFrames:=false;
    FCamera.AlignFrames:=false;
  end;
  Binning:=Finternalguider.Binning.Value;
  Fcamera.ResetFrame;
  FRecoveringCameraCount:=0;
  Application.QueueAsyncCall(@StartGuideExposureAsync,0);
end;

Procedure T_autoguider_internal.StartGuideExposureAsync(Data: PtrInt);
begin
  StartGuideExposure;
end;

Procedure T_autoguider_internal.StartGuideExposure;
var e: double;
    binx,biny,gain: integer;
begin
if (FCamera.Status=devConnected) then begin
  // check internal pulse guide is in progress
  if PulseGuiding then begin
     CheckSynchronize();
     sleep(50);
     Application.QueueAsyncCall(@StartGuideExposureAsync,0);
     exit;
  end;
  // check exposure time
  e:=finternalguider.Exposure.value;
  binx:=Binning;
  biny:=binx;
  if (binx<FCamera.BinXrange.min)or(biny<FCamera.BinYrange.min) or
     (binx>FCamera.BinXrange.max)or(biny>FCamera.BinYrange.max)
     then begin
        msg(Format(rsInvalidBinni, [inttostr(binx)]),1);
        InternalguiderStop;
        exit;
     end;
     if (FCamera.BinX<>binx)or(FCamera.BinY<>biny) then begin
        FCamera.SetBinning(binx,biny);
     end;
  if FCamera.hasGain then begin
    gain:=finternalguider.Gain.Value;
    if FCamera.Gain<>gain then begin
      FCamera.Gain:=gain;
    end;
    if FCamera.hasOffset then begin
       if FCamera.Offset<>finternalguider.Offset.Value then
         FCamera.Offset:=finternalguider.Offset.Value;
    end;
  end;
  if InternalguiderCapturingDark then begin
    if (FCamera.FrameType<>DARK) then
      FCamera.FrameType:=DARK
  end
  else if FCamera.FrameType<>LIGHT then
    FCamera.FrameType:=LIGHT;
  FCamera.ObjectName:=rsGuide;
  FCamera.GuidePixelScale:=Finternalguider.pixel_size;

  FCamera.StartExposure(e);
  FRecoveringCamera:=false;

end
else begin
   InternalguiderStop;
   if not AllDevicesConnected then msg(rsSomeDefinedD,1);
end;
end;

procedure T_autoguider_internal.InternalguiderStart;
begin
  Finternalguider.cbSpectro.enabled:=false;
  Finternalguider.cbGuideLock.enabled:=false;
  SetStatus('Start Guiding',GUIDER_BUSY);
  Application.QueueAsyncCall(@InternalguiderStartAsync,0);
end;

procedure T_autoguider_internal.InternalguiderStartAsync(Data: PtrInt); {internal guider}
var
  i: integer;
  txt,pier,frametxt: string;
  ra,de,alt,az,lha: double;
begin
  if AllDevicesConnected=false then
  begin
    msg('Internal guider: '+rsSomeDefinedD,1);
    InternalguiderStop;
    SetStatus('Devices not connected',GUIDER_ALERT);
    exit;
  end;
  if FCamera.Status<>devConnected then
  begin
    msg('Internal guider: Guide camera not connected!',1);
    InternalguiderStop;
    SetStatus('Guide camera not connected',GUIDER_ALERT);
    exit;
  end;
  if Fmount.canpulseguide=false then
  begin
    msg('Abort, mount does not support pulse guiding!',1);
    InternalguiderStop;
    SetStatus('Mount not supported',GUIDER_ALERT);
    exit;
  end;
  if Finternalguider.pier_side='N/A' then
  begin
    msg('A calibration is required before guiding can be started.',1);
    InternalguiderStop;
    SetStatus('Calibration required',GUIDER_ALERT);
    exit;
  end;

  if InternalguiderGuiding then
  begin
    // already starting
    exit;
  end;
  SetStatus('Start Guiding',GUIDER_BUSY);
  StopInternalguider:=false;
  InternalguiderGuiding:=true;
  FPaused:=false;
  finternalguider.LabelStatusRA.Caption:='';
  finternalguider.LabelStatusDec.Caption:='';

  if Fmount.Tracking=false then
  begin
    msg('Start tracking. Wait 20 seconds',2);
    Fmount.Track;//start tracking
    wait(20);
  end;

  if FMount.CanSetGuideRates and Finternalguider.ForceGuideSpeed.Checked then begin
    //Set the same speed as calibration
    Fmount.GuideRateRa:=Finternalguider.GuideSpeedRA.Value*360/(24*60*60);  //set pulse speed in degree per seconds
    Fmount.GuideRateDe:=Finternalguider.GuideSpeedDEC.Value*360/(24*60*60); //DEC option use same unit [* sidereal] as RA rate
  end
  else begin
    // read speed from mount
    Finternalguider.GuideSpeedRA.Value:=FMount.GuideRateRa*(24*60*60)/360;
    Finternalguider.GuideSpeedDEC.Value:=FMount.GuideRateDe*(24*60*60)/360;
  end;
  // check here for in mount guide speed change
  if not Finternalguider.CalibrationIsValid(txt) then
  begin
    StopInternalguider:=true;
    Finternalguider.Info:='A new calibration is required before guiding can be started:';
    Finternalguider.CameraStatus:=txt;
    msg('A new calibration is required before guiding can be started:',1);
    msg(txt,1);
    InternalguiderStop;
    SetStatus('Calibration required',GUIDER_ALERT);
    exit;
  end;

  setlength(xy_trend,nrpointsTrend);
  for i:=0 to nrpointsTrend-1 do {clear}
  begin
   xy_trend[i].ra:=1E100;//delta ra, 1E100 is an empthy marker
   xy_trend[i].dec:=0;//delta dec
   xy_trend[i].racorr:=0;//ra correction
   xy_trend[i].deccorr:=0; //dec correction
   xy_trend[i].dither:=false; //dither
  end;

  old_moveRA:=0;
  old_moveDEC:=0;
  LastDecSign:=0;
  SameDecSignCount:=maxreverse;
  LastBacklash:=false;
  FirstDecDirectionChange:=true;
  LastBacklashDuration:=0;

  SelectSpectroTarget;

  InternalguiderInitialize:=true; //initialize;

  Binning:=Finternalguider.Binning.Value;
  frame_size:=Finternalguider.FrameSize div Binning;
  if frame_size=999999 then
    frametxt:='Full'
  else
    frametxt:=inttostr(frame_size);

  // initialize the guide log
  case mount.PierSide of
    pierEast: pier:='East';
    pierWest: pier:='West';
    pierUnknown: pier:='Unknown';
  end;
  ra:=mount.RA;
  de:=mount.Dec;
  MountToLocal(mount.EquinoxJD,ra,de);
  cmdEq2Hz(ra,de,az,alt);
  lha:=rmod(CurrentSidTim*rad2deg/15-ra+24,24);
  if lha>12 then lha:=lha-24;
  GuideFrameCount:=0;
  GuideStartTime:=now;
  WriteLog('Guiding Begins at '+FormatDateTime('YYYY-MM-DD HH:NN:SS',GuideStartTime));
  WriteLog('Equipment Profile = '+profile);
  txt:='Dither = ';
  if DitherRAonly then
    txt:=txt+'RA only'
  else
    txt:=txt+'both axes';
  txt:=txt+', Dither scale = '+formatfloat(f3,DitherPixel);
  WriteLog(txt);
  WriteLog('Pixel scale = '+FormatFloat(f2,Finternalguider.pixel_size)+' arc-sec/px, Binning = '+IntToStr(Finternalguider.Binning.Value));
  WriteLog('Frame size = '+frametxt);
  WriteLog('Camera = '+camera.DeviceName);
  WriteLog('Exposure = '+FormatFloat(f0,finternalguider.Exposure.value*1000)+' ms');
  //Following is required for correct pulse indication. Indicated pulse amplitude=xRate*RADuration. xAngle is not used and only for info.
  WriteLog('Mount = '+mount.DeviceName+','+BoolToStr(mount.Status=devConnected,'connected','disconnected')+
           ',guiding '+BoolToStr(not Finternalguider.disable_guiding,'enabled','disabled')+',xAngle = '+FormatFloat(f2,Finternalguider.PA)+
           ', xRate = '+FormatFloat(f2,abs(cos(mount.dec*pi/180)*(Finternalguider.pulsegainEast+Finternalguider.pulsegainWest)/2))+
           ',, yRate = '+FormatFloat(f2,abs((Finternalguider.pulsegainNorth+Finternalguider.pulsegainSouth)/2)));
  WriteLog('RA Gain = '+IntToStr(Finternalguider.RAgain)+', RA Hyst = '+IntToStr(Finternalguider.RA_hysteresis));
  WriteLog('DEC Gain = '+IntToStr(Finternalguider.DECgain)+', DEC Hyst = '+IntToStr(Finternalguider.DEC_hysteresis));
  WriteLog('Pulse gain East = '+FormatFloat(f2,Finternalguider.pulsegainEast)+', Pulse gain West = '+FormatFloat(f2,Finternalguider.pulsegainWest));
  WriteLog('Pulse gain North = '+FormatFloat(f2,Finternalguider.pulsegainNorth)+', Pulse gain South = '+FormatFloat(f2,Finternalguider.pulsegainSouth));
  WriteLog('Shortest guide pulse setting = '+IntToStr(Finternalguider.shortestPulse));
  WriteLog('Max RA duration = '+IntToStr(Finternalguider.LongestPulse)+', Max DEC duration = '+IntToStr(Finternalguider.LongestPulse));
  WriteLog('Minimum HFD setting = '+FormatFloat(f2,Finternalguider.minHFD));
  WriteLog('Minimum SNR setting = '+FormatFloat(f2,Finternalguider.minSNR));
  WriteLog('RA Guide Speed = '+FormatFloat(f1,Finternalguider.GuideSpeedRA.Value*15)+' a-s/s, '+
           'Dec Guide Speed = '+FormatFloat(f1,Finternalguider.GuideSpeedDEC.Value*15)+' a-s/s, '+
           'Cal Dec = '+Finternalguider.CalDeclination.Text+', '+
           'Last Cal Issue = '+Finternalguider.CalIssue.Text+', '+
           'Timestamp = '+Finternalguider.CalDate.Text);
  WriteLog('RA = '+FormatFloat(f2,mount.Ra)+' hr, Dec = '+FormatFloat(f2,mount.Dec)+' deg, Hour angle = '+FormatFloat(f2,lha)+
           ' hr, Pier side = '+pier+', Alt = '+FormatFloat(f1,alt)+' deg, Az = '+FormatFloat(f1,az));
  if finternalguider.SolarTracking then
    WriteLog('SolarTracking = true, Rate = '+FormatFloat(f3,Finternalguider.v_solar)+', PA = '+FormatFloat(f2,Finternalguider.vpa_solar))
  else
    WriteLog('SolarTracking = false');
  WriteLog('');
  WriteLog('Frame,Time,mount,dx,dy,RARawDistance,DECRawDistance,RAGuideDistance,DECGuideDistance,RADuration,RADirection,DECDuration,DECDirection,XStep,YStep,StarMass,SNR,ErrorCode');

  InternalguiderLoop;
  StartSettle;

end;

procedure T_autoguider_internal.ParameterChange(txt: string);
begin
  WriteLog('INFO: Guiding parameter change, '+txt);
end;

procedure T_autoguider_internal.InternalAutoguiding;
var i,maxpulse: integer;
    RADuration,DECDuration,BacklashDuration,NewPulseDEC: LongInt;
    RADirection,DECDirection: string;
    mflipcorr,PAsolar,moveRA2,dsettle,DecSign: double;
    meridianflip, largepulse: boolean;

          procedure track_solar_object;//neo and comet tracking
          // Calculates the total integrated correction in pixels for the reference stars in the guider image (DitherX, DitherY)
          // The Pixel scale internalguider.pixel_size in tab advanced should be reasonable accurate.
          var
            tickcount : qword;
            deltaticks,flipdec : integer;
            ra_rate,dec_rate, delta_ditherX,delta_ditherY, dRaPixelsSolar,dDecPixelsSolar : double;
          begin
            tickcount:=GetTickCount64;
            deltaticks:=tickcount-oldtickcount;// number of milliseconds since last cycle
            oldtickcount:=tickcount;//remember tickcount for next cycle

            if abs(deltaticks)<30000 then
            begin //less then 30 seconds passed so a valid oldtickcount. Increase drift to follow comet with the mount
              PAsolar:=internalguider.vpa_solar;
              if finternalguider.InverseSolarTracking then PAsolar:=180-PAsolar; //inverse ra and dec control equals 180 degrees rotation. T
              sincos(PAsolar*pi/180,ra_rate,dec_rate);
              ra_rate:=ra_rate*internalguider.v_solar;//solar object sky movement in RA ["/min]
              dec_rate:=dec_rate*internalguider.v_solar;//solar object sky movement in DEC ["/min]
              dRaPixelsSolar:= - ra_rate * deltaticks/(60*1000*internalguider.pixel_size);//Solar object sky movement in RA. Unit in guider pixels.
              dDecPixelsSolar:=+ dec_rate * deltaticks/(60*1000*internalguider.pixel_size);//Solar object sky movement in DEC. Unit in guider pixels

              if meridianflip then // Did a meridian flip occur since calibration.
                mflipcorr:=180 // A meridian flip occurred
              else
                mflipcorr:=0;

              if finternalguider.pulsegainNorth<0 then flipDec:=-1 else flipDec:=+1;//flipped image correction. E.g. an image where north is up and east on the right size.
              rotate2(((+finternalguider.PA+mflipcorr)*pi/180),dRaPixelsSolar,flipDec*dDecPixelsSolar,delta_ditherX,delta_ditherY);// rotate RA, DEC drift scope to X,Y drift guider image. Positive ditherY is go North. Postive ditherX is go West!
              ditherX:=ditherX+delta_ditherX; //integrate offset solar object in X
              ditherY:=ditherY+delta_ditherY; //integrate offset solar object in Y
              Finternalguider.OffsetX:=ditherX;  // show in spectro offset
              Finternalguider.OffsetY:=ditherY;
            end;
          end;
begin
 if not FPaused then begin

  meridianflip:= mount.isGem and ((mount.PierSide=pierWest) <> (pos('E',finternalguider.pier_side)>0));
  if ((mount.isGem) and (mount.PierSide=pierWest) and (finternalguider.ReverseDec=false)) then //Correct measurement for reverse Dec by merdian flip
  begin //Swap definition north and south
    north:=1;
    south:=0
  end
  else
  begin //Fork mount or mount software keeps action button north north
    north:=0;
    south:=1
  end;

  xy_trend[0].dither:=FSettling;

  //For tracking Solar object only
  if ((FSettling=false) and (finternalguider.SolarTracking)) then
  begin
    track_solar_object; //Calculate the ditherX, Y factors to track a solar object
    solar_tracking:=true;// Block any dithering
    //msg(floattostr(ditherX)+' '+floattostr(ditherY)+' || '+floattostr(ditherX2)+' '+floattostr(ditherY2),3);
  end
  else
    solar_tracking:=false;// Allow dithering

  // read eventually modified spectro offset value to manually adjust guide position with the slit during guiding
  if (finternalguider.SpectroFunctions)and(not solar_tracking) then begin
    ditherX:=Finternalguider.OffsetX;
    ditherY:=Finternalguider.OffsetY;
  end;

  //Measure drift
  measure_drift(InternalguiderInitialize,driftX,driftY);// ReferenceX,Y indicates the total drift, driftX,driftY the drift since previous call. Arrays xy_array_old,xy_array are for storage star positions

  if InternalguiderInitialize then begin
     SetStatus(StarLostStatus,GUIDER_ALERT);
     inc(GuideFrameCount);
     WriteLog(IntToStr(GuideFrameCount)+','+
              FormatFloat(f3,(now-GuideStartTime)*secperday)+','+
              '"DROP"'+',,,,,,,,,,,,,'+
              FormatFloat(f0,LogFlux)+','+
              FormatFloat(f2,LogSNR)+','+
              '2,"Star lost"'    // error code
              );
     exit; //until star(s) detected. If no stars are detected initialize is returned true
  end;

  // Process settling
  if FSettling then begin
     finternalguider.trend_message('Guider is settling.','','');
     if ((now-FSettleStartTime)*SecsPerDay)<FSettleTmax then begin
       // check current distance
       dsettle:=sqrt(driftx*driftx+drifty*drifty);
       if dsettle<=FSettlePix then begin
         // distance in range
         if FSettlingInRange then begin
           // check for how long we are in range
           if ((now-FSettleTime)*SecsPerDay)>=FSettleTmin then begin
             // settling complete
             FSettling:=false;
             FDithering:=false;
             SetStatus('Guiding',GUIDER_GUIDING);
             WriteLog('INFO: SETTLING STATE CHANGE, Settling complete');
           end;
         end
         else begin
           // initialize in range
           FSettlingInRange:=true;
           FSettleTime:=now;
         end;
       end
       else begin
         // no more in range
         FSettlingInRange:=false;
       end;
     end
     else begin
       // timeout reach
       FSettling:=false;
       FDithering:=false;
       SetStatus('Guiding',GUIDER_GUIDING);
       WriteLog('INFO: SETTLING STATE CHANGE, Settling failed');
     end;
  end
  else begin
    // star lost recovered
    if FState=GUIDER_ALERT then
      SetStatus('Guiding',GUIDER_GUIDING);
  end;

  // Apply camera orientation and meridian flip if required
  if meridianflip then // Did a meridian flip occur since calibration.
    mflipcorr:=180 // A meridian flip occurred
  else
    mflipcorr:=0;
  rotate2((- (finternalguider.PA+mflipcorr)*pi/180),driftX,driftY, driftRA,driftDec);{rotate a vector point, counter clockwise}

  if finternalguider.pulsegainNorth>0 then driftDEC:=-driftDEC;//flipped image correction. E.g. an image where north is up and east on the right size.

  xy_trend[0].ra:=-DriftRa;//store RA drift in pixels.
  xy_trend[0].dec:=+DriftDec;//store DEC drift in pixels.

  if finternalguider.disable_guiding=false then //guiding enabled
  begin
    //calculate required RA and DEC correction in pixels
    if FSettling and finternalguider.SpectroFunctions then begin
      // No hysteresis when moving the star to the slit
      moveRA:=- driftRA * finternalguider.RAgain/100;
      old_moveRA:=0;
      moveDEC:=- driftDEC * finternalguider.DECgain/100;
      old_moveDEC:=0;
    end
    else begin
      //Hysteresis as in PHD1  { #todo : Make old_moveXX a running mean of N previous values? }
      moveRA:=(- driftRA*(1 - finternalguider.RA_hysteresis/100) +   old_moveRA * finternalguider.RA_hysteresis/100 ) * finternalguider.RAgain/100;
      old_moveRA:=moveRA;//Store for next cycle hysteresis calculation
      moveDEC:=(- driftDEC*(1 - finternalguider.DEC_hysteresis/100) +   old_moveDEC * finternalguider.DEC_hysteresis/100 ) * finternalguider.DECgain/100;
      old_moveDEC:=moveDEC;//Store for next cycle hysteresis calculation
    end;

    Guidethecos:=cos(mount.Dec*pi/180); if Guidethecos<0.000001 then Guidethecos:=0.000001;
    moveRA2:=moveRA/Guidethecos; //correct pixels with cos(dec). Rotation in pixels near celestial pole decreases with cos(dec)

    pulseRA:=0;
    pulseDEC:=0;
    RADuration:=0;
    RADirection:='';
    DECDuration:=0;
    DECDirection:='';
    BacklashDuration:=0;

    if moveRA2>0 then //going East increases the RA
    begin
       pulseRA:=min(finternalguider.LongestPulse,round(1000*abs(moveRA2/finternalguider.pulsegainEast))); {duration msec}
       if pulseRA>finternalguider.shortestPulse then //Large enough correction to follow by motors/relays. Complementary with minimum_move
       begin
         //msg('East: '+inttostr(pulseRA),3);
         mount.PulseGuide(2,pulseRA);  // 0=north, 1=south, 2 East, 3 West
         RADuration:=abs(pulseRA);
         RADirection:='E';
       end
       else moveRA:=0; // for trend in pixels
    end
    else
    if moveRA2<0 then //going West
    begin
      pulseRA:=min(finternalguider.LongestPulse,round(1000*abs(moveRA2/finternalguider.pulsegainWest))); {duration msec}
      if pulseRA>finternalguider.shortestPulse then
      begin
        //msg('West: '+inttostr(pulseRA),3);
        mount.PulseGuide(3,pulseRA);  // 0=north, 1=south, 2 East, 3 West
        RADuration:=abs(pulseRA);
        RADirection:='W';
      end
       else moveRA:=0; // for trend in pixels
    end;

    // to prevent Dec oscillation, wait 3 corrections in the same direction,
    // except if the correction is more than 3X shortestpulse
    DecSign:=sgn(moveDEC);
    largepulse:=round(1000*abs(moveDEC/finternalguider.pulsegainNorth))>(3*finternalguider.ShortestPulse);  // 3 * minimal pulse
    if (not LastBacklash)and (not finternalguider.SolarTracking) then begin
      // tracking comet likely make the correction always in the same direction, disable this process in this case
      if largepulse then begin
        // force pulse
        LastDecSign:=DecSign;
        SameDecSignCount:=maxreverse-1;
      end;
      if LastDecSign<>0 then begin
        if (LastDecSign=DecSign) then begin
          inc(SameDecSignCount);
          if SameDecSignCount<maxreverse then begin
            // wait more
            moveDEC:=0;
          end
          else if (SameDecSignCount=maxreverse)and(DecSign<>sign(LastBacklashDuration)) then begin
            if FirstDecDirectionChange then begin
              // no backlash compensation for the first change after start guiding
              FirstDecDirectionChange:=false;
              LastBacklash:=false;
              BacklashDuration:=0;
              LastBacklashDuration:=round(DecSign*finternalguider.DecBacklash);
            end
            else begin
              // eventual backlash compensation
              BacklashDuration:=round(DecSign*abs(LastBacklashDuration));
            end;
          end;
        end
        else begin
          // initialize new direction
          SameDecSignCount:=0;
          moveDEC:=0;
        end;
      end;
      LastDecSign:=DecSign;
    end;

    // backlash compensation
    if LastBacklash then begin
      // last pulse was a backlash compensation, look at the result
      NewPulseDEC:=round(1000*abs(moveDEC/finternalguider.pulsegainNorth)); {next pulse duration msec after backlash}
      if NewPulseDEC>(finternalguider.ShortestPulse) then begin
        // more correction are required after backlash compensation
        if DecSign=sgn(LastBacklashDuration) then begin
          // still in same direction, increase compensation for next time
          LastBacklashDuration:=round(sgn(LastBacklashDuration)*min(finternalguider.LongestPulse,abs(LastBacklashDuration)+NewPulseDEC/2));
          finternalguider.DecBacklash:=abs(LastBacklashDuration); //update backlash in config
        end
        else begin
          // direction change, decrease compensation for next time
          LastBacklashDuration:=round(sgn(LastBacklashDuration)*min(finternalguider.LongestPulse,abs(LastBacklashDuration)-NewPulseDEC));
          finternalguider.DecBacklash:=abs(LastBacklashDuration); //update backlash in config
        end;
      end;
      LastBacklash:=false;
    end
    else begin
      if (BacklashDuration<>0) and Finternalguider.BacklashCompensation then begin
         // use backlash compensation now
         if DecSign>0 then
           moveDEC:=abs(BacklashDuration*finternalguider.pulsegainNorth/1000) // convert duration to pixel
         else
           moveDEC:=-abs(BacklashDuration*finternalguider.pulsegainSouth/1000);
         LastBacklashDuration:=BacklashDuration;
         LastBacklash:=true;
      end;
    end;

    if moveDEC>0 then //go North increase the DEC.
    begin
      pulseDEC:=min(finternalguider.LongestPulse,round(1000*abs(moveDEC/finternalguider.pulsegainNorth))); {duration msec}
      if pulseDEC>finternalguider.shortestPulse then
      begin
        //msg('North: '+inttostr(pulseDEC),3);
        mount.PulseGuide(north,pulseDEC);  // 0=north, 1=south, 2 East, 3 West
        DECDuration:=abs(pulseDEC);
        DECDirection:='N';
      end
      else moveDEC:=0; // for trend in pixels
    end
    else
    if moveDEC<0 then //go South
    begin
      pulseDEC:=min(finternalguider.LongestPulse,round(1000*abs(moveDEC/finternalguider.pulsegainSouth))); {duration msec}
      if pulseDEC>finternalguider.shortestPulse then
      begin
        //msg('South: '+inttostr(pulseDEC),3);
        mount.PulseGuide(south,pulseDEC);  // 0=north, 1=south, 2 East, 3 West
        DECDuration:=abs(pulseDEC);
        DECDirection:='S';
      end
      else moveDEC:=0; // for trend in pixels
    end;


    // wait for puls guide move completed
    maxpulse:=max(pulseRA,pulseDEC);
    if maxpulse>finternalguider.shortestPulse then
    begin
      WaitPulseGuiding(maxpulse);
    end;

    xy_trend[0].racorr:=-moveRA;//store RA correction in pixels for trend
    xy_trend[0].deccorr:=+moveDEC;//store DEC correction in pixels for trend


    if InternalguiderRunning then begin
      // write log
      inc(GuideFrameCount);
      //Frame,Time,mount,dx,dy,RARawDistance,DECRawDistance,RAGuideDistance,DECGuideDistance,RADuration,RADirection,DECDuration,DECDirection,XStep,YStep,StarMass,SNR,ErrorCode
      WriteLog(IntToStr(GuideFrameCount)+','+
               FormatFloat(f3,(now-GuideStartTime)*secperday)+','+
               '"Mount"'+','+
               FormatFloat(f3,driftX)+','+
               FormatFloat(f3,driftY)+','+
               FormatFloat(f3,driftRA)+','+
               FormatFloat(f3,driftDec)+','+
               FormatFloat(f3,moveRA2)+','+ //moveRA2 is in pixels
               FormatFloat(f3,moveDEC)+','+
               IntToStr(RADuration)+','+
               RADirection+','+
               IntToStr(DECDuration)+','+
               DECDirection+','+
               ',,'+  // AO
               FormatFloat(f0,LogFlux)+','+
               FormatFloat(f2,LogSNR)+','+
               '0'    // error code
               );
      //Send stats to main
      GuideStat(-finternalguider.pixel_size*driftRA,finternalguider.pixel_size*driftDec,LogFlux);
      //Status line
      if RADuration>0 then
         finternalguider.LabelStatusRA.Caption:=RADirection+': '+IntToStr(RADuration)+'ms, '+FormatFloat(f1,driftRA)+'px'
      else
         finternalguider.LabelStatusRA.Caption:='';
      if DECDuration>0 then begin
         if LastBacklash then
           finternalguider.LabelStatusDec.Caption:='Backlash '+DECDirection+': '+IntToStr(DECDuration)+'ms, '+FormatFloat(f1,driftDec)+'px'
         else
           finternalguider.LabelStatusDec.Caption:=DECDirection+': '+IntToStr(DECDuration)+'ms, '+FormatFloat(f1,driftDec)+'px';
      end
      else begin
       if LastDecSign<>0 then begin
         if SameDecSignCount>maxreverse then
           finternalguider.LabelStatusDec.Caption:=''
         else begin
           if LastDecSign>0 then
             DECDirection:='N'
           else
             DECDirection:='S';
           if SameDecSignCount=0 then
             finternalguider.LabelStatusDec.Caption:='Reversal: '+DECDirection
           else
             finternalguider.LabelStatusDec.Caption:='Wait reversal: '+DECDirection+', '+IntToStr(SameDecSignCount);
         end;
       end;
      end;
    end;

  end //guiding enabled
  else
  begin  //guiding disabled
    if InternalguiderRunning then begin
       // write drift to log
       moveRA:=(- driftRA*(1 - finternalguider.RA_hysteresis/100) +   old_moveRA * finternalguider.RA_hysteresis/100 ) * finternalguider.RAgain/100;//Hysteresis as in PHD1
       old_moveRA:=moveRA;//Store for next cycle hysteresis calculation
       //calculate required DEC correction in pixels
       moveDEC:=(- driftDEC*(1 - finternalguider.DEC_hysteresis/100) +   old_moveDEC * finternalguider.DEC_hysteresis/100 ) * finternalguider.DECgain/100;//Hysteresis as in PHD1
       old_moveDEC:=moveDEC;//Store for next cycle hysteresis calculation
       Guidethecos:=cos(mount.Dec*pi/180); if Guidethecos<0.000001 then Guidethecos:=0.000001;
       moveRA2:=moveRA/Guidethecos; //correct pixels with cos(dec). Rotation in pixels near celestial pole decreases with cos(dec)
       RADuration:=0;
       DECDuration:=0;
       RADirection:='';
       DECDirection:='';
       // write log
       inc(GuideFrameCount);
       //Frame,Time,mount,dx,dy,RARawDistance,DECRawDistance,RAGuideDistance,DECGuideDistance,RADuration,RADirection,DECDuration,DECDirection,XStep,YStep,StarMass,SNR,ErrorCode
       WriteLog(IntToStr(GuideFrameCount)+','+
                FormatFloat(f3,(now-GuideStartTime)*secperday)+','+
                '"Mount"'+','+
                FormatFloat(f3,driftX)+','+
                FormatFloat(f3,driftY)+','+
                FormatFloat(f3,driftRA)+','+
                FormatFloat(f3,driftDec)+','+
                FormatFloat(f3,moveRA2)+','+ //moveRA2 is in pixels
                FormatFloat(f3,moveDEC)+','+
                IntToStr(RADuration)+','+
                RADirection+','+
                IntToStr(DECDuration)+','+
                DECDirection+','+
                ',,'+  // AO
                FormatFloat(f0,LogFlux)+','+
                FormatFloat(f2,LogSNR)+','+
                '0'    // error code
                );
    end;
    xy_trend[0].racorr:=0;
    xy_trend[0].deccorr:=0;
  end;

  // Plot graph
  if not FSettling then begin
    finternalguider.draw_xy(xy_trend);//plot xy values
    finternalguider.draw_trend(xy_trend);// plot trends
    for i:=nrpointsTrend-2 downto 0 do {shift values and make place for new values}
      xy_trend[i+1]:=xy_trend[i];//move records one position
  end;

 end;
end;

function T_autoguider_internal.WaitPulseGuiding(pulse:longint): boolean;
begin
 PulseGuiding:=true;
 TimerWaitPulseGuiding.Interval:=pulse;
 TimerWaitPulseGuiding.Enabled:=false;
 TimerWaitPulseGuiding.Enabled:=true;
 result:=true;
end;

procedure T_autoguider_internal.TimerWaitPulseGuidingTimer(const Sender: TObject);
begin
  TimerWaitPulseGuiding.Enabled:=false;
  PulseGuiding:=false;
end;

procedure T_autoguider_internal.InternalguiderStop;
begin
  if InternalguiderGuiding and (not InternalguiderCapturingDark)and(not StopInternalguider) then begin
    WriteLog('Guiding Ends at '+FormatDateTime('YYYY-MM-DD HH:NN:SS',now));
    WriteLog('');
  end;
  InternalguiderRunning:=false;
  if not StopInternalguider then begin
    StopInternalguider:=true;
    FCamera.AbortExposure;
    Finternalguider.Info:='';
  end;
  InternalguiderGuiding:=false;
  InternalguiderCalibrating:=false;
  InternalguiderCalibratingBacklash:=false;
  InternalguiderCapturingDark:=false;
  InternalguiderCalibratingMeridianFlip:=false;
  Finternalguider.ButtonLoop.enabled:=true;
  Finternalguider.ButtonCalibrate.enabled:=true;
  Finternalguider.ButtonGuide.enabled:=true;
  Finternalguider.ButtonDark.enabled:=true;
  Finternalguider.cbSpectro.enabled:=true;
  Finternalguider.cbGuideLock.enabled:=true;
  Finternalguider.led.Brush.Color:=clGray;
  SetStatus('Stopped',GUIDER_IDLE);
end;

procedure T_autoguider_internal.InternalguiderRecoverCamera;
begin
  // we go here after a guide camera error
  // try to recover by aborting the current exposure and start another one
  if InternalguiderRunning and (not (InternalguiderCalibrating or InternalguiderCalibratingBacklash)) and (not FRecoveringCamera) then begin
   if FRecoveringCameraCount<5 then begin
    inc(FRecoveringCameraCount);
    FRecoveringCamera:=true;
    PulseGuiding:=false;
    FCamera.AbortExposureButNotSequence;
    msg('Try to recover from guide camera error, please check the USB connection',1);
    WriteLog('INFO: Try to recover from guide camera error');
    wait(1);
    Application.QueueAsyncCall(@StartGuideExposureAsync,0);
   end
   else begin
     msg('Too much guide camera error, stop guiding',0);
     WriteLog('INFO: Too much guide camera error, stop guiding');
     InternalguiderStop;
   end;
  end;
end;


function  T_autoguider_internal.angular_distance(a1,a2:double):double;//in radians
begin
  result:=(a2-a1);
  if result>pi then result:=result-2*pi
  else
  if result<-pi then result:=result+2*pi;
end;

procedure T_autoguider_internal.InternalguiderCalibrate;
begin
  if AllDevicesConnected=false then
  begin
    msg('Internal guider: '+rsSomeDefinedD,1);
    InternalguiderStop;
    exit;
  end;
  if FCamera.Status<>devConnected then
  begin
    msg('Internal guider: Guide camera not connected!',1);
    SetStatus('Guide camera not connected',GUIDER_ALERT);
    exit;
  end;
  if Fmount.canpulseguide=false then
  begin
    msg('Abort, mount does not support pulse guiding!',1);
    InternalguiderStop;
    exit;
  end;
  if abs(mount.Dec)>60 then
  begin
    msg('Abort, calibration at high declination is not possible!',1);
    InternalguiderStop;
    exit;
  end;

  StopInternalguider:=false;
  InternalguiderCalibrating:=true;
  SetStatus('Start Calibration',GUIDER_BUSY);

  finternalguider.trend_message('Guider is in calibration mode.','This will take a few minutes.','');

  if mount.Tracking=false then
  begin
    msg('Start tracking. Wait 20 seconds',3);
    mount.Track;//start tracking
    wait(20);
  end;

  Calthecos:=cos(mount.Dec*pi/180); if Calthecos=0 then Calthecos:=0.00000001; //prevent dividing by zero

  InternalguiderCalibrationDirection:=1;
  InternalguiderCalibrationStep:=0;

  InternalguiderLoop;
end;

procedure T_autoguider_internal.InternalCalibration;
var drift,unequal                            : double;
    saveInternalguiderCalibratingMeridianFlip: boolean;
    msgA, msgB,msgC,pattern                  : string;
            procedure StopError;
            begin
              InternalguiderStop;
              msg('Calibration error',1);
              SetStatus('Calibration Failed',GUIDER_ALERT);
              raise exception.Create('Calibration error');
            end;
begin
  try
    case InternalguiderCalibrationDirection of
    1:begin  //EAST, measure pulse guide speed
        case InternalguiderCalibrationStep of
          0: begin
               if FMount.CanSetGuideRates and Finternalguider.ForceGuideSpeed.Checked then begin
                 //force speed
                 Fmount.GuideRateRa:=Finternalguider.GuideSpeedRA.Value*360/(24*60*60);  //set pulse speed in degree per seconds
                 Fmount.GuideRateDe:=Finternalguider.GuideSpeedDEC.Value*360/(24*60*60); //DEC option use same unit [* sidereal] as RA rate
               end
               else begin
                 // read speed
                 Finternalguider.GuideSpeedRA.Value:=FMount.GuideRateRa*(24*60*60)/360;
                 Finternalguider.GuideSpeedDEC.Value:=FMount.GuideRateDe*(24*60*60)/360;
               end;
               msg('Calibration guide speed: '+FormatFloat(f2,Finternalguider.GuideSpeedRA.Value)+'/'+FormatFloat(f2,Finternalguider.GuideSpeedDEC.Value),3);
               CalibrationDuration:=round(Finternalguider.InitialCalibrationStep.Value/1.5); //duration of pulse guiding
               InternalguiderCalibrationStep:=1;
               InternalCalibration; // iterate without new image

               if ((mount.isGem) and (mount.PierSide=pierWest) and (finternalguider.ReverseDec=false)) then //Correct measurement for reverse Dec action by merdian flip
               begin //Swap definition north and south
                 north:=1;
                 south:=0
               end
               else
               begin //Fork mount or mount software keeps action button north north
                 north:=0;
                 south:=1
               end;
             end;
          1: begin
               CalibrationDuration:=round(CalibrationDuration*1.5);
               msg('Testing pulse guiding East for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
               InternalCalibrationInitialize:=true;
               CalEastRa1:=mount.ra;//mount right ascension at start of east calibration
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
               MinimumDrift:=max(5,min(25,3*CurrentHFD));
               mount.PulseGuide(2,CalibrationDuration {duration msec} );  // 0=north, 1=south, 2 East, 3 West

               WaitPulseGuiding(CalibrationDuration);
               InternalguiderCalibrationStep:=2;
             end;
          2: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
               drift:=sqrt(sqr(driftX)+sqr(driftY));//  For image with north up and east left, driftX become negative.


               msg('Measured drift ' + FormatFloat(f1,drift)+' px',3);
               if ((drift>MinimumDrift) or (CalibrationDuration>20000)) then begin// OK, next direction
                 if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
                 pulsegainEast:=drift*1000/(CalibrationDuration*Calthecos); // [px*cos(dec)/sec]
                 paEast:=arctan2(driftY,driftX);//-pi..pi, For north up and east left this gives zero angle
                 InternalguiderCalibrationDirection:=2;
                 InternalguiderCalibrationStep:=0;
               end
               else begin // retry with bigger pulse
                 InternalguiderCalibrationStep:=1;
               end;
               CalEastRa2:=Mount.ra; //mount right ascension at end of east calibration
             end;
        end;
      end;
    2:begin  //WEST, measure pulse guide. Use same duration as East
        case InternalguiderCalibrationStep of
          0: begin
               msg('Testing pulse guiding West for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
               mount.PulseGuide(3,CalibrationDuration {duration msec} );  // 0=north, 1=south, 2 East, 3 West

               WaitPulseGuiding(CalibrationDuration);
               InternalguiderCalibrationStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
               drift:=sqrt(sqr(driftX)+sqr(driftY)); //For image with north up and east left, driftX become positive.

               msg('Measured drift ' + FormatFloat(f1,drift)+' px',3);
               pulsegainWest:=drift*1000/(CalibrationDuration*Calthecos); // [px*cos(dec)/sec]
               msg('Internal guider calibration:  Pulse gain measured East/West: '+ floattostrF(pulsegainEast,ffgeneral,0,2)+'/'+ floattostrF(pulsegainWest,ffgeneral,0,2)+' [px*cos(δ)/sec], Camera angle: '+floattostrF(paEast*180/pi,ffgeneral,3,1)+'°',3);
               InternalguiderCalibrationDirection:=3;
               InternalguiderCalibrationStep:=0;
               InternalCalibration;  // iterate without new image
             end;
        end;
      end;
    3:begin  //NORTH measure pulse guide speed.
        case InternalguiderCalibrationStep of
          0: begin

               msg('Remove backlash North',3);
               if Finternalguider.BacklashCompensation then begin
                 // backlash already calibrated, use the defined value
                 CalDecBacklash:=Finternalguider.DecBacklash;
               end
               else begin
                 // use a safe value
                 CalDecBacklash:=min(Finternalguider.LongestPulse,max(300,3*Finternalguider.InitialCalibrationStep.Value));
               end;
               InternalCalibrationInitialize:=true;
               CalNorthDec1:=Mount.Dec; //mount declination at start of north calibration
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               mount.PulseGuide(north,CalDecBacklash);
               WaitPulseGuiding(CalDecBacklash);
               InternalguiderCalibrationStep:=1;
               BacklashStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               msg('Backlash step '+inttostr(BacklashStep)+' drift '+FormatFloat(f1,drift),3);
               if drift<MinimumDrift then begin
                 // more backlash
                 inc(BacklashStep);
                 if BacklashStep>30 then begin
                   msg('Mount do not move after '+inttostr(BacklashStep-1)+' steps, try to fix mechanical backlash or increase "Longest guide pulse"',3);
                   StopError;
                 end
                 else begin
                   mount.PulseGuide(north,CalDecBacklash);
                   WaitPulseGuiding(CalDecBacklash);
                 end;
               end
               else begin
                 // start North measurement
                 CaldriftOld:=0;
                 CalibrationDuration:=round(Finternalguider.InitialCalibrationStep.Value/1.5); //duration of pulse guiding
                 InternalguiderCalibrationStep:=2;
                 InternalCalibration;  // iterate without new image
               end;
             end;
          2: begin
               CalibrationDuration:=round(CalibrationDuration*1.5);
               msg('Testing pulse guiding North for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
               InternalCalibrationInitialize:=true;//for measure drift
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
               mount.PulseGuide(north,CalibrationDuration {duration msec} );  // 0=north, 1=south, 2 East, 3 West
               WaitPulseGuiding(CalibrationDuration);
               InternalguiderCalibrationStep:=3;
             end;
          3: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
               drift:=sqrt(sqr(driftX)+sqr(driftY));

               msg('Measured drift ' + FormatFloat(f1,drift)+' px',3);
               if ( ((drift>MinimumDrift) and (CaldriftOld>MinimumDrift/1.5)) or (CalibrationDuration>20000)) then begin// OK both drift and CaldriftOld show movement so backlash must be fully gone. Go next direction
                 if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
                 paNorth:=arctan2(driftY,driftX); // Relative to the positive X axis and CCW
                 Caltheangle:=angular_distance(paEast,paNorth);// CCW angles, calculate angle North relative to West. Range [-pi..+pi]

                 if  Caltheangle<0 then //flipped?
                   Calflip:=+1  // Normal. If North is up then East is left in the image
                 else
                   Calflip:=-1; // Flipped image. E.g.if North is up then East is on the right side}

                 Orthogonality:=abs(Caltheangle*rad2deg)-90;

                 CalNorthDec2:=Mount.Dec; //mount declination at end of north calibration

                 pulsegainNorth:=Calflip*drift*1000/(CalibrationDuration); // [px/sec]
                 InternalguiderCalibrationDirection:=4;
                 InternalguiderCalibrationStep:=0;
                 InternalCalibration;  // iterate without new image
               end
               else begin // retry with bigger pulse
                 CaldriftOld:=drift;
                 InternalguiderCalibrationStep:=2;
               end;
             end;
        end;
      end;
    4:begin  //SOUTH, measure pulse guide speed.
        case InternalguiderCalibrationStep of
          0: begin
               msg('Remove backlash South',3);
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               mount.PulseGuide(south,CalDecBacklash);
               WaitPulseGuiding(CalDecBacklash);
               InternalguiderCalibrationStep:=1;
               BacklashStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               msg('Backlash step '+inttostr(BacklashStep)+' drift '+FormatFloat(f1,drift),3);
               if drift<MinimumDrift then begin
                 // more backlash
                 inc(BacklashStep);
                 if BacklashStep>30 then begin
                   msg('Mount do not move after '+inttostr(BacklashStep-1)+' steps, try to fix mechanical backlash or increase "Longest guide pulse"',3);
                   StopError;
                 end
                 else begin
                   mount.PulseGuide(south,CalDecBacklash);
                   WaitPulseGuiding(CalDecBacklash);
                 end;
               end
               else begin
                 // start South measurement
                 CalCount:=0;
                 CaldriftOld:=0;
                 InternalguiderCalibrationStep:=2;
                 msg('Testing pulse guiding South for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
                 InternalCalibration;  // iterate without new image
               end;
             end;
          2: begin
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
               mount.PulseGuide(south,CalibrationDuration {duration msec} );  // 0=north, 1=south, 2 East, 3 West
               WaitPulseGuiding(CalibrationDuration);
               InternalguiderCalibrationStep:=3;
             end;
          3: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               inc(CalCount);
               msg('Measured drift ' + FormatFloat(f1,drift)+' px',3);
               if ((CaldriftOld>(MinimumDrift/1.5)) or (Calcount>=4)) then begin  //previous cycle showed movement so backlash must be fully gone
                 if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
                 pulsegainSouth:=Calflip*drift*1000/(CalibrationDuration); // [px*cos(dec)/sec]   Flipped is already measured
                 msg('Internal guider calibration:  Pulse gain measured North/South: '+ floattostrF(pulsegainNorth,ffgeneral,0,2)+'/'+ floattostrF(pulsegainSouth,ffgeneral,0,2)+' [px/sec]',3);


                 //measure first minimum pulse north. Backlash is gone after testing speed north
                 InternalguiderCalibrationDirection:=5;
                 InternalguiderCalibrationStep:=0;
                 InternalCalibration;  // iterate without new image
               end
               else begin
                 CaldriftOld:=drift;
                 InternalguiderCalibrationStep:=2; //repeat loop until CaldriftOld>2 and backlash is gone
               end;
             end;
        end;
      end;
    5:begin  //Display findings
        if mount.PierSide=pierWest then  //measured west or east ??
           finternalguider.pier_side:='E'
        else if mount.PierSide=pierEast then
          finternalguider.pier_side:='W'
        else
          finternalguider.pier_side:='Unk';

        finternalguider.PA:=paEast*180/pi; // this is the relative angle between the image and the mount.
        finternalguider.pulsegainEast:=pulsegainEast;
        finternalguider.pulsegainWest:=pulsegainWest;
        finternalguider.pulsegainNorth:=pulsegainNorth;
        finternalguider.pulsegainSouth:=pulsegainSouth;

        finternalguider.InverseSolarTracking:=(CalNorthDec1>CalNorthDec2); //north calibration moved south, inverse solar tracking

        //report the uncorrected mount behaviour
        if mount.PierSide=pierWest then pattern:='Mount pulse guide pattern in the East is ' else pattern:='Mount pulse guide pattern in the West is ';
        if CalEastRa2>CalEastRa1 then pattern:=pattern+'E->E' else pattern:=pattern+'E->W';;// In which direction does the mount go after pulse East
        if ((CalNorthDec2>CalNorthDec1) xor (North<>0)) then pattern:=pattern+', N->N' else pattern:=pattern+', N->S';// In which direction does the mount go after pulse North (uncorrected).
        msg(pattern,3);


        finternalguider.pixel_size:=Finternalguider.GuideSpeedRA.Value*15*2/(pulsegainEast+pulsegainWest);//Use the set pulse speed

        if finternalguider.measure_method2.checked then begin  //Alternative method. Measure pixel size in arc seconds by stopping tracking
          InternalguiderCalibrationDirection:=6;
          InternalguiderCalibrationStep:=0;
          InternalCalibration;  // iterate without new image
        end
        else begin
          InternalguiderCalibrationDirection:=7;
          InternalguiderCalibrationStep:=0;
          InternalCalibration;  // iterate without new image
        end;
      end;
    6:begin //Optional, measure pixel scale guider camera in arcseconds. Do not initialize, use the position from the South measurement
        case InternalguiderCalibrationStep of
          0: begin
               msg('Stop tracking a few times for 0.5 seconds to measure pixel scale',1);
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
               mount.AbortMotion;// stop to measure the pixel scale. Assume scale is 1"/px or larger. This results in 7.5 pixel drift max.
               sleep(500);
               mount.Track;//start tracking again
               Calnrtest:=1;
               InternalguiderCalibrationStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then //sync the drift measurement with the new position.
               begin
                 msg('Used alternative method to calculate the pixel scale.',1);
                 InternalguiderCalibrationDirection:=7;
                 InternalguiderCalibrationStep:=0;
                 InternalCalibration;  // iterate without new image
               end;
               if Calnrtest<8 then begin
                 mount.AbortMotion;// stop to measure the pixel scale. Assume scale is 1"/px or larger. This results in 7.5 pixel drift max.
                 sleep(500);
                 mount.Track;//start tracking again
                 inc(Calnrtest);
                 InternalguiderCalibrationStep:=1;
               end
               else begin
                 InternalguiderCalibrationStep:=2;
               end;
             end;
          2: begin
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               finternalguider.pixel_size:=Calnrtest*0.5*15/drift;
               msg('Total drift: '+ floattostrF(drift,ffgeneral,0,2)+ ' pixels after '+inttostr(Calnrtest)+ ' tracking stops of 0.5 seconds. Estimated pixel size '+floattostrF(finternalguider.pixel_size,ffgeneral,0,2)+' "/px' ,3);
               InternalguiderCalibrationDirection:=7;
               InternalguiderCalibrationStep:=0;
               InternalCalibration;  // iterate without new image
             end;
        end;
      end;
    7:begin

        msgC:='';
        unequal:=abs(1-(pulsegainEast/pulsegainWest));
        if unequal>0.2 then begin msgA:='Warning unequal East/West pulse gain!'; msg(msgA,1); msgC:='Unequal'; end else msgA:='';
        unequal:=abs(1-(pulsegainNorth/pulsegainSouth));
        if unequal>0.2 then begin msgB:='Warning unequal North/South pulse gain!'; msg(msgB,1); msgC:='Unequal'; end else msgB:='';

        if abs(Orthogonality)>15 then begin  // 15° tolerance on measured orthogonality
           msg('Warning Orthogonality error = '+FormatFloat(f1,abs(Orthogonality))+' degrees',1);
           msgC:=trim(msgC+' '+'Ortho');
        end;

        if msgC='' then msgC:='None';

        Finternalguider.CalDate.Text:=FormatDateTime(dateisoshort,now);
        Finternalguider.CalDeclination.Text:=FormatFloat(f1,CalNorthDec1);
        Finternalguider.CalBinning.Text:=IntToStr(Finternalguider.Binning.Value);
        Finternalguider.CalRAspeed.Text:=FormatFloat(f1,Finternalguider.GuideSpeedRA.Value);
        Finternalguider.CalDECspeed.Text:=FormatFloat(f1,Finternalguider.GuideSpeedDEC.Value);
        Finternalguider.CalIssue.Text:=msgC;

        msg('Ready to guide!',1);
        finternalguider.trend_message('Calibration is ready.',msgA,msgB);
        saveInternalguiderCalibratingMeridianFlip:=InternalguiderCalibratingMeridianFlip;
        InternalguiderStop;
        SetStatus('Calibration Complete',GUIDER_IDLE);
        InternalguiderCalibratingMeridianFlip := saveInternalguiderCalibratingMeridianFlip;
      end;
  end;
  except
  end;
end;

procedure T_autoguider_internal.InternalguiderCalibrateBacklash;
begin
  if AllDevicesConnected=false then
  begin
    msg('Internal guider: '+rsSomeDefinedD,1);
    InternalguiderStop;
    exit;
  end;
  if FCamera.Status<>devConnected then
  begin
    msg('Internal guider: Guide camera not connected!',1);
    SetStatus('Guide camera not connected',GUIDER_ALERT);
    exit;
  end;
  if Fmount.canpulseguide=false then
  begin
    msg('Abort, mount does not support pulse guiding!',1);
    InternalguiderStop;
    exit;
  end;
  if abs(mount.Dec)>60 then
  begin
    msg('Abort, calibration at high declination is not possible!',1);
    InternalguiderStop;
    exit;
  end;
  if Finternalguider.pier_side='N/A' then
  begin
    msg('Run the guider calibration before the backlash calibration.',1);
    InternalguiderStop;
    exit;
  end;

  StopInternalguider:=false;
  InternalguiderCalibratingBacklash:=true;
  SetStatus('Start Calibration',GUIDER_BUSY);

  finternalguider.trend_message('Guider is in backlash calibration mode.','This will take a few minutes.','');

  if mount.Tracking=false then
  begin
    msg('Start tracking. Wait 20 seconds',3);
    mount.Track;//start tracking
    wait(20);
  end;

  InternalguiderCalibrationDirection:=1;
  InternalguiderCalibrationStep:=0;

  InternalguiderLoop;
end;

procedure T_autoguider_internal.BacklashCalibration;
var drift : double;
            procedure StopError;
            begin
              InternalguiderStop;
              msg('Calibration error',1);
              SetStatus('Calibration Failed',GUIDER_ALERT);
              raise exception.Create('Calibration error');
            end;
begin
  try
    case InternalguiderCalibrationDirection of
    1:begin  //Move NORTH to remove backlash
        case InternalguiderCalibrationStep of
          0: begin
               msg('Remove backlash North',3);
               if Finternalguider.BacklashCompensation then begin
                 // backlash already calibrated, use the defined value
                 CalDecBacklash:=Finternalguider.DecBacklash;
               end
               else begin
                 // use a safe value
                 CalDecBacklash:=min(Finternalguider.LongestPulse,max(300,3*Finternalguider.InitialCalibrationStep.Value));
               end;
               MinimumDrift:=max(5,min(25,3*CurrentHFD));
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               mount.PulseGuide(north,CalDecBacklash);
               WaitPulseGuiding(CalDecBacklash);
               InternalguiderCalibrationStep:=1;
               BacklashStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               msg('Backlash step '+inttostr(BacklashStep)+' drift '+FormatFloat(f1,drift),3);
               if drift<MinimumDrift then begin
                 // more backlash
                 inc(BacklashStep);
                 if BacklashStep>30 then begin
                   msg('Mount do not move after '+inttostr(BacklashStep-1)+' steps, try to fix mechanical backlash or increase "Longest guide pulse"',3);
                   StopError;
                 end
                 else begin
                   mount.PulseGuide(north,CalDecBacklash);
                   WaitPulseGuiding(CalDecBacklash);
                 end;
               end
               else begin
                 // start South backlash measurement
                 CalibrationDuration:=round(3*1000/finternalguider.pulsegainNorth); //duration for 3 pixel move
                 InternalguiderCalibrationDirection:=2;
                 InternalguiderCalibrationStep:=0;
                 BacklashCalibration;  // iterate without new image
               end;
             end;
        end;
      end;
    2:begin  //SOUTH, measure pulse guide speed.
        case InternalguiderCalibrationStep of
          0: begin
               msg('Measure backlash South',3);
               InternalCalibrationInitialize:=true;
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               mount.PulseGuide(south,CalibrationDuration);
               WaitPulseGuiding(CalibrationDuration);
               InternalguiderCalibrationStep:=1;
               BacklashStep:=1;
             end;
          1: begin
               if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;
               drift:=sqrt(sqr(driftX)+sqr(driftY));
               msg('Backlash step '+inttostr(BacklashStep)+' drift '+FormatFloat(f1,drift),3);
               if drift<MinimumDrift then begin
                 // more backlash
                 inc(BacklashStep);
                 if BacklashStep>50 then begin
                   msg('Mount do not move after '+inttostr(BacklashStep-1)+' steps, try to fix mechanical backlash"',3);
                   StopError;
                 end
                 else begin
                   mount.PulseGuide(south,CalibrationDuration);
                   WaitPulseGuiding(CalibrationDuration);
                 end;
               end
               else begin
                 // measurement completed
                 finternalguider.DecBacklash:=CalibrationDuration*(BacklashStep-1);
                 msg('Measured Declination backlash '+IntToStr(finternalguider.DecBacklash)+ ' milliseconds',3);
                 finternalguider.trend_message('Backlash measurement complete','','');
                 msg('Check "Use backlash compensation" in the Options tab to apply.',3);
                 InternalguiderStop;
                 SetStatus('Calibration Complete',GUIDER_IDLE);
               end;
             end;
        end;
      end;
  end;
  except
  end;
end;

procedure T_autoguider_internal.InternalguiderCaptureDark;
begin
  if FCamera.Status<>devConnected then
  begin
    msg('Internal guider: Guide camera not connected!',1);
    exit;
  end;

  StopInternalguider:=false;
  InternalguiderCapturingDark:=true;
  SetStatus('Capture dark',GUIDER_BUSY);
  InternalguiderLoop;
end;

procedure T_autoguider_internal.NewImageReceived;
begin
  // we receive an image, reset recovery count
  if FRecoveringCameraCount>0 then begin
    FRecoveringCameraCount:=0;
    msg('Guide camera recovered',1);
    WriteLog('INFO: Guide camera recovered');
  end;
end;

procedure  T_autoguider_internal.ShowImgInfo;
var
  i,fitsx,fitsy,stepsize,xsize,ysize,star_counter: integer;
  hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr,flux,min_SNR,min_HFD,maxSNR,y,peak : double;
const
    searchA=28;//square search area
    overlap=6;
    maxstars=1000;
begin
  star_counter:=0;
  stepsize:=searchA-overlap;//some overlap


  xsize:=guidefits.HeaderInfo.naxis1;// width image
  ysize:=guidefits.HeaderInfo.naxis2;// height image

  min_SNR:=finternalguider.minSNR;//make local to reduce some CPU load
  min_HFD:=finternalguider.minHFD;//make local to reduce some CPU load
  maxSNR:=0;
  peak:=0;

  // Divide the image in square areas. Try to detect a star in each area.
    mean_hfd:=0;
    fitsy:=stepsize div 2;
    repeat
      fitsx:=stepsize div 2;
      repeat
        guidefits.GetHFD3(fitsX,fitsY,searchA,true{autocenter},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);//find a star in this segment. Auto center is true

        if (vmax+bg)>peak then peak:=vmax+bg;
        if ((snr>Min_SNR) and (hfd1>Min_HFD) and (abs(fitsX-xc)<stepsize div 2) and (abs(fitsY-yc)<stepsize div 2) and (star_counter<maxstars))  then //detection and no other area closer
        begin // star in this area
          mean_hfd:=mean_hfd+hfd1;
          if (snr>maxSNR) then begin
            maxSNR:=snr;
          end;
          inc(star_counter);
        end;
        inc(fitsx,stepsize);
      until fitsx>=xsize-1+stepsize div 2;;
      inc(fitsy,stepsize);
    until fitsy>=ysize-1+stepsize div 2;
    if star_counter>0 then
    begin
      mean_hfd:=mean_hfd/star_counter;
    end;
    finternalguider.Info:=IntToStr(star_counter)+' stars, HFD: '+FormatFloat(f1,mean_hfd)+', SNR: '+FormatFloat(f0,maxSNR)+', PEAK: '+FormatFloat(f0,peak);
end;

function T_autoguider_internal.SpectroSetTarget(TargetRa,TargetDec: double):boolean;
begin
// set Spectro target position ra,de J2000 in hh.hhhh dd.dddd
// this position is used the next time guiding is started
  if finternalguider.SpectroFunctions and finternalguider.cbUseAstrometry.Checked
     and(cdcwcs_sky2xy<>nil) and (TargetRa<>NullCoord)and(TargetDec<>NullCoord) then begin
    FSpectroTarget.RA:=TargetRa;
    FSpectroTarget.DEC:=TargetDec;
    FSpectroTarget.valid:=true;
    result:=true;
  end
  else begin
    FSpectroTarget.RA:=NullCoord;
    FSpectroTarget.DEC:=NullCoord;
    FSpectroTarget.valid:=false;
    result:=false;
  end;
end;

function T_autoguider_internal.SelectSpectroTarget:boolean;
var n,bin,gain,offset: integer;
    exp,xt,yt: double;
    c: TcdcWCScoord;
begin
// set Spectro target at the position set by SpectroSetTarget
// compute the target x,y position in the guide image
// so the target is moved to the slit when the guiding is started
  if finternalguider.SpectroFunctions and finternalguider.cbUseAstrometry.Checked
     and(cdcwcs_sky2xy<>nil) and FSpectroTarget.valid and (FSpectroTarget.RA<>NullCoord)and(FSpectroTarget.DEC<>NullCoord) then begin
    result:=false;
    FSpectroTarget.valid:=false; // use coordinates only once
    exp:=finternalguider.AstrometryExp.value;
    bin:=finternalguider.Binning.Value;
    gain:=finternalguider.Gain.Value;
    offset:=finternalguider.Offset.Value;
    FCamera.ObjectName:=rsGuide;
    FCamera.GuidePixelScale:=finternalguider.pixel_size;
    if FCamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry,gain,offset) then begin
      FAstrometry.SolveGuideImage(true);
      if Fastrometry.LastResult then begin
         c.ra:=FSpectroTarget.RA*15;
         c.dec:=FSpectroTarget.DEC;
         n:=cdcwcs_sky2xy(@c,1);
         if n=0 then begin
           xt:=c.x;
           yt:=c.y;
           msg('Spectro target x='+FormatFloat(f1,xt)+' y='+FormatFloat(f1,yt),3);
           if Finternalguider.GuideLock then begin
             // single star guiding, set the search position for a bright star
             finternalguider.GuideLockNextX:=round(xt);
             finternalguider.GuideLockNextY:=round(yt);
             finternalguider.DrawSettingChange:=true;
           end
           else begin
             // multi-star guiding, set the guide offset to move the target on the slit
             OffsetFromTarget:=true;
             finternalguider.OffsetX:=-finternalguider.LockX+xt;
             finternalguider.OffsetY:=finternalguider.LockY-yt;
           end;
           result:=true;
         end
         else begin
           msg('Spectro target out of frame x='+FormatFloat(f1,c.x)+' y='+FormatFloat(f1,c.y),0);
           result:=false;
         end;
      end
      else if Finternalguider.GuideLock then begin
         // try to find the brightest star instead
         msg('Astrometry fail, try to use the brigthest star instead of the specified coordinates',1);
         finternalguider.GuideLockNextX:=-10;
         finternalguider.GuideLockNextY:=-10;
         result:=false;
      end
      else begin
        // nothing can be done
        msg('Fail to solve guide image',0);
        result:=false;
      end;
    end
    else begin
      msg('Guide camera exposure fail',0);
      result:=false;
    end;
  end
  else begin
    result:=false;
  end;
end;

end.

