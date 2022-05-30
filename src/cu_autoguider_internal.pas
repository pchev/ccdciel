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

uses cu_autoguider, u_global, u_utils, math, fu_internalguider, indiapi,
  u_translation, Graphics, Forms, Classes, SysUtils;

type
  star_position=record x1,y1,x2,y2,flux: double; end;//for internal guider
  star_position_array= array of star_position;//for internal guider


  T_autoguider_internal = class(T_autoguider)
  private
    InternalguiderInitialize: boolean;
    pulseRA,pulseDEC  : integer;
    driftX,driftY,driftRA,driftDec,correctionRA,correctionDEC, PactionRA,PactionDEC,Guidethecos,old_PactionRA,old_PactionDEC : double;
    xy_trend : xy_guiderlist;{fu_internalguider}
    InternalguiderCalibrationDirection,InternalguiderCalibrationStep: integer;
    InternalCalibrationInitialize: boolean;
    paEast, paNorth, pulsegainEast,pulsegainWest,pulsegainNorth,pulsegainSouth,Calthecos,Caltheangle,CaldriftOld : double;
    CalibrationDuration,Calflip,CalCount,Calnrtest: integer;
    xy_array,xy_array_old : star_position_array;//internal guider for measure drift
    Fguidespeed: double;
    function  measure_drift(var initialize: boolean; out drX,drY :double) : integer;
    Procedure StartGuideExposure;
    procedure InternalguiderStartAsync(Data: PtrInt);
    function  WaitPulseGuiding(pulse:double): boolean;
    procedure SetStatus(aStatus: string ; aState: TAutoguiderState);
  protected
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure Terminate;
    procedure StarLostTimerTimer(Sender: TObject); override;
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
    procedure InternalguiderLoop;
    procedure InternalguiderStart;
    procedure InternalguiderStop;
    procedure InternalguiderCalibrate;
    procedure InternalAutoguiding;
    procedure InternalCalibration;
    Procedure StartGuideExposureAsync(Data: PtrInt);
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
  end;

implementation

const
   nrpointsTrend=50; //number of trend points plotted
   max_duration=2500;//max duration guide puls in milliseconds


Constructor T_autoguider_internal.Create ;
begin
  inherited Create;
  FAutoguiderType:=agINTERNAL;
  FStatus:=rsInternal;
  FState:=GUIDER_IDLE;
  FRunning:=true;
end;

Destructor T_autoguider_internal.Destroy;
begin
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
  { #todo : dither }
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

procedure T_autoguider_internal.Pause(onoff:boolean; settle:boolean=true);
begin
  { #todo : guide pause }
end;

procedure T_autoguider_internal.Dither(pixel:double; raonly:boolean; waittime:double);
begin
  { #todo : dither }
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

/////////////////////////////////////////////////////////////////////////////////////////


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

function  T_autoguider_internal.measure_drift(var initialize:boolean; out drX,drY :double) : integer;// ReferenceX,Y indicates the total drift, drX,drY to drift since previouse call. Arrays old_xy_array,xy_array are for storage star positions
var
  i,fitsx,fitsy,stepsize,xsize,ysize,star_counter,match_counter,r, rxc,ryc,len: integer;
  drift_arrayX,drift_arrayY : array of double;
  hfd1,star_fwhm,vmax,bg,bgdev,xc,yc,snr,flux,fluxratio  : double;
const
    searchA=28;//square search area
    overlap=14;
    maxstars=1000;
begin
  result:=1;// Assume no stars detected
{  Application.ProcessMessages; if StopInternalguider then
  begin
    msg('Guider stop pressed.',1);
    result:=2;//mark stop with value 2
    exit;
  end;  }
  star_counter:=0;
  stepsize:=searchA-overlap;//some overlap

  FGuideBmp.Canvas.Pen.Color:=clYellow;
  FGuideBmp.Canvas.Pen.Mode:=pmMerge;
  FGuideBmp.Canvas.Pen.Style:=psSolid;
  FGuideBmp.Canvas.Pen.Width:=1;

  xsize:=guidefits.HeaderInfo.naxis1;// width image
  ysize:=guidefits.HeaderInfo.naxis2;// height image

  if initialize then
    setlength(xy_array,maxstars);

  // Divide the image in square areas. Try to detect a star in each area. Store the star position and flux in the xy_array
  if initialize then
  begin
    fitsy:=stepsize div 2;
    repeat
      fitsx:=stepsize div 2;
      repeat

        guidefits.GetHFD2(fitsX,fitsY,searchA,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);{find a star in this segment}

        if ((snr>10) and (abs(fitsX-xc)<stepsize div 2) and (abs(fitsY-yc)<stepsize div 2) and (star_counter<maxstars))  then //detection and no other area closer
        begin // star in this area
          xy_array[star_counter].x1:=xc;//store initial measured position for recovering if star is lost
          xy_array[star_counter].y1:=yc;

          xy_array[star_counter].x2:=xc;//store measured star position
          xy_array[star_counter].y2:=yc;
          xy_array[star_counter].flux:=flux;
          inc(star_counter);

          // Mark star area
          r:=round(hfd1*3);
          rxc:=round(xc);
          ryc:=round(yc);
          FGuideBmp.Canvas.Frame(rxc-r,ryc-r,rxc+r,ryc+r);

        end
        else
        begin //no star in this area
          xy_array[star_counter].x2:=0;
          xy_array[star_counter].y2:=0;
          xy_array[star_counter].flux:=0;
        end;

        inc(fitsx,stepsize);
      until fitsx>=xsize-1+stepsize div 2;;
      inc(fitsy,stepsize);
    until fitsy>=ysize-1+stepsize div 2;
    setlength(xy_array,star_counter);
    setlength(xy_array_old,star_counter);//for later
  end
  else
  begin //second, third ... call
    for i:=0 to length(xy_array_old)-1 do
    begin
      if xy_array_old[i].flux<>0 then //keep tracking if star drifts away
        guidefits.GetHFD2(round(xy_array_old[i].x2),round(xy_array_old[i].y2),searchA{area},xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false) {find a star at previous position. So keep tracking while it is drifting}
      else // try in initial area
        guidefits.GetHFD2(round(xy_array_old[i].x1),round(xy_array_old[i].y1),searchA,xc,yc,bg,bgdev,hfd1,star_fwhm,vmax,snr,flux,false);{find a star in the orginal segment}

      if snr>10 then //detection
      begin // star in this area
        xy_array[i].x2:=xc;
        xy_array[i].y2:=yc;
        xy_array[i].flux:=flux;
        inc(star_counter);

        // Mark star area
        r:=round(hfd1*3);
        rxc:=round(xc);
        ryc:=round(yc);
        FGuideBmp.Canvas.Frame(rxc-r,ryc-r,rxc+r,ryc+r);

      end
      else
      begin //Star lost temporary
        xy_array[i].flux:=0;
      end;
    end;
  end;
  if star_counter<1 then
  begin
    msg('No stars detected!',1);
    initialize:=true;//return initialize=true for fresh restart next call.
    exit;
  end
  else
    msg(inttostr(star_counter)+' stars detected',3);

  // calculate movement in each area
  match_counter:=0;
  if ((initialize=false) and (length(xy_array_old)>0)) then//not empthy, second round or later
  begin
    len:=length(xy_array_old);
    setlength(drift_arrayX,len);
    setlength(drift_arrayY,len);
    for i:=0 to len-1 do
    begin
      fluxratio:=xy_array_old[i].flux/(xy_array[i].flux+0.001);
      if  ((fluxratio>1/1.5) and (fluxratio<1.5)) then //star flux difference is within 50%
      begin
        drift_arrayX[match_counter]:=xy_array[i].x2 - xy_array_old[i].x1; //drift in pixels relative to initial measurement x1,y1
        drift_arrayY[match_counter]:=xy_array[i].y2 - xy_array_old[i].y1;
        inc(match_counter);
      end;
    end;
//    application.processmessages;

    if match_counter/star_counter<0.5 then  //second round and 50% of stars are still in the area
      msg('Guider, warning lost track or exposure time changed!',2); //more then 7.5 pixels drift in one cycle

    //Remove outliers and calculate mean drift in X and Y.
    get_best_mean(drift_arrayX,match_counter {length},drX );
    get_best_mean(drift_arrayY,match_counter {length},drY );
  end;

  {copy xy_array to xy_array_old}
  for i:=0 to length(xy_array_old)-1 do
      xy_array_old[i]:=xy_array[i];
  initialize:=false;//succes, first data collected
  result:=0; //good result
end;

procedure T_autoguider_internal.InternalguiderLoop;
begin
  SetStatus('Looping Exposures',GUIDER_IDLE);
  StopInternalguider:=false;
  InternalguiderRunning:=true;
  Finternalguider.ButtonLoop.enabled:=false;
  Finternalguider.ButtonCalibrate.enabled:=false;
  Finternalguider.ButtonGuide.enabled:=false;
  Fguidespeed:=FMount.GuideRateRa*3600/siderealrate;
  if Fguidespeed=0 then Fguidespeed:=0.5;
  StartGuideExposure;
end;

Procedure T_autoguider_internal.StartGuideExposureAsync(Data: PtrInt);
begin
  StartGuideExposure;
end;

Procedure T_autoguider_internal.StartGuideExposure;
var e: double;
    binx,biny: integer;
//    buf,f: string;
//    p,i,x,y,w,h,sx,sy,sw,sh: integer;
begin
if (FCamera.Status=devConnected) then begin
  // check exposure time
  e:=finternalguider.Exposure.value;
  binx:=finternalguider.Binning.Value;
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
{  sx:=StrToIntDef(f_frame.FX.Text,-1);
  sy:=StrToIntDef(f_frame.FY.Text,-1);
  sw:=StrToIntDef(f_frame.FWidth.Text,-1);
  sh:=StrToIntDef(f_frame.FHeight.Text,-1);
  if (sx>=0)and(sy>=0)and(sw>0)and(sh>0) then begin
    camera.GetFrame(x,y,w,h,true);
    if (x<>sx)or(y<>sy)or(w<>sw)or(h<>sh) then
      camera.SetFrame(sx,sy,sw,sh);
  end;  }
{  if camera.CanSetGain then begin
    if camera.Gain<>f_preview.Gain then begin
      camera.Gain:=f_preview.Gain;
    end;
    if camera.hasOffset then begin
       if camera.Offset<>f_preview.Offset then camera.Offset:=f_preview.Offset;
    end;
  end;  }
  if FCamera.FrameType<>LIGHT then FCamera.FrameType:=LIGHT;
  FCamera.ObjectName:=rsGuide;
  FCamera.StackNum:=-1;
  FCamera.AddFrames:=false;
  FCamera.SaveFrames:=false;
  FCamera.AlignFrames:=false;
  //fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  FCamera.StartExposure(e);
end
else begin
   InternalguiderStop;
   if not AllDevicesConnected then msg(rsSomeDefinedD,1);
end;
end;

procedure T_autoguider_internal.InternalguiderStart;
begin
  SetStatus('Start Guiding',GUIDER_BUSY);
  Application.QueueAsyncCall(@InternalguiderStartAsync,0);
end;

procedure T_autoguider_internal.InternalguiderStartAsync(Data: PtrInt); {internal guider}
var
  i: integer;
begin
  if AllDevicesConnected=false then
  begin
    msg('Internal guider: Devices not connected!',1);
    SetStatus('Devices not connected',GUIDER_ALERT);
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
    SetStatus('Mount not supported',GUIDER_ALERT);
    exit;
  end;
  SetStatus('Start Guiding',GUIDER_BUSY);
  StopInternalguider:=false;
  InternalguiderGuiding:=true;

  if Fmount.Tracking=false then
  begin
    msg('Start tracking. Wait 20 seconds',2);
    Fmount.Track;//start tracking
    wait(20000);
  end;

  setlength(xy_trend,nrpointsTrend,4);
  for i:=0 to nrpointsTrend-1 do {clear}
  begin
   xy_trend[i,0]:=1E100;//delta ra, 1E100 is an empthy marker
   xy_trend[i,1]:=0;//delta dec
   xy_trend[i,2]:=0;//ra correction
   xy_trend[i,3]:=0 //dec correction
  end;

  old_PactionRA:=0;
  old_PactionDEC:=0;

  InternalguiderInitialize:=true; //initialize;

  InternalguiderLoop;

end;

procedure T_autoguider_internal.InternalAutoguiding;
var i,maxpulse: integer;
begin
    finternalguider.draw_xy(xy_trend);//plot xy values
    finternalguider.draw_trend(xy_trend);// plot trends
    for i:=nrpointsTrend-2 downto 0 do {shift values and make place for new values}
    begin
     xy_trend[i+1,0]:=xy_trend[i,0];//x value
     xy_trend[i+1,1]:=xy_trend[i,1];//y value
     xy_trend[i+1,2]:=xy_trend[i,2];//x correction
     xy_trend[i+1,3]:=xy_trend[i,3];//y correction
    end;

    //Measure drift
    if measure_drift(InternalguiderInitialize,driftX,driftY)=2 then exit;// ReferenceX,Y indicates the total drift, driftX,driftY to drift since previous call. Arrays xy_array_old,xy_array are for storage star positions
    if InternalguiderInitialize then begin
       SetStatus(StarLostStatus,GUIDER_ALERT);
       exit; //until star(s) detected. If no stars are detected initialize is returned true
    end;
    SetStatus('Guiding',GUIDER_GUIDING);
    //rotate drift if required.
    rotate2((- finternalguider.PA*pi/180),driftX,driftY, driftRA,driftDec);{rotate a vector point, counter clockwise}

    // Apply meridian flip if required
    if (mount.PierSide=pierWest) <> (pos('E',finternalguider.pier_side)>0) then // Did a meridian flip occur since calibration.
      driftRA:=-driftRA; // A meridian flip only requires RA direction to inverse! For Declination there is no change.

    if finternalguider.pulsegainNorth<0 then driftDEC:=-driftDEC;//flipped image correction. E.g. an image where north is up and east on the right size.

    xy_trend[0,0]:=-DriftRa;//store RA drift in  pixels.
    xy_trend[0,1]:=+DriftDec;//store DEC drift in pixels.

    //calculate required RA correction in pixels
    PactionRA:=driftRA*finternalguider.RAgain/100;// Proportional action of the controller.
    correctionRA:=-(PactionRA-old_PactionRA *finternalguider.RA_hysteresis/100); //Hysteresis. For a higher setting the control will rely more on historical values. Typical set at 70%
    old_PactionRA:=PactionRA;//Store for next cycle hysteresis calculation

    //calculate required DEC correction in pixels
    PactionDEC:=driftDEC*finternalguider.DECgain/100;// proportional action of the controller.
    correctionDEC:=-(PactionDEC-old_PactionDEC*finternalguider.DEC_hysteresis/100); //HHysteresis. For a higher setting the control will rely more on historical values. Typical set at 70%
    old_PactionDEC:=PactionDEC; //Store for next cycle hysteresis calculation

    if finternalguider.disable_guiding=false then //guiding enabled
    begin
      xy_trend[0,2]:=-correctionRA;//store RA correction in pixels for trend
      xy_trend[0,3]:=+correctionDEC;//store DEC correction in pixels for trend

      if abs(correctionRA)<finternalguider.minimum_moveRA then correctionRA:=0;//avoid chasing the seeing. Improves the stability
      if abs(correctionDEC)<finternalguider.minimum_moveDEC then correctionDEC:=0;//avoid chasing the seeing. Improves the stability

      Guidethecos:=cos(mount.Dec*pi/180); if Guidethecos=0 then Guidethecos:=0.000001;
      correctionRA:=correctionRA/Guidethecos; //correct pixels with cos(dec). Rotation in pixels near celestial pole decreases with cos(dec)

      pulseRA:=0;
      pulseDEC:=0;

      if correctionRA>0 then //going East increases the RA
      begin
         pulseRA:=min(max_duration,round(1000*abs(correctionRA/finternalguider.pulsegainEast))); {duration msec}
         if pulseRA>10 then //Large enough correction to follow by motors/relays. Complementary with minimum_move
         begin
           msg('East: '+inttostr(pulseRA),3);
           mount.PulseGuide(2,pulseRA);  {0=north, 1=south, 2 East, 3 West}
         end;
      end
      else
      if correctionRA<0 then //going West
      begin
        pulseRA:=min(max_duration,round(1000*abs(correctionRA/finternalguider.pulsegainWest))); {duration msec}
        if pulseRA>10 then
        begin
          msg('West: '+inttostr(pulseRA),3);
          mount.PulseGuide(3,pulseRA);  {0=north, 1=south, 2 East, 3 West}
        end;
      end;

      if correctionDEC>0 then //go North increase the DEC.
      begin
        pulseDEC:=min(max_duration,round(1000*abs(correctionDec/finternalguider.pulsegainNorth))); {duration msec}
        if pulseDEC>10 then
        begin
          msg('North: '+inttostr(pulseDEC),3);
          mount.PulseGuide(0,pulseDEC);  {0=north, 1=south, 2 East, 3 West}
        end;
      end
      else
      if correctionDEC<0 then //go South
      begin
        pulseDEC:=min(max_duration,round(1000*abs(correctionDec/finternalguider.pulsegainSouth))); {duration msec}
        if pulseDEC>10 then
        begin
          msg('South: '+inttostr(pulseDEC),3);
          mount.PulseGuide(1,pulseDEC);  {0=north, 1=south, 2 East, 3 West}
        end;
      end;

      // wait for puls guide move completed

      maxpulse:=max(pulseRA,pulseDEC);
      if maxpulse>0 then
      begin
        WaitPulseGuiding(maxpulse);
      end;

    end //guiding enabled
    else
    begin  //guiding disabled
      xy_trend[0,2]:=0;
      xy_trend[0,3]:=0;
    end;
end;

function T_autoguider_internal.WaitPulseGuiding(pulse:double): boolean;
var timeend:double;
begin
  result:=false;
  timeend:=now+(pulse/(secperday*1000));
  while now<timeend do begin
    sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if StopInternalguider then
    begin
      msg('Guider stop pressed.',3);
      exit;
    end;
    if not mount.PulseGuiding then begin
      result:=true;
      break;
    end;
  end;
end;

procedure T_autoguider_internal.InternalguiderStop;
begin
  StopInternalguider:=true;
  InternalguiderRunning:=false;
  InternalguiderGuiding:=false;
  InternalguiderCalibrating:=false;
  Finternalguider.ButtonLoop.enabled:=true;
  Finternalguider.ButtonCalibrate.enabled:=true;
  Finternalguider.ButtonGuide.enabled:=true;
  SetStatus('Stopped',GUIDER_IDLE);
end;


procedure T_autoguider_internal.InternalguiderCalibrate;
begin
  if AllDevicesConnected=false then
  begin
    msg('Internal guider: Devices not connected!',1);
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
  StopInternalguider:=false;
  InternalguiderCalibrating:=true;
  SetStatus('Start Calibration',GUIDER_BUSY);

  finternalguider.trend_message('Guider is in calibration mode.','This will take a few minutes.');

  if mount.Tracking=false then
  begin
    msg('Start tracking. Wait 20 seconds',3);
    mount.Track;//start tracking
    sleep(20000);
  end;

  Calthecos:=cos(mount.Dec*pi/180); if Calthecos=0 then Calthecos:=0.00000001; //prevent dividing by zero

  InternalguiderCalibrationDirection:=1;
  InternalguiderCalibrationStep:=0;

  InternalguiderLoop;

end;

procedure T_autoguider_internal.InternalCalibration;
var drift: double;
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
             CalibrationDuration:=667; //duration of pulse guiding
             InternalguiderCalibrationStep:=1;
             InternalCalibration; // iterate without new image
           end;
        1: begin
             CalibrationDuration:=round(CalibrationDuration*1.5);
             msg('Testing pulse guiding East for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
             InternalCalibrationInitialize:=true;
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
             mount.PulseGuide(2,CalibrationDuration {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(CalibrationDuration);
             InternalguiderCalibrationStep:=2;
           end;
        2: begin
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
             drift:=sqrt(sqr(driftX)+sqr(driftY));//  For image with north up and east left, driftX become negative.
             //newmessage('DriftX ' + floattostrf(driftx,ffgeneral,0,2)+' DriftY ' + floattostrf(driftY,ffgeneral,0,2));
             if ((drift>5) or (CalibrationDuration>20000)) then begin// OK, next direction
               if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
               pulsegainEast:=drift*1000/(CalibrationDuration*Calthecos); // [px*cos(dec)/sec]
               paEast:=arctan2(driftY,driftX);//-pi..pi, For north up and east left this gives zero angle
               InternalguiderCalibrationDirection:=2;
               InternalguiderCalibrationStep:=0;
             end
             else begin // retry with bigger pulse
               InternalguiderCalibrationStep:=1;
             end;
           end;
      end;
    end;
  2:begin  //WEST, measure pulse guide. Use same duration as East
      case InternalguiderCalibrationStep of
        0: begin
             msg('Testing pulse guiding West for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',2);
             InternalCalibrationInitialize:=true;
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
             mount.PulseGuide(3,CalibrationDuration {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(CalibrationDuration);
             InternalguiderCalibrationStep:=1;
           end;
        1: begin
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
             drift:=sqrt(sqr(driftX)+sqr(driftY)); //For image with north up and east left, driftX become positive.
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
             msg('Guider, removing backlash North',3);
             mount.PulseGuide(0,5000 {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(5000);
             CalibrationDuration:=667; //duration of pulse guiding
             InternalguiderCalibrationStep:=1;
           end;
        1: begin
             CalibrationDuration:=round(CalibrationDuration*1.5);
             msg('Testing pulse guiding North for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',3);
             InternalCalibrationInitialize:=true;//for measure drift
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
             mount.PulseGuide(0,CalibrationDuration {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(CalibrationDuration);
             InternalguiderCalibrationStep:=2;
           end;
        2: begin
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
             drift:=sqrt(sqr(driftX)+sqr(driftY));
             if ((drift>5) or (CalibrationDuration>20000)) then begin// OK, next direction
               if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
               paNorth:=arctan2(driftY,driftX); // Relative to the positive X axis and CCW
               Caltheangle:=paNorth - paEast;// CCW angles, calculate angle North relative to West
               if Caltheangle<pi then Caltheangle:=Caltheangle+pi*2;
               if Caltheangle>pi then Caltheangle:=Caltheangle-pi*2;
               if  Caltheangle>0 then //is turning to from West to North positive or negative pi/2
                 Calflip:=+1  // Normal. If North is up then East is left in the image
               else
                 Calflip:=-1; // Flipped image. E.g.if North is up then East is on the right side}
               pulsegainNorth:=Calflip*drift*1000/(CalibrationDuration); // [px/sec]
               InternalguiderCalibrationDirection:=4;
               InternalguiderCalibrationStep:=0;
               InternalCalibration;  // iterate without new image
             end
             else begin // retry with bigger pulse
               InternalguiderCalibrationStep:=1;
             end;
           end;
      end;
    end;
  4:begin  //SOUTH, measure pulse guide speed.
      case InternalguiderCalibrationStep of
        0: begin
             msg('Removing backlash South',3);
             mount.PulseGuide(1,5000 {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(5000);
             CalCount:=0;
             CaldriftOld:=0;
             InternalguiderCalibrationStep:=1;
             msg('Testing pulse guiding South for '+floattostrF(CalibrationDuration/1000,FFgeneral,0,2)+ ' seconds',3);
           end;
        1: begin
             InternalCalibrationInitialize:=true;
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure reference star positions
             mount.PulseGuide(1,CalibrationDuration {duration msec} );  {0=north, 1=south, 2 East, 3 West}
             WaitPulseGuiding(CalibrationDuration);
             InternalguiderCalibrationStep:=2;
           end;
        2: begin
             if measure_drift(InternalCalibrationInitialize,driftX,driftY)>0 then StopError;//measure drift
             drift:=sqrt(sqr(driftX)+sqr(driftY));
             inc(CalCount);
             if ((CaldriftOld>2) or (Calcount>=4)) then begin
               if drift<2 then begin msg('Abort calibration, no movement measured!',1); StopError; end;
               pulsegainSouth:=Calflip*drift*1000/(CalibrationDuration); // [px*cos(dec)/sec]   Flipped is already measured
               msg('Internal guider calibration:  Pulse gain measured North/South: '+ floattostrF(pulsegainNorth,ffgeneral,0,2)+'/'+ floattostrF(pulsegainSouth,ffgeneral,0,2)+' [px/sec]',3);
               InternalguiderCalibrationDirection:=5;
               InternalguiderCalibrationStep:=0;
               InternalCalibration;  // iterate without new image
             end
             else begin
               InternalguiderCalibrationStep:=1;
             end;
           end;
      end;
    end;
  5:begin  //Display findings
      if mount.PierSide=pierWest then finternalguider.pier_side:='E' else finternalguider.pier_side:='W'; //measured west or east ??
      finternalguider.PA:=paEast*180/pi; // this is the relative angle between the image and the mount.
      finternalguider.pulsegainEast:=pulsegainEast;
      finternalguider.pulsegainWest:=pulsegainWest;
      finternalguider.pulsegainNorth:=pulsegainNorth;
      finternalguider.pulsegainSouth:=pulsegainSouth;
      finternalguider.pixel_size:=Fguidespeed*15*2/(pulsegainEast+pulsegainWest);//alternative method assuming 0.5x and 1.5x pulse speed
      if finternalguider.measure_method2.checked then begin
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
               msg('Ready to guide. Used alternative method to calculate the pixel scale which could be inaccurate.',1);
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
      msg('Ready to guide!',1);
      finternalguider.trend_message('Calibration is ready.','');
      InternalguiderStop;
      SetStatus('Calibration Complete',GUIDER_IDLE);
    end;
end;
except
end;
end;


end.

