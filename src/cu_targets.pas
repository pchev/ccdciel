unit cu_targets;

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

{$mode objfpc}{$H+}

interface

uses u_global, cu_plan, u_utils, indiapi, pu_scriptengine, pu_pause, cu_rotator, cu_planetarium,
  fu_capture, fu_preview, fu_filterwheel, cu_mount, cu_camera, cu_autoguider, cu_astrometry,
  fu_safety, fu_weather, cu_dome, u_ccdconfig,
  u_translation, LazFileUtils, Controls, Dialogs, ExtCtrls,Classes, Forms, SysUtils;

type
  TTargetList = array of TTarget;

  T_Targets = class(TComponent)
    private
      TargetTimer: TTimer;
      TargetRepeatTimer: TTimer;
      StopTimer: TTimer;
      StopTargetTimer: TTimer;
      WeatherRestartTimer: TTimer;
      FTargetsChange: TNotifyEvent;
      FPlanChange: TNotifyEvent;
      FonMsg: TNotifyMsg;
      FDelayMsg: TNotifyStr;
      FonEndSequence: TNotifyEvent;
      FonShutdown: TNotifyEvent;
      Fcapture: Tf_capture;
      Fpreview: Tf_preview;
      Ffilter: Tf_filterwheel;
      Fweather: Tf_weather;
      Fsafety: Tf_safety;
      Fdome: T_dome;
      Fmount: T_mount;
      Fcamera: T_camera;
      Frotaror: T_rotator;
      Fautoguider: T_autoguider;
      Fastrometry: TAstrometry;
      Fplanetarium: TPlanetarium;
      StartPlanTimer: TTimer;
      FTargetCoord: boolean;
      FTargetRA,FTargetDE: double;
      FIgnoreRestart: boolean;
      FTargetsRepeatCount: integer;
      FFileVersion, FSlewRetry: integer;
      FAtEndPark, FAtEndCloseDome, FAtEndStopTracking,FAtEndWarmCamera,FAtEndRunScript,FOnErrorRunScript,FAtEndShutdown: boolean;
      FAtEndScript, FOnErrorScript: string;
      SkipTarget: boolean;
      TargetForceNext: boolean;
      FDoneStatus, FLastDoneStep: string;
      function GetBusy: boolean;
      procedure SetTargetName(val: string);
      procedure SetPreview(val: Tf_preview);
      procedure SetCapture(val: Tf_capture);
      procedure SetMount(val: T_mount);
      procedure SetCamera(val: T_camera);
      procedure SetFilter(val: Tf_filterwheel);
      procedure SetAutoguider(val: T_autoguider);
      procedure SetAstrometry(val: TAstrometry);
      procedure msg(txt:string; level:integer);
      procedure ShowDelayMsg(txt:string);
      procedure StopSequence(abort: boolean);
      procedure NextTargetAsync(Data: PtrInt);
      procedure NextTarget;
      function InitTarget(restart:boolean=false):boolean;
      function InitSkyFlat: boolean;
      procedure StartPlan;
      procedure RunErrorAction;
      procedure RunEndAction(confirm: boolean=true);
      function StopGuider:boolean;
      function StartGuider:boolean;
      function Slew(ra,de: double; precision,planprecision: boolean):boolean;
      procedure TargetTimerTimer(Sender: TObject);
      procedure TargetRepeatTimerTimer(Sender: TObject);
      procedure StartPlanTimerTimer(Sender: TObject);
      procedure StopTimerTimer(Sender: TObject);
      procedure StopTargetTimerTimer(Sender: TObject);
      procedure WeatherRestartTimerTimer(Sender: TObject);
      procedure PlanStepChange(Sender: TObject);
    protected
      Ftargets: TTargetList;
      NumTargets: integer;
      FCurrentTarget: integer;
      FTargetsRepeat: integer;
      FResetRepeat: boolean;
      FSeqStartAt,FSeqStopAt,FSeqStartTime: TDateTime;
      FSeqStart,FSeqStop: boolean;
      FSeqStartTwilight,FSeqStopTwilight: boolean;
      TargetTimeStart,TargetDelayEnd: TDateTime;
      FRunning,FScriptRunning: boolean;
      FInitializing: boolean;
      FUnattended: boolean;
      FName: string;
    public
      FTargetInitializing, FWaitStarting: boolean;
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear;
      function Add(t: TTarget):integer;
      procedure Start;
      procedure Stop;
      procedure Abort;
      procedure WeatherPause;
      procedure WeatherRestart;
      procedure ForceNextTarget;
      procedure SaveDoneCount(RepeatDone: integer);
      function  CheckDoneCount:boolean;
      procedure ClearDoneCount(ClearRepeat: boolean);
      property FileVersion: integer read FFileVersion write FFileVersion;
      property TargetsRepeat: integer read FTargetsRepeat write FTargetsRepeat;
      property TargetsRepeatCount: integer read FTargetsRepeatCount write FTargetsRepeatCount;
      property ResetRepeat: boolean read FResetRepeat write FResetRepeat;
      property IgnoreRestart: boolean read FIgnoreRestart write FIgnoreRestart;
      property SeqStartAt: TDateTime read FSeqStartAt write FSeqStartAt;
      property SeqStopAt: TDateTime read FSeqStopAt write FSeqStopAt;
      property SeqStart: boolean read FSeqStart write FSeqStart;
      property SeqStop: boolean read FSeqStop write FSeqStop;
      property SeqStartTwilight: boolean read FSeqStartTwilight write FSeqStartTwilight;
      property SeqStopTwilight: boolean read FSeqStopTwilight write FSeqStopTwilight;
      property Targets: TTargetList read Ftargets;
      property Count: integer read NumTargets;
      property CurrentTarget: integer read FCurrentTarget;
      property TargetName: string read FName write SetTargetName;
      property Busy: boolean read GetBusy;
      property Running: boolean read FRunning;
      property ScriptRunning: boolean read FScriptRunning;
      property TargetCoord: boolean read FTargetCoord;
      property TargetRA: double read FTargetRA;
      property TargetDE: double read FTargetDE;
      property TargetInitializing: boolean read FTargetInitializing;
      property WaitStarting: boolean read FWaitStarting;
      property DoneStatus: string read FDoneStatus;
      property LastDoneStep: string read FLastDoneStep;
      property Unattended: boolean read FUnattended write FUnattended;
      property onTargetsChange: TNotifyEvent read FTargetsChange write FTargetsChange;
      property onPlanChange: TNotifyEvent read FPlanChange write FPlanChange;
      property Preview: Tf_preview read Fpreview write Setpreview;
      property Capture: Tf_capture read Fcapture write Setcapture;
      property Mount: T_mount read Fmount write SetMount;
      property Camera: T_camera read Fcamera write SetCamera;
      property Rotaror: T_rotator read Frotaror write Frotaror;
      property Filter: Tf_filterwheel read Ffilter write SetFilter;
      property Weather: Tf_weather read Fweather write Fweather;
      property Safety: Tf_safety read Fsafety write Fsafety;
      property Dome: T_dome read Fdome write Fdome;
      property Autoguider: T_autoguider read Fautoguider write SetAutoguider;
      property Astrometry: TAstrometry read Fastrometry write SetAstrometry;
      property Planetarium: TPlanetarium read Fplanetarium write Fplanetarium;
      property DelayMsg: TNotifyStr read FDelayMsg write FDelayMsg;
      property onMsg: TNotifyMsg read FonMsg write FonMsg;
      property onEndSequence: TNotifyEvent read FonEndSequence write FonEndSequence;
      property AtEndPark: boolean read FAtEndPark write FAtEndPark;
      property AtEndCloseDome: boolean read FAtEndCloseDome write FAtEndCloseDome;
      property AtEndStopTracking: boolean read FAtEndStopTracking write FAtEndStopTracking;
      property AtEndWarmCamera: boolean read FAtEndWarmCamera write FAtEndWarmCamera;
      property AtEndRunScript: boolean read FAtEndRunScript write FAtEndRunScript;
      property OnErrorRunScript: boolean read FOnErrorRunScript write FOnErrorRunScript;
      property AtEndScript: string read FAtEndScript write FAtEndScript;
      property OnErrorScript: string read FOnErrorScript write FOnErrorScript;
      property AtEndShutdown: boolean read FAtEndShutdown write FAtEndShutdown;
      property OnShutdown: TNotifyEvent read FonShutdown write FonShutdown;
  end;

implementation

constructor T_Targets.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargetInitializing:=false;
  FWaitStarting:=false;
  NumTargets := 0;
  WeatherPauseCapture:=false;
  WeatherCapturePaused:=false;
  WeatherPauseCanceled:=false;
  WeatherCancelRestart:=false;
  FTargetsRepeat:=1;
  TargetForceNext:=false;
  Frunning:=false;
  FSeqStartAt:=0;
  FSeqStopAt:=0;
  FSeqStart:=false;
  FSeqStop:=false;
  FSeqStartTwilight:=false;
  FSeqStopTwilight:=false;
  FAtEndPark:=false;
  FAtEndCloseDome:=false;
  FAtEndStopTracking:=true;
  FAtEndWarmCamera:=false;
  FAtEndRunScript:=false;
  FOnErrorRunScript:=false;
  FAtEndShutdown:=false;
  FAtEndScript:='';
  FOnErrorScript:='';
  FDoneStatus:='';
  FLastDoneStep:='';
  FIgnoreRestart:=true;
  FResetRepeat:=true;
  FInitializing:=false;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  SkipTarget:=false;
  TargetTimer:=TTimer.Create(self);
  TargetTimer.Enabled:=false;
  TargetTimer.Interval:=1000;
  TargetTimer.OnTimer:=@TargetTimerTimer;
  TargetRepeatTimer:=TTimer.Create(self);
  TargetRepeatTimer.Enabled:=false;
  TargetRepeatTimer.Interval:=1000;
  TargetRepeatTimer.OnTimer:=@TargetRepeatTimerTimer;
  StartPlanTimer:=TTimer.Create(self);
  StartPlanTimer.Enabled:=false;
  StartPlanTimer.Interval:=5000;
  StartPlanTimer.OnTimer:=@StartPlanTimerTimer;
  StopTimer:=TTimer.Create(self);
  StopTimer.Enabled:=false;
  StopTimer.OnTimer:=@StopTimerTimer;
  StopTargetTimer:=TTimer.Create(self);
  StopTargetTimer.Enabled:=false;
  StopTargetTimer.OnTimer:=@StopTargetTimerTimer;
  WeatherRestartTimer:=TTimer.Create(self);
  WeatherRestartTimer.Enabled:=false;
  WeatherRestartTimer.OnTimer:=@WeatherRestartTimerTimer;
end;

destructor  T_Targets.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure T_Targets.SetTargetName(val: string);
begin
  FName:=val;
  if Assigned(FTargetsChange) then FTargetsChange(self);
end;

procedure T_Targets.SetPreview(val: Tf_preview);
var i: integer;
begin
  Fpreview:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Preview:=Fpreview;
end;

procedure T_Targets.SetCapture(val: Tf_capture);
var i: integer;
begin
  Fcapture:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Capture:=Fcapture;
end;

procedure T_Targets.SetMount(val: T_mount);
var i: integer;
begin
  Fmount:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Mount:=Fmount;
end;

procedure T_Targets.SetCamera(val: T_camera);
var i: integer;
begin
  Fcamera:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Camera:=Fcamera;
end;

procedure T_Targets.SetFilter(val: Tf_filterwheel);
var i: integer;
begin
  Ffilter:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Filter:=Ffilter;
end;

procedure T_Targets.SetAutoguider(val: T_autoguider);
var i: integer;
begin
  Fautoguider:=val;
  for i:=0 to NumTargets-1 do T_Plan(FTargets[i].plan).Autoguider:=Fautoguider;
end;

procedure T_Targets.SetAstrometry(val: TAstrometry);
begin
  Fastrometry:=val;
end;

procedure T_Targets.msg(txt:string; level:integer);
begin
  if Assigned(FonMsg) then FonMsg(txt,level);
end;

procedure T_Targets.ShowDelayMsg(txt:string);
begin
  if Assigned(FDelayMsg) then FDelayMsg(txt);
end;

procedure  T_Targets.Clear;
var i: integer;
begin
  ClearDoneCount(true);
  for i:=0 to NumTargets-1 do if FTargets[i]<>nil then FTargets[i].Free;
  SetLength(Ftargets,0);
  NumTargets := 0;
  FName:='';
  if Assigned(FTargetsChange) then FTargetsChange(self);
end;

function T_Targets.Add(t: TTarget):integer;
var p:T_Plan;
begin
  p:=T_Plan.Create(nil);
  p.onPlanChange:=FPlanChange;
  p.onStepProgress:=@PlanStepChange;
  p.Preview:=Fpreview;
  p.Capture:=Fcapture;
  p.Mount:=Fmount;
  p.Camera:=Fcamera;
  p.Filter:=Ffilter;
  p.Autoguider:=Fautoguider;
  p.onMsg:=FonMsg;
  t.plan:=p;
  inc(NumTargets);
  SetLength(Ftargets,NumTargets);
  Ftargets[NumTargets-1]:=t;
  if Assigned(FTargetsChange) then FTargetsChange(self);
  result:=NumTargets-1;
end;

procedure T_Targets.Start;
var hm,he: double;
    twok,wtok,nd: boolean;
    j,stw:integer;
begin
  try
  FWaitStarting:=true;
  FCurrentTarget:=-1;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  CancelAutofocus:=false;
  WeatherCancelRestart:=false;
  FRunning:=true;
  // look for a dawn sky flat
  for j:=0 to NumTargets-1 do begin
    if (Targets[j].objectname=SkyFlatTxt)and(Targets[j].planname=FlatTimeName[1]) then begin
      // Force stop at dawn
      FSeqStop:=true;
      FSeqStopTwilight:=true;
    end;
  end;
  twok:=TwilightAstro(now,hm,he);
  if twok then begin
    if FSeqStartTwilight then
       FSeqStartAt:=he/24;
    if FSeqStopTwilight then
       FSeqStopAt:=hm/24;
  end else begin
    if FSeqStartTwilight then begin
      if hm<>0 then begin
        msg(Format(rsSequenceCanc, [FName]),1);
        msg(rsNoDuskToday,1);
        FRunning:=false;
        exit;
      end else begin
        msg(Format(rsSequenceStar, [FName]),1);
        msg(rsNoDuskToday,1);
        FSeqStart:=false;
      end;
    end;
    if FSeqStopTwilight then begin
      msg(Format(rsSequenceIgno, [FName]),1);
      msg(rsNoDawnToday,1);
      FSeqStop:=false;
    end;
  end;
  if FSeqStart then begin
     msg(Format(rsWaitToStartS, [FName, TimeToStr(FSeqStartAt)]),1);
     wtok:=WaitTill(TimeToStr(FSeqStartAt),true);
     if not wtok then begin
        msg(Format(rsSequenceCanc, [FName]),1);
        FRunning:=false;
        exit;
     end;
  end;
  if FSeqStop then begin
     SecondsToWait(FSeqStopAt,true,stw,nd);
     if stw>0 then begin
        msg(Format(rsTheSequenceW, [FName, TimeToStr(FSeqStopAt), IntToStr(stw)]), 1);
        StopTimer.Interval:=1000*stw;
        StopTimer.Enabled:=true;
     end else begin
       msg(Format(rsSequenceCanc, [FName]),1);
       msg(Format(rsStopTimeAlre, [TimeToStr(FSeqStopAt)]),1);
       FRunning:=false;
       exit;
     end;
  end
  else
     StopTimer.Enabled:=false;
  if FTargetsRepeat=1 then
    msg(Format(rsStartingSequ, [FName]),1)
  else
    msg(Format(rsStartingSequ2, [FName, inttostr(FTargetsRepeatCount+1),inttostr(FTargetsRepeat)]),1);
  finally
    FWaitStarting:=false;
  end;
  FSeqStartTime:=now;
  NextTarget;
end;

procedure T_Targets.StopTimerTimer(Sender: TObject);
var j: integer;
    p:t_plan;
begin
  StopTimer.Enabled:=false;
  msg(Format(rsStopTheCurre, [TimeToStr(FSeqStopAt)]),1);
  if FSeqStopTwilight then begin
    // look for a dawn sky flat
    for j:=0 to NumTargets-1 do begin
     if (Targets[j].objectname=SkyFlatTxt)and(Targets[j].planname=FlatTimeName[1]) then begin
        // stop current step
        if FCurrentTarget>=0 then
           p:=t_plan(Ftargets[FCurrentTarget].plan)
        else
           p:=nil;
        if (p<>nil) and p.Running then p.Stop;
        wait(5);
        // run sky flat
        FCurrentTarget:=j-1;
        FTargetsRepeatCount:=FTargetsRepeat-1;
        NextTarget;
        exit;
     end;
    end;
  end;
  StopSequence(true);
end;

procedure T_Targets.Stop;
begin
  msg(rsRequestToSto2,1);
  StopSequence(false);
end;

procedure T_Targets.Abort;
begin
  msg(rsAbortTheCurr,1);
  StopSequence(true);
end;

procedure T_Targets.WeatherPause;
begin
  // set to pause after the current exposure is complete
  WeatherPauseCapture:=true;
  // cancel any restart in progress
  WeatherRestartTimer.Enabled:=false;
  WeatherCancelRestart:=true;
end;

procedure T_Targets.WeatherRestart;
begin
  if WeatherCapturePaused then begin
    // we really pause during capture, wait restart delay
    WeatherRestartTimer.Enabled:=false;
    WeatherRestartTimer.Interval:=1000*60*WeatherRestartDelay;
    WeatherRestartTimer.Enabled:=true;
    WeatherCancelRestart:=false;
    msg(Format(rsWaitingForMi, [IntToStr(WeatherRestartDelay)]), 1);
  end
  else begin
    // we not pause, just remove the condition
    WeatherPauseCapture:=false;
  end;
end;

procedure T_Targets.WeatherRestartTimerTimer(Sender: TObject);
var initok: boolean;
begin
  WeatherRestartTimer.Enabled:=false;
  if FRunning then begin
    // try to recenter, restart mount and guiding.
    WeatherPauseCanceled:=false;
    initok:=InitTarget(true);
    if (not initok)and(not WeatherCancelRestart) then begin
       WeatherPauseCanceled:=true;
       if FRunning then NextTarget;
    end;
  end
  else begin
    WeatherPauseCanceled:=true;
  end;
  // continue the capture
  if not WeatherCancelRestart then
     WeatherPauseCapture:=false
  else begin
    // stop and continue to wait
     StopGuider;
     mount.AbortMotion;
     msg(rsSequencePaus, 1);
  end;
end;

procedure T_Targets.StopSequence(abort: boolean);
var p: T_Plan;
    RepeatDone:integer;
    r: string;
begin
 StopTimer.Enabled:=false;
 StopTargetTimer.Enabled:=false;
 WeatherRestartTimer.Enabled:=false;
 InplaceAutofocus:=AutofocusInPlace;
 RepeatDone:=FTargetsRepeatCount;
 FTargetsRepeatCount:=FTargetsRepeat+1;
 if FRunning then begin
   FRunning:=false;
   WeatherPauseCanceled:=true;
   WeatherPauseCapture:=false;
   WeatherCancelRestart:=false;
   if WaitTillrunning then begin
     if wt_pause<>nil
      then wt_pause.BtnCancel.Click
      else cancelWaitTill:=true;
   end;
   if Autofocusing then msg(rsRequestToSto3,1);
   CancelAutofocus:=true;
   Camera.AbortExposure;
   if Mount.MountSlewing then Mount.AbortMotion;
   if Astrometry.Busy then Astrometry.StopAstrometry;
   if f_scriptengine.scr.Running then begin
      f_scriptengine.StopScript;
   end;
   if FCurrentTarget>=0 then
      p:=t_plan(Ftargets[FCurrentTarget].plan)
   else
      p:=nil;
   if (not abort) then begin
     if (p<>nil) and p.Running then p.Stop;
     FRunning:=false;
     StopGuider;
     if Unattended then begin
       RunEndAction;
     end
     else begin
       msg(rsSequenceStop,1);
     end;
     ShowDelayMsg('');
     if EmailEndSequence then begin
       r:=email(Format(rsSequenceStop2, [FName]),Format(rsSequenceStop2, [FName]));
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
   end
   else begin
     if (p<>nil) and p.Running then p.Stop;
     FRunning:=false;
     if Mount.MountSlewing then Mount.AbortMotion;
     if Astrometry.Busy then Astrometry.StopAstrometry;
     if Capture.Running then begin
        Camera.AbortExposure;
        Capture.Stop;
     end;
     if Preview.Running then begin
        Camera.AbortExposure;
        Preview.Stop;
     end;
     StopGuider;
     if f_scriptengine.scr.Running then f_scriptengine.StopScript;
     msg(rsSequenceAbor,0);
     RunErrorAction;
     ShowDelayMsg('');
     if EmailAbortSequence or EmailEndSequence then begin
       r:=email(rsSequenceAbor,rsSequenceAbor+crlf+FName);
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
   end;
   SaveDoneCount(RepeatDone);
   if assigned(FonEndSequence) then FonEndSequence(nil);
   CurrentTargetName:='';
   CurrentStepName:='';
 end
 else msg(rsNotRunningNo,1);
end;

function T_Targets.CheckDoneCount:boolean;
var i,j: integer;
    t: TTarget;
    p: T_Plan;
begin
 result:=false;
 FDoneStatus:='';
 FLastDoneStep:='';
 if IgnoreRestart then exit;
 if FTargetsRepeatCount>0 then begin
   result:=true;
   FLastDoneStep:=rsGlobalRepeat+blank+IntToStr(FTargetsRepeatCount)+'/'+IntToStr(FTargetsRepeat);
   FDoneStatus:=FLastDoneStep;
 end;
 for i:=0 to NumTargets-1 do begin
    t:=Targets[i];
    if t=nil then Continue;
    if t.repeatdone>0 then begin
      result:=true;
      FLastDoneStep:=t.objectname+blank+rsRepeat+':'+blank+IntToStr(t.repeatdone)+'/'+IntToStr(t.repeatcount);
      FDoneStatus:=FDoneStatus+crlf+FLastDoneStep;
    end;
    p:=t_plan(t.plan);
    if p=nil then Continue;
    if p.Count<=0 then Continue;
    for j:=0 to p.Count-1 do begin
      if p.Steps[j].donecount>0 then begin
        result:=true;
        FLastDoneStep:=t.objectname+blank+p.PlanName+blank+rsStep+':'+blank+p.Steps[j].description+blank+rsDone+':'+IntToStr(p.Steps[j].donecount)+'/'+IntToStr(p.Steps[j].count);
        FDoneStatus:=FDoneStatus+crlf+FLastDoneStep;
      end;
    end;
 end;
end;

procedure T_Targets.ClearDoneCount(ClearRepeat: boolean);
var i,j: integer;
    t: TTarget;
    p: T_Plan;
begin
 if ClearRepeat then FTargetsRepeatCount:=0;
 for i:=0 to NumTargets-1 do begin
    t:=Targets[i];
    if t=nil then Continue;
    t.repeatdone:=0;
    p:=t_plan(t.plan);
    if p=nil then Continue;
    if p.Count<=0 then Continue;
    SetLength(t.DoneList,p.Count);
    for j:=0 to p.Count-1 do begin
      p.Steps[j].donecount:=0;
      t.DoneList[j]:=0;
    end;
 end;
end;

procedure T_Targets.SaveDoneCount(RepeatDone: integer);
var p: T_Plan;
    t: TTarget;
    i,n: integer;
    tfile: TCCDconfig;
begin
 try
  if (FCurrentTarget<0)or(FCurrentTarget>Count) then exit;
  t:=Targets[FCurrentTarget];
  if t=nil then exit;
  p:=t_plan(t.plan);
  if p=nil then exit;
  if p.Count<=0 then exit;
  tfile:=TCCDconfig.Create(self);
  tfile.Filename:=CurrentSequenceFile;
  i:=FCurrentTarget+1;
  // global sequence repeat
  if FIgnoreRestart then
     tfile.SetValue('/Targets/RepeatDone',0)
  else
     tfile.SetValue('/Targets/RepeatDone',RepeatDone);
  // target repeat
  if FIgnoreRestart then
     tfile.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0)
  else
     tfile.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',t.repeatdone);
  // plan step done
  n:=p.CurrentStep;
  if (n>=0)and(n<p.Count) then begin
    SetLength(t.DoneList,p.Count);
    if FIgnoreRestart then
       t.DoneList[n]:=0
    else
       t.DoneList[n]:=p.Steps[n].donecount;
    tfile.SetValue('/Targets/Target'+inttostr(i)+'/StepDone/StepCount',Length(t.DoneList));
    tfile.SetValue('/Targets/Target'+inttostr(i)+'/StepDone/Step'+inttostr(n)+'/Done',t.DoneList[n]);
  end;
  // save file
  tfile.Flush;
  tfile.Free;
 except
   on E: Exception do msg('Error saving sequence state:'+ E.Message,1);
 end;
end;

procedure T_Targets.PlanStepChange(Sender: TObject);
begin
  SaveDoneCount(FTargetsRepeatCount);
end;

procedure T_Targets.ForceNextTarget;
var p: T_Plan;
    t: TTarget;
begin
 msg(rsTryNextTarge2,1);
 if FRunning then begin
   t:=Targets[FCurrentTarget];
   p:=t_plan(t.plan);
   TargetForceNext:=true;
   if Autofocusing then begin
     CancelAutofocus:=true;
     msg(rsRequestToSto3,1);
     if Mount.MountSlewing then Mount.AbortMotion;
     if Astrometry.Busy then Astrometry.StopAstrometry;
     wait(30);
   end;
   if p.Running then begin
     msg(Format(rsStopPlan, [Ftargets[FCurrentTarget].planname]),1);
     p.Stop;
     ShowDelayMsg('');
   end
   else
     FRunning:=false;
 end
 else msg(rsNotRunningNo,1);
end;

procedure T_Targets.NextTarget;
begin
  Application.QueueAsyncCall(@NextTargetAsync,0);
end;

procedure T_Targets.NextTargetAsync(Data: PtrInt);
var initok: boolean;
    r: string;
begin
  TargetTimer.Enabled:=false;
  StopTargetTimer.Enabled:=false;
  // stop autoguider
  if (Autoguider<>nil)and(Autoguider.Running)and(Autoguider.State=GUIDER_GUIDING) then
     StopGuider;
  // stop mount tracking
  if mount.MountSlewing then
     Mount.AbortMotion;
  NeedRecenterTarget:=false;
  InplaceAutofocus:=AutofocusInPlace;
  CancelAutofocus:=false;
  SaveDoneCount(FTargetsRepeatCount);
  inc(FCurrentTarget);
  if FRunning and (FCurrentTarget<NumTargets) then begin
   CurrentTargetName:=Targets[FCurrentTarget].objectname;
   if Targets[FCurrentTarget].objectname=ScriptTxt then begin
     FInitializing:=false;
     TargetForceNext:=false;
     Targets[FCurrentTarget].autoguiding:=false;
     FScriptRunning:=true;
     if not f_scriptengine.RunScript(Targets[FCurrentTarget].planname,Targets[FCurrentTarget].path)then begin
       FScriptRunning:=false;
       msg(Format(rsScriptFailed, [Targets[FCurrentTarget].planname]),0);
       if FRunning then begin
       if FUnattended then begin
         StopSequence(true);
         exit;
       end else begin
         f_pause.Caption:=Format(rsScriptFailed, ['']);
         f_pause.Text:=Format(rsScriptFailed, [Targets[FCurrentTarget].planname]
           )+crlf+rsDoYouWantToR;
         if f_pause.Wait(WaitResponseTime,false) then begin
            Dec(FCurrentTarget);
         end else begin
            StopSequence(false);
            exit;
         end;
       end;
       end;
     end;
     FScriptRunning:=false;
     if FRunning then NextTarget;
     exit;
   end
   else if (Targets[FCurrentTarget].objectname=SkyFlatTxt) then begin
    if ((Targets[FCurrentTarget].planname=FlatTimeName[0])and(FTargetsRepeatCount=0)  // Dusk, run only on first repeat
        or((Targets[FCurrentTarget].planname=FlatTimeName[1])and(FTargetsRepeatCount=FTargetsRepeat-1)))  // Dawn, run only on last repeat
    then begin
     FInitializing:=true;
     ShowDelayMsg('');
     TargetForceNext:=false;
     Mount.AbortMotion;
     initok:=InitSkyFlat;
     if not FRunning then begin
       exit;
     end;
     if initok then begin
       StartPlan;
       TargetTimer.Enabled:=true;
     end
     else begin
       msg(Targets[FCurrentTarget].objectname+', '+rsTargetInitia,0);
       if FUnattended then begin
         FInitializing:=false;
         NextTarget;
         exit;
       end else begin
         FInitializing:=false;
         f_pause.Caption:=rsTargetInitia;
         f_pause.Text:=rsTargetInitia+' '+Targets[FCurrentTarget].objectname+
           crlf+rsDoYouWantToR;
         if f_pause.Wait(WaitResponseTime,false) then begin
            Dec(FCurrentTarget);
         end;
         NextTarget;
         exit;
       end;
     end;
    end
    else begin
     if Targets[FCurrentTarget].planname=FlatTimeName[1] then begin
        // dawn flat, do not run final scripts now, only on last repeat
        FCurrentTarget:=NumTargets-1;
     end;
     NextTarget;
     exit;
    end;
   end
   else begin
     FInitializing:=true;
     ShowDelayMsg('');
     TargetForceNext:=false;
     initok:=InitTarget;
     if not FRunning then begin
       exit;
     end;
     if initok then begin
       StartPlan;
       TargetTimer.Enabled:=true;
     end
     else begin
       if SkipTarget then begin
         FInitializing:=false;
         if FRunning then NextTarget;
         exit;
       end
       else begin
         msg(Targets[FCurrentTarget].objectname+', '+rsTargetInitia,0);
         if EmailTargetInitialisation then email(rsTargetInitia,Targets[FCurrentTarget].objectname+', '+rsTargetInitia);
         if FUnattended then begin
           FInitializing:=false;
           NextTarget;
           exit;
         end else begin
           FInitializing:=false;
           f_pause.Caption:=rsTargetInitia;
           f_pause.Text:=rsTargetInitia+' '+Targets[FCurrentTarget].objectname+
             crlf+rsDoYouWantToR;
           if f_pause.Wait(WaitResponseTime,false) then begin
              Dec(FCurrentTarget);
           end;
           NextTarget;
           exit;
         end;
       end;
     end;
   end;
  end
  else begin
   inc(FTargetsRepeatCount);
   if FTargetsRepeatCount<FTargetsRepeat then begin
     FCurrentTarget:=-1;
     FTargetCoord:=false;
     FTargetRA:=NullCoord;
     FTargetDE:=NullCoord;
     if FResetRepeat then ClearDoneCount(false);
     FRunning:=true;
     msg(Format(rsStartingSequ2, [FName, inttostr(FTargetsRepeatCount+1),inttostr(FTargetsRepeat)]),1);
     wait(1);
     NextTarget;
   end
   else begin
     FRunning:=false;
     TargetTimer.Enabled:=false;
     StopTimer.Enabled:=false;
     StopGuider;
     msg(Format(rsSequenceFini, [FName]),1);
     RunEndAction;
     ShowDelayMsg('');
     FCurrentTarget:=-1;
     if EmailEndSequence then begin
       r:=email(Format(rsSequenceFini, [FName]),Format(rsSequenceFini, [FName]));
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
     if assigned(FonEndSequence) then FonEndSequence(nil);
   end;
  end;
end;

function T_Targets.InitTarget(restart:boolean=false):boolean;
var t: TTarget;
    p: T_Plan;
    ok,wtok,nd:boolean;
    stw,i,intime,ri,si,ti: integer;
    hr,hs,ht,tt,dt,st,newra,newde,appra,appde,enddelay,chkendtime: double;
    autofocusstart, astrometrypointing, autostartguider,isCalibrationTarget: boolean;
    skipmsg: string;
begin
  SkipTarget:=false;
  result:=false;
  if not FRunning then exit;
  try
  FWaitStarting:=true;
  t:=Targets[FCurrentTarget];
  if t<>nil then begin
    p:=t_plan(t.plan);
    if t.repeatcount=0 then begin
      SkipTarget:=true;
      result:=false;
      msg(Format(rsSkipTarget, [t.objectname+', '+rsRepeat+'=0']), 2);
      exit;
    end;
    if t.repeatdone>=t.repeatcount then begin
      SkipTarget:=true;
      result:=false;
      msg(Format(rsSkipTarget, [t.objectname+', '+Format(rsSeqFinished,[t.planname])]), 2);
      exit;
    end;
    msg(Format(rsInitializeTa, [t.objectname]),1);
    // check weather
    if (not WeatherPauseCapture)and(FWeather.Connected)and(not FWeather.Clear) then begin
       msg(rsSequencePaus, 1);
       // stop guiding and mount tracking now
       StopGuider;
       mount.AbortMotion;
       WeatherCapturePaused:=false;
       while (FWeather.Connected)and(not FWeather.Clear) and Frunning do begin
          Wait(5);
       end;
       // continue if not aborted
       if not FRunning then exit;
       msg(rsContinueSequ, 1);
    end;
    t.autoguiding:=false;
    InplaceAutofocus:=t.inplaceautofocus;
    // adjust moving object coordinates from planetarium
    if t.updatecoord then begin
       if Fplanetarium.Search(t.objectname,newra,newde) then begin
          msg(Format(rsNewCoordinat, [RAToStr(newra), DEToStr(newde)]),2);
          t.ra:=newra;
          t.de:=newde;
       end;
    end;
    if (p<>nil)and (p.Count>0) then
       enddelay:=(p.Steps[0].exposure+180)/3600/24  // first exposure time + 3 minutes for telescope pointing, in days
    else
       enddelay:=0;

    // compute apparent coord.
    if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
       appra:=t.ra*15*deg2rad;
       appde:=t.de*deg2rad;
       J2000ToApparent(appra,appde);
       appra:=appra*rad2deg/15;
       appde:=appde*rad2deg;
    end;
    st:=frac(FSeqStartTime); // if no coordinates, pivot time is sequence start time
    // adjust start/end from coordinates
    if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
       ObjRise(appra,appde,hr,ri);
       // check object visibility
       if ri=2 then begin
          msg(Format(rsSkipTarget, [t.objectname])+', '+rsThisObjectIs2, 2);
          SkipTarget:=true;
          result:=false;
          exit;
       end;
       ObjSet(appra,appde,hs,si);
       ObjTransit(appra,appde,ht,ti);
       st:=rmod(ht+12+24,24); // pivot time 12h from transit
       // adjust rise/set time
       // begin at rise
       if t.startrise and (ri=0) then
          t.starttime:=hr/24;
       // end at set
       if t.endset and (si=0) then
          t.endtime:=hs/24;
       // start from meridian
       if (t.startmeridian<>NullCoord) and (ti<2) then begin
          if abs(t.startmeridian)>5 then t.startmeridian:=sgn(t.startmeridian)*5;
          if t.startmeridian=0 then dt:=5/60 // 5 minutes after meridian to prevent flip
                               else dt:=t.startmeridian;
          tt:=rmod(ht+dt+24,24);
          // check elevation
          if InTimeInterval(tt/24,hr/24,hs/24,st/24)=0 then
             t.starttime:=tt/24
          else
             t.starttime:=hr/24;
       end;
       // end from meridian
       if (t.endmeridian<>NullCoord) and (ti<2) then begin
          if abs(t.endmeridian)>5 then t.endmeridian:=sgn(t.endmeridian)*5;
          if t.endmeridian=0 then dt:=-5/60 // 5 minutes before meridian to prevent flip
                             else dt:=t.endmeridian;
          tt:=rmod(ht+dt+24,24);
          // check elevation
          if InTimeInterval(tt/24,hr/24,hs/24,st/24)=0 then
             t.endtime:=tt/24
          else
             t.endtime:=hs/24;
       end;
    end;
    // let time for the first exposure to run
    chkendtime:=rmod(t.endtime-enddelay+1,1);
    // test if in time interval
    intime:=InTimeInterval(frac(now),t.starttime,chkendtime,st/24);
    // test if skiped
    if t.skip then begin
      skipmsg:='';
      if (intime<0) and (t.starttime>=0) then begin
        SecondsToWait(t.starttime,true,stw,nd);
        if (stw>60) then begin
          SkipTarget:=true;
          skipmsg:=skipmsg+', '+Format(rsWaitToStartA, [TimeToStr(t.starttime)]);
        end;
      end;
      if (intime<=0) and (t.endtime>=0) then begin
        SecondsToWait(chkendtime,true,stw,nd);
        if stw<60 then begin
          SkipTarget:=true;
          skipmsg:=skipmsg+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]);
        end;
      end;
      if (intime>0) then begin
        SkipTarget:=true;
        skipmsg:=skipmsg+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]);
      end;
      if t.darknight then begin
        if (not DarkNight(now)) then begin
          SkipTarget:=true;
          skipmsg:=skipmsg+', '+rsWaitingForDa2;
        end;
      end;
      if SkipTarget then begin
        msg(Format(rsSkipTarget, [t.objectname])+skipmsg, 2);
        result:=false;
        exit;
      end;
    end;
    // start / stop timer
    if (intime>0) then begin
      msg(Format(rsTargetCancel, [t.objectname])+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]),2);
      result:=false;
      exit;
    end;
    if (intime<0) and (t.starttime>=0) then begin
      Mount.AbortMotion;
      msg(Format(rsWaitToStartA, [TimeToStr(t.starttime)]),1);
      wtok:=WaitTill(TimeToStr(t.starttime),true);
      if not wtok then begin
         msg(Format(rsTargetCancel, [t.objectname]),1);
         result:=false;
         exit;
      end;
    end;
    if (intime<=0) and (t.endtime>=0) then begin
       SecondsToWait(chkendtime,true,stw,nd);
       if stw>60 then begin
          SecondsToWait(t.endtime,true,stw,nd);
          msg(Format(rsTargetWillBe, [TimeToStr(t.endtime), inttostr(stw)]), 3);
          StopTargetTimer.Interval:=1000*stw;
          StopTargetTimer.Enabled:=true;
       end else begin
         msg(Format(rsTargetCancel, [t.objectname])+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]),2);
         result:=false;
         exit;
       end;
    end;
    // detect autofocus
    if (p<>nil)and (p.Count>0) then
       autofocusstart:=p.Steps[0].autofocusstart
    else
       autofocusstart:=false;

    if WeatherCancelRestart then exit;

    FTargetInitializing:=true;
    FWaitStarting:=false;

    if (not restart) then begin
      if  ((t.ra<>NullCoord)and(t.de<>NullCoord))or(t.pa<>NullCoord) then begin
        if (Autoguider<>nil)and(Autoguider.AutoguiderType<>agNONE)and(Autoguider.AutoguiderType<>agDITHER) then begin
          // stop guiding
          if Autoguider.State<>GUIDER_DISCONNECTED then begin
            if not StopGuider then exit;
            Wait(2);
            if not FRunning then exit;
            if WeatherCancelRestart then exit;
          end;
        end;
        // set rotator position
        if (t.pa<>NullCoord)and(Frotaror.Status=devConnected) then begin
          Frotaror.Angle:=t.pa;
        end;
        // set coordinates
        if ((t.ra<>NullCoord)and(t.de<>NullCoord)) then begin
          // disable astrometrypointing and autoguiding if first step is to move to focus star
          astrometrypointing:=t.astrometrypointing and (not (autofocusstart and (not InplaceAutofocus))) ;
          // must track before to slew
          mount.Track;
          // slew to coordinates
          FSlewRetry:=1;
          ok:=Slew(t.ra,t.de,astrometrypointing,t.astrometrypointing);
          if not ok then exit;
          Wait;
        end;
        if not FRunning then exit;
        if WeatherCancelRestart then exit;
      end;
      // check if the plans contain only calibration
      isCalibrationTarget:=true;
      if (p<>nil) then
        for i:=0 to p.Count-1 do begin
           if p.Steps[i].frtype=LIGHT then begin
              isCalibrationTarget:=false;
              break;
           end;
        end;
      // start mount tracking
      if isCalibrationTarget then
        mount.AbortMotion
      else if ((t.ra=NullCoord)or(t.de=NullCoord)) then
         mount.Track;
      // start guiding
      autostartguider:=(Autoguider<>nil)and(Autoguider.AutoguiderType<>agNONE)and
                       (Autoguider.AutoguiderType<>agDITHER) and (Autoguider.State<>GUIDER_DISCONNECTED)and
                       ((not autofocusstart)or (InplaceAutofocus and (not AutofocusPauseGuider))) and
                       (not isCalibrationTarget);
      if autostartguider then begin
        if not StartGuider then exit;
        Wait;
        if not FRunning then exit;
        if WeatherCancelRestart then exit;
        t.autoguiding:=true;
      end;
    end;
    result:=true;
  end;
  finally
    FTargetInitializing:=false;
    FWaitStarting:=false;
  end;
end;

function T_Targets.InitSkyFlat: boolean;
var i,n:integer;
    wtok,nd,ForceNextStartTime:boolean;
    stw: integer;
    sra,sde,sl,hp1,hp2: double;
    flt,nextt: TTarget;
    flp:T_Plan;
    fls:TStep;
    flfilter: TStringList;
    flexp: TFilterExp;
begin
  result:=false;
  ForceNextStartTime:=false;
  if not FRunning then exit;
  FTargetInitializing:=true;
  try
  // create a dynamic plan with all steps to run the flats, one step per filter
  flt:=Targets[FCurrentTarget];
  flt.autoguiding:=false;
  flp:=T_Plan(flt.plan);
  flfilter:=TStringList.Create;
  SplitRec(flt.flatfilters,';',flfilter);
  // remove blank filter name
  for i:=flfilter.count-1 downto 0 do
   if trim(flfilter[i])='' then flfilter.Delete(i);
  if flfilter.count>0 then begin
    // add filter exposure factor
    for i:=0 to flfilter.count-1 do begin
      flexp:=TFilterExp.Create;
      n:=FilterList.IndexOf(flfilter[i]);
      if n>=0 then
         flexp.ExpFact:=FilterExpFact[n]
      else
         flexp.ExpFact:=1.0;
      flfilter.Objects[i]:=flexp;
    end;
    // sort by filter exposure factor
    if flt.planname=FlatTimeName[0] then // Dusk, take darker filter first
      SortFilterListDec(flfilter)
    else                                 // Dawn, take lighter filter first
      SortFilterListInc(flfilter);
  end
  else begin
    flexp:=TFilterExp.Create;           // No filter
    flexp.ExpFact:=1.0;
    flfilter.AddObject(Filter0,flexp);
  end;
  flp.Clear;
  for i:=0 to flfilter.count-1 do
    if trim(flfilter[i])<>'' then begin
      fls:=TStep.Create;
      fls.frtype:=FLAT;
      fls.filter:=FilterList.IndexOf(flfilter[i]);
      fls.binx:=flt.FlatBinX;
      fls.biny:=flt.FlatBinY;
      fls.gain:=flt.FlatGain;
      fls.fstop:=flt.FlatFstop;
      fls.count:=flt.FlatCount;
      fls.exposure:=FlatMinExp;
      fls.dither:=false;
      fls.dithercount:=1;
      fls.autofocusstart:=false;
      fls.autofocus:=false;
      fls.autofocuscount:=10;
      if flfilter[i]=Filter0 then
         fls.description:=flt.planname+' flat '
      else
         fls.description:=flt.planname+' flat '+flfilter[i];
      flp.Add(fls);
    end;
  if flt.planname=FlatTimeName[0] then begin    // Dusk
    FlatWaitDusk:=true;
    FlatWaitDawn:=false;
    //Start when the Sun is 2 degree below horizon
    Sun(jdtoday+0.5,sra,sde,sl);
    Time_Alt(jdtoday, sra, sde, -2, hp1, hp2);
    if abs(hp2)<90 then
       flt.starttime:=rmod(hp2+ObsTimeZone+24,24)/24
    else begin
      msg(rsNoSuitableDu,1);
      exit;
    end;
    //Force stop when the Sun is 16 degree below horizon
    Time_Alt(jdtoday, sra, sde, -16, hp1, hp2);
    if abs(hp2)<90 then
       flt.endtime:=rmod(hp2+ObsTimeZone+24,24)/24
    else begin
      msg(rsNoSuitableDu,1);
      exit;
    end;
    // Update start time of next step to astronomical twilight if not already set
    if (FCurrentTarget+1)<NumTargets then begin
      nextt:=Targets[FCurrentTarget+1];
      if nextt.starttime<0 then begin
        Time_Alt(jdtoday, sra, sde, -18, hp1, hp2);
        if abs(hp2)<90 then begin
           ForceNextStartTime:=true;
           nextt.starttime:=rmod(hp2+ObsTimeZone+24,24)/24
        end;
      end;
    end;
  end
  else if flt.planname=FlatTimeName[1] then begin  // Dawn
    // stop main twilight timer
    if FSeqStopTwilight then StopTimer.Enabled:=false;
    FlatWaitDawn:=true;
    FlatWaitDusk:=false;
    //Start when the Sun is 16 degree below horizon
    Sun(jdtoday+0.5,sra,sde,sl);
    Time_Alt(jdtoday, sra, sde, -16, hp1, hp2);
    if abs(hp1)<90 then
       flt.starttime:=rmod(hp1+ObsTimeZone+24,24)/24
    else begin
      msg(rsNoSuitableDa,1);
      exit;
    end;
    //Force stop when the Sun is 2 degree below horizon
    Time_Alt(jdtoday, sra, sde, -2, hp1, hp2);
    if abs(hp1)<90 then
       flt.endtime:=rmod(hp1+ObsTimeZone+24,24)/24
    else begin
      msg(rsNoSuitableDa,1);
      exit;
    end;
  end
  else begin
    flt.starttime:=-1;
    flt.endtime:=-1;
  end;
  // slew near zenith and  stop tracking
  StopGuider;
  FlatSlewTime:=0;
  mount.SlewToSkyFlatPosition;
  // wait twilight
  if flt.starttime>=0 then begin
    msg(Format(rsWaitingForTw, [TimeToStr(flt.starttime)]),1);
    wtok:=WaitTill(TimeToStr(flt.starttime),true);
    if not wtok then begin
       msg(Format(rsTargetCancel, [flt.objectname]),1);
       if ForceNextStartTime then nextt.starttime:=-1;
       exit;
    end;
  end;
  if flt.endtime>=0 then begin
     SecondsToWait(flt.endtime,true,stw,nd);
     if stw>0 then begin
        StopTargetTimer.Interval:=1000*stw;
        StopTargetTimer.Enabled:=true;
     end else begin
       msg(Format(rsTargetCancel, [flt.objectname]),1);
       msg(Format(rsStopTimeAlre, [TimeToStr(flt.endtime)]),1);
       exit;
     end;
  end;
  // slew near zenith
  mount.SlewToSkyFlatPosition;
  result:=true;
  finally
    FTargetInitializing:=false;
    for i:=0 to flfilter.count-1 do flfilter.Objects[i].Free;
    flfilter.Free;
  end;
end;

procedure T_Targets.StopTargetTimerTimer(Sender: TObject);
begin
  StopTargetTimer.Enabled:=false;
  msg(Format(rsStopTheCurre2, [TimeToStr(now)]),1);
  ForceNextTarget;
end;

procedure T_Targets.StartPlanTimerTimer(Sender: TObject);
begin
  StartPlanTimer.Enabled:=false;
  StartPlan;
end;

procedure T_Targets.StartPlan;
var t: TTarget;
    p: T_Plan;
begin
  if preview.Running then begin
      msg(rsStopPreview,2);
      camera.AbortExposure;
      preview.stop;
      StartPlanTimer.Enabled:=true;
      exit;
  end;
  t:=Targets[FCurrentTarget];
  if t<>nil then begin
    p:=T_Plan(t.plan);
    if p.Running then exit;
    if t.objectname<>'None' then
       Fcapture.Fname.Text:=trim(t.objectname);
    p.ObjectName:=t.objectname;
    TargetTimeStart:=now;
    p.Start;
  end;
end;

function T_Targets.GetBusy: boolean;
var t: TTarget;
    p: T_Plan;
begin
  result:= FInitializing and FTargetInitializing;
  if FRunning and(FCurrentTarget>=0) then begin
    t:=Targets[FCurrentTarget];
    if t<>nil then begin
      p:=T_Plan(t.plan);
      result:=result or p.Running;
    end;
  end;
end;

function T_Targets.StopGuider:boolean;
begin
  result:=false;
  if (Autoguider=nil)or(not Autoguider.Running) then exit;
  msg(rsStopAutoguid,2);
  Autoguider.Guide(false);
  result:=Autoguider.WaitBusy(60);
end;

function T_Targets.StartGuider:boolean;
begin
 result:=false;
 if Autoguider=nil then exit;
  msg(rsStartAutogui,2);
  Autoguider.Guide(true);
  result:=Autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
  if FRunning and (not result)and(not Unattended) then begin
    f_pause.Caption:=rsAutoguiderSt2;
    f_pause.Text:=Format(rsAutoguiderNo2, [inttostr(CalibrationDelay+
      SettleMaxTime), crlf]);
    if f_pause.Wait(WaitResponseTime,false) then begin
       result:=StartGuider();
       exit;
    end;
  end;
end;

function T_Targets.Slew(ra,de: double; precision,planprecision: boolean):boolean;
var err: double;
    errtxt: string;
    prec,exp:double;
    fi,cormethod,bin,maxretry,delay: integer;
begin
  // Slew to J2000 ra,de
  result:=false;
  FTargetCoord:=false;
  NeedRecenterTarget:=false;
  if FSlewRetry>3 then begin
     msg(rsSlewAbortedA,0);
     exit;
  end;
  if (Mount=nil)or(Mount.Status<>devConnected) then begin
    msg(rsErrorMountNo,0);
    exit;
  end;
  J2000ToMount(mount.EquinoxJD,ra,de);
  prec:=config.GetValue('/PrecSlew/Precision',5.0)/60;
  if precision then begin
    cormethod:=config.GetValue('/PrecSlew/Method',1);
    maxretry:=config.GetValue('/PrecSlew/Retry',3);
    exp:=config.GetValue('/PrecSlew/Exposure',10.0);
    bin:=config.GetValue('/PrecSlew/Binning',1);
    fi:=config.GetValue('/PrecSlew/Filter',0);
    result:=astrometry.PrecisionSlew(ra,de,prec,exp,fi,bin,bin,cormethod,maxretry,err);
    if result then begin
      FTargetCoord:=true;
      FTargetRA:=ra;
      FTargetDE:=de;
      MountToLocal(mount.EquinoxJD,FTargetRA,FTargetDE);
      FTargetRA:=deg2rad*15*FTargetRA;
      FTargetDE:=deg2rad*FTargetDE;
    end;
  end
  else begin
    delay:=config.GetValue('/PrecSlew/Delay',5);
    Mount.Slew(ra, de);
    Wait(delay);
    err:=0;
    result:=true;
    if result and planprecision then begin
      FTargetCoord:=true;
      FTargetRA:=ra;
      FTargetDE:=de;
      MountToLocal(mount.EquinoxJD,FTargetRA,FTargetDE);
      FTargetRA:=deg2rad*15*FTargetRA;
      FTargetDE:=deg2rad*FTargetDE;
    end;
  end;
  if not FRunning then exit;
  if not result then begin
    if (err*60)<9000 then
       errtxt:=Format(rsTelescopeSle, [FormatFloat(f2, err*60)])
    else
       errtxt:=rsTelescopeSle3;
    msg(errtxt,0);
    if not Unattended then begin
      f_pause.Caption:=rsTelescopeSle2;
      if (err*60)<9000 then
         errtxt:=Format(rsAfterTelesco, [FormatFloat(f2, err*60), crlf])
      else
         errtxt:=rsTelescopeSle3+crlf+rsDoYouWantToR;
      f_pause.Text:=errtxt;
      if f_pause.Wait(WaitResponseTime) then begin
         inc(FSlewRetry);
         MountToJ2000(mount.EquinoxJD,ra,de);
         result:=Slew(ra,de,precision,planprecision);
         exit;
      end;
    end;
  end;
end;

procedure T_Targets.TargetTimerTimer(Sender: TObject);
var tt: double;
    t: TTarget;
    r: string;
begin
 if FRunning then begin
   FInitializing:=false;
   t:=Targets[FCurrentTarget];
   if not TargetRepeatTimer.Enabled then begin
      if not T_Plan(t.plan).Running then begin
        if (t<>nil) then begin
        inc(t.repeatdone);
        if (TargetForceNext)or(t.repeatdone>=t.repeatcount) then begin
           NextTarget;
        end
        else begin
           tt:=t.delay-(Now-TargetTimeStart)*secperday;
           if tt<1 then tt:=1;
           if tt>1 then msg(Format(rsWaitSecondsB, [FormatFloat(f1, tt),
             IntToStr(t.repeatdone)]),2);
           TargetRepeatTimer.Interval:=trunc(1000*tt);
           TargetRepeatTimer.Enabled:=true;
           TargetDelayEnd:=now+tt/secperday;
           if t.preview and (tt>5)and(tt>(2*t.previewexposure)) then begin
             if t.previewexposure>0 then Preview.Exposure:=t.previewexposure;
             Preview.Binning.Text:=Capture.Binning.Text;
             Preview.BtnLoop.Click;
           end;
        end;
        end
        else begin
         NextTarget;
        end;
      end;
   end
   else begin
     tt:=(TargetDelayEnd-Now)*secperday;
     ShowDelayMsg(Format(rsContinueInSe, [FormatFloat(f0, tt)]));
   end;
 end
 else begin
  TargetTimer.Enabled:=false;
  StopTimer.Enabled:=false;
  FCurrentTarget:=-1;
  msg(Format(rsSequenceStop2, [FName]),1);
  if assigned(FonEndSequence) then FonEndSequence(nil);
  ShowDelayMsg('');
  if EmailEndSequence then begin
    r:=email(Format(rsSequenceStop2, [FName]),Format(rsSequenceStop2, [FName]));
    if r='' then r:=rsEmailSentSuc;
    msg(r,9);
  end;
 end;
end;

procedure T_Targets.TargetRepeatTimerTimer(Sender: TObject);
var t: TTarget;
    p: T_Plan;
    i,n: integer;
begin
 if FRunning then begin
    TargetRepeatTimer.Enabled:=false;
    ShowDelayMsg('');
    t:=Targets[FCurrentTarget];
    if t<>nil then begin
      Msg(Format(rsRepeatTarget, [blank+inttostr(t.repeatdone+1),t.repeatcount_str, t.objectname]),1);
      ShowDelayMsg(Format(rsRepeatTarget, [blank+inttostr(t.repeatdone+1),t.repeatcount_str, t.objectname]));
      if t.preview and Preview.Running then Preview.BtnLoop.Click;
      p:=T_Plan(t.plan);
      if p<>nil then begin
        // reset step done counts
        n:=p.Count;
        for i:=0 to n-1 do begin
           p.Steps[i].donecount:=0;
        end;
      end;
      TargetTimeStart:=now;
      StartPlan;
    end
    else FRunning:=false;
 end;
end;

procedure T_Targets.RunErrorAction;
var path,sname: string;
begin
  f_pause.Caption:=rsTerminationO;
  f_pause.Text := rsDoYouWantToR2;
  if not f_pause.Wait(20, true, rsYes, rsNo) then begin
    msg(rsRequestedToN, 1);
    exit;
  end;
  msg(rsExecutingThe,1);
  RunEndAction(false);
  if OnErrorRunScript then begin
    path:=ScriptDir[1].path;
    sname:=OnErrorScript;
    if FileExistsUTF8(slash(path)+sname+'.script') then begin
       f_scriptengine.RunScript(sname,path);
    end;
  end;
end;

procedure T_Targets.RunEndAction(confirm: boolean=true);
var path,sname: string;
begin
if AtEndStopTracking or AtEndPark or AtEndCloseDome or AtEndWarmCamera or AtEndRunScript or AtEndShutdown then begin
  if confirm then begin
    f_pause.Caption:=rsTerminationO;
    f_pause.Text := rsDoYouWantToR2;
    if not f_pause.Wait(20, true, rsYes, rsNo) then begin
      msg(rsRequestedToN, 1);
      exit;
    end;
  end;
  msg(rsExecutingThe2,1);
  if AtEndStopTracking then begin
    msg(rsStopTelescop2,1);
    Mount.AbortMotion;
  end;
  if AtEndPark then begin
    msg(rsParkTheTeles2,1);
    Mount.Park:=true;
  end;
  if AtEndCloseDome then begin
    msg(rsStopDomeSlav,1);
    Dome.Slave:=false;
    msg(rsParkDome,1);
    Dome.Park:=true;
    msg(rsCloseDome,1);
    Dome.Shutter:=false;
  end;
  if AtEndWarmCamera then begin
    msg(rsWarmTheCamer,1);
    Camera.Temperature:=20.0;
  end;
  if AtEndRunScript then begin
    path:=ScriptDir[1].path;
    sname:=AtEndScript;
    if FileExistsUTF8(slash(path)+sname+'.script') then begin
       f_scriptengine.RunScript(sname,path);
    end;
  end;
  if AtEndShutdown then begin
     msg(rsExitProgram,1);
     if Assigned(FonShutdown) then FonShutdown(self);
  end;
end
else
  msg(rsNoTerminatio, 1);
end;

end.

