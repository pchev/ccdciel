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
  fu_capture, fu_preview, fu_filterwheel, cu_mount, cu_camera, cu_autoguider, cu_autoguider_internal, cu_astrometry,
  fu_safety, fu_weather, cu_dome, u_ccdconfig, cu_sequencefile, fu_internalguider, math,
  u_translation, LazFileUtils, Controls, Dialogs, ExtCtrls,Classes, Forms, SysUtils;

type

  TTarget = Class(TObject)
              public
              objectname, planname, path, scriptargs: shortstring;
              starttime,endtime,startmeridian,endmeridian,ra,de,pa,solarV,solarPA: double;
              startrise,endset,darknight,skip: boolean;
              repeatcount,repeatdone: integer;
              FlatBinX,FlatBinY,FlatCount: integer;
              FlatGain,FlatOffset: integer;
              FlatFilters: shortstring;
              FlatFstop: shortstring;
              preview,astrometrypointing,updatecoord,inplaceautofocus,autofocustemp,
              autoguiding,solartracking,noautoguidingchange: boolean;
              delay, previewexposure: double;
              plan :TComponent;
              constructor Create;
              destructor Destroy; override;
              procedure Assign(Source: TTarget);
              function previewexposure_str: string;
              function delay_str: string;
              function repeatcount_str: string;
              function ra_str: string;
              function de_str: string;
              function pa_str: string;
              function id: LongWord;
            end;

  TTargetList = array of TTarget;

  T_Targets = class(TComponent)
    private
      TargetTimer: TTimer;
      TargetRepeatTimer: TTimer;
      RestartTimer: TTimer;
      StopTimer: TTimer;
      StopTargetTimer: TTimer;
      WeatherRestartTimer: TTimer;
      FTargetsChange: TNotifyEvent;
      FPlanChange: TNotifyEvent;
      FonMsg: TNotifyMsg;
      FDelayMsg: TNotifyStr;
      FonEndSequence: TNotifyEvent;
      FonShutdown: TNotifyEvent;
      FonRestart: TNotifyEvent;
      Fcapture: Tf_capture;
      Fpreview: Tf_preview;
      Ffilter: Tf_filterwheel;
      Fweather: Tf_weather;
      Fsafety: Tf_safety;
      Fdome: T_dome;
      Fmount: T_mount;
      Fcamera: T_camera;
      Frotator: T_rotator;
      Fautoguider: T_autoguider;
      Finternalguider: Tf_internalguider;
      Fastrometry: TAstrometry;
      Fplanetarium: TPlanetarium;
      StartPlanTimer: TTimer;
      FTargetCoord: boolean;
      FTargetRA,FTargetDE: double;
      FIgnoreRestart: boolean;
      FTargetsRepeatCount: integer;
      FFileVersion, FSlewRetry: integer;
      FUpdatecoordDelay: integer;
      FAtEndPark, FAtEndCloseDome, FAtEndStopTracking,FAtEndWarmCamera,FAtEndRunScript,FOnErrorRunScript,FAtEndShutdown: boolean;
      FAtEndScript, FOnErrorScript: string;
      FAtStartCool,FAtStartUnpark, FAtStartRunScript: boolean;
      FAtStartScript: string;
      SkipTarget: boolean;
      TargetForceNext: boolean;
      FDoneStatus, FLastDoneStep: string;
      FAllDone, FAllStepsDone: boolean;
      FFilterList,FBinningList: Tstringlist;
      FOriginalFilter: TSaveFilter;
      FSequenceFile: T_SequenceFile;
      FRestarting, FRealRestart: boolean;
      Fslewing: boolean;
      InitTargetError: string;
      LockTargetTimer: boolean;
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
      function SetTargetTime(t:TTarget; middletime:double; out pivot:double):boolean;
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
      procedure RestartTimerTimer(Sender: TObject);
      procedure StopTimerTimer(Sender: TObject);
      procedure StopTargetTimerTimer(Sender: TObject);
      procedure WeatherRestartTimerTimer(Sender: TObject);
      procedure PlanStepChange(Sender: TObject);
      procedure CompatLoadPlan(p: T_Plan; plan,obj:string);
      procedure ClearRunning;
      function  GetScriptRunning: boolean;
    protected
      Ftargets: TTargetList;
      NumTargets: integer;
      FCurrentTarget: integer;
      FTargetsRepeat: integer;
      FResetRepeat: boolean;
      FSeqStartAt,FSeqStopAt,FSeqStartTime: TDateTime;
      FSeqStart,FSeqStop: boolean;
      FSeqStartTwilight,FSeqStopTwilight,FSeqLockTwilight: boolean;
      TargetTimeStart,TargetDelayEnd: TDateTime;
      FRunning,FScriptRunning: boolean;
      FInitializing, FFinalizing, FStopping, FDawnFlatNow: boolean;
      FUnattended: boolean;
      FName: string;
    public
      FTargetInitializing, FWaitStarting: boolean;
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear;
      procedure LoadTargets(fn: string);
      procedure SaveTargets(fn:string);
      function Add(t: TTarget):integer;
      procedure AssignTarget(Source: T_Targets);
      procedure UpdateLive(Source:T_Targets);
      procedure Start;
      procedure Restart;
      procedure Stop;
      procedure Abort;
      procedure WeatherPause;
      procedure WeatherRestart;
      procedure ForceNextTarget;
      procedure SaveDoneCount;
      function  CheckStatus:boolean;
      function  CheckDoneCount:boolean;
      procedure ClearDoneCount(ClearRepeat: boolean);
      procedure CreateSkyFlatPlan(flt: TTarget);
      function IndexOf(id:LongWord):integer;
      property SequenceFile: T_SequenceFile read FSequenceFile write FSequenceFile;
      property FileVersion: integer read FFileVersion write FFileVersion;
      property FilList: Tstringlist read FFilterList;
      property OriginalFilter: TSaveFilter read FOriginalFilter;
      property BinList: Tstringlist read FBinningList;
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
      property AllDone: boolean read FAllDone;
      property Busy: boolean read GetBusy;
      property Restarting: boolean read FRestarting;
      property Slewing: boolean read Fslewing;
      property Running: boolean read FRunning;
      property ScriptRunning: boolean read GetScriptRunning;
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
      property Rotator: T_rotator read Frotator write Frotator;
      property Filter: Tf_filterwheel read Ffilter write SetFilter;
      property Weather: Tf_weather read Fweather write Fweather;
      property Safety: Tf_safety read Fsafety write Fsafety;
      property Dome: T_dome read Fdome write Fdome;
      property Autoguider: T_autoguider read Fautoguider write SetAutoguider;
      property InternalGuider: Tf_internalguider read Finternalguider write Finternalguider;
      property Astrometry: TAstrometry read Fastrometry write SetAstrometry;
      property Planetarium: TPlanetarium read Fplanetarium write Fplanetarium;
      property DelayMsg: TNotifyStr read FDelayMsg write FDelayMsg;
      property onMsg: TNotifyMsg read FonMsg write FonMsg;
      property onEndSequence: TNotifyEvent read FonEndSequence write FonEndSequence;
      property AtStartCool: boolean  read FAtStartCool write FAtStartCool;
      property AtStartUnpark: boolean read FAtStartUnpark write FAtStartUnpark;
      property AtStartRunScript: boolean read FAtStartRunScript write FAtStartRunScript;
      property AtStartScript: string read FAtStartScript write FAtStartScript;
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
      property OnRestart: TNotifyEvent read FonRestart write FonRestart;
  end;

  function TemplateModified(p:T_Plan):boolean;

implementation

constructor T_Targets.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSequenceFile:=T_SequenceFile.Create(nil);
  FFilterList:=Tstringlist.Create;
  FBinningList:=Tstringlist.Create;
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
  FRestarting:=False;
  Fslewing:=False;
  FCurrentTarget:=-1;
  FSeqStartAt:=0;
  FSeqStopAt:=0;
  FSeqStart:=false;
  FSeqStop:=false;
  FSeqStartTwilight:=false;
  FSeqStopTwilight:=false;
  FSeqLockTwilight:=false;
  FAtStartCool:=false;
  FAtStartUnpark:=false;
  FAtStartRunScript:=false;
  FAtStartScript:='';
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
  FFinalizing:=false;
  FStopping:=false;
  FDawnFlatNow:=false;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  SkipTarget:=false;
  LockTargetTimer:=false;
  TargetTimer:=TTimer.Create(self);
  TargetTimer.Enabled:=false;
  TargetTimer.Interval:=1000;
  TargetTimer.OnTimer:=@TargetTimerTimer;
  TargetRepeatTimer:=TTimer.Create(self);
  TargetRepeatTimer.Enabled:=false;
  TargetRepeatTimer.Interval:=1000;
  TargetRepeatTimer.OnTimer:=@TargetRepeatTimerTimer;
  RestartTimer:=TTimer.Create(self);
  RestartTimer.Enabled:=false;
  RestartTimer.Interval:=1000;
  RestartTimer.OnTimer:=@RestartTimerTimer;
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
  FFilterList.Free;
  FBinningList.Free;
  FSequenceFile.Free;
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
  FFilterList.Clear;
  FBinningList.Clear;
  if FilterList<>nil then FFilterList.Assign(FilterList);
  if BinningList<>nil then FBinningList.Assign(BinningList);
  FName:='';
  if Assigned(FTargetsChange) then FTargetsChange(self);
end;

function T_Targets.Add(t: TTarget):integer;
var p:T_Plan;
begin
  p:=T_Plan(t.plan);
  if p=nil then p:=T_Plan.Create(nil);
  p.onPlanChange:=FPlanChange;
  p.onStepProgress:=@PlanStepChange;
  p.SequenceFile:=FSequenceFile;
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

procedure T_Targets.AssignTarget(Source: T_Targets);
var i,n:integer;
    t:TTarget;
begin
  // No feedback to sequence control
{ FTargetsChange
  FPlanChange
  FDelayMsg
  FonEndSequence
  FonShutdown }

  // use own sequence file
{ FSequenceFile }

  // static properties
  FonMsg := Source.FonMsg;
  Fcapture := Source.Fcapture;
  Fpreview := Source.Fpreview;
  Ffilter :=  Source.Ffilter;
  Fweather := Source.Fweather;
  Fsafety := Source.Fsafety;
  Fdome := Source.Fdome;
  Fmount := Source.Fmount;
  Fcamera := Source.Fcamera;
  Frotator := Source.Frotator;
  Fautoguider := Source.Fautoguider;
  Fastrometry := Source.Fastrometry;
  Fplanetarium := Source.Fplanetarium;

  // do not copy working variable
{ FTargetCoord
  FTargetRA
  FTargetDE
  FSlewRetry
  SkipTarget
  TargetForceNext
  FDoneStatus
  FLastDoneStep
  FAllDone
  FCurrentTarget
  TargetTimeStart
  TargetDelayEnd
  FRunning
  FScriptRunning
  FInitializing
  FFinalizing
  FStopping
  FDawnFlat
  FUnattended
  FTargetInitializing
  FSeqLockTwilight
  FSeqStartTime
  FWaitStarting }

  // sequence definition
  FName:= Source.FName;
  FFileVersion := Source.FFileVersion;
  FIgnoreRestart := Source.FIgnoreRestart;
  FTargetsRepeatCount := Source.FTargetsRepeatCount;
  FAtEndPark := Source.FAtEndPark;
  FAtEndCloseDome := Source.FAtEndCloseDome;
  FAtEndStopTracking := Source.FAtEndStopTracking;
  FAtEndWarmCamera := Source.FAtEndWarmCamera;
  FAtEndShutdown := Source.FAtEndShutdown;
  FAtEndRunScript := Source.FAtEndRunScript;
  FAtEndScript := Source.FAtEndScript;
  FOnErrorRunScript := Source.FOnErrorRunScript;
  FOnErrorScript := Source.FOnErrorScript;
  FAtStartCool := Source.FAtStartCool;
  FAtStartUnpark := Source.FAtStartUnpark;
  FAtStartRunScript := Source.FAtStartRunScript;
  FAtStartScript := Source.FAtStartScript;
  FFilterList.Clear;
  for i:=0 to Source.FFilterList.Count-1 do
    FFilterList.Add(Source.FFilterList[i]);
  FBinningList.Clear;
  for i:=0 to Source.FBinningList.Count-1 do
    FBinningList.Add(Source.FBinningList[i]);
  for i:=0 to SaveFilterNum do
    FOriginalFilter[i] := Source.FOriginalFilter[i];
  FTargetsRepeat := Source.FTargetsRepeat;
  FResetRepeat := Source.FResetRepeat;
  FSeqStart := Source.FSeqStart;
  FSeqStop := Source.FSeqStop;
  FSeqStartAt := Source.FSeqStartAt;
  FSeqStopAt := Source.FSeqStopAt;
  FSeqStartTwilight := Source.FSeqStartTwilight;
  FSeqStopTwilight := Source.FSeqStopTwilight;
  n := Source.NumTargets;
  for i:=0 to n-1 do begin
    t:=TTarget.Create;
    t.Assign(Source.Ftargets[i]);
    Add(t);
  end;
end;

function T_Targets.IndexOf(id:LongWord):integer;
var i: integer;
begin
  result:=-1;
  for i:=0 to NumTargets-1 do begin
     if id=Ftargets[i].id then begin
       result:=i;
       break;
     end;
  end;
end;

procedure T_Targets.UpdateLive(Source:T_Targets);
var StartChange,RestartTarget: boolean;
    i,newcurTarget,newcurStep,newdonecount: integer;
    tid,sid: LongWord;
    t: TTarget;
    p: TStep;
begin
  try
  msg(rsApplyEditing, 1);
  RestartTarget:=false;
  StartChange:=false;
  // properties that can be changed without specific action
  FFileVersion := Source.FFileVersion;
  FTargetsRepeatCount := Source.FTargetsRepeatCount;
  FAtEndPark := Source.FAtEndPark;
  FAtEndCloseDome := Source.FAtEndCloseDome;
  FAtEndStopTracking := Source.FAtEndStopTracking;
  FAtEndWarmCamera := Source.FAtEndWarmCamera;
  FAtEndShutdown := Source.FAtEndShutdown;
  FAtEndRunScript := Source.FAtEndRunScript;
  FAtEndScript := Source.FAtEndScript;
  FOnErrorRunScript := Source.FOnErrorRunScript;
  FOnErrorScript := Source.FOnErrorScript;

  // change to start condition
  StartChange:=
    (FAtStartCool <> Source.FAtStartCool) or
    (FAtStartUnpark <> Source.FAtStartUnpark) or
    (FAtStartRunScript <> Source.FAtStartRunScript) or
    (FAtStartScript <> Source.FAtStartScript) or
    (FSeqStart <> Source.FSeqStart) or
    (FSeqStop <> Source.FSeqStop) or
    (FSeqStartAt <> Source.FSeqStartAt) or
    (FSeqStopAt <> Source.FSeqStopAt) or
    (FSeqStartTwilight <> Source.FSeqStartTwilight) or
    (FSeqStopTwilight <> Source.FSeqStopTwilight);
  FAtStartCool := Source.FAtStartCool;
  FAtStartUnpark := Source.FAtStartUnpark;
  FAtStartRunScript := Source.FAtStartRunScript;
  FAtStartScript := Source.FAtStartScript;
  FSeqStart := Source.FSeqStart;
  FSeqStop := Source.FSeqStop;
  FSeqStartAt := Source.FSeqStartAt;
  FSeqStopAt := Source.FSeqStopAt;
  FSeqStartTwilight := Source.FSeqStartTwilight;
  FSeqStopTwilight := Source.FSeqStopTwilight;

  // the targets and steps list
  if FCurrentTarget>=0 then begin
    // identify the current target in the new sequence
    tid:=Ftargets[FCurrentTarget].id;
    newcurTarget:=Source.IndexOf(tid);
    if newcurTarget>=0 then begin
      // identify the current step in the new sequence
      sid:=T_Plan(Ftargets[FCurrentTarget].plan).Steps[T_Plan(Ftargets[FCurrentTarget].plan).CurrentStep].id;
      newcurStep:=T_Plan(Source.Ftargets[newcurTarget].plan).indexof(sid);
      if newcurStep<0 then begin
        // the running step is no more found, need to restart all
        msg('Running step of active target no more found.', 9);
        ClearRunning;
        RestartTarget:=true;
      end;
      // check global target setting that require a restart
      if ((Ftargets[FCurrentTarget].starttime<>Source.Ftargets[newcurTarget].starttime)and((Source.Ftargets[newcurTarget].startmeridian=NullCoord)and(not Source.Ftargets[newcurTarget].startrise))) or
         ((Ftargets[FCurrentTarget].endtime<>Source.Ftargets[newcurTarget].endtime)and((Source.Ftargets[newcurTarget].endmeridian=NullCoord)and(not Source.Ftargets[newcurTarget].endset))) or
         (Ftargets[FCurrentTarget].startmeridian<>Source.Ftargets[newcurTarget].startmeridian) or
         (Ftargets[FCurrentTarget].endmeridian<>Source.Ftargets[newcurTarget].endmeridian) or
         (Ftargets[FCurrentTarget].startrise<>Source.Ftargets[newcurTarget].startrise) or
         (Ftargets[FCurrentTarget].endset<>Source.Ftargets[newcurTarget].endset) or
         (Ftargets[FCurrentTarget].darknight<>Source.Ftargets[newcurTarget].darknight) or
         (Ftargets[FCurrentTarget].skip<>Source.Ftargets[newcurTarget].skip)
         then begin
           msg('Start or end conditions of active target are modified.', 9);
           ClearRunning;
           RestartTarget:=true;
         end;
    end
    else begin
      // the running target is no more found, need to restart all
      msg('Running target no more found.', 9);
      ClearRunning;
      RestartTarget:=true;
    end;
    if (not RestartTarget) then begin
      // Target and plan can be updated without interruption,
      // modification to target or step before the running one are
      // taken into account only at the next repeat, they are not redone now.
      msg(rsUpdateSequen, 3);
      newdonecount:=T_Plan(Ftargets[FCurrentTarget].plan).Steps[T_Plan(Ftargets[FCurrentTarget].plan).CurrentStep].donecount;
      for i:=0 to NumTargets-1 do
        if FTargets[i]<>nil then FTargets[i].Free;
      SetLength(Ftargets,0);
      NumTargets := 0;
      for i:=0 to Source.NumTargets-1 do begin
        // create new target
        t:=TTarget.Create;
        t.Assign(Source.Ftargets[i]);
        if i=newcurTarget then begin
          // this is the running target, update the value for the current step
          // in the Capture tool for next exposure
          T_Plan(t.plan).CurrentStep:=newcurStep;
          p:=T_Plan(t.plan).Steps[newcurStep];
          p.donecount:=newdonecount;
          if p.exposure>=0 then Fcapture.ExposureTime:=p.exposure;
          Fcapture.StackNum.Value:=p.stackcount;
          Fcapture.Binning.Text:=p.binning_str;
          Fcapture.Gain:=p.gain;
          Fcapture.Offset:=p.offset;
          if p.fstop<>'' then Fcapture.Fnumber.Text:=p.fstop;
          Fcapture.SeqNum.Value:=p.count;
          Fcapture.SeqCount:=CurrentDoneCount+1;
          Fcapture.FrameType:=p.frtype;
          Fcapture.CheckBoxDither.Checked:=p.dither;
          Fcapture.DitherCount.Value:=p.dithercount;
          Fcapture.CheckBoxFocus.Checked:=p.autofocus;
          Fcapture.FocusCount.Value:=p.autofocuscount;
          Fcapture.CheckLight(nil);
        end;
        // add target to sequence
        Add(t);
      end;
      // set new current target
      FCurrentTarget:=newcurTarget;
    end
    else begin
      // running target or step deleted, clear and apply new sequence in block
      msg(rsUpdateActive, 3);
      for i:=0 to NumTargets-1 do
        if FTargets[i]<>nil then FTargets[i].Free;
      SetLength(Ftargets,0);
      NumTargets := 0;
      for i:=0 to Source.NumTargets-1 do begin
        t:=TTarget.Create;
        t.Assign(Source.Ftargets[i]);
        Add(t);
      end;
    end;
  end
  else begin
    // no running target, clear and apply new sequence in block
    msg(rsUpdateFullSe, 3);
    for i:=0 to NumTargets-1 do
      if FTargets[i]<>nil then FTargets[i].Free;
    SetLength(Ftargets,0);
    NumTargets := 0;
    for i:=0 to Source.NumTargets-1 do begin
      t:=TTarget.Create;
      t.Assign(Source.Ftargets[i]);
      Add(t);
    end;
    if assigned(FPlanChange) then FPlanChange(Ftargets[0].plan);
  end;

  // change to global sequence repeat
  // after assigning the new targets
  FIgnoreRestart := Source.FIgnoreRestart;
  if FIgnoreRestart then ClearDoneCount(true);
  FTargetsRepeat := Source.FTargetsRepeat;
  FResetRepeat := Source.FResetRepeat; // will be applied at the next repeat

  TargetName:= Source.TargetName;

  // save the updated file
  SaveTargets(FSequenceFile.Filename);

  // Apply startup change immediatelly only if the sequence is waiting to start
  if StartChange and WaitStarting then begin
    msg(rsRestartTheSe, 3);
    if assigned(FonRestart) then FonRestart(self);
    exit;
  end;

  FRestarting:=true;
  if RestartTarget then begin
    // Stop and restart the current target
    msg(rsActiveTarget, 3);
    FRealRestart:=true;
    Capture.Running:=false;
    RunningCapture:=false;
    camera.AbortExposureButNotSequence;
    wait(1);
  end
  else begin
    msg(rsContinueTheS, 3);
    FRealRestart:=false;
  end;
  Restart;

  except
    on E: Exception do msg(Format(rsErrorUpdatin, [E.Message]), 1);
  end;

end;

procedure T_Targets.LoadTargets(fn: string);
var t:TTarget;
    p:T_Plan;
    s: TStep;
    x, str:string;
    i,j,k,m,n: integer;
begin
   msg('',2);
   FSequenceFile.Clear;
   FSequenceFile.Filename:=fn;
   CurrentSeqName:=FSequenceFile.CurrentName;
   Clear;
   TargetName:=CurrentSeqName;
   n:=FSequenceFile.Items.GetValue('/TargetNum',0);
   FileVersion      :=FSequenceFile.Items.GetValue('/Version',1);
   TargetsRepeat    :=FSequenceFile.Items.GetValue('/RepeatCount',1);
   IgnoreRestart    := FSequenceFile.Items.GetValue('/Targets/IgnoreRestart',true);
   if IgnoreRestart then
      TargetsRepeatCount:=0
   else
      TargetsRepeatCount:=FSequenceFile.Items.GetValue('/Targets/RepeatDone',0);
   ResetRepeat      := FSequenceFile.Items.GetValue('/Targets/ResetRepeat',true);
   SeqStart         := FSequenceFile.Items.GetValue('/Startup/SeqStart',false);
   SeqStop          := FSequenceFile.Items.GetValue('/Startup/SeqStop',false);
   SeqStartTwilight := FSequenceFile.Items.GetValue('/Startup/SeqStartTwilight',false);
   SeqStopTwilight  := FSequenceFile.Items.GetValue('/Startup/SeqStopTwilight',false);
   SeqStartAt       := StrToTimeDef(FSequenceFile.Items.GetValue('/Startup/SeqStartAt','00:00:00'),0);
   SeqStopAt        := StrToTimeDef(FSequenceFile.Items.GetValue('/Startup/SeqStopAt','00:00:00'),0);
   AtStartCool      := FSequenceFile.Items.GetValue('/Startup/CoolCamera',false);
   AtStartUnpark    := FSequenceFile.Items.GetValue('/Startup/Unpark',false);
   AtStartRunScript := FSequenceFile.Items.GetValue('/Startup/RunScript',false);
   AtStartScript    := FSequenceFile.Items.GetValue('/Startup/StartScript','');
   AtEndStopTracking:= FSequenceFile.Items.GetValue('/Termination/StopTracking',true);
   AtEndPark        := FSequenceFile.Items.GetValue('/Termination/Park',false);
   AtEndCloseDome   := FSequenceFile.Items.GetValue('/Termination/CloseDome',false);
   AtEndWarmCamera  := FSequenceFile.Items.GetValue('/Termination/WarmCamera',false);
   AtEndRunScript   := FSequenceFile.Items.GetValue('/Termination/RunScript',false);
   OnErrorRunScript := FSequenceFile.Items.GetValue('/Termination/ErrorRunScript',false);
   AtEndScript      := FSequenceFile.Items.GetValue('/Termination/EndScript','');
   OnErrorScript    := FSequenceFile.Items.GetValue('/Termination/ErrorScript','');
   if FileVersion<>TargetFileVersion then
      msg('Upgrade sequence file '+CurrentSeqName+' from version '+IntToStr(FileVersion)+' to version '+IntToStr(TargetFileVersion),1);
   if FileVersion<3 then begin
     // compatibility with previous version
     if FileExistsUTF8(slash(ScriptDir[1].path)+'end_sequence.script') then begin
        AtEndStopTracking:=false;
        AtEndRunScript:=true;
        AtEndScript:='end_sequence';
     end;
   end;
   if n>0 then begin
     for i:=1 to n do begin
       t:=TTarget.Create;
       t.objectname:=trim(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/ObjectName',''));
       t.planname:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan','');
       t.path:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Path','');
       t.scriptargs:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/ScriptArgs','');
       if FileVersion>=2 then begin
         t.starttime:=StrToTimeDef(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartTime',''),-1);
         t.endtime:=StrToTimeDef(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndTime',''),-1);
       end else begin
         t.starttime:=-1;
         t.endtime:=-1;
       end;
       t.startrise:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartRise',false);
       t.endset:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndSet',false);
       t.startmeridian:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartMeridian',NullCoord);
       t.endmeridian:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndMeridian',NullCoord);
       t.darknight:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/DarkNight',false);
       t.skip:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Skip',false);
       x:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RA','');
       if x='-' then
         t.ra:=NullCoord
       else
         t.ra:=StrToAR(x);
       x:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Dec','');
       if x='-' then
         t.de:=NullCoord
       else
         t.de:=StrToDE(x);
       // default to rise/set if nothing indicated but valid coordinates
       if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
          if (t.starttime=-1)and(t.startrise=false)and(t.startmeridian=NullCoord) then
            t.startrise:=true;
          if (t.endtime=-1)and(t.endset=false)and(t.endmeridian=NullCoord) then
            t.endset:=true;
       end;
       if (FileVersion<4)and(t.ra<>NullCoord)and(t.de<>NullCoord) then begin
         // previous version, up to 3, store apparent coordinates
         t.ra:=t.ra*15*deg2rad;
         t.de:=t.de*deg2rad;
         ApparentToJ2000(t.ra,t.de);
         t.ra:=rad2deg*t.ra/15;
         t.de:=rad2deg*t.de;
       end;
       x:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/PA','-');
       if x='-' then
         t.pa:=NullCoord
       else
         t.pa:=StrToFloatDef(x,NullCoord);
       t.astrometrypointing:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',false);
       t.updatecoord:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/UpdateCoord',false);
       t.solartracking:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/SolarTracking',false);
       t.inplaceautofocus:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/InplaceAutofocus',AutofocusInPlace);
       t.autofocustemp:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/AutofocusTemp',(AutofocusTempChange>0));
       t.noautoguidingchange:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/NoAutoguidingChange',false);
       t.previewexposure:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',1.0);
       t.preview:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Preview',false);
       t.repeatcount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',1));
       if IgnoreRestart then
          t.repeatdone:=0
       else
          t.repeatdone:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0));
       t.delay:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Delay',1.0);
       t.FlatCount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatCount',1));
       t.FlatBinX:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatBinX',1));
       t.FlatBinY:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatBinY',1));
       t.FlatGain:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatGain',0));
       t.FlatOffset:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatOffset',0));
       t.FlatFstop:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatFstop','');
       t.FlatFilters:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatFilters','');
       if FileVersion>=5 then begin
          Add(t);
          p:=T_Plan(t.plan);
          p.SequenceFile:=FSequenceFile;
          p.ObjectName:=t.objectname;
          p.PlanName:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Name',t.planname);
          m:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/StepNum',0));
          for j:=1 to m do begin
            s:=TStep.Create;
            s.steptype:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Type',0); // capture by default for compatibility
            s.description:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Description','');
            str:=UpperCase(trim(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/FrameType','Light')));
            s.frtype:=Str2Frametype(str);
            s.exposure:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Exposure',1.0);
            s.stackcount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/StackCount',1));
            str:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Binning','1x1');
            k:=FBinningList.IndexOf(str);
            if k<0 then
              FBinningList.Add(str);
            k:=pos('x',str);
            if k>0 then begin
               s.binx:=StrToIntDef(trim(copy(str,1,k-1)),1);
               s.biny:=StrToIntDef(trim(copy(str,k+1,9)),1);
            end else begin
              s.binx:=1;
              s.biny:=1;
            end;
            str:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Gain','');
            s.gain:=StrToIntDef(str,Gain);
            str:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Offset','');
            s.offset:=StrToIntDef(str,Offset);
            s.fstop:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Fstop','');
            str:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Filter','');
            FOriginalFilter[i]:=str;
            k:=FFilterList.IndexOf(str);
            if k<0 then k:=0;
            s.filter:=k;
            s.count:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Count',1));
            s.dither:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Dither',false);
            s.dithercount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/DitherCount',1));
            s.autofocusstart:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusStart',false);
            s.autofocus:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Autofocus',false);
            s.autofocuscount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusCount',10));
            s.donecount:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Done',0));
            s.scriptname:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptName','');
            s.scriptpath:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptPath','');
            s.scriptargs:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptArgs','');
            s.switchnickname:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchNickname','');
            s.switchname:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchName','');
            s.switchvalue:=FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchValue','');
            p.Add(s);
          end;
          if TemplateModified(p) then begin
            if  pos('*',p.PlanName)<=0 then begin
              p.PlanName:=p.PlanName+'*';
              t.planname:=p.PlanName;
            end;
          end
          else begin
            if pos('*',p.PlanName)>0 then begin
              p.PlanName:=StringReplace(p.PlanName,'*','',[]);
              t.planname:=p.PlanName;
            end;
          end
         end
         else begin
           // compatibility with old sequence format
           m:=trunc(FSequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StepDone/StepCount',0));
           Add(t);
           CompatLoadPlan(T_Plan(t.plan), t.planname, t.objectname);
         end;
     end;
   end;
   if FileVersion<5 then SaveTargets(fn);
end;

procedure T_Targets.SaveTargets(fn:string);
var t:TTarget;
    p:T_Plan;
    i,j: integer;
begin
try
 if Count>0 then begin
    FSequenceFile.Clear;
    FSequenceFile.Filename:=fn;
    FSequenceFile.ClearContent;
    CurrentSeqName:=FSequenceFile.CurrentName;
    TargetName:=CurrentSeqName;
    FSequenceFile.Items.SetValue('/Version',TargetFileVersion);
    FSequenceFile.Items.SetValue('/ListName',CurrentSeqName);
    FSequenceFile.Items.SetValue('/TargetNum',Count);
    FSequenceFile.Items.SetValue('/RepeatCount',TargetsRepeat);
    FSequenceFile.Items.SetValue('/Targets/IgnoreRestart',IgnoreRestart);
    if IgnoreRestart then
       FSequenceFile.Items.SetValue('/Targets/RepeatDone',0)
    else
       FSequenceFile.Items.SetValue('/Targets/RepeatDone',TargetsRepeatCount);
    FSequenceFile.Items.SetValue('/Targets/ResetRepeat',ResetRepeat);
    FSequenceFile.Items.SetValue('/Startup/SeqStart',SeqStart);
    FSequenceFile.Items.SetValue('/Startup/SeqStop',SeqStop);
    FSequenceFile.Items.SetValue('/Startup/SeqStartTwilight',SeqStartTwilight);
    FSequenceFile.Items.SetValue('/Startup/SeqStopTwilight',SeqStopTwilight);
    FSequenceFile.Items.SetValue('/Startup/SeqStartAt',TimeToStr(SeqStartAt));
    FSequenceFile.Items.SetValue('/Startup/SeqStopAt',TimeToStr(SeqStopAt));
    FSequenceFile.Items.SetValue('/Startup/CoolCamera',AtStartCool);
    FSequenceFile.Items.SetValue('/Startup/Unpark',AtStartUnpark);
    FSequenceFile.Items.SetValue('/Startup/RunScript',AtStartRunScript);
    FSequenceFile.Items.SetValue('/Startup/StartScript',AtStartScript);
    FSequenceFile.Items.SetValue('/Termination/StopTracking',AtEndStopTracking);
    FSequenceFile.Items.SetValue('/Termination/Park',AtEndPark);
    FSequenceFile.Items.SetValue('/Termination/CloseDome',AtEndCloseDome);
    FSequenceFile.Items.SetValue('/Termination/WarmCamera',AtEndWarmCamera);
    FSequenceFile.Items.SetValue('/Termination/RunScript',AtEndRunScript);
    FSequenceFile.Items.SetValue('/Termination/ErrorRunScript',OnErrorRunScript);
    FSequenceFile.Items.SetValue('/Termination/EndScript',AtEndScript);
    FSequenceFile.Items.SetValue('/Termination/ErrorScript',OnErrorScript);
    for i:=1 to Count do begin
      t:=FTargets[i-1];
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/ObjectName',t.objectname);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan',t.planname);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Path',t.path);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/ScriptArgs',t.scriptargs);
      if t.starttime>=0 then
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartTime',TimetoStr(t.starttime))
      else
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartTime','');
      if t.endtime>=0 then
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndTime',TimetoStr(t.endtime))
      else
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndTime','');
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartRise',t.startrise);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndSet',t.endset);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartMeridian',t.startmeridian);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndMeridian',t.endmeridian);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/DarkNight',t.darknight);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Skip',t.skip);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RA',RAToStr(t.ra));
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Dec',DEToStr(t.de));
      if t.pa=NullCoord then
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PA','-')
      else
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PA',FormatFloat(f1,t.pa));
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',t.astrometrypointing);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/UpdateCoord',t.updatecoord);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/SolarTracking',t.solartracking);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/InplaceAutofocus',t.inplaceautofocus);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/AutofocusTemp',t.autofocustemp);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/NoAutoguidingChange',t.noautoguidingchange);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',t.previewexposure);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Preview',t.preview);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',t.repeatcount);
      if IgnoreRestart then
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0)
      else
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',t.repeatdone);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Delay',t.delay);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatCount',t.FlatCount);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatBinX',t.FlatBinX);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatBinY',t.FlatBinY);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatGain',t.FlatGain);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatOffset',t.FlatOffset);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatFstop',t.FlatFstop);
      FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatFilters',t.FlatFilters);
      if (t.objectname<>ScriptTxt) then begin
        if (t.objectname=SkyFlatTxt) then CreateSkyFlatPlan(t);
        p:=T_Plan(t.plan);
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Name',p.PlanName);
        FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/StepNum',p.Count);
        for j:=1 to p.Count do begin
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Type',p.Steps[j-1].steptype);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Description',p.Steps[j-1].description);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/FrameType',p.Steps[j-1].frtype_str);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Exposure',p.Steps[j-1].exposure);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/StackCount',p.Steps[j-1].stackcount);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Binning',IntToStr(p.Steps[j-1].binx)+'x'+IntToStr(p.Steps[j-1].biny));
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Gain',p.Steps[j-1].gain);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Offset',p.Steps[j-1].offset);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Fstop',p.Steps[j-1].fstop);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Filter',p.Steps[j-1].filter_str);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Count',p.Steps[j-1].count);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Dither',p.Steps[j-1].dither);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/DitherCount',p.Steps[j-1].dithercount);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusStart',p.Steps[j-1].autofocusstart);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Autofocus',p.Steps[j-1].autofocus);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusCount',p.Steps[j-1].autofocuscount);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Done',p.Steps[j-1].donecount);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptName',p.Steps[j-1].scriptname);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptPath',p.Steps[j-1].scriptpath);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/ScriptArgs',p.Steps[j-1].scriptargs);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchNickname',p.Steps[j-1].switchnickname);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchName',p.Steps[j-1].switchname);
          FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/SwitchValue',p.Steps[j-1].switchvalue);
        end;
      end;
    end;
    FSequenceFile.Save;
 end;
except
  on E: Exception do msg('Error saving target file: '+ E.Message,1);
end;
end;

procedure T_Targets.CompatLoadPlan(p: T_plan; plan,obj:string);
var fn,buf1,buf2,msgstr,str,buf: string;
    i,j,n:integer;
    pfile: TCCDconfig;
    s: TStep;
begin
  // Load the plan from the template for pre V5 files compatibility
  fn:=slash(SequenceDir)+plan+'.plan';
  if FileExistsUTF8(fn) then begin
     p.Clear;
     p.SequenceFile:=FSequenceFile;
     p.PlanName:=plan;
     pfile:=TCCDconfig.Create(self);
     pfile.Filename:=fn;
     n:=pfile.GetValue('/StepNum',0);
     if camera.CanSetGain then begin
       msgstr:='';
       for i:=1 to n do begin
          buf1:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Gain','');
          buf2:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Offset','');
          if (buf1='')or(buf2='') then
            msgstr:=rsPlan+blank+plan+': '+rsPleaseBeCare;
       end;
       if msgstr<>'' then
          msg(msgstr,1);
     end;
     msgstr:='';
     for i:=1 to n do begin
       s:=TStep.Create;
       str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Description','');
       s.description:=str;
       str:=trim(pfile.GetValue('/Steps/Step'+inttostr(i)+'/FrameType','Light'));
       s.frtype:=Str2FrameType(str);
       str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Binning','1x1');
       j:=FBinningList.IndexOf(str);
       if j<0 then
         FBinningList.Add(str);
       j:=pos('x',str);
       if j>0 then begin
          buf:=trim(copy(str,1,j-1));
          s.binx:=StrToIntDef(buf,1);
          buf:=trim(copy(str,j+1,9));
          s.biny:=StrToIntDef(buf,1);
       end else begin
         s.binx:=1;
         s.biny:=1;
       end;
       str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Gain','');
       s.gain:=StrToIntDef(str,Gain);
       str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Offset','');
       s.offset:=StrToIntDef(str,Offset);
       str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Filter','');
       if i<SaveFilterNum then FOriginalFilter[i]:=str;
       j:=FFilterList.IndexOf(str);
       if j<0 then j:=0;
       s.filter:=j;
       s.fstop:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Fstop','');
       s.exposure:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Exposure',1.0);
       s.stackcount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/StackCount',1));
       s.count:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/Count',1));
       s.dither:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Dither',false);
       s.dithercount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/DitherCount',1));
       s.autofocusstart:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',false);
       s.autofocus:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Autofocus',false);
       s.autofocuscount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',10));
       s.donecount:=0;
       p.Add(s);
     end;
     msgstr:=msgstr+blank+obj+' '+plan+' warning! the completion status is cleared.';
     msg(msgstr,1);
  end;
end;

procedure T_Targets.Restart;
begin
  RestartTimer.Enabled:=true;
end;

procedure T_Targets.RestartTimerTimer(Sender: TObject);
begin
  RestartTimer.Enabled:=false;
  if FRealRestart then
    // restart, skipping done steps
    Start
  else begin
    // continue the sequence
    T_Plan(Targets[CurrentTarget].plan).Running:=true;
    T_Plan(Targets[CurrentTarget].plan).PlanTimer.Enabled:=true;
    FRunning:=true;
    FRestarting:=False;
  end;
end;

procedure T_Targets.Start;
var hm,he,ccdtemp: double;
    twok,wtok,nd,scriptfound: boolean;
    i,j,stw:integer;
begin
  try
  FWaitStarting:=true;
  FCurrentTarget:=-1;
  FStopping:=false;
  FDawnFlatNow:=false;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  CancelAutofocus:=false;
  WeatherCancelRestart:=false;
  FSeqLockTwilight:=false;
  FRunning:=true;
  FRestarting:=False;
  TargetForceNext:=false;
  FFinalizing:=false;
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
  if AtStartCool then begin
    ccdtemp:=config.GetValue('/Cooler/CameraAutoCoolTemp',0.0);
    if not camera.Cooler then begin
      msg(Format(rsCameraNotCoo, [FormatFloat(f1, ccdtemp)]),1);
      ccdtemp:=TempCelsius(TemperatureScale,ccdtemp);
      camera.Temperature:=ccdtemp;
      wait;
    end;
  end;
  if AtStartUnpark and Mount.Park then begin
    msg(rsUnparkTheTel,1);
    Mount.Park:=false;
    wait;
  end;
  if AtStartRunScript then begin
    scriptfound:=false;
    for i:=1 to MaxScriptDir do begin
      if FileExistsUTF8(slash(ScriptDir[i].path)+AtStartScript+'.script') then begin
         scriptfound:=true;
         f_scriptengine.RunScript(AtStartScript,ScriptDir[i].path,'');
         break;
      end;
    end;
    if not scriptfound then begin
      msg(Format(rsFileNotFound,[AtStartScript+'.script']),1);
    end;
  end;
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
        FSeqLockTwilight:=true;
        // stop current step
        if FCurrentTarget>=0 then
           p:=t_plan(Ftargets[FCurrentTarget].plan)
        else
           p:=nil;
        if (p<>nil) and p.Running then p.Stop;
        wait(15);
        // run sky flat
        FSeqLockTwilight:=false;
        FCurrentTarget:=j-1;
        FDawnFlatNow:=true;
        FStopping:=true;
        msg('Nexttarget 1254',9);
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

procedure T_Targets.ClearRunning;
var p: T_Plan;
begin
  // stop running without condition and without notifying
  StopTimer.Enabled:=false;
  StopTargetTimer.Enabled:=false;
  WeatherRestartTimer.Enabled:=false;
  InplaceAutofocus:=AutofocusInPlace;
  FStopping:=true;
  FRunning:=false;
  WeatherPauseCanceled:=true;
  WeatherPauseCapture:=false;
  WeatherCancelRestart:=false;
  if WaitTillrunning then begin
    if wt_pause<>nil
     then wt_pause.BtnCancel.Click
     else cancelWaitTill:=true;
  end;
  if FCurrentTarget>=0 then begin
     p:=t_plan(Ftargets[FCurrentTarget].plan);
     p.ClearRunning;
  end;
  CurrentTargetName:='';
  CurrentStepName:='';
end;


procedure T_Targets.StopSequence(abort: boolean);
var p: T_Plan;
    r: string;
begin
 StopTimer.Enabled:=false;
 StopTargetTimer.Enabled:=false;
 WeatherRestartTimer.Enabled:=false;
 FStopping:=true;
 InplaceAutofocus:=AutofocusInPlace;
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
   if f_scriptengine.ScriptRunning then begin
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
     if f_scriptengine.ScriptRunning then f_scriptengine.StopScript;
     msg(rsSequenceAbor,0);
     RunErrorAction;
     ShowDelayMsg('');
     if EmailAbortSequence or EmailEndSequence then begin
       r:=email(rsSequenceAbor,rsSequenceAbor+crlf+FName);
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
   end;
   SaveDoneCount;
   if assigned(FonEndSequence) then FonEndSequence(nil);
   CurrentTargetName:='';
   CurrentStepName:='';
 end
 else msg(rsNotRunningNo,1);
end;

function T_Targets.CheckStatus:boolean;
var i,j,totalcount,donecount,totalspteps,donesteps,intime,rglobal,forceendtime : integer;
    steptime,targettime,totaltime,pivot,stime,etime,ctime,hm,he,st,et: double;
    tfuture,tdone,trunning,twok,dotarget,rfuture,nd,forceset,seqbreak,flatnow: boolean;
    txt: string;
    t: TTarget;
    p: T_Plan;
begin
 // Same as CheckDoneCount but with more timing detail
 result:=false;
 FAllDone:=false;
 FAllStepsDone:=false;
 seqbreak:=false;
 FDoneStatus:='';
 FLastDoneStep:='';
 totalcount:=0; donecount:=0;
 totalspteps:=0; donesteps:=0;
 if IgnoreRestart then exit;
 if FResetRepeat then begin
   totalcount:=totalcount+FTargetsRepeat;
   donecount:=donecount+FTargetsRepeatCount;
 end;
 FDoneStatus:='';//rsGlobalRepeat+blank+IntToStr(FTargetsRepeatCount)+'/'+IntToStr(FTargetsRepeat);
 if (FTargetsRepeatCount>0)and(FTargetsRepeatCount<=FTargetsRepeat) then begin
   result:=true;
   FLastDoneStep:=rsGlobalRepeat+blank+IntToStr(FTargetsRepeatCount)+'/'+IntToStr(FTargetsRepeat);
 end;
 if FRunning then begin
   // Sequence is running, use real start/end time
   stime:=FSeqStartTime;
   if FSeqStop then begin
     etime:=FSeqStopAt;
     if etime>frac(stime) then
       etime:=trunc(stime)+etime
     else
       etime:=trunc(stime)+1+etime
   end
   else
     etime:=MaxDouble;
 end
 else begin
   // sequence is not running, estimate start/end time
   for i:=0 to NumTargets-1 do begin
     if (Targets[i].objectname=SkyFlatTxt)and(Targets[i].planname=FlatTimeName[0]) then begin
       // dusk skyflat require start time
       FSeqStart:=true;
       FSeqStartTwilight:=true;
     end;
     if (Targets[i].objectname=SkyFlatTxt)and(Targets[i].planname=FlatTimeName[1]) then begin
       // dawn skyflat require end time
       FSeqStop:=true;
       FSeqStopTwilight:=true;
     end;
   end;
   if FSeqStart then
     stime:=trunc(now)+FSeqStartAt  // specified start time
   else
     stime:=now;                    // otherwise simulate start at current time
    if FSeqStop then begin
      etime:=FSeqStopAt;            // specified end time
      if etime>frac(stime) then
        etime:=trunc(stime)+etime
      else
        etime:=trunc(stime)+1+etime // end time on next day
    end
    else
      etime:=MaxDouble;             // no end time, can run infinitly
   twok:=TwilightAstro(now,hm,he);  // compute twilight
   if twok then begin
     if FSeqStartTwilight then
       stime:=trunc(now)+he/24;     // replace start time by dusk
     if FSeqStopTwilight then
       etime:=trunc(now)+1+hm/24;   // replace end time by dawn
   end;
 end;
 FDoneStatus:=FDoneStatus+crlf+rsSequence+blank+rsStartAt+FormatDateTime(datehms,stime);
 ctime:=stime;
 totaltime:=0;

 // Loop global sequence repeat
 for rglobal:=FTargetsRepeatCount+1 to min(FTargetsRepeat,FTargetsRepeatCount+5) do begin  // print maximum of 5 global repeat
   rfuture:=rglobal>FTargetsRepeatCount;
   if rfuture then begin
      FDoneStatus:=FDoneStatus+crlf+crlf+rsGlobalRepeat+blank+IntToStr(rglobal)+'/'+IntToStr(FTargetsRepeat);
   end;
   // loop each target in sequence
   for i:=0 to NumTargets-1 do begin
      // create a dummy copy to not alter the current sequence
      t:=TTarget.Create;
      t.Assign(Targets[i]);
      if t=nil then Continue;
      if rfuture and FResetRepeat and (FTargetsRepeat>1) then begin
        // simulate reset count on repeat
        t.repeatdone:=0;
        p:=t_plan(t.plan);
        if p=nil then Continue;
        if p.Count<=0 then Continue;
        for j:=0 to p.Count-1 do begin
          p.Steps[j].donecount:=0;
        end;
      end;
      tfuture:=(i>FCurrentTarget);
      trunning:=(i=FCurrentTarget);
      // plan is only read, no need to copy
      p:=t_plan(t.plan);
      if p=nil then Continue;
      if t.objectname=ScriptTxt then begin
        // todo: script mean execution time read from comment in code?
        txt:=crlf+t.objectname+blank+p.PlanName;
        FDoneStatus:=FDoneStatus+crlf+txt;
      end
      else begin
        // standard target and skyflat
        totalcount:=totalcount+t.repeatcount;
        donecount:=donecount+t.repeatdone;
        totalspteps:=totalspteps+t.repeatcount;
        donesteps:=donesteps+t.repeatdone;
        if (t.repeatdone>0) then begin
          result:=true;
        end;
        dotarget:=false;
        if t.objectname=SkyFlatTxt then begin
          flatnow:=((t.planname=FlatTimeName[1]) and (seqbreak or (rglobal=min(FTargetsRepeat,FTargetsRepeatCount+5)))) //dawn
                   or ((t.planname=FlatTimeName[0]) and (rglobal=1)); // dusk
        end;
        tdone:=t.repeatdone>=t.repeatcount;
        if t.objectname<>SkyFlatTxt then begin
          txt:=crlf+rsTarget+blank+t.objectname+blank+rsRepeat+':'+blank+IntToStr(t.repeatdone)+'/'+IntToStr(t.repeatcount);
          if tfuture and (not tdone) then begin
            // target is planned in the future and is not completed
            SetTargetTime(t,stime,pivot); // compute the start/end time, running and old target already have time set
            if (t.starttime>0)and(t.endtime>0) then begin
              // target need to run between start and end
              st:=t.starttime;
              et:=t.endtime;
              intime:=InTimeInterval(frac(ctime),st,et,pivot/24);
              if intime=0 then begin
                // target can run in planned time
                dotarget:=true;
                txt:=txt+','+blank+rsStartAt+FormatDateTime(datehms, ctime);
              end
              else if intime<0 then begin
                // target need to wait start time
                if st<frac(ctime) then st:=st+1;
                ctime:=trunc(ctime)+st;
                dotarget:=true;
                txt:=txt+','+blank+rsStartAt+FormatDateTime(datehms, ctime);
              end
              else if intime>0 then begin
                // target cannot run because end time is already passed
                dotarget:=false;
                txt:=txt+','+blank+Format(rsSkipTarget, [''])+','+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]);;
              end;
              if dotarget then begin
                SecondsToWait(t.endtime,true,forceendtime,nd);
              end;
            end
            else begin
               // no start/end time, no coordinates, run anyway
               dotarget:=true;
            end;
          end
          else if trunning then begin
            // target is currently running
            dotarget:=true;
            st:=t.starttime+trunc(ctime);
            ctime:=max(ctime,st);
            txt:=txt+','+blank+rsStartAt+FormatDateTime(datehms, ctime);
          end
          else begin
            // target is already completly done
            dotarget:=false;
            txt:=txt+','+blank+rsDone;
          end;
        end
        else begin
           if flatnow then
              txt:=crlf+rsTarget+blank+t.objectname
           else
              txt:='';
        end;
        FDoneStatus:=FDoneStatus+crlf+txt;
        if t.repeatdone=t.repeatcount then
          FLastDoneStep:=txt;
        if p.Count<=0 then Continue;
        targettime:=0;
        // loop all steps in plan
        for j:=0 to p.Count-1 do begin
          totalcount:=totalcount+p.Steps[j].count;
          donecount:=donecount+p.Steps[j].donecount;
          totalspteps:=totalspteps+p.Steps[j].count;
          donesteps:=donesteps+p.Steps[j].donecount;
          if t.objectname<>SkyFlatTxt then begin
            // cumulate execution time
            steptime:=(p.Steps[j].count-p.Steps[j].donecount)*p.Steps[j].exposure*p.Steps[j].stackcount;
            targettime:=targettime+steptime;
            if steptime>0 then
              txt:=t.objectname+blank+p.PlanName+blank+rsStep+':'+blank+p.Steps[j].description+blank+','+rsDone+':'+blank+IntToStr(p.Steps[j].donecount)+'/'+IntToStr(p.Steps[j].count)+','+blank+'Step time'+':'+blank+raToStr(steptime/3600)
            else
              txt:=t.objectname+blank+p.PlanName+blank+rsStep+':'+blank+p.Steps[j].description+blank+','+rsDone+':'+blank+IntToStr(p.Steps[j].donecount)+'/'+IntToStr(p.Steps[j].count);
          end
          else begin
             if flatnow then begin
               // skyflat detail cannot be computed because of variable exposure time
               txt:=t.objectname+blank+p.PlanName+blank+rsStep+':'+blank+p.Steps[j].description+blank+','+rsDone+':'+blank+IntToStr(p.Steps[j].donecount)+'/'+IntToStr(p.Steps[j].count);
             end
             else txt:='';
          end;
          if txt>'' then begin
            FDoneStatus:=FDoneStatus+crlf+txt;
            if p.Steps[j].donecount>0 then begin
              result:=true;
              FLastDoneStep:=txt;
            end;
          end;
        end;
        // total execution time for target
        targettime:=targettime*(t.repeatcount-t.repeatdone);
        if dotarget and (targettime>forceendtime) then begin
           // interupted by object set
           targettime:=forceendtime;
           forceset:=true;
        end
        else
          forceset:=false;
        if dotarget then ctime:=ctime+targettime/secperday;
        if targettime>0 then begin
          // total sequence time
          totaltime:=totaltime+targettime;
          txt:=t.objectname+blank+'Target elapsed time'+':'+blank+TimToStr(targettime/3600,'h',false);
          txt:=txt+','+blank+'Sequence elapsed time'+':'+blank+TimToStr(totaltime/3600,'h',false);
          FDoneStatus:=FDoneStatus+crlf+txt;
          if FSeqStop and (ctime>etime) then begin
            // interupted by sequence end time
            txt:=t.objectname+blank+'Interrupted by end of sequence at:'+blank+FormatDateTime(datehms,etime);
            seqbreak:=true;
          end
          else begin
            if forceset then
              // interupted by object set
              txt:=t.objectname+blank+'Interrupted by object set at:'+blank+FormatDateTime(datehms,ctime)
            else
              // completed time
              txt:=t.objectname+blank+'End at:'+blank+FormatDateTime(datehms,ctime);
          end;
          FDoneStatus:=FDoneStatus+crlf+txt;
        end;
      end;
      t.Free;
   end;
   if seqbreak then break;
 end;
 if rglobal<FTargetsRepeat then begin
   FDoneStatus:=FDoneStatus+crlf+crlf+rsGlobalRepeat+blank+IntToStr(rglobal+1)+' to '+IntToStr(FTargetsRepeat)+' not printed';
 end;
 // all exposure steps done
 FAllStepsDone:=(totalspteps<=donesteps);
 // all done including global repeat
 FAllDone:=(totalcount<=donecount);
 if FAllDone then begin
   FLastDoneStep:=format(rsSequenceFini,[TargetName]);
   FDoneStatus:=FDoneStatus+crlf+crlf+FLastDoneStep;
 end;
end;

function T_Targets.CheckDoneCount:boolean;
var i,j,totalcount,donecount,totalspteps,donesteps : integer;
    txt: string;
    t: TTarget;
    p: T_Plan;
begin
 result:=false;
 FAllDone:=false;
 FAllStepsDone:=false;
 FDoneStatus:='';
 FLastDoneStep:='';
 totalcount:=0; donecount:=0;
 totalspteps:=0; donesteps:=0;
 if IgnoreRestart then exit;
 if FResetRepeat then begin
   totalcount:=totalcount+FTargetsRepeat;
   donecount:=donecount+FTargetsRepeatCount;
 end;
 FDoneStatus:=rsGlobalRepeat+blank+IntToStr(FTargetsRepeatCount)+'/'+IntToStr(FTargetsRepeat);
 if (FTargetsRepeatCount>0)and(FTargetsRepeatCount<=FTargetsRepeat) then begin
   result:=true;
   FLastDoneStep:=rsGlobalRepeat+blank+IntToStr(FTargetsRepeatCount)+'/'+IntToStr(FTargetsRepeat);
 end;
 for i:=0 to NumTargets-1 do begin
    t:=Targets[i];
    if t=nil then Continue;
    p:=t_plan(t.plan);
    if p=nil then Continue;
    if t.objectname=ScriptTxt then begin
      txt:=t.objectname+blank+p.PlanName;
      FDoneStatus:=FDoneStatus+crlf+txt;
    end
    else begin
      totalcount:=totalcount+t.repeatcount;
      donecount:=donecount+t.repeatdone;
      totalspteps:=totalspteps+t.repeatcount;
      donesteps:=donesteps+t.repeatdone;
      if (t.repeatdone>0) then begin
        result:=true;
      end;
      txt:=t.objectname+blank+rsRepeat+':'+blank+IntToStr(t.repeatdone)+'/'+IntToStr(t.repeatcount);
      FDoneStatus:=FDoneStatus+crlf+txt;
      if t.repeatdone=t.repeatcount then
        FLastDoneStep:=txt;
      if p.Count<=0 then Continue;
      for j:=0 to p.Count-1 do begin
        totalcount:=totalcount+p.Steps[j].count;
        donecount:=donecount+p.Steps[j].donecount;
        totalspteps:=totalspteps+p.Steps[j].count;
        donesteps:=donesteps+p.Steps[j].donecount;
        txt:=t.objectname+blank+p.PlanName+blank+rsStep+':'+blank+p.Steps[j].description+blank+rsDone+':'+IntToStr(p.Steps[j].donecount)+'/'+IntToStr(p.Steps[j].count);
        FDoneStatus:=FDoneStatus+crlf+txt;
        if p.Steps[j].donecount>0 then begin
          result:=true;
          FLastDoneStep:=txt;
        end;
      end;
    end;
 end;
 // all exposure steps done
 FAllStepsDone:=(totalspteps<=donesteps);
 // all done including global repeat
 FAllDone:=(totalcount<=donecount);
 if FAllDone then begin
   FLastDoneStep:=format(rsSequenceFini,[TargetName]);
   FDoneStatus:=FDoneStatus+crlf+crlf+FLastDoneStep;
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
    for j:=0 to p.Count-1 do begin
      p.Steps[j].donecount:=0;
    end;
 end;
end;

procedure T_Targets.SaveDoneCount;
var p: T_Plan;
    t: TTarget;
    i: integer;
begin
 try
  if (FCurrentTarget<0)or(FCurrentTarget>=Count) then exit;
  t:=Targets[FCurrentTarget];
  if t=nil then exit;
  p:=t_plan(t.plan);
  if p=nil then exit;
  if p.Count<=0 then exit;
  i:=FCurrentTarget+1;
  // global sequence repeat
  if FIgnoreRestart then
     FSequenceFile.Items.SetValue('/Targets/RepeatDone',0)
  else
     FSequenceFile.Items.SetValue('/Targets/RepeatDone',FTargetsRepeatCount);
  // target repeat
  if FIgnoreRestart then
     FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0)
  else
     FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',t.repeatdone);
  // save file
  FSequenceFile.Save;
 except
   on E: Exception do msg('Error saving sequence state:'+ E.Message,1);
 end;
end;

procedure T_Targets.PlanStepChange(Sender: TObject);
begin
  SaveDoneCount;
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
   if RecenteringTarget then begin
     CancelAutofocus:=true;
     msg('Stop target recenter',1);
     if Mount.MountSlewing then Mount.AbortMotion;
     if Astrometry.Busy then Astrometry.StopAstrometry;
     wait(30);
   end;
   if Astrometry.Busy then Astrometry.StopAstrometry;
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
    sw: Tstringlist;
begin
  TargetTimer.Enabled:=false;
  StopTargetTimer.Enabled:=false;
  // do not try to start a new target when stopped for dawn flat
  if FSeqLockTwilight then exit;
  // stop mount if let slewing
  if mount.MountSlewing then
     Mount.AbortMotion;
  NeedRecenterTarget:=false;
  InplaceAutofocus:=AutofocusInPlace;
  CancelAutofocus:=false;
  // save current state
  SaveDoneCount;
  // refresh alldone count
  CheckDoneCount;
  // try next target
  inc(FCurrentTarget);
  if FRunning and (FCurrentTarget<NumTargets) then begin
   // there is more target to process
   CurrentTargetName:=Targets[FCurrentTarget].objectname;
   if Targets[FCurrentTarget].objectname=ScriptTxt then begin
     // process script
     FInitializing:=false;
     TargetForceNext:=false;
     Targets[FCurrentTarget].autoguiding:=false;
     FScriptRunning:=true;
     if not f_scriptengine.RunScript(Targets[FCurrentTarget].planname,Targets[FCurrentTarget].path,Targets[FCurrentTarget].scriptargs)then begin
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
   else if (Targets[FCurrentTarget].objectname=SwitchTxt) then begin
     // process switch setting
     FInitializing:=false;
     TargetForceNext:=false;
     Targets[FCurrentTarget].autoguiding:=false;
     sw:=TStringList.Create;
     SplitRec(Targets[FCurrentTarget].planname,tab,sw);
     if sw.Count=3 then
       r:=f_scriptengine.cmd_setswitch(sw[0],sw[1],sw[2])
     else
       r:='Wrong parameters count';
     if r<>msgOK then begin
       msg(Format(rsSwitchFailed, [Targets[FCurrentTarget].planname])+' '+r,0);
       if FRunning then begin
       if FUnattended then begin
         StopSequence(true);
         exit;
       end else begin
         f_pause.Caption:=Format(rsSwitchFailed, ['']);
         f_pause.Text:=Format(rsSwitchFailed, [Targets[FCurrentTarget].planname])+crlf+rsDoYouWantToR;
         if f_pause.Wait(WaitResponseTime,false) then begin
            Dec(FCurrentTarget);
         end else begin
            StopSequence(false);
            exit;
         end;
       end;
       end;
     end;
     sw.Free;
     if FRunning then NextTarget;
     exit;
   end
   else if (Targets[FCurrentTarget].objectname=SkyFlatTxt) then begin
    if ((Targets[FCurrentTarget].planname=FlatTimeName[0])and(FTargetsRepeatCount=0)  // Dusk, run only on first repeat
        or((Targets[FCurrentTarget].planname=FlatTimeName[1])and(FAllDone or FDawnFlatNow or(FTargetsRepeatCount=FTargetsRepeat-1))))  // Dawn, run only on last repeat
    then begin
     // process sky flat
     FInitializing:=true;
     ShowDelayMsg('');
     TargetForceNext:=false;
     // stop autoguider
     if (Autoguider<>nil)and(Autoguider.Running)and(Autoguider.State in [GUIDER_GUIDING,GUIDER_BUSY,GUIDER_ALERT,GUIDER_INITIALIZING]) then
       StopGuider;
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
       if SkipTarget then begin
         FInitializing:=false;
         if FRunning then NextTarget;
         exit;
       end;
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
         if f_pause.Wait(WaitResponseTime,false, rsRetry, rsNextTarget) then begin
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
     // process target
     FInitializing:=true;
     ShowDelayMsg('');
     TargetForceNext:=false;
     // initialize target
     initok:=InitTarget;
     if not FRunning then begin
       exit;
     end;
     if initok then begin
       // start target
       StartPlan;
       TargetTimer.Enabled:=true;
     end
     else begin
       // not initialized
       if SkipTarget then begin
         // skip expected, try next target
         FInitializing:=false;
         if FRunning then NextTarget;
         exit;
       end
       else begin
         // unexpected error, show message and try next
         msg(Targets[FCurrentTarget].objectname+', '+rsTargetInitia,0);
         if EmailTargetInitialisation then email(rsTargetInitia,Targets[FCurrentTarget].objectname+', '+rsTargetInitia+', '+InitTargetError);
         if FUnattended then begin
           FInitializing:=false;
           NextTarget;
           exit;
         end else begin
           FInitializing:=false;
           f_pause.Caption:=rsTargetInitia;
           f_pause.Text:=rsTargetInitia+' '+Targets[FCurrentTarget].objectname+crlf+crlf+InitTargetError+crlf+rsDoYouWantToR;
           if f_pause.Wait(WaitResponseTime, false, rsRetry, rsNextTarget) then begin
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
   // there is no more target to process
   if (not TargetForceNext) then // do not mark an interrupted target as complete
     Inc(FTargetsRepeatCount);
   if (not TargetForceNext)and(not FAllDone)and(not FStopping)and(FTargetsRepeatCount<FTargetsRepeat) then begin
     // do global repeat
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
     // nothing more to do, stop the sequence
     try
     FFinalizing:=true;
     FRunning:=false;
     TargetTimer.Enabled:=false;
     StopTimer.Enabled:=false;
     msg(Format(rsSequenceFini, [FName]),1);
     RunEndAction;
     ShowDelayMsg('');
     if (not TargetForceNext)and(not FAllStepsDone) then // do not mark the global sequence done if some step are incomplete
       Dec(FTargetsRepeatCount);
     Dec(FCurrentTarget);
     // save status
     SaveDoneCount;
     FCurrentTarget:=-1;
     if EmailEndSequence then begin
       r:=email(Format(rsSequenceFini, [FName]),Format(rsSequenceFini, [FName]));
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
     if assigned(FonEndSequence) then FonEndSequence(nil);
     finally
       FFinalizing:=false;
     end;
   end;
  end;
end;

function T_Targets.SetTargetTime(t:TTarget; middletime:double; out pivot:double): boolean;
var hr,hs,ht,tt,dt,appra,appde: double;
    ri,si,ti: integer;
begin
  // convert rise/set or MC to start/end time
  result:=false;
  // compute apparent coord.
  if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
     appra:=t.ra*15*deg2rad;
     appde:=t.de*deg2rad;
     J2000ToApparent(appra,appde);
     appra:=appra*rad2deg/15;
     appde:=appde*rad2deg;
  end;
  pivot:=frac(middletime); // if no coordinates, pivot time is sequence start time
  // adjust start/end from coordinates
  if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
     ObjRise(appra,appde,hr,ri);
     // check object visibility
     if ri=2 then begin
        InitTargetError:=Format(rsSkipTarget, [t.objectname])+', '+rsThisObjectIs2;
        msg(InitTargetError, 3);
        exit;
     end;
     ObjSet(appra,appde,hs,si);
     ObjTransit(appra,appde,ht,ti);
     pivot:=rmod(ht+12+24,24); // pivot time 12h from transit
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
        if InTimeInterval(tt/24,hr/24,hs/24,pivot/24)=0 then
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
        if InTimeInterval(tt/24,hr/24,hs/24,pivot/24)=0 then
           t.endtime:=tt/24
        else
           t.endtime:=hs/24;
     end;
  end;
  result:=true;
end;

function T_Targets.InitTarget(restart:boolean=false):boolean;
var t: TTarget;
    p: T_Plan;
    ok,wtok,nd:boolean;
    stw,i,intime: integer;
    st,newra,newde,newV,newPA,enddelay,chkendtime: double;
    autofocusstart, astrometrypointing, autostartguider,isCalibrationTarget: boolean;
    skipmsg, buf: string;
begin
  InitTargetError:='';
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
      InitTargetError:=Format(rsSkipTarget, [t.objectname+', '+rsRepeat+'=0']);
      msg(InitTargetError, 3);
      exit;
    end;
    if t.repeatdone>=t.repeatcount then begin
      SkipTarget:=true;
      result:=false;
      InitTargetError:=Format(rsSkipTarget, [t.objectname+', '+Format(rsCaptureSFini,[t.planname])]);
      msg(InitTargetError, 3);
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
       if Fplanetarium.Search(t.objectname,newra,newde,newV,newPa) then begin
          msg(Format(rsNewCoordinat, [RAToStr(newra), DEToStr(newde)]),3);
          t.ra:=newra;
          t.de:=newde;
          if newv<>NullCoord then {finternalguider.v_solar;}
            t.solarV:=newv
          else
            t.solarV:=0;
          if newPa<>NullCoord then {finternalguider.vpa_solar;}
            t.solarPA:=newPA
          else
            t.solarPA:=0;
       end
       else begin
          msg(rsPlanetariumE+blank+Fplanetarium.LastErrorTxt,3);
          msg(Format(rsTargetSCoord, [t.objectname]), 3);
       end;
    end;
    if (p<>nil)and (p.Count>0) then
       enddelay:=(p.Steps[0].exposure*p.Steps[0].stackcount+180)/3600/24  // first exposure time + 3 minutes for telescope pointing, in days
    else
       enddelay:=0;

    // initialize start and end time
    if not SetTargetTime(t,FSeqStartTime,st) then begin
      SkipTarget:=true;
      result:=false;
      exit;
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
        InitTargetError:=Format(rsSkipTarget, [t.objectname])+skipmsg;
        msg(InitTargetError, 3);
        result:=false;
        exit;
      end;
    end;
    // start / stop timer
    if (intime>0) then begin
      InitTargetError:=Format(rsTargetCancel, [t.objectname])+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]);
      msg(InitTargetError,3);
      result:=false;
      exit;
    end;
    if (intime<0) and (t.starttime>=0) then begin
      StopGuider;
      Mount.AbortMotion;
      msg(Format(rsWaitToStartA, [TimeToStr(t.starttime)]),1);
      wtok:=WaitTill(TimeToStr(t.starttime),true);
      if not wtok then begin
         InitTargetError:=Format(rsTargetCancel, [t.objectname]);
         msg(InitTargetError,1);
         result:=false;
         exit;
      end;
    end;
    if (intime<=0) and (t.endtime>=0) then begin
       SecondsToWait(chkendtime,true,stw,nd);
       if stw>60 then begin
          SecondsToWait(t.endtime,true,stw,nd);
          msg(Format(rsObjectSetsAt, [FormatDateTime('hh:nn', t.endtime),
            FormatFloat(f1, stw/3600)]), 3);
          StopTargetTimer.Interval:=1000*stw;
          StopTargetTimer.Enabled:=true;
       end else begin
         InitTargetError:=Format(rsTargetCancel, [t.objectname])+', '+Format(rsStopTimeAlre, [TimeToStr(t.endtime)]);
         msg(InitTargetError,3);
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
        // prepare for slewing to target
        if (Autoguider<>nil)and(Autoguider.AutoguiderType<>agNONE)and(Autoguider.AutoguiderType<>agDITHER) then begin
          // stop guiding
          if Autoguider.State<>GUIDER_DISCONNECTED then begin
            if not StopGuider then begin
              InitTargetError:=rsFailToStopTh;
              exit;
            end;
            Wait(2);
            if not FRunning then exit;
            if WeatherCancelRestart then exit;
          end;
        end;
        // set rotator position
        if (t.pa<>NullCoord)and(Frotator.Status=devConnected) then begin
          Frotator.Angle:=t.pa;
        end;
        // set autofocus on temperature change
        Fcapture.CheckBoxFocus.Checked:=t.autofocustemp;
        // Set internal guider solar object motion
        if (Autoguider<>nil)and(Autoguider.AutoguiderType=agINTERNAL) then begin
           if (t.solartracking)and(t.solarV<>NullCoord)and(t.solarPA<>NullCoord) then begin
             finternalguider.SolarTracking:=t.solartracking;
             finternalguider.v_solar:=t.solarV;
             finternalguider.vpa_solar:=t.solarPA;
            end
            else begin
             finternalguider.SolarTracking:=false;
            end;
        end;
        // set coordinates
        if ((t.ra<>NullCoord)and(t.de<>NullCoord)) then begin
          // Check dome open and slaving
          if (dome.Status=devConnected) and
            ((not dome.Shutter)or(dome.hasSlaving and(not dome.Slave)))
            then begin
             buf:='Dome is: ';
             if not dome.Shutter then buf:=buf+'closed, ';
             if dome.hasSlaving and(not dome.Slave) then buf:=buf+'not slaved ';
             InitTargetError:='Dome is not ready: '+buf;
             msg(InitTargetError,1);
             StopSequence(true);
             exit;
          end;
          // check mount not parked
          if mount.Park then begin
             InitTargetError:=rsTheTelescope;
             msg(InitTargetError, 1);
             StopSequence(true);
             exit;
          end;
          // disable astrometrypointing and autoguiding if first step is to move to focus star
          astrometrypointing:=t.astrometrypointing and (not (autofocusstart and (not InplaceAutofocus))) ;
          // must track before to slew
          if not mount.Tracking then mount.Track;
          // slew to coordinates
          FSlewRetry:=1;
          ok:=Slew(t.ra,t.de,astrometrypointing,t.astrometrypointing);
          if not ok then begin
            InitTargetError:=rsTelescopeSle3;
            exit;
          end;
          Wait;
        end;
        if not FRunning then exit;
        if WeatherCancelRestart then exit;
      end;
      // check if the plans contain only calibration
      isCalibrationTarget:=not astrometrypointing; // astrometry done, do not stop tracking
      if (p<>nil) then begin
        if p.Count>0 then begin
          for i:=0 to p.Count-1 do begin
             if (p.Steps[i].frtype=ord(LIGHT))or(p.Steps[i].frtype>ord(high(TFrameType))) then begin
                isCalibrationTarget:=false;
                break;
             end;
          end;
        end
        else begin
          isCalibrationTarget:=false; // no exposure here, this only position target, do not stop tracking
        end;
      end;
      // start mount tracking
      if isCalibrationTarget then begin
        StopGuider;
        mount.AbortMotion;
      end
      else if ((t.ra=NullCoord)or(t.de=NullCoord))and(not mount.Tracking) then
         mount.Track;
      // start guiding
      if autoguider is T_autoguider_internal then begin
        // set the target position if the spectroscopy function is activated, do nothing otherwise.
        // implemented only for the internal guider.
        autoguider.SpectroSetTarget(t.ra,t.de);
      end;
      autostartguider:=(Autoguider<>nil)and(Autoguider.AutoguiderType<>agNONE)and
                       (Autoguider.AutoguiderType<>agDITHER) and (Autoguider.State<>GUIDER_DISCONNECTED)and
                       (Autoguider.State<>GUIDER_GUIDING)and(not t.noautoguidingchange)and
                       ((not autofocusstart)or (InplaceAutofocus and (not AutofocusPauseGuider))) and
                       (not isCalibrationTarget);
      if autostartguider then begin
        if not StartGuider then begin
          InitTargetError:=rsFailedToStar;
          exit;
        end;
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

procedure T_Targets.CreateSkyFlatPlan(flt: TTarget);
var i,n:integer;
    flp:T_Plan;
    fls:TStep;
    flfilter: TStringList;
    flexp: TFilterExp;
begin
  try
  // create a dynamic plan with all steps to run the flats, one step per filter
  flt.autoguiding:=false;
  flp:=T_Plan(flt.plan);
  flp.SequenceFile:=FSequenceFile;
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
      fls.frtype:=ord(FLAT);
      fls.filter:=FilterList.IndexOf(flfilter[i]);
      fls.binx:=flt.FlatBinX;
      fls.biny:=flt.FlatBinY;
      fls.gain:=flt.FlatGain;
      fls.offset:=flt.FlatOffset;
      fls.fstop:=flt.FlatFstop;
      fls.count:=flt.FlatCount;
      fls.exposure:=FlatMinExp;
      fls.stackcount:=1;
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
  finally
    for i:=0 to flfilter.count-1 do flfilter.Objects[i].Free;
    flfilter.Free;
  end;
end;

function T_Targets.InitSkyFlat: boolean;
var wtok,nd,ForceNextStartTime:boolean;
    stw: integer;
    sra,sde,sl,hp1,hp2: double;
    flt,nextt: TTarget;
begin
  result:=false;
  SkipTarget:=false;
  ForceNextStartTime:=false;
  if not FRunning then exit;
  FTargetInitializing:=true;
  try
  // get plan
  flt:=Targets[FCurrentTarget];
  if flt.repeatdone>=flt.repeatcount then begin
    SkipTarget:=true;
    result:=false;
    msg(Format(rsSkipTarget, [flt.objectname+', '+Format(rsCaptureSFini,[flt.planname])]), 3);
    exit;
  end;
  if flt.planname=FlatTimeName[0] then begin    // Dusk
    FlatWaitDusk:=true;
    FlatWaitDawn:=false;
    // look for eventual next target
    if (FCurrentTarget+1)<NumTargets then
      nextt:=Targets[FCurrentTarget+1]
    else
      nextt:=nil;
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
      // no real night but try to do something
      if nextt<>nil then begin
        if nextt.starttime>=0 then begin
          // stop the flat at specified next target start time
          flt.endtime:=nextt.starttime;
        end
        else begin
          flt.endtime:=23.9999/24;
        end;
      end
      else begin
        // no next target, eventually stop at midnight
        flt.endtime:=23.9999/24;
      end;
    end;
    // Update start time of next step to astronomical twilight if not already set
    if nextt<>nil then begin
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
    if p.Running then begin
      msg('Plan already running!',0);
      Abort;
      exit;
    end;
    if t.objectname<>'None' then
       Fcapture.Fname.Text:=trim(t.objectname);
    p.ObjectName:=t.objectname;
    if FIgnoreRestart then
      p.RestartTargetNum:=-1
    else
      p.RestartTargetNum:=FCurrentTarget+1;
    FUpdatecoordDelay:=0;
    TargetTimeStart:=now;
    p.Start;
  end;
end;

function T_Targets.GetBusy: boolean;
var t: TTarget;
    p: T_Plan;
begin
  result:= FInitializing and FTargetInitializing;
  result:= result or FFinalizing;
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
 if (mount.Status=devConnected)and(not mount.Tracking) then exit;
  msg(rsStartAutogui,2);
  Autoguider.Guide(true);
  result:=Autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
  if FRunning and (not result) then begin
    Autoguider.Guide(false);
    Autoguider.WaitBusy(60);
    Autoguider.Guide(true);
    result:=Autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
  end;
  if FRunning and (not result)and(not Unattended) then begin
    f_pause.Caption:=rsAutoguiderSt2;
    f_pause.Text:=Format(rsAutoguiderNo2, [inttostr(CalibrationDelay+
      SettleMaxTime), crlf]);
    if f_pause.Wait(WaitResponseTime,false) then begin
       result:=StartGuider;
       exit;
    end;
  end;
end;

function T_Targets.Slew(ra,de: double; precision,planprecision: boolean):boolean;
var err: double;
    errtxt: string;
    prec,exp:double;
    sgain,soffset: integer;
    fi,cormethod,bin,maxretry,delay: integer;
begin
 try
  Fslewing:=true;
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
  prec:=config.GetValue('/PrecSlew/Precision',SlewPrecision)/60;
  if precision then begin
    cormethod:=config.GetValue('/PrecSlew/Method',1);
    maxretry:=config.GetValue('/PrecSlew/Retry',3);
    exp:=config.GetValue('/PrecSlew/Exposure',10.0);
    sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
    soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
    bin:=config.GetValue('/PrecSlew/Binning',1);
    fi:=config.GetValue('/PrecSlew/Filter',0);
    result:=astrometry.PrecisionSlew(ra,de,prec,exp,fi,bin,bin,cormethod,maxretry,sgain,soffset,err);
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
 finally
   Fslewing:=false;
 end;
end;

procedure T_Targets.TargetTimerTimer(Sender: TObject);
var tt,newra,newde,newV,newPa: double;
    t: TTarget;
    r: string;
begin
 if LockTargetTimer then
   exit;
 try
 LockTargetTimer:=true;
 if FRunning and (FCurrentTarget>=0) and (FCurrentTarget<NumTargets) then begin
   FInitializing:=false;
   t:=Targets[FCurrentTarget];
   if not TargetRepeatTimer.Enabled then begin
      if (t=nil) then begin
        msg('Nexttarget 2480',9);
        NextTarget;
        exit;
      end;
      if t.updatecoord and T_Plan(t.plan).Running then begin
       inc(FUpdatecoordDelay);
       // update every 60*timerinterval = every minute
       if ((FUpdatecoordDelay mod 60)=0) and Fplanetarium.Search(t.objectname,newra,newde,newV,newPa) then begin
          t.ra:=newra;
          t.de:=newde;
          // update coordinate for an eventual meridian flip
          if (FTargetRA<>NullCoord)and(FTargetDE<>NullCoord) then begin
            FTargetRA:=deg2rad*15*newra;
            FTargetDE:=deg2rad*newde;
            J2000ToApparent(FTargetRA,FTargetDE);
          end;
          if (newv<>NullCoord)and(newPa<>NullCoord)and(Autoguider<>nil)and(Autoguider.AutoguiderType=agINTERNAL) and t.solartracking then begin
            // update object motion
            t.solarV:=newv;
            t.solarPA:=newPA;
            Finternalguider.v_solar:=newv;
            Finternalguider.vpa_solar:=newPa;
          end;
       end;
      end;
      if (not T_Plan(t.plan).Running) and (not FRestarting) then begin
        if not TargetForceNext then  // do not mark an interrupted target as complete
          inc(t.repeatdone);
        if (TargetForceNext)or(t.repeatdone>=t.repeatcount) then begin
           msg('Nexttarget 2461',9);
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
      end;
   end
   else begin
     tt:=(TargetDelayEnd-Now)*secperday;
     ShowDelayMsg(Format(rsContinueInSe, [FormatFloat(f0, tt)]));
   end;
 end
 else begin
  FRunning:=false;
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
 finally
   LockTargetTimer:=false;
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
var scriptfound:boolean;
    i:integer;
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
    scriptfound:=false;
    for i:=1 to MaxScriptDir do begin
      if FileExistsUTF8(slash(ScriptDir[i].path)+OnErrorScript+'.script') then begin
         scriptfound:=true;
         f_scriptengine.RunScript(OnErrorScript,ScriptDir[i].path,'');
         break;
      end;
    end;
    if not scriptfound then begin
      msg(Format(rsFileNotFound,[OnErrorScript+'.script']),1);
    end;
  end;
end;

procedure T_Targets.RunEndAction(confirm: boolean=true);
var i: integer;
    scriptfound: boolean;
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
    StopGuider;
    msg(rsStopTelescop2,1);
    Mount.AbortMotion;
  end;
  if AtEndPark then begin
    StopGuider;
    msg(rsParkTheTeles2,1);
    Mount.Park:=true;
  end;
  if AtEndCloseDome then begin
    StopGuider;
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
    scriptfound:=false;
    for i:=1 to MaxScriptDir do begin
      if FileExistsUTF8(slash(ScriptDir[i].path)+AtEndScript+'.script') then begin
         scriptfound:=true;
         f_scriptengine.RunScript(AtEndScript,ScriptDir[i].path,'');
         break;
      end;
    end;
    if not scriptfound then begin
      msg(Format(rsFileNotFound,[AtEndScript+'.script']),1);
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

function  T_Targets.GetScriptRunning: boolean;
var t: TTarget;
    p: T_Plan;
begin
 result:=false;
 if (FCurrentTarget>=0)and(FCurrentTarget<NumTargets) then begin
   t:=Targets[FCurrentTarget];
   if t<>nil then begin
     p:=T_Plan(t.plan);
     if p<>nil then begin
       result:=p.ScriptRunning;
     end;
   end;
 end;
 result:=result or FScriptRunning;
end;


////////////////////  TTarget  /////////////////////////////

constructor TTarget.Create;
begin
  inherited Create;
  plan:=T_Plan.Create(nil);
  objectname:='None';
  planname:='';
  path:='';
  scriptargs:='';
  starttime:=NullCoord;
  endtime:=NullCoord;
  startmeridian:=NullCoord;
  endmeridian:=NullCoord;
  ra:=NullCoord;
  de:=NullCoord;
  pa:=NullCoord;
  solarV:=0;
  solarPA:=0;
  astrometrypointing:=(astrometryResolver<>ResolverNone);
  updatecoord:=false;
  solartracking:=false;
  inplaceautofocus:=AutofocusInPlace;
  autofocustemp:=(AutofocusTempChange>0);
  noautoguidingchange:=false;
  autoguiding:=false;
  repeatcount:=1;
  repeatdone:=0;
  preview:=False;
  delay:=1;
  previewexposure:=1;
  darknight:=false;
  skip:=false;
end;

destructor TTarget.Destroy;
begin
  try
  FreeAndNil(plan);
  except
  end;
  Inherited Destroy;
end;

procedure TTarget.Assign(Source: TTarget);
begin
  objectname:=Source.objectname;
  planname:=Source.planname;
  path:=Source.path;
  scriptargs:=Source.scriptargs;
  T_Plan(plan).Clear;
  T_Plan(plan).AssignPlan(T_Plan(Source.plan));
  starttime:=Source.starttime;
  endtime:=Source.endtime;
  startrise:=Source.startrise;
  endset:=Source.endset;
  startmeridian:=Source.startmeridian;
  endmeridian:=Source.endmeridian;
  ra:=Source.ra;
  de:=Source.de;
  pa:=Source.pa;
  solarV:=Source.solarV;
  solarPA:=Source.solarPA;
  astrometrypointing:=source.astrometrypointing;
  updatecoord:=Source.updatecoord;
  solartracking:=Source.solartracking;
  repeatcount:=Source.repeatcount;
  repeatdone:=Source.repeatdone;
  inplaceautofocus:=Source.inplaceautofocus;
  autofocustemp:=Source.autofocustemp;
  noautoguidingchange:=Source.noautoguidingchange;
  autoguiding:=Source.autoguiding;
  preview:=Source.preview;
  delay:=Source.delay;
  previewexposure:=Source.previewexposure;
  FlatCount:=Source.FlatCount;
  FlatBinX:=Source.FlatBinX;
  FlatBinY:=Source.FlatBinY;
  FlatGain:=Source.FlatGain;
  FlatOffset:=Source.FlatOffset;
  FlatFilters:=Source.FlatFilters;
  FlatFstop:=Source.FlatFstop;
  darknight:=Source.darknight;
  skip:=Source.skip;
end;

function TTarget.previewexposure_str: string;
begin
 Result:=FloatToStr(previewexposure);
end;

function TTarget.delay_str: string;
begin
  Result:=FloatToStr(delay);
end;

function TTarget.repeatcount_str: string;
begin
  Result:=IntToStr(repeatcount);
end;

function TTarget.ra_str: string;
begin
  Result:=FormatFloat(f5,ra);
end;

function TTarget.de_str: string;
begin
  Result:=FormatFloat(f5,de);
end;

function TTarget.pa_str: string;
begin
  Result:=FormatFloat(f2,pa);
end;


function TTarget.id: LongWord;
var buf: string;
begin
  // if any of this change we consider it another target
  buf:=trim(objectname)+StringReplace(trim(planname),'*','',[])+ra_str+de_str;
  if UseRotator then buf:=buf+pa_str;
  result:=Hash(buf);
end;

function TemplateModified(p:T_Plan):boolean;
var template,fn: string;
    pfile: TCCDconfig;
    i,n: integer;
begin
  if (p.PlanName='')or(p.objectname=SkyFlatTxt)or(p.objectname=ScriptTxt) then begin
    result:=false;
    exit;
  end;
  result:=true;
  template:=StringReplace(p.PlanName,'*','',[]);
  fn:=slash(SequenceDir)+template+'.plan';
  if not FileExistsUTF8(fn) then exit;
  pfile:=TCCDconfig.Create(nil);
  try
  pfile.Filename:=fn;
  if pfile.GetValue('/PlanName','')<>template then exit;
  if pfile.GetValue('/StepNum',-1)<>p.Count then exit;
  for i:=1 to p.Count do begin
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Type',0)<>p.Steps[i-1].steptype then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Exposure',1.0)<>p.Steps[i-1].exposure then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/StackCount',1)<>p.Steps[i-1].stackcount then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Count',1)<>p.Steps[i-1].count then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Dither',false)<>p.Steps[i-1].dither then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/DitherCount',1)<>p.Steps[i-1].dithercount then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',false)<>p.Steps[i-1].autofocusstart then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Autofocus',false)<>p.Steps[i-1].autofocus then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',10)<>p.Steps[i-1].autofocuscount then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Binning','1x1')<>IntToStr(p.Steps[i-1].binx)+'x'+IntToStr(p.Steps[i-1].biny) then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Gain',Gain)<>p.Steps[i-1].gain then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Offset',Offset)<>p.Steps[i-1].offset then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/FrameType','Light')<>p.Steps[i-1].frtype_str then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Fstop','')<>p.Steps[i-1].fstop then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Description','')<>p.Steps[i-1].description then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptName','')<>p.Steps[i-1].scriptname then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptPath','')<>p.Steps[i-1].scriptpath then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptArgs','')<>p.Steps[i-1].scriptargs then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchNickname','')<>p.Steps[i-1].switchnickname then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchName','')<>p.Steps[i-1].switchname then exit;
     if pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchValue','')<>p.Steps[i-1].switchvalue then exit;
     n:=p.Steps[i-1].filter;
     if FilterList.Count>=n then begin
       if pfile.GetValue('/Steps/Step'+inttostr(i)+'/Filter','')<>FilterList[n] then exit;
     end;
  end;
  result:=false;
  finally
    pfile.Free;
  end;
end;


end.

