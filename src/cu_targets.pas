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

uses u_global, cu_plan, u_utils, indiapi, pu_scriptengine, pu_pause,
  fu_capture, fu_preview, fu_filterwheel, cu_mount, cu_camera, cu_autoguider, cu_astrometry,
  math, LazFileUtils, Controls, Dialogs, ExtCtrls,Classes, Forms, SysUtils;

type
  TTargetList = array of TTarget;

  T_Targets = class(TComponent)
    private
      TargetTimer: TTimer;
      TargetRepeatTimer: TTimer;
      FTargetsChange: TNotifyEvent;
      FPlanChange: TNotifyEvent;
      FonMsg,FDelayMsg: TNotifyMsg;
      Fcapture: Tf_capture;
      Fpreview: Tf_preview;
      Ffilter: Tf_filterwheel;
      Fmount: T_mount;
      Fcamera: T_camera;
      Fautoguider: T_autoguider;
      Fastrometry: TAstrometry;
      StartPlanTimer: TTimer;
      FTargetCoord: boolean;
      FTargetRA,FTargetDE: double;
      FTargetsRepeatCount: integer;
      function GetBusy: boolean;
      procedure SetTargetName(val: string);
      procedure SetPreview(val: Tf_preview);
      procedure SetCapture(val: Tf_capture);
      procedure SetMount(val: T_mount);
      procedure SetCamera(val: T_camera);
      procedure SetFilter(val: Tf_filterwheel);
      procedure SetAutoguider(val: T_autoguider);
      procedure SetAstrometry(val: TAstrometry);
      procedure msg(txt:string);
      procedure ShowDelayMsg(txt:string);
      procedure StopSequence(abort: boolean);
      procedure NextTarget;
      function InitTarget:boolean;
      procedure StartPlan;
      procedure RunErrorScript;
      procedure RunEndScript;
      function StopGuider:boolean;
      function StartGuider:boolean;
      function Slew(ra,de: double; precision:boolean):boolean;
      procedure TargetTimerTimer(Sender: TObject);
      procedure TargetRepeatTimerTimer(Sender: TObject);
      procedure StartPlanTimerTimer(Sender: TObject);
    protected
      Ftargets: TTargetList;
      NumTargets: integer;
      FCurrentTarget: integer;
      FTargetsRepeat: integer;
      TargetTimeStart,TargetDelayEnd: TDateTime;
      FRunning: boolean;
      FInitializing: boolean;
      FUnattended: boolean;
      FName: string;
    public
      TargetRepeatCount, TargetTotalCount: integer;
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure Clear;
      function Add(t: TTarget):integer;
      procedure Start;
      procedure Stop;
      procedure Abort;
      procedure ForceNextTarget;
      property TargetsRepeat: integer read FTargetsRepeat write FTargetsRepeat;
      property Targets: TTargetList read Ftargets;
      property Count: integer read NumTargets;
      property CurrentTarget: integer read FCurrentTarget;
      property TargetName: string read FName write SetTargetName;
      property Busy: boolean read GetBusy;
      property Running: boolean read FRunning;
      property TargetCoord: boolean read FTargetCoord;
      property TargetRA: double read FTargetRA;
      property TargetDE: double read FTargetDE;
      property Unattended: boolean read FUnattended write FUnattended;
      property onTargetsChange: TNotifyEvent read FTargetsChange write FTargetsChange;
      property onPlanChange: TNotifyEvent read FPlanChange write FPlanChange;
      property Preview: Tf_preview read Fpreview write Setpreview;
      property Capture: Tf_capture read Fcapture write Setcapture;
      property Mount: T_mount read Fmount write SetMount;
      property Camera: T_camera read Fcamera write SetCamera;
      property Filter: Tf_filterwheel read Ffilter write SetFilter;
      property Autoguider: T_autoguider read Fautoguider write SetAutoguider;
      property Astrometry: TAstrometry read Fastrometry write SetAstrometry;
      property DelayMsg: TNotifyMsg read FDelayMsg write FDelayMsg;
      property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

implementation

constructor T_Targets.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NumTargets := 0;
  FTargetsRepeat:=1;
  Frunning:=false;
  FInitializing:=false;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
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

procedure T_Targets.msg(txt:string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure T_Targets.ShowDelayMsg(txt:string);
begin
  if Assigned(FDelayMsg) then FDelayMsg(txt);
end;

procedure  T_Targets.Clear;
var i: integer;
begin
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
begin
  FTargetsRepeatCount:=0;
  FCurrentTarget:=-1;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  FRunning:=true;
  if FTargetsRepeat=1 then
    msg('Starting sequence '+FName)
  else
    msg('Starting sequence '+FName+' repeat '+inttostr(FTargetsRepeatCount+1)+'/'+inttostr(FTargetsRepeat));
  NextTarget;
end;

procedure T_Targets.Stop;
begin
  msg('Request to stop the current sequence');
  StopSequence(false);
end;

procedure T_Targets.Abort;
begin
  msg('Abort the current sequence');
  StopSequence(true);
end;

procedure T_Targets.StopSequence(abort: boolean);
var p: T_Plan;
begin
 if FRunning then begin
   p:=t_plan(Ftargets[FCurrentTarget].plan);
   if (not abort) and p.Running then begin
     p.Stop;
     FRunning:=false;
     msg('Sequence stopped.');
     RunEndScript;
     ShowDelayMsg('');
   end
   else begin
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
     msg('Sequence aborted.');
     RunErrorScript;
     ShowDelayMsg('');
   end;
 end
 else msg('Not running, nothing to do.');
end;

procedure T_Targets.ForceNextTarget;
var p: T_Plan;
begin
 msg('Try next target');
 if FRunning then begin
   p:=t_plan(Ftargets[FCurrentTarget].plan);
   if p.Running then begin
     p.Stop;
     msg('Plan '+Ftargets[FCurrentTarget].planname+' stopped.');
     ShowDelayMsg('');
   end;
   NextTarget;
 end
 else msg('Not running, nothing to do.');
end;

procedure T_Targets.NextTarget;
var initok: boolean;
begin
  TargetTimer.Enabled:=false;
  inc(FCurrentTarget);
  if FRunning and (FCurrentTarget<NumTargets) then begin
   if Targets[FCurrentTarget].objectname='Script' then begin
     FInitializing:=false;
     if not f_scriptengine.RunScript(Targets[FCurrentTarget].planname,Targets[FCurrentTarget].path)then begin
       msg('Script '+Targets[FCurrentTarget].planname+' failed!');
       if FUnattended then begin
         StopSequence(true);
         exit;
       end else begin
         f_pause.Caption:='Script failed';
         f_pause.Text:='Script '+Targets[FCurrentTarget].planname+' failed!'+crlf+'Do you want to retry?';
         if f_pause.Wait(WaitResponseTime) then begin
            Dec(FCurrentTarget);
         end else begin
            StopSequence(false);
            exit;
         end;
       end;
     end;
     NextTarget;
     exit;
   end
   else begin
     FInitializing:=true;
     ShowDelayMsg('');
     TargetRepeatCount:=1;
     initok:=InitTarget;
     if not FRunning then begin
       NextTarget;
       exit;
     end;
     if initok then begin
       StartPlan;
       TargetTimer.Enabled:=true;
     end
     else begin
       msg(Targets[FCurrentTarget].objectname+', Target initialisation failed!');
       if FUnattended then begin
         FInitializing:=false;
         StopSequence(true);
         exit;
       end else begin
         FInitializing:=false;
         f_pause.Caption:='Target failed';
         f_pause.Text:='Target initialisation failed for '+Targets[FCurrentTarget].objectname+crlf+'Do you want to retry?';
         if f_pause.Wait(WaitResponseTime) then begin
            Dec(FCurrentTarget);
         end;
         NextTarget;
         exit;
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
     FRunning:=true;
     msg('Starting sequence '+FName+' repeat '+inttostr(FTargetsRepeatCount+1)+'/'+inttostr(FTargetsRepeat));
     NextTarget;
   end
   else begin
     FRunning:=false;
     TargetTimer.Enabled:=false;
     StopGuider;
     msg('Sequence '+FName+' terminated.');
     RunEndScript;
     ShowDelayMsg('');
     FCurrentTarget:=-1;
   end;
  end;
end;

function T_Targets.InitTarget:boolean;
var t: TTarget;
    ok:boolean;
begin
  result:=false;
  if not FRunning then exit;
  t:=Targets[FCurrentTarget];
  if t<>nil then begin
    msg('Initialize target '+t.objectname);
    if (t.ra<>NullCoord)and(t.de<>NullCoord) then begin
      if Autoguider<>nil then begin
        // stop guiding
        if Autoguider.State<>GUIDER_DISCONNECTED then begin
          if not StopGuider then exit;
          Wait(2);
          if not FRunning then exit;
        end;
      end;
      // slew to coordinates
      ok:=Slew(t.ra,t.de,t.astrometrypointing);
      if not ok then exit;
      Wait;
      if not FRunning then exit;
      if Autoguider<>nil then begin
        // start guiding
        if Autoguider.State<>GUIDER_DISCONNECTED then begin
          if not StartGuider then exit;
          Wait;
          if not FRunning then exit;
        end;
      end;
    end;
    result:=true;
  end;
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
      msg('Stop preview');
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
    TargetTimeStart:=now;
    p.Start;
  end;
end;

function T_Targets.GetBusy: boolean;
var t: TTarget;
    p: T_Plan;
begin
  result:= FInitializing ;
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
  msg('Stop autoguider');
  Autoguider.Guide(false);
  result:=Autoguider.WaitBusy(60);
  if (not result)and(not Unattended) then begin
    f_pause.Caption:='Autoguider Stop';
    f_pause.Text:='Autoguider is still active 60 seconds after a stop request.'+crlf+'Do you want to wait more?';
    if f_pause.Wait(WaitResponseTime) then begin
       result:=StopGuider();
       exit;
    end;
  end;
end;

function T_Targets.StartGuider:boolean;
begin
 result:=false;
 if Autoguider=nil then exit;
  msg('Start autoguider');
  Autoguider.Guide(true);
  result:=Autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
  if (not result)and(not Unattended) then begin
    f_pause.Caption:='Autoguider Start';
    f_pause.Text:='Autoguider not guiding '+inttostr(CalibrationDelay+SettleMaxTime)+' seconds after requested to start.'+crlf+'Do you want to retry?';
    if f_pause.Wait(WaitResponseTime) then begin
       result:=StartGuider();
       exit;
    end;
  end;
end;

function T_Targets.Slew(ra,de: double; precision:boolean):boolean;
var err,maxerr: double;
    prec,exp:double;
    fi,cormethod,bin,maxretry: integer;
begin
  result:=false;
  FTargetCoord:=false;
  if (Mount=nil)or(Mount.Status<>devConnected) then begin
    msg('Error! Mount not connected');
    exit;
  end;
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
      FTargetRA:=deg2rad*15*ra;
      FTargetDE:=deg2rad*de;
    end;
  end
  else begin
    maxerr:=max(2*prec,1/60);
    Mount.Slew(ra, de);
    err:=rad2deg*rmod(AngularDistance(deg2rad*15*ra,deg2rad*de,deg2rad*15*mount.RA,deg2rad*mount.Dec)+pi2,pi2);
    result:=(err<maxerr);
  end;
  if not FRunning then exit;
  if not result then begin
    msg('Telescope slew error: '+FormatFloat(f2,err*60)+' arcminutes.');
    if not Unattended then begin
      f_pause.Caption:='Telescope slew';
      f_pause.Text:='After telescope pointing to target the offset relative to requested position is '+FormatFloat(f2,err*60)+' arcminutes.'+crlf+'Do you want to retry the slew?';
      if f_pause.Wait(WaitResponseTime) then begin
         result:=Slew(ra,de,precision);
         exit;
      end;
    end;
  end;
end;

procedure T_Targets.TargetTimerTimer(Sender: TObject);
var tt: double;
    t: TTarget;
begin
 if FRunning then begin
   FInitializing:=false;
   t:=Targets[FCurrentTarget];
   if not TargetRepeatTimer.Enabled then begin
      if not T_Plan(t.plan).Running then begin
        inc(TargetRepeatCount);
        if (t<>nil)and(TargetRepeatCount<=t.repeatcount) then begin
           tt:=t.delay-(Now-TargetTimeStart)*secperday;
           if tt<0.1 then tt:=0.1;
           msg('Wait '+FormatFloat(f1,tt)+' seconds before repeated target '+IntToStr(TargetRepeatCount));
           TargetRepeatTimer.Interval:=trunc(1000*tt);
           TargetRepeatTimer.Enabled:=true;
           TargetDelayEnd:=now+tt/secperday;
           if t.preview and (tt>5)and(tt>(2*t.previewexposure)) then begin
             if t.previewexposure>0 then Preview.Exposure:=t.previewexposure;
             Preview.Binning.Text:=Capture.Binning.Text;
             Preview.BtnLoop.Click;
           end;
        end
        else begin
         NextTarget;
        end;
      end;
   end
   else begin
     tt:=(TargetDelayEnd-Now)*secperday;
     ShowDelayMsg('Continue in '+FormatFloat(f0,tt)+' seconds');
   end;
 end
 else begin
  TargetTimer.Enabled:=false;
  FCurrentTarget:=-1;
  msg('Sequence '+FName+' stopped.');
  ShowDelayMsg('');
 end;
end;

procedure T_Targets.TargetRepeatTimerTimer(Sender: TObject);
var t: TTarget;
begin
 if FRunning then begin
    TargetRepeatTimer.Enabled:=false;
    ShowDelayMsg('');
    t:=Targets[FCurrentTarget];
    if t<>nil then begin
      Msg('Repeat target'+inttostr(TargetRepeatCount)+'/'+t.repeatcount_str+' '+t.objectname);
      ShowDelayMsg('Repeat target'+inttostr(TargetRepeatCount)+'/'+t.repeatcount_str+' '+t.objectname);
      if t.preview and Preview.Running then Preview.BtnLoop.Click;
      TargetTimeStart:=now;
      StartPlan;
    end
    else FRunning:=false;
 end;
end;

procedure T_Targets.RunErrorScript;
var path,sname: string;
begin
  path:=ScriptDir[1].path;
  sname:='unattended_error';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path);
  end;
end;

procedure T_Targets.RunEndScript;
var path,sname: string;
begin
  path:=ScriptDir[1].path;
  sname:='end_sequence';
  if FileExistsUTF8(slash(path)+sname+'.script') then begin
    f_scriptengine.RunScript(sname,path);
  end;
end;

end.

