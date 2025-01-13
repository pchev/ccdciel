unit cu_plan;

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

uses u_global, u_utils, u_translation, cu_sequencefile, pu_scriptengine,
  fu_capture, fu_preview, fu_filterwheel, cu_mount, cu_camera, cu_autoguider,
  ExtCtrls, Classes, SysUtils;

type

T_Steps = array of TStep;

T_Plan = class(TComponent)
  private
    StartTimer: TTimer;
    RestartTimer: TTimer;
    FPlanChange: TNotifyEvent;
    FonMsg,FDelayMsg: TNotifyMsg;
    Fcapture: Tf_capture;
    Fpreview: Tf_preview;
    Ffilter: Tf_filterwheel;
    Fmount: T_mount;
    Fcamera: T_camera;
    Fautoguider: T_autoguider;
    FonStepProgress: TNotifyEvent;
    FSequenceFile: T_SequenceFile;
    FScriptRunning: boolean;
    procedure SetPlanName(val: string);
    procedure NextStep;
    procedure StartStep;
    procedure msg(txt:string; level: integer);
    procedure ShowDelayMsg(txt:string);
    procedure PlanTimerTimer(Sender: TObject);
    procedure StartTimerTimer(Sender: TObject);
    procedure RestartTimerTimer(Sender: TObject);
    procedure StartCapture;
  protected
    FSteps: T_Steps;
    NumSteps: integer;
    FCurrentStep: integer;
    StepRunning: boolean;
    StepTimeStart,StepDelayEnd: TDateTime;
    FName,FObjectName: string;
    FRestartTargetNum: integer;
    FRunning: boolean;
    Fautostartguider: boolean;
    FStartGuiding, FStopGuiding: TNotifyEvent;
  public
    PlanTimer: TTimer;
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy; override;
    procedure Clear;
    procedure AssignPlan(Source: T_plan);
    function Add(s: TStep):integer;
    procedure Start;
    procedure Stop;
    procedure ClearRunning;
    procedure UpdateDoneCount(progress: boolean);
    procedure Restart;
    function IndexOf(id:LongWord):integer;
    function totaltime(skipdone:boolean): double;
    property SequenceFile: T_SequenceFile read FSequenceFile write FSequenceFile;
    property Count: integer read NumSteps;
    property ScriptRunning: boolean read FScriptRunning;
    property CurrentStep: integer read FCurrentStep write FCurrentStep;
    property Running: boolean read FRunning write FRunning;
    property PlanName: string read FName write SetPlanName;
    property ObjectName: string read FObjectName write FObjectName;
    property RestartTargetNum: integer read FRestartTargetNum write FRestartTargetNum;
    property Steps: T_Steps read FSteps;
    property Preview: Tf_preview read Fpreview write Fpreview;
    property Capture: Tf_capture read Fcapture write Fcapture;
    property Mount: T_mount read Fmount write Fmount;
    property Camera: T_camera read Fcamera write Fcamera;
    property Filter: Tf_filterwheel read Ffilter write Ffilter;
    property Autoguider: T_autoguider read Fautoguider write Fautoguider;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property DelayMsg: TNotifyMsg read FDelayMsg write FDelayMsg;
    property autostartguider: boolean read Fautostartguider write Fautostartguider;
    property onPlanChange: TNotifyEvent read FPlanChange write FPlanChange;
    property onStepProgress: TNotifyEvent read FonStepProgress write FonStepProgress;
    property onStartGuiding:TNotifyEvent read FStartGuiding write FStartGuiding;
    property onStopGuiding:TNotifyEvent read FStopGuiding write FStopGuiding;
end;


implementation

constructor T_Plan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NumSteps:=0;
  FRunning:=false;
  PlanTimer:=TTimer.Create(self);
  PlanTimer.Enabled:=false;
  PlanTimer.Interval:=1000;
  PlanTimer.OnTimer:=@PlanTimerTimer;
  StartTimer:=TTimer.Create(self);
  StartTimer.Enabled:=false;
  StartTimer.Interval:=5000;
  StartTimer.OnTimer:=@StartTimerTimer;
  RestartTimer:=TTimer.Create(self);
  RestartTimer.Enabled:=false;
  RestartTimer.Interval:=1000;
  RestartTimer.OnTimer:=@RestartTimerTimer;
  FObjectName:='';
end;

destructor  T_Plan.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure T_Plan.SetPlanName(val: string);
begin
  FName:=val;
  if Assigned(FPlanChange) then FPlanChange(self);
end;

procedure T_Plan.msg(txt:string; level: integer);
begin
  if Assigned(FonMsg) then FonMsg(txt,level);
end;

procedure T_Plan.ShowDelayMsg(txt:string);
begin
  if Assigned(FDelayMsg) then FDelayMsg(txt);
end;

procedure T_Plan.AssignPlan(Source: T_plan);
var i: integer;
begin
 FPlanChange:=Source.FPlanChange;
 FonMsg:=Source.FonMsg;
 FDelayMsg:=Source.FDelayMsg;
 Fcapture:=Source.Fcapture;
 Fpreview:=Source.Fpreview;
 Ffilter:=Source.Ffilter;
 Fmount:=Source.Fmount;
 Fcamera:=Source.Fcamera;
 Fautoguider:=Source.Fautoguider;
 FSequenceFile:=Source.FSequenceFile;
 FonStepProgress:=Source.FonStepProgress;
 NumSteps:=Source.NumSteps;
 SetLength(FSteps,NumSteps);
 for i:=0 to NumSteps-1 do begin
   FSteps[i]:=TStep.Create;
   FSteps[i].Assign(Source.FSteps[i]);
 end;
 FCurrentStep:=Source.FCurrentStep;
 StepRunning:=Source.StepRunning;
 StepTimeStart:=Source.StepTimeStart;
 StepDelayEnd:=Source.StepDelayEnd;
 FName:=Source.FName;
 FObjectName:=Source.FObjectName;
 FRestartTargetNum:=Source.FRestartTargetNum;
end;

procedure  T_Plan.Clear;
var i: integer;
begin
  for i:=0 to NumSteps-1 do FSteps[i].Free;
  SetLength(FSteps,0);
  NumSteps := 0;
  FName:='';
  if Assigned(FPlanChange) then FPlanChange(self);
end;

function T_Plan.Add(s: TStep):integer;
begin
  inc(NumSteps);
  SetLength(FSteps,NumSteps);
  FSteps[NumSteps-1]:=s;
  if Assigned(FPlanChange) then FPlanChange(self);
  result:=NumSteps-1;
end;

procedure T_Plan.RestartTimerTimer(Sender: TObject);
begin
  RestartTimer.Enabled:=false;
  Start;
end;

procedure T_Plan.Start;
begin
  FRunning:=true;
  if FObjectName<>'' then
     msg(Format(rsObjectStartP, [FObjectName, FName]),1)
  else
     msg(Format(rsStartPlan, [FName]),1);
  FCurrentStep:=-1;
  NextStep;
end;

procedure T_Plan.Stop;
begin
  UpdateDoneCount(true);
  FRunning:=false;
  if Capture.Running then Capture.BtnStartClick(Self);
end;

procedure T_Plan.ClearRunning;
begin
  FRunning:=false;
  PlanTimer.Enabled:=false;
  FCurrentStep:=-1;
  CurrentStepName:='';
end;

procedure T_Plan.Restart;
begin
  RestartTimer.Enabled:=true;
end;

procedure T_Plan.NextStep;
begin
  PlanTimer.Enabled:=false;
  FlatWaitDusk:=false;
  FlatWaitDawn:=false;
  UpdateDoneCount(true);
  inc(FCurrentStep);
  if FCurrentStep<NumSteps then begin
    StartStep;
    wait(2);
    PlanTimer.Enabled:=true;
  end
  else begin
    FRunning:=false;
    PlanTimer.Enabled:=false;
    FCurrentStep:=-1;
    CurrentStepName:='';
    if FObjectName<>'' then
      msg(Format(rsObjectPlanFi, [FObjectName, FName]),1)
    else
      msg(Format(rsPlanFinished, [FName]),1);
  end;
end;

procedure T_Plan.StartStep;
var p: TStep;
    r: string;
begin
  StepRunning:=true;
  p:=FSteps[CurrentStep];
  if p<>nil then begin
    CurrentStepNum:=CurrentStep;
    case p.steptype of
    0: begin
        // capture
        CurrentDoneCount:=p.donecount;
        if CurrentDoneCount>=p.count then begin
           // step already complete
           msg(Format(rsStepComplete, [p.description]), 2);
           exit;
        end;
        // set capture parameters
        if p.exposure>=0 then Fcapture.ExposureTime:=p.exposure;
        if Fcapture.PanelStack.Visible then
          Fcapture.StackNum.Value:=p.stackcount
        else
          Fcapture.StackNum.Value:=1;
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
        Fcapture.CheckLight(self); // be sure to apply the change now
        if p.autofocusstart then Fcapture.FocusNow:=true;
        Ffilter.Filters.ItemIndex:=p.filter;
        Ffilter.FiltersChange(self);
        if p.frtype=ord(FLAT) then begin
          if words(p.description,'',1,1)='Dusk' then
            FlatWaitDusk:=true;
          if words(p.description,'',1,1)='Dawn' then
            FlatWaitDawn:=true;
        end;
        msg(Format(rsStartStep, [p.description_str]),1);
        // start guiding
        if Fautostartguider and (p.frtype=ord(LIGHT)) and assigned(FStartGuiding) and (Fautoguider.State<>GUIDER_GUIDING) then begin
          FStartGuiding(self);
          if Fautoguider.State<>GUIDER_GUIDING then begin
            // cannot start guiding
            msg(rsFailedToStar, 1);
            exit;
          end;
        end;
        if not FRunning then exit;
        StepTimeStart:=now;
        CurrentStepName:=p.description;
        ShowDelayMsg('');
        StartCapture;
       end;
    1: begin
        // script
        FScriptRunning:=true;
        StepTimeStart:=now;
        CurrentStepName:=p.description;
        ShowDelayMsg('');
        msg(Format(rsStartStep, [p.description_str]),1);
        if not f_scriptengine.RunScript(p.scriptname,p.scriptpath,p.scriptargs)then begin
          msg(Format(rsScriptFailed, [p.scriptname]),0);
        end;
        FScriptRunning:=false;
       end;
    2: begin
        //switch
        StepTimeStart:=now;
        CurrentStepName:=p.description;
        msg(Format(rsStartStep, [p.description_str]),1);
        r:=f_scriptengine.cmd_setswitch(p.switchnickname,p.switchname,p.switchvalue);
        if r<>msgOK then begin
          msg(Format(rsSwitchFailed, [p.switchnickname+' '+p.switchname])+' '+r,0);
        end;
       end;
    end;
  end;
end;

procedure T_Plan.UpdateDoneCount(progress: boolean);
var s: TStep;
begin
 if (CurrentStep<0)or(FObjectName=ScriptTxt) then exit;
 s:=FSteps[CurrentStep];
 if s<>nil then begin
   // store image count
   if s.donecount<>CurrentDoneCount then begin
      s.donecount:=CurrentDoneCount;
      if FRestartTargetNum>0 then FSequenceFile.Items.SetValue('/Targets/Target'+inttostr(FRestartTargetNum)+'/Plan/Steps/Step'+inttostr(CurrentStep+1)+'/Done',s.donecount);
      if progress and assigned(FonStepProgress) then FonStepProgress(self);
   end;
 end;
end;

procedure T_Plan.PlanTimerTimer(Sender: TObject);
begin
 if FRunning then begin
   UpdateDoneCount(true);
   StepRunning:=Capture.Running or FScriptRunning;
   if not StepRunning then begin
       NextStep;
   end;
 end
 else begin
    PlanTimer.Enabled:=false;
    FCurrentStep:=-1;
    msg(Format(rsPlanStopped, [FName]),1);
    ShowDelayMsg('');
 end;
end;


procedure T_Plan.StartTimerTimer(Sender: TObject);
begin
 StartTimer.Enabled:=false;
 StartCapture;
end;

procedure T_Plan.StartCapture;
begin
 if preview.Running then begin
     msg(rsStopPreview,2);
     camera.AbortExposure;
     preview.stop;
     StartTimer.Enabled:=true;
     exit;
 end;
  Fcapture.BtnStartClick(nil);
end;

function T_Plan.IndexOf(id:LongWord):integer;
var i: integer;
begin
  result:=-1;
  for i:=0 to NumSteps-1 do begin
     if id=FSteps[i].id then begin
       result:=i;
       break;
     end;
  end;
end;

function T_Plan.totaltime(skipdone:boolean): double;
var i: integer;
begin
  result:=0;
  if (Autoguider<>nil)and(Autoguider.AutoguiderType<>agNONE)and(Autoguider.AutoguiderType<>agDITHER)
  then
    result:=result+SettleMaxTime;
  for i:=0 to Count-1 do begin
    result:=result+Steps[i].totaltime(skipdone);
  end;
end;

end.

