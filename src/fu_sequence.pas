unit fu_sequence;

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

uses
  pu_edittargets, u_ccdconfig, u_global, u_utils, UScaleDPI, indiapi, cu_sequencefile, u_speech,
  fu_capture, fu_preview, fu_filterwheel, u_translation, u_hints, math,
  cu_mount, cu_camera, cu_autoguider, cu_astrometry, cu_rotator, pu_viewtext,
  cu_targets, cu_plan, cu_planetarium, pu_pause, fu_safety, fu_weather, cu_dome,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { Tf_sequence }

  Tf_sequence = class(TFrame)
    BtnReset: TButton;
    BtnEditTargets: TButton;
    BtnStart: TButton;
    BtnStop: TButton;
    BtnNewTargets: TButton;
    BtnLoadTargets: TButton;
    BtnCopy: TButton;
    BtnDelete: TButton;
    BtnPause: TButton;
    BtnStatus: TButton;
    led: TShape;
    Panel5: TPanel;
    StatusTimer: TTimer;
    StartTimer: TTimer;
    Unattended: TCheckBox;
    DelayMsg: TLabel;
    StatusMsg: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog1: TSaveDialog;
    Title1: TPanel;
    Title2: TPanel;
    Title3: TPanel;
    TargetGrid: TStringGrid;
    PlanGrid: TStringGrid;
    procedure BtnCopyClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnEditTargetsClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnLoadTargetsClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnStatusClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure PlanGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StartTimerTimer(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure TargetGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TargetGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure TargetsChange(Sender: TObject);
    procedure PlanChange(Sender: TObject);
    procedure UnattendedChange(Sender: TObject);
  private
    { private declarations }
    TargetRow, PlanRow: integer;
    StartingSequence: Boolean;
    Targets: T_Targets;
    FonMsg: TNotifyMsg;
    FConnectAutoguider: TNotifyEvent;
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
    Fastrometry: TAstrometry;
    Fplanetarium: TPlanetarium;
    AutoguiderAlert,AutoguiderStarting: boolean;
    AutoguiderAlertTime,AutoguiderMsgTime: double;
    MountTrackingAlert: boolean;
    MountTrackingAlertTime: double;
    procedure SetPreview(val: Tf_preview);
    procedure SetCapture(val: Tf_capture);
    procedure SetMount(val: T_mount);
    procedure SetCamera(val: T_camera);
    procedure SetRotator(val: T_rotator);
    procedure SetFilter(val: Tf_filterwheel);
    procedure SetWeather(val: Tf_weather);
    procedure SetSafety(val: Tf_safety);
    procedure SetDome(val: T_dome);
    procedure SetAutoguider(val: T_autoguider);
    procedure SetAstrometry(val: TAstrometry);
    procedure SetPlanetarium(val: TPlanetarium);
    function GetRunning: boolean;
    function GetBusy: boolean;
    function GetTargetCoord: boolean;
    function GetTargetRA: double;
    function GetTargetDE: double;
    function GetCurrentPlan: T_Plan;
    procedure SaveTargets(fn,defaultname:string);
    procedure StartSequence;
    procedure ClearTargetGrid;
    procedure ClearPlanGrid;
    procedure msg(txt:string; level: integer);
    procedure ShowDelayMsg(txt:string);
    procedure StopSequence;
    procedure EndSequence(Sender: TObject);
    procedure SetEditBtn(onoff:boolean);
    function GetEndShutdown: boolean;
    procedure SetEndShutdown(value:boolean);
    function GetOnShutdown: TNotifyEvent;
    procedure SetOnShutdown(value:TNotifyEvent);
    function GetPercentComplete: double;
    function GetTargetPercentComplete: double;
    procedure ClearRestartHistory(Confirm:boolean);
    procedure CompatLoadPlan(p: T_Plan; plan,obj:string);
  public
    { public declarations }
    StepRepeatCount, StepTotalCount: integer;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure AutoguiderDisconnected;
    procedure AutoguiderIddle;
    procedure MountTrackingStarted;
    procedure MountTrackingStopped;
    procedure ExposureAborted;
    procedure CameraDisconnected;
    procedure LoadTargets(fn: string);
    procedure AbortSequence;
    procedure WeatherChange(value:boolean);
    property Busy: boolean read GetBusy;
    property Running: boolean read GetRunning;
    property PercentComplete: double read GetPercentComplete;
    property TargetPercentComplete: double read GetTargetPercentComplete;
    property TargetCoord: boolean read GetTargetCoord;
    property TargetRA: double read GetTargetRA;
    property TargetDE: double read GetTargetDE;
    property CurrentPlan: T_Plan read GetCurrentPlan;
    property Preview: Tf_preview read Fpreview write SetPreview;
    property Capture: Tf_capture read Fcapture write SetCapture;
    property Mount: T_mount read Fmount write SetMount;
    property Camera: T_camera read Fcamera write SetCamera;
    property Rotator: T_rotator read Frotator write SetRotator;
    property Filter: Tf_filterwheel read Ffilter write SetFilter;
    property Weather: Tf_weather read Fweather write SetWeather;
    property Safety: Tf_safety read Fsafety write SetSafety;
    property Dome: T_dome read Fdome write SetDome;
    property Autoguider: T_autoguider read Fautoguider write SetAutoguider;
    property Astrometry: TAstrometry read Fastrometry write SetAstrometry;
    property Planetarium: TPlanetarium read FPlanetarium write SetPlanetarium;
    property AtEndShutdown: boolean read GetEndShutdown write SetEndShutdown;
    property OnShutdown: TNotifyEvent read GetOnShutdown write SetOnShutdown;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onConnectAutoguider: TNotifyEvent read FConnectAutoguider write FConnectAutoguider;
  end;

var
  f_sequence: Tf_sequence;

implementation

uses LazFileUtils;

{$R *.lfm}

constructor Tf_sequence.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title1.Color:=clWindowFrame;
 Title2.Color:=clWindowFrame;
 Title3.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel2.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 Panel2.ChildSizing.VerticalSpacing:=4;
 TargetGrid.FixedColor := clBackground;
 PlanGrid.FixedColor := clBackground;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 CurrentSeqName:='';
 CurrentTargetName:='';
 CurrentStepName:='';
 StartingSequence:=false;
 Targets:=T_Targets.Create(nil);
 Targets.Preview:=Fpreview;
 Targets.Capture:=Fcapture;
 Targets.Mount:=Fmount;
 Targets.Camera:=Fcamera;
 Targets.Filter:=Ffilter;
 Targets.Autoguider:=Fautoguider;
 Targets.Astrometry:=Fastrometry;
 Targets.Planetarium:=Fplanetarium;
 Targets.onMsg:=@Msg;
 Targets.onEndSequence:=@EndSequence;
 Targets.DelayMsg:=@ShowDelayMsg;
 Targets.onTargetsChange:=@TargetsChange;
 Targets.onPlanChange:=@PlanChange;
 TargetGrid.ColWidths[2]:=0;
 TargetGrid.ColWidths[3]:=0;
 f_EditTargets:=Tf_EditTargets.Create(nil);
 f_EditTargets.Astrometry:=Fastrometry;
end;

destructor  Tf_sequence.Destroy;
begin
 Targets.Free;
 f_EditTargets.Free;
 ClearTargetGrid;
 ClearPlanGrid;
 SequenceFile.Free;
 inherited Destroy;
end;

procedure Tf_sequence.SetLang;
begin
  Title1.Caption:=rsSequence;
  Title2.Caption:=rsCurrentPlan;
  Title3.Caption:=rsTargets;
  BtnEditTargets.Caption:=rsEdit;
  BtnLoadTargets.Caption:=rsLoad;
  BtnNewTargets.Caption:=rsNew;
  BtnStart.Caption:=rsStart;
  BtnStop.Caption:=rsStop;
  Unattended.Caption:=rsRunUnattende;
  BtnCopy.Caption:=rsCopy;
  BtnDelete.Caption:=rsDelete;
  BtnReset.Caption:=rsReset;
  BtnPause.Caption:=rsPause;
  BtnLoadTargets.Hint:=rsLoadASequenc;
  BtnNewTargets.Hint:=rsCreateANewSe;
  BtnEditTargets.Hint:=rsEditTheCurre;
  BtnStart.Hint:=rsStartTheSequ;
  BtnStop.Hint:=rsStopTheSeque;
  BtnStatus.Caption:=rsStatus2;
  Unattended.Hint:=rsIfCheckedNoC;
  BtnCopy.Hint:=rsCopyTheSeque;
  BtnDelete.Hint:=rsDeleteTheSeq;
  BtnReset.Hint:=rsClearTheSequ;
  BtnPause.Hint:=rsPauseTheSequ;
  BtnStatus.Hint:=rsShowCompleti;
  TargetGrid.Cells[0, 0]:=rsObject;
  TargetGrid.Cells[1, 0]:=rsPlan;
  TargetGrid.Cells[2, 0]:=rsBegin;
  TargetGrid.Cells[3, 0]:=rsEnd;
  TargetGrid.Cells[4, 0]:=rsRA;
  TargetGrid.Cells[5,0]:=rsDec;
  PlanGrid.Cells[0, 0]:=rsDesc;
  PlanGrid.Cells[1, 0]:=rsExp2;
  PlanGrid.Cells[2, 0]:=rsCount;
  PlanGrid.Cells[3, 0]:=rsType;
  PlanGrid.Cells[4, 0]:=rsFilter;
end;

procedure Tf_sequence.SetPreview(val: Tf_preview);
begin
  Fpreview:=val;
  Targets.Preview:=Fpreview;
end;

procedure Tf_sequence.SetCapture(val: Tf_capture);
begin
  Fcapture:=val;
  Targets.Capture:=Fcapture;
end;

procedure Tf_sequence.SetMount(val: T_mount);
begin
  Fmount:=val;
  Targets.Mount:=Fmount;
end;

procedure Tf_sequence.SetCamera(val: T_camera);
begin
  Fcamera:=val;
  Targets.Camera:=Fcamera;
end;

procedure Tf_sequence.SetRotator(val: T_rotator);
begin
  Frotator:=val;
  Targets.Rotaror:=Frotator;
end;

procedure Tf_sequence.SetFilter(val: Tf_filterwheel);
begin
  Ffilter:=val;
  Targets.Filter:=Ffilter;
end;

procedure Tf_sequence.SetWeather(val: Tf_weather);
begin
  Fweather:=val;
  Targets.Weather:=Fweather;
end;

procedure Tf_sequence.SetSafety(val: Tf_safety);
begin
  Fsafety:=val;
  Targets.Safety:=Fsafety;
end;

procedure Tf_sequence.SetDome(val: T_dome);
begin
  Fdome:=val;
  Targets.Dome:=Fdome;
end;

procedure Tf_sequence.SetAutoguider(val: T_autoguider);
begin
  Fautoguider:=val;
  Targets.Autoguider:=Fautoguider;
end;

procedure Tf_sequence.SetAstrometry(val: TAstrometry);
begin
  Fastrometry:=val;
  Targets.Astrometry:=Fastrometry;
  f_EditTargets.Astrometry:=Fastrometry;
end;

procedure Tf_sequence.SetPlanetarium(val: TPlanetarium);
begin
  Fplanetarium:=val;
  Targets.Planetarium:=Fplanetarium;
end;

procedure Tf_sequence.ClearTargetGrid;
begin
  TargetGrid.RowCount:=1;
end;

procedure Tf_sequence.ClearPlanGrid;
begin
  PlanGrid.RowCount:=1;
end;

procedure Tf_sequence.msg(txt:string; level: integer);
begin
  if level<9 then begin
     StatusMsg.Caption:=txt;
     if VoiceSequence and Targets.Running then Speak(txt);
  end;
  if Assigned(FonMsg) then FonMsg(txt, level);
end;

procedure Tf_sequence.ShowDelayMsg(txt:string);
begin
  DelayMsg.Caption:=txt;
end;

procedure Tf_sequence.BtnCopyClick(Sender: TObject);
var txt,fn1,fn2: string;
begin
  fn1:=SequenceFile.Filename;
  txt:=FormEntry(self, rsCopyTo, '');
  if txt='' then exit;
  fn2:=slash(ConfigDir)+txt+'.targets';
  if FileExistsUTF8(fn2) then begin
     if MessageDlg(Format(rsSequenceAlre, [fn2]), mtConfirmation, mbYesNo, 0)<>
       mrYes then
       exit;
  end;
  if CopyFile(fn1,fn2,false) then begin
    SequenceFile.Filename:=fn2;
    SequenceFile.Items.SetValue('/ListName',txt);
    SequenceFile.Save;
    LoadTargets(fn2);
    ClearRestartHistory(false);
  end;
end;

procedure Tf_sequence.BtnDeleteClick(Sender: TObject);
var fn: string;
begin
   fn:=SequenceFile.Filename;
   if MessageDlg(Format(rsDoYouWantToD, [fn]), mtConfirmation, mbYesNo, 0)=
     mrYes then begin
      SequenceFile.Clear;
      DeleteFileUTF8(fn);
      ClearTargetGrid;
      ClearPlanGrid;
      f_EditTargets.TargetList.RowCount:=1;
      Targets.Clear;
   end;
end;

procedure Tf_sequence.BtnResetClick(Sender: TObject);
begin
   ClearRestartHistory(true);
end;

procedure Tf_sequence.BtnEditTargetsClick(Sender: TObject);
var i,n:integer;
    t:TTarget;
    defaultname: string;
begin
   f_EditTargets.ClearTargetList;
   f_EditTargets.LoadPlanList;
   f_EditTargets.LoadScriptList;
   if (Sender=BtnEditTargets)and(Targets.Count>0) then begin
      // Edit
      f_EditTargets.TargetName.Caption:=Targets.TargetName;
      f_EditTargets.CheckBoxRestartStatus.Checked:=not Targets.IgnoreRestart;
      f_EditTargets.CheckBoxResetRepeat.Checked:=Targets.ResetRepeat;
      f_EditTargets.TargetsRepeat:=Targets.TargetsRepeat;
      f_EditTargets.TargetsRepeatCount:=Targets.TargetsRepeatCount;
      f_EditTargets.TargetList.RowCount:=Targets.Count+1;
      f_EditTargets.SeqStart.Checked:=Targets.SeqStart;
      f_EditTargets.SeqStop.Checked:=Targets.SeqStop;
      f_EditTargets.SeqStartTwilight.Checked:=Targets.SeqStartTwilight;
      f_EditTargets.SeqStopTwilight.Checked:=Targets.SeqStopTwilight;
      f_EditTargets.SeqStartAt.Text:=TimeToStr(Targets.SeqStartAt);
      f_EditTargets.SeqStopAt.Text:=TimeToStr(Targets.SeqStopAt);
      f_EditTargets.ccNone.Checked:=not(Targets.AtStartCool or Targets.AtStartUnpark);
      f_EditTargets.ccCool.Checked:=Targets.AtStartCool;
      f_EditTargets.ccUnpark.Checked:=Targets.AtStartUnpark;
      f_EditTargets.cbNone.Checked:=not(Targets.AtEndStopTracking or Targets.AtEndPark or Targets.AtEndCloseDome or Targets.AtEndWarmCamera or Targets.AtEndRunScript);
      f_EditTargets.cbStopTracking.Checked:=Targets.AtEndStopTracking;
      f_EditTargets.cbParkScope.Checked:=Targets.AtEndPark;
      f_EditTargets.cbParkDome.Checked:=Targets.AtEndCloseDome;
      f_EditTargets.cbWarm.Checked:=Targets.AtEndWarmCamera;
      f_EditTargets.cbScript.Checked:=Targets.AtEndRunScript;
      f_EditTargets.cbUnattended.Checked:=Targets.OnErrorRunScript;
      f_EditTargets.BtnEndScript.Hint:=Targets.AtEndScript;
      f_EditTargets.BtnUnattendedScript.Hint:=Targets.OnErrorScript;
      for i:=1 to Targets.Count do begin
        t:=TTarget.Create;
        t.Assign(Targets.Targets[i-1]);
        f_EditTargets.SetTarget(i,t);
        f_EditTargets.TargetList.Objects[colseq,i]:=t;
      end;
    end else begin
      // New
      Targets.Clear;
      SequenceFile.Clear;
      CurrentSeqName:='';
      f_EditTargets.TargetName.Caption:='New targets';
      f_EditTargets.CheckBoxRestartStatus.Checked:=true;
      f_EditTargets.CheckBoxResetRepeat.Checked:=true;
      f_EditTargets.TargetsRepeat:=1;
      f_EditTargets.TargetsRepeatCount:=0;
      f_EditTargets.SeqStart.Checked:=false;
      f_EditTargets.SeqStop.Checked:=false;
      f_EditTargets.SeqStartTwilight.Checked:=false;
      f_EditTargets.SeqStopTwilight.Checked:=false;
      f_EditTargets.SeqStartAt.Text:='00:00:00';
      f_EditTargets.SeqStopAt.Text:='00:00:00';
      f_EditTargets.ccNone.Checked:=false;
      f_EditTargets.ccCool.Checked:=true;
      f_EditTargets.ccUnpark.Checked:=true;
      f_EditTargets.cbNone.Checked:=false;
      f_EditTargets.cbStopTracking.Checked:=true;
      f_EditTargets.cbParkScope.Checked:=false;
      f_EditTargets.cbParkDome.Checked:=false;
      f_EditTargets.cbWarm.Checked:=false;
      f_EditTargets.cbScript.Checked:=false;
      f_EditTargets.cbUnattended.Checked:=false;
      f_EditTargets.TargetList.RowCount:=1;
    end;
    FormPos(f_EditTargets,mouse.CursorPos.X,mouse.CursorPos.Y);
    if f_EditTargets.ShowModal=mrOK then begin
      n:=f_EditTargets.TargetList.RowCount;
      Targets.Clear;
      defaultname:=FormatDateTime('mmdd',now);
      for i:=1 to n-1 do begin
        if (f_EditTargets.TargetList.Cells[1,i]<>ScriptTxt) and (f_EditTargets.TargetList.Cells[1,i]<>SkyFlatTxt) then
           defaultname:=f_EditTargets.TargetList.Cells[1,i];
        t:=TTarget.Create;
        t.Assign(TTarget(f_EditTargets.TargetList.Objects[0,i]));
        Targets.Add(t);
      end;
      Targets.IgnoreRestart    := not f_EditTargets.CheckBoxRestartStatus.Checked;
      Targets.ResetRepeat      := f_EditTargets.CheckBoxResetRepeat.Checked;
      Targets.TargetsRepeat    := f_EditTargets.TargetsRepeat;
      Targets.TargetsRepeatCount:=f_EditTargets.TargetsRepeatCount;
      Targets.SeqStart         := f_EditTargets.SeqStart.Checked;
      Targets.SeqStop          := f_EditTargets.SeqStop.Checked;
      Targets.SeqStartTwilight := f_EditTargets.SeqStartTwilight.Checked;
      Targets.SeqStopTwilight  := f_EditTargets.SeqStopTwilight.Checked;
      Targets.SeqStartAt       := StrToTimeDef(f_EditTargets.SeqStartAt.Text,Targets.SeqStartAt);
      Targets.SeqStopAt        := StrToTimeDef(f_EditTargets.SeqStopAt.Text,Targets.SeqStopAt);
      Targets.AtStartCool      := f_EditTargets.ccCool.Checked;
      Targets.AtStartUnpark    := f_EditTargets.ccUnpark.Checked;
      Targets.AtEndStopTracking := f_EditTargets.cbStopTracking.Checked;
      Targets.AtEndPark         := f_EditTargets.cbParkScope.Checked;
      Targets.AtEndCloseDome    := f_EditTargets.cbParkDome.Checked;
      Targets.AtEndWarmCamera   := f_EditTargets.cbWarm.Checked;
      Targets.AtEndRunScript    := f_EditTargets.cbScript.Checked;
      Targets.OnErrorRunScript  := f_EditTargets.cbUnattended.Checked;
      Targets.AtEndScript       := f_EditTargets.BtnEndScript.Hint;
      Targets.OnErrorScript     := f_EditTargets.BtnUnattendedScript.Hint;
      SaveTargets(SequenceFile.Filename,defaultname);
      LoadTargets(SequenceFile.Filename);
    end else begin
      // reset last saved
      LoadTargets(SequenceFile.Filename);
    end;
    BtnReset.Enabled:=not Targets.IgnoreRestart;
end;

procedure Tf_sequence.LoadTargets(fn: string);
var t:TTarget;
    p:T_Plan;
    s: TStep;
    x, str:string;
    i,j,k,m,n: integer;
begin
   msg('',2);
   SequenceFile.Filename:='';
   SequenceFile.Clear;
   SequenceFile.Filename:=fn;
   CurrentSeqName:=SequenceFile.CurrentName;
   Targets.Clear;
   Targets.TargetName:=CurrentSeqName;
   n:=SequenceFile.Items.GetValue('/TargetNum',0);
   Targets.FileVersion      :=SequenceFile.Items.GetValue('/Version',1);
   Targets.TargetsRepeat    :=SequenceFile.Items.GetValue('/RepeatCount',1);
   Targets.IgnoreRestart    := SequenceFile.Items.GetValue('/Targets/IgnoreRestart',true);
   if Targets.IgnoreRestart then
      Targets.TargetsRepeatCount:=0
   else
      Targets.TargetsRepeatCount:=SequenceFile.Items.GetValue('/Targets/RepeatDone',0);
   Targets.ResetRepeat      := SequenceFile.Items.GetValue('/Targets/ResetRepeat',true);
   Targets.SeqStart         := SequenceFile.Items.GetValue('/Startup/SeqStart',false);
   Targets.SeqStop          := SequenceFile.Items.GetValue('/Startup/SeqStop',false);
   Targets.SeqStartTwilight := SequenceFile.Items.GetValue('/Startup/SeqStartTwilight',false);
   Targets.SeqStopTwilight  := SequenceFile.Items.GetValue('/Startup/SeqStopTwilight',false);
   Targets.SeqStartAt       := StrToTimeDef(SequenceFile.Items.GetValue('/Startup/SeqStartAt','00:00:00'),0);
   Targets.SeqStopAt        := StrToTimeDef(SequenceFile.Items.GetValue('/Startup/SeqStopAt','00:00:00'),0);
   Targets.AtStartCool      := SequenceFile.Items.GetValue('/Startup/CoolCamera',false);
   Targets.AtStartUnpark    := SequenceFile.Items.GetValue('/Startup/Unpark',false);
   Targets.AtEndStopTracking:= SequenceFile.Items.GetValue('/Termination/StopTracking',true);
   Targets.AtEndPark        := SequenceFile.Items.GetValue('/Termination/Park',false);
   Targets.AtEndCloseDome   := SequenceFile.Items.GetValue('/Termination/CloseDome',false);
   Targets.AtEndWarmCamera  := SequenceFile.Items.GetValue('/Termination/WarmCamera',false);
   Targets.AtEndRunScript   := SequenceFile.Items.GetValue('/Termination/RunScript',false);
   Targets.OnErrorRunScript := SequenceFile.Items.GetValue('/Termination/ErrorRunScript',false);
   Targets.AtEndScript      := SequenceFile.Items.GetValue('/Termination/EndScript','');
   Targets.OnErrorScript    := SequenceFile.Items.GetValue('/Termination/ErrorScript','');
   if Targets.FileVersion<3 then begin
     // compatibility with previous version
     if FileExistsUTF8(slash(ScriptDir[1].path)+'end_sequence.script') then begin
        Targets.AtEndStopTracking:=false;
        Targets.AtEndRunScript:=true;
        Targets.AtEndScript:='end_sequence';
     end;
   end;
   if n>0 then begin
     for i:=1 to n do begin
       t:=TTarget.Create;
       t.objectname:=trim(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/ObjectName',''));
       t.planname:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan','');
       t.path:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Path','');
       if Targets.FileVersion>=2 then begin
         t.starttime:=StrToTimeDef(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartTime',''),-1);
         t.endtime:=StrToTimeDef(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndTime',''),-1);
       end else begin
         t.starttime:=-1;
         t.endtime:=-1;
       end;
       t.startrise:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartRise',false);
       t.endset:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndSet',false);
       t.startmeridian:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StartMeridian',NullCoord);
       t.endmeridian:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/EndMeridian',NullCoord);
       t.darknight:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/DarkNight',false);
       t.skip:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Skip',false);
       x:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RA','');
       if x='-' then
         t.ra:=NullCoord
       else
         t.ra:=StrToAR(x);
       x:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Dec','');
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
       if (Targets.FileVersion<4)and(t.ra<>NullCoord)and(t.de<>NullCoord) then begin
         // previous version, up to 3, store apparent coordinates
         t.ra:=t.ra*15*deg2rad;
         t.de:=t.de*deg2rad;
         ApparentToJ2000(t.ra,t.de);
         t.ra:=rad2deg*t.ra/15;
         t.de:=rad2deg*t.de;
       end;
       x:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/PA','-');
       if x='-' then
         t.pa:=NullCoord
       else
         t.pa:=StrToFloatDef(x,NullCoord);
       t.astrometrypointing:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',false);
       t.updatecoord:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/UpdateCoord',false);
       t.inplaceautofocus:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/InplaceAutofocus',AutofocusInPlace);
       t.previewexposure:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',1.0);
       t.preview:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Preview',false);
       t.repeatcount:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',1));
       if Targets.IgnoreRestart then
          t.repeatdone:=0
       else
          t.repeatdone:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0));
       t.delay:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Delay',1.0);
       t.FlatCount:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatCount',1));
       t.FlatBinX:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatBinX',1));
       t.FlatBinY:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatBinY',1));
       t.FlatGain:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatGain',0));
       t.FlatOffset:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatOffset',0));
       t.FlatFstop:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatFstop','');
       t.FlatFilters:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/FlatFilters','');
       if Targets.FileVersion>=5 then begin
          Targets.Add(t);
          p:=T_Plan(t.plan);
          p.ObjectName:=t.objectname;
          p.PlanName:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Name',t.planname);
          m:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/StepNum',0));
          for j:=0 to m-1 do begin
            s:=TStep.Create;
            s.description:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Description','');
            str:=UpperCase(trim(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/FrameType','Light')));
            if str='LIGHT' then s.frtype:=LIGHT
            else if str='BIAS' then s.frtype:=BIAS
            else if str='DARK' then s.frtype:=DARK
            else if str='FLAT' then s.frtype:=FLAT
            else s.frtype:=LIGHT;
            s.exposure:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Exposure',1.0);
            str:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Binning','1x1');
            k:=f_EditTargets.StepList.Columns[pcolbin-1].PickList.IndexOf(str);
            if k<0 then
              f_EditTargets.StepList.Columns[pcolbin-1].PickList.Add(str);
            k:=pos('x',str);
            if k>0 then begin
               str:=trim(copy(str,1,k-1));
               s.binx:=StrToIntDef(str,1);
               str:=trim(copy(str,k+1,9));
               s.biny:=StrToIntDef(str,1);
            end else begin
              s.binx:=1;
              s.biny:=1;
            end;
            str:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Gain','');
            s.gain:=StrToIntDef(str,Gain);
            str:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Offset','');
            s.offset:=StrToIntDef(str,Offset);
            s.fstop:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Fstop','');
            str:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Filter','');
            f_EditTargets.originalFilter[i]:=str;
            k:=f_EditTargets.StepList.Columns[pcolfilter-1].PickList.IndexOf(str);
            if k<0 then k:=0;
            s.filter:=k;
            s.count:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Count',1));
            s.dither:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Dither',false);
            s.dithercount:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/DitherCount',1));
            s.autofocusstart:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusStart',false);
            s.autofocus:=SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Autofocus',false);
            s.autofocuscount:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusCount',10));
            s.donecount:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Done',0));
            p.Add(s);
          end;
          if TemplateModified(p) then begin
            if  pos('*',p.PlanName)<=0 then begin
              p.PlanName:=p.PlanName+'*';
              t.planname:=p.PlanName;
              TargetsChange(nil);
            end;
          end
          else begin
            if pos('*',p.PlanName)>0 then begin
              p.PlanName:=StringReplace(p.PlanName,'*','',[]);
              t.planname:=p.PlanName;
              TargetsChange(nil);
            end;
          end
         end
         else begin
           // compatibility with old sequence format
           m:=trunc(SequenceFile.Items.GetValue('/Targets/Target'+inttostr(i)+'/StepDone/StepCount',0));
           Targets.Add(t);
           CompatLoadPlan(T_Plan(t.plan), t.planname, t.objectname);
         end;
     end;
   end;
   TargetGrid.Row:=1;
   TargetGridSelection(TargetGrid,0,1);
   BtnReset.Enabled:=not Targets.IgnoreRestart;
   if Targets.CheckDoneCount then begin
      msg(Format(rsThisSequence,['"'+CurrentSeqName+'"']), 2);
      msg(targets.LastDoneStep,2);
   end;
end;

procedure Tf_sequence.ClearRestartHistory(Confirm:boolean);
begin
   if Targets.CheckDoneCount then begin
     if (not Confirm) or
       (MessageDlg(rsClearTheComp, Format(rsThisSequence2, [crlf, crlf+crlf]),mtConfirmation,mbYesNo,0)=mrYes)
       then begin
        msg('',2);
        Targets.ClearDoneCount(true);
        SaveTargets(SequenceFile.Filename,'');
        LoadTargets(SequenceFile.Filename);
     end;
   end;
end;

procedure Tf_sequence.CompatLoadPlan(p: T_plan; plan,obj:string);
var fn,buf1,buf2,msgstr: string;
    i,n:integer;
    pfile: TCCDconfig;
    s: TStep;
begin
  fn:=slash(ConfigDir)+plan+'.plan';
  if FileExistsUTF8(fn) then begin
     p.Clear;
     p.PlanName:=plan;
     pfile:=TCCDconfig.Create(self);
     pfile.Filename:=fn;
     n:=pfile.GetValue('/StepNum',0);
     if camera.CanSetGain then begin
       msgstr:='';
       for i:=0 to n do begin
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
       f_EditTargets.ReadStep(pfile,i,s,msgstr);
       s.donecount:=0;
       p.Add(s);
     end;
     msgstr:=msgstr+blank+obj+' '+plan+' warning! the completion status is cleared.';
     msg(msgstr,1);
  end;
end;

procedure Tf_sequence.TargetsChange(Sender: TObject);
var i: integer;
begin
   ClearTargetGrid;
   TargetGrid.RowCount:=Targets.count+1;
   Title3.Caption:=rsTargets+': '+Targets.TargetName;
   if Targets.TargetsRepeat>1 then
     Title3.Caption:=Title3.Caption+' x'+inttostr(Targets.TargetsRepeat);
   for i:=1 to Targets.count do begin
     TargetGrid.Cells[0,i]:=Targets.Targets[i-1].objectname;
     if Targets.Targets[i-1].repeatcount>0 then
        TargetGrid.Cells[1,i]:=Targets.Targets[i-1].planname
     else
        TargetGrid.Cells[1,i]:=format(rsSkipTarget,['']);
     TargetGrid.Cells[2,i]:=TimetoStr(Targets.Targets[i-1].starttime);
     TargetGrid.Cells[3,i]:=TimetoStr(Targets.Targets[i-1].endtime);
     TargetGrid.Cells[4,i]:=RAToStr(Targets.Targets[i-1].ra);
     TargetGrid.Cells[5,i]:=DEToStr(Targets.Targets[i-1].de);
   end;
end;

procedure Tf_sequence.PlanChange(Sender: TObject);
var i: integer;
    p: T_plan;
begin
  try
   p:=T_Plan(sender);
   ClearPlanGrid;
   Title2.Caption:=rsPlan+': '+p.PlanName;
   PlanGrid.RowCount:=p.count+1;
   for i:=1 to p.count do begin
     PlanGrid.Cells[0,i]:=T_Plan(sender).Steps[i-1].description_str;
     PlanGrid.Cells[1,i]:=T_Plan(sender).Steps[i-1].exposure_str;
     PlanGrid.Cells[2,i]:=T_Plan(sender).Steps[i-1].count_str;
     PlanGrid.Cells[3,i]:=T_Plan(sender).Steps[i-1].frtype_str;
     PlanGrid.Cells[4,i]:=T_Plan(sender).Steps[i-1].filter_str;
   end;
  except
  end;
end;

procedure Tf_sequence.UnattendedChange(Sender: TObject);
begin
 Targets.Unattended:=Unattended.Checked;
end;

procedure Tf_sequence.BtnLoadTargetsClick(Sender: TObject);
begin
 OpenDialog1.InitialDir:=ConfigDir;
 OpenDialog1.FileName:='*.targets';
 if OpenDialog1.Execute then begin
   LoadTargets(OpenDialog1.FileName);
 end;
end;

procedure Tf_sequence.SaveTargets(fn,defaultname:string);
var t:TTarget;
    p:T_Plan;
    i,j: integer;
begin
 if TargetGrid.RowCount>1 then begin
    if fn='' then begin
      SaveDialog1.InitialDir:=ConfigDir;
      SaveDialog1.FileName:=slash(ConfigDir)+defaultname+'.targets';
      if SaveDialog1.Execute then begin
        fn:=SaveDialog1.FileName;
      end
      else exit;
    end;
    SequenceFile.Clear;
    SequenceFile.Filename:=fn;
    CurrentSeqName:=SequenceFile.CurrentName;
    Targets.TargetName:=CurrentSeqName;
    SequenceFile.Items.SetValue('/Version',TargetFileVersion);
    SequenceFile.Items.SetValue('/ListName',CurrentSeqName);
    SequenceFile.Items.SetValue('/TargetNum',Targets.Count);
    SequenceFile.Items.SetValue('/RepeatCount',Targets.TargetsRepeat);
    SequenceFile.Items.SetValue('/Targets/IgnoreRestart',Targets.IgnoreRestart);
    if Targets.IgnoreRestart then
       SequenceFile.Items.SetValue('/Targets/RepeatDone',0)
    else
       SequenceFile.Items.SetValue('/Targets/RepeatDone',Targets.TargetsRepeatCount);
    SequenceFile.Items.SetValue('/Targets/ResetRepeat',Targets.ResetRepeat);
    SequenceFile.Items.SetValue('/Startup/SeqStart',Targets.SeqStart);
    SequenceFile.Items.SetValue('/Startup/SeqStop',Targets.SeqStop);
    SequenceFile.Items.SetValue('/Startup/SeqStartTwilight',Targets.SeqStartTwilight);
    SequenceFile.Items.SetValue('/Startup/SeqStopTwilight',Targets.SeqStopTwilight);
    SequenceFile.Items.SetValue('/Startup/SeqStartAt',TimeToStr(Targets.SeqStartAt));
    SequenceFile.Items.SetValue('/Startup/SeqStopAt',TimeToStr(Targets.SeqStopAt));
    SequenceFile.Items.SetValue('/Startup/CoolCamera',Targets.AtStartCool);
    SequenceFile.Items.SetValue('/Startup/Unpark',Targets.AtStartUnpark);
    SequenceFile.Items.SetValue('/Termination/StopTracking',Targets.AtEndStopTracking);
    SequenceFile.Items.SetValue('/Termination/Park',Targets.AtEndPark);
    SequenceFile.Items.SetValue('/Termination/CloseDome',Targets.AtEndCloseDome);
    SequenceFile.Items.SetValue('/Termination/WarmCamera',Targets.AtEndWarmCamera);
    SequenceFile.Items.SetValue('/Termination/RunScript',Targets.AtEndRunScript);
    SequenceFile.Items.SetValue('/Termination/ErrorRunScript',Targets.OnErrorRunScript);
    SequenceFile.Items.SetValue('/Termination/EndScript',Targets.AtEndScript);
    SequenceFile.Items.SetValue('/Termination/ErrorScript',Targets.OnErrorScript);
    for i:=1 to Targets.Count do begin
      t:=Targets.Targets[i-1];
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/ObjectName',t.objectname);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan',t.planname);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Path',t.path);
      if t.starttime>=0 then
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartTime',TimetoStr(t.starttime))
      else
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartTime','');
      if t.endtime>=0 then
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndTime',TimetoStr(t.endtime))
      else
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndTime','');
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartRise',t.startrise);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndSet',t.endset);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/StartMeridian',t.startmeridian);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/EndMeridian',t.endmeridian);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/DarkNight',t.darknight);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Skip',t.skip);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RA',RAToStr(t.ra));
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Dec',DEToStr(t.de));
      if t.pa=NullCoord then
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PA','-')
      else
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PA',FormatFloat(f1,t.pa));
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',t.astrometrypointing);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/UpdateCoord',t.updatecoord);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/InplaceAutofocus',t.inplaceautofocus);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',t.previewexposure);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Preview',t.preview);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',t.repeatcount);
      if Targets.IgnoreRestart then
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',0)
      else
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/RepeatDone',t.repeatdone);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Delay',t.delay);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatCount',t.FlatCount);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatBinX',t.FlatBinX);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatBinY',t.FlatBinY);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatGain',t.FlatGain);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatOffset',t.FlatOffset);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatFstop',t.FlatFstop);
      SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/FlatFilters',t.FlatFilters);
      if (t.objectname<>ScriptTxt) then begin
        if (t.objectname=SkyFlatTxt) then Targets.CreateSkyFlatPlan(t);
        p:=T_Plan(t.plan);
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Name',p.PlanName);
        SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/StepNum',p.Count);
        for j:=0 to p.Count-1 do begin
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Description',p.Steps[j].description);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/FrameType',trim(FrameName[ord(p.Steps[j].frtype)]));
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Exposure',p.Steps[j].exposure);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Binning',IntToStr(p.Steps[j].binx)+'x'+IntToStr(p.Steps[j].biny));
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Gain',p.Steps[j].gain);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Offset',p.Steps[j].offset);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Fstop',p.Steps[j].fstop);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Filter',p.Steps[j].filter_str);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Count',p.Steps[j].count);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Dither',p.Steps[j].dither);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/DitherCount',p.Steps[j].dithercount);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusStart',p.Steps[j].autofocusstart);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Autofocus',p.Steps[j].autofocus);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/AutofocusCount',p.Steps[j].autofocuscount);
          SequenceFile.Items.SetValue('/Targets/Target'+inttostr(i)+'/Plan/Steps/Step'+inttostr(j)+'/Done',p.Steps[j].donecount)
        end;
      end;
    end;
    SequenceFile.Save;
 end;
end;

procedure Tf_sequence.TargetGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
 if (TargetRow>0)and(aCol=0)and(aRow=TargetRow) then begin
    TargetGrid.Canvas.FillRect(aRect);
    TargetGrid.Canvas.Font.Style:=[fsBold];
    TargetGrid.Canvas.TextOut(aRect.Left,aRect.Top,TargetGrid.Cells[aCol,aRow]);
 end;
end;

procedure Tf_sequence.TargetGridSelection(Sender: TObject; aCol, aRow: Integer);
var p : T_plan;
begin
  if (not Running)and(arow<=Targets.Count) then begin
    p:=T_Plan(Targets.Targets[arow-1].plan);
    if p<>nil then PlanChange(p);
  end;
end;

procedure Tf_sequence.PlanGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
 if (PlanRow>0)and(aCol=0)and(aRow=PlanRow) then begin
    PlanGrid.Canvas.FillRect(aRect);
    PlanGrid.Canvas.Font.Style:=[fsBold];
    PlanGrid.Canvas.TextOut(aRect.Left,aRect.Top,PlanGrid.Cells[aCol,aRow]);
 end;
end;

function Tf_sequence.GetBusy: boolean;
begin
  result:=Targets.Busy;
end;

function Tf_sequence.GetRunning: boolean;
begin
  result:=Targets.Running;
end;

function Tf_sequence.GetTargetCoord: boolean;
begin
  result:=Targets.TargetCoord;
end;

function Tf_sequence.GetTargetRA: double;
begin
  result:=Targets.TargetRA;
end;

function Tf_sequence.GetTargetDE: double;
begin
  result:=Targets.TargetDE;
end;

function Tf_sequence.GetCurrentPlan: T_Plan;
begin
  if (Targets<>nil)and(Targets.CurrentTarget>=0) then
    result:=T_Plan(Targets.Targets[Targets.CurrentTarget].plan)
  else
    result:=nil;
end;

procedure Tf_sequence.SetEditBtn(onoff:boolean);
begin
  BtnLoadTargets.Enabled:=onoff;
  BtnNewTargets.Enabled:=onoff;
  BtnEditTargets.Enabled:=onoff;
  BtnCopy.Enabled:=onoff;
  BtnDelete.Enabled:=onoff;
end;

procedure Tf_sequence.StartTimerTimer(Sender: TObject);
begin
  StartTimer.Enabled:=false;
  if not StartingSequence then exit;
  StartSequence;
end;

procedure Tf_sequence.StartSequence;
var ccdtemp: double;
    i,j: integer;
    isCalibrationSequence, waitcooling: boolean;
    buf: string;
begin
 StartingSequence:=true;
 msg(Format(rsStartingSequ,['']),1);
 led.Brush.Color:=clYellow;
 // check mount park
 if mount.Park then begin
    msg(rsTheTelescope, 1);
 end;
 if not StartingSequence then exit;
 if Targets.CheckDoneCount then begin
    msg(targets.DoneStatus,2);
    msg(Format(rsThisSequence,['"'+CurrentSeqName+'"']),2);
    msg(rsItWillContin, 2);
 end;
 if preview.Running then begin
     msg(rsStopPreview,2);
     camera.AbortExposure;
     preview.stop;
     StartTimer.Interval:=5000;
     StartTimer.Enabled:=true;
     exit;
 end;
 // check if the sequence contain only calibration
 isCalibrationSequence:=true;
 for j:=0 to Targets.Count-1 do begin
   if Targets.Targets[j].objectname=ScriptTxt then continue;
   if Targets.Targets[j].objectname=SkyFlatTxt then continue;
   for i:=0 to T_Plan(Targets.Targets[j].plan).Count-1 do begin
      if T_Plan(Targets.Targets[j].plan).Steps[i].frtype=LIGHT then begin
         isCalibrationSequence:=false;
         break;
      end;
   end;
   if not isCalibrationSequence then break;
 end;
 if not StartingSequence then exit;
 // check camera cooler
 waitcooling:=config.GetValue('/Cooler/CameraAutoCool',false);
 ccdtemp:=config.GetValue('/Cooler/CameraAutoCoolTemp',0.0);
 if not camera.Cooler then begin
    if waitcooling then begin
       msg(Format(rsCameraNotCoo, [FormatFloat(f1, ccdtemp)]),1);
       ccdtemp:=TempCelsius(TemperatureScale,ccdtemp);
       camera.Temperature:=ccdtemp;
    end;
 end;
 if not StartingSequence then exit;
 // check if autoguider is required and connected
 if (not isCalibrationSequence)and(Autoguider.AutoguiderType<>agNONE)and(Autoguider.State=GUIDER_DISCONNECTED)and(assigned(FConnectAutoguider)) then begin
   if AutoguiderStarting then begin
     f_pause.Caption:=rsAutoguiderNo;
     f_pause.Text:=Format(rsCannotConnec, [crlf]);
     if f_pause.Wait(30) then begin
       msg(rsSequenceWill2,0);
     end
     else begin
       StopSequence;
     end;
   end
   else begin
     msg(rsTryToConnect,1);
     FConnectAutoguider(self);
     AutoguiderStarting:=true;
     StartTimer.Interval:=10000;
     StartTimer.Enabled:=true;
     exit;
   end;
 end;
 if not StartingSequence then exit;
 // initialize sequence
 if not StartingSequence then exit;
 StartingSequence:=false;
 AutoguiderStarting:=false;
 AutoguiderAlert:=false;
 MountTrackingAlert:=false;
 Preview.StackPreview.Checked:=false;
 led.Brush.Color:=clLime;
 SetEditBtn(false);
 StatusTimer.Enabled:=true;
 Targets.Unattended:=Unattended.Checked;
 Targets.Start;
 if not Targets.Running then AbortSequence;
end;

procedure Tf_sequence.StopSequence;
begin
 StartTimer.Enabled:=false;
 StatusTimer.Enabled:=false;
 if StartingSequence then begin
    led.Brush.Color:=clRed;
    msg(Format(rsSequenceStop2,['']),1);
 end;
 StartingSequence:=false;
 if targets.TargetInitializing or targets.WaitStarting or targets.ScriptRunning then begin
   led.Brush.Color:=clRed;
   SetEditBtn(true);
 end;
 if Targets.Running then Targets.Stop;
 TargetRow:=-1;
 PlanRow:=-1;
end;

procedure Tf_sequence.WeatherChange(value: boolean);
begin
  if value then begin
    // good weather, restart the sequence
    Targets.WeatherRestart;
  end
  else begin
    // bad weather, pause the sequence
    Targets.WeatherPause;
  end;
end;

procedure Tf_sequence.AbortSequence;
begin
 StatusTimer.Enabled:=false;
 Targets.Abort;
 TargetRow:=-1;
 PlanRow:=-1;
 led.Brush.Color:=clRed;
 SetEditBtn(true);
 if Unattended.Checked then mount.AbortMotion;
end;

procedure Tf_sequence.EndSequence(Sender: TObject);
begin
 FlatWaitDusk:=false;
 FlatWaitDawn:=false;
 led.Brush.Color:=clRed;
 SetEditBtn(true);
 PlanGrid.Invalidate;
 TargetGrid.Invalidate;
end;

procedure Tf_sequence.BtnStopClick(Sender: TObject);
begin
 StopSequence;
end;

procedure Tf_sequence.BtnStatusClick(Sender: TObject);
var f: Tf_viewtext;
begin
  Targets.CheckDoneCount;
  f:=Tf_viewtext.Create(self);
  f.Caption:=rsStatus2;
  f.Memo1.Text:=rsSequence+blank+Targets.TargetName+crlf+targets.DoneStatus;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.Show;
end;

procedure Tf_sequence.FrameResize(Sender: TObject);
var minw: integer;
begin
 minw:=DoScaleX(60);
 TargetGrid.DefaultColWidth:=max(minw,TargetGrid.ClientWidth div 4);
 TargetGrid.ColWidths[2]:=0;
 TargetGrid.ColWidths[3]:=0;
 TargetGrid.Invalidate;
 minw:=DoScaleX(50);
 PlanGrid.DefaultColWidth:=max(minw,PlanGrid.ClientWidth div 5);
 PlanGrid.Invalidate;
end;

procedure Tf_sequence.BtnPauseClick(Sender: TObject);
begin
 if Targets.Running then begin
  PauseSequence:=not PauseSequence;
  if PauseSequence then
     msg(rsSequenceWil2, 1)
  else
     msg(rsPauseSequenc3, 1)
 end;
end;

procedure Tf_sequence.BtnStartClick(Sender: TObject);
var buf:string;
begin
 if (AllDevicesConnected) then begin
   if (ObsTimeZone<>0) and (abs(ObsTimeZone+ObsLongitude/15)>3) then begin
     if ObsLongitude<0 then buf:=rsEast else buf:=rsWest;
     msg(Format(rsTheComputerT, [FormatFloat(f1, ObsTimeZone), FormatFloat(f1, abs(ObsLongitude))+blank+buf])+crlf+rsBeSureTheCom+crlf+rsBeCarefulOft, 0);
   end
   else if Targets.Running or Fcapture.Running then begin
     msg(rsCaptureAlrea,0);
   end
   else if Targets.Count=0 then begin
     msg(rsPleaseLoadOr,0);
   end
   else if (not DomeNoSafetyCheck) and (Fsafety<>nil) and Fsafety.Connected and (not Fsafety.Safe) then begin
      msg(rsUnsafeCondit,0);
   end
   else begin
     if Targets.IgnoreRestart then begin
       Targets.ClearDoneCount(true);
       SaveTargets(SequenceFile.Filename,'');
     end;
     Targets.TargetsRepeatCount:=SequenceFile.Items.GetValue('/Targets/RepeatDone',0);
     StartSequence;
   end;
 end
 else msg(rsSomeDefinedD,0);
end;

procedure Tf_sequence.StatusTimerTimer(Sender: TObject);
var buf1,buf2,r:string;
    i:integer;
    p: T_Plan;
    agAlerttime, trAlerttime, msgtime: integer;
const
    agAlerttimeout=5;
    trAlerttimeout=1;
begin
 try
  StatusTimer.Enabled:=false;
  TargetRow:=Targets.CurrentTarget+1;
  if Targets.Running then begin
   try
   // show plan status
   if (TargetRow>0) then begin
    buf1:=Targets.Targets[Targets.CurrentTarget].planname;
    buf2:=Title2.Caption;
    i:=pos(blank,buf2);
    delete(buf2,1,i);
    buf2:=trim(buf2);
    p:=T_Plan(Targets.Targets[Targets.CurrentTarget].plan);
    if p=nil then exit;
    if buf1<>buf2 then begin
      PlanChange(p);
    end;
    PlanRow:=p.CurrentStep+1;
    TargetGrid.TopRow:=TargetRow;
    PlanGrid.TopRow:=PlanRow;
    TargetGrid.Invalidate;
    PlanGrid.Invalidate;
    Application.ProcessMessages;
   end;
   // process autoguider problem during sequence
   if AutoguiderAlert then begin
    if (Autoguider<>nil)and(Autoguider.State=GUIDER_GUIDING) then begin
      // autoguiding restarted, clear alert
      AutoguiderAlert:=false;
      msg(rsAutoguidingR,1);
    end
    else begin
      agAlerttime:=trunc((now-AutoguiderAlertTime)*minperday);
      msgtime:=trunc((now-AutoguiderMsgTime)*minperday);
      if agAlerttime>=agAlerttimeout then begin
        // timeout
        if (Autoguider=nil)or(Autoguider.State=GUIDER_DISCONNECTED) then begin
           // no more autoguider, stop sequence
           msg(Format(rsSequenceAbor2, [IntToStr(agAlerttime)]),0);
           AbortSequence;
           AutoguiderAlert:=false;
        end
        else begin
           // autoguider connected but not guiding, try next target
           msg(Format(rsAutoguiderWa, [IntToStr(agAlerttime)]),1);
           msg(rsTryNextTarge,1);
           if EmailAutoguider then begin
             r:=email(rsAutoguiderEr, Format(rsAutoguiderWa, [IntToStr(agAlerttime)])+crlf+rsTryNextTarge);
             if r='' then r:=rsEmailSentSuc;
             msg(r,9);
           end;
           Targets.ForceNextTarget;
           AutoguiderAlert:=false;
        end;
      end
      else begin
       // continue to wait for a restart
       if msgtime>=1 then begin
         // display a message every minute
         AutoguiderMsgTime:=now;
         msg(Format(rsAutoguidingS, [IntToStr(agAlerttime), IntToStr(agAlerttimeout-agAlerttime)]),1);
       end;
      end;
    end;
   end;
   // process mount tracking problem during sequence
   if MountTrackingAlert then begin
    trAlerttime:=trunc((now-MountTrackingAlertTime)*minperday);
    if trAlerttime>=trAlerttimeout then begin
       // timeout, mount still not tracking, try next target
       msg(Format(rsMountWasStil, [IntToStr(trAlerttime)]), 1);
       msg(rsTryNextTarge,1);
       Targets.ForceNextTarget;
       MountTrackingAlert:=false;
    end
    else begin
      // try to restart tracking
      mount.Track;
    end;
   end;
   finally
     StatusTimer.Enabled:=true;
   end;
  end
  else StopSequence;
except
  AbortSequence;
end;
end;


procedure Tf_sequence.AutoguiderDisconnected;
var r: string;
begin
  if Autoguider.AutoguiderType=agNONE then exit;
  if Targets.Running and
    (Targets.CurrentTarget>=0) and
    T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running and
    (Targets.Targets[Targets.CurrentTarget].autoguiding)
    then begin
    if not AutoguiderAlert then begin
     AutoguiderAlert:=true;
     AutoguiderAlertTime:=now;
     AutoguiderMsgTime:=0;
     if EmailAutoguider then begin
       r:=email(rsAutoguiderEr, rsAutoguiderSt+blank+rsDisconnected3);
       if r='' then r:=rsEmailSentSuc;
       msg(r,9);
     end;
    end;
  end;
end;

procedure Tf_sequence.AutoguiderIddle;
var r: string;
begin
  if Autoguider.AutoguiderType=agNONE then exit;
  if Targets.Running and
    (Targets.CurrentTarget>=0) and
    T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running and
    (Targets.Targets[Targets.CurrentTarget].autoguiding) and
    (not Fautoguider.Recovering)
    then begin
    if not AutoguiderAlert then begin
      AutoguiderAlert:=true;
      AutoguiderAlertTime:=now;
      AutoguiderMsgTime:=0;
      if EmailAutoguider then begin
        r:=email(rsAutoguiderEr, rsAutoguiderSt+blank+rsIdle);
        if r='' then r:=rsEmailSentSuc;
        msg(r,9);
      end;
    end;
  end;
end;

procedure Tf_sequence.MountTrackingStarted;
begin
  if MountTrackingAlert then begin
    MountTrackingAlert:=false;
    msg(rsMountTrackin, 1);
    // maybe we can recenter the target here
    // but as this function is mainly to handle mount limits we not do it for now
    // for very short interruption the autoguider may handle the recenter better
  end;
end;

procedure Tf_sequence.MountTrackingStopped;
begin
  if Targets.Running and
    (Targets.CurrentTarget>=0) and
    (Targets.Targets[Targets.CurrentTarget].objectname<>SkyFlatTxt) and
    (Targets.Targets[Targets.CurrentTarget].objectname<>ScriptTxt) and
    (T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running) and
    (T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Steps[T_Plan(Targets.Targets[Targets.CurrentTarget].plan).CurrentStep].frtype=LIGHT)
    then begin
       MountTrackingAlert:=true;
       MountTrackingAlertTime:=now;
       msg(Format(rsMountTrackin2, ['1']), 1);
    end;
end;

procedure Tf_sequence.ExposureAborted;
begin
  if Targets.Running and T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running then begin
    AbortSequence;
  end;
end;

procedure Tf_sequence.CameraDisconnected;
begin
 if Targets.Running then AbortSequence;
end;

function Tf_sequence.GetEndShutdown: boolean;
begin
   result:=Targets.AtEndShutdown;
end;

procedure Tf_sequence.SetEndShutdown(value:boolean);
begin
  Targets.AtEndShutdown:=value;
end;

function Tf_sequence.GetOnShutdown: TNotifyEvent;
begin
   result:=Targets.OnShutdown;
end;

procedure Tf_sequence.SetOnShutdown(value:TNotifyEvent);
begin
  Targets.OnShutdown:=value;
end;

function Tf_sequence.GetPercentComplete: double;
begin
{ TODO : Improve to take account for execution time }
try
  if Targets.Running and (Targets.CurrentTarget>=0) then
     result := (Targets.CurrentTarget)/Targets.Count
  else
     result:=0;
  except
    result:=0;
  end;
end;

function Tf_sequence.GetTargetPercentComplete: double;
begin
{ TODO : Improve to take account for execution time }
try
  if Targets.Running and (T_Plan(Targets.Targets[Targets.CurrentTarget].plan).CurrentStep>=0) then
     result := (T_Plan(Targets.Targets[Targets.CurrentTarget].plan).CurrentStep)/ T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Count
  else
     result:=0;
except
  result:=0;
end;
end;

end.


