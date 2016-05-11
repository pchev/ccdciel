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

uses pu_editplan, pu_edittargets, u_ccdconfig, u_global, u_utils,
  fu_capture, fu_preview, fu_filterwheel,
  cu_mount, cu_camera, cu_autoguider, cu_astrometry,
  cu_targets, cu_plan,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { Tf_sequence }

  Tf_sequence = class(TFrame)
    BtnEditTargets: TButton;
    BtnStart: TButton;
    BtnStop: TButton;
    BtnNewTargets: TButton;
    BtnSaveTargets: TButton;
    BtnLoadTargets: TButton;
    StatusTimer: TTimer;
    Unattended: TCheckBox;
    DelayMsg: TLabel;
    StatusMsg: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog1: TSaveDialog;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    TargetGrid: TStringGrid;
    PlanGrid: TStringGrid;
    procedure BtnEditTargetsClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnLoadTargetsClick(Sender: TObject);
    procedure BtnSaveTargetsClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure PlanGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StatusTimerTimer(Sender: TObject);
    procedure TargetGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TargetsChange(Sender: TObject);
    procedure PlanChange(Sender: TObject);
  private
    { private declarations }
    TargetRow, PlanRow: integer;
    Targets: T_Targets;
    FonMsg: TNotifyMsg;
    Fcapture: Tf_capture;
    Fpreview: Tf_preview;
    Ffilter: Tf_filterwheel;
    Fmount: T_mount;
    Fcamera: T_camera;
    Fautoguider: T_autoguider;
    Fastrometry: TAstrometry;
    procedure SetPreview(val: Tf_preview);
    procedure SetCapture(val: Tf_capture);
    procedure SetMount(val: T_mount);
    procedure SetCamera(val: T_camera);
    procedure SetFilter(val: Tf_filterwheel);
    procedure SetAutoguider(val: T_autoguider);
    procedure SetAstrometry(val: TAstrometry);
    function GetRunning: boolean;
    procedure StartSequence;
    procedure StopSequence;
    procedure ClearTargetGrid;
    procedure ClearPlanGrid;
    procedure LoadPlan(p: T_Plan; plan:string);
    procedure msg(txt:string);
    procedure ShowDelayMsg(txt:string);
  public
    { public declarations }
    CurrentName, CurrentTarget, CurrentFile, CurrentStep: string;
    StepRepeatCount, StepTotalCount: integer;

    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure AutoguiderDisconnected;
    procedure AutoguiderIddle;
    procedure ExposureAborted;
    procedure CameraDisconnected;
    procedure LoadTargets(fn: string);
    property Running: boolean read GetRunning;
    property Preview: Tf_preview read Fpreview write SetPreview;
    property Capture: Tf_capture read Fcapture write SetCapture;
    property Mount: T_mount read Fmount write SetMount;
    property Camera: T_camera read Fcamera write SetCamera;
    property Filter: Tf_filterwheel read Ffilter write SetFilter;
    property Autoguider: T_autoguider read Fautoguider write SetAutoguider;
    property Astrometry: TAstrometry read Fastrometry write SetAstrometry;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

var
  f_sequence: Tf_sequence;

implementation

{$R *.lfm}

constructor Tf_sequence.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 Targets:=T_Targets.Create;
 Targets.Preview:=Fpreview;
 Targets.Capture:=Fcapture;
 Targets.Mount:=Fmount;
 Targets.Camera:=Fcamera;
 Targets.Filter:=Ffilter;
 Targets.Autoguider:=Fautoguider;
 Targets.Astrometry:=Fastrometry;
 Targets.onMsg:=@Msg;
 Targets.DelayMsg:=@ShowDelayMsg;
 Targets.onTargetsChange:=@TargetsChange;
 Targets.onPlanChange:=@PlanChange;
 TargetGrid.Cells[0,0]:='Object';
 TargetGrid.Cells[1,0]:='Plan';
 TargetGrid.Cells[2,0]:='Start';
 TargetGrid.Cells[3,0]:='End';
 TargetGrid.ColWidths[2]:=0;
 TargetGrid.ColWidths[3]:=0;
 TargetGrid.Cells[4,0]:='RA';
 TargetGrid.Cells[5,0]:='DEC';
 PlanGrid.Cells[0,0]:='Desc.';
 PlanGrid.Cells[1,0]:='Exp.';
 PlanGrid.Cells[2,0]:='Count';
 PlanGrid.Cells[3,0]:='Repeat';
 PlanGrid.Cells[4,0]:='Type';
 PlanGrid.Cells[5,0]:='Filter';
end;

destructor  Tf_sequence.Destroy;
begin
 Targets.Free;
 ClearTargetGrid;
 ClearPlanGrid;
 inherited Destroy;
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

procedure Tf_sequence.SetFilter(val: Tf_filterwheel);
begin
  Ffilter:=val;
  Targets.Filter:=Ffilter;
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
end;

procedure Tf_sequence.ClearTargetGrid;
begin
  TargetGrid.RowCount:=1;
end;

procedure Tf_sequence.ClearPlanGrid;
begin
  PlanGrid.RowCount:=1;
end;

procedure Tf_sequence.msg(txt:string);
begin
  StatusMsg.Caption:=txt;
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_sequence.ShowDelayMsg(txt:string);
begin
  DelayMsg.Caption:=txt;
end;

procedure Tf_sequence.BtnEditTargetsClick(Sender: TObject);
var i,n:integer;
    t:TTarget;
begin
   if (Sender=BtnEditTargets)and(Targets.Count>0) then begin
     f_EditTargets.TargetList.RowCount:=Targets.Count+1;
     for i:=1 to Targets.Count do begin
       t:=TTarget.Create;
       t.Assign(Targets.Targets[i-1]);
       f_EditTargets.TargetList.Cells[0,i]:=IntToStr(i);
       f_EditTargets.TargetList.Cells[1,i]:=t.objectname;
       f_EditTargets.TargetList.Cells[2,i]:=t.planname;
       f_EditTargets.TargetList.Objects[0,i]:=t;
     end;
   end else begin
     t:=TTarget.Create;
     f_EditTargets.TargetList.RowCount:=2;
     f_EditTargets.TargetList.Cells[0,1]:='1';
     f_EditTargets.TargetList.Cells[1,1]:=t.objectname;
     f_EditTargets.TargetList.Cells[2,1]:=t.planname;
     f_EditTargets.TargetList.Objects[0,1]:=t;
   end;
   FormPos(f_EditTargets,mouse.CursorPos.X,mouse.CursorPos.Y);
   f_EditTargets.ShowModal;
   n:=f_EditTargets.TargetList.RowCount;
   Targets.Clear;
   for i:=1 to n-1 do begin
     t:=TTarget(f_EditTargets.TargetList.Objects[0,i]);
     Targets.Add(t);
     LoadPlan(T_Plan(t.plan), t.planname);
   end;
end;

procedure Tf_sequence.LoadTargets(fn: string);
var tfile: TCCDconfig;
    t:TTarget;
    x:string;
    i,n: integer;
begin
   tfile:=TCCDconfig.Create(self);
   tfile.Filename:=fn;
   CurrentName:=ExtractFileNameOnly(fn);
   Targets.TargetName:=CurrentName;
   CurrentFile:=fn;
   n:=tfile.GetValue('/TargetNum',0);
   if n>0 then begin
     Targets.Clear;
     for i:=1 to n do begin
       t:=TTarget.Create;
       t.objectname:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/ObjectName','');
       t.planname:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Plan','');
       t.starttime:=StrToTime(tfile.GetValue('/Targets/Target'+inttostr(i)+'/StartTime',''));
       t.endtime:=StrToTime(tfile.GetValue('/Targets/Target'+inttostr(i)+'/EndTime',''));
       x:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/RA','');
       if x='-' then
         t.ra:=NullCoord
       else
         t.ra:=StrToAR(x);
       x:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Dec','');
       if x='-' then
         t.de:=NullCoord
       else
         t.de:=StrToDE(x);
       t.astrometrypointing:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',false);
       t.previewexposure:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',1.0);
       t.preview:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Preview',false);
       t.repeatcount:=trunc(tfile.GetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',1));
       t.delay:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Delay',1.0);
       Targets.Add(t);
       LoadPlan(T_Plan(t.plan), t.planname);
     end;
   end;
end;

procedure Tf_sequence.LoadPlan(p: T_plan; plan:string);
var fn: string;
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
     for i:=1 to n do begin
       s:=TStep.Create;
       f_EditPlan.ReadStep(pfile,i,s);
       p.Add(s);
     end;
  end;
end;

procedure Tf_sequence.TargetsChange(Sender: TObject);
var i: integer;
begin
   ClearTargetGrid;
   TargetGrid.RowCount:=Targets.count+1;
   StaticText3.Caption:='Targets: '+Targets.TargetName;
   for i:=1 to Targets.count do begin
     TargetGrid.Cells[0,i]:=Targets.Targets[i-1].objectname;
     TargetGrid.Cells[1,i]:=Targets.Targets[i-1].planname;
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
   p:=T_Plan(sender);
   ClearPlanGrid;
   StaticText2.Caption:='Plan: '+p.PlanName;
   PlanGrid.RowCount:=p.count+1;
   for i:=1 to p.count do begin
     PlanGrid.Cells[0,i]:=T_Plan(sender).Steps[i-1].description_str;
     PlanGrid.Cells[1,i]:=T_Plan(sender).Steps[i-1].exposure_str;
     PlanGrid.Cells[2,i]:=T_Plan(sender).Steps[i-1].count_str;
     PlanGrid.Cells[3,i]:=T_Plan(sender).Steps[i-1].repeatcount_str;
     PlanGrid.Cells[4,i]:=T_Plan(sender).Steps[i-1].frtype_str;
     PlanGrid.Cells[5,i]:=T_Plan(sender).Steps[i-1].filter_str;
   end;
end;

procedure Tf_sequence.BtnLoadTargetsClick(Sender: TObject);
begin
 OpenDialog1.InitialDir:=ConfigDir;
 OpenDialog1.FileName:='*.targets';
 if OpenDialog1.Execute then begin
   LoadTargets(OpenDialog1.FileName);
 end;
end;

procedure Tf_sequence.BtnSaveTargetsClick(Sender: TObject);
var tfile: TCCDconfig;
    t:TTarget;
    i: integer;
begin
 if TargetGrid.RowCount>1 then begin
  SaveDialog1.InitialDir:=ConfigDir;
  if SaveDialog1.Execute then begin
    tfile:=TCCDconfig.Create(self);
    tfile.Filename:=SaveDialog1.FileName;
    tfile.Clear;
    CurrentName:=ExtractFileNameOnly(SaveDialog1.FileName);
    Targets.TargetName:=CurrentName;
    CurrentFile:=SaveDialog1.FileName;
    tfile.SetValue('/ListName',CurrentName);
    tfile.SetValue('/TargetNum',Targets.Count);
    for i:=1 to Targets.Count do begin
      t:=Targets.Targets[i-1];
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/ObjectName',t.objectname);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Plan',t.planname);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/StartTime',TimetoStr(t.starttime));
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/EndTime',TimetoStr(t.endtime));
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/RA',RAToStr(t.ra));
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Dec',DEToStr(t.de));
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/AstrometryPointing',t.astrometrypointing);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',t.previewexposure);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Preview',t.preview);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',t.repeatcount);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Delay',t.delay);
    end;
    tfile.Flush;
    tfile.Free;
  end;
 end;
end;

procedure Tf_sequence.TargetGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
 if (TargetRow>0)and(aCol=0)and(aRow=TargetRow) then begin
    TargetGrid.Canvas.Brush.Color := clNavy;
    TargetGrid.Canvas.Font.Color := clWhite;
    TargetGrid.Canvas.FillRect(aRect);
    TargetGrid.Canvas.TextOut(aRect.Left,aRect.Top,TargetGrid.Cells[aCol,aRow]);
 end;
end;

procedure Tf_sequence.PlanGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
 if (PlanRow>0)and(aCol=0)and(aRow=PlanRow) then begin
    PlanGrid.Canvas.Brush.Color := clNavy;
    PlanGrid.Canvas.Font.Color := clWhite;
    PlanGrid.Canvas.FillRect(aRect);
    PlanGrid.Canvas.TextOut(aRect.Left,aRect.Top,PlanGrid.Cells[aCol,aRow]);
 end;
end;

function Tf_sequence.GetRunning: boolean;
begin
  result:=Targets.Running;
end;

procedure Tf_sequence.StartSequence;
begin
 StatusTimer.Enabled:=true;
 Targets.Unattended:=Unattended.Checked;
 Targets.Start;
end;

procedure Tf_sequence.StopSequence;
begin
 StatusTimer.Enabled:=false;
 Targets.Stop;
 TargetRow:=-1;
 PlanRow:=-1;
end;

procedure Tf_sequence.BtnStopClick(Sender: TObject);
begin
 StopSequence;
end;

procedure Tf_sequence.BtnStartClick(Sender: TObject);
begin
 if (Fcamera.Status=devConnected) then begin
   if Targets.Running or Fcapture.Running then begin
     msg('Capture already running! please stop it first if you want to start a new sequence.');
   end
   else if Targets.Count=0 then begin
     msg('Please load or create a target list first.');
   end
   else begin
     StartSequence;
   end;
 end
 else msg('Camera is not connected');
end;

procedure Tf_sequence.StatusTimerTimer(Sender: TObject);
begin
  TargetRow:=Targets.CurrentTarget+1;
  if TargetRow>0 then begin
    PlanRow:=T_Plan(Targets.Targets[Targets.CurrentTarget].plan).CurrentStep+1;
    TargetGrid.Invalidate;
    PlanGrid.Invalidate;
  end
  else PlanRow:=-1;
end;


procedure Tf_sequence.AutoguiderDisconnected;
begin
  if Targets.Running then  StopSequence;
end;

procedure Tf_sequence.AutoguiderIddle;
begin
  if Targets.Running and T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running then begin
    msg('Autoguiding stopped unexpectedly!');
    StopSequence;
  end;
end;

procedure Tf_sequence.ExposureAborted;
begin
  if Targets.Running and T_Plan(Targets.Targets[Targets.CurrentTarget].plan).Running then begin
    StopSequence;
  end;
end;

procedure Tf_sequence.CameraDisconnected;
begin
 if Targets.Running then StopSequence;
end;


end.


