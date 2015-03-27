unit fu_sequence;

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

{$mode objfpc}{$H+}

interface

uses pu_editplan, pu_edittargets, u_ccdconfig, u_global, u_utils,
  fu_capture, fu_preview, fu_filterwheel,
  cu_mount, cu_camera, cu_autoguider,
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
    TargetTimer: TTimer;
    PlanTimer: TTimer;
    StepRepeatTimer: TTimer;
    TargetRepeatTimer: TTimer;
    procedure BtnEditTargetsClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnLoadTargetsClick(Sender: TObject);
    procedure BtnSaveTargetsClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure PlanGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PlanTimerTimer(Sender: TObject);
    procedure StepRepeatTimerTimer(Sender: TObject);
    procedure TargetGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TargetRepeatTimerTimer(Sender: TObject);
    procedure TargetTimerTimer(Sender: TObject);
  private
    { private declarations }
    FRunning, PlanRunning, StepRunning: boolean;
    TargetRow, PlanRow: integer;
    StepTimeStart,StepDelayEnd: TDateTime;
    TargetTimeStart,TargetDelayEnd: TDateTime;
    FonMsg: TNotifyMsg;
    Fcapture: Tf_capture;
    Fpreview: Tf_preview;
    Ffilter: Tf_filterwheel;
    Fmount: T_mount;
    Fcamera: T_camera;
    Fautoguider: T_autoguider;
    procedure InitTarget;
    procedure StartPlan;
    procedure StartStep;
    procedure ClearTargetGrid;
    procedure ClearPlanGrid;
    procedure LoadPlan(plan:string);
    procedure msg(txt:string);
  public
    { public declarations }
    CurrentName, CurrentFile, CurrentStep: string;
    StepRepeatCount, StepTotalCount: integer;
    TargetRepeatCount, TargetTotalCount: integer;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadTargets(fn: string);
    property Running: boolean read FRunning;
    property Preview: Tf_preview read Fpreview write Fpreview;
    property Capture: Tf_capture read Fcapture write Fcapture;
    property Filter: Tf_filterwheel read Ffilter write Ffilter;
    property Mount: T_mount read Fmount write Fmount;
    property Camera: T_camera read Fcamera write Fcamera;
    property Autoguider: T_autoguider read Fautoguider write Fautoguider;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
  end;

var
  f_sequence: Tf_sequence;

implementation

{$R *.lfm}

constructor Tf_sequence.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 TargetGrid.Cells[0,0]:='Object';
 TargetGrid.Cells[1,0]:='Plan';
 TargetGrid.Cells[2,0]:='Start';
 TargetGrid.Cells[3,0]:='End';
 TargetGrid.Cells[4,0]:='RA';
 TargetGrid.Cells[5,0]:='DEC';
 PlanGrid.Cells[0,0]:='Desc.';
 PlanGrid.Cells[1,0]:='Exp.';
 PlanGrid.Cells[2,0]:='Count';
 PlanGrid.Cells[3,0]:='Repeat';
 PlanGrid.Cells[4,0]:='Type';
 PlanGrid.Cells[5,0]:='Filter';
 Frunning:=false;
end;

destructor  Tf_sequence.Destroy;
begin
 ClearTargetGrid;
 ClearPlanGrid;
 inherited Destroy;
end;

procedure Tf_sequence.ClearTargetGrid;
var i:integer;
begin
 for i:=1 to TargetGrid.RowCount-1 do begin
  if TargetGrid.Objects[0,i]<>nil then TargetGrid.Objects[0,i].Free;
  TargetGrid.Objects[0,i]:=nil;
end;
  TargetGrid.RowCount:=1;
end;

procedure Tf_sequence.ClearPlanGrid;
var i:integer;
begin
   for i:=1 to PlanGrid.RowCount-1 do begin
    if PlanGrid.Objects[0,i]<>nil then PlanGrid.Objects[0,i].Free;
    PlanGrid.Objects[0,i]:=nil;
  end;
  PlanGrid.RowCount:=1;
end;

procedure Tf_sequence.msg(txt:string);
begin
  StatusMsg.Caption:=txt;
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure Tf_sequence.BtnEditTargetsClick(Sender: TObject);
var i:integer;
    t:TTarget;
begin
   f_EditTargets.TargetList.RowCount:=TargetGrid.RowCount;
   if (Sender=BtnEditTargets)and(TargetGrid.RowCount>1) then begin
     for i:=1 to TargetGrid.RowCount-1 do begin
       t:=TTarget(TargetGrid.Objects[0,i]);
       f_EditTargets.TargetList.Cells[0,i]:=IntToStr(i);
       f_EditTargets.TargetList.Cells[1,i]:=t.objectname;
       f_EditTargets.TargetList.Objects[0,i]:=t;
     end;
   end;
   FormPos(f_EditTargets,mouse.CursorPos.X,mouse.CursorPos.Y);
   f_EditTargets.ShowModal;
   TargetGrid.RowCount:=f_EditTargets.TargetList.RowCount;
   for i:=1 to f_EditTargets.TargetList.RowCount-1 do begin
     t:=TTarget(f_EditTargets.TargetList.Objects[0,i]);
     TargetGrid.Objects[0,i]:=t;
     with t do begin
       TargetGrid.Cells[0,i]:=objectname;
       TargetGrid.Cells[1,i]:=plan;
       TargetGrid.Cells[2,i]:=FormatDateTime('hh:nn:ss',starttime);
       TargetGrid.Cells[3,i]:=FormatDateTime('hh:nn:ss',endtime);;
       if ra=NullCoord then
         TargetGrid.Cells[4,i]:='-'
       else
         TargetGrid.Cells[4,i]:=RAToStr(ra);
       if de=NullCoord then
         TargetGrid.Cells[5,i]:='-'
       else
         TargetGrid.Cells[5,i]:=DEToStr(de);
     end;
   end;
   LoadPlan(TargetGrid.Cells[1,1]);
end;

procedure Tf_sequence.LoadTargets(fn: string);
var tfile: TCCDconfig;
    t:TTarget;
    i,n: integer;
begin
   tfile:=TCCDconfig.Create(self);
   tfile.Filename:=fn;
   CurrentName:=ExtractFileNameOnly(fn);
   StaticText3.Caption:='Targets: '+CurrentName;
   CurrentFile:=fn;
   n:=tfile.GetValue('/TargetNum',0);
   if n>0 then begin
     ClearTargetGrid;
     TargetGrid.RowCount:=n+1;
     for i:=1 to n do begin
       t:=TTarget.Create;
       TargetGrid.Cells[0,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/ObjectName','');
       TargetGrid.Cells[1,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Plan','');
       TargetGrid.Cells[2,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/StartTime','');
       TargetGrid.Cells[3,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/EndTime','');
       TargetGrid.Cells[4,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/RA','');
       TargetGrid.Cells[5,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Dec','');
       t.objectname:=TargetGrid.Cells[0,i];
       t.plan:=TargetGrid.Cells[1,i];
       t.starttime:=StrToTime(TargetGrid.Cells[2,i]);
       t.endtime:=StrToTime(TargetGrid.Cells[3,i]);
       if TargetGrid.Cells[4,i]='-' then
         t.ra:=NullCoord
       else
         t.ra:=StrToAR(TargetGrid.Cells[4,i]);
       if TargetGrid.Cells[5,i]='-' then
         t.de:=NullCoord
       else
         t.de:=StrToDE(TargetGrid.Cells[5,i]);
       t.previewexposure:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/PreviewExposure',1.0);
       t.preview:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Preview',false);
       t.repeatcount:=trunc(tfile.GetValue('/Targets/Target'+inttostr(i)+'/RepeatCount',1));
       t.delay:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Delay',1.0);
       TargetGrid.Objects[0,i]:=t;
     end;
     LoadPlan(TargetGrid.Cells[1,1]);
   end;
end;

procedure Tf_sequence.LoadPlan(plan:string);
var fn: string;
    i,n:integer;
    pfile: TCCDconfig;
    p: TPlan;
begin
  fn:=slash(ConfigDir)+plan+'.plan';
  if FileExistsUTF8(fn) then begin
     StaticText2.Caption:='Plan: '+plan;
     pfile:=TCCDconfig.Create(self);
     pfile.Filename:=fn;
     n:=pfile.GetValue('/StepNum',0);
     ClearPlanGrid;
     PlanGrid.RowCount:=n+1;
     for i:=1 to n do begin
       p:=TPlan.Create;
       f_EditPlan.ReadStep(pfile,i,p);
       PlanGrid.Objects[0,i]:=p;
       PlanGrid.Cells[0,i]:=p.description_str;
       PlanGrid.Cells[1,i]:=p.exposure_str;
       PlanGrid.Cells[2,i]:=p.count_str;
       PlanGrid.Cells[3,i]:=p.repeatcount_str;
       PlanGrid.Cells[4,i]:=p.frtype_str;
       PlanGrid.Cells[5,i]:=p.filter_str;
     end;
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
    StaticText3.Caption:='Targets: '+CurrentName;
    CurrentFile:=SaveDialog1.FileName;
    tfile.SetValue('/ListName',CurrentName);
    tfile.SetValue('/TargetNum',TargetGrid.RowCount-1);
    for i:=1 to TargetGrid.RowCount-1 do begin
      t:=TTarget(TargetGrid.Objects[0,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/ObjectName',TargetGrid.Cells[0,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Plan',TargetGrid.Cells[1,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/StartTime',TargetGrid.Cells[2,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/EndTime',TargetGrid.Cells[3,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/RA',TargetGrid.Cells[4,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Dec',TargetGrid.Cells[5,i]);
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

procedure Tf_sequence.BtnStopClick(Sender: TObject);
begin
 if FRunning and PlanRunning then begin
   FRunning:=false;
   PlanRunning:=false;
   if StepRepeatTimer.Enabled and Preview.Running then Preview.BtnLoop.Click;
   StepRepeatTimer.Enabled:=false;
   if Capture.Running then Capture.BtnStart.Click;
 end
 else msg('Cannot stop now.');
end;

procedure Tf_sequence.BtnStartClick(Sender: TObject);
begin
 if (Fcamera.Status=devConnected) then begin
   if Fcapture.Running then begin
     msg('Capture already running! please stop it first if you want to start a new sequence.');
   end
   else if TargetGrid.RowCount<2 then begin
     msg('Please load or create a target list first.');
   end
   else begin
     TargetRow:=1;
     TargetRepeatCount:=1;
     FRunning:=true;
     PlanRunning:=false;
     StepRunning:=false;
     msg('Starting sequence '+CurrentName);
     TargetGrid.Row:=TargetRow;
     TargetGrid.Invalidate;
     InitTarget;
     StartPlan;
     TargetTimer.Enabled:=true;
   end;
 end
 else msg('Camera is not connected');
end;

procedure Tf_sequence.InitTarget;
var t: TTarget;
begin
  t:=TTarget(TargetGrid.Objects[0,TargetRow]);
  if t<>nil then begin
    msg('Initialize target '+t.objectname);
    { TODO :  }
    // check if current time in range
    // slew to coordinates
    if (t.ra<>NullCoord)and(t.de<>NullCoord)and(Mount<>nil)and(Mount.Status=devConnected) then begin
       Mount.Slew(t.ra, t.de);
       // astrometry
    end;
    // start guiding
    // etc...
  end;
end;

procedure Tf_sequence.TargetTimerTimer(Sender: TObject);
var tt: double;
    t: TTarget;
begin
 if FRunning then begin
  t:=TTarget(TargetGrid.Objects[0,TargetRow]);
  if not TargetRepeatTimer.Enabled then begin
  if not PlanRunning then begin
    inc(TargetRepeatCount);
    if (t<>nil)and(TargetRepeatCount<=t.repeatcount) then begin
       tt:=t.delay-(Now-TargetTimeStart)*secperday;
       if tt<0.1 then tt:=0.1;
       msg('Wait '+FormatFloat(f1,tt)+' seconds before repeated target '+IntToStr(TargetRepeatCount));
       TargetRepeatTimer.Interval:=trunc(1000*tt);
       TargetRepeatTimer.Enabled:=true;
       TargetDelayEnd:=now+tt/secperday;
       if t.preview and (tt>5)and(tt>(2*t.previewexposure)) then begin
         if t.previewexposure>0 then Preview.ExpTime.Text:=t.previewexposure_str;
         Preview.Binning.Text:=Capture.Binning.Text;
         Preview.BtnLoop.Click;
       end;
    end
    else begin
     inc(TargetRow);
     if TargetRow<TargetGrid.RowCount then begin
        TargetGrid.Row:=TargetRow;
        TargetGrid.Invalidate;
        InitTarget;
        StartPlan;
     end
     else begin
        FRunning:=false;
        TargetTimer.Enabled:=false;
        TargetRow:=0;
        msg('Sequence '+CurrentName+' terminated.');
        TargetGrid.Invalidate;
     end;
    end;
  end;

     end
   else begin
     tt:=(TargetDelayEnd-Now)*secperday;
     DelayMsg.Caption:='Continue in '+FormatFloat(f0,tt)+' seconds';
   end;


 end
 else begin
  TargetTimer.Enabled:=false;
  TargetRow:=0;
  msg('Sequence '+CurrentName+' stopped.');
  TargetGrid.Invalidate;
 end;
end;

procedure Tf_sequence.TargetRepeatTimerTimer(Sender: TObject);
var t: TTarget;
begin
 if FRunning then begin
    TargetRepeatTimer.Enabled:=false;
    DelayMsg.Caption:='';
    t:=TTarget(TargetGrid.Objects[0,TargetRow]);
    if t<>nil then begin
      msg('Repeat '+inttostr(TargetRepeatCount)+'/'+t.repeatcount_str+' '+t.objectname);
      if t.preview and Preview.Running then Preview.BtnLoop.Click;
      TargetTimeStart:=now;
      StartPlan;
    end
    else FRunning:=false;
 end;
end;


procedure Tf_sequence.StartPlan;
var t: TTarget;
begin
  t:=TTarget(TargetGrid.Objects[0,TargetRow]);
  if t<>nil then begin
    if PlanRunning then exit;
    PlanRunning:=true;
    msg('Start plan '+t.plan);
    LoadPlan(t.plan);
    PlanRow:=1;
    PlanGrid.Row:=PlanRow;
    PlanGrid.Invalidate;
    StartStep;
    PlanTimer.Enabled:=true;
  end;
end;

procedure Tf_sequence.PlanTimerTimer(Sender: TObject);
var tt: double;
    str: string;
    p: TPlan;
    t: TTarget;
begin
 if PlanRunning then begin
   p:=TPlan(PlanGrid.Objects[0,PlanRow]);
   if not StepRepeatTimer.Enabled then begin
     StepRunning:=Capture.Running;
     if not StepRunning then begin
       inc(StepRepeatCount);
       if (p<>nil)and(StepRepeatCount<=p.repeatcount) then begin
          tt:=p.delay-(Now-StepTimeStart)*secperday;
          if tt<0.1 then tt:=0.1;
          msg('Wait '+FormatFloat(f1,tt)+' seconds before repeated sequence '+IntToStr(StepRepeatCount));
          StepRepeatTimer.Interval:=trunc(1000*tt);
          StepRepeatTimer.Enabled:=true;
          StepDelayEnd:=now+tt/secperday;
          if p.preview and (tt>5)and(tt>(2*p.previewexposure)) then begin
            if p.previewexposure>0 then Preview.ExpTime.Text:=p.previewexposure_str;
            Preview.Binning.Text:=p.binning_str;
            Preview.BtnLoop.Click;
          end;
       end
       else begin
         inc(PlanRow);
         if PlanRow<PlanGrid.RowCount then begin
           PlanGrid.Row:=PlanRow;
           PlanGrid.Invalidate;
           StartStep;
         end
         else begin
           PlanRunning:=false;
           PlanTimer.Enabled:=false;
           PlanRow:=0;
           t:=TTarget(TargetGrid.Objects[0,TargetRow]);
           if t<> nil then str:=t.plan else str:='';
           msg('Plan '+str+' terminated.');
           PlanGrid.Invalidate;
         end;
       end;
     end;
   end
   else begin
     tt:=(StepDelayEnd-Now)*secperday;
     DelayMsg.Caption:='Continue in '+FormatFloat(f0,tt)+' seconds';
   end;
 end
 else begin
    PlanTimer.Enabled:=false;
    StepRepeatTimer.Enabled:=false;
    PlanRow:=0;
    t:=TTarget(TargetGrid.Objects[0,TargetRow]);
    if t<> nil then str:=t.plan else str:='';
    msg('Plan '+str+' stopped.');
    DelayMsg.Caption:='';
    PlanGrid.Invalidate;
 end;
end;

procedure Tf_sequence.StepRepeatTimerTimer(Sender: TObject);
var p: TPlan;
begin
 if FRunning then begin
    StepRunning:=true;
    StepRepeatTimer.Enabled:=false;
    DelayMsg.Caption:='';
    p:=TPlan(PlanGrid.Objects[0,PlanRow]);
    if p<>nil then begin
      msg('Repeat '+inttostr(StepRepeatCount)+'/'+p.repeatcount_str+' '+p.description_str);
      if p.preview and Preview.Running then Preview.BtnLoop.Click;
      StepTimeStart:=now;
      Fcapture.BtnStart.Click;
    end
    else PlanRunning:=false;
 end;
end;

procedure Tf_sequence.StartStep;
var p: TPlan;
    t: TTarget;
begin
  StepRunning:=true;
  StepRepeatCount:=1;
  p:=TPlan(PlanGrid.Objects[0,PlanRow]);
  if p<>nil then begin
    CurrentStep:=p.description_str;
    StepTotalCount:=p.repeatcount;
    if p.exposure>0 then Fcapture.ExpTime.Text:=p.exposure_str;
    Fcapture.Binning.Text:=p.binning_str;
    t:=TTarget(TargetGrid.Objects[0,TargetRow]);
    if t<> nil then
       Fcapture.Fname.Text:=t.objectname
    else
       Fcapture.Fname.Text:='Unknow';
    Fcapture.SeqNum.Text:=p.count_str;
    Fcapture.FrameType.ItemIndex:=ord(p.frtype);
    Ffilter.Filters.ItemIndex:=p.filter-1;
    Ffilter.BtnSetFilter.Click;
    StepTimeStart:=now;
    msg('Start step '+p.description_str);
    Fcapture.BtnStart.Click;
  end;
end;

end.

