unit pu_edittargets;

{$mode objfpc}{$H+}

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

interface

uses pu_editplan, u_global, u_utils, Classes, SysUtils,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  maskedit, Grids;

type

  { Tf_EditTargets }

  Tf_EditTargets = class(TForm)
    BtnNewPlan: TButton;
    BtnNewObject: TButton;
    BtnEditPlan: TButton;
    BtnDeleteObject: TButton;
    BtnAnytime: TButton;
    BtnCurrentCoord: TButton;
    BtnClose: TButton;
    Delay: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label9: TLabel;
    ObjectName: TEdit;
    Label7: TLabel;
    LabelSeq: TLabel;
    Preview: TCheckBox;
    PreviewExposure: TEdit;
    RepeatCount: TEdit;
    StartTime: TMaskEdit;
    EndTime: TMaskEdit;
    PlanList: TComboBox;
    PointRA: TEdit;
    PointDEC: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    TargetList: TStringGrid;
    procedure BtnAnytimeClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnPlanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TargetChange(Sender: TObject);
    procedure TargetListSelection(Sender: TObject; aCol, aRow: Integer);
  private
    { private declarations }
    LockTarget: boolean;
    procedure LoadPlanList;
    procedure ResetSequences;
  public
    { public declarations }
  end;

var
  f_EditTargets: Tf_EditTargets;

implementation

{$R *.lfm}

{ Tf_EditTargets }

procedure Tf_EditTargets.FormCreate(Sender: TObject);
begin
  LockTarget:=false;
end;

procedure Tf_EditTargets.FormDestroy(Sender: TObject);
begin
  // objects are destroyed in fu_sequence
end;

procedure Tf_EditTargets.FormShow(Sender: TObject);
begin
  LoadPlanList;
  if TargetList.RowCount>1 then begin
     TargetList.Row:=1;
     TargetListSelection(nil,0,1);
  end
  else begin
    LabelSeq.Caption:='0';
    StartTime.Text:='00:00:00';
    EndTime.Text:='23:59:59';
  end;
end;

procedure Tf_EditTargets.LoadPlanList;
var i: integer;
    fs : TSearchRec;
begin
  PlanList.Clear;
  i:=FindFirstUTF8(slash(ConfigDir)+'*.plan',0,fs);
  while i=0 do begin
    PlanList.Items.Add(ExtractFileNameOnly(fs.Name));
    i:=FindNextUTF8(fs);
  end;
  FindCloseUTF8(fs);
end;

procedure Tf_EditTargets.BtnPlanClick(Sender: TObject);
begin
  if Sender=BtnNewPlan then
    f_EditPlan.PlanName.Caption:=FormEntry(self,'New plan','Plan')
  else
    f_EditPlan.PlanName.Caption:=PlanList.Text;
  FormPos(f_EditPlan,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_EditPlan.ShowModal;
  if Sender=BtnNewPlan then begin
     LoadPlanList;
     PlanList.Text:=f_EditPlan.PlanName.Caption;
  end;
end;

procedure Tf_EditTargets.BtnNewObjectClick(Sender: TObject);
var txt:string;
    i: integer;
    t: TTarget;
begin
  txt:=FormEntry(self,'Object name','None');
  t:=TTarget.Create;
  TargetList.RowCount:=TargetList.RowCount+1;
  i:=TargetList.RowCount-1;
  TargetList.Cells[0,i]:=IntToStr(i);
  TargetList.Cells[1,i]:=txt;
  TargetList.Objects[0,i]:=t;
  TargetList.Row:=i;
  ObjectName.Text:=txt;
  PointRA.Text:='-';
  PointDEC.Text:='-';
  TargetChange(nil);
end;

procedure Tf_EditTargets.BtnDeleteObjectClick(Sender: TObject);
var i: integer;
    str: string;
begin
  i:=TargetList.Row;
  if i>0 then begin
     str:=TargetList.Cells[0,i]+', '+TargetList.Cells[1,i];
     if MessageDlg('Delete sequence '+str+' ?',mtConfirmation,mbYesNo,0)=mrYes then begin
        if TargetList.Objects[0,i]<>nil then TargetList.Objects[0,i].Free;
        TargetList.Objects[0,i]:=nil;
        TargetList.DeleteRow(i);
     end;
  end;
  ResetSequences;
end;

procedure Tf_EditTargets.ResetSequences;
var i: integer;
begin
  for i:=1 to TargetList.RowCount-1 do begin
    TargetList.Cells[0,i]:=IntToStr(i);
  end;
end;

procedure Tf_EditTargets.BtnAnytimeClick(Sender: TObject);
begin
  StartTime.Text:='00:00:00';
  EndTime.Text:='23:59:59';
end;

procedure Tf_EditTargets.BtnCurrentCoordClick(Sender: TObject);
begin
  PointRA.Text:='-';
  PointDEC.Text:='-';
end;

procedure Tf_EditTargets.TargetListSelection(Sender: TObject; aCol, aRow: Integer);
var n:integer;
    t: TTarget;
begin
  LockTarget:=true;
  n:=aRow;
  LabelSeq.Caption:=IntToStr(n);
  t:=TTarget(TargetList.Objects[0,n]);
  ObjectName.Text:=t.objectname;
  PlanList.Text:=t.plan;
  StartTime.Text:=TimeToStr(t.starttime);
  EndTime.Text:=TimeToStr(t.endtime);
  if t.ra=NullCoord then
    PointRA.Text:='-'
  else
    PointRA.Text:=RAToStr(t.ra);
  if t.de=NullCoord then
    PointDEC.Text:='-'
  else
    PointDEC.Text:=DEToStr(t.de);
  RepeatCount.Text:=t.repeatcount_str;
  Delay.Text:=t.delay_str;
  PreviewExposure.Text:=t.previewexposure_str;
  Preview.Checked:=t.preview;
  LockTarget:=false;
end;

procedure Tf_EditTargets.TargetChange(Sender: TObject);
var n:integer;
    t: TTarget;
begin
  if LockTarget then exit;
  n:=TargetList.Row;
  if n < 1 then exit;
  t:=TTarget(TargetList.Objects[0,n]);
  t.objectname:=ObjectName.Text;
  t.plan:=PlanList.Text;
  t.starttime:=StrToTime(StartTime.Text);
  t.endtime:=StrToTime(EndTime.Text);
  if PointRA.Text='-' then
    t.ra:=NullCoord
  else
    t.ra:=StrToAR(PointRA.Text);
  if PointDEC.Text='-' then
    t.de:=NullCoord
  else
    t.de:=StrToDE(PointDEC.Text);
  t.repeatcount:=StrToIntDef(RepeatCount.Text,1);
  t.delay:=StrToFloatDef(Delay.Text,1);
  t.previewexposure:=StrToFloatDef(PreviewExposure.Text,1);
  t.preview:=Preview.Checked;
  TargetList.Cells[1,n]:=t.objectname;
end;


end.

