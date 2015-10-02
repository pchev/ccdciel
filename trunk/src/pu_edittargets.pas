unit pu_edittargets;

{$mode objfpc}{$H+}

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

interface

uses pu_editplan, pu_planetariuminfo, u_global, u_utils, Classes, SysUtils,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  maskedit, Grids, ExtCtrls;

type

  { Tf_EditTargets }

  Tf_EditTargets = class(TForm)
    BtnCdCCoord: TButton;
    BtnCdCTime: TButton;
    BtnNewPlan: TButton;
    BtnNewObject: TButton;
    BtnEditPlan: TButton;
    BtnDeleteObject: TButton;
    BtnAnytime: TButton;
    BtnCurrentCoord: TButton;
    BtnClose: TButton;
    PointAstrometry: TCheckBox;
    CheckBoxRepeat: TCheckBox;
    Delay: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    ObjectName: TEdit;
    Label7: TLabel;
    LabelSeq: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelRepeat: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
    procedure BtnCdCCoordClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnPlanClick(Sender: TObject);
    procedure CheckBoxRepeatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TargetChange(Sender: TObject);
    procedure TargetListSelection(Sender: TObject; aCol, aRow: Integer);
  private
    { private declarations }
    LockTarget: boolean;
    procedure LoadPlanList;
    procedure SetPlanList(pl:string);
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

procedure Tf_EditTargets.SetPlanList(pl:string);
var i:integer;
begin
  i:=PlanList.Items.IndexOf(pl);
  if i>=0 then PlanList.ItemIndex:=i;
end;

procedure Tf_EditTargets.BtnPlanClick(Sender: TObject);
var txt,fn: string;
    newplan: boolean;
begin
  newplan:=(Sender=BtnNewPlan)or(PlanList.Text='');
  if newplan then begin
    txt:=FormEntry(self,'New plan','');
    if txt='' then exit;
    fn:=slash(ConfigDir)+txt+'.plan';
    if FileExistsUTF8(fn) then begin
       if MessageDlg('Plan '+txt+' already exist. Do you want to edit this plan?',mtConfirmation,mbYesNo,0)<>mrYes then exit;
    end;
    f_EditPlan.PlanName.Caption:=txt;
  end
  else begin
    if PlanList.Text='' then exit;
    f_EditPlan.PlanName.Caption:=PlanList.Text;
  end;
  FormPos(f_EditPlan,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_EditPlan.ShowModal;
  if newplan then begin
     LoadPlanList;
     SetPlanList(f_EditPlan.PlanName.Caption);
     TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.CheckBoxRepeatChange(Sender: TObject);
begin
  if CheckBoxRepeat.Checked then begin
     PanelRepeat.Visible:=true;
     RepeatCount.Text:='2';
  end else begin
     PanelRepeat.Visible:=false;
     RepeatCount.Text:='1';
  end;
  TargetChange(nil);
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
  TargetList.Cells[2,i]:=t.planname;
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

procedure Tf_EditTargets.BtnCdCCoordClick(Sender: TObject);
begin
  f_planetariuminfo.Ra.Text  := PointRA.Text;
  f_planetariuminfo.De.Text  := PointDEC.Text;
  f_planetariuminfo.Obj.Text := ObjectName.Text;
  FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_planetariuminfo.ShowModal;
  if f_planetariuminfo.ModalResult=mrOK then begin
     PointRA.Text:=f_planetariuminfo.Ra.Text;
     PointDEC.Text:=f_planetariuminfo.De.Text;
     if f_planetariuminfo.Obj.Text<>'' then ObjectName.Text:=f_planetariuminfo.Obj.Text;
  end;
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
  SetPlanList(t.planname);
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
  PointAstrometry.Checked:=t.astrometrypointing;
  RepeatCount.Text:=t.repeatcount_str;
  Delay.Text:=t.delay_str;
  PreviewExposure.Text:=t.previewexposure_str;
  Preview.Checked:=t.preview;
  CheckBoxRepeat.Checked:=(t.repeatcount>1);
  PanelRepeat.Visible:=CheckBoxRepeat.Checked;
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
  t.planname:=PlanList.Text;
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
  if PointAstrometry.Checked and ((t.ra=NullCoord)or(t.de=NullCoord)) then PointAstrometry.Checked:=false;
  t.astrometrypointing:=PointAstrometry.Checked;
  t.repeatcount:=StrToIntDef(RepeatCount.Text,1);
  t.delay:=StrToFloatDef(Delay.Text,1);
  t.previewexposure:=StrToFloatDef(PreviewExposure.Text,1);
  t.preview:=Preview.Checked;
  TargetList.Cells[1,n]:=t.objectname;
  TargetList.Cells[2,n]:=t.planname;
end;


end.

