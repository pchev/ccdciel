unit pu_edittargets;

{$mode objfpc}{$H+}

interface

uses pu_editplan, u_ccdconfig, u_global, u_utils, Classes, SysUtils,
  FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, maskedit;

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
    Label7: TLabel;
    LabelSeq: TLabel;
    StartTime: TMaskEdit;
    EndTime: TMaskEdit;
    PlanList: TComboBox;
    ObjectList: TComboBox;
    PointRA: TEdit;
    PointDEC: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure BtnAnytimeClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnEditPlanClick(Sender: TObject);
    procedure BtnNewPlanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ObjectListChange(Sender: TObject);
    procedure TargetChange(Sender: TObject);
  private
    { private declarations }
    LockTarget: boolean;
  public
    { public declarations }
    procedure ClearObjectList;
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
  ClearObjectList;
end;

procedure Tf_EditTargets.FormShow(Sender: TObject);
var i: integer;
    fs : TSearchRec;
begin
  if ObjectList.Items.Count>0 then begin
     ObjectList.ItemIndex:=0;
     ObjectListChange(nil);
  end
  else
    LabelSeq.Caption:='0';
  PlanList.Clear;
  i:=FindFirstUTF8(slash(ConfigDir)+'*.plan',0,fs);
  while i=0 do begin
    PlanList.Items.Add(ExtractFileNameWithoutExt(fs.Name));
    i:=FindNextUTF8(fs);
  end;
  FindCloseUTF8(fs);
end;

procedure Tf_EditTargets.ClearObjectList;
var i: integer;
begin
  for i:=0 to ObjectList.Items.Count-1 do begin
    ObjectList.Items.Objects[i].Free;
  end;
  ObjectList.Clear;
end;

procedure Tf_EditTargets.BtnNewPlanClick(Sender: TObject);
begin

end;

procedure Tf_EditTargets.BtnEditPlanClick(Sender: TObject);
begin
  f_EditPlan.ShowModal;
end;

procedure Tf_EditTargets.BtnNewObjectClick(Sender: TObject);
var f: TForm;
    l: Tlabel;
    e: Tedit;
    b: TButton;
    i: integer;
    t: TTarget;
begin
  f:=TForm.Create(self);
  l:=TLabel.Create(f);
  e:=TEdit.Create(f);
  b:=TButton.Create(f);
  l.Caption:='Object name';
  l.Parent:=f;
  e.Text:='None';
  e.Parent:=f;
  b.Caption:='OK';
  b.ModalResult:=mrOK;
  b.Parent:=f;
  f.ChildSizing.ControlsPerLine:=2;
  f.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  f.AutoSize:=true;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.ShowModal;
  if f.ModalResult=mrOK then begin
    t:=TTarget.Create;
    i:=ObjectList.Items.AddObject(e.text,t);
    ObjectList.ItemIndex:=i;
    PointRA.Text:='-';
    PointDEC.Text:='-';
    TargetChange(nil);
  end;
  f.Free;
end;

procedure Tf_EditTargets.BtnDeleteObjectClick(Sender: TObject);
begin

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

procedure Tf_EditTargets.ObjectListChange(Sender: TObject);
var n:integer;
    t: TTarget;
begin
  LockTarget:=true;
  n:=ObjectList.ItemIndex;
  LabelSeq.Caption:=IntToStr(n+1);
  t:=TTarget(ObjectList.Items.Objects[n]);
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
  LockTarget:=false;
end;

procedure Tf_EditTargets.TargetChange(Sender: TObject);
var n:integer;
    t: TTarget;
begin
  if LockTarget then exit;
  n:=ObjectList.ItemIndex;
  if n < 0 then exit;
  t:=TTarget(ObjectList.Items.Objects[n]);
  t.objectname:=ObjectList.Text;
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
end;

end.

