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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids;

type

  { Tf_sequence }

  Tf_sequence = class(TFrame)
    BtnEditTargets: TButton;
    BtnLoadTargets1: TButton;
    BtnLoadTargets2: TButton;
    BtnLoadTargets3: TButton;
    BtnLoadTargets4: TButton;
    BtnNewTargets: TButton;
    BtnSaveTargets: TButton;
    BtnLoadTargets: TButton;
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
    procedure BtnLoadTargetsClick(Sender: TObject);
    procedure BtnSaveTargetsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CurrentName, CurrentFile: string;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ClearTargetGrid;
    procedure ClearPlanGrid;
    procedure LoadTargets(fn: string);
    procedure LoadPlan(plan:string);
  end;

var
  f_sequence: Tf_sequence;

implementation

{$R *.lfm}

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

procedure Tf_sequence.BtnEditTargetsClick(Sender: TObject);
var i:integer;
    t:TTarget;
begin
   f_EditTargets.ClearTargetList;
   if (Sender=BtnEditTargets)and(TargetGrid.RowCount>1) then begin
     for i:=1 to TargetGrid.RowCount-1 do begin
       t:=TTarget.Create;
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
       f_EditTargets.TargetList.RowCount:=i+1;
       f_EditTargets.TargetList.Cells[0,i]:=IntToStr(i);
       f_EditTargets.TargetList.Cells[1,i]:=t.objectname;
       f_EditTargets.TargetList.Objects[0,i]:=t;
     end;
   end;
   FormPos(f_EditTargets,mouse.CursorPos.X,mouse.CursorPos.Y);
   f_EditTargets.ShowModal;
   ClearTargetGrid;
   TargetGrid.RowCount:=f_EditTargets.TargetList.RowCount;
   for i:=1 to f_EditTargets.TargetList.RowCount-1 do begin
     TargetGrid.Objects[0,i]:=f_EditTargets.TargetList.Objects[0,i];
     with f_EditTargets.TargetList.Objects[0,i] as TTarget do begin
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
   CurrentFile:=fn;
   n:=tfile.GetValue('/TargetNum',0);
   if n>0 then begin
     t:=TTarget.Create;
     ClearTargetGrid;
     TargetGrid.RowCount:=n+1;
     for i:=1 to n do begin
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
       TargetGrid.Objects[0,i]:=t;
     end;
     LoadPlan(TargetGrid.Cells[1,1]);
   end;
end;

procedure Tf_sequence.LoadPlan(plan:string);
var fn,str,buf: string;
    i,j,n:integer;
    pfile: TCCDconfig;
    p: TPlan;
begin
  fn:=slash(ConfigDir)+plan+'.plan';
  if FileExistsUTF8(fn) then begin
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
    i: integer;
begin
 if TargetGrid.RowCount>1 then begin
  SaveDialog1.InitialDir:=ConfigDir;
  if SaveDialog1.Execute then begin
    tfile:=TCCDconfig.Create(self);
    tfile.Filename:=SaveDialog1.FileName;
    tfile.Clear;
    CurrentName:=ExtractFileNameOnly(SaveDialog1.FileName);
    CurrentFile:=SaveDialog1.FileName;
    tfile.SetValue('/ListName',CurrentName);
    tfile.SetValue('/TargetNum',TargetGrid.RowCount-1);
    for i:=1 to TargetGrid.RowCount-1 do begin
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/ObjectName',TargetGrid.Cells[0,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Plan',TargetGrid.Cells[1,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/StartTime',TargetGrid.Cells[2,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/EndTime',TargetGrid.Cells[3,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/RA',TargetGrid.Cells[4,i]);
      tfile.SetValue('/Targets/Target'+inttostr(i)+'/Dec',TargetGrid.Cells[5,i]);
    end;
    tfile.Flush;
    tfile.Free;
  end;
 end;
end;

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
end;

destructor  Tf_sequence.Destroy;
begin
 ClearTargetGrid;
 ClearPlanGrid;
 inherited Destroy;
end;

end.

