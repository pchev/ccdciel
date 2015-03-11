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
    BtnSaveTargets: TButton;
    BtnLoadTargets: TButton;
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
    procedure LoadTargets(fn: string);
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
  end;

var
  f_sequence: Tf_sequence;

implementation

{$R *.lfm}

procedure Tf_sequence.BtnEditTargetsClick(Sender: TObject);
var i:integer;
    t:TTarget;
begin
   f_EditTargets.ClearObjectList;
   if TargetGrid.RowCount>1 then begin
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
       f_EditTargets.ObjectList.Items.AddObject(TargetGrid.Cells[0,i],t);
     end;
   end;
   f_EditTargets.ShowModal;
   TargetGrid.RowCount:=f_EditTargets.ObjectList.Items.Count+1;
   for i:=0 to f_EditTargets.ObjectList.Items.Count-1 do begin
     with f_EditTargets.ObjectList.Items.Objects[i] as TTarget do begin
       TargetGrid.Cells[0,i+1]:=objectname;
       TargetGrid.Cells[1,i+1]:=plan;
       TargetGrid.Cells[2,i+1]:=FormatDateTime('hh:nn:ss',starttime);
       TargetGrid.Cells[3,i+1]:=FormatDateTime('hh:nn:ss',endtime);;
       if ra=NullCoord then
         TargetGrid.Cells[4,i+1]:='-'
       else
         TargetGrid.Cells[4,i+1]:=RAToStr(ra);
       if de=NullCoord then
         TargetGrid.Cells[5,i+1]:='-'
       else
         TargetGrid.Cells[5,i+1]:=DEToStr(de);
     end;
   end;
end;

procedure Tf_sequence.LoadTargets(fn: string);
var tfile: TCCDconfig;
    i,n: integer;
begin
   tfile:=TCCDconfig.Create(self);
   tfile.Filename:=fn;
   CurrentName:=ExtractFileNameOnly(fn);
   CurrentFile:=fn;
   n:=tfile.GetValue('/TargetNum',0);
   if n>0 then begin
     TargetGrid.RowCount:=n+1;
     for i:=1 to n do begin
       TargetGrid.Cells[0,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/ObjectName','');
       TargetGrid.Cells[1,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Plan','');
       TargetGrid.Cells[2,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/StartTime','');
       TargetGrid.Cells[3,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/EndTime','');
       TargetGrid.Cells[4,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/RA','');
       TargetGrid.Cells[5,i]:=tfile.GetValue('/Targets/Target'+inttostr(i)+'/Dec','');
     end;
   end;
end;

procedure Tf_sequence.BtnLoadTargetsClick(Sender: TObject);
begin
 OpenDialog1.InitialDir:=ConfigDir;
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
 PlanGrid.Cells[0,0]:='Exp.';
 PlanGrid.Cells[1,0]:='Count';
 PlanGrid.Cells[2,0]:='Filter';
 PlanGrid.Cells[3,0]:='Type';
end;

destructor  Tf_sequence.Destroy;
begin
 inherited Destroy;
end;

end.

