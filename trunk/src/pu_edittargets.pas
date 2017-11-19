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

uses pu_editplan, pu_planetariuminfo, u_global, u_utils, u_ccdconfig, pu_pascaleditor,
  pu_scriptengine, cu_astrometry,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UScaleDPI,
  maskedit, Grids, ExtCtrls, ComCtrls, EditBtn, CheckLst;

type

  { Tf_EditTargets }

  Tf_EditTargets = class(TForm)
    FlatCount: TEdit;
    FlatBinning: TComboBox;
    BtnAnytime: TButton;
    BtnCdCCoord: TButton;
    BtnImgCoord: TButton;
    BtnCurrentCoord: TButton;
    BtnEditPlan: TButton;
    BtnEditScript: TButton;
    BtnImgRot: TButton;
    BtnNewObject: TButton;
    BtnDeleteObject: TButton;
    BtnClose: TButton;
    BtnEditNewScript: TButton;
    BtnNewScript: TButton;
    BtnNewPlan: TButton;
    BtnCopyPlan: TButton;
    BtnDeletePlan: TButton;
    BtnCancel: TButton;
    BtnNewFlat: TButton;
    FlatFilterList: TCheckListBox;
    InplaceAutofocus: TCheckBox;
    Label12: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    LabelSeq2: TLabel;
    FlatTime: TRadioGroup;
    TabSheet3: TTabSheet;
    UpdateCoord: TCheckBox;
    Panel8: TPanel;
    Panel9: TPanel;
    UseRotator: TCheckBox;
    RotatorAngle: TEdit;
    ObjStartRise: TCheckBox;
    ObjEndSet: TCheckBox;
    Panel7: TPanel;
    SeqStart: TCheckBox;
    SeqStopAt: TMaskEdit;
    SeqStop: TCheckBox;
    SeqStartTwilight: TCheckBox;
    SeqStopTwilight: TCheckBox;
    CheckBoxRepeatList: TCheckBox;
    CheckBoxRepeat: TCheckBox;
    Delay: TEdit;
    RepeatCountList: TEdit;
    ObjEndTime: TMaskEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelSeq: TLabel;
    LabelSeq1: TLabel;
    ObjectName: TEdit;
    PageControl1: TPageControl;
    Panel3: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    PanelRepeat: TPanel;
    PlanList: TComboBox;
    ScriptList: TComboBox;
    PointAstrometry: TCheckBox;
    PointDEC: TEdit;
    PointRA: TEdit;
    Preview: TCheckBox;
    PreviewExposure: TEdit;
    RepeatCount: TEdit;
    ObjStartTime: TMaskEdit;
    SeqStartAt: TMaskEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TargetList: TStringGrid;
    procedure BtnAnytimeClick(Sender: TObject);
    procedure BtnCdCCoordClick(Sender: TObject);
    procedure BtnCopyPlanClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeletePlanClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnImgCoordClick(Sender: TObject);
    procedure BtnImgRotClick(Sender: TObject);
    procedure BtnNewFlatClick(Sender: TObject);
    procedure BtnScriptClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnNewScriptClick(Sender: TObject);
    procedure BtnPlanClick(Sender: TObject);
    procedure CheckBoxRepeatChange(Sender: TObject);
    procedure CheckBoxRepeatListChange(Sender: TObject);
    procedure FlatTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ObjEndSetChange(Sender: TObject);
    procedure ObjStartRiseChange(Sender: TObject);
    procedure PointCoordChange(Sender: TObject);
    procedure RepeatCountListChange(Sender: TObject);
    procedure SeqStartChange(Sender: TObject);
    procedure SeqStartTwilightChange(Sender: TObject);
    procedure SeqStopChange(Sender: TObject);
    procedure SeqStopTwilightChange(Sender: TObject);
    procedure TargetChange(Sender: TObject);
    procedure TargetListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure TargetListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure UseRotatorChange(Sender: TObject);
  private
    { private declarations }
    FAstrometry: TAstrometry;
    LockTarget: boolean;
    FTargetsRepeat: integer;
    procedure LoadPlanList;
    procedure SetPlanList(pl:string);
    procedure LoadScriptList;
    procedure SetScriptList(sl:string);
    procedure ResetSequences;
  public
    { public declarations }
    property TargetsRepeat: integer read FTargetsRepeat write FTargetsRepeat;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
  end;

var
  f_EditTargets: Tf_EditTargets;

implementation

uses LazFileUtils;

const   AutoFlatTxt='AutoFlat';
        ScriptTxt='Script';

{$R *.lfm}

{ Tf_EditTargets }

procedure Tf_EditTargets.FormCreate(Sender: TObject);
begin
  ShowHint:=true;
  ScaleDPI(Self);
  LockTarget:=false;
  FTargetsRepeat:=1;
end;

procedure Tf_EditTargets.FormDestroy(Sender: TObject);
begin
  // objects are destroyed in fu_sequence
end;

procedure Tf_EditTargets.FormShow(Sender: TObject);
begin
  TargetList.Cells[0,0]:='Seq';
  LoadPlanList;
  LoadScriptList;
  if TargetList.RowCount>1 then begin
     TargetList.Row:=1;
     TargetListSelection(nil,0,1);
  end
  else begin
    PageControl1.ActivePageIndex:=0;
    LabelSeq.Caption:='0';
    LabelSeq1.Caption:='0';
    LabelSeq2.Caption:='0';
    ObjStartTime.Text:='';
    ObjEndTime.Text:='';
    ObjStartRise.Checked:=false;
    ObjEndSet.Checked:=false;
  end;
  RepeatCountList.Text:=IntToStr(FTargetsRepeat);
  CheckBoxRepeatList.Checked:=(FTargetsRepeat>1);
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
end;

procedure Tf_EditTargets.ObjEndSetChange(Sender: TObject);
var ra,de,hs: double;
    i:integer;
begin
if ObjEndSet.Checked then begin
  ra:=StrToAR(PointRA.Text);
  de:=StrToDE(PointDEC.Text);
  if (ra=NullCoord)or(de=NullCoord) then begin
     ShowMessage('Invalid object coordinates!');
     ObjEndSet.Checked:=false;
     exit;
  end;
  if ObjSet(ra,de,hs,i) then
     ObjEndTime.Text:=TimeToStr(hs/24)
  else begin
     if i=1 then
       ShowMessage('This object is never below the requested elevation')
     else
       ShowMessage('This object is never above the requested elevation');
     ObjEndTime.Text:='';
     ObjEndSet.Checked:=false;
  end;
end;
TargetChange(Sender);
end;

procedure Tf_EditTargets.ObjStartRiseChange(Sender: TObject);
var ra,de,hr: double;
    i:integer;
begin
if ObjStartRise.Checked then begin
  ra:=StrToAR(PointRA.Text);
  de:=StrToDE(PointDEC.Text);
  if (ra=NullCoord)or(de=NullCoord) then begin
     ShowMessage('Invalid object coordinates!');
     ObjStartRise.Checked:=false;
     exit;
  end;
  if ObjRise(ra,de,hr,i) then
     ObjStartTime.Text:=TimeToStr(hr/24)
   else begin
      if i=1 then
        ShowMessage('This object is never below the requested elevation')
      else
        ShowMessage('This object is never above the requested elevation');
      ObjStartTime.Text:='';
      ObjStartRise.Checked:=false;
   end;
end;
TargetChange(Sender);
end;

procedure Tf_EditTargets.PointCoordChange(Sender: TObject);
begin
  if ObjStartRise.Checked then ObjStartRiseChange(Sender);
  if ObjEndSet.Checked then ObjEndSetChange(Sender);
  TargetChange(Sender);
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

procedure Tf_EditTargets.LoadScriptList;
var i,k: integer;
    fs : TSearchRec;
    s: TStringlist;
begin
  s:=TStringlist.Create;
  ScriptList.Clear;
  for k:=1 to MaxScriptDir do begin
    i:=FindFirstUTF8(ScriptDir[k].path+'*.script',0,fs);
    while i=0 do begin
      s.AddObject(ExtractFileNameOnly(fs.Name),ScriptDir[k]);
      i:=FindNextUTF8(fs);
    end;
    FindCloseUTF8(fs);
  end;
  s.CustomSort(@ScriptListCompare);
  ScriptList.Items.Assign(s);
  ScriptList.ItemIndex:=0;
end;

procedure Tf_EditTargets.SetScriptList(sl:string);
var i:integer;
begin
  i:=ScriptList.Items.IndexOf(sl);
  if i>=0 then ScriptList.ItemIndex:=i;
end;

procedure Tf_EditTargets.BtnCopyPlanClick(Sender: TObject);
var txt,fn1,fn2: string;
    pfile: TCCDconfig;
begin
  txt:=PlanList.Text;
  fn1:=slash(ConfigDir)+txt+'.plan';
  txt:=FormEntry(self,'Copy to ','');
  if txt='' then exit;
  fn2:=slash(ConfigDir)+txt+'.plan';
  if FileExistsUTF8(fn2) then begin
     if MessageDlg('Plan '+fn2+' already exist. Do you want to replace this file?',mtConfirmation,mbYesNo,0)<>mrYes then
       exit;
  end;
  if CopyFile(fn1,fn2,false) then begin
     pfile:=TCCDconfig.Create(self);
     pfile.Filename:=fn2;
     pfile.SetValue('/PlanName',txt);
     pfile.Flush;
     pfile.Free;
     LoadPlanList;
     SetPlanList(txt);
     BtnPlanClick(Sender);
     TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.BtnDeletePlanClick(Sender: TObject);
var txt,fn: string;
begin
  txt:=PlanList.Text;
  fn:=slash(ConfigDir)+txt+'.plan';
  if MessageDlg('Do you want to delete file '+fn+' ?',mtConfirmation,mbYesNo,0)=mrYes then begin
     DeleteFileUTF8(fn);
     LoadPlanList;
     PlanList.Text:='';
     TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.BtnPlanClick(Sender: TObject);
var txt,fn: string;
    newplan: boolean;
begin
  newplan:=(Sender=BtnNewPlan)or(PlanList.Text='');
  if newplan then begin
    txt:=FormEntry(self,'New plan ','');
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
  if f_EditPlan.ShowModal=mrOK then begin
    if newplan then begin
       LoadPlanList;
       SetPlanList(f_EditPlan.PlanName.Caption);
       TargetChange(nil);
    end;
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
    i,n: integer;
    t,tt: TTarget;
begin
  PageControl1.ActivePageIndex:=0;
  txt:=FormEntry(self,'Object name','None');
  if txt=ScriptTxt then txt:='_Script';
  t:=TTarget.Create;
  n:=TargetList.Row;
  if n>=1 then begin
    tt:=TTarget(TargetList.Objects[0,n]);
    if tt.objectname<>ScriptTxt then t.Assign(tt);
  end;
  TargetList.RowCount:=TargetList.RowCount+1;
  i:=TargetList.RowCount-1;
  TargetList.Cells[0,i]:=IntToStr(i);
  TargetList.Cells[1,i]:=txt;
  TargetList.Cells[2,i]:=t.planname;
  TargetList.Objects[0,i]:=t;
  TargetList.Row:=i;
  ObjectName.Text:=trim(txt);
  TargetChange(nil);
end;

procedure Tf_EditTargets.BtnNewScriptClick(Sender: TObject);
var i: integer;
    t: TTarget;
begin
  PageControl1.ActivePageIndex:=1;
  t:=TTarget.Create;
  t.objectname:=ScriptTxt;
  TargetList.RowCount:=TargetList.RowCount+1;
  i:=TargetList.RowCount-1;
  TargetList.Cells[0,i]:=IntToStr(i);
  TargetList.Cells[1,i]:=ScriptTxt;
  TargetList.Cells[2,i]:=t.planname;
  TargetList.Objects[0,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
end;

procedure Tf_EditTargets.BtnNewFlatClick(Sender: TObject);
var ft:string;
    i,n,k: integer;
    t: TTarget;
begin
  n:=0;
  for i:=1 to TargetList.RowCount-1 do
    if TargetList.Cells[1,i]=AutoFlatTxt then begin
      inc(n);
      k:=i;
    end;
  if n=0 then begin // first flat at dusk by default
    LockTarget:=true;
    TargetList.InsertColRow(false,1);
    i:=1;
    FlatTime.ItemIndex:=0;
    ft:=FlatTimeName[0];
    LockTarget:=false;
  end
  else if n=1 then begin
    LockTarget:=true;
    if k=1 then begin
      TargetList.RowCount:=TargetList.RowCount+1;
      i:=TargetList.RowCount-1;
      FlatTime.ItemIndex:=1;
      ft:=FlatTimeName[1];
    end
    else begin
      TargetList.InsertColRow(false,1);
      i:=1;
      FlatTime.ItemIndex:=0;
      ft:=FlatTimeName[0];
    end;
    LockTarget:=false;
  end
  else begin // no more than two flat series
    ShowMessage('Can only add one flat serie at dusk and one at dawn');
    exit;
  end;
  PageControl1.ActivePageIndex:=2;
  t:=TTarget.Create;
  t.objectname:=AutoFlatTxt;
  t.planname:=ft;
  t.FlatFilters:='';
  t.FlatBinX:=1;
  t.FlatBinY:=1;
  TargetList.Cells[0,i]:=IntToStr(i);
  TargetList.Cells[1,i]:=AutoFlatTxt;
  TargetList.Cells[2,i]:=t.planname;
  TargetList.Objects[0,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
  ResetSequences;
  TargetListSelection(Sender,0,i);
end;

procedure Tf_EditTargets.BtnScriptClick(Sender: TObject);
var txt,fn: string;
    scdir:TScriptDir;
    i:integer;
    newscript: boolean;
    s: TStringList;
begin
  newscript:=(Sender=BtnEditNewScript)or(ScriptList.Text='');
  s:=TStringList.Create;
  if f_pascaleditor=nil then begin
     f_pascaleditor:=Tf_pascaleditor.Create(self);
     f_pascaleditor.DebugScript:=f_scriptengine.dbgscr;
  end;
  if newscript then begin
    s.Clear;
    txt:=FormEntry(self,'New script','');
    if txt='' then exit;
    scdir:=ScriptDir[1];
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=scdir.path+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg('Script '+txt+' already exist. Do you want to edit this script?',mtConfirmation,mbYesNo,0)=mrYes then
         s.LoadFromFile(fn)
       else
         exit;
    end;
    f_pascaleditor.ScriptName:=txt;
  end
  else begin
      i:=ScriptList.ItemIndex;
      if i<0 then exit;
      txt:=ScriptList.Items[i];
      scdir:=TScriptDir(ScriptList.Items.Objects[i]);
      if (txt='')or(scdir=nil) then exit;
      fn:=scdir.path+txt+'.script';
      s.LoadFromFile(fn);
      if scdir<>ScriptDir[1] then begin
         if copy(txt,1,2)='T_' then
            delete(txt,1,2)
         else begin
           if txt[1]<>'_' then txt:='_'+txt
         end;
         scdir:=ScriptDir[1];
         fn:=scdir.path+txt+'.script';
         newscript:=true;
         if FileExistsUTF8(fn) then begin
            if MessageDlg('Script '+fn+' already exist. Do you want to replace this custom script by the template?',mtConfirmation,mbYesNo,0)<>mrYes then
              exit;
         end;
      end;
      f_pascaleditor.ScriptName:=txt;
  end;
  f_pascaleditor.SynEdit1.Lines.Assign(s);
  FormPos(f_pascaleditor,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_pascaleditor.ShowModal;
  if f_pascaleditor.ModalResult=mrOK then begin
    s.Assign(f_pascaleditor.SynEdit1.Lines);
    s.SaveToFile(fn);
    if newscript then begin
     LoadScriptList;
     SetScriptList(f_pascaleditor.ScriptName);
     TargetChange(nil);
    end;
  end;
  s.Free;
end;


procedure Tf_EditTargets.BtnDeleteObjectClick(Sender: TObject);
var i,j: integer;
    str: string;
begin
  i:=TargetList.Row;
  if i>0 then begin
     str:=TargetList.Cells[0,i]+', '+TargetList.Cells[1,i];
     if MessageDlg('Delete sequence '+str+' ?',mtConfirmation,mbYesNo,0)=mrYes then begin
        j:=i-1;
        if j<1 then j:=i+1;
        if j>=TargetList.RowCount then j:=TargetList.RowCount-1;
        TargetList.Row:=j;
        Application.ProcessMessages;
        LockTarget:=true;
        //if TargetList.Objects[0,i]<>nil then TargetList.Objects[0,i].Free; Cause crash because plan is shared
        TargetList.Objects[0,i]:=nil;
        TargetList.DeleteRow(i);
        Application.ProcessMessages;
        ResetSequences;
        if i>TargetList.RowCount then i:=TargetList.RowCount-1;
        TargetList.Row:=i;
        Application.ProcessMessages;
        LockTarget:=false;
        TargetChange(nil);
     end;
  end;

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
  ObjStartRise.Checked:=false;
  ObjEndSet.Checked:=false;
  ObjStartTime.Text:='';
  ObjEndTime.Text:='';
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
     if f_planetariuminfo.Obj.Text<>'' then ObjectName.Text:=trim(f_planetariuminfo.Obj.Text);
  end;
end;

procedure Tf_EditTargets.BtnImgCoordClick(Sender: TObject);
var ra,de,eq,pa: double;
begin
try
  screen.Cursor:=crHourGlass;
  FAstrometry.SolveCurrentImage(true);
  if FAstrometry.CurrentCoord(ra,de,eq,pa) then begin
    ra:=ra*15*deg2rad;
    de:=de*deg2rad;
    J2000ToApparent(ra,de);
    ra:=rad2deg*ra/15;
    de:=rad2deg*de;
    PointRA.Text:=RAToStr(ra);
    PointDEC.Text:=DEToStr(de);
    PointAstrometry.Checked:=true;
  end;
finally
  screen.Cursor:=crDefault;
end;
end;

procedure Tf_EditTargets.BtnImgRotClick(Sender: TObject);
var ra,de,eq,pa: double;
begin
try
  screen.Cursor:=crHourGlass;
  FAstrometry.SolveCurrentImage(true);
  if FAstrometry.CurrentCoord(ra,de,eq,pa) then begin
    UseRotator.Checked:=true;
    RotatorAngle.Text:=FormatFloat(f1,pa);
  end;
finally
  screen.Cursor:=crDefault;
end;
end;

procedure Tf_EditTargets.BtnCurrentCoordClick(Sender: TObject);
begin
  PointRA.Text:='-';
  PointDEC.Text:='-';
end;

procedure Tf_EditTargets.TargetListSelection(Sender: TObject; aCol, aRow: Integer);
var n,i,j:integer;
    buf: string;
    filterlst:TStringList;
    t: TTarget;
begin
  LockTarget:=true;
  n:=aRow;
  LabelSeq.Caption:=IntToStr(n);
  LabelSeq1.Caption:=IntToStr(n);
  LabelSeq2.Caption:=IntToStr(n);
  t:=TTarget(TargetList.Objects[0,n]);
  if t.objectname=ScriptTxt then begin
    PageControl1.ActivePageIndex:=1;
    SetScriptList(t.planname);
  end
  else if t.objectname=AutoFlatTxt then begin
    PageControl1.ActivePageIndex:=2;
    if t.planname=FlatTimeName[0]
       then FlatTime.ItemIndex:=0
       else FlatTime.ItemIndex:=1;
    FlatCount.Text:=IntToStr(t.FlatCount);
    buf:=inttostr(t.FlatBinX)+'x'+inttostr(t.FlatBinY);
    j:=FlatBinning.Items.IndexOf(buf);
    if j<0 then
      j:=FlatBinning.Items.Add(buf);
    FlatBinning.ItemIndex:=j;
    filterlst:=TStringList.Create();
    SplitRec(t.FlatFilters,';',filterlst);
    for i:=0 to FlatFilterList.Count-1 do begin
      FlatFilterList.Checked[i]:=false;
      if trim(FlatFilterList.Items[i])<>'' then
       for j:=0 to filterlst.Count-1 do begin
        if FlatFilterList.Items[i]=filterlst[j] then
          FlatFilterList.Checked[i]:=true;
      end;
    end;
  end
  else begin
    PageControl1.ActivePageIndex:=0;
    ObjectName.Text:=t.objectname;
    SetPlanList(t.planname);
    if t.starttime>=0 then
       ObjStartTime.Text:=TimeToStr(t.starttime)
    else
       ObjStartTime.Text:='';
    if t.endtime>=0 then
       ObjEndTime.Text:=TimeToStr(t.endtime)
    else
       ObjEndTime.Text:='';
    if t.ra=NullCoord then
      PointRA.Text:='-'
    else
      PointRA.Text:=RAToStr(t.ra);
    if t.de=NullCoord then
      PointDEC.Text:='-'
    else
      PointDEC.Text:=DEToStr(t.de);
    ObjStartRise.Checked:=t.startrise;
    ObjEndSet.Checked:=t.endset;
    PointAstrometry.Checked:=t.astrometrypointing;
    UpdateCoord.Checked:=t.updatecoord;
    UseRotator.Checked:=(t.pa<>NullCoord);
    InplaceAutofocus.Checked:=t.inplaceautofocus;
    if t.pa=NullCoord then
      RotatorAngle.Text:='-'
    else
      RotatorAngle.Text:=FormatFloat(f1,t.pa);
    RepeatCount.Text:=t.repeatcount_str;
    Delay.Text:=t.delay_str;
    PreviewExposure.Text:=t.previewexposure_str;
    Preview.Checked:=t.preview;
    CheckBoxRepeat.Checked:=(t.repeatcount>1);
    PanelRepeat.Visible:=CheckBoxRepeat.Checked;
  end;
  LockTarget:=false;
end;

procedure Tf_EditTargets.UseRotatorChange(Sender: TObject);
begin
  RotatorAngle.Enabled:=UseRotator.Checked;
  if RotatorAngle.Enabled then begin
    RotatorAngle.Text:='0.0';
  end
  else begin
    RotatorAngle.Text:='-'
  end;
end;

procedure Tf_EditTargets.TargetChange(Sender: TObject);
var i,n,j:integer;
    scdir:TScriptDir;
    sname,str,buf: string;
    t: TTarget;
begin
  if LockTarget then exit;
  n:=TargetList.Row;
  if n < 1 then exit;
  t:=TTarget(TargetList.Objects[0,n]);
  if t.objectname=ScriptTxt then begin
    PageControl1.ActivePageIndex:=1;
    i:=ScriptList.ItemIndex;
    sname:=ScriptList.Items[i];
    scdir:=TScriptDir(ScriptList.Items.Objects[i]);
    t.planname:=sname;
    if scdir=nil then t.path:=''
                 else t.path:=scdir.path;
  end
  else if t.objectname=AutoFlatTxt then begin
    PageControl1.ActivePageIndex:=2;
    t.planname:=FlatTimeName[FlatTime.ItemIndex];
    t.FlatCount:=StrToIntDef(FlatCount.Text,1);
    str:=FlatBinning.Text;
    j:=pos('x',str);
    if j>0 then begin
       buf:=trim(copy(str,1,j-1));
       t.FlatBinX:=StrToIntDef(buf,1);
       buf:=trim(copy(str,j+1,9));
       t.FlatBinY:=StrToIntDef(buf,1);
    end else begin
      t.FlatBinX:=1;
      t.FlatBinY:=1;
    end;
    t.FlatFilters:='';
    for j:=0 to FlatFilterList.Count-1 do begin
      if FlatFilterList.Checked[j] then
         t.FlatFilters:=t.FlatFilters+FlatFilterList.Items[j]+';';
    end;
  end
  else begin
    PageControl1.ActivePageIndex:=0;
    t.objectname:=trim(ObjectName.Text);
    t.planname:=PlanList.Text;
    t.starttime:=StrToTimeDef(ObjStartTime.Text,-1);
    t.endtime:=StrToTimeDef(ObjEndTime.Text,-1);
    if PointRA.Text='-' then
      t.ra:=NullCoord
    else
      t.ra:=StrToAR(PointRA.Text);
    if PointDEC.Text='-' then
      t.de:=NullCoord
    else
      t.de:=StrToDE(PointDEC.Text);
    t.startrise:=ObjStartRise.Checked;
    t.endset:=ObjEndSet.Checked;
    ObjStartTime.Enabled:= not t.startrise;
    ObjEndTime.Enabled:= not t.endset;
    if RotatorAngle.Text='-' then
      t.pa:=NullCoord
    else
      t.pa:=StrToFloatDef(RotatorAngle.Text,NullCoord);
    if PointAstrometry.Checked and ((t.ra=NullCoord)or(t.de=NullCoord)) then PointAstrometry.Checked:=false;
    t.astrometrypointing:=PointAstrometry.Checked;
    t.updatecoord:=UpdateCoord.Checked;
    t.inplaceautofocus:=InplaceAutofocus.Checked;
    t.repeatcount:=StrToIntDef(RepeatCount.Text,1);
    t.delay:=StrToFloatDef(Delay.Text,1);
    t.previewexposure:=StrToFloatDef(PreviewExposure.Text,1);
    t.preview:=Preview.Checked;
  end;
  TargetList.Cells[1,n]:=t.objectname;
  TargetList.Cells[2,n]:=t.planname;
end;

procedure Tf_EditTargets.FlatTimeClick(Sender: TObject);
var r1,r2,i,n: integer;
    t:TTarget;
begin
if LockTarget then exit;
  n:=0;
  for i:=1 to TargetList.RowCount-1 do
    if TargetList.Cells[1,i]=AutoFlatTxt then inc(n);
  if n>=2 then begin
    LockTarget:=true;
    if FlatTime.ItemIndex=0 then  begin
       FlatTime.ItemIndex:=1;
       ShowMessage('There is already a dusk flat plan');
    end
    else begin
       FlatTime.ItemIndex:=0;
       ShowMessage('There is already a dawn flat plan');
    end;
    LockTarget:=false;
    exit;
  end;
  if FlatTime.ItemIndex=0 then begin
     r1:=TargetList.Row;
     r2:=1;
     t:=TTarget(TargetList.Objects[0,r1]);
     t.planname:=FlatTimeName[0];
     TargetList.MoveColRow(false,r1,r2);
  end
  else begin
    r1:=TargetList.Row;
    t:=TTarget(TargetList.Objects[0,r1]);
    t.planname:=FlatTimeName[1];
    r2:=TargetList.RowCount-1;
    TargetList.MoveColRow(false,r1,r2);
  end;
  TargetChange(Sender);
end;

procedure Tf_EditTargets.TargetListColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  ResetSequences;
  TargetListSelection(Sender,0,tIndex);
end;

procedure Tf_EditTargets.CheckBoxRepeatListChange(Sender: TObject);
begin
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=StrToIntDef(RepeatCountList.Text,1)
  else
    FTargetsRepeat:=1;
end;

procedure Tf_EditTargets.RepeatCountListChange(Sender: TObject);
begin
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=StrToIntDef(RepeatCountList.Text,1);
end;

procedure Tf_EditTargets.SeqStartChange(Sender: TObject);
begin
  SeqStartTwilight.Enabled:=SeqStart.Checked;
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
end;

procedure Tf_EditTargets.SeqStopChange(Sender: TObject);
begin
  SeqStopTwilight.Enabled:=SeqStop.Checked;
  SeqStopAt.Enabled:=SeqStop.Checked and (not SeqStopTwilight.Checked);
end;

procedure Tf_EditTargets.SeqStartTwilightChange(Sender: TObject);
var he,hm: double;
begin
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
  if SeqStartTwilight.Checked and TwilightAstro(now,hm,he) then
     SeqStartAt.Text:=TimeToStr(he/24);
end;

procedure Tf_EditTargets.SeqStopTwilightChange(Sender: TObject);
var he,hm: double;
begin
  SeqStopAt.Enabled:=SeqStop.Checked and (not SeqStopTwilight.Checked);
  if SeqStopTwilight.Checked and TwilightAstro(now,hm,he) then
     SeqStopAt.Text:=TimeToStr(hm/24);
end;

end.

