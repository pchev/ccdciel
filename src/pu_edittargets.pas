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

uses pu_planetariuminfo, u_global, u_utils, u_ccdconfig, pu_pascaleditor, u_annotation,
  pu_scriptengine, cu_astrometry, u_translation, u_hints, pu_selectscript, Classes, math,
  SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UScaleDPI,
  LazUTF8, maskedit, Grids, ExtCtrls, ComCtrls, EditBtn, CheckLst, Spin,
  Buttons;

const
  colseq=0; colname=1; colplan=2; colra=3; coldec=4; colpa=5; colstart=6; colend=7; coldark=8; colskip=9; colrepeat=10;
  pcolseq=0; pcoldesc=1; pcoltype=2; pcolexp=3; pcolbin=4; pcolfilter=5; pcolcount=6;
  titleadd=0; titledel=1;

type

  { Tf_EditTargets }

  Tf_EditTargets = class(TForm)
    AutofocusCount: TSpinEdit;
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnAddStep: TButton;
    Btn_coord_internal: TButton;
    BtnUnattendedScript: TButton;
    BtnSaveAs: TButton;
    BtnSavePlan: TButton;
    BtnRemoveStep: TButton;
    BtnSavePlanAs: TButton;
    BtnImport: TButton;
    BtnApplyToAll: TButton;
    BtnEndScript: TButton;
    cbNone: TCheckBox;
    cbStopTracking: TCheckBox;
    cbParkScope: TCheckBox;
    cbParkDome: TCheckBox;
    cbWarm: TCheckBox;
    cbScript: TCheckBox;
    cbUnattended: TCheckBox;
    CheckBoxResetRepeat: TCheckBox;
    CheckBoxRestartStatus: TCheckBox;
    CheckBoxAutofocusTemp: TCheckBox;
    CheckBoxAutofocus: TCheckBox;
    CheckBoxAutofocusStart: TCheckBox;
    CheckBoxDither: TCheckBox;
    GroupBoxStep: TGroupBox;
    ImageListNight: TImageList;
    ImageListDay: TImageList;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel3: TPanel;
    Panel8: TPanel;
    PanelSep: TPanel;
    DitherCount: TSpinEdit;
    FlatBinning: TComboBox;
    BtnAnytime: TButton;
    BtnCdCCoord: TButton;
    BtnImgCoord: TButton;
    BtnCurrentCoord: TButton;
    BtnEditScript: TButton;
    BtnImgRot: TButton;
    BtnNewObject: TButton;
    BtnDeleteObject: TButton;
    BtnSave: TButton;
    BtnEditNewScript: TButton;
    BtnNewScript: TButton;
    BtnDeletePlan: TButton;
    BtnCancel: TButton;
    BtnSkyFlat: TButton;
    FlatFilterList: TCheckListBox;
    PGainEdit: TSpinEdit;
    GroupBoxTarget: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    PISObox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    LabelGain1: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    PanelPlan: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    PanelGain1: TPanel;
    PlanName: TLabel;
    SaveDialog1: TSaveDialog;
    BtnRepeatInf: TSpeedButton;
    TargetName: TLabel;
    PreviewExposure: TFloatSpinEdit;
    InplaceAutofocus: TCheckBox;
    FISObox: TComboBox;
    Label16: TLabel;
    Label18: TLabel;
    LabelGain: TLabel;
    FlatTime: TRadioGroup;
    PanelGain: TPanel;
    TDelay: TSpinEdit;
    RepeatCountList: TSpinEdit;
    FlatCount: TSpinEdit;
    FGainEdit: TSpinEdit;
    StepList: TStringGrid;
    TabSheet3: TTabSheet;
    UpdateCoord: TCheckBox;
    SeqStart: TCheckBox;
    SeqStopAt: TMaskEdit;
    SeqStop: TCheckBox;
    SeqStartTwilight: TCheckBox;
    SeqStopTwilight: TCheckBox;
    CheckBoxRepeatList: TCheckBox;
    Label11: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    PanelBottom: TPanel;
    PanelTarget: TPanel;
    ScriptList: TComboBox;
    PointAstrometry: TCheckBox;
    Preview: TCheckBox;
    SeqStartAt: TMaskEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TargetList: TStringGrid;
    procedure BtnAddStepClick(Sender: TObject);
    procedure BtnAnytimeClick(Sender: TObject);
    procedure BtnApplyToAllClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnCdCCoordClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeletePlanClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnEndScriptClick(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
    procedure BtnRemoveStepClick(Sender: TObject);
    procedure BtnImgCoordClick(Sender: TObject);
    procedure BtnImgRotClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSavePlanAsClick(Sender: TObject);
    procedure BtnSavePlanClick(Sender: TObject);
    procedure BtnSkyFlatClick(Sender: TObject);
    procedure BtnScriptClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnNewScriptClick(Sender: TObject);
    procedure BtnUnattendedScriptClick(Sender: TObject);
    procedure Btn_coord_internalClick(Sender: TObject);
    procedure cbTermOptionClick(Sender: TObject);
    procedure CheckLightOnlyChange(Sender: TObject);
    procedure CheckBoxRepeatListChange(Sender: TObject);
    procedure FlatTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PointCoordChange(Sender: TObject);
    procedure RepeatCountListChange(Sender: TObject);
    procedure SeqStartChange(Sender: TObject);
    procedure SeqStartTwilightChange(Sender: TObject);
    procedure SeqStopChange(Sender: TObject);
    procedure SeqStopTwilightChange(Sender: TObject);
    procedure BtnRepeatInfClick(Sender: TObject);
    procedure StepListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure StepListEditingDone(Sender: TObject);
    procedure StepListSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StepListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StepChange(Sender: TObject);
    procedure TargetChange(Sender: TObject);
    procedure TargetListCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure TargetListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure TargetListCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure TargetListEditingDone(Sender: TObject);
    procedure TargetListHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure TargetListSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure TargetListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure TargetListValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    { private declarations }
    FAstrometry: TAstrometry;
    LockTarget: boolean;
    FTargetsRepeat: integer;
    LockStep, StepsModified: boolean;
    Lockcb: boolean;
    originalFilter: array[0..99] of string;
    SortDirection: integer;
    FDoneWarning: boolean;
    procedure SetPlanList(n: integer; pl:string);
    procedure SetScriptList(n: integer; sl:string);
    procedure ResetSequences;
    procedure SetLang;
    procedure ResetSteps;
    procedure SetStep(n: integer; p: TStep);
    procedure CheckPlanModified;
    function  CheckRiseSet(n: integer; showmsg:boolean=true): boolean;
    procedure FrameTypeChange(n: integer; newtype: TFrameType);
    procedure SetStartTime(buf: string; var t:TTarget);
    procedure SetEndTime(buf: string; var t:TTarget);
  public
    { public declarations }
    procedure LoadPlanList;
    procedure LoadScriptList;
    procedure SetTarget(n: integer; t: TTarget);
    procedure ClearStepList;
    procedure ShowPlan;
    procedure SavePlan;
    procedure ReadStep(pfile:TCCDconfig; i: integer; var p:TStep; var msg:string);
    property TargetsRepeat: integer read FTargetsRepeat write FTargetsRepeat;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property DoneWarning: boolean read FDoneWarning write FDoneWarning;
  end;

var
  f_EditTargets: Tf_EditTargets;

implementation

uses LazFileUtils;

{$R *.lfm}

{ Tf_EditTargets }

procedure Tf_EditTargets.FormCreate(Sender: TObject);
begin
  {$ifdef lclcocoa}
  PageControl1.BorderSpacing.Around:=8;
  TargetList.FixedColor:=clBackground;
  StepList.FixedColor:=clBackground;
  {$endif}
  ScaleDPI(Self);
  TargetList.RowHeights[0]:=40;
  SetLang;
  LockTarget:=false;
  FTargetsRepeat:=1;
  LockStep:=false;
  StepsModified:=false;
  Lockcb:=false;
  SortDirection:=-1;
  f_selectscript:=Tf_selectscript.Create(self);
  cbStopTracking.Checked:=true;
  LoadPlanList;
  LoadScriptList;
end;

procedure Tf_EditTargets.FormDestroy(Sender: TObject);
begin
  // objects are destroyed in fu_sequence
  ClearStepList;
end;

procedure Tf_EditTargets.FormResize(Sender: TObject);
begin
  PanelTarget.Height:=(ClientHeight-Panel1.Height) div 2;
end;

procedure Tf_EditTargets.FormShow(Sender: TObject);
begin
  TargetList.Cells[colseq,0]:='Seq';
  StepList.Cells[0,0]:='Seq';
  if TargetList.RowCount>1 then begin
     TargetList.Row:=1;
     TargetListSelection(nil,0,1);
  end
  else begin
    PageControl1.ActivePageIndex:=0;
  end;
  SeqStopAt.Enabled:=SeqStop.Checked and (not SeqStopTwilight.Checked);
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
  RepeatCountList.Value:=FTargetsRepeat;
  CheckBoxRepeatList.Checked:=(FTargetsRepeat>1);
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
  BtnRepeatInf.Enabled:=CheckBoxRepeatList.Checked;
  if BtnEndScript.Hint='' then
     BtnEndScript.Caption:='.?.'
  else
     BtnEndScript.Caption:='...';
  if BtnUnattendedScript.Hint='' then
     BtnUnattendedScript.Caption:='.?.'
  else
     BtnUnattendedScript.Caption:='...';
end;

procedure Tf_EditTargets.SetLang;
begin
  Caption := rsEditTargetLi;
  BtnSave.Caption := rsSave;
  BtnSaveAs.Caption := rsSaveAs;
  BtnNewObject.Caption := rsNewObject;
  BtnDeleteObject.Caption := rsRemoveObject;
  BtnNewScript.Caption := rsNewScript;
  BtnCancel.Caption := rsCancel;
  BtnSkyFlat.Caption := rsSkyFlat;
  TargetList.Columns.Items[colname-1].Title.Caption := Format(rsTargetName, [crlf]);
  TargetList.Columns.Items[colplan-1].Title.Caption := rsPlan;
  TargetList.Columns.Items[colra-1].Title.Caption := rsRA;
  TargetList.Columns.Items[coldec-1].Title.Caption := rsDec;
  TargetList.Columns.Items[colpa-1].Title.Caption := rsPA;
  TargetList.Columns.Items[colstart-1].Title.Caption := rsBegin;
  TargetList.Columns.Items[colend-1].Title.Caption := rsEnd;
  TargetList.Columns.Items[coldark-1].Title.Caption := Format(rsDarkNight, [crlf]);
  TargetList.Columns.Items[colskip-1].Title.Caption := Format(rsDonTSwait+'', [crlf]);
  TargetList.Columns.Items[colrepeat-1].Title.Caption := rsRepeat;
  TargetList.Columns.Items[colstart-1].PickList.Clear;
  TargetList.Columns.Items[colstart-1].PickList.Add('');
  TargetList.Columns.Items[colstart-1].PickList.Add(rsRise);
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-4.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-3.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-2.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-1.5h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-1.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'-0.5h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing);
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+0.5h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+1.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+1.5h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+2.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+3.0h');
  TargetList.Columns.Items[colstart-1].PickList.Add(MeridianCrossing+'+4.0h');
  TargetList.Columns.Items[colend-1].PickList.Clear;
  TargetList.Columns.Items[colend-1].PickList.Add('');
  TargetList.Columns.Items[colend-1].PickList.Add(rsSet2);
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-4.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-3.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-2.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-1.5h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-1.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'-0.5h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing);
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+0.5h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+1.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+1.5h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+2.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+3.0h');
  TargetList.Columns.Items[colend-1].PickList.Add(MeridianCrossing+'+4.0h');
  CheckBoxRepeatList.Caption := rsRepeatTheWho;
  CheckBoxRestartStatus.Caption:=rsKeepCompleti;
  CheckBoxResetRepeat.Caption:=rsResetComplet;
  SeqStart.Caption := rsStartAt;
  SeqStop.Caption := rsStopAt;
  SeqStartTwilight.Caption := rsDusk;
  SeqStopTwilight.Caption := rsDawn;
  TabSheet1.Caption := rsObject;
  Label9.Caption := rsExposureTime2;
  Preview.Caption := rsPreviewWhenW;
  Label13.Caption := rsSeconds2;
  Label11.Caption := rsInterval;
  Label14.Caption := rsSeconds2;
  BtnAnytime.Caption := rsAnyTime;
  BtnCurrentCoord.Caption := rsNoMove;
  BtnCdCCoord.Caption := rsPlanetarium;
  Btn_coord_internal.Caption:=rsSearch;
  PointAstrometry.Caption := rsUseAstrometr;
  BtnImgCoord.Caption := rsCurrentImage;
  UpdateCoord.Caption := rsUpdateRADecF;
  InplaceAutofocus.Caption := rsStayInPlaceF;
  BtnApplyToAll.Caption:=rsApplyToAll;
  BtnImgRot.Caption := rsCurrentImage;
  TabSheet2.Caption := rsScript;
  Label15.Caption := rsScript;
  BtnEditScript.Caption := rsEdit;
  BtnEditNewScript.Caption := rsNew;
  FlatTime.Caption := rsFlatTime;
  Label16.Caption := rsBinning;
  GroupBox6.Caption := rsFilters;
  GroupBox7.Caption := rsExposure;
  Label18.Caption := rsCount;
  LabelGain.Caption := rsGain;
  FlatTime.Items[0]:=rsAtDusk;
  FlatTime.Items[1]:=rsAtDawn;
  label2.Caption:=rsSequence;
  GroupBoxTarget.Caption:=rsObject;
  GroupBox3.Caption:=rsPA;
  GroupBox4.Caption:=rsBegin+'/'+rsEnd;
  GroupBox5.Caption:=rsRepeat;
  // plan
  GroupBoxStep.Caption:=rsStep;
  CheckBoxDither.Caption := rsDitherEvery;
  CheckBoxAutofocusStart.Caption := rsAutofocusBef;
  CheckBoxAutofocus.Caption := rsAutofocusEve;
  CheckBoxAutofocusTemp.Caption := rsAutofocusWhe;
  LabelGain1.Caption := rsGain;
  BtnDeletePlan.Caption := rsDeletePlan;
  BtnSavePlan.Caption := rsSavePlan;
  BtnSavePlanAs.Caption:=rsSavePlanAs;
  BtnRemoveStep.Caption := rsRemoveStep;
  BtnAddStep.Caption := rsAddStep;
  StepList.Columns.Items[pcoldesc-1].Title.Caption := rsDescription;
  StepList.Columns.Items[pcoltype-1].Title.Caption := rsType;
  StepList.Columns.Items[pcolexp-1].Title.Caption := rsExposure;
  StepList.Columns.Items[pcolbin-1].Title.Caption := rsBinning;
  StepList.Columns.Items[pcolfilter-1].Title.Caption := rsFilter;
  StepList.Columns.Items[pcolcount-1].Title.Caption := rsCount;
  Label1.Caption := rsPlan;
  // termination options
  Label3.Caption:=rsTerminationO;
  cbNone.Caption:=rsDoNothing;
  cbStopTracking.Caption:=rsStopTelescop2;
  cbParkScope.Caption:=rsParkTheTeles2;
  cbParkDome.Caption:=rsParkAndClose;
  cbWarm.Caption:=rsWarmTheCamer;
  cbScript.Caption:=rsRunAScript;
  Label4.Caption:=rsUnattendedEr;
  // hint
  BtnCurrentCoord.Hint:=rsDoNotMoveThe;
  BtnCdCCoord.Hint:=rsGetTheCoordi;
  PointAstrometry.Hint:=rsUsePlateSolv;
  BtnImgCoord.Hint:=rsSolveTheCurr;
  UpdateCoord.Hint:=Format(rsForMovingObj, [crlf]);
  InplaceAutofocus.Hint:=Format(rsYouCanAvoidT, [crlf]);
  BtnApplyToAll.Hint:=rsApplyThisSet;
  Btn_coord_internal.Hint:=rsGetTheCoordi2;
  Preview.Hint:=rsStartAPrevie;
  BtnImgRot.Hint:=rsSolveTheCurr2;
  BtnAnytime.Hint:=rsClearTimeCon;
  FISObox.Hint:=rsCameraISO;
  FGainEdit.Hint:=rsCameraGain;
  CheckBoxRepeatList.Hint:=rsRepeatTheWho2;
  BtnRepeatInf.Hint:=rsInfiniteNumb;
  SeqStart.Hint:=rsActivateTheS;
  SeqStartAt.Hint:=rsWaitToThisTi;
  SeqStartTwilight.Hint:=rsSetTheStartT;
  SeqStop.Hint:=rsActivateTheS2;
  SeqStopAt.Hint:=rsStopAtThisTi;
  SeqStopTwilight.Hint:=rsSetTheStopTi;
  CheckBoxRestartStatus.Hint:=rsThisAllowToR;
  CheckBoxResetRepeat.Hint:=rsControlHowCo;
  BtnNewObject.Hint:=rsAddAnObjectT;
  BtnDeleteObject.Hint:=rsDeleteTheSel;
  BtnNewScript.Hint:=rsAddAScriptTo;
  BtnSkyFlat.Hint:=rsAddAFlatSequ;
  TargetList.Hint:=rsTheListOfTar;
  PGainEdit.Hint:=rsCameraGain;
  PISObox.Hint:=rsCameraISO;
  BtnSave.Hint:=rsSaveTheListA;
  BtnSaveAs.Hint:=rsSaveTheListW;



end;

procedure Tf_EditTargets.PointCoordChange(Sender: TObject);
begin
//  if ObjStartRise.Checked then ObjStartRiseChange(Sender);
//  if ObjEndSet.Checked then ObjEndSetChange(Sender);
  TargetChange(Sender);
end;

procedure Tf_EditTargets.LoadPlanList;
var i: integer;
    fs : TSearchRec;
    s: TStringlist;
begin
  s:=TStringlist.Create;
  TargetList.Columns[colplan-1].PickList.Clear;
  i:=FindFirstUTF8(slash(ConfigDir)+'*.plan',0,fs);
  while i=0 do begin
    s.Add(ExtractFileNameOnly(fs.Name));
    i:=FindNextUTF8(fs);
  end;
  FindCloseUTF8(fs);
  s.Sorted:=true;
  TargetList.Columns[colplan-1].PickList.Assign(s);
  s.Free;
end;

procedure Tf_EditTargets.SetPlanList(n: integer; pl:string);
var i:integer;
begin
  i:=TargetList.Columns[colplan-1].PickList.IndexOf(pl);
  if i>=0 then TargetList.Cells[colplan,n]:=pl;
end;

procedure Tf_EditTargets.LoadScriptList;
var i,k: integer;
    fs : TSearchRec;
    s: TStringlist;
    scr: string;
begin
  s:=TStringlist.Create;
  ScriptList.Clear;
  for k:=1 to MaxScriptDir do begin
    i:=FindFirstUTF8(ScriptDir[k].path+'*.script',0,fs);
    while i=0 do begin
      scr:=ExtractFileNameOnly(fs.Name);
      if s.IndexOf(scr)<0 then begin
        s.AddObject(scr,ScriptDir[k]);
      end;
      i:=FindNextUTF8(fs);
    end;
    FindCloseUTF8(fs);
  end;
  s.CustomSort(@ScriptListCompare);
  ScriptList.Items.Assign(s);
  ScriptList.ItemIndex:=0;
  f_selectscript.ComboBoxScript.Items.Assign(s);
  s.Free;
end;

procedure Tf_EditTargets.SetScriptList(n: integer; sl:string);
var i:integer;
begin
  i:=ScriptList.Items.IndexOf(sl);
  if i>=0 then ScriptList.ItemIndex:=i;
  TargetList.Cells[colplan,n]:=sl;
end;

procedure Tf_EditTargets.BtnDeletePlanClick(Sender: TObject);
var txt,fn: string;
    n: integer;
begin
  n:=TargetList.Row;
  txt:=TargetList.Cells[colplan,n];
  fn:=slash(ConfigDir)+txt+'.plan';
  if MessageDlg(Format(rsDoYouWantToD, [fn]), mtConfirmation, mbYesNo, 0)=mrYes
    then begin
     DeleteFileUTF8(fn);
     LoadPlanList;
     if TargetList.Columns[colplan-1].PickList.Count>0 then
       TargetList.Cells[colplan,n]:=TargetList.Columns[colplan-1].PickList[0]
     else
       TargetList.Cells[colplan,n]:='';
     PlanName.Caption:=TargetList.Cells[colplan,n];
     ShowPlan;
     TargetChange(nil);
     StepsModified:=false;
  end;
end;

procedure Tf_EditTargets.BtnImportClick(Sender: TObject);
var obj:string;
    i,n: integer;
    t,tt: TTarget;
    f: textfile;
    title, buf, buf1: string;
    ra, de: double;
const
  objl = 32;
  radecl = 10;
begin
  // Import Cartes du Ciel observation list
  if OpenDialog1.Execute then begin
     AssignFile(f, UTF8ToSys(OpenDialog1.FileName));
     reset(f);
     readln(f, title);
     while not EOF(f) do
     begin
       // read object
       readln(f, buf);
       buf1 := copy(buf, 1, objl);
       obj := trim(buf1);
       if (obj=ScriptTxt)or(obj=SkyFlatTxt) then continue;
       Delete(buf, 1, objl);
       buf1 := trim(copy(buf, 1, radecl));
       ra := strtofloatdef(buf1, -999);
       if ra < -900 then continue;
       Delete(buf, 1, radecl);
       buf1 := trim(copy(buf, 1, radecl));
       de := strtofloatdef(buf1, -999);
       if de < -900 then continue;
       ra:=ra/15;
       // create new target
       t:=TTarget.Create;
       n:=TargetList.Row;
       if n>=1 then begin
         // copy current target
         tt:=TTarget(TargetList.Objects[colseq,n]);
         if (tt.objectname<>ScriptTxt) and (tt.objectname<>SkyFlatTxt) then t.Assign(tt);
       end;
       // assign name and coordinates J2000
       t.objectname:=obj;
       t.ra:=ra;
       t.de:=de;
       // default autofocus and plate solving
       t.astrometrypointing:=(astrometryResolver<>ResolverNone);
       t.inplaceautofocus:=AutofocusInPlace;
       // add target
       TargetList.RowCount:=TargetList.RowCount+1;
       i:=TargetList.RowCount-1;
       TargetList.Cells[colseq,i]:=IntToStr(i);
       TargetList.Cells[colname,i]:=obj;
       TargetList.Cells[colplan,i]:=t.planname;
       TargetList.Objects[colseq,i]:=t;
       TargetList.Row:=i;
       SetTarget(i,t);
     end;
     CloseFile(f);
  end;
end;

procedure Tf_EditTargets.BtnNewObjectClick(Sender: TObject);
var i,n: integer;
    t,tt: TTarget;
begin
  PageControl1.ActivePageIndex:=0;
  t:=TTarget.Create;
  n:=TargetList.Row;
  if n>=1 then begin
    tt:=TTarget(TargetList.Objects[colseq,n]);
    if (tt.objectname<>ScriptTxt) and (tt.objectname<>SkyFlatTxt) then begin
      t.Assign(tt);
      t.objectname:='';
    end;
  end;
  if (t.planname='')and(TargetList.Columns[colplan-1].PickList.Count>0) then
     t.planname:=TargetList.Columns[colplan-1].PickList[0];
  TargetList.RowCount:=TargetList.RowCount+1;
  i:=TargetList.RowCount-1;
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:='';
  TargetList.Cells[colplan,i]:=t.planname;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
  Btn_coord_internalClick(Sender);
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
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:=ScriptTxt;
  TargetList.Cells[colplan,i]:=t.planname;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
end;

procedure Tf_EditTargets.BtnEndScriptClick(Sender: TObject);
begin
  f_selectscript.SetScript(BtnEndScript.Hint);
  FormPos(f_selectscript,mouse.CursorPos.X,mouse.CursorPos.Y);
  if f_selectscript.ShowModal=mrOK then begin
     BtnEndScript.Hint:=f_selectscript.ComboBoxScript.Items[f_selectscript.ComboBoxScript.ItemIndex];
  end;
  if BtnEndScript.Hint='' then
     BtnEndScript.Caption:='.?.'
  else
     BtnEndScript.Caption:='...';
end;

procedure Tf_EditTargets.BtnUnattendedScriptClick(Sender: TObject);
begin
  f_selectscript.SetScript(BtnUnattendedScript.Hint);
  FormPos(f_selectscript,mouse.CursorPos.X,mouse.CursorPos.Y);
  if f_selectscript.ShowModal=mrOK then begin
     BtnUnattendedScript.Hint:=f_selectscript.ComboBoxScript.Items[f_selectscript.ComboBoxScript.ItemIndex];
  end;
  if BtnUnattendedScript.Hint='' then
     BtnUnattendedScript.Caption:='.?.'
  else
     BtnUnattendedScript.Caption:='...';
end;

procedure Tf_EditTargets.Btn_coord_internalClick(Sender: TObject);{Retrieve position from deepsky database}
var n: integer;
    ra0,dec0,length0,width0,pa : double;
    objname : string;
begin
  n:=TargetList.Row;

  objname:=uppercase(inputbox('Retrieve position from deepsky database','Object:' , ''));
  if length(objname)>1 then {Object name length should be two or longer}
  begin
    load_deep;{Load the deepsky database once. If already loaded, no action}
    linepos:=0;{Set pointer to the beginning}
    repeat
      read_deepsky('T' {full database search} ,0 {ra},0 {dec},1 {cos(telescope_dec)},2*pi{fov},{var} ra0,dec0,length0,width0,pa);{Deepsky database search}
      if ((objname=uppercase(naam2)) or (objname=uppercase(naam3)) or (objname=uppercase(naam4))) then
      begin
        TargetList.Cells[colra,n]:=RAToStr(ra0*12/pi);{Add position}
        TargetList.Cells[coldec,n]:=DEToStr(dec0*180/pi);

        if naam3='' then TargetList.Cells[colname,n]:=naam2 {Add one object name only}
        else
        TargetList.Cells[colname,n]:=naam2+'_'+naam3;{Add two object names}

        linepos:=$FFFFFF; {Stop searching}
        TargetChange(nil);
     end;
    until linepos>=$FFFFFF;{Found object or end of database}
  end;
end;

procedure Tf_EditTargets.cbTermOptionClick(Sender: TObject);
begin
  if not(Sender is TCheckBox) then exit;
  if Lockcb then exit;
  Lockcb:=true;
  if (Sender=cbNone) then begin
     cbNone.Checked:=true;
     cbStopTracking.Checked:=false;
     cbParkScope.Checked:=false;
     cbParkDome.Checked:=false;
     cbWarm.Checked:=false;
     cbScript.Checked:=false;
  end;
  if (Sender<>cbNone) and TCheckBox(Sender).Checked then begin
     cbNone.Checked:=false;
  end;
  if (Sender=cbStopTracking) and cbStopTracking.Checked then begin
     cbParkScope.Checked:=false;
  end;
  if (Sender=cbParkScope) and cbParkScope.Checked then begin
     cbStopTracking.Checked:=false;
  end;
  Lockcb:=false;
end;

procedure Tf_EditTargets.BtnSkyFlatClick(Sender: TObject);
var ft:string;
    i,j,n,k: integer;
    t: TTarget;
begin
  if FlatType<>ftSKY then begin
     ShowMessage(rsYouMustConfi);
     exit;
  end;
  n:=0;
  for i:=1 to TargetList.RowCount-1 do
    if TargetList.Cells[colname,i]=SkyFlatTxt then begin
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
    SeqStartTwilight.Checked:=false;
    SeqStart.Checked:=false;
  end
  else if n=1 then begin
    LockTarget:=true;
    if k=1 then begin
      TargetList.RowCount:=TargetList.RowCount+1;
      i:=TargetList.RowCount-1;
      FlatTime.ItemIndex:=1;
      ft:=FlatTimeName[1];
      SeqStopTwilight.Checked:=false;
      SeqStop.Checked:=false;
    end
    else begin
      TargetList.InsertColRow(false,1);
      i:=1;
      FlatTime.ItemIndex:=0;
      ft:=FlatTimeName[0];
      SeqStartTwilight.Checked:=false;
      SeqStart.Checked:=false;
    end;
    LockTarget:=false;
  end
  else begin // no more than two flat series
    ShowMessage(rsCanOnlyAddOn);
    exit;
  end;
  PageControl1.ActivePageIndex:=2;
  t:=TTarget.Create;
  t.objectname:=SkyFlatTxt;
  t.planname:=ft;
  t.FlatFilters:='';
  for j:=0 to FilterList.Count-1 do begin
    if trim(FilterList[j])<>'' then
       t.FlatFilters:=t.FlatFilters+FilterList[j]+';';
  end;
  t.FlatBinX:=1;
  t.FlatBinY:=1;
  t.FlatGain:=Gain;
  t.FlatCount:=15;
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:=SkyFlatTxt;
  TargetList.Cells[colplan,i]:=t.planname;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
  ResetSequences;
  TargetListSelection(Sender,0,i);
end;

procedure Tf_EditTargets.BtnScriptClick(Sender: TObject);
var txt,fn: string;
    scdir:TScriptDir;
    i,n:integer;
    newscript: boolean;
    s: TStringList;
begin
  n:=TargetList.Row;
  newscript:=(Sender=BtnEditNewScript)or(ScriptList.Text='');
  s:=TStringList.Create;
  if f_pascaleditor=nil then begin
     f_pascaleditor:=Tf_pascaleditor.Create(self);
     f_pascaleditor.DebugScript:=f_scriptengine.dbgscr;
  end;
  if newscript then begin
    s.Clear;
    txt:=FormEntry(self, rsNewScript, '');
    if txt='' then exit;
    scdir:=ScriptDir[1];
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=scdir.path+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg(Format(rsScriptAlread2, [txt]), mtConfirmation, mbYesNo, 0)
         =mrYes then
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
            if MessageDlg(Format(rsScriptAlread3, [fn]), mtConfirmation,
              mbYesNo, 0)<>mrYes then
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
     SetScriptList(n,f_pascaleditor.ScriptName);
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
     str:=TargetList.Cells[colseq,i]+', '+TargetList.Cells[colname,i];
     if MessageDlg(Format(rsDeleteSequen, [str]), mtConfirmation, mbYesNo, 0)=
       mrYes then begin
        j:=i-1;
        if j<1 then j:=i+1;
        if j>=TargetList.RowCount then j:=TargetList.RowCount-1;
        TargetList.Row:=j;
        Application.ProcessMessages;
        LockTarget:=true;
        //if TargetList.Objects[0,i]<>nil then TargetList.Objects[0,i].Free; Cause crash because plan is shared
        TargetList.Objects[colseq,i]:=nil;
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
    TargetList.Cells[colseq,i]:=IntToStr(i);
  end;
end;

procedure Tf_EditTargets.BtnAnytimeClick(Sender: TObject);
begin
  TargetList.Cells[colstart,TargetList.Row]:='';
  TargetList.Cells[colend,TargetList.Row]:='';
  TargetChange(nil);
end;

procedure Tf_EditTargets.BtnApplyToAllClick(Sender: TObject);
var i: integer;
    t: TTarget;
begin
  with TargetList do begin
   for i:=1 to RowCount-1 do
      if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
        t:=TTarget(Objects[colseq,i]);
        t.astrometrypointing := (PointAstrometry.Checked and (t.ra<>NullCoord) and (t.de<>NullCoord));
        t.updatecoord := UpdateCoord.Checked;
        t.inplaceautofocus := InplaceAutofocus.Checked;
      end;
  end;
end;

procedure Tf_EditTargets.BtnCancelClick(Sender: TObject);
begin
  CheckPlanModified;
  ModalResult:=mrCancel;
end;

procedure Tf_EditTargets.BtnCdCCoordClick(Sender: TObject);
var n: integer;
begin
  n:=TargetList.Row;
  f_planetariuminfo.Ra.Text  := TargetList.Cells[colra,n];
  f_planetariuminfo.De.Text  := TargetList.Cells[coldec,n];
  f_planetariuminfo.Obj.Text := TargetList.Cells[colname,n];
  FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_planetariuminfo.ShowModal;
  if f_planetariuminfo.ModalResult=mrOK then begin
     TargetList.Cells[colra,n]:=f_planetariuminfo.Ra.Text;
     TargetList.Cells[coldec,n]:=f_planetariuminfo.De.Text;
     if f_planetariuminfo.Obj.Text<>'' then TargetList.Cells[colname,n]:=trim(f_planetariuminfo.Obj.Text);
     PointAstrometry.Checked:=(astrometryResolver<>ResolverNone);
     TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.BtnImgCoordClick(Sender: TObject);
var ra,de,eq,pa: double;
    n: integer;
begin
try
  n:=TargetList.Row;
  screen.Cursor:=crHourGlass;
  FAstrometry.SolveCurrentImage(true);
  if FAstrometry.CurrentCoord(ra,de,eq,pa) then begin
    ra:=ra*15*deg2rad;
    de:=de*deg2rad;
    J2000ToApparent(ra,de);
    ra:=rad2deg*ra/15;
    de:=rad2deg*de;
    TargetList.Cells[colra,n]:=RAToStr(ra);
    TargetList.Cells[coldec,n]:=DEToStr(de);
    PointAstrometry.Checked:=(astrometryResolver<>ResolverNone);
    TargetChange(nil);
  end;
finally
  screen.Cursor:=crDefault;
end;
end;

procedure Tf_EditTargets.BtnImgRotClick(Sender: TObject);
var ra,de,eq,pa: double;
    n: integer;
begin
try
  n:=TargetList.Row;
  screen.Cursor:=crHourGlass;
  FAstrometry.SolveCurrentImage(true);
  if FAstrometry.CurrentCoord(ra,de,eq,pa) then begin
    TargetList.Cells[colpa,n]:=FormatFloat(f2,pa);
    TargetChange(nil);
  end;
finally
  screen.Cursor:=crDefault;
end;
end;

procedure Tf_EditTargets.BtnSaveAsClick(Sender: TObject);
var defaultname: string;
    i,n: integer;
begin
CheckPlanModified;
SaveDialog1.InitialDir:=ConfigDir;
SaveDialog1.DefaultExt:='.targets';
SaveDialog1.filter:='CCDciel Sequence|*.targets';
if CurrentSequenceFile='' then begin
  n:=TargetList.RowCount;
  defaultname:=FormatDateTime('mmdd',now);
  for i:=1 to n-1 do begin
    if (TargetList.Cells[1,i]<>ScriptTxt) and (TargetList.Cells[1,i]<>SkyFlatTxt) then
       defaultname:=TargetList.Cells[1,i];
  end;
  SaveDialog1.FileName:=slash(ConfigDir)+defaultname+'.targets';
end
else
  SaveDialog1.FileName:=CurrentSequenceFile;

if SaveDialog1.Execute then begin
  CurrentSequenceFile:=SaveDialog1.FileName;
  ModalResult:=mrOK;
end;
end;

procedure Tf_EditTargets.BtnSaveClick(Sender: TObject);
begin
  CheckPlanModified;
  if FDoneWarning then begin
    if MessageDlg(Format(rsThisSequence, [''])+crlf+Format(rsThisWillBeLo, [crlf]), mtConfirmation, mbYesNo, 0)=mryes then begin
       BtnSaveAsClick(Sender);
       exit;
    end;
  end;
  ModalResult:=mrOK;
end;

procedure Tf_EditTargets.BtnCurrentCoordClick(Sender: TObject);
begin
  TargetList.Cells[colra,TargetList.Row]:='-';
  TargetList.Cells[coldec,TargetList.Row]:='-';
  TargetChange(nil);
end;

procedure Tf_EditTargets.CheckPlanModified;
begin
if StepsModified then begin
  if MessageDlg(Format(rsThePlanIsMod, [PlanName.Caption]), mtConfirmation, mbYesNo, 0)=mrYes then begin
     SavePlan;
  end;
  StepsModified:=false;
end;
end;

procedure Tf_EditTargets.SetTarget(n: integer; t: TTarget);
var i,j:integer;
    buf: string;
    filterlst:TStringList;
begin
  if (t=nil)or(n=0)or(n>=TargetList.RowCount) then exit;
  CheckPlanModified;
  LockTarget:=true;
  TargetList.Cells[colseq,n]:=IntToStr(n);
  TargetList.Cells[colname,n]:=t.objectname;
  if t.objectname=ScriptTxt then begin
    TargetList.Cells[colra,n]:='';
    TargetList.Cells[coldec,n]:='';
    TargetList.Cells[colpa,n]:='';
    TargetList.Cells[colstart,n]:='';
    TargetList.Cells[colend,n]:='';
    TargetList.Cells[coldark,n]:='';
    TargetList.Cells[colskip,n]:='';
    TargetList.Cells[colrepeat,n]:='';
    PanelPlan.Visible:=false;
    PageControl1.ActivePageIndex:=1;
    SetScriptList(n,t.planname);
  end
  else if t.objectname=SkyFlatTxt then begin
    PanelPlan.Visible:=false;
    PageControl1.ActivePageIndex:=2;
    if t.planname=FlatTimeName[0]
       then FlatTime.ItemIndex:=0
       else FlatTime.ItemIndex:=1;
    TargetList.Cells[colplan,n]:=t.planname;
    TargetList.Cells[colra,n]:='';
    TargetList.Cells[coldec,n]:='';
    TargetList.Cells[colpa,n]:='';
    TargetList.Cells[colstart,n]:='';
    TargetList.Cells[colend,n]:='';
    TargetList.Cells[coldark,n]:='';
    TargetList.Cells[colskip,n]:='';
    TargetList.Cells[colrepeat,n]:='';
    FlatCount.Value:=t.FlatCount;
    buf:=inttostr(t.FlatBinX)+'x'+inttostr(t.FlatBinY);
    j:=FlatBinning.Items.IndexOf(buf);
    if j<0 then
      j:=FlatBinning.Items.Add(buf);
    FlatBinning.ItemIndex:=j;
    if hasGainISO then
      FISObox.ItemIndex:=t.FlatGain
    else
      FGainEdit.Value:=t.FlatGain;
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
    filterlst.Free;
  end
  else begin
    PanelPlan.Visible:=true;
    PageControl1.ActivePageIndex:=0;
    SetPlanList(n,t.planname);
    if t.starttime>=0 then
       TargetList.Cells[colstart,n]:=TimeToStr(t.starttime)
    else
       TargetList.Cells[colstart,n]:='';
    if t.endtime>=0 then
       TargetList.Cells[colend,n]:=TimeToStr(t.endtime)
    else
       TargetList.Cells[colend,n]:='';
    if t.ra=NullCoord then
      TargetList.Cells[colra,n]:='-'
    else
      TargetList.Cells[colra,n]:=RAToStr(t.ra);
    if t.de=NullCoord then
      TargetList.Cells[coldec,n]:='-'
    else
      TargetList.Cells[coldec,n]:=DEToStr(t.de);
    if t.startrise then TargetList.Cells[colstart,n]:=rsRise;
    if t.endset then TargetList.Cells[colend,n]:=rsSet2;
    if t.startmeridian<>NullCoord then begin
       if abs(t.startmeridian)>5 then t.startmeridian:=sgn(t.startmeridian)*5;
       TargetList.Cells[colstart,n]:=MeridianCrossing+formatfloat(f1mc,t.startmeridian);
    end;
    if t.endmeridian<>NullCoord then begin
      if abs(t.endmeridian)>5 then t.endmeridian:=sgn(t.endmeridian)*5;
      TargetList.Cells[colend,n]:=MeridianCrossing+formatfloat(f1mc,t.endmeridian);
    end;
    if t.darknight then
      TargetList.Cells[coldark,n]:='1'
    else
      TargetList.Cells[coldark,n]:='0';
    if t.skip then
      TargetList.Cells[colskip,n]:='1'
    else
      TargetList.Cells[colskip,n]:='0';
    PointAstrometry.Checked:=t.astrometrypointing;
    UpdateCoord.Checked:=t.updatecoord;
    InplaceAutofocus.Checked:=t.inplaceautofocus;
    if t.pa=NullCoord then
      TargetList.Cells[colpa,n]:='-'
    else
      TargetList.Cells[colpa,n]:=FormatFloat(f2,t.pa);
    TargetList.Cells[colrepeat,n]:=IntToStr(t.repeatcount);
    GroupBox5.Visible:=t.repeatcount>1;
    TDelay.Value:=t.delay;
    PreviewExposure.Value:=t.previewexposure;
    Preview.Checked:=t.preview;
    PlanName.Caption:=TargetList.Cells[colplan,n];
    ShowPlan;
  end;
  LockTarget:=false;
end;

procedure Tf_EditTargets.TargetListSelection(Sender: TObject; aCol, aRow: Integer);
var t: TTarget;
begin
  t:=TTarget(TargetList.Objects[colseq,aRow]);
  SetTarget(aRow,t);
  GroupBoxTarget.Caption:=rsObject+blank+TargetList.Cells[1,aRow];
end;

procedure Tf_EditTargets.TargetListValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if (TargetList.Cells[colname,aRow]=SkyFlatTxt)or(TargetList.Cells[colname,aRow]=ScriptTxt) then begin
     if aCol=coldark then NewValue:='';
     if aCol=colskip then NewValue:='';
  end;
end;

procedure Tf_EditTargets.SetStartTime(buf: string; var t:TTarget);
var i,j: integer;
begin
  i:=pos(MeridianCrossing,buf);
  if i=1 then begin
     Delete(buf,1,length(MeridianCrossing));
     j:=pos('h',buf);
     if j>0 then buf:=copy(buf,1,j-1);
     t.startmeridian:=StrToFloatDef(buf,0);
     t.starttime:=-1;
     t.startrise:=false;
  end
  else begin
    t.starttime:=StrToTimeDef(buf,-1);
    t.startrise:=buf=rsRise;
    t.startmeridian:=NullCoord;
  end;
end;

procedure Tf_EditTargets.SetEndTime(buf: string; var t:TTarget);
var i,j: integer;
begin
  i:=pos(MeridianCrossing,buf);
  if i=1 then begin
    Delete(buf,1,length(MeridianCrossing));
    j:=pos('h',buf);
    if j>0 then buf:=copy(buf,1,j-1);
    t.endmeridian:=StrToFloatDef(buf,0);
    t.endtime:=-1;
    t.endset:=false;
  end
  else begin
    t.endtime:=StrToTimeDef(buf,-1);
    t.endset:=buf=rsSet2;
    t.endmeridian:=NullCoord;
  end;
end;

procedure Tf_EditTargets.TargetChange(Sender: TObject);
var i,n,j:integer;
    scdir:TScriptDir;
    sname,str,buf: string;
    t: TTarget;
    planchange: boolean;
begin
  if LockTarget then exit;
  // is table empty?
  if TargetList.RowCount<=1 then exit;
  n:=TargetList.Row;
  // is title row?
  if n < 1 then exit;
  t:=TTarget(TargetList.Objects[colseq,n]);
  if t=nil then exit;
  if t.objectname=ScriptTxt then begin
    PageControl1.ActivePageIndex:=1;
    i:=ScriptList.ItemIndex;
    sname:=ScriptList.Items[i];
    TargetList.Cells[colplan,n]:=sname;
    scdir:=TScriptDir(ScriptList.Items.Objects[i]);
    t.planname:=sname;
    if scdir=nil then t.path:=''
                 else t.path:=scdir.path;
  end
  else if t.objectname=SkyFlatTxt then begin
    PageControl1.ActivePageIndex:=2;
    t.planname:=FlatTimeName[FlatTime.ItemIndex];
    t.FlatCount:=FlatCount.Value;
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
    if hasGainISO then begin
       t.FlatGain:=FISObox.ItemIndex;
    end
    else begin
       t.FlatGain:=FGainEdit.Value;
    end;
    t.FlatFilters:='';
    for j:=0 to FlatFilterList.Count-1 do begin
      if FlatFilterList.Checked[j] then
         t.FlatFilters:=t.FlatFilters+FlatFilterList.Items[j]+';';
    end;
  end
  else begin
    PageControl1.ActivePageIndex:=0;
    CheckRiseSet(n);
    t.objectname:=trim(TargetList.Cells[colname,n]);
    planchange:=(t.planname<>TargetList.Cells[colplan,n]);
    t.planname:=TargetList.Cells[colplan,n];
    SetStartTime(trim(TargetList.Cells[colstart,n]),t);
    SetEndTime(trim(TargetList.Cells[colend,n]),t);
    if TargetList.Cells[colra,n]='-' then
      t.ra:=NullCoord
    else
      t.ra:=StrToAR(TargetList.Cells[colra,n]);
    if TargetList.Cells[coldec,n]='-' then
      t.de:=NullCoord
    else
      t.de:=StrToDE(TargetList.Cells[coldec,n]);
    t.darknight:=(TargetList.Cells[coldark,n]='1');
    t.skip:=(TargetList.Cells[colskip,n]='1');
    if TargetList.Cells[colpa,n]='-' then
      t.pa:=NullCoord
    else
      t.pa:=StrToFloatDef(TargetList.Cells[colpa,n],t.pa);
    if PointAstrometry.Checked and ((t.ra=NullCoord)or(t.de=NullCoord)) then PointAstrometry.Checked:=false;
    t.astrometrypointing:=PointAstrometry.Checked;
    t.updatecoord:=UpdateCoord.Checked;
    t.inplaceautofocus:=InplaceAutofocus.Checked;
    t.repeatcount:=StrToIntDef(TargetList.Cells[colrepeat,n],1);
    if t.repeatcount<0 then t.repeatcount:=1;
    GroupBox5.Visible:=t.repeatcount>1;
    t.delay:=TDelay.Value;
    t.previewexposure:=PreviewExposure.Value;
    t.preview:=Preview.Checked;
    if planchange then begin
      PlanName.Caption:=t.planname;
      ShowPlan;
    end;
  end;
end;

procedure Tf_EditTargets.TargetListCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
if (TargetList.Cells[colname,aRow]=SkyFlatTxt)or(TargetList.Cells[colname,aRow]=ScriptTxt) then begin
  TargetList.Cells[colskip,aRow]:='';
  TargetList.Cells[coldark,aRow]:='';
end
else begin
  if aCol=coldark then begin
    if TargetList.Cells[coldark,aRow]='1' then
      TargetList.Cells[colskip,aRow]:='1';
    if (TargetList.Cells[colstart,aRow]='')and(TargetList.Cells[colend,aRow]='')and(TargetList.Cells[coldark,aRow]='0') then
       TargetList.Cells[colskip,aRow]:='0';
  end;
  if aCol=colskip then begin
    if (TargetList.Cells[colstart,aRow]='')and(TargetList.Cells[colend,aRow]='')and(TargetList.Cells[coldark,aRow]='0') then
       TargetList.Cells[colskip,aRow]:='0';
    if TargetList.Cells[colskip,aRow]='0' then
      TargetList.Cells[coldark,aRow]:='0';
  end;
  if sender<>nil then TargetChange(Sender);
end;
end;

function Tf_EditTargets.CheckRiseSet(n: integer; showmsg:boolean=true): boolean;
var ra,de,h: double;
    i:integer;
begin
result:=true;
if (TargetList.Cells[colstart,n]='')and(TargetList.Cells[colend,n]='')and(TargetList.Cells[coldark,n]='0') then
   TargetList.Cells[colskip,n]:='0';
ra:=StrToAR(TargetList.Cells[colra,n]);
de:=StrToDE(TargetList.Cells[coldec,n]);
if TargetList.Cells[colstart,n]=rsRise then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then ShowMessage(rsCannotComput+crlf+rsInvalidObjec);
     TargetList.Cells[colstart,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=1 then begin
        if showmsg then ShowMessage(rsThisObjectIs);
      end
      else begin
        if showmsg then ShowMessage(rsThisObjectIs2);
      end;
      TargetList.Cells[colstart,n]:='';
      result:=false;
    end;
  end;
end;
if TargetList.Cells[colend,n]=rsSet2 then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then ShowMessage(rsCannotComput+crlf+rsInvalidObjec);
     TargetList.Cells[colend,n]:='';
     result:=false;
  end
  else begin
    if not ObjSet(ra,de,h,i) then begin
      if i=1 then begin
        if showmsg then ShowMessage(rsThisObjectIs);
      end
      else begin
        if showmsg then ShowMessage(rsThisObjectIs2);
      end;
      TargetList.Cells[colend,n]:='';
      result:=false;
    end;
  end;
end;
if copy(trim(TargetList.Cells[colstart,n]),1,length(MeridianCrossing))=MeridianCrossing then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then ShowMessage(rsCannotComput2+crlf+rsInvalidObjec);
     TargetList.Cells[colstart,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=2 then begin
        if showmsg then ShowMessage(rsThisObjectIs2);
      end;
      TargetList.Cells[colstart,n]:='';
      result:=false;
    end;
  end;
end;
if copy(trim(TargetList.Cells[colend,n]),1,length(MeridianCrossing))=MeridianCrossing then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then ShowMessage(rsCannotComput2+crlf+rsInvalidObjec);
     TargetList.Cells[colend,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=2 then begin
        if showmsg then ShowMessage(rsThisObjectIs2);
      end;
      TargetList.Cells[colend,n]:='';
      result:=false;
    end;
  end;
end;
end;

procedure Tf_EditTargets.TargetListColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  ResetSequences;
end;

procedure Tf_EditTargets.TargetListCompareCells(Sender: TObject; ACol, ARow,
  BCol, BRow: Integer; var Result: integer);
var v1,v2: float;
    b1, b2, p1, p2, buf, c: string;
    n1, n2: double;
    p, i1, i2: integer;

  procedure GetPrefix(str: string; var pref: string; var n: double; var i: integer);
    var
      j: integer;
    begin
      i := 1;
      n := 0;
      pref := '';
      buf := trim(str);
      p := pos(' ', buf);   // prefix separated by space
      if p = 0 then
      begin  // try to separate the numeric part
        for j := 1 to Length(buf) do
        begin
          c := copy(buf, j, 1);
          if (c >= '0') and (c <= '9') then
          begin
            p := j - 1;
            break;
          end;
        end;
      end;
      if p > 0 then
      begin  // first prefix
        pref := uppercase(trim(copy(buf, 1, p)));
        Delete(buf, 1, p);
        j:=pos(',',buf);
        if j>0 then buf:=copy(buf,1,j-1);
        j:=pos('_',buf);
        if j>0 then buf:=copy(buf,1,j-1);
        j:=pos('-',buf);
        if j>0 then buf:=copy(buf,1,j-1);
        Val(trim(buf), n, i);
      end;
    end;

begin
 with TargetList do begin
   case ACol of
     colra: begin
              v1:=StrToAR(Cells[ACol,ARow]);
              v2:=StrToAR(Cells[BCol,BRow]);
              Result:=CompareValue(v1,v2);
            end;
     coldec:begin
              v1:=StrToDE(Cells[ACol,ARow]);
              v2:=StrToDE(Cells[BCol,BRow]);
              Result:=CompareValue(v1,v2);
            end;
     colname:begin
              b1:=Cells[ACol,ARow];
              b2:=Cells[BCol,BRow];
              Val(trim(b1), n1, i1);
              Val(trim(b2), n2, i2);
              if (i1 = 0) and (i2 = 0) then
              begin
                // numeric compare
                if n1 > n2 then
                  Result := 1
                else if n1 < n2 then
                  Result := -1
                else
                  Result := 0;
              end
              else
              begin
                // try prefix + numeric
                GetPrefix(b1, p1, n1, i1);
                GetPrefix(b2, p2, n2, i2);
                if (i1 = 0) and (i2 = 0) and (p1 = p2) then
                begin
                  // same prefix, numeric compare
                  if n1 > n2 then
                    Result := 1
                  else if n1 < n2 then
                    Result := -1
                  else
                    Result := 0;
                end
                else
                begin
                  // case insensitive string compare
                  Result := CompareText(b1, b2);
                end;
              end;
            end
     else Result:=CompareText(Cells[ACol,ARow],Cells[BCol,BRow]);
   end;
   if SortDirection=-1 then Result:=-Result;
 end;
end;

procedure Tf_EditTargets.TargetListEditingDone(Sender: TObject);
begin
  TargetChange(Sender);
end;

procedure Tf_EditTargets.TargetListHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var i: integer;
    onoff: boolean;
    buf: string;
    t: TTarget;
begin
 if IsColumn then with TargetList do begin
  case index of
    colplan : begin
                buf:=FormEntryCB(self, Columns[Index-1].PickList, Columns[Index-1].Title.Caption, '');
                if buf<>'' then begin
                  for i:=1 to RowCount-1 do
                     if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                       Cells[Index,i]:=buf;
                       t:=TTarget(Objects[colseq,i]);
                       t.planname:=buf;
                     end;
                end;
              end;
    colname,colra,coldec: begin
                if SortDirection=0 then SortDirection := 1
                else SortDirection:=-1*SortDirection;
                SortColRow(True, Index);
                ResetSequences;
                Columns[colname-1].Title.ImageIndex:=-1;
                Columns[colra-1].Title.ImageIndex:=-1;
                Columns[coldec-1].Title.ImageIndex:=-1;
                if SortDirection>0 then
                  Columns[Index-1].Title.ImageIndex:=3
                else
                  Columns[Index-1].Title.ImageIndex:=2;
             end;
    colpa  : begin
                buf:=FormEntry(self, Columns[Index-1].Title.Caption, '');
                if buf<>'' then begin
                  for i:=1 to RowCount-1 do
                     if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                       Cells[Index,i]:=buf;
                       t:=TTarget(Objects[colseq,i]);
                       if buf='-' then
                         t.pa:=NullCoord
                       else
                         t.pa:=StrToFloatDef(buf,t.pa);
                     end;
                end;
              end;
    colstart: begin
               buf:=FormEntryCB(self, Columns[Index-1].PickList, Columns[Index-1].Title.Caption, '-');
               if buf<>'-' then begin
                for i:=1 to RowCount-1 do
                   if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                     Cells[Index,i]:=buf;
                     CheckRiseSet(i,false);
                     TargetListCheckboxToggled(nil,Index,i,cbChecked);
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
                     t.darknight:=(Cells[coldark,i]='1');
                     t.skip:=(Cells[colskip,i]='1');
                   end;
               end;
              end;
    colend:   begin
               buf:=FormEntryCB(self, Columns[Index-1].PickList, Columns[Index-1].Title.Caption, '-');
               if buf<>'-' then begin
                for i:=1 to RowCount-1 do
                   if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                     Cells[Index,i]:=buf;
                     CheckRiseSet(i,false);
                     TargetListCheckboxToggled(nil,Index,i,cbChecked);
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
                     t.darknight:=(Cells[coldark,i]='1');
                     t.skip:=(Cells[colskip,i]='1');
                   end;
               end;
              end;
    coldark:   begin
                onoff:=Columns[Index-1].Title.ImageIndex=titleadd;
                if onoff then Columns[Index-1].Title.ImageIndex:=titledel
                         else Columns[Index-1].Title.ImageIndex:=titleadd;
                for i:=1 to RowCount-1 do
                   if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                     if onoff then
                        Cells[Index,i]:='1'
                     else
                        Cells[Index,i]:='0';
                     CheckRiseSet(i,false);
                     TargetListCheckboxToggled(nil,Index,i,cbChecked);
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
                     t.darknight:=(Cells[coldark,i]='1');
                     t.skip:=(Cells[colskip,i]='1');
                   end;
              end;
    colskip:   begin
                onoff:=Columns[Index-1].Title.ImageIndex=titleadd;
                if onoff then Columns[Index-1].Title.ImageIndex:=titledel
                         else Columns[Index-1].Title.ImageIndex:=titleadd;
                for i:=1 to RowCount-1 do
                   if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then  begin
                     if onoff then
                        Cells[Index,i]:='1'
                     else
                        Cells[Index,i]:='0';
                     CheckRiseSet(i,false);
                     TargetListCheckboxToggled(nil,Index,i,cbChecked);
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
                     t.darknight:=(Cells[coldark,i]='1');
                     t.skip:=(Cells[colskip,i]='1');
                   end;
              end;
    colrepeat:begin
                buf:=FormEntry(self, Columns[Index-1].Title.Caption, '');
                if buf<>'' then begin
                  for i:=1 to RowCount-1 do
                     if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                       Cells[Index,i]:=buf;
                       t:=TTarget(Objects[colseq,i]);
                       t.repeatcount:=StrToIntDef(buf,1);
                       if t.repeatcount<0 then t.repeatcount:=1;
                       t.delay:=TDelay.Value;
                       t.previewexposure:=PreviewExposure.Value;
                       t.preview:=Preview.Checked;
                     end;
                end;
              end;

  end;
 end;
end;

procedure Tf_EditTargets.TargetListSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
 if (TargetList.Cells[colname,aRow]=SkyFlatTxt) or (TargetList.Cells[colname,aRow]=ScriptTxt) then
    Editor:=TargetList.EditorByStyle(cbsNone)     // no edition for flat and script
 else if (aCol=colplan) then
    Editor:=TargetList.EditorByStyle(cbsPickList) // plan selection
 else if (aCol=colpa) then
    Editor:=TargetList.EditorByStyle(cbsPickList) // selection for null pa
 else if (aCol=colstart)or(aCol=colend) then
    Editor:=TargetList.EditorByStyle(cbsPickList) // selection for rise, set
 else if (aCol=coldark)or(aCol=colskip) then
    Editor:=TargetList.EditorByStyle(cbsCheckboxColumn) // dark night and skip
 else
    Editor:=TargetList.EditorByStyle(cbsAuto);
end;

procedure Tf_EditTargets.FlatTimeClick(Sender: TObject);
var r1,r2,i,n: integer;
    t:TTarget;
begin
if LockTarget then exit;
  n:=0;
  for i:=1 to TargetList.RowCount-1 do
    if TargetList.Cells[colname,i]=SkyFlatTxt then inc(n);
  if n>=2 then begin
    LockTarget:=true;
    if FlatTime.ItemIndex=0 then  begin
       FlatTime.ItemIndex:=1;
       ShowMessage(rsThereIsAlrea);
    end
    else begin
       FlatTime.ItemIndex:=0;
       ShowMessage(rsThereIsAlrea2);
    end;
    LockTarget:=false;
    exit;
  end;
  if FlatTime.ItemIndex=0 then begin
     r1:=TargetList.Row;
     r2:=1;
     t:=TTarget(TargetList.Objects[colseq,r1]);
     t.planname:=FlatTimeName[0];
     TargetList.MoveColRow(false,r1,r2);
     SeqStartTwilight.Checked:=false;
     SeqStart.Checked:=false;
  end
  else begin
    r1:=TargetList.Row;
    t:=TTarget(TargetList.Objects[colseq,r1]);
    t.planname:=FlatTimeName[1];
    r2:=TargetList.RowCount-1;
    TargetList.MoveColRow(false,r1,r2);
    SeqStopTwilight.Checked:=false;
    SeqStop.Checked:=false;
  end;
  TargetChange(Sender);
end;

procedure Tf_EditTargets.CheckBoxRepeatListChange(Sender: TObject);
begin
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
  BtnRepeatInf.Enabled:=CheckBoxRepeatList.Checked;
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=RepeatCountList.Value
  else
    FTargetsRepeat:=1;
end;

procedure Tf_EditTargets.RepeatCountListChange(Sender: TObject);
begin
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=RepeatCountList.Value;
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
    ok: boolean;
begin
  ok:=TwilightAstro(now,hm,he);
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
  if SeqStartTwilight.Checked then begin
    if ok then
     SeqStartAt.Text:=TimeToStr(he/24)
    else begin
     ShowMessage(rsNoAstronomic);
     SeqStartTwilight.Checked:=false;
    end;
  end;
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
end;

procedure Tf_EditTargets.SeqStopTwilightChange(Sender: TObject);
var he,hm: double;
    ok: boolean;
begin
  ok:=TwilightAstro(now,hm,he);
  SeqStopAt.Enabled:=SeqStop.Checked and (not SeqStopTwilight.Checked);
  if SeqStopTwilight.Checked then begin
    if ok then
      SeqStopAt.Text:=TimeToStr(hm/24)
    else begin
      ShowMessage(rsNoAstronomic);
      SeqStopTwilight.Checked:=false;
    end;
  end;
end;

procedure Tf_EditTargets.BtnRepeatInfClick(Sender: TObject);
begin
  RepeatCountList.Value:=RepeatCountList.MaxValue;
end;


///////////// Plan /////////////////

procedure Tf_EditTargets.ReadStep(pfile:TCCDconfig; i: integer; var p:TStep; var msg:string);
var str,buf: string;
    j:integer;
begin
  StepsModified:=false;
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Description','');
  p.description:=str;
  str:=trim(pfile.GetValue('/Steps/Step'+inttostr(i)+'/FrameType','Light'));
  j:=StepList.Columns[pcoltype-1].PickList.IndexOf(str);
  if j<0 then j:=0;
  p.frtype:=TFrameType(j);
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Binning','1x1');
  j:=StepList.Columns[pcolbin-1].PickList.IndexOf(str);
  if j<0 then
    StepList.Columns[pcolbin-1].PickList.Add(str);
  j:=pos('x',str);
  if j>0 then begin
     buf:=trim(copy(str,1,j-1));
     p.binx:=StrToIntDef(buf,1);
     buf:=trim(copy(str,j+1,9));
     p.biny:=StrToIntDef(buf,1);
  end else begin
    p.binx:=1;
    p.biny:=1;
  end;
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Gain','');
  p.gain:=StrToIntDef(str,Gain);
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Filter','');
  originalFilter[i]:=str;
  j:=StepList.Columns[pcolfilter-1].PickList.IndexOf(str);
  if j<0 then j:=0;
  p.filter:=j;
  p.exposure:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Exposure',1.0);
  p.count:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/Count',1));
  p.dither:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Dither',false);
  p.dithercount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/DitherCount',1));
  p.autofocusstart:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',false);
  p.autofocus:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Autofocus',false);
  p.autofocustemp:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusTemp',false);
  p.autofocuscount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',10));
  // obsolete option
  if trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/RepeatCount',1)) > 1 then
     msg:='Warning! the Repeat option at the step level as been removed. Please use the Repeat option at the target level instead.';
end;

procedure Tf_EditTargets.ShowPlan;
var pfile: TCCDconfig;
    fn,buf: string;
    i,n:integer;
    p: TStep;
  procedure NewPlan;
  begin
    StepList.RowCount:=2;
    p:=TStep.Create;
    StepList.Cells[0,1]:='1';
    StepList.Cells[1,1]:=p.description_str;
    StepList.Objects[0,1]:=p;
    StepListSelection(nil,0,1);
  end;
begin
  ClearStepList;
  fn:=slash(ConfigDir)+PlanName.Caption+'.plan';
  if FileExistsUTF8(fn) then begin
    pfile:=TCCDconfig.Create(self);
    pfile.Filename:=fn;
    n:=pfile.GetValue('/StepNum',0);
    if n=0 then begin
      NewPlan;
    end else begin
      StepList.RowCount:=n+1;
      buf:='';
      for i:=1 to n do begin
        p:=TStep.Create;
        ReadStep(pfile,i,p,buf);
        StepList.Cells[0,i]:=IntToStr(i);
        StepList.Cells[1,i]:=p.description_str;
        StepList.Objects[0,i]:=p;
        StepListSelection(nil,0,i);
      end;
      pfile.Free;
      StepList.Row:=1;
      StepListSelection(StepList,0,1);
      if buf>'' then ShowMessage(buf);
    end;
  end else begin
    NewPlan;
    if PlanName.Caption<>'' then StepsModified:=true;
  end;
end;

procedure Tf_EditTargets.ClearStepList;
var i: integer;
begin
  for i:=1 to StepList.RowCount-1 do begin
    if StepList.Objects[0,i]<>nil then StepList.Objects[0,i].Free;
    StepList.Objects[0,i]:=nil;
  end;
  StepList.RowCount:=1;
end;

procedure Tf_EditTargets.FrameTypeChange(n: integer; newtype: TFrameType);
begin
  case newtype of
    Light : begin

        end;
    Bias : begin
           StepList.Cells[pcolexp,n]:='0.01';
           StepList.Cells[pcolfilter,n]:=Filter0;
        end;
    Dark : begin
           StepList.Cells[pcolfilter,n]:=Filter0;
        end;
    Flat : begin

        end;
  end;
end;


procedure Tf_EditTargets.StepListSelection(Sender: TObject; aCol, aRow: Integer);
var p: Tstep;
begin
  p:=TStep(StepList.Objects[pcolseq,aRow]);
  SetStep(aRow,p);
end;

procedure Tf_EditTargets.SetStep(n: integer; p: TStep);
begin
  LockStep:=true;
  StepList.Cells[pcoldesc,n]:=p.description_str;
  StepList.Cells[pcoltype,n]:=trim(FrameName[ord(p.frtype)]);
  StepList.Cells[pcolexp,n]:=formatfloat(f3,p.exposure);
  StepList.Cells[pcolbin,n]:=p.binning_str;
  if hasGainISO then
    PISObox.ItemIndex:=p.gain
  else
    PGainEdit.Value:=p.gain;
  StepList.Cells[pcolfilter,n]:=StepList.Columns[pcolfilter-1].PickList[p.filter];
  StepList.Cells[pcolcount,n]:=IntToStr(p.count);
  GroupBoxStep.Caption:=rsStep+blank+p.description_str;
  CheckBoxDither.Checked:=p.dither;
  DitherCount.Value:=p.dithercount;
  CheckBoxAutofocusStart.Checked:=p.autofocusstart;
  CheckBoxAutofocus.Checked:=p.autofocus;
  CheckBoxAutofocusTemp.Checked:=p.autofocustemp;
  AutofocusCount.Value:=p.autofocuscount;
  LockStep:=false;
end;

procedure Tf_EditTargets.CheckLightOnlyChange(Sender: TObject);
var n:integer;
    p: TStep;
begin
  if LockStep then exit;
  if not (sender is TCheckBox) then exit;
  // is table empty?
  if StepList.RowCount<=1 then exit;
  n:=StepList.Row;
  // is title row?
  if n < 1 then exit;
  p:=TStep(StepList.Objects[0,n]);
  if p=nil then exit;
  if TCheckBox(Sender).Checked and (p.frtype<>LIGHT) then
     TCheckBox(Sender).Checked:=false
  else
     StepChange(Sender);
end;

procedure Tf_EditTargets.StepChange(Sender: TObject);
var n,j,i:integer;
    p: TStep;
    x: double;
    str,buf: string;
begin
  if LockStep then exit;
  // is table empty?
  if StepList.RowCount<=1 then exit;
  n:=StepList.Row;
  // is title row?
  if n < 1 then exit;
  p:=TStep(StepList.Objects[0,n]);
  if p=nil then exit;
  StepsModified:=StepsModified or (p.description<>StepList.Cells[pcoldesc,n]);
  p.description:=StepList.Cells[pcoldesc,n];
  str:=StepList.Cells[pcoltype,n];
  if str='Dark' then
    j:=0;
  j:=StepList.Columns[pcoltype-1].PickList.IndexOf(str);
  if j<0 then j:=0;
  StepsModified:=StepsModified or (p.frtype<>TFrameType(j));
  if p.frtype<>TFrameType(j) then FrameTypeChange(n,TFrameType(j));
  p.frtype:=TFrameType(j);
  x:=StrToFloatDef(StepList.Cells[pcolexp,n],p.exposure);
  StepsModified:=StepsModified or (p.exposure<>x);
  p.exposure:=x;
  str:=StepList.Cells[pcolbin,n];
  j:=pos('x',str);
  if j>0 then begin
     buf:=trim(copy(str,1,j-1));
     i:=StrToIntDef(buf,1);
     StepsModified:=StepsModified or (p.binx<>i);
     p.binx:=i;
     buf:=trim(copy(str,j+1,9));
     i:=StrToIntDef(buf,1);
     StepsModified:=StepsModified or (p.biny<>i);
     p.biny:=i;
  end else begin
    StepsModified:=StepsModified or (p.binx<>1);
    p.binx:=1;
    StepsModified:=StepsModified or (p.biny<>1);
    p.biny:=1;
  end;
  if hasGainISO then begin
     StepsModified:=StepsModified or (p.gain<>PISObox.ItemIndex);
     p.gain:=PISObox.ItemIndex;
  end
  else begin
     StepsModified:=StepsModified or (p.gain<>PGainEdit.Value);
     p.gain:=PGainEdit.Value;
  end;
  str:=StepList.Cells[pcolfilter,n];
  j:=StepList.Columns[pcolfilter-1].PickList.IndexOf(str);
  if j<0 then j:=0;
  StepsModified:=StepsModified or (p.filter<>j);
  p.filter:=j;
  j:=StrToIntDef(StepList.Cells[pcolcount,n],p.count);
  StepsModified:=StepsModified or (p.count<>j);
  p.count:=j;
  StepsModified:=StepsModified or (p.dither<>CheckBoxDither.Checked);
  p.dither:=(p.frtype=LIGHT) and CheckBoxDither.Checked;
  StepsModified:=StepsModified or (p.dithercount<>DitherCount.Value);
  p.dithercount:=DitherCount.Value;
  StepsModified:=StepsModified or (p.autofocusstart<>CheckBoxAutofocusStart.Checked);
  p.autofocusstart:=(p.frtype=LIGHT) and CheckBoxAutofocusStart.Checked;
  StepsModified:=StepsModified or (p.autofocus<>CheckBoxAutofocus.Checked);
  p.autofocus:=(p.frtype=LIGHT) and CheckBoxAutofocus.Checked;
  StepsModified:=StepsModified or (p.autofocuscount<>AutofocusCount.Value);
  p.autofocuscount:=AutofocusCount.Value;
  StepsModified:=StepsModified or (p.autofocustemp<>CheckBoxAutofocusTemp.Checked);
  p.autofocustemp:=(p.frtype=LIGHT) and CheckBoxAutofocusTemp.Checked;
  StepList.Cells[1,n]:=p.description;
  SetStep(n,p);
end;

procedure Tf_EditTargets.StepListColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  ResetSteps;
  StepsModified:=true;
end;

procedure Tf_EditTargets.StepListEditingDone(Sender: TObject);
begin
  StepChange(Sender);
end;

procedure Tf_EditTargets.StepListSelectEditor(Sender: TObject; aCol,
  aRow: Integer; var Editor: TWinControl);
begin
  if (aCol=pcoltype) then
     Editor:=StepList.EditorByStyle(cbsPickList)   // type selection
  else if (aCol=pcolbin) then
     Editor:=StepList.EditorByStyle(cbsPickList) // binning selection
  else if (aCol=pcolfilter) then
     Editor:=StepList.EditorByStyle(cbsPickList) // filter selection
  else
     Editor:=StepList.EditorByStyle(cbsAuto);
end;

procedure Tf_EditTargets.BtnAddStepClick(Sender: TObject);
var txt:string;
    i,n: integer;
    p,pp: TStep;
begin
  txt:=FormEntry(self, rsStepDescript, 'Step'+inttostr(StepList.RowCount));
  p:=TStep.Create;
  n:=StepList.Row;
  if n >= 1 then begin
    pp:=TStep(StepList.Objects[0,n]);
    p.Assign(pp);
  end;
  p.description:=txt;
  StepList.RowCount:=StepList.RowCount+1;
  i:=StepList.RowCount-1;
  StepList.Cells[pcolseq,i]:=IntToStr(i);
  StepList.Cells[pcoldesc,i]:=txt;
  StepList.Objects[pcolseq,i]:=p;
  StepList.Row:=i;
  StepsModified:=true;
  SetStep(i,p);
end;

procedure Tf_EditTargets.BtnRemoveStepClick(Sender: TObject);
var i,j: integer;
    str: string;
begin
  i:=StepList.Row;
  if i>0 then begin
     str:=StepList.Cells[0,i]+', '+StepList.Cells[1,i];
     if MessageDlg(Format(rsDeleteStep, [str]), mtConfirmation, mbYesNo, 0)=
       mrYes then begin
        j:=i-1;
        if j<1 then j:=i+1;
        if j>=StepList.RowCount then j:=StepList.RowCount-1;
        StepList.Row:=j;
        StepListSelection(StepList,0,j);
        Application.ProcessMessages;
        if StepList.Objects[0,i]<>nil then StepList.Objects[0,i].Free;
        StepList.Objects[0,i]:=nil;
        StepList.DeleteRow(i);
        StepsModified:=true;
     end;
     ResetSteps;
  end;
end;

procedure Tf_EditTargets.ResetSteps;
var i: integer;
begin
  for i:=1 to StepList.RowCount-1 do begin
    StepList.Cells[0,i]:=IntToStr(i);
  end;
end;

procedure Tf_EditTargets.BtnSavePlanAsClick(Sender: TObject);
begin
  SaveDialog1.InitialDir:=ConfigDir;
  SaveDialog1.DefaultExt:='.plan';
  SaveDialog1.filter:='Plan|*.plan';
  SaveDialog1.FileName:=PlanName.Caption+'.plan';
  if SaveDialog1.Execute then begin
     PlanName.Caption:=ExtractFileNameOnly(SaveDialog1.FileName);
     SavePlan;
     SetPlanList(TargetList.Row,PlanName.Caption);
     TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.BtnSavePlanClick(Sender: TObject);
begin
  SavePlan;
end;

procedure Tf_EditTargets.SavePlan;
var pfile: TCCDconfig;
    fn,str: string;
    i,n,k: integer;
    p: TStep;
begin
try
  fn:=slash(ConfigDir)+PlanName.Caption+'.plan';
  pfile:=TCCDconfig.Create(self);
  pfile.Filename:=fn;
  pfile.Clear;
  n:=StepList.RowCount-1;
  pfile.SetValue('/PlanName',PlanName.Caption);
  pfile.SetValue('/StepNum',n);
  for i:=1 to n do begin
    p:=TStep(StepList.Objects[0,i]);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Description',p.description);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/FrameType',trim(FrameName[ord(p.frtype)]));
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Exposure',p.exposure);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Binning',IntToStr(p.binx)+'x'+IntToStr(p.biny));
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Gain',p.gain);
    if StepList.Columns[pcolfilter-1].PickList.Count>0 then begin // do not erase the filters if the filter wheel is not connected
      k:=p.filter;
      if (k<0)or(k>StepList.Columns[pcolfilter-1].PickList.Count-1) then str:=''
         else str:=StepList.Columns[pcolfilter-1].PickList[k];
    end else begin
      str:=originalFilter[i];
    end;
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Filter',str);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Count',p.count);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Dither',p.dither);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/DitherCount',p.dithercount);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',p.autofocusstart);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Autofocus',p.autofocus);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',p.autofocuscount);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/AutofocusTemp',p.autofocustemp);
  end;
  pfile.Flush;
  pfile.Free;
  LoadPlanList;
  TargetChange(nil);
  StepsModified:=false;
except
  on E: Exception do ShowMessage('Error saving plan: '+ E.Message);
end;
end;


end.

