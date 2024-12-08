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

uses pu_planetariuminfo, u_global, u_utils, u_ccdconfig, pu_pascaleditor, u_annotation, pu_keyboard, pu_newscript, pu_onlineinfo,
  pu_scriptengine, cu_astrometry, u_hints, u_translation, pu_selectscript, Classes, math, cu_targets, pu_viewtext, cu_switch,
  SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UScaleDPI, cu_plan, LCLType,
  LazUTF8, maskedit, Grids, ExtCtrls, ComCtrls, Spin, Buttons, Menus, CheckLst, Types;

const
  colseq=0; colname=1; colplan=2; colra=3; coldec=4; colpa=5; colstart=6; colend=7; colrepeat=8;
  pcolseq=0; pcoldesc=1; pcoltype=2; pcolexp=3; pcolstack=4; pcolbin=5; pcolfilter=6; pcolcount=7; pcolafstart=8; pcolafevery=9; pcoldither=10; pcolgain=11; pcoloffset=12; pcolfstop=13;
  titleadd=0; titledel=1;
  pageobject=0; pagescript=1; pageflat=2; pagenone=3; pageswitch=4;
  cbNone=0; cbStopTracking=1; cbWarm=2; cbParkScope=3; cbParkDome=4; cbScript=5; cbUnattended=6;
  ccNone=0; ccCool=1; ccUnpark=2; ccScript=3;

type

  { Tf_EditTargets }

  Tf_EditTargets = class(TForm)
    Bevel3: TBevel;
    Bevel4: TBevel;
    BtnAddStep: TButton;
    BtnDeleteObject: TButton;
    BtnDeleteTemplate: TButton;
    BtnInsert: TButton;
    BtnOptions: TButton;
    BtnRemoveStep: TButton;
    BtnSaveAs: TButton;
    BtnSaveTemplate: TButton;
    BtnTiming: TButton;
    cbDarkNight: TCheckBox;
    cbSkip: TCheckBox;
    cbAstrometry: TCheckBox;
    cbInplace: TCheckBox;
    cbUpdCoord: TCheckBox;
    cbSolarTracking: TCheckBox;
    cbAutofocusTemp: TCheckBox;
    cbNoAutoguidingChange: TCheckBox;
    cbAutofocusHFD: TCheckBox;
    cbMandatoryStartTime: TCheckBox;
    cbInitScript: TCheckBox;
    CheckBoxResetRepeat: TCheckBox;
    CheckBoxRestartStatus: TCheckBox;
    Label22: TLabel;
    Label23: TLabel;
    MenuAddCaptureStep: TMenuItem;
    MenuAddScriptStep: TMenuItem;
    MenuAddSwitchStep: TMenuItem;
    MenuInsertOnline: TMenuItem;
    MenuOnlineCoord: TMenuItem;
    Panel10: TPanel;
    Panel16: TPanel;
    PopupAddStep: TPopupMenu;
    ScriptList2: TComboBox;
    ScriptParam2: TEdit;
    Switches: TComboBox;
    Switchlist: TComboBox;
    SwitchValue: TEdit;
    Label10: TLabel;
    Label12: TLabel;
    Label9: TLabel;
    lblName: TLabel;
    MenuSwitch: TMenuItem;
    Panel3: TPanel;
    PanelSolarTracking: TPanel;
    PanelTargetDetail: TPanel;
    ScriptParam: TEdit;
    Label5: TLabel;
    Label8: TLabel;
    MenuBlankRow: TMenuItem;
    ScrollBox2: TScrollBox;
    Splitter2: TSplitter;
    StartOpt: TCheckListBox;
    PlanSwitch: TTabSheet;
    TermOpt: TCheckListBox;
    FFstopbox: TComboBox;
    FlatFilterList: TCheckGroup;
    FOffsetEdit: TSpinEdit;
    Label4: TLabel;
    MenuApplyTemplate: TMenuItem;
    MenuSaveTemplate: TMenuItem;
    MenuSaveTemplateAs: TMenuItem;
    MenuNewObject: TMenuItem;
    MenuSearchCoord: TMenuItem;
    MenuPlanetariumCoord: TMenuItem;
    MenuImgCoord: TMenuItem;
    MenuNoMove: TMenuItem;
    MenuAnyTime: TMenuItem;
    MenuImgRot: TMenuItem;
    MenuInsertPlanetarium: TMenuItem;
    MenuSkyFlat: TMenuItem;
    MenuNewScript: TMenuItem;
    MenuImportObsList: TMenuItem;
    MenuImportMosaic: TMenuItem;
    MenuCoordinates: TMenuItem;
    MenuTime: TMenuItem;
    MenuRotator: TMenuItem;
    Panel6: TPanel;
    Panel8: TPanel;
    PanelRepeat: TPanel;
    ImageListNight: TImageList;
    ImageListDay: TImageList;
    Label11: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelOffset: TLabel;
    PanelOffset: TPanel;
    PopupInsert: TPopupMenu;
    PopupSaveTemplate: TPopupMenu;
    PopupOptions: TPopupMenu;
    Preview: TCheckBox;
    PreviewExposure: TFloatSpinEdit;
    Splitter1: TSplitter;
    TargetMsg: TLabel;
    LabelMsg: TLabel;
    OpenDialog1: TOpenDialog;
    PageControlPlan: TPageControl;
    Panel1: TPanel;
    Panel11: TPanel;
    PanelSeq: TPanel;
    Panel17: TPanel;
    PanelFstop: TPanel;
    PanelTermination: TPanel;
    FlatBinning: TComboBox;
    BtnEditScript: TButton;
    BtnSave: TButton;
    BtnEditNewScript: TButton;
    BtnCancel: TButton;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    PanelBtn: TPanel;
    PanelTools: TPanel;
    Panel14: TPanel;
    Panel2: TPanel;
    PanelPlan: TPanel;
    Panel4: TPanel;
    PanelStartStop: TPanel;
    PanelStartOption: TPanel;
    Panel9: TPanel;
    PlanName: TLabel;
    SaveDialog1: TSaveDialog;
    BtnRepeatInf: TSpeedButton;
    ScrollBox1: TScrollBox;
    PlanObject: TTabSheet;
    PlanScript: TTabSheet;
    PlanFlat: TTabSheet;
    PlanNone: TTabSheet;
    TDelay: TSpinEdit;
    TargetName: TLabel;
    FISObox: TComboBox;
    Label16: TLabel;
    Label18: TLabel;
    LabelGain: TLabel;
    FlatTime: TRadioGroup;
    PanelGain: TPanel;
    RepeatCountList: TSpinEdit;
    FlatCount: TSpinEdit;
    FGainEdit: TSpinEdit;
    StepList: TStringGrid;
    SeqStart: TCheckBox;
    SeqStopAt: TMaskEdit;
    SeqStop: TCheckBox;
    SeqStartTwilight: TCheckBox;
    SeqStopTwilight: TCheckBox;
    CheckBoxRepeatList: TCheckBox;
    Label15: TLabel;
    PanelTargetOptions: TPanel;
    PanelTarget: TPanel;
    ScriptList: TComboBox;
    SeqStartAt: TMaskEdit;
    TargetList: TStringGrid;
    BtnEditNewScript1: TButton;
    BtnEditScript1: TButton;
    Label14: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    PageControlStepType: TPageControl;
    Panel15: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    ScriptList1: TComboBox;
    ScriptParam1: TEdit;
    Switches1: TComboBox;
    Switchlist1: TComboBox;
    StepTypeCapture: TTabSheet;
    StepTypeScript: TTabSheet;
    StepTypeSwitch: TTabSheet;
    SwitchValue1: TEdit;
    procedure BtnAddStepClick(Sender: TObject);
    procedure BtnAnytimeClick(Sender: TObject);
    procedure BtnApplyTemplateClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnInsertPopup(Sender: TObject);
    procedure BtnInsertPlanetariumClick(Sender: TObject);
    procedure BtnOptionsPopup(Sender: TObject);
    procedure BtnPlanetariumCoordClick(Sender: TObject);
    procedure BtnCurrentCoordClick(Sender: TObject);
    procedure BtnDeleteTemplateClick(Sender: TObject);
    procedure BtnDeleteObjectClick(Sender: TObject);
    procedure BtnImportMosaicClick(Sender: TObject);
    procedure BtnImportObslistClick(Sender: TObject);
    procedure BtnRemoveStepClick(Sender: TObject);
    procedure BtnImgCoordClick(Sender: TObject);
    procedure BtnImgRotClick(Sender: TObject);
    procedure BtnSaveAsClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnSaveTemplateAsClick(Sender: TObject);
    procedure BtnSaveTemplateClick(Sender: TObject);
    procedure BtnSaveTemplatePopup(Sender: TObject);
    procedure BtnScript1Click(Sender: TObject);
    procedure BtnSkyFlatClick(Sender: TObject);
    procedure BtnScriptClick(Sender: TObject);
    procedure BtnNewObjectClick(Sender: TObject);
    procedure BtnNewScriptClick(Sender: TObject);
    procedure Btn_coord_internalClick(Sender: TObject);
    procedure BtnTimingClick(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure CheckRestartStatus(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBoxRepeatListChange(Sender: TObject);
    procedure FlatFilterListItemClick(Sender: TObject; Index: integer);
    procedure FlatTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuAddCaptureStepClick(Sender: TObject);
    procedure MenuAddScriptStepClick(Sender: TObject);
    procedure MenuAddSwitchStepClick(Sender: TObject);
    procedure MenuBlankRowClick(Sender: TObject);
    procedure MenuInsertOnlineClick(Sender: TObject);
    procedure MenuOnlineCoordClick(Sender: TObject);
    procedure MenuSwitchClick(Sender: TObject);
    procedure PointCoordChange(Sender: TObject);
    procedure RepeatCountListChange(Sender: TObject);
    procedure SeqStartChange(Sender: TObject);
    procedure SeqStartTwilightChange(Sender: TObject);
    procedure SeqStopChange(Sender: TObject);
    procedure SeqStopTwilightChange(Sender: TObject);
    procedure BtnRepeatInfClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure StartOptItemClick(Sender: TObject; Index: integer);
    procedure StepListCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure StepListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure StepListEditingDone(Sender: TObject);
    procedure StepListGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure StepListSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure StepListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StepChange(Sender: TObject);
    procedure StepListValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure Switches1Change(Sender: TObject);
    procedure SwitchesChange(Sender: TObject);
    procedure TargetChange(Sender: TObject);
    procedure TargetListColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure TargetListCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure TargetListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure TargetListEditingDone(Sender: TObject);
    procedure TargetListGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
    procedure TargetListHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure TargetListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TargetListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TargetListSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure TargetListSelection(Sender: TObject; aCol, aRow: Integer);
    procedure TermOptItemClick(Sender: TObject; Index: integer);
  private
    { private declarations }
    FAstrometry: TAstrometry;
    LockTarget: boolean;
    FTargetsRepeat,FTargetsRepeatCount: integer;
    LockStep, StepsModified, ObjectNameChange: boolean;
    Lockcb: boolean;
    SortDirection,planheight: integer;
    FCoordWarning: boolean;
    FCoordWarningRow: integer;
    FFilename: string;
    FSolarTracking: boolean;
    FSwitch: TSwitches;
    procedure SetPlanList(n: integer; pl:string);
    procedure SetScriptList(n: integer; sl, args:string);
    procedure SetScriptList1(n: integer; sp, sl, args:string);
    procedure SetScriptList2(n: integer; sl, args:string);
    procedure SetSwitch(n: integer; swl:string);
    procedure SetSwitch1(n: integer; swni,swna,swval:string);
    procedure ResetSequences;
    procedure ResetSteps;
    procedure SetStep(n: integer; p: TStep);
    procedure SavePlanModified(isTemplate: boolean);
    function  CheckRiseSet(n: integer; showmsg:boolean=true): boolean;
    procedure FrameTypeChange(n: integer; newtype: integer);
    procedure SetStartTime(buf: string; var t:TTarget);
    procedure SetEndTime(buf: string; var t:TTarget);
    function AsDuskFlat:boolean;
    function AsDawnFlat:boolean;
    procedure MoveFlat(Sender: TObject);
    procedure NewObject;
    procedure NewPlanetariumTarget(Sender: TObject);
    procedure SetTemplateButton;
    procedure MarkModifiedTemplate;
    procedure SetStartScriptName;
    procedure SetEndScriptName;
   public
    { public declarations }
    originalFilter: TSaveFilter;
    EndScript, UnattendedScript, StartScript: string;
    procedure SetLang;
    procedure LoadTypeList;
    procedure LoadPlanList;
    procedure LoadScriptList;
    procedure LoadSwitchList;
    procedure SetTarget(n: integer; t: TTarget);
    procedure ClearTargetList;
    procedure ClearStepList;
    procedure ShowPlan;
    procedure LoadTemplate;
    procedure SaveTemplate;
    procedure ReadStep(pfile:TCCDconfig; i: integer; var p:TStep; var msg:string);
    property TargetsRepeat: integer read FTargetsRepeat write FTargetsRepeat;
    property TargetsRepeatCount: integer read FTargetsRepeatCount write FTargetsRepeatCount;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property Filename: string read FFilename write FFilename;
    property SolarTracking: boolean read FSolarTracking write FSolarTracking;
    property Switch: TSwitches read FSwitch write FSwitch;
    procedure SetTargets(value:T_Targets);
    procedure GetTargets(var value:T_Targets; out fn,defaultname: string);
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
  TargetList.FixedColor:=clBackground;
  StepList.FixedColor:=clBackground;
  {$endif}
  ScaleDPI(Self);
  TargetList.RowHeights[0]:=DoScaleY(40);
  StepList.RowHeights[0]:=DoScaleY(40);
  keyboard1:=Tkeyboard1.Create(self);
  targetmsg.Caption:='';
  LockTarget:=false;
  FTargetsRepeat:=1;
  LockStep:=false;
  StepsModified:=false;
  Lockcb:=false;
  SortDirection:=-1;
  FCoordWarning:=false;
  planheight:=-1;
  f_selectscript:=Tf_selectscript.Create(self);
  TermOpt.Checked[cbStopTracking]:=true;
  SetLang;
  LoadPlanList;
  LoadScriptList;
  LoadTypeList;
end;

procedure Tf_EditTargets.FormDestroy(Sender: TObject);
begin
  ClearTargetList;
  ClearStepList;
end;

procedure Tf_EditTargets.FormShow(Sender: TObject);
begin
  LabelMsg.Caption:='';
  ObjectNameChange:=false;
  TargetList.Cells[colseq,0]:='Seq';
  StepList.Cells[0,0]:='Seq';
  if TargetList.RowCount>1 then begin
     TargetList.Row:=1;
     TargetListSelection(nil,0,1);
  end
  else begin
    PanelTools.Visible:=false;
    PageControlPlan.ActivePageIndex:=pagenone;
  end;
  SeqStopAt.Enabled:=SeqStop.Checked and (not SeqStopTwilight.Checked);
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
  RepeatCountList.Value:=FTargetsRepeat;
  CheckBoxRepeatList.Checked:=(FTargetsRepeat>1);
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
  BtnRepeatInf.Enabled:=CheckBoxRepeatList.Checked;
  FlatFilterList.ClientHeight:=ceil(FlatFilterList.Items.Count/FlatFilterList.Columns)*DoScaleY(25);
  SetStartScriptName;
  SetEndScriptName;
  CheckRestartStatus(nil);
end;

procedure Tf_EditTargets.SetLang;
begin
  Caption := rsEditTargetLi;
  BtnSave.Caption := rsSave;
  BtnSaveAs.Caption := rsSaveAs;
  BtnTiming.Caption:=rsTimingEstima;
  BtnInsert.Caption := rsInsertRows;
  MenuNewObject.Caption := rsNewObject;
  MenuBlankRow.Caption:=rsInsertBlankR;
  MenuInsertPlanetarium.Caption := rsFromPlanetar;
  MenuInsertOnline.Caption:=rsSearchOnline;
  BtnDeleteObject.Caption := rsRemoveRow;
  BtnOptions.Caption:=rsOptions2;
  MenuNewScript.Caption := rsScript;
  MenuSwitch.Caption:=rsSwitch;
  BtnCancel.Caption := rsCancel;
  MenuSkyFlat.Caption := rsSkyFlat;
  MenuImportObslist.Caption:=rsImportCdCObs;
  MenuImportMosaic.Caption:=rsImportCdCMos;
  MenuAddCaptureStep.Caption:=rsCapture;
  MenuAddScriptStep.Caption:=rsScript;
  MenuAddSwitchStep.Caption:=rsSwitch;
  TargetList.Columns.Items[colname-1].Title.Caption := Format(rsTargetName, [crlf]);
  TargetList.Columns.Items[colplan-1].Title.Caption := rsTemplate;
  TargetList.Columns.Items[colra-1].Title.Caption := rsRA+j2000;
  TargetList.Columns.Items[coldec-1].Title.Caption := rsDec+j2000;
  TargetList.Columns.Items[colpa-1].Title.Caption := rsPA;
  TargetList.Columns.Items[colstart-1].Title.Caption := rsBegin;
  TargetList.Columns.Items[colend-1].Title.Caption := rsEnd;
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
  cbDarkNight.Caption := Format(rsDarkNight, [blank]);
  cbSkip.Caption := Format(rsDonTSwait+'', [blank]);
  cbAstrometry.Caption := Format(rsUseAstromet2+'', [blank]);
  cbInplace.Caption := Format(rsStayInPlace2+'', [blank]);
  cbAutofocusTemp.Caption:=rsAutofocusAft2;
  cbAutofocusHFD.Caption:=rsAutofocusHFD2;
  cbUpdCoord.Caption := Format(rsUpdateRADec2+'', [blank]);
  cbSolarTracking.Caption:= rsActivateSola;
  cbNoAutoguidingChange.Caption:=rsDoNotStartAu;
  cbInitScript.Caption:=rsRunAnInitial;
  Label22.Caption:=rsScript;
  label23.Caption:=rsAdditionalSc;
  cbDarkNight.Hint:=rsWaitForFullD;
  cbSkip.Hint:=Format(rsDonTWaitForT, [crlf]);
  cbAstrometry.Hint:=rsUsePlateSolv;
  cbInplace.Hint:=Format(rsStayAtTheTar, [crlf, crlf]);
  cbNoAutoguidingChange.Hint:=rsThisOptionIs;
  cbAutofocusTemp.Hint:=rsSetTheTemper;
  cbAutofocusHFD.Hint:=rsSetTheHFD;
  cbUpdCoord.Hint:=Format(rsPriorToSlewi, [crlf, crlf]);
  CheckBoxRepeatList.Caption := rsRepeatTheWho;
  CheckBoxRestartStatus.Caption:=rsRecordRestar;
  CheckBoxResetRepeat.Caption:=rsClearRestart;
  Label5.Caption:=rsSequenceStar2;
  SeqStart.Caption := rsStartAt;
  SeqStop.Caption := rsStopAt;
  SeqStartTwilight.Caption := rsDusk;
  SeqStopTwilight.Caption := rsDawn;
  Preview.Caption := rsPreview;
  Label13.Caption := rsSeconds2;
  Label4.Caption:=rsRepeat+':';
  Label11.Caption := rsInterval;
  MenuTime.Caption:=rsTime;
  MenuAnytime.Caption := rsAnyTime;
  MenuCoordinates.Caption:=rsCoordinates;
  MenuNoMove.Caption := rsNone2;
  MenuPlanetariumCoord.Caption := rsPlanetarium;
  MenuSearchCoord.Caption:=rsSearch;
  MenuOnlineCoord.Caption:=rsSearchOnline;
  MenuRotator.Caption:=rsRotator;
  MenuImgCoord.Caption := rsCurrentImage;
  MenuImgRot.Caption := rsCurrentImage;
  Label9.Caption:=rsSwitchConnec;
  Label10.Caption:=rsSwitch;
  Label12.Caption:=rsValue;
  Label15.Caption := rsScript;
  Label8.Caption := rsScriptArgume;
  BtnEditScript.Caption := rsEdit;
  BtnEditNewScript.Caption := rsNew;
  Label19.Caption:=rsSwitchConnec;
  Label20.Caption:=rsSwitch;
  Label21.Caption:=rsValue;
  Label17.Caption := rsScript;
  Label14.Caption := rsScriptArgume;
  BtnEditScript1.Caption := rsEdit;
  BtnEditNewScript1.Caption := rsNew;
  FlatTime.Caption := rsFlatTime;
  Label16.Caption := rsBinning;
  GroupBox6.Caption := rsFilters;
  GroupBox7.Caption := rsExposure;
  Label18.Caption := rsCount;
  LabelGain.Caption := rsGain;
  FlatTime.Items[0]:=rsAtDusk;
  FlatTime.Items[1]:=rsAtDawn;
  label2.Caption:=rsSequence;
  // plan
  BtnDeleteTemplate.Caption := rsDeleteTempl;
  BtnSaveTemplate.Caption:=rsSaveTempl;
  MenuSaveTemplate.Caption := rsSaveTempl;
  MenuSaveTemplateAs.Caption:=rsSaveTemplAs;
  MenuApplyTemplate.Caption:=rsSaveAndApply;
  BtnRemoveStep.Caption := rsRemoveStep;
  BtnAddStep.Caption := rsAddStep;
  StepList.Columns.Items[pcoldesc-1].Title.Caption := rsDescription;
  StepList.Columns.Items[pcoltype-1].Title.Caption := rsType;
  StepList.Columns.Items[pcolexp-1].Title.Caption := rsExposure;
  StepList.Columns.Items[pcolstack-1].Title.Caption := rsStack;
  StepList.Columns.Items[pcolbin-1].Title.Caption := rsBinning;
  StepList.Columns.Items[pcolfilter-1].Title.Caption := rsFilter;
  StepList.Columns.Items[pcolcount-1].Title.Caption := rsCount;
  StepList.Columns.Items[pcolafstart-1].Title.Caption := Format(rsAutofocusBef,[crlf]);
  StepList.Columns.Items[pcolafevery-1].Title.Caption := Format(rsAutofocusEve,[crlf]);
  StepList.Columns.Items[pcoldither-1].Title.Caption := Format(rsDitherEvery2,[crlf]);
  StepList.Columns.Items[pcolgain-1].Title.Caption := rsGain;
  StepList.Columns.Items[pcoloffset-1].Title.Caption := rsOffset2;
  StepList.Columns.Items[pcolfstop-1].Title.Caption := rsFStop;
  if StepList.Columns[pcolfilter-1].PickList.Count>0 then StepList.Columns[pcolfilter-1].PickList[0]:=Filter0;
  Label1.Caption := rsTemplate;
  // start options
  Label7.Caption:=rsStartOptions;
  StartOpt.Items[ccNone]:=rsDoNothing;
  StartOpt.Items[ccCool]:=rsCoolTheCamer;
  StartOpt.Items[ccUnpark]:=rsUnparkTheTel;
  StartOpt.Items[ccScript]:=rsRunAScript;
  // termination options
  Label3.Caption:=rsTerminationO;
  TermOpt.Items[cbNone]:=rsDoNothing;
  TermOpt.Items[cbStopTracking]:=rsStopTelescop2;
  TermOpt.Items[cbParkScope]:=rsParkTheTeles2;
  TermOpt.Items[cbParkDome]:=rsParkAndClose;
  TermOpt.Items[cbWarm]:=rsWarmTheCamer;
  TermOpt.Items[cbScript]:=rsRunAScript;
  TermOpt.Items[cbUnattended]:=rsUnattendedEr;
  // hint
  Preview.Hint:=rsStartAPrevie;
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
  BtnDeleteObject.Hint:=rsDeleteTheSel;
  TargetList.Hint:=rsTheListOfTar;
  BtnSave.Hint:=rsSaveTheListA;
  BtnSaveAs.Hint:=rsSaveTheListW;
  PanelTermination.Hint:=rsSetHereTheAc;
  if f_pascaleditor<>nil then f_pascaleditor.SetLang;
  if f_selectscript<>nil then f_selectscript.SetLang;
end;

procedure Tf_EditTargets.PointCoordChange(Sender: TObject);
begin
  TargetChange(Sender);
end;

procedure Tf_EditTargets.LoadTypeList;
var i: integer;
    s: TStringlist;
begin
  s:=TStringlist.Create;
  for i:=0 to ord(high(TFrameType)) do
    s.Add(trim(FrameName[i]));
  for i:=0 to NumCustomFrameType-1 do
    s.Add(trim(CustomFrameType[i].Name));
  StepList.Columns[pcoltype-1].PickList.Assign(s);
  s.Free;
end;

procedure Tf_EditTargets.LoadPlanList;
var i: integer;
    fs : TSearchRec;
    s: TStringlist;
begin
  s:=TStringlist.Create;
  s.add('');
  TargetList.Columns[colplan-1].PickList.Clear;
  i:=FindFirstUTF8(slash(SequenceDir)+'*.plan',0,fs);
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
  i:=TargetList.Columns[colplan-1].PickList.IndexOf(StringReplace(pl,'*','',[]));
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
  ScriptList1.Clear;
  ScriptList2.Clear;
  i:=FindFirstUTF8(slash(ConfigDir)+'*.script',0,fs);
  while i=0 do begin
    {$if defined(CPUARM) or defined(CPUAARCH64)}
    if f_scriptengine.ScriptType(slash(ConfigDir)+fs.name)<>stPascal then
    {$endif}
    begin
      scr:=ExtractFileNameOnly(fs.Name);
      if s.IndexOf(scr)<0 then begin
        s.Add(scr);
      end;
    end;
    i:=FindNextUTF8(fs);
  end;
  FindCloseUTF8(fs);
  s.CustomSort(@ScriptListCompare);
  ScriptList.Items.Assign(s);
  ScriptList.ItemIndex:=0;
  ScriptList1.Items.Assign(s);
  ScriptList1.ItemIndex:=0;
  ScriptList2.Items.Assign(s);
  ScriptList2.ItemIndex:=0;
  f_selectscript.ComboBoxScript.Items.Assign(s);
  s.Free;
end;

procedure Tf_EditTargets.SetScriptList(n: integer; sl, args:string);
var i:integer;
begin
  i:=ScriptList.Items.IndexOf(sl);
  if i>=0 then ScriptList.ItemIndex:=i;
  TargetList.Cells[colplan,n]:=sl;
  ScriptParam.Text:=args;
end;

procedure Tf_EditTargets.SetScriptList2(n: integer; sl, args:string);
var i:integer;
begin
  i:=ScriptList2.Items.IndexOf(sl);
  if i>=0 then ScriptList2.ItemIndex:=i;
  ScriptParam2.Text:=args;
end;

procedure Tf_EditTargets.SetScriptList1(n: integer; sp, sl, args:string);
var i:integer;
    s:TStep;
begin
  s:=TStep(StepList.Objects[0,n]);
  i:=ScriptList1.Items.IndexOf(sl);
  if i>=0 then ScriptList1.ItemIndex:=i;
  StepList.Cells[pcoldesc,n]:=sl+' '+args;
  StepList.Cells[pcoltype,n]:=rsScript;
  s.scriptname:=sl;
  s.scriptpath:=sp;
  s.scriptargs:=args;
  ScriptParam1.Text:=args;
end;

procedure Tf_EditTargets.LoadSwitchList;
var i: integer;
begin
  Switches.Clear;
  Switches1.Clear;
  Switchlist.Clear;
  Switchlist1.Clear;
  SwitchValue.Text:='';
  SwitchValue1.Text:='';
  try
  for i:=0 to NumSwitches-1 do begin
     Switches.Items.Add(FSwitch[i].Nickname);
     Switches1.Items.Add(FSwitch[i].Nickname);
  end;
  if NumSwitches>0 then begin
    Switches.ItemIndex:=0;
    SwitchesChange(Switches);
    Switches1.ItemIndex:=0;
    Switches1Change(Switches1);
  end;
  except
  end;
end;

procedure Tf_EditTargets.SwitchesChange(Sender: TObject);
var i,j,n: integer;
    sw: TSwitchList;
begin
  Switchlist.Clear;
  SwitchValue.Text:='';
  n:=-1;
  for i:=0 to NumSwitches-1 do begin
     if Switches.Text=switch[i].Nickname then begin
       n:=i;
       break;
     end;
  end;
  if n>=0 then begin
    sw:=Fswitch[n].Switch;
    for j:=0 to Fswitch[n].NumSwitch-1 do begin
      Switchlist.Items.Add(sw[j].Name);
    end;
    Switchlist.ItemIndex:=0;
  end;
  TargetChange(Sender);
end;

procedure Tf_EditTargets.Switches1Change(Sender: TObject);
var i,j,n: integer;
    sw: TSwitchList;
begin
  Switchlist1.Clear;
  SwitchValue1.Text:='';
  n:=-1;
  for i:=0 to NumSwitches-1 do begin
     if Switches1.Text=switch[i].Nickname then begin
       n:=i;
       break;
     end;
  end;
  if n>=0 then begin
    sw:=Fswitch[n].Switch;
    for j:=0 to Fswitch[n].NumSwitch-1 do begin
      Switchlist1.Items.Add(sw[j].Name);
    end;
    Switchlist1.ItemIndex:=0;
  end;
  StepChange(Sender);
end;

procedure Tf_EditTargets.SetSwitch(n: integer; swl:string);
var i:integer;
    sw: TStringList;
begin
  sw:=TStringList.Create;
  SplitRec(swl,tab,sw);
  if sw.Count=3 then begin
    i:=Switches.Items.IndexOf(sw[0]);
    if (i>=0) then begin
      if i<>Switches.ItemIndex then begin
        Switches.ItemIndex:=i;
        SwitchesChange(Switches);
      end
    end
    else
       Switches.Text:=sw[0];
    i:=Switchlist.Items.IndexOf(sw[1]);
    if i>=0 then begin
       Switchlist.ItemIndex:=i;
    end
    else
       Switchlist.Text:=sw[1];
    SwitchValue.Text:=sw[2];
  end;
  TargetList.Cells[colplan,n]:=swl;
  sw.Free;
end;

procedure Tf_EditTargets.SetSwitch1(n: integer; swni,swna,swval:string);
var i:integer;
begin
  i:=Switches1.Items.IndexOf(swni);
  if (i>=0) then begin
    if i<>Switches1.ItemIndex then begin
      Switches1.ItemIndex:=i;
      Switches1Change(Switches1);
    end
  end
  else
     Switches1.Text:=swni;
  i:=Switchlist1.Items.IndexOf(swna);
  if i>=0 then begin
     Switchlist1.ItemIndex:=i;
  end
  else
     Switchlist1.Text:=swna;
  SwitchValue1.Text:=swval;
  StepList.Cells[pcoldesc,n]:=swni+', '+swna+'='+swval;
  StepList.Cells[pcoltype,n]:=rsSwitch;
end;

procedure Tf_EditTargets.BtnDeleteTemplateClick(Sender: TObject);
var txt,fn: string;
    n: integer;
begin
  n:=TargetList.Row;
  txt:=trim(TargetList.Cells[colplan,n]);
  if txt='' then exit;
  fn:=slash(SequenceDir)+txt+'.plan';
  if not FileExistsUTF8(fn) then exit;
  if MessageDlg(Format(rsDoYouWantToD, [fn]), mtConfirmation, mbYesNo, 0)=mrYes
    then begin
     DeleteFileUTF8(fn);
     LoadPlanList;
     TargetList.Cells[colplan,n]:='';
     PlanName.Caption:=TargetList.Cells[colplan,n];
     ShowPlan;
     TargetChange(nil);
     StepsModified:=false;
  end;
end;

procedure Tf_EditTargets.BtnImportObslistClick(Sender: TObject);
var obj:string;
    i,n: integer;
    t,tt: TTarget;
    f: textfile;
    title, buf, buf1: string;
    ra, de: double;
    firstrow: boolean;
const
  objl = 32;
  radecl = 10;
begin
  // Import Cartes du Ciel observation list
  OpenDialog1.Filter:='';
  if OpenDialog1.Execute then begin
     firstrow:=true;
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
       if firstrow then begin
          // select new plan
          buf:=FormEntryCB(self, TargetList.Columns[colplan-1].PickList, TargetList.Columns[colplan-1].Title.Caption, '');
          if (buf='')and(TargetList.Columns[colplan-1].PickList.Count>0) then buf:=TargetList.Columns[colplan-1].PickList[0];
          if buf<>'' then begin
            PlanName.Caption:=buf;
            t.planname:=buf;
            ShowPlan;
          end;
          firstrow:=false;
        end
        else begin
          if n>=1 then begin
            // copy previous row
            tt:=TTarget(TargetList.Objects[colseq,n]);
            if (tt.objectname<>ScriptTxt) and (tt.objectname<>SkyFlatTxt) then t.Assign(tt);
          end
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
     MoveFlat(Sender);
  end;
end;

procedure Tf_EditTargets.BtnImportMosaicClick(Sender: TObject);
var buf,buf2,obj,tra,tde,trot:string;
    i,n: integer;
    t,tt: TTarget;
    f: textfile;
    eq,ra,de: double;
    rec: Tstringlist;
    firstrow: boolean;
begin
  // Import Cartes du Ciel mosaic
  OpenDialog1.Filter:='CdC circle file |*.cdcc';
  if OpenDialog1.Execute then begin
     AssignFile(f, UTF8ToSys(OpenDialog1.FileName));
     reset(f);
     rec:=Tstringlist.Create;
     eq:=jdtoday;  // old cdc version do not store the equinox but most probably use equinox of date
     firstrow:=true;
     try
     while not EOF(f) do
     begin
       // read object
       readln(f,buf);
       if copy(buf,1,8)='EQUINOX='  then begin
          buf2:=copy(buf,9,99);
          eq:=StrToFloatDef(buf2,eq);
          if eq<>jd2000 then InitCoord(eq);
          continue;
       end;
       SplitRec(buf,' ',rec);
       obj:=StringReplace(rec[0],'Circle_','Mosaic_',[]);
       tra:=rec[1];
       tde:=rec[2];
       if rec.Count>3 then
         trot:=rec[3]
       else
         trot:='';
       if (obj=ScriptTxt)or(obj=SkyFlatTxt) then continue;
       ra := StrToAR(tra);
       if ra=NullCoord then continue;
       de := StrToDE(tde);
       if de=NullCoord then continue;
       ra:=deg2rad*15*ra;
       de:=deg2rad*de;
       if eq<>jd2000 then ApparentToJ2000(ra,de);
       ra:=rad2deg*ra/15;
       de:=rad2deg*de;
       // create new target
       t:=TTarget.Create;
       n:=TargetList.Row;
       if firstrow then begin
         // select new plan
         buf:=FormEntryCB(self, TargetList.Columns[colplan-1].PickList, TargetList.Columns[colplan-1].Title.Caption, '');
         if (buf='')and(TargetList.Columns[colplan-1].PickList.Count>0) then buf:=TargetList.Columns[colplan-1].PickList[0];
         if buf<>'' then begin
           PlanName.Caption:=buf;
           t.planname:=buf;
           ShowPlan;
         end;
         firstrow:=false;
       end
       else begin
         if n>=1 then begin
           // copy previous row
           tt:=TTarget(TargetList.Objects[colseq,n]);
           if (tt.objectname<>ScriptTxt) and (tt.objectname<>SkyFlatTxt) then t.Assign(tt);
         end
       end;
       // assign name and coordinates J2000
       t.objectname:=obj;
       t.ra:=ra;
       t.de:=de;
       // assign rotation angle
       if trot<>'' then
         t.pa := StrToFloatDef(trot,NullCoord)
       else
         t.pa:=NullCoord;
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
     finally
       InitCoord;
       CloseFile(f);
       rec.Free;
       TargetList.EditorMode := false;
     end;
     MoveFlat(Sender);
  end;
end;

procedure Tf_EditTargets.BtnNewObjectClick(Sender: TObject);
var i,n: integer;
    chkobj: string;
begin
  PanelTools.visible:=true;
  PageControlPlan.ActivePageIndex:=pageobject;
  NewObject;
  n:=TargetList.Row;
  keyboard_caption:=rsNewObject;
  keyboard_question:=rsObject;
  keyboard_text:='';
  keyboard1.showmodal; {get object data for name in keyboard_txt}

  if object_found {found object in database}  then
  begin
    TargetList.Cells[colra,n]:=RAToStr(ra_data*12/pi);{Add position}
    TargetList.Cells[coldec,n]:=DEToStr(dec_data*180/pi);
    TargetList.Cells[colstart,n]:=rsRise;
    TargetList.Cells[colend,n]:=rsSet2;
    TargetList.Cells[colname,n]:=keyboard_text;
    lblName.Caption:=rsAdditionalOp+' : Seq '+IntToStr(n)+', '+keyboard_text;
  end
  else
  begin
    FCoordWarning:=true;
    FCoordWarningRow:=n;
    chkobj:=keyboard_text+'##%%##';
    TargetList.Cells[colname,n]:=chkobj;
    lblName.Caption:=rsAdditionalOp+' : Seq '+IntToStr(n)+', '+chkobj;
  end;
  if FCoordWarning then begin
    // search if warning row is moved
    for i:=0 to TargetList.RowCount-1 do begin
      if TargetList.Cells[colname,i]=chkobj then begin
        TargetList.Cells[colname,i]:=keyboard_text;
        FCoordWarningRow:=i;
        break;
      end;
    end;
  end;  
  TargetChange(nil);
  Targetlist.Refresh;{apply Fcoordwarning directly}
  ShowPlan;
  Application.ProcessMessages;
  FCoordWarning:=false;
end;

procedure Tf_EditTargets.MenuInsertOnlineClick(Sender: TObject);
var n: integer;
begin
  PanelTools.visible:=true;
  PageControlPlan.ActivePageIndex:=pageobject;
  f_onlineinfo.Ra.Text:='';
  f_onlineinfo.De.Text:='';
  f_onlineinfo.ShowModal;
  if (f_onlineinfo.ModalResult=mrOK)and(f_onlineinfo.Ra.Text<>'')and(f_onlineinfo.De.Text<>'') then
  begin
    NewObject;
    n:=TargetList.Row;
    TargetList.Cells[colra,n]:=f_onlineinfo.Ra.Text;{Add position}
    TargetList.Cells[coldec,n]:=f_onlineinfo.De.Text;
    TargetList.Cells[colstart,n]:=rsRise;
    TargetList.Cells[colend,n]:=rsSet2;
    TargetList.Cells[colname,n]:=f_onlineinfo.Obj.Text;
    lblName.Caption:=rsAdditionalOp+' : Seq '+IntToStr(n)+', '+f_onlineinfo.Obj.Text;
    TargetChange(nil);
    ShowPlan;
  end;
end;

procedure Tf_EditTargets.MenuOnlineCoordClick(Sender: TObject);
var n: integer;
begin
  n:=TargetList.Row;
  f_onlineinfo.Obj.Text:=TargetList.Cells[colname,n];
  f_onlineinfo.Ra.Text:='';
  f_onlineinfo.De.Text:='';
  f_onlineinfo.LabelResolver.Caption:='';
  f_onlineinfo.ShowModal;
  if (f_onlineinfo.ModalResult=mrOK)and(f_onlineinfo.Ra.Text<>'')and(f_onlineinfo.De.Text<>'') then
  begin
    TargetList.Cells[colra,n]:=f_onlineinfo.Ra.Text;{Add position}
    TargetList.Cells[coldec,n]:=f_onlineinfo.De.Text;
    TargetList.Cells[colname,n]:=f_onlineinfo.Obj.Text;
    cbAstrometry.Checked:=(astrometryResolver<>ResolverNone);
  end
  else
  begin
    FCoordWarning:=true;
    FCoordWarningRow:=n;
  end;
  TargetChange(nil);
  Targetlist.Refresh;{apply Fcoordwarning directly}
  ShowPlan;
  Application.ProcessMessages;
  FCoordWarning:=false;
end;

procedure Tf_EditTargets.MenuBlankRowClick(Sender: TObject);
var i,n: integer;
    t: TTarget;
begin
  PanelTools.visible:=true;
  PageControlPlan.ActivePageIndex:=pageobject;
  t:=TTarget.Create;
  n:=TargetList.Row;
  t.objectname:='';
  PlanName.Caption:=t.planname;
  i:=n+1;
  TargetList.InsertColRow(false,i);
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:='';
  TargetList.Cells[colplan,i]:=t.planname;
  if t.astrometrypointing then cbAstrometry.Checked:=true;
  if t.inplaceautofocus then cbInplace.Checked:=true;
  if t.autofocustemp then cbAutofocusTemp.Checked:=true;
  if t.autofocushfd then cbAutofocusHFD.Checked:=true;
  cbNoAutoguidingChange.Checked:=false;
  cbMandatoryStartTime.Checked:=false;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  ResetSequences;
end;

procedure Tf_EditTargets.NewObject;
var i,n: integer;
    t,tt: TTarget;
begin
  t:=TTarget.Create;
  n:=TargetList.Row;
  if n>=1 then begin
    tt:=TTarget(TargetList.Objects[colseq,n]);
    if (tt.objectname<>ScriptTxt) and (tt.objectname<>SkyFlatTxt) then begin
      t.Assign(tt);
      t.objectname:='';
    end;
  end;
  PlanName.Caption:=t.planname;
  i:=n+1;
  TargetList.InsertColRow(false,i);
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:='';
  TargetList.Cells[colplan,i]:=t.planname;
  if t.astrometrypointing then cbAstrometry.Checked:=true;
  if t.inplaceautofocus then cbInplace.Checked:=true;
  if t.autofocustemp then cbAutofocusTemp.Checked:=true;
  if t.autofocushfd then cbAutofocusHFD.Checked:=true;
  cbNoAutoguidingChange.Checked:=false;
  cbMandatoryStartTime.Checked:=false;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  ResetSequences;
end;

procedure Tf_EditTargets.BtnNewScriptClick(Sender: TObject);
var i: integer;
    t: TTarget;
begin
  PanelTools.visible:=false;
  PageControlPlan.ActivePageIndex:=pagescript;
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

procedure Tf_EditTargets.MenuSwitchClick(Sender: TObject);
var i: integer;
    t: TTarget;
begin
  PanelTools.visible:=false;
  PageControlPlan.ActivePageIndex:=pageswitch;
  t:=TTarget.Create;
  t.objectname:=SwitchTxt;
  TargetList.RowCount:=TargetList.RowCount+1;
  i:=TargetList.RowCount-1;
  TargetList.Cells[colseq,i]:=IntToStr(i);
  TargetList.Cells[colname,i]:=SwitchTxt;
  TargetList.Cells[colplan,i]:=t.planname;
  TargetList.Objects[colseq,i]:=t;
  TargetList.Row:=i;
  TargetChange(nil);
end;


procedure Tf_EditTargets.Btn_coord_internalClick(Sender: TObject);{Retrieve position from deepsky database}
var n: integer;
begin
  n:=TargetList.Row;

  keyboard_caption:=rsRetrievePosi;
  keyboard_question:=rsObject;
  keyboard_text:=TargetList.Cells[colname,n];
  keyboard1.showmodal; {get object data for name in keyboard_txt}

  if object_found {find object in database}  then
  begin
    TargetList.Cells[colra,n]:=RAToStr(ra_data*12/pi);{Add position}
    TargetList.Cells[coldec,n]:=DEToStr(dec_data*180/pi);
    TargetList.Cells[colname,n]:=keyboard_text;
    cbAstrometry.Checked:=(astrometryResolver<>ResolverNone);
  end
  else
  begin
    FCoordWarning:=true;
    FCoordWarningRow:=n;
  end;
  TargetChange(nil);
  Targetlist.Refresh;{apply Fcoordwarning directly}
  ShowPlan;
  Application.ProcessMessages;
  FCoordWarning:=false;
end;

procedure Tf_EditTargets.BtnTimingClick(Sender: TObject);
var f: Tf_viewtext;
    tt:T_Targets;
    s1,s2: string;
begin
  f:=Tf_viewtext.Create(self);
  tt:=T_Targets.Create(f);
  GetTargets(tt,s1,s2);
  tt.CheckStatus;
  f:=Tf_viewtext.Create(self);
  f.Caption:=rsStatus2;
  f.Memo1.Text:=rsSequence+blank+tt.TargetName+crlf+crlf+tt.DoneStatus;
  FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
  f.Show;
end;

procedure Tf_EditTargets.cbChange(Sender: TObject);
begin
  if Sender=cbUpdCoord then begin
    PanelSolarTracking.Visible:=TCheckBox(Sender).Checked and FSolarTracking;
  end;
  if Sender=cbInitScript then begin
    Panel16.Visible:=TCheckBox(Sender).Checked;
  end;
  TargetChange(Sender);
end;

procedure Tf_EditTargets.BtnInsertPopup(Sender: TObject);
var p: TPoint;
begin
p:=Point(Tcontrol(Sender).Left,Tcontrol(Sender).Top+Tcontrol(Sender).Height);
p:=Tcontrol(Sender).Parent.ClientToScreen(p);
PopupInsert.PopUp(p.x,p.y);
end;

procedure Tf_EditTargets.BtnOptionsPopup(Sender: TObject);
var p: TPoint;
begin
p:=Point(Tcontrol(Sender).Left,Tcontrol(Sender).Top+Tcontrol(Sender).Height);
p:=Tcontrol(Sender).Parent.ClientToScreen(p);
PopupOptions.PopUp(p.x,p.y);
end;

procedure Tf_EditTargets.BtnSaveTemplatePopup(Sender: TObject);
var p: TPoint;
begin
p:=Point(Tcontrol(Sender).Left,Tcontrol(Sender).Top+Tcontrol(Sender).Height);
p:=Tcontrol(Sender).Parent.ClientToScreen(p);
PopupSaveTemplate.PopUp(p.x,p.y);
end;

procedure Tf_EditTargets.StartOptItemClick(Sender: TObject; Index: integer);
begin
  if Lockcb then exit;
  try
  Lockcb:=true;
  if (Index=ccNone) then begin
     StartOpt.Checked[ccNone]:=true;
     StartOpt.Checked[ccCool]:=false;
     StartOpt.Checked[ccUnpark]:=false;
     StartOpt.Checked[ccScript]:=false;
  end;
  if (Index<>ccNone) and StartOpt.Checked[Index] then begin
     StartOpt.Checked[ccNone]:=false;
  end;
  if (Index=ccScript) and StartOpt.Checked[ccScript] then begin
     f_selectscript.SetScript(StartScript);
     FormPos(f_selectscript,mouse.CursorPos.X,mouse.CursorPos.Y);
     if (f_selectscript.ShowModal=mrOK)and(f_selectscript.ComboBoxScript.ItemIndex>=0) then begin
        StartScript:=f_selectscript.ComboBoxScript.Items[f_selectscript.ComboBoxScript.ItemIndex];
     end;
  end;
  SetStartScriptName;
  finally
  Lockcb:=false;
  end;
end;

procedure Tf_EditTargets.SetStartScriptName;
begin
  if StartOpt.Checked[ccScript] then
    StartOpt.Items[ccScript]:=rsRunAScript+': '+StartScript
  else
    StartOpt.Items[ccScript]:=rsRunAScript;
end;

procedure Tf_EditTargets.TermOptItemClick(Sender: TObject; Index: integer);
begin
  if Lockcb then exit;
  try
  Lockcb:=true;
  if (Index=cbNone) then begin
     TermOpt.Checked[cbNone]:=true;
     TermOpt.Checked[cbStopTracking]:=false;
     TermOpt.Checked[cbParkScope]:=false;
     TermOpt.Checked[cbParkDome]:=false;
     TermOpt.Checked[cbWarm]:=false;
     TermOpt.Checked[cbScript]:=false;
  end;
  if (Index<>cbNone) and TermOpt.Checked[Index] then begin
     TermOpt.Checked[cbNone]:=false;
  end;
  if (Index=cbStopTracking) and TermOpt.Checked[cbStopTracking] then begin
     TermOpt.Checked[cbParkScope]:=false;
  end;
  if (Index=cbParkScope) and TermOpt.Checked[cbParkScope] then begin
     TermOpt.Checked[cbStopTracking]:=false;
  end;
  if (Index=cbScript) and TermOpt.Checked[cbScript] then begin
     f_selectscript.SetScript(EndScript);
     FormPos(f_selectscript,mouse.CursorPos.X,mouse.CursorPos.Y);
     if (f_selectscript.ShowModal=mrOK)and(f_selectscript.ComboBoxScript.ItemIndex>=0) then begin
        EndScript:=f_selectscript.ComboBoxScript.Items[f_selectscript.ComboBoxScript.ItemIndex];
     end;
  end;
  if (Index=cbUnattended) and TermOpt.Checked[cbUnattended] then begin
     f_selectscript.SetScript(UnattendedScript);
     FormPos(f_selectscript,mouse.CursorPos.X,mouse.CursorPos.Y);
     if (f_selectscript.ShowModal=mrOK)and(f_selectscript.ComboBoxScript.ItemIndex>=0) then begin
        UnattendedScript:=f_selectscript.ComboBoxScript.Items[f_selectscript.ComboBoxScript.ItemIndex];
     end;
  end;
  SetEndScriptName;
  finally
  Lockcb:=false;
  end;
end;

procedure Tf_EditTargets.SetEndScriptName;
begin
  if TermOpt.Checked[cbScript] then
    TermOpt.Items[cbScript]:=rsRunAScript+': '+EndScript
  else
    TermOpt.Items[cbScript]:=rsRunAScript;
  if TermOpt.Checked[cbUnattended] then
    TermOpt.Items[cbUnattended]:=rsUnattendedEr+': '+UnattendedScript
  else
    TermOpt.Items[cbUnattended]:=rsUnattendedEr;
end;

procedure Tf_EditTargets.FormResize(Sender: TObject);
begin
  if planheight<0 then planheight:=PanelPlan.Height;
  Splitter1.Width:=ClientWidth;
  Splitter1.Top:=ClientHeight-planheight-PanelBtn.Height-Splitter1.Height;
end;

procedure Tf_EditTargets.Splitter1Moved(Sender: TObject);
begin
  planheight:=PanelPlan.Height
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
  PanelTools.visible:=false;
  PageControlPlan.ActivePageIndex:=pageflat;
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
  t.FlatOffset:=Offset;
  t.FlatFstop:='';
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
    i,n:integer;
    newscript: boolean;
    s: TStringList;
    ns: Tf_newscript;
    st: TScriptType;
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
    ns:=Tf_newscript.Create(self);
    ns.ShowModal;
    if ns.ModalResult<>mrOK then exit;
    txt:=trim(ns.Edit1.text);
    if txt='' then exit;
    st:=TScriptType(ns.ScriptLanguage.ItemIndex+1);
    ns.free;
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=slash(ConfigDir)+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg(Format(rsScriptAlread2, [fn]), mtConfirmation, mbYesNo, 0)=mrYes then
         s.LoadFromFile(fn)
       else
         exit;
    end
    else begin
      if st=stPascal then begin
        s.Add('{');
        s.Add('Pascal script for CCDciel');
        s.Add('see: https://www.ap-i.net/ccdciel/en/documentation/script_reference');
        s.Add('}');
        s.Add('begin');
        s.Add('');
        s.Add('end.');
      end
      else if st=stPython then begin
        s.Add('# Python program for CCDciel');
        s.Add('# see: https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference');
        s.Add('');
        s.Add('from ccdciel import ccdciel');
        s.Add('');
      end;
    end;
    f_pascaleditor.ScriptName:=txt;
    f_pascaleditor.ScriptType:=st;
  end
  else begin
    i:=ScriptList.ItemIndex;
    if i<0 then exit;
    txt:=ScriptList.Items[i];
    if (txt='') then exit;
    fn:=slash(ConfigDir)+txt+'.script';
    s.LoadFromFile(fn);
    f_pascaleditor.ScriptType:=f_scriptengine.ScriptType(fn);
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
     SetScriptList(n,f_pascaleditor.ScriptName,ScriptParam.Text);
     TargetChange(nil);
    end;
  end;
  s.Free;
end;

procedure Tf_EditTargets.BtnScript1Click(Sender: TObject);
var txt,fn: string;
    i,n:integer;
    newscript: boolean;
    s: TStringList;
    ns: Tf_newscript;
    st: TScriptType;
begin
  n:=StepList.Row;
  newscript:=(Sender=BtnEditNewScript1)or(ScriptList1.Text='');
  s:=TStringList.Create;
  if f_pascaleditor=nil then begin
     f_pascaleditor:=Tf_pascaleditor.Create(self);
     f_pascaleditor.DebugScript:=f_scriptengine.dbgscr;
  end;
  if newscript then begin
    s.Clear;
    ns:=Tf_newscript.Create(self);
    ns.ShowModal;
    if ns.ModalResult<>mrOK then exit;
    txt:=trim(ns.Edit1.text);
    if txt='' then exit;
    st:=TScriptType(ns.ScriptLanguage.ItemIndex+1);
    ns.free;
    if copy(txt,1,2)='T_' then delete(txt,1,2);
    fn:=slash(ConfigDir)+txt+'.script';
    if FileExistsUTF8(fn) then begin
       if MessageDlg(Format(rsScriptAlread2, [fn]), mtConfirmation, mbYesNo, 0)=mrYes then
         s.LoadFromFile(fn)
       else
         exit;
    end
    else begin
      if st=stPascal then begin
        s.Add('{');
        s.Add('Pascal script for CCDciel');
        s.Add('see: https://www.ap-i.net/ccdciel/en/documentation/script_reference');
        s.Add('}');
        s.Add('begin');
        s.Add('');
        s.Add('end.');
      end
      else if st=stPython then begin
        s.Add('# Python program for CCDciel');
        s.Add('# see: https://www.ap-i.net/ccdciel/en/documentation/jsonrpc_reference');
        s.Add('');
        s.Add('from ccdciel import ccdciel');
        s.Add('');
      end;
    end;
    f_pascaleditor.ScriptName:=txt;
    f_pascaleditor.ScriptType:=st;
  end
  else begin
    i:=ScriptList1.ItemIndex;
    if i<0 then exit;
    txt:=ScriptList1.Items[i];
    if (txt='') then exit;
    fn:=slash(ConfigDir)+txt+'.script';
    s.LoadFromFile(fn);
    f_pascaleditor.ScriptType:=f_scriptengine.ScriptType(fn);
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
     SetScriptList1(n,ConfigDir,f_pascaleditor.ScriptName,ScriptParam1.Text);
     StepChange(nil);
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

procedure Tf_EditTargets.BtnApplyTemplateClick(Sender: TObject);
var s:TStep;
    p:T_Plan;
    t: TTarget;
    i,j,n: integer;
begin
  BtnSaveTemplateClick(Sender); // ensure modification are saved
  for j:=1 to TargetList.RowCount-1 do begin
    if StringReplace(TargetList.Cells[colplan,j],'*','',[])=PlanName.Caption then begin
      TargetList.Cells[colplan,j]:=PlanName.Caption;
      t:=TTarget(TargetList.Objects[colseq,j]);
      p:=T_Plan(t.plan);
      p.Clear;
      p.PlanName:=PlanName.Caption;
      t.PlanName:=PlanName.Caption;
      n:=StepList.RowCount-1;
      for i:=1 to n do begin
         s:=TStep.Create;
         s.Assign(TStep(StepList.Objects[0,i]));
         p.Add(s);
      end;
    end;
  end;
end;

procedure Tf_EditTargets.BtnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure Tf_EditTargets.BtnPlanetariumCoordClick(Sender: TObject);
var n: integer;
begin
  n:=TargetList.Row;
  f_planetariuminfo.Ra.Text  := TargetList.Cells[colra,n];
  f_planetariuminfo.De.Text  := TargetList.Cells[coldec,n];
  f_planetariuminfo.PA.Text := TargetList.Cells[colpa,n];
  f_planetariuminfo.Obj.Text := TargetList.Cells[colname,n];
  f_planetariuminfo.onNewTarget := nil;
  FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_planetariuminfo.ShowModal;
  if f_planetariuminfo.ModalResult=mrOK then begin
    TargetList.Cells[colra,n]:=f_planetariuminfo.Ra.Text;
    TargetList.Cells[coldec,n]:=f_planetariuminfo.De.Text;
    TargetList.Cells[colpa,n]:=f_planetariuminfo.PA.Text;
    if f_planetariuminfo.Obj.Text<>'' then TargetList.Cells[colname,n]:=trim(f_planetariuminfo.Obj.Text);
    cbAstrometry.Checked:=(astrometryResolver<>ResolverNone);
    TargetChange(nil);
  end;
end;

procedure Tf_EditTargets.BtnInsertPlanetariumClick(Sender: TObject);
var n: integer;
begin
  if (f_planetariuminfo.planetarium=nil) or (not f_planetariuminfo.planetarium.Connected) then begin
    ShowMessage(rsPleaseConnec);
    exit;
  end;
  n:=TargetList.Row;
  f_planetariuminfo.Ra.Text  := TargetList.Cells[colra,n];
  f_planetariuminfo.De.Text  := TargetList.Cells[coldec,n];
  f_planetariuminfo.PA.Text  := TargetList.Cells[colpa,n];
  f_planetariuminfo.Obj.Text := TargetList.Cells[colname,n];
  f_planetariuminfo.onNewTarget := @NewPlanetariumTarget;
  FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_planetariuminfo.ShowModal;
end;

procedure Tf_EditTargets.NewPlanetariumTarget(Sender: TObject);
var n: integer;
begin
  NewObject;
  n:=TargetList.Row;
  TargetList.Cells[colra,n]:=f_planetariuminfo.Ra.Text;
  TargetList.Cells[coldec,n]:=f_planetariuminfo.De.Text;
  TargetList.Cells[colpa,n]:=trim(f_planetariuminfo.PA.Text);
  if f_planetariuminfo.Obj.Text<>'' then TargetList.Cells[colname,n]:=trim(f_planetariuminfo.Obj.Text);
  cbAstrometry.Checked:=(astrometryResolver<>ResolverNone);
  TargetChange(nil);
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
    TargetList.Cells[colra,n]:=RAToStr(ra);
    TargetList.Cells[coldec,n]:=DEToStr(de);
    cbAstrometry.Checked:=(astrometryResolver<>ResolverNone);
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
SaveDialog1.InitialDir:=SequenceDir;
SaveDialog1.DefaultExt:='.targets';
SaveDialog1.filter:='CCDciel Sequence|*.targets';
if FFilename='' then begin
  n:=TargetList.RowCount;
  defaultname:=FormatDateTime('mmdd',now);
  for i:=1 to n-1 do begin
    if (TargetList.Cells[1,i]<>ScriptTxt) and (TargetList.Cells[1,i]<>SkyFlatTxt) then
       defaultname:=TargetList.Cells[1,i];
  end;
  SaveDialog1.FileName:=slash(SequenceDir)+defaultname+'.targets';
end
else begin
  if ObjectNameChange then begin
    n:=TargetList.RowCount;
    defaultname:=CurrentSeqName;
    for i:=1 to n-1 do begin
      if (TargetList.Cells[1,i]<>ScriptTxt) and (TargetList.Cells[1,i]<>SkyFlatTxt) then
         defaultname:=TargetList.Cells[1,i];
    end;
    SaveDialog1.FileName:=slash(SequenceDir)+defaultname+'.targets';
  end
  else begin
    SaveDialog1.FileName:=FFilename;
  end;
end;

if SaveDialog1.Execute then begin
  FFilename:=SaveDialog1.FileName;
  ModalResult:=mrOK;
end;
end;

procedure Tf_EditTargets.BtnSaveClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure Tf_EditTargets.BtnCurrentCoordClick(Sender: TObject);
begin
  TargetList.Cells[colra,TargetList.Row]:='-';
  TargetList.Cells[coldec,TargetList.Row]:='-';
  TargetChange(nil);
end;

procedure Tf_EditTargets.SavePlanModified(isTemplate: boolean);
var s:TStep;
    p:T_Plan;
    t: TTarget;
    i,n: integer;
begin
if StepsModified then begin
  t:=TTarget(TargetList.Objects[colseq,TargetList.Row]);
  p:=T_Plan(t.plan);
  p.Clear;
  p.PlanName:=PlanName.Caption;
  n:=StepList.RowCount-1;
  for i:=1 to n do begin
     s:=TStep.Create;
     s.Assign(TStep(StepList.Objects[0,i]));
     p.Add(s);
  end;
  if (not isTemplate) and TemplateModified(p) then begin
    if (pos('*',PlanName.Caption)=0) then begin
      PlanName.Caption:=PlanName.Caption+'*';
      t.planname:=PlanName.Caption;
      TargetList.Cells[colplan,TargetList.Row]:=t.PlanName;
    end;
  end
  else begin
    if (pos('*',PlanName.Caption)>0) then begin
      PlanName.Caption:=StringReplace(PlanName.Caption,'*','',[]);
      t.planname:=PlanName.Caption;
      TargetList.Cells[colplan,TargetList.Row]:=t.PlanName;
    end;
  end;
  StepsModified:=false;
  SetTemplateButton;
end;
end;

procedure Tf_EditTargets.SetTarget(n: integer; t: TTarget);
var i,j:integer;
    buf: string;
    filterlst:TStringList;
begin
  if (t=nil)or(n=0)or(n>=TargetList.RowCount) then exit;
  LockTarget:=true;
  TargetMsg.Caption:='';
  TargetList.Cells[colseq,n]:=IntToStr(n);
  TargetList.Cells[colname,n]:=t.objectname;
  lblName.Caption:=rsAdditionalOp+' : Seq '+IntToStr(n)+', '+t.objectname;
  if t.objectname=ScriptTxt then begin
    TargetList.Cells[colra,n]:='';
    TargetList.Cells[coldec,n]:='';
    TargetList.Cells[colpa,n]:='';
    TargetList.Cells[colstart,n]:='';
    TargetList.Cells[colend,n]:='';
    TargetList.Cells[colrepeat,n]:='';
    cbDarkNight.Checked:=false;
    cbSkip.Checked:=false;
    cbAstrometry.Checked:=false;
    cbInplace.Checked:=false;
    cbAutofocusTemp.Checked:=false;
    cbAutofocusHFD.Checked:=false;
    cbNoAutoguidingChange.Checked:=false;
    cbMandatoryStartTime.Checked:=false;
    cbUpdCoord.Checked:=false;
    PanelSolarTracking.Visible:=false;
    ScrollBox2.Visible:=false;
    PanelTools.visible:=false;
    PageControlPlan.ActivePageIndex:=pagescript;
    SetScriptList(n,t.planname,t.scriptargs);
  end
  else if t.objectname=SwitchTxt then begin
    TargetList.Cells[colra,n]:='';
    TargetList.Cells[coldec,n]:='';
    TargetList.Cells[colpa,n]:='';
    TargetList.Cells[colstart,n]:='';
    TargetList.Cells[colend,n]:='';
    TargetList.Cells[colrepeat,n]:='';
    cbDarkNight.Checked:=false;
    cbSkip.Checked:=false;
    cbAstrometry.Checked:=false;
    cbInplace.Checked:=false;
    cbAutofocusTemp.Checked:=false;
    cbAutofocusHFD.Checked:=false;
    cbNoAutoguidingChange.Checked:=false;
    cbMandatoryStartTime.Checked:=false;
    cbUpdCoord.Checked:=false;
    PanelSolarTracking.Visible:=false;
    ScrollBox2.Visible:=false;
    PanelTools.visible:=false;
    PageControlPlan.ActivePageIndex:=pageswitch;
    SetSwitch(n,t.planname);
  end
  else if t.objectname=SkyFlatTxt then begin
    PanelTools.visible:=false;
    ScrollBox2.Visible:=false;
    PageControlPlan.ActivePageIndex:=pageflat;
    if t.planname=FlatTimeName[0]
       then FlatTime.ItemIndex:=0
       else FlatTime.ItemIndex:=1;
    TargetList.Cells[colplan,n]:=t.planname;
    TargetList.Cells[colra,n]:='';
    TargetList.Cells[coldec,n]:='';
    TargetList.Cells[colpa,n]:='';
    TargetList.Cells[colstart,n]:='';
    TargetList.Cells[colend,n]:='';
    TargetList.Cells[colrepeat,n]:='';
    cbDarkNight.Checked:=false;
    cbSkip.Checked:=false;
    cbAstrometry.Checked:=false;
    cbInplace.Checked:=false;
    cbAutofocusTemp.Checked:=false;
    cbAutofocusHFD.Checked:=false;
    cbNoAutoguidingChange.Checked:=false;
    cbMandatoryStartTime.Checked:=false;
    cbUpdCoord.Checked:=false;
    PanelSolarTracking.Visible:=false;
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
    FOffsetEdit.Value:=t.FlatOffset;
    FFstopbox.Text:=t.FlatFstop;
    filterlst:=TStringList.Create();
    SplitRec(t.FlatFilters,';',filterlst);
    for i:=0 to FlatFilterList.Items.Count-1 do begin
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
    PanelTools.visible:=true;
    ScrollBox2.Visible:=true;
    PageControlPlan.ActivePageIndex:=pageobject;
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
    cbDarkNight.Checked:=t.darknight;
    cbSkip.Checked:=t.skip;
    cbAstrometry.Checked:=t.astrometrypointing;
    cbInplace.Checked:=t.inplaceautofocus;
    cbAutofocusTemp.Checked:=t.autofocustemp;

    cbInitScript.Checked:=t.initscript;
    panel16.Visible:=t.initscript;
    SetScriptList2(n,t.initscriptname,t.initscriptargs);

    // When the form is saved, the corresponding values for each target are
    // saved off. IOW, the form determines the target value, and vice versa
    // when the target is selected/loaded.
    cbAutofocusHFD.Checked:=t.autofocushfd;
    cbNoAutoguidingChange.Checked:=t.noautoguidingchange;
    cbMandatoryStartTime.Checked:=t.mandatorystarttime;
    cbUpdCoord.Checked:=t.updatecoord;
    PanelSolarTracking.Visible:=t.updatecoord and FSolarTracking;
    cbSolarTracking.Checked:=t.solartracking and t.updatecoord and FSolarTracking;
    if t.pa=NullCoord then
      TargetList.Cells[colpa,n]:='-'
    else
      TargetList.Cells[colpa,n]:=FormatFloat(f2,t.pa);
    TargetList.Cells[colrepeat,n]:=IntToStr(t.repeatcount);
    PanelRepeat.Visible:=t.repeatcount>1;
    TDelay.Value:=round(t.delay);
    PreviewExposure.Value:=t.previewexposure;
    Preview.Checked:=t.preview;
    PlanName.Caption:=TargetList.Cells[colplan,n];
    ShowPlan;
    CheckRiseSet(n);
  end;
  LockTarget:=false;
end;

procedure Tf_EditTargets.TargetListSelection(Sender: TObject; aCol, aRow: Integer);
var t: TTarget;
begin
  t:=TTarget(TargetList.Objects[colseq,aRow]);
  SetTarget(aRow,t);
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
    oldid: LongWord;
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
  oldid:=t.id;
  if t.objectname=ScriptTxt then begin
    PanelTools.visible:=false;
    PageControlPlan.ActivePageIndex:=pagescript;
    i:=ScriptList.ItemIndex;
    if i>=0 then
      sname:=ScriptList.Items[i]
    else
      sname:='';
    TargetList.Cells[colplan,n]:=sname;
    t.planname:=sname;
    t.scriptargs:=trim(ScriptParam.Text);
    t.path:=slash(ConfigDir);
  end
  else if t.objectname=SwitchTxt then begin
    PanelTools.visible:=false;
    PageControlPlan.ActivePageIndex:=pageswitch;
    buf:=Switches.Text+tab+Switchlist.Text+tab+SwitchValue.Text;
    TargetList.Cells[colplan,n]:=buf;
    t.planname:=buf;
  end
  else if t.objectname=SkyFlatTxt then begin
    PanelTools.visible:=false;
    PageControlPlan.ActivePageIndex:=pageflat;
    t.planname:=FlatTimeName[FlatTime.ItemIndex];
    t.FlatCount:=FlatCount.Value;
    t.FlatFstop:=FFstopbox.Text;
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
    else if hasGain then begin
       t.FlatGain:=FGainEdit.Value;
    end;
    t.FlatOffset:=FOffsetEdit.Value;
    t.FlatFilters:='';
    for j:=0 to FlatFilterList.Items.Count-1 do begin
      if FlatFilterList.Checked[j] then
         t.FlatFilters:=t.FlatFilters+FlatFilterList.Items[j]+';';
    end;
  end
  else begin
    PanelTools.visible:=true;
    PageControlPlan.ActivePageIndex:=pageobject;
    CheckRiseSet(n);
    if (t.objectname=CurrentSeqName)and(trim(TargetList.Cells[colname,n])<>CurrentSeqName) then
       ObjectNameChange:=true;
    t.objectname:=trim(TargetList.Cells[colname,n]);
    planchange:=(t.planname<>TargetList.Cells[colplan,n]);
    t.planname:=TargetList.Cells[colplan,n];
    SetStartTime(trim(TargetList.Cells[colstart,n]),t);
    SetEndTime(trim(TargetList.Cells[colend,n]),t);
    t.initscript:=cbInitScript.Checked;
    i:=ScriptList2.ItemIndex;
    if i>=0 then
      t.initscriptname:=ScriptList2.Items[i]
    else
      t.initscriptname:='';
    t.initscriptpath:=slash(ConfigDir);
    t.initscriptargs:=ScriptParam2.text;
    if TargetList.Cells[colra,n]='-' then
      t.ra:=NullCoord
    else
      t.ra:=StrToAR(TargetList.Cells[colra,n]);
    if TargetList.Cells[coldec,n]='-' then
      t.de:=NullCoord
    else
      t.de:=StrToDE(TargetList.Cells[coldec,n]);
    t.darknight:=cbDarkNight.Checked;
    t.skip:=cbSkip.Checked;
    if TargetList.Cells[colpa,n]='-' then
      t.pa:=NullCoord
    else
      t.pa:=StrToFloatDef(TargetList.Cells[colpa,n],t.pa);
    if (cbAstrometry.Checked)and ((t.ra=NullCoord)or(t.de=NullCoord)) then cbAstrometry.Checked:=false;
    t.astrometrypointing:=cbAstrometry.Checked;
    t.inplaceautofocus:=cbInplace.Checked;
    t.autofocustemp:=cbAutofocusTemp.Checked;

    // set the target autofocus value based on the value set in the form. The
    // form drives the target value, and vice versa when the target is selected
    t.autofocushfd:=cbAutofocusHFD.Checked;
    t.noautoguidingchange:=cbNoAutoguidingChange.Checked;
    if (t.starttime>0) and (not t.startrise) and (t.startmeridian=NullCoord) then
      t.mandatorystarttime:=cbMandatoryStartTime.Checked
    else begin
      t.mandatorystarttime:=false;
      cbMandatoryStartTime.Checked:=false;
    end;
    t.updatecoord:=cbUpdCoord.Checked;
    t.solartracking:=t.updatecoord and cbSolarTracking.Checked and FSolarTracking;
    t.repeatcount:=StrToIntDef(TargetList.Cells[colrepeat,n],1);
    if t.repeatcount<0 then t.repeatcount:=1;
    PanelRepeat.Visible:=t.repeatcount>1;
    t.delay:=TDelay.Value;
    t.previewexposure:=PreviewExposure.Value;
    t.preview:=Preview.Checked;
    if planchange then begin
      PlanName.Caption:=t.planname;
      if trim(t.planname)>'' then LoadTemplate;
      SetTemplateButton;
    end;
  end;
  if t.id<>oldid then begin
    // this is another target, reset all done count
    t.repeatdone:=0;
    for i:=0 to T_Plan(t.plan).Count-1 do begin
      T_Plan(t.plan).Steps[i].donecount:=0;
    end;
  end;
end;

function Tf_EditTargets.CheckRiseSet(n: integer; showmsg:boolean=true): boolean;
var ra,de,h: double;
    i:integer;
begin
result:=true;
targetmsg.Caption:='';
ra:=StrToAR(TargetList.Cells[colra,n]);
de:=StrToDE(TargetList.Cells[coldec,n]);
if (ra<>NullCoord)and(de<>NullCoord) then begin
  if (TargetList.Cells[colstart,n]='') then TargetList.Cells[colstart,n]:=rsRise;
  if (TargetList.Cells[colend,n]='') then TargetList.Cells[colend,n]:=rsSet2;
end;
if (TargetList.Cells[colstart,n]='')and(TargetList.Cells[colend,n]='')and(not cbDarkNight.Checked) then
   cbSkip.Checked:=false;
if TargetList.Cells[colstart,n]=rsRise then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then targetmsg.Caption:=(rsCannotComput+crlf+rsInvalidObjec);
     TargetList.Cells[colstart,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=1 then begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs);
      end
      else begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs2);
      end;
      TargetList.Cells[colstart,n]:='';
      result:=false;
    end;
  end;
end;
if TargetList.Cells[colend,n]=rsSet2 then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then targetmsg.Caption:=(rsCannotComput+crlf+rsInvalidObjec);
     TargetList.Cells[colend,n]:='';
     result:=false;
  end
  else begin
    if not ObjSet(ra,de,h,i) then begin
      if i=1 then begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs);
      end
      else begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs2);
      end;
      TargetList.Cells[colend,n]:='';
      result:=false;
    end;
  end;
end;
if copy(trim(TargetList.Cells[colstart,n]),1,length(MeridianCrossing))=MeridianCrossing then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then targetmsg.Caption:=(rsCannotComput2+crlf+rsInvalidObjec);
     TargetList.Cells[colstart,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=2 then begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs2);
      end;
      TargetList.Cells[colstart,n]:='';
      result:=false;
    end;
  end;
end;
if copy(trim(TargetList.Cells[colend,n]),1,length(MeridianCrossing))=MeridianCrossing then begin
  if (ra=NullCoord)or(de=NullCoord) then begin
     if showmsg then targetmsg.Caption:=(rsCannotComput2+crlf+rsInvalidObjec);
     TargetList.Cells[colend,n]:='';
     result:=false;
  end
  else begin
    if not ObjRise(ra,de,h,i) then begin
      if i=2 then begin
        if showmsg then targetmsg.Caption:=(rsThisObjectIs2);
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

  procedure GetPrefix(str: string; out pref: string; out n: double; out i: integer);
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

procedure Tf_EditTargets.TargetListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
 if FCoordWarning then begin
   if ((aCol=colra)or(aCol=coldec))and(aRow=FCoordWarningRow) then begin
      TargetList.Canvas.Brush.Style:=bsSolid;
      TargetList.Canvas.Brush.Color:=clred;
      TargetList.Canvas.FillRect(aRect);
      TargetList.Canvas.TextOut(aRect.Left,aRect.Top,TargetList.Cells[aCol,aRow]);
   end;
 end;
end;

procedure Tf_EditTargets.TargetListEditingDone(Sender: TObject);
begin
  TargetChange(Sender);
end;

procedure Tf_EditTargets.TargetListGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
TargetList.EditorMode := false; // mouse move to new cell, auto-close editor
if ARow=0 then begin
  case ACol of
    colseq        : HintText:=rsDragDropToCh;
    colname       : HintText:=Format(rsClickToSortB, [rsObjectName]);
    colra         : HintText:=Format(rsClickToSortB, [rsRA]);
    coldec        : HintText:=Format(rsClickToSortB, [rsDec]);
    else HintText:=rsClickToSetVa;
  end;
end
else
  case ACol of
    colseq        : HintText:=rsDragDropToCh;
    colname       : HintText:=rsTheTargetNam;
    colplan       : HintText:=Format(rsStepPlanFor, [crlf]);
    colra         : HintText:=rsTargetPositi;
    coldec        : HintText:=rsTargetPositi;
    colpa         : HintText:=rsCameraPositi;
    colstart      : HintText:=rsStartTimeCap;
    colend        : HintText:=rsStopTimeCapt;
    colrepeat     : HintText:=Format(rsRepeatThePla, [crlf]);
    else HintText:=rsTheListOfTar;
  end;
end;

procedure Tf_EditTargets.TargetListHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
var i: integer;
    buf: string;
    t: TTarget;
begin
 if IsColumn and (TargetList.RowCount>1) then with TargetList do begin
  case index of
    colplan : begin
                buf:=FormEntryCB(self, Columns[Index-1].PickList, Columns[Index-1].Title.Caption, '');
                if buf<>'' then begin
                  for i:=1 to RowCount-1 do
                     if (Cells[colname,i]<>SkyFlatTxt)and(Cells[colname,i]<>ScriptTxt) then begin
                       Cells[Index,i]:=buf;
                       t:=TTarget(Objects[colseq,i]);
                       t.planname:=buf;
                       PlanName.Caption:=t.planname;
                       Row:=i;
                       LoadTemplate;
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
                MoveFlat(Sender);
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
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
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
                     t:=TTarget(Objects[colseq,i]);
                     SetStartTime(trim(Cells[colstart,i]),t);
                     SetEndTime(trim(Cells[colend,i]),t);
                   end;
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
 TargetList.EditorMode := false;
end;

procedure Tf_EditTargets.TargetListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Shift = [ssCtrl]) and (key <> VK_CONTROL) then
 begin
   // Ctrl + key handling
   case key of
     VK_A: begin  // ctrl+a
           MenuBlankRowClick(Sender);
           TargetList.SetFocus;
           TargetList.EditorMode:=True;
       end;
     VK_O: begin  // ctrl+o
           BtnNewObjectClick(Sender);
           TargetList.SetFocus;
           TargetList.EditorMode:=True;
       end;
   end;
 end;
end;

procedure Tf_EditTargets.TargetListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var acol,arow: integer;
begin
  if Button=mbRight then begin
    TargetList.MouseToCell(x,y,acol,arow);
    if (acol>=0)and(acol<TargetList.ColCount)and(arow>0)and(arow<TargetList.RowCount) then begin
      TargetList.Row:=arow;
      TargetChange(TargetList);
      if acol<=1 then
        PopupInsert.PopUp(mouse.CursorPos.X,mouse.CursorPos.Y)
      else if (TargetList.Cells[colname,arow]<>SkyFlatTxt)and(TargetList.Cells[colname,arow]<>ScriptTxt) then
        PopupOptions.PopUp(mouse.CursorPos.X,mouse.CursorPos.Y);
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
 else
    Editor:=TargetList.EditorByStyle(cbsAuto);
end;

procedure Tf_EditTargets.FlatTimeClick(Sender: TObject);
var i,n: integer;
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
  MoveFlat(Sender);
end;

procedure Tf_EditTargets.MoveFlat(Sender: TObject);
var r1,r2,i,n: integer;
    t:TTarget;
begin
  n:=-1;
  for i:=1 to TargetList.RowCount-1 do begin
    if TargetList.Cells[colname,i]=SkyFlatTxt then n:=i;
  end;
  if n>=0 then begin
    if FlatTime.ItemIndex=0 then begin
       r1:=n;
       r2:=1;
       t:=TTarget(TargetList.Objects[colseq,r1]);
       t.planname:=FlatTimeName[0];
       TargetList.Cells[colplan,r1]:=t.planname;
       TargetList.MoveColRow(false,r1,r2);
       SeqStartTwilight.Checked:=false;
       SeqStart.Checked:=false;
    end
    else begin
      r1:=n;
      t:=TTarget(TargetList.Objects[colseq,r1]);
      t.planname:=FlatTimeName[1];
      TargetList.Cells[colplan,r1]:=t.planname;
      r2:=TargetList.RowCount-1;
      TargetList.MoveColRow(false,r1,r2);
      SeqStopTwilight.Checked:=false;
      SeqStop.Checked:=false;
    end;
    if Sender<>nil then TargetChange(Sender);
  end;
end;

procedure Tf_EditTargets.CheckRestartStatus(Sender: TObject);
begin
  RepeatCountList.Enabled:=CheckBoxRepeatList.Checked;
  BtnRepeatInf.Enabled:=RepeatCountList.Enabled;
  CheckBoxResetRepeat.Enabled:=CheckBoxRestartStatus.Checked and CheckBoxRepeatList.Checked and (RepeatCountList.Value>1);
end;

procedure Tf_EditTargets.CheckBoxRepeatListChange(Sender: TObject);
begin
  CheckRestartStatus(nil);
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=RepeatCountList.Value
  else
    FTargetsRepeat:=1;
end;

procedure Tf_EditTargets.RepeatCountListChange(Sender: TObject);
begin
  CheckRestartStatus(nil);
  if CheckBoxRepeatList.Checked then
    FTargetsRepeat:=RepeatCountList.Value;
end;

procedure Tf_EditTargets.FlatFilterListItemClick(Sender: TObject; Index: integer);
begin
  TargetChange(Sender);
end;

function Tf_EditTargets.AsDuskFlat:boolean;
var i:integer;
begin
result:=false;
for i:=1 to TargetList.RowCount-1 do
  if (TargetList.Cells[colname,i]=SkyFlatTxt) and (TargetList.Cells[colplan,i]=FlatTimeName[0])
    then result:=true;
end;

function Tf_EditTargets.AsDawnFlat:boolean;
var i:integer;
begin
result:=false;
for i:=1 to TargetList.RowCount-1 do
  if (TargetList.Cells[colname,i]=SkyFlatTxt) and (TargetList.Cells[colplan,i]=FlatTimeName[1])
    then result:=true;
end;

procedure Tf_EditTargets.SeqStartChange(Sender: TObject);
begin
  if SeqStart.Checked and AsDuskFlat then begin
    ShowMessage(rsCannotChange);
    SeqStart.Checked:=false;
  end;
  SeqStartTwilight.Enabled:=SeqStart.Checked;
  SeqStartAt.Enabled:=SeqStart.Checked and (not SeqStartTwilight.Checked);
end;

procedure Tf_EditTargets.SeqStopChange(Sender: TObject);
begin
  if SeqStop.Checked and AsDawnFlat then begin
    ShowMessage(rsCannotChange2);
    SeqStop.Checked:=false;
  end;
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
    gainmsg:boolean;
begin
  StepsModified:=false;
  p.steptype:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Type',0); // capture by default for compatibility  
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Description','');
  p.description:=str;
  str:=trim(pfile.GetValue('/Steps/Step'+inttostr(i)+'/FrameType','Light'));
  p.frtype:=Str2Frametype(str);
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
  gainmsg:=(str='');
  p.gain:=StrToIntDef(str,Gain);
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Offset','');
  gainmsg:=gainmsg or (str='');
  p.offset:=StrToIntDef(str,Offset);
  str:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Filter','');
  if i<SaveFilterNum then originalFilter[i]:=str;
  j:=StepList.Columns[pcolfilter-1].PickList.IndexOf(str);
  if j<0 then j:=0;
  p.filter:=j;
  p.fstop:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Fstop','');
  p.exposure:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Exposure',1.0);
  p.stackcount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/StackCount',1));
  p.count:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/Count',1));
  p.dither:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Dither',false);
  p.dithercount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/DitherCount',1));
  p.autofocusstart:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',false);
  p.autofocus:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/Autofocus',false);
  p.autofocuscount:=trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',10));
  p.scriptname:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptName','');
  p.scriptpath:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptPath','');
  p.scriptargs:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/ScriptArgs','');
  p.switchnickname:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchNickname','');
  p.switchname:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchName','');
  p.switchvalue:=pfile.GetValue('/Steps/Step'+inttostr(i)+'/SwitchValue','');
  if gainmsg and StepList.Columns[pcolgain-1].Visible then
     LabelMsg.Caption:=rsPleaseBeCare;
  // obsolete option
  if trunc(pfile.GetValue('/Steps/Step'+inttostr(i)+'/RepeatCount',1)) > 1 then
     msg:=msg+crlf+p.description+' warning! the Repeat option at the step level as been removed. Please use the Repeat option at the target level instead.';
end;

procedure Tf_EditTargets.ShowPlan;
var s:TStep;
    p:T_Plan;
    t: TTarget;
    i,n: integer;
begin
  ClearStepList;
  PageControlPlan.ActivePageIndex:=pageobject;
  t:=TTarget(TargetList.Objects[colseq,TargetList.Row]);
  if t<>nil then begin
    p:=T_Plan(t.plan);
    if (p=nil)or(p.Count=0) then begin
      LoadTemplate;
    end
    else begin
      n:=p.Count;
      StepList.RowCount:=n+1;
      for i:=1 to n do begin
        s:=TStep.Create;
        s.Assign(p.Steps[i-1]);
        StepList.Cells[0,i]:=IntToStr(i);
        StepList.Cells[1,i]:=s.description_str;
        StepList.Objects[0,i]:=s;
        StepListSelection(nil,0,i);
      end;
    end;
    StepList.Row:=1;
    StepListSelection(StepList,0,1);
    SetTemplateButton;
  end;
end;

procedure Tf_EditTargets.LoadTemplate;
var pfile: TCCDconfig;
    fn,buf: string;
    i,n:integer;
    p: TStep;
  procedure NewTemplate;
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
LabelMsg.Caption:='';
if (trim(PlanName.Caption)<>'')and(pos('*',PlanName.Caption)<=0) then begin
  PageControlPlan.ActivePageIndex:=pageobject;
  fn:=slash(SequenceDir)+PlanName.Caption+'.plan';
  if FileExistsUTF8(fn) then begin
    pfile:=TCCDconfig.Create(self);
    pfile.Filename:=fn;
    n:=pfile.GetValue('/StepNum',0);
    if n=0 then begin
      NewTemplate;
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
    NewTemplate;
  end;
  StepsModified:=true;
  SavePlanModified(true);
  end
  else begin
   ClearStepList;
   PageControlPlan.ActivePageIndex:=pageobject;
  end;
end;

procedure Tf_EditTargets.ClearTargetList;
var i: integer;
begin
for i:=1 to TargetList.RowCount-1 do begin
  if TargetList.Objects[0,i]<>nil then TargetList.Objects[0,i].Free;
  TargetList.Objects[0,i]:=nil;
end;
TargetList.RowCount:=1;
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

procedure Tf_EditTargets.FrameTypeChange(n: integer; newtype: integer);
begin
  case newtype of
    ord(Light) : begin

        end;
    ord(Bias) : begin
           StepList.Cells[pcolexp,n]:='0';
           StepList.Cells[pcolfilter,n]:=Filter0;
           StepList.Cells[pcolafstart,n]:='';
           StepList.Cells[pcolafevery,n]:='0';
           StepList.Cells[pcoldither,n]:='0';
        end;
    ord(Dark) : begin
           StepList.Cells[pcolfilter,n]:=Filter0;
           StepList.Cells[pcolafstart,n]:='';
           StepList.Cells[pcolafevery,n]:='0';
           StepList.Cells[pcoldither,n]:='0';
        end;
    ord(Flat) : begin
           StepList.Cells[pcolafstart,n]:='';
           StepList.Cells[pcolafevery,n]:='0';
           StepList.Cells[pcoldither,n]:='0';
        end;
     else begin

        end;

  end;
end;


procedure Tf_EditTargets.StepListSelection(Sender: TObject; aCol, aRow: Integer);
var p: Tstep;
begin
if aRow<StepList.RowCount then begin
  p:=TStep(StepList.Objects[pcolseq,aRow]);
  SetStep(aRow,p);
end;
end;

procedure Tf_EditTargets.SetStep(n: integer; p: TStep);
begin
  LockStep:=true;
  PageControlStepType.ActivePageIndex:=p.steptype;
  StepList.Cells[pcoldesc,n]:=p.description_str;
  if p.steptype=0 then begin
    StepList.Cells[pcoltype,n]:=p.frtype_str;
    StepList.Cells[pcolexp,n]:=formatfloat(f3,p.exposure);
    StepList.Cells[pcolstack,n]:=inttostr(p.stackcount);
    StepList.Cells[pcolbin,n]:=p.binning_str;
    if hasGainISO then begin
      if (p.gain<StepList.Columns[pcolgain-1].PickList.Count) then
        StepList.Cells[pcolgain,n]:=StepList.Columns[pcolgain-1].PickList[p.gain]
      else
        LabelMsg.Caption:='Gain setting not compatible!';
    end
    else
      StepList.Cells[pcolgain,n]:=IntToStr(p.gain);
    StepList.Cells[pcoloffset,n]:=IntToStr(p.offset);
    StepList.Cells[pcolfstop,n]:=p.fstop;
    if p.filter<StepList.Columns[pcolfilter-1].PickList.count then
      StepList.Cells[pcolfilter,n]:=StepList.Columns[pcolfilter-1].PickList[p.filter]
    else
      LabelMsg.Caption:='Filter setting not compatible!';
    StepList.Cells[pcolcount,n]:=IntToStr(p.count);
    if p.dither then
      StepList.Cells[pcoldither,n]:=IntToStr(p.dithercount)
    else
      StepList.Cells[pcoldither,n]:='';
    StepList.Cells[pcolafstart,n]:=BoolToStr(p.autofocusstart,'1','0');
    if p.autofocus then
      StepList.Cells[pcolafevery,n]:=IntToStr(p.autofocuscount)
    else
      StepList.Cells[pcolafevery,n]:='';
  end
  else if p.steptype=1 then begin
    StepList.Cells[pcoltype,n]:=rsScript;
    StepList.Cells[pcolexp,n]:='';
    StepList.Cells[pcolstack,n]:='';
    StepList.Cells[pcolbin,n]:='';
    StepList.Cells[pcolfilter,n]:='';
    StepList.Cells[pcolcount,n]:='';
    StepList.Cells[pcolafstart,n]:='';
    StepList.Cells[pcolafevery,n]:='';
    StepList.Cells[pcoldither,n]:='';
    StepList.Cells[pcolgain,n]:='';
    StepList.Cells[pcoloffset,n]:='';
    StepList.Cells[pcolfstop,n]:='';
    SetScriptList1(n,p.scriptpath,p.scriptname,p.scriptargs);
  end
  else if p.steptype=2 then begin
    StepList.Cells[pcoltype,n]:=rsSwitch;
    StepList.Cells[pcolexp,n]:='';
    StepList.Cells[pcolstack,n]:='';
    StepList.Cells[pcolbin,n]:='';
    StepList.Cells[pcolfilter,n]:='';
    StepList.Cells[pcolcount,n]:='';
    StepList.Cells[pcolafstart,n]:='';
    StepList.Cells[pcolafevery,n]:='';
    StepList.Cells[pcoldither,n]:='';
    StepList.Cells[pcolgain,n]:='';
    StepList.Cells[pcoloffset,n]:='';
    StepList.Cells[pcolfstop,n]:='';
    SetSwitch1(n,p.switchnickname,p.switchname,p.switchvalue);
  end;
  LockStep:=false;
end;

procedure Tf_EditTargets.StepChange(Sender: TObject);
var n,j,i:integer;
    oldid: LongWord;
    p: TStep;
    x: double;
    str,buf,snick,sname: string;
    ok:boolean;
begin
  if LockStep then exit;
  // is table empty?
  if StepList.RowCount<=1 then exit;
  n:=StepList.Row;
  // is title row?
  if n < 1 then exit;
  p:=TStep(StepList.Objects[0,n]);
  if p=nil then exit;
  PageControlStepType.ActivePageIndex:=p.steptype;
  if p.steptype=0 then begin
    oldid:=p.id;
    StepsModified:=StepsModified or (p.description<>StepList.Cells[pcoldesc,n]);
    p.description:=StepList.Cells[pcoldesc,n];
    str:=StepList.Cells[pcoltype,n];
    j:=StepList.Columns[pcoltype-1].PickList.IndexOf(str);
    if j<0 then j:=0;
    StepsModified:=StepsModified or (p.frtype<>j);
    if p.frtype<>j then FrameTypeChange(n,j);
    p.frtype:=j;
    x:=StrToFloatDef(stringReplace(StepList.Cells[pcolexp,n],',','.',[]),p.exposure);
    StepsModified:=StepsModified or (p.exposure<>x);
    p.exposure:=x;
    j:=StrToIntDef(StepList.Cells[pcolstack,n],p.stackcount);
    StepsModified:=StepsModified or (p.stackcount<>j);
    p.stackcount:=j;
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
      str:=StepList.Cells[pcolgain,n];
      j:=StepList.Columns[pcolgain-1].PickList.IndexOf(str);
      StepsModified:=StepsModified or (p.gain<>j);
      if j>=0 then p.gain:=j;
    end
    else if hasGain then begin
      j:=StrToIntDef(StepList.Cells[pcolgain,n],p.gain);
      if j>GainMax then begin j:=GainMax; StepList.Cells[pcolgain,n]:=inttostr(j); end;
      if j<GainMin then begin j:=GainMin; StepList.Cells[pcolgain,n]:=inttostr(j); end;
      StepsModified:=StepsModified or (p.gain<>j);
      p.gain:=j;
    end;
    j:=StrToIntDef(StepList.Cells[pcoloffset,n],p.offset);
    StepsModified:=StepsModified or (p.offset<>j);
    p.offset:=j;
    str:=StepList.Cells[pcolfstop,n];
    StepsModified:=StepsModified or (p.fstop<>str);
    p.fstop:=str;
    str:=StepList.Cells[pcolfilter,n];
    j:=StepList.Columns[pcolfilter-1].PickList.IndexOf(str);
    if j<0 then j:=0;
    StepsModified:=StepsModified or (p.filter<>j);
    p.filter:=j;
    j:=StrToIntDef(StepList.Cells[pcolcount,n],p.count);
    StepsModified:=StepsModified or (p.count<>j);
    p.count:=j;
    j:=StrToIntDef(StepList.Cells[pcoldither,n],-1);
    ok:=j>0;
    StepsModified:=StepsModified or (p.dither<>ok);
    p.dither:=(p.frtype=ord(LIGHT)) and ok;
    StepsModified:=StepsModified or (ok and (p.dithercount<>j));
    if ok then
       p.dithercount:=j
    else
      p.dithercount:=0;
    ok:=StepList.Cells[pcolafstart,n]='1';
    StepsModified:=StepsModified or (p.autofocusstart<>ok);
    p.autofocusstart:=(p.frtype=ord(LIGHT)) and ok;
    j:=StrToIntDef(StepList.Cells[pcolafevery,n],-1);
    ok:=j>0;
    StepsModified:=StepsModified or (p.autofocus<>ok);
    p.autofocus:=(p.frtype=ord(LIGHT)) and ok;
    StepsModified:=StepsModified or (ok and (p.autofocuscount<>j));
    if ok then
       p.autofocuscount:=j
    else
       p.autofocuscount:=0;
    // reset donecount if the id change
    if p.id<>oldid then
      p.donecount:=0;
    StepList.Cells[pcoldesc,n]:=p.description;
  end
  else if p.steptype=1 then begin
    i:=ScriptList1.ItemIndex;
    if i>=0 then
      sname:=ScriptList1.Items[i]
    else
      sname:='';
    buf:=trim(ScriptParam1.Text);
    p.description:=sname+' '+buf;
    StepList.Cells[pcoldesc,n]:=p.description;
    StepList.Cells[pcoltype,n]:=rsScript;
    StepsModified:=StepsModified or (p.scriptname<>sname);
    p.scriptname:=sname;
    StepsModified:=StepsModified or (p.scriptargs<>buf);
    p.scriptargs:=buf;
    p.scriptpath:=slash(ConfigDir);
  end
  else if p.steptype=2 then begin
    snick:=Switches1.Text;
    sname:=Switchlist1.Text;
    buf:=SwitchValue1.Text;
    p.description:=snick+', '+sname+'='+buf;
    StepList.Cells[pcoldesc,n]:=p.description;
    StepList.Cells[pcoltype,n]:=rsSwitch;
    StepsModified:=StepsModified or (p.switchnickname<>snick);
    p.switchnickname:=snick;
    StepsModified:=StepsModified or (p.switchname<>sname);
    p.switchname:=sname;
    StepsModified:=StepsModified or (p.switchvalue<>buf);
    p.switchvalue:=buf;
  end;
  SetStep(n,p);
  SavePlanModified(false);
end;

procedure Tf_EditTargets.StepListValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String);
begin
  if (aRow>0)and(StepList.Cells[pcoltype,aRow]<>trim(FrameName[ord(LIGHT)])) then begin
    StepList.Cells[pcolafstart,aRow]:='';
    StepList.Cells[pcolafevery,aRow]:='0';
    StepList.Cells[pcoldither,aRow]:='0';
  end;
end;

procedure Tf_EditTargets.StepListCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  if (aRow>0)and(StepList.Cells[pcoltype,aRow]<>trim(FrameName[ord(LIGHT)])) then begin
    StepList.Cells[pcolafstart,aRow]:='';
    StepList.Cells[pcolafevery,aRow]:='0';
    StepList.Cells[pcoldither,aRow]:='0';
  end;
  StepChange(sender);
end;

procedure Tf_EditTargets.StepListColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
begin
  ResetSteps;
  StepsModified:=true;
  SavePlanModified(false);
end;

procedure Tf_EditTargets.StepListEditingDone(Sender: TObject);
begin
  StepChange(Sender);
end;

procedure Tf_EditTargets.StepListGetCellHint(Sender: TObject; ACol, ARow: Integer; var HintText: String);
begin
  StepList.EditorMode := false; // mouse move to new cell, auto-close editor
  case ACol of
    pcolseq     : HintText:=rsDragDropToCh;
    pcoldesc    : HintText:=rsADescription;
    pcoltype    : HintText:=rsTheTypeOfFra;
    pcolexp     : HintText:=rsExposureTime;
    pcolstack   : HintText:=rsStackingCoun ;
    pcolbin     : HintText:=rsPixelBinning;
    pcolfilter  : HintText:=rsFilterName;
    pcolcount   : HintText:=rsTheNumberOfI4;
    pcolafstart : HintText:=rsAutofocusAtT;
    pcolafevery : HintText:=rsRedoAutofocu;
    pcoldither  : HintText:=rsDitherAfterT;
    pcolgain    : HintText:=rsCameraGain;
    pcoloffset  : HintText:=rsCameraOffset;
    pcolfstop   : HintText:=rsFStop;
    else HintText:='';
  end;
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
  else if (aCol=pcolafstart) then
     Editor:=StepList.EditorByStyle(cbsCheckboxColumn) // autofocus at start selection
  else if (aCol=pcolgain) then begin
    if hasGainISO then
      Editor:=StepList.EditorByStyle(cbsPickList) // ISO list
    else
      Editor:=StepList.EditorByStyle(cbsAuto)     // Gain
  end
  else if (aCol=pcolfstop) then
     Editor:=StepList.EditorByStyle(cbsPickList) // f-stop
  else
     Editor:=StepList.EditorByStyle(cbsAuto);
end;

procedure Tf_EditTargets.BtnAddStepClick(Sender: TObject);
var p: TPoint;
begin
  p:=Point(Tcontrol(Sender).Left,Tcontrol(Sender).Top+Tcontrol(Sender).Height);
  p:=Tcontrol(Sender).Parent.ClientToScreen(p);
  PopupAddStep.PopUp(p.x,p.y);
end;

procedure Tf_EditTargets.MenuAddCaptureStepClick(Sender: TObject);
var txt:string;
    i,n: integer;
    p,pp: TStep;
begin
  txt:=FormEntry(self, rsStepDescript, 'Step'+inttostr(StepList.RowCount));
  p:=TStep.Create;
  n:=StepList.Row;
  if n >= 1 then begin
    pp:=TStep(StepList.Objects[0,n]);
    if pp.steptype=0 then p.Assign(pp);
  end;
  p.steptype:=0;
  p.description:=txt;
  StepList.RowCount:=StepList.RowCount+1;
  i:=StepList.RowCount-1;
  StepList.Cells[pcolseq,i]:=IntToStr(i);
  StepList.Cells[pcoldesc,i]:=txt;
  StepList.Objects[pcolseq,i]:=p;
  StepList.Row:=i;
  StepsModified:=true;
  SetStep(i,p);
  SavePlanModified(false);
end;

procedure Tf_EditTargets.MenuAddScriptStepClick(Sender: TObject);
var txt:string;
    i: integer;
    p: TStep;
begin
  p:=TStep.Create;
  txt:=rsScript;
  p.description:=txt;
  p.steptype:=1;
  StepList.RowCount:=StepList.RowCount+1;
  i:=StepList.RowCount-1;
  StepList.Cells[pcolseq,i]:=IntToStr(i);
  StepList.Cells[pcoltype,i]:=txt;
  StepList.Objects[pcolseq,i]:=p;
  StepList.Row:=i;
  StepsModified:=true;
  SetStep(i,p);
  SavePlanModified(false);
end;

procedure Tf_EditTargets.MenuAddSwitchStepClick(Sender: TObject);
var txt:string;
    i: integer;
    p: TStep;
begin
  p:=TStep.Create;
  txt:=rsSwitch;
  p.description:=txt;
  p.steptype:=2;
  StepList.RowCount:=StepList.RowCount+1;
  i:=StepList.RowCount-1;
  StepList.Cells[pcolseq,i]:=IntToStr(i);
  StepList.Cells[pcoltype,i]:=txt;
  StepList.Objects[pcolseq,i]:=p;
  StepList.Row:=i;
  StepsModified:=true;
  SetStep(i,p);
  SavePlanModified(false);
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
     SavePlanModified(false);
  end;
end;

procedure Tf_EditTargets.ResetSteps;
var i: integer;
begin
  for i:=1 to StepList.RowCount-1 do begin
    StepList.Cells[0,i]:=IntToStr(i);
  end;
end;

procedure Tf_EditTargets.BtnSaveTemplateAsClick(Sender: TObject);
var buf: string;
begin
  SaveDialog1.InitialDir:=SequenceDir;
  SaveDialog1.DefaultExt:='.plan';
  SaveDialog1.filter:='Plan|*.plan';
  SaveDialog1.FileName:=StringReplace(PlanName.Caption,'*','',[])+'.plan';
  if SaveDialog1.Execute then begin
     buf:=ExtractFileNameOnly(SaveDialog1.FileName);
     if (trim(buf)<>'')and(trim(buf)<>'.plan') then begin
       PlanName.Caption:=buf;
       SaveTemplate;
     end
     else
       ShowMessage('No template name selected!');
  end;
end;

procedure Tf_EditTargets.BtnSaveTemplateClick(Sender: TObject);
begin
  if trim(PlanName.Caption)='' then
    BtnSaveTemplateAsClick(Sender)
  else
    SaveTemplate;
end;

procedure Tf_EditTargets.SaveTemplate;
var pfile: TCCDconfig;
    fn,str: string;
    i,n,k: integer;
    p: TStep;
begin
try
  PlanName.Caption:=StringReplace(PlanName.Caption,'*','',[]);
  if (trim(PlanName.Caption)='')or(trim(PlanName.Caption)='.plan') then begin
    ShowMessage('No template name selected!');
    exit;
  end;
  fn:=slash(SequenceDir)+PlanName.Caption+'.plan';
  pfile:=TCCDconfig.Create(self);
  pfile.Filename:=fn;
  pfile.Clear;
  n:=StepList.RowCount-1;
  pfile.SetValue('/PlanName',PlanName.Caption);
  pfile.SetValue('/StepNum',n);
  for i:=1 to n do begin
    p:=TStep(StepList.Objects[0,i]);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Type',p.steptype);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Description',p.description);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/FrameType',p.frtype_str);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Exposure',p.exposure);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/StackCount',p.stackcount);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Binning',IntToStr(p.binx)+'x'+IntToStr(p.biny));
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Gain',p.gain);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Offset',p.offset);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Fstop',p.fstop);
    if StepList.Columns[pcolfilter-1].PickList.Count>0 then begin // do not erase the filters if the filter wheel is not connected
      k:=p.filter;
      if (k<0)or(k>StepList.Columns[pcolfilter-1].PickList.Count-1) then str:=''
         else str:=StepList.Columns[pcolfilter-1].PickList[k];
    end else begin
      if i<SaveFilterNum then str:=originalFilter[i]
      else str:='';
    end;
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Filter',str);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Count',p.count);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Dither',p.dither);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/DitherCount',p.dithercount);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/AutofocusStart',p.autofocusstart);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/Autofocus',p.autofocus);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/AutofocusCount',p.autofocuscount);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/ScriptName',p.scriptname);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/ScriptPath',p.scriptpath);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/ScriptArgs',p.scriptargs);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/SwitchNickname',p.switchnickname);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/SwitchName',p.switchname);
    pfile.SetValue('/Steps/Step'+inttostr(i)+'/SwitchValue',p.switchvalue);
  end;
  pfile.Flush;
  pfile.Free;
  LoadPlanList;
  SetPlanList(TargetList.Row,PlanName.Caption);
  TargetChange(nil);
  StepsModified:=false;
  MarkModifiedTemplate;
except
  on E: Exception do ShowMessage('Error saving template: '+ E.Message);
end;
end;

procedure Tf_EditTargets.SetTemplateButton;
begin
  MenuSaveTemplate.Visible:=trim(PlanName.Caption)<>'';
  BtnDeleteTemplate.Visible:=MenuSaveTemplate.Visible;
  MenuApplyTemplate.Visible:=MenuSaveTemplate.Visible;
end;

procedure Tf_EditTargets.MarkModifiedTemplate;
var p:T_Plan;
    t: TTarget;
    i: integer;
begin
  for i:=1 to TargetList.RowCount-1 do begin
    t:=TTarget(TargetList.Objects[colseq,i]);
    p:=T_Plan(t.plan);
    if TemplateModified(p) then begin
      if (pos('*',t.planname)=0) then begin
        t.planname:=t.planname+'*';
        TargetList.Cells[colplan,i]:=t.PlanName;
      end;
    end
    else begin
      if (pos('*',t.planname)>0) then begin
        t.planname:=StringReplace(t.planname,'*','',[]);
        TargetList.Cells[colplan,i]:=t.PlanName;
      end;
    end;
  end;
end;

procedure Tf_EditTargets.GetTargets(var value:T_Targets; out fn,defaultname: string);
var i,n:integer;
    t:TTarget;
begin
  n:=TargetList.RowCount;
  value.Clear;
  fn:=Filename;
  defaultname:=FormatDateTime('mmdd',now);
  for i:=1 to n-1 do begin
    if (TargetList.Cells[1,i]<>ScriptTxt) and (TargetList.Cells[1,i]<>SkyFlatTxt) then
       defaultname:=TargetList.Cells[1,i];
    t:=TTarget.Create;
    t.Assign(TTarget(TargetList.Objects[0,i]));
    value.Add(t);
  end;
  value.IgnoreRestart    := not CheckBoxRestartStatus.Checked;
  value.ResetRepeat      := CheckBoxResetRepeat.Checked;
  value.TargetsRepeat    := TargetsRepeat;
  value.TargetsRepeatCount:=TargetsRepeatCount;
  value.SeqStart         := SeqStart.Checked;
  value.SeqStop          := SeqStop.Checked;
  value.SeqStartTwilight := SeqStartTwilight.Checked;
  value.SeqStopTwilight  := SeqStopTwilight.Checked;
  value.SeqStartAt       := StrToTimeDef(SeqStartAt.Text,value.SeqStartAt);
  value.SeqStopAt        := StrToTimeDef(SeqStopAt.Text,value.SeqStopAt);
  value.AtStartCool      := StartOpt.Checked[ccCool];
  value.AtStartUnpark    := StartOpt.Checked[ccUnpark];
  value.AtStartRunScript := StartOpt.Checked[ccScript];
  value.AtStartScript    := StartScript;
  value.AtEndStopTracking := TermOpt.Checked[cbStopTracking];
  value.AtEndPark         := TermOpt.Checked[cbParkScope];
  value.AtEndCloseDome    := TermOpt.Checked[cbParkDome];
  value.AtEndWarmCamera   := TermOpt.Checked[cbWarm];
  value.AtEndRunScript    := TermOpt.Checked[cbScript];
  value.OnErrorRunScript  := TermOpt.Checked[cbUnattended];
  value.AtEndScript       := EndScript;
  value.OnErrorScript     := UnattendedScript;
end;

procedure Tf_EditTargets.SetTargets(value:T_Targets);
var i:integer;
    t:TTarget;
begin
  Filename:=value.SequenceFile.Filename;
  ClearTargetList;
  if (value.Count>0) then begin
     // Edit
     TargetName.Caption:=value.TargetName;
     CheckBoxRestartStatus.Checked:=not value.IgnoreRestart;
     CheckBoxResetRepeat.Checked:=value.ResetRepeat;
     TargetsRepeat:=value.TargetsRepeat;
     TargetsRepeatCount:=value.TargetsRepeatCount;
     TargetList.RowCount:=value.Count+1;
     SeqStart.Checked:=value.SeqStart;
     SeqStop.Checked:=value.SeqStop;
     SeqStartTwilight.Checked:=value.SeqStartTwilight;
     SeqStopTwilight.Checked:=value.SeqStopTwilight;
     SeqStartAt.Text:=TimeToStr(value.SeqStartAt);
     SeqStopAt.Text:=TimeToStr(value.SeqStopAt);
     StartOpt.Checked[ccNone]:=not(value.AtStartCool or value.AtStartUnpark);
     StartOpt.Checked[ccCool]:=value.AtStartCool;
     StartOpt.Checked[ccUnpark]:=value.AtStartUnpark;
     StartOpt.Checked[ccScript]:=value.AtStartRunScript;
     StartScript:=value.AtStartScript;
     TermOpt.Checked[cbNone]:=not(value.AtEndStopTracking or value.AtEndPark or value.AtEndCloseDome or value.AtEndWarmCamera or value.AtEndRunScript);
     TermOpt.Checked[cbStopTracking]:=value.AtEndStopTracking;
     TermOpt.Checked[cbParkScope]:=value.AtEndPark;
     TermOpt.Checked[cbParkDome]:=value.AtEndCloseDome;
     TermOpt.Checked[cbWarm]:=value.AtEndWarmCamera;
     TermOpt.Checked[cbScript]:=value.AtEndRunScript;
     TermOpt.Checked[cbUnattended]:=value.OnErrorRunScript;
     EndScript:=value.AtEndScript;
     UnattendedScript:=value.OnErrorScript;
     for i:=1 to value.Count do begin
       t:=TTarget.Create;
       t.Assign(value.Targets[i-1]);
       SetTarget(i,t);
       TargetList.Objects[colseq,i]:=t;
     end;
   end else begin
     // New
     value.Clear;
     TargetName.Caption:='New targets';
     CheckBoxRestartStatus.Checked:=true;
     CheckBoxResetRepeat.Checked:=true;
     TargetsRepeat:=1;
     TargetsRepeatCount:=0;
     SeqStart.Checked:=false;
     SeqStop.Checked:=false;
     SeqStartTwilight.Checked:=false;
     SeqStopTwilight.Checked:=false;
     SeqStartAt.Text:='00:00:00';
     SeqStopAt.Text:='00:00:00';
     StartOpt.Checked[ccNone]:=false;
     StartOpt.Checked[ccCool]:=true;
     StartOpt.Checked[ccUnpark]:=true;
     StartOpt.Checked[ccScript]:=false;
     StartScript:='';
     TermOpt.Checked[cbNone]:=false;
     TermOpt.Checked[cbStopTracking]:=true;
     TermOpt.Checked[cbParkScope]:=false;
     TermOpt.Checked[cbParkDome]:=false;
     TermOpt.Checked[cbWarm]:=false;
     TermOpt.Checked[cbScript]:=false;
     TermOpt.Checked[cbUnattended]:=false;
     EndScript:='';
     UnattendedScript:='';
     TargetList.RowCount:=1;
   end;
end;

end.

