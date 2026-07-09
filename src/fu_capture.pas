unit fu_capture;

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

uses u_global, Graphics, UScaleDPI, u_hints, u_translation, cu_mount, u_utils, math, Dialogs,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, SpinEx;

type
  TCaptureRunType = (CAPTURE, SEQUENCE, NONE);

  { Tf_capture }

  Tf_capture = class(TFrame)
    Binning: TComboBox;
    BtnStart: TButton;
    cbEndScriptImage: TCheckBox;
    cbOverwrite: TCheckBox;
    cbStartScriptImage: TCheckBox;
    cbTerminationScript: TCheckBox;
    cbStartupScript: TCheckBox;
    CheckBoxFocusHFD: TCheckBox;
    CheckBoxFocusTemp: TCheckBox;
    CheckBoxDither: TCheckBox;
    CheckBoxFocus: TCheckBox;
    cbScript: TComboBox;
    edParams: TEdit;
    LabelExpInfo: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelTime: TLabel;
    PanelRunScript: TPanel;
    Panel11: TPanel;
    PanelScriptname: TPanel;
    PanelScript: TPanel;
    PanelScriptParam: TPanel;
    Panel14: TPanel;
    StackNum: TSpinEditEx;
    Fnumber: TComboBox;
    cbFrameType: TComboBox;
    ExpTime: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    led: TShape;
    Panel1: TPanel;
    PanelStack: TPanel;
    PanelFnumber: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Fname: TEdit;
    SeqNum: TSpinEditEx;
    DitherCount: TSpinEditEx;
    FocusCount: TSpinEditEx;
    TimerExp: TTimer;
    Title: TLabel;
    procedure BtnStartClick(Sender: TObject);
    procedure cbScriptChange(Sender: TObject);
    procedure ExpTimeChange(Sender: TObject);
    procedure ExpTimeKeyPress(Sender: TObject; var Key: char);
    procedure CheckLight(Sender: TObject);
    procedure LabelExpInfoClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure SeqNumChange(Sender: TObject);
    procedure StackNumChange(Sender: TObject);
    procedure TimerExpTimer(Sender: TObject);
  private
    { private declarations }
    FMount: T_mount;
    FstartedBy: TCaptureRunType;
    FExposureTime,FCameraExposureRemain: double;
    FSeqCount, FStackCount: integer;
    FDitherNum: integer;
    FFocusNum: integer;
    FFocusNow: boolean;
    Frunning: boolean;
    FResetHFM: boolean;
    FonMsg: TNotifyMsg;
    FonOptions:  TNotifyEvent;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
    FonStopExposure: TNotifyEvent;
    FonStopPreview: TNotifyEvent;
    FonResetStack: TNotifyEvent;
    FonFrameTypeChange: TNotifyEvent;
    FonResetHFM: TNotifyEvent;
    FRunScript: TRunScript;
    procedure SetExposureTime(val: double);
    function GetFrameType:integer;
    procedure SetFrameType(value:integer);
    function GetFrameTypeText:string;
    function GetScript:string;
    function GetScriptParams:string;
    function GetEndScriptImage:boolean;
    function GetStartScriptImage:boolean;
    procedure ComputeTotalTime;
    function GetOverwrite:boolean;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    procedure Stop;
    procedure setCustomFrameType;
    property Mount: T_mount read FMount write FMount;
    property Running: boolean read Frunning write Frunning;
    property Overwrite: boolean read GetOverwrite;
    property SeqCount: Integer read FSeqCount write FSeqCount;
    property StackCount: Integer read FStackCount write FStackCount;
    property ExposureTime: double read FExposureTime write SetExposureTime;
    property CameraExposureRemain: double read FCameraExposureRemain write FCameraExposureRemain;
    property FrameType: integer read GetFrameType write SetFrameType;
    property FrameTypeText: string read GetFrameTypeText;
    property DitherNum: Integer read FDitherNum write FDitherNum;
    property FocusNum: Integer read FFocusNum write FFocusNum;
    property FocusNow: boolean read FFocusNow write FFocusNow;
    property onStopPreview: TNotifyEvent read FonStopPreview write FonStopPreview;
    property onStartExposure: TNotifyEvent read FonStartExposure write FonStartExposure;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onStopExposure: TNotifyEvent read FonStopExposure write FonStopExposure;
    property onResetStack: TNotifyEvent read FonResetStack write FonResetStack;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onOptions: TNotifyEvent read FonOptions write FonOptions;
    property onFrameTypeChange: TNotifyEvent read FonFrameTypeChange write FonFrameTypeChange;
    property onResetHFM: TNotifyEvent read FonResetHFM write FonResetHFM;
    property ResetHFM: boolean read FResetHFM write FResetHFM;
    property Script: string read GetScript;
    property ScriptParams: string read GetScriptParams;
    property StartScriptImage: boolean read GetStartScriptImage;
    property EndScriptImage: boolean read GetEndScriptImage;
    property onRunScript: TRunScript read FRunScript write FRunScript;
end;

implementation

{$R *.lfm}

{ Tf_capture }

constructor Tf_capture.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 Frunning:=false;
 FFocusNow:=false;
 FResetHFM:=true;
 FExposureTime:=-1;
 FCameraExposureRemain:=0;
 FstartedBy:=NONE;
 SetLang;
 led.Canvas.AntialiasingMode:=amOn;
 LabelTime.Caption:='';
 LabelExpInfo.Caption:='';
end;

destructor  Tf_capture.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_capture.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_capture.SetLang;
begin
  Title.Caption:=rsCapture;
  Label1.Caption:=rsExposure;
  Label7.Caption:=rsStackingCoun;
  Label2.Caption:=rsBinning;
  Label6.Caption:=rsFStop;
  Label3.Caption:=rsObject;
  Label4.Caption:=rsCount;
  cbOverwrite.Caption:=rsOverwrite;
  Label5.Caption:=rsType;
  CheckBoxDither.Caption:=rsDitherEvery;
  CheckBoxFocus.Caption:=rsFocusEvery;
  CheckBoxFocusTemp.Caption:=rsAutofocusAft2;
  CheckBoxFocusHFD.Caption:=rsAutofocusHFD2;
  Label8.Caption:=rsScript;
  Label9.Caption:=rsParameter;
  cbStartupScript.Caption:=rsRunScriptAtT2;
  cbStartScriptImage.Caption:=rsRunScriptBef;
  cbEndScriptImage.Caption:=rsRunScriptAft;
  cbTerminationScript.Caption:=rsRunScriptAtT;
  BtnStart.Caption:=rsStart;
  ExpTime.Hint:=rsExposureTime;
  Binning.Hint:=rsCameraBinnin;
  Fname.Hint:=rsTheObjectNam;
  SeqNum.Hint:=rsTheNumberOfI;
  cbFrameType.Hint:=rsTheTypeOfFra;
  DitherCount.Hint:=rsTheNumberOfI2;
  FocusCount.Hint:=rsTheNumberOfI3;
  CheckBoxFocusTemp.Hint:=rsSetTheTemper;
  CheckBoxFocusHFD.Hint:=rsSetTheHFD;
  BtnStart.Hint:=rsStartTheCapt
end;

procedure Tf_capture.BtnStartClick(Sender: TObject);
var
  needobjname: boolean;
  i: integer;
begin
  Frunning:=not Frunning;
  if (Sender=nil) then FstartedBy:=SEQUENCE else FstartedBy:=CAPTURE;
  if Frunning then begin
    if assigned(FonStopPreview) then FonStopPreview(self);
    Frunning:=true;
    CancelAutofocus:=false;
    if FstartedBy=CAPTURE then FSeqCount:=1; // otherwise set by plan
    FDitherNum:=0;
    FFocusNum:=0;

    // Ensure HFD autofocus history and vars are reset for new measurements.
    // If started by a sequence then only reset when the sequence starts, not
    // for each plan/target or step. This allows the monitor to track across
    // parfocal filters or across targets if skies and airmass are similar, but
    // will trigger a refocus if nonparfocal or skies vary. Of course, the user
    // can trigger an autofocus via the plan or manually, which will reset the
    // HFM. If started by the capture button then always reset values as the
    // user could have done any number of manual events that require a reset.
    if (FstartedBy=SEQUENCE) and FResetHFM then begin
      if Assigned(FonResetHFM) then FonResetHFM(self);

      // don't reset again for a sequence unless triggered by an autofocus
      FResetHFM:=false;
    end
    else if (FstartedBy=CAPTURE) then begin
      if Assigned(FonResetHFM) then FonResetHFM(self);
    end;

    if (FstartedBy=CAPTURE) and (trim(Fname.Text)='') then begin
      // check if object name is required for image file name
      needobjname:=false;
      for i:=0 to FileNameCount-1 do
        if (FileNameOpt[i]=fnObj) and FileNameActive[i] then needobjname:=true;
      for i:=0 to SubDirCount-1 do
        if (SubDirOpt[i]=sdObj) and SubDirActive[i] then needobjname:=true;
      if needobjname and
         (TFrameType(cbFrameType.ItemIndex)<>BIAS) and
         (TFrameType(cbFrameType.ItemIndex)<>DARK) and
         (TFrameType(cbFrameType.ItemIndex)<>FLAT)
       then begin
         if MessageDlg(rsTheObjectNam2, mtConfirmation, mbYesNo, 0)<>mrYes then begin
           Frunning:=false;
         end;
       end;
    end;

    if Frunning then begin
      DomeFlatExpAdjust:=0;
      doFlatAutoExposure:=(TFrameType(cbFrameType.ItemIndex)=FLAT) and FlatAutoExposure;
      if (TFrameType(cbFrameType.ItemIndex)=FLAT)and(FlatType=ftDome) then begin
         if DomeFlatSetLight and (DomeFlatSetLightON<>'') then begin
            AdjustFlatLight:=true;
            ExecProcess(DomeFlatSetLightON,nil,false);
         end;
         if FlatAutoExposure then
            AdjustDomeFlat:=true;
         if DomeFlatTelescopeSlew and (FMount<>nil) then
            Mount.SlewToDomeFlatPosition;
      end;
      if doFlatAutoExposure then begin
        if ExposureTime<FlatMinExp then ExposureTime:=FlatMinExp;
        if ExposureTime>FlatMaxExp then ExposureTime:=FlatMaxExp;
      end;
      if Assigned(FonMsg) then FonMsg(rsStartCapture,2);
      if (cbStartupScript.Checked)and(cbScript.text>'')and(FileExists(slash(ConfigDir)+cbScript.text+'.script'))and assigned(FRunScript) then begin
        FRunScript(cbScript.text,ConfigDir,'-1 '+ScriptParams)
      end;
      EarlyNextExposure:= ConfigExpEarlyStart and (ExposureTime>=1) and (not Overwrite)
          and (not cbStartScriptImage.Checked) and (not cbEndScriptImage.Checked)
          and ((TFrameType(cbFrameType.ItemIndex)=LIGHT)or(TFrameType(cbFrameType.ItemIndex)=DARK)or(cbFrameType.ItemIndex>ord(high(TFrameType))));
      if PanelStack.Visible and (StackNum.Value>1) and Assigned(FonResetStack) then FonResetStack(self);
      if Assigned(FonStartExposure) then FonStartExposure(self);
      if (not Frunning) and Assigned(FonMsg) then FonMsg(rsCannotStartC,0);
    end;
  end else begin
    if GetKeyShiftState=[ssShift] then begin
      if Assigned(FonStopExposure) then FonStopExposure(self);
    end
    else begin
      CancelAutofocus:=true;
      EarlyNextExposure:=false;
      if Assigned(FonAbortExposure) then FonAbortExposure(self);
    end;
  end;
  if Frunning then begin
    led.Brush.Color:=clLime;
    BtnStart.Caption:=rsStop;
    FCameraExposureRemain:=0;
    TimerExp.Enabled:=true;
  end else begin
    EarlyNextExposure:=false;
    led.Brush.Color:=clGray;
    BtnStart.Caption:=rsStart;
    TimerExp.Enabled:=false;
    LabelTime.Caption:='';
    if Assigned(FonMsg) then FonMsg(rsStopCapture,2);
  end;
end;

procedure Tf_capture.cbScriptChange(Sender: TObject);
begin
  PanelScriptParam.Visible:=trim(cbScript.Text)<>'';
  PanelRunScript.Visible:=PanelScriptParam.Visible;
end;

procedure Tf_capture.ExpTimeChange(Sender: TObject);
begin
  FExposureTime:=StrToFloatDef(ExpTime.Text,-1);
  ComputeTotalTime;
end;

procedure Tf_capture.ExpTimeKeyPress(Sender: TObject; var Key: char);
begin
  // Only accept positive decimal value
  // inspired from TCustomFloatSpinEdit.KeyPress
  inherited KeyPress(Key);
  if (Key in ['.',',']) then Key := DefaultFormatSettings.Decimalseparator;
  if not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator,'+',#8,#9,^C,^X,^V,^Z]) then Key := #0;
end;

procedure Tf_capture.SetExposureTime(val: double);
begin
  FExposureTime:=val;
  ExpTime.Text:=FormatFloat(f9v,val);
end;

function Tf_capture.GetOverwrite:boolean;
begin
  result:= cbOverwrite.Visible and cbOverwrite.Checked;
end;

procedure Tf_capture.CheckLight(Sender: TObject);
begin
  if cbFrameType.ItemIndex<>0 then begin
     CheckBoxDither.Checked:=false;
     CheckBoxFocus.Checked:=false;
     CheckBoxFocusHFD.Checked:=false;
     CheckBoxFocusTemp.Checked:=false;
  end;
  ExpTime.Enabled:=(cbFrameType.ItemIndex<>1);
  if ExpTime.Enabled and (ExpTime.Text='0') then ExpTime.ItemIndex:=0;
  if Sender<>nil then begin
    // run custom frame type script immediately when changed interactively
    if assigned(FonFrameTypeChange) then FonFrameTypeChange(self);
  end;
end;

procedure Tf_capture.LabelExpInfoClick(Sender: TObject);
begin
  if Assigned(FonOptions) then FonOptions(self);
end;

procedure Tf_capture.Panel1Resize(Sender: TObject);
begin
  // resize when stacking is enabled or disabled
  ComputeTotalTime;
end;

procedure Tf_capture.SeqNumChange(Sender: TObject);
begin
  ComputeTotalTime;
end;

procedure Tf_capture.StackNumChange(Sender: TObject);
begin
  ComputeTotalTime;
end;

procedure Tf_capture.Stop;
var r:string;
begin
  Frunning:=false;
  if (led.Brush.Color<>clGray) then begin
    if ExpectedStop then  begin
      if (FstartedBy=CAPTURE) and EmailEndCapture then begin
        r:=email(Format(rsCaptureSFini, ['']),Format(rsCaptureSFini, [FName.Text]));
        if r='' then r:=rsEmailSentSuc;
        if Assigned(FonMsg) then FonMsg(r,9);
      end;
      if (cbTerminationScript.Checked)and(cbScript.text>'')and(FileExists(slash(ConfigDir)+cbScript.text+'.script'))and assigned(FRunScript) then begin
        FRunScript(cbScript.text,ConfigDir,'2 '+ScriptParams)
      end;
    end
    else begin
       if (FstartedBy=CAPTURE) and EmailAbortCapture then begin
         r:=email(rsCaptureStopp2,rsCaptureStopp2 +' '+FName.Text);
         if r='' then r:=rsEmailSentSuc;
         if Assigned(FonMsg) then FonMsg(r,9);
       end;
       if (cbTerminationScript.Checked)and(cbScript.text>'')and(FileExists(slash(ConfigDir)+cbScript.text+'.script'))and assigned(FRunScript) then begin
         FRunScript(cbScript.text,ConfigDir,'3 '+ScriptParams);
       end;
    end;
  end;
  cbOverwrite.Checked:=false;
  EarlyNextExposure:=false;
  CameraProcessingImage:=false;
  led.Brush.Color:=clGray;
  BtnStart.Caption:=rsStart;
  TimerExp.Enabled:=false;
  LabelTime.Caption:='';
  if (TFrameType(cbFrameType.ItemIndex)=FLAT)and(FlatType=ftDome) then begin
     if DomeFlatSetLight and AdjustFlatLight and (DomeFlatSetLightOFF<>'') then begin
        AdjustFlatLight:=false;
        ExecProcess(DomeFlatSetLightOFF,nil,false);
     end;
  end;
  ComputeTotalTime;
end;

procedure Tf_capture.setCustomFrameType;
var i,n: integer;
begin
  n:=cbFrameType.ItemIndex;
  cbFrameType.Clear;
  for i:=0 to ord(high(TFrameType)) do
    cbFrameType.Items.Add(trim(FrameName[i]));
  for i:=0 to NumCustomFrameType-1 do
    cbFrameType.Items.Add(trim(CustomFrameType[i].Name));
  n:=min(cbFrameType.Items.Count-1,max(n,0));
  cbFrameType.ItemIndex:=n;
end;

function Tf_capture.GetFrameType:integer;
begin
  result:=cbFrameType.ItemIndex;
end;

procedure Tf_capture.SetFrameType(value:integer);
begin
  if (value>=0)and(value<cbFrameType.Items.Count) then
    cbFrameType.ItemIndex:=value
  else
    cbFrameType.ItemIndex:=0;
end;

function Tf_capture.GetFrameTypeText:string;
begin
  result:=cbFrameType.Text;
end;

function Tf_capture.GetScript:string;
begin
  result:=cbScript.text;
end;

function Tf_capture.GetScriptParams:string;
begin
  result:=trim(edParams.text);
end;

function Tf_capture.GetStartScriptImage:boolean;
begin
  result:=cbStartScriptImage.Checked;
end;

function Tf_capture.GetEndScriptImage:boolean;
begin
  result:=cbEndScriptImage.Checked;
end;

procedure Tf_capture.TimerExpTimer(Sender: TObject);
var t: double;
begin
  if FCameraExposureRemain>0 then begin
    if PanelStack.Visible and (StackNum.Value>1) then begin
      FStackCount:=FStackCount mod StackNum.Value;
      if FStackCount=0 then begin
        t:=(SeqNum.Value-FSeqCount+1)*FExposureTime*StackNum.Value;
        LabelTime.Caption:=TimToStr(t/3600);
      end;
    end
    else begin
      t:=FCameraExposureRemain+(SeqNum.Value-FSeqCount)*FExposureTime;
      LabelTime.Caption:=TimToStr(t/3600);
    end;
  end;
end;

procedure Tf_capture.ComputeTotalTime;
var t: double;
begin
  if not Frunning then begin
    if PanelStack.Visible and (StackNum.Value>1) then begin
      t:=SeqNum.Value*StackNum.Value*(FExposureTime+1);
      LabelTime.Caption:=TimToStr(t/3600);
    end
    else begin
      t:=SeqNum.Value*(FExposureTime+1);
      LabelTime.Caption:=TimToStr(t/3600);
    end;
  end;
end;

end.

