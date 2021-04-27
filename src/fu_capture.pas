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

uses u_global, Graphics, UScaleDPI, u_translation, u_hints, cu_mount, u_utils,
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Spin;

type

  { Tf_capture }

  Tf_capture = class(TFrame)
    Binning: TComboBox;
    BtnStart: TButton;
    CheckBoxDither: TCheckBox;
    CheckBoxFocus: TCheckBox;
    StackNum: TSpinEdit;
    Fnumber: TComboBox;
    ISObox: TComboBox;
    FrameType: TComboBox;
    ExpTime: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelGain: TLabel;
    LabelOffset: TLabel;
    led: TShape;
    OffsetEdit: TSpinEdit;
    Panel1: TPanel;
    PanelStack: TPanel;
    PanelFnumber: TPanel;
    PanelGain: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Fname: TEdit;
    GainEdit: TSpinEdit;
    PanelOffset: TPanel;
    SeqNum: TSpinEdit;
    DitherCount: TSpinEdit;
    FocusCount: TSpinEdit;
    Title: TLabel;
    procedure BtnStartClick(Sender: TObject);
    procedure ExpTimeChange(Sender: TObject);
    procedure ExpTimeKeyPress(Sender: TObject; var Key: char);
    procedure FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure CheckLight(Sender: TObject);
  private
    { private declarations }
    FMount: T_mount;
    FExposureTime: double;
    FSeqCount: integer;
    FDitherNum: integer;
    FFocusNum: integer;
    FFocusNow: boolean;
    Frunning: boolean;
    FonMsg: TNotifyMsg;
    FonStartExposure: TNotifyEvent;
    FonAbortExposure: TNotifyEvent;
    FonResetStack: TNotifyEvent;
    procedure SetExposureTime(val: double);
    function GetGain:integer;
    procedure SetGain(value:integer);
    function GetOffset:integer;
    procedure SetOffset(value:integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure Stop;
    property Mount: T_mount read FMount write FMount;
    property Running: boolean read Frunning write Frunning;
    property SeqCount: Integer read FSeqCount write FSeqCount;
    property ExposureTime: double read FExposureTime write SetExposureTime;
    property Gain: integer read GetGain write SetGain;
    property Offset: integer read GetOffset write SetOffset;
    property DitherNum: Integer read FDitherNum write FDitherNum;
    property FocusNum: Integer read FFocusNum write FFocusNum;
    property FocusNow: boolean read FFocusNow write FFocusNow;
    property onStartExposure: TNotifyEvent read FonStartExposure write FonStartExposure;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onResetStack: TNotifyEvent read FonResetStack write FonResetStack;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
end;

implementation

{$R *.lfm}

{ Tf_capture }

constructor Tf_capture.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 Frunning:=false;
 FFocusNow:=false;
 FExposureTime:=-1;
 SetLang;
end;

destructor  Tf_capture.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_capture.SetLang;
begin
  Title.Caption:=rsCapture;
  Label1.Caption:=rsExposure;
  LabelGain.Caption:=rsGain;
  Label2.Caption:=rsBinning;
  Label6.Caption:=rsFStop;
  Label3.Caption:=rsObject;
  Label4.Caption:=rsCount;
  Label5.Caption:=rsType;
  CheckBoxDither.Caption:=rsDitherEvery;
  CheckBoxFocus.Caption:=rsFocusEvery;
  BtnStart.Caption:=rsStart;
  ExpTime.Hint:=rsExposureTime;
  Binning.Hint:=rsCameraBinnin;
  ISObox.Hint:=rsCameraISO;
  GainEdit.Hint:=rsCameraGain;
  Fname.Hint:=rsTheObjectNam;
  SeqNum.Hint:=rsTheNumberOfI;
  FrameType.Hint:=rsTheTypeOfFra;
  DitherCount.Hint:=rsTheNumberOfI2;
  FocusCount.Hint:=rsTheNumberOfI3;
  BtnStart.Hint:=rsStartTheCapt
end;

procedure Tf_capture.BtnStartClick(Sender: TObject);
begin
  Frunning:=not Frunning;
  if Frunning then begin
    CancelAutofocus:=false;
    if Sender<>nil then FSeqCount:=1; // otherwise set by plan
    FDitherNum:=0;
    FFocusNum:=0;
    if (TFrameType(FrameType.ItemIndex)=FLAT)and(FlatType=ftDome) then begin
       if DomeFlatSetLight and (DomeFlatSetLightON<>'') then begin
          AdjustFlatLight:=true;
          ExecProcess(DomeFlatSetLightON,nil,false);
       end;
       if FlatAutoExposure then
          AdjustDomeFlat:=true;
       if DomeFlatTelescopeSlew and (FMount<>nil) then
          Mount.SlewToDomeFlatPosition;
    end;
    if Assigned(FonMsg) then FonMsg(rsStartCapture,2);
    EarlyNextExposure:=((TFrameType(FrameType.ItemIndex)=LIGHT)or(TFrameType(FrameType.ItemIndex)=DARK)) and not(PanelStack.Visible and (StackNum.Value>1)) and ConfigExpEarlyStart;
    if PanelStack.Visible and (StackNum.Value>1) and Assigned(FonResetStack) then FonResetStack(self);
    if Assigned(FonStartExposure) then FonStartExposure(self);
    if (not Frunning) and Assigned(FonMsg) then FonMsg(rsCannotStartC,0);
  end else begin
    CancelAutofocus:=true;
    EarlyNextExposure:=false;
    if Assigned(FonAbortExposure) then FonAbortExposure(self);
  end;
  if Frunning then begin
    led.Brush.Color:=clLime;
    BtnStart.Caption:=rsStop;
  end else begin
    EarlyNextExposure:=false;
    led.Brush.Color:=clGray;
    BtnStart.Caption:=rsStart;
    if Assigned(FonMsg) then FonMsg(rsStopCapture,2);
  end;
end;

procedure Tf_capture.ExpTimeChange(Sender: TObject);
begin
  FExposureTime:=StrToFloatDef(ExpTime.Text,-1);
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

function Tf_capture.GetGain:integer;
begin
  if hasGainISO then
    result:=ISObox.ItemIndex
  else
    result:=GainEdit.Value;
end;

procedure Tf_capture.SetGain(value:integer);
begin
  if hasGainISO then begin
    if (value>=0)and(value<ISObox.Items.Count) then
      ISObox.ItemIndex:=value
  end
  else begin
    GainEdit.Value:=value;
  end;
end;

function Tf_capture.GetOffset:integer;
begin
  result:=OffsetEdit.Value;
end;

procedure Tf_capture.SetOffset(value:integer);
begin
  OffsetEdit.Value:=value;
end;

procedure Tf_capture.FrameEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TPanel then begin
     if TPanel(Target).Width>TPanel(Target).Height then begin
        Panel1.ChildSizing.ControlsPerLine:=2;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
         Panel1.ChildSizing.ControlsPerLine:=99;
         Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
end;

procedure Tf_capture.FrameResize(Sender: TObject);
begin
  if Parent is TPanel then begin
     if TPanel(Parent).Width>TPanel(Parent).Height then begin
        Panel1.ChildSizing.ControlsPerLine:=2;
        Panel1.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
     end else begin
         Panel1.ChildSizing.ControlsPerLine:=99;
         Panel1.ChildSizing.Layout:=cclTopToBottomThenLeftToRight;
     end;
  end;
end;

procedure Tf_capture.CheckLight(Sender: TObject);
begin
  if FrameType.ItemIndex<>0 then begin
     CheckBoxDither.Checked:=false;
     CheckBoxFocus.Checked:=false;
  end;
end;

procedure Tf_capture.Stop;
begin
  Frunning:=false;
  EarlyNextExposure:=false;
  CameraProcessingImage:=false;
  led.Brush.Color:=clGray;
  BtnStart.Caption:=rsStart;
  if (TFrameType(FrameType.ItemIndex)=FLAT)and(FlatType=ftDome) then begin
     if DomeFlatSetLight and AdjustFlatLight and (DomeFlatSetLightOFF<>'') then begin
        AdjustFlatLight:=false;
        ExecProcess(DomeFlatSetLightOFF,nil,false);
     end;
  end;
end;

end.

