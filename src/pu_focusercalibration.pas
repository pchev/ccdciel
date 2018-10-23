unit pu_focusercalibration;

{$mode objfpc}{$H+}

interface

uses u_global, cu_focuser, u_translation, Classes, SysUtils, FileUtil,
  TASources, TAGraph, TASeries, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ValEdit, Spin, TACustomSeries;

type

  { Tf_focusercalibration }

  Tf_focusercalibration = class(TForm)
    BtnCancel: TButton;
    BtnNext: TButton;
    BtnBack: TButton;
    BtnDefault: TButton;
    BtnNoBacklash: TButton;
    Focusdir: TComboBox;
    FitSourceL: TListChartSource;
    FitSourceR: TListChartSource;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
    Page6: TPage;
    Panel1: TPanel;
    PtSourceL: TListChartSource;
    PtSourceR: TListChartSource;
    Backlash: TSpinEdit;
    hfdmax: TSpinEdit;
    SpinEdit1: TSpinEdit;
    stepmin: TSpinEdit;
    ValueListEditor1: TValueListEditor;
    ValueListEditor2: TValueListEditor;
    VcChart: TChart;
    VcChartPtL: TLineSeries;
    VcChartPtR: TLineSeries;
    VcChartRegL: TLineSeries;
    VcChartRegR: TLineSeries;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnDefaultClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnNoBacklashClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Ffocuser: T_focuser;
    FCalibration: TNotifyEvent;
    FonCalibrationClose: TNotifyEvent;
    FAbsolute, FRunning, FCalibrationOK: boolean;
    function GetMaxHfd: double;
    function GetMinStep: integer;
    procedure RunCalibration(Data: PtrInt);
    procedure Saveconfig;
    procedure SetLang;
  public
    procedure ProgressL(n:integer; x,y: double);
    procedure ProgressR(n:integer; x,y: double);
    procedure CalibrationCancel(reason:string);
    property focuser: T_focuser read Ffocuser write Ffocuser;
    property FocAbsolute: boolean read FAbsolute write FAbsolute;
    property MaxHfd: double read GetMaxHfd;
    property MinStep: integer read GetMinStep;
    property onCalibration: TNotifyEvent read FCalibration write FCalibration;
    property onCalibrationClose: TNotifyEvent read FonCalibrationClose write FonCalibrationClose;
  end;

var
  f_focusercalibration: Tf_focusercalibration;

implementation

{$R *.lfm}

{ Tf_focusercalibration }

procedure Tf_focusercalibration.FormCreate(Sender: TObject);
begin
  SetLang;
  hfdmax.Value:=20;
  stepmin.Value:=1;
end;

procedure Tf_focusercalibration.SetLang;
begin
  Caption := rsFocuserCalib;
  Label1.Caption := Format(rsThisProcedur, [#10, #10#10, #10, #10#10, #10#10]);
  Label2.Caption := Format(rsGlobalFocuse, [#10#10]);
  Label10.Caption := rsPreferedFocu;
  Label11.Caption := rsIfYourFocuse;
  Label12.Caption := rsBacklashComp;
  BtnNoBacklash.Caption := rsNoBacklash;
  Label3.Caption := rsClickNextToS;
  Label5.Caption := rsMaximumDefoc;
  Label6.Caption := rsMinimumStart;
  Label7.Caption := Format(rsYouCanSetThe, [#10#10]);
  BtnDefault.Caption := rsDefault;
  ValueListEditor1.TitleCaptions[0] := rsParameter;
  ValueListEditor1.TitleCaptions[1] := rsValue;
  ValueListEditor2.TitleCaptions[0] := rsParameter;
  ValueListEditor2.TitleCaptions[1] := rsValue;
  Label8.Caption := rsClickNextToS2;
  Label9.Caption := Format(rsTheDataAreNo, [#10#10, #10, #10#10, #10]);
  BtnCancel.Caption := rsCancel;
  BtnNext.Caption := rsNext;
  BtnBack.Caption := rsBack;
  Focusdir.Items[0]:=rsIn;
  Focusdir.Items[1]:=rsOut;
end;

procedure Tf_focusercalibration.FormShow(Sender: TObject);
begin
  if AutofocusMoveDir then
     Focusdir.ItemIndex:=0
  else
     Focusdir.ItemIndex:=1;
  if Ffocuser.BacklashActive then
    Backlash.Value:=Ffocuser.Backlash
  else
    Backlash.Value:=0;
  FRunning:=false;
  BtnBack.Visible:=false;
  BtnNext.Visible:=true;
  BtnCancel.Visible:=true;
  BtnCancel.Caption:=rsCancel;
  Notebook1.PageIndex:=0;
end;

procedure Tf_focusercalibration.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := not FRunning;
  if (BtnCancel.Caption=rsClose) and Assigned(FonCalibrationClose)
    then FonCalibrationClose(self);
end;

procedure Tf_focusercalibration.CalibrationCancel(reason:string);
begin
  FCalibrationOK:=false;
  label4.Caption:=reason;
end;

procedure Tf_focusercalibration.BtnCancelClick(Sender: TObject);
begin
  TerminateFocuserCalibration:=true;
  if not FRunning then close;
end;

procedure Tf_focusercalibration.BtnNoBacklashClick(Sender: TObject);
begin
  Backlash.Value:=0;
end;

procedure Tf_focusercalibration.BtnDefaultClick(Sender: TObject);
begin
  hfdmax.Value:=20;
  stepmin.Value:=1;
end;

function Tf_focusercalibration.GetMaxHfd: double;
begin
  result:=hfdmax.Value;
end;

function Tf_focusercalibration.GetMinStep: integer;
begin
  result:=stepmin.Value;
end;

procedure Tf_focusercalibration.BtnBackClick(Sender: TObject);
begin
  if Notebook1.PageIndex>0 then begin
    Notebook1.PageIndex:=Notebook1.PageIndex-1;
    BtnBack.Visible:=Notebook1.PageIndex>0;
    BtnNext.Visible:=true;
    BtnCancel.Visible:=true;
  end;
end;

procedure Tf_focusercalibration.BtnNextClick(Sender: TObject);
begin
  if Notebook1.PageIndex=2 then begin
    BtnCancel.Caption:=rsCancel;
    Ffocuser.Backlash:=Backlash.Value;
    Ffocuser.BacklashDirection:=(Focusdir.ItemIndex=0);
    Ffocuser.BacklashActive:=(Ffocuser.Backlash<>0);
    AutofocusMoveDir:=Ffocuser.BacklashDirection;
    if not FRunning then Application.QueueAsyncCall(@RunCalibration,0);
  end
  else if Notebook1.PageIndex=4 then begin
    Saveconfig;
    Notebook1.PageIndex:=Notebook1.PageIndex+1;
    BtnBack.Visible:=true;
    BtnNext.Visible:=false;
    BtnCancel.Visible:=true;
    BtnCancel.Caption:=rsClose;
  end
  else begin
     Notebook1.PageIndex:=Notebook1.PageIndex+1;
     BtnBack.Visible:=(Notebook1.PageIndex>0)and(Notebook1.PageIndex<4);
     BtnNext.Visible:=Notebook1.PageIndex<Notebook1.PageCount;
     BtnCancel.Visible:=true;
     BtnCancel.Caption:=rsCancel;
  end;
end;

procedure Tf_focusercalibration.ProgressL(n:integer; x,y: double);
begin
  PtSourceL.Add(x,y,'',clGreen);
  FitSourceL.Add(x,y);
end;

procedure Tf_focusercalibration.ProgressR(n:integer; x,y: double);
begin
  PtSourceR.Add(x,y,'',clGreen);
  FitSourceR.Add(x,y);
end;

procedure Tf_focusercalibration.RunCalibration(Data: PtrInt);
begin
  FRunning:=true;
  label4.Caption:=rsFocuserCalib3;
  Notebook1.PageIndex:=3;
  BtnBack.Visible:=false;
  BtnNext.Visible:=false;
  BtnCancel.Visible:=true;
  FCalibrationOK:=true;
  PtSourceL.Clear;
  FitSourceL.Clear;
  PtSourceR.Clear;
  FitSourceR.Clear;
  if Assigned(FCalibration) then FCalibration(self);
  BtnBack.Visible:=true;
  BtnNext.Visible:=false;
  BtnCancel.Visible:=true;
  if FCalibrationOK then begin
    label4.Caption:=rsFocuserCalib4;
    BtnNext.Visible:=true;
  end;
  FRunning:=false;
end;

procedure Tf_focusercalibration.Saveconfig;
begin
  config.SetValue('/StarAnalysis/Window',Starwindow);
  config.SetValue('/StarAnalysis/Focus',Focuswindow);
  config.SetValue('/StarAnalysis/AutoFocusMode',ord(AutofocusMode));
  config.SetValue('/StarAnalysis/AutofocusBinning',AutofocusBinning);
  config.SetValue('/StarAnalysis/AutofocusStartHFD',AutofocusStartHFD);
  config.SetValue('/StarAnalysis/AutofocusNearHFD',AutofocusNearHFD);
  config.SetValue('/StarAnalysis/AutofocusNearNum',AutofocusNearNum);
  config.SetValue('/StarAnalysis/AutofocusTolerance',AutofocusTolerance);
  config.SetValue('/StarAnalysis/AutofocusMinSNR',AutofocusMinSNR);
  config.SetValue('/StarAnalysis/AutofocusMoveDir',AutofocusMoveDir);
  if FAbsolute then begin
    config.SetValue('/StarAnalysis/Vcurve/VcCenterpos',VcCenterpos);
    config.SetValue('/StarAnalysis/Vcurve/VcHalfwidth',VcHalfwidth);
    config.SetValue('/StarAnalysis/Vcurve/VcNsteps',VcNsteps);
  end;
  config.SetValue('/StarAnalysis/AutofocusDynamicNumPoint',AutofocusDynamicNumPoint);
  config.SetValue('/StarAnalysis/AutofocusDynamicMovement',AutofocusDynamicMovement);
  config.SetValue('/StarAnalysis/AutofocusMinSpeed',AutofocusMinSpeed);
  config.SetValue('/StarAnalysis/AutofocusMaxSpeed',AutofocusMaxSpeed);
  config.SetValue('/StarAnalysis/FocuserBacklash',Ffocuser.Backlash);
  config.SetValue('/StarAnalysis/FocuserBacklashActive',Ffocuser.BacklashActive);
  config.SetValue('/StarAnalysis/FocuserBacklashDirection',Ffocuser.BacklashDirection);

  config.Flush;
end;

end.

