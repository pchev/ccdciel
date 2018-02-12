unit pu_focusercalibration;

{$mode objfpc}{$H+}

interface

uses u_global, Classes, SysUtils, FileUtil, TASources, TAGraph, TASeries, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ValEdit, TADrawUtils, TACustomSeries;

type

  { Tf_focusercalibration }

  Tf_focusercalibration = class(TForm)
    BtnCancel: TButton;
    BtnNext: TButton;
    BtnBack: TButton;
    BtnDefault: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FitSourceL: TListChartSource;
    FitSourceR: TListChartSource;
    Label1: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCalibration: TNotifyEvent;
    FonVcurveLearning: TNotifyEvent;
    FAbsolute, FRunning, FCalibrationOK: boolean;
    function GetMaxHfd: integer;
    function GetMinStep: integer;
    procedure RunCalibration(Data: PtrInt);
    procedure Saveconfig;
  public
    procedure ProgressL(n:integer; x,y: double);
    procedure ProgressR(n:integer; x,y: double);
    procedure CalibrationCancel(reason:string);
    property FocAbsolute: boolean read FAbsolute write FAbsolute;
    property MaxHfd: integer read GetMaxHfd;
    property MinStep: integer read GetMinStep;
    property onCalibration: TNotifyEvent read FCalibration write FCalibration;
    property onVcurveLearning: TNotifyEvent read FonVcurveLearning write FonVcurveLearning;
  end;

var
  f_focusercalibration: Tf_focusercalibration;

implementation

{$R *.lfm}

{ Tf_focusercalibration }

procedure Tf_focusercalibration.FormCreate(Sender: TObject);
begin
  edit1.Text:='20';
  edit2.Text:='1';
end;

procedure Tf_focusercalibration.FormShow(Sender: TObject);
begin
  FRunning:=false;
  BtnBack.Visible:=false;
  BtnNext.Visible:=true;
  BtnCancel.Visible:=true;
  BtnCancel.Caption:='Cancel';
  Notebook1.PageIndex:=0;
end;

procedure Tf_focusercalibration.BtnDefaultClick(Sender: TObject);
begin
  edit1.Text:='20';
  edit2.Text:='1';
end;

function Tf_focusercalibration.GetMaxHfd: integer;
begin
  result:=StrToIntDef(Edit1.Text,20);
end;

function Tf_focusercalibration.GetMinStep: integer;
begin
  result:=StrToIntDef(Edit2.Text,1);
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
    BtnCancel.Caption:='Cancel';
    if not FRunning then Application.QueueAsyncCall(@RunCalibration,0);
  end
  else if Notebook1.PageIndex=4 then begin
    Saveconfig;
    Notebook1.PageIndex:=Notebook1.PageIndex+1;
    BtnBack.Visible:=true;
    BtnNext.Visible:=false;
    BtnCancel.Visible:=true;
    BtnCancel.Caption:='Close';
  end
  else begin
     Notebook1.PageIndex:=Notebook1.PageIndex+1;
     BtnBack.Visible:=(Notebook1.PageIndex>0)and(Notebook1.PageIndex<4);
     BtnNext.Visible:=Notebook1.PageIndex<Notebook1.PageCount;
     BtnCancel.Visible:=true;
     BtnCancel.Caption:='Cancel';
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

procedure Tf_focusercalibration.BtnCancelClick(Sender: TObject);
begin
  TerminateFocuserCalibration:=true;
  if (BtnCancel.Caption='Close') and Assigned(FonVcurveLearning) then FonVcurveLearning(self);
  if not FRunning then close;
end;

procedure Tf_focusercalibration.CalibrationCancel(reason:string);
begin
  FCalibrationOK:=false;
  label4.Caption:=reason;
end;

procedure Tf_focusercalibration.RunCalibration(Data: PtrInt);
begin
  FRunning:=true;
  label4.Caption:='Focuser calibration started, please wait...';
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
    label4.Caption:='Focuser calibration completed, click Next to see the result.';
    BtnNext.Visible:=true;
  end;
  FRunning:=false;
end;

procedure Tf_focusercalibration.Saveconfig;
begin
  config.SetValue('/StarAnalysis/Window',Starwindow);
  config.SetValue('/StarAnalysis/Focus',Focuswindow);
  config.SetValue('/StarAnalysis/AutofocusBinning',AutofocusBinning);
  config.SetValue('/StarAnalysis/AutofocusStartHFD',AutofocusStartHFD);
  config.SetValue('/StarAnalysis/AutofocusNearHFD',AutofocusNearHFD);
  config.SetValue('/StarAnalysis/AutofocusNearNum',AutofocusNearNum);
  config.SetValue('/StarAnalysis/AutofocusTolerance',AutofocusTolerance);
  config.SetValue('/StarAnalysis/AutofocusMinSNR',AutofocusMinSNR);
  if FAbsolute then begin
    config.SetValue('/StarAnalysis/Vcurve/VcCenterpos',VcCenterpos);
    config.SetValue('/StarAnalysis/Vcurve/VcHalfwidth',VcHalfwidth);
    config.SetValue('/StarAnalysis/Vcurve/VcNsteps',VcNsteps);
  end;
  config.SetValue('/StarAnalysis/AutofocusDynamicNumPoint',AutofocusDynamicNumPoint);
  config.SetValue('/StarAnalysis/AutofocusDynamicMovement',AutofocusDynamicMovement);
  config.SetValue('/StarAnalysis/AutofocusMinSpeed',AutofocusMinSpeed);
  config.SetValue('/StarAnalysis/AutofocusMaxSpeed',AutofocusMaxSpeed);
  config.Flush;
end;

end.

