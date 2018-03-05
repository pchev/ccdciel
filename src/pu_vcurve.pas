unit pu_vcurve;

{$mode objfpc}{$H+}

interface

uses fu_starprofile, fu_focuser, fu_preview, u_global, u_utils, Classes, SysUtils, u_translation,
  FileUtil, TAGraph, TAFuncSeries, TASources, TAMultiSeries, TAChartUtils, Forms, Controls,
  Math, Graphics, Dialogs, StdCtrls, ComCtrls, Spin, TACustomSeries, TASeries;

type

  { Tf_vcurve }

  Tf_vcurve = class(TForm)
    BtnLearnVcurve: TButton;
    BtnSave: TButton;
    BtnStopVcurve: TButton;
    Label12: TLabel;
    LabelStepProgress: TLabel;
    LabelCoord: TLabel;
    LabelFocusdir: TLabel;
    LabelQuality: TLabel;
    Label9: TLabel;
    RefSource: TListChartSource;
    GetPos: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelSL: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    LabelCenter: TLabel;
    LabelSR: TLabel;
    LabelPID: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelPIL: TLabel;
    LabelPIR: TLabel;
    FitSourceL: TListChartSource;
    FitSourceR: TListChartSource;
    PtSourceL: TListChartSource;
    PtSourceR: TListChartSource;
    FocusPos: TSpinEdit;
    HalfWidth: TSpinEdit;
    Nsteps: TSpinEdit;
    TrackBar1: TTrackBar;
    VcChart: TChart;
    VcChartRef: TLineSeries;
    VcChartRegR: TLineSeries;
    VcChartRegL: TLineSeries;
    VcChartPtR: TLineSeries;
    VcChartPtL: TLineSeries;
    procedure BtnLearnVcurveClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnStopVcurveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetPosClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure VcChartMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { private declarations }
    Fstarprofile: Tf_starprofile;
    Ffocuser: Tf_focuser;
    Fpreview: Tf_preview;
    FQuality: double;
    FonLearnVcurve: TNotifyEvent;
    FonStopVcurve: TNotifyEvent;
    FonSaveVcurve: TNotifyEvent;
    procedure LearnVcurveAsync(Data: PtrInt);
    procedure SetLang;
  public
    { public declarations }
    Procedure FindLinearPart;
    Procedure LoadCurve;
    Procedure ClearGraph;
    procedure LearnProgress(n:integer; x,y: double);
    property Quality: double read FQuality;
    property preview:Tf_preview read Fpreview write Fpreview;
    property focuser:Tf_focuser read Ffocuser write Ffocuser;
    property starprofile:Tf_starprofile read Fstarprofile write Fstarprofile;
    property onLearnVcurve: TNotifyEvent read FonLearnVcurve write FonLearnVcurve;
    property onStopVcurve: TNotifyEvent read FonStopVcurve write FonStopVcurve;
    property onSaveVcurve: TNotifyEvent read FonSaveVcurve write FonSaveVcurve;
  end;


implementation

{$R *.lfm}

{ Tf_vcurve }

procedure Tf_vcurve.LearnVcurveAsync(Data: PtrInt);
begin
  BtnStopVcurve.Visible:=true;
  GetPos.Visible:=false;
  BtnLearnVcurve.Visible:=false;
  BtnSave.Visible:=false;
  FocusPos.Enabled:=false;
  HalfWidth.Enabled:=false;
  Nsteps.Enabled:=false;
  TrackBar1.Enabled:=false;
  ClearGraph;
  try
   if Assigned(FonLearnVcurve) then FonLearnVcurve(self);
  finally
   LabelStepProgress.Caption:='';
   BtnStopVcurve.Visible:=false;
   BtnLearnVcurve.Visible:=true;
   GetPos.Visible:=true;
   BtnSave.Visible:=true;
   FocusPos.Enabled:=true;
   HalfWidth.Enabled:=true;
   Nsteps.Enabled:=true;
   TrackBar1.Enabled:=true;
  end;
end;

procedure Tf_vcurve.FormCreate(Sender: TObject);
begin
  SetLang;
end;


procedure Tf_vcurve.SetLang;
begin
  label1.Caption:=rsFocusPositio;
  label2.Caption:=rsMaxOffset;
  label3.Caption:=rsNumberOfStep;
  GetPos.Caption:=rsGetCurrent;
  LabelFocusdir.Caption:=rsFocusDirecti;
  Label9.Caption:=rsFit;
  Label12.Caption:=rsQuality;
  Label4.Caption:=rsSlope+' L';
  Label6.Caption:=rsSlope+' R';
  Label11.Caption:=rsCenter;
  BtnStopVcurve.Caption:=rsStop;
  BtnLearnVcurve.Caption:=rsLearn;
  BtnSave.Caption:=rsSave;

end;

procedure Tf_vcurve.BtnLearnVcurveClick(Sender: TObject);
begin
  if BtnStopVcurve.Visible then exit;
  application.QueueAsyncCall(@LearnVcurveAsync,0);
end;

procedure Tf_vcurve.BtnSaveClick(Sender: TObject);
var r2: double;
begin
  r2:=StrToFloatDef(LabelQuality.Caption,-1);
  if r2<0.9 then begin
    if MessageDlg(rsVCurveQualit, mtConfirmation, mbYesNo, 0)<>mrYes
    then
       exit;
  end;
  if Assigned(FonSaveVcurve) then FonSaveVcurve(self);
  Close;
end;

procedure Tf_vcurve.BtnStopVcurveClick(Sender: TObject);
begin
  if Assigned(FonStopVcurve) then FonStopVcurve(self);
end;

procedure Tf_vcurve.FormShow(Sender: TObject);
begin
  LabelFocusdir.Caption:=rsFocusDirecti2;
  if AutofocusMoveDir=FocusDirIn then
     LabelFocusdir.Caption:=LabelFocusdir.Caption+' '+rsIn+' <='
  else
     LabelFocusdir.Caption:=LabelFocusdir.Caption+' '+rsOut+' =>';
end;

procedure Tf_vcurve.GetPosClick(Sender: TObject);
begin
  FocusPos.Value:=Ffocuser.Position.Value;
end;

procedure Tf_vcurve.TrackBar1Change(Sender: TObject);
begin
  AutofocusVcSkipNum:=TrackBar1.Position;
  LoadCurve;
end;

procedure Tf_vcurve.VcChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var pointi: TPoint;
    pointg: TDoublePoint;
begin
  if (x>5)and(x<(VcChart.Width-5))and(y>5)and(y<(VcChart.Height-5)) then begin
  try
  pointi.x:=X;
  pointi.y:=Y;
  pointg:=VcChart.ImageToGraph(pointi);
  LabelCoord.Caption:='Pos:'+IntToStr(trunc(pointg.x))+' HFD:'+FormatFloat(f1,pointg.y);
  except
  end;
  end else begin
    LabelCoord.Caption:='';
  end;
end;

Procedure Tf_vcurve.FindLinearPart;
var i,n,s: integer;
  a,b,rL,rR,r1,r2: double;
  p:array of TDouble2;
begin
try
if (AutofocusVcNum>0)and(AutofocusVcDir=AutofocusMoveDir) then begin
  r1:=0;
  s:=-1;
  // search flat central part
  repeat
    r2:=r1;
    inc(s);
    // right part
    n:=AutofocusVcNum-(PosFocus+s)+1;
    SetLength(p,n);
    for i:=0 to n-1 do begin
      p[i,1]:=AutofocusVc[PosFocus+s+i,1];
      p[i,2]:=AutofocusVc[PosFocus+s+i,2];
    end;
    LeastSquares(p,a,b,rR);
    // left part
    n:=PosFocus-s+1;
    SetLength(p,n);
    for i:=0 to n-1 do begin
      p[i,1]:=AutofocusVc[i,1];
      p[i,2]:=AutofocusVc[i,2];
    end;
    LeastSquares(p,a,b,rL);
    // worst of the two values
    r1:=min(rR*rR,rL*rL);
  until (s>=(AutofocusVcNum/4))or((r1>0.97)and(abs(r1-r2)<0.01));
end;
AutofocusVcSkipNum:=s;
except
end;
end;

Procedure Tf_vcurve.ClearGraph;
begin
  FitSourceL.DataPoints.Clear;
  FitSourceR.DataPoints.Clear;
  PtSourceL.DataPoints.Clear;
  PtSourceR.DataPoints.Clear;
  RefSource.DataPoints.Clear;
end;

Procedure Tf_vcurve.LoadCurve;
var i,nl,nr: integer;
  r2,rl,rr,bl,br:double;
  pl,pr:array of TDouble2;
  col: TColor;
begin
try
if (AutofocusVcNum>0)and(AutofocusVcDir=AutofocusMoveDir) then begin
  ClearGraph;
  // skip flat central part
  TrackBar1.Max:=max(AutofocusVcSkipNum,round(1+AutofocusVcNum/4));
  TrackBar1.Position:=AutofocusVcSkipNum;

  // fit left part
  nl:=PosFocus-AutofocusVcSkipNum+1;
  SetLength(pl,nl);
  for i:=0 to nl-1 do begin
    pl[i,1]:=AutofocusVc[i,1];
    pl[i,2]:=AutofocusVc[i,2];
  end;
  LeastSquares(pl,AutofocusVcSlopeL,bl,rl);
  AutofocusVcpiL:=-bl/AutofocusVcSlopeL;

  // fit right part
  nr:=AutofocusVcNum-(PosFocus+AutofocusVcSkipNum)+1;
  SetLength(pr,nr);
  for i:=0 to nr-1 do begin
    pr[i,1]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,1];
    pr[i,2]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,2];
  end;
  LeastSquares(pr,AutofocusVcSlopeR,br,rr);
  AutofocusVcpiR:=-br/AutofocusVcSlopeR;

  AutofocusVcPID:=AutofocusVcpiR-AutofocusVcpiL;
  if AutofocusVcDir then r2:=rr*rr else r2:=rl*rl;

  // draw linear regression
  FitSourceL.Add(AutofocusVc[0,1],AutofocusVc[0,1]*AutofocusVcSlopeL+bl);
  FitSourceL.Add(AutofocusVcpiL,0);
  FitSourceR.Add(AutofocusVcpiR,0);
  FitSourceR.Add(AutofocusVc[AutofocusVcNum,1],AutofocusVc[AutofocusVcNum,1]*AutofocusVcSlopeR+br);

  // draw data points
  for i:=0 to PosFocus do begin
    if i<nl then col:=clGreen  else col:=clRed;
    PtSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;
  for i:=PosFocus to AutofocusVcNum do begin
    if i>=(PosFocus+AutofocusVcSkipNum) then col:=clGreen else col:=clRed;
    PtSourceR.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;

  // draw near focus reference
  RefSource.Add(AutofocusVc[0,1],AutofocusNearHFD);
  RefSource.Add(AutofocusVc[AutofocusVcNum,1],AutofocusNearHFD);


  // print data
  FQuality:=r2;
  LabelQuality.Caption:=FormatFloat(f4,r2);
  LabelSL.Caption:=FormatFloat(f6,AutofocusVcSlopeL);
  LabelSR.Caption:=FormatFloat(f6,AutofocusVcSlopeR);
  LabelCenter.Caption:=inttostr(round((AutofocusVcpiL+AutofocusVcpiR)/2));
  LabelPIL.Caption:=FormatFloat(f2,AutofocusVcpiL);;
  LabelPID.Caption:=FormatFloat(f2,AutofocusVcPID);
  LabelPIR.Caption:=FormatFloat(f2,AutofocusVcpiR);;

end;
except
end;
end;

procedure Tf_vcurve.LearnProgress(n:integer; x,y: double);
begin
  LabelStepProgress.Caption:='/ '+IntToStr(n+1);
  PtSourceL.Add(x,y,'',clGreen);
  FitSourceL.Add(x,y);
end;

end.

