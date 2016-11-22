unit pu_vcurve;

{$mode objfpc}{$H+}

interface

uses fu_starprofile, fu_focuser, fu_preview, u_global, u_utils, Classes, SysUtils,
  FileUtil, TAGraph, TAFuncSeries, TASources, TAMultiSeries, Forms, Controls,
  Math, Graphics, Dialogs, StdCtrls, ComCtrls, TACustomSeries, TASeries;

type

  { Tf_vcurve }

  Tf_vcurve = class(TForm)
    BtnLearnVcurve: TButton;
    BtnSave: TButton;
    Label12: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Nsteps: TEdit;
    GetPos: TButton;
    FocusPos: TEdit;
    HalfWidth: TEdit;
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
    TrackBar1: TTrackBar;
    VcChart: TChart;
    VcChartL: TFitSeries;
    VcChartPtR: TLineSeries;
    VcChartPtL: TLineSeries;
    VcChartR: TFitSeries;
    procedure BtnLearnVcurveClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure GetPosClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
    Fstarprofile: Tf_starprofile;
    Ffocuser: Tf_focuser;
    Fpreview: Tf_preview;
    FonLearnVcurve: TNotifyEvent;
    FonSaveVcurve: TNotifyEvent;
  public
    { public declarations }
    Procedure FindLinearPart;
    Procedure LoadCurve;
    property preview:Tf_preview read Fpreview write Fpreview;
    property focuser:Tf_focuser read Ffocuser write Ffocuser;
    property starprofile:Tf_starprofile read Fstarprofile write Fstarprofile;
    property onLearnVcurve: TNotifyEvent read FonLearnVcurve write FonLearnVcurve;
    property onSaveVcurve: TNotifyEvent read FonSaveVcurve write FonSaveVcurve;
  end;


implementation

{$R *.lfm}

{ Tf_vcurve }


procedure Tf_vcurve.BtnLearnVcurveClick(Sender: TObject);
begin
  if Assigned(FonLearnVcurve) then FonLearnVcurve(self);
end;

procedure Tf_vcurve.BtnSaveClick(Sender: TObject);
begin
  if Assigned(FonSaveVcurve) then FonSaveVcurve(self);
  Close;
end;

procedure Tf_vcurve.GetPosClick(Sender: TObject);
begin
  FocusPos.Text:=Ffocuser.Position.Text;
end;

procedure Tf_vcurve.TrackBar1Change(Sender: TObject);
begin
  AutofocusVcSkipNum:=TrackBar1.Position;
  LoadCurve;
end;

Procedure Tf_vcurve.FindLinearPart;
var i,a: integer;
  g1,g2: double;
begin
try
if (AutofocusVcNum>0)and(AutofocusVcDir=AutofocusMoveDir) then begin
  g1:=0;
  a:=-1;
  repeat
    g2:=g1;
    inc(a);
    FitSourceL.DataPoints.Clear;
    if AutofocusVcDir then begin
      for i:=0 to PosFocus-a do
        FitSourceL.Add(AutofocusVc[i,2],AutofocusVc[i,1]);
    end else begin
      for i:=PosFocus+a to AutofocusVcNum do
        FitSourceL.Add(AutofocusVc[i,2],AutofocusVc[i,1]);
    end;
    VcChartL.ExecFit;
    g1:=VcChartL.GoodnessOfFit;
  until (a>=(AutofocusVcNum/4))or((g1>0.97)and(abs(g1-g2)<0.01));
end;
AutofocusVcSkipNum:=a;
except
end;
end;

Procedure Tf_vcurve.LoadCurve;
var i,nl,nr: integer;
  g,gl,gr,bl,br,x:double;
  pl,pr:array of TDouble2;
  col: TColor;
begin
try
if (AutofocusVcNum>0)and(AutofocusVcDir=AutofocusMoveDir) then begin
  TrackBar1.Max:=max(AutofocusVcSkipNum,round(1+AutofocusVcNum/4));
  TrackBar1.Position:=AutofocusVcSkipNum;

  nl:=PosFocus-AutofocusVcSkipNum+1;
  SetLength(pl,nl);
  nr:=AutofocusVcNum-(PosFocus+AutofocusVcSkipNum)+1;
  SetLength(pr,nr);
  for i:=0 to nl-1 do begin
    pl[i,2]:=AutofocusVc[i,1];
    pl[i,1]:=AutofocusVc[i,2];
  end;
  LeastSquares(pl,x,AutofocusVcpiL,gl);
  for i:=0 to nr-1 do begin
    pr[i,2]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,1];
    pr[i,1]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,2];
  end;
  LeastSquares(pr,x,AutofocusVcpiR,gr);
  AutofocusVcPID:=abs(AutofocusVcpiL-AutofocusVcpiR);
  if AutofocusVcDir then g:=gr*gr else g:=gl*gl;
  label8.Caption:=FormatFloat(f4,g);

  for i:=0 to nl-1 do begin
    pl[i,1]:=AutofocusVc[i,1];
    pl[i,2]:=AutofocusVc[i,2];
  end;
  LeastSquares(pl,AutofocusVcSlopeL,bl,gl);
  for i:=0 to nr-1 do begin
    pr[i,1]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,1];
    pr[i,2]:=AutofocusVc[PosFocus+AutofocusVcSkipNum+i,2];
  end;
  LeastSquares(pr,AutofocusVcSlopeR,br,gr);

  FitSourceL.DataPoints.Clear;
  FitSourceR.DataPoints.Clear;
  FitSourceL.Add(AutofocusVc[0,1],AutofocusVc[0,1]*AutofocusVcSlopeL+bl);
  FitSourceL.Add(AutofocusVcpiL,0);
  FitSourceR.Add(AutofocusVcpiR,0);
  FitSourceR.Add(AutofocusVc[AutofocusVcNum,1],AutofocusVc[AutofocusVcNum,1]*AutofocusVcSlopeR+br);

  PtSourceL.DataPoints.Clear;
  PtSourceR.DataPoints.Clear;
  for i:=0 to PosFocus do begin
    if i<nl then col:=clLime  else col:=clRed;
    PtSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;
  for i:=PosFocus to AutofocusVcNum do begin
    if i>=(PosFocus+AutofocusVcSkipNum) then col:=clLime else col:=clRed;
    PtSourceR.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;

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

end.

