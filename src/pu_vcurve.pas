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
    LabelFocusdir: TLabel;
    LabelQuality: TLabel;
    Label9: TLabel;
    RefSource: TListChartSource;
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
    VcChartRef: TLineSeries;
    VcChartRegR: TLineSeries;
    VcChartRegL: TLineSeries;
    VcChartPtR: TLineSeries;
    VcChartPtL: TLineSeries;
    procedure BtnLearnVcurveClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure Tf_vcurve.FormShow(Sender: TObject);
begin
  LabelFocusdir.Caption:='Focus direction';
  if AutofocusMoveDir=FocusDirIn then
     LabelFocusdir.Caption:=LabelFocusdir.Caption+' <='
  else
     LabelFocusdir.Caption:=LabelFocusdir.Caption+' =>';
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
var i,n,s: integer;
  a,b,r,r1,r2: double;
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
    if AutofocusVcDir then begin
      // focus IN, use right part
      n:=AutofocusVcNum-(PosFocus+s)+1;
      SetLength(p,n);
      for i:=0 to n-1 do begin
        p[i,1]:=AutofocusVc[PosFocus+s+i,1];
        p[i,2]:=AutofocusVc[PosFocus+s+i,2];
      end;
    end else begin
      // focus OUT, use left part
      n:=PosFocus-s+1;
      SetLength(p,n);
      for i:=0 to n-1 do begin
        p[i,1]:=AutofocusVc[i,1];
        p[i,2]:=AutofocusVc[i,2];
      end;
    end;
    LeastSquares(p,a,b,r);
    r1:=r*r;
  until (s>=(AutofocusVcNum/4))or((r1>0.97)and(abs(r1-r2)<0.01));
end;
AutofocusVcSkipNum:=s;
except
end;
end;

Procedure Tf_vcurve.LoadCurve;
var i,nl,nr: integer;
  r2,rl,rr,bl,br,x:double;
  pl,pr:array of TDouble2;
  col: TColor;
begin
try
if (AutofocusVcNum>0)and(AutofocusVcDir=AutofocusMoveDir) then begin
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

  AutofocusVcPID:=abs(AutofocusVcpiL-AutofocusVcpiR);
  if AutofocusVcDir then r2:=rr*rr else r2:=rl*rl;

  // draw linear regression
  FitSourceL.DataPoints.Clear;
  FitSourceR.DataPoints.Clear;
  FitSourceL.Add(AutofocusVc[0,1],AutofocusVc[0,1]*AutofocusVcSlopeL+bl);
  FitSourceL.Add(AutofocusVcpiL,0);
  FitSourceR.Add(AutofocusVcpiR,0);
  FitSourceR.Add(AutofocusVc[AutofocusVcNum,1],AutofocusVc[AutofocusVcNum,1]*AutofocusVcSlopeR+br);

  // draw data points
  PtSourceL.DataPoints.Clear;
  PtSourceR.DataPoints.Clear;
  for i:=0 to PosFocus do begin
    if i<nl then col:=clGreen  else col:=clRed;
    PtSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;
  for i:=PosFocus to AutofocusVcNum do begin
    if i>=(PosFocus+AutofocusVcSkipNum) then col:=clGreen else col:=clRed;
    PtSourceR.Add(AutofocusVc[i,1],AutofocusVc[i,2],'',col);
  end;

  // draw near focus reference
  RefSource.DataPoints.Clear;
  RefSource.Add(AutofocusVc[0,1],AutofocusNearHFD);
  RefSource.Add(AutofocusVc[AutofocusVcNum,1],AutofocusNearHFD);


  // print data
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

end.

