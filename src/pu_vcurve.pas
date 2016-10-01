unit pu_vcurve;

{$mode objfpc}{$H+}

interface

uses fu_starprofile, fu_focuser, fu_preview, u_global, Classes, SysUtils,
  FileUtil, TAGraph, TAFuncSeries, TASources, TAMultiSeries, Forms, Controls,
  Graphics, Dialogs, StdCtrls, TACustomSeries, TASeries;

type

  { Tf_vcurve }

  Tf_vcurve = class(TForm)
    BtnLearnVcurve: TButton;
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
    Memo1: TMemo;
    VcChart: TChart;
    VcChartL: TFitSeries;
    VcChartPtR: TLineSeries;
    VcChartPtL: TLineSeries;
    VcChartR: TFitSeries;
    procedure BtnLearnVcurveClick(Sender: TObject);
  private
    { private declarations }
    Fstarprofile: Tf_starprofile;
    Ffocuser: Tf_focuser;
    Fpreview: Tf_preview;
    FonLearnVcurve: TNotifyEvent;
  public
    { public declarations }
    Procedure LoadCurve;
    property preview:Tf_preview read Fpreview write Fpreview;
    property focuser:Tf_focuser read Ffocuser write Ffocuser;
    property starprofile:Tf_starprofile read Fstarprofile write Fstarprofile;
    property onLearnVcurve: TNotifyEvent read FonLearnVcurve write FonLearnVcurve;
  end;


implementation

{$R *.lfm}

{ Tf_vcurve }


procedure Tf_vcurve.BtnLearnVcurveClick(Sender: TObject);
begin
  if Assigned(FonLearnVcurve) then FonLearnVcurve(self);
end;

Procedure Tf_vcurve.LoadCurve;
var i: integer;
begin
try
if AutofocusVcNum>0 then begin
  memo1.Clear;
  memo1.Lines.Add('Start L:'+inttostr(round(AutofocusVc[PosStartL,1]))+' '+FormatFloat(f1,AutofocusVc[PosStartL,2]));
//  memo1.Lines.Add('Near  L:'+inttostr(round(AutofocusVc[PosNearL,1]))+' '+FormatFloat(f1,AutofocusVc[PosNearL,2]));
  memo1.Lines.Add('Center :'+inttostr(round(AutofocusVc[PosFocus,1]))+' '+FormatFloat(f1,AutofocusVc[PosFocus,2]));
//  memo1.Lines.Add('Near  R:'+inttostr(round(AutofocusVc[PosNearR,1]))+' '+FormatFloat(f1,AutofocusVc[PosNearR,2]));
  memo1.Lines.Add('End   R:'+inttostr(round(AutofocusVc[PosStartR,1]))+' '+FormatFloat(f1,AutofocusVc[PosStartR,2]));

  FitSourceL.DataPoints.Clear;
  FitSourceR.DataPoints.Clear;
  for i:=0 to PosFocus do
    FitSourceL.Add(AutofocusVc[i,2],AutofocusVc[i,1]);
  for i:=PosFocus to AutofocusVcNum do
    FitSourceR.Add(AutofocusVc[i,2],AutofocusVc[i,1]);
  VcChartL.ExecFit;
  VcChartR.ExecFit;
  AutofocusVcpiL:=VcChartL.Param[0];
  AutofocusVcpiR:=VcChartR.Param[0];
  AutofocusVcPID:=abs(AutofocusVcpiL-AutofocusVcpiR);

  FitSourceL.DataPoints.Clear;
  FitSourceR.DataPoints.Clear;
  for i:=0 to PosFocus do
    FitSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2]);
  for i:=PosFocus to AutofocusVcNum do
    FitSourceR.Add(AutofocusVc[i,1],AutofocusVc[i,2]);
  VcChartL.ExecFit;
  VcChartR.ExecFit;
  AutofocusVcSlopeL:=VcChartL.Param[1];
  AutofocusVcSlopeR:=VcChartR.Param[1];

  PtSourceL.DataPoints.Clear;
  PtSourceR.DataPoints.Clear;
  for i:=0 to PosFocus do
    PtSourceL.Add(AutofocusVc[i,1],AutofocusVc[i,2]);
  for i:=PosFocus to AutofocusVcNum do
    PtSourceR.Add(AutofocusVc[i,1],AutofocusVc[i,2]);

  LabelSL.Caption:=FormatFloat(f6,AutofocusVcSlopeL);
  LabelSR.Caption:=FormatFloat(f6,AutofocusVcSlopeR);
  LabelCenter.Caption:=inttostr(round(AutofocusVc[PosFocus,1]));
  LabelPIL.Caption:=FormatFloat(f2,AutofocusVcpiL);;
  LabelPID.Caption:=FormatFloat(f2,AutofocusVcPID);
  LabelPIR.Caption:=FormatFloat(f2,AutofocusVcpiR);;

end;
except
end;
end;

end.

