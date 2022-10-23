unit fu_autoguider;

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

{$mode objfpc}{$H+}

interface

uses  UScaleDPI, u_translation, u_hints, u_global,
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASeries, Forms, Graphics, Controls, StdCtrls, ExtCtrls;

type

  { Tf_autoguider }

  Tf_autoguider = class(TFrame)
    BtnConnect: TButton;
    BtnCal: TButton;
    BtnGuide: TButton;
    BtnDither: TButton;
    GuideChart: TChart;
    GuideChartDecdist: TLineSeries;
    GuideChartStarmass: TLineSeries;
    GuideChartRAdist: TLineSeries;
    LabelStat: TLabel;
    Panel5: TPanel;
    ShowStat: TCheckBox;
    Panel1: TPanel;
    led: TShape;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Status: TEdit;
    Title: TLabel;
    procedure BtnCalClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDitherClick(Sender: TObject);
    procedure BtnGuideClick(Sender: TObject);
    procedure GuideChartDblClick(Sender: TObject);
    procedure ShowStatChange(Sender: TObject);
  private
    { private declarations }
    FonConnect,FonCalibrate,FonGuide,FonDither,FonShowStat, FonClearStat: TNotifyEvent;
    FAutoguiderType: TAutoguiderType;
    procedure SetAutoguiderType(value:TAutoguiderType);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    property AutoguiderType: TAutoguiderType read FAutoguiderType write SetAutoguiderType;
    property onConnect: TNotifyEvent read FonConnect write FonConnect;
    property onCalibrate: TNotifyEvent read FonCalibrate write FonCalibrate;
    property onGuide: TNotifyEvent read FonGuide write FonGuide;
    property onDither: TNotifyEvent read FonDither write FonDither;
    property onShowStat: TNotifyEvent read FonShowStat write FonShowStat;
    property onClearStat: TNotifyEvent read FonClearStat write FonClearStat;
  end;

implementation

{$R *.lfm}

{ Tf_autoguider }

constructor Tf_autoguider.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 Panel1.ChildSizing.TopBottomSpacing:=8;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_autoguider.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_autoguider.SetLang;
begin
  Title.Caption:=rsAutoguider;
  BtnConnect.Caption:=rsConnect;
  BtnCal.Caption:=rsCalibrate;
  BtnGuide.Caption:=rsGuide;
  BtnDither.Caption:=rsDither;
  Status.Hint:=rsAutoguiderSt;
  ShowStat.Hint:=rsShowGuidingS;
  GuideChart.Hint:=Format(rsGuidingHisto,[crlf,crlf,crlf]);
end;

procedure Tf_autoguider.SetAutoguiderType(value:TAutoguiderType);
begin
  FAutoguiderType:=value;
  panel3.Visible:=(FAutoguiderType<>agDITHER)and(FAutoguiderType<>agNONE)and(FAutoguiderType<>agINTERNAL);
  panel4.Visible:=(FAutoguiderType<>agNONE);
  BtnGuide.Visible:=(FAutoguiderType<>agDITHER);
  ShowStat.Visible:=(FAutoguiderType=agPHD);
  LabelStat.Visible:=(FAutoguiderType=agPHD)or(FAutoguiderType=agINTERNAL);
end;

procedure Tf_autoguider.BtnConnectClick(Sender: TObject);
begin
   if Assigned(FonConnect) then FonConnect(self);
end;

procedure Tf_autoguider.BtnDitherClick(Sender: TObject);
begin
   if Assigned(FonDither) then FonDither(self);
end;

procedure Tf_autoguider.BtnGuideClick(Sender: TObject);
begin
   if Assigned(FonGuide) then FonGuide(self);
end;

procedure Tf_autoguider.GuideChartDblClick(Sender: TObject);
begin
  if Assigned(FonClearStat) then FonClearStat(self);
end;

procedure Tf_autoguider.ShowStatChange(Sender: TObject);
begin
  Panel5.Visible:=ShowStat.Checked;
  if ShowStat.Checked and Assigned(FonShowStat) then FonShowStat(self);
end;

procedure Tf_autoguider.BtnCalClick(Sender: TObject);
begin
   if Assigned(FonCalibrate) then FonCalibrate(self);
end;

end.

