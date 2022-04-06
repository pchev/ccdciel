unit pu_collimation;

{$mode objfpc}{$H+}
{
Copyright (C) 2020 Patrick Chevalley

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

uses  u_translation, u_global, UScaleDPI,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls;

type

  { Tf_collimation }

  Tf_collimation = class(TForm)
    BtnApplySplit: TButton;
    BtnApplyInspection: TButton;
    BtnStart: TButton;
    BtnStartSplit: TButton;
    BtnStartInspection: TButton;
    BtnStop: TButton;
    BtnCenter: TButton;
    BtnStopSplit: TButton;
    BtnStopInspection: TButton;
    CircleNum: TSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelErrmsg: TLabel;
    Label8: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelTriangle: TPanel;
    RadioGroupInspectionMode: TRadioGroup;
    TabSheetSingle: TTabSheet;
    TabSheet9panel: TTabSheet;
    TabSheetInspection: TTabSheet;
    TrackBarZoom: TTrackBar;
    TrackBarMargin: TTrackBar;
    TriangleAngle: TFloatSpinEdit;
    procedure BtnApplyInspectionClick(Sender: TObject);
    procedure BtnApplySplitClick(Sender: TObject);
    procedure BtnCenterClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStartInspectionClick(Sender: TObject);
    procedure BtnStartSplitClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnStopInspectionClick(Sender: TObject);
    procedure BtnStopSplitClick(Sender: TObject);
    procedure CircleNumChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupInspectionModeClick(Sender: TObject);
    procedure TrackBarMarginChange(Sender: TObject);
    procedure TrackBarZoomChange(Sender: TObject);
    procedure TriangleAngleChange(Sender: TObject);
  private
    FonStart,FonStop,FonCircleChange,FonCenterStar: TNotifyEvent;
    FonStartSplit,FonStopSplit,FonApplySplit: TNotifyEvent;
    FonStartInspection,FonStopInspection,FonApplyInspection: TNotifyEvent;
  public
    procedure SetLang;
    property onStart: TNotifyEvent read FonStart write FonStart;
    property onStop: TNotifyEvent read FonStop write FonStop;
    property onCenterStar: TNotifyEvent read FonCenterStar write FonCenterStar;
    property onCircleChange: TNotifyEvent read FonCircleChange write FonCircleChange;
    property onStartSplit: TNotifyEvent read FonStartSplit write FonStartSplit;
    property onStopSplit: TNotifyEvent read FonStopSplit write FonStopSplit;
    property onApplySplit: TNotifyEvent read FonApplySplit write FonApplySplit;
    property onStartInspection: TNotifyEvent read FonStartInspection write FonStartInspection;
    property onStopInspection: TNotifyEvent read FonStopInspection write FonStopInspection;
    property onApplyInspection: TNotifyEvent read FonApplyInspection write FonApplyInspection;
  end;

var
  f_collimation: Tf_collimation;

implementation

{$R *.lfm}

{ Tf_collimation }

procedure Tf_collimation.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
end;

procedure Tf_collimation.SetLang;
begin
  Caption:=rsInspectionAn;
  Label8.Caption:=rsInspectTheRe;
  TabSheetSingle.Caption:=rsSingleStar;
  Label1.Caption:=rsNumberOfCirc;
  Label2.Caption:=rsForReflector+crlf+Format(StringReplace(rsCenterABrigh, ' %s ', '%s', [rfReplaceAll]), [crlf, crlf]);
  BtnCenter.Caption:=rsCenter;
  BtnStart.Caption:=rsStart;
  BtnStop.Caption:=rsStop;
  TabSheet9panel.Caption:=rs9PanelImage;
  label5.Caption:=Format(StringReplace(rsSplitTheImag,' %s ','%s',[rfReplaceAll]),[crlf]);
  label3.Caption:=rsZoom;
  label4.Caption:=rsMarginOffset;
  BtnStartSplit.Caption:=rsStart;
  BtnStopSplit.Caption:=rsStop;
  BtnApplySplit.Caption:=rsSet;
  TrackBarZoom.Hint:='1'+ellipsis+'5 x';
  TabSheetInspection.Caption:=rsImageInspect;
  BtnStartInspection.Caption:=rsStart;
  BtnStopInspection.Caption:=rsStop;
  BtnApplyInspection.Caption:=rsInspect;
  RadioGroupInspectionMode.Caption:=rsMode;
  RadioGroupInspectionMode.Items[0]:=rsOctagon;
  RadioGroupInspectionMode.Items[1]:=rsTriangle;
  Label6.Caption:=rsTriangleRota;
end;

procedure Tf_collimation.BtnStartClick(Sender: TObject);
begin
  LabelErrmsg.Caption:='';
  if Assigned(FonStart) then FonStart(self);
end;

procedure Tf_collimation.BtnCenterClick(Sender: TObject);
begin
  LabelErrmsg.Caption:='';
  if Assigned(FonCenterStar) then FonCenterStar(self);
end;

procedure Tf_collimation.BtnStopClick(Sender: TObject);
begin
  LabelErrmsg.Caption:='';
  if Assigned(FonStop) then FonStop(self);
end;

procedure Tf_collimation.CircleNumChange(Sender: TObject);
begin
  if Assigned(FonCircleChange) then FonCircleChange(self);
end;

procedure Tf_collimation.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(FonStop) then FonStop(self);
  if Assigned(FonStopSplit) then FonStopSplit(self);
  if Assigned(FonStopInspection) then FonStopInspection(self);
end;

procedure Tf_collimation.BtnStartSplitClick(Sender: TObject);
begin
  if Assigned(FonStartSplit) then FonStartSplit(self);
end;

procedure Tf_collimation.BtnStopSplitClick(Sender: TObject);
begin
  if Assigned(FonStopSplit) then FonStopSplit(self);
end;

procedure Tf_collimation.BtnApplySplitClick(Sender: TObject);
begin
  if Assigned(FonApplySplit) then FonApplySplit(self);
end;

procedure Tf_collimation.TrackBarMarginChange(Sender: TObject);
begin
  SplitMargin:=TrackBarMargin.Position;
end;

procedure Tf_collimation.TrackBarZoomChange(Sender: TObject);
begin
  SplitZoom:=TrackBarZoom.Position/10;
end;

procedure Tf_collimation.RadioGroupInspectionModeClick(Sender: TObject);
begin
  TriangleInspection:=(RadioGroupInspectionMode.ItemIndex=1);
  PanelTriangle.Visible:=TriangleInspection;
end;

procedure Tf_collimation.TriangleAngleChange(Sender: TObject);
begin
  TriangleInspectionAngle:=TriangleAngle.Value;
end;

procedure Tf_collimation.BtnStartInspectionClick(Sender: TObject);
begin
  if Assigned(FonStartInspection) then FonStartInspection(self);
end;

procedure Tf_collimation.BtnStopInspectionClick(Sender: TObject);
begin
  if Assigned(FonStopInspection) then FonStopInspection(self);
end;

procedure Tf_collimation.BtnApplyInspectionClick(Sender: TObject);
begin
  if Assigned(FonApplyInspection) then FonApplyInspection(self);
end;

end.

