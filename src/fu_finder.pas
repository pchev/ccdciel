unit fu_finder;

{$mode objfpc}{$H+}

{
Copyright (C) 2023 Patrick Chevalley

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

uses  UScaleDPI, u_global, u_utils, Graphics, Dialogs, u_translation, u_hints, cu_camera, indiapi,
      fu_visu, cu_astrometry, pu_findercalibration, math,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, SpinEx, Buttons;

type

  { Tf_finder }

  Tf_finder = class(TFrame)
    Binning: TSpinEditEx;
    BtnPreviewLoop: TButton;
    Button1: TButton;
    ButtonCalibrate: TButton;
    ButtonMousePosition: TSpeedButton;
    ButtonSetTemp: TButton;
    cbSaveImages: TCheckBox;
    Cooler: TCheckBox;
    Gain: TSpinEditEx;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label21: TLabel;
    LabelInfo: TLabel;
    LabelTemperature: TLabel;
    Offset: TSpinEditEx;
    Panel3: TPanel;
    Panel4: TPanel;
    PanelBinning: TPanel;
    PanelGain: TPanel;
    PanelOffset: TPanel;
    PanelTemperature: TPanel;
    PreviewExp: TFloatSpinEditEx;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    OffsetX: TFloatSpinEditEx;
    Label1: TLabel;
    Label2: TLabel;
    OffsetY: TFloatSpinEditEx;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelVisu: TPanel;
    Temperature: TSpinEditEx;
    Title: TLabel;
    procedure BinningChange(Sender: TObject);
    procedure BtnPreviewLoopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonCalibrateClick(Sender: TObject);
    procedure ButtonMousePositionClick(Sender: TObject);
    procedure ButtonSetTempClick(Sender: TObject);
    procedure CoolerClick(Sender: TObject);
    procedure GainChange(Sender: TObject);
    procedure OffsetChange(Sender: TObject);
    procedure OffsetXChange(Sender: TObject);
    procedure OffsetYChange(Sender: TObject);
  private
    { private declarations }
    FCamera: T_camera;
    FAstrometry: TAstrometry;
    FonShowMessage: TNotifyMsg;
    FonRedraw,FonSetTemperature,FonSetCooler: TNotifyEvent;
    FonConfigureFinder: TNotifyEvent;
    FDrawSettingChange: boolean;
    LoopExp:double;
    LoopBin,LoopGain,LoopOffset: integer;
    procedure msg(txt:string; level: integer);
    procedure ForceRedraw;
    Procedure Redraw(Sender: TObject);
    Procedure ZoomImage(Sender: TObject);
  public
    { public declarations }
    visu: Tf_visu;
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetTitleColor;
    procedure ShowCalibration;
    Procedure StartExposureAsync(Data: PtrInt);
    procedure StartLoop;
    procedure StopLoop;
    function Snapshot(exp: double; fn: string):boolean;
    property Camera: T_camera read FCamera write FCamera;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property DrawSettingChange: boolean read FDrawSettingChange write FDrawSettingChange;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onRedraw: TNotifyEvent read FonRedraw write FonRedraw;
    property onConfigureFinder: TNotifyEvent read FonConfigureFinder write FonConfigureFinder;
    property onSetTemperature: TNotifyEvent read FonSetTemperature write FonSetTemperature;
    property onSetCooler: TNotifyEvent read FonSetCooler write FonSetCooler;
  end;

implementation

{$R *.lfm}

{ Tf_finder }

constructor Tf_finder.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 visu:=Tf_visu.Create(self);
 visu.BtnFlipHorz.Visible:=false;
 visu.BtnFlipVert.Visible:=false;
 visu.BtnClipping.Visible:=false;
 visu.BtnShowImage.Visible:=false;
 visu.BtnPinVisu.Visible:=false;
 visu.Panel6.Align:=alNone;
 visu.Panel6.Parent:=visu.Panel7;
 visu.Panel6.Left:=visu.BtnZoom2.Left+visu.BtnZoom2.Width+2;
 visu.Panel6.Top:=0;
 visu.LabelPos.Visible:=true;
 visu.BtnClipRange.Down:=true;
 visu.panel1.Parent:=PanelVisu;
 visu.onRedraw:=@Redraw;
 visu.onZoom:=@ZoomImage;
 {$ifdef lclcocoa}
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
 FinderPreviewLoop:=false;
 visu.BtnBullsEye.Down:=true;
 LabelInfo.Caption:='';
end;

destructor  Tf_finder.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_finder.SetTitleColor;
begin
  Title.Color:=InterfaceColor[TitleColor,1];
  Title.Font.Color:=InterfaceColor[TitleColor,2];
  Title.Font.Style:=[fsBold];
end;

procedure Tf_finder.SetLang;
begin
  Title.Caption:=rsFinderCamera;
  BtnPreviewLoop.Caption:=rsStartPreview;
  cbSaveImages.Caption:=rsSavePreviewI;
  ButtonMousePosition.Caption:=rsClickOnImage;
  ButtonCalibrate.Caption:=rsCalibrationM;
  groupbox1.Caption:=rsTargetPositi2;
  label1.Caption:='X '+rsPixel;
  label2.Caption:='Y '+rsPixel;
  label3.Caption:=rsExposure;
  label21.Caption:=rsTemperature;
  ButtonSetTemp.Caption:=rsSet;
  Cooler.Caption:=rsCooler;
  Label14.Caption:=rsBinning;
  Label15.Caption:=rsGain;
  Label16.Caption:=rsOffset2;
  if visu<>nil then visu.SetLang;
end;

procedure Tf_finder.msg(txt:string; level: integer);
begin
 if assigned(FonShowMessage) then FonShowMessage(rsFinderCamera+': '+txt,level);
end;

procedure Tf_finder.StartLoop;
begin
  FinderPreviewLoop:=true;
  BtnPreviewLoop.Caption:=rsStopPreviewL;
  msg(rsStartPreview,0);
  Application.QueueAsyncCall(@StartExposureAsync,0);
end;

procedure Tf_finder.StopLoop;
begin
  FinderPreviewLoop:=false;
  FCamera.AbortExposure;
  BtnPreviewLoop.Caption:=rsStartPreview;
  msg(rsStopPreviewL,3);
end;

Procedure Tf_finder.StartExposureAsync(Data: PtrInt);
begin
 if (FCamera.Status=devConnected) then begin
   LoopExp:=max(FCamera.ExposureRange.min,PreviewExp.Value);
   LoopGain:=config.GetValue('/PrecSlew/Gain',NullInt);
   LoopOffset:=config.GetValue('/PrecSlew/Offset',NullInt);
   LoopBin:=config.GetValue('/PrecSlew/Binning',1);
   if (LoopBin<>FCamera.BinX)or(LoopBin<>FCamera.BinY) then FCamera.SetBinning(LoopBin,LoopBin);
   if FCamera.hasGain then begin
     if FCamera.Gain<>LoopGain then begin
       FCamera.Gain:=LoopGain;
     end;
     if FCamera.hasOffset then begin
        if FCamera.Offset<>LoopOffset then
          FCamera.Offset:=LoopOffset;
     end;
   end;
   if FCamera.FrameType<>LIGHT then
     FCamera.FrameType:=LIGHT;
   FCamera.ObjectName:=rsFinderCamera;
   FCamera.StartExposure(LoopExp);
 end
 else begin
    StopLoop;
    if not AllDevicesConnected then msg(rsSomeDefinedD,1);
 end;
end;

procedure Tf_finder.BtnPreviewLoopClick(Sender: TObject);
begin
  if FinderPreviewLoop then begin
    StopLoop;
  end
  else begin
    StartLoop;
  end;
end;

procedure Tf_finder.ButtonCalibrateClick(Sender: TObject);
var exp,ra2000,de2000:double;
    bin,sgain,soffset: integer;
    restartloop:boolean;
begin
if (FCamera.Status=devConnected) then begin
  restartloop:=FinderPreviewLoop;
  if FinderPreviewLoop then begin
    StopLoop;
    wait(1);
  end;
  msg(rsCalibration,3);
  f_findercalibration.Astrometry:=FAstrometry;
  if pseudomodal(f_findercalibration) = mrOK then begin
    ra2000:=f_findercalibration.RA;
    de2000:=f_findercalibration.DE;
    if (ra2000=NullCoord)or(de2000=NullCoord) then begin
      msg(rsInvalidCoord,1);
      exit;
    end;
    exp:=max(FCamera.ExposureRange.min,PreviewExp.Value);
    sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
    soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
    bin:=config.GetValue('/PrecSlew/Binning',1);
    msg(format(rsExposureS,[FormatFloat(f3,exp)])+blank+rsSeconds,3);
    if not Fcamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry,sgain,soffset) then begin
      msg(rsExposureFail,0);
      exit;
    end;
    msg(rsResolve,3);
    FAstrometry.SolveFinderImage;
    if FAstrometry.LastResult and FAstrometry.GetFinderOffset(ra2000,de2000) then begin
      ShowCalibration;
      FAstrometry.FinderBinning:=bin;
      msg(rsNewFinderCal, 3);
      msg(Format(rsMainImageCen, [OffsetX.Text, OffsetY.Text]), 3);
    end
    else begin
      msg('Calibration failed',1);
    end;
  end;
  if restartloop then StartLoop;
end
else msg(rsSomeDefinedD,1);
end;

procedure Tf_finder.ButtonMousePositionClick(Sender: TObject);
begin
ButtonMousePosition.Down:=true;
end;

procedure Tf_finder.ButtonSetTempClick(Sender: TObject);
begin
  if Assigned(FonSetTemperature) then FonSetTemperature(self);
end;

procedure Tf_finder.CoolerClick(Sender: TObject);
begin
  if Assigned(FonSetCooler) then FonSetCooler(self);
end;

procedure Tf_finder.GainChange(Sender: TObject);
begin
  config.SetValue('/PrecSlew/Gain',Gain.Value);
end;

procedure Tf_finder.OffsetChange(Sender: TObject);
begin
  config.SetValue('/PrecSlew/Offset',Offset.Value);
end;

Procedure Tf_finder.Redraw(Sender: TObject);
begin
 FDrawSettingChange:=true;
 if Assigned(FonRedraw) then FonRedraw(self);
end;

Procedure Tf_finder.ZoomImage(Sender: TObject);
begin
 FinderImgZoom:=visu.Zoom;
 if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.ForceRedraw;
begin
  FDrawSettingChange:=true;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.OffsetXChange(Sender: TObject);
begin
  FAstrometry.FinderOffsetX:=OffsetX.Value;
  if visu.BtnBullsEye.Down then ForceRedraw;
end;

procedure Tf_finder.OffsetYChange(Sender: TObject);
begin
  FAstrometry.FinderOffsetY:=OffsetY.Value;
  if visu.BtnBullsEye.Down then ForceRedraw;
end;

procedure Tf_finder.ShowCalibration;
begin
  OffsetX.Value:=FAstrometry.FinderOffsetX;
  OffsetY.Value:=FAstrometry.FinderOffsetY;
end;

procedure Tf_finder.BinningChange(Sender: TObject);
var oldbin: integer;
    scaling: double;
begin
  oldbin:=FAstrometry.FinderBinning;
  scaling:=oldbin/Binning.Value;
  FAstrometry.FinderOffsetX:=FAstrometry.FinderOffsetX*scaling;
  FAstrometry.FinderOffsetY:=FAstrometry.FinderOffsetY*scaling;
  FAstrometry.FinderBinning:=Binning.Value;
  config.SetValue('/Finder/OffsetX',astrometry.FinderOffsetX);
  config.SetValue('/Finder/OffsetY',astrometry.FinderOffsetY);
  config.SetValue('/Finder/Binning',astrometry.FinderBinning);
  config.SetValue('/PrecSlew/Binning',Binning.Value);
  ShowCalibration;
  if FinderPreviewLoop then begin
    StopLoop;
    wait(1);
    StartLoop;
  end;
end;

procedure Tf_finder.Button1Click(Sender: TObject);
begin
  if Assigned(FonConfigureFinder) then FonConfigureFinder(self);
end;

function Tf_finder.Snapshot(exp: double; fn: string): boolean;
var bin,sgain,soffset: integer;
    restartloop:boolean;
begin
try
  if (FCamera.Status=devConnected) then begin
    restartloop:=FinderPreviewLoop;
    if FinderPreviewLoop then begin
      StopLoop;
      wait(1);
    end;
    sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
    soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
    bin:=config.GetValue('/PrecSlew/Binning',1);
    msg(format(rsExposureS,[FormatFloat(f3,exp)])+blank+rsSeconds,3);
    if not Fcamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeCapture,sgain,soffset,true) then begin
      msg(rsExposureFail,0);
      result:=false;
      exit;
    end;
    FCamera.Fits.SaveToFile(fn);
    if restartloop then StartLoop;
    result:=true;
  end
  else begin
    msg(rsSomeDefinedD,1);
    result:=false;
  end;
except
  result:=false;
end;
end;

end.

