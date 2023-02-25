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

uses  UScaleDPI, u_global, Graphics, Dialogs, u_translation, cu_camera, cu_astrometry, pu_findercalibration,
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons;

type

  { Tf_finder }

  Tf_finder = class(TFrame)
    BtnZoom05: TSpeedButton;
    BtnZoom1: TSpeedButton;
    BtnZoom2: TSpeedButton;
    BtnZoomAdjust: TSpeedButton;
    Button1: TButton;
    ButtonCalibrate: TButton;
    GroupBox1: TGroupBox;
    OffsetX: TFloatSpinEdit;
    Gamma: TTrackBar;
    Label1: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Luminosity: TTrackBar;
    OffsetY: TFloatSpinEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel8: TPanel;
    Title: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure BtnZoom05Click(Sender: TObject);
    procedure BtnZoom1Click(Sender: TObject);
    procedure BtnZoom2Click(Sender: TObject);
    procedure BtnZoomAdjustClick(Sender: TObject);
    procedure ButtonCalibrateClick(Sender: TObject);
    procedure GammaChange(Sender: TObject);
    procedure LuminosityChange(Sender: TObject);
  private
    { private declarations }
    FCamera: T_camera;
    FAstrometry: TAstrometry;
    FonShowMessage: TNotifyMsg;
    FonRedraw: TNotifyEvent;
    procedure msg(txt:string; level: integer);
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure ShowCalibration;
    property Camera: T_camera read FCamera write FCamera;
    property Astrometry: TAstrometry read FAstrometry write FAstrometry;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
    property onRedraw: TNotifyEvent read FonRedraw write FonRedraw;
  end;

implementation

{$R *.lfm}

{ Tf_finder }

constructor Tf_finder.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 SetLang;
end;

destructor  Tf_finder.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_finder.SetLang;
begin
  Title.Caption:=rsFinderCamera;
  button1.Caption:=rsPreview;
  ButtonCalibrate.Caption:=rsCalibrate;
  groupbox1.Caption:=rsCalibrationR;
  label1.Caption:='X '+rsPixel;
  label2.Caption:='Y '+rsPixel;
  label17.Caption:=rsGamma;
  label18.Caption:=rsLuminosity;
  label19.Caption:=rsZoom;
end;

procedure Tf_finder.msg(txt:string; level: integer);
begin
 if assigned(FonShowMessage) then FonShowMessage(txt,level);
end;

procedure Tf_finder.Button1Click(Sender: TObject);
var exp:double;
    bin,sgain,soffset: integer;
begin
  exp:=config.GetValue('/PrecSlew/Exposure',10.0);
  sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
  soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
  bin:=config.GetValue('/PrecSlew/Binning',1);
  if Fcamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry,sgain,soffset) then begin
    msg(rsFinderCamera+': '+rsEndPreview,0);
  end
  else begin
    msg(rsFinderCamera+': '+rsExposureFail,0);
  end;
end;

procedure Tf_finder.ButtonCalibrateClick(Sender: TObject);
var exp,ra2000,de2000:double;
    bin,sgain,soffset: integer;
begin
  if f_findercalibration.ShowModal = mrOK then begin
    ra2000:=f_findercalibration.RA;
    de2000:=f_findercalibration.DE;
    exp:=config.GetValue('/PrecSlew/Exposure',10.0);
    sgain:=config.GetValue('/PrecSlew/Gain',NullInt);
    soffset:=config.GetValue('/PrecSlew/Offset',NullInt);
    bin:=config.GetValue('/PrecSlew/Binning',1);
    if not Fcamera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeAstrometry,sgain,soffset) then begin
      msg(rsExposureFail,0);
      exit;
    end;
    FAstrometry.SolveFinderImage;
    if FAstrometry.LastResult and FAstrometry.GetFinderOffset(ra2000,de2000) then begin
      ShowCalibration;
      msg(rsNewFinderCal, 3);
      msg(Format(rsMainImageCen, [OffsetX.Text, OffsetY.Text]), 3);
    end
    else begin
      msg('Calibration failed',1);
    end;

  end;
end;

procedure Tf_finder.GammaChange(Sender: TObject);
begin
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.LuminosityChange(Sender: TObject);
begin
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.ShowCalibration;
begin
  OffsetX.Value:=FAstrometry.FinderOffsetX;
  OffsetY.Value:=FAstrometry.FinderOffsetY;
end;

procedure Tf_finder.BtnZoomAdjustClick(Sender: TObject);
begin
  FinderImgZoom:=0;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.BtnZoom05Click(Sender: TObject);
begin
  FinderImgZoom:=0.5;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.BtnZoom1Click(Sender: TObject);
begin
  FinderImgZoom:=1;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

procedure Tf_finder.BtnZoom2Click(Sender: TObject);
begin
  FinderImgZoom:=2;
  if Assigned(FonRedraw) then FonRedraw(self);
end;

end.

