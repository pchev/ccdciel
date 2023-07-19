unit pu_sensoranalysis;

{$mode ObjFPC}{$H+}

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

uses u_translation, u_utils, u_global, cu_fits, cu_camera, indiapi, UScaleDPI, math,
  LazSysUtils, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin;

type

  { Tf_sensoranalysis }

  Tf_sensoranalysis = class(TForm)
    ButtonDark: TButton;
    ButtonClose1: TButton;
    ButtonClose2: TButton;
    ButtonLight: TButton;
    ButtonNext2: TButton;
    ButtonNext1: TButton;
    Exposure: TFloatSpinEdit;
    Instruction: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMaxAdu: TLabel;
    LabelFullwellcapacity: TLabel;
    LabelTemperature: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Gain: TSpinEdit;
    Offset: TSpinEdit;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDarkClick(Sender: TObject);
    procedure ButtonLightClick(Sender: TObject);
    procedure ButtonNext1Click(Sender: TObject);
    procedure ButtonNext2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFits: TFits;
    Fcamera: T_camera;
    FonShowMessage: TNotifyMsg;
    procedure msg(txt:string; level: integer=3);
    procedure SetROI;
    function TakeLight: boolean;
    function TakeDark: boolean;
    procedure CameraInfo;
  public
    property Fits: TFits read FFits write FFits;
    property Camera: T_camera read Fcamera write Fcamera;
    property onShowMessage: TNotifyMsg read FonShowMessage write FonShowMessage;
  end;

var
  f_sensoranalysis: Tf_sensoranalysis;

implementation

{$R *.lfm}

{ Tf_sensoranalysis }

procedure Tf_sensoranalysis.msg(txt:string; level: integer=3);
begin
 if assigned(FonShowMessage) then FonShowMessage('Sensor analysis: '+txt,level);
end;

procedure Tf_sensoranalysis.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
end;

procedure Tf_sensoranalysis.FormShow(Sender: TObject);
begin
  Exposure.MaxValue:=camera.ExposureRange.max;
  Exposure.MinValue:=camera.ExposureRange.min;
  Exposure.Increment:=camera.ExposureRange.step;
  gain.Visible:=camera.hasGain;
  gain.MaxValue:=camera.GainMax;
  gain.MinValue:=camera.GainMin;
  offset.Visible:=camera.hasGain;
  offset.MaxValue:=camera.OffsetMax;
  offset.MinValue:=camera.OffsetMin;
  PageControl1.ActivePageIndex:=0;
  ButtonNext2Click(nil);
end;

procedure Tf_sensoranalysis.ButtonCloseClick(Sender: TObject);
begin
  close;
end;

procedure Tf_sensoranalysis.CameraInfo;
begin
  LabelTemperature.Caption:=FormatFloat(f2,camera.Temperature);
  LabelFullwellcapacity.Caption:=FormatFloat(f0,camera.FullWellCapacity);
  LabelMaxAdu.Caption:=FormatFloat(f0,camera.MaxADU);
end;

procedure Tf_sensoranalysis.SetROI;
var w,h,sx,sy: integer;
begin
  w:=100;
  h:=100;
  sx:=round(camera.MaxX-w) div 2;
  sy:=round(camera.MaxY-h) div 2;
  camera.SetFrame(sx,sy,w,h);
end;

function Tf_sensoranalysis.TakeLight: boolean;
var exp:double;
    bin,pgain,poffset: integer;
begin
SetROI;
fits.SetBPM(bpm,0,0,0,0);
fits.DarkOn:=false;
exp:=Exposure.Value;
bin:=1;
pgain:=Gain.Value;
poffset:=Offset.Value;
msg('Light exposure='+FormatFloat(f3,exp)+' binning='+inttostr(bin));
if not camera.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeCapture,pgain,poffset) then begin
    msg(rsExposureFail,1);
    result:=false;
    exit;
end;
result:=true;
end;

function Tf_sensoranalysis.TakeDark: boolean;
var exp:double;
    bin,pgain,poffset: integer;
begin
SetROI;
fits.SetBPM(bpm,0,0,0,0);
fits.DarkOn:=false;
exp:=Exposure.Value;
bin:=1;
pgain:=Gain.Value;
poffset:=Offset.Value;
msg('Dark exposure='+FormatFloat(f3,exp)+' binning='+inttostr(bin));
if not camera.ControlExposure(exp,bin,bin,DARK,ReadoutModeCapture,pgain,poffset) then begin
    msg(rsExposureFail,1);
    result:=false;
    exit
end;
result:=true;
end;

procedure Tf_sensoranalysis.ButtonNext1Click(Sender: TObject);
begin
  CameraInfo;
  Instruction.Clear;
  Instruction.Lines.Add('instruction 2');
  Instruction.Lines.Add('Click Take Light button');
  PageControl1.ActivePageIndex:=1;
end;

procedure Tf_sensoranalysis.ButtonNext2Click(Sender: TObject);
begin
  CameraInfo;
  Instruction.Clear;
  Instruction.Lines.Add('instruction 1');
  Instruction.Lines.Add('Click Take Dark button');
  PageControl1.ActivePageIndex:=0;
end;

procedure Tf_sensoranalysis.ButtonLightClick(Sender: TObject);
begin
  Exposure.Value:=1;
  if TakeLight then
    Instruction.Text:=FFits.GetStatistics;
  CameraInfo;
end;

procedure Tf_sensoranalysis.ButtonDarkClick(Sender: TObject);
begin
  Exposure.Value:=1;
  if TakeDark then
    Instruction.Text:=FFits.GetStatistics;
  CameraInfo;
end;

end.

