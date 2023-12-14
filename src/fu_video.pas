unit fu_video;

{$mode objfpc}{$H+}

{
Copyright (C) 2016 Patrick Chevalley

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

uses u_global, u_utils, UScaleDPI, cu_camera, cu_wheel, indiapi, pu_indigui, math, u_translation,
  Classes, SysUtils, LazFileUtils, Spin, Forms, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls;

type

  { Tf_video }

  Tf_video = class(TFrame)
    BtnStartRec: TButton;
    BtnStopRec: TButton;
    BtnOptions: TButton;
    LabelRecording: TLabel;
    VideoEncoder: TComboBox;
    Exprange: TComboBox;
    Label10: TLabel;
    Label9: TLabel;
    PanelEncoder: TPanel;
    VideoCaptureDir: TEdit;
    StreamExp: TFloatSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ObjectName: TEdit;
    FrameRate: TComboBox;
    FPSlabel: TLabel;
    Label3: TLabel;
    Exposure: TTrackBar;
    Gain: TTrackBar;
    Gamma: TTrackBar;
    Brightness: TTrackBar;
    PanelExposure2: TPanel;
    PanelMore: TPanel;
    PanelRecord: TPanel;
    PanelOptions: TPanel;
    PanelBrightness: TPanel;
    PanelGamma: TPanel;
    PanelGain: TPanel;
    PanelPreview: TPanel;
    PanelExposure1: TPanel;
    VideoSize: TComboBox;
    Duration: TCheckBox;
    Frames: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    RecDuration: TComboBox;
    RecFrames: TComboBox;
    Preview: TCheckBox;
    Panel1: TPanel;
    Title: TLabel;
    procedure BrightnessKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure BrightnessMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnOptionsClick(Sender: TObject);
    procedure BtnStartRecClick(Sender: TObject);
    procedure BtnStopRecClick(Sender: TObject);
    procedure DurationClick(Sender: TObject);
    procedure ExposureKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExposureMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExprangeChange(Sender: TObject);
    procedure FrameRateChange(Sender: TObject);
    procedure FramesClick(Sender: TObject);
    procedure GainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GammaKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GammaMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewChange(Sender: TObject);
    procedure StreamExpChange(Sender: TObject);
    procedure VideoEncoderChange(Sender: TObject);
    procedure VideoSizeChange(Sender: TObject);
  private
    { private declarations }
    FCamera: T_camera;
    Fwheel: T_wheel;
    FVideoindigui:Tf_indigui;
    FVideoGUIready: boolean;
    Frunning: boolean;
    FonMsg: TNotifyMsg;
    Ffps: double;
    Ifps: integer;
    FCheckReady: TNotifyBool;
    procedure GUIdestroy(Sender: TObject);
    procedure SetFps(value:double);
    procedure SetRecordFile;
    procedure BrightnessChange;
    procedure GainChange;
    procedure GammaChange;
  public
    { public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure SetLang;
    procedure SetImageControls;
    procedure ShowExposure(value:double);
    procedure Stop;
    property camera: T_camera read FCamera write FCamera;
    property wheel: T_wheel read Fwheel write Fwheel;
    property Running: boolean read Frunning;
    property FPS: double read Ffps write SetFps;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onCheckReady: TNotifyBool read FCheckReady write FCheckReady;
  end;

implementation

{$R *.lfm}

constructor Tf_video.Create(aOwner: TComponent);
begin
 inherited Create(aOwner);
 {$ifdef lclcocoa}
 Title.Color:=clWindowFrame;
 Panel1.ChildSizing.LeftRightSpacing:=8;
 Panel1.ChildSizing.VerticalSpacing:=4;
 {$endif}
 ScaleDPI(Self);
 Frunning:=false;
 FVideoGUIready:=false;
 Ffps:=1;
 LabelRecording.Caption:='';
 SetLang;
end;

destructor  Tf_video.Destroy;
begin
 inherited Destroy;
end;

procedure Tf_video.SetLang;
begin
  Title.Caption:=rsVideo;
  preview.Caption:=rsPreview;
  label10.Caption:=rsEncoder;
  label4.Caption:=rsExposure;
  label8.Caption:=rsExposure;
  label5.Caption:=rsGain;
  label6.Caption:=rsGamma;
  label7.Caption:=rsBrightness;
  BtnOptions.Caption:=rsMoreSettings;
  label2.Caption:=rsSize2;
  label1.Caption:=rsFrameRate;
  Duration.Caption:=rsDuration;
  Frames.Caption:=rsFrames;
  label3.Caption:=rsObject;
  label9.Caption:=rsDirectory;
  BtnStartRec.Caption:=rsStart;
  BtnStopRec.Caption:=rsStop;
end;

procedure Tf_video.SetImageControls;
var r: TNumRange;
    sr:TONumRange;
begin
 // try video exposure as in V4L2
 r:=camera.VideoExposureRange;
 PanelExposure1.Visible:=(r.max>0);
 if PanelExposure1.Visible then begin
   PanelExposure2.Visible:=false;
   if r.max<=100 then begin
     Exprange.Visible:=false;
     ResetTrackBar(Exposure);
     Exposure.Min:=round(r.min);
     Exposure.Max:=round(r.max);
     Exposure.LineSize:=round(r.step);
     Exposure.PageSize:=round(5*r.step);
   end else begin
     Exprange.Clear;
     sr:=TONumRange.Create;
     sr.range.min:=1; sr.range.max:=9; sr.range.step:=r.step;
     Exprange.Items.AddObject('1-9',sr);
     sr:=TONumRange.Create;
     sr.range.min:=10; sr.range.max:=99; sr.range.step:=r.step;
     Exprange.Items.AddObject('10-99',sr);
     if r.max<1000 then begin
       sr:=TONumRange.Create;
       sr.range.min:=100; sr.range.max:=r.max; sr.range.step:=r.step;
       Exprange.Items.AddObject('100-'+IntToStr(round(r.max)),sr);
     end else begin
       sr:=TONumRange.Create;
       sr.range.min:=100; sr.range.max:=999; sr.range.step:=r.step;
       Exprange.Items.AddObject('100-999',sr);
       if r.max<10000 then begin
         sr:=TONumRange.Create;
         sr.range.min:=1000; sr.range.max:=r.max; sr.range.step:=r.step;
         Exprange.Items.AddObject('1000-'+IntToStr(round(r.max)),sr);
       end else begin
         sr:=TONumRange.Create;
         sr.range.min:=1000; sr.range.max:=10000; sr.range.step:=r.step;
         Exprange.Items.AddObject('1000-10000',sr);
       end;
     end;
   end;
   ShowExposure(camera.VideoExposure);
 end
 else begin
   // try streaming exposure as in DSLR
   r:=camera.StreamingExposureRange;
   PanelExposure2.Visible:=(r.max>0);
   if PanelExposure2.Visible then begin
     PanelExposure1.Visible:=false;
     StreamExp.Hint:=FormatFloat(f3,r.min)+ellipsis+FormatFloat(f3,r.max);
     StreamExp.MinValue:=r.min;
     StreamExp.MaxValue:=r.max;
     StreamExp.Value:=camera.StreamingExposure;
   end
 end;
 PanelEncoder.Visible:=camera.VideoEncoders.Count>0;
 if PanelEncoder.Visible then begin
   VideoEncoder.items.Assign(camera.VideoEncoders);
   if VideoEncoder.Items.Count>0 then
     VideoEncoder.ItemIndex:=camera.VideoEncoder;
 end;
 r:=camera.VideoGainRange;
 PanelGain.Visible:=(r.max>0);
 if PanelGain.Visible then begin
   ResetTrackBar(Gain);
   Gain.Min:=round(r.min);
   Gain.Max:=round(r.max);
   Gain.LineSize:=round(r.step);
   Gain.PageSize:=round(5*r.step);
   Gain.Position:=max(min(round(camera.VideoGain),Gain.Max),Gain.Min);
 end;
 r:=camera.VideoGammaRange;
 PanelGamma.Visible:=(r.max>0);
 if PanelGamma.Visible then begin
   ResetTrackBar(Gamma);
   Gamma.Min:=round(r.min);
   Gamma.Max:=round(r.max);
   Gamma.LineSize:=round(r.step);
   Gamma.PageSize:=round(5*r.step);
   Gamma.Position:=max(min(round(camera.VideoGamma),Gamma.Max),Gamma.Min);
 end;
 r:=camera.VideoBrightnessRange;
 PanelBrightness.Visible:=(r.max>0);
 if PanelBrightness.Visible then begin
   ResetTrackBar(Brightness);
   Brightness.Min:=round(r.min);
   Brightness.Max:=round(r.max);
   Brightness.LineSize:=round(r.step);
   Brightness.PageSize:=round(5*r.step);
   Brightness.Position:=max(min(round(camera.VideoBrightness),Brightness.Max),Brightness.Min);
 end;
 PanelOptions.Visible:=(VideoSize.Items.Count>0)or(FrameRate.Items.Count>0);
end;

procedure Tf_video.ShowExposure(value:double);
var sr:TONumRange;
    val:integer;
begin
val:=round(value);
if (val>0)and PanelExposure1.visible then begin
 if Exprange.Visible then begin
   if (val<10)and(Exprange.Items.Count>0) then begin
      Exprange.ItemIndex:=0;
      sr:=TONumRange(Exprange.Items.Objects[0]);
   end
   else if (val<100)and(Exprange.Items.Count>1) then begin
      Exprange.ItemIndex:=1;
      sr:=TONumRange(Exprange.Items.Objects[1]);
   end
   else if (val<1000)and(Exprange.Items.Count>2) then begin
      Exprange.ItemIndex:=2;
      sr:=TONumRange(Exprange.Items.Objects[2]);
   end
   else if (val<=10000)and(Exprange.Items.Count>3) then begin
      Exprange.ItemIndex:=3;
      sr:=TONumRange(Exprange.Items.Objects[3]);
   end
   else exit;
   ResetTrackBar(Exposure);
   Exposure.Min:=round(sr.range.min);
   Exposure.Max:=round(sr.range.max);
   Exposure.LineSize:=round(sr.range.step);
   Exposure.PageSize:=round(5*sr.range.step);
   Exposure.Position:=val;
 end
 else
   Exposure.Position:=val;
end
else if PanelExposure2.visible then begin
  StreamExp.Value:=value;
end;
end;

procedure Tf_video.ExprangeChange(Sender: TObject);
var sr:TONumRange;
    exp:integer;
begin
if PanelExposure1.visible then begin
  exp:=camera.VideoExposure;
  sr:=TONumRange(Exprange.Items.Objects[Exprange.ItemIndex]);
  if exp>sr.range.max then
     camera.VideoExposure:=round(sr.range.max)
  else if exp<sr.range.min then
     camera.VideoExposure:=round(sr.range.min)
  else
     ShowExposure(camera.VideoExposure);
end;
end;

procedure Tf_video.PreviewChange(Sender: TObject);
var ok: boolean;
begin
  if assigned(FCheckReady) then begin
    FCheckReady(ok);
    if not ok then begin
      preview.Checked:=false;
      exit;
    end;
  end;
  if FCamera<>nil then begin
   if Preview.Checked then begin
      Ifps:=0;
      Camera.VideoPreviewLimit:=MaxVideoPreviewRate;
      Camera.StartVideoPreview;
      Frunning:=true;
   end
   else begin
      Camera.StopVideoPreview;
      Frunning:=false;
   end;
  end;
end;

procedure Tf_video.SetRecordFile;
var fd,fn: string;
  fnobj,fnfilter: boolean;
begin
  fd:=Trim(VideoCaptureDir.Text);
  config.SetValue('/Files/VideoCapturePath',fd);
  fnobj:=config.GetValue('/Files/FilenameObjname',true);
  fnfilter:=config.GetValue('/Files/FilenameFilter',true);
  fn:='';
  if fnobj then begin
     fn:=trim(ObjectName.Text)
  end;
  if fn='' then
    fn:='video';
  if fnfilter and (wheel.Status=devConnected) then
      fn:=fn+'_'+trim(wheel.CurrentFilterName);
  fn:=fn+'_T_.ser';  // let INDI increment the time
  camera.VideoRecordDir:=fd;
  camera.VideoRecordFile:=fn;
end;

procedure Tf_video.BtnStartRecClick(Sender: TObject);
var ok: boolean;
begin
  if assigned(FCheckReady) then begin
    FCheckReady(ok);
    if not ok then exit;
  end;
  camera.VideoPreviewLimit:=MaxVideoPreviewRate;
  SetRecordFile;
  if Duration.Checked then begin
    camera.VideoRecordDuration:=StrToIntDef(RecDuration.Text,10);
    camera.StartVideoRecord(rmDuration);
  end
  else if Frames.Checked then begin
    camera.VideoRecordFrames:=StrToIntDef(RecFrames.Text,50);
    camera.StartVideoRecord(rmFrame);
  end
  else begin
    camera.StartVideoRecord(rmUnlimited);
  end;
end;

procedure Tf_video.BtnStopRecClick(Sender: TObject);
begin
  camera.StopVideoRecord;
end;

procedure Tf_video.FramesClick(Sender: TObject);
begin
   if Frames.Checked then Duration.Checked:=false;
end;

procedure Tf_video.DurationClick(Sender: TObject);
begin
   if Duration.Checked then Frames.Checked:=false;
end;

procedure Tf_video.StreamExpChange(Sender: TObject);
begin
  camera.StreamingExposure:=StreamExp.Value;
end;

procedure Tf_video.VideoEncoderChange(Sender: TObject);
begin
  camera.VideoEncoder:=VideoEncoder.ItemIndex;
end;

procedure Tf_video.ExposureKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  camera.VideoExposure:=Exposure.Position;
end;

procedure Tf_video.ExposureMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  camera.VideoExposure:=Exposure.Position;
end;

procedure Tf_video.GainChange;
begin
  camera.VideoGain:=Gain.Position;
end;

procedure Tf_video.GainKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  GainChange;
end;

procedure Tf_video.GainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GainChange;
end;

procedure Tf_video.GammaChange;
begin
  camera.VideoGamma:=Gamma.Position;
end;

procedure Tf_video.GammaKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  GammaChange;
end;

procedure Tf_video.GammaMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GammaChange;
end;

procedure Tf_video.BrightnessChange;
begin
  camera.VideoBrightness:=Brightness.Position;
end;

procedure Tf_video.BrightnessKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  BrightnessChange;
end;

procedure Tf_video.BrightnessMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BrightnessChange;
end;

procedure Tf_video.BtnOptionsClick(Sender: TObject);
begin
  if not FVideoGUIready then begin
    FVideoindigui:=Tf_indigui.Create(Application.MainForm);
    ScaleDPI(f_indigui);
    FVideoindigui.onDestroy:=@GUIdestroy;
    FVideoindigui.IndiServer:=config.GetValue('/INDIcamera/Server','');
    FVideoindigui.IndiPort:=config.GetValue('/INDIcamera/ServerPort','');
    FVideoindigui.IndiDevice:=CameraName;
  end;
  try
    FormPos(FVideoindigui,mouse.CursorPos.X,mouse.CursorPos.Y);
    FVideoindigui.Show;
    FVideoGUIready:=true;
  except
    FVideoGUIready:=false;
    if Sender<>nil then BtnOptionsClick(nil);
  end;
end;

procedure Tf_video.GUIdestroy(Sender: TObject);
begin
  FVideoGUIready:=false;
end;

procedure Tf_video.Stop;
// when disconnecting from INDI
begin
  try
    Preview.Checked:=false;
    Frunning:=false;
    if FVideoGUIready then FVideoindigui.Close;
  except
  end;
end;

procedure Tf_video.FrameRateChange(Sender: TObject);
var prw: boolean;
begin
  if FrameRate.Text<>camera.VideoRate then begin
    prw:=Frunning;
    if prw then begin
      camera.StopVideoPreview;
      wait(1)
    end;
    camera.VideoRate:=FrameRate.Text;
    if prw then begin
      camera.StartVideoPreview;
    end;
  end;
end;

procedure Tf_video.VideoSizeChange(Sender: TObject);
var prw: boolean;
begin
  if VideoSize.Text<>camera.VideoSize then begin
    prw:=Frunning;
    if prw then begin
      camera.StopVideoPreview;
      wait(1)
    end;
    camera.VideoSize:=VideoSize.Text;
    if prw then begin
      camera.StartVideoPreview;
    end;
  end;
end;

procedure Tf_video.SetFps(value:double);
var j: integer;
begin
  j:=trunc(10*value);
  if j<>Ifps then begin
    Ifps:=j;
    Ffps:=value;
    FPSlabel.Caption:=FormatFloat(f1,Ffps)+' fps';
  end;
end;

end.

