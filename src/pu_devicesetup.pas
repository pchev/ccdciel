unit pu_devicesetup;

{$mode objfpc}{$H+}

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

interface

uses indibaseclient, indibasedevice, indiapi, u_global, u_utils, u_ccdconfig, UScaleDPI, u_translation,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

type

  { Tf_setup }

  Tf_setup = class(TForm)
    AscomRotator: TEdit;
    AscomWheel: TEdit;
    AscomFocuser: TEdit;
    AscomMount: TEdit;
    BtnAboutRotator: TButton;
    BtnChooseRotator: TButton;
    BtnNewProfile: TButton;
    BtnDeleteProfile: TButton;
    BtnSetupRotator: TButton;
    BtnCopyProfile: TButton;
    CameraAutoLoadConfig: TCheckBox;
    BtnAboutCamera1: TButton;
    BtnAboutCamera2: TButton;
    BtnAboutCamera3: TButton;
    BtnSetupCamera1: TButton;
    BtnSetupCamera2: TButton;
    BtnSetupCamera3: TButton;
    Button1: TButton;
    BtnChooseCamera: TButton;
    BtnSetupCamera: TButton;
    Button3: TButton;
    BtnChooseFilter: TButton;
    BtnChooseFocuser: TButton;
    BtnChooseMount: TButton;
    BtnAboutCamera: TButton;
    CameraIndiTransfertDir: TEdit;
    MountSetDateTime: TCheckBox;
    MountSetObservatory: TCheckBox;
    DeviceFilterWheel: TCheckBox;
    DeviceFocuser: TCheckBox;
    DeviceWatchdog: TCheckBox;
    DeviceRotator: TCheckBox;
    DeviceMount: TCheckBox;
    WatchdogThreshold: TEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label8: TLabel;
    WatchdogAutoLoadConfig: TCheckBox;
    WatchdogIndiDevice: TComboBox;
    PanelWatchdogIndi: TPanel;
    PanelRotatorAscom: TPanel;
    RotatorAutoLoadConfig: TCheckBox;
    RotatorIndiDevice: TComboBox;
    RotatorIndiDevPort: TEdit;
    Label11: TLabel;
    Label14: TLabel;
    Label5: TLabel;
    CameraDiskPanel: TPanel;
    PanelRotatorIndi: TPanel;
    ProfileList: TComboBox;
    Label2: TLabel;
    MountAutoLoadConfig: TCheckBox;
    FocuserAutoLoadConfig: TCheckBox;
    CameraIndiTransfert: TRadioGroup;
    Rotator: TTabSheet;
    Watchdog: TTabSheet;
    WheelAutoLoadConfig: TCheckBox;
    IndiTimeout: TEdit;
    IndiSensor: TComboBox;
    FilterWheelInCameraBox: TCheckBox;
    FocuserInMountBox: TCheckBox;
    InterfaceSelectionBox: TRadioGroup;
    IndiPort: TEdit;
    IndiServer: TEdit;
    GetIndiDevices: TButton;
    Label1: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    LabelIndiDevCount: TLabel;
    Label22: TLabel;
    PanelIndiServer: TPanel;
    PanelFocuserInMount: TPanel;
    Label10: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label9: TLabel;
    PanelFocuserAscom: TPanel;
    PanelMountAscom: TPanel;
    PanelWheelIncamera: TPanel;
    Focuser: TTabSheet;
    Mount: TTabSheet;
    PanelFocuserIndi: TPanel;
    PanelMountIndi: TPanel;
    DeviceInterface: TTabSheet;
    WheelIndiDevice: TComboBox;
    FocuserIndiDevice: TComboBox;
    MountIndiDevice: TComboBox;
    WheelIndiDevPort: TEdit;
    FocuserIndiDevPort: TEdit;
    MountIndiDevPort: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    PanelWheelAscom: TPanel;
    PanelWheelIndi: TPanel;
    CameraIndiDevPort: TEdit;
    AscomCamera: TEdit;
    CameraIndiDevice: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Pagecontrol1: TPageControl;
    Panel1: TPanel;
    PanelCameraAscom: TPanel;
    PanelCameraIndi: TPanel;
    IndiTimer: TTimer;
    Camera: TTabSheet;
    Filterwheel: TTabSheet;
    procedure BtnAboutAscomClick(Sender: TObject);
    procedure BtnChooseClick(Sender: TObject);
    procedure BtnCopyProfileClick(Sender: TObject);
    procedure BtnDeleteProfileClick(Sender: TObject);
    procedure BtnNewProfileClick(Sender: TObject);
    procedure BtnSetupAscomClick(Sender: TObject);
    procedure CameraIndiTransfertClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IndiSensorChange(Sender: TObject);
    procedure FilterWheelInCameraBoxClick(Sender: TObject);
    procedure FocuserInMountBoxClick(Sender: TObject);
    procedure GetIndiDevicesClick(Sender: TObject);
    procedure InterfaceSelectionBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IndiTimerTimer(Sender: TObject);
    procedure ProfileListChange(Sender: TObject);
  private
    { private declarations }
    indiclient: TIndiBaseClient;
    camsavedev,wheelsavedev,focusersavedev,mountsavedev,rotatorsavedev,watchdogsavedev,FCameraSensor: string;
    FRestartRequired, LockInterfaceChange,InitialLock,ProfileLock: boolean;
    FConnectionInterface,FCameraConnection,FWheelConnection,FFocuserConnection,FMountConnection,FRotatorConnection: TDevInterface;
    IndiTimerCount:integer;
    receiveindidevice:boolean;
    procedure IndiNewDevice(dp: Basedevice);
    procedure SetConnectionInterface(value: TDevInterface);
    procedure SetCameraConnection(value: TDevInterface);
    procedure SetWheelConnection(value: TDevInterface);
    procedure SetFocuserConnection(value: TDevInterface);
    procedure SetRotatorConnection(value: TDevInterface);
    procedure SetMountConnection(value: TDevInterface);
    procedure SetCameraSensor(value: string);
    procedure SetLang;
  public
    { public declarations }
    DefaultCameraInterface, DefaultMountInterface, DefaultWheelInterface, DefaultFocuserInterface, DefaultRotatorInterface: TDevInterface;
    profile: string;
    procedure LoadProfileList;
    procedure Loadconfig(conf: TCCDConfig);
    property CameraSensor: string read FCameraSensor write SetCameraSensor;
    property RestartRequired: boolean read FRestartRequired;
    property ConnectionInterface: TDevInterface read FConnectionInterface write SetConnectionInterface;
    property CameraConnection: TDevInterface read FCameraConnection write SetCameraConnection;
    property WheelConnection: TDevInterface read FWheelConnection write SetWheelConnection;
    property FocuserConnection: TDevInterface read FFocuserConnection write SetFocuserConnection;
    property RotatorConnection: TDevInterface read FRotatorConnection write SetRotatorConnection;
    property MountConnection: TDevInterface read FMountConnection write SetMountConnection;
  end;

var
  f_setup: Tf_setup;

implementation

uses LazFileUtils;

{$R *.lfm}

{ Tf_setup }


procedure Tf_setup.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self);
  SetLang;
  FRestartRequired:=false;
  LockInterfaceChange:=false;
  InitialLock:=true;
  Pagecontrol1.ActivePage:=DeviceInterface;
end;

procedure Tf_setup.SetLang;
begin
  DeviceInterface.Caption:=rsInterface;
  GetIndiDevices.Caption:=rsConnectAndGe;
  Label18.Caption:=rsServer;
  Label22.Caption:=rsPort;
  Label1.Caption:=rsTimeout;
  Label2.Caption:=format(rsProfile,['']);
  BtnNewProfile.Caption:=rsNew;
  BtnDeleteProfile.Caption:=rsDelete;
  BtnCopyProfile.Caption:=rsCopy;
  Camera.Caption:=rsCamera;
  BtnChooseCamera.Caption:=rsChoose;
  BtnSetupCamera.Caption:=rsSetup;
  BtnAboutCamera.Caption:=rsAbout;
  Label3.Caption:=rsDevices;
  Label4.Caption:=rsPort;
  Label15.Caption:=rsSensor;
  IndiSensor.Items[0]:=rsMainSensor;
  IndiSensor.Items[1]:=rsGuiderSensor;
  CameraAutoLoadConfig.Caption:=rsLoadConfigur;
  CameraIndiTransfert.Caption:=rsImageTransfe;
  CameraIndiTransfert.Items[0]:=rsNetwork;
  CameraIndiTransfert.Items[1]:=rsRAMDisk;
  Label5.Caption:=rsDirectory;
  Filterwheel.Caption:=rsFilterWheel;
  BtnChooseFilter.Caption:=rsChoose;
  BtnAboutCamera1.Caption:=rsAbout;
  BtnSetupCamera1.Caption:=rsSetup;
  FilterWheelInCameraBox.Caption:=rsFilterWheelI;
  Label17.Caption:=rsBeSureToConf;
  Label6.Caption:=rsDevices;
  Label7.Caption:=rsPort;
  WheelAutoLoadConfig.Caption:=rsLoadConfigur;
  DeviceFilterWheel.Caption:=rsUseFilterWhe;
  Focuser.Caption:=rsFocuser;
  Label16.Caption:=rsNotImplement;
  BtnChooseFocuser.Caption:=rsChoose;
  BtnAboutCamera2.Caption:=rsAbout;
  BtnSetupCamera2.Caption:=rsSetup;
  FocuserInMountBox.Caption:=rsFocuserInMou;
  Label9.Caption:=rsDevices;
  Label10.Caption:=rsPort;
  FocuserAutoLoadConfig.Caption:=rsLoadConfigur;
  DeviceFocuser.Caption:=rsUseFocuser;
  Rotator.Caption:=rsRotator;
  DeviceRotator.Caption:=rsUseRotator;
  BtnChooseRotator.Caption:=rsChoose;
  BtnAboutRotator.Caption:=rsAbout;
  BtnSetupRotator.Caption:=rsSetup;
  Label11.Caption:=rsDevices;
  Label14.Caption:=rsPort;
  RotatorAutoLoadConfig.Caption:=rsLoadConfigur;
  Mount.Caption:=rsMount;
  BtnChooseMount.Caption:=rsChoose;
  BtnAboutCamera3.Caption:=rsAbout;
  BtnSetupCamera3.Caption:=rsSetup;
  Label12.Caption:=rsDevices;
  Label13.Caption:=rsPort;
  MountAutoLoadConfig.Caption:=rsLoadConfigur;
  DeviceMount.Caption:=rsUseMount;
  MountSetDateTime.Caption:=rsSetMountTime;
  MountSetObservatory.Caption:=rsSetMountSite;
  Watchdog.Caption:=rsWatchdog;
  Label19.Caption:=rsDevices;
  WatchdogAutoLoadConfig.Caption:=rsLoadConfigur;
  Label20.Caption:=rsHeartBeatThr;
  DeviceWatchdog.Caption:=rsUseWatchdog;
end;

procedure Tf_setup.LoadProfileList;
var fs : TSearchRec;
    i,j,n: integer;
    buf:string;
begin
ProfileLock:=true;
try
ProfileList.Clear;
ProfileList.Items.Add('default');
i:=FindFirstUTF8(slash(ConfigDir)+'ccdciel_*.conf',0,fs);
while i=0 do begin
  buf:=ExtractFileNameOnly(fs.Name);
  delete(buf,1,8);
  j:=ProfileList.Items.Add(buf);
  i:=FindNextUTF8(fs);
end;
FindCloseUTF8(fs);
n:=ProfileList.Items.IndexOf(profile);
if n>=0 then ProfileList.ItemIndex:=n
        else ProfileList.ItemIndex:=0;
finally
  ProfileLock:=false;
end;
end;

procedure Tf_setup.Loadconfig(conf: TCCDConfig);
begin
ConnectionInterface:=TDevInterface(conf.GetValue('/Interface',ord(DefaultCameraInterface)));
IndiServer.Text:=conf.GetValue('/INDI/Server','localhost');
IndiPort.Text:=conf.GetValue('/INDI/ServerPort','7624');
IndiTimeout.Text:=conf.GetValue('/Devices/Timeout','100');

DeviceFilterWheel.Checked:=conf.GetValue('/Devices/FilterWheel',false);
DeviceFocuser.Checked:=conf.GetValue('/Devices/Focuser',false);
DeviceRotator.Checked:=conf.GetValue('/Devices/Rotator',false);
DeviceMount.Checked:=conf.GetValue('/Devices/Mount',false);
DeviceWatchdog.Checked:=conf.GetValue('/Devices/Watchdog',false);

CameraConnection:=TDevInterface(conf.GetValue('/CameraInterface',ord(DefaultCameraInterface)));
if CameraIndiDevice.Items.Count=0 then begin
  CameraIndiDevice.Items.Add(conf.GetValue('/INDIcamera/Device',''));
  CameraIndiDevice.ItemIndex:=0;
end;
CameraIndiDevice.Text:=conf.GetValue('/INDIcamera/Device','');
CameraSensor:=conf.GetValue('/INDIcamera/Sensor','CCD1');
CameraIndiDevPort.Text:=conf.GetValue('/INDIcamera/DevicePort','');
CameraAutoLoadConfig.Checked:=conf.GetValue('/INDIcamera/AutoLoadConfig',false);
CameraIndiTransfert.ItemIndex:=conf.GetValue('/INDIcamera/IndiTransfert',ord(itNetwork));
CameraIndiTransfertDir.Text:=conf.GetValue('/INDIcamera/IndiTransfertDir','/tmp');
AscomCamera.Text:=conf.GetValue('/ASCOMcamera/Device','');
CameraDiskPanel.Visible:=CameraIndiTransfert.ItemIndex>0;

WheelConnection:=TDevInterface(conf.GetValue('/FilterWheelInterface',ord(DefaultWheelInterface)));
if WheelIndiDevice.Items.Count=0 then begin
  WheelIndiDevice.Items.Add(conf.GetValue('/INDIwheel/Device',''));
  WheelIndiDevice.ItemIndex:=0;
end;
WheelIndiDevice.Text:=conf.GetValue('/INDIwheel/Device','');
WheelIndiDevPort.Text:=conf.GetValue('/INDIwheel/DevicePort','');
WheelAutoLoadConfig.Checked:=conf.GetValue('/INDIwheel/AutoLoadConfig',false);
AscomWheel.Text:=conf.GetValue('/ASCOMwheel/Device','');

FocuserConnection:=TDevInterface(conf.GetValue('/FocuserInterface',ord(DefaultFocuserInterface)));
if FocuserIndiDevice.Items.Count=0 then begin
  FocuserIndiDevice.Items.Add(conf.GetValue('/INDIfocuser/Device',''));
  FocuserIndiDevice.ItemIndex:=0;
end;
FocuserIndiDevice.Text:=conf.GetValue('/INDIfocuser/Device','');
FocuserIndiDevPort.Text:=conf.GetValue('/INDIfocuser/DevicePort','');
FocuserAutoLoadConfig.Checked:=conf.GetValue('/INDIfocuser/AutoLoadConfig',false);
AscomFocuser.Text:=conf.GetValue('/ASCOMfocuser/Device','');

RotatorConnection:=TDevInterface(conf.GetValue('/RotatorInterface',ord(DefaultRotatorInterface)));
if RotatorIndiDevice.Items.Count=0 then begin
  RotatorIndiDevice.Items.Add(conf.GetValue('/INDIrotator/Device',''));
  RotatorIndiDevice.ItemIndex:=0;
end;
RotatorIndiDevice.Text:=conf.GetValue('/INDIrotator/Device','');
RotatorIndiDevPort.Text:=conf.GetValue('/INDIrotator/DevicePort','');
RotatorAutoLoadConfig.Checked:=conf.GetValue('/INDIrotator/AutoLoadConfig',false);
AscomRotator.Text:=conf.GetValue('/ASCOMrotator/Device','');

MountConnection:=TDevInterface(conf.GetValue('/MountInterface',ord(DefaultMountInterface)));
if MountIndiDevice.Items.Count=0 then begin
  MountIndiDevice.Items.Add(conf.GetValue('/INDImount/Device',''));
  MountIndiDevice.ItemIndex:=0;
end;
MountIndiDevice.Text:=conf.GetValue('/INDImount/Device','');
MountIndiDevPort.Text:=conf.GetValue('/INDImount/DevicePort','');
MountAutoLoadConfig.Checked:=conf.GetValue('/INDImount/AutoLoadConfig',false);
AscomMount.Text:=conf.GetValue('/ASCOMmount/Device','');
MountSetDateTime.Checked:=conf.GetValue('/Mount/SetDateTime',false);
MountSetObservatory.Checked:=conf.GetValue('/Mount/SetObservatory',false);

if WatchdogIndiDevice.Items.Count=0 then begin
  WatchdogIndiDevice.Items.Add(conf.GetValue('/INDIwatchdog/Device',''));
  WatchdogIndiDevice.ItemIndex:=0;
end;
WatchdogIndiDevice.Text:=conf.GetValue('/INDIwatchdog/Device','');
WatchdogThreshold.Text:=conf.GetValue('/INDIwatchdog/Threshold','10');
WatchdogAutoLoadConfig.Checked:=conf.GetValue('/INDIwatchdog/AutoLoadConfig',false);

end;

procedure Tf_setup.InterfaceSelectionBoxClick(Sender: TObject);
begin
  if LockInterfaceChange then begin
    LockInterfaceChange:=false;
    exit;
  end;
  if InterfaceSelectionBox.ItemIndex=0 then begin
     if (sender<>nil)and(not InitialLock) then FRestartRequired:=true;
     FConnectionInterface:=INDI;
     PanelIndiServer.Visible:=true;
     FCameraConnection:=INDI;
     PanelCameraIndi.Visible:=true;
     PanelCameraAscom.Visible:=false;
     FMountConnection:=INDI;
     PanelMountIndi.Visible:=true;
     PanelMountAscom.Visible:=false;
     FRotatorConnection:=INDI;
     PanelRotatorIndi.Visible:=true;
     PanelRotatorAscom.Visible:=false;
     // INDI internal filter use same driver as camera
     FilterWheelInCameraBox.Visible:=true;
     if (not FilterWheelInCameraBox.Checked) then begin
        FWheelConnection:=INDI;
        PanelWheelIndi.Visible:=true;
        PanelWheelAscom.Visible:=false;
        PanelWheelIncamera.Visible:=false;
     end;
     FocuserInMountBox.Visible:=true;
     if (not FocuserInMountBox.Checked) then begin
        FFocuserConnection:=INDI;
        PanelFocuserIndi.Visible:=true;
        PanelFocuserAscom.Visible:=false;
        PanelFocuserInMount.Visible:=false;
     end;
     Watchdog.TabVisible:=true;
 end;
{$ifdef mswindows}
  if InterfaceSelectionBox.ItemIndex=1 then begin
     if (sender<>nil)and(not InitialLock) then FRestartRequired:=true;
     FConnectionInterface:=ASCOM;
     PanelIndiServer.Visible:=false;
     FCameraConnection:=ASCOM;
     PanelCameraIndi.Visible:=false;
     PanelCameraAscom.Visible:=true;
     FMountConnection:=ASCOM;
     PanelMountIndi.Visible:=false;
     PanelMountAscom.Visible:=true;
     FRotatorConnection:=ASCOM;
     PanelRotatorIndi.Visible:=false;
     PanelRotatorAscom.Visible:=true;
     FilterWheelInCameraBox.Visible:=true;
     if (not FilterWheelInCameraBox.Checked) then begin
        FWheelConnection:=ASCOM;
        PanelWheelIndi.Visible:=false;
        PanelWheelAscom.Visible:=true;
        PanelWheelIncamera.Visible:=false;
     end;
     FocuserInMountBox.Checked:=false;
     FocuserInMountBox.Visible:=false;
     FFocuserConnection:=ASCOM;
     PanelFocuserIndi.Visible:=false;
     PanelFocuserAscom.Visible:=true;
     PanelFocuserInMount.Visible:=false;
     DeviceWatchdog.Checked:=false;
     Watchdog.TabVisible:=false;
  end;
{$else}
 if InterfaceSelectionBox.ItemIndex=1 then begin
    LockInterfaceChange:=true;
    InterfaceSelectionBox.ItemIndex:=0;
    ShowMessage(rsASCOMInterfa);
 end;
{$endif}
end;

procedure Tf_setup.SetConnectionInterface(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FConnectionInterface:=value;
  case FConnectionInterface of
    INDI: InterfaceSelectionBox.ItemIndex:=0;
    ASCOM: InterfaceSelectionBox.ItemIndex:=1;
  end;
  InterfaceSelectionBoxClick(nil);
 end;

procedure Tf_setup.SetCameraConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FCameraConnection:=value;
end;

procedure Tf_setup.SetRotatorConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FRotatorConnection:=value;
end;

procedure Tf_setup.SetWheelConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FWheelConnection:=value;
  FilterWheelInCameraBox.Checked:=(FWheelConnection = INCAMERA);
  if FWheelConnection=INDI then begin
    PanelWheelIndi.Visible:=true;
    PanelWheelAscom.Visible:=false;
    PanelWheelIncamera.Visible:=false;
  end
  else if FWheelConnection=ASCOM then begin
    PanelWheelIndi.Visible:=false;
    PanelWheelAscom.Visible:=true;
    PanelWheelIncamera.Visible:=false;
  end
  else if FWheelConnection=INCAMERA then begin
    PanelWheelIndi.Visible:=false;
    PanelWheelAscom.Visible:=false;
    PanelWheelIncamera.Visible:=true;
  end;
end;

procedure Tf_setup.SetFocuserConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FFocuserConnection:=value;
  FocuserInMountBox.Checked:=(FFocuserConnection = INTELESCOPE);
  if FFocuserConnection=INDI then begin
    PanelFocuserIndi.Visible:=true;
    PanelFocuserAscom.Visible:=false;
    PanelFocuserInMount.Visible:=false;
  end
  else if FFocuserConnection=ASCOM then begin
    PanelFocuserIndi.Visible:=false;
    PanelFocuserAscom.Visible:=true;
    PanelFocuserInMount.Visible:=false;
  end
  else if FFocuserConnection=INTELESCOPE then begin
    PanelFocuserIndi.Visible:=false;
    PanelFocuserAscom.Visible:=false;
    PanelFocuserInMount.Visible:=true;
  end;
end;

procedure Tf_setup.SetMountConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FMountConnection:=value;
end;

procedure Tf_setup.FilterWheelInCameraBoxClick(Sender: TObject);
begin
  if (sender<>nil)and(not InitialLock) then FRestartRequired:=true;
  if FilterWheelInCameraBox.Checked then begin
    SetWheelConnection(INCAMERA);
  end else begin
    SetWheelConnection(FConnectionInterface);
  end;
end;

procedure Tf_setup.FocuserInMountBoxClick(Sender: TObject);
begin
  if (sender<>nil)and(not InitialLock) then FRestartRequired:=true;
  if FocuserInMountBox.Checked then begin
    SetFocuserConnection(INTELESCOPE);
  end else begin
    SetFocuserConnection(FConnectionInterface);
  end;
end;

procedure Tf_setup.BtnChooseClick(Sender: TObject);
{$ifdef mswindows}
var
  V: variant;
  t,dev: WideString;
  err: string;
{$endif}
begin
{$ifdef mswindows}
  case TButton(Sender).Tag of
    1 : begin t:='Camera'; dev:=AscomCamera.Text; end;
    2 : begin t:='FilterWheel'; dev:=AscomWheel.Text; end;
    3 : begin t:='Focuser'; dev:=AscomFocuser.Text; end;
    4 : begin t:='Telescope'; dev:=AscomMount.Text; end;
    5 : begin t:='Rotator'; dev:=AscomRotator.Text; end;
  end;
  try
    try
      V := CreateOleObject('ASCOM.Utilities.Chooser');
    except
      V := CreateOleObject('DriverHelper.Chooser');
    end;
    V.DeviceType:=t;
    dev:=widestring(V.Choose(dev));
    V:=Unassigned;
    case TButton(Sender).Tag of
      1 : AscomCamera.Text:=dev;
      2 : AscomWheel.Text:=dev;
      3 : AscomFocuser.Text:=dev;
      4 : AscomMount.Text:=dev;
      5 : AscomRotator.Text:=dev;
    end;
  except
    on E: Exception do begin
      err:='ASCOM exception:'+E.Message;
      Showmessage(err+crlf+'Please ensure the latest ASCOM drivers are installed.'+crlf+'See: http://ascom-standards.org');
    end;
  end;
{$endif}
end;

procedure Tf_setup.BtnAboutAscomClick(Sender: TObject);
{$ifdef mswindows}
var buf : string;
  V: variant;
  t,dev: WideString;
  err: string;
{$endif}
begin
{$ifdef mswindows}
  case TButton(Sender).Tag of
    1 : begin t:='Camera'; dev:=AscomCamera.Text; end;
    2 : begin t:='FilterWheel'; dev:=AscomWheel.Text; end;
    3 : begin t:='Focuser'; dev:=AscomFocuser.Text; end;
    4 : begin t:='Telescope'; dev:=AscomMount.Text; end;
    5 : begin t:='Rotator'; dev:=AscomRotator.Text; end;
  end;

  try
    V := CreateOleObject(widestring(dev));
    buf:=V.Description;
    buf:=buf+crlf+V.DriverInfo;
    V:=Unassigned;
    ShowMessage(buf);
  except
    on E: EOleException do ShowMessage('Error : ' + E.Message);
  end;
{$endif}
end;

procedure Tf_setup.BtnSetupAscomClick(Sender: TObject);
{$ifdef mswindows}
var
  V: variant;
  t,dev: WideString;
  err: string;
{$endif}
begin
{$ifdef mswindows}
  case TButton(Sender).Tag of
    1 : begin t:='Camera'; dev:=AscomCamera.Text; end;
    2 : begin t:='FilterWheel'; dev:=AscomWheel.Text; end;
    3 : begin t:='Focuser'; dev:=AscomFocuser.Text; end;
    4 : begin t:='Telescope'; dev:=AscomMount.Text; end;
    5 : begin t:='Rotator'; dev:=AscomRotator.Text; end;
  end;

  try
    V := CreateOleObject(widestring(dev));
    V.SetupDialog;
    V:=Unassigned;
  except
    on E: EOleException do ShowMessage('Error : ' + E.Message);
  end;
{$endif}
end;

procedure Tf_setup.CameraIndiTransfertClick(Sender: TObject);
begin
  CameraDiskPanel.Visible:=CameraIndiTransfert.ItemIndex>0;
end;

procedure Tf_setup.FormShow(Sender: TObject);
begin
  InitialLock:=false;
end;

procedure Tf_setup.IndiSensorChange(Sender: TObject);
begin
case IndiSensor.ItemIndex of
  0: FCameraSensor:='CCD1';
  1: FCameraSensor:='CCD2';
end;
end;

procedure Tf_setup.SetCameraSensor(value: string);
begin
 FCameraSensor:=value;
 if FCameraSensor='CCD2' then begin
   IndiSensor.ItemIndex:=1;
 end else begin
   IndiSensor.ItemIndex:=0;
 end;
end;

procedure Tf_setup.GetIndiDevicesClick(Sender: TObject);
begin
  if IndiTimer.Enabled then exit;
  IndiTimerCount:=0;
  receiveindidevice:=false;
  camsavedev:=CameraIndiDevice.Text;
  wheelsavedev:=WheelIndiDevice.Text;
  focusersavedev:=FocuserIndiDevice.Text;
  rotatorsavedev:=RotatorIndiDevice.Text;
  mountsavedev:=MountIndiDevice.Text;
  watchdogsavedev:=WatchdogIndiDevice.Text;
  LabelIndiDevCount.Caption:='...';
  CameraIndiDevice.Clear;
  FocuserIndiDevice.Clear;
  RotatorIndiDevice.Clear;
  WheelIndiDevice.Clear;
  MountIndiDevice.Clear;
  WatchdogIndiDevice.Clear;
  indiclient:=TIndiBaseClient.Create;
  indiclient.onNewDevice:=@IndiNewDevice;
  indiclient.SetServer(IndiServer.Text,IndiPort.Text);
  indiclient.ConnectServer;
  IndiTimer.Enabled:=true;
  Screen.Cursor:=crHourGlass;
end;

procedure Tf_setup.IndiTimerTimer(Sender: TObject);
var i: integer;
    drint:word;
begin
  inc(IndiTimerCount);
  if (not receiveindidevice)and(IndiTimerCount<=5) then exit;
  IndiTimer.Enabled:=false;
  try
  if indiclient.Terminated then begin
    Screen.Cursor:=crDefault;
    ShowMessage('No response from INDI server. Is the Indi server running?');
    exit;
  end;
  for i:=0 to indiclient.devices.Count-1 do begin
     drint:=BaseDevice(indiclient.devices[i]).getDriverInterface();
     if (drint and CCD_INTERFACE)<>0 then
        CameraIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (drint and FILTER_INTERFACE)<>0 then
        WheelIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (drint and FOCUSER_INTERFACE)<>0 then
        FocuserIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (drint and ROTATOR_INTERFACE)<>0 then
        RotatorIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (drint and TELESCOPE_INTERFACE)<>0 then
        MountIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (drint and AUX_INTERFACE)<>0 then
        WatchdogIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
  end;
  if CameraIndiDevice.Items.Count>0 then CameraIndiDevice.ItemIndex:=0;
  if WheelIndiDevice.Items.Count>0 then WheelIndiDevice.ItemIndex:=0;
  if FocuserIndiDevice.Items.Count>0 then FocuserIndiDevice.ItemIndex:=0;
  if RotatorIndiDevice.Items.Count>0 then RotatorIndiDevice.ItemIndex:=0;
  if MountIndiDevice.Items.Count>0 then MountIndiDevice.ItemIndex:=0;
  if WatchdogIndiDevice.Items.Count>0 then WatchdogIndiDevice.ItemIndex:=0;
  for i:=0 to CameraIndiDevice.Items.Count-1 do
     if CameraIndiDevice.Items[i]=camsavedev then CameraIndiDevice.ItemIndex:=i;
  for i:=0 to WheelIndiDevice.Items.Count-1 do
     if WheelIndiDevice.Items[i]=wheelsavedev then WheelIndiDevice.ItemIndex:=i;
  for i:=0 to FocuserIndiDevice.Items.Count-1 do
     if FocuserIndiDevice.Items[i]=focusersavedev then FocuserIndiDevice.ItemIndex:=i;
  for i:=0 to RotatorIndiDevice.Items.Count-1 do
     if RotatorIndiDevice.Items[i]=rotatorsavedev then RotatorIndiDevice.ItemIndex:=i;
  for i:=0 to MountIndiDevice.Items.Count-1 do
     if MountIndiDevice.Items[i]=mountsavedev then MountIndiDevice.ItemIndex:=i;
  for i:=0 to WatchdogIndiDevice.Items.Count-1 do
     if WatchdogIndiDevice.Items[i]=watchdogsavedev then WatchdogIndiDevice.ItemIndex:=i;
  LabelIndiDevCount.Caption:=Format(rsFoundDevices, [IntToStr(indiclient.devices.Count)]);
  indiclient.DisconnectServer;
  Screen.Cursor:=crDefault;
  except
  end;
end;

procedure Tf_setup.IndiNewDevice(dp: Basedevice);
begin
   receiveindidevice:=true
end;

procedure Tf_setup.ProfileListChange(Sender: TObject);
var chkconf:TCCDconfig;
    configfile:string;
begin
  if ProfileLock then exit;
  if profile<>ProfileList.Text then  begin
    profile:=ProfileList.Text;
    if profile='default' then
       configfile:='ccdciel.conf'
    else
       configfile:='ccdciel_'+profile+'.conf';
    chkconf:=TCCDConfig.Create(nil);
    try
    chkconf.Filename:=slash(ConfigDir)+configfile;
    Loadconfig(chkconf);
    finally
      chkconf.Free;
    end;
  end;
end;

procedure Tf_setup.BtnCopyProfileClick(Sender: TObject);
var newp,curconfig,newconfig:string;
  n:integer;
begin
  newp:=FormEntry(self, rsCopyCurrentP, '');
  newp:=trim(newp);
  if (newp<>'')and(newp<>'default') then begin
    if profile='default' then
       curconfig:='ccdciel.conf'
    else
       curconfig:='ccdciel_'+profile+'.conf';
    curconfig:=slash(ConfigDir)+curconfig;
    newconfig:=slash(ConfigDir)+'ccdciel_'+newp+'.conf';
    if CopyFile(curconfig,newconfig) then begin
       LoadProfileList;
       n:=ProfileList.Items.IndexOf(newp);
       ProfileList.ItemIndex:=n;
       ProfileListChange(Sender);
    end
    else ShowMessage('Error creating file '+newconfig);
  end;
end;

procedure Tf_setup.BtnNewProfileClick(Sender: TObject);
var newp,curconfig,newconfig:string;
  n:integer;
  f:textfile;
begin
  newp:=FormEntry(self, rsCreateNewEmp, '');
  newp:=trim(newp);
  if (newp<>'')and(newp<>'default') then begin
    newconfig:=slash(ConfigDir)+'ccdciel_'+newp+'.conf';
    AssignFile(f,newconfig);
    Rewrite(f);
    writeln(f,'<?xml version="1.0" encoding="utf-8"?>');
    writeln(f,'<CONFIG>');
    writeln(f,'</CONFIG>');
    CloseFile(f);
    LoadProfileList;
    n:=ProfileList.Items.IndexOf(newp);
    ProfileList.ItemIndex:=n;
    ProfileListChange(Sender);
  end;
end;

procedure Tf_setup.BtnDeleteProfileClick(Sender: TObject);
var fn: string;
    n: integer;
begin
  if profile='default' then exit;
  fn:='ccdciel_'+profile+'.conf';
  fn:=slash(ConfigDir)+fn;
  if MessageDlgPos(Format(rsDoYouWantToD, [fn]), mtConfirmation, mbYesNo, 0,
    mouse.CursorPos.x, mouse.CursorPos.y)=mrYes then begin
     DeleteFileUTF8(fn);
     LoadProfileList;
     n:=ProfileList.Items.IndexOf('default');
     ProfileList.ItemIndex:=n;
     ProfileListChange(Sender);
  end;
end;


end.

