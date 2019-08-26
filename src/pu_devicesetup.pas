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

uses indibaseclient, indibasedevice, indiapi, u_global, u_utils, u_ccdconfig, UScaleDPI, u_translation, u_hints,
  {$ifdef mswindows}
    Variants, comobj, math,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Spin, Types;

type

  { Tf_setup }

  Tf_setup = class(TForm)
    ButtonHelp: TButton;
    CameraARestPass: TEdit;
    DefaultARestPass: TEdit;
    DefaultARestUser: TEdit;
    Label107: TLabel;
    Label108: TLabel;
    WheelARestPass: TEdit;
    FocuserARestPass: TEdit;
    RotatorARestPass: TEdit;
    MountARestPass: TEdit;
    DomeARestPass: TEdit;
    WeatherARestPass: TEdit;
    SafetyARestPass: TEdit;
    CameraARestUser: TEdit;
    WheelARestUser: TEdit;
    FocuserARestUser: TEdit;
    RotatorARestUser: TEdit;
    MountARestUser: TEdit;
    DomeARestUser: TEdit;
    WeatherARestUser: TEdit;
    SafetyARestUser: TEdit;
    AscomDome: TEdit;
    AscomWeather: TEdit;
    AscomSafety: TEdit;
    AscomRotator: TEdit;
    AscomRestWeatherType: TRadioGroup;
    AscomWheel: TEdit;
    AscomFocuser: TEdit;
    AscomMount: TEdit;
    BtnAboutDome: TButton;
    BtnAboutWeather: TButton;
    BtnAboutSafety: TButton;
    BtnAboutRotator: TButton;
    BtnChooseDome: TButton;
    BtnChooseWeather: TButton;
    BtnChooseSafety: TButton;
    BtnChooseRotator: TButton;
    BtnNewProfile: TButton;
    BtnDeleteProfile: TButton;
    BtnSetupDome: TButton;
    BtnSetupWeather: TButton;
    BtnSetupSafety: TButton;
    BtnSetupRotator: TButton;
    BtnCopyProfile: TButton;
    ApplyAscomRemote: TButton;
    ApplyIndi: TButton;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label91: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    WatchdogMsg: TLabel;
    WeatherMsg: TLabel;
    SafetyMsg: TLabel;
    DomeMsg: TLabel;
    MountMsg: TLabel;
    RotatorMsg: TLabel;
    FocuserMsg: TLabel;
    WheelMsg: TLabel;
    GetIndi: TButton;
    GetIndi1: TButton;
    GetIndi2: TButton;
    GetIndi3: TButton;
    GetIndi4: TButton;
    GetIndi5: TButton;
    GetIndi6: TButton;
    GetIndi7: TButton;
    GetIndi8: TButton;
    CameraMsg: TLabel;
    WatchdogIndiPort: TEdit;
    WatchdogIndiServer: TEdit;
    Label89: TLabel;
    Label90: TLabel;
    Panel19: TPanel;
    SafetyIndiPort: TEdit;
    SafetyIndiServer: TEdit;
    Label87: TLabel;
    Label88: TLabel;
    Panel18: TPanel;
    WeatherIndiPort: TEdit;
    WeatherIndiServer: TEdit;
    DomeIndiPort: TEdit;
    DomeIndiServer: TEdit;
    Label83: TLabel;
    Label84: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    MountIndiPort: TEdit;
    MountIndiServer: TEdit;
    Label81: TLabel;
    Label82: TLabel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    RotatorIndiPort: TEdit;
    RotatorIndiServer: TEdit;
    FocuserIndiPort: TEdit;
    FocuserIndiServer: TEdit;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    Panel13: TPanel;
    Panel14: TPanel;
    WheelIndiPort: TEdit;
    WheelIndiServer: TEdit;
    DefaultARestHost: TEdit;
    DefaultARestPort: TSpinEdit;
    DefaultARestProtocol: TComboBox;
    CameraIndiPort: TEdit;
    CameraIndiServer: TEdit;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    WheelARestDevice: TSpinEdit;
    FocuserARestDevice: TSpinEdit;
    RotatorARestDevice: TSpinEdit;
    MountARestDevice: TSpinEdit;
    DomeARestDevice: TSpinEdit;
    WeatherARestDevice: TSpinEdit;
    SafetyARestDevice: TSpinEdit;
    WheelARestHost: TEdit;
    FocuserARestHost: TEdit;
    RotatorARestHost: TEdit;
    MountARestHost: TEdit;
    DomeARestHost: TEdit;
    WeatherARestHost: TEdit;
    SafetyARestHost: TEdit;
    WheelARestPort: TSpinEdit;
    FocuserARestPort: TSpinEdit;
    RotatorARestPort: TSpinEdit;
    MountARestPort: TSpinEdit;
    DomeARestPort: TSpinEdit;
    WeatherARestPort: TSpinEdit;
    SafetyARestPort: TSpinEdit;
    WheelARestProtocol: TComboBox;
    FocuserARestProtocol: TComboBox;
    RotatorARestProtocol: TComboBox;
    MountARestProtocol: TComboBox;
    DomeARestProtocol: TComboBox;
    WeatherARestProtocol: TComboBox;
    SafetyARestProtocol: TComboBox;
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
    CameraARestProtocol: TComboBox;
    DeviceDome: TCheckBox;
    DeviceWeather: TCheckBox;
    DeviceSafety: TCheckBox;
    CameraARestHost: TEdit;
    FlipImage: TCheckBox;
    AscomWeatherType: TRadioGroup;
    Dome: TTabSheet;
    FlipImage1: TCheckBox;
    Label23: TLabel;
    Label25: TLabel;
    DomeIndiDevPort: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    DeviceCamera: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    PageControlWatchdog: TPageControl;
    PageControlSafety: TPageControl;
    PageControlWeather: TPageControl;
    PageControlDome: TPageControl;
    PageControlMount: TPageControl;
    PageControlRotator: TPageControl;
    PageControlFocuser: TPageControl;
    PageControlWheel: TPageControl;
    PageControlCamera: TPageControl;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PanelDomeAscom: TPanel;
    PanelDomeIndi: TPanel;
    DomeAutoLoadConfig: TCheckBox;
    DomeIndiDevice: TComboBox;
    CameraIndi: TTabSheet;
    CameraAscom: TTabSheet;
    FocuserIndi: TTabSheet;
    FocuserAscom: TTabSheet;
    FocuserInMount: TTabSheet;
    RotatorIndi: TTabSheet;
    RotatorAscom: TTabSheet;
    MountIndi: TTabSheet;
    MountAscom: TTabSheet;
    DomeIndi: TTabSheet;
    DomeAscom: TTabSheet;
    SafetyIndi: TTabSheet;
    SafetyAscom: TTabSheet;
    CameraAscomRest: TTabSheet;
    CameraARestPort: TSpinEdit;
    CameraARestDevice: TSpinEdit;
    WheelAscomRest: TTabSheet;
    FocuserAscomRest: TTabSheet;
    RotatorAscomRest: TTabSheet;
    MountAscomRest: TTabSheet;
    DomeAscomRest: TTabSheet;
    WeatherAscomRest: TTabSheet;
    SafetyAscomRest: TTabSheet;
    WatchdogIndi: TTabSheet;
    WeatherIndi: TTabSheet;
    WeatherAscom: TTabSheet;
    WheelInCamera: TTabSheet;
    WheelIndi: TTabSheet;
    WheelAscom: TTabSheet;
    WeatherAutoLoadConfig: TCheckBox;
    SafetyAutoLoadConfig: TCheckBox;
    WeatherIndiDevice: TComboBox;
    SafetyIndiDevice: TComboBox;
    Label21: TLabel;
    Label24: TLabel;
    MountSetDateTime: TCheckBox;
    MountSetObservatory: TCheckBox;
    DeviceFilterWheel: TCheckBox;
    DeviceFocuser: TCheckBox;
    DeviceWatchdog: TCheckBox;
    DeviceRotator: TCheckBox;
    DeviceMount: TCheckBox;
    MountGetObservatory: TCheckBox;
    PanelWeatherAscom: TPanel;
    PanelSafetyAscom: TPanel;
    PanelWeatherIndi: TPanel;
    PanelSafetyIndi: TPanel;
    Weather: TTabSheet;
    Safety: TTabSheet;
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
    IndiPort: TEdit;
    IndiServer: TEdit;
    Label1: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
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
    procedure ApplyIndiClick(Sender: TObject);
    procedure BtnAboutAscomClick(Sender: TObject);
    procedure BtnChooseClick(Sender: TObject);
    procedure BtnCopyProfileClick(Sender: TObject);
    procedure BtnDeleteProfileClick(Sender: TObject);
    procedure BtnNewProfileClick(Sender: TObject);
    procedure BtnSetupAscomClick(Sender: TObject);
    procedure ApplyAscomRemoteClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure CameraARestProtocolChange(Sender: TObject);
    procedure CameraIndiTransfertClick(Sender: TObject);
    procedure DefaultARestProtocolChange(Sender: TObject);
    procedure DomeARestProtocolChange(Sender: TObject);
    procedure FocuserARestProtocolChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetIndiClick(Sender: TObject);
    procedure IndiSensorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IndiTimerTimer(Sender: TObject);
    procedure MountARestProtocolChange(Sender: TObject);
    procedure MountGetObservatoryClick(Sender: TObject);
    procedure MountSetObservatoryClick(Sender: TObject);
    procedure PageControlCameraChange(Sender: TObject);
    procedure PageControlDomeChange(Sender: TObject);
    procedure PageControlFocuserChange(Sender: TObject);
    procedure PageControlMountChange(Sender: TObject);
    procedure PageControlRotatorChange(Sender: TObject);
    procedure PageControlSafetyChange(Sender: TObject);
    procedure PageControlWatchdogChange(Sender: TObject);
    procedure PageControlWeatherChange(Sender: TObject);
    procedure PageControlWheelChange(Sender: TObject);
    procedure ProfileListChange(Sender: TObject);
    procedure AscomWeatherTypeClick(Sender: TObject);
    procedure RotatorARestProtocolChange(Sender: TObject);
    procedure SafetyARestProtocolChange(Sender: TObject);
    procedure WeatherARestProtocolChange(Sender: TObject);
    procedure WheelARestProtocolChange(Sender: TObject);
  private
    { private declarations }
    indiclient: TIndiBaseClient;
    camsavedev,wheelsavedev,focusersavedev,mountsavedev,domesavedev,rotatorsavedev,weathersavedev,safetysavedev,watchdogsavedev,FCameraSensor: string;
    LockInterfaceChange,InitialLock,ProfileLock: boolean;
    FCameraConnection,FWheelConnection,FFocuserConnection,FMountConnection,FDomeConnection,FRotatorConnection,FWeatherConnection,FSafetyConnection: TDevInterface;
    IndiTimerCount,GetDeviceType:integer;
    receiveindidevice: boolean;
    FShowHelp: TNotifyEvent;
    procedure GetIndiDevicesStart;
    procedure IndiNewDevice(dp: Basedevice);
    procedure IndiDisconnected(Sender: TObject);
    procedure SetCameraConnection(value: TDevInterface);
    procedure SetWheelConnection(value: TDevInterface);
    procedure SetFocuserConnection(value: TDevInterface);
    procedure SetRotatorConnection(value: TDevInterface);
    procedure SetMountConnection(value: TDevInterface);
    procedure SetDomeConnection(value: TDevInterface);
    procedure SetWeatherConnection(value: TDevInterface);
    procedure SetSafetyConnection(value: TDevInterface);
    procedure SetCameraSensor(value: string);
    procedure SetLang;
  public
    { public declarations }
    DefaultCameraInterface, DefaultMountInterface, DefaultDomeInterface, DefaultWheelInterface, DefaultFocuserInterface, DefaultRotatorInterface, DefaultWeatherInterface, DefaultSafetyInterface: TDevInterface;
    profile: string;
    procedure LoadProfileList;
    procedure Loadconfig(conf,credentialconf: TCCDConfig);
    property CameraSensor: string read FCameraSensor write SetCameraSensor;
    property CameraConnection: TDevInterface read FCameraConnection write SetCameraConnection;
    property WheelConnection: TDevInterface read FWheelConnection write SetWheelConnection;
    property FocuserConnection: TDevInterface read FFocuserConnection write SetFocuserConnection;
    property RotatorConnection: TDevInterface read FRotatorConnection write SetRotatorConnection;
    property MountConnection: TDevInterface read FMountConnection write SetMountConnection;
    property DomeConnection: TDevInterface read FDomeConnection write SetDomeConnection;
    property WeatherConnection: TDevInterface read FWeatherConnection write SetWeatherConnection;
    property SafetyConnection: TDevInterface read FSafetyConnection write SetSafetyConnection;
    property onShowHelp: TNotifyEvent read FShowHelp write FShowHelp;
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
  LockInterfaceChange:=false;
  InitialLock:=true;
  Pagecontrol1.ActivePage:=DeviceInterface;
  {$ifndef mswindows}
  CameraAscom.TabVisible:=false;
  WheelAscom.TabVisible:=false;
  FocuserAscom.TabVisible:=false;
  RotatorAscom.TabVisible:=false;
  MountAscom.TabVisible:=false;
  DomeAscom.TabVisible:=false;
  WeatherAscom.TabVisible:=false;
  SafetyAscom.TabVisible:=false;
  {$endif}
end;

procedure Tf_setup.SetLang;
begin
  DeviceInterface.Caption:=rsInterface;
  Button1.Caption:=rsOK;
  Button3.Caption:=rsCancel;
  ButtonHelp.Caption:=rsHelp;
  Label18.Caption:=rsServer;
  Label22.Caption:=rsPort;
  Label1.Caption:=rsTimeout;
  ApplyIndi.Caption:=rsApplyToAllDe;
  Label70.Caption:=rsProtocol;
  Label71.Caption:=rsServer;
  Label72.Caption:=rsPort;
  Label107.Caption:=rsUserName;
  Label108.Caption:=rsPassword;
  ApplyAscomRemote.Caption:=rsApplyToAllDe;
  Label2.Caption:=format(rsProfile,['']);
  BtnNewProfile.Caption:=rsNew;
  BtnDeleteProfile.Caption:=rsDelete;
  BtnCopyProfile.Caption:=rsCopy;
  Camera.Caption:=rsCamera;
  Label73.Caption:=rsServer;
  Label74.Caption:=rsPort;
  Label30.Caption:=rsProtocol;
  Label31.Caption:=rsServer;
  Label32.Caption:=rsPort;
  Label33.Caption:=rsRemoteDevice;
  GetIndi.Caption:=rsGet;
  Label91.Caption:=rsUserName;
  Label92.Caption:=rsPassword;
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
  Label75.Caption:=rsServer;
  Label76.Caption:=rsPort;
  GetIndi1.Caption:=rsGet;
  Label34.Caption:=rsProtocol;
  Label35.Caption:=rsServer;
  Label36.Caption:=rsPort;
  Label37.Caption:=rsRemoteDevice;
  Label93.Caption:=rsUserName;
  Label94.Caption:=rsPassword;
  BtnChooseFilter.Caption:=rsChoose;
  BtnAboutCamera1.Caption:=rsAbout;
  BtnSetupCamera1.Caption:=rsSetup;
  FlipImage.Caption:=rsFlipTheImageV;
  FlipImage1.Caption:=rsFlipTheImageV;
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
  Label9.Caption:=rsDevices;
  Label10.Caption:=rsPort;
  FocuserAutoLoadConfig.Caption:=rsLoadConfigur;
  DeviceFocuser.Caption:=rsUseFocuser;
  Label77.Caption:=rsServer;
  Label78.Caption:=rsPort;
  GetIndi2.Caption:=rsGet;
  Label38.Caption:=rsProtocol;
  Label39.Caption:=rsServer;
  Label40.Caption:=rsPort;
  Label41.Caption:=rsRemoteDevice;
  Label95.Caption:=rsUserName;
  Label96.Caption:=rsPassword;
  Rotator.Caption:=rsRotator;
  Label79.Caption:=rsServer;
  Label80.Caption:=rsPort;
  GetIndi3.Caption:=rsGet;
  Label42.Caption:=rsProtocol;
  Label43.Caption:=rsServer;
  Label44.Caption:=rsPort;
  Label45.Caption:=rsRemoteDevice;
  Label97.Caption:=rsUserName;
  Label98.Caption:=rsPassword;
  DeviceRotator.Caption:=rsUseRotator;
  BtnChooseRotator.Caption:=rsChoose;
  BtnAboutRotator.Caption:=rsAbout;
  BtnSetupRotator.Caption:=rsSetup;
  Label11.Caption:=rsDevices;
  Label14.Caption:=rsPort;
  RotatorAutoLoadConfig.Caption:=rsLoadConfigur;
  Mount.Caption:=rsMount;
  Label81.Caption:=rsServer;
  Label82.Caption:=rsPort;
  GetIndi4.Caption:=rsGet;
  Label46.Caption:=rsProtocol;
  Label47.Caption:=rsServer;
  Label48.Caption:=rsPort;
  Label49.Caption:=rsRemoteDevice;
  Label99.Caption:=rsUserName;
  Label100.Caption:=rsPassword;
  BtnChooseMount.Caption:=rsChoose;
  BtnAboutCamera3.Caption:=rsAbout;
  BtnSetupCamera3.Caption:=rsSetup;
  Label12.Caption:=rsDevices;
  Label13.Caption:=rsPort;
  MountAutoLoadConfig.Caption:=rsLoadConfigur;
  DeviceMount.Caption:=rsUseMount;
  MountSetDateTime.Caption:=rsSetMountTime;
  MountSetObservatory.Caption:=rsSetMountSite;
  MountGetObservatory.Caption:=rsGetSiteLongL;
  Dome.Caption:=rsDome;
  Label83.Caption:=rsServer;
  Label84.Caption:=rsPort;
  GetIndi5.Caption:=rsGet;
  Label50.Caption:=rsProtocol;
  Label51.Caption:=rsServer;
  Label52.Caption:=rsPort;
  Label53.Caption:=rsRemoteDevice;
  Label101.Caption:=rsUserName;
  Label102.Caption:=rsPassword;
  DeviceDome.Caption:=rsUseDome;
  Label25.Caption:=rsDevices;
  Label23.Caption:=rsPort;
  DomeAutoLoadConfig.Caption:=rsLoadConfigur;
  BtnChooseDome.Caption:=rsChoose;
  BtnAboutDome.Caption:=rsAbout;
  BtnSetupDome.Caption:=rsSetup;
  Weather.Caption:=rsWeatherStati;
  Label85.Caption:=rsServer;
  Label86.Caption:=rsPort;
  GetIndi6.Caption:=rsGet;
  Label54.Caption:=rsProtocol;
  Label55.Caption:=rsServer;
  Label56.Caption:=rsPort;
  Label57.Caption:=rsRemoteDevice;
  Label103.Caption:=rsUserName;
  Label104.Caption:=rsPassword;
  DeviceWeather.Caption:=rsUseWeatherSt;
  Label21.Caption:=rsDevices;
  WeatherAutoLoadConfig.Caption:=rsLoadConfigur;
  BtnChooseWeather.Caption:=rsChoose;
  BtnAboutWeather.Caption:=rsAbout;
  BtnSetupWeather.Caption:=rsSetup;
  AscomWeatherType.Caption:=rsInterfaceTyp;
  AscomWeatherType.Items[0]:=rsObservingCon;
  AscomWeatherType.Items[1]:=rsSafetyMonito;
  Safety.Caption:=rsSafetyMonito;
  DeviceSafety.Caption:=rsUseSafetyMon;
  Label87.Caption:=rsServer;
  Label88.Caption:=rsPort;
  GetIndi7.Caption:=rsGet;
  Label58.Caption:=rsProtocol;
  Label59.Caption:=rsServer;
  Label60.Caption:=rsPort;
  Label61.Caption:=rsRemoteDevice;
  Label105.Caption:=rsUserName;
  Label106.Caption:=rsPassword;
  Label24.Caption:=rsDevices;
  SafetyAutoLoadConfig.Caption:=rsLoadConfigur;
  BtnChooseSafety.Caption:=rsChoose;
  BtnAboutSafety.Caption:=rsAbout;
  BtnSetupSafety.Caption:=rsSetup;
  Watchdog.Caption:=rsWatchdog;
  Label89.Caption:=rsServer;
  Label90.Caption:=rsPort;
  GetIndi8.Caption:=rsGet;
  Label19.Caption:=rsDevices;
  WatchdogAutoLoadConfig.Caption:=rsLoadConfigur;
  Label20.Caption:=rsHeartBeatThr;
  DeviceWatchdog.Caption:=rsUseWatchdog+': '+DevInterfaceName[0];
  CameraIndi.Caption:=DevInterfaceName[0];
  CameraAscom.Caption:=DevInterfaceName[1];
  CameraAscomRest.Caption:=DevInterfaceName[4];
  WheelIndi.Caption:=DevInterfaceName[0];
  WheelAscom.Caption:=DevInterfaceName[1];
  WheelAscomRest.Caption:=DevInterfaceName[4];
  WheelInCamera.Caption:=DevInterfaceName[2];
  FocuserIndi.Caption:=DevInterfaceName[0];
  FocuserAscom.Caption:=DevInterfaceName[1];
  FocuserAscomRest.Caption:=DevInterfaceName[4];
  FocuserInMount.Caption:=DevInterfaceName[3];
  RotatorIndi.Caption:=DevInterfaceName[0];
  RotatorAscom.Caption:=DevInterfaceName[1];
  RotatorAscomRest.Caption:=DevInterfaceName[4];
  MountIndi.Caption:=DevInterfaceName[0];
  MountAscom.Caption:=DevInterfaceName[1];
  MountAscomRest.Caption:=DevInterfaceName[4];
  DomeIndi.Caption:=DevInterfaceName[0];
  DomeAscom.Caption:=DevInterfaceName[1];
  DomeAscomRest.Caption:=DevInterfaceName[4];
  WeatherIndi.Caption:=DevInterfaceName[0];
  WeatherAscom.Caption:=DevInterfaceName[1];
  WeatherAscomRest.Caption:=DevInterfaceName[4];
  SafetyIndi.Caption:=DevInterfaceName[0];
  SafetyAscom.Caption:=DevInterfaceName[1];
  SafetyAscomRest.Caption:=DevInterfaceName[4];
  WatchdogIndi.Caption:=DevInterfaceName[0];
  ProfileList.Hint:=rsListOfProfil;
  BtnNewProfile.Hint:=Format(rsCreateANewEm, [crlf, crlf]);
  BtnDeleteProfile.Hint:=rsDeleteThisPr;
  BtnCopyProfile.Hint:=Format(rsCopyTheCurre, [crlf]);
  PanelIndiServer.Hint:=rsGlobalINDISe;
  Panel10.Hint:=rsGlobalALPACA;
  CameraIndiTransfert.Hint:=Format(rsMakeTestToDe, [crlf]);
  CameraIndiTransfertDir.Hint:=rsTheTemporary;
end;

procedure Tf_setup.LoadProfileList;
var fs : TSearchRec;
    i,n: integer;
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
  ProfileList.Items.Add(buf);
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

procedure Tf_setup.Loadconfig(conf,credentialconf: TCCDConfig);
var defautindiserver, defaultindiport: string;
begin
// default value from old config
defautindiserver:=conf.GetValue('/INDI/Server','localhost');
defaultindiport:=conf.GetValue('/INDI/ServerPort','7624');

IndiTimeout.Text:=conf.GetValue('/Devices/Timeout','100');

DeviceFilterWheel.Checked:=conf.GetValue('/Devices/FilterWheel',false);
DeviceFocuser.Checked:=conf.GetValue('/Devices/Focuser',false);
DeviceRotator.Checked:=conf.GetValue('/Devices/Rotator',false);
DeviceMount.Checked:=conf.GetValue('/Devices/Mount',false);
DeviceDome.Checked:=conf.GetValue('/Devices/Dome',false);
DeviceWeather.Checked:=conf.GetValue('/Devices/Weather',false);
DeviceSafety.Checked:=conf.GetValue('/Devices/Safety',false);
DeviceWatchdog.Checked:=conf.GetValue('/Devices/Watchdog',false);

CameraConnection:=TDevInterface(conf.GetValue('/CameraInterface',ord(DefaultCameraInterface)));
CameraIndiServer.Text:=conf.GetValue('/INDIcamera/Server',defautindiserver);
CameraIndiPort.Text:=conf.GetValue('/INDIcamera/ServerPort',defaultindiport);
IndiServer.Text:=CameraIndiServer.Text;
IndiPort.Text:=CameraIndiPort.Text;
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
FlipImage.Checked:=conf.GetValue('/ASCOMcamera/FlipImage',true);
CameraDiskPanel.Visible:=CameraIndiTransfert.ItemIndex>0;
CameraARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestcamera/Protocol',0);
CameraARestHost.Text:=conf.GetValue('/ASCOMRestcamera/Host','127.0.0.1');
CameraARestPort.Value:=conf.GetValue('/ASCOMRestcamera/Port',11111);
CameraARestDevice.Value:=conf.GetValue('/ASCOMRestcamera/Device',0);
FlipImage1.Checked:=conf.GetValue('/ASCOMRestcamera/FlipImage',true);
DefaultARestProtocol.ItemIndex:=CameraARestProtocol.ItemIndex;
DefaultARestHost.Text:=CameraARestHost.Text;
DefaultARestPort.Value:=CameraARestPort.Value;

WheelConnection:=TDevInterface(conf.GetValue('/FilterWheelInterface',ord(DefaultWheelInterface)));
WheelIndiServer.Text:=conf.GetValue('/INDIwheel/Server',defautindiserver);
WheelIndiPort.Text:=conf.GetValue('/INDIwheel/ServerPort',defaultindiport);
if WheelIndiDevice.Items.Count=0 then begin
  WheelIndiDevice.Items.Add(conf.GetValue('/INDIwheel/Device',''));
  WheelIndiDevice.ItemIndex:=0;
end;
WheelIndiDevice.Text:=conf.GetValue('/INDIwheel/Device','');
WheelIndiDevPort.Text:=conf.GetValue('/INDIwheel/DevicePort','');
WheelAutoLoadConfig.Checked:=conf.GetValue('/INDIwheel/AutoLoadConfig',false);
AscomWheel.Text:=conf.GetValue('/ASCOMwheel/Device','');
WheelARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestwheel/Protocol',0);
WheelARestHost.Text:=conf.GetValue('/ASCOMRestwheel/Host','127.0.0.1');
WheelARestPort.Value:=conf.GetValue('/ASCOMRestwheel/Port',11111);
WheelARestDevice.Value:=conf.GetValue('/ASCOMRestwheel/Device',0);

FocuserConnection:=TDevInterface(conf.GetValue('/FocuserInterface',ord(DefaultFocuserInterface)));
FocuserIndiServer.Text:=conf.GetValue('/INDIfocuser/Server',defautindiserver);
FocuserIndiPort.Text:=conf.GetValue('/INDIfocuser/ServerPort',defaultindiport);
if FocuserIndiDevice.Items.Count=0 then begin
  FocuserIndiDevice.Items.Add(conf.GetValue('/INDIfocuser/Device',''));
  FocuserIndiDevice.ItemIndex:=0;
end;
FocuserIndiDevice.Text:=conf.GetValue('/INDIfocuser/Device','');
FocuserIndiDevPort.Text:=conf.GetValue('/INDIfocuser/DevicePort','');
FocuserAutoLoadConfig.Checked:=conf.GetValue('/INDIfocuser/AutoLoadConfig',false);
AscomFocuser.Text:=conf.GetValue('/ASCOMfocuser/Device','');
FocuserARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestfocuser/Protocol',0);
FocuserARestHost.Text:=conf.GetValue('/ASCOMRestfocuser/Host','127.0.0.1');
FocuserARestPort.Value:=conf.GetValue('/ASCOMRestfocuser/Port',11111);
FocuserARestDevice.Value:=conf.GetValue('/ASCOMRestfocuser/Device',0);

RotatorConnection:=TDevInterface(conf.GetValue('/RotatorInterface',ord(DefaultRotatorInterface)));
RotatorIndiServer.Text:=conf.GetValue('/INDIrotator/Server',defautindiserver);
RotatorIndiPort.Text:=conf.GetValue('/INDIrotator/ServerPort',defaultindiport);
if RotatorIndiDevice.Items.Count=0 then begin
  RotatorIndiDevice.Items.Add(conf.GetValue('/INDIrotator/Device',''));
  RotatorIndiDevice.ItemIndex:=0;
end;
RotatorIndiDevice.Text:=conf.GetValue('/INDIrotator/Device','');
RotatorIndiDevPort.Text:=conf.GetValue('/INDIrotator/DevicePort','');
RotatorAutoLoadConfig.Checked:=conf.GetValue('/INDIrotator/AutoLoadConfig',false);
AscomRotator.Text:=conf.GetValue('/ASCOMrotator/Device','');
RotatorARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestrotator/Protocol',0);
RotatorARestHost.Text:=conf.GetValue('/ASCOMRestrotator/Host','127.0.0.1');
RotatorARestPort.Value:=conf.GetValue('/ASCOMRestrotator/Port',11111);
RotatorARestDevice.Value:=conf.GetValue('/ASCOMRestrotator/Device',0);

MountConnection:=TDevInterface(conf.GetValue('/MountInterface',ord(DefaultMountInterface)));
MountIndiServer.Text:=conf.GetValue('/INDImount/Server',defautindiserver);
MountIndiPort.Text:=conf.GetValue('/INDImount/ServerPort',defaultindiport);
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
MountGetObservatory.Checked:=conf.GetValue('/Mount/GetObservatory',false);
MountARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestmount/Protocol',0);
MountARestHost.Text:=conf.GetValue('/ASCOMRestmount/Host','127.0.0.1');
MountARestPort.Value:=conf.GetValue('/ASCOMRestmount/Port',11111);
MountARestDevice.Value:=conf.GetValue('/ASCOMRestmount/Device',0);

DomeConnection:=TDevInterface(conf.GetValue('/DomeInterface',ord(DefaultDomeInterface)));
DomeIndiServer.Text:=conf.GetValue('/INDIdome/Server',defautindiserver);
DomeIndiPort.Text:=conf.GetValue('/INDIdome/ServerPort',defaultindiport);
if DomeIndiDevice.Items.Count=0 then begin
  DomeIndiDevice.Items.Add(conf.GetValue('/INDIdome/Device',''));
  DomeIndiDevice.ItemIndex:=0;
end;
DomeIndiDevice.Text:=conf.GetValue('/INDIdome/Device','');
DomeIndiDevPort.Text:=conf.GetValue('/INDIdome/DevicePort','');
DomeAutoLoadConfig.Checked:=conf.GetValue('/INDIdome/AutoLoadConfig',false);
AscomDome.Text:=conf.GetValue('/ASCOMdome/Device','');
DomeARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestdome/Protocol',0);
DomeARestHost.Text:=conf.GetValue('/ASCOMRestdome/Host','127.0.0.1');
DomeARestPort.Value:=conf.GetValue('/ASCOMRestdome/Port',11111);
DomeARestDevice.Value:=conf.GetValue('/ASCOMRestdome/Device',0);

WeatherConnection:=TDevInterface(conf.GetValue('/WeatherInterface',ord(DefaultWeatherInterface)));
WeatherIndiServer.Text:=conf.GetValue('/INDIweather/Server',defautindiserver);
WeatherIndiPort.Text:=conf.GetValue('/INDIweather/ServerPort',defaultindiport);
if WeatherIndiDevice.Items.Count=0 then begin
  WeatherIndiDevice.Items.Add(conf.GetValue('/INDIweather/Device',''));
  WeatherIndiDevice.ItemIndex:=0;
end;
WeatherIndiDevice.Text:=conf.GetValue('/INDIweather/Device','');
WeatherAutoLoadConfig.Checked:=conf.GetValue('/INDIweather/AutoLoadConfig',false);
f_setup.AscomWeatherType.ItemIndex:=config.GetValue('/ASCOMweather/DeviceType',0);
AscomWeather.Text:=conf.GetValue('/ASCOMweather/Device','');
WeatherARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestweather/Protocol',0);
WeatherARestHost.Text:=conf.GetValue('/ASCOMRestweather/Host','127.0.0.1');
WeatherARestPort.Value:=conf.GetValue('/ASCOMRestweather/Port',11111);
WeatherARestDevice.Value:=conf.GetValue('/ASCOMRestweather/Device',0);
f_setup.AscomRestWeatherType.ItemIndex:=config.GetValue('/ASCOMRestweather/DeviceType',0);

SafetyConnection:=TDevInterface(conf.GetValue('/SafetyInterface',ord(DefaultSafetyInterface)));
SafetyIndiServer.Text:=conf.GetValue('/INDIsafety/Server',defautindiserver);
SafetyIndiPort.Text:=conf.GetValue('/INDIsafety/ServerPort',defaultindiport);
if SafetyIndiDevice.Items.Count=0 then begin
  SafetyIndiDevice.Items.Add(conf.GetValue('/INDIsafety/Device',''));
  SafetyIndiDevice.ItemIndex:=0;
end;
SafetyIndiDevice.Text:=conf.GetValue('/INDIsafety/Device','');
SafetyAutoLoadConfig.Checked:=conf.GetValue('/INDIsafety/AutoLoadConfig',false);
AscomSafety.Text:=conf.GetValue('/ASCOMsafety/Device','');
SafetyARestProtocol.ItemIndex:=conf.GetValue('/ASCOMRestsafety/Protocol',0);
SafetyARestHost.Text:=conf.GetValue('/ASCOMRestsafety/Host','127.0.0.1');
SafetyARestPort.Value:=conf.GetValue('/ASCOMRestsafety/Port',11111);
SafetyARestDevice.Value:=conf.GetValue('/ASCOMRestsafety/Device',0);

WatchdogIndiServer.Text:=conf.GetValue('/INDIwatchdog/Server',defautindiserver);
WatchdogIndiPort.Text:=conf.GetValue('/INDIwatchdog/ServerPort',defaultindiport);
if WatchdogIndiDevice.Items.Count=0 then begin
  WatchdogIndiDevice.Items.Add(conf.GetValue('/INDIwatchdog/Device',''));
  WatchdogIndiDevice.ItemIndex:=0;
end;
WatchdogIndiDevice.Text:=conf.GetValue('/INDIwatchdog/Device','');
WatchdogThreshold.Text:=conf.GetValue('/INDIwatchdog/Threshold','10');
WatchdogAutoLoadConfig.Checked:=conf.GetValue('/INDIwatchdog/AutoLoadConfig',false);

DeviceFilterWheel.Caption:=rsUseFilterWhe+': '+DevInterfaceName[ord(FWheelConnection)];
DeviceWeather.Caption:=rsUseWeatherSt+': '+DevInterfaceName[ord(FWeatherConnection)];
DeviceSafety.Caption:=rsUseSafetyMon+': '+DevInterfaceName[ord(FSafetyConnection)];
DeviceRotator.Caption:=rsUseRotator+': '+DevInterfaceName[ord(FRotatorConnection)];
DeviceMount.Caption:=rsUseMount+': '+DevInterfaceName[ord(FMountConnection)];
DeviceFocuser.Caption:=rsUseFocuser+': '+DevInterfaceName[ord(FocuserConnection)];
DeviceDome.Caption:=rsUseDome+': '+DevInterfaceName[ord(FocuserConnection)];
DeviceCamera.Caption:=rsCamera+': '+DevInterfaceName[ord(FCameraConnection)];

CameraARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestcamera/User','')), encryptpwd);
WheelARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestwheel/User','')), encryptpwd);
FocuserARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestfocuser/User','')), encryptpwd);
RotatorARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestrotator/User','')), encryptpwd);
MountARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestmount/User','')), encryptpwd);
DomeARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestdome/User','')), encryptpwd);
WeatherARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestweather/User','')), encryptpwd);
SafetyARestUser.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestsafety/User','')), encryptpwd);
CameraARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestcamera/Pass','')), encryptpwd);
WheelARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestwheel/Pass','')), encryptpwd);
FocuserARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestfocuser/Pass','')), encryptpwd);
RotatorARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestrotator/Pass','')), encryptpwd);
MountARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestmount/Pass','')), encryptpwd);
DomeARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestdome/Pass','')), encryptpwd);
WeatherARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestweather/Pass','')), encryptpwd);
SafetyARestPass.Text:=DecryptStr(hextostr(credentialconf.GetValue('/ASCOMRestsafety/Pass','')), encryptpwd);
DefaultARestUser.Text:=CameraARestUser.Text;
DefaultARestPass.Text:=CameraARestPass.Text;

end;


procedure Tf_setup.SetCameraConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FCameraConnection:=value;
  case FCameraConnection of
   INDI: PageControlCamera.ActivePageIndex:=0;
   ASCOM: PageControlCamera.ActivePageIndex:=1;
   ASCOMREST: PageControlCamera.ActivePageIndex:=2;
  end;
end;

procedure Tf_setup.SetRotatorConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FRotatorConnection:=value;
  case FRotatorConnection of
   INDI: PageControlRotator.ActivePageIndex:=0;
   ASCOM: PageControlRotator.ActivePageIndex:=1;
   ASCOMREST: PageControlRotator.ActivePageIndex:=2;
  end;
end;

procedure Tf_setup.SetWeatherConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FWeatherConnection:=value;
  case FWeatherConnection of
   INDI: PageControlWeather.ActivePageIndex:=0;
   ASCOM: PageControlWeather.ActivePageIndex:=1;
   ASCOMREST: PageControlWeather.ActivePageIndex:=2;
  end;
end;

procedure Tf_setup.SetSafetyConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FSafetyConnection:=value;
  case FSafetyConnection of
   INDI: PageControlSafety.ActivePageIndex:=0;
   ASCOM: PageControlSafety.ActivePageIndex:=1;
   ASCOMREST: PageControlSafety.ActivePageIndex:=2;
  end;
end;

procedure Tf_setup.SetWheelConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FWheelConnection:=value;
  case FWheelConnection of
   INDI: PageControlWheel.ActivePageIndex:=0;
   ASCOM: PageControlWheel.ActivePageIndex:=1;
   INCAMERA: PageControlWheel.ActivePageIndex:=2;
   ASCOMREST: PageControlWheel.ActivePageIndex:=3;
  end;
end;

procedure Tf_setup.SetFocuserConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FFocuserConnection:=value;
  case FFocuserConnection of
   INDI: PageControlFocuser.ActivePageIndex:=0;
   ASCOM: PageControlFocuser.ActivePageIndex:=1;
   INTELESCOPE: PageControlFocuser.ActivePageIndex:=2;
   ASCOMREST: PageControlFocuser.ActivePageIndex:=3;
  end;
end;

procedure Tf_setup.SetMountConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FMountConnection:=value;
  case FMountConnection of
   INDI: PageControlMount.ActivePageIndex:=0;
   ASCOM: PageControlMount.ActivePageIndex:=1;
   ASCOMREST: PageControlMount.ActivePageIndex:=2;
  end;
end;

procedure Tf_setup.SetDomeConnection(value: TDevInterface);
begin
{$ifndef mswindows}
  if value=ASCOM then value:=INDI;
{$endif}
  FDomeConnection:=value;
  case FDomeConnection of
   INDI: PageControlDome.ActivePageIndex:=0;
   ASCOM: PageControlDome.ActivePageIndex:=1;
   ASCOMREST: PageControlDome.ActivePageIndex:=2;
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
    1 : begin t:='Camera'; dev:=widestring(AscomCamera.Text); end;
    2 : begin t:='FilterWheel'; dev:=widestring(AscomWheel.Text); end;
    3 : begin t:='Focuser'; dev:=widestring(AscomFocuser.Text); end;
    4 : begin t:='Telescope'; dev:=widestring(AscomMount.Text); end;
    5 : begin t:='Rotator'; dev:=widestring(AscomRotator.Text); end;
    6 : begin
          if AscomWeatherType.ItemIndex=0 then
            t:='ObservingConditions'
          else
            t:='SafetyMonitor';
          dev:=widestring(AscomWeather.Text);
        end;
    7 : begin t:='SafetyMonitor'; dev:=widestring(AscomSafety.Text); end;
    8 : begin t:='Dome'; dev:=widestring(AscomDome.Text); end;
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
      1 : AscomCamera.Text:=string(dev);
      2 : begin AscomWheel.Text:=string(dev); DeviceFilterWheel.Checked:=true; end;
      3 : begin AscomFocuser.Text:=string(dev); DeviceFocuser.Checked:=true; end;
      4 : begin AscomMount.Text:=string(dev); DeviceMount.Checked:=true; end;
      5 : begin AscomRotator.Text:=string(dev); DeviceRotator.Checked:=true; end;
      6 : begin AscomWeather.Text:=string(dev); DeviceWeather.Checked:=true; end;
      7 : begin AscomSafety.Text:=string(dev); DeviceSafety.Checked:=true; end;
      8 : begin AscomDome.Text:=string(dev); DeviceDome.Checked:=true; end;
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
  dev: WideString;
{$endif}
begin
{$ifdef mswindows}
  case TButton(Sender).Tag of
    1 : begin dev:=widestring(AscomCamera.Text); end;
    2 : begin dev:=widestring(AscomWheel.Text); end;
    3 : begin dev:=widestring(AscomFocuser.Text); end;
    4 : begin dev:=widestring(AscomMount.Text); end;
    5 : begin dev:=widestring(AscomRotator.Text); end;
    6 : begin dev:=widestring(AscomWeather.Text); end;
    7 : begin dev:=widestring(AscomSafety.Text); end;
    8 : begin dev:=widestring(AscomDome.Text); end;
  end;

  try
    V := CreateOleObject(string(dev));
    buf:=V.DriverInfo;
    try
    buf:=buf+crlf+V.Description;
    except
      // Description is sometime not available when not connected
    end;
    V:=Unassigned;
    ShowMessage(buf);
  except
    on E: Exception do begin
       buf:=E.Message;
       if pos(AscomInvalidArchitecture,buf)>0 then
          buf:=buf+crlf+rs32bitdriver;
       ShowMessage('Error : ' + buf);
    end;
  end;
{$endif}
end;

procedure Tf_setup.BtnSetupAscomClick(Sender: TObject);
{$ifdef mswindows}
var
  V: variant;
  dev: WideString;
  buf: string;
{$endif}
begin
{$ifdef mswindows}
  case TButton(Sender).Tag of
    1 : begin dev:=widestring(AscomCamera.Text); end;
    2 : begin dev:=widestring(AscomWheel.Text); end;
    3 : begin dev:=widestring(AscomFocuser.Text); end;
    4 : begin dev:=widestring(AscomMount.Text); end;
    5 : begin dev:=widestring(AscomRotator.Text); end;
    6 : begin dev:=widestring(AscomWeather.Text); end;
    7 : begin dev:=widestring(AscomSafety.Text); end;
    8 : begin dev:=widestring(AscomDome.Text); end;
  end;

  try
    V := CreateOleObject(string(dev));
    V.SetupDialog;
    V:=Unassigned;
  except
    on E: Exception do begin
        buf:=E.Message;
        if pos(AscomInvalidArchitecture,buf)>0 then
           buf:=buf+crlf+rs32bitdriver;
        ShowMessage('Error : ' + buf);
    end;
  end;
{$endif}
end;

procedure Tf_setup.ApplyAscomRemoteClick(Sender: TObject);
begin
   CameraARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   WheelARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   FocuserARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   RotatorARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   MountARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   DomeARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   WeatherARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   SafetyARestProtocol.ItemIndex:=DefaultARestProtocol.ItemIndex;
   CameraARestHost.Text:=DefaultARestHost.Text;
   WheelARestHost.Text:=DefaultARestHost.Text;
   FocuserARestHost.Text:=DefaultARestHost.Text;
   RotatorARestHost.Text:=DefaultARestHost.Text;
   MountARestHost.Text:=DefaultARestHost.Text;
   DomeARestHost.Text:=DefaultARestHost.Text;
   WeatherARestHost.Text:=DefaultARestHost.Text;
   SafetyARestHost.Text:=DefaultARestHost.Text;
   CameraARestPort.Value:=DefaultARestPort.Value;
   WheelARestPort.Value:=DefaultARestPort.Value;
   FocuserARestPort.Value:=DefaultARestPort.Value;
   RotatorARestPort.Value:=DefaultARestPort.Value;
   MountARestPort.Value:=DefaultARestPort.Value;
   DomeARestPort.Value:=DefaultARestPort.Value;
   WeatherARestPort.Value:=DefaultARestPort.Value;
   SafetyARestPort.Value:=DefaultARestPort.Value;
   CameraARestUser.Text:=DefaultARestUser.Text;
   WheelARestUser.Text:=DefaultARestUser.Text;
   FocuserARestUser.Text:=DefaultARestUser.Text;
   RotatorARestUser.Text:=DefaultARestUser.Text;
   MountARestUser.Text:=DefaultARestUser.Text;
   DomeARestUser.Text:=DefaultARestUser.Text;
   WeatherARestUser.Text:=DefaultARestUser.Text;
   SafetyARestUser.Text:=DefaultARestUser.Text;
   CameraARestPass.Text:=DefaultARestPass.Text;
   WheelARestPass.Text:=DefaultARestPass.Text;
   FocuserARestPass.Text:=DefaultARestPass.Text;
   RotatorARestPass.Text:=DefaultARestPass.Text;
   MountARestPass.Text:=DefaultARestPass.Text;
   DomeARestPass.Text:=DefaultARestPass.Text;
   WeatherARestPass.Text:=DefaultARestPass.Text;
   SafetyARestPass.Text:=DefaultARestPass.Text;
end;

procedure Tf_setup.ButtonHelpClick(Sender: TObject);
begin
  if Assigned(FShowHelp) then FShowHelp(self);
end;

procedure Tf_setup.CameraARestProtocolChange(Sender: TObject);
begin
  case CameraARestProtocol.ItemIndex of
    0: CameraARestPort.Value:=11111;
    1: CameraARestPort.Value:=443;
  end;
end;

procedure Tf_setup.ApplyIndiClick(Sender: TObject);
begin
  CameraIndiServer.text:=IndiServer.text;
  WheelIndiServer.text:=IndiServer.text;
  FocuserIndiServer.text:=IndiServer.text;
  RotatorIndiServer.text:=IndiServer.text;
  MountIndiServer.text:=IndiServer.text;
  DomeIndiServer.text:=IndiServer.text;
  WeatherIndiServer.text:=IndiServer.text;
  SafetyIndiServer.text:=IndiServer.text;
  WatchdogIndiServer.text:=IndiServer.text;
  CameraIndiPort.text:=IndiPort.text;
  WheelIndiPort.text:=IndiPort.text;
  FocuserIndiPort.text:=IndiPort.text;
  RotatorIndiPort.text:=IndiPort.text;
  MountIndiPort.text:=IndiPort.text;
  DomeIndiPort.text:=IndiPort.text;
  WeatherIndiPort.text:=IndiPort.text;
  SafetyIndiPort.text:=IndiPort.text;
  WatchdogIndiPort.text:=IndiPort.text;
end;

procedure Tf_setup.CameraIndiTransfertClick(Sender: TObject);
begin
  CameraDiskPanel.Visible:=CameraIndiTransfert.ItemIndex>0;
end;

procedure Tf_setup.DefaultARestProtocolChange(Sender: TObject);
begin
  case DefaultARestProtocol.ItemIndex of
    0: DefaultARestPort.Value:=11111;
    1: DefaultARestPort.Value:=443;
  end;
end;

procedure Tf_setup.DomeARestProtocolChange(Sender: TObject);
begin
  case DomeARestProtocol.ItemIndex of
    0: DomeARestPort.Value:=11111;
    1: DomeARestPort.Value:=443;
  end;
end;

procedure Tf_setup.FocuserARestProtocolChange(Sender: TObject);
begin
  case FocuserARestProtocol.ItemIndex of
    0: FocuserARestPort.Value:=11111;
    1: FocuserARestPort.Value:=443;
  end;
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

procedure Tf_setup.GetIndiClick(Sender: TObject);
begin
  if IndiTimer.Enabled then exit;
  GetDeviceType:=TButton(Sender).tag;
  GetIndiDevicesStart;
end;

procedure Tf_setup.GetIndiDevicesStart;
begin
  IndiTimerCount:=0;
  receiveindidevice:=false;
  camsavedev:=CameraIndiDevice.Text;
  wheelsavedev:=WheelIndiDevice.Text;
  focusersavedev:=FocuserIndiDevice.Text;
  rotatorsavedev:=RotatorIndiDevice.Text;
  mountsavedev:=MountIndiDevice.Text;
  domesavedev:=DomeIndiDevice.Text;
  weathersavedev:=WeatherIndiDevice.Text;
  safetysavedev:=SafetyIndiDevice.Text;
  watchdogsavedev:=WatchdogIndiDevice.Text;
  indiclient:=TIndiBaseClient.Create;
  indiclient.onNewDevice:=@IndiNewDevice;
  indiclient.onServerDisconnected:=@IndiDisconnected;
  case GetDeviceType of
    1: begin
        CameraMsg.Caption:='';
        CameraIndiDevice.Clear;
        indiclient.SetServer(CameraIndiServer.Text,CameraIndiPort.Text);
       end;
    2: begin
        WheelIndiDevice.Clear;
        indiclient.SetServer(WheelIndiServer.Text,WheelIndiPort.Text);
       end;
    3: begin
        FocuserIndiDevice.Clear;
        indiclient.SetServer(FocuserIndiServer.Text,FocuserIndiPort.Text);
       end;
    4: begin
        RotatorIndiDevice.Clear;
        indiclient.SetServer(RotatorIndiServer.Text,RotatorIndiPort.Text);
       end;
    5: begin
        MountIndiDevice.Clear;
        indiclient.SetServer(MountIndiServer.Text,MountIndiPort.Text);
       end;
    6: begin
        DomeIndiDevice.Clear;
        indiclient.SetServer(DomeIndiServer.Text,DomeIndiPort.Text);
       end;
    7: begin
        WeatherIndiDevice.Clear;
        indiclient.SetServer(WeatherIndiServer.Text,WeatherIndiPort.Text);
       end;
    8: begin
        SafetyIndiDevice.Clear;
        indiclient.SetServer(SafetyIndiServer.Text,SafetyIndiPort.Text);
       end;
    9: begin
        WatchdogIndiDevice.Clear;
        indiclient.SetServer(WatchdogIndiServer.Text,WatchdogIndiPort.Text);
       end;
  end;
  indiclient.ConnectServer;
  IndiTimer.Interval:=5000;  // wait 5 sec for initial connection
  IndiTimer.Enabled:=true;
  Screen.Cursor:=crHourGlass;
end;

procedure Tf_setup.IndiNewDevice(dp: Basedevice);
begin
   IndiTimer.Interval:=1000; // wait up to 5 x 1 sec for next device
   IndiTimer.Enabled:=false;
   IndiTimer.Enabled:=true;
   receiveindidevice:=true
end;

procedure Tf_setup.IndiDisconnected(Sender: TObject);
begin
   IndiTimer.Interval:=100; // not connect, stop immediately
   IndiTimer.Enabled:=false;
   IndiTimer.Enabled:=true;
end;

procedure Tf_setup.IndiTimerTimer(Sender: TObject);
var i: integer;
    drint:word;
begin
  inc(IndiTimerCount);
  if (not receiveindidevice)and(IndiTimerCount<=5)and(indiclient<>nil)and(indiclient.Connected) then exit;
  IndiTimer.Enabled:=false;
  Screen.Cursor:=crDefault;
  try
  if (indiclient=nil)or indiclient.Finished or indiclient.Terminated or (not indiclient.Connected)  then begin
    if (GetDeviceType=1) then begin
       CameraMsg.Caption:=rsNoResponseFr;
       CameraIndiDevice.Items.Add(camsavedev);
       CameraIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=2) then begin
       WheelMsg.Caption:=rsNoResponseFr;
       WheelIndiDevice.Items.Add(wheelsavedev);
       WheelIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=3) then begin
       FocuserMsg.Caption:=rsNoResponseFr;
       FocuserIndiDevice.Items.Add(focusersavedev);
       FocuserIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=4) then begin
       RotatorMsg.Caption:=rsNoResponseFr;
       RotatorIndiDevice.Items.Add(rotatorsavedev);
       RotatorIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=5) then begin
       MountMsg.Caption:=rsNoResponseFr;
       MountIndiDevice.Items.Add(mountsavedev);
       MountIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=6) then begin
       DomeMsg.Caption:=rsNoResponseFr;
       DomeIndiDevice.Items.Add(domesavedev);
       DomeIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=7) then begin
       WeatherMsg.Caption:=rsNoResponseFr;
       WeatherIndiDevice.Items.Add(weathersavedev);
       WeatherIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=8) then begin
       SafetyMsg.Caption:=rsNoResponseFr;
       SafetyIndiDevice.Items.Add(safetysavedev);
       SafetyIndiDevice.ItemIndex:=0;
    end;
    if (GetDeviceType=9) then begin
       WatchdogMsg.Caption:=rsNoResponseFr;
       WatchdogIndiDevice.Items.Add(watchdogsavedev);
       WatchdogIndiDevice.ItemIndex:=0;
    end;
    exit;
  end;
  try
  for i:=0 to indiclient.devices.Count-1 do begin
     drint:=BaseDevice(indiclient.devices[i]).getDriverInterface();
     if (GetDeviceType=1)and((drint and CCD_INTERFACE)<>0) then
        CameraIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=2)and((drint and FILTER_INTERFACE)<>0) then
        WheelIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=3)and((drint and FOCUSER_INTERFACE)<>0) then
        FocuserIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=4)and((drint and ROTATOR_INTERFACE)<>0) then
        RotatorIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=5)and((drint and TELESCOPE_INTERFACE)<>0) then
        MountIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=6)and((drint and DOME_INTERFACE)<>0) then
        DomeIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=7)and((drint and WEATHER_INTERFACE)<>0) then
        WeatherIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=8)and((drint and WEATHER_INTERFACE)<>0) then
        SafetyIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
     if (GetDeviceType=9)and((drint and AUX_INTERFACE)<>0) then
        WatchdogIndiDevice.Items.Add(BaseDevice(indiclient.devices[i]).getDeviceName);
  end;
  if (GetDeviceType=1) then begin
    if CameraIndiDevice.Items.Count>0 then
       CameraIndiDevice.ItemIndex:=0
    else begin
       CameraIndiDevice.Items.Add(camsavedev); ;
       CameraMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to CameraIndiDevice.Items.Count-1 do
       if CameraIndiDevice.Items[i]=camsavedev then CameraIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=2) then begin
    if WheelIndiDevice.Items.Count>0 then begin
       WheelIndiDevice.ItemIndex:=0;
       DeviceFilterWheel.Checked:=true;
    end
    else begin
       WheelIndiDevice.Items.Add(wheelsavedev);
       WheelMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to WheelIndiDevice.Items.Count-1 do
       if WheelIndiDevice.Items[i]=wheelsavedev then WheelIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=3) then begin
    if FocuserIndiDevice.Items.Count>0 then begin
       FocuserIndiDevice.ItemIndex:=0;
       DeviceFocuser.Checked:=true;
    end
    else begin
       FocuserIndiDevice.Items.Add(focusersavedev);
       FocuserMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to FocuserIndiDevice.Items.Count-1 do
       if FocuserIndiDevice.Items[i]=focusersavedev then FocuserIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=4) then begin
    if RotatorIndiDevice.Items.Count>0 then begin
       RotatorIndiDevice.ItemIndex:=0;
       DeviceRotator.Checked:=true;
    end
    else begin
       RotatorIndiDevice.Items.Add(rotatorsavedev);
       RotatorMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to RotatorIndiDevice.Items.Count-1 do
       if RotatorIndiDevice.Items[i]=rotatorsavedev then RotatorIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=5) then begin
    if MountIndiDevice.Items.Count>0 then begin
       MountIndiDevice.ItemIndex:=0;
       DeviceMount.Checked:=true;
    end
    else begin
       MountIndiDevice.Items.Add(mountsavedev);
       MountMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to MountIndiDevice.Items.Count-1 do
       if MountIndiDevice.Items[i]=mountsavedev then MountIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=6) then begin
    if DomeIndiDevice.Items.Count>0 then begin
       DomeIndiDevice.ItemIndex:=0;
       DeviceDome.Checked:=true;
    end
    else begin
       DomeIndiDevice.Items.Add(domesavedev);
       DomeMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to DomeIndiDevice.Items.Count-1 do
       if DomeIndiDevice.Items[i]=domesavedev then DomeIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=7) then begin
    if WeatherIndiDevice.Items.Count>0 then begin
       WeatherIndiDevice.ItemIndex:=0;
       DeviceWeather.Checked:=true;
    end
    else begin
       WeatherIndiDevice.Items.Add(weathersavedev);
       WeatherMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to WeatherIndiDevice.Items.Count-1 do
       if WeatherIndiDevice.Items[i]=weathersavedev then WeatherIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=8) then begin
    if SafetyIndiDevice.Items.Count>0 then begin
       SafetyIndiDevice.ItemIndex:=0;
       DeviceSafety.Checked:=true;
    end
    else begin
       SafetyIndiDevice.Items.Add(safetysavedev);
       SafetyMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to SafetyIndiDevice.Items.Count-1 do
       if SafetyIndiDevice.Items[i]=safetysavedev then SafetyIndiDevice.ItemIndex:=i;
  end;
  if (GetDeviceType=9) then begin
    if WatchdogIndiDevice.Items.Count>0 then begin
       WatchdogIndiDevice.ItemIndex:=0;
       DeviceWatchdog.Checked:=true;
    end
    else begin
       WatchdogIndiDevice.Items.Add(watchdogsavedev);
       WatchdogMsg.Caption:=rsNoDevice;
    end;
    for i:=0 to WatchdogIndiDevice.Items.Count-1 do
       if WatchdogIndiDevice.Items[i]=watchdogsavedev then WatchdogIndiDevice.ItemIndex:=i;
  end;
  indiclient.onServerDisconnected:=nil;
  indiclient.DisconnectServer;
  except
  end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tf_setup.MountARestProtocolChange(Sender: TObject);
begin
  case MountARestProtocol.ItemIndex of
    0: MountARestPort.Value:=11111;
    1: MountARestPort.Value:=443;
  end;
end;

procedure Tf_setup.MountGetObservatoryClick(Sender: TObject);
begin
  if MountGetObservatory.Checked then MountSetObservatory.Checked:=false;
end;

procedure Tf_setup.MountSetObservatoryClick(Sender: TObject);
begin
  if MountSetObservatory.Checked then MountGetObservatory.Checked:=false;
end;

procedure Tf_setup.PageControlCameraChange(Sender: TObject);
begin
  case PageControlCamera.ActivePageIndex of
    0: FCameraConnection:=INDI;
    1: FCameraConnection:=ASCOM;
    2: FCameraConnection:=ASCOMREST;
  end;
  DeviceCamera.Caption:=rsCamera+': '+DevInterfaceName[ord(FCameraConnection)];
end;

procedure Tf_setup.PageControlDomeChange(Sender: TObject);
begin
  case PageControlDome.ActivePageIndex of
    0: FDomeConnection:=INDI;
    1: FDomeConnection:=ASCOM;
    2: FDomeConnection:=ASCOMREST;
  end;
  DeviceDome.Caption:=rsUseDome+': '+DevInterfaceName[ord(FDomeConnection)];
end;

procedure Tf_setup.PageControlFocuserChange(Sender: TObject);
begin
  case PageControlFocuser.ActivePageIndex of
    0: FocuserConnection:=INDI;
    1: FocuserConnection:=ASCOM;
    2: FocuserConnection:=INTELESCOPE;
    3: FocuserConnection:=ASCOMREST;
  end;
  DeviceFocuser.Caption:=rsUseFocuser+': '+DevInterfaceName[ord(FocuserConnection)];
end;

procedure Tf_setup.PageControlMountChange(Sender: TObject);
begin
  case PageControlMount.ActivePageIndex of
    0: FMountConnection:=INDI;
    1: FMountConnection:=ASCOM;
    2: FMountConnection:=ASCOMREST;
  end;
  DeviceMount.Caption:=rsUseMount+': '+DevInterfaceName[ord(FMountConnection)];
end;

procedure Tf_setup.PageControlRotatorChange(Sender: TObject);
begin
  case PageControlRotator.ActivePageIndex of
    0: FRotatorConnection:=INDI;
    1: FRotatorConnection:=ASCOM;
    2: FRotatorConnection:=ASCOMREST;
  end;
  DeviceRotator.Caption:=rsUseRotator+': '+DevInterfaceName[ord(FRotatorConnection)];
end;

procedure Tf_setup.PageControlSafetyChange(Sender: TObject);
begin
  case PageControlSafety.ActivePageIndex of
    0: FSafetyConnection:=INDI;
    1: FSafetyConnection:=ASCOM;
    2: FSafetyConnection:=ASCOMREST;
  end;
  DeviceSafety.Caption:=rsUseSafetyMon+': '+DevInterfaceName[ord(FSafetyConnection)];
end;

procedure Tf_setup.PageControlWeatherChange(Sender: TObject);
begin
  case PageControlWeather.ActivePageIndex of
    0: FWeatherConnection:=INDI;
    1: FWeatherConnection:=ASCOM;
    2: FWeatherConnection:=ASCOMREST;
  end;
  DeviceWeather.Caption:=rsUseWeatherSt+': '+DevInterfaceName[ord(FWeatherConnection)];
end;

procedure Tf_setup.PageControlWheelChange(Sender: TObject);
begin
  case PageControlWheel.ActivePageIndex of
    0: FWheelConnection:=INDI;
    1: FWheelConnection:=ASCOM;
    2: FWheelConnection:=INCAMERA;
    3: FWheelConnection:=ASCOMREST;
  end;
  DeviceFilterWheel.Caption:=rsUseFilterWhe+': '+DevInterfaceName[ord(FWheelConnection)];
end;

procedure Tf_setup.PageControlWatchdogChange(Sender: TObject);
begin
// no alternative
end;

procedure Tf_setup.ProfileListChange(Sender: TObject);
var chkconf,chkcred:TCCDconfig;
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
    chkcred:=TCCDConfig.Create(nil);
    try
    chkconf.Filename:=slash(ConfigDir)+configfile;
    chkcred.Filename:=chkconf.Filename+'.credential';
    Loadconfig(chkconf,chkcred);
    finally
      chkconf.Free;
      chkcred.Free;
    end;
  end;
end;

procedure Tf_setup.AscomWeatherTypeClick(Sender: TObject);
begin
  AscomWeather.Text:='';
end;

procedure Tf_setup.RotatorARestProtocolChange(Sender: TObject);
begin
  case RotatorARestProtocol.ItemIndex of
    0: RotatorARestPort.Value:=11111;
    1: RotatorARestPort.Value:=443;
  end;
end;

procedure Tf_setup.SafetyARestProtocolChange(Sender: TObject);
begin
  case SafetyARestProtocol.ItemIndex of
    0: SafetyARestPort.Value:=11111;
    1: SafetyARestPort.Value:=443;
  end;
end;

procedure Tf_setup.WeatherARestProtocolChange(Sender: TObject);
begin
  case WeatherARestProtocol.ItemIndex of
    0: WeatherARestPort.Value:=11111;
    1: WeatherARestPort.Value:=443;
  end;
end;

procedure Tf_setup.WheelARestProtocolChange(Sender: TObject);
begin
  case WheelARestProtocol.ItemIndex of
    0: WheelARestPort.Value:=11111;
    1: WheelARestPort.Value:=443;
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
var newp,newconfig:string;
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

initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

