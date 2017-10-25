unit pu_main;

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

uses fu_devicesconnection, fu_preview, fu_capture, fu_msg, fu_visu, fu_frame,
  fu_starprofile, fu_filterwheel, fu_focuser, fu_mount, fu_ccdtemp, fu_autoguider,
  fu_sequence, fu_planetarium, fu_script, u_ccdconfig, pu_editplan, pu_scriptengine,
  fu_video, pu_devicesetup, pu_options, pu_indigui, cu_fits, cu_camera, pu_pause,
  pu_viewtext, cu_wheel, cu_mount, cu_focuser, XMLConf, u_utils, u_global, UScaleDPI,
  cu_indimount, cu_ascommount, cu_indifocuser, cu_ascomfocuser, pu_vcurve,
  fu_rotator, cu_rotator, cu_indirotator, cu_ascomrotator,
  cu_indiwheel, cu_ascomwheel, cu_incamerawheel, cu_indicamera, cu_ascomcamera, cu_astrometry,
  cu_autoguider, cu_autoguider_phd, cu_planetarium, cu_planetarium_cdc, cu_planetarium_samp,
  cu_planetarium_hnsky, pu_planetariuminfo, indiapi, BGRABitmap, BGRABitmapTypes, LCLVersion, InterfaceBase,
  LazUTF8, LazUTF8SysUtils, Classes, dynlibs, LCLType, LMessages, IniFiles,
  SysUtils, LazFileUtils, Forms, Controls, Math, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, Types;

type

  { Tf_main }

  Tf_main = class(TForm)
    Image1: TImage;
    ImageListNight: TImageList;
    ImageListDay: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuIndiSettings: TMenuItem;
    MenuHelpAbout: TMenuItem;
    MenuClearRef: TMenuItem;
    MenuBPM: TMenuItem;
    MenuItem5: TMenuItem;
    MenuDownload: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuResolveRotate: TMenuItem;
    MenuViewClock: TMenuItem;
    MenuResolveSyncRotator: TMenuItem;
    MenuRotatorRotate: TMenuItem;
    MenuRotator: TMenuItem;
    MenuViewRotator: TMenuItem;
    TimerStampTimer: TTimer;
    Timestamp: TMenuItem;
    MenuPdfHelp: TMenuItem;
    MenuOnlineHelp: TMenuItem;
    MenuBugReport: TMenuItem;
    MenuShowCCDFrame: TMenuItem;
    MenuItemRaw: TMenuItem;
    MenuItemDebayer: TMenuItem;
    MenuItemBPM: TMenuItem;
    MenuClearBPM: TMenuItem;
    MenuVideoPreview: TMenuItem;
    MenuVideoStart: TMenuItem;
    MenuVideoStop: TMenuItem;
    MenuVideo: TMenuItem;
    MenuTabVideo: TMenuItem;
    MenuViewVideo: TMenuItem;
    MenuTabSequence: TMenuItem;
    MenuTabCapture: TMenuItem;
    MenuTabFocus: TMenuItem;
    MenuTabConnect: TMenuItem;
    MenuConnect: TMenuItem;
    MenuCaptureStart: TMenuItem;
    MenuFrameSet: TMenuItem;
    MenuFrameReset: TMenuItem;
    MenuFocusaid: TMenuItem;
    MenuFocuserIn: TMenuItem;
    MenuFocuserOut: TMenuItem;
    MenuFilter1: TMenuItem;
    MenuCCDtempSet: TMenuItem;
    MenuAutoguiderConnect: TMenuItem;
    MenuAutoguiderCalibrate: TMenuItem;
    MenuAutoguiderGuide: TMenuItem;
    MenuAutoguiderDither: TMenuItem;
    MenuScriptStop: TMenuItem;
    MenuScriptEdit: TMenuItem;
    MenuScriptNew: TMenuItem;
    MenuScriptRun: TMenuItem;
    MenuPlanetariumNewtarget: TMenuItem;
    MenuPlanetariumConnect: TMenuItem;
    MenuMountTrack: TMenuItem;
    MenuMountPark: TMenuItem;
    MenuVisuZoomAdjust: TMenuItem;
    MenuVisuZoom2: TMenuItem;
    MenuVisuZoom1: TMenuItem;
    MenuVisuZoom12: TMenuItem;
    MenuVisuLinear: TMenuItem;
    MenuVisuLog: TMenuItem;
    MenuVisuSqrt: TMenuItem;
    MenuSequenceNew: TMenuItem;
    MenuSequenceEdit: TMenuItem;
    MenuSequenceStart: TMenuItem;
    MenuSequenceStop: TMenuItem;
    MenuSequenceLoad: TMenuItem;
    MenuPreviewLoop: TMenuItem;
    MenuPreviewStart: TMenuItem;
    MenuResolveSlewCenter: TMenuItem;
    MenuResolve: TMenuItem;
    MenuRefimage: TMenuItem;
    N7: TMenuItem;
    MenuViewScript: TMenuItem;
    MenuViewPlanetarium: TMenuItem;
    MenuResolveSlew: TMenuItem;
    MenuResolveSync: TMenuItem;
    MenuViewSequence: TMenuItem;
    MenuViewAutoguider: TMenuItem;
    MenuViewAstrometryLog: TMenuItem;
    MenuStopAstrometry: TMenuItem;
    MenuResolvePlanetarium: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    N6: TMenuItem;
    MenuViewFrame: TMenuItem;
    N5: TMenuItem;
    MenuOptions: TMenuItem;
    MenuViewCCDtemp: TMenuItem;
    N4: TMenuItem;
    MenuResetTools: TMenuItem;
    N3: TMenuItem;
    MenuViewFilters: TMenuItem;
    MenuViewStarProfile: TMenuItem;
    MenuViewFocuser: TMenuItem;
    MenuViewMount: TMenuItem;
    MenuViewMessages: TMenuItem;
    MenuViewPreview: TMenuItem;
    MenuViewCapture: TMenuItem;
    MenuViewHistogram: TMenuItem;
    MenuViewConnection: TMenuItem;
    MenuViewhdr: TMenuItem;
    MenuQuit: TMenuItem;
    MenuSetup: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    MenuConnection: TMenuItem;
    MenuPreview: TMenuItem;
    MenuCapture: TMenuItem;
    MenuSequence: TMenuItem;
    MenuFrame: TMenuItem;
    MenuHistogram: TMenuItem;
    MenuStarProfile: TMenuItem;
    MenuFocuser: TMenuItem;
    MenuFilters: TMenuItem;
    MenuCCDtemp: TMenuItem;
    MenuMount: TMenuItem;
    MenuAutoguider: TMenuItem;
    MenuPlanetarium: TMenuItem;
    MenuScript: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControlRight: TPageControl;
    Panel1: TPanel;
    PanelRight5: TPanel;
    PanelCenter: TPanel;
    PanelLeft: TPanel;
    PanelRight1: TPanel;
    PanelRight2: TPanel;
    PanelRight3: TPanel;
    PanelRight4: TPanel;
    PanelTop: TPanel;
    ImagePopupMenu: TPopupMenu;
    PanelBottom: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ConnectTimer: TTimer;
    StatusbarTimer: TTimer;
    PageConnect: TTabSheet;
    PageFocus: TTabSheet;
    PageCapture: TTabSheet;
    PageSequence: TTabSheet;
    AbortTimer: TTimer;
    StartCaptureTimer: TTimer;
    StartupTimer: TTimer;
    StartSequenceTimer: TTimer;
    StatusTimer: TTimer;
    PageVideo: TTabSheet;
    procedure AbortTimerTimer(Sender: TObject);
    procedure ConnectTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Image1Paint(Sender: TObject);
    procedure Image1Resize(Sender: TObject);
    procedure MenuAutoguiderCalibrateClick(Sender: TObject);
    procedure MenuAutoguiderConnectClick(Sender: TObject);
    procedure MenuAutoguiderDitherClick(Sender: TObject);
    procedure MenuAutoguiderGuideClick(Sender: TObject);
    procedure MenuBPMClick(Sender: TObject);
    procedure MenuBugReportClick(Sender: TObject);
    procedure MenuCaptureStartClick(Sender: TObject);
    procedure MenuCCDtempSetClick(Sender: TObject);
    procedure MenuClearBPMClick(Sender: TObject);
    procedure MenuClearRefClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuDownloadClick(Sender: TObject);
    procedure MenuFilterClick(Sender: TObject);
    procedure MenuFocusaidClick(Sender: TObject);
    procedure MenuFocuserInClick(Sender: TObject);
    procedure MenuFocuserOutClick(Sender: TObject);
    procedure MenuFrameResetClick(Sender: TObject);
    procedure MenuFrameSetClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuIndiSettingsClick(Sender: TObject);
    procedure MenuResolveRotateClick(Sender: TObject);
    procedure MenuResolveSyncRotatorClick(Sender: TObject);
    procedure MenuItemDebayerClick(Sender: TObject);
    procedure MenuItemRawClick(Sender: TObject);
    procedure MenuMountParkClick(Sender: TObject);
    procedure MenuMountTrackClick(Sender: TObject);
    procedure MenuOnlineHelpClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuOptionsClick(Sender: TObject);
    procedure MenuPdfHelpClick(Sender: TObject);
    procedure MenuPlanetariumConnectClick(Sender: TObject);
    procedure MenuPlanetariumNewtargetClick(Sender: TObject);
    procedure MenuPreviewLoopClick(Sender: TObject);
    procedure MenuPreviewStartClick(Sender: TObject);
    procedure MenuRefimageClick(Sender: TObject);
    procedure MenuResetToolsClick(Sender: TObject);
    procedure MenuResolveClick(Sender: TObject);
    procedure MenuResolveSlewCenterClick(Sender: TObject);
    procedure MenuResolveSlewClick(Sender: TObject);
    procedure MenuResolveSyncClick(Sender: TObject);
    procedure MenuRotatorRotateClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuResolvePlanetariumClick(Sender: TObject);
    procedure MenuScriptEditClick(Sender: TObject);
    procedure MenuScriptNewClick(Sender: TObject);
    procedure MenuScriptRunClick(Sender: TObject);
    procedure MenuScriptStopClick(Sender: TObject);
    procedure MenuSequenceEditClick(Sender: TObject);
    procedure MenuSequenceLoadClick(Sender: TObject);
    procedure MenuSequenceNewClick(Sender: TObject);
    procedure MenuSequenceStartClick(Sender: TObject);
    procedure MenuSequenceStopClick(Sender: TObject);
    procedure MenuShowCCDFrameClick(Sender: TObject);
    procedure MenuStopAstrometryClick(Sender: TObject);
    procedure MenuTabClick(Sender: TObject);
    procedure MenuVideoPreviewClick(Sender: TObject);
    procedure MenuVideoStartClick(Sender: TObject);
    procedure MenuVideoStopClick(Sender: TObject);
    procedure MenuViewAstrometryLogClick(Sender: TObject);
    procedure MenuViewAutoguiderClick(Sender: TObject);
    procedure MenuViewCCDtempClick(Sender: TObject);
    procedure MenuViewClockClick(Sender: TObject);
    procedure MenuViewConnectionClick(Sender: TObject);
    procedure MenuViewFiltersClick(Sender: TObject);
    procedure MenuViewFocuserClick(Sender: TObject);
    procedure MenuViewFrameClick(Sender: TObject);
    procedure MenuViewhdrClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuSetupClick(Sender: TObject);
    procedure MenuViewHistogramClick(Sender: TObject);
    procedure MenuViewMessagesClick(Sender: TObject);
    procedure MenuViewMountClick(Sender: TObject);
    procedure MenuViewPlanetariumClick(Sender: TObject);
    procedure MenuViewPreviewClick(Sender: TObject);
    procedure MenuViewCaptureClick(Sender: TObject);
    procedure MenuViewRotatorClick(Sender: TObject);
    procedure MenuViewScriptClick(Sender: TObject);
    procedure MenuViewSequenceClick(Sender: TObject);
    procedure MenuViewStarProfileClick(Sender: TObject);
    procedure MenuVisuLinearClick(Sender: TObject);
    procedure MenuVisuLogClick(Sender: TObject);
    procedure MenuVisuSqrtClick(Sender: TObject);
    procedure MenuVisuZoom12Click(Sender: TObject);
    procedure MenuVisuZoom1Click(Sender: TObject);
    procedure MenuVisuZoom2Click(Sender: TObject);
    procedure MenuVisuZoomAdjustClick(Sender: TObject);
    procedure PanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PanelDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
    procedure StartCaptureTimerTimer(Sender: TObject);
    procedure StartSequenceTimerTimer(Sender: TObject);
    procedure StartupTimerTimer(Sender: TObject);
    procedure StatusbarTimerTimer(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure TimerStampTimerTimer(Sender: TObject);
  private
    { private declarations }
    camera: T_camera;
    wheel: T_wheel;
    focuser: T_focuser;
    rotator: T_rotator;
    mount: T_mount;
    autoguider:T_autoguider;
    planetarium:TPlanetarium;
    astrometry:TAstrometry;
    WantCamera,WantWheel,WantFocuser,WantRotator, WantMount: boolean;
    FOpenSetup: boolean;
    f_devicesconnection: Tf_devicesconnection;
    f_filterwheel: Tf_filterwheel;
    f_ccdtemp: Tf_ccdtemp;
    f_frame: Tf_frame;
    f_preview: Tf_preview;
    f_capture: Tf_capture;
    f_video: Tf_video;
    f_sequence: Tf_sequence;
    f_starprofile: Tf_starprofile;
    f_focuser: Tf_focuser;
    f_rotator: Tf_rotator;
    f_vcurve:Tf_vcurve;
    f_mount: Tf_mount;
    f_autoguider: Tf_autoguider;
    f_planetarium: Tf_planetarium;
    f_script: Tf_script;
    f_visu: Tf_visu;
    f_msg: Tf_msg;
    fits: TFits;
    ImaBmp: TBGRABitmap;
    refmask: boolean;
    reftreshold,refcolor: integer;
    reffile: string;
    refbmp:TBGRABitmap;
    cdcWCSinfo: TcdcWCSinfo;
    WCSxyrot: double;
    SaveFocusZoom: double;
    ImgCx, ImgCy, Mx, My,Starwindow,Focuswindow: integer;
    StartX, StartY, EndX, EndY, MouseDownX,MouseDownY: integer;
    FrameX,FrameY,FrameW,FrameH: integer;
    DeviceTimeout: integer;
    MouseMoving, MouseFrame, LockMouse, LockMouseWheel: boolean;
    Capture,Preview,meridianflipping,autofocusing,learningvcurve: boolean;
    LogToFile,LogFileOpen,DeviceLogFileOpen: Boolean;
    NeedRestart, GUIready, AppClose: boolean;
    LogFile,DeviceLogFile : UTF8String;
    MsgLog,MsgDeviceLog: Textfile;
    AccelList: array[0..MaxMenulevel] of string;
    SaveAutofocusBinning: string;
    SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH,SaveAutofocusBX,SaveAutofocusBY: integer;
    TerminateVcurve: boolean;
    Procedure GetAppDir;
    procedure ScaleMainForm;
    Procedure InitLog;
    Procedure InitDeviceLog;
    Procedure CloseLog;
    Procedure WriteLog( buf : string);
    Procedure WriteDeviceLog( buf : string);
    procedure SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; chkmenu,toolmenu: TMenuItem);
    procedure UpdConfig(oldver:string);
    procedure SetConfig;
    procedure SetOptions;
    procedure OpenConfig(n: string);
    procedure SaveConfig;
    procedure SaveVcurve;
    procedure LoadVcurve;
    procedure LoadBPM;
    procedure ComputeVcSlope;
    procedure OptionGetPixelSize(Sender: TObject);
    procedure OptionGetFocaleLength(Sender: TObject);
    procedure Restart;
    procedure SetTheme;
    procedure SetRefImage;
    procedure GUIdestroy(Sender: TObject);
    Procedure Connect(Sender: TObject);
    Procedure Disconnect(Sender: TObject);
    Procedure CheckConnectionStatus;
    Procedure ConnectCamera(Sender: TObject);
    Procedure DisconnectCamera(Sender: TObject);
    procedure SetCameraActiveDevices;
    procedure ShowBinningRange;
    procedure ShowFrameRange;
    procedure ShowFrame;
    procedure SetFrame(Sender: TObject);
    procedure ResetFrame(Sender: TObject);
    Procedure FrameChange(Sender: TObject);
    procedure ShowExposureRange;
    procedure ShowTemperatureRange;
    procedure SetTemperature(Sender: TObject);
    procedure SetCooler(Sender: TObject);
    procedure SetMountPark(Sender: TObject);
    procedure SetMountTrack(Sender: TObject);
    procedure SetFocusMode;
    Procedure ConnectWheel(Sender: TObject);
    Procedure DisconnectWheel(Sender: TObject);
    Procedure ConnectFocuser(Sender: TObject);
    Procedure DisconnectFocuser(Sender: TObject);
    Procedure ConnectRotator(Sender: TObject);
    Procedure DisconnectRotator(Sender: TObject);
    Procedure ConnectMount(Sender: TObject);
    Procedure DisconnectMount(Sender: TObject);
    Procedure SetFilter(Sender: TObject);
    Procedure SetFilterMenu;
    Procedure NewMessage(msg: string);
    Procedure DeviceMessage(msg: string);
    Procedure CameraStatus(Sender: TObject);
    Procedure CameraDisconnected(Sender: TObject);
    Procedure CameraExposureAborted(Sender: TObject);
    procedure CameraProgress(n:double);
    procedure CameraTemperatureChange(t:double);
    procedure CameraCoolerChange(var v:boolean);
    Procedure WheelStatus(Sender: TObject);
    procedure FilterChange(n:double);
    procedure FilterNameChange(Sender: TObject);
    Procedure FocusStart(Sender: TObject);
    Procedure FocusStop(Sender: TObject);
    Procedure AutoFocusStart(Sender: TObject);
    Procedure AutoFocusStop(Sender: TObject);
    procedure LoadFocusStar;
    function  FindFocusStar(tra, tde:double; out sra,sde: double; out id: string): Boolean;
    function  AutoAutofocus(ReturnToTarget: boolean=true): Boolean;
    procedure cmdAutomaticAutofocus(var ok: boolean);
    procedure cmdAutofocus(var ok: boolean);
    Procedure FocuserStatus(Sender: TObject);
    procedure FocuserPositionChange(n:double);
    procedure FocuserSpeedChange(n:double);
    procedure FocuserTimerChange(n:double);
    procedure FocuserTemperatureChange(n:double);
    procedure FocusIN(Sender: TObject);
    procedure FocusOUT(Sender: TObject);
    procedure FocusSetAbsolutePosition(Sender: TObject);
    procedure FocusVcurveLearning(Sender: TObject);
    procedure LearnVcurve(Sender: TObject);
    procedure StopVcurve(Sender: TObject);
    procedure doSaveVcurve(Sender: TObject);
    function doVcurve(centerp,hw,n,nsum: integer;exp:double;bin:integer):boolean;
    Procedure RotatorStatus(Sender: TObject);
    Procedure RotatorAngleChange(Sender: TObject);
    Procedure RotatorRotate(Sender: TObject);
    Procedure RotatorHalt(Sender: TObject);
    Procedure RotatorReverse(Sender: TObject);
    Procedure MountStatus(Sender: TObject);
    Procedure MountCoordChange(Sender: TObject);
    Procedure MountPiersideChange(Sender: TObject);
    Procedure MountParkChange(Sender: TObject);
    Procedure AutoguiderConnectClick(Sender: TObject);
    Procedure AutoguiderCalibrateClick(Sender: TObject);
    Procedure AutoguiderGuideClick(Sender: TObject);
    Procedure AutoguiderDitherClick(Sender: TObject);
    Procedure AutoguiderConnect(Sender: TObject);
    Procedure AutoguiderDisconnect(Sender: TObject);
    Procedure AutoguiderStatus(Sender: TObject);
    Procedure PlanetariumConnectClick(Sender: TObject);
    Procedure PlanetariumConnect(Sender: TObject);
    Procedure PlanetariumDisconnect(Sender: TObject);
    Procedure PlanetariumNewTarget(Sender: TObject);
    procedure CameraNewImage(Sender: TObject);
    procedure CameraNewImageAsync(Data: PtrInt);
    procedure CameraVideoFrame(Sender: TObject);
    procedure CameraVideoPreviewChange(Sender: TObject);
    procedure CameraVideoFrameAsync(Data: PtrInt);
    procedure CameraVideoSizeChange(Sender: TObject);
    procedure CameraVideoRateChange(Sender: TObject);
    procedure CameraVideoExposureChange(Sender: TObject);
    procedure CameraFPSChange(Sender: TObject);
    Procedure AbortExposure(Sender: TObject);
    Procedure StartPreviewExposure(Sender: TObject);
    Procedure StartPreviewExposureAsync(Data: PtrInt);
    Procedure StartCaptureExposure(Sender: TObject);
    procedure StartCaptureExposureAsync(Data: PtrInt);
    Procedure RedrawHistogram(Sender: TObject);
    Procedure Redraw(Sender: TObject);
    Procedure ZoomImage(Sender: TObject);
    Procedure ClearImage;
    Procedure DrawImage;
    Procedure PlotImage;
    procedure plot_north(bmp:TBGRABitmap);
    Procedure DrawHistogram(SetLevel: boolean);
    procedure AstrometryStart(Sender: TObject);
    procedure AstrometryEnd(Sender: TObject);
    procedure EndControlExposure(Sender: TObject);
    procedure AstrometryToPlanetarium(Sender: TObject);
    procedure AstrometryToPlanetariumFrame(Sender: TObject);
    procedure ResolveSlewCenter(Sender: TObject);
    procedure ResolveSyncRotator(Sender: TObject);
    procedure ResolveRotate(Sender: TObject);
    procedure LoadFitsFile(fn:string);
    procedure SaveFitsFile(fn:string);
    procedure OpenRefImage(fn:string);
    procedure ClearRefImage(Sender: TObject);
    procedure CCDCIELMessageHandler(var Message: TLMessage); message LM_CCDCIEL;
    Procedure StartSequence(SeqName: string);
    procedure ScriptExecute(Sender: TObject);
    procedure ScriptAfterExecute(Sender: TObject);
    function CheckMeridianFlip(nextexposure:double=0):integer;
  public
    { public declarations }
  end;

var
  f_main: Tf_main;

implementation

{$if (lcl_fullversion >= 1070000)}
  uses lclplatformdef;
{$endif}

{$R *.lfm}

{ Tf_main }

Procedure Tf_main.InitLog;
begin
  try
     LogFile:=slash(LogDir)+'Log_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.log';
     Filemode:=2;
     AssignFile(MsgLog,LogFile);
     Rewrite(MsgLog);
     WriteLn(MsgLog,FormatDateTime(dateiso,Now)+'  CCDciel '+ccdciel_version+'-'+RevisionStr+blank+compile_time);
     WriteLn(MsgLog,FormatDateTime(dateiso,Now)+'  Compiled with: '+compile_version);
     LogFileOpen:=true;
  except
  {$I-}
     LogFileOpen:=false;
     LogToFile:=false;
     CloseFile(MsgLog);
     IOResult;
  {$I+}
  end;
end;

Procedure Tf_main.InitDeviceLog;
begin
  try
     DeviceLogFile:=slash(LogDir)+'Devices_Log_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.log';
     Filemode:=2;
     AssignFile(MsgDeviceLog,DeviceLogFile);
     Rewrite(MsgDeviceLog);
     WriteLn(MsgDeviceLog,FormatDateTime(dateiso,Now)+'  Start new log');
     DeviceLogFileOpen:=true;
  except
  {$I-}
     DeviceLogFileOpen:=false;
     LogToFile:=false;
     CloseFile(MsgDeviceLog);
     IOResult;
  {$I+}
  end;
end;

Procedure Tf_main.CloseLog;
begin
  try
    if LogFileOpen then begin
      LogFileOpen:=false;
      CloseFile(MsgLog);
    end;
    if DeviceLogFileOpen then begin
      DeviceLogFileOpen:=false;
      CloseFile(MsgDeviceLog);
    end;
  except
    {$I-}
    IOResult;
    {$I+}
  end;
end;

Procedure Tf_main.WriteLog( buf : string);
begin
  try
    if LogToFile then begin
     if not LogFileOpen then begin
        InitLog;
        if not LogFileOpen then exit;
     end;
     WriteLn(MsgLog,FormatDateTime(dateiso,Now)+'  '+UTF8ToSys(buf));
     Flush(MsgLog);
    end;
  except
    {$I-}
    LogFileOpen:=false;
    LogToFile:=false;
    CloseFile(MsgLog);
    {$I+}
  end;
end;

Procedure Tf_main.WriteDeviceLog( buf : string);
begin
  try
    if LogToFile then begin
     if not DeviceLogFileOpen then begin
        InitDeviceLog;
        if not DeviceLogFileOpen then exit;
     end;
     WriteLn(MsgDeviceLog,FormatDateTime(dateiso,Now)+'  '+UTF8ToSys(buf));
     Flush(MsgDeviceLog);
    end;
  except
    {$I-}
    DeviceLogFileOpen:=false;
    LogToFile:=false;
    CloseFile(MsgDeviceLog);
    {$I+}
  end;
end;

procedure Tf_main.Restart;
begin
  ShowMessage('The program will restart now...');
  NeedRestart:=true;
  Close;
end;

procedure Tf_main.SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; chkmenu,toolmenu: TMenuItem);
var pn: string;
    i: integer;
    par: Tpanel;
    opm,npm: Tmenuitem;
begin
pn:=config.GetValue('/Tools/'+configname+'/Parent',defaultParent.Name);
par:=defaultParent;
for i:=0 to ComponentCount-1 do begin
   if Components[i].Name=pn then begin
      par:=TPanel(Components[i]);
      break;
   end;
end;
if par.Width>par.Height then begin
   tool.Align:=alLeft;
end else begin
   tool.Align:=alTop;
end;
tool.Top:=config.GetValue('/Tools/'+widestring(configname)+'/Top',defaultpos);
tool.Left:=config.GetValue('/Tools/'+widestring(configname)+'/Left',defaultpos);
tool.Parent:=par;
tool.Visible:=config.GetValue('/Tools/'+widestring(configname)+'/Visible',true);
chkmenu.Checked:=tool.Visible;
tool.Tag:=PtrInt(toolmenu);
if (toolmenu<>nil)and(par.tag>0) then begin
   npm:=TMenuItem(par.tag);
   opm:=toolmenu.Parent;
   if (opm<>nil)and(npm<>opm) then begin
     i:=opm.IndexOf(toolmenu);
     opm.Delete(i);
     npm.Add(toolmenu);
   end;
end;
end;

Procedure Tf_main.GetAppDir;
var buf:string;
    {$ifdef darwin}
    i:integer;
    {$endif}
begin
 {$ifdef darwin}
 Appdir:=getcurrentdir;
 if not DirectoryExists(slash(Appdir)+slash('scripts')) then begin
    Appdir:=ExtractFilePath(ParamStr(0));
    i:=pos('.app/',Appdir);
    if i>0 then begin
      Appdir:=ExtractFilePath(copy(Appdir,1,i));
    end;
 end;
 {$else}
 Appdir:=getcurrentdir;
 if not DirectoryExists(slash(Appdir)+slash('scripts')) then begin
     Appdir:=ExtractFilePath(ParamStr(0));
 end;
 {$endif}
 {$ifdef unix}
 Appdir:=expandfilename(Appdir);
 {$endif}
 // Be sur the script directory exists
 if (not directoryexists(slash(appdir)+slash('scripts'))) then begin
   // try under the current directory
   buf:=GetCurrentDir;
   if (directoryexists(slash(buf)+slash('scripts'))) then
      appdir:=buf
   else begin
      // try under the program directory
      buf:=ExtractFilePath(ParamStr(0));
      if (directoryexists(slash(buf)+slash('scripts'))) then
         appdir:=buf
      else begin
          // try share directory under current location
          buf:=ExpandFileName(slash(GetCurrentDir)+SharedDir);
          if (directoryexists(slash(buf)+slash('scripts'))) then
             appdir:=buf
          else begin
             // try share directory at the same location as the program
             buf:=ExpandFileName(slash(ExtractFilePath(ParamStr(0)))+SharedDir);
             if (directoryexists(slash(buf)+slash('scripts'))) then
                appdir:=buf
          else begin
             // try in /usr
             buf:=ExpandFileName(slash('/usr/bin')+SharedDir);
             if (directoryexists(slash(buf)+slash('scripts'))) then
                appdir:=buf
          else begin
             // try /usr/local
             buf:=ExpandFileName(slash('/usr/local/bin')+SharedDir);
             if (directoryexists(slash(buf)+slash('scripts'))) then
                appdir:=buf

             else begin
                 Showmessage('Error: Can''t locate the scripts directory !!'+crlf+'Please try to reinstall the software');
             end;
          end;
          end;
          end;
      end;
   end;
 end;
 DataDir:=slash(Appdir)+slash('data');
 ConfigDir:=GetAppConfigDirUTF8(false,true);
 TmpDir:=slash(ConfigDir)+'tmp';
 if not DirectoryExistsUTF8(TmpDir) then  CreateDirUTF8(TmpDir);
 LogDir:=slash(ConfigDir)+'Log';
 if not DirectoryExistsUTF8(LogDir) then  CreateDirUTF8(LogDir);
end;

procedure Tf_main.ScaleMainForm;
var rl: integer;
const teststr = 'The Lazy Fox Jumps';
      designlen = 120;
begin
  ScreenScaling:=true;
  UScaleDPI.UseScaling:=ScreenScaling;
  {$ifdef SCALE_BY_DPI_ONLY}
  UScaleDPI.DesignDPI:=96;
  UScaleDPI.RunDPI:=Screen.PixelsPerInch;
  {$else}
  rl:=Canvas.TextWidth(teststr);
  if abs(rl-designlen)<20 then rl:=designlen;
  UScaleDPI.DesignDPI:=designlen;
  UScaleDPI.RunDPI:=rl;
  {$endif}
  ScaleDPI(Self);
  ScaleImageList(ImageListDay);
  ScaleImageList(ImageListNight);
end;

procedure Tf_main.FormCreate(Sender: TObject);
var DefaultInterface,aInt: TDevInterface;
    inif: TIniFile;
    configfile: string;
    i:integer;
begin
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.TimeSeparator:=':';
  lclver:=lcl_version;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  compile_system:={$I %FPCTARGETOS%};
  {$ifdef mswindows}
  DefaultInterface:=ASCOM;
  Application.UpdateFormatSettings := False;
  {$else}
  DefaultInterface:=INDI;
  {$endif}
  AppClose:=false;
  ConfirmClose:=true;
  ScaleMainForm;
  NeedRestart:=false;
  GUIready:=false;
  filteroffset_initialized:=false;
  MsgHandle:=handle;
  meridianflipping:=false;
  TemperatureSlope:=0;
  learningvcurve:=false;
  autofocusing:=false;
  CancelAutofocus:=false;
  InplaceAutofocus:=false;
  AutofocusExposureFact:=1;
  FocuserLastTemp:=NullCoord;
  WaitTillrunning:=false;
  cancelWaitTill:=false;
  onMsgGlobal:=@NewMessage;
  ImgPixRatio:=1;
  ZoomMin:=1;
  refmask:=false;
  reftreshold:=128;
  refbmp:=TBGRABitmap.Create;
  FilterList:=TStringList.Create;
  BinningList:=TStringList.Create;
  CurrentFilterOffset:=0;
  PageControlRight.ActivePageIndex:=0;
  cdcwcs_initfitsfile:=nil;
  cdcwcs_release:=nil;
  cdcwcs_sky2xy:=nil;
  cdcwcs_xy2sky:=nil;
  cdcwcslib:=LoadLibrary(libwcs);
  if cdcwcslib<>0 then begin
    cdcwcs_initfitsfile:= Tcdcwcs_initfitsfile(GetProcedureAddress(cdcwcslib,'cdcwcs_initfitsfile'));
    cdcwcs_release:= Tcdcwcs_release(GetProcedureAddress(cdcwcslib,'cdcwcs_release'));
    cdcwcs_sky2xy:= Tcdcwcs_sky2xy(GetProcedureAddress(cdcwcslib,'cdcwcs_sky2xy'));
    cdcwcs_xy2sky:= Tcdcwcs_sky2xy(GetProcedureAddress(cdcwcslib,'cdcwcs_xy2sky'));
    cdcwcs_getinfo:= Tcdcwcs_getinfo(GetProcedureAddress(cdcwcslib,'cdcwcs_getinfo'));
  end;
  zlibok:=false;
  uncompress:=nil;
  zlib:=LoadLibrary(libz);
  if zlib<>0 then begin
    uncompress:= Tuncompress(GetProcedureAddress(zlib,'uncompress'));
    if uncompress<>nil then zlibok:=true;
  end;
  GetAppDir;
  ConfigExtension:= '.conf';
  config:=TCCDConfig.Create(self);
  if Application.HasOption('c', 'config') then begin
    profile:=Application.GetOptionValue('c', 'config');
  end
  else begin
    inif:=TIniFile.Create(slash(ConfigDir)+'ccdciel.rc');
    profile:=inif.ReadString('main','profile','default');
    inif.Free;
  end;
  if profile='default' then
     configfile:='ccdciel.conf'
  else
     configfile:='ccdciel_'+profile+'.conf';
  FOpenSetup:=not FileExistsUTF8(slash(ConfigDir)+configfile);
  OpenConfig(configfile);

  LogFileOpen:=false;
  for i:=1 to MaxScriptDir do ScriptDir[i]:=TScriptDir.Create;
  ScriptDir[1].path:=slash(ConfigDir);
  ScriptDir[2].path:=slash(Appdir)+slash('scripts');

  Top:=config.GetValue('/Window/Top',0);
  Left:=config.GetValue('/Window/Left',0);
  Width:=config.GetValue('/Window/Width',1024);
  Height:=config.GetValue('/Window/Height',768);

  aInt:=TDevInterface(config.GetValue('/FilterWheelInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  wheel:=T_indiwheel.Create(nil);
    ASCOM: wheel:=T_ascomwheel.Create(nil);
    INCAMERA: wheel:=T_incamerawheel.Create(nil);
  end;
  wheel.onMsg:=@NewMessage;
  wheel.onDeviceMsg:=@DeviceMessage;
  wheel.onFilterChange:=@FilterChange;
  wheel.onFilterNameChange:=@FilterNameChange;
  wheel.onStatusChange:=@WheelStatus;

  aInt:=TDevInterface(config.GetValue('/FocuserInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  focuser:=T_indifocuser.Create(nil);
    ASCOM: focuser:=T_ascomfocuser.Create(nil);
  end;
  focuser.onMsg:=@NewMessage;
  focuser.onDeviceMsg:=@DeviceMessage;
  focuser.onPositionChange:=@FocuserPositionChange;
  focuser.onSpeedChange:=@FocuserSpeedChange;
  focuser.onTimerChange:=@FocuserTimerChange;
  focuser.onStatusChange:=@FocuserStatus;
  focuser.onTemperatureChange:=@FocuserTemperatureChange;

  aInt:=TDevInterface(config.GetValue('/RotatorInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  rotator:=T_indirotator.Create(nil);
    ASCOM: rotator:=T_ascomrotator.Create(nil);
  end;
  rotator.onMsg:=@NewMessage;
  rotator.onAngleChange:=@RotatorAngleChange;
  rotator.onStatusChange:=@RotatorStatus;

  aInt:=TDevInterface(config.GetValue('/MountInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  mount:=T_indimount.Create(nil);
    ASCOM: mount:=T_ascommount.Create(nil);
  end;
  mount.onMsg:=@NewMessage;
  mount.onDeviceMsg:=@DeviceMessage;
  mount.onCoordChange:=@MountCoordChange;
  mount.onPiersideChange:=@MountPiersideChange;
  mount.onParkChange:=@MountParkChange;
  mount.onStatusChange:=@MountStatus;


  fits:=TFits.Create(self);

  aInt:=TDevInterface(config.GetValue('/CameraInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  camera:=T_indicamera.Create(nil);
    ASCOM: camera:=T_ascomcamera.Create(nil);
  end;
  if wheel.WheelInterface=INCAMERA then wheel.camera:=camera;
  camera.Mount:=mount;
  camera.wheel:=wheel;
  camera.Fits:=fits;
  camera.onMsg:=@NewMessage;
  camera.onDeviceMsg:=@DeviceMessage;
  camera.onExposureProgress:=@CameraProgress;
  camera.onFrameChange:=@FrameChange;
  camera.onTemperatureChange:=@CameraTemperatureChange;
  camera.onCoolerChange:=@CameraCoolerChange;
  camera.onNewImage:=@CameraNewImage;
  camera.onVideoFrame:=@CameraVideoFrame;
  camera.onVideoPreviewChange:=@CameraVideoPreviewChange;
  camera.onVideoSizeChange:=@CameraVideoSizeChange;
  camera.onVideoRateChange:=@CameraVideoRateChange;
  camera.onFPSChange:=@CameraFPSChange;
  camera.onVideoExposureChange:=@CameraVideoExposureChange;
  camera.onStatusChange:=@CameraStatus;
  camera.onCameraDisconnected:=@CameraDisconnected;
  camera.onAbortExposure:=@CameraExposureAborted;

  astrometry:=TAstrometry.Create(nil);
  astrometry.Camera:=camera;
  astrometry.Mount:=mount;
  astrometry.Wheel:=wheel;
  astrometry.Fits:=fits;
  astrometry.onAstrometryStart:=@AstrometryStart;
  astrometry.onAstrometryEnd:=@AstrometryEnd;
  astrometry.onShowMessage:=@NewMessage;

  i:=config.GetValue('/Autoguider/Software',0);
  case TAutoguiderType(i) of
    PHD: autoguider:=T_autoguider_phd.Create;
  end;
  autoguider.onStatusChange:=@AutoguiderStatus;
  autoguider.onConnect:=@AutoguiderConnect;
  autoguider.onDisconnect:=@AutoguiderDisconnect;
  autoguider.onShowMessage:=@NewMessage;

  i:=config.GetValue('/Planetarium/Software',0);
  case TPlanetariumType(i) of
    CDC: planetarium:=TPlanetarium_cdc.Create;
    SAMP:planetarium:=TPlanetarium_samp.Create;
    HNSKY:planetarium:=TPlanetarium_hnsky.Create;
  end;
  planetarium.onConnect:=@PlanetariumConnect;
  planetarium.onDisconnect:=@PlanetariumDisconnect;
  planetarium.onShowMessage:=@NewMessage;

  f_devicesconnection:=Tf_devicesconnection.Create(self);
  f_devicesconnection.onConnect:=@Connect;
  f_devicesconnection.onDisconnect:=@Disconnect;
  f_devicesconnection.ProfileLabel.Caption:='Profile: '+profile;

  f_visu:=Tf_visu.Create(self);
  f_visu.onRedraw:=@Redraw;
  f_visu.onZoom:=@ZoomImage;
  f_visu.onRedrawHistogram:=@RedrawHistogram;

  f_msg:=Tf_msg.Create(self);

  f_frame:=Tf_frame.Create(self);
  f_frame.onSet:=@SetFrame;
  f_frame.onReset:=@ResetFrame;

  f_preview:=Tf_preview.Create(self);
  f_preview.Camera:=camera;
  f_preview.onStartExposure:=@StartPreviewExposure;
  f_preview.onAbortExposure:=@AbortExposure;
  f_preview.onMsg:=@NewMessage;
  f_preview.onEndControlExposure:=@EndControlExposure;
  astrometry.preview:=f_preview;

  f_capture:=Tf_capture.Create(self);
  f_capture.onStartExposure:=@StartCaptureExposure;
  f_capture.onAbortExposure:=@AbortExposure;
  f_capture.onMsg:=@NewMessage;

  f_video:=Tf_video.Create(self);
  f_video.camera:=camera;
  f_video.wheel:=wheel;
  f_video.onMsg:=@NewMessage;

  f_filterwheel:=Tf_filterwheel.Create(self);
  f_filterwheel.onSetFilter:=@SetFilter;

  f_focuser:=Tf_focuser.Create(self);
  f_focuser.onFocusIN:=@FocusIN;
  f_focuser.onFocusOUT:=@FocusOUT;
  f_focuser.onSetAbsolutePosition:=@FocusSetAbsolutePosition;
  f_focuser.onVcurveLearning:=@FocusVcurveLearning;

  f_starprofile:=Tf_starprofile.Create(self);
  f_starprofile.preview:=f_preview;
  f_starprofile.focuser:=f_focuser;
  f_starprofile.onMsg:=@NewMessage;
  f_starprofile.onFocusStart:=@FocusStart;
  f_starprofile.onFocusStop:=@FocusStop;
  f_starprofile.onAutoFocusStart:=@AutoFocusStart;
  f_starprofile.onAutoFocusStop:=@AutoFocusStop;
  f_starprofile.onFocusIN:=@FocusIN;
  f_starprofile.onFocusOUT:=@FocusOUT;
  f_starprofile.onAbsolutePosition:=@FocusSetAbsolutePosition;

  f_ccdtemp:=Tf_ccdtemp.Create(self);
  f_ccdtemp.onSetTemperature:=@SetTemperature;
  f_ccdtemp.onSetCooler:=@SetCooler;

  f_mount:=Tf_mount.Create(self);
  f_mount.onPark:=@SetMountPark;
  f_mount.onTrack:=@SetMountTrack;

  f_rotator:=Tf_rotator.Create(self);
  f_rotator.onRotate:=@RotatorRotate;
  f_rotator.onHalt:=@RotatorHalt;
  f_rotator.onReverse:=@RotatorReverse;

  f_autoguider:=Tf_autoguider.Create(self);
  f_autoguider.onConnect:=@AutoguiderConnectClick;
  f_autoguider.onCalibrate:=@AutoguiderCalibrateClick;
  f_autoguider.onGuide:=@AutoguiderGuideClick;
  f_autoguider.onDither:=@AutoguiderDitherClick;
  f_autoguider.Status.Text:='Disconnected';

  f_sequence:=Tf_sequence.Create(self);
  f_sequence.onMsg:=@NewMessage;
  f_sequence.Preview:=f_preview;
  f_sequence.Capture:=f_capture;
  f_sequence.Filter:=f_filterwheel;
  f_sequence.Mount:=mount;
  f_sequence.Camera:=camera;
  f_sequence.Rotator:=rotator;
  f_sequence.Autoguider:=autoguider;
  f_sequence.Astrometry:=astrometry;
  f_sequence.Planetarium:=planetarium;

  f_planetarium:=Tf_planetarium.Create(self);
  f_planetarium.onConnect:=@PlanetariumConnectClick;
  f_planetarium.onNewTarget:=@PlanetariumNewTarget;
  f_planetarium.Status.Text:='Disconnected';

  f_scriptengine:=Tf_scriptengine.Create(self);
  f_scriptengine.Fits:=fits;
  f_scriptengine.onMsg:=@NewMessage;
  f_scriptengine.onStartSequence:=@StartSequence;
  f_scriptengine.onScriptExecute:=@ScriptExecute;
  f_scriptengine.onScriptAfterExecute:=@ScriptAfterExecute;
  f_scriptengine.onOpenFitsFile:=@LoadFitsFile;
  f_scriptengine.onSaveFitsFile:=@SaveFitsFile;
  f_scriptengine.onOpenReferenceImage:=@OpenRefImage;
  f_scriptengine.onClearReferenceImage:=@ClearRefImage;
  f_scriptengine.onSlewImageCenter:=@ResolveSlewCenter;
  f_scriptengine.onAutomaticAutofocus:=@cmdAutomaticAutofocus;
  f_scriptengine.onAutofocus:=@cmdAutofocus;
  f_scriptengine.DevicesConnection:=f_devicesconnection;
  f_scriptengine.Preview:=f_preview;
  f_scriptengine.Capture:=f_capture;
  f_scriptengine.Ccdtemp:=f_ccdtemp;
  f_scriptengine.Filter:=wheel;
  f_scriptengine.Mount:=mount;
  f_scriptengine.Camera:=camera;
  f_scriptengine.Focuser:=focuser;
  f_scriptengine.Autoguider:=autoguider;
  f_scriptengine.Astrometry:=astrometry;
  f_scriptengine.Planetarium:=planetarium;

  f_script:=Tf_script.Create(self);
  f_script.onMsg:=@NewMessage;
  f_script.Camera:=camera;
  f_script.Preview:=f_preview;
  f_script.Capture:=f_capture;
  f_script.Mount:=mount;
  f_script.Autoguider:=autoguider;
  f_script.Astrometry:=astrometry;
  f_script.LoadScriptList;

  InitCoord;
  SetConfig;
  SetOptions;
  LoadVcurve;
  LoadBPM;

  f_ccdtemp.Setpoint.Text:=config.GetValue('/Temperature/Setpoint','0');
  f_preview.ExpTime.Text:=config.GetValue('/Preview/Exposure','1');
  f_capture.ExpTime.Text:=config.GetValue('/Capture/Exposure','1');
  f_capture.Fname.Text:=config.GetValue('/Capture/FileName','');
  f_capture.SeqNum.Text:=config.GetValue('/Capture/Count','1');

  f_visu.BtnLinear.Checked:=config.GetValue('/Visu/Linear',True);
  f_visu.BtnLog.Checked:=config.GetValue('/Visu/Log',False);
  f_visu.BtnSqrt.Checked:=config.GetValue('/Visu/Sqrt',False);


  ImaBmp:=TBGRABitmap.Create;
  LockMouse:=false;
  ImgCx:=0;
  ImgCy:=0;
  StartX:=0;
  StartY:=0;
  EndX:=0;
  EndY:=0;
  Capture:=false;
  Preview:=false;
  MenuIndiSettings.Enabled:=(camera.CameraInterface=INDI);
  ObsTimeZone:=-GetLocalTimeOffset/60;

  NewMessage('CCDciel '+ccdciel_version+'-'+RevisionStr+' initialized');
end;

procedure Tf_main.FormShow(Sender: TObject);
var str: string;
    i,n: integer;
    posprev,poscapt:integer;
    binprev,bincapt:string;
begin
  if (cdcwcs_initfitsfile=nil)or(cdcwcs_release=nil)or(cdcwcs_sky2xy=nil)or(cdcwcs_xy2sky=nil)or(cdcwcs_getinfo=nil) then begin
     NewMessage('Could not load libcdcwcs'+crlf+'Some astrometry function are not available.');
  end;

  SetTheme;

  PanelBottom.Tag:=PtrInt(MenuTabConnect);
  PanelRight1.Tag:=PtrInt(MenuTabConnect);
  PanelRight2.Tag:=PtrInt(MenuTabFocus);
  PanelRight3.Tag:=PtrInt(MenuTabCapture);
  PanelRight4.Tag:=PtrInt(MenuTabSequence);

  SetTool(f_visu,'Histogram',PanelBottom,0,MenuViewHistogram,MenuHistogram);
  SetTool(f_msg,'Messages',PanelBottom,f_visu.left+1,MenuViewMessages,nil);

  SetTool(f_devicesconnection,'Connection',PanelRight1,0,MenuViewConnection,MenuConnection);
  SetTool(f_preview,'Preview',PanelRight1,f_devicesconnection.top+1,MenuViewPreview,MenuPreview);
  SetTool(f_autoguider,'Autoguider',PanelRight1,f_preview.top+1,MenuViewAutoguider,MenuAutoguider);
  SetTool(f_planetarium,'Planetarium',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium,MenuPlanetarium);
  SetTool(f_script,'Script',PanelRight1,f_planetarium.top+1,MenuViewScript,MenuScript);

  SetTool(f_focuser,'Focuser',PanelRight2,0,MenuViewFocuser,MenuFocuser);
  SetTool(f_starprofile,'Starprofile',PanelRight2,f_focuser.top+1,MenuViewStarProfile,MenuStarProfile);

  SetTool(f_capture,'Capture',PanelRight3,0,MenuViewCapture,MenuCapture);
  SetTool(f_filterwheel,'Filters',PanelRight3,f_capture.top+1,MenuViewFilters,MenuFilters);
  SetTool(f_frame,'Frame',PanelRight3,f_filterwheel.top+1,MenuViewFrame,MenuFrame);
  SetTool(f_rotator,'Rotator',PanelRight3,f_frame.top+1,MenuViewRotator,MenuRotator);
  SetTool(f_ccdtemp,'CCDTemp',PanelRight3,f_rotator.top+1,MenuViewCCDtemp,MenuCCDtemp);
  SetTool(f_mount,'Mount',PanelRight3,f_ccdtemp.top+1,MenuViewMount,MenuMount);

  SetTool(f_sequence,'Sequence',PanelRight4,0,MenuViewSequence,MenuSequence);

  SetTool(f_video,'Video',PanelRight5,0,MenuViewVideo,MenuVideo);

  MenuViewClock.Checked:=config.GetValue('/Tools/Clock/Visible',true);
  MenuViewClockClick(nil);

  for i:=0 to MaxMenulevel do AccelList[i]:='';
  SetMenuAccelerator(MainMenu1.items,0,AccelList);

  StatusBar1.Visible:=false; // bug with statusbar visibility
  StatusbarTimer.Enabled:=true;

  n:=config.GetValue('/Filters/Num',0);
  for i:=0 to MaxFilter do FilterOffset[i]:=0;
  for i:=0 to MaxFilter do FilterExpFact[i]:=1.0;
  FilterList.Clear;
  FilterList.Add(Filter0);
  for i:=1 to n do begin
     FilterOffset[i]:=trunc(config.GetValue('/Filters/Offset'+IntToStr(i),0));
     FilterExpFact[i]:=config.GetValue('/Filters/ExpFact'+IntToStr(i),1.0);
     str:=config.GetValue('/Filters/Filter'+IntToStr(i),'');
     FilterList.Add(str);
  end;
  f_filterwheel.Filters.Items.Assign(FilterList);
  f_filterwheel.Filters.ItemIndex:=0;
  f_EditPlan.Filter.Items.Assign(FilterList);
  SetFilterMenu;

  n:=config.GetValue('/Binning/Num',0);
  BinningList.Clear;
  binprev:=config.GetValue('/Preview/Binning','1x1');
  bincapt:=config.GetValue('/Capture/Binning','1x1');
  posprev:=0;
  poscapt:=0;
  if n>0 then begin
    for i:=0 to n-1 do begin
       str:=config.GetValue('/Binning/Binning'+IntToStr(i),'');
       n:=BinningList.Add(str);
       if str=binprev then posprev:=n;
       if str=bincapt then poscapt:=n;
    end;
  end
  else
    BinningList.Add(Binning0);
  f_preview.Binning.Items.Assign(BinningList);
  f_capture.Binning.Items.Assign(BinningList);
  f_editplan.Binning.Items.Assign(BinningList);
  f_preview.Binning.ItemIndex:=posprev;
  f_capture.Binning.ItemIndex:=poscapt;

  str:=config.GetValue('/Sequence/Targets','');
  if str<>'' then f_sequence.LoadTargets(str);

  f_planetariuminfo.planetarium:=planetarium;

  f_script.SetScriptList(config.GetValue('/Tools/Script/ScriptName',''));

  LoadFocusStar;

  StartupTimer.Enabled:=true;
end;

procedure Tf_main.SetTheme;
var c:TBGRAPixel;
    btn: TPortableNetworkGraphic;
    i:integer;
begin
  // detect if theme color is dark
  c:=ColorToBGRA(ColorToRGB(clBtnFace));
  i:=round((c.red+c.green+c.blue)/3);
  // change imagelist
  if i>=128 then begin
    PageControlRight.Images:=ImageListDay;
    MainMenu1.Images:=ImageListDay;
  end
  else begin
    PageControlRight.Images:=ImageListNight;
    MainMenu1.Images:=ImageListNight;
  end;
  // change individual buttons
  btn := TPortableNetworkGraphic.Create;
  PageControlRight.Images.GetBitmap(5, btn);
  f_visu.BtnZoomAdjust.Glyph.Assign(btn);
  PageControlRight.Images.GetBitmap(6, btn);
  f_visu.BtnBullsEye.Glyph.Assign(btn);
  PageControlRight.Images.GetBitmap(7, btn);
  f_visu.histminmax.Glyph.Assign(btn);
  btn.Free;
end;

procedure Tf_main.StartupTimerTimer(Sender: TObject);
begin
  StartupTimer.Enabled:=false;
  if FOpenSetup then begin
     MenuSetup.Click;
     MenuOptions.Click;
  end
    else f_script.RunStartupScript;
end;

procedure Tf_main.ScriptExecute(Sender: TObject);
begin
  f_script.led.Brush.Color:=clLime;
  if (f_scriptengine.ScriptFilename<>'startup')and
     (f_scriptengine.ScriptFilename<>'shutdown') and
     (f_scriptengine.ScriptFilename<>'unattended_error')
     then
      f_script.ComboBoxScript.Text:=f_scriptengine.ScriptFilename;
end;

procedure Tf_main.ScriptAfterExecute(Sender: TObject);
begin
  f_script.led.Brush.Color:=clGray;
end;

Procedure Tf_main.StartSequence(SeqName: string);
begin
  if f_sequence.Running then exit;
  f_sequence.LoadTargets(slash(ConfigDir)+SeqName+'.targets');
  StartSequenceTimer.Enabled:=true;
end;

procedure Tf_main.StartSequenceTimerTimer(Sender: TObject);
begin
  StartSequenceTimer.Enabled:=false;
  f_sequence.BtnStartClick(nil);
end;

procedure Tf_main.StatusbarTimerTimer(Sender: TObject);
begin
 StatusbarTimer.Enabled:=false;
 StatusBar1.Visible:=true;  // bug with statusbar visibility
end;

procedure Tf_main.MenuResetToolsClick(Sender: TObject);
var i: integer;
begin
  SetTool(f_visu,'',PanelBottom,0,MenuViewHistogram,MenuHistogram);
  SetTool(f_msg,'',PanelBottom,f_visu.left+1,MenuViewMessages,nil);

  SetTool(f_devicesconnection,'',PanelRight1,0,MenuViewConnection,MenuConnection);
  SetTool(f_preview,'',PanelRight1,f_devicesconnection.top+1,MenuViewPreview,MenuPreview);
  SetTool(f_autoguider,'',PanelRight1,f_preview.top+1,MenuViewAutoguider,MenuAutoguider);
  SetTool(f_planetarium,'',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium,MenuPlanetarium);
  SetTool(f_script,'',PanelRight1,f_planetarium.top+1,MenuViewScript,MenuScript);

  SetTool(f_focuser,'',PanelRight2,0,MenuViewFocuser,MenuFocuser);
  SetTool(f_starprofile,'',PanelRight2,f_focuser.top+1,MenuViewStarProfile,MenuStarProfile);

  SetTool(f_capture,'',PanelRight3,0,MenuViewCapture,MenuCapture);
  SetTool(f_filterwheel,'',PanelRight3,f_capture.top+1,MenuViewFilters,MenuFilters);
  SetTool(f_frame,'',PanelRight3,f_filterwheel.top+1,MenuViewFrame,MenuFrame);
  SetTool(f_rotator,'',PanelRight3,f_frame.top+1,MenuViewRotator,MenuRotator);
  SetTool(f_ccdtemp,'',PanelRight3,f_rotator.top+1,MenuViewCCDtemp,MenuCCDtemp);
  SetTool(f_mount,'',PanelRight3,f_ccdtemp.top+1,MenuViewMount,MenuMount);

  SetTool(f_sequence,'',PanelRight4,0,MenuViewSequence,MenuSequence);

  SetTool(f_video,'',PanelRight5,0,MenuViewVideo,MenuVideo);

  for i:=0 to MaxMenulevel do AccelList[i]:='';
  SetMenuAccelerator(MainMenu1.items,0,AccelList);
end;

procedure Tf_main.UpdConfig(oldver:string);
begin
  if oldver<'0.0.1a' then begin
     config.DeletePath('/Tools');
     config.Flush;
  end;
end;

procedure Tf_main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i,n: integer;
begin
  if AppClose then exit;
  AppClose:=true;

  config.SetValue('/Configuration/Version',ccdcielver);

  config.SetValue('/Tools/Connection/Parent',f_devicesconnection.Parent.Name);
  config.SetValue('/Tools/Connection/Visible',f_devicesconnection.Visible);
  config.SetValue('/Tools/Connection/Top',f_devicesconnection.Top);
  config.SetValue('/Tools/Connection/Left',f_devicesconnection.Left);

  config.SetValue('/Tools/Histogram/Parent',f_visu.Parent.Name);
  config.SetValue('/Tools/Histogram/Visible',f_visu.Visible);
  config.SetValue('/Tools/Histogram/Top',f_visu.Top);
  config.SetValue('/Tools/Histogram/Left',f_visu.Left);

  config.SetValue('/Tools/Messages/Parent',f_msg.Parent.Name);
  config.SetValue('/Tools/Messages/Visible',f_msg.Visible);
  config.SetValue('/Tools/Messages/Top',f_msg.Top);
  config.SetValue('/Tools/Messages/Left',f_msg.Left);

  config.SetValue('/Tools/Focuser/Parent',f_focuser.Parent.Name);
  config.SetValue('/Tools/Focuser/Visible',f_focuser.Visible);
  config.SetValue('/Tools/Focuser/Top',f_focuser.Top);
  config.SetValue('/Tools/Focuser/Left',f_focuser.Left);

  config.SetValue('/Tools/Starprofile/Parent',f_starprofile.Parent.Name);
  config.SetValue('/Tools/Starprofile/Visible',f_starprofile.Visible);
  config.SetValue('/Tools/Starprofile/Top',f_starprofile.Top);
  config.SetValue('/Tools/Starprofile/Left',f_starprofile.Left);

  config.SetValue('/Tools/Frame/Parent',f_frame.Parent.Name);
  config.SetValue('/Tools/Frame/Visible',f_frame.Visible);
  config.SetValue('/Tools/Frame/Top',f_frame.Top);
  config.SetValue('/Tools/Frame/Left',f_frame.Left);

  config.SetValue('/Tools/Rotator/Parent',f_rotator.Parent.Name);
  config.SetValue('/Tools/Rotator/Visible',f_rotator.Visible);
  config.SetValue('/Tools/Rotator/Top',f_rotator.Top);
  config.SetValue('/Tools/Rotator/Left',f_rotator.Left);

  config.SetValue('/Tools/Preview/Parent',f_preview.Parent.Name);
  config.SetValue('/Tools/Preview/Visible',f_preview.Visible);
  config.SetValue('/Tools/Preview/Top',f_preview.Top);
  config.SetValue('/Tools/Preview/Left',f_preview.Left);

  config.SetValue('/Tools/Capture/Parent',f_capture.Parent.Name);
  config.SetValue('/Tools/Capture/Visible',f_capture.Visible);
  config.SetValue('/Tools/Capture/Top',f_capture.Top);
  config.SetValue('/Tools/Capture/Left',f_capture.Left);

  config.SetValue('/Tools/Filters/Parent',f_filterwheel.Parent.Name);
  config.SetValue('/Tools/Filters/Visible',f_filterwheel.Visible);
  config.SetValue('/Tools/Filters/Top',f_filterwheel.Top);
  config.SetValue('/Tools/Filters/Left',f_filterwheel.Left);

  config.SetValue('/Tools/CCDTemp/Parent',f_ccdtemp.Parent.Name);
  config.SetValue('/Tools/CCDTemp/Visible',f_ccdtemp.Visible);
  config.SetValue('/Tools/CCDTemp/Top',f_ccdtemp.Top);
  config.SetValue('/Tools/CCDTemp/Left',f_ccdtemp.Left);

  config.SetValue('/Tools/Mount/Parent',f_mount.Parent.Name);
  config.SetValue('/Tools/Mount/Visible',f_mount.Visible);
  config.SetValue('/Tools/Mount/Top',f_mount.Top);
  config.SetValue('/Tools/Mount/Left',f_mount.Left);

  config.SetValue('/Tools/Autoguider/Parent',f_autoguider.Parent.Name);
  config.SetValue('/Tools/Autoguider/Visible',f_autoguider.Visible);
  config.SetValue('/Tools/Autoguider/Top',f_autoguider.Top);
  config.SetValue('/Tools/Autoguider/Left',f_autoguider.Left);

  config.SetValue('/Tools/Planetarium/Parent',f_planetarium.Parent.Name);
  config.SetValue('/Tools/Planetarium/Visible',f_planetarium.Visible);
  config.SetValue('/Tools/Planetarium/Top',f_planetarium.Top);
  config.SetValue('/Tools/Planetarium/Left',f_planetarium.Left);

  config.SetValue('/Tools/Script/Parent',f_script.Parent.Name);
  config.SetValue('/Tools/Script/Visible',f_script.Visible);
  config.SetValue('/Tools/Script/Top',f_script.Top);
  config.SetValue('/Tools/Script/Left',f_script.Left);
  config.SetValue('/Tools/Script/ScriptName',f_script.ComboBoxScript.Text);

  config.SetValue('/Tools/Clock/Visible',MenuViewClock.Checked);

  config.SetValue('/Window/Top',Top);
  config.SetValue('/Window/Left',Left);
  config.SetValue('/Window/Width',Width);
  config.SetValue('/Window/Height',Height);

  config.SetValue('/Temperature/Setpoint',f_ccdtemp.Setpoint.Text);
  config.SetValue('/Preview/Exposure',f_preview.ExpTime.Text);
  config.SetValue('/Preview/Binning',f_preview.Binning.Text);
  config.SetValue('/Capture/Exposure',f_capture.ExpTime.Text);
  config.SetValue('/Capture/Binning',f_capture.Binning.Text);
  config.SetValue('/Capture/FileName',f_capture.Fname.Text);
  config.SetValue('/Capture/Count',f_capture.SeqNum.Text);

  config.SetValue('/Tools/Sequence/Parent',f_sequence.Parent.Name);
  config.SetValue('/Tools/Sequence/Visible',f_sequence.Visible);
  config.SetValue('/Tools/Sequence/Top',f_sequence.Top);
  config.SetValue('/Tools/Sequence/Left',f_sequence.Left);

  config.SetValue('/Sequence/Targets',f_sequence.CurrentFile);

  config.SetValue('/Visu/Linear',f_visu.BtnLinear.Checked);
  config.SetValue('/Visu/Log',f_visu.BtnLog.Checked);
  config.SetValue('/Visu/Sqrt',f_visu.BtnSqrt.Checked);

  n:=FilterList.Count-1;
  config.SetValue('/Filters/Num',n);
  for i:=1 to n do begin
     config.SetValue('/Filters/Filter'+IntToStr(i),FilterList[i]);
     config.SetValue('/Filters/Offset'+IntToStr(i),FilterOffset[i]);
     config.SetValue('/Filters/ExpFact'+IntToStr(i),FilterExpFact[i]);
  end;

  n:=BinningList.Count;
  config.SetValue('/Binning/Num',n);
  for i:=0 to n-1 do begin
     config.SetValue('/Binning/Binning'+IntToStr(i),BinningList[i]);
  end;

  config.SetValue('/Rotator/Reverse',f_rotator.Reverse.Checked);
  config.SetValue('/Rotator/CalibrationAngle',rotator.CalibrationAngle);


  SaveConfig;
  NewMessage('Configuration saved');

  TerminateVcurve:=true;
  if autoguider.Running then begin
    autoguider.Disconnect;
    autoguider.Terminate;
 {$ifndef mswindows}
  end else begin
    autoguider.Free;
 {$endif}
  end;
  if planetarium.Running then begin
    planetarium.Disconnect;
 {$ifndef mswindows}
  end else begin
    planetarium.Free;
 {$endif}
  end;
  if astrometry.Busy then begin
    astrometry.StopAstrometry;
  end;
  wait(2); // time for other thread to terminate
  astrometry.Free;
  CloseAction:=caFree;
end;

procedure Tf_main.FormDestroy(Sender: TObject);
var i: integer;
begin
  camera.Free;
  wheel.Free;
  focuser.Free;
  rotator.Free;
  mount.Free;
  ImaBmp.Free;
  refbmp.Free;
  config.Free;
  FilterList.Free;
  BinningList.Free;
  for i:=1 to MaxScriptDir do ScriptDir[i].Free;
  if NeedRestart then begin
     ExecNoWait(paramstr(0));
     NewMessage('Program restart');
  end
  else NewMessage('Program exit');
  CloseLog;
end;

procedure Tf_main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  case Key of
    VK_F1 : PageControlRight.ActivePageIndex:=0;
    VK_F2 : PageControlRight.ActivePageIndex:=1;
    VK_F3 : PageControlRight.ActivePageIndex:=2;
    VK_F4 : PageControlRight.ActivePageIndex:=3;

  end;
end;

procedure Tf_main.FormResize(Sender: TObject);
begin
  // special tool resizing
  if (f_msg<>nil) then f_msg.FrameResize(Sender);
end;

procedure Tf_main.Image1DblClick(Sender: TObject);
var x,y: integer;
begin
 if fits.HeaderInfo.valid and (not f_starprofile.AutofocusRunning) then begin
   Screen2fits(Mx,My,x,y);
   f_starprofile.showprofile(fits.image,fits.imageC,fits.imageMin,x,y,Starwindow div camera.BinX,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,mount.FocaleLength,camera.PixelSize);
   Image1.Invalidate;
 end;
end;

procedure Tf_main.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
MouseDownX:=X;
MouseDownY:=Y;
if Shift=[ssLeft] then begin
   if ImgZoom>0 then begin
     Mx:=X;
     My:=y;
     MouseMoving:=true;
     screen.Cursor:=crHandPoint;
   end;
 end else if ssShift in Shift then begin
   if EndX>0 then begin
     Image1.Canvas.Pen.Color:=clWhite;
     Image1.Canvas.Pen.Mode:=pmXor;
     Image1.Canvas.Frame(StartX,StartY,EndX,EndY);
   end;
   MouseFrame:=true;
   Startx:=X;
   Starty:=y;
   EndX:=-1;
   EndY:=-1
 end;

end;

procedure Tf_main.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xx,yy,n: integer;
    val,xxc,yyc,rc,s:integer;
    sval:string;
    ra,de: double;
    c: TcdcWCScoord;
    bg,xc,yc,hfd,fwhm,vmax: double;
begin
if LockMouse then exit;
 if MouseMoving and fits.HeaderInfo.valid then begin
    LockMouse:=true;
    ImgCx:=ImgCx+round((X-Mx) / ImgZoom);
    ImgCy:=ImgCy+round((Y-My) / ImgZoom);
    PlotImage;
    LockMouse:=false;
 end
 else if MouseFrame then begin
    Image1.Canvas.Pen.Color:=clWhite;
    Image1.Canvas.Pen.Mode:=pmXor;
    if EndX>0 then begin
       Image1.Canvas.Frame(StartX,StartY,EndX,EndY);
    end;
    EndX:=X;
    EndY:=Y;
    Image1.Canvas.Frame(StartX,StartY,EndX,EndY);
 end
 else if (fits.HeaderInfo.naxis1>0)and(ImgScale0<>0) then begin
    Screen2fits(x,y,xx,yy);
    if (xx>0)and(xx<fits.HeaderInfo.naxis1)and(yy>0)and(yy<fits.HeaderInfo.naxis2) then
       if fits.HeaderInfo.naxis=2 then begin
         val:=trunc(fits.imageMin+fits.image[0,yy,xx]/fits.imageC);
         sval:=inttostr(val);
       end
       else if (fits.HeaderInfo.naxis=3)and(fits.HeaderInfo.naxis3=3) then begin
         val:=trunc(fits.imageMin+fits.image[0,yy,xx]/fits.imageC);
         sval:=inttostr(val);
         val:=trunc(fits.imageMin+fits.image[1,yy,xx]/fits.imageC);
         sval:=sval+'/'+inttostr(val);
         val:=trunc(fits.imageMin+fits.image[2,yy,xx]/fits.imageC);
         sval:=sval+'/'+inttostr(val);
       end
    else sval:='';
    s:=Starwindow div camera.BinX;
    if (xx>s)and(xx<(fits.HeaderInfo.naxis1-s))and(yy>s)and(yy<(fits.HeaderInfo.naxis2-s)) then begin
      f_starprofile.FindStarPos(fits.image,fits.imageC,fits.imageMin,xx,yy,s,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,xxc,yyc,rc,vmax,bg);
      if vmax>0 then begin
        f_starprofile.GetHFD(fits.image,fits.imageC,fits.imageMin,xxc,yyc,rc,bg,xc,yc,hfd,fwhm,vmax);
        if hfd>0.8 then begin
           sval:=sval+' hfd='+FormatFloat(f1,hfd)+' fwhm='+FormatFloat(f1,fwhm);
        end;
      end;
    end;
    if fits.HeaderInfo.solved and (cdcWCSinfo.secpix<>0) then begin
      c.x:=xx;
      c.y:=cdcWCSinfo.hp-yy;
      n:=cdcwcs_xy2sky(@c,0);
      if n=0 then begin
        ra:=c.ra;
        de:=c.dec;
        if mount.Equinox=0 then begin
          ra:=deg2rad*ra;
          de:=deg2rad*de;
          J2000ToApparent(ra,de);
          ra:=rad2deg*ra;
          de:=rad2deg*de;
        end;
        StatusBar1.Panels[1].Text:=ARToStr3(ra/15)+' '+DEToStr(de);
      end;
    end;
    if camera.VerticalFlip then yy:=img_Height-yy;
    StatusBar1.Panels[0].Text:=inttostr(xx)+'/'+inttostr(yy)+': '+sval;
 end;
Mx:=X;
My:=Y;
end;

procedure Tf_main.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xx,x1,y1,x2,y2,w,h: integer;
begin
if MouseMoving and fits.HeaderInfo.valid then begin
  ImgCx:=ImgCx+X-Mx;
  ImgCy:=ImgCy+Y-My;
  PlotImage;
  Mx:=X;
  My:=Y;
end;
if MouseFrame and fits.HeaderInfo.valid then begin
  Image1.Canvas.Pen.Color:=clBlack;
  Image1.Canvas.Pen.Mode:=pmCopy;
  EndX:=X;
  EndY:=Y;
  Screen2CCD(StartX,StartY,camera.VerticalFlip,x1,y1);
  Screen2CCD(EndX,EndY,camera.VerticalFlip,x2,y2);
  if camera.CameraInterface=INDI then begin
    // INDI frame in unbinned pixel
    x1:=x1*camera.BinX;
    x2:=x2*camera.BinX;
    y1:=y1*camera.BinY;
    y2:=y2*camera.BinY;
  end;
  if x1>x2 then begin
    xx:=x1; x1:=x2; x2:=xx;
  end;
  if y1>y2 then begin
    xx:=y1; y1:=y2; y2:=xx;
  end;
  w:=x2-x1;
  h:=y2-y1;
  f_frame.FX.Text:=inttostr(x1);
  f_frame.FY.Text:=inttostr(y1);
  f_frame.FWidth.Text:=inttostr(w);
  f_frame.FHeight.Text:=inttostr(h);
end;
MouseMoving:=false;
MouseFrame:=false;
screen.Cursor:=crDefault;
end;

procedure Tf_main.Image1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  zf: double;
begin
if fits.HeaderInfo.naxis>0 then begin
  if LockMouseWheel then
    exit;
  LockMouseWheel := True;
  try
    handled := True;
    if wheeldelta > 0 then
      zf := 1.25
    else
      zf := 0.8;
    if ImgZoom=0 then imgzoom:=ZoomMin;
    ImgZoom:=ImgZoom*zf;
    if ImgZoom>ZoomMax then ImgZoom:=ZoomMax;
    if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
    PlotImage;
    Application.ProcessMessages;
  finally
    LockMouseWheel := False;
  end;
end;
end;

procedure Tf_main.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=false;
  // Thing to do after all devices are connected
  SetCameraActiveDevices;
  ShowTemperatureRange;
  ShowExposureRange;
  ShowBinningRange;
  ShowFrameRange;
  SetFocusMode;
end;

procedure Tf_main.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
if AppClose then exit;
if (camera.Status<>devDisconnected)and(ConfirmClose) then begin
   CanClose:=(MessageDlg('The camera is connected. Do you want to exit the program now?',mtConfirmation,mbYesNo,0)=mrYes);
end else begin
   CanClose:=true;
end;
if CanClose then begin
 TerminateVcurve:=true;
 if f_capture.Running or f_preview.Running then begin
   AbortExposure(nil);
 end;
 if f_video.Running then begin
  Camera.StopVideoPreview;
 end;
 if f_sequence.Running then f_sequence.AbortSequence;
 f_script.RunShutdownScript;
 NewMessage('Disconnecting devices ...');
 Disconnect(nil);
end;
end;

procedure Tf_main.Image1Resize(Sender: TObject);
begin
  image1.Picture.Bitmap.SetSize(image1.Width,image1.Height);
  ClearImage;
  DrawImage;
end;

procedure Tf_main.MenuAutoguiderCalibrateClick(Sender: TObject);
begin
 AutoguiderCalibrateClick(Sender);
end;

procedure Tf_main.MenuAutoguiderConnectClick(Sender: TObject);
begin
 AutoguiderConnectClick(Sender);
end;

procedure Tf_main.MenuAutoguiderDitherClick(Sender: TObject);
begin
 AutoguiderDitherClick(Sender);
end;

procedure Tf_main.MenuAutoguiderGuideClick(Sender: TObject);
begin
 AutoguiderGuideClick(Sender);
end;

procedure Tf_main.MenuBPMClick(Sender: TObject);
var bin,x,y,i: integer;
    lb,val,val1,val2: double;
begin
f_pause.Caption:='Bad pixel map';
f_pause.Text:='Cover the camera and set the exposure time and binning in the Preview pane now.'+crlf+'Click Continue when ready.';
if f_pause.Wait then begin
  bin:=f_preview.Bin;
  camera.ResetFrame;
  Camera.FrameType:=DARK;
  fits.SetBPM(bpm,0,0,0,0);
  f_preview.ControlExposure(f_preview.Exposure,bin,bin);
  lb:=fits.imageMean+BPMsigma*fits.imageSigma;
  if lb>MAXWORD then lb:=MAXWORD/2;
  bpmNum:=0;
  bpmX:=fits.HeaderInfo.naxis1;
  bpmY:=fits.HeaderInfo.naxis2;
  bpmAxis:=fits.HeaderInfo.naxis;
  for x:=0 to fits.HeaderInfo.naxis1-1 do begin
     for y:=0 to fits.HeaderInfo.naxis2-1 do begin
        val:=trunc(fits.imageMin+fits.image[0,y,x]/fits.imageC);
        if fits.HeaderInfo.naxis=3 then begin
          val1:=trunc(fits.imageMin+fits.image[1,y,x]/fits.imageC);
          val2:=trunc(fits.imageMin+fits.image[2,y,x]/fits.imageC);
          val:=maxvalue([val,val1,val2]);
        end;
        if val>lb then begin
           if bpmnum<1000 then begin
             inc(bpmNum);
             bpm[bpmnum,1]:=x;
             bpm[bpmnum,2]:=y;
           end;
        end;
     end;
  end;
  if bpmnum<1000 then
     NewMessage('Bad pixel detection found '+inttostr(bpmNum)+' hot pixels.')
  else begin
     NewMessage('Too many hot pixel found!');
     NewMessage('Please increase the threshold.');
  end;
  config.DeletePath('/BadPixelMap/');
  config.SetValue('/BadPixelMap/Count',bpmNum);
  config.SetValue('/BadPixelMap/CCDWidth',bpmX);
  config.SetValue('/BadPixelMap/CCDHeight',bpmY);
  config.SetValue('/BadPixelMap/CCDAxis',bpmAxis);
  for i:=1 to bpmnum do begin
    config.SetValue('/BadPixelMap/BPMX'+IntToStr(i),bpm[i,1]);
    config.SetValue('/BadPixelMap/BPMY'+IntToStr(i),bpm[i,2]);
  end;
  SaveConfig;
end;
end;

procedure Tf_main.MenuClearBPMClick(Sender: TObject);
begin
  bpmNum:=0;
  bpmX:=0;
  bpmY:=0;
  bpmAxis:=0;
  NewMessage('Bad pixel map cleared.');
  fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  config.DeletePath('/BadPixelMap/');
  config.SetValue('/BadPixelMap/Count',bpmNum);
  config.SetValue('/BadPixelMap/CCDWidth',bpmX);
  config.SetValue('/BadPixelMap/CCDHeight',bpmY);
  config.SetValue('/BadPixelMap/CCDAxis',bpmAxis);
  SaveConfig;
end;


procedure Tf_main.LoadBPM;
var i:integer;
begin
 bpmNum:=config.GetValue('/BadPixelMap/Count',0);
 bpmX:=config.GetValue('/BadPixelMap/CCDWidth',0);
 bpmY:=config.GetValue('/BadPixelMap/CCDHeight',0);
 bpmAxis:=config.GetValue('/BadPixelMap/CCDAxis',0);
 for i:=1 to bpmnum do begin
   bpm[i,1]:=round(config.GetValue('/BadPixelMap/BPMX'+IntToStr(i),0));
   bpm[i,2]:=round(config.GetValue('/BadPixelMap/BPMY'+IntToStr(i),0));
 end;
end;

procedure Tf_main.MenuCaptureStartClick(Sender: TObject);
begin
  f_capture.BtnStart.Click;
end;

procedure Tf_main.MenuCCDtempSetClick(Sender: TObject);
begin
  SetTemperature(Sender);
end;

procedure Tf_main.SetConfig;
begin
case camera.CameraInterface of
   INDI : CameraName:=config.GetValue('/INDIcamera/Device','');
   ASCOM: CameraName:=config.GetValue('/ASCOMcamera/Device','');
end;
case wheel.WheelInterface of
   INCAMERA: WheelName:=CameraName;
   INDI : WheelName:=config.GetValue('/INDIwheel/Device','');
   ASCOM: WheelName:=config.GetValue('/ASCOMwheel/Device','');
end;
case focuser.FocuserInterface of
   INDI : FocuserName:=config.GetValue('/INDIfocuser/Device','');
   ASCOM: FocuserName:=config.GetValue('/ASCOMfocuser/Device','');
end;
case rotator.RotatorInterface of
   INDI : RotatorName:=config.GetValue('/INDIrotator/Device','');
   ASCOM: RotatorName:=config.GetValue('/ASCOMrotator/Device','');
end;
case mount.MountInterface of
   INDI : MountName:=config.GetValue('/INDImount/Device','');
   ASCOM: MountName:=config.GetValue('/ASCOMmount/Device','');
end;
wheel.AutoLoadConfig:=config.GetValue('/INDIwheel/AutoLoadConfig',false);
focuser.AutoLoadConfig:=config.GetValue('/INDIfocuser/AutoLoadConfig',false);
rotator.AutoLoadConfig:=config.GetValue('/INDIrotator/AutoLoadConfig',false);
mount.AutoLoadConfig:=config.GetValue('/INDImount/AutoLoadConfig',false);
camera.AutoLoadConfig:=config.GetValue('/INDIcamera/AutoLoadConfig',false);
DeviceTimeout:=config.GetValue('/Devices/Timeout',100);
camera.Timeout:=DeviceTimeout;
focuser.Timeout:=DeviceTimeout;
rotator.Timeout:=DeviceTimeout;
wheel.Timeout:=DeviceTimeout;
mount.Timeout:=DeviceTimeout;
end;

procedure Tf_main.SetOptions;
var i,n: integer;
begin
  ObsLatitude:=config.GetValue('/Info/ObservatoryLatitude',46.0);
  ObsLongitude:=config.GetValue('/Info/ObservatoryLongitude',-6.0);
  BayerColor:=config.GetValue('/Color/Bayer',false);
  BayerMode:=TBayerMode(config.GetValue('/Color/BayerMode',0));
  RedBalance:=config.GetValue('/Color/RedBalance',1.0);
  GreenBalance:=config.GetValue('/Color/GreenBalance',1.0);
  BlueBalance:=config.GetValue('/Color/BlueBalance',1.0);
  reftreshold:=config.GetValue('/RefImage/Treshold',128);
  refcolor:=config.GetValue('/RefImage/Color',0);
  BPMsigma:=config.GetValue('/BadPixel/Sigma',5);
  MaxVideoPreviewRate:=config.GetValue('/Video/PreviewRate',5);
  TemperatureSlope:=config.GetValue('/Cooler/TemperatureSlope',0);
  Starwindow:=config.GetValue('/StarAnalysis/Window',20);
  Focuswindow:=config.GetValue('/StarAnalysis/Focus',200);
  n:=config.GetValue('/Filters/Num',0);
  for i:=0 to MaxFilter do FilterOffset[i]:=0;
  for i:=0 to MaxFilter do FilterExpFact[i]:=1.0;
  for i:=1 to n do begin
     FilterOffset[i]:=trunc(config.GetValue('/Filters/Offset'+IntToStr(i),0));
     FilterExpFact[i]:=config.GetValue('/Filters/ExpFact'+IntToStr(i),1.0);
     if wheel.Filter=i then CurrentFilterOffset:=FilterOffset[i];
  end;
  AutofocusExposureFact:=FilterExpFact[wheel.Filter];
  AutoFocusMode:=TAutoFocusMode(config.GetValue('/StarAnalysis/AutoFocusMode',3));
  AutofocusMinSpeed:=config.GetValue('/StarAnalysis/AutofocusMinSpeed',500);
  AutofocusMaxSpeed:=config.GetValue('/StarAnalysis/AutofocusMaxSpeed',5000);
  AutofocusStartHFD:=config.GetValue('/StarAnalysis/AutofocusStartHFD',20.0);
  AutofocusNearHFD:=config.GetValue('/StarAnalysis/AutofocusNearHFD',10.0);
  AutofocusExposure:=config.GetValue('/StarAnalysis/AutofocusExposure',5.0);
  AutofocusBinning:=config.GetValue('/StarAnalysis/AutofocusBinning',1);
  FocuserBacklash:=config.GetValue('/StarAnalysis/FocuserBacklash',0);
  FocuserTempCoeff:=config.GetValue('/StarAnalysis/FocuserTempCoeff',0.0);
  AutofocusMoveDir:=config.GetValue('/StarAnalysis/AutofocusMoveDir',FocusDirIn);
  AutofocusNearNum:=config.GetValue('/StarAnalysis/AutofocusNearNum',3);
  AutofocusMeanNumPoint:=config.GetValue('/StarAnalysis/AutofocusMeanNumPoint',7);
  AutofocusMeanMovement:=config.GetValue('/StarAnalysis/AutofocusMeanMovement',100);
  AutofocusTolerance:=config.GetValue('/StarAnalysis/AutofocusTolerance',99.0);
  AutofocusMinSNR:=config.GetValue('/StarAnalysis/AutofocusMinSNR',0.0);
  LogToFile:=config.GetValue('/Log/Messages',true);
  if LogToFile<>LogFileOpen then CloseLog;
  DitherPixel:=config.GetValue('/Autoguider/Dither/Pixel',1.0);
  DitherRAonly:=config.GetValue('/Autoguider/Dither/RAonly',true);
  SettlePixel:=config.GetValue('/Autoguider/Settle/Pixel',1.0);
  SettleMinTime:=config.GetValue('/Autoguider/Settle/MinTime',5);
  SettleMaxTime:=config.GetValue('/Autoguider/Settle/MaxTime',30);
  CalibrationDelay:=config.GetValue('/Autoguider/Settle/CalibrationDelay',300);
  MeridianOption:=config.GetValue('/Meridian/MeridianOption',0);
  MinutesPastMeridian:=config.GetValue('/Meridian/MinutesPast',0);
  MinutesPastMeridianMin:=config.GetValue('/Meridian/MinutesPastMin',0);
  MeridianFlipPauseBefore:=config.GetValue('/Meridian/MeridianFlipPauseBefore',false);
  MeridianFlipPauseAfter:=config.GetValue('/Meridian/MeridianFlipPauseAfter',false);
  MeridianFlipPauseTimeout:=config.GetValue('/Meridian/MeridianFlipPauseTimeout',0);
  MeridianFlipCalibrate:=config.GetValue('/Meridian/MeridianFlipCalibrate',false);
  MeridianFlipAutofocus:=config.GetValue('/Meridian/MeridianFlipAutofocus',false);
  astrometryResolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
  if (autoguider<>nil)and(autoguider.State<>GUIDER_DISCONNECTED) then autoguider.SettleTolerance(SettlePixel,SettleMinTime, SettleMaxTime);
  if refmask then SetRefImage;
  if f_focuser<>nil then f_focuser.BtnVcurve.Visible:=(AutoFocusMode=afVcurve);
  LoadHorizon(config.GetValue('/Info/HorizonFile',''));
  ElevationMin:=config.GetValue('/Info/ElevationMin',10);
end;

procedure Tf_main.SaveConfig;
var inif:TIniFile;
begin
  config.Flush;
  inif:=TIniFile.Create(slash(ConfigDir)+'ccdciel.rc');
  inif.WriteString('main','profile',profile);
  inif.UpdateFile;
  inif.Free;
end;

procedure Tf_main.OpenConfig(n: string);
var configver: string;
begin
 config.Filename:=slash(ConfigDir)+n;
 configver:=config.GetValue('/Configuration/Version','');
 UpdConfig(configver);
end;

Procedure Tf_main.Connect(Sender: TObject);
begin
  WantCamera:=true;
  WantWheel:=config.GetValue('/Devices/FilterWheel',false);
  WantFocuser:=config.GetValue('/Devices/Focuser',false);;
  WantRotator:=config.GetValue('/Devices/Rotator',false);;
  WantMount:=config.GetValue('/Devices/Mount',false);;

  if WantCamera and (CameraName='') then begin
    ShowMessage('Please configure your camera!');
    MenuSetup.Click;
    exit;
  end;
  if WantWheel and (WheelName='') then begin
    ShowMessage('Please configure your filter wheel!');
    MenuSetup.Click;
    exit;
  end;
  if WantFocuser and (FocuserName='') then begin
    ShowMessage('Please configure your focuser!');
    MenuSetup.Click;
    exit;
  end;
  if WantRotator and (RotatorName='') then begin
    ShowMessage('Please configure your rotator!');
    MenuSetup.Click;
    exit;
  end;
  if WantMount and (MountName='') then begin
    ShowMessage('Please configure your mount!');
    MenuSetup.Click;
    exit;
  end;

  f_devicesconnection.LabelCamera.Visible:=WantCamera;
  f_devicesconnection.LabelWheel.Visible:=WantWheel;
  f_devicesconnection.LabelFocuser.Visible:=WantFocuser;
  f_devicesconnection.LabelRotator.Visible:=WantRotator;
  f_devicesconnection.LabelMount.Visible:=WantMount;
  f_devicesconnection.PanelDev.Visible:=true;

  if WantCamera  then ConnectCamera(Sender);
  if WantWheel   then ConnectWheel(Sender);
  if WantFocuser then ConnectFocuser(Sender);
  if WantRotator then ConnectRotator(Sender);
  if WantMount   then ConnectMount(Sender);
end;

Procedure Tf_main.Disconnect(Sender: TObject);
begin
if camera.Status<>devDisconnected then begin
   if (sender=nil) or (MessageDlg('Are you sure you want to disconnect all the devices now?',mtConfirmation,mbYesNo,0)=mrYes) then begin
     camera.AbortExposure;
     StartCaptureTimer.Enabled:=false;
     f_preview.stop;
     f_capture.stop;
     Capture:=false;
     StatusBar1.Panels[1].Text:='';
     DisconnectCamera(Sender);
     DisconnectWheel(Sender);
     DisconnectFocuser(Sender);
     DisconnectRotator(Sender);
     DisconnectMount(Sender);
   end;
end;
end;

Procedure Tf_main.CheckConnectionStatus;
var allcount, upcount, downcount, concount: integer;
procedure SetDisconnected;
begin
 f_devicesconnection.led.Brush.Color:=clRed;
 f_devicesconnection.BtnConnect.Caption:='Connect';
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;
procedure SetConnected;
begin
 f_devicesconnection.led.Brush.Color:=clLime;
 f_devicesconnection.BtnConnect.Caption:='Disconnect';
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;
procedure SetConnecting;
begin
 f_devicesconnection.led.Brush.Color:=clYellow;
 f_devicesconnection.BtnConnect.Caption:='Disconnect';
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;

begin
allcount:=0; upcount:=0; downcount:=0; concount:=0;
 if WantCamera then begin
  inc(allcount);
  case camera.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantWheel then begin
  inc(allcount);
  case wheel.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantFocuser then begin
  inc(allcount);
  case focuser.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantRotator then begin
  inc(allcount);
  case rotator.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantMount then begin
  inc(allcount);
  case mount.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if allcount=0 then SetDisconnected
 else if (upcount=allcount) then begin
   SetConnected;
   ConnectTimer.Enabled:=true;
 end
 else if (concount>0)or(upcount>0) then SetConnecting
 else SetDisconnected;
end;

Procedure Tf_main.ConnectCamera(Sender: TObject);
begin
   case camera.CameraInterface of
    INDI : begin
           camera.IndiTransfert:=TIndiTransfert(config.GetValue('/INDIcamera/IndiTransfert',ord(itNetwork)));
           camera.IndiTransfertDir:=config.GetValue('/INDIcamera/IndiTransfertDir','/tmp');
           camera.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDIcamera/Device',''),
                          config.GetValue('/INDIcamera/Sensor','CCD1'),
                          config.GetValue('/INDIcamera/DevicePort',''));
           end;
    ASCOM: camera.Connect(config.GetValue('/ASCOMcamera/Device',''));
  end;
end;

Procedure Tf_main.DisconnectCamera(Sender: TObject);
begin
 camera.Disconnect;
end;

procedure Tf_main.SetCameraActiveDevices;
var fn,wn,mn: string;
begin
 if WantFocuser then fn:=FocuserName else fn:='';
 if WantWheel then wn:=WheelName else wn:='';
 if WantMount then mn:=MountName else mn:='';
 camera.SetActiveDevices(fn,wn,mn);
end;

procedure Tf_main.ShowTemperatureRange;
var buf: string;
begin
  f_ccdtemp.Current.Text:=FormatFloat(f1,camera.Temperature);
  buf:=FormatFloat(f0,camera.TemperatureRange.min)+'...'+FormatFloat(f0,camera.TemperatureRange.max);
  f_ccdtemp.Setpoint.Hint:='Desired temperature'+crlf+buf;
end;

procedure Tf_main.SetTemperature(Sender: TObject);
var t: double;
begin
  t:=StrToFloatDef(f_ccdtemp.Setpoint.Text,-1000);
  if t<>-1000 then begin
     camera.Temperature:=t;
  end;
end;

procedure Tf_main.SetCooler(Sender: TObject);
var onoff,coolerstatus: boolean;
begin
  onoff:=f_ccdtemp.CCDcooler.Checked;
  coolerstatus:=camera.Cooler;
  if coolerstatus<>onoff then begin
    camera.Cooler:=onoff;
    if onoff then SetTemperature(Sender);
  end;
end;

procedure Tf_main.ShowExposureRange;
var buf: string;
begin
 buf:=FormatFloat(f0,camera.ExposureRange.min)+'...'+FormatFloat(f0,camera.ExposureRange.max);
 buf:='Exposure time in secondes'+crlf+buf;
 f_capture.ExpTime.Hint:=buf;
 f_preview.ExpTime.Hint:=buf;
end;

procedure Tf_main.ShowFrame;
var x,y,w,h: integer;
begin
 camera.GetFrame(x,y,w,h);
 if (x<>FrameX)or(y<>FrameY)or(w<>FrameW)or(h<>FrameH) then begin
   FrameX:=x;
   FrameY:=y;
   FrameW:=w;
   FrameH:=h;
   f_frame.FX.Text:=inttostr(FrameX);
   f_frame.FY.Text:=inttostr(FrameY);
   f_frame.FWidth.Text:=inttostr(FrameW);
   f_frame.FHeight.Text:=inttostr(FrameH);
   NewMessage('Camera frame x='+f_frame.FX.Text+' y='+f_frame.FY.Text+' width='+f_frame.FWidth.Text+' height='+f_frame.FHeight.Text);
 end;
end;

procedure Tf_main.ShowFrameRange;
var rx,ry,rw,rh:TNumRange;
begin
 camera.GetFrameRange(rx,ry,rw,rh);
 f_frame.FX.Hint:=FormatFloat(f0,rx.min)+'...'+FormatFloat(f0,rx.max);
 f_frame.FY.Hint:=FormatFloat(f0,ry.min)+'...'+FormatFloat(f0,ry.max);
 f_frame.FWidth.Hint:=FormatFloat(f0,rw.min)+'...'+FormatFloat(f0,rw.max);
 f_frame.FHeight.Hint:=FormatFloat(f0,rh.min)+'...'+FormatFloat(f0,rh.max);
 ShowFrame;
end;

Procedure Tf_main.FrameChange(Sender: TObject);
begin
 ShowFrame;
end;

procedure Tf_main.SetFrame(Sender: TObject);
var x,y,w,h: integer;
begin
  x:=StrToIntDef(f_frame.FX.Text,-1);
  y:=StrToIntDef(f_frame.FY.Text,-1);
  w:=StrToIntDef(f_frame.FWidth.Text,-1);
  h:=StrToIntDef(f_frame.FHeight.Text,-1);
  if (x<0)or(y<0)or(w<0)or(h<0) then
     NewMessage('Invalid frame values')
  else
     camera.SetFrame(x,y,w,h);
end;

procedure Tf_main.ResetFrame(Sender: TObject);
begin
  camera.ResetFrame;
end;

procedure Tf_main.ShowBinningRange;
var rxmin,rxmax,rxstep,rymin,rymax,rystep: integer;
    i,j,n,posprev,poscapt:integer;
    binstr,binprev,bincapt:string;
begin
 binprev:=config.GetValue('/Preview/Binning','1x1');
 bincapt:=config.GetValue('/Capture/Binning','1x1');
 posprev:=0;
 poscapt:=0;
 rxmin:=round(camera.BinXrange.min);
 rxmax:=round(camera.BinXrange.max);
 rxstep:=round(camera.BinXrange.step);
 rymin:=round(camera.BinYrange.min);
 rymax:=round(camera.BinYrange.max);
 rystep:=round(camera.BinYrange.step);
 if rxmin<1 then rxmin:=1;
 if rxmax<rxmin then rxmax:=rxmin;
 if rxmax>8 then rxmax:=8;
 if rxstep<1 then rxstep:=1;
 if rymin<1 then rymin:=1;
 if rymax<rxmin then rymax:=rymin;
 if rymax>8 then rymax:=8;
 if rystep<1 then rystep:=1;
 BinningList.Clear;
 i:=rxmin;
 while i<=rxmax do begin
   j:=rymin;
   while j<=rymax do begin
     if i=j then begin  // only "square" binning in combobox list
       binstr:=inttostr(i)+'x'+inttostr(j);
       n:=BinningList.Add(binstr);
       if binstr=binprev then posprev:=n;
       if binstr=bincapt then poscapt:=n;
     end;
     inc(j,rystep);
   end;
   inc(i,rxstep);
 end;
 f_preview.Binning.Items.Assign(BinningList);
 f_capture.Binning.Items.Assign(BinningList);
 f_editplan.Binning.Items.Assign(BinningList);
 f_preview.Binning.ItemIndex:=posprev;
 f_capture.Binning.ItemIndex:=poscapt;
 f_editplan.Binning.ItemIndex:=0;
end;

Procedure Tf_main.ConnectWheel(Sender: TObject);
begin
  case wheel.WheelInterface of
    INCAMERA : wheel.Connect('');
    INDI : wheel.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDIwheel/Device',''),
                          config.GetValue('/INDIwheel/DevicePort',''));
    ASCOM: wheel.Connect(config.GetValue('/ASCOMwheel/Device',''));
  end;
end;

Procedure Tf_main.DisconnectWheel(Sender: TObject);
begin
wheel.Disconnect;
end;

Procedure Tf_main.ConnectFocuser(Sender: TObject);
begin
  case focuser.FocuserInterface of
    INDI : focuser.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDIfocuser/Device',''),
                          config.GetValue('/INDIfocuser/DevicePort',''));
    ASCOM: focuser.Connect(config.GetValue('/ASCOMfocuser/Device',''));
  end;
end;

Procedure Tf_main.DisconnectFocuser(Sender: TObject);
begin
focuser.Disconnect;
end;

procedure Tf_main.SetFocusMode;
var r: TNumRange;
begin
   FocuserTemp:=focuser.Temperature; // first call to test ascom property
   FocuserLastTemp:=FocuserTemp;
   if focuser.hasTemperature then begin
      f_focuser.PanelTemp.Visible:=true;
      f_focuser.Temp.Text:=FormatFloat(f1,focuser.Temperature);
   end
   else
      f_focuser.PanelTemp.Visible:=false;
  if focuser.hasAbsolutePosition then begin
     f_focuser.Notebook1.PageIndex:=2;
  end
  else if focuser.hasRelativePosition then begin
     f_focuser.Notebook1.PageIndex:=1;
  end
  else begin
     f_focuser.Notebook1.PageIndex:=0;
  end;
  f_focuser.Position.Text:=inttostr(focuser.Position);
  FocuserPositionMin:=0;
  FocuserPositionMax:=MAXWORD;
  r:=focuser.PositionRange;
  if r.step>0 then begin
   FocuserPositionMin:=round(r.min);
   FocuserPositionMax:=round(r.max);
   f_focuser.Position.Hint:='Current focuser absolute position, '+
                   IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
    f_focuser.PosIncr.ItemIndex:=0;
  end;
  f_focuser.speed.Text:=inttostr(focuser.Speed);
  f_focuser.timer.Text:=inttostr(focuser.Timer);
  r:=focuser.RelPositionRange;
  if r.step>0 then begin
    f_focuser.RelIncr.Hint:='Relative increment for the inward or outward movement, '+
                    IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
    f_focuser.RelIncr.ItemIndex:=0;
  end;
end;

Procedure Tf_main.ConnectRotator(Sender: TObject);
begin
  case rotator.RotatorInterface of
    INDI : rotator.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDIrotator/Device',''),
                          config.GetValue('/INDIrotator/DevicePort',''));
    ASCOM: rotator.Connect(config.GetValue('/ASCOMrotator/Device',''));
  end;
end;

Procedure Tf_main.DisconnectRotator(Sender: TObject);
begin
 rotator.Disconnect;
end;

Procedure Tf_main.ConnectMount(Sender: TObject);
begin
  case mount.MountInterface of
    INDI : mount.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDImount/Device',''),
                          config.GetValue('/INDImount/DevicePort',''));
    ASCOM: mount.Connect(config.GetValue('/ASCOMmount/Device',''));
  end;
end;

Procedure Tf_main.DisconnectMount(Sender: TObject);
begin
mount.Disconnect;
end;

procedure Tf_main.NewMessage(msg: string);
begin
 if msg<>'' then begin
  if f_msg.msg.Lines.Count>100 then f_msg.msg.Lines.Delete(0);
  f_msg.msg.Lines.Add(FormatDateTime('hh:nn:ss',now)+':'+msg);
  f_msg.msg.SelStart:=f_msg.msg.GetTextLen-1;
  f_msg.msg.SelLength:=0;
  if LogToFile then begin
    WriteLog(msg);
  end;
 end;
end;

procedure Tf_main.DeviceMessage(msg: string);
begin
 if msg<>'' then begin
  if LogToFile then begin
    WriteDeviceLog(msg);
  end;
 end;
end;

Procedure Tf_main.CameraStatus(Sender: TObject);
var cool:boolean;
begin
 case camera.Status of
   devDisconnected:begin
                   f_preview.stop;
                   f_capture.stop;
                   Capture:=false;
                   StartCaptureTimer.Enabled:=false;
                   f_sequence.CameraDisconnected;
                   StatusBar1.Panels[1].Text:='';
                   f_devicesconnection.LabelCamera.Font.Color:=clRed;
                   if PageVideo.TabVisible then begin
                     PageVideo.TabVisible:=false;
                     MenuTabVideo.Visible:=false;
                     PageControlRight.ActivePageIndex:=0;
                   end;
                   end;
   devConnecting:  begin
                   NewMessage('Connecting camera...');
                   f_devicesconnection.LabelCamera.Font.Color:=clOrange;
                   end;
   devConnected:   begin
                   if f_devicesconnection.LabelCamera.Font.Color<>clGreen then NewMessage('Camera connected');
                   f_devicesconnection.LabelCamera.Font.Color:=clGreen;
                   wait(1);
                   cool:=camera.Cooler;
                   CameraCoolerChange(cool);
                   if config.GetValue('/Cooler/CameraAutoCool',false) then begin
                      f_ccdtemp.Setpoint.Text:=FormatFloat(f1,config.GetValue('/Cooler/CameraAutoCoolTemp',0));
                      f_ccdtemp.BtnSet.Click;
                   end;
                   if camera.hasVideo then begin
                      wait(1);
                      PageVideo.TabVisible:=true;
                      MenuTabVideo.Visible:=true;
                      CameraVideoPreviewChange(nil);
                      f_video.FrameRate.Items.Assign(camera.VideoRates);
                      f_video.VideoSize.Items.Assign(camera.VideoSizes);
                      CameraVideoSizeChange(nil);
                      CameraVideoRateChange(nil);
                      f_video.SetImageControls;
                   end;
                   if wheel.WheelInterface=INCAMERA then begin
                     wait(1);
                     WheelStatus(Sender);
                   end;
                   end;
 end;
 CheckConnectionStatus;
end;

Procedure Tf_main.CameraDisconnected(Sender: TObject);
begin
 // device disconnected from server.
 // disconnect from server to allow a clean reconnection
 NewMessage('Camera disconnected!');
 camera.Disconnect;
end;

Procedure Tf_main.CameraExposureAborted(Sender: TObject);
begin
 AbortTimer.Enabled:=true;
end;

procedure Tf_main.AbortTimerTimer(Sender: TObject);
begin
  AbortTimer.Enabled:=false;
  StartCaptureTimer.Enabled:=false;
  if Capture and f_capture.Running then NewMessage('Exposure aborted!');
  f_preview.stop;
  f_capture.stop;
  Capture:=false;
  Preview:=false;
  StatusBar1.Panels[1].Text:='Stop';
  MenuCaptureStart.Caption:=f_capture.BtnStart.Caption;
end;

procedure  Tf_main.CameraTemperatureChange(t:double);
begin
 f_ccdtemp.Current.Text:=FormatFloat(f1,t);
 if camera.TemperatureRampActive then f_ccdtemp.BtnSet.Caption:='Cancel' else f_ccdtemp.BtnSet.Caption:='Set';
end;

procedure Tf_main.CameraCoolerChange(var v:boolean);
begin
 if f_ccdtemp.CCDcooler.Checked<>v then begin
    f_ccdtemp.CCDcooler.Checked:=v;
    NewMessage('Camera cooler '+BoolToStr(v,true));
 end;
end;

procedure Tf_main.CameraVideoPreviewChange(Sender: TObject);
begin
  f_video.Preview.Checked:=camera.VideoPreviewRunning;
end;

procedure Tf_main.CameraVideoSizeChange(Sender: TObject);
var i: integer;
begin
  i:=f_video.VideoSize.Items.IndexOf(camera.VideoSize);
  if i>=0 then f_video.VideoSize.ItemIndex:=i;
end;

procedure Tf_main.CameraVideoRateChange(Sender: TObject);
var i: integer;
begin
  i:=f_video.FrameRate.Items.IndexOf(camera.VideoRate);
  if i>=0 then f_video.FrameRate.ItemIndex:=i;
end;

procedure Tf_main.CameraFPSChange(Sender: TObject);
begin
  f_video.FPS:=camera.FPS;
end;

procedure Tf_main.CameraVideoExposureChange(Sender: TObject);
begin
  f_video.ShowExposure(round(camera.VideoExposure));
  f_video.Gain.Position:=round(camera.VideoGain);
  f_video.Gamma.Position:=round(camera.VideoGamma);
  f_video.Brightness.Position:=round(camera.VideoBrightness);
end;

Procedure Tf_main.WheelStatus(Sender: TObject);
begin
case wheel.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelWheel.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting filter wheel...');
                      f_devicesconnection.LabelWheel.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage('Filter wheel connected');
                      f_devicesconnection.LabelWheel.Font.Color:=clGreen;
                      f_filterwheel.Filters.Items.Assign(wheel.FilterNames);
                      f_EditPlan.Filter.Items.Assign(wheel.FilterNames);
                      FilterList.Assign(wheel.FilterNames);
                      SetFilterMenu;
                      if (wheel.Filter>0)and(wheel.Filter<=f_filterwheel.Filters.Items.Count) then
                         f_filterwheel.Filters.ItemIndex:=round(wheel.Filter);
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.SetFilter(Sender: TObject);
begin
  wheel.Filter:=f_filterwheel.Filters.ItemIndex;
end;

procedure Tf_main.FilterChange(n:double);
var o: integer;
begin
// show new filter name
if (n>=0)and(n<=f_filterwheel.Filters.Items.Count) then
   f_filterwheel.Filters.ItemIndex:=round(n);
// set exposure factor
AutofocusExposureFact:=FilterExpFact[round(n)];
// adjust focus
if (n>0)and(n<=MaxFilter)and(focuser.Status=devConnected) then begin
 if CurrentFilterOffset<>FilterOffset[round(n)] then begin
   if filteroffset_initialized then begin
    o:=FilterOffset[round(n)]-CurrentFilterOffset;
    f_focuser.FocusSpeed:=abs(o);
    if o>0 then
      FocusOUT(nil)
    else
      FocusIN(nil);
    CurrentFilterOffset:=FilterOffset[round(n)];
   end
   else begin
    CurrentFilterOffset:=FilterOffset[round(n)];
    filteroffset_initialized:=true;
   end;
 end;
end;
end;

procedure Tf_main.FilterNameChange(Sender: TObject);
begin
f_filterwheel.Filters.Items.Assign(wheel.FilterNames);
f_EditPlan.Filter.Items.Assign(wheel.FilterNames);
FilterList.Assign(wheel.FilterNames);
SetFilterMenu;
if (wheel.Filter>=0)and(wheel.Filter<=f_filterwheel.Filters.Items.Count) then
   f_filterwheel.Filters.ItemIndex:=round(wheel.Filter);
end;

Procedure Tf_main.SetFilterMenu;
var i:integer;
    m: TMenuItem;
begin
 for i:=MenuFilters.Count-1 downto 0 do
   MenuFilters.Delete(i);
 for i:=0 to FilterList.Count-1 do begin
   m:=TMenuItem.Create(Self);
   m.Caption:=FilterList[i];
   m.OnClick:=@MenuFilterClick;
   m.Tag:=i;
   MenuFilters.Add(m);
 end;
end;

procedure Tf_main.MenuHelpAboutClick(Sender: TObject);
var aboutmsg: string;
begin
aboutmsg:='CCDciel '+crlf;
aboutmsg:=aboutmsg+ccdciel_version+'-'+RevisionStr+blank+compile_time+crlf;
aboutmsg:=aboutmsg+'Compiled with:'+crlf;
aboutmsg:=aboutmsg+blank+compile_version+crlf+crlf;
aboutmsg:=aboutmsg+'Credits:'+crlf;
aboutmsg:=aboutmsg+tab+'Patrick Chevalley'+crlf;
aboutmsg:=aboutmsg+tab+'Han Kleijn'+crlf+crlf;
aboutmsg:=aboutmsg+'Copyright (C) 2017 Patrick Chevalley pch@ap-i.net'+crlf;
aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf;
aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf;
MessageDlg('About CCDciel',aboutmsg,mtInformation,[mbClose],0);
end;

procedure Tf_main.MenuDownloadClick(Sender: TObject);
begin
  ExecuteFile(URL_DOWNLOAD);
end;

procedure Tf_main.MenuBugReportClick(Sender: TObject);
begin
  ExecuteFile(URL_BUGREPORT);
end;

procedure Tf_main.MenuOnlineHelpClick(Sender: TObject);
begin
  ExecuteFile(URL_ONLINEHELP);
end;

Procedure Tf_main.FocuserStatus(Sender: TObject);
begin
case focuser.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelFocuser.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting focuser...');
                      f_devicesconnection.LabelFocuser.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage('Focuser connected');
                      f_devicesconnection.LabelFocuser.Font.Color:=clGreen;
                   end;
end;
CheckConnectionStatus;
end;

procedure Tf_main.FocuserPositionChange(n:double);
begin
  f_focuser.Position.Text:=inttostr(round(n));
end;

procedure Tf_main.FocuserSpeedChange(n:double);
begin
  f_focuser.speed.Text:=inttostr(round(n));
end;

procedure Tf_main.FocuserTimerChange(n:double);
begin
  f_focuser.timer.Text:=inttostr(round(n));
end;

procedure Tf_main.FocuserTemperatureChange(n:double);
begin
  f_focuser.Temp.Text:=FormatFloat(f1,n);
  FocuserTemp:=n;
end;

procedure Tf_main.FocusIN(Sender: TObject);
var n,p:integer;
begin
 n:=0;
 if focuser.hasAbsolutePosition then begin
    val(f_focuser.PosIncr.Text,p,n);
    if n=0 then begin
       focuser.Position:=focuser.Position-p;
    end;
 end
 else if focuser.hasRelativePosition then begin
    val(f_focuser.RelIncr.Text,p,n);
    if n=0 then begin
      if focuser.LastDirection<>FocusDirIn then p:=p+FocuserBacklash;
      focuser.FocusIn;
      focuser.RelPosition:=p;
    end;
 end
 else begin
    val(f_focuser.speed.Text,p,n);
    if n=0 then begin
      focuser.Speed:=p;
      focuser.FocusIn;
      val(f_focuser.timer.Text,p,n);
      if n=0 then begin
        if focuser.LastDirection<>FocusDirIn then p:=p+FocuserBacklash;
        focuser.Timer:=p;
      end;
    end;
 end;
 if n<>0 then NewMessage('Invalid numeric value');
end;

procedure Tf_main.FocusOUT(Sender: TObject);
var n,p:integer;
begin
 n:=0;
 if focuser.hasAbsolutePosition then begin
    val(f_focuser.PosIncr.Text,p,n);
    if n=0 then begin
       focuser.Position:=focuser.Position+p;
    end;
 end
 else if focuser.hasRelativePosition then begin
    val(f_focuser.RelIncr.Text,p,n);
    if n=0 then begin
      if focuser.LastDirection<>FocusDirOut then p:=p+FocuserBacklash;
      focuser.FocusOut;
      focuser.RelPosition:=p;
    end;
 end
 else begin
    val(f_focuser.speed.Text,p,n);
    if n=0 then begin
      focuser.Speed:=p;
      focuser.FocusOut;
      val(f_focuser.timer.Text,p,n);
      if n=0 then begin
        if focuser.LastDirection<>FocusDirOut then p:=p+FocuserBacklash;
        focuser.Timer:=p;
      end;
    end;
 end;
 if n<>0 then NewMessage('Invalid numeric value');
end;

procedure Tf_main.FocusSetAbsolutePosition(Sender: TObject);
var p,n: integer;
begin
 if focuser.hasAbsolutePosition then begin
   Val(f_focuser.Position.Text,p,n);
   if n=0 then focuser.Position:=p;
 end
end;

procedure Tf_main.FocusVcurveLearning(Sender: TObject);
begin
  if not focuser.hasAbsolutePosition then begin
    NewMessage('Cannot get focuser absolute position');
    exit;
  end;
  if f_vcurve=nil then begin
    f_vcurve:=Tf_vcurve.Create(self);
    f_vcurve.focuser:=f_focuser;
    f_vcurve.starprofile:=f_starprofile;
    f_vcurve.preview:=f_preview;
    f_vcurve.onLearnVcurve:=@LearnVcurve;
    f_vcurve.onStopVcurve:=@StopVcurve;
    f_vcurve.onSaveVcurve:=@doSaveVcurve;
    if VcCenterpos<>NullCoord then f_vcurve.FocusPos.Text:=IntToStr(VcCenterpos) else f_vcurve.FocusPos.Text:='';
    if VcHalfwidth<>NullCoord then f_vcurve.HalfWidth.Text:=IntToStr(VcHalfwidth) else f_vcurve.HalfWidth.Text:='';
    f_vcurve.Nsteps.Text:=IntToStr(VcNsteps);
  end;
  formpos(f_vcurve,mouse.CursorPos.x,mouse.CursorPos.y);
  f_vcurve.Show;
  f_vcurve.LoadCurve;
end;

function Tf_main.doVcurve(centerp,hw,n,nsum: integer;exp:double;bin:integer):boolean;
var i,j,k,minpos,maxpos,step:integer;
    hfdmin,hfd:double;
    hfdlist: array of double;
begin
 result:=false;
 TerminateVcurve:=false;
 AutofocusVcNum:=-1;
 minpos:=centerp-hw;
 maxpos:=centerp+hw;
 step:=round(2*hw/n);
 PosStartL:=-1;
 PosStartR:=-1;
 PosNearL:=-1;
 PosFocus:=-1;
 PosNearR:=-1;
 NewMessage('From: '+IntToStr(minpos)+' to '+IntToStr(centerp)+' by '+IntToStr(step));
 if focuser.hasTemperature then begin
    NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
    AutofocusVcTemp1:=FocuserTemp;
 end;
 if step<1 then exit;
 hfdmin:=9999;
 // initial focuser position in right direction
  if AutofocusMoveDir=FocusDirOut then begin
    i:=max(FocuserPositionMin,minpos-step);
    focuser.Position:=i;
  end
  else begin
    i:=min(FocuserPositionMax,maxpos+step);
    focuser.Position:=i;
  end;
  wait(1);
 // main loop for n measurement
 for i:=0 to n do begin
   // set new focuser position
   if AutofocusMoveDir=FocusDirOut then begin
     focuser.Position:=minpos+i*step;
     k:=i;
   end
   else begin
     focuser.Position:=maxpos-i*step;
     k:=n-i;
   end;
   wait(1);
   SetLength(hfdlist,nsum);
   // use bad pixel map
   fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
   // average hfd for nsum exposures
   for j:=1 to nsum do begin
     f_preview.ControlExposure(exp,bin,bin);
     if TerminateVcurve then begin
       NewMessage('Stop Vcurve learning');
       LoadVcurve;
       f_vcurve.LoadCurve;
       exit;
     end;
     f_starprofile.showprofile(fits.image,fits.imageC,fits.imageMin,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow div camera.BinX,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,mount.FocaleLength,camera.PixelSize);
     hfdlist[j-1]:=f_starprofile.HFD;
     NewMessage('Measurement '+inttostr(j)+' hfd:'+FormatFloat(f1,f_starprofile.hfd)+' peak:'+FormatFloat(f1,f_starprofile.ValMax)+' snr:'+FormatFloat(f1,f_starprofile.SNR));
   end;
   hfd:=SMedian(hfdlist);
   // store result always from left to right
   AutofocusVc[k,1]:=focuser.Position;
   AutofocusVc[k,2]:=hfd;
   NewMessage('Vcurve n'+inttostr(i)+' pos:'+FormatFloat(f0,AutofocusVc[k,1])+' hfd:'+FormatFloat(f1,AutofocusVc[k,2])+' peak:'+FormatFloat(f1,f_starprofile.ValMax)+' snr:'+FormatFloat(f1,f_starprofile.SNR));
   if f_vcurve<>nil then
      f_vcurve.LearnProgress(i,AutofocusVc[k,1],AutofocusVc[k,2]);
   // use minimal hfd as a rought focus position used to split right and left curve
   if AutofocusVc[k,2]<hfdmin then begin
     hfdmin:=AutofocusVc[k,2];
     PosFocus:=k;
   end;
 end;
 AutofocusVcNum:=n;
 AutofocusVcDir:=AutofocusMoveDir;
 if PosFocus<0 then begin
   NewMessage('Cannot detect star.');
   exit;
 end;
 //  search near focus pos, cancel if we not reach enough defocalisation
 for i:=0 to PosFocus do begin
   if AutofocusVc[i,2]>=AutofocusNearHFD then PosNearL:=i;
   if AutofocusVc[i,2]>=AutofocusStartHFD then PosStartL:=i;
 end;
 for i:=n downto PosFocus do begin
   if AutofocusVc[i,2]>=AutofocusNearHFD then PosNearR:=i;
   if AutofocusVc[i,2]>=AutofocusStartHFD then PosStartR:=i;
 end;
 if (PosNearL<0)or(PosNearR<0) then begin
   NewMessage('Cannot reach near focus HFD, please increase Half Width or better center the curve.');
   exit;
 end;
 if (PosNearL<0)or(PosNearR<0) then begin
   NewMessage('Cannot reach start focus HFD, please increase Half Width or decrease the start HFD');
   exit;
 end;
 AutofocusVcNum:=n;
 if focuser.hasTemperature then begin
   AutofocusVcTemp2:=FocuserTemp;
   AutofocusVcTemp:=(AutofocusVcTemp1+AutofocusVcTemp2)/2;
   NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
 end
 else
   AutofocusVcTemp:=0;
 NewMessage('Near L:'+inttostr(round(AutofocusVc[PosNearL,1])));
 NewMessage('Center:'+inttostr(round(AutofocusVc[PosFocus,1])));
 NewMessage('Near R:'+inttostr(round(AutofocusVc[PosNearR,1])));
 result:=true;
end;

procedure Tf_main.LearnVcurve(Sender: TObject);
var bin: integer;
    x,y,xc,yc,xc1,yc1,s,s2,s3,s4: integer;
    SaveZoom,vmax: double;
begin
 if not focuser.hasAbsolutePosition then exit;
 // read parameters
 VcCenterpos:=StrToIntDef(f_vcurve.FocusPos.Text,round(NullCoord));
 VcHalfwidth:=StrToIntDef(f_vcurve.HalfWidth.Text,round(NullCoord));
 VcNsteps:=StrToIntDef(f_vcurve.Nsteps.Text,30);
 if (VcCenterpos=NullCoord)or(VcHalfwidth=NullCoord) then exit;
 AutofocusVcFilterOffset:=CurrentFilterOffset;
 // find a bright star
 focuser.Position:=VcCenterpos;
 wait(1);
 bin:=AutofocusBinning;
 // use bad pixel map
 fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
 if (not f_starprofile.FindStar) then begin
   f_preview.ControlExposure(f_preview.Exposure,bin,bin);
   x:=fits.HeaderInfo.naxis1 div 2;
   y:=fits.HeaderInfo.naxis2 div 2;
   s:=min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 2;
   f_starprofile.FindBrightestPixel(fits.image,fits.imageC,fits.imageMin,x,y,s,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,starwindow div (2*camera.BinX),xc1,yc1,vmax);
   f_starprofile.FindStar:=(vmax>0);
   f_starprofile.StarX:=xc1;
   f_starprofile.StarY:=yc1;
 end
 else begin
   xc1 := round(f_starprofile.StarX);
   yc1 := round(f_starprofile.StarY);
 end;

 if not f_starprofile.FindStar then begin
   NewMessage('Cannot find a star at his position. Move to a bright star or increase the preview exposure time, or the autofocus binning.');
   exit;
 end;
 try
 learningvcurve:=true;
 // set focus frame around the star
 s:=Focuswindow div camera.BinX;
 s2:=s div 2;
 Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
 Screen2CCD(x,y,camera.VerticalFlip,xc,yc);
 if camera.CameraInterface=INDI then begin
   // INDI frame in unbinned pixel
   xc:=xc*camera.BinX;
   yc:=yc*camera.BinY;
   s3:=s2*camera.BinX;
   s4:=s*camera.BinX;
   camera.SetFrame(xc-s3,yc-s3,s4,s4);
 end
 else begin
   camera.SetFrame(xc-s2,yc-s2,s,s);
 end;
 f_starprofile.StarX:=s2;
 f_starprofile.StarY:=s2;
 SaveZoom:=f_visu.Zoom;
 f_visu.Zoom:=0;
 ImgZoom:=0;
 // do vcurve exposures
 NewMessage('Start learning V curve');
 if not doVcurve(VcCenterpos,VcHalfwidth,VcNsteps,AutofocusNearNum,f_preview.Exposure,bin) then begin
   // error return focuser to initial position
   focuser.Position:=VcCenterpos;
   wait(1);
   exit;
 end;
 // compute and save the curve
 ComputeVcSlope;
 SaveVcurve;
 // position focuser at new center
 if f_vcurve.Quality>0.9 then
    focuser.Position:=round((AutofocusVcpiL+AutofocusVcpiR)/2)
 else
    focuser.Position:=round(AutofocusVc[PosFocus,1]);
 wait(1);
 finally
 // reset camera
 learningvcurve:=false;
 camera.ResetFrame;
 f_visu.Zoom:=SaveZoom;
 ImgZoom:=f_visu.Zoom;
 f_starprofile.StarX:=xc1;
 f_starprofile.StarY:=yc1;
 f_starprofile.FindStar:=true;
 StartPreviewExposure(nil);
 end;
end;

procedure Tf_main.StopVcurve(Sender: TObject);
begin
  TerminateVcurve:=true;
end;

procedure Tf_main.ComputeVcSlope;
begin
 f_vcurve.FindLinearPart;
 f_vcurve.LoadCurve;
end;

procedure Tf_main.doSaveVcurve(Sender: TObject);
begin
 SaveVcurve;
end;

Procedure Tf_main.SaveVcurve;
var i:integer;
begin
 if AutofocusVcNum>0 then begin
  config.DeletePath('/StarAnalysis/Vcurve');
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcDir',AutofocusVcDir);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcTemp',AutofocusVcTemp);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcFilterOffset',AutofocusVcFilterOffset);
  config.SetValue('/StarAnalysis/Vcurve/VcCenterpos',VcCenterpos);
  config.SetValue('/StarAnalysis/Vcurve/VcHalfwidth',VcHalfwidth);
  config.SetValue('/StarAnalysis/Vcurve/VcNsteps',VcNsteps);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcSkipNum',AutofocusVcSkipNum);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcNum',AutofocusVcNum);
  for i:=0 to AutofocusVcNum do begin
     config.SetValue('/StarAnalysis/Vcurve/AutofocusVcPos'+inttostr(i),AutofocusVc[i,1]);
     config.SetValue('/StarAnalysis/Vcurve/AutofocusVcHfd'+inttostr(i),AutofocusVc[i,2]);
  end;
  config.SetValue('/StarAnalysis/Vcurve/PosNearL',PosNearL);
  config.SetValue('/StarAnalysis/Vcurve/PosFocus',PosFocus);
  config.SetValue('/StarAnalysis/Vcurve/PosNearR',PosNearR);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcSlopeL',AutofocusVcSlopeL);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcSlopeR',AutofocusVcSlopeR);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcPID',AutofocusVcPID);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcpiR',AutofocusVcpiR);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcpiL',AutofocusVcpiL);
  config.Flush;
 end;
end;

Procedure Tf_main.LoadVcurve;
var i:integer;
begin
   AutofocusVcDir:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcDir',AutofocusMoveDir);
   AutofocusVcTemp:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcTemp',NullCoord);
   AutofocusVcFilterOffset:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcFilterOffset',0);
   VcCenterpos:=config.GetValue('/StarAnalysis/Vcurve/VcCenterpos',round(NullCoord));
   VcHalfwidth:=config.GetValue('/StarAnalysis/Vcurve/VcHalfwidth',round(NullCoord));
   VcNsteps:=config.GetValue('/StarAnalysis/Vcurve/VcNsteps',30);
   AutofocusVcSkipNum:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcSkipNum',0);
   AutofocusVcNum:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcNum',-1);
   if AutofocusVcNum>0 then begin
     for i:=0 to AutofocusVcNum do begin
        AutofocusVc[i,1]:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcPos'+inttostr(i),0);
        AutofocusVc[i,2]:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcHfd'+inttostr(i),0);
     end;
     PosNearL:=config.GetValue('/StarAnalysis/Vcurve/PosNearL',-1);
     PosFocus:=config.GetValue('/StarAnalysis/Vcurve/PosFocus',-1);
     PosNearR:=config.GetValue('/StarAnalysis/Vcurve/PosNearR',-1);
     AutofocusVcSlopeL:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcSlopeL',-1.0);
     AutofocusVcSlopeR:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcSlopeR',-1.0);
     AutofocusVcPID:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcPID',-1.0);
     AutofocusVcpiR:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcpiR',-1.0);
     AutofocusVcpiL:=config.GetValue('/StarAnalysis/Vcurve/AutofocusVcpiL',-1.0);
   end;
end;

Procedure Tf_main.RotatorStatus(Sender: TObject);
begin
case rotator.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelRotator.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting rotator...');
                      f_devicesconnection.LabelRotator.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelRotator.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelRotator.Font.Color:=clGreen;
                      NewMessage('Rotator connected');
                      f_rotator.SetReverse(config.GetValue('/Rotator/Reverse',false));
                      rotator.CalibrationAngle:=config.GetValue('/Rotator/CalibrationAngle',0.0);
                      f_rotator.SetCalibrated(rotator.CalibrationAngle<>0);
                      wait(1);
                      RotatorAngleChange(self);
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.RotatorAngleChange(Sender: TObject);
begin
 f_rotator.Angle.Text:=FormatFloat(f1,rotator.Angle);
 f_rotator.SetCalibrated(rotator.CalibrationAngle<>0);
end;

Procedure Tf_main.RotatorRotate(Sender: TObject);
var a: double;
begin
 a:=StrToFloatDef(f_rotator.Angle.Text,NullCoord);
 if a<>NullCoord then begin
  a:=rmod(a+360,360);
  rotator.Angle:=a;
 end;
end;

Procedure Tf_main.RotatorHalt(Sender: TObject);
begin
  rotator.Halt;
end;

Procedure Tf_main.RotatorReverse(Sender: TObject);
begin
  rotator.Reverse:=f_rotator.Reverse.Checked;
end;

Procedure Tf_main.MountStatus(Sender: TObject);
begin
case mount.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelMount.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage('Connecting mount...');
                      f_devicesconnection.LabelMount.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelMount.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelMount.Font.Color:=clGreen;
                      NewMessage('Mount connected');
                      wait(1);
                      MountCoordChange(Sender);
                      CheckMeridianFlip;
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.MountCoordChange(Sender: TObject);
begin
 f_mount.RA.Text:=RAToStr(mount.RA);
 f_mount.DE.Text:=DEToStr(mount.Dec);
end;

Procedure Tf_main.MountPiersideChange(Sender: TObject);
begin
  case mount.PierSide of
    pierEast: f_mount.Pierside.Text:='East (Pointing West)';
    pierWest: f_mount.Pierside.Text:='West (Pointing East)';
    pierUnknown: f_mount.Pierside.Text:='Unknow pier side';
  end;
end;

Procedure Tf_main.MountParkChange(Sender: TObject);
begin
 if mount.Park then begin
    f_mount.BtnPark.Caption:='Parked';
    f_mount.BtnPark.Font.Color:=clRed
 end
 else begin
    f_mount.BtnPark.Caption:='Unparked';
    f_mount.BtnPark.Font.Color:=clGreen;
 end;
 MenuMountPark.Caption:=f_mount.BtnPark.Caption;
end;

procedure Tf_main.SetMountPark(Sender: TObject);
begin
 mount.Park:=not mount.Park;
end;

procedure Tf_main.SetMountTrack(Sender: TObject);
begin
 mount.Track;
end;

Procedure Tf_main.AutoguiderConnectClick(Sender: TObject);
begin
 if f_autoguider.BtnConnect.Caption='Connect' then begin
   autoguider.Connect(config.GetValue('/Autoguider/PHDhostname','localhost'),
                      config.GetValue('/Autoguider/PHDport','4400'));
 end else begin
   autoguider.Disconnect;
 end;
 f_autoguider.Status.Text:=autoguider.Status;
 NewMessage('Autoguider: '+autoguider.Status);
end;

Procedure Tf_main.AutoguiderCalibrateClick(Sender: TObject);
begin
  autoguider.Calibrate;
end;

Procedure Tf_main.AutoguiderGuideClick(Sender: TObject);
var onoff:boolean;
begin
 if f_autoguider.BtnGuide.Caption='Guide' then begin
    onoff:=true;
 end else begin
   onoff:=false;
 end;
 autoguider.Guide(onoff);
end;

Procedure Tf_main.AutoguiderDitherClick(Sender: TObject);
begin
 autoguider.Dither(DitherPixel, DitherRAonly);
end;

Procedure Tf_main.AutoguiderConnect(Sender: TObject);
begin
 f_autoguider.BtnConnect.Caption:='Disconnect';
 MenuAutoguiderConnect.Caption:=f_autoguider.BtnConnect.Caption;
 autoguider.ConnectGear;
 autoguider.SettleTolerance(SettlePixel,SettleMinTime, SettleMaxTime);
end;

Procedure Tf_main.AutoguiderDisconnect(Sender: TObject);
var i: integer;
begin
 if not AppClose then begin
   NewMessage('Disconnected from autoguider software!');
   f_sequence.AutoguiderDisconnected;
   // autoguider will be free automatically, create a new one for next connection
   i:=config.GetValue('/Autoguider/Software',0);
   case TAutoguiderType(i) of
     PHD: autoguider:=T_autoguider_phd.Create;
   end;
   autoguider.onStatusChange:=@AutoguiderStatus;
   autoguider.onConnect:=@AutoguiderConnect;
   autoguider.onDisconnect:=@AutoguiderDisconnect;
   autoguider.onShowMessage:=@NewMessage;
   f_sequence.Autoguider:=autoguider;
   f_autoguider.Status.Text:=autoguider.Status;
   NewMessage('Autoguider: '+autoguider.Status);
   f_autoguider.BtnConnect.Caption:='Connect';
   f_autoguider.BtnGuide.Caption:='Guide';
   f_autoguider.led.Brush.Color:=clGray;
   MenuAutoguiderConnect.Caption:=f_autoguider.BtnConnect.Caption;
   MenuAutoguiderGuide.Caption:=f_autoguider.BtnGuide.Caption;
 end;
end;

Procedure Tf_main.AutoguiderStatus(Sender: TObject);
begin
 if f_autoguider.Status.Text<>autoguider.Status then NewMessage('Autoguider: '+autoguider.Status);
 f_autoguider.Status.Text:=autoguider.Status;
 case autoguider.State of
   GUIDER_DISCONNECTED:begin
                       f_autoguider.led.Brush.Color:=clGray;
                       f_autoguider.BtnGuide.Caption:='Guide';
                       MenuAutoguiderGuide.Caption:='Guide';
                       end;
   GUIDER_IDLE        :begin
                       f_autoguider.led.Brush.Color:=clYellow;
                       f_autoguider.BtnGuide.Caption:='Guide';
                       MenuAutoguiderGuide.Caption:='Guide';
                       if (not meridianflipping)and(not autofocusing) then f_sequence.AutoguiderIddle;
                       end;
   GUIDER_GUIDING     :begin
                       f_autoguider.led.Brush.Color:=clLime;
                       f_autoguider.BtnGuide.Caption:='Stop';
                       MenuAutoguiderGuide.Caption:='Stop guiding';
                       end;
   GUIDER_BUSY        :begin
                       f_autoguider.led.Brush.Color:=clOrange;
                       f_autoguider.BtnGuide.Caption:='Stop';
                       MenuAutoguiderGuide.Caption:='Stop guiding';
                       end;
   GUIDER_ALERT       :begin
                       f_autoguider.led.Brush.Color:=clRed;
                       f_autoguider.BtnGuide.Caption:='Guide';
                       MenuAutoguiderGuide.Caption:='Guide';
                       end;
 end;
 if autoguider.LastError<>'' then NewMessage('Autoguider: '+autoguider.LastError);

end;

procedure Tf_main.MenuViewhdrClick(Sender: TObject);
begin
  fits.ViewHeaders;
end;

procedure Tf_main.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_main.MenuSetupClick(Sender: TObject);
var configfile: string;
    loadopt: boolean;
begin
  if camera.Status<>devDisconnected then begin
    ShowMessage('Disconnect the camera before to change the configuration.');
    exit;
  end;
  f_setup.DefaultCameraInterface:=camera.CameraInterface;
  f_setup.DefaultMountInterface:=mount.MountInterface;
  f_setup.DefaultWheelInterface:=wheel.WheelInterface;
  f_setup.DefaultFocuserInterface:=focuser.FocuserInterface;
  f_setup.DefaultRotatorInterface:=rotator.RotatorInterface;
  f_setup.profile:=profile;
  f_setup.LoadProfileList;
  f_setup.Loadconfig(config);
  FormPos(f_setup,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_setup.ShowModal;

  if f_setup.ModalResult=mrOK then begin
    if profile<>f_setup.profile then begin
      profile:=f_setup.profile;
      if profile='default' then
         configfile:='ccdciel.conf'
      else
         configfile:='ccdciel_'+profile+'.conf';
      loadopt:=FileExistsUTF8(slash(ConfigDir)+configfile);
      OpenConfig(configfile);
      f_devicesconnection.ProfileLabel.Caption:='Profile: '+profile;
    end;
    config.SetValue('/Interface',ord(f_setup.ConnectionInterface));
    config.SetValue('/INDI/Server',f_setup.IndiServer.Text);
    config.SetValue('/INDI/ServerPort',f_setup.IndiPort.Text);
    config.SetValue('/Devices/Timeout',f_setup.IndiTimeout.Text);

    config.SetValue('/Devices/Camera',true);
    config.SetValue('/Devices/FilterWheel',f_setup.DeviceFilterWheel.Checked);
    config.SetValue('/Devices/Focuser',f_setup.DeviceFocuser.Checked);
    config.SetValue('/Devices/Rotator',f_setup.DeviceRotator.Checked);
    config.SetValue('/Devices/Mount',f_setup.DeviceMount.Checked);

    config.SetValue('/CameraInterface',ord(f_setup.CameraConnection));
    if f_setup.CameraIndiDevice.Text<>'' then config.SetValue('/INDIcamera/Device',f_setup.CameraIndiDevice.Text);
    config.SetValue('/INDIcamera/Sensor',f_setup.CameraSensor);
    config.SetValue('/INDIcamera/DevicePort',f_setup.CameraIndiDevPort.Text);
    config.SetValue('/INDIcamera/AutoLoadConfig',f_setup.CameraAutoLoadConfig.Checked);
    config.SetValue('/INDIcamera/IndiTransfert',f_setup.CameraIndiTransfert.ItemIndex);
    config.SetValue('/INDIcamera/IndiTransfertDir',f_setup.CameraIndiTransfertDir.Text);
    config.SetValue('/ASCOMcamera/Device',f_setup.AscomCamera.Text);

    config.SetValue('/FilterWheelInterface',ord(f_setup.WheelConnection));
    if f_setup.WheelIndiDevice.Text<>'' then config.SetValue('/INDIwheel/Device',f_setup.WheelIndiDevice.Text);
    config.SetValue('/INDIwheel/DevicePort',f_setup.WheelIndiDevPort.Text);
    config.SetValue('/INDIwheel/AutoLoadConfig',f_setup.WheelAutoLoadConfig.Checked);
    config.SetValue('/ASCOMwheel/Device',f_setup.AscomWheel.Text);

    config.SetValue('/FocuserInterface',ord(f_setup.FocuserConnection));
    if f_setup.FocuserIndiDevice.Text<>'' then config.SetValue('/INDIfocuser/Device',f_setup.FocuserIndiDevice.Text);
    config.SetValue('/INDIfocuser/DevicePort',f_setup.FocuserIndiDevPort.Text);
    config.SetValue('/INDIfocuser/AutoLoadConfig',f_setup.FocuserAutoLoadConfig.Checked);
    config.SetValue('/ASCOMfocuser/Device',f_setup.AscomFocuser.Text);

    config.SetValue('/RotatorInterface',ord(f_setup.RotatorConnection));
    if f_setup.RotatorIndiDevice.Text<>'' then config.SetValue('/INDIrotator/Device',f_setup.RotatorIndiDevice.Text);
    config.SetValue('/INDIrotator/DevicePort',f_setup.RotatorIndiDevPort.Text);
    config.SetValue('/INDIrotator/AutoLoadConfig',f_setup.RotatorAutoLoadConfig.Checked);
    config.SetValue('/ASCOMrotator/Device',f_setup.AscomRotator.Text);

    config.SetValue('/MountInterface',ord(f_setup.MountConnection));
    if f_setup.MountIndiDevice.Text<>'' then config.SetValue('/INDImount/Device',f_setup.MountIndiDevice.Text);
    config.SetValue('/INDImount/DevicePort',f_setup.MountIndiDevPort.Text);
    config.SetValue('/INDImount/AutoLoadConfig',f_setup.MountAutoLoadConfig.Checked);
    config.SetValue('/ASCOMmount/Device',f_setup.AscomMount.Text);

    SaveConfig;

    if f_setup.RestartRequired then
       Restart
    else
       SetConfig;
       if loadopt then SetOptions;
  end;
end;

procedure Tf_main.MenuOptionsClick(Sender: TObject);
var ok,PlanetariumChange: boolean;
    i,n,FocusStarMagIndex: integer;
    buf:string;
begin
   PlanetariumChange:=false;
   f_option.Caption:='Options :'+profile;
   f_option.onGetPixelSize:=@OptionGetPixelSize;
   f_option.onGetFocale:=@OptionGetFocaleLength;
   f_option.CaptureDir.Text:=config.GetValue('/Files/CapturePath',defCapturePath);
   f_option.SubfolderSequence.Checked:=config.GetValue('/Files/SubfolderSequence',false);
   f_option.SubfolderObjname.Checked:=config.GetValue('/Files/SubfolderObjname',false);
   f_option.SubfolderStep.Checked:=config.GetValue('/Files/SubfolderStep',false);
   f_option.SubfolderFrametype.Checked:=config.GetValue('/Files/SubfolderFrametype',false);
   f_option.SubfolderExp.Checked:=config.GetValue('/Files/SubfolderExposure',false);
   f_option.SubfolderBin.Checked:=config.GetValue('/Files/SubfolderBinning',false);
   f_option.FileObjname.Checked:=config.GetValue('/Files/FilenameObjname',true);
   f_option.FileFiltername.Checked:=config.GetValue('/Files/FilenameFilter',true);
   f_option.FileDate.Checked:=config.GetValue('/Files/FilenameDate',true);
   f_option.FileExp.Checked:=config.GetValue('/Files/FilenameExposure',false);
   f_option.FileBin.Checked:=config.GetValue('/Files/FilenameBinning',false);
   f_option.Logtofile.Checked:=config.GetValue('/Log/Messages',true);
   f_option.Logtofile.Hint:='Log files are saved in '+ExtractFilePath(LogFile);
   f_option.ObservatoryName.Text:=config.GetValue('/Info/ObservatoryName','');
   f_option.Latitude:=config.GetValue('/Info/ObservatoryLatitude',46.0);
   f_option.Longitude:=config.GetValue('/Info/ObservatoryLongitude',-6.0);
   f_option.ObserverName.Text:=config.GetValue('/Info/ObserverName','');
   f_option.TelescopeName.Text:=config.GetValue('/Info/TelescopeName','');
   f_option.HorizonFile.FileName:=config.GetValue('/Info/HorizonFile','');
   f_option.ElevationMin.Text:=FormatFloat(f1,config.GetValue('/Info/ElevationMin',10.0));
   f_option.DebayerPreview.Checked:=config.GetValue('/Color/Bayer',false);
   f_option.BayerMode.ItemIndex:=config.GetValue('/Color/BayerMode',0);
   f_option.RedBalance.Position:=round(100*config.GetValue('/Color/RedBalance',1.0));
   f_option.GreenBalance.Position:=round(100*config.GetValue('/Color/GreenBalance',1.0));
   f_option.BlueBalance.Position:=round(100*config.GetValue('/Color/BlueBalance',1.0));
   f_option.BPMsigma.Text:=inttostr(config.GetValue('/BadPixel/Sigma',5));
   f_option.VideoPreviewRate.Text:=inttostr(config.GetValue('/Video/PreviewRate',5));
   f_option.VideoGroup.Visible:=(camera.CameraInterface=INDI);
   f_option.RefTreshold.Position:=config.GetValue('/RefImage/Treshold',128);
   f_option.RefColor.ItemIndex:=config.GetValue('/RefImage/Color',0);
   f_option.TemperatureSlope.Text:=FormatFloat(f1,config.GetValue('/Cooler/TemperatureSlope',TemperatureSlope));
   f_option.CameraAutoCool.Checked:=config.GetValue('/Cooler/CameraAutoCool',false);
   f_option.CameraAutoCoolTemp.Text:=FormatFloat(f1,config.GetValue('/Cooler/CameraAutoCoolTemp',0));
   f_option.StarWindow.Text:=inttostr(config.GetValue('/StarAnalysis/Window',Starwindow));
   f_option.FocusWindow.Text:=inttostr(config.GetValue('/StarAnalysis/Focus',Focuswindow));
   f_option.FilterList.Cells[0,0]:='Filter name';
   f_option.FilterList.Cells[1,0]:='Focuser offset';
   f_option.FilterList.Cells[2,0]:='Exposure factor';
   for i:=1 to f_option.FilterList.RowCount-1 do begin
     f_option.FilterList.Cells[0,i]:='';
     f_option.FilterList.Cells[1,i]:='';
     f_option.FilterList.Cells[2,i]:='';
   end;
   for i:=1 to FilterList.Count-1 do begin
     f_option.FilterList.Cells[0,i]:=FilterList[i];
     f_option.FilterList.Cells[1,i]:=config.GetValue('/Filters/Offset'+IntToStr(i),'0');
     f_option.FilterList.Cells[2,i]:=config.GetValue('/Filters/ExpFact'+IntToStr(i),'1.0');
   end;
   f_option.FilterList.Row:=0;
   f_option.FilterList.Col:=0;
   f_option.Autofocusmode.ItemIndex:=config.GetValue('/StarAnalysis/AutoFocusMode',ord(AutoFocusMode));
   f_option.AutofocusMinSpeed.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusMinSpeed',AutofocusMinSpeed));
   f_option.AutofocusMaxSpeed.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusMaxSpeed',AutofocusMaxSpeed));
   f_option.AutofocusStartHFD.Text:=FormatFloat(f1,config.GetValue('/StarAnalysis/AutofocusStartHFD',AutofocusStartHFD));
   f_option.AutofocusNearHFD.Text:=FormatFloat(f1,config.GetValue('/StarAnalysis/AutofocusNearHFD',AutofocusNearHFD));
   f_option.AutofocusExposure.Text:=FormatFloat(f1,config.GetValue('/StarAnalysis/AutofocusExposure',AutofocusExposure));
   f_option.AutofocusBinning.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusBinning',AutofocusBinning));
   f_option.FocuserBacklash.Text:=inttostr(config.GetValue('/StarAnalysis/FocuserBacklash',FocuserBacklash));
   f_option.FocuserTempCoeff.text:=FormatFloat(f2,config.GetValue('/StarAnalysis/FocuserTempCoeff',FocuserTempCoeff));
   f_option.AutofocusTolerance.Text:=FormatFloat(f1,config.GetValue('/StarAnalysis/AutofocusTolerance',AutofocusTolerance));
   f_option.AutofocusMinSNR.Text:=FormatFloat(f1,config.GetValue('/StarAnalysis/AutofocusMinSNR',AutofocusMinSNR));
   FocusStarMagIndex:=config.GetValue('/StarAnalysis/AutofocusStarMag',4)-4;
   if (FocusStarMagIndex<0)or(FocusStarMagIndex>4) then FocusStarMagIndex:=0;
   f_option.FocusStarMag.ItemIndex:=FocusStarMagIndex;
   f_option.AutofocusPrecisionSlew.Text:=FormatFloat(f2,config.GetValue('/StarAnalysis/AutofocusPrecisionSlew',2.0));
   f_option.GroupBacklash.Visible:=(focuser.Status=devConnected)and(not focuser.hasAbsolutePosition);
   ok:=config.GetValue('/StarAnalysis/AutofocusMoveDir',FocusDirIn);
   f_option.AutofocusMoveDirIn.Checked:=ok;
   f_option.AutofocusMoveDirOut.Checked:=not ok;
   f_option.AutofocusNearNum.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusNearNum',AutofocusNearNum));
   f_option.AutofocusMeanNumPoint.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusMeanNumPoint',AutofocusMeanNumPoint));
   f_option.AutofocusMeanMovement.Text:=inttostr(config.GetValue('/StarAnalysis/AutofocusMeanMovement',AutofocusMeanMovement));
   f_option.PixelSize.Text:=config.GetValue('/Astrometry/PixelSize','');
   f_option.Focale.Text:=config.GetValue('/Astrometry/FocaleLength','');
   f_option.PixelSizeFromCamera.Checked:=config.GetValue('/Astrometry/PixelSizeFromCamera',true);
   f_option.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
   if f_option.PixelSizeFromCamera.Checked and (camera.PixelSizeX>0) then
      f_option.PixelSize.Text:=FormatFloat(f2,camera.PixelSizeX);
   f_option.FocaleFromTelescope.Checked:=config.GetValue('/Astrometry/FocaleFromTelescope',true);
   if f_option.FocaleFromTelescope.Checked then
      f_option.Focale.Text:=FormatFloat(f0,mount.FocaleLength);
   f_option.Tolerance.Text:=FormatFloat(f2,config.GetValue('/Astrometry/ScaleTolerance',0.1));
   f_option.MinRadius.Text:=FormatFloat(f1,config.GetValue('/Astrometry/MinRadius',15.0));
   f_option.AstrometryTimeout.Text:=FormatFloat(f0,config.GetValue('/Astrometry/Timeout',60.0));
   f_option.Downsample.Text:=IntToStr(config.GetValue('/Astrometry/DownSample',4));
   f_option.SourcesLimit.Text:=IntToStr(config.GetValue('/Astrometry/SourcesLimit',150));
   f_option.Plot.Checked:=config.GetValue('/Astrometry/Plot',false);
   f_option.OtherOptions.Text:=config.GetValue('/Astrometry/OtherOptions','--no-fits2fits');
   f_option.AstUseScript.Checked:=config.GetValue('/Astrometry/AstUseScript',false);
   f_option.AstCustScript.Text:=config.GetValue('/Astrometry/AstCustScript','');
   f_option.CygwinPath.Text:=config.GetValue('/Astrometry/CygwinPath','C:\cygwin');
   f_option.ElbrusFolder.Text:=config.GetValue('/Astrometry/ElbrusFolder','C:\Elbrus\Images');
   {$ifdef unix}
   f_option.ElbrusUnixpath.Text:=config.GetValue('/Astrometry/ElbrusUnixpath',ExpandFileName('~/Elbrus/Images'));
   {$endif}
   f_option.PlatesolveFolder.Text:=config.GetValue('/Astrometry/PlatesolveFolder','C:\PlateSolve2.28');
   f_option.PlatesolveWait.Text:=IntToStr(config.GetValue('/Astrometry/PlatesolveWait',0));
   f_option.PrecSlewBox.ItemIndex:=config.GetValue('/PrecSlew/Method',0);
   f_option.SlewPrec.Text:=FormatFloat(f2,config.GetValue('/PrecSlew/Precision',5.0));
   f_option.SlewRetry.Text:=IntToStr(config.GetValue('/PrecSlew/Retry',3));
   f_option.SlewExp.Text:=FormatFloat(f1,config.GetValue('/PrecSlew/Exposure',10));
   f_option.SlewBin.Text:=IntToStr(config.GetValue('/PrecSlew/Binning',1));
   f_option.SlewDelay.Text:=IntToStr(config.GetValue('/PrecSlew/Delay',5));
   f_option.SlewFilter.Items.Assign(FilterList);
   f_option.SlewFilter.ItemIndex:=config.GetValue('/PrecSlew/Filter',0);
   if (mount.Status=devConnected)and(mount.PierSide=pierUnknown) then f_option.MeridianWarning.caption:='Mount is not reporting pier side, meridian process is unreliable.' else f_option.MeridianWarning.caption:='';
   f_option.MeridianOption.ItemIndex:=config.GetValue('/Meridian/MeridianOption',0);
   f_option.MinutesPastMeridian.Text:=IntToStr(config.GetValue('/Meridian/MinutesPast',0));
   f_option.MinutesPastMeridianMin.Text:=IntToStr(config.GetValue('/Meridian/MinutesPastMin',0));
   f_option.MeridianFlipPauseBefore.Checked:=config.GetValue('/Meridian/MeridianFlipPauseBefore',false);
   f_option.MeridianFlipPauseAfter.Checked:=config.GetValue('/Meridian/MeridianFlipPauseAfter',false);
   f_option.MeridianFlipPauseTimeout.Text:=IntToStr(config.GetValue('/Meridian/MeridianFlipPauseTimeout',0));
   f_option.MeridianFlipPanel.Visible:=(f_option.MeridianOption.ItemIndex=1);
   f_option.MeridianFlipCalibrate.Checked:=config.GetValue('/Meridian/MeridianFlipCalibrate',false);
   f_option.MeridianFlipAutofocus.Checked:=config.GetValue('/Meridian/MeridianFlipAutofocus',false);
   f_option.AutoguiderBox.ItemIndex:=config.GetValue('/Autoguider/Software',0);
   f_option.PHDhostname.Text:=config.GetValue('/Autoguider/PHDhostname','localhost');
   f_option.PHDport.Text:=config.GetValue('/Autoguider/PHDport','4400');
   f_option.DitherPixel.Text:=config.GetValue('/Autoguider/Dither/Pixel','1.0');
   f_option.DitherRAonly.Checked:=config.GetValue('/Autoguider/Dither/RAonly',true);
   f_option.SettlePixel.Text:=config.GetValue('/Autoguider/Settle/Pixel','1.0');
   f_option.SettleMinTime.Text:=config.GetValue('/Autoguider/Settle/MinTime','5');
   f_option.SettleMaxTime.Text:=config.GetValue('/Autoguider/Settle/MaxTime','30');
   f_option.CalibrationDelay.Text:=config.GetValue('/Autoguider/Settle/CalibrationDelay','300');
   f_option.StarLostRestart.Text:=IntToStr(config.GetValue('/Autoguider/Recovery/RestartTimeout',0));
   f_option.StarLostCancel.Text:=IntToStr(config.GetValue('/Autoguider/Recovery/CancelTimeout',1800));
   f_option.PlanetariumBox.ItemIndex:=config.GetValue('/Planetarium/Software',0);
   f_option.CdChostname.Text:=config.GetValue('/Planetarium/CdChostname','localhost');
   f_option.CdCport.Text:=config.GetValue('/Planetarium/CdCport','');
   f_option.CheckBoxLocalCdc.Checked:=f_option.CdCport.Text='';
   f_option.PanelRemoteCdc.Visible:=not f_option.CheckBoxLocalCdc.Checked;

   FormPos(f_option,mouse.CursorPos.X,mouse.CursorPos.Y);
   f_option.ShowModal;

   if f_option.ModalResult=mrOK then begin
     config.SetValue('/Files/CapturePath',f_option.CaptureDir.Text);
     config.SetValue('/Files/SubfolderSequence',f_option.SubfolderSequence.Checked);
     config.SetValue('/Files/SubfolderObjname',f_option.SubfolderObjname.Checked);
     config.SetValue('/Files/SubfolderStep',f_option.SubfolderStep.Checked);
     config.SetValue('/Files/SubfolderFrametype',f_option.SubfolderFrametype.Checked);
     config.SetValue('/Files/SubfolderExposure',f_option.SubfolderExp.Checked);
     config.SetValue('/Files/SubfolderBinning',f_option.SubfolderBin.Checked);
     config.SetValue('/Files/FilenameObjname',f_option.FileObjname.Checked);
     config.SetValue('/Files/FilenameFilter',f_option.FileFiltername.Checked);
     config.SetValue('/Files/FilenameDate',f_option.FileDate.Checked);
     config.SetValue('/Files/FilenameExposure',f_option.FileExp.Checked);
     config.SetValue('/Files/FilenameBinning',f_option.FileBin.Checked);
     config.SetValue('/StarAnalysis/Window',StrToIntDef(f_option.StarWindow.Text,Starwindow));
     config.SetValue('/StarAnalysis/Focus',StrToIntDef(f_option.FocusWindow.Text,Focuswindow));
     n:=FilterList.Count-1;
     config.SetValue('/Filters/Num',n);
     for i:=1 to n do begin
        config.SetValue('/Filters/Filter'+IntToStr(i),FilterList[i]);
        buf:=trim(f_option.FilterList.Cells[1,i]);
        if not IsNumber(buf) then buf:='0';
        config.SetValue('/Filters/Offset'+IntToStr(i),buf);
        buf:=trim(f_option.FilterList.Cells[2,i]);
        if not IsNumber(buf) then buf:='1.0';
        config.SetValue('/Filters/ExpFact'+IntToStr(i),buf);
     end;
     config.SetValue('/StarAnalysis/AutoFocusMode',f_option.Autofocusmode.ItemIndex);
     config.SetValue('/StarAnalysis/AutofocusMinSpeed',StrToIntDef(f_option.AutofocusMinSpeed.Text,AutofocusMinSpeed));
     config.SetValue('/StarAnalysis/AutofocusMaxSpeed',StrToIntDef(f_option.AutofocusMaxSpeed.Text,AutofocusMaxSpeed));
     config.SetValue('/StarAnalysis/AutofocusStartHFD',StrToFloatDef(f_option.AutofocusStartHFD.Text,AutofocusStartHFD));
     config.SetValue('/StarAnalysis/AutofocusNearHFD',StrToFloatDef(f_option.AutofocusNearHFD.Text,AutofocusNearHFD));
     config.SetValue('/StarAnalysis/AutofocusExposure',StrToFloatDef(f_option.AutofocusExposure.Text,AutofocusExposure));
     config.SetValue('/StarAnalysis/AutofocusBinning',StrToIntDef(f_option.AutofocusBinning.Text,AutofocusBinning));
     config.SetValue('/StarAnalysis/FocuserBacklash',StrToIntDef(f_option.FocuserBacklash.Text,FocuserBacklash));
     config.SetValue('/StarAnalysis/FocuserTempCoeff',StrToFloatDef(f_option.FocuserTempCoeff.text,FocuserTempCoeff));
     config.SetValue('/StarAnalysis/AutofocusTolerance',StrToFloatDef(f_option.AutofocusTolerance.Text,AutofocusTolerance));
     config.SetValue('/StarAnalysis/AutofocusMinSNR',StrToFloatDef(f_option.AutofocusMinSNR.Text,AutofocusMinSNR));
     config.SetValue('/StarAnalysis/AutofocusStarMag',f_option.FocusStarMag.ItemIndex+4);
     if FocusStarMagIndex<>f_option.FocusStarMag.ItemIndex then LoadFocusStar;
     config.SetValue('/StarAnalysis/AutofocusPrecisionSlew',StrToFloatDef(f_option.AutofocusPrecisionSlew.Text,2.0));
     config.SetValue('/StarAnalysis/AutofocusMoveDir',f_option.AutofocusMoveDirIn.Checked);
     config.SetValue('/StarAnalysis/AutofocusNearNum',StrToIntDef(f_option.AutofocusNearNum.Text,AutofocusNearNum));
     config.SetValue('/StarAnalysis/AutofocusMeanNumPoint',StrToIntDef(f_option.AutofocusMeanNumPoint.Text,AutofocusMeanNumPoint));
     config.SetValue('/StarAnalysis/AutofocusMeanMovement',StrToIntDef(f_option.AutofocusMeanMovement.Text,AutofocusMeanMovement));
     config.SetValue('/Log/Messages',f_option.Logtofile.Checked);
     config.SetValue('/Info/ObservatoryName',f_option.ObservatoryName.Text);
     config.SetValue('/Info/ObservatoryLatitude',f_option.Latitude);
     config.SetValue('/Info/ObservatoryLongitude',f_option.Longitude);
     config.SetValue('/Info/ObserverName',f_option.ObserverName.Text);
     config.SetValue('/Info/TelescopeName',f_option.TelescopeName.Text);
     config.SetValue('/Info/HorizonFile',f_option.HorizonFile.FileName);
     config.SetValue('/Info/ElevationMin',StrToFloatDef(f_option.ElevationMin.Text,10.0));
     config.SetValue('/Color/Bayer',f_option.DebayerPreview.Checked);
     config.SetValue('/Color/BayerMode',f_option.BayerMode.ItemIndex);
     config.SetValue('/Color/RedBalance',f_option.RedBalance.Position/100);
     config.SetValue('/Color/GreenBalance',f_option.GreenBalance.Position/100);
     config.SetValue('/Color/BlueBalance',f_option.BlueBalance.Position/100);
     config.SetValue('/BadPixel/Sigma',StrToIntDef(f_option.BPMsigma.Text,BPMsigma));
     config.SetValue('/Video/PreviewRate',StrToIntDef(f_option.VideoPreviewRate.Text,MaxVideoPreviewRate));
     config.SetValue('/RefImage/Treshold',f_option.RefTreshold.Position);
     config.SetValue('/RefImage/Color',f_option.RefColor.ItemIndex);
     config.SetValue('/Cooler/TemperatureSlope',StrToFloatDef(f_option.TemperatureSlope.Text,0));
     config.SetValue('/Cooler/CameraAutoCool',f_option.CameraAutoCool.Checked);
     config.SetValue('/Cooler/CameraAutoCoolTemp',StrToFloatDef(f_option.CameraAutoCoolTemp.Text,0));
     config.SetValue('/Astrometry/Resolver',f_option.Resolver);
     config.SetValue('/Astrometry/PixelSizeFromCamera',f_option.PixelSizeFromCamera.Checked);
     config.SetValue('/Astrometry/FocaleFromTelescope',f_option.FocaleFromTelescope.Checked);
     config.SetValue('/Astrometry/PixelSize',f_option.PixelSize.Text);
     config.SetValue('/Astrometry/FocaleLength',f_option.Focale.Text);
     config.SetValue('/Astrometry/ScaleTolerance',StrToFloatDef(f_option.Tolerance.Text,0.1 ));
     config.SetValue('/Astrometry/MinRadius',StrToFloatDef(f_option.MinRadius.Text,15.0));
     config.SetValue('/Astrometry/Timeout',StrToFloatDef(f_option.AstrometryTimeout.Text,60.0));
     config.SetValue('/Astrometry/DownSample',StrToIntDef(f_option.Downsample.Text,4));
     config.SetValue('/Astrometry/SourcesLimit',StrToIntDef(f_option.SourcesLimit.Text,0));
     config.SetValue('/Astrometry/Plot',f_option.Plot.Checked);
     config.SetValue('/Astrometry/OtherOptions',f_option.OtherOptions.Text);
     config.SetValue('/Astrometry/AstUseScript',f_option.AstUseScript.Checked);
     config.SetValue('/Astrometry/AstCustScript',f_option.AstCustScript.Text);
     config.SetValue('/Astrometry/CygwinPath',f_option.CygwinPath.Text);
     config.SetValue('/Astrometry/ElbrusFolder',f_option.ElbrusFolder.Text);
     {$ifdef unix}
     config.SetValue('/Astrometry/ElbrusUnixpath',f_option.ElbrusUnixpath.Text);
     {$endif}
     config.SetValue('/Astrometry/PlatesolveFolder',f_option.PlatesolveFolder.Text);
     config.SetValue('/Astrometry/PlatesolveWait',StrToIntDef(f_option.PlatesolveWait.Text,0));
     config.SetValue('/PrecSlew/Method',f_option.PrecSlewBox.ItemIndex);
     config.SetValue('/PrecSlew/Precision',StrToFloatDef(f_option.SlewPrec.Text,5.0));
     config.SetValue('/PrecSlew/Retry',StrToIntDef(f_option.SlewRetry.Text,3));
     config.SetValue('/PrecSlew/Exposure',StrToFloatDef(f_option.SlewExp.Text,10.0));
     config.SetValue('/PrecSlew/Binning',StrToIntDef(f_option.SlewBin.Text,1));
     config.SetValue('/PrecSlew/Delay',StrToIntDef(f_option.SlewDelay.Text,5));
     config.SetValue('/PrecSlew/Filter',f_option.SlewFilter.ItemIndex);
     config.SetValue('/Meridian/MeridianOption',f_option.MeridianOption.ItemIndex);
     config.SetValue('/Meridian/MinutesPast',StrToIntDef(f_option.MinutesPastMeridian.Text,0));
     config.SetValue('/Meridian/MinutesPastMin',StrToIntDef(f_option.MinutesPastMeridianMin.Text,0));
     config.SetValue('/Meridian/MeridianFlipPauseBefore',f_option.MeridianFlipPauseBefore.Checked);
     config.SetValue('/Meridian/MeridianFlipPauseAfter',f_option.MeridianFlipPauseAfter.Checked);
     config.SetValue('/Meridian/MeridianFlipPauseTimeout',StrToIntDef(f_option.MeridianFlipPauseTimeout.Text,0));
     config.SetValue('/Meridian/MeridianFlipCalibrate',f_option.MeridianFlipCalibrate.Checked);
     config.SetValue('/Meridian/MeridianFlipAutofocus',f_option.MeridianFlipAutofocus.Checked);
     config.SetValue('/Autoguider/Software',f_option.AutoguiderBox.ItemIndex);
     config.SetValue('/Autoguider/PHDhostname',f_option.PHDhostname.Text);
     config.SetValue('/Autoguider/PHDport',f_option.PHDport.Text);
     config.SetValue('/Autoguider/Dither/Pixel',f_option.DitherPixel.Text);
     config.SetValue('/Autoguider/Dither/RAonly',f_option.DitherRAonly.Checked);
     config.SetValue('/Autoguider/Settle/Pixel',f_option.SettlePixel.Text);
     config.SetValue('/Autoguider/Settle/MinTime',f_option.SettleMinTime.Text);
     config.SetValue('/Autoguider/Settle/MaxTime',f_option.SettleMaxTime.Text);
     config.SetValue('/Autoguider/Settle/CalibrationDelay',f_option.CalibrationDelay.Text);
     config.SetValue('/Autoguider/Recovery/RestartTimeout',StrToIntDef(f_option.StarLostRestart.Text,0));
     config.SetValue('/Autoguider/Recovery/CancelTimeout',StrToIntDef(f_option.StarLostCancel.Text,1800));
     PlanetariumChange := (f_option.PlanetariumBox.ItemIndex <> config.GetValue('/Planetarium/Software',0));
     config.SetValue('/Planetarium/Software',f_option.PlanetariumBox.ItemIndex);
     config.SetValue('/Planetarium/CdChostname',f_option.CdChostname.Text);
     config.SetValue('/Planetarium/CdCport',trim(f_option.CdCport.Text));

     SaveConfig;

     SetOptions;

     if PlanetariumChange and (not planetarium.Connected) then begin
        planetarium.Terminate;
        i:=config.GetValue('/Planetarium/Software',0);
        case TPlanetariumType(i) of
          CDC: planetarium:=TPlanetarium_cdc.Create;
          SAMP:planetarium:=TPlanetarium_samp.Create;
          HNSKY:planetarium:=TPlanetarium_hnsky.Create;
        end;
        planetarium.onConnect:=@PlanetariumConnect;
        planetarium.onDisconnect:=@PlanetariumDisconnect;
        planetarium.onShowMessage:=@NewMessage;
        f_planetariuminfo.planetarium:=planetarium;
        f_scriptengine.Planetarium:=planetarium;
        f_sequence.Planetarium:=planetarium;
     end;

   end;
end;

procedure Tf_main.MenuPdfHelpClick(Sender: TObject);
var pdffn: string;
begin
  pdffn:=ExpandFileNameUTF8(slash(Appdir)+slash('doc')+'doc_ccdciel_en.pdf');
  ExecuteFile(pdffn);
end;

procedure Tf_main.MenuPlanetariumConnectClick(Sender: TObject);
begin
  PlanetariumConnectClick(Sender);
end;

procedure Tf_main.MenuPlanetariumNewtargetClick(Sender: TObject);
begin
  PlanetariumNewTarget(Sender);
end;

procedure Tf_main.MenuPreviewLoopClick(Sender: TObject);
begin
  f_preview.BtnLoop.Click;
end;

procedure Tf_main.MenuPreviewStartClick(Sender: TObject);
begin
  f_preview.BtnPreview.Click;
end;

procedure Tf_main.OptionGetPixelSize(Sender: TObject);
begin
   if camera.PixelSizeX>0 then
      f_option.PixelSize.Text:=FormatFloat(f2,camera.PixelSizeX);
end;

procedure Tf_main.OptionGetFocaleLength(Sender: TObject);
begin
   if mount.FocaleLength>0 then
      f_option.Focale.Text:=FormatFloat(f0,mount.FocaleLength);
end;

procedure Tf_main.MenuViewConnectionClick(Sender: TObject);
begin
  f_devicesconnection.Visible:=MenuViewConnection.Checked;
end;

procedure Tf_main.MenuViewFiltersClick(Sender: TObject);
begin
  f_filterwheel.Visible:=MenuViewFilters.Checked;
end;

procedure Tf_main.MenuViewCCDtempClick(Sender: TObject);
begin
  f_ccdtemp.Visible:=MenuViewCCDtemp.Checked;
end;

procedure Tf_main.MenuViewClockClick(Sender: TObject);
begin
  Timestamp.Visible:=MenuViewClock.Checked;
  TimerStampTimer.Enabled:=Timestamp.Visible;
  TimerStampTimerTimer(nil);
end;

procedure Tf_main.MenuViewFocuserClick(Sender: TObject);
begin
  f_focuser.Visible:=MenuViewFocuser.Checked;
end;

procedure Tf_main.MenuViewFrameClick(Sender: TObject);
begin
  f_frame.Visible:=MenuViewFrame.Checked;
end;

procedure Tf_main.MenuViewHistogramClick(Sender: TObject);
begin
  f_visu.Visible:=MenuViewHistogram.Checked;
end;

procedure Tf_main.MenuViewMessagesClick(Sender: TObject);
begin
  f_msg.Visible:=MenuViewMessages.Checked;
end;

procedure Tf_main.MenuViewMountClick(Sender: TObject);
begin
  f_mount.Visible:=MenuViewMount.Checked;
end;

procedure Tf_main.MenuViewRotatorClick(Sender: TObject);
begin
  f_rotator.Visible:=MenuViewRotator.Checked;
end;

procedure Tf_main.MenuViewPlanetariumClick(Sender: TObject);
begin
  f_planetarium.Visible:=MenuViewPlanetarium.Checked;
end;

procedure Tf_main.MenuViewPreviewClick(Sender: TObject);
begin
  f_preview.Visible:=MenuViewPreview.Checked;
end;

procedure Tf_main.MenuViewCaptureClick(Sender: TObject);
begin
  f_capture.Visible:=MenuViewCapture.Checked;
end;

procedure Tf_main.MenuViewScriptClick(Sender: TObject);
begin
  f_script.Visible:=MenuViewScript.Checked;
end;

procedure Tf_main.MenuViewSequenceClick(Sender: TObject);
begin
  f_sequence.Visible:=MenuViewSequence.Checked;
end;

procedure Tf_main.MenuViewStarProfileClick(Sender: TObject);
begin
  f_starprofile.Visible:=MenuViewStarProfile.Checked;
end;

procedure Tf_main.MenuVisuLinearClick(Sender: TObject);
begin
  f_visu.BtnLinear.Checked:=True;
end;

procedure Tf_main.MenuVisuLogClick(Sender: TObject);
begin
  f_visu.BtnLog.Checked:=True;
end;

procedure Tf_main.MenuVisuSqrtClick(Sender: TObject);
begin
  f_visu.BtnSqrt.Checked:=True;
end;

procedure Tf_main.MenuVisuZoom12Click(Sender: TObject);
begin
  f_visu.BtnZoom05.Click;
end;

procedure Tf_main.MenuVisuZoom1Click(Sender: TObject);
begin
  f_visu.BtnZoom1.Click;
end;

procedure Tf_main.MenuVisuZoom2Click(Sender: TObject);
begin
  f_visu.BtnZoom2.Click;
end;

procedure Tf_main.MenuVisuZoomAdjustClick(Sender: TObject);
begin
  f_visu.BtnZoomAdjust.Click;
end;

procedure Tf_main.MenuViewAutoguiderClick(Sender: TObject);
begin
  f_autoguider.Visible:=MenuViewAutoguider.Checked;
end;

procedure Tf_main.PanelDragDrop(Sender, Source: TObject; X, Y: Integer);
var toolmenu,opm,npm: TMenuItem;
    i: integer;
begin
npm:=nil;
opm:=nil;
if sender is TPanel then begin
  if sender is TPanel then begin
    if TPanel(Sender).Tag>0 then npm:=TMenuItem(TPanel(Sender).tag);
    if source is TStaticText then begin
     if TFrame(TStaticText(Source).Parent).tag>0 then toolmenu:=TMenuItem(TFrame(TStaticText(Source).Parent).tag);
     TFrame(TStaticText(Source).Parent).Parent:=TPanel(Sender);
     TFrame(TStaticText(Source).Parent).Top:=Y;
     TFrame(TStaticText(Source).Parent).Left:=X;
     if TPanel(Sender).Width>TPanel(Sender).Height then begin
        TFrame(TStaticText(Source).Parent).Align:=alLeft;
     end else begin
        TFrame(TStaticText(Source).Parent).Align:=alTop;
     end;
    end
    else if source is TMemo then begin
      if TFrame(TPanel(TMemo(Source).Parent).Parent).tag>0 then toolmenu:=TMenuItem(TFrame(TPanel(TMemo(Source).Parent).Parent).tag);
      TFrame(TPanel(TMemo(Source).Parent).Parent).Parent:=TPanel(Sender);
      TFrame(TPanel(TMemo(Source).Parent).Parent).Top:=Y;
      TFrame(TPanel(TMemo(Source).Parent).Parent).Left:=X;
      if TPanel(Sender).Width>TPanel(Sender).Height then begin
         TFrame(TPanel(TMemo(Source).Parent).Parent).Align:=alLeft;
      end else begin
         TFrame(TPanel(TMemo(Source).Parent).Parent).Align:=alTop;
      end;
     end
    else if source is TDragObject then begin
      if TFrame(TDragObject(Source).Control).tag>0 then toolmenu:=TMenuItem(TFrame(TDragObject(Source).Control).tag);
      TFrame(TDragObject(Source).Control).Parent:=TPanel(Sender);
      TFrame(TDragObject(Source).Control).Top:=Y;
      TFrame(TDragObject(Source).Control).Left:=X;
      if TPanel(Sender).Width>TPanel(Sender).Height then begin
         TFrame(TDragObject(Source).Control).Align:=alLeft;
      end else begin
         TFrame(TDragObject(Source).Control).Align:=alTop;
      end;
    end
    else if source is TFrame then begin
     if TFrame(Source).tag>0 then toolmenu:=TMenuItem(TFrame(Source).tag);
     TFrame(Source).Parent:=TPanel(Sender);
     TFrame(Source).Top:=Y;
     TFrame(Source).Left:=X;
     if TPanel(Sender).Width>TPanel(Sender).Height then begin
        TFrame(Source).Align:=alLeft;
     end else begin
        TFrame(Source).Align:=alTop;
     end;
    end;
    if (npm<>nil)and(toolmenu<>nil) then begin
      opm:=toolmenu.Parent;
      if (opm<>nil)and(npm<>opm) then begin
        i:=opm.IndexOf(toolmenu);
        opm.Delete(i);
        npm.Add(toolmenu);
      end;
    end;
   end;
end;
end;

procedure Tf_main.PanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 if Source is TStaticText then Accept:=TStaticText(Source).Parent is TFrame
 else if Source is TMemo then Accept:=TPanel(TMemo(Source).Parent).Parent is TFrame
 else if Source is TDragObject then  Accept:=TDragObject(Source).Control is TFrame
 else if source is TFrame then Accept:=true
 else Accept:=false;
end;

Procedure Tf_main.AbortExposure(Sender: TObject);
begin
  StartCaptureTimer.Enabled:=false;
  camera.AbortExposure;
  Preview:=false;
  Capture:=false;
  NewMessage('Abort exposure');
  StatusBar1.Panels[1].Text:='Stop';
end;

Procedure Tf_main.StartPreviewExposureAsync(Data: PtrInt);
begin
  StartPreviewExposure(nil);
end;

Procedure Tf_main.StartPreviewExposure(Sender: TObject);
var e: double;
    buf: string;
    p,binx,biny: integer;
begin
if (camera.Status=devConnected) and ((not f_capture.Running) or autofocusing) and (not learningvcurve) then begin
  Preview:=true;
  e:=f_preview.Exposure;
  if e<0 then begin
    NewMessage('Invalid exposure time '+f_preview.ExpTime.Text);
    f_preview.stop;
    Preview:=false;
    exit;
  end;
  // check focuser temperature compensation
  if focuser.hasTemperature and (FocuserTempCoeff<>0.0) and (FocuserLastTemp<>NullCoord) and (camera.FrameType=LIGHT) and not (autofocusing or learningvcurve or f_starprofile.ChkAutofocus.Checked) then begin
    // only if temperature change by more than 0.5 C
    if abs(FocuserLastTemp-FocuserTemp)>0.5 then begin
      p:=f_focuser.TempOffset(FocuserLastTemp,FocuserTemp);
      if focuser.hasAbsolutePosition and (p<>0) then begin
        NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp)+' , adjust position by '+IntToStr(p));
        focuser.Position:=focuser.Position+p;
      end
      else if focuser.hasRelativePosition and (p<>0) then begin
        NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp)+' , adjust position by '+IntToStr(p));
        if p>0 then focuser.FocusOut else focuser.FocusIn;
        focuser.RelPosition:=abs(p);
      end;
      wait(1);
    end;
  end;
  p:=pos('x',f_preview.Binning.Text);
  if p>0 then begin
     buf:=trim(copy(f_preview.Binning.Text,1,p-1));
     binx:=StrToIntDef(buf,-1);
     buf:=trim(copy(f_preview.Binning.Text,p+1,9));
     biny:=StrToIntDef(buf,-1);
     if (binx<camera.BinXrange.min)or(biny<camera.BinYrange.min) or
        (binx>camera.BinXrange.max)or(biny>camera.BinYrange.max)
         then begin
           NewMessage('Invalid binning '+f_preview.Binning.Text);
           f_preview.stop;
           Preview:=false;
           exit;
         end;
     if (camera.BinX<>binx)or(camera.BinY<>biny) then begin
        NewMessage('Set Binning '+inttostr(binx)+'x'+inttostr(biny));
        camera.SetBinning(binx,biny);
     end;
  end;
  if camera.FrameType<>LIGHT then camera.FrameType:=LIGHT;
  camera.ObjectName:=f_capture.Fname.Text;
  fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  camera.StartExposure(e);
end
else begin
   f_preview.stop;
   Preview:=false;
   StatusBar1.Panels[1].Text:='';
end;
end;

procedure Tf_main.StartCaptureTimerTimer(Sender: TObject);
begin
  StartCaptureTimer.Enabled:=false;
  StartCaptureExposure(Sender);
end;

procedure Tf_main.StartCaptureExposureAsync(Data: PtrInt);
begin
  StartCaptureExposure(nil);
end;

Procedure Tf_main.StartCaptureExposure(Sender: TObject);
var e: double;
    buf: string;
    p,binx,biny,waittime: integer;
    ftype:TFrameType;
begin
if (camera.Status=devConnected)and(not autofocusing)and (not learningvcurve) then begin
  // check if we need to cancel running preview
  if f_preview.Running then begin
    NewMessage('Stop preview');
    StatusBar1.Panels[1].Text:='Stop preview';
    camera.AbortExposure;
    f_preview.stop;
    // retry after 5 sec.
    StartCaptureTimer.Interval:=5000;
    StartCaptureTimer.Enabled:=true;
    exit;
  end;
  f_capture.Running:=true;
  MenuCaptureStart.Caption:='Stop';
  Preview:=false;
  Capture:=true;
  // check exposure time
  e:=StrToFloatDef(f_capture.ExpTime.Text,-1);
  if e<0 then begin
    NewMessage('Invalid exposure time '+f_capture.ExpTime.Text);
    f_capture.Stop;
    Capture:=false;
    exit;
  end;
  // check and set binning
  p:=pos('x',f_capture.Binning.Text);
  if p>0 then begin
     buf:=trim(copy(f_capture.Binning.Text,1,p-1));
     binx:=StrToIntDef(buf,-1);
     buf:=trim(copy(f_capture.Binning.Text,p+1,9));
     biny:=StrToIntDef(buf,-1);
     if (binx<camera.BinXrange.min)or(biny<camera.BinYrange.min) or
        (binx>camera.BinXrange.max)or(biny>camera.BinYrange.max)
        then begin
          NewMessage('Invalid binning '+f_capture.Binning.Text);
          f_capture.Stop;
          Capture:=false;
          exit;
        end;
     if (camera.BinX<>binx)or(camera.BinY<>biny) then begin
        NewMessage('Set Binning '+inttostr(binx)+'x'+inttostr(biny));
        camera.SetBinning(binx,biny);
     end;
  end;
  // check and set frame
  if (f_capture.FrameType.ItemIndex>=0)and(f_capture.FrameType.ItemIndex<=ord(High(TFrameType))) then begin
    ftype:=TFrameType(f_capture.FrameType.ItemIndex);
    if camera.FrameType<>ftype then camera.FrameType:=ftype;
    if ftype<>LIGHT then begin
       f_capture.CheckBoxDither.Checked:=false;
       f_capture.CheckBoxFocus.Checked:=false;
    end;
  end;
  // check for meridian and do flip now if required
  waittime:=CheckMeridianFlip(e);
  if not f_capture.Running then begin
    // stop current capture if meridian flip failed
    NewMessage('Meridian aborted!');
    f_capture.Stop;
    Capture:=false;
    exit;
  end;
  // check if we need to wait for flip before to continue (time to meridian < exposure time)
  if waittime>0 then begin
    f_capture.DitherNum:=0; // no dither after flip
    // wait meridian
    StartCaptureTimer.Interval:=waittime*1000;
    StartCaptureTimer.Enabled:=true;
    exit;
  end;
  // check focuser temperature compensation
  if focuser.hasTemperature and (FocuserTempCoeff<>0.0) and (FocuserLastTemp<>NullCoord) and (camera.FrameType=LIGHT) then begin
    // only if temperature change by more than 0.5 C
    if abs(FocuserLastTemp-FocuserTemp)>0.5 then begin
      p:=f_focuser.TempOffset(FocuserLastTemp,FocuserTemp);
      if focuser.hasAbsolutePosition and (p<>0) then begin
        NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp)+' , adjust position by '+IntToStr(p));
        focuser.Position:=focuser.Position+p;
      end
      else if focuser.hasRelativePosition and (p<>0) then begin
        NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp)+' , adjust position by '+IntToStr(p));
        if p>0 then focuser.FocusOut else focuser.FocusIn;
        focuser.RelPosition:=abs(p);
      end;
      wait(1);
    end;
  end;
  // check if refocusing is required
  if f_capture.FocusNow or(f_capture.CheckBoxFocus.Checked and (f_capture.FocusNum>=StrToIntDef(f_capture.FocusCount.Text,1))) then begin
     f_capture.FocusNum:=0;
     f_capture.FocusNow:=false;
     // do autofocus
     if AutoAutofocus then begin
       if f_capture.Running then begin
         // ok, restart exposure
         f_capture.DitherNum:=0; // no dither after focus
         Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
         exit;
       end else begin
         NewMessage('Capture stopped during autofocus');
         f_capture.Stop;
         Capture:=false;
         exit;
       end;
     end else begin
       // failed, cancel current capture
       NewMessage('Autofocus failed!');
       f_capture.Stop;
       Capture:=false;
       exit;
     end;
  end;
  // check if dithering is required
  if f_capture.CheckBoxDither.Checked and (f_capture.DitherNum>=StrToIntDef(f_capture.DitherCount.Text,1)) then begin
    f_capture.DitherNum:=0;
    if autoguider.State=GUIDER_GUIDING then begin
      NewMessage('Dithering...');
      StatusBar1.Panels[1].Text:='Dithering...';
      autoguider.Dither(DitherPixel, DitherRAonly);
      autoguider.WaitBusy(SettleMaxTime);
      Wait(1);
    end else begin
      NewMessage('Not autoguiding! dithering ignored.');
    end;
  end;
  // set object for filename
  camera.ObjectName:=f_capture.Fname.Text;
  NewMessage('Starting '+f_capture.FrameType.Text+' exposure '+inttostr(f_capture.SeqCount)+' for '+f_capture.ExpTime.Text+' seconds');
  // disable BPM
  fits.SetBPM(bpm,0,0,0,0);
  // start exposure for time e
  camera.StartExposure(e);
end
else begin
   // camera not connected
   f_capture.Stop;
   Capture:=false;
   StatusBar1.Panels[1].Text := '';
end;
end;

procedure Tf_main.CameraProgress(n:double);
var txt: string;
begin
 if (n<=0) then begin
   if meridianflipping or autofocusing then exit;
   if ((f_capture.Running)or(f_preview.Running)) then begin
     txt := 'Downloading...';
     if Capture then begin
       if f_capture.Running then
         StatusBar1.Panels[1].Text := 'Seq: '+inttostr(f_capture.SeqCount)+' '+txt;
     end
     else begin
        StatusBar1.Panels[1].Text := txt;
     end;
   end
   else begin
      StatusBar1.Panels[1].Text := '';
   end;
 end else begin
  if n>=10 then txt:=FormatFloat(f0, n)
           else txt:=FormatFloat(f1, n);
  if Capture then begin
    if f_capture.Running then
      StatusBar1.Panels[1].Text := 'Seq: '+inttostr(f_capture.SeqCount)
                                   +'  Exp: '+txt+' sec.';
  end
  else begin
     StatusBar1.Panels[1].Text := 'Exp: '+txt+' sec.';
  end;
 end;
end;

procedure Tf_main.CameraNewImage(Sender: TObject);
begin
 Application.QueueAsyncCall(@CameraNewImageAsync,0);
end;

procedure Tf_main.CameraNewImageAsync(Data: PtrInt);
var dt: Tdatetime;
    fn,imgsize,buf: string;
    subseq,subobj,substep,subfrt,subexp,subbin: boolean;
    fnobj,fnfilter,fndate,fnexp,fnbin: boolean;
    fileseqnum: integer;
begin
  dt:=NowUTC;
  ImgFrameX:=FrameX;
  ImgFrameY:=FrameY;
  ImgFrameW:=FrameW;
  ImgFrameH:=FrameH;
  imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
  DrawHistogram(true);
  DrawImage;
  if Capture then begin
     subseq:=config.GetValue('/Files/SubfolderSequence',false);
     subobj:=config.GetValue('/Files/SubfolderObjname',false);
     subfrt:=config.GetValue('/Files/SubfolderFrametype',false);
     substep:=config.GetValue('/Files/SubfolderStep',false);
     subexp:=config.GetValue('/Files/SubfolderExposure',false);
     subbin:=config.GetValue('/Files/SubfolderBinning',false);
     fn:=slash(config.GetValue('/Files/CapturePath',defCapturePath));
     if subseq and f_sequence.Running then fn:=slash(fn+trim(f_sequence.CurrentName));
     if subfrt then fn:=slash(fn+trim(f_capture.FrameType.Text));
     if subobj then begin
       buf:=StringReplace(f_capture.Fname.Text,' ','',[rfReplaceAll]);
       buf:=StringReplace(buf,'/','_',[rfReplaceAll]);
       buf:=StringReplace(buf,'\','_',[rfReplaceAll]);
       buf:=StringReplace(buf,':','_',[rfReplaceAll]);
       fn:=slash(fn+buf);
     end;

     if substep and f_sequence.Running then begin
        if f_sequence.StepTotalCount>1 then begin
          fn:=slash(fn+trim(f_sequence.CurrentStep)+'_'+IntToStr(f_sequence.StepRepeatCount))
        end
        else begin
          fn:=slash(fn+trim(f_sequence.CurrentStep));
        end;
     end;
     if subexp then fn:=slash(fn+StringReplace(f_capture.ExpTime.Text,'.','_',[])+'s');
     if subbin then fn:=slash(fn+f_capture.Binning.Text);
     ForceDirectoriesUTF8(fn);
     fnobj:=config.GetValue('/Files/FilenameObjname',true);
     fnfilter:=config.GetValue('/Files/FilenameFilter',true);
     fndate:=config.GetValue('/Files/FilenameDate',true);
     fnexp:=config.GetValue('/Files/FilenameExposure',false);
     fnbin:=config.GetValue('/Files/FilenameBinning',false);
     if fnobj then begin
       if trim(f_capture.FrameType.Text)=trim(FrameName[0]) then begin
           buf:=StringReplace(f_capture.Fname.Text,' ','',[rfReplaceAll]);
           buf:=StringReplace(buf,'/','_',[rfReplaceAll]);
           buf:=StringReplace(buf,'\','_',[rfReplaceAll]);
           buf:=StringReplace(buf,':','_',[rfReplaceAll]);
           fn:=fn+buf+'_';
       end
       else
           fn:=fn+trim(f_capture.FrameType.Text)+'_';
     end;
     if fnfilter and (wheel.Status=devConnected)and(f_capture.FrameType.ItemIndex<>1)and(f_capture.FrameType.ItemIndex<>2) then
         fn:=fn+trim(wheel.FilterNames[wheel.Filter])+'_';
     if fnexp then fn:=fn+StringReplace(f_capture.ExpTime.Text,'.','_',[])+'s_';
     if fnbin then fn:=fn+f_capture.Binning.Text+'_';
     if fndate then
        fn:=fn+FormatDateTime('yyyymmdd_hhnnss',dt)
     else begin
        fileseqnum:=1;
        while FileExistsUTF8(fn+IntToStr(fileseqnum)+'.fits') do
          inc(fileseqnum);
        fn:=fn+IntToStr(fileseqnum);
     end;
     fn:=fn+'.fits';
     fits.SaveToFile(fn);
     NewMessage('Saved file '+fn);
     StatusBar1.Panels[2].Text:='Saved '+fn+' '+imgsize;
     StatusBar1.Panels[1].Text := '';
     f_capture.SeqCount:=f_capture.SeqCount+1;
     f_capture.DitherNum:=f_capture.DitherNum+1;
     f_capture.FocusNum:=f_capture.FocusNum+1;
     if f_capture.SeqCount<=StrToInt(f_capture.SeqNum.Text) then begin
        if f_capture.Running then Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
     end else begin
        Capture:=false;
        f_capture.Stop;
        NewMessage('Stop capture');
        StatusBar1.Panels[1].Text := 'Seq: '+inttostr(f_capture.SeqCount-1)+' Finished';
        MenuCaptureStart.Caption:=f_capture.BtnStart.Caption
     end;
  end
  else if Preview then begin
    StatusBar1.Panels[2].Text:='Preview '+FormatDateTime('hh:nn:ss',now)+'  '+imgsize;
    if f_preview.Loop and f_preview.Running then Application.QueueAsyncCall(@StartPreviewExposureAsync,0)
       else begin
         f_preview.stop;
         Preview:=false;
         NewMessage('End preview');
         StatusBar1.Panels[1].Text:='';
    end;
  end;
end;

procedure Tf_main.CameraVideoFrame(Sender: TObject);
begin
  Application.QueueAsyncCall(@CameraVideoFrameAsync,0);
end;

procedure Tf_main.CameraVideoFrameAsync(Data: PtrInt);
begin
ImgFrameX:=FrameX;
ImgFrameY:=FrameY;
ImgFrameW:=FrameW;
ImgFrameH:=FrameH;
DrawHistogram(true);
DrawImage;
end;

Procedure Tf_main.RedrawHistogram(Sender: TObject);
begin
  DrawHistogram(false);
end;

Procedure Tf_main.Redraw(Sender: TObject);
begin
  DrawHistogram(false);
  DrawImage;
end;

Procedure Tf_main.ZoomImage(Sender: TObject);
begin
  ImgZoom:=f_visu.Zoom;
  PlotImage;
end;

procedure debayer(raw: TBGRABitmap; t:TBayerMode; var ima:TBGRABitmap) ;
var
 i,j,k:integer;
 pix1,pix2,pix3,pix4,pix5,pix6,pix7,pix8,pix9:byte;
 p,p1,p2,p3: PBGRAPixel;
 imgW,imgH:Integer;
 pixel:TBGRAPixel;
begin
imgW:=raw.width;
imgH:=raw.height;
ima.SetSize(imgW,imgH);
pixel:=BGRABlack;
k:=1;
for i:=0 to imgH-1 do begin
 p:=ima.scanline[i];
 p1:=raw.ScanLine[max(i-1,0)];
 p2:=raw.ScanLine[i];
 p3:=raw.ScanLine[min(i+1,imgH-1)];
 for j:=0 to imgW-1 do begin
   pix1:= p1[max(j-1,0)*k].red;
   pix2:= p1[j*k].red;
   pix3:= p1[min(j+1,imgW-1)*k].red;
   pix4:= p2[max(j-1,0)*k].red;
   pix5:= p2[j*k].red;
   pix6:= p2[min(j+1,imgW-1)*k].red;
   pix7:= p3[max(j-1,0)*k].red;
   pix8:= p3[j*k].red;
   pix9:= p3[min(j+1,imgW-1)*k].red;
   if (i mod 2)>0 then begin //ligne paire
      if (j mod 2)>0 then begin //colonne paire et ligne paire
        case t of
        bayerGR: begin
            pixel.red:= round(RedBalance*(pix2+pix8)/2);
            pixel.green:= round(GreenBalance*pix5);
            pixel.blue:= round(BlueBalance*(pix4+pix6)/2);
           end;
        bayerRG: begin
            pixel.red:= round(RedBalance*(pix1+pix3+pix7+pix9)/4);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:=round(BlueBalance*pix5);
           end;
        bayerBG: begin
            pixel.red:= round(RedBalance*pix5);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:= round(BlueBalance*(pix1+pix3+pix7+pix9)/4);
           end;
        bayerGB: begin
            pixel.red:= round(RedBalance*(pix4+pix6)/2);
            pixel.green:= round(GreenBalance*pix5);
            pixel.blue:= round(BlueBalance*(pix2+pix8)/2);
           end;
        end;
      end
      else begin //colonne impaire et ligne paire
        case t of
        bayerGR: begin
            pixel.red:= round(RedBalance*(pix1+pix3+pix7+pix9)/4);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:=round(BlueBalance*pix5);
           end;
        bayerRG: begin
            pixel.red:= round(RedBalance*(pix2+pix8)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:=round(BlueBalance*(pix4+pix6)/2);
           end;
        bayerBG: begin
            pixel.red:= round(RedBalance*(pix4+pix6)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:=round(BlueBalance*(pix2+pix8)/2);
           end;
        bayerGB: begin
            pixel.red:=round(RedBalance*pix5);
            pixel.green:= round(GreenBalance**(pix2+pix4+pix6+pix8)/4);
            pixel.blue:= round(BlueBalance*(pix1+pix3+pix7+pix9)/4);
           end;
        end;
      end;
   end
   else begin //ligne impaire
      if (j mod 2)>0 then begin //colonne paire et ligne impaire
        case t of
        bayerGR: begin
            pixel.red:=round(RedBalance*pix5);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:= round(BlueBalance*(pix1+pix3+pix7+pix9)/4);
           end;
        bayerRG: begin
            pixel.red:= round(RedBalance*(pix4+pix6)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:=round(BlueBalance*(pix2+pix8)/2);
           end;
        bayerBG: begin
            pixel.red:=round(RedBalance*(pix2+pix8)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:= round(BlueBalance*(pix4+pix6)/2);
           end;
        bayerGB: begin
            pixel.red:= round(RedBalance*(pix1+pix3+pix7+pix9)/4);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:=round(BlueBalance*pix5);
           end;
        end;
      end
      else begin //colonne impaire et ligne impaire
        case t of
        bayerGR: begin
            pixel.red:= round(RedBalance*(pix4+pix6)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:= round(BlueBalance*(pix2+pix8)/2);
           end;
        bayerRG: begin
            pixel.red:=round(RedBalance*pix5);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:= round(BlueBalance*(pix1+pix3+pix7+pix9)/4);
           end;
        bayerBG: begin
            pixel.red:= round(RedBalance*(pix1+pix3+pix7+pix9)/4);
            pixel.green:= round(GreenBalance*(pix2+pix4+pix6+pix8)/4);
            pixel.blue:=round(BlueBalance*pix5);
           end;
        bayerGB: begin
            pixel.red:= round(RedBalance*(pix2+pix8)/2);
            pixel.green:=round(GreenBalance*pix5);
            pixel.blue:= round(BlueBalance*(pix4+pix6)/2);
           end;
        end;
      end;
   end;
   p[j]:=pixel;
end;
end;
end;

Procedure Tf_main.DrawImage;
var tmpbmp:TBGRABitmap;
    co: TBGRAPixel;
    s,cx,cy: integer;
begin
if fits.HeaderInfo.naxis>0 then begin
  if f_visu.BtnLinear.Checked then fits.itt:=ittlinear
  else if f_visu.BtnLog.Checked then fits.itt:=ittlog
  else if f_visu.BtnSqrt.Checked then fits.itt:=ittsqrt;
  fits.ImgDmax:=round(f_visu.ImgMax);
  fits.ImgDmin:=round(f_visu.ImgMin);
  fits.GetBGRABitmap(ImaBmp);
  ImgPixRatio:=fits.HeaderInfo.pixratio;
  if BayerColor or (fits.HeaderInfo.pixratio<>1) then begin
     if BayerColor then begin
       tmpbmp:=TBGRABitmap.Create;
       debayer(ImaBmp,BayerMode,tmpbmp);
       ImaBmp.Assign(tmpbmp);
       tmpbmp.Free;
     end;
     if (fits.HeaderInfo.pixratio<>1) then begin
       tmpbmp:=TBGRABitmap.Create(ImaBmp);
       ImaBmp.SetSize(round(fits.HeaderInfo.pixratio*ImaBmp.Width),ImaBmp.Height);
       ImaBmp.Canvas.StretchDraw(rect(0,0,ImaBmp.Width,ImaBmp.Height),tmpbmp.Bitmap);
       tmpbmp.Free;
     end;
  end;
  if refmask then begin
    ImaBmp.StretchPutImage(rect(0,0,ImaBmp.Width,ImaBmp.Height),refbmp,dmLinearBlend);
  end;
  img_Width:=ImaBmp.Width;
  img_Height:=ImaBmp.Height;
  if Preview or Capture then begin // not on control exposure
    if f_starprofile.AutofocusRunning then
       f_starprofile.Autofocus(fits.image,fits.imageC,fits.imageMin,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow div camera.BinX,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2)
    else if f_starprofile.FindStar or f_starprofile.ChkFocus.Checked then
      f_starprofile.showprofile(fits.image,fits.imageC,fits.imageMin,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow div camera.BinX,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,mount.FocaleLength,camera.PixelSize);
  end;
  if f_visu.BullsEye then begin
    co:=ColorToBGRA(clRed);
    cx:=img_Width div 2;
    cy:=img_Height div 2;
    imabmp.DrawHorizLine(0,cy,img_Width,co);
    imabmp.DrawVertLine(cx,0,img_Height,co);
    s:=min(img_Height,img_Width) div 3;
    imabmp.EllipseAntialias(cx,cy,s,s,co,1);
    s:=min(img_Height,img_Width) div 8;
    imabmp.EllipseAntialias(cx,cy,s,s,co,1);
    if fits.HeaderInfo.solved and (cdcWCSinfo.secpix<>0) then plot_north(imabmp);
  end;
  PlotImage;
end;
end;

Procedure Tf_main.ClearImage;
begin
image1.Picture.Bitmap.Canvas.Brush.Color:=clDarkBlue;
image1.Picture.Bitmap.Canvas.Pen.Color:=clBlack;
image1.Picture.Bitmap.Canvas.FillRect(0,0,image1.Width,image1.Height);
EndX:=-1;
end;

Procedure Tf_main.PlotImage;
var r1,r2: double;
    w,h,px,py: integer;
    tmpbmp,str: TBGRABitmap;
begin
if (img_Height=0)or(img_Width=0) then exit;
r1:=image1.Width/imabmp.Width;
r2:=image1.Height/imabmp.Height;
ZoomMin:=min(r1,r2);
if (ImgZoom<ZoomMin)or(abs(ImgZoom-ZoomMin)<0.01) then ImgZoom:=0;
ClearImage;
imabmp.ResampleFilter:=rfBestQuality;
if ImgZoom=0 then begin
  // adjust
  r1:=img_Width/img_Height;
  w:=image1.width;
  h:=image1.height;
  r2:=w/h;
  if r1>r2 then begin
    h:=trunc(w/r1);
    ImgScale0:=h/img_Height;
  end else begin
    w:=trunc(h*r1);
    ImgScale0:=w/img_Width;
  end;
  str:=ImaBmp.Resample(w,h) as TBGRABitmap;
  str.Draw(image1.Picture.Bitmap.Canvas,0,0,True);
  str.Free;
end
else if ImgZoom=1 then begin
   // zoom 1
   px:=ImgCx-((img_Width-Image1.Width) div 2);
   py:=ImgCy-((img_Height-Image1.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   ImaBmp.Draw(image1.Picture.Bitmap.Canvas,px,py,True);
end
else begin
   // other zoom
   if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
   tmpbmp:=TBGRABitmap.Create(round(Image1.Width/ImgZoom),round(Image1.Height/ImgZoom),clDarkBlue);
   px:=ImgCx-((img_Width-tmpbmp.Width) div 2);
   py:=ImgCy-((img_Height-tmpbmp.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   tmpbmp.PutImage(px,py,ImaBmp,dmSet);
   str:=tmpbmp.Resample(image1.Width,image1.Height,rmSimpleStretch) as TBGRABitmap;
   str.Draw(image1.Picture.Bitmap.Canvas,0,0,True);
   str.Free;
   tmpbmp.Free;
end;
Application.ProcessMessages;
end;

procedure Tf_main.plot_north(bmp:TBGRABitmap);

var scale,s,c: double;
    xpos,ypos,leng: single;
begin
if fits.HeaderInfo.solved and
 (cdcWCSinfo.secpix<>0) and
 (abs(cdcWCSinfo.cdec)<89.0)
 then begin
  scale:=img_Width/image1.Width;
  xpos:=27*scale;
  ypos:=27*scale;
  leng:=24*scale;

  sincos(WCSxyrot,s,c);
  bmp.ArrowEndAsClassic(false,false,3);
  bmp.DrawLineAntialias(xpos,ypos,xpos+leng*s,ypos+leng*c,ColorToBGRA(clRed),scale);
  bmp.ArrowEndAsNone;

  bmp.FontHeight:=round(12*scale);
  bmp.TextOut(xpos,ypos,'  '+FormatFloat(f1,Rmod(cdcWCSinfo.rot+360,360)),clRed);
 end;
end;

procedure Tf_main.Image1Paint(Sender: TObject);
var x,y,s,r: integer;
begin
  Inherited paint;
  if f_starprofile.FindStar and(f_starprofile.StarX>0)and(f_starprofile.StarY>0) then begin
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
     if ImgZoom=0 then begin
       s:=round((Starwindow/camera.BinX/2)*ImgScale0);
       r:=round(f_starprofile.HFD*ImgScale0/2);
     end
     else  begin
       s:=round(ImgZoom*Starwindow/camera.BinX/2);
       r:=round(ImgZoom*f_starprofile.HFD/2);
     end;
     with Image1.Canvas do begin
        Pen.Color:=clLime;
        Frame(x-s,y-s,x+s,y+s);
        brush.Style:=bsClear;
        if r>0 then EllipseC(x,y,r,r);
        brush.Style:=bsSolid;
     end;
  end;
end;

Procedure Tf_main.DrawHistogram(SetLevel: boolean);
begin
  if fits.HeaderInfo.naxis>0 then begin
     f_visu.DrawHistogram(fits.Histogram,SetLevel);
  end;
end;

procedure Tf_main.MenuIndiSettingsClick(Sender: TObject);
begin
  if not GUIready then begin
     f_indigui:=Tf_indigui.Create(self);
     f_indigui.onDestroy:=@GUIdestroy;
     f_indigui.IndiServer:=config.GetValue('/INDI/Server','');
     f_indigui.IndiPort:=config.GetValue('/INDI/ServerPort','');
     GUIready:=true;
  end;
  FormPos(f_indigui,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_indigui.Show;
end;

procedure Tf_main.MenuItemDebayerClick(Sender: TObject);
begin
  BayerColor:=True;
  DrawImage;
  NewMessage('Image Debayered');
end;

procedure Tf_main.MenuItemRawClick(Sender: TObject);
begin
  BayerColor:=False;
  DrawImage;
  NewMessage('Image Un-debayered');
end;

procedure Tf_main.MenuMountParkClick(Sender: TObject);
begin
  f_mount.BtnPark.Click;
end;

procedure Tf_main.MenuMountTrackClick(Sender: TObject);
begin
  f_mount.BtnTrack.Click;
end;

procedure Tf_main.MenuOpenClick(Sender: TObject);
var fn: string;
begin
  if OpenDialog1.Execute then begin
     fn:=OpenDialog1.FileName;
     LoadFitsFile(fn);
  end;
end;

procedure Tf_main.MenuSaveClick(Sender: TObject);
var fn: string;
begin
if fits.HeaderInfo.naxis>0 then begin
   if SaveDialog1.Execute then begin
      fn:=SaveDialog1.FileName;
      SaveFitsFile(fn);
   end;
end;
end;

procedure Tf_main.SetRefImage;
var mem: TMemoryStream;
    i: integer;
    p: PBGRAPixel;
    f: TFits;
begin
if refmask then begin
  refmask:=false;
  mem:=TMemoryStream.Create;
  f:=TFits.Create(nil);
  try
  mem.LoadFromFile(reffile);
  f.Stream:=mem;
  f.LoadStream;
  if f.HeaderInfo.naxis>0 then begin
    if f_visu.BtnLinear.Checked then f.itt:=ittlinear
    else if f_visu.BtnLog.Checked then f.itt:=ittlog
    else if f_visu.BtnSqrt.Checked then f.itt:=ittsqrt;
    f.ImgDmax:=round(f_visu.ImgMax);
    f.ImgDmin:=round(f_visu.ImgMin);
    f.GetBGRABitmap(refbmp);
    p:=refbmp.data;
    for i:=0 to refbmp.NbPixels-1 do begin
     p[i].alpha:=128;
     case refcolor of
       0: begin
          p[i].blue:=0;
          p[i].green:=0;
          if p[i].red<reftreshold then
             p[i].red:=0
          else
               p[i].red:=180;
          end;
       1: begin
          p[i].blue:=0;
          p[i].red:=0;
          if p[i].green<reftreshold then p[i].green:=0 else p[i].green:=180;
          end;
       2: begin
          p[i].red:=0;
          p[i].green:=0;
          if p[i].blue<reftreshold then p[i].blue:=0 else p[i].blue:=180;
          end;
       end;
    end;
    refbmp.InvalidateBitmap;
    refmask:=true;
    DrawImage;
  end;
  finally
    mem.Free;
    f.free;
  end;
end;
end;

procedure Tf_main.MenuRefimageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenRefImage(OpenDialog1.FileName);
  end;
end;

procedure Tf_main.MenuClearRefClick(Sender: TObject);
begin
  ClearRefImage(Sender);
end;

procedure Tf_main.MenuConnectClick(Sender: TObject);
begin
  f_devicesconnection.BtnConnect.Click;
end;

procedure Tf_main.MenuFilterClick(Sender: TObject);
begin
  wheel.Filter:=TMenuItem(Sender).Tag;
end;

procedure Tf_main.MenuFocusaidClick(Sender: TObject);
begin
  f_starprofile.ChkFocus.Checked:=not f_starprofile.ChkFocus.Checked;
end;

procedure Tf_main.MenuFocuserInClick(Sender: TObject);
begin
  FocusIN(Sender);
end;

procedure Tf_main.MenuFocuserOutClick(Sender: TObject);
begin
  FocusOUT(Sender);
end;

procedure Tf_main.MenuFrameResetClick(Sender: TObject);
begin
   ResetFrame(Sender);
end;

procedure Tf_main.MenuFrameSetClick(Sender: TObject);
begin
  SetFrame(Sender);
end;

Procedure Tf_main.FocusStart(Sender: TObject);
var x,y,xc,yc,s,s2: integer;
    vmax:double;
begin
  if  f_capture.Running  then begin
    NewMessage('Cannot start manual focus now, stop capture and retry');
    f_starprofile.ChkFocus.Checked:=false;
    exit;
  end;
  if (not f_starprofile.FindStar)and(fits.HeaderInfo.valid) then begin
    x:=fits.HeaderInfo.naxis1 div 2;
    y:=fits.HeaderInfo.naxis2 div 2;
    s:=min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 2;
    f_starprofile.FindBrightestPixel(fits.image,fits.imageC,fits.imageMin,x,y,s,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,starwindow div (2*camera.BinX),xc,yc,vmax);
    f_starprofile.FindStar:=(vmax>0);
    f_starprofile.StarX:=xc;
    f_starprofile.StarY:=yc;
  end;
  if f_starprofile.FindStar then begin
     s:=Focuswindow div camera.BinX;
     s2:=s div 2;
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
     Screen2CCD(x,y,camera.VerticalFlip,xc,yc);
     if camera.CameraInterface=INDI then begin
       // INDI frame in unbinned pixel
       xc:=xc*camera.BinX;
       yc:=yc*camera.BinY;
     end;
     camera.SetFrame(xc-s2,yc-s2,s,s);
     f_starprofile.StarX:=s2;
     f_starprofile.StarY:=s2;
     SaveFocusZoom:=f_visu.Zoom;
     f_visu.Zoom:=0;
     ImgZoom:=0;
     if not f_preview.Loop then f_preview.Loop:=true;
     if not f_preview.Running then begin
       f_preview.Running:=true;
       StartPreviewExposure(nil);
     end;
     NewMessage('Focus aid started');
  end
  else begin
    f_starprofile.ChkFocus.Checked:=false;
    NewMessage('Select a star first!');
  end;
end;

Procedure Tf_main.FocusStop(Sender: TObject);
begin
   if  f_capture.Running then exit;
   f_preview.Running:=false;
   f_preview.Loop:=false;
   camera.AbortExposure;
   fits.SetBPM(bpm,0,0,0,0);
   camera.ResetFrame;
   f_visu.Zoom:=SaveFocusZoom;
   ImgZoom:=f_visu.Zoom;
   f_starprofile.StarX:=-1;
   f_starprofile.StarY:=-1;
   f_starprofile.FindStar:=false;
   StartPreviewExposure(nil);
   NewMessage('Focus aid stopped');
   if focuser.hasTemperature then NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
end;

procedure Tf_main.LoadFocusStar;
var f: textfile;
    buf,fn,id: string;
    ra,de: double;
    focusmag: integer;
begin
 fn:='focus_star_4';
 SetLength(FocusStars,1000);
 NFocusStars:=0;
 focusmag:=config.GetValue('/StarAnalysis/AutofocusStarMag',4);
 if (focusmag<4)or(focusmag>8) then focusmag:=4;
 case focusmag of
  4: fn:='focus_star_4';
  5: fn:='focus_star_5';
  6: fn:='focus_star_6';
  7: fn:='focus_star_7';
  8: fn:='focus_star_8';
 end;
 try
 AssignFile(f,slash(DataDir)+slash('stars')+fn);
 reset(f);
 repeat
   readln(f,buf);
   ra:=StrToFloatDef(copy(buf,8,12),NullCoord);
   de:=StrToFloatDef(copy(buf,21,12),NullCoord);
   id:='HIP '+trim(copy(buf,1,6));
   if (ra<>NullCoord)and(de<>NullCoord) then begin
     inc(NFocusStars);
     if NFocusStars>=Length(FocusStars) then
        SetLength(FocusStars,NFocusStars+1000);
     ra:=deg2rad*ra;
     de:=deg2rad*de;
     // store coordinates of the date
     J2000ToApparent(ra,de);
     FocusStars[NFocusStars].ra:=ra;
     FocusStars[NFocusStars].de:=de;
     FocusStars[NFocusStars].id:=id;
   end;
 until eof(f);
 CloseFile(f);
 except
   NewMessage('Error loading focus star list '+slash(DataDir)+slash('stars')+fn);
 end;
end;

function Tf_main.FindFocusStar(tra, tde:double; out sra,sde: double; out id: string): Boolean;
var i: integer;
    jd0,CurSt,CurTime: double;
    Year, Month, Day: Word;
    d,dmin,hh,hl,ta,th,a,h: double;
    tm,sm: TPierSide;
begin
  // all parameters coordinates of the date
  result:=false;
  dmin:=pi2;
  DecodeDate(now, Year, Month, Day);
  CurTime:=frac(now)*24;
  jd0:=jd(Year,Month,Day,0);
  CurST:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
  hh:=CurSt-tra;
  Eq2Hz(hh,tde,ta,th);
  tm:=mount.PierSide;
  if tm=pierUnknown then begin
    if (ta>0)and(ta<pi) then
      tm:=pierEast
    else
      tm:=pierWest;
  end;
  hl:=max(th,30*deg2rad);
  hl:=min(th,70*deg2rad);
  for i:=1 to NFocusStars do begin
    if pos(' '+FocusStars[i].id+' ',FocusStarsBlacklist)>0 then continue;
    d:=AngularDistance(tra,tde,FocusStars[i].ra,FocusStars[i].de);
    if d<dmin then begin
      hh:=CurSt-FocusStars[i].ra;
      Eq2Hz(hh,FocusStars[i].de,a,h);
      if (a>0)and(a<pi) then
        sm:=pierEast
      else
        sm:=pierWest;
      if (h>hl)and(tm=sm) then begin
        sra:=FocusStars[i].ra;
        sde:=FocusStars[i].de;
        id:=FocusStars[i].id;
        dmin:=d;
        result:=true;
      end;
    end;
  end;
end;

function Tf_main.AutoAutofocus(ReturnToTarget: boolean=true): Boolean;
var tra,tde,teq,tpa,sra,sde,jd0,jd1,err: double;
    sid: string;
    focusretry,maxretry: integer;
    tpos,pslew,savecapture,restartguider: boolean;
begin
 maxretry:=3;
 result:=false;
 CancelAutofocus:=false;
 if autofocusing then begin
   NewMessage('Autofocus already running.');
   exit;
 end;
 if (AutofocusMode=afNone) then begin
   NewMessage('Please configure the Autofocus options.');
   f_starprofile.ChkAutofocus.Checked:=false;
   exit;
 end;
 if (AutofocusMode=afVcurve) and((AutofocusVcDir<>AutofocusMoveDir)or(AutofocusVcNum<=0)) then begin
   NewMessage('Please run the V-curve learning for this focuser direction first. Button V-learn.');
   f_starprofile.ChkAutofocus.Checked:=false;
   exit;
 end;
 autofocusing:=true;
 savecapture:=Capture;
 try
 Capture:=false;
 NewMessage('Autofocus now');
 tpos:=false;
 pslew:=false;
 // stop autoguider
 restartguider:=(Autoguider.State<>GUIDER_DISCONNECTED);
 if restartguider and (Autoguider.State=GUIDER_GUIDING) then begin
   NewMessage('Stop autoguider');
   autoguider.Guide(false);
   autoguider.WaitBusy(15);
 end;
 if InplaceAutofocus then begin
   NewMessage('Stay at the current position for autofocus');
   if CancelAutofocus then exit;
   // do autofocus
   if focuser.hasTemperature then NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
   f_starprofile.ChkAutofocus.Checked:=true;
   while f_starprofile.ChkAutofocus.Checked do begin
    sleep(100);
    Application.ProcessMessages;
    if CancelAutofocus then begin
      f_starprofile.ChkAutofocus.Checked:=false;
      exit;
    end;
   end;
   // check result
   if f_starprofile.AutofocusResult then begin
     result:=true;
   end
   else begin
      NewMessage('In place autofocus failed, try with the focus stars list.');
      f_preview.ControlExposure(AutofocusExposure*AutofocusExposureFact,AutofocusBinning,AutofocusBinning);
      wait(1);
   end;
 end;
 if (not InplaceAutofocus) or (not result) then begin
   // get current position from target object
   if (f_sequence.Running) and (f_sequence.TargetCoord) then begin
     NewMessage('Get current position from current target');
     if (f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord) then begin
       tra:=f_sequence.TargetRA;
       tde:=f_sequence.TargetDE;
       tpos:=true;
       pslew:=true;
     end;
   end;
   // get current position from last capture image
   if (not tpos)and(f_capture.Running) and fits.HeaderInfo.valid and (astrometryResolver<>ResolverNone) then begin
     NewMessage('Get current position from last image');
     astrometry.SolveCurrentImage(true);
     if astrometry.LastResult then begin
       astrometry.CurrentCoord(tra,tde,teq,tpa);
       tra:=deg2rad*15*tra;
       tde:=deg2rad*tde;
       J2000ToApparent(tra,tde);
       tpos:=true;
       pslew:=true;
     end
     else begin
      NewMessage('Cannot solve current image.');
     end;
   end;
   // get current position from telescope
   if (not tpos) then begin
    NewMessage('Get current position from telescope');
    tra:=deg2rad*15*mount.RA;
    tde:=deg2rad*mount.Dec;
    if mount.Equinox<>0 then begin
      jd0:=Jd(trunc(mount.Equinox),0,0,0);
      jd1:=DateTimetoJD(now);
      PrecessionFK5(jd0,jd1,tra,tde);
    end;
    pslew:=false;
    tpos:=true;
   end;
   if CancelAutofocus then exit;
   // Loop star list until focus success
   focusretry:=0;
   FocusStarsBlacklist:='';
   repeat
     inc(focusretry);
     // search focus star
     if FindFocusStar(tra,tde,sra,sde,sid) then begin
       // slew to star
       NewMessage('Slew to focus star '+sid);
       if mount.Equinox<>0 then begin
         jd0:=Jd(trunc(mount.Equinox),0,0,0);
         jd1:=DateTimetoJD(now);
         PrecessionFK5(jd1,jd0,sra,sde);
       end;
       astrometry.AutofocusPrecisionSlew(rad2deg*sra/15,rad2deg*sde,err);
     end
     else begin
      NewMessage('Cannot find a focus star.');
      if NFocusStars=0 then NewMessage('Star database is empty, check path to star database files!!');
      exit;
     end;
     wait(1);
     if CancelAutofocus then exit;
     // do autofocus
     if focuser.hasTemperature then NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
     f_starprofile.ChkAutofocus.Checked:=true;
     while f_starprofile.ChkAutofocus.Checked do begin
      sleep(100);
      Application.ProcessMessages;
      if CancelAutofocus then begin
        f_starprofile.ChkAutofocus.Checked:=false;
        exit;
      end;
     end;
     // if not successful, blacklist the current star and try another
     if not f_starprofile.AutofocusResult then begin
        FocusStarsBlacklist:=FocusStarsBlacklist+' '+sid+' ';
        NewMessage('Autofocus failed, try with another star...');
     end;
     if CancelAutofocus then exit;
   until f_starprofile.AutofocusResult or (focusretry>=maxretry);
   if not f_starprofile.AutofocusResult then begin
      NewMessage('Autofocus failed after '+inttostr(maxretry)+' retries, continue without focusing.');
   end;
   if CancelAutofocus then exit;
   if ReturnToTarget then begin
     // recenter to previous position
     NewMessage('Return to target position');
     if mount.Equinox<>0 then begin
       jd0:=Jd(trunc(mount.Equinox),0,0,0);
       jd1:=DateTimetoJD(now);
       PrecessionFK5(jd1,jd0,tra,tde);
     end;
     if pslew then begin
        result:=astrometry.PrecisionSlew(rad2deg*tra/15,rad2deg*tde,err);
     end else begin
        result:=mount.Slew(rad2deg*tra/15,rad2deg*tde);
     end;
     if CancelAutofocus then exit;
    end
   else begin
    result:=true;
   end;
 end;
 // start autoguider
 if restartguider then begin
  NewMessage('Restart autoguider');
  autoguider.Guide(false);
  wait(5);
  autoguider.Guide(true);
  autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
  if autoguider.State<>GUIDER_GUIDING then begin
    f_pause.Caption:='Pause';
    f_pause.Text:='Failed to start guiding!';
    NewMessage(f_pause.Text);
    if not f_pause.Wait(120) then begin
       NewMessage('Failed to start guiding!');
       result:=false;
    end;
  end;
 end;
 Wait(2);
 finally
   autofocusing:=false;
   CancelAutofocus:=false;
   Capture:=savecapture;
 end;
end;

procedure Tf_main.cmdAutomaticAutofocus(var ok: boolean);
begin
  ok:=AutoAutofocus;
end;

procedure Tf_main.cmdAutofocus(var ok: boolean);
begin
 f_starprofile.ChkAutofocus.Checked:=true;
 while f_starprofile.ChkAutofocus.Checked do begin
  sleep(100);
  Application.ProcessMessages;
 end;
 ok:=f_starprofile.AutofocusResult;
end;

Procedure Tf_main.AutoFocusStart(Sender: TObject);
var x,y,xc,yc,s,s2: integer;
    vmax: double;
begin
  f_starprofile.AutofocusResult:=false;
  SaveAutofocusBinning:=f_preview.Binning.Text;
  SaveAutofocusBX:=camera.BinX;
  SaveAutofocusBY:=camera.BinY;
  camera.GetFrame(SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH);
  if (camera.Status<>devConnected)or(focuser.Status<>devConnected) then begin
   NewMessage('Camera or focuser are not connected');
   f_starprofile.ChkAutofocus.Checked:=false;
   exit;
  end;
  if  f_preview.Running then begin
   NewMessage('Cannot start autofocus now, stop preview and retry');
   f_starprofile.ChkAutofocus.Checked:=false;
   exit;
  end;
  if  astrometry.Busy then begin
   NewMessage('Cannot start autofocus now, astrometry is running');
   f_starprofile.ChkAutofocus.Checked:=false;
   exit;
  end;
  if (AutofocusMode=afNone) then begin
    NewMessage('Please configure the Autofocus options.');
    f_starprofile.ChkAutofocus.Checked:=false;
    exit;
  end;
  if (f_capture.Running and (not autofocusing)) then begin
    NewMessage('Cannot start autofocus now, stop capture and retry');
    f_starprofile.ChkAutofocus.Checked:=false;
    exit;
  end;
  if (AutofocusMode=afVcurve) and((AutofocusVcDir<>AutofocusMoveDir)or(AutofocusVcNum<=0)or(AutofocusVcpiL<0)or(AutofocusVcpiR<0)) then begin
    NewMessage('Please run the V-curve learning for this focuser direction first. Button V-learn.');
    f_starprofile.ChkAutofocus.Checked:=false;
    exit;
  end;
  // start a new exposure as the current frame is probably not a preview
  f_preview.Exposure:=AutofocusExposure*AutofocusExposureFact;
  f_preview.Binning.Text:=inttostr(AutofocusBinning)+'x'+inttostr(AutofocusBinning);
  camera.SetBinning(AutofocusBinning,AutofocusBinning);
  fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  f_preview.ControlExposure(AutofocusExposure*AutofocusExposureFact,AutofocusBinning,AutofocusBinning);
  x:=fits.HeaderInfo.naxis1 div 2;
  y:=fits.HeaderInfo.naxis2 div 2;
  s:=min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 2;
  f_starprofile.FindBrightestPixel(fits.image,fits.imageC,fits.imageMin,x,y,s,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,starwindow div (2*camera.BinX),xc,yc,vmax);
  f_starprofile.FindStar:=(vmax>0);
  f_starprofile.StarX:=xc;
  f_starprofile.StarY:=yc;
  Image1.Invalidate;
  wait(1);
  if f_starprofile.FindStar then begin  // star selected OK
     s:=Focuswindow div camera.BinX;
     s2:=s div 2;
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
     Screen2CCD(x,y,camera.VerticalFlip,xc,yc);
     if camera.CameraInterface=INDI then begin
       // INDI frame in unbinned pixel
       xc:=xc*camera.BinX;
       yc:=yc*camera.BinY;
     end;
     camera.SetFrame(xc-s2,yc-s2,s,s);
     f_starprofile.StarX:=s2;
     f_starprofile.StarY:=s2;
     f_starprofile.InitAutofocus;
     SaveFocusZoom:=f_visu.Zoom;
     f_visu.Zoom:=0;
     ImgZoom:=0;
     if not f_preview.Loop then f_preview.Loop:=true;
     if not f_preview.Running then begin
       f_preview.Running:=true;
       StartPreviewExposure(nil);
     end;
     if focuser.hasTemperature then NewMessage('Focuser temperature: '+FormatFloat(f1,FocuserTemp));
     if f_starprofile.PreFocusPos>0 then
        NewMessage('AutoFocus started, initial position: '+inttostr(f_starprofile.PreFocusPos))
     else
        NewMessage('AutoFocus started');
  end
  else begin                             // no star, manual action is required
    f_starprofile.ChkAutofocus.Checked:=false;
    NewMessage('Autofocus cannot find a star!'+crlf+'Please adjust your parameters');
  end;
end;

Procedure Tf_main.AutoFocusStop(Sender: TObject);
begin
   if  f_capture.Running and (not autofocusing) then exit;
   f_preview.Running:=false;
   f_preview.Loop:=false;
   if not f_capture.Running then camera.AbortExposure;
   fits.SetBPM(bpm,0,0,0,0);
   f_preview.Binning.Text:=SaveAutofocusBinning;
   camera.SetBinning(SaveAutofocusBX,SaveAutofocusBY);
   camera.SetFrame(SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH);
   f_visu.Zoom:=SaveFocusZoom;
   ImgZoom:=f_visu.Zoom;
   f_starprofile.StarX:=-1;
   f_starprofile.StarY:=-1;
   f_starprofile.FindStar:=false;
   if f_starprofile.AutofocusResult then begin
     NewMessage('AutoFocus successful');
   end
   else begin
     NewMessage('AutoFocus error');
     if f_starprofile.PreFocusPos>0 then begin  // only for absolute position focuser
       NewMessage('Return the focuser to previous position '+inttostr(f_starprofile.PreFocusPos));
       f_focuser.FocusPosition:=f_starprofile.PreFocusPos;
       FocusSetAbsolutePosition(nil);
       Wait(1);
     end;
   end;
   if not f_capture.Running then StartPreviewExposure(nil);
end;

procedure Tf_main.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_main.AstrometryStart(Sender: TObject);
begin
  // update Menu
  MenuResolvePlanetarium.Enabled:=false;
  MenuResolveSync.Enabled:=false;
  MenuResolveSlew.Enabled:=false;
  MenuResolve.Enabled:=false;
  MenuResolveSlewCenter.Enabled:=false;
  {$ifdef mswindows}
  MenuViewAstrometryLog.Enabled:=false;
  {$endif}
  MenuStopAstrometry.Visible:=true;
end;

procedure Tf_main.AstrometryEnd(Sender: TObject);
begin
  // update Menu
  MenuStopAstrometry.Visible:=false;
  MenuResolvePlanetarium.Enabled:=true;
  MenuResolveSync.Enabled:=true;
  MenuResolveSlew.Enabled:=true;
  MenuResolve.Enabled:=true;
  MenuResolveSlewCenter.Enabled:=true;
  MenuViewAstrometryLog.Enabled:=true;
  if astrometry.LastResult then begin
     LoadFitsFile(astrometry.ResultFile);
     NewMessage(astrometry.Resolver+' resolve successful.');
  end else begin
    NewMessage(astrometry.Resolver+' resolve error.');
  end;
end;

procedure Tf_main.EndControlExposure(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:='';
end;

procedure Tf_main.MenuStopAstrometryClick(Sender: TObject);
begin
  astrometry.StopAstrometry;
end;

procedure Tf_main.MenuTabClick(Sender: TObject);
var i: integer;
begin
  i:=TMenuItem(Sender).Tag;
  PageControlRight.ActivePageIndex:=i;
end;

procedure Tf_main.MenuVideoPreviewClick(Sender: TObject);
begin
  f_video.Preview.Checked:=not f_video.Preview.Checked;
end;

procedure Tf_main.MenuVideoStartClick(Sender: TObject);
begin
  f_video.BtnStartRec.Click;
end;

procedure Tf_main.MenuVideoStopClick(Sender: TObject);
begin
  f_video.BtnStopRec.Click;
end;

procedure Tf_main.MenuViewAstrometryLogClick(Sender: TObject);
var logf: string;
begin
  logf:=slash(TmpDir)+'ccdcieltmp.log';
  if FileExistsUTF8(logf) then begin
    f_viewtext.Caption:='Astrometry resolver log';
    f_viewtext.Memo1.Clear;
    f_viewtext.Memo1.Lines.LoadFromFile(logf);
    FormPos(f_viewtext,mouse.CursorPos.X,mouse.CursorPos.Y);
    f_viewtext.Show;
  end;
end;

procedure Tf_main.MenuResolveClick(Sender: TObject);
begin
  astrometry.SolveCurrentImage(false);
end;

procedure Tf_main.ResolveSlewCenter(Sender: TObject);
var xx,yy,x,y,Timeout: integer;
    wt: boolean;
    endt: TDateTime;
begin
 if (Mount.Status<>devConnected)or(Camera.Status<>devConnected) then begin
   NewMessage('Camera and mount must be connected!');
   exit;
 end;
 wt:=(Sender<>nil);
 if fits.HeaderInfo.valid then begin
   xx:=fits.HeaderInfo.naxis1 div 2;
   yy:=fits.HeaderInfo.naxis2 div 2;
   Fits2Screen(xx,yy,x,y);
   astrometry.SlewScreenXY(x,y);
   if wt then begin
     Timeout:=600;
     endt:=now+Timeout/secperday;
     while (astrometry.SlewBusy)and(now<endt) do begin
        sleep(100);
        Application.ProcessMessages;
     end;
   end;
 end;
end;

procedure Tf_main.MenuResolveSlewCenterClick(Sender: TObject);
begin
 ResolveSlewCenter(nil);
end;

procedure Tf_main.MenuResolveSyncClick(Sender: TObject);
begin
  astrometry.SyncCurrentImage(false);
end;

procedure Tf_main.ResolveRotate(Sender: TObject);
var ra,de,eq,pa: double;
begin
 if (rotator.Status<>devConnected)or(Camera.Status<>devConnected) then begin
   NewMessage('Camera and rotator must be connected!');
   exit;
 end;
 if fits.HeaderInfo.valid then begin
   astrometry.SolveCurrentImage(true);
   if astrometry.CurrentCoord(ra,de,eq,pa) then begin
     rotator.Angle:=pa;
   end;
 end;
end;

procedure Tf_main.MenuResolveRotateClick(Sender: TObject);
begin
  ResolveRotate(Sender);
end;

procedure Tf_main.MenuRotatorRotateClick(Sender: TObject);
begin
  RotatorRotate(Sender);
end;

procedure Tf_main.MenuResolveSyncRotatorClick(Sender: TObject);
begin
 if fits.HeaderInfo.valid then begin
  if rotator.Status=devConnected then begin
     if fits.HeaderInfo.solved then begin
       fits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
       ResolveSyncRotator(self);
     end else begin
       if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
         fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
         astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@ResolveSyncRotator);
       end;
     end;
  end
  else
     NewMessage('Rotator is not connected');
 end;

end;

procedure Tf_main.MenuResolveSlewClick(Sender: TObject);
begin
  astrometry.SlewScreenXY(MouseDownX,MouseDownY);
end;

procedure Tf_main.MenuResolvePlanetariumClick(Sender: TObject);
begin
  if fits.HeaderInfo.valid then begin
   if planetarium.Connected then begin
      if fits.HeaderInfo.solved then begin
        NewMessage('Send image to planetarium');
        fits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
        if planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits') then
           NewMessage('Send image to planetarium')
        else
           NewMessage('Planetarium error.');
      end else begin
        if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
          fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
          astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryToPlanetarium);
        end;
      end;
   end
   else
      NewMessage('Planetarium is not connected');
  end;
end;

procedure Tf_main.MenuShowCCDFrameClick(Sender: TObject);
begin
  if fits.HeaderInfo.valid then begin
   if planetarium.Connected then begin
      if fits.HeaderInfo.solved then begin
        fits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
        AstrometryToPlanetariumFrame(Sender);
      end else begin
        if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
          fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
          astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryToPlanetariumFrame);
        end;
      end;
   end
   else
      NewMessage('Planetarium is not connected');
  end;
end;

procedure Tf_main.MenuScriptEditClick(Sender: TObject);
begin
  f_script.BtnEdit.Click;
end;

procedure Tf_main.MenuScriptNewClick(Sender: TObject);
begin
  f_script.BtnNew.Click;
end;

procedure Tf_main.MenuScriptRunClick(Sender: TObject);
begin
  f_script.BtnRun.Click;
end;

procedure Tf_main.MenuScriptStopClick(Sender: TObject);
begin
  f_script.BtnStop.Click;
end;

procedure Tf_main.MenuSequenceEditClick(Sender: TObject);
begin
  f_sequence.BtnEditTargets.Click;
end;

procedure Tf_main.MenuSequenceLoadClick(Sender: TObject);
begin
  f_sequence.BtnLoadTargets.Click;
end;

procedure Tf_main.MenuSequenceNewClick(Sender: TObject);
begin
  f_sequence.BtnNewTargets.Click;
end;

procedure Tf_main.MenuSequenceStartClick(Sender: TObject);
begin
  f_sequence.BtnStart.Click;
end;

procedure Tf_main.MenuSequenceStopClick(Sender: TObject);
begin
  f_sequence.BtnStop.Click;
end;

procedure Tf_main.ResolveSyncRotator(Sender: TObject);
var fn: string;
    rot: Double;
    n: integer;
    wcsinfo: TcdcWCSinfo;
begin
rot:=NullCoord;

fn:=slash(TmpDir)+'ccdcielsolved.fits';
n:=cdcwcs_initfitsfile(pchar(fn),0);
if n=0 then n:=cdcwcs_getinfo(addr(wcsinfo),0);

if (n=0) and (rotator.Status=devConnected) then begin

  rot:=wcsinfo.rot;

  rotator.Sync(rot);
  f_rotator.SetCalibrated(true);

end;
end;

procedure Tf_main.AstrometryToPlanetariumFrame(Sender: TObject);
var fn: string;
    ra, dec, rot, sizeH, sizeV: Double;
    n: integer;
    wcsinfo: TcdcWCSinfo;
begin
ra:=NullCoord; dec:=NullCoord; rot:=NullCoord; sizeH:=0; sizeV:=0;

fn:=slash(TmpDir)+'ccdcielsolved.fits';
n:=cdcwcs_initfitsfile(pchar(fn),0);
if n=0 then n:=cdcwcs_getinfo(addr(wcsinfo),0);

if (n=0) and planetarium.Connected then begin

  ra:=wcsinfo.cra;
  dec:=wcsinfo.cdec;
  rot:=wcsinfo.rot;
  sizeH:=wcsinfo.secpix*wcsinfo.wp/3600;
  sizeV:=wcsinfo.secpix*wcsinfo.hp/3600;


  if((ra=NullCoord) or (dec=NullCoord) or (sizeV=0) or (sizeH=0) or (rot=NullCoord)) then
  begin
    NewMessage('Unable to find resolve data!');
  end
  else
  begin
    // send cmd to planetarium
    ra:=deg2rad*ra;
    dec:=deg2rad*dec;
    J2000ToApparent(ra,dec);
    ra:=rad2deg*ra;
    dec:=rad2deg*dec;
    if planetarium.DrawFrame(ra,dec,sizeH,sizeV,rot) then
       NewMessage('CCD frame sent to planetarium.')
    else
       NewMessage('Planetarium error.');
  end;

 end;
end;

procedure Tf_main.AstrometryToPlanetarium(Sender: TObject);
begin
if astrometry.LastResult and planetarium.Connected then begin
  if planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits') then
     NewMessage('Send image to planetarium')
  else
     NewMessage('Planetarium error.');
end;
end;


Procedure Tf_main.PlanetariumConnectClick(Sender: TObject);
var i: integer;
begin
 if f_planetarium.BtnConnect.Caption='Connect' then begin
   f_planetarium.BtnConnect.Caption:='Disconnect';
   MenuPlanetariumConnect.Caption:=f_planetarium.BtnConnect.Caption;
   i:=ord(planetarium.PlanetariumType);
   case TPlanetariumType(i) of
     CDC:  planetarium.Connect(config.GetValue('/Planetarium/CdChostname','localhost'),
                      config.GetValue('/Planetarium/CdCport',''));
     SAMP: planetarium.Connect('');
     HNSKY: planetarium.Connect('');
   end;
 end else begin
   planetarium.Disconnect;
 end;
end;

Procedure Tf_main.PlanetariumConnect(Sender: TObject);
begin
 f_planetarium.BtnConnect.Caption:='Disconnect';
 MenuPlanetariumConnect.Caption:=f_planetarium.BtnConnect.Caption;
 f_planetarium.led.Brush.Color:=clLime;
 f_planetarium.Status.Text:='Connected '+PlanetariumName[ord(planetarium.PlanetariumType)];
 NewMessage('Planetarium: '+PlanetariumName[ord(planetarium.PlanetariumType)]+' connected');
 planetarium.InitTimer.Enabled:=true;
end;

Procedure Tf_main.PlanetariumDisconnect(Sender: TObject);
var i: integer;
begin
 if not AppClose then begin
   f_planetarium.led.Brush.Color:=clGray;
   f_planetarium.Status.Text:='Disconnected';
   f_planetarium.BtnConnect.Caption:='Connect';
   MenuPlanetariumConnect.Caption:=f_planetarium.BtnConnect.Caption;
   NewMessage('Planetarium: '+PlanetariumName[ord(planetarium.PlanetariumType)]+' disconnected');
   i:=config.GetValue('/Planetarium/Software',0);
   case TPlanetariumType(i) of
     CDC: planetarium:=TPlanetarium_cdc.Create;
     SAMP:planetarium:=TPlanetarium_samp.Create;
     HNSKY:planetarium:=TPlanetarium_hnsky.Create;
   end;
   planetarium.onConnect:=@PlanetariumConnect;
   planetarium.onDisconnect:=@PlanetariumDisconnect;
   planetarium.onShowMessage:=@NewMessage;
   f_planetariuminfo.planetarium:=planetarium;
   f_scriptengine.Planetarium:=planetarium;
   f_sequence.Planetarium:=planetarium;
 end;
end;

Procedure Tf_main.PlanetariumNewTarget(Sender: TObject);
var ra,de,err:double;
    tra,tde,objn: string;
begin
 if planetarium.Connected and (Mount.Status=devConnected)and(Camera.Status=devConnected) then begin
    f_planetariuminfo.Ra.Text  := '-';
    f_planetariuminfo.De.Text  := '-';
    f_planetariuminfo.Obj.Text := '';
    FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
    f_planetariuminfo.ShowModal;
    if f_planetariuminfo.ModalResult=mrOK then begin
      tra:= f_planetariuminfo.Ra.Text;
      tde:=f_planetariuminfo.De.Text;
      objn:=trim(f_planetariuminfo.Obj.Text);
      NewMessage('Move to new planetarium object '+objn);
       if tra='-' then
         ra:=NullCoord
       else
         ra:=StrToAR(tra);
       if tde='-' then
         de:=NullCoord
       else
         de:=StrToDE(tde);
      if (ra<>NullCoord) and (de<>NullCoord) then begin
        if MessageDlg('Please confirm you want to slew the telescope to '+objn+' at coordinates '+tra+'/'+tde,mtConfirmation,mbOKCancel,0)=mrOK then begin
          if astrometry.PrecisionSlew(ra,de,err) then begin
            f_capture.Fname.Text:=objn;
            NewMessage('Planetarium target set to '+objn);
          end
          else NewMessage('Planetarium target slew fail');
        end;
      end
      else NewMessage('Invalid coordinates');
    end;
 end else begin
   NewMessage('Before to use this tool you must connect the camera, the mount and the planetarium');
 end;
end;

procedure Tf_main.SaveFitsFile(fn:string);
begin
  if fits.HeaderInfo.valid then
     fits.SaveToFile(fn);
end;

procedure Tf_main.LoadFitsFile(fn:string);
var mem: TMemoryStream;
    imgsize: string;
    n:integer;
    c: TcdcWCScoord;
    x1,y1,x2,y2: double;
begin
   StatusBar1.Panels[1].Text:='';
   mem:=TMemoryStream.Create;
   mem.LoadFromFile(fn);
   fits.SetBPM(bpm,0,0,0,0);
   fits.Stream:=mem;
   fits.LoadStream;
   mem.free;
   if fits.HeaderInfo.valid then begin
     if fits.HeaderInfo.solved then begin
       n:=cdcwcs_initfitsfile(pchar(fn),0);
       if n=0 then n:=cdcwcs_getinfo(addr(cdcWCSinfo),0);
       if (n=0) and (abs(cdcWCSinfo.cdec)<89.0) then begin
         c.ra:=cdcWCSinfo.cra;
         c.dec:=cdcWCSinfo.cdec;
         n:=cdcwcs_sky2xy(@c,0);
         x1:=c.x;
         y1:=c.y;
         c.ra:=cdcWCSinfo.cra;
         c.dec:=cdcWCSinfo.cdec+0.01;
         n:=cdcwcs_sky2xy(@c,0);
         x2:=c.x;
         y2:=c.y;
         WCSxyrot := arctan2((x2 - x1), (y1 - y2));
       end
       else cdcWCSinfo.secpix:=0;
     end
       else cdcWCSinfo.secpix:=0;
     DrawHistogram(true);
     DrawImage;
     imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
     NewMessage('Open file '+fn);
     StatusBar1.Panels[2].Text:='Open file '+fn+' '+imgsize;
   end
   else begin
    NewMessage('Invalid or unsupported FITS file '+fn);
   end;
end;

procedure Tf_main.OpenRefImage(fn:string);
begin
  reffile:=fn;
  refmask:=true;
  SetRefImage;
end;

procedure Tf_main.ClearRefImage(Sender: TObject);
begin
   refmask:=false;
   reffile:='';
   refbmp.SetSize(0,0);
   DrawImage;
end;

procedure Tf_main.CCDCIELMessageHandler(var Message: TLMessage);
begin
  if AppClose then exit;
  case Message.wParam of
    M_AutoguiderStatusChange: AutoguiderStatus(nil);
    M_AutoguiderMessage: if autoguider.ErrorDesc<>'' then begin
                          NewMessage(autoguider.ErrorDesc);
                          autoguider.ErrorDesc:='';
                         end;
    M_AstrometryDone: astrometry.AstrometryDone;
    else
      NewMessage('Receive unknow message: '+inttostr(Message.wParam));
  end;
end;

procedure Tf_main.StatusTimerTimer(Sender: TObject);
begin
 // Periodic check
 StatusTimer.Enabled:=false;
 CheckMeridianFlip;
 StatusTimer.Enabled:=true;
end;

procedure Tf_main.TimerStampTimerTimer(Sender: TObject);
begin
   Timestamp.Caption:=TimeToStr(now);
end;

function Tf_main.CheckMeridianFlip(nextexposure:double=0):integer;
var ra,de,hh,a,h,tra,tde,err: double;
    jd0,CurSt,CurTime: double;
    Year, Month, Day: Word;
    MeridianDelay1,MeridianDelay2,NextDelay,hhmin,waittimeout: integer;
    slewtopos,slewtoimg, restartguider, SaveCapture, ok: boolean;
  procedure DoAbort;
  begin
    NewMessage('Meridian abort!');
    mount.AbortMotion;
    if f_capture.Running then CameraExposureAborted(nil);
    if autoguider.Running and (autoguider.State=GUIDER_GUIDING) then autoguider.Guide(false);
  end;
begin
  result:=-1;
  if (mount.Status=devConnected) and (not mount.MountSlewing) and (not autofocusing) and ((not meridianflipping)or(nextexposure<>0)) then begin
    DecodeDate(now, Year, Month, Day);
    CurTime:=frac(now)*24;
    jd0:=jd(Year,Month,Day,0);
    CurST:=Sidtim(jd0,CurTime-ObsTimeZone,ObsLongitude);
    ra:=mount.RA;
    de:=mount.Dec;
    ra:=deg2rad*15*ra;
    hh:=CurSt-ra;
    Eq2Hz(hh,deg2rad*de,a,h) ;
    a:=rad2deg*rmod(a+pi,pi2);
    h:=rad2deg*h;
    if hh>pi then hh:=hh-pi2;
    if hh<-pi then hh:=hh+pi2;
    hhmin:=round(rad2deg*60*hh/15);
    f_mount.TimeToMeridian.Caption:=inttostr(abs(hhmin));
    if hhmin<=0 then f_mount.LabelMeridian.Caption:='Meridian in'
                else f_mount.LabelMeridian.Caption:='Meridian since';
    if (f_capture.Running  or f_sequence.Busy) and (nextexposure=0) then exit;
    if MeridianOption=0 then exit; // fork mount
    if mount.PierSide=pierEast then exit; // already on the right side
    MeridianDelay1:=MinutesPastMeridianMin-hhmin;
    if mount.PierSide=pierUnknown
      then MeridianDelay2:=MeridianDelay1
      else MeridianDelay2:=MinutesPastMeridian-hhmin;
    NextDelay:=ceil(nextexposure/60);
    if MeridianDelay1>0  then begin // before meridian limit
      if MeridianDelay2>NextDelay then begin // enough time for next exposure or no capture in progress
         result:=-1;
         exit;
      end else if (NextDelay>0)and(MeridianDelay1>0) then begin // too short for next exposure
        if (MeridianOption=1) then begin   // if autoflip, wait
           wait(2);
           meridianflipping:=true;
           NewMessage('Wait meridian flip for '+inttostr(MeridianDelay1)+' minutes');
           StatusBar1.Panels[1].Text := 'Wait meridian flip';
           result:=15+abs(round(rad2deg*3600*hh/15)); // time to wait for meridian
           exit;
        end else begin
         result:=-1;   // if abort, continue
         exit;
        end;
      end;
    end;
    if ((MeridianDelay1<=0)and(mount.PierSide=pierWest)) or
       (MeridianDelay1=0)
      then begin                    // Do meridian action
      if MeridianOption=1 then begin  // Flip
        meridianflipping:=true;
        if mount.PierSide=pierUnknown then begin
          NewMessage('Mount is not reporting pier side, meridian flip can be unreliable.');
        end;
        slewtopos:=false; slewtoimg:=false;
        // get current position from target object
        if (f_sequence.Running) and (f_sequence.TargetCoord) then begin
          if (f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord) then begin
            tra:=f_sequence.TargetRA;
            tde:=f_sequence.TargetDE;
            slewtopos:=true;
            slewtoimg:=false;
          end;
        end;
        // get current position from last capture image
        if (not slewtopos) and (f_capture.Running) and fits.HeaderInfo.valid and (astrometryResolver<>ResolverNone) then begin
          DeleteFileUTF8(slash(TmpDir)+'meridianflip.fits');
          fits.SaveToFile(slash(TmpDir)+'meridianflip.fits');
          slewtopos:=false;
          slewtoimg:=true;
        end;
        // stop autoguider
        restartguider:=(autoguider.State=GUIDER_GUIDING);
        if restartguider then begin
          autoguider.Guide(false);
          autoguider.WaitBusy(15);
        end;
        wait(2);
        // Pause before
        if MeridianFlipPauseBefore then begin
          waittimeout:=60*MeridianDelay2;
          if waittimeout<=0 then waittimeout:=30;
          f_pause.Caption:='Pause';
          f_pause.Text:='Meridian flip will occur now.'+crlf+'Click Continue when ready';
          if not f_pause.Wait(waittimeout) then begin
            meridianflipping:=false;
            NewMessage('Meridian flip canceled before flip');
            DoAbort;
            exit;
          end;
        end;
        // flip
        NewMessage('Meridian flip now');
        StatusBar1.Panels[1].Text := 'Meridian flip';
        mount.FlipMeridian;
        wait(2);
        if mount.PierSide=pierWest then begin
          f_pause.Caption:='Pause';
          f_pause.Text:='Meridian flip error!';
          NewMessage(f_pause.Text);
          if not f_pause.Wait(120) then begin
             DoAbort;
             exit;
          end;
        end;
        if mount.PierSide=pierUnknown then begin
          NewMessage('Wait 1 minute ...');
          wait(60); // ensure we not do the flip two time
        end;
        NewMessage('Meridian flip done');
        // Pause after
        if MeridianFlipPauseAfter then begin
          SaveCapture:=Capture;
          try
          Capture:=false; // allow preview and refocusing
          waittimeout:=60*MeridianFlipPauseTimeout;
          f_pause.Caption:='Pause';
          f_pause.Text:='Meridian flip done.'+crlf+'Click Continue when ready.';
          ok:=f_pause.Wait(waittimeout);
          finally
            Capture:=SaveCapture;
          end;
          if not ok then begin
            NewMessage('Meridian flip canceled after flip');
            DoAbort;
            exit;
          end;
        end;
        // autofocus
        if MeridianFlipAutofocus then begin
          AutoAutofocus(false);
        end;
        // precision slew with saved coordinates
        if slewtopos then begin
          NewMessage('Recenter on last position');
          try
          Capture:=false;  // do not save the control images
          astrometry.PrecisionSlew(rad2deg*tra/15,rad2deg*tde,err);
          wait(2);
          finally
            Capture:=true;
          end;
          if (astrometry.LastResult)and(astrometry.LastSlewErr>config.GetValue('/PrecSlew/Precision',5.0)/60) then begin
            f_pause.Caption:='Pause';
            f_pause.Text:='Recenter image error!'+crlf+'distance: '+FormatFloat(f1,astrometry.LastSlewErr);
            NewMessage(f_pause.Text);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        // precision slew with saved image
        if slewtoimg  then begin
          NewMessage('Recenter on last image');
          try
          Capture:=false;  // do not save the control images
          LoadFitsFile(slash(TmpDir)+'meridianflip.fits');
          DeleteFileUTF8(slash(TmpDir)+'meridianflip.fits');
          ResolveSlewCenter(Self);
          wait(2);
          finally
            Capture:=true;
          end;
          if (astrometry.LastResult)and(astrometry.LastSlewErr>config.GetValue('/PrecSlew/Precision',5.0)/60) then begin
            f_pause.Caption:='Pause';
            f_pause.Text:='Recenter image error!'+crlf+'distance: '+FormatFloat(f1,astrometry.LastSlewErr);
            NewMessage(f_pause.Text);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        // start autoguider
        if restartguider then begin
          NewMessage('Restart autoguider');
          autoguider.Guide(false);
          wait(5);
          if MeridianFlipCalibrate then
            autoguider.Calibrate
          else
            autoguider.Guide(true);
          autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
          if autoguider.State<>GUIDER_GUIDING then begin
            f_pause.Caption:='Pause';
            f_pause.Text:='Failed to start guiding!';
            NewMessage(f_pause.Text);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        Wait(2);
        f_capture.DitherNum:=0; // no dither after flip
        meridianflipping:=false;
        NewMessage('Meridian flip finished');
        StatusBar1.Panels[1].Text := '';
      end else begin  // Abort
        DoAbort;
      end;
    end;
  end;
end;

end.


