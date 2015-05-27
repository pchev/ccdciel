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
  fu_sequence, fu_planetarium, u_ccdconfig, pu_editplan,
  pu_devicesetup, pu_options, pu_valueseditor, pu_indigui, cu_fits, cu_camera,
  pu_viewtext, cu_wheel, cu_mount, cu_focuser, XMLConf, u_utils, u_global,
  cu_indimount, cu_ascommount, cu_indifocuser, cu_ascomfocuser,
  cu_indiwheel, cu_ascomwheel, cu_indicamera, cu_ascomcamera, cu_astrometry,
  cu_autoguider, cu_autoguider_phd, cu_planetarium, cu_planetarium_cdc, cu_planetarium_samp,
  pu_planetariuminfo,
  lazutf8sysutils, Classes, dynlibs,
  SysUtils, FileUtil, Forms, Controls, Math, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls;

type

  { Tf_main }

  Tf_main = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuIndiSettings: TMenuItem;
    MenuHelpAbout: TMenuItem;
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
    OpenDialog1: TOpenDialog;
    PageControlRight: TPageControl;
    Panel1: TPanel;
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
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure ConnectTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1Paint(Sender: TObject);
    procedure Image1Resize(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuIndiSettingsClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuOptionsClick(Sender: TObject);
    procedure MenuResetToolsClick(Sender: TObject);
    procedure MenuResolveSlewClick(Sender: TObject);
    procedure MenuResolveSyncClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuResolvePlanetariumClick(Sender: TObject);
    procedure MenuStopAstrometryClick(Sender: TObject);
    procedure MenuViewAstrometryLogClick(Sender: TObject);
    procedure MenuViewAutoguiderClick(Sender: TObject);
    procedure MenuViewCCDtempClick(Sender: TObject);
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
    procedure MenuViewSequenceClick(Sender: TObject);
    procedure MenuViewStarProfileClick(Sender: TObject);
    procedure PanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PanelDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StatusbarTimerTimer(Sender: TObject);
  private
    { private declarations }
    camera: T_camera;
    wheel: T_wheel;
    focuser: T_focuser;
    mount: T_mount;
    autoguider:T_autoguider;
    planetarium:TPlanetarium;
    astrometry:TAstrometry;
    CameraName,WheelName,FocuserName,MountName: string;
    WantCamera,WantWheel,WantFocuser,WantMount: boolean;
    f_devicesconnection: Tf_devicesconnection;
    f_filterwheel: Tf_filterwheel;
    f_ccdtemp: Tf_ccdtemp;
    f_frame: Tf_frame;
    f_preview: Tf_preview;
    f_capture: Tf_capture;
    f_sequence: Tf_sequence;
    f_starprofile: Tf_starprofile;
    f_focuser: Tf_focuser;
    f_mount: Tf_mount;
    f_autoguider: Tf_autoguider;
    f_planetarium: Tf_planetarium;
    f_visu: Tf_visu;
    f_msg: Tf_msg;
    fits: TFits;
    ImaBmp: TBitmap;
    SaveFocusZoom: double;
    ImgCx, ImgCy, Mx, My,Starwindow,Focuswindow: integer;
    StartX, StartY, EndX, EndY, MouseDownX,MouseDownY: integer;
    FrameX,FrameY,FrameW,FrameH: integer;
    MouseMoving, MouseFrame, LockMouse: boolean;
    Capture,Preview,PreviewLoop: boolean;
    LogToFile,LogFileOpen: Boolean;
    NeedRestart, GUIready, AppClose: boolean;
    LogFile: UTF8String;
    MsgLog: Textfile;
    Procedure InitLog;
    Procedure CloseLog;
    Procedure WriteLog( buf : string);
    procedure SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; amenu: TMenuItem);
    procedure UpdConfig(oldver:string);
    procedure SetConfig;
    procedure SetOptions;
    procedure OptionGetPixelSize(Sender: TObject);
    procedure OptionGetFocaleLength(Sender: TObject);
    procedure Restart;
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
    procedure SetFocusMode;
    Procedure ConnectWheel(Sender: TObject);
    Procedure DisconnectWheel(Sender: TObject);
    Procedure ConnectFocuser(Sender: TObject);
    Procedure DisconnectFocuser(Sender: TObject);
    Procedure ConnectMount(Sender: TObject);
    Procedure DisconnectMount(Sender: TObject);
    Procedure SetFilter(Sender: TObject);
    Procedure NewMessage(msg: string);
    Procedure CameraStatus(Sender: TObject);
    Procedure CameraDisconnected(Sender: TObject);
    Procedure CameraExposureAborted(Sender: TObject);
    procedure CameraProgress(n:double);
    procedure CameraTemperatureChange(t:double);
    Procedure WheelStatus(Sender: TObject);
    procedure FilterChange(n:double);
    procedure FilterNameChange(Sender: TObject);
    Procedure FocusStart(Sender: TObject);
    Procedure FocusStop(Sender: TObject);
    Procedure FocuserStatus(Sender: TObject);
    procedure FocuserPositionChange(n:double);
    procedure FocuserSpeedChange(n:double);
    procedure FocuserTimerChange(n:double);
    procedure FocusIN(Sender: TObject);
    procedure FocusOUT(Sender: TObject);
    Procedure MountStatus(Sender: TObject);
    Procedure MountCoordChange(Sender: TObject);
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
    procedure CameraNewImage(Sender: TObject);
    Procedure AbortExposure(Sender: TObject);
    Procedure StartPreviewExposure(Sender: TObject);
    Procedure StartCaptureExposure(Sender: TObject);
    Procedure RedrawHistogram(Sender: TObject);
    Procedure Redraw(Sender: TObject);
    Procedure ImgFullRange(Sender: TObject);
    Procedure ZoomImage(Sender: TObject);
    Procedure ClearImage;
    Procedure DrawImage;
    Procedure PlotImage;
    Procedure DrawHistogram;
    procedure AstrometryStart(Sender: TObject);
    procedure AstrometryEnd(Sender: TObject);
    procedure AstrometryToPlanetarium(Sender: TObject);
  public
    { public declarations }
  end;

var
  f_main: Tf_main;

implementation

{$R *.lfm}

{ Tf_main }

Procedure Tf_main.InitLog;
begin
  try
     LogFile:=slash(LogDir)+'Log_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.log';
     Filemode:=2;
     AssignFile(MsgLog,LogFile);
     Rewrite(MsgLog);
     WriteLn(MsgLog,FormatDateTime(dateiso,Now)+'  Start new log');
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

Procedure Tf_main.CloseLog;
begin
  try
    if LogFileOpen then begin
      LogFileOpen:=false;
      CloseFile(MsgLog);
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

procedure Tf_main.Restart;
begin
  ShowMessage('The program will restart now...');
  NeedRestart:=true;
  Close;
end;

procedure Tf_main.SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; amenu: TMenuItem);
var pn: string;
    i: integer;
    par: Tpanel;
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
amenu.Checked:=tool.Visible;
end;

procedure Tf_main.FormCreate(Sender: TObject);
var DefaultInterface,aInt: TDevInterface;
    configver,configfile: string;
    i:integer;
begin
  {$ifdef mswindows}
  DefaultInterface:=ASCOM;
  {$else}
  DefaultInterface:=INDI;
  {$endif}
  AppClose:=false;
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.TimeSeparator:=':';
  NeedRestart:=false;
  GUIready:=false;
  Filters:=TStringList.Create;
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
  ConfigExtension:= '.conf';
  config:=TCCDConfig.Create(self);
  ConfigDir:=GetAppConfigDirUTF8(false,true);
  if Application.HasOption('c', 'config') then begin
    configfile:='ccdciel_'+Application.GetOptionValue('c', 'config')+'.conf';
  end
  else configfile:='ccdciel.conf';
  config.Filename:=slash(ConfigDir)+configfile;
  LogFile:=slash(ConfigDir)+'Log_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.log';
  LogFileOpen:=false;
  TmpDir:=slash(ConfigDir)+'tmp';
  if not DirectoryExistsUTF8(TmpDir) then  CreateDirUTF8(TmpDir);
  LogDir:=slash(ConfigDir)+'Log';
  if not DirectoryExistsUTF8(LogDir) then  CreateDirUTF8(LogDir);

  configver:=config.GetValue('/Configuration/Version','');

  UpdConfig(configver);

  Top:=config.GetValue('/Window/Top',0);
  Left:=config.GetValue('/Window/Left',0);
  Width:=config.GetValue('/Window/Width',1024);
  Height:=config.GetValue('/Window/Height',768);

  aInt:=TDevInterface(config.GetValue('/FilterWheelInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  wheel:=T_indiwheel.Create;
    ASCOM: wheel:=T_ascomwheel.Create;
  end;
  wheel.onMsg:=@NewMessage;
  wheel.onFilterChange:=@FilterChange;
  wheel.onFilterNameChange:=@FilterNameChange;
  wheel.onStatusChange:=@WheelStatus;

  aInt:=TDevInterface(config.GetValue('/FocuserInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  focuser:=T_indifocuser.Create;
    ASCOM: focuser:=T_ascomfocuser.Create;
  end;
  focuser.onMsg:=@NewMessage;
  focuser.onPositionChange:=@FocuserPositionChange;
  focuser.onSpeedChange:=@FocuserSpeedChange;
  focuser.onTimerChange:=@FocuserTimerChange;
  focuser.onStatusChange:=@FocuserStatus;

  aInt:=TDevInterface(config.GetValue('/MountInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  mount:=T_indimount.Create;
    ASCOM: mount:=T_ascommount.Create;
  end;
  mount.onMsg:=@NewMessage;
  mount.onCoordChange:=@MountCoordChange;
  mount.onStatusChange:=@MountStatus;

  fits:=TFits.Create(self);

  aInt:=TDevInterface(config.GetValue('/CameraInterface',ord(DefaultInterface)));
  case aInt of
    INDI:  camera:=T_indicamera.Create;
    ASCOM: camera:=T_ascomcamera.Create;
  end;
  camera.Mount:=mount;
  camera.wheel:=wheel;
  camera.Fits:=fits;
  camera.onMsg:=@NewMessage;
  camera.onExposureProgress:=@CameraProgress;
  camera.onFrameChange:=@FrameChange;
  camera.onTemperatureChange:=@CameraTemperatureChange;
  camera.onNewImage:=@CameraNewImage;
  camera.onStatusChange:=@CameraStatus;
  camera.onCameraDisconnected:=@CameraDisconnected;
  camera.onAbortExposure:=@CameraExposureAborted;

  astrometry:=TAstrometry.Create;
  astrometry.Camera:=camera;
  astrometry.Mount:=mount;
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
  end;
  planetarium.onConnect:=@PlanetariumConnect;
  planetarium.onDisconnect:=@PlanetariumDisconnect;
  planetarium.onShowMessage:=@NewMessage;

  f_devicesconnection:=Tf_devicesconnection.Create(self);
  f_devicesconnection.onConnect:=@Connect;

  f_visu:=Tf_visu.Create(self);
  f_visu.onRedraw:=@Redraw;
  f_visu.onZoom:=@ZoomImage;
  f_visu.onRedrawHistogram:=@RedrawHistogram;
  f_visu.onFullRange:=@ImgFullRange;

  f_msg:=Tf_msg.Create(self);

  f_frame:=Tf_frame.Create(self);
  f_frame.onSet:=@SetFrame;
  f_frame.onReset:=@ResetFrame;

  f_preview:=Tf_preview.Create(self);
  f_preview.onStartExposure:=@StartPreviewExposure;
  f_preview.onAbortExposure:=@AbortExposure;
  f_preview.onMsg:=@NewMessage;

  f_capture:=Tf_capture.Create(self);
  f_capture.onStartExposure:=@StartCaptureExposure;
  f_capture.onAbortExposure:=@AbortExposure;
  f_capture.onMsg:=@NewMessage;

  f_filterwheel:=Tf_filterwheel.Create(self);
  f_filterwheel.onSetFilter:=@SetFilter;

  f_focuser:=Tf_focuser.Create(self);
  f_focuser.onFocusIN:=@FocusIN;
  f_focuser.onFocusOUT:=@FocusOUT;

  f_starprofile:=Tf_starprofile.Create(self);
  f_starprofile.onFocusStart:=@FocusStart;
  f_starprofile.onFocusStop:=@FocusStop;

  f_ccdtemp:=Tf_ccdtemp.Create(self);
  f_ccdtemp.onSetTemperature:=@SetTemperature;

  f_mount:=Tf_mount.Create(self);

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
  f_sequence.Autoguider:=autoguider;
  f_sequence.Astrometry:=astrometry;

  f_planetarium:=Tf_planetarium.Create(self);
  f_planetarium.onConnect:=@PlanetariumConnectClick;
  f_planetarium.Status.Text:='Disconnected';

  SetConfig;
  SetOptions;

  f_ccdtemp.Setpoint.Text:=config.GetValue('/Temperature/Setpoint','0');
  f_preview.ExpTime.Text:=config.GetValue('/Preview/Exposure','1');
  f_capture.ExpTime.Text:=config.GetValue('/Capture/Exposure','1');
  f_capture.Fname.Text:=config.GetValue('/Capture/FileName','');
  f_capture.SeqNum.Text:=config.GetValue('/Capture/Count','1');

  ImaBmp:=TBitmap.Create;
  LockMouse:=false;
  ImgCx:=0;
  ImgCy:=0;
  StartX:=0;
  StartY:=0;
  EndX:=0;
  EndY:=0;
  Capture:=false;
  Preview:=false;
  PreviewLoop:=false;
  MenuIndiSettings.Enabled:=(camera.CameraInterface=INDI);

  NewMessage('Initialized');
end;

procedure Tf_main.FormShow(Sender: TObject);
var str: string;
    i,n: integer;
begin
  if (cdcwcs_initfitsfile=nil)or(cdcwcs_release=nil)or(cdcwcs_sky2xy=nil)or(cdcwcs_xy2sky=nil)or(cdcwcs_getinfo=nil) then begin
     NewMessage('Could not load libcdcwcs'+crlf+'Some astrometry function are not available.');
  end;

  SetTool(f_visu,'Histogram',PanelBottom,0,MenuViewHistogram);
  SetTool(f_msg,'Messages',PanelBottom,f_visu.left+1,MenuViewMessages);

  SetTool(f_devicesconnection,'Connection',PanelRight1,0,MenuViewConnection);
  SetTool(f_preview,'Preview',PanelRight1,f_devicesconnection.top+1,MenuViewPreview);
  SetTool(f_mount,'Mount',PanelRight1,f_preview.top+1,MenuViewMount);
  SetTool(f_autoguider,'Autoguider',PanelRight1,f_mount.top+1,MenuViewAutoguider);
  SetTool(f_planetarium,'Planetarium',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium);

  SetTool(f_focuser,'Focuser',PanelRight2,0,MenuViewFocuser);
  SetTool(f_starprofile,'Starprofile',PanelRight2,f_focuser.top+1,MenuViewStarProfile);

  SetTool(f_capture,'Capture',PanelRight3,0,MenuViewCapture);
  SetTool(f_filterwheel,'Filters',PanelRight3,f_capture.top+1,MenuViewFilters);
  SetTool(f_frame,'Frame',PanelRight3,f_filterwheel.top+1,MenuViewFrame);
  SetTool(f_ccdtemp,'CCDTemp',PanelRight3,f_frame.top+1,MenuViewCCDtemp);

  SetTool(f_sequence,'Sequence',PanelRight4,0,MenuViewSequence);

  StatusBar1.Visible:=false; // bug with statusbar visibility
  StatusbarTimer.Enabled:=true;

  n:=config.GetValue('/Filters/Num',0);
  Filters.Clear;
  Filters.Add(Filter0);
  for i:=1 to n do begin
     str:=config.GetValue('/Filters/Filter'+IntToStr(i),'');
     Filters.Add(str);
  end;
  f_filterwheel.Filters.Items.Assign(Filters);
  f_filterwheel.Filters.ItemIndex:=0;
  f_EditPlan.Filter.Items.Assign(Filters);

  str:=config.GetValue('/Sequence/Targets','');
  if str<>'' then f_sequence.LoadTargets(str);

   f_planetariuminfo.planetarium:=planetarium;

end;

procedure Tf_main.StatusbarTimerTimer(Sender: TObject);
begin
 StatusbarTimer.Enabled:=false;
 StatusBar1.Visible:=true;  // bug with statusbar visibility
end;

procedure Tf_main.MenuResetToolsClick(Sender: TObject);
begin
  SetTool(f_visu,'',PanelBottom,0,MenuViewHistogram);
  SetTool(f_msg,'',PanelBottom,f_visu.left+1,MenuViewMessages);

  SetTool(f_devicesconnection,'',PanelRight1,0,MenuViewConnection);
  SetTool(f_preview,'',PanelRight1,f_devicesconnection.top+1,MenuViewPreview);
  SetTool(f_mount,'',PanelRight1,f_preview.top+1,MenuViewMount);
  SetTool(f_autoguider,'',PanelRight1,f_mount.top+1,MenuViewAutoguider);
  SetTool(f_planetarium,'',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium);

  SetTool(f_focuser,'',PanelRight2,0,MenuViewFocuser);
  SetTool(f_starprofile,'',PanelRight2,f_focuser.top+1,MenuViewStarProfile);

  SetTool(f_capture,'',PanelRight3,0,MenuViewCapture);
  SetTool(f_filterwheel,'',PanelRight3,f_capture.top+1,MenuViewFilters);
  SetTool(f_frame,'',PanelRight3,f_filterwheel.top+1,MenuViewFrame);
  SetTool(f_ccdtemp,'',PanelRight3,f_frame.top+1,MenuViewCCDtemp);

  SetTool(f_sequence,'',PanelRight4,0,MenuViewSequence);
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

  config.SetValue('/Window/Top',Top);
  config.SetValue('/Window/Left',Left);
  config.SetValue('/Window/Width',Width);
  config.SetValue('/Window/Height',Height);

  config.SetValue('/Temperature/Setpoint',f_ccdtemp.Setpoint.Text);
  config.SetValue('/Preview/Exposure',f_preview.ExpTime.Text);
  config.SetValue('/Capture/Exposure',f_capture.ExpTime.Text);
  config.SetValue('/Capture/FileName',f_capture.Fname.Text);
  config.SetValue('/Capture/Count',f_capture.SeqNum.Text);

  config.SetValue('/Tools/Sequence/Parent',f_sequence.Parent.Name);
  config.SetValue('/Tools/Sequence/Visible',f_sequence.Visible);
  config.SetValue('/Tools/Sequence/Top',f_sequence.Top);
  config.SetValue('/Tools/Sequence/Left',f_sequence.Left);

  config.SetValue('/Sequence/Targets',f_sequence.CurrentFile);

  n:=Filters.Count-1;
  config.SetValue('/Filters/Num',n);
  for i:=1 to n do
     config.SetValue('/Filters/Filter'+IntToStr(i),Filters[i]);

  config.Flush;
  NewMessage('Program exit');
  CloseLog;
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
  sleep(1000); // time for other thread to terminate
  astrometry.Free;
  CloseAction:=caFree;
end;

procedure Tf_main.FormDestroy(Sender: TObject);
begin
  camera.Free;
  wheel.Free;
  focuser.Free;
  mount.Free;
  ImaBmp.Free;
  config.Free;
  Filters.Free;
  if NeedRestart then ExecNoWait(paramstr(0));
end;

procedure Tf_main.Image1DblClick(Sender: TObject);
var x,y: integer;
begin
 Screen2fits(Mx,My,x,y);
 f_starprofile.showprofile(fits.image,fits.imageC,fits.imageMin,x,y,Starwindow,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,mount.FocaleLength,camera.PixelSize);
 Image1.Invalidate;
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
var xx,yy: integer;
    val:integer;
begin
if LockMouse then exit;
 if MouseMoving then begin
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
 else if (fits.HeaderInfo.naxis1>0) then begin
    Screen2fits(x,y,xx,yy);
    if (xx>0)and(xx<fits.HeaderInfo.naxis1)and(yy>0)and(yy<fits.HeaderInfo.naxis2) then
       val:=trunc(fits.imageMin+fits.image[0,yy,xx]/fits.imageC)
    else val:=0;
    StatusBar1.Panels[0].Text:=inttostr(xx)+'/'+inttostr(yy)+': '+inttostr(val);
end;
Mx:=X;
My:=Y;
end;

procedure Tf_main.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xx,x1,y1,x2,y2,w,h: integer;
begin
if MouseMoving then begin
  ImgCx:=ImgCx+X-Mx;
  ImgCy:=ImgCy+Y-My;
  PlotImage;
  Mx:=X;
  My:=Y;
end;
if MouseFrame then begin
  EndX:=X;
  EndY:=Y;
  Screen2CCD(StartX,StartY,x1,y1);
  Screen2CCD(EndX,EndY,x2,y2);
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
if camera.Status<>devDisconnected then begin
   CanClose:=(MessageDlg('The camera is connected. Do you want to exit the program now?',mtConfirmation,mbYesNo,0)=mrYes);
end else begin
   CanClose:=true;
end;
if CanClose then begin
 AbortExposure(nil);
end;
end;

procedure Tf_main.Image1Resize(Sender: TObject);
begin
  image1.Picture.Bitmap.SetSize(image1.Width,image1.Height);
  ClearImage;
  DrawImage;
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
case mount.MountInterface of
   INDI : MountName:=config.GetValue('/INDImount/Device','');
   ASCOM: MountName:=config.GetValue('/ASCOMmount/Device','');
end;
end;

procedure Tf_main.SetOptions;
begin
  Starwindow:=config.GetValue('/StarAnalysis/Window',20);
  Focuswindow:=config.GetValue('/StarAnalysis/Focus',200);
  LogToFile:=config.GetValue('/Log/Messages',true);
  if LogToFile<>LogFileOpen then CloseLog;
  DitherPixel:=config.GetValue('/Autoguider/Dither/Pixel',1.0);
  DitherRAonly:=config.GetValue('/Autoguider/Dither/RAonly',true);
  SettlePixel:=config.GetValue('/Autoguider/Settle/Pixel',1.0);
  SettleMinTime:=config.GetValue('/Autoguider/Settle/MinTime',5);
  SettleMaxTime:=config.GetValue('/Autoguider/Settle/MaxTime',30);
  CalibrationDelay:=config.GetValue('/Autoguider/Settle/CalibrationDelay',300);
  if (autoguider<>nil)and(autoguider.State<>GUIDER_DISCONNECTED) then autoguider.SettleTolerance(SettlePixel,SettleMinTime, SettleMaxTime);
end;

Procedure Tf_main.Connect(Sender: TObject);
begin
if f_devicesconnection.BtnConnect.Caption='Disconnect' then begin
  Disconnect(Sender);
end else begin
  WantCamera:=true;
  WantWheel:=config.GetValue('/Devices/FilterWheel',false);
  WantFocuser:=config.GetValue('/Devices/Focuser',false);;
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
  if WantMount and (MountName='') then begin
    ShowMessage('Please configure your mount!');
    MenuSetup.Click;
    exit;
  end;

  f_devicesconnection.LabelCamera.Visible:=WantCamera;
  f_devicesconnection.LabelWheel.Visible:=WantWheel;
  f_devicesconnection.LabelFocuser.Visible:=WantFocuser;
  f_devicesconnection.LabelMount.Visible:=WantMount;
  f_devicesconnection.PanelDev.Visible:=true;

  if WantCamera  then ConnectCamera(Sender);
  if WantWheel   then ConnectWheel(Sender);
  if WantFocuser then ConnectFocuser(Sender);
  if WantMount   then ConnectMount(Sender);
end;
end;

Procedure Tf_main.Disconnect(Sender: TObject);
begin
if camera.Status<>devDisconnected then begin
   if (sender=nil) or (MessageDlg('Are you sure you want to disconnect all the devices now?',mtConfirmation,mbYesNo,0)=mrYes) then begin
     camera.AbortExposure;
     f_preview.stop;
     f_capture.stop;
     Capture:=false;
     StatusBar1.Panels[1].Text:='';
     DisconnectCamera(Sender);
     DisconnectWheel(Sender);
     DisconnectFocuser(Sender);
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
end;
procedure SetConnected;
begin
f_devicesconnection.led.Brush.Color:=clLime;
f_devicesconnection.BtnConnect.Caption:='Disconnect';
end;
procedure SetConnecting;
begin
f_devicesconnection.led.Brush.Color:=clYellow;
f_devicesconnection.BtnConnect.Caption:='Disconnect';
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
 else if concount>0 then SetConnecting
 else SetDisconnected;
end;

Procedure Tf_main.ConnectCamera(Sender: TObject);
begin
   case camera.CameraInterface of
    INDI : camera.Connect(config.GetValue('/INDI/Server',''),
                          config.GetValue('/INDI/ServerPort',''),
                          config.GetValue('/INDIcamera/Device',''),
                          config.GetValue('/INDIcamera/Sensor','CCD1'),
                          config.GetValue('/INDIcamera/DevicePort',''));
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
    i,j:integer;
    binstr:string;
begin
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
 f_preview.Binning.Clear;
 f_capture.Binning.Clear;
 i:=rxmin;
 while i<=rxmax do begin
   j:=rymin;
   while j<=rymax do begin
     if i=j then begin  // only "square" binning in combobox list
       binstr:=inttostr(i)+'x'+inttostr(j);
       f_preview.Binning.Items.Add(binstr);
       f_capture.Binning.Items.Add(binstr);
       f_editplan.Binning.Items.Add(binstr);
     end;
     inc(j,rystep);
   end;
   inc(i,rxstep);
 end;
 f_preview.Binning.ItemIndex:=0;
 f_capture.Binning.ItemIndex:=0;
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
begin
  if focuser.hasAbsolutePosition then begin
     f_focuser.PanelAbsPos.Visible:=true;
     f_focuser.PanelRelPos.Visible:=false;
     f_focuser.PanelTimerMove.Visible:=false;
  end
  else if focuser.hasRelativePosition then begin
     f_focuser.PanelAbsPos.Visible:=false;
     f_focuser.PanelRelPos.Visible:=true;
     f_focuser.PanelTimerMove.Visible:=false;
  end
  else begin
     f_focuser.PanelAbsPos.Visible:=false;
     f_focuser.PanelRelPos.Visible:=false;
     f_focuser.PanelTimerMove.Visible:=true;
  end;
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
  f_msg.msg.ScrollBy(0,f_msg.msg.Lines.Count);
  if LogToFile then begin
    WriteLog(msg);
  end;
 end;
end;

Procedure Tf_main.CameraStatus(Sender: TObject);
var bx,by: integer;
    buf: string;
begin
 case camera.Status of
   devDisconnected:begin
                   f_preview.stop;
                   f_capture.stop;
                   Capture:=false;
                   f_sequence.CameraDisconnected;
                   StatusBar1.Panels[1].Text:='';
                   f_devicesconnection.LabelCamera.Font.Color:=clRed;
                   end;
   devConnecting:  begin
                   NewMessage('Connecting camera...');
                   f_devicesconnection.LabelCamera.Font.Color:=clOrange;
                   end;
   devConnected:   begin
                   NewMessage('Camera connected');
                   bx:=camera.BinX;
                   by:=camera.BinY;
                   buf:=inttostr(bx)+'x'+inttostr(by);
                   f_preview.Binning.Text:=buf;
                   f_devicesconnection.LabelCamera.Font.Color:=clGreen;
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
 NewMessage('Exposure aborted!');
 f_preview.stop;
 f_capture.stop;
 Capture:=false;
 f_sequence.ExposureAborted;
end;

procedure  Tf_main.CameraTemperatureChange(t:double);
begin
 f_ccdtemp.Current.Text:=FormatFloat(f1,t);
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
                      Filters.Assign(wheel.FilterNames);
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
begin
if (n>=0)and(n<=f_filterwheel.Filters.Items.Count) then
   f_filterwheel.Filters.ItemIndex:=round(n);
end;

procedure Tf_main.FilterNameChange(Sender: TObject);
begin
f_filterwheel.Filters.Items.Assign(wheel.FilterNames);
f_EditPlan.Filter.Items.Assign(wheel.FilterNames);
Filters.Assign(wheel.FilterNames);
if (wheel.Filter>=0)and(wheel.Filter<=f_filterwheel.Filters.Items.Count) then
   f_filterwheel.Filters.ItemIndex:=round(wheel.Filter);
end;

procedure Tf_main.MenuHelpAboutClick(Sender: TObject);
var aboutmsg: string;
begin
aboutmsg:='CCDciel '+crlf;
aboutmsg:=aboutmsg+ccdciel_version+'-'+RevisionStr+blank+compile_time+crlf;
aboutmsg:=aboutmsg+'Compiled with:'+crlf;
aboutmsg:=aboutmsg+blank+compile_version+crlf+crlf;
aboutmsg:=aboutmsg+'Copyright (C) 2015 Patrick Chevalley'+crlf;
aboutmsg:=aboutmsg+'http://www.ap-i.net'+crlf+crlf;
aboutmsg:=aboutmsg+'This program is free software; you can redistribute it and/or'+crlf;
aboutmsg:=aboutmsg+'modify it under the terms of the GNU General Public License'+crlf;
aboutmsg:=aboutmsg+'as published by the Free Software Foundation; either version 3'+crlf;
aboutmsg:=aboutmsg+'of the License, or (at your option) any later version.'+crlf;
ShowMessage(aboutmsg);
end;

Procedure Tf_main.FocuserStatus(Sender: TObject);
var i,n: integer;
    r: TNumRange;
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
                      f_focuser.Position.Text:=inttostr(focuser.Position);
                      r:=focuser.PositionRange;
                      if r.step>0 then begin
                       f_focuser.Position.Hint:='Current focuser absolute position, '+
                                       IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
                        n:=round(max(1000,r.step));
                        f_focuser.PosIncr.Clear;
                        for i:=1 to 5 do begin
                          f_focuser.PosIncr.Items.Add(inttostr(i*n));
                        end;
                        f_focuser.PosIncr.ItemIndex:=0;
                      end;
                      f_focuser.speed.Text:=inttostr(focuser.Speed);
                      f_focuser.timer.Text:=inttostr(focuser.Timer);
                      r:=focuser.RelPositionRange;
                      if r.step>0 then begin
                        f_focuser.RelIncr.Hint:='Relative increment for the inward or outward movement, '+
                                        IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
                        n:=round(max(1000,r.step));
                        f_focuser.RelIncr.Clear;
                        for i:=1 to 5 do begin
                          f_focuser.RelIncr.Items.Add(inttostr(i*n));
                        end;
                        f_focuser.RelIncr.ItemIndex:=0;
                      end;
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

procedure Tf_main.FocusIN(Sender: TObject);
var n:integer;
begin
 if focuser.hasAbsolutePosition then begin
    focuser.Position:=focuser.Position-StrToIntDef(f_focuser.PosIncr.Text,1000);
 end
 else if focuser.hasRelativePosition then begin
    focuser.FocusIn;
    focuser.RelPosition:=StrToIntDef(f_focuser.RelIncr.Text,1000);
 end
 else begin
    n:=StrToIntDef(f_focuser.speed.Text,-1);
    if n>0 then focuser.Speed:=n;
    focuser.FocusIn;
    n:=StrToIntDef(f_focuser.timer.Text,-1);
    if n>0 then focuser.Timer:=n;
 end;
end;

procedure Tf_main.FocusOUT(Sender: TObject);
var n:integer;
begin
 if focuser.hasAbsolutePosition then begin
   focuser.Position:=focuser.Position+StrToIntDef(f_focuser.PosIncr.Text,1000);
 end
 else if focuser.hasRelativePosition then begin
    focuser.FocusIn;
    focuser.RelPosition:=StrToIntDef(f_focuser.RelIncr.Text,1000);
 end
 else begin
   n:=StrToIntDef(f_focuser.speed.Text,-1);
   if n>0 then focuser.Speed:=n;
   focuser.FocusOut;
   n:=StrToIntDef(f_focuser.timer.Text,-1);
   if n>0 then focuser.Timer:=n;
 end;
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
                      NewMessage('Mount connected');
                      f_devicesconnection.LabelMount.Font.Color:=clGreen;
                      MountCoordChange(Sender);
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.MountCoordChange(Sender: TObject);
begin
 f_mount.RA.Text:=RAToStr(mount.RA);
 f_mount.DE.Text:=DEToStr(mount.Dec);
end;

Procedure Tf_main.AutoguiderConnectClick(Sender: TObject);
begin
 if f_autoguider.BtnConnect.Caption='Connect' then begin
   autoguider.Connect(config.GetValue('/Autoguider/PHDhostname','localhost'),
                      config.GetValue('/Autoguider/PHDport','4400'));
   f_autoguider.BtnConnect.Caption:='Disconnect';
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
                       end;
   GUIDER_IDLE        :begin
                       f_autoguider.led.Brush.Color:=clYellow;
                       f_autoguider.BtnGuide.Caption:='Guide';
                       f_sequence.AutoguiderIddle;
                       end;
   GUIDER_GUIDING     :begin
                       f_autoguider.led.Brush.Color:=clLime;
                       f_autoguider.BtnGuide.Caption:='Stop';
                       end;
   GUIDER_BUSY        :begin
                       f_autoguider.led.Brush.Color:=clOrange;
                       f_autoguider.BtnGuide.Caption:='Stop';
                       end;
   GUIDER_ALERT       :begin
                       f_autoguider.led.Brush.Color:=clRed;
                       end;
 end;
 if autoguider.LastError<>'' then NewMessage(autoguider.LastError);

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
begin
  if camera.Status<>devDisconnected then begin
    ShowMessage('Disconnect the camera before to change the configuration.');
    exit;
  end;

  f_setup.ConnectionInterface:=TDevInterface(config.GetValue('/Interface',ord(camera.CameraInterface)));
  f_setup.IndiServer.Text:=config.GetValue('/INDI/Server','localhost');
  f_setup.IndiPort.Text:=config.GetValue('/INDI/ServerPort','7624');

  f_setup.DeviceList.Checked[0]:=true;
  f_setup.DeviceList.Checked[1]:=config.GetValue('/Devices/FilterWheel',false);
  f_setup.DeviceList.Checked[2]:=config.GetValue('/Devices/Focuser',false);;
  f_setup.DeviceList.Checked[3]:=config.GetValue('/Devices/Mount',false);;

  f_setup.CameraConnection:=TDevInterface(config.GetValue('/CameraInterface',ord(camera.CameraInterface)));
  if f_setup.CameraIndiDevice.Items.Count=0 then begin
    f_setup.CameraIndiDevice.Items.Add(config.GetValue('/INDIcamera/Device',''));
    f_setup.CameraIndiDevice.ItemIndex:=0;
  end;
  f_setup.CameraIndiDevice.Text:=config.GetValue('/INDIcamera/Device','');
  f_setup.CameraSensor:=config.GetValue('/INDIcamera/Sensor','CCD1');
  f_setup.CameraIndiDevPort.Text:=config.GetValue('/INDIcamera/DevicePort','');
  f_setup.AscomCamera.Text:=config.GetValue('/ASCOMcamera/Device','');

  f_setup.WheelConnection:=TDevInterface(config.GetValue('/FilterWheelInterface',ord(wheel.WheelInterface)));
  if f_setup.WheelIndiDevice.Items.Count=0 then begin
    f_setup.WheelIndiDevice.Items.Add(config.GetValue('/INDIwheel/Device',''));
    f_setup.WheelIndiDevice.ItemIndex:=0;
  end;
  f_setup.WheelIndiDevice.Text:=config.GetValue('/INDIwheel/Device','');
  f_setup.WheelIndiDevPort.Text:=config.GetValue('/INDIwheel/DevicePort','');
  f_setup.AscomWheel.Text:=config.GetValue('/ASCOMwheel/Device','');

  f_setup.FocuserConnection:=TDevInterface(config.GetValue('/FocuserInterface',ord(focuser.FocuserInterface)));
  if f_setup.FocuserIndiDevice.Items.Count=0 then begin
    f_setup.FocuserIndiDevice.Items.Add(config.GetValue('/INDIfocuser/Device',''));
    f_setup.FocuserIndiDevice.ItemIndex:=0;
  end;
  f_setup.FocuserIndiDevice.Text:=config.GetValue('/INDIfocuser/Device','');
  f_setup.FocuserIndiDevPort.Text:=config.GetValue('/INDIfocuser/DevicePort','');
  f_setup.AscomFocuser.Text:=config.GetValue('/ASCOMfocuser/Device','');

  f_setup.MountConnection:=TDevInterface(config.GetValue('/MountInterface',ord(mount.MountInterface)));
  if f_setup.MountIndiDevice.Items.Count=0 then begin
    f_setup.MountIndiDevice.Items.Add(config.GetValue('/INDImount/Device',''));
    f_setup.MountIndiDevice.ItemIndex:=0;
  end;
  f_setup.MountIndiDevice.Text:=config.GetValue('/INDImount/Device','');
  f_setup.MountIndiDevPort.Text:=config.GetValue('/INDImount/DevicePort','');
  f_setup.AscomMount.Text:=config.GetValue('/ASCOMmount/Device','');

  FormPos(f_setup,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_setup.ShowModal;

  if f_setup.ModalResult=mrOK then begin
    config.SetValue('/Interface',ord(f_setup.ConnectionInterface));
    config.SetValue('/INDI/Server',f_setup.IndiServer.Text);
    config.SetValue('/INDI/ServerPort',f_setup.IndiPort.Text);

    config.SetValue('/Devices/Camera',f_setup.DeviceList.Checked[0]);
    config.SetValue('/Devices/FilterWheel',f_setup.DeviceList.Checked[1]);
    config.SetValue('/Devices/Focuser',f_setup.DeviceList.Checked[2]);;
    config.SetValue('/Devices/Mount',f_setup.DeviceList.Checked[3]);;

    config.SetValue('/CameraInterface',ord(f_setup.CameraConnection));
    if f_setup.CameraIndiDevice.Text<>'' then config.SetValue('/INDIcamera/Device',f_setup.CameraIndiDevice.Text);
    config.SetValue('/INDIcamera/Sensor',f_setup.CameraSensor);
    config.SetValue('/INDIcamera/DevicePort',f_setup.CameraIndiDevPort.Text);
    config.SetValue('/ASCOMcamera/Device',f_setup.AscomCamera.Text);

    config.SetValue('/FilterWheelInterface',ord(f_setup.WheelConnection));
    if f_setup.WheelIndiDevice.Text<>'' then config.SetValue('/INDIwheel/Device',f_setup.WheelIndiDevice.Text);
    config.SetValue('/INDIwheel/DevicePort',f_setup.WheelIndiDevPort.Text);
    config.SetValue('/ASCOMwheel/Device',f_setup.AscomWheel.Text);

    config.SetValue('/FocuserInterface',ord(f_setup.FocuserConnection));
    if f_setup.FocuserIndiDevice.Text<>'' then config.SetValue('/INDIfocuser/Device',f_setup.FocuserIndiDevice.Text);
    config.SetValue('/INDIfocuser/DevicePort',f_setup.FocuserIndiDevPort.Text);
    config.SetValue('/ASCOMfocuser/Device',f_setup.AscomFocuser.Text);

    config.SetValue('/MountInterface',ord(f_setup.MountConnection));
    if f_setup.MountIndiDevice.Text<>'' then config.SetValue('/INDImount/Device',f_setup.MountIndiDevice.Text);
    config.SetValue('/INDImount/DevicePort',f_setup.MountIndiDevPort.Text);
    config.SetValue('/ASCOMmount/Device',f_setup.AscomMount.Text);

    config.Flush;

    if f_setup.RestartRequired then
       Restart
    else
       SetConfig;
  end;
end;

procedure Tf_main.MenuOptionsClick(Sender: TObject);
begin
   f_option.onGetPixelSize:=@OptionGetPixelSize;
   f_option.onGetFocale:=@OptionGetFocaleLength;
   f_option.CaptureDir.Text:=config.GetValue('/Files/CapturePath',defCapturePath);
   f_option.SubfolderSequence.Checked:=config.GetValue('/Files/SubfolderSequence',false);
   f_option.SubfolderObjname.Checked:=config.GetValue('/Files/SubfolderObjname',false);
   f_option.SubfolderStep.Checked:=config.GetValue('/Files/SubfolderStep',false);
   f_option.SubfolderFrametype.Checked:=config.GetValue('/Files/SubfolderFrametype',false);
   f_option.FileObjname.Checked:=config.GetValue('/Files/FilenameObjname',true);
   f_option.FileFiltername.Checked:=config.GetValue('/Files/FilenameFilter',true);
   f_option.FileDate.Checked:=config.GetValue('/Files/FilenameDate',true);
   f_option.Logtofile.Checked:=config.GetValue('/Log/Messages',true);
   f_option.Logtofile.Hint:='Log files are saved in '+ExtractFilePath(LogFile);
   f_option.ObservatoryName.Text:=config.GetValue('/Info/ObservatoryName','');
   f_option.ObserverName.Text:=config.GetValue('/Info/ObserverName','');
   f_option.TelescopeName.Text:=config.GetValue('/Info/TelescopeName','');
   f_option.StarWindow.Text:=inttostr(config.GetValue('/StarAnalysis/Window',Starwindow));
   f_option.FocusWindow.Text:=inttostr(config.GetValue('/StarAnalysis/Focus',Focuswindow));
   f_option.PixelSize.Text:=config.GetValue('/Astrometry/PixelSize','');
   f_option.Focale.Text:=config.GetValue('/Astrometry/FocaleLength','');
   f_option.PixelSizeFromCamera.Checked:=config.GetValue('/Astrometry/PixelSizeFromCamera',true);
   f_option.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
   if f_option.PixelSizeFromCamera.Checked and (camera.PixelSizeX>0) then
      f_option.PixelSize.Text:=FormatFloat(f2,camera.PixelSizeX);
   f_option.FocaleFromTelescope.Checked:=config.GetValue('/Astrometry/FocaleFromTelescope',true);
   if f_option.FocaleFromTelescope.Checked and (mount.FocaleLength>0) then
      f_option.Focale.Text:=FormatFloat(f0,mount.FocaleLength);
   f_option.Tolerance.Text:=FormatFloat(f2,config.GetValue('/Astrometry/ScaleTolerance',0.1));
   f_option.MinRadius.Text:=FormatFloat(f1,config.GetValue('/Astrometry/MinRadius',5.0));
   f_option.Downsample.Text:=IntToStr(config.GetValue('/Astrometry/DownSample',4));
   f_option.SourcesLimit.Text:=IntToStr(config.GetValue('/Astrometry/SourcesLimit',150));
   f_option.Plot.Checked:=config.GetValue('/Astrometry/Plot',false);
   f_option.ElbrusFolder.Text:=config.GetValue('/Astrometry/ElbrusFolder','C:\Elbrus\Images');
   {$ifdef unix}
   f_option.ElbrusUnixpath.Text:=config.GetValue('/Astrometry/ElbrusUnixpath',ExpandFileName('~/Elbrus/Images'));
   {$endif}
   f_option.AutoguiderBox.ItemIndex:=config.GetValue('/Autoguider/Software',0);
   f_option.PHDhostname.Text:=config.GetValue('/Autoguider/PHDhostname','localhost');
   f_option.PHDport.Text:=config.GetValue('/Autoguider/PHDport','4400');
   f_option.DitherPixel.Text:=config.GetValue('/Autoguider/Dither/Pixel','1.0');
   f_option.DitherRAonly.Checked:=config.GetValue('/Autoguider/Dither/RAonly',true);
   f_option.SettlePixel.Text:=config.GetValue('/Autoguider/Settle/Pixel','1.0');
   f_option.SettleMinTime.Text:=config.GetValue('/Autoguider/Settle/MinTime','5');
   f_option.SettleMaxTime.Text:=config.GetValue('/Autoguider/Settle/MaxTime','30');
   f_option.CalibrationDelay.Text:=config.GetValue('/Autoguider/Settle/CalibrationDelay','300');
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
     config.SetValue('/Files/FilenameObjname',f_option.FileObjname.Checked);
     config.SetValue('/Files/FilenameFilter',f_option.FileFiltername.Checked);
     config.SetValue('/Files/FilenameDate',f_option.FileDate.Checked);
     config.SetValue('/StarAnalysis/Window',StrToIntDef(f_option.StarWindow.Text,Starwindow));
     config.SetValue('/StarAnalysis/Focus',StrToIntDef(f_option.FocusWindow.Text,Focuswindow));
     config.SetValue('/Log/Messages',f_option.Logtofile.Checked);
     config.SetValue('/Info/ObservatoryName',f_option.ObservatoryName.Text);
     config.SetValue('/Info/ObserverName',f_option.ObserverName.Text);
     config.SetValue('/Info/TelescopeName',f_option.TelescopeName.Text);
     config.SetValue('/Astrometry/Resolver',f_option.Resolver);
     config.SetValue('/Astrometry/PixelSizeFromCamera',f_option.PixelSizeFromCamera.Checked);
     config.SetValue('/Astrometry/FocaleFromTelescope',f_option.FocaleFromTelescope.Checked);
     config.SetValue('/Astrometry/PixelSize',f_option.PixelSize.Text);
     config.SetValue('/Astrometry/FocaleLength',f_option.Focale.Text);
     config.SetValue('/Astrometry/ScaleTolerance',StrToFloatDef(f_option.Tolerance.Text,0.1 ));
     config.SetValue('/Astrometry/MinRadius',StrToFloatDef(f_option.MinRadius.Text,5.0));
     config.SetValue('/Astrometry/DownSample',StrToIntDef(f_option.Downsample.Text,4));
     config.SetValue('/Astrometry/SourcesLimit',StrToIntDef(f_option.SourcesLimit.Text,0));
     config.SetValue('/Astrometry/Plot',f_option.Plot.Checked);
     config.SetValue('/Astrometry/ElbrusFolder',f_option.ElbrusFolder.Text);
     {$ifdef unix}
     config.SetValue('/Astrometry/ElbrusUnixpath',f_option.ElbrusUnixpath.Text);
     {$endif}
     config.SetValue('/Autoguider/Software',f_option.AutoguiderBox.ItemIndex);
     config.SetValue('/Autoguider/PHDhostname',f_option.PHDhostname.Text);
     config.SetValue('/Autoguider/PHDport',f_option.PHDport.Text);
     config.SetValue('/Autoguider/Dither/Pixel',f_option.DitherPixel.Text);
     config.SetValue('/Autoguider/Dither/RAonly',f_option.DitherRAonly.Checked);
     config.SetValue('/Autoguider/Settle/Pixel',f_option.SettlePixel.Text);
     config.SetValue('/Autoguider/Settle/MinTime',f_option.SettleMinTime.Text);
     config.SetValue('/Autoguider/Settle/MaxTime',f_option.SettleMaxTime.Text);
     config.SetValue('/Autoguider/Settle/CalibrationDelay',f_option.CalibrationDelay.Text);
     config.SetValue('/Planetarium/Software',f_option.PlanetariumBox.ItemIndex);
     config.SetValue('/Planetarium/CdChostname',f_option.CdChostname.Text);
     config.SetValue('/Planetarium/CdCport',trim(f_option.CdCport.Text));

     config.Flush;

     SetOptions;
   end;
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

procedure Tf_main.MenuViewSequenceClick(Sender: TObject);
begin
  f_sequence.Visible:=MenuViewSequence.Checked;
end;

procedure Tf_main.MenuViewStarProfileClick(Sender: TObject);
begin
  f_starprofile.Visible:=MenuViewStarProfile.Checked;
end;

procedure Tf_main.MenuViewAutoguiderClick(Sender: TObject);
begin
  f_autoguider.Visible:=MenuViewAutoguider.Checked;
end;

procedure Tf_main.PanelDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
if sender is TPanel then begin
  if sender is TPanel then begin
    if source is TStaticText then begin
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
     TFrame(Source).Parent:=TPanel(Sender);
     TFrame(Source).Top:=Y;
     TFrame(Source).Left:=X;
     if TPanel(Sender).Width>TPanel(Sender).Height then begin
        TFrame(Source).Align:=alLeft;
     end else begin
        TFrame(Source).Align:=alTop;
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
  camera.AbortExposure;
  Preview:=false;
  Capture:=false;
  NewMessage('Abort exposure');
  StatusBar1.Panels[1].Text:='Stop';
end;

Procedure Tf_main.StartPreviewExposure(Sender: TObject);
var e: double;
    buf: string;
    p,binx,biny: integer;
begin
if (camera.Status=devConnected) and (not Capture) then begin
  Preview:=true;
  PreviewLoop:=f_preview.Loop;
  e:=StrToFloatDef(f_preview.ExpTime.Text,-1);
  if e<0 then begin
    NewMessage('Invalid exposure time '+f_preview.ExpTime.Text);
    f_preview.stop;
    Preview:=false;
    exit;
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
  camera.StartExposure(e);
end
else begin
   f_preview.stop;
   Preview:=false;
   StatusBar1.Panels[1].Text:='';
end;
end;

Procedure Tf_main.StartCaptureExposure(Sender: TObject);
var e: double;
    buf: string;
    p,binx,biny: integer;
    ftype:TFrameType;
begin
if (camera.Status=devConnected) then begin
  if Preview then begin
    camera.AbortExposure;
    f_preview.stop;
    NewMessage('Stop preview');
    StatusBar1.Panels[1].Text:='';
  end;
  Preview:=false;
  Capture:=true;
  e:=StrToFloatDef(f_capture.ExpTime.Text,-1);
  if e<0 then begin
    NewMessage('Invalid exposure time '+f_capture.ExpTime.Text);
    f_capture.Stop;
    Capture:=false;
    exit;
  end;
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
  if (f_capture.FrameType.ItemIndex>=0)and(f_capture.FrameType.ItemIndex<=ord(High(TFrameType))) then begin
    ftype:=TFrameType(f_capture.FrameType.ItemIndex);
    if camera.FrameType<>ftype then camera.FrameType:=ftype;
  end;
  camera.ObjectName:=f_capture.Fname.Text;
  NewMessage('Starting '+f_capture.FrameType.Text+' exposure '+inttostr(f_capture.SeqCount)+' for '+f_capture.ExpTime.Text+' seconds');
  camera.StartExposure(e);
end
else begin
   f_capture.Stop;
   Capture:=false;
   StatusBar1.Panels[1].Text := '';
end;
end;

procedure Tf_main.CameraProgress(n:double);
var txt: string;
begin
 if (n<=0) then begin
   if ((f_capture.Running)or(f_preview.Running)) then begin
     txt := 'Downloading...';
     if Capture then begin
       if f_capture.Running then
         StatusBar1.Panels[1].Text := 'Seq: '+inttostr(f_capture.SeqCount)+' '+txt;
     end
     else begin
        StatusBar1.Panels[1].Text := txt;
     end;
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
var dt: Tdatetime;
    fn,imgsize: string;
    subseq,subobj,substep,subfrt: boolean;
    fnobj,fnfilter,fndate: boolean;
    fileseqnum,i: integer;
begin
  dt:=NowUTC;
  ImgFrameX:=FrameX;
  ImgFrameY:=FrameY;
  ImgFrameW:=FrameW;
  ImgFrameH:=FrameH;
  imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
  DrawImage;
  DrawHistogram;
  if Capture then begin
     subseq:=config.GetValue('/Files/SubfolderSequence',false);
     subobj:=config.GetValue('/Files/SubfolderObjname',false);
     subfrt:=config.GetValue('/Files/SubfolderFrametype',false);
     substep:=config.GetValue('/Files/SubfolderStep',false);
     fn:=slash(config.GetValue('/Files/CapturePath',defCapturePath));
     if subseq and f_sequence.Running then fn:=slash(fn+f_sequence.CurrentName);
     if subfrt then fn:=slash(fn+f_capture.FrameType.Text);
     if subobj then fn:=slash(fn+f_capture.Fname.Text);
     if substep and f_sequence.Running then begin
        if f_sequence.StepTotalCount>1 then begin
          fn:=slash(fn+f_sequence.CurrentStep+'_'+IntToStr(f_sequence.StepRepeatCount))
        end
        else begin
          fn:=slash(fn+f_sequence.CurrentStep);
        end;
     end;
     ForceDirectoriesUTF8(fn);
     fnobj:=config.GetValue('/Files/FilenameObjname',true);
     fnfilter:=config.GetValue('/Files/FilenameFilter',true);
     fndate:=config.GetValue('/Files/FilenameDate',true);
     if fnobj then begin
       if trim(f_capture.FrameType.Text)=trim(FrameName[0]) then
           fn:=fn+f_capture.Fname.Text+'_'
       else
           fn:=fn+f_capture.FrameType.Text+'_';
     end;
     if fnfilter and (wheel.Status=devConnected)and(f_capture.FrameType.ItemIndex<>1)and(f_capture.FrameType.ItemIndex<>2) then
         fn:=fn+wheel.FilterNames[wheel.Filter]+'_';
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
     f_capture.SeqCount:=f_capture.SeqCount+1;
     if f_capture.SeqCount<=StrToInt(f_capture.SeqNum.Text) then begin
        if f_capture.Running then StartCaptureExposure(nil);
     end else begin
        Capture:=false;
        f_capture.Stop;
        NewMessage('Stop capture');
        StatusBar1.Panels[1].Text := 'Seq: '+inttostr(f_capture.SeqCount-1)+' Finished';
     end;
  end
  else if Preview then begin
    StatusBar1.Panels[2].Text:='Preview '+FormatDateTime('hh:nn:ss',now)+'  '+imgsize;
    if f_preview.Loop and f_preview.Running then StartPreviewExposure(nil)
       else begin
         f_preview.stop;
         NewMessage('End preview');
         StatusBar1.Panels[1].Text:='';
    end;
  end;
end;

Procedure Tf_main.RedrawHistogram(Sender: TObject);
begin
  DrawHistogram;
end;

Procedure Tf_main.Redraw(Sender: TObject);
begin
  DrawImage;
  DrawHistogram;
end;

Procedure Tf_main.ZoomImage(Sender: TObject);
begin
  ImgZoom:=f_visu.Zoom;
  PlotImage;
end;

Procedure Tf_main.ImgFullRange(Sender: TObject);
begin
  fits.ImgFullRange:=f_visu.FullRange.Down;
  DrawImage;
  DrawHistogram;
end;

Procedure Tf_main.DrawImage;
begin
if fits.HeaderInfo.naxis>0 then begin
  if f_visu.BtnLinear.Checked then fits.itt:=ittlinear
  else if f_visu.BtnLog.Checked then fits.itt:=ittlog
  else if f_visu.BtnSqrt.Checked then fits.itt:=ittsqrt;
  fits.ImgDmax:=f_visu.ImgMax*256;
  fits.ImgDmin:=f_visu.ImgMin*256;
  fits.GetIntfImg;
  fits.GetBitmap(ImaBmp);
  img_Width:=ImaBmp.Width;
  img_Height:=ImaBmp.Height;
  if f_starprofile.FindStar then
    f_starprofile.showprofile(fits.image,fits.imageC,fits.imageMin,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2,mount.FocaleLength,camera.PixelSize);
  PlotImage;
end;
end;

Procedure Tf_main.ClearImage;
begin
image1.Picture.Bitmap.Canvas.Brush.Color:=clDarkBlue;
image1.Picture.Bitmap.Canvas.Pen.Color:=clBlack;
image1.Picture.Bitmap.Canvas.FillRect(0,0,image1.Width,image1.Height);
end;

Procedure Tf_main.PlotImage;
var r1,r2: double;
    w,h,px,py: integer;
    bmp2:Tbitmap;
begin
ClearImage;
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
  image1.Picture.Bitmap.Canvas.StretchDraw(rect(0,0,w,h),ImaBmp);
end
else if ImgZoom=0.5 then begin
   // zoom 0.5
   bmp2:=Tbitmap.Create;
   bmp2.SetSize(Image1.Width * 2,Image1.Height * 2);
   bmp2.Canvas.Brush.Color:=clDarkBlue;
   bmp2.Canvas.Pen.Color:=clBlack;
   bmp2.Canvas.FillRect(0,0,bmp2.Width,bmp2.Height);
   px:=ImgCx-((img_Width-bmp2.Width) div 2);
   py:=ImgCy-((img_Height-bmp2.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   bmp2.Canvas.Draw(px,py,ImaBmp);
   image1.Picture.Bitmap.Canvas.StretchDraw(rect(0,0,image1.width,image1.Height),bmp2);
   bmp2.Free;
end
else if ImgZoom=1 then begin
   // zoom 1
   px:=ImgCx-((img_Width-Image1.Width) div 2);
   py:=ImgCy-((img_Height-Image1.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   image1.Picture.Bitmap.Canvas.Draw(px,py,ImaBmp);
end
else if ImgZoom=2 then begin
   // zoom 2
   bmp2:=Tbitmap.Create;
   bmp2.SetSize(Image1.Width div 2,Image1.Height div 2);
   bmp2.Canvas.Brush.Color:=clDarkBlue;
   bmp2.Canvas.Pen.Color:=clBlack;
   bmp2.Canvas.FillRect(0,0,bmp2.Width,bmp2.Height);
   px:=ImgCx-((img_Width-bmp2.Width) div 2);
   py:=ImgCy-((img_Height-bmp2.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   bmp2.Canvas.Draw(px,py,ImaBmp);
   image1.Picture.Bitmap.Canvas.StretchDraw(rect(0,0,image1.width,image1.Height),bmp2);
   bmp2.Free;
end;
Application.ProcessMessages;
end;

procedure Tf_main.Image1Paint(Sender: TObject);
var x,y,s: integer;
begin
  Inherited paint;
  if f_starprofile.FindStar then begin
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
     if ImgZoom=0      then s:=round(Starwindow * ImgScale0)
     else if ImgZoom=0.5 then s:=Starwindow div 2
     else if ImgZoom=1 then s:=Starwindow
     else if ImgZoom=2 then s:=2*Starwindow;
     with Image1.Canvas do begin
        Pen.Color:=clLime;
        Frame(x-s,y-s,x+s,y+s);
     end;
  end;
end;

Procedure Tf_main.DrawHistogram;
begin
  if fits.HeaderInfo.naxis>0 then begin
     f_visu.DrawHistogram(fits.Histogram);
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

procedure Tf_main.MenuOpenClick(Sender: TObject);
var mem: TMemoryStream;
    fn,imgsize: string;
begin
  if OpenDialog1.Execute then begin
     fn:=OpenDialog1.FileName;
     mem:=TMemoryStream.Create;
     mem.LoadFromFile(fn);
     fits.Stream:=mem;
     mem.free;
     DrawImage;
     DrawHistogram;
     imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
     NewMessage('Open file '+fn);
     StatusBar1.Panels[2].Text:='Open file '+fn+' '+imgsize;
  end;
end;

procedure Tf_main.MenuSaveClick(Sender: TObject);
var fn: string;
begin
if fits.HeaderInfo.naxis>0 then begin
   if SaveDialog1.Execute then begin
      fn:=SaveDialog1.FileName;
      fits.SaveToFile(fn);
   end;
end;
end;

Procedure Tf_main.FocusStart(Sender: TObject);
var x,y,xc,yc,s,s2: integer;
begin
  if f_starprofile.FindStar then begin
     s:=Focuswindow;
     s2:=s div 2;
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),x,y);
     Screen2CCD(x,y,xc,yc);
     camera.SetFrame(xc-s2,yc-s2,s,s);
     f_preview.Loop:=true;
     f_preview.Running:=true;
     f_starprofile.StarX:=s2;
     f_starprofile.StarY:=s2;
     NewMessage('Focus aid started');
     SaveFocusZoom:=f_visu.Zoom;
     f_visu.Zoom:=0;
     ImgZoom:=0;
     StartPreviewExposure(nil);
  end
  else begin
    f_starprofile.focus.Checked:=false;
    NewMessage('Select a star first!');
  end;
end;

Procedure Tf_main.FocusStop(Sender: TObject);
begin
   camera.ResetFrame;
   f_preview.Running:=false;
   f_preview.Loop:=false;
   f_visu.Zoom:=SaveFocusZoom;
   ImgZoom:=f_visu.Zoom;
   StartPreviewExposure(nil);
   NewMessage('Focus aid stoped');
end;

procedure Tf_main.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_main.AstrometryStart(Sender: TObject);
begin
  // update menu
  MenuResolvePlanetarium.Enabled:=false;
  MenuResolveSync.Enabled:=false;
  MenuResolveSlew.Enabled:=false;
  MenuStopAstrometry.Visible:=true;
end;

procedure Tf_main.AstrometryEnd(Sender: TObject);
begin
  // update menu
  MenuStopAstrometry.Visible:=false;
  MenuResolvePlanetarium.Enabled:=true;
  MenuResolveSync.Enabled:=true;
  MenuResolveSlew.Enabled:=true;
  if not astrometry.LastResult then NewMessage(astrometry.Resolver+' resolve error.');
end;

procedure Tf_main.MenuStopAstrometryClick(Sender: TObject);
begin
  astrometry.StopAstrometry;
  MenuStopAstrometry.Visible:=false;
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

procedure Tf_main.MenuResolveSyncClick(Sender: TObject);
begin
  astrometry.SyncCurrentImage(false);
end;

procedure Tf_main.MenuResolveSlewClick(Sender: TObject);
begin
  astrometry.SlewScreenXY(MouseDownX,MouseDownY,false);
end;

procedure Tf_main.MenuResolvePlanetariumClick(Sender: TObject);
begin
 if planetarium.Connected then begin
    if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
      fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
      astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryToPlanetarium);
    end;
 end
 else
    NewMessage('Planetarium is not connected');
end;

procedure Tf_main.AstrometryToPlanetarium(Sender: TObject);
begin
if astrometry.LastResult and planetarium.Connected then begin
  NewMessage('Send image to planetarium');
  planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits');
end;
end;


Procedure Tf_main.PlanetariumConnectClick(Sender: TObject);
var i: integer;
begin
 if f_planetarium.BtnConnect.Caption='Connect' then begin
   f_planetarium.BtnConnect.Caption:='Disconnect';
   i:=config.GetValue('/Planetarium/Software',0);
   case TPlanetariumType(i) of
     CDC:  planetarium.Connect(config.GetValue('/Planetarium/CdChostname','localhost'),
                      config.GetValue('/Planetarium/CdCport',''));
     SAMP: planetarium.Connect('');
   end;
 end else begin
   planetarium.Disconnect;
 end;
end;

Procedure Tf_main.PlanetariumConnect(Sender: TObject);
begin
 f_planetarium.led.Brush.Color:=clLime;
 f_planetarium.Status.Text:='Connected';
 NewMessage('Planetarium: Connected');
end;

Procedure Tf_main.PlanetariumDisconnect(Sender: TObject);
var i: integer;
begin
 if not AppClose then begin
   f_planetarium.led.Brush.Color:=clGray;
   f_planetarium.Status.Text:='Disconnected';
   f_planetarium.BtnConnect.Caption:='Connect';
   NewMessage('Planetarium: Disconnected');
   i:=config.GetValue('/Planetarium/Software',0);
   case TPlanetariumType(i) of
     CDC: planetarium:=TPlanetarium_cdc.Create;
     SAMP:planetarium:=TPlanetarium_samp.Create;
   end;
   planetarium.onConnect:=@PlanetariumConnect;
   planetarium.onDisconnect:=@PlanetariumDisconnect;
   planetarium.onShowMessage:=@NewMessage;
   f_planetariuminfo.planetarium:=planetarium;
 end;
end;

end.


