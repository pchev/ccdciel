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

//{$define debug_raw}

interface

uses
  {$ifdef mswindows}
  ShlObj, comobj, windows,
  {$endif}
  {$ifdef unix}
  BaseUnix,
  {$endif}
  fu_devicesconnection, fu_preview, fu_capture, fu_msg, fu_visu, fu_frame, fu_magnifyer,
  fu_starprofile, fu_filterwheel, fu_focuser, fu_mount, fu_ccdtemp, fu_autoguider, fu_cover, fu_switch,
  fu_sequence, fu_planetarium, fu_script, u_ccdconfig, pu_edittargets, pu_scriptengine,
  fu_video, pu_devicesetup, pu_options, pu_indigui, cu_fits, cu_camera, pu_pause, cu_tcpserver,
  pu_viewtext, cu_wheel, cu_mount, cu_focuser, XMLConf, u_utils, u_global, UScaleDPI,
  cu_indimount, cu_ascommount, cu_indifocuser, cu_ascomfocuser, pu_vcurve, pu_focusercalibration,
  fu_rotator, cu_rotator, cu_indirotator, cu_ascomrotator, cu_watchdog, cu_indiwatchdog,
  cu_weather, cu_ascomweather, cu_indiweather, cu_safety, cu_ascomsafety, cu_indisafety, fu_weather, fu_safety,
  cu_dome, cu_ascomdome, cu_indidome, fu_dome, pu_about, pu_goto, pu_photometry, u_libraw, pu_image_sharpness,
  cu_indiwheel, cu_ascomwheel, cu_incamerawheel, cu_indicamera, cu_ascomcamera, cu_astrometry,
  cu_autoguider, cu_autoguider_phd, cu_autoguider_linguider, cu_autoguider_none, cu_autoguider_dither, cu_planetarium,
  cu_planetarium_cdc, cu_planetarium_samp, cu_planetarium_hnsky, cu_planetarium_none, pu_planetariuminfo, indiapi,
  cu_ascomrestcamera, cu_ascomrestdome, cu_ascomrestfocuser, cu_ascomrestmount, cu_manualwheel,
  cu_ascomrestrotator, cu_ascomrestsafety, cu_ascomrestweather, cu_ascomrestwheel, pu_polaralign, pu_collimation,
  cu_switch, cu_ascomswitch, cu_ascomrestswitch, cu_indiswitch, cu_cover, cu_ascomcover, cu_ascomrestcover, cu_indicover,
  u_annotation, BGRABitmap, BGRABitmapTypes, LCLVersion, InterfaceBase, lclplatformdef,
  LazUTF8, Classes, dynlibs, LCLType, LMessages, IniFiles, IntfGraphics, FPImage, GraphType,
  SysUtils, LazFileUtils, Forms, Controls, Math, Graphics, Dialogs, u_speech,
  StdCtrls, ExtCtrls, Menus, ComCtrls, Buttons, Types, u_translation;

type

  TImgDrawingControl = class(TGraphicControl)
  public
    property OnPaint;
    property OnDblClick;
    property onMouseDown;
    property onMouseMove;
    property onMouseUp;
    property OnMouseWheel;
    property OnResize;
    property PopupMenu;
  end;

  TMessageData = class(TObject)
    public
      msgtxt: string;
      msglevel: integer;
  end;

  { Tf_main }

  Tf_main = class(TForm)
    FocuserConnectTimer: TTimer;
    CameraConnectTimer: TTimer;
    ImageListNight: TImageList;
    ImageListDay: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuBrowseLog: TMenuItem;
    MenuApplyBPM: TMenuItem;
    MenuFocuserCalibration: TMenuItem;
    MenuBPMDark: TMenuItem;
    MenuItem11: TMenuItem;
    MenuDarkApply: TMenuItem;
    MenuItem13: TMenuItem;
    MenuDarkCamera: TMenuItem;
    MenuDarkFile: TMenuItem;
    MenuDarkClear: TMenuItem;
    MenuItem14: TMenuItem;
    MenuAscomCameraSetup: TMenuItem;
    MenuAscomWheelSetup: TMenuItem;
    MenuAscomFocuserSetup: TMenuItem;
    MenuAscomMountSetup: TMenuItem;
    MenuAscomRotatorSetup: TMenuItem;
    MenuAscomWeatherSetup: TMenuItem;
    MenuAscomSafetySetup: TMenuItem;
    MenuAscomDomeSetup: TMenuItem;
    MenuAlpacaServerSetup: TMenuItem;
    MenuAlpacaCameraSetup: TMenuItem;
    MenuAlpacaWheelSetup: TMenuItem;
    MenuAlpacaFocuserSetup: TMenuItem;
    MenuAlpacaMountSetup: TMenuItem;
    MenuAlpacaRotatorSetup: TMenuItem;
    MenuAlpacaWeatherSetup: TMenuItem;
    MenuAlpacaSafetySetup: TMenuItem;
    MenuAlpacaDomeSetup: TMenuItem;
    MenuImgStat: TMenuItem;
    MenuImage: TMenuItem;
    MenuItem15: TMenuItem;
    MenuCollimation: TMenuItem;
    MenuAscomSwitchSetup: TMenuItem;
    MenuAscomCoverSetup: TMenuItem;
    MenuAlpacaCoverSetup: TMenuItem;
    MenuAlpacaSwitchSetup: TMenuItem;
    MenuItemUnselect2: TMenuItem;
    MenuItemUnselect: TMenuItem;
    MenuViewCover: TMenuItem;
    MenuViewSwitch: TMenuItem;
    MenuSavePicture: TMenuItem;
    MenuPolarAlignment: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItemPhotometry2: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItemPhotometry: TMenuItem;
    MenuItem16: TMenuItem;
    MenuResolveHyperLeda: TMenuItem;
    MenuResolveHyperLeda2: TMenuItem;
    MenuReset1col: TMenuItem;
    MenuReset2col: TMenuItem;
    MenuViewDome: TMenuItem;
    MenuViewWeather: TMenuItem;
    MenuViewSafety: TMenuItem;
    MenuItemDark: TMenuItem;
    MenuStatus: TMenuItem;
    MenuUsergroup: TMenuItem;
    MenuResolveDSO: TMenuItem;
    MenuResolveDSO2: TMenuItem;
    MenuViewMagnifyer: TMenuItem;
    MenuSaveConfig: TMenuItem;
    MenuItemCleanup: TMenuItem;
    MenuOpenPicture: TMenuItem;
    MenuShowINDIlog: TMenuItem;
    MenuShowLog: TMenuItem;
    MenuItem12: TMenuItem;
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
    MenuItem9: TMenuItem;
    MenuResolveRotate: TMenuItem;
    MenuResolveRotate2: TMenuItem;
    MenuViewClock: TMenuItem;
    MenuResolveSyncRotator: TMenuItem;
    MenuResolveSyncRotator2: TMenuItem;
    MenuRotatorRotate: TMenuItem;
    MenuRotatorRotate2: TMenuItem;
    MenuRotator: TMenuItem;
    MenuViewRotator: TMenuItem;
    OpenPictureDialog1: TOpenDialog;
    PanelMsgTabs: TPanel;
    PanelRight: TPanel;
    MagnifyerTimer: TTimer;
    MeasureTimer: TTimer;
    PlotTimer: TTimer;
    ImageResizeTimer: TTimer;
    SaveDialogPicture: TSaveDialog;
    Splitter1: TSplitter;
    TabMsgLevel: TTabControl;
    TimerStampTimer: TTimer;
    MenuPdfHelp: TMenuItem;
    MenuOnlineHelp: TMenuItem;
    MenuBugReport: TMenuItem;
    MenuShowCCDFrame: TMenuItem;
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
    MenuSequenceNew: TMenuItem;
    MenuSequenceEdit: TMenuItem;
    MenuSequenceStart: TMenuItem;
    MenuSequenceStop: TMenuItem;
    MenuSequenceLoad: TMenuItem;
    MenuPreviewLoop: TMenuItem;
    MenuPreviewStart: TMenuItem;
    MenuResolveSlewCenter: TMenuItem;
    MenuResolveSlewCenter2: TMenuItem;
    MenuResolve: TMenuItem;
    MenuResolve2: TMenuItem;
    MenuRefimage: TMenuItem;
    N7: TMenuItem;
    MenuViewScript: TMenuItem;
    MenuViewPlanetarium: TMenuItem;
    MenuResolveSlew: TMenuItem;
    MenuResolveSlew2: TMenuItem;
    MenuResolveSync: TMenuItem;
    MenuResolveSync2: TMenuItem;
    MenuViewSequence: TMenuItem;
    MenuViewAutoguider: TMenuItem;
    MenuViewAstrometryLog: TMenuItem;
    MenuStopAstrometry: TMenuItem;
    MenuResolvePlanetarium: TMenuItem;
    MenuResolvePlanetarium2: TMenuItem;
    MenuShowCCDFrame2: TMenuItem;
    MenuViewAstrometryLog2 :TMenuItem;
    MenuStopAstrometry2: TMenuItem;
    MenuItemDebayer2: TMenuItem;
    MenuItemCleanup2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuSave: TMenuItem;
    N6: TMenuItem;
    MenuViewFrame: TMenuItem;
    N5: TMenuItem;
    MenuOptions: TMenuItem;
    MenuViewCCDtemp: TMenuItem;
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
    SaveDialogFits: TSaveDialog;
    StatusBar1: TStatusBar;
    ConnectTimer: TTimer;
    StatusbarTimer: TTimer;
    PageConnect: TTabSheet;
    PageFocus: TTabSheet;
    PageCapture: TTabSheet;
    PageSequence: TTabSheet;
    AbortTimer: TTimer;
    StartupTimer: TTimer;
    StartSequenceTimer: TTimer;
    StatusTimer: TTimer;
    PageVideo: TTabSheet;
    TBTabs: TToolBar;
    TBConnect: TToolButton;
    TBFocus: TToolButton;
    TBCapture: TToolButton;
    TBSequence: TToolButton;
    TBVideo: TToolButton;
    procedure AbortTimerTimer(Sender: TObject);
    procedure CameraConnectTimerTimer(Sender: TObject);
    procedure ConnectTimerTimer(Sender: TObject);
    procedure FocuserConnectTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageResizeTimerTimer(Sender: TObject);
    procedure MagnifyerTimerTimer(Sender: TObject);
    procedure MeasureTimerTimer(Sender: TObject);
    procedure MenuApplyBPMClick(Sender: TObject);
    procedure MenuAscomSetupClick(Sender: TObject);
    procedure MenuAlpacaSetupClick(Sender: TObject);
    procedure MenuAutoguiderCalibrateClick(Sender: TObject);
    procedure MenuAutoguiderConnectClick(Sender: TObject);
    procedure MenuAutoguiderDitherClick(Sender: TObject);
    procedure MenuAutoguiderGuideClick(Sender: TObject);
    procedure MenuBPMClick(Sender: TObject);
    procedure MenuBPMDarkClick(Sender: TObject);
    procedure MenuBrowseLogClick(Sender: TObject);
    procedure MenuBugReportClick(Sender: TObject);
    procedure MenuCaptureStartClick(Sender: TObject);
    procedure MenuCCDtempSetClick(Sender: TObject);
    procedure MenuClearBPMClick(Sender: TObject);
    procedure MenuClearRefClick(Sender: TObject);
    procedure MenuCollimationClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuDarkApplyClick(Sender: TObject);
    procedure MenuDarkCameraClick(Sender: TObject);
    procedure MenuDarkClearClick(Sender: TObject);
    procedure MenuDarkFileClick(Sender: TObject);
    procedure MenuDownloadClick(Sender: TObject);
    procedure MenuFilterClick(Sender: TObject);
    procedure MenuFocusaidClick(Sender: TObject);
    procedure MenuFocuserCalibrationClick(Sender: TObject);
    procedure MenuFocuserInClick(Sender: TObject);
    procedure MenuFocuserOutClick(Sender: TObject);
    procedure MenuFrameResetClick(Sender: TObject);
    procedure MenuFrameSetClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure MenuImgStatClick(Sender: TObject);
    procedure MenuIndiSettingsClick(Sender: TObject);
    procedure MenuItemUnselectClick(Sender: TObject);
    procedure MenuPolarAlignmentClick(Sender: TObject);
    procedure MenuItemCleanupClick(Sender: TObject);
    procedure MenuItemPhotometryClick(Sender: TObject);
    procedure MenuResolveDSOClick(Sender: TObject);
    procedure MenuSaveConfigClick(Sender: TObject);
    procedure MenuOpenPictureClick(Sender: TObject);
    procedure MenuResolveRotateClick(Sender: TObject);
    procedure MenuResolveSyncRotatorClick(Sender: TObject);
    procedure MenuItemDebayerClick(Sender: TObject);
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
    procedure MenuSavePictureClick(Sender: TObject);
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
    procedure MenuShowINDIlogClick(Sender: TObject);
    procedure MenuShowLogClick(Sender: TObject);
    procedure MenuStatusClick(Sender: TObject);
    procedure MenuStopAstrometryClick(Sender: TObject);
    procedure MenuTabClick(Sender: TObject);
    procedure MenuUsergroupClick(Sender: TObject);
    procedure MenuVideoPreviewClick(Sender: TObject);
    procedure MenuVideoStartClick(Sender: TObject);
    procedure MenuVideoStopClick(Sender: TObject);
    procedure MenuViewAstrometryLogClick(Sender: TObject);
    procedure MenuViewAutoguiderClick(Sender: TObject);
    procedure MenuViewCCDtempClick(Sender: TObject);
    procedure MenuViewClockClick(Sender: TObject);
    procedure MenuViewConnectionClick(Sender: TObject);
    procedure MenuViewCoverClick(Sender: TObject);
    procedure MenuViewDomeClick(Sender: TObject);
    procedure MenuViewFiltersClick(Sender: TObject);
    procedure MenuViewFocuserClick(Sender: TObject);
    procedure MenuViewFrameClick(Sender: TObject);
    procedure MenuViewhdrClick(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuSetupClick(Sender: TObject);
    procedure MenuViewHistogramClick(Sender: TObject);
    procedure MenuViewMagnifyerClick(Sender: TObject);
    procedure MenuViewMessagesClick(Sender: TObject);
    procedure MenuViewMountClick(Sender: TObject);
    procedure MenuViewPlanetariumClick(Sender: TObject);
    procedure MenuViewPreviewClick(Sender: TObject);
    procedure MenuViewCaptureClick(Sender: TObject);
    procedure MenuViewRotatorClick(Sender: TObject);
    procedure MenuViewSafetyClick(Sender: TObject);
    procedure MenuViewScriptClick(Sender: TObject);
    procedure MenuViewSequenceClick(Sender: TObject);
    procedure MenuViewStarProfileClick(Sender: TObject);
    procedure MenuViewSwitchClick(Sender: TObject);
    procedure MenuViewWeatherClick(Sender: TObject);
    procedure MenuVisuZoom12Click(Sender: TObject);
    procedure MenuVisuZoom1Click(Sender: TObject);
    procedure MenuVisuZoom2Click(Sender: TObject);
    procedure MenuVisuZoomAdjustClick(Sender: TObject);
    procedure PanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PanelDragOver(Sender, Source: TObject; X, Y: Integer;State: TDragState; var Accept: Boolean);
    procedure PanelMsgTabsMouseEnter(Sender: TObject);
    procedure PanelMsgTabsMouseLeave(Sender: TObject);
    procedure PlotTimerTimer(Sender: TObject);
    procedure SelectTab(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure StartSequenceTimerTimer(Sender: TObject);
    procedure StartupTimerTimer(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBar1Resize(Sender: TObject);
    procedure StatusbarTimerTimer(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure ButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ButtonDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabMsgLevelChange(Sender: TObject);
    procedure TimerStampTimerTimer(Sender: TObject);
  private
    { private declarations }
    camera: T_camera;
    wheel: T_wheel;
    focuser: T_focuser;
    rotator: T_rotator;
    mount: T_mount;
    dome: T_dome;
    watchdog: T_watchdog;
    weather: T_weather;
    safety: T_safety;
    switch: T_switch;
    cover: T_cover;
    autoguider:T_autoguider;
    planetarium:TPlanetarium;
    astrometry:TAstrometry;
    WantCamera,WantWheel,WantFocuser,WantRotator, WantMount, WantDome, WantWeather, WantSafety, WantSwitch, WantCover, WantWatchdog: boolean;
    CameraInitialized: boolean;
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
    f_magnifyer: Tf_magnifyer;
    f_rotator: Tf_rotator;
    f_vcurve:Tf_vcurve;
    f_mount: Tf_mount;
    f_dome: Tf_dome;
    f_weather: Tf_weather;
    f_safety: Tf_safety;
    f_cover: Tf_cover;
    f_switch: Tf_switch;
    f_autoguider: Tf_autoguider;
    f_planetarium: Tf_planetarium;
    f_script: Tf_script;
    f_visu: Tf_visu;
    f_msg: Tf_msg;
    fits: TFits;
    ImaBmp: TBGRABitmap;
    TCPDaemon: TTCPDaemon;
    refmask: boolean;
    reftreshold,refcolor: integer;
    reffile: string;
    refbmp:TBGRABitmap;
    cdcWCSinfo: TcdcWCSinfo;
    WCSxyNrot,WCSxyErot,WCScenterRA,WCScenterDEC,WCSpoleX,WCSpoleY,WCSwidth,WCSheight: double;
    SaveFocusZoom,ImgCx, ImgCy: double;
    Mx, My, PolX, PolY: integer;
    StartX, StartY, EndX, EndY, MouseDownX,MouseDownY: integer;
    FrameX,FrameY,FrameW,FrameH: integer;
    DeviceTimeout: integer;
    MouseMoving, MouseFrame, LockTimerPlot, LockMouseWheel, LockRestartExposure, PolarMoving: boolean;
    Capture,Preview,learningvcurve: boolean;
    LogFileOpen,DeviceLogFileOpen: Boolean;
    NeedRestart, GUIready, AppClose: boolean;
    LogFile,DeviceLogFile : UTF8String;
    MsgLog,MsgDeviceLog: Textfile;
    AccelList: array[0..MaxMenulevel] of string;
    SaveAutofocusBinning: string;
    SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH,SaveAutofocusBX,SaveAutofocusBY: integer;
    SaveAutofocusGain, SaveAutofocusOffset, SaveAutofocusPreviewGain, SaveAutofocusPreviewOffset: integer;
    TerminateVcurve: boolean;
    ScrBmp: TBGRABitmap;
    Image1: TImgDrawingControl;
    ImageSaved: boolean;

    trpx1,trpx2,trpx3,trpx4,trpy1,trpy2,trpy3,trpy4: integer;{for image inspection}
    median_center,median_top_left, median_top_right,median_bottom_left,median_bottom_right : double;{for image inspection}

    LastPixelSize: double;
    trpOK: boolean;
    AllMsg: TStringList;
    CameraExposureRemain:double;
    CursorImage1: TCursorImage;
    crRetic: TCursor;
    procedure CreateDevices;
    procedure SetDevices;
    procedure DestroyDevices;
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Image1Paint(Sender: TObject);
    procedure Image1Resize(Sender: TObject);
    Procedure GetAppDir;
    procedure ScaleMainForm;
    Procedure InitLog;
    Procedure InitDeviceLog;
    Procedure CloseLog;
    Procedure WriteLog( buf : string);
    Procedure WriteDeviceLog( buf : string);
    Procedure PurgeOldLog;
    procedure SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; chkmenu,toolmenu: TMenuItem; DeviceSelected:boolean);
    procedure UpdConfig(oldver:string);
    procedure SetConfig;
    procedure SetOptions;
    procedure OpenConfig(n: string);
    procedure SaveScreenConfig;
    procedure SaveSettings;
    procedure SaveConfig;
    procedure ShowActiveTools;
    procedure SaveVcurve;
    procedure LoadVcurve;
    procedure CreateBPM(f: TFits);
    procedure LoadBPM;
    procedure ComputeVcSlope;
    procedure OptionGetMaxADU(Sender: TObject);
    procedure OptionGetPixelSize(Sender: TObject);
    procedure OptionGetFocaleLength(Sender: TObject);
    procedure Restart;
    procedure SetTheme;
    procedure SetRefImage;
    procedure GUIdestroy(Sender: TObject);
    Procedure Connect(Sender: TObject);
    Procedure Disconnect(Sender: TObject);
    Procedure ConnectDevice(num: double);
    Procedure DisconnectDevice(num: double);
    Procedure CheckConnectionStatus;
    Procedure ConnectCamera(Sender: TObject);
    Procedure DisconnectCamera(Sender: TObject);
    procedure SetCameraActiveDevices;
    procedure SetBinningList(posprev,poscapt: integer);
    procedure ShowBinningRange;
    procedure SetGainList;
    procedure ShowGain;
    procedure GainStatus(Sender: TObject);
    procedure ShowFnumber;
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
    procedure MountGoto(Sender: TObject);
    procedure SetFocusMode;
    Procedure ConnectWheel(Sender: TObject);
    Procedure DisconnectWheel(Sender: TObject);
    Procedure ConnectFocuser(Sender: TObject);
    Procedure DisconnectFocuser(Sender: TObject);
    Procedure ConnectRotator(Sender: TObject);
    Procedure DisconnectRotator(Sender: TObject);
    Procedure ConnectMount(Sender: TObject);
    Procedure DisconnectMount(Sender: TObject);
    Procedure ConnectWatchdog(Sender: TObject);
    Procedure DisconnectWatchdog(Sender: TObject);
    Procedure ConnectWeather(Sender: TObject);
    Procedure DisconnectWeather(Sender: TObject);
    Procedure ConnectSafety(Sender: TObject);
    Procedure DisconnectSafety(Sender: TObject);
    Procedure ConnectSwitch(Sender: TObject);
    Procedure DisconnectSwitch(Sender: TObject);
    Procedure ConnectCover(Sender: TObject);
    Procedure DisconnectCover(Sender: TObject);
    Procedure ConnectDome(Sender: TObject);
    Procedure DisconnectDome(Sender: TObject);
    Procedure SetFilter(Sender: TObject);
    Procedure SetFilterMenu;
    procedure ShowMsgTabs(Sender: TObject);
    Procedure NewMessage(msg: string; level: integer=1);
    Procedure DeviceMessage(msg: string; level: integer=1);
    Procedure WatchdogStatus(Sender: TObject);
    Procedure CameraStatus(Sender: TObject);
    Procedure CameraDisconnected(Sender: TObject);
    Procedure CameraExposureAborted(Sender: TObject);
    procedure CameraProgress(n:double);
    procedure CameraTemperatureChange(t:double);
    procedure CameraCoolerPowerChange(t:double);
    procedure CameraCoolerChange(var v:boolean);
    procedure CameraFnumberChange(f:string);
    Procedure WheelStatus(Sender: TObject);
    procedure FilterChange(n:double);
    procedure FilterNameChange(Sender: TObject);
    Procedure FocuserCalibration(Sender: TObject);
    Procedure FocuserCalibrationClose(Sender: TObject);
    Procedure FocusStart(Sender: TObject);
    Procedure FocusStop(Sender: TObject);
    Procedure AutoFocusStart(Sender: TObject);
    Procedure AutoFocusStop(Sender: TObject);
    Procedure DoAutoFocus;
    procedure LoadFocusStar(focusmag:integer);
    function  FindFocusStar(tra, tde:double; out sra,sde: double; out id: string): Boolean;
    function  AutoAutofocus(ReturnToTarget: boolean=true): Boolean;
    procedure cmdAutomaticAutofocus(var ok: boolean);
    procedure cmdAutofocus(var ok: boolean);
    Procedure FocuserStatus(Sender: TObject);
    function  FocuserTemperatureCompensation(canwait:boolean):boolean;
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
    function doVcurve(centerp,hw,n,nsum: integer;exp:double;bin,again,aoffset:integer):boolean;
    procedure MeasureImage(plot: boolean);
    procedure PrintStarList;
    procedure StarSelection(Sender: TObject);
    Procedure RotatorStatus(Sender: TObject);
    Procedure RotatorAngleChange(Sender: TObject);
    Procedure RotatorRotate(Sender: TObject);
    Procedure RotatorHalt(Sender: TObject);
    Procedure RotatorReverse(Sender: TObject);
    Procedure MountStatus(Sender: TObject);
    Procedure MountCoordChange(Sender: TObject);
    Procedure MountPiersideChange(Sender: TObject);
    Procedure MountParkChange(Sender: TObject);
    Procedure MountTrackingChange(Sender: TObject);
    Procedure DomeStatus(Sender: TObject);
    Procedure DomeShutterChange(Sender: TObject);
    Procedure DomeSlaveChange(Sender: TObject);
    procedure ParkDome(Sender: TObject);
    procedure StartDomeSlaving(Sender: TObject);
    Procedure WeatherStatus(Sender: TObject);
    Procedure WeatherClearChange(Sender: TObject);
    Procedure SafetyStatus(Sender: TObject);
    Procedure SafetySafeChange(Sender: TObject);
    Procedure SwitchStatus(Sender: TObject);
    Procedure SwitchChange(Sender: TObject);
    Procedure SetSwitch(Sender: TObject);
    Procedure CoverStatus(Sender: TObject);
    Procedure CoverChange(Sender: TObject);
    Procedure OpenCover(Sender: TObject);
    Procedure CloseCover(Sender: TObject);
    Procedure BrightnessChange(Sender: TObject);
    Procedure SetCalibratorLight(Sender: TObject);
    Procedure AutoguiderConnectClick(Sender: TObject);
    Procedure AutoguiderCalibrateClick(Sender: TObject);
    Procedure AutoguiderGuideClick(Sender: TObject);
    Procedure AutoguiderDitherClick(Sender: TObject);
    Procedure AutoguiderConnect(Sender: TObject);
    Procedure AutoguiderDisconnect(Sender: TObject);
    Procedure AutoguiderStatus(Sender: TObject);
    Procedure AutoguiderGetSigma(axis:integer; out sigma: double);
    Procedure AutoguiderGuideStat;
    Procedure AutoguiderGuideGraph(Sender: TObject);
    Procedure AutoguiderClearStat(Sender: TObject);
    Procedure PlanetariumConnectClick(Sender: TObject);
    Procedure PlanetariumConnect(Sender: TObject);
    Procedure PlanetariumDisconnect(Sender: TObject);
    Procedure PlanetariumNewTarget(Sender: TObject);
    procedure CameraNewImage(Sender: TObject);
    procedure CameraNewImageAsync(Data: PtrInt);
    procedure CameraNewExposure(Sender: TObject);
    procedure CameraSaveNewImage;
    procedure CameraMeasureNewImage;
    function  CameraNewSkyFlat: boolean;
    function  CameraNewDomeFlat: boolean;
    procedure CameraVideoFrame(Sender: TObject);
    procedure CameraVideoPreviewChange(Sender: TObject);
    procedure CameraVideoRecordChange(Sender: TObject);
    procedure CameraVideoSizeChange(Sender: TObject);
    procedure CameraVideoRateChange(Sender: TObject);
    procedure CameraVideoExposureChange(Sender: TObject);
    procedure CameraVideoEncoderChange(Sender: TObject);
    procedure CameraFPSChange(Sender: TObject);
    procedure ShowLastImage(Sender: TObject);
    procedure ResetPreviewStack(Sender: TObject);
    Procedure StopExposure(Sender: TObject);
    Procedure StartPreviewExposure(Sender: TObject);
    Procedure StartPreviewExposureAsync(Data: PtrInt);
    function  PrepareCaptureExposure(canwait:boolean):boolean;
    procedure CaptureDither;
    Procedure StartCaptureExposure(Sender: TObject);
    procedure StartCaptureExposureAsync(Data: PtrInt);
    Procedure StartCaptureExposureNow;
    procedure CancelRestartExposure(delay: integer);
    Procedure RecenterTarget;
    Procedure ShowHistogramPos(msg:string);
    Procedure Redraw(Sender: TObject);
    Procedure ZoomImage(Sender: TObject);
    Procedure ClearImage;
    Procedure DrawImage(WaitCursor:boolean=false);
    Procedure PlotImage;
    procedure plot_north(bmp:TBGRABitmap);
    Procedure DrawHistogram(SetLevel,ResetCursor: boolean);
    procedure AstrometryStart(Sender: TObject);
    procedure AstrometryEnd(Sender: TObject);
    procedure EndControlExposure(Sender: TObject);
    procedure AstrometryPlotDSO(Sender: TObject);
    procedure AstrometryPlotHyperleda(Sender: TObject);
    procedure AstrometryToPlanetarium(Sender: TObject);
    procedure AstrometryToPlanetariumFrame(Sender: TObject);
    procedure ResolveSlewCenter(Sender: TObject);
    procedure ResolveSyncRotator(Sender: TObject);
    procedure ResolveRotate(Sender: TObject);
    procedure LoadPictureFile(fn:string);
    procedure LoadRawFile(fn:string);
    procedure LoadFitsFile(fn:string);
    procedure SaveFitsFile(fn:string);
    procedure OpenRefImage(fn:string);
    procedure ClearRefImage(Sender: TObject);
    procedure CCDCIELMessageHandler(var Message: TLMessage); message LM_CCDCIEL;
    Procedure StartSequence(SeqName: string);
    procedure ScriptExecute(Sender: TObject);
    procedure ScriptAfterExecute(Sender: TObject);
    function CheckMeridianFlip(nextexposure:double; canwait:boolean; out waittime:integer):boolean;
    procedure CheckMeridianFlip; overload;
    procedure StartServer;
    procedure StopServer;
    procedure RestartServer;
    procedure TCPShowError(var msg: string);
    procedure TCPShowSocket(var msg: string);
    function TCPcmd(s: string):string;
    function TCPjsoncmd(id:string; attrib,value:Tstringlist):string;
    function jsoncmd_status(attrib,value:Tstringlist):string;
    procedure TCPgetimage(n: string;  var img: Tmemorystream);
    procedure SetLang;
    procedure UpdateMagnifyer(x,y:integer);
    procedure MagnitudeCalibrationChange(Sender: TObject);
    procedure MeasureAtPos(x,y:integer; photometry:boolean);
    procedure ShutdownProgram(Sender: TObject);
    function  CheckImageInfo:boolean;
    procedure PolaralignClose(Sender: TObject);
    procedure PhotometryClose(Sender: TObject);
    procedure CollimationStart(Sender: TObject);
    procedure CollimationStop(Sender: TObject);
    procedure CollimationCenterStar(Sender: TObject);
    procedure CollimationCircleChange(Sender: TObject);
    procedure ReadyForVideo(var v: boolean);
    procedure ShowStatus(str: string);
  public
    { public declarations }
  end;

var
  f_main: Tf_main;

implementation

uses
{$if lcl_major > 1}
LazSysUtils;
{$else}
LazUTF8SysUtils;
{$endif}

{$R *.lfm}

const panelcursor=0; panelstatus=1; panelfile=2; panelclock=3; panelled=4;

{ Tf_main }

Procedure Tf_main.InitLog;
begin
  try
     LogFile:=slash(LogDir)+'Log_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.log';
     Filemode:=2;
     AssignFile(MsgLog,LogFile);
     Rewrite(MsgLog);
     WriteLn(MsgLog,FormatDateTime(dateiso,Now)+'  CCDciel '+ccdciel_version+'-'+RevisionStr+blank+compile_time);
     WriteLn(MsgLog, FormatDateTime(dateiso, Now)+blank+blank+rsCompiledWith+': '+compile_version);
     LogFileOpen:=true;
     PurgeOldLog;
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
     WriteLn(MsgDeviceLog, FormatDateTime(dateiso, Now)+blank+blank+rsStartNewLog);
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
     if buf>'' then WriteLn(MsgLog,FormatDateTime(dateiso,Now)+blank+UTF8ToSys(buf));
     Flush(MsgLog);
    end;
  except
    {$I-}
    on E: Exception do begin
      LogFileOpen:=false;
      LogToFile:=false;
      NewMessage('Error writing log file: '+ E.Message,1);
      CloseFile(MsgLog);
    end;
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
     if buf>'' then WriteLn(MsgDeviceLog,FormatDateTime(dateiso,Now)+'  '+UTF8ToSys(buf));
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

Procedure Tf_main.PurgeOldLog;
var fs : TSearchRec;
    i: integer;
    tl: Longint;
    buf: string;
begin
 // purge file older than 30 days
 tl:=DateTimeToFileDate(now-30);
 // purge standard log
 i:=FindFirstUTF8(slash(LogDir)+'Log_*',0,fs);
 while i=0 do begin
   if (fs.Time>0)and(fs.Time<tl) then begin
     buf:=slash(LogDir)+fs.Name;
     DeleteFileUTF8(buf);
   end;
   i:=FindNextUTF8(fs);
 end;
 FindCloseUTF8(fs);
 // purge indi log
 i:=FindFirstUTF8(slash(LogDir)+'Devices_Log_*',0,fs);
 while i=0 do begin
   if (fs.Time>0)and(fs.Time<tl) then begin
     buf:=slash(LogDir)+fs.Name;
     DeleteFileUTF8(buf);
   end;
   i:=FindNextUTF8(fs);
 end;
 FindCloseUTF8(fs);
 // purge focus error pictures
 i:=FindFirstUTF8(slash(LogDir)+'focus_fail_*',0,fs);
 while i=0 do begin
   if (fs.Time>0)and(fs.Time<tl) then begin
     buf:=slash(LogDir)+fs.Name;
     DeleteFileUTF8(buf);
   end;
   i:=FindNextUTF8(fs);
 end;
 FindCloseUTF8(fs);
 // purge astrometry error pictures
 i:=FindFirstUTF8(slash(LogDir)+'astrometry_fail_*',0,fs);
 while i=0 do begin
   if (fs.Time>0)and(fs.Time<tl) then begin
     buf:=slash(LogDir)+fs.Name;
     DeleteFileUTF8(buf);
   end;
   i:=FindNextUTF8(fs);
 end;
 FindCloseUTF8(fs);
end;

procedure Tf_main.Restart;
begin
  ShowMessage('The program will restart now...');
  NeedRestart:=true;
  Close;
end;

procedure Tf_main.SetTool(tool:TFrame; configname: string; defaultParent: TPanel; defaultpos: integer; chkmenu,toolmenu: TMenuItem; DeviceSelected:boolean);
var pn: string;
    i: integer;
    par: Tpanel;
    opm,npm: Tmenuitem;
begin
pn:=screenconfig.GetValue('/Tools/'+configname+'/Parent',defaultParent.Name);
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
tool.Top:=screenconfig.GetValue('/Tools/'+widestring(configname)+'/Top',defaultpos);
tool.Left:=screenconfig.GetValue('/Tools/'+widestring(configname)+'/Left',defaultpos);
tool.Parent:=par;
tool.Visible:=DeviceSelected and screenconfig.GetValue('/Tools/'+widestring(configname)+'/Visible',true);
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
    {$ifdef mswindows}
    PIDL: LPITEMIDLIST;
    Folder: array[0..MAX_PATH] of char;
    {$endif}
begin
 {$ifdef darwin}
   //  try current path
   Appdir := getcurrentdir;
   if not DirectoryExists(slash(Appdir)+slash('scripts')) then
   begin
     // try under app bundle
     Appdir := ExtractFilePath(ParamStr(0));
     i := pos('.app/', Appdir);
     if i > 0 then
     begin
       Appdir := ExtractFilePath(copy(Appdir, 1, i));
     end;
     if not DirectoryExists(slash(Appdir)+slash('scripts')) then
     begin
        // try default location
       Appdir := '/Applications/CCDciel';
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
              // try in C:\Program Files
              buf:='C:\Program Files\CCDciel';
              if (directoryexists(slash(buf)+slash('scripts'))) then
                 appdir:=buf
          else begin
             // try in C:\Program Files (x86)
             buf:='C:\Program Files (x86)\CCDciel';
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
   end;
 end;
 DataDir:=slash(Appdir)+slash('data');
 ScriptsDir:=slash(Appdir)+slash('scripts');
 ConfigDir:=GetAppConfigDirUTF8(false,true);
 if Application.HasOption('b', 'basedir') then begin
   buf:=Application.GetOptionValue('b', 'basedir');
   if buf<>'' then ConfigDir:=ExpandFileNameUTF8(buf);
 end;
 TmpDir:=slash(ConfigDir)+'tmp';
 if not DirectoryExistsUTF8(TmpDir) then  CreateDirUTF8(TmpDir);
 LogDir:=slash(ConfigDir)+'Log';
 if not DirectoryExistsUTF8(LogDir) then  CreateDirUTF8(LogDir);
 {$ifdef unix}
   HomeDir := expandfilename('~/');
   defCapturePath:=HomeDir;
   defPython:='python3';
 {$endif}
 {$ifdef mswindows}
   SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
   SHGetPathFromIDList(PIDL, Folder);
   HomeDir := trim(WinCPToUTF8(Folder));
   defCapturePath:=HomeDir+'\Documents';
   defPython:=slash(ScriptsDir)+slash('python')+'python.exe';
 {$endif}
end;

procedure Tf_main.ScaleMainForm;
var rl: integer;
begin
  ScreenScaling:=true;
  UScaleDPI.UseScaling:=ScreenScaling;
  UScaleDPI.SetScale(Canvas);
  ScaleDPI(Self);
  ScaleImageList(ImageListDay);
  ScaleImageList(ImageListNight);
  // resize empty drop area
  if Screen.Height<1024 then
    rl:=DoScaleX(3)
  else
    rl:=DoScaleX(5);
  PanelTop.Constraints.MinHeight:=rl;
  PanelBottom.Constraints.MinHeight:=rl;
  PanelLeft.Constraints.MinWidth:=rl;
end;

Procedure UppercaseFilter(opendialog:TOpenDialog);
var i:integer;
    filst: TStringList;
    buf,newfilter: string;
begin
    filst:=TStringList.Create;
    SplitRec(opendialog.Filter,'|',filst);
    newfilter:='';
    for i:=0 to filst.Count-1 do begin
      if odd(i) and (filst[i]<>'*.*') then begin
        buf:=uppercase(filst[i]);
        newfilter:=newfilter+'|'+filst[i]+';'+buf;
      end else begin
        newfilter:=newfilter+'|'+filst[i]
      end;
    end;
    delete(newfilter,1,1);
    opendialog.Filter:=newfilter;
    filst.Free;
end;

procedure Tf_main.FormCreate(Sender: TObject);
var inif: TIniFile;
    configfile: string;
    i:integer;
begin
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.TimeSeparator:=':';
  lclver:=lcl_version;
  compile_time:={$I %DATE%}+' '+{$I %TIME%};
  compile_version:='Lazarus '+lcl_version+' Free Pascal '+{$I %FPCVERSION%}+' '+{$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%}+'-'+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  compile_system:={$I %FPCTARGETOS%}+'-'+{$I %FPCTARGETCPU%};
  cdate:={$I %DATE%};
  cdate:=copy(cdate,1,4);
  isAdmin := False;
  debug_msg := false;
  {$ifdef mswindows}
  Application.{%H-}UpdateFormatSettings := False;
  isAdmin := IsUserAnAdmin;
  {$endif}
  {$ifdef unix}
  isAdmin := (FpGetuid=0);
  {$endif}
  {$ifdef lclgtk2}
    TBTabs.Color:=clBtnShadow;
    // GTK2 open dialog is case sensitive
    UppercaseFilter(OpenDialog1);
    UppercaseFilter(OpenPictureDialog1);
  {$endif}
  {$ifdef darwin}
    TBTabs.Color:=clBtnHighlight; // on Mac highlight is darker...
  {$endif}
  if isAdmin then begin
     {$ifdef mswindows}
     Caption := Format(rsDoNotRunAsAd, [Caption, rsAdministrato])
     {$else}
     Caption := Format(rsDoNotRunAsAd, [Caption, 'root'])
     {$endif}
  end;
  PageControlRight.ActivePageIndex:=0;
  AppClose:=false;
  ConfirmClose:=true;
  ScaleMainForm;
  NeedRestart:=false;
  AllDevicesConnected:=false;
  GUIready:=false;
  filteroffset_initialized:=false;
  MsgHandle:=handle;
  meridianflipping:=false;
  TemperatureScale:=0;
  TempLabel:=sdeg+'C';
  TemperatureSlope:=0;
  learningvcurve:=false;
  autofocusing:=false;
  CameraProcessingImage:=false;
  CameraProcessingNum:=0;
  CancelAutofocus:=false;
  InplaceAutofocus:=false;
  AutofocusExposureFact:=1;
  NeedRecenterTarget:=false;
  CheckRecenterBusy:=false;
  CheckRecenterTarget:=false;
  RecenteringTarget:=false;
  SlewPrecision:=5;
  RecenterTargetDistance:=10;
  FocuserLastTemp:=NullCoord;
  AutoFocusLastTime:=NullCoord;
  FocusStarMag:=-1;
  PauseSequence:=false;
  WaitTillrunning:=false;
  cancelWaitTill:=false;
  FlatWaitDusk:=false;
  FlatWaitDawn:=false;
  FlatSlewTime:=0;
  AdjustDomeFlat:=false;
  AdjustFlatLight:=false;
  onMsgGlobal:=@NewMessage;
  PolarAlignmentOverlay:=false;
  ImgPixRatio:=1;
  Undersampled:=false;
  ZoomMin:=1;
  LogLevel:=2;
  LogToFile:=false;
  AllMsg:=TStringList.Create;
  AllMsg.OwnsObjects:=true;
  refmask:=false;
  reftreshold:=128;
  refbmp:=TBGRABitmap.Create;
  FilterList:=TStringList.Create;
  BinningList:=TStringList.Create;
  ReadoutList:=TStringList.Create;
  ISOList:=TStringList.Create;
  CurrentFilterOffset:=0;
  ReadoutModeCapture:=0;
  ReadoutModePreview:=0;
  ReadoutModeFocus:=0;
  ReadoutModeAstrometry:=0;
  DomeNoSafetyCheck:=false;
  EarlyNextExposure:=false;
  DisplayCapture:=true;
  LowQualityDisplay:={$ifdef cpuarm}true{$else}false{$endif};
  ConfigExpEarlyStart:=true;
  EarlyDither:=true;
  CameraProcessingImage:=false;
  WantExif:=true;
  MagnitudeCalibration:=NullCoord;
  Collimation:=false;
  CollimationCircle:=4;
  ManualFilterNames:=TStringList.Create;
  ScrBmp := TBGRABitmap.Create;
  Image1 := TImgDrawingControl.Create(Self);
  Image1.Parent := PanelCenter;
  Image1.Align := alClient;
  image1.OnDblClick := @Image1DblClick;
  Image1.OnMouseDown := @Image1MouseDown;
  Image1.OnMouseMove := @Image1MouseMove;
  Image1.OnMouseUp := @Image1MouseUp;
  Image1.OnMouseWheel := @Image1MouseWheel;
  Image1.OnResize := @Image1Resize;
  Image1.OnPaint := @Image1Paint;
  Image1.PopupMenu := ImagePopupMenu;
  GetAppDir;
  chdir(Appdir);
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
  WCScenterRA:=NullCoord;
  WCScenterDEC:=NullCoord;
  WCSpoleX:=NullCoord;
  WCSpoleY:=NullCoord;
  WCSwidth:=NullCoord;
  WCSheight:=NullCoord;
  zlibok:=false;
  uncompress:=nil;
  zlib:=LoadLibrary(libz);
  if zlib<>0 then begin
    uncompress:= Tuncompress(GetProcedureAddress(zlib,'uncompress'));
    if uncompress<>nil then zlibok:=true;
  end;
  Load_Libraw;
  ConfigExtension:= '.conf';
  config:=TCCDConfig.Create(self);
  screenconfig:=TCCDConfig.Create(self);
  credentialconfig:=TCCDConfig.Create(self);
  emailconfig:=TCCDConfig.Create(self);
  bpmconfig:=TCCDconfig.Create(self);
  ProfileFromCommandLine:=false;
  if Application.HasOption('c', 'config') then begin
    profile:=Application.GetOptionValue('c', 'config');
    ProfileFromCommandLine:=true;
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
  ConfigDarkFile:=slash(ConfigDir)+'darkframe_'+profile+'.fits';

  LogFileOpen:=false;
  for i:=1 to MaxScriptDir do ScriptDir[i]:=TScriptDir.Create;
  ScriptDir[1].path:=slash(ConfigDir);
  ScriptDir[2].path:=slash(ScriptsDir);

  lang:=config.GetValue('/Language','');;
  lang:=u_translation.translate(lang);
  SetLang;

  Top:=screenconfig.GetValue('/Window/Top',0);
  Left:=screenconfig.GetValue('/Window/Left',0);
  if Screen.Width<1400 then begin
    // up to 1366 x 768
    ScreenMargin:=80;
    Width:=screenconfig.GetValue('/Window/Width',Screen.Width);
    Height:=screenconfig.GetValue('/Window/Height',Screen.Height);
  end else begin
    // from 1400 x 1050 or 1440 x 900
    ScreenMargin:=120;
    Width:=screenconfig.GetValue('/Window/Width',1280);
    Height:=screenconfig.GetValue('/Window/Height',800);
  end;
  if width>(Screen.width-ScreenMargin) then width:=Screen.width-ScreenMargin;
  if left<ScreenMargin then left:=ScreenMargin;
  if left+width>(Screen.Width-ScreenMargin) then left:=Screen.Width-width-ScreenMargin;
  if left<0 then left:=0;
  if top<ScreenMargin then top:=ScreenMargin;
  if height>(Screen.height-ScreenMargin) then height:=Screen.height-ScreenMargin;
  if top+height>(Screen.height-ScreenMargin) then top:=Screen.height-height-ScreenMargin;
  if top<0 then top:=0;

  if screenconfig.GetValue('/Window/Maximized',false) then WindowState := wsMaximized;

  PanelRight.Width:=screenconfig.GetValue('/Window/PanelRight',PanelRight.Width);

  f_msg:=Tf_msg.Create(self);
  f_msg.onShowTabs:=@ShowMsgTabs;
  f_msg.onOpenLog:=@MenuShowLogClick;

  fits:=TFits.Create(self);
  fits.onMsg:=@NewMessage;
  if FileExistsUTF8(ConfigDarkFile) then begin
     fits.DarkFrame:=TFits.Create(nil);
     fits.DarkFrame.DarkOn:=false;
     fits.DarkFrame.onMsg:=@NewMessage;
     fits.DarkFrame.LoadFromFile(ConfigDarkFile);
  end;

  CreateDevices;

  astrometry:=TAstrometry.Create(nil);
  astrometry.Fits:=fits;
  astrometry.onAstrometryStart:=@AstrometryStart;
  astrometry.onAstrometryEnd:=@AstrometryEnd;
  astrometry.onShowMessage:=@NewMessage;

  i:=config.GetValue('/Autoguider/Software',2);
  case TAutoguiderType(i) of
    agPHD: autoguider:=T_autoguider_phd.Create;
    agLINGUIDER: autoguider:=T_autoguider_linguider.Create;
    agNONE: autoguider:=T_autoguider_none.Create;
    agDITHER: autoguider:=T_autoguider_dither.Create;
  end;
  autoguider.onStatusChange:=@AutoguiderStatus;
  autoguider.onConnect:=@AutoguiderConnect;
  autoguider.onDisconnect:=@AutoguiderDisconnect;
  autoguider.onShowMessage:=@NewMessage;

  i:=config.GetValue('/Planetarium/Software',ord(plaNONE));
  case TPlanetariumType(i) of
    CDC: planetarium:=TPlanetarium_cdc.Create;
    SAMP:planetarium:=TPlanetarium_samp.Create;
    HNSKY:planetarium:=TPlanetarium_hnsky.Create;
    plaNONE: planetarium:=TPlanetarium_none.Create;
  end;
  planetarium.onConnect:=@PlanetariumConnect;
  planetarium.onDisconnect:=@PlanetariumDisconnect;
  planetarium.onShowMessage:=@NewMessage;

  f_devicesconnection:=Tf_devicesconnection.Create(self);
  f_devicesconnection.onSelectProfile:=@MenuSetupClick;
  f_devicesconnection.onConnect:=@Connect;
  f_devicesconnection.onDisconnect:=@Disconnect;
  f_devicesconnection.onConnectDevice:=@ConnectDevice;
  f_devicesconnection.onDisconnectDevice:=@DisconnectDevice;
  f_devicesconnection.ProfileLabel.Caption:=profile;
  caption:='CCDciel '+ccdcielver+blank+profile;

  f_visu:=Tf_visu.Create(self);
  f_visu.onRedraw:=@Redraw;
  f_visu.onZoom:=@ZoomImage;
  f_visu.onShowHistogramPos:=@ShowHistogramPos;
  f_visu.onShowLastImage:=@ShowLastImage;

  f_frame:=Tf_frame.Create(self);
  f_frame.onSet:=@SetFrame;
  f_frame.onReset:=@ResetFrame;

  f_preview:=Tf_preview.Create(self);
  f_preview.onResetStack:=@ResetPreviewStack;
  f_preview.onStartExposure:=@StartPreviewExposure;
  f_preview.onAbortExposure:=@StopExposure;
  f_preview.onMsg:=@NewMessage;
  f_preview.onEndControlExposure:=@EndControlExposure;
  astrometry.preview:=f_preview;
  astrometry.visu:=f_visu;

  f_capture:=Tf_capture.Create(self);
  f_capture.onResetStack:=@ResetPreviewStack;
  f_capture.onStartExposure:=@StartCaptureExposure;
  f_capture.onAbortExposure:=@StopExposure;
  f_capture.onMsg:=@NewMessage;

  f_video:=Tf_video.Create(self);
  f_video.onMsg:=@NewMessage;
  f_video.onCheckReady:=@ReadyForVideo;

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
  f_starprofile.onMeasureImage:=@MeasureImage;
  f_starprofile.onStarSelection:=@StarSelection;
  f_starprofile.onStatus:=@ShowStatus;

  f_magnifyer:=Tf_magnifyer.Create(self);

  f_ccdtemp:=Tf_ccdtemp.Create(self);
  f_ccdtemp.onSetTemperature:=@SetTemperature;
  f_ccdtemp.onSetCooler:=@SetCooler;

  f_mount:=Tf_mount.Create(self);
  f_mount.onPark:=@SetMountPark;
  f_mount.onTrack:=@SetMountTrack;
  f_mount.onGoto:=@MountGoto;

  f_dome:=Tf_dome.Create(self);
  f_dome.onParkDome:=@ParkDome;
  f_dome.onStartSlaving:=@StartDomeSlaving;

  f_rotator:=Tf_rotator.Create(self);
  f_rotator.onRotate:=@RotatorRotate;
  f_rotator.onHalt:=@RotatorHalt;
  f_rotator.onReverse:=@RotatorReverse;

  f_weather:=Tf_weather.Create(self);

  f_safety:=Tf_safety.Create(self);
  if mount<>nil then mount.Safety:=f_safety;
  if dome<>nil then dome.Safety:=f_safety;

  f_cover:=Tf_cover.Create(self);
  f_cover.onOpenCover:=@OpenCover;
  f_cover.onCloseCover:=@CloseCover;
  f_cover.onSetLight:=@SetCalibratorLight;
  f_cover.onChangeBrightness:=@BrightnessChange;

  f_switch:=Tf_switch.Create(self);
  f_switch.onSetSwitch:=@SetSwitch;

  f_autoguider:=Tf_autoguider.Create(self);
  f_autoguider.onConnect:=@AutoguiderConnectClick;
  f_autoguider.onCalibrate:=@AutoguiderCalibrateClick;
  f_autoguider.onGuide:=@AutoguiderGuideClick;
  f_autoguider.onDither:=@AutoguiderDitherClick;
  f_autoguider.onClearStat:=@AutoguiderClearStat;
  f_autoguider.onShowStat:=@AutoguiderGuideGraph;
  f_autoguider.Status.Text:=autoguider.Status;
  f_autoguider.DitherOnly:=autoguider.AutoguiderType=agDITHER;

  f_sequence:=Tf_sequence.Create(self);
  f_sequence.onMsg:=@NewMessage;
  f_sequence.Preview:=f_preview;
  f_sequence.Capture:=f_capture;
  f_sequence.Filter:=f_filterwheel;
  f_sequence.Weather:=f_weather;
  f_sequence.Safety:=f_safety;
  f_sequence.Autoguider:=autoguider;
  f_sequence.Astrometry:=astrometry;
  f_sequence.Planetarium:=planetarium;
  f_sequence.onConnectAutoguider:=@AutoguiderConnectClick;

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
  f_scriptengine.Autoguider:=autoguider;
  f_scriptengine.Astrometry:=astrometry;
  f_scriptengine.Planetarium:=planetarium;

  f_script:=Tf_script.Create(self);
  f_script.onMsg:=@NewMessage;
  f_script.Preview:=f_preview;
  f_script.Capture:=f_capture;
  f_script.Autoguider:=autoguider;
  f_script.Astrometry:=astrometry;
  f_script.LoadScriptList;
end;

procedure Tf_main.CreateDevices;
var DefaultInterface,aInt: TDevInterface;
begin
   {$ifdef mswindows}
   DefaultInterface:=ASCOM;
   {$else}
   DefaultInterface:=INDI;
   {$endif}
   aInt:=TDevInterface(config.GetValue('/FilterWheelInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  wheel:=T_indiwheel.Create(nil);
     ASCOM: wheel:=T_ascomwheel.Create(nil);
     INCAMERA: wheel:=T_incamerawheel.Create(nil);
     ASCOMREST: wheel:=T_ascomrestwheel.Create(nil);
     MANUAL: wheel:=T_manualwheel.Create(nil);
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
     ASCOMREST: focuser:=T_ascomrestfocuser.Create(nil);
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
     ASCOMREST: rotator:=T_ascomrestrotator.Create(nil);
   end;
   rotator.onMsg:=@NewMessage;
   rotator.onDeviceMsg:=@DeviceMessage;
   rotator.onAngleChange:=@RotatorAngleChange;
   rotator.onStatusChange:=@RotatorStatus;

   aInt:=TDevInterface(config.GetValue('/WeatherInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  weather:=T_indiweather.Create(nil);
     ASCOM: weather:=T_ascomweather.Create(nil);
     ASCOMREST: weather:=T_ascomrestweather.Create(nil);
   end;
   weather.onMsg:=@NewMessage;
   weather.onDeviceMsg:=@DeviceMessage;
   weather.onStatusChange:=@WeatherStatus;
   weather.onClearChange:=@WeatherClearChange;

   focuser.weather:=weather;

   aInt:=TDevInterface(config.GetValue('/SafetyInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  safety:=T_indisafety.Create(nil);
     ASCOM: safety:=T_ascomsafety.Create(nil);
     ASCOMREST: safety:=T_ascomrestsafety.Create(nil);
   end;
   safety.onMsg:=@NewMessage;
   safety.onDeviceMsg:=@DeviceMessage;
   safety.onStatusChange:=@SafetyStatus;
   safety.onSafeChange:=@SafetySafeChange;

   aInt:=TDevInterface(config.GetValue('/SwitchInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  switch:=T_indiswitch.Create(nil);
     ASCOM: switch:=T_ascomswitch.Create(nil);
     ASCOMREST: switch:=T_ascomrestswitch.Create(nil);
   end;
   switch.onMsg:=@NewMessage;
   switch.onDeviceMsg:=@DeviceMessage;
   switch.onStatusChange:=@SwitchStatus;
   switch.onSwitchChange:=@SwitchChange;

   aInt:=TDevInterface(config.GetValue('/CoverInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  cover:=T_indicover.Create(nil);
     ASCOM: cover:=T_ascomcover.Create(nil);
     ASCOMREST: cover:=T_ascomrestcover.Create(nil);
   end;
   cover.onMsg:=@NewMessage;
   cover.onDeviceMsg:=@DeviceMessage;
   cover.onStatusChange:=@CoverStatus;
   cover.onCoverChange:=@CoverChange;

   aInt:=TDevInterface(config.GetValue('/MountInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  mount:=T_indimount.Create(nil);
     ASCOM: mount:=T_ascommount.Create(nil);
     ASCOMREST: mount:=T_ascomrestmount.Create(nil);
   end;
   mount.onMsg:=@NewMessage;
   mount.onDeviceMsg:=@DeviceMessage;
   mount.onCoordChange:=@MountCoordChange;
   mount.onPiersideChange:=@MountPiersideChange;
   mount.onParkChange:=@MountParkChange;
   mount.onTrackingChange:=@MountTrackingChange;
   mount.onStatusChange:=@MountStatus;
   mount.Safety:=f_safety;

   aInt:=TDevInterface(config.GetValue('/DomeInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  dome:=T_indidome.Create(nil);
     ASCOM: dome:=T_ascomdome.Create(nil);
     ASCOMREST: dome:=T_ascomrestdome.Create(nil);
   end;
   dome.onMsg:=@NewMessage;
   dome.onDeviceMsg:=@DeviceMessage;
   dome.onStatusChange:=@DomeStatus;
   dome.onShutterChange:=@DomeShutterChange;
   dome.onSlaveChange:=@DomeSlaveChange;
   dome.Safety:=f_safety;

   mount.Dome:=dome;

   aInt:=TDevInterface(config.GetValue('/CameraInterface',ord(DefaultInterface)));
   case aInt of
     INDI:  camera:=T_indicamera.Create(nil);
     ASCOM: camera:=T_ascomcamera.Create(nil);
     ASCOMREST: camera:=T_ascomrestcamera.Create(nil);
   end;
   if wheel.WheelInterface=INCAMERA then wheel.camera:=camera;
   camera.Mount:=mount;
   camera.Wheel:=wheel;
   camera.Focuser:=focuser;
   camera.Fits:=fits;
   camera.onMsg:=@NewMessage;
   camera.onDeviceMsg:=@DeviceMessage;
   camera.onExposureProgress:=@CameraProgress;
   camera.onFrameChange:=@FrameChange;
   camera.onTemperatureChange:=@CameraTemperatureChange;
   camera.onCoolerPowerChange:=@CameraCoolerPowerChange;
   camera.onCoolerChange:=@CameraCoolerChange;
   camera.onFnumberChange:=@CameraFnumberChange;
   camera.onNewImage:=@CameraNewImage;
   camera.onNewExposure:=@CameraNewExposure;
   camera.onVideoFrame:=@CameraVideoFrame;
   camera.onVideoPreviewChange:=@CameraVideoPreviewChange;
   camera.onVideoRecordChange:=@CameraVideoRecordChange;
   camera.onVideoSizeChange:=@CameraVideoSizeChange;
   camera.onVideoRateChange:=@CameraVideoRateChange;
   camera.onFPSChange:=@CameraFPSChange;
   camera.onVideoExposureChange:=@CameraVideoExposureChange;
   camera.onEncoderChange:=@CameraVideoEncoderChange;
   camera.onStatusChange:=@CameraStatus;
   camera.onCameraDisconnected:=@CameraDisconnected;
   camera.onAbortExposure:=@CameraExposureAborted;
   camera.onGainStatus:=@GainStatus;

   if config.GetValue('/Devices/Watchdog',false) then begin
     watchdog:=T_indiwatchdog.Create(nil);
     watchdog.onMsg:=@NewMessage;
     watchdog.onDeviceMsg:=@DeviceMessage;
     watchdog.onStatusChange:=@WatchdogStatus;
   end
   else
     watchdog:=nil;

   SetDevices;
end;

procedure Tf_main.SetDevices;
begin
 if astrometry<>nil then begin
   astrometry.Camera:=camera;
   astrometry.Mount:=mount;
   astrometry.Wheel:=wheel;
 end;
 if f_preview<>nil then begin
   f_preview.Camera:=camera;
 end;
 if f_capture<>nil then begin
   f_capture.Mount:=mount;
 end;
 if f_video<>nil then begin
   f_video.camera:=camera;
   f_video.wheel:=wheel;
 end;
 if f_sequence<>nil then begin
   f_sequence.Dome:=dome;
   f_sequence.Mount:=mount;
   f_sequence.Camera:=camera;
   f_sequence.Rotator:=rotator;
 end;
 if f_scriptengine<>nil then begin
   f_scriptengine.Filter:=wheel;
   f_scriptengine.Mount:=mount;
   f_scriptengine.Camera:=camera;
   f_scriptengine.Focuser:=focuser;
 end;
 if f_script<>nil then begin
   f_script.Camera:=camera;
   f_script.Mount:=mount;
 end;
 if autoguider<>nil then begin
   autoguider.Mount:=mount;
 end;
end;

procedure Tf_main.DestroyDevices;
begin
 try
 camera.Free;
 wheel.Free;
 focuser.Free;
 rotator.Free;
 mount.Free;
 dome.Free;
 watchdog.Free;
 weather.Free;
 safety.Free;
 switch.Free;
 cover.Free;
 except
 end;
end;

procedure Tf_main.SetLang;
begin
   MenuItem1.Caption := rsFile;
   MenuSetup.Caption:=Format(rsDevicesSetup, [ellipsis]);
   MenuItemBPM.Caption := rsBadPixelMap;
   MenuBPM.Caption := rsCreateFromCa;
   MenuBPMDark.Caption:=rsCreateFromDa;
   MenuClearBPM.Caption := rsClearBadPixe;
   MenuApplyBPM.Caption := rsApplyToCurre;
   MenuItemDark.Caption:=rsDarkFrame;
   MenuDarkApply.Caption:=rsApplyToCurre;
   MenuDarkCamera.Caption:=rsCreateFromCa;
   MenuDarkFile.Caption:=rsLoadDarkFile;
   MenuDarkClear.Caption:=rsClearDarkFra;
   MenuFocuserCalibration.Caption := rsFocuserCalib;
   MenuOpenPicture.Caption := Format(rsOpenPictureF, [ellipsis]);
   MenuSave.Caption := Format(rsSaveFITSFile, [ellipsis]);
   MenuSavePicture.Caption := Format(rsSavePictureS, [ellipsis]);
   MenuRefimage.Caption := rsOpenReferenc;
   MenuClearRef.Caption := rsClearReferen;
   MenuSaveConfig.Caption := rsSaveConfigur;
   MenuQuit.Caption := rsQuit;
   MenuItem2.Caption := rsEdit;
   MenuOptions.Caption := Format(rsPreferences, [ellipsis]);
   MenuPolarAlignment.Caption:=rsPolarAlignme;
   MenuIndiSettings.Caption := rsINDISettings;
   MenuAscomCameraSetup.Caption:='ASCOM '+rsCamera+blank+rsSetup;
   MenuAscomWheelSetup.Caption:='ASCOM '+rsFilterWheel+blank+rsSetup;
   MenuAscomFocuserSetup.Caption:='ASCOM '+rsFocuser+blank+rsSetup;
   MenuAscomMountSetup.Caption:='ASCOM '+rsMount+blank+rsSetup;
   MenuAscomRotatorSetup.Caption:='ASCOM '+rsRotator+blank+rsSetup;
   MenuAscomWeatherSetup.Caption:='ASCOM '+rsWeatherStati+blank+rsSetup;
   MenuAscomSafetySetup.Caption:='ASCOM '+rsSafetyMonito+blank+rsSetup;
   MenuAscomDomeSetup.Caption:='ASCOM '+rsDome+blank+rsSetup;
   MenuAlpacaServerSetup.Caption:='Alpaca '+rsServer+blank+rsSetup;
   MenuAlpacaCameraSetup.Caption:='Alpaca '+rsCamera+blank+rsSetup;
   MenuAlpacaWheelSetup.Caption:='Alpaca '+rsFilterWheel+blank+rsSetup;
   MenuAlpacaFocuserSetup.Caption:='Alpaca '+rsFocuser+blank+rsSetup;
   MenuAlpacaMountSetup.Caption:='Alpaca '+rsMount+blank+rsSetup;
   MenuAlpacaRotatorSetup.Caption:='Alpaca '+rsRotator+blank+rsSetup;
   MenuAlpacaWeatherSetup.Caption:='Alpaca '+rsWeatherStati+blank+rsSetup;
   MenuAlpacaSafetySetup.Caption:='Alpaca '+rsSafetyMonito+blank+rsSetup;
   MenuAlpacaDomeSetup.Caption:='Alpaca '+rsDome+blank+rsSetup;
   MenuViewhdr.Caption := rsViewHeader;
   MenuImgStat.Caption:=rsImageStatist;
   MenuImage.Caption:=rsImage;
   MenuItem15.Caption := rsTools;
   MenuItem4.Caption := rsDisplay;
   MenuViewConnection.Caption := rsConnection;
   MenuViewPreview.Caption := rsPreview;
   MenuViewAutoguider.Caption := rsAutoguider;
   MenuViewPlanetarium.Caption := rsPlanetarium;
   MenuViewScript.Caption := rsScript;
   MenuViewFocuser.Caption := rsFocuser;
   MenuViewStarProfile.Caption := rsFocus;
   MenuViewMagnifyer.Caption := rsMagnifyer;
   MenuViewCapture.Caption := rsCapture;
   MenuViewFilters.Caption := rsFilters;
   MenuViewFrame.Caption := rsFrame;
   MenuViewRotator.Caption := rsRotator;
   MenuViewCCDtemp.Caption := rsSensorTemperatu;
   MenuViewMount.Caption := rsTelescopeMou;
   MenuViewDome.Caption := rsDome;
   MenuViewSequence.Caption := rsSequence;
   MenuViewVideo.Caption := rsVideo;
   MenuViewHistogram.Caption := rsVisualisatio;
   MenuViewMessages.Caption := rsMessages;
   MenuViewClock.Caption := rsClock;
   MenuResetTools.Caption := rsResetToDefau;
   MenuReset1col.Caption:='1 '+rsColumn;
   MenuReset2col.Caption:='2 '+rsColumn;
   MenuTabConnect.Caption := rsConnect;
   MenuConnection.Caption := rsConnection;
   MenuConnect.Caption := rsConnect;
   MenuPreview.Caption := rsPreview;
   MenuPreviewStart.Caption := rsStart;
   MenuPreviewLoop.Caption := rsLoop;
   MenuAutoguider.Caption := rsAutoguider;
   MenuAutoguiderConnect.Caption := rsConnect;
   MenuAutoguiderCalibrate.Caption := rsCalibrate;
   MenuAutoguiderGuide.Caption := rsGuide;
   MenuAutoguiderDither.Caption := rsDither;
   MenuPlanetarium.Caption := rsPlanetarium;
   MenuPlanetariumConnect.Caption := rsConnect;
   MenuPlanetariumNewtarget.Caption := rsNewTarget;
   MenuScript.Caption := rsScript;
   MenuScriptRun.Caption := rsRun;
   MenuScriptStop.Caption := rsStop;
   MenuScriptEdit.Caption := rsEdit;
   MenuScriptNew.Caption := rsNew;
   MenuHistogram.Caption := rsVisualisatio;
   MenuVisuZoom2.Caption := rsZoom+' 2:1';
   MenuVisuZoom1.Caption := rsZoom+' 1:1';
   MenuVisuZoom12.Caption := rsZoom+' 1:2';
   MenuVisuZoomAdjust.Caption := rsAdjustToWind;
   MenuTabFocus.Caption := rsFocus;
   MenuFocuser.Caption := rsFocuser;
   MenuFocuserIn.Caption := rsMoveInward;
   MenuFocuserOut.Caption := rsMoveOutward;
   MenuStarProfile.Caption := rsFocus;
   MenuFocusaid.Caption := rsFocusAid;
   MenuTabCapture.Caption := rsCapture;
   MenuCapture.Caption := rsCapture;
   MenuCaptureStart.Caption := rsStart;
   MenuFilters.Caption := rsFilters;
   if MenuFilters.Count>0 then MenuFilters.Items[0].Caption:=rsFilter0;
   MenuFrame.Caption := rsFrame;
   MenuFrameSet.Caption := rsSet;
   MenuFrameReset.Caption := rsReset;
   MenuRotator.Caption := rsRotator;
   MenuRotatorRotate.Caption := rsRotate;
   MenuCCDtemp.Caption := rsSensorTemperatu;
   MenuCCDtempSet.Caption := rsSet;
   MenuMount.Caption := rsTelescopeMou;
   MenuMountPark.Caption := rsPark;
   MenuMountTrack.Caption := rsTrack;
   MenuTabSequence.Caption := rsSequence;
   MenuSequence.Caption := rsSequence;
   MenuSequenceLoad.Caption := rsLoad;
   MenuSequenceNew.Caption := rsNew;
   MenuSequenceEdit.Caption := rsEdit;
   MenuSequenceStart.Caption := rsStart;
   MenuSequenceStop.Caption := rsStop;
   MenuTabVideo.Caption := rsVideo;
   MenuVideo.Caption := rsVideo;
   MenuVideoPreview.Caption := rsPreview;
   MenuVideoStart.Caption := rsStartRecord;
   MenuVideoStop.Caption := rsStopRecord;
   MenuItem3.Caption := rsHelp;
   MenuPdfHelp.Caption := rsPDFDocumenta;
   MenuOnlineHelp.Caption := rsOnlineDocume;
   MenuUsergroup.Caption := rsUserGroup;
   MenuShowLog.Caption := rsShowCurrentL;
   MenuShowINDIlog.Caption:=rsShowINDILog;
   MenuBrowseLog.Caption := rsBrowseLogFil;
   MenuBugReport.Caption := rsReportAProbl;
   MenuDownload.Caption := rsDownloadLate;
   MenuHelpAbout.Caption := rsAbout;
   MenuResolve.Caption := rsResolve;
   MenuResolve2.Caption := rsResolve;
   MenuResolveSlewCenter.Caption := rsResolveAndSl;
   MenuResolveSlewCenter2.Caption := rsResolveAndSl;
   MenuResolveSlew.Caption := rsResolveAndSl2;
   MenuResolveSlew2.Caption := rsResolveAndSl2;
   MenuResolveSync.Caption := rsResolveAndSy;
   MenuResolveSync2.Caption := rsResolveAndSy;
   MenuResolveRotate.Caption := rsResolveAndRo;
   MenuResolveRotate2.Caption := rsResolveAndRo;
   MenuResolveSyncRotator.Caption := rsResolveAndSy2;
   MenuResolveSyncRotator2.Caption := rsResolveAndSy2;
   MenuResolveDSO.Caption:=rsResolveAndPl;
   MenuResolveDSO2.Caption:=rsResolveAndPl;
   MenuResolveHyperLeda.Caption:=rsResolveAndPl2;
   MenuResolveHyperLeda2.Caption:=rsResolveAndPl2;
   MenuResolvePlanetarium.Caption := rsResolveAndSh;
   MenuResolvePlanetarium2.Caption := rsResolveAndSh;
   MenuShowCCDFrame.Caption := rsResolveAndSh2;
   MenuShowCCDFrame2.Caption := rsResolveAndSh2;
   MenuViewAstrometryLog.Caption := rsViewLastReso;
   MenuViewAstrometryLog2.Caption := rsViewLastReso;
   MenuStopAstrometry.Caption := rsStopAstromet;
   MenuStopAstrometry2.Caption := rsStopAstromet;
   MenuItemPhotometry.Caption:=rsPhotometry;
   MenuItemPhotometry2.Caption:=rsPhotometry;
   MenuItemDebayer.Caption := rsPreviewDebay;
   MenuItemDebayer2.Caption := rsPreviewDebay;
   MenuItemCleanup.Caption:=rsImageCleanup;
   MenuItemCleanup2.Caption:=rsImageCleanup;
   MenuItemUnselect.Caption:=rsUnselectStar;
   MenuItemUnselect2.Caption:=rsUnselectStar;
   SubDirName[0]:=rsSubfolderByS;
   SubDirName[1]:=rsSubfolderByF;
   SubDirName[2]:=rsSubfolderByO;
   SubDirName[3]:=rsSubfolderByP;
   SubDirName[4]:=rsSubfolderByE;
   SubDirName[5]:=rsSubfolderByB;
   SubDirName[6]:=rsSubfolderByD;
   SubDirName[7]:=rsSubfolderByD2;
   Filter0:=rsFilter0;
   if FilterList.Count>0 then FilterList[0]:=rsFilter0;
   FilenameName[0]:=rsObjectName;
   FilenameName[1]:=rsFilter;
   FilenameName[2]:=rsExposureTime2;
   FilenameName[3]:=rsBinning;
   FilenameName[4]:=rsSensorTemperatu;
   FilenameName[5]:=rsDateUTSequen;
   FilenameName[6]:=rsGain;
   FilenameName[7]:=rsFocusPositio;
   TBConnect.Hint := rsConnect;
   TBFocus.Hint := rsFocus;
   TBCapture.Hint := rsCapture;
   TBSequence.Hint := rsSequence;
   TBVideo.Hint := rsVideo;
   SafetyActionName[0]:='';
   SafetyActionName[1]:=trim(rsShowPrompt);
   SafetyActionName[2]:=trim(rsAbortTheCurr);
   SafetyActionName[3]:=trim(rsStopTelescop2);
   SafetyActionName[4]:=trim(rsParkTheTeles2);
   SafetyActionName[5]:=trim(rsStopDomeSlav);
   SafetyActionName[6]:=trim(rsParkDome);
   SafetyActionName[7]:=trim(rsCloseDome);
   SafetyActionName[8]:=trim(rsWarmTheCamer);
   SafetyActionName[9]:=trim(rsAutoguiderSh);
   SafetyActionName[10]:=trim(rsPlanetariumS);
   SafetyActionName[11]:=trim(rsCallExternal);
   SafetyActionName[12]:=trim(rsExitProgram);
   DevInterfaceName[2]:=rsInCamera;
   DevInterfaceName[3]:=rsInMount;
   DevInterfaceName[5]:=rsManual;
   DomeCloseActionName[0]:='';
   DomeCloseActionName[1]:=trim(rsStopTelescop2);
   DomeCloseActionName[2]:=trim(rsParkTheTeles2);
   DomeCloseActionName[3]:=trim(rsStopDomeSlav);
   DomeCloseActionName[4]:=trim(rsParkDome);
   DomeCloseActionName[5]:=trim(rsCloseDome);
   DomeOpenActionName[0]:='';
   DomeOpenActionName[1]:=trim(rsOpenTheDomeS);
   DomeOpenActionName[2]:=trim(rsUnparkTheDom);
   DomeOpenActionName[3]:=trim(rsUnparkTheTel);
   DomeOpenActionName[4]:=trim(rsStartTelesco);
   DomeOpenActionName[5]:=trim(rsSlaveTheDome);
   TabMsgLevel.Tabs[0]:=rsSummary;
   TabMsgLevel.Tabs[1]:=rsCommands;
   TabMsgLevel.Tabs[2]:=rsDetails;
   u_speech.InitSpeak;
   if  f_devicesconnection<>nil then f_devicesconnection.SetLang;
   if  f_preview<>nil then f_preview.SetLang;
   if  f_visu<>nil then f_visu.SetLang;
   if  f_capture<>nil then f_capture.SetLang;
   if  f_setup<>nil then f_setup.SetLang;
   if  f_starprofile<>nil then f_starprofile.SetLang;
   if  f_filterwheel<>nil then f_filterwheel.SetLang;
   if  f_focuser<>nil then f_focuser.SetLang;
   if  f_script<>nil then f_script.SetLang;
   if  f_ccdtemp<>nil then f_ccdtemp.SetLang;
   if  f_option<>nil then f_option.Setlang;
   if  f_frame<>nil then f_frame.SetLang;
   if  f_autoguider<>nil then f_autoguider.SetLang;
   if  f_sequence<>nil then f_sequence.SetLang;
   if  f_EditTargets<>nil then f_EditTargets.SetLang;
   if  f_planetarium<>nil then f_planetarium.SetLang;
   if  f_planetariuminfo<>nil then f_planetariuminfo.SetLang;
   if  f_mount<>nil then f_mount.SetLang;
   if  f_pause<>nil then f_pause.setlang;
   if  f_vcurve<>nil then f_vcurve.SetLang;
   if  f_rotator<>nil then f_rotator.SetLang;
   if  f_focusercalibration<>nil then f_focusercalibration.SetLang;
   if  f_magnifyer<>nil then f_magnifyer.SetLang;
   if  f_weather<>nil then f_weather.SetLang;
   if  f_dome<>nil then f_dome.SetLang;
   if  f_safety<>nil then f_safety.SetLang;
   if  f_cover<>nil then f_cover.SetLang;
   if  f_switch<>nil then f_switch.SetLang;
   if  f_about<>nil then f_about.SetLang;
   if  f_goto<>nil then f_goto.SetLang;
   if  f_photometry<>nil then f_photometry.SetLang;
   if  f_polaralign<>nil then f_polaralign.SetLang;
   if  f_collimation<>nil then f_collimation.SetLang;
end;

procedure Tf_main.FormShow(Sender: TObject);
var str: string;
    i: integer;
begin

  SetDevices;

  InitCoord;
  SetConfig;
  SetOptions;
  LoadVcurve;
  LoadBPM;

  FocuserLastTemp:=config.GetValue('/StarAnalysis/FocuserLastTemp',NullCoord);
  AutofocusLastTemp:=FocuserLastTemp;

  f_ccdtemp.Setpoint.Value:=config.GetValue('/Temperature/Setpoint',0);
  f_preview.ExpTime.Text:=config.GetValue('/Preview/Exposure','1');
  f_capture.ExposureTime:=config.GetValue('/Capture/Exposure',1.0);
  f_capture.StackNum.Value:=config.GetValue('/Capture/StackNum',1);
  f_capture.Fname.Text:=config.GetValue('/Capture/FileName','');
  f_capture.SeqNum.Value:=config.GetValue('/Capture/Count',1);

  f_visu.Gamma.Value:=config.GetValue('/Visu/Gamma',1.0);
  f_visu.HistBar.Position:=config.GetValue('/Visu/HistBar',50);
  f_visu.FlipHorz:=config.GetValue('/Visu/FlipHorz',false);
  f_visu.FlipVert:=config.GetValue('/Visu/FlipVert',false);
  f_visu.BtnClipRange.Down:=config.GetValue('/Visu/ClipRange',false);

  LogLevel:=config.GetValue('/Log/LogLevel',LogLevel);
  TabMsgLevel.TabIndex:=LogLevel-1;

  ImaBmp:=TBGRABitmap.Create(1,1);
  LockTimerPlot:=false;
  LockMouseWheel:=false;
  LockRestartExposure:=false;
  ImgCx:=0;
  ImgCy:=0;
  StartX:=0;
  StartY:=0;
  EndX:=0;
  EndY:=0;
  Capture:=false;
  Preview:=false;
  MenuIndiSettings.Enabled:=true;//(camera.CameraInterface=INDI);
  MenuShowINDIlog.Visible:=true;//(camera.CameraInterface=INDI);
  ObsTimeZone:=-GetLocalTimeOffset/60;
  crRetic:=6;
  CursorImage1 := TCursorImage.Create;
  if fileexists(slash(DataDir) + slash('resources') + 'smallcross.cur') then
  begin
    try
      CursorImage1.LoadFromFile(SysToUTF8(slash(DataDir) + slash('resources') + 'smallcross.cur'));
      Screen.Cursors[crRetic] := CursorImage1.Handle;
    except
      crRetic := crCross;
    end;
  end
  else
    crRetic := crCross;
  Image1.Cursor:=crRetic;
  MaxThreadCount := GetThreadCount;
  NewMessage(Format('Using a maximum of %d parallel processor',[MaxThreadCount]),9);
  if isAdmin then NewMessage(Caption);
  NewMessage(SystemInformation,9);
  {$ifdef mswindows}
  NewMessage(AscomVersion,9);
  {$endif}
  {$ifdef unix}
   {$ifndef darwin}
   NewMessage(IndiVersion,9);
   {$endif}
  {$endif}

  NewMessage('CCDciel '+ccdciel_version+' Copyright (C) '+cdate+' Patrick Chevalley. This is free software, you can redistribute it under certain conditions.');
  NewMessage('This program comes with ABSOLUTELY NO WARRANTY; for details see '+rsHelp+'/'+rsAbout);
  NewMessage(Format(rsUsingConfigu, [config.Filename]), 3);

  if (cdcwcs_initfitsfile=nil)or(cdcwcs_release=nil)or(cdcwcs_sky2xy=nil)or(cdcwcs_xy2sky=nil)or(cdcwcs_getinfo=nil) then begin
     NewMessage('Could not load '+libwcs+crlf+'Some astrometry function are not available.',1);
  end;
  if (libraw=0)and(DcrawCmd='')and(RawUnpCmd='') then begin
     NewMessage('Could not find '+librawname
     {$ifdef unix}
               +', libraw.so, unprocessed_raw, raw-identify'
     {$endif}
               +' or '+dcrawname+'. Loading camera raw files is not possible.',1);
  end;

  SetTheme;

  PanelBottom.Tag:=PtrInt(MenuTabConnect);
  PanelRight1.Tag:=PtrInt(MenuTabConnect);
  PanelRight2.Tag:=PtrInt(MenuTabFocus);
  PanelRight3.Tag:=PtrInt(MenuTabCapture);
  PanelRight4.Tag:=PtrInt(MenuTabSequence);

  ShowActiveTools;

  for i:=0 to MaxMenulevel do AccelList[i]:='';
  SetMenuAccelerator(MainMenu1.items,0,AccelList);

  StatusBar1.Visible:=false; // bug with statusbar visibility
  StatusbarTimer.Enabled:=true;

  f_option.onShowHelp:=@MenuPdfHelpClick;
  f_setup.onShowHelp:=@MenuPdfHelpClick;
  f_setup.onMsg:=@NewMessage;

  SetOptions;

  str:=config.GetValue('/Sequence/Targets','');
  if str<>'' then f_sequence.LoadTargets(str);
  f_sequence.Unattended.Checked:=config.GetValue('/Sequence/Unattended',false);
  f_EditTargets.Width:=config.GetValue('/Sequence/EditTarget/Width',f_EditTargets.Width);
  f_EditTargets.Height:=config.GetValue('/Sequence/EditTarget/Height',f_EditTargets.Height);
  f_EditTargets.Splitter1.Top:=config.GetValue('/Sequence/EditTarget/SepPos',f_EditTargets.Splitter1.Top);

  f_planetariuminfo.planetarium:=planetarium;

  f_script.SetScriptList(config.GetValue('/Script/ScriptName',''));

  f_photometry.onMagnitudeCalibrationChange:=@MagnitudeCalibrationChange;
  f_photometry.onClosePhotometry:=@PhotometryClose;

  LoadFocusStar(config.GetValue('/StarAnalysis/AutofocusStarMag',4));
  deepstring:=TStringList.Create;

  f_polaralign.Fits:=fits;
  f_polaralign.Preview:=f_preview;
  f_polaralign.Visu:=f_visu;
  f_polaralign.Astrometry:=astrometry;
  f_polaralign.onShowMessage:=@NewMessage;
  f_polaralign.onClose:=@PolaralignClose;

  StartupTimer.Enabled:=true;
end;

procedure Tf_main.ShowActiveTools;
begin
  WantCamera:=true;
  WantWheel:=config.GetValue('/Devices/FilterWheel',false);
  WantFocuser:=config.GetValue('/Devices/Focuser',false);
  WantRotator:=config.GetValue('/Devices/Rotator',false);
  WantMount:=config.GetValue('/Devices/Mount',false);
  WantDome:=config.GetValue('/Devices/Dome',false);
  WantWeather:=config.GetValue('/Devices/Weather',false);
  WantSafety:=config.GetValue('/Devices/Safety',false);
  WantSwitch:=config.GetValue('/Devices/Switch',false);
  WantCover:=config.GetValue('/Devices/Cover',false);
  WantWatchdog:=(watchdog<>nil) and config.GetValue('/Devices/Watchdog',false);

  MenuAscomCameraSetup.Visible:=WantCamera and (camera.CameraInterface=ASCOM);
  MenuAscomWheelSetup.Visible:=WantWheel and (wheel.WheelInterface=ASCOM);
  MenuAscomFocuserSetup.Visible:=WantFocuser and (focuser.FocuserInterface=ASCOM);
  MenuAscomMountSetup.Visible:=WantMount and (mount.MountInterface=ASCOM);
  MenuAscomRotatorSetup.Visible:=WantRotator and (rotator.RotatorInterface=ASCOM);
  MenuAscomWeatherSetup.Visible:=WantWeather and (weather.WeatherInterface=ASCOM);
  MenuAscomSafetySetup.Visible:=WantSafety and (safety.SafetyInterface=ASCOM);
  MenuAscomDomeSetup.Visible:=WantDome and (dome.DomeInterface=ASCOM);
  MenuAscomSwitchSetup.Visible:=WantSwitch and (switch.SwitchInterface=ASCOM);
  MenuAscomCoverSetup.Visible:=WantCover and (cover.CoverInterface=ASCOM);

  MenuAlpacaServerSetup.Visible:=WantCamera and (camera.CameraInterface=ASCOMREST);
  MenuAlpacaCameraSetup.Visible:=WantCamera and (camera.CameraInterface=ASCOMREST);
  MenuAlpacaWheelSetup.Visible:=WantWheel and (wheel.WheelInterface=ASCOMREST);
  MenuAlpacaFocuserSetup.Visible:=WantFocuser and (focuser.FocuserInterface=ASCOMREST);
  MenuAlpacaMountSetup.Visible:=WantMount and (mount.MountInterface=ASCOMREST);
  MenuAlpacaRotatorSetup.Visible:=WantRotator and (rotator.RotatorInterface=ASCOMREST);
  MenuAlpacaWeatherSetup.Visible:=WantWeather and (weather.WeatherInterface=ASCOMREST);
  MenuAlpacaSafetySetup.Visible:=WantSafety and (safety.SafetyInterface=ASCOMREST);
  MenuAlpacaDomeSetup.Visible:=WantDome and (dome.DomeInterface=ASCOMREST);
  MenuAlpacaSwitchSetup.Visible:=WantSwitch and (switch.SwitchInterface=ASCOMREST);
  MenuAlpacaCoverSetup.Visible:=WantCover and (cover.CoverInterface=ASCOMREST);

  MenuIndiSettings.Visible:= (camera.CameraInterface=INDI)or(wheel.WheelInterface=INDI)or(focuser.FocuserInterface=INDI)or
                             (mount.MountInterface=INDI)or(rotator.RotatorInterface=INDI)or(weather.WeatherInterface=INDI)or
                             (safety.SafetyInterface=INDI)or(dome.DomeInterface=INDI)or(switch.SwitchInterface=INDI)or(cover.CoverInterface=INDI);

  SetTool(f_visu,'Histogram',PanelBottom,0,MenuViewHistogram,MenuHistogram,true);
  SetTool(f_msg,'Messages',PanelBottom,f_visu.left+1,MenuViewMessages,nil,true);

  SetTool(f_devicesconnection,'Connection',PanelRight1,0,MenuViewConnection,MenuConnection,true);
  SetTool(f_autoguider,'Autoguider',PanelRight1,f_devicesconnection.top+1,MenuViewAutoguider,MenuAutoguider,true);
  SetTool(f_planetarium,'Planetarium',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium,MenuPlanetarium,true);
  SetTool(f_preview,'Preview',PanelRight1,f_planetarium.top+1,MenuViewPreview,MenuPreview,true);
  SetTool(f_script,'Script',PanelRight1,f_preview.top+1,MenuViewScript,MenuScript,true);
  SetTool(f_dome,'Dome',PanelRight1,f_script.top+1,MenuViewDome,nil,WantDome);
  SetTool(f_weather,'Weather',PanelRight1,f_dome.top+1,MenuViewWeather,nil,WantWeather);
  SetTool(f_safety,'Safety',PanelRight1,f_weather.top+1,MenuViewSafety,nil,WantSafety);
  SetTool(f_cover,'Cover',PanelRight1,f_safety.top+1,MenuViewCover,nil,WantCover);
  SetTool(f_switch,'Switch',PanelRight1,f_cover.top+1,MenuViewSwitch,nil,WantSwitch);

  SetTool(f_focuser,'Focuser',PanelRight2,0,MenuViewFocuser,MenuFocuser,WantFocuser);
  SetTool(f_starprofile,'Starprofile',PanelRight2,f_focuser.top+1,MenuViewStarProfile,MenuStarProfile,true);
  SetTool(f_magnifyer,'Magnifyer',PanelRight2,f_starprofile.top+1,MenuViewMagnifyer,nil,true);

  SetTool(f_capture,'Capture',PanelRight3,0,MenuViewCapture,MenuCapture,true);
  SetTool(f_ccdtemp,'CCDTemp',PanelRight3,f_capture.top+1,MenuViewCCDtemp,MenuCCDtemp,true);
  SetTool(f_filterwheel,'Filters',PanelRight3,f_ccdtemp.top+1,MenuViewFilters,MenuFilters,WantWheel);
  SetTool(f_frame,'Frame',PanelRight3,f_filterwheel.top+1,MenuViewFrame,MenuFrame,true);
  SetTool(f_rotator,'Rotator',PanelRight3,f_frame.top+1,MenuViewRotator,MenuRotator,WantRotator);
  SetTool(f_mount,'Mount',PanelRight3,f_rotator.top+1,MenuViewMount,MenuMount,WantMount);

  SetTool(f_sequence,'Sequence',PanelRight4,0,MenuViewSequence,MenuSequence,true);

  SetTool(f_video,'Video',PanelRight5,0,MenuViewVideo,MenuVideo,true);

  MenuViewClock.Checked:=screenconfig.GetValue('/Tools/Clock/Visible',true);
  MenuViewClockClick(nil);
end;

procedure Tf_main.SetTheme;
var c:TBGRAPixel;
    btn: TPortableNetworkGraphic;
    i:integer;
begin
  // detect if theme color is dark
  {$ifdef lclcocoa}
  c:=ColorToBGRA(ColorToRGB(clBackground));
  {$else}
  c:=ColorToBGRA(ColorToRGB(clBtnFace));
  {$endif}
  i:=round((c.red+c.green+c.blue)/3);
  // change imagelist
  if i>=128 then begin
    TBTabs.Images:=ImageListDay;
    MainMenu1.Images:=ImageListDay;
    colorGreen:=clGreen;
    colorBlue:=clBlue;
    colorRed:=clRed;
  end
  else begin
    TBTabs.Images:=ImageListNight;
    MainMenu1.Images:=ImageListNight;
    colorGreen:=clLime;
    colorBlue:=clAqua;
    colorRed:=clFuchsia;
    {$ifdef lclcocoa}
    TBTabs.Color:=clBackground;
    {$endif}
  end;
  // change individual buttons
  btn := TPortableNetworkGraphic.Create;
  TBTabs.Images.GetBitmap(5, btn);
  f_visu.BtnZoomAdjust.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(6, btn);
  f_visu.BtnBullsEye.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(7, btn);
  f_visu.BtnClipRange.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(8, btn);
  f_visu.BtnClipping.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(11, btn);
  f_visu.BtnInvert.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(12, btn);
  f_visu.BtnFlipHorz.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(13, btn);
  f_visu.BtnFlipVert.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(14, btn);
  f_visu.BtnShowImage.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(9, btn);
  f_starprofile.BtnPinGraph.Glyph.Assign(btn);
  f_starprofile.BtnPinProfile.Glyph.Assign(btn);
  f_starprofile.BtnPin2D.Glyph.Assign(btn);
  f_starprofile.BtnPinTrend.Glyph.Assign(btn);
  TBTabs.Images.GetBitmap(10, btn);
  f_EditTargets.BtnRepeatInf.Glyph.Assign(btn);
  btn.Free;
  {$ifdef lclcocoa}
  if i<128 then begin
   if f_devicesconnection<>nil then f_devicesconnection.Title.Color:=clBtnShadow;
   if f_filterwheel<>nil then f_filterwheel.Title.Color:=clBtnShadow;
   if f_ccdtemp<>nil then f_ccdtemp.Title.Color:=clBtnShadow;
   if f_frame<>nil then f_frame.Title.Color:=clBtnShadow;
   if f_preview<>nil then f_preview.Title.Color:=clBtnShadow;
   if f_capture<>nil then f_capture.Title.Color:=clBtnShadow;
   if f_video<>nil then f_video.Title.Color:=clBtnShadow;
   if f_sequence<>nil then f_sequence.Title1.Color:=clBtnShadow;
   if f_sequence<>nil then f_sequence.Title2.Color:=clBtnShadow;
   if f_sequence<>nil then f_sequence.Title3.Color:=clBtnShadow;
   if f_starprofile<>nil then f_starprofile.Title.Color:=clBtnShadow;
   if f_focuser<>nil then f_focuser.Title.Color:=clBtnShadow;
   if f_magnifyer<>nil then f_magnifyer.Title.Color:=clBtnShadow;
   if f_rotator<>nil then f_rotator.Title.Color:=clBtnShadow;
   if f_mount<>nil then f_mount.Title.Color:=clBtnShadow;
   if f_dome<>nil then f_dome.Title.Color:=clBtnShadow;
   if f_weather<>nil then f_weather.Title.Color:=clBtnShadow;
   if f_safety<>nil then f_safety.Title.Color:=clBtnShadow;
   if f_cover<>nil then f_cover.Title.Color:=clBtnShadow;
   if f_switch<>nil then f_switch.Title.Color:=clBtnShadow;
   if f_autoguider<>nil then f_autoguider.Title.Color:=clBtnShadow;
   if f_planetarium<>nil then f_planetarium.Title.Color:=clBtnShadow;
   if f_script<>nil then f_script.Title.Color:=clBtnShadow;
   if f_visu<>nil then f_visu.Title.Color:=clBtnShadow;
   if f_msg<>nil then f_msg.Title.Color:=clBtnShadow;
  end;
 {$endif}
end;

procedure Tf_main.StartupTimerTimer(Sender: TObject);
var buf: string;
    timeout,endt: double;
    shutdown: boolean;
begin
  StartupTimer.Enabled:=false;
  if FOpenSetup then begin
     // first setup screen
     if width<=1024 then
        MenuReset1col.Click
     else
        MenuReset2col.Click;
     SaveScreenConfig;
     MenuSetup.Click;
  end
    else begin
     // process automated startup options
     f_script.RunStartupScript;
     if Application.HasOption('r', 'run_sequence') then begin
        buf:=Application.GetOptionValue('r', 'run_sequence');
        NewMessage('Run sequence '+buf);
        if fileexists(buf) then begin
          f_sequence.LoadTargets(buf);
          if not AllDevicesConnected then begin
            Connect(nil);
            timeout:=60;
            endt:=now+timeout/secperday;
            while (not AllDevicesConnected )and(now<endt) do begin
               sleep(100);
               if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
            end;
          end;
          if AllDevicesConnected then begin
             shutdown:=Application.HasOption('s', 'shutdown');
             f_sequence.AtEndShutdown:=shutdown;
             f_sequence.OnShutdown:=@ShutdownProgram;
             f_sequence.BtnStart.Click;
          end
          else NewMessage('Devices not connected!');
        end
        else NewMessage('File not found: '+buf);
     end;
    end;
end;

procedure Tf_main.StatusBar1DrawPanel(StatusBar: TStatusBar;  Panel: TStatusPanel; const Rect: TRect);
var msg: string;
    s,x,y: integer;
begin
  if StatusBar=StatusBar1 then begin;
    if panel=StatusBar.Panels[panelled] then begin
      msg:='';
      s:=(StatusBar.Height-6) div 2;
      y:=StatusBar.Height div 2;
      // clear
      statusbar.Canvas.Brush.Color:=clDefault;
      statusbar.Canvas.FillRect(Rect);
      // planetarium
      x:=Rect.Left+s+4;
      if (f_planetarium<>nil)and(not planetarium.Terminated)and(planetarium.Connected) then begin
        statusbar.Canvas.Brush.Color:=cllime;
        msg:=msg+Format(rsConnected,[rsPlanetarium]);
      end
      else begin
        statusbar.Canvas.Brush.Color:=clRed;
        msg:=msg+Format(rsDisconnected,[rsPlanetarium]);
      end;
      statusbar.Canvas.Ellipse(x-s,y-s,x+s,y+s);
      // guider
      x:=x+2*s+4;
      if f_autoguider=nil then begin
         statusbar.Canvas.Brush.Color:=clGray;
         msg:=msg+', '+Format(rsDisconnected,[rsAutoguider]);
      end
      else begin
        if autoguider.State=GUIDER_DISCONNECTED then begin
          if Autoguider.AutoguiderType=agNONE
          then
            statusbar.Canvas.Brush.Color:=clGray
          else
            statusbar.Canvas.Brush.Color:=clred;
          msg:=msg+', '+Format(rsDisconnected,[rsAutoguider]);
        end
        else if (autoguider.State=GUIDER_GUIDING) then begin
          statusbar.Canvas.Brush.Color:=cllime;
          msg:=msg+', '+Format(rsGuiding,[rsAutoguider]);
        end
        else begin
          statusbar.Canvas.Brush.Color:=clYellow;
          msg:=msg+', '+Format(rsConnected,[rsAutoguider]);
        end;
      end;
      statusbar.Canvas.Ellipse(x-s,y-s,x+s,y+s);
      // device
      x:=x+2*s+4;
      if AllDevicesConnected then begin
        statusbar.Canvas.Brush.Color:=cllime;
        msg:=msg+', '+Format(rsConnected, [rsDevices]);
      end
      else begin
        statusbar.Canvas.Brush.Color:=clred;
        msg:=msg+', '+Format(rsDisconnected, [rsDevices]);
      end;
      statusbar.Canvas.Ellipse(x-s,y-s,x+s,y+s);
      // set hint
      statusbar.Hint:=msg;
    end;
  end;
end;

procedure Tf_main.StatusBar1Resize(Sender: TObject);
var i: integer;
begin
  StatusBar1.Panels[panelled].Width:=3*(StatusBar1.Height);
  i:=StatusBar1.ClientWidth-StatusBar1.Panels[panelcursor].Width-StatusBar1.Panels[panelstatus].Width-StatusBar1.Panels[panelclock].Width-StatusBar1.Panels[panelled].Width;
  if i>0 then
    StatusBar1.Panels[panelfile].Width:=i
  else
    StatusBar1.Panels[panelfile].Width:=0;
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
var i,n: integer;
begin
if sender is TMenuItem then begin
  n:=TMenuItem(sender).Tag;
  if n=1 then begin
    // all in the right panel
    SetTool(f_visu,'',PanelBottom,0,MenuViewHistogram,MenuHistogram,true);
    SetTool(f_msg,'',PanelBottom,f_visu.left+1,MenuViewMessages,nil,true);

    SetTool(f_devicesconnection,'',PanelRight1,0,MenuViewConnection,MenuConnection,true);
    SetTool(f_autoguider,'',PanelRight1,f_devicesconnection.top+1,MenuViewAutoguider,MenuAutoguider,true);
    SetTool(f_planetarium,'',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium,MenuPlanetarium,true);
    SetTool(f_preview,'',PanelRight1,f_planetarium.top+1,MenuViewPreview,MenuPreview,true);
    SetTool(f_script,'',PanelRight1,f_preview.top+1,MenuViewScript,MenuScript,true);
    SetTool(f_dome,'',PanelRight1,f_script.top+1,MenuViewDome,nil,WantDome);
    SetTool(f_weather,'',PanelRight1,f_dome.top+1,MenuViewWeather,nil,WantWeather);
    SetTool(f_safety,'',PanelRight1,f_weather.top+1,MenuViewSafety,nil,WantSafety);
    SetTool(f_cover,'',PanelRight1,f_safety.top+1,MenuViewCover,nil,WantCover);
    SetTool(f_switch,'',PanelRight1,f_cover.top+1,MenuViewSwitch,nil,WantSwitch);

    SetTool(f_focuser,'',PanelRight2,0,MenuViewFocuser,MenuFocuser,WantFocuser);
    SetTool(f_starprofile,'',PanelRight2,f_focuser.top+1,MenuViewStarProfile,MenuStarProfile,true);
    SetTool(f_magnifyer,'',PanelRight2,f_starprofile.top+1,MenuViewMagnifyer,nil,true);

    SetTool(f_capture,'',PanelRight3,0,MenuViewCapture,MenuCapture,true);
    SetTool(f_ccdtemp,'',PanelRight3,f_capture.top+1,MenuViewCCDtemp,MenuCCDtemp,true);
    SetTool(f_filterwheel,'',PanelRight3,f_ccdtemp.top+1,MenuViewFilters,MenuFilters,WantWheel);
    SetTool(f_frame,'',PanelRight3,f_filterwheel.top+1,MenuViewFrame,MenuFrame,true);
    SetTool(f_rotator,'',PanelRight3,f_frame.top+1,MenuViewRotator,MenuRotator,WantRotator);
    SetTool(f_mount,'',PanelRight3,f_rotator.top+1,MenuViewMount,MenuMount,WantMount);

    SetTool(f_sequence,'',PanelRight4,0,MenuViewSequence,MenuSequence,true);

    SetTool(f_video,'',PanelRight5,0,MenuViewVideo,MenuVideo,true);
  end
  else if n=2 then begin
    // use left and right panel
   SetTool(f_visu,'',PanelBottom,0,MenuViewHistogram,MenuHistogram,true);
   SetTool(f_msg,'',PanelBottom,f_visu.left+1,MenuViewMessages,nil,true);

   SetTool(f_ccdtemp,'',PanelLeft,0,MenuViewCCDtemp,MenuCCDtemp,true);
   SetTool(f_filterwheel,'',PanelLeft,f_ccdtemp.top+1,MenuViewFilters,MenuFilters,WantWheel);
   SetTool(f_frame,'',PanelLeft,f_filterwheel.top+1,MenuViewFrame,MenuFrame,true);
   SetTool(f_rotator,'',PanelLeft,f_frame.top+1,MenuViewRotator,MenuRotator,WantRotator);
   SetTool(f_focuser,'',PanelLeft,f_rotator.top+1,MenuViewFocuser,MenuFocuser,WantFocuser);
   SetTool(f_mount,'',PanelLeft,f_focuser.top+1,MenuViewMount,MenuMount,WantMount);

   SetTool(f_devicesconnection,'',PanelRight1,0,MenuViewConnection,MenuConnection,true);
   SetTool(f_autoguider,'',PanelRight1,f_devicesconnection.top+1,MenuViewAutoguider,MenuAutoguider,true);
   SetTool(f_planetarium,'',PanelRight1,f_autoguider.top+1,MenuViewPlanetarium,MenuPlanetarium,true);
   SetTool(f_preview,'',PanelRight1,f_planetarium.top+1,MenuViewPreview,MenuPreview,true);
   SetTool(f_weather,'',PanelRight1,f_preview.top+1,MenuViewWeather,nil,WantWeather);
   SetTool(f_safety,'',PanelRight1,f_weather.top+1,MenuViewSafety,nil,WantSafety);
   SetTool(f_dome,'',PanelRight1,f_safety.top+1,MenuViewDome,nil,WantDome);
   SetTool(f_cover,'',PanelRight1,f_dome.top+1,MenuViewCover,nil,WantCover);
   SetTool(f_switch,'',PanelRight1,f_cover.top+1,MenuViewSwitch,nil,WantSwitch);

   SetTool(f_starprofile,'',PanelRight2,0,MenuViewStarProfile,MenuStarProfile,true);
   SetTool(f_magnifyer,'',PanelRight2,f_starprofile.top+1,MenuViewMagnifyer,nil,true);

   SetTool(f_capture,'',PanelRight3,0,MenuViewCapture,MenuCapture,true);
   SetTool(f_script,'',PanelRight3,f_capture.top+1,MenuViewScript,MenuScript,true);

   SetTool(f_sequence,'',PanelRight4,0,MenuViewSequence,MenuSequence,true);

   SetTool(f_video,'',PanelRight5,0,MenuViewVideo,MenuVideo,true);
  end;
  for i:=0 to MaxMenulevel do AccelList[i]:='';
  SetMenuAccelerator(MainMenu1.items,0,AccelList);
end;
end;

procedure Tf_main.UpdConfig(oldver:string);
var ok:boolean;
    i: integer;
    f: double;
    bm: TBayerMode;
    msg,buf: string;
procedure movetoolconfig(tool:string; defaultParent: TPanel);
begin
  screenconfig.SetValue('/Tools/'+tool+'/Parent',config.GetValue('/Tools/'+tool+'/Parent',defaultParent.name));
  screenconfig.SetValue('/Tools/'+widestring(tool)+'/Top',config.GetValue('/Tools/'+widestring(tool)+'/Top',0));
  screenconfig.SetValue('/Tools/'+widestring(tool)+'/Left',config.GetValue('/Tools/'+widestring(tool)+'/Left',0));
  screenconfig.SetValue('/Tools/'+widestring(tool)+'/Visible',config.GetValue('/Tools/'+widestring(tool)+'/Visible',true));
end;
begin
try
  if trim(oldver)='' then
     exit;
  if oldver<'0.0.1a' then begin
     config.DeletePath('/Tools');
     config.Flush;
  end;
  if oldver<'0.9.20' then begin
    for i:=0 to SubDirCount-1 do begin
      case i of
        0: ok:=config.GetValue('/Files/SubfolderSequence',false);
        1: ok:=config.GetValue('/Files/SubfolderFrametype',false);
        2: ok:=config.GetValue('/Files/SubfolderObjname',false);
        3: ok:=config.GetValue('/Files/SubfolderStep',false);
        4: ok:=config.GetValue('/Files/SubfolderExposure',false);
        5: ok:=config.GetValue('/Files/SubfolderBinning',false);
        else ok:=false;
      end;
      SubDirOpt[i]:=TSubDirList(i);
      SubDirActive[i]:=ok;
      config.SetValue('/Files/SubDirOpt'+inttostr(i),ord(SubDirOpt[i]));
      config.SetValue('/Files/SubDirActive'+inttostr(i),SubDirActive[i]);
    end;
    for i:=0 to FileNameCount-1 do begin
      case i of
        0: ok:=config.GetValue('/Files/FilenameObjname',true);
        1: ok:=config.GetValue('/Files/FilenameFilter',true);
        2: ok:=config.GetValue('/Files/FilenameExposure',false);
        3: ok:=config.GetValue('/Files/FilenameBinning',false);
        4: ok:=config.GetValue('/Files/FilenameCCDtemp',false);
        5: ok:=config.GetValue('/Files/FilenameDate',true);
        else ok:=false;
      end;
      FilenameOpt[i]:=TFilenameList(i);
      FilenameActive[i]:=ok;
      config.SetValue('/Files/FileNameOpt'+inttostr(i),ord(FilenameOpt[i]));
      config.SetValue('/Files/FileNameActive'+inttostr(i),FilenameActive[i]);
    end;
    config.DeleteValue('/Files/SubfolderSequence');
    config.DeleteValue('/Files/SubfolderObjname');
    config.DeleteValue('/Files/SubfolderStep');
    config.DeleteValue('/Files/SubfolderFrametype');
    config.DeleteValue('/Files/SubfolderExposure');
    config.DeleteValue('/Files/SubfolderBinning');
    config.DeleteValue('/Files/FilenameObjname');
    config.DeleteValue('/Files/FilenameFilter');
    config.DeleteValue('/Files/FilenameExposure');
    config.DeleteValue('/Files/FilenameBinning');
    config.DeleteValue('/Files/FilenameCCDtemp');
    config.DeleteValue('/Files/FilenameDate');
    SaveConfig;
  end;
  if oldver<'0.9.22' then begin
    AutofocusDynamicNumPoint:=config.GetValue('/StarAnalysis/AutofocusMeanNumPoint',7);
    AutofocusDynamicMovement:=config.GetValue('/StarAnalysis/AutofocusMeanMovement',100);
    config.SetValue('/StarAnalysis/AutofocusDynamicNumPoint',AutofocusDynamicNumPoint);
    config.SetValue('/StarAnalysis/AutofocusDynamicMovement',AutofocusDynamicMovement);
    config.DeleteValue('/StarAnalysis/AutofocusMeanNumPoint');
    config.DeleteValue('/StarAnalysis/AutofocusMeanMovement');
    SaveConfig;
  end;
  if oldver<'0.9.24' then begin
    msg:='This version add Gain control for the camera that support this option.'+crlf+
         'Please be careful of the default value for the Gain in Preview, Capture and Sequences tools';
    NewMessage(msg,1);
    MessageDlg(caption,msg,mtWarning,[mbOK],0);
  end;
  if oldver<'0.9.27' then begin
     f:=config.GetValue('/Astrometry/MinRadius',NullCoord);
     if f<>NullCoord then
        config.SetValue('/Astrometry/MaxRadius',f)
  end;
  if (oldver<'0.9.53') then begin
   // move screen layout to own config
   if (screenconfig.GetValue('/Configuration/Version','')='')  then begin
     // copy only the layout of the first opened old config
     screenconfig.SetValue('/Window/Top',config.GetValue('/Window/Top',0));
     screenconfig.SetValue('/Window/Left',config.GetValue('/Window/Left',0));
     screenconfig.SetValue('/Window/Width',config.GetValue('/Window/Width',1024));
     screenconfig.SetValue('/Window/Height',config.GetValue('/Window/Height',768));
     screenconfig.SetValue('/Tools/Clock/Visible',config.GetValue('/Tools/Clock/Visible',true));
     screenconfig.SetValue('/Configuration/Version',ccdcielver);
     movetoolconfig('Histogram',PanelBottom);
     movetoolconfig('Messages',PanelBottom);
     movetoolconfig('Connection',PanelRight1);
     movetoolconfig('Preview',PanelRight1);
     movetoolconfig('Autoguider',PanelRight1);
     movetoolconfig('Planetarium',PanelRight1);
     movetoolconfig('Script',PanelRight1);
     movetoolconfig('Dome',PanelRight1);
     movetoolconfig('Weather',PanelRight1);
     movetoolconfig('Safety',PanelRight1);
     movetoolconfig('Focuser',PanelRight2);
     movetoolconfig('Starprofile',PanelRight2);
     movetoolconfig('Magnifyer',PanelRight2);
     movetoolconfig('Capture',PanelRight3);
     movetoolconfig('Filters',PanelRight3);
     movetoolconfig('Frame',PanelRight3);
     movetoolconfig('Rotator',PanelRight3);
     movetoolconfig('CCDTemp',PanelRight3);
     movetoolconfig('Mount',PanelRight3);
     movetoolconfig('Sequence',PanelRight4);
     movetoolconfig('Video',PanelRight5);
     screenconfig.Flush;
   end;
   config.SetValue('/Script/ScriptName',config.GetValue('/Tools/Script/ScriptName',''));
   config.SetValue('/Log/LogLevel',config.GetValue('/Tools/Messages/LogLevel',LogLevel));
   // delete old config path
   config.DeletePath('/Tools/Script/ScriptName');
   config.DeletePath('/Tools/Messages/LogLevel');
   config.DeletePath('/Tools');
   config.DeletePath('/Window');
   config.Flush;
  end;
  if oldver<'0.9.57' then begin
     // hide gain if not used
     i:=StrToIntDef(config.GetValue('/Preview/Gain',''),0);
     i:=i+StrToIntDef(config.GetValue('/Capture/Gain',''),0);
     ok:=(i=0);
     config.SetValue('/Sensor/GainFromCamera',ok);
  end;
  if oldver<'0.9.65' then begin
     bm:=TBayerMode(config.GetValue('/Color/BayerMode',4));
     case bm of
       bayerGR: config.SetValue('/Color/BayerMode',ord(bayerBG));
       bayerRG: config.SetValue('/Color/BayerMode',ord(bayerGB));
       bayerBG: config.SetValue('/Color/BayerMode',ord(bayerGR));
       bayerGB: config.SetValue('/Color/BayerMode',ord(bayerRG));
     end;
  end;
  if oldver<'0.9.66' then begin
   buf:=config.GetValue('/Files/SaveBitmapFormat','png');
   if TryStrToInt(buf,i) then begin
     // convert old numeric format
     case i of
       0: buf:='png';
       1: buf:='jpg';
       2: buf:='bmp';
       else buf:='png';
     end;
     config.SetValue('/Files/SaveBitmapFormat',buf);
   end;
  end;
  if oldver<'0.9.67' then begin
     // Move BPM to own file
     bpmNum:=config.GetValue('/BadPixelMap/Count',0);
     bpmX:=config.GetValue('/BadPixelMap/CCDWidth',0);
     bpmY:=config.GetValue('/BadPixelMap/CCDHeight',0);
     bpmAxis:=config.GetValue('/BadPixelMap/CCDAxis',0);
     for i:=1 to bpmnum do begin
       bpm[i,1]:=round(config.GetValue('/BadPixelMap/BPMX'+IntToStr(i),0));
       bpm[i,2]:=round(config.GetValue('/BadPixelMap/BPMY'+IntToStr(i),0));
     end;
     bpmconfig.DeletePath('/BadPixelMap/');
     bpmconfig.SetValue('/BadPixelMap/Count',bpmNum);
     bpmconfig.SetValue('/BadPixelMap/CCDWidth',bpmX);
     bpmconfig.SetValue('/BadPixelMap/CCDHeight',bpmY);
     bpmconfig.SetValue('/BadPixelMap/CCDAxis',bpmAxis);
     for i:=1 to bpmNum do begin
       bpmconfig.SetValue('/BadPixelData/BPMX'+IntToStr(i),bpm[i,1]);
       bpmconfig.SetValue('/BadPixelData/BPMY'+IntToStr(i),bpm[i,2]);
     end;
     config.DeletePath('/BadPixelMap/');
     // new ssl/tls option
     buf:=trim(emailconfig.GetValue('/SMTP/Port',''));
     emailconfig.SetValue('/SMTP/SSLTLS',(buf<>'25'));
  end;
  if oldver<'0.9.70' then begin
     // reset default star detection window size
     config.SetValue('/StarAnalysis/Window',60);     ;
  end;
  if oldver<'0.9.73' then begin
    ok:=(not config.GetValue('/Sensor/GainFromCamera',true));
    config.SetValue('/Sensor/CanSetGain',ok);
    config.DeleteValue('/Sensor/GainFromCamera');
    msg:='This version add Offset control for the camera that support this option.'+crlf+
         'Please be careful to review and set the value for the Offset in Preview, Capture and Sequences tools,'+crlf+
         'and in the Autofocus and Slewing preferences.';
    NewMessage(msg,1);
    MessageDlg(caption,msg,mtWarning,[mbOK],0);
  end;
  if config.Modified then
     SaveConfig;
except
  on E: Exception do NewMessage('Error upgrading configuration: '+ E.Message,1);
end;
end;

procedure Tf_main.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if AppClose then exit;
  AppClose:=true;

  SaveSettings;
  SaveConfig;

  TerminateVcurve:=true;
  try
  if autoguider.Running then begin
    autoguider.Disconnect;
    autoguider.Terminate;
  end else begin
    autoguider.Terminate;
    autoguider.Connect('','');
  end;
  except
  end;
  try
  if planetarium.Running then begin
    planetarium.Disconnect;
  end else begin
    planetarium.Terminate;
    planetarium.Connect('','');
  end;
  except
  end;
  try
  if astrometry.Busy then begin
    astrometry.StopAstrometry;
  end;
  except
  end;
  wait(2); // time for other thread to terminate
  astrometry.Free;
  CloseAction:=caFree;
end;

procedure Tf_main.FormDestroy(Sender: TObject);
var i: integer;
begin
  try
  DestroyDevices;
  ImaBmp.Free;
  refbmp.Free;
  config.Free;
  screenconfig.Free;
  credentialconfig.Free;
  emailconfig.Free;
  bpmconfig.Free;
  ScrBmp.Free;
  FreeAndNil(FilterList);
  FreeAndNil(BinningList);
  ReadoutList.Free;
  ISOList.Free;
  deepstring.Free;
  ManualFilterNames.Free;
  for i:=1 to MaxScriptDir do ScriptDir[i].Free;
  if NeedRestart then begin
     ExecNoWait(paramstr(0));
     NewMessage('Program restart',1);
  end
  else NewMessage('Program exit',1);
  CloseLog;
  AllMsg.Free;
  {$ifndef lclqt}{$ifndef lclqt5}{$ifndef lclcocoa}
  if CursorImage1 <> nil then CursorImage1.Free;
  {$endif}{$endif}{$endif}
  except
  end;
end;

procedure Tf_main.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
   fext:string;
begin
  fext:=uppercase(extractfileext(FileNames[0]));{take the first file name in the drop list}
  if ((fext='.FIT') or (fext='.FITS') or (fext='.FTS') or (fext='.FZ')) then
     LoadFitsFile(FileNames[0]) {load fits file}
  else
     LoadPictureFile(FileNames[0]); {load picture file}
end;

procedure Tf_main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F1 : TBConnect.Click;
    VK_F2 : TBFocus.Click;
    VK_F3 : TBCapture.Click;
    VK_F4 : TBSequence.Click;
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
   if f_photometry.Visible then begin
      MeasureAtPos(Mx,My,true);
   end
   else begin
      Screen2fits(Mx,My,f_visu.FlipHorz,f_visu.FlipVert,x,y);
      f_starprofile.ShowProfile(fits,x,y,Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
   end;
   Image1.Invalidate;
 end;
end;

procedure Tf_main.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
MouseDownX:=X;
MouseDownY:=Y;
if Shift=[ssLeft] then begin
  if PolarAlignmentOverlay and (not PolarAlignmentLock) then begin
     Screen2Fits(X,Y,false,false,Polx,Poly);
     PolarMoving:=true;
  end
  else if (ImgZoom>0) then begin
     Mx:=X;
     My:=y;
     MouseMoving:=true;
     screen.Cursor:=crHandPoint;
  end;
end
else if (ssCtrl in Shift) then begin
  if (ImgZoom>0) then begin
     Mx:=X;
     My:=y;
     MouseMoving:=true;
     screen.Cursor:=crHandPoint;
  end;
end
else if (ssShift in Shift)and(not (f_capture.Running or f_preview.Running)) then begin
   if EndX>0 then begin
      scrbmp.Rectangle(StartX,StartY,EndX,EndY,BGRAWhite,dmXor);
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
var px,py,dx,dy: integer;
    z: double;
begin
 MagnifyerTimer.Enabled:=true;
 if PolarMoving  and fits.HeaderInfo.valid then begin
    Screen2Fits(X,Y,false,false,px,py);
    dx:=px-PolX;
    dy:=py-PolY;
    Polx:=px;
    Poly:=py;
    if f_visu.FlipHorz then
      PolarAlignmentOverlayOffsetX:=PolarAlignmentOverlayOffsetX - dx
    else
      PolarAlignmentOverlayOffsetX:=PolarAlignmentOverlayOffsetX + dx;
    if f_visu.FlipVert then
      PolarAlignmentOverlayOffsetY:=PolarAlignmentOverlayOffsetY - dy
    else
      PolarAlignmentOverlayOffsetY:=PolarAlignmentOverlayOffsetY + dy;
    Image1.Invalidate;
 end
 else if MouseMoving and fits.HeaderInfo.valid then begin
    if f_visu.FlipHorz then
      ImgCx:=ImgCx - (X-Mx) / ImgZoom
    else
      ImgCx:=ImgCx + (X-Mx) / ImgZoom;
    if f_visu.FlipVert then
      ImgCy:=ImgCy - (Y-My) / ImgZoom
    else
      ImgCy:=ImgCy + (Y-My) / ImgZoom;
    PlotTimer.Enabled:=true;
 end
 else if MouseFrame then begin
    if EndX>0 then begin
       scrbmp.Rectangle(StartX,StartY,EndX,EndY,BGRAWhite,dmXor);
    end;
    EndX:=X;
    EndY:=Y;
    scrbmp.Rectangle(StartX,StartY,EndX,EndY,BGRAWhite,dmXor);
    image1.Invalidate;
 end
 else if (fits.HeaderInfo.naxis1>0)and(ImgScale0<>0) then begin
   MeasureTimer.Enabled:=true;
 end;
Mx:=X;
My:=Y;
end;

procedure Tf_main.PlotTimerTimer(Sender: TObject);
begin
  if LockTimerPlot then exit;
  PlotTimer.Enabled:=false;
  LockTimerPlot:=true;
  PlotImage;
  LockTimerPlot:=false;
end;

procedure Tf_main.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xx,x1,y1,x2,y2,w,h: integer;
begin
if PolarMoving then begin
    Mx:=X;
    My:=Y;
end;
if MouseMoving and fits.HeaderInfo.valid then begin
    ImgCx:=ImgCx + (X-Mx) / ImgZoom;
    ImgCy:=ImgCy + (Y-My) / ImgZoom;
    PlotImage;
    Mx:=X;
    My:=Y;
end;
if MouseFrame and fits.HeaderInfo.valid then begin
  Image1.Canvas.Pen.Color:=clBlack;
  Image1.Canvas.Pen.Mode:=pmCopy;
  EndX:=X;
  EndY:=Y;
  Screen2CCD(StartX,StartY,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,x1,y1);
  Screen2CCD(EndX,EndY,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,x2,y2);
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
PolarMoving:=false;
screen.Cursor:=crDefault;
end;

procedure Tf_main.Image1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  zf,r1,r2: double;
begin
if (fits.HeaderInfo.naxis>0) then begin
  if LockMouseWheel then
    exit;
  LockMouseWheel := True;
  try
    handled := True;
    if wheeldelta > 0 then
      zf := 1.25
    else
      zf := 0.8;
    if ImgZoom=0 then begin
      r1:=ScrBmp.Width/imabmp.Width;
      r2:=ScrBmp.Height/imabmp.Height;
      ImgZoom:=minvalue([r1,r2]);
    end;
    ImgZoom:=ImgZoom*zf;
    if ImgZoom>ZoomMax then ImgZoom:=ZoomMax;
    if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
    f_visu.BtnZoomAdjust.Down:=false;
    f_visu.BtnZoom05.Down:=false;
    f_visu.BtnZoom1.Down:=false;
    f_visu.BtnZoom2.Down:=false;
    PlotImage;
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
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
end;

procedure Tf_main.CameraConnectTimerTimer(Sender: TObject);
begin
  CameraConnectTimer.Enabled:=false;
  if not CameraInitialized then begin
    //Thing to do after camera is connected
    CameraInitialized:=true;
    ShowTemperatureRange;
    ShowExposureRange;
    ShowBinningRange;
    ShowGain;
    ShowFrameRange;
    ShowFnumber;
  end;
end;

procedure Tf_main.FocuserConnectTimerTimer(Sender: TObject);
begin
  FocuserConnectTimer.Enabled:=false;
  //Thing to do after focuser is connected
  SetFocusMode;
end;

procedure Tf_main.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
if AppClose then exit;
if (camera.Status<>devDisconnected)and(ConfirmClose) then begin
   CanClose:=(MessageDlg(rsTheCameraIsC, mtConfirmation, mbYesNo, 0)=mrYes);
end else begin
   CanClose:=true;
end;
if CanClose then begin
 TerminateVcurve:=true;
 if f_capture.Running or f_preview.Running then begin
   StopExposure(nil);
 end;
 if f_video.Running then begin
  Camera.StopVideoPreview;
 end;
 if f_sequence.Running then f_sequence.AbortSequence;
 f_script.RunShutdownScript;
 if (TCPDaemon<>nil) then StopServer;
 NewMessage(rsDisconnectin+blank+ellipsis,1);
 Disconnect(nil);
 wait(1);
end;
end;

procedure Tf_main.Image1Resize(Sender: TObject);
begin
 ImageResizeTimer.Enabled:=true;
end;

procedure Tf_main.ImageResizeTimerTimer(Sender: TObject);
begin
  ImageResizeTimer.Enabled:=false;
  ScrWidth:=Image1.Width;
  ScrHeigth:=Image1.Height;
  ScrBmp.SetSize(ScrWidth,ScrHeigth);
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

procedure Tf_main.CreateBPM(f: TFits);
var lb,val,val1,val2: double;
    x,y,i: integer;
begin
    lb:=f.imageMean+BPMsigma*f.imageSigma;
    if lb>MAXWORD then lb:=MAXWORD/2;
    bpmNum:=0;
    bpmX:=f.HeaderInfo.naxis1;
    bpmY:=f.HeaderInfo.naxis2;
    bpmAxis:=f.HeaderInfo.naxis;
    for x:=0 to f.HeaderInfo.naxis1-1 do begin
       for y:=0 to f.HeaderInfo.naxis2-1 do begin
          val:=f.imageMin+f.image[0,y,x]/f.imageC;
          if f.HeaderInfo.naxis=3 then begin
            val1:=f.imageMin+f.image[1,y,x]/f.imageC;
            val2:=f.imageMin+f.image[2,y,x]/f.imageC;
            val:=maxvalue([val,val1,val2]);
          end;
          if val>lb then begin
             if bpmnum<BPMMax then begin
               inc(bpmNum);
               bpm[bpmnum,1]:=x;
               bpm[bpmnum,2]:=y;
             end;
          end;
       end;
    end;
    if bpmnum<BPMMax then
       NewMessage(Format(rsBadPixelDete, [inttostr(bpmNum)]),1)
    else begin
       NewMessage(rsTooManyHotPi,1);
       NewMessage(rsPleaseIncrea,1);
    end;
    bpmconfig.DeletePath('/BadPixelMap/');
    bpmconfig.DeletePath('/BadPixelData/');
    bpmconfig.SetValue('/BadPixelMap/Count',bpmNum);
    bpmconfig.SetValue('/BadPixelMap/CCDWidth',bpmX);
    bpmconfig.SetValue('/BadPixelMap/CCDHeight',bpmY);
    bpmconfig.SetValue('/BadPixelMap/CCDAxis',bpmAxis);
    for i:=1 to bpmnum do begin
      bpmconfig.SetValue('/BadPixelData/BPMX'+IntToStr(i),bpm[i,1]);
      bpmconfig.SetValue('/BadPixelData/BPMY'+IntToStr(i),bpm[i,2]);
    end;
    SaveConfig;
end;

procedure Tf_main.MenuBPMClick(Sender: TObject);
var bin: integer;
begin
f_pause.Caption:=rsBadPixelMap;
f_pause.Text:=rsCoverTheCame+crlf+rsClickContinu;
if f_pause.Wait then begin
  bin:=f_preview.Bin;
  camera.ResetFrame;
  fits.SetBPM(bpm,0,0,0,0);
  fits.DarkOn:=false;
  fits.DisableBayer:=true;
  try
  if f_preview.ControlExposure(f_preview.Exposure,bin,bin,DARK,ReadoutModeCapture,f_preview.Gain,f_preview.Offset) then begin
    CreateBPM(fits);
  end
  else
    NewMessage(rsExposureFail,1);
  finally
   fits.DisableBayer:=false;
  end;
end;
end;

procedure Tf_main.MenuBPMDarkClick(Sender: TObject);
var fn : string;
begin
  OpenDialog1.Title:=rsOpenDarkFile;
  if OpenDialog1.Execute then begin
    fn:=OpenDialog1.FileName;
    fits.SetBPM(bpm,0,0,0,0);
    fits.DarkOn:=false;
    fits.DisableBayer:=true;
    try
    fits.LoadFromFile(fn);
    if fits.HeaderInfo.valid then begin
      DrawHistogram(true,true);
      DrawImage;
      wait(2);
      CreateBPM(fits);
      MenuApplyBPMClick(Sender);
    end
    else begin
      NewMessage(Format(rsInvalidOrUns, [fn]),1);
    end;
    finally
      fits.DisableBayer:=false;
    end;
  end;
end;

procedure Tf_main.MenuClearBPMClick(Sender: TObject);
begin
  if MessageDlg(rsDestroyAllBa, mtConfirmation, mbYesNo, 0)=mrYes then begin
    bpmNum:=0;
    bpmX:=0;
    bpmY:=0;
    bpmAxis:=0;
    NewMessage(rsBadPixelMapC,1);
    fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
    bpmconfig.DeletePath('/BadPixelMap/');
    bpmconfig.DeletePath('/BadPixelData/');
    bpmconfig.SetValue('/BadPixelMap/Count',bpmNum);
    bpmconfig.SetValue('/BadPixelMap/CCDWidth',bpmX);
    bpmconfig.SetValue('/BadPixelMap/CCDHeight',bpmY);
    bpmconfig.SetValue('/BadPixelMap/CCDAxis',bpmAxis);
    SaveConfig;
  end;
end;

procedure Tf_main.LoadBPM;
var i:integer;
begin
 bpmNum:=bpmconfig.GetValue('/BadPixelMap/Count',0);
 bpmX:=bpmconfig.GetValue('/BadPixelMap/CCDWidth',0);
 bpmY:=bpmconfig.GetValue('/BadPixelMap/CCDHeight',0);
 bpmAxis:=bpmconfig.GetValue('/BadPixelMap/CCDAxis',0);
 for i:=1 to bpmnum do begin
   bpm[i,1]:=round(bpmconfig.GetValue('/BadPixelData/BPMX'+IntToStr(i),0));
   bpm[i,2]:=round(bpmconfig.GetValue('/BadPixelData/BPMY'+IntToStr(i),0));
 end;
end;

procedure Tf_main.MenuApplyBPMClick(Sender: TObject);
var hasBPM:boolean;
begin
 if fits.BPMProcess then exit; // already applied
 hasBPM:=fits.hasBPM;
 if not hasBPM then
    fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
 fits.LoadStream;
 DrawImage;
 if not hasBPM then
    fits.SetBPM(bpm,0,0,0,0);
end;

procedure Tf_main.MenuDarkApplyClick(Sender: TObject);
begin
 if fits.DarkProcess then exit; // already applied
 try
 fits.DarkOn:=true;
 fits.LoadStream;
 DrawHistogram(true,false);
 DrawImage;
 finally
 fits.DarkOn:=false;
 end;
end;

procedure Tf_main.MenuDarkCameraClick(Sender: TObject);
var bin: integer;
begin
 f_pause.Caption:='Dark frame';
 f_pause.Text:=rsCoverTheCame+crlf+rsClickContinu;
 if f_pause.Wait then begin
   bin:=f_preview.Bin;
   camera.ResetFrame;
   fits.SetBPM(bpm,0,0,0,0);
   fits.DarkOn:=false;
   if f_preview.ControlExposure(f_preview.Exposure,bin,bin,DARK,ReadoutModeCapture,f_preview.Gain,f_preview.Offset) then begin
     fits.SaveToFile(ConfigDarkFile);
     if fits.DarkFrame=nil then begin
        fits.DarkFrame:=TFits.Create(nil);
        fits.DarkFrame.onMsg:=@NewMessage;
     end;
     fits.DarkFrame.LoadFromFile(ConfigDarkFile);
   end
   else
     NewMessage(rsExposureFail,1);
 end;
end;

procedure Tf_main.MenuDarkClearClick(Sender: TObject);
begin
  fits.FreeDark;
  DeleteFile(ConfigDarkFile);
end;

procedure Tf_main.MenuDarkFileClick(Sender: TObject);
var fn : string;
begin
  OpenDialog1.Title:=rsOpenDarkFile;
  if OpenDialog1.Execute then begin
    fn:=OpenDialog1.FileName;
    fits.SetBPM(bpm,0,0,0,0);
    fits.DarkOn:=false;
    if fits.DarkFrame=nil then begin
       fits.DarkFrame:=TFits.Create(nil);
       fits.DarkFrame.onMsg:=@NewMessage;
    end;
    fits.DarkFrame.LoadFromFile(fn);
    if fits.DarkFrame.HeaderInfo.valid then begin
      fits.DarkFrame.SaveToFile(ConfigDarkFile);
    end
    else begin
      fits.FreeDark;
      NewMessage(Format(rsInvalidOrUns, [fn]),1);
    end;
  end;
end;

procedure Tf_main.MenuConnectClick(Sender: TObject);
begin
  f_devicesconnection.BtnConnect.Click;
end;

procedure Tf_main.MenuCaptureStartClick(Sender: TObject);
begin
  f_capture.BtnStartClick(Sender);
end;

procedure Tf_main.MenuCCDtempSetClick(Sender: TObject);
begin
  SetTemperature(Sender);
end;

procedure Tf_main.SetConfig;
var defautindiserver, defaultindiport: string;
    i,n: integer;
begin
 // Upgrade old config with single server
defautindiserver:=config.GetValue('/INDI/Server','localhost');
defaultindiport:=config.GetValue('/INDI/ServerPort','7624');
if config.GetValue('/INDIcamera/Server','')='' then begin
   config.SetValue('/INDIcamera/Server',defautindiserver);
   config.SetValue('/INDIcamera/ServerPort',defaultindiport);
   config.SetValue('/INDIwheel/Server',defautindiserver);
   config.SetValue('/INDIwheel/ServerPort',defaultindiport);
   config.SetValue('/INDIfocuser/Server',defautindiserver);
   config.SetValue('/INDIfocuser/ServerPort',defaultindiport);
   config.SetValue('/INDIrotator/Server',defautindiserver);
   config.SetValue('/INDIrotator/ServerPort',defaultindiport);
   config.SetValue('/INDImount/Server',defautindiserver);
   config.SetValue('/INDImount/ServerPort',defaultindiport);
   config.SetValue('/INDIdome/Server',defautindiserver);
   config.SetValue('/INDIdome/ServerPort',defaultindiport);
   config.SetValue('/INDIwatchdog/Server',defautindiserver);
   config.SetValue('/INDIwatchdog/ServerPort',defaultindiport);
   config.SetValue('/INDIweather/Server',defautindiserver);
   config.SetValue('/INDIweather/ServerPort',defaultindiport);
   config.SetValue('/INDIsafety/Server',defautindiserver);
   config.SetValue('/INDIsafety/ServerPort',defaultindiport);
   config.SetValue('/INDIswitch/Server',defautindiserver);
   config.SetValue('/INDIswitch/ServerPort',defaultindiport);
   config.SetValue('/INDIcover/Server',defautindiserver);
   config.SetValue('/INDIcover/ServerPort',defaultindiport);
end;
case camera.CameraInterface of
   INDI : CameraName:=config.GetValue('/INDIcamera/Device','');
   ASCOM: CameraName:=config.GetValue('/ASCOMcamera/Device','');
   ASCOMREST: CameraName:='Camera/'+IntToStr(config.GetValue('/ASCOMRestcamera/Device',0));
end;
case wheel.WheelInterface of
   INCAMERA: WheelName:=CameraName;
   INDI : WheelName:=config.GetValue('/INDIwheel/Device','');
   ASCOM: WheelName:=config.GetValue('/ASCOMwheel/Device','');
   ASCOMREST: WheelName:='FilterWheel/'+IntToStr(config.GetValue('/ASCOMRestwheel/Device',0));
   MANUAL: WheelName:=rsManual;
end;
case focuser.FocuserInterface of
   INDI : FocuserName:=config.GetValue('/INDIfocuser/Device','');
   ASCOM: FocuserName:=config.GetValue('/ASCOMfocuser/Device','');
   ASCOMREST: FocuserName:='Focuser/'+IntToStr(config.GetValue('/ASCOMRestfocuser/Device',0));
end;
case rotator.RotatorInterface of
   INDI : RotatorName:=config.GetValue('/INDIrotator/Device','');
   ASCOM: RotatorName:=config.GetValue('/ASCOMrotator/Device','');
   ASCOMREST: RotatorName:='Rotator/'+IntToStr(config.GetValue('/ASCOMRestrotator/Device',0));
end;
case mount.MountInterface of
   INDI : MountName:=config.GetValue('/INDImount/Device','');
   ASCOM: MountName:=config.GetValue('/ASCOMmount/Device','');
   ASCOMREST: MountName:='Telescope/'+IntToStr(config.GetValue('/ASCOMRestmount/Device',0));
end;
case dome.DomeInterface of
   INDI : DomeName:=config.GetValue('/INDIdome/Device','');
   ASCOM: DomeName:=config.GetValue('/ASCOMdome/Device','');
   ASCOMREST: DomeName:='Dome/'+IntToStr(config.GetValue('/ASCOMRestdome/Device',0));
end;
case weather.WeatherInterface of
   INDI : WeatherName:=config.GetValue('/INDIweather/Device','');
   ASCOM: WeatherName:=config.GetValue('/ASCOMweather/Device','');
   ASCOMREST: begin
              if config.GetValue('/ASCOMRestweather/DeviceType',0)=0 then
               WeatherName:='ObservingConditions/'+IntToStr(config.GetValue('/ASCOMRestweather/Device',0))
              else
               WeatherName:='SafetyMonitor/'+IntToStr(config.GetValue('/ASCOMRestweather/Device',0));
              end;
end;
case safety.SafetyInterface of
   INDI : SafetyName:=config.GetValue('/INDIsafety/Device','');
   ASCOM: SafetyName:=config.GetValue('/ASCOMsafety/Device','');
   ASCOMREST: SafetyName:='SafetyMonitor/'+IntToStr(config.GetValue('/ASCOMRestsafety/Device',0));
end;
case switch.SwitchInterface of
   INDI : SwitchName:=config.GetValue('/INDIswitch/Device','');
   ASCOM: SwitchName:=config.GetValue('/ASCOMswitch/Device','');
   ASCOMREST: SwitchName:='Switch/'+IntToStr(config.GetValue('/ASCOMRestswitch/Device',0));
end;
case cover.CoverInterface of
   INDI : CoverName:=config.GetValue('/INDIcover/Device','');
   ASCOM: CoverName:=config.GetValue('/ASCOMcover/Device','');
   ASCOMREST: CoverName:='CoverCalibrator/'+IntToStr(config.GetValue('/ASCOMRestcover/Device',0));
end;
DeviceTimeout:=config.GetValue('/Devices/Timeout',100);
camera.Timeout:=DeviceTimeout;
focuser.Timeout:=DeviceTimeout;
rotator.Timeout:=DeviceTimeout;
wheel.Timeout:=DeviceTimeout;
mount.Timeout:=DeviceTimeout;
dome.Timeout:=DeviceTimeout;
weather.Timeout:=DeviceTimeout;
safety.Timeout:=DeviceTimeout;
switch.Timeout:=DeviceTimeout;
cover.Timeout:=DeviceTimeout;
wheel.AutoLoadConfig:=config.GetValue('/INDIwheel/AutoLoadConfig',false);
focuser.AutoLoadConfig:=config.GetValue('/INDIfocuser/AutoLoadConfig',false);
rotator.AutoLoadConfig:=config.GetValue('/INDIrotator/AutoLoadConfig',false);
mount.AutoLoadConfig:=config.GetValue('/INDImount/AutoLoadConfig',false);
dome.AutoLoadConfig:=config.GetValue('/INDIdome/AutoLoadConfig',false);
camera.AutoLoadConfig:=config.GetValue('/INDIcamera/AutoLoadConfig',false);
if camera.CameraInterface=ASCOM then
   camera.ASCOMFlipImage:=config.GetValue('/ASCOMcamera/FlipImage',true);
if camera.CameraInterface=ASCOMREST then
   camera.ASCOMFlipImage:=config.GetValue('/ASCOMRestcamera/FlipImage',true);
if wheel.WheelInterface=MANUAL then begin
   ManualFilterNames.Clear;
   ManualFilterNames.Add(rsFilter0);
   n:=config.GetValue('/Manualwheel/Slots',5);
   for i:=1 to n do begin
     ManualFilterNames.Add(config.GetValue('/Manualwheel/Slot'+inttostr(i),''));
   end;
   wheel.FilterNames:=ManualFilterNames;
   FilterNameChange(Self);
end;
weather.AutoLoadConfig:=config.GetValue('/INDIweather/AutoLoadConfig',false);
safety.AutoLoadConfig:=config.GetValue('/INDIsafety/AutoLoadConfig',false);
switch.AutoLoadConfig:=config.GetValue('/INDIswitch/AutoLoadConfig',false);
cover.AutoLoadConfig:=config.GetValue('/INDIcover/AutoLoadConfig',false);
if watchdog<>nil then begin
  watchdog.Timeout:=DeviceTimeout;
  WatchdogName:=config.GetValue('/INDIwatchdog/Device','');
  watchdog.AutoLoadConfig:=config.GetValue('/INDIwatchdog/AutoLoadConfig',false);
end;
end;

procedure Tf_main.SetOptions;
var i,n: integer;
    buf,v,str: string;
    ok: boolean;
    oldbayer: TBayerMode;
    oldRed,oldGreen,oldBlue:double;
    oldBalance, oldBGneutralization:boolean;
    posprev,poscapt:integer;
    binprev,bincapt:string;
begin
  ShowHint:=screenconfig.GetValue('/Hint/Show',true);
  if f_option<>nil then f_option.ShowHint:=ShowHint;
  if f_setup<>nil then f_setup.ShowHint:=ShowHint;
  if f_EditTargets<>nil then f_EditTargets.ShowHint:=ShowHint;
  if f_vcurve<>nil then f_vcurve.ShowHint:=ShowHint;
  TmpDir:=config.GetValue('/Files/TmpDir',TmpDir);
  if copy(TmpDir,1,1)='.' then TmpDir:=ExpandFileName(slash(Appdir)+TmpDir);
  if not DirectoryExistsUTF8(TmpDir) then begin
    ok:=CreateDirUTF8(TmpDir);
    if not ok then NewMessage('Cannot create directory '+TmpDir,1);
  end;
  if pos(' ', TmpDir)>0 then NewMessage(rsPleaseSelect2,1);
  PythonCmd:=config.GetValue('/Script/PythonCmd',defPython);
  {$ifdef mswindows}
  // reset python path when switching between 32 and 64 bit version
  if (pos('CCDCIEL\SCRIPTS',uppercase(PythonCmd))>0)and(not FileExists(PythonCmd)) then  begin
    PythonCmd:=defPython;
    config.SetValue('/Script/PythonCmd',defPython);
  end;
  {$endif}
  TCPIPConfigPort:=config.GetValue('/Files/TCPIPConfigPort','3277');
  SaveBitmap:=config.GetValue('/Files/SaveBitmap',false);
  SaveBitmapFormat:=config.GetValue('/Files/SaveBitmapFormat','png');
  OpenPictureDialog1.InitialDir:=config.GetValue('/Files/CapturePath',defCapturePath);
  f_video.VideoCaptureDir.Text:=config.GetValue('/Files/VideoCapturePath','/tmp');
  ObsLatitude:=config.GetValue('/Info/ObservatoryLatitude',0.0);
  ObsLongitude:=config.GetValue('/Info/ObservatoryLongitude',0.0);
  ObsElevation:=config.GetValue('/Info/ObservatoryElevation',0.0);
  BayerColor:=config.GetValue('/Color/Bayer',false);
  oldbayer:=DefaultBayerMode;
  oldRed:=RedBalance;
  oldGreen:=GreenBalance;
  oldBlue:=BlueBalance;
  oldBalance:=BalanceFromCamera;
  oldBGneutralization:=BGneutralization;
  DefaultBayerMode:=TBayerMode(config.GetValue('/Color/BayerMode',4));
  BalanceFromCamera:=config.GetValue('/Color/BalanceFromCamera',true);
  BGneutralization:=config.GetValue('/Color/BGneutralization',true);
  RedBalance:=config.GetValue('/Color/RedBalance',1.0);
  GreenBalance:=config.GetValue('/Color/GreenBalance',0.7);
  BlueBalance:=config.GetValue('/Color/BlueBalance',0.9);
  ClippingOverflow:=config.GetValue('/Color/ClippingOverflow',MAXWORD);
  ClippingUnderflow:=config.GetValue('/Color/ClippingUnderflow',0);
  ok:=config.GetValue('/Sensor/CanSetGain',false);
  if ok<>camera.CanSetGain then begin
    camera.CanSetGain:=ok;
    Showgain;
  end;
  MaxADU:=config.GetValue('/Sensor/MaxADU',MAXWORD);
  DisplayCapture:=config.GetValue('/Visu/DisplayCapture',DisplayCapture);
  ok:=f_visu.PanelNoDisplay.Visible<>(not DisplayCapture);
  f_visu.PanelNoDisplay.Visible:=not DisplayCapture;
  if ok then f_visu.FrameResize(nil);
  LowQualityDisplay:=config.GetValue('/Visu/LowQualityDisplay',LowQualityDisplay);
  ConfigExpEarlyStart:=config.GetValue('/Sensor/ExpEarlyStart',ConfigExpEarlyStart);
  MeasureNewImage:=config.GetValue('/Files/MeasureNewImage',false) and ConfigExpEarlyStart;
  CheckRecenterTarget:=config.GetValue('/PrecSlew/CheckRecenterTarget',false) and ConfigExpEarlyStart;
  ClippingOverflow:=min(ClippingOverflow,MaxADU);
  reftreshold:=config.GetValue('/RefImage/Treshold',128);
  refcolor:=config.GetValue('/RefImage/Color',0);
  BPMsigma:=config.GetValue('/BadPixel/Sigma',5);
  f_preview.StackPreview.Visible:=config.GetValue('/PreviewStack/StackShow',false);
  f_capture.PanelStack.Visible:=f_preview.StackPreview.Visible;
  MaxVideoPreviewRate:=config.GetValue('/Video/PreviewRate',5);
  i:=TemperatureScale;
  TemperatureScale:=config.GetValue('/Cooler/TemperatureScale',0);
  if TemperatureScale<>i then begin
    if TemperatureScale=0 then begin
       TempLabel:=sdeg+'C';
       f_ccdtemp.Title.Caption:=rsSensorTemperatu+blank+TempLabel;
       f_ccdtemp.Setpoint.Value:=TempCelsius(1,f_ccdtemp.Setpoint.Value);
       f_focuser.lblTemp.Caption:=TempLabel;
    end
    else begin
       TempLabel:=sdeg+'F';
       f_ccdtemp.Title.Caption:=rsSensorTemperatu+blank+TempLabel;
       f_ccdtemp.Setpoint.Value:=TempDisplay(1,f_ccdtemp.Setpoint.Value);
       f_focuser.lblTemp.Caption:=TempLabel;
    end;
    if camera.Status=devConnected then begin
       ShowTemperatureRange;
       CameraTemperatureChange(camera.Temperature);
       CameraCoolerPowerChange(camera.CoolerPower);
    end;
    if (focuser.Status=devConnected) and focuser.hasTemperature then begin
       FocuserTemperatureChange(focuser.Temperature);
    end;
  end;
  TemperatureSlope:=config.GetValue('/Cooler/TemperatureSlope',0);
  ReadoutModeCapture:=config.GetValue('/Readout/Capture',0);
  ReadoutModePreview:=config.GetValue('/Readout/Preview',0);
  ReadoutModeFocus:=config.GetValue('/Readout/Focus',0);
  ReadoutModeAstrometry:=config.GetValue('/Readout/Astrometry',0);
  Starwindow:=config.GetValue('/StarAnalysis/Window',60);
  Focuswindow:=config.GetValue('/StarAnalysis/Focus',400);
  Focuswindow:=max(Focuswindow,4*Starwindow);
  Undersampled:=config.GetValue('/StarAnalysis/Undersampled',false);
  n:=config.GetValue('/Filters/Num',0);
  for i:=0 to MaxFilter do FilterOffset[i]:=0;
  for i:=0 to MaxFilter do FilterExpFact[i]:=1.0;
  for i:=1 to n do begin
     FilterOffset[i]:=trunc(config.GetValue('/Filters/Offset'+IntToStr(i),0));
     FilterExpFact[i]:=config.GetValue('/Filters/ExpFact'+IntToStr(i),1.0);
     if (wheel.Status=devConnected)and(wheel.Filter=i) then CurrentFilterOffset:=FilterOffset[i];
  end;
  CurrentFilterOffset:=0;
  if (wheel.Status=devConnected) then AutofocusExposureFact:=FilterExpFact[wheel.Filter];
  AutoFocusMode:=TAutoFocusMode(config.GetValue('/StarAnalysis/AutoFocusMode',3)); // default to no autofocus
  if AutofocusMode=afPlanet then begin
    f_starprofile.label1.Caption:=rsSharpness+':';
    f_starprofile.Label2.Caption:=rsIntensity+':';
    f_starprofile.Label3.Caption:='';
    f_starprofile.Label4.Caption:='';
  end
  else begin
    f_starprofile.label1.Caption:=rsHFD+':';
    f_starprofile.Label2.Caption:=rsIntensity+':';
    f_starprofile.Label3.Caption:=rsFWHM+':';
    f_starprofile.Label4.Caption:='SNR:';
  end;
  AutofocusMinSpeed:=config.GetValue('/StarAnalysis/AutofocusMinSpeed',500);
  AutofocusMaxSpeed:=config.GetValue('/StarAnalysis/AutofocusMaxSpeed',5000);
  AutofocusStartHFD:=config.GetValue('/StarAnalysis/AutofocusStartHFD',20.0);
  AutofocusNearHFD:=config.GetValue('/StarAnalysis/AutofocusNearHFD',10.0);
  AutofocusExposure:=config.GetValue('/StarAnalysis/AutofocusExposure',5.0);
  AutofocusBinning:=config.GetValue('/StarAnalysis/AutofocusBinning',1);
  focuser.Backlash:=config.GetValue('/StarAnalysis/FocuserBacklash',0);
  focuser.BacklashDirection:=config.GetValue('/StarAnalysis/FocuserBacklashDirection',FocusDirIn);
  focuser.BacklashActive:=config.GetValue('/StarAnalysis/FocuserBacklashActive',(focuser.Backlash<>0));
  f_focuser.BacklashActive:=focuser.BacklashActive;
  FocuserDelay:=config.GetValue('/StarAnalysis/FocuserDelay',0);
  if focuser<>nil then focuser.Delay:=FocuserDelay;
  FocuserTempCoeff:=config.GetValue('/StarAnalysis/FocuserTempCoeff',0.0);
  if abs(FocuserTempCoeff)<0.001 then FocuserTempCoeff:=0;
  AutofocusTempChange:=config.GetValue('/StarAnalysis/AutofocusTemp',0.0);
  if abs(AutofocusTempChange)<0.001 then AutofocusTempChange:=0;
  AutofocusPeriod:=config.GetValue('/StarAnalysis/AutofocusPeriod',0);
  AutofocusMoveDir:=config.GetValue('/StarAnalysis/AutofocusMoveDir',FocusDirIn);
  AutofocusNearNum:=config.GetValue('/StarAnalysis/AutofocusNearNum',3);
  AutofocusInPlace:=config.GetValue('/StarAnalysis/AutofocusInPlace',true);
  if AutofocusInPlace then
     AutofocusPauseGuider:=config.GetValue('/StarAnalysis/AutofocusPauseGuider',true)
  else
     AutofocusPauseGuider:=true;
  if not f_sequence.Running then InplaceAutofocus:=AutofocusInPlace;
  AutofocusMultiStarCenter:=config.GetValue('/StarAnalysis/AutofocusMultiStarCenter',true);
  LoadFocusStar(config.GetValue('/StarAnalysis/AutofocusStarMag',4));
  FocusStarMagAdjust:=config.GetValue('/StarAnalysis/FocusStarMagAdjust',false);
  AutofocusDynamicNumPoint:=config.GetValue('/StarAnalysis/AutofocusDynamicNumPoint',7);
  AutofocusDynamicMovement:=config.GetValue('/StarAnalysis/AutofocusDynamicMovement',100);
  AutofocusPlanetNumPoint:=config.GetValue('/StarAnalysis/AutofocusPlanetNumPoint',AutofocusDynamicNumPoint);
  AutofocusPlanetMovement:=config.GetValue('/StarAnalysis/AutofocusPlanetMovement',AutofocusDynamicMovement);
  AutofocusTolerance:=config.GetValue('/StarAnalysis/AutofocusTolerance',99.0);
  AutofocusMinSNR:=config.GetValue('/StarAnalysis/AutofocusMinSNR',3.0);
  AutofocusSlippageCorrection:=config.GetValue('/StarAnalysis/AutofocusSlippageCorrection',false);
  if AutofocusSlippageCorrection then
     AutofocusSlippageOffset:=config.GetValue('/StarAnalysis/AutofocusSlippageOffset',0)
  else
    AutofocusSlippageOffset:=0;
  AutofocusGain:=config.GetValue('/StarAnalysis/AutofocusGain',f_preview.Gain);
  AutofocusOffset:=config.GetValue('/StarAnalysis/AutofocusOffset',f_preview.Offset);

  MagnitudeCalibration:=config.GetValue('/StarAnalysis/MagnitudeCalibration',MagnitudeCalibration);

  LogToFile:=config.GetValue('/Log/Messages',true);
  if LogToFile<>LogFileOpen then CloseLog;
  if LogToFile then begin
    WriteLog('');
    WriteDeviceLog('');
  end;
  debug_msg:=config.GetValue('/Log/debug_msg',false);
  DitherPixel:=config.GetValue('/Autoguider/Dither/Pixel',1.0);
  DitherRAonly:=config.GetValue('/Autoguider/Dither/RAonly',true);
  DitherWaitTime:=config.GetValue('/Autoguider/Dither/WaitTime',5);
  EarlyDither:=config.GetValue('/Autoguider/Dither/EarlyDither',EarlyDither);
  SettlePixel:=config.GetValue('/Autoguider/Settle/Pixel',1.0);
  SettleMinTime:=config.GetValue('/Autoguider/Settle/MinTime',5);
  SettleMaxTime:=config.GetValue('/Autoguider/Settle/MaxTime',30);
  CalibrationDelay:=config.GetValue('/Autoguider/Settle/CalibrationDelay',300);
  MeridianOption:=config.GetValue('/Meridian/MeridianOption',0);
  MinutesPastMeridian:=config.GetValue('/Meridian/MinutesPast',15);
  MinutesPastMeridianMin:=config.GetValue('/Meridian/MinutesPastMin',10);
  MeridianFlipPauseBefore:=config.GetValue('/Meridian/MeridianFlipPauseBefore',false);
  MeridianFlipPauseAfter:=config.GetValue('/Meridian/MeridianFlipPauseAfter',false);
  MeridianFlipPauseTimeout:=config.GetValue('/Meridian/MeridianFlipPauseTimeout',0);
  MeridianFlipCalibrate:=config.GetValue('/Meridian/MeridianFlipCalibrate',false);
  MeridianFlipAutofocus:=config.GetValue('/Meridian/MeridianFlipAutofocus',false);
  MeridianFlipStopSlaving:=config.GetValue('/Meridian/MeridianFlipStopSlaving',false);
  astrometryResolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
  AstrometryTimeout:=config.GetValue('/Astrometry/Timeout',60.0);
  LastPixelSize:=config.GetValue('/Astrometry/LastPixelSize',1.0);
  buf:=config.GetValue('/Astrometry/OtherOptions','');
  if (astrometryResolver=ResolverAstrometryNet)and(pos('--no-fits2fits',buf)>0) then begin
    v:=AstrometryVersion(astrometryResolver,config.GetValue('/Astrometry/CygwinPath','C:\cygwin'),config.GetValue('/Astrometry/AstrometryPath',''),config.GetValue('/Astrometry/AstUseScript',false));
    if v<>'unknown' then begin
      if v>='0.68' then begin // option --no-fits2fits was removed in version 0.68
         buf:=StringReplace(buf,'--no-fits2fits','',[rfReplaceAll]);
         config.SetValue('/Astrometry/OtherOptions',buf);
      end;
    end;
  end;
  SlewPrecision:=config.GetValue('/PrecSlew/Precision',5.0);
  RecenterTargetDistance:=config.GetValue('/PrecSlew/RecenterTargetDistance',10.0);
  if (autoguider<>nil)and(autoguider.State<>GUIDER_DISCONNECTED) then autoguider.SettleTolerance(SettlePixel,SettleMinTime, SettleMaxTime);
  if refmask then SetRefImage;
  if f_focuser<>nil then f_focuser.BtnVcurve.Visible:=(AutoFocusMode=afVcurve);
  LoadHorizon(config.GetValue('/Info/HorizonFile',''));
  ElevationMin:=config.GetValue('/Info/ElevationMin',10.0);
  FlatType:=TFlatType(config.GetValue('/Flat/FlatType',ord(ftNone)));
  FlatAutoExposure:=config.GetValue('/Flat/FlatAutoExposure',false);
  FlatMinExp:=config.GetValue('/Flat/FlatMinExp',1.0);
  FlatMaxExp:=config.GetValue('/Flat/FlatMaxExp',60.0);
  FlatLevelMin:=config.GetValue('/Flat/FlatLevelMin',20000);
  FlatLevelMax:=config.GetValue('/Flat/FlatLevelMax',30000);
  DomeFlatTelescopeSlew:=config.GetValue('/Flat/DomeFlatTelescopeSlew',false);
  DomeFlatTelescopeAz:=config.GetValue('/Flat/DomeFlatTelescopeAz',90.0);
  DomeFlatTelescopeAlt:=config.GetValue('/Flat/DomeFlatTelescopeAlt',5.0);
  DomeFlatSetLight:=config.GetValue('/Flat/DomeFlatSetLight',false);
  DomeFlatSetLightON:=config.GetValue('/Flat/DomeFlatSetLightON','');
  DomeFlatSetLightOFF:=config.GetValue('/Flat/DomeFlatSetLightOFF','');
  for i:=0 to SubDirCount-1 do begin
    SubDirOpt[i]:=TSubDirList(round(config.GetValue('/Files/SubDirOpt'+inttostr(i),i)));
    SubDirActive[i]:=config.GetValue('/Files/SubDirActive'+inttostr(i),false);
  end;
  for i:=0 to FileNameCount-1 do begin
    FileNameOpt[i]:=TFilenameList(round(config.GetValue('/Files/FileNameOpt'+inttostr(i),i)));
    FileNameActive[i]:=config.GetValue('/Files/FileNameActive'+inttostr(i),i in [0,1,5]);
  end;
  FilenameSep:=config.GetValue('/Files/FileNameSep','_');
  FileSequenceWidth:=config.GetValue('/Files/FileSequenceWidth',0);
  FilePack:=config.GetValue('/Files/Pack',false);
  WantExif:=config.GetValue('/Files/Exif',WantExif);
  if ((TCPDaemon=nil)or(TCPDaemon.stoping)) then
     StartServer
  else if (TCPIPConfigPort<>TCPIPServerPort) then
     RestartServer;
  WeatherRestartDelay:=config.GetValue('/Weather/RestartDelay',5);
  weather.UseCloudCover:=config.GetValue('/Weather/Use/CloudCover',false);
  weather.UseDewPoint:=config.GetValue('/Weather/Use/DewPoint',false);
  weather.UseHumidity:=config.GetValue('/Weather/Use/Humidity',false);
  weather.UsePressure:=config.GetValue('/Weather/Use/Pressure',false);
  weather.UseRainRate:=config.GetValue('/Weather/Use/RainRate',false);
  weather.UseSkyBrightness:=config.GetValue('/Weather/Use/SkyBrightness',false);
  weather.UseSkyQuality:=config.GetValue('/Weather/Use/SkyQuality',false);
  weather.UseSkyTemperature:=config.GetValue('/Weather/Use/SkyTemperature',false);
  weather.UseStarFWHM:=config.GetValue('/Weather/Use/StarFWHM',false);
  weather.UseTemperature:=config.GetValue('/Weather/Use/Temperature',false);
  weather.UseWindDirection:=config.GetValue('/Weather/Use/WindDirection',false);
  weather.UseWindGust:=config.GetValue('/Weather/Use/WindGust',false);
  weather.UseWindSpeed:=config.GetValue('/Weather/Use/WindSpeed',false);
  weather.MinCloudCover:=config.GetValue('/Weather/Min/CloudCover',0);
  weather.MinDewPoint:=config.GetValue('/Weather/Min/DewPoint',0);
  weather.MinHumidity:=config.GetValue('/Weather/Min/Humidity',0);
  weather.MinPressure:=config.GetValue('/Weather/Min/Pressure',0);
  weather.MinRainRate:=config.GetValue('/Weather/Min/RainRate',0);
  weather.MinSkyBrightness:=config.GetValue('/Weather/Min/SkyBrightness',0);
  weather.MinSkyQuality:=config.GetValue('/Weather/Min/SkyQuality',0);
  weather.MinSkyTemperature:=config.GetValue('/Weather/Min/SkyTemperature',0);
  weather.MinStarFWHM:=config.GetValue('/Weather/Min/StarFWHM',0);
  weather.MinTemperature:=config.GetValue('/Weather/Min/Temperature',0);
  weather.MinWindDirection:=config.GetValue('/Weather/Min/WindDirection',0);
  weather.MinWindGust:=config.GetValue('/Weather/Min/WindGust',0);
  weather.MinWindSpeed:=config.GetValue('/Weather/Min/WindSpeed',0);
  weather.MaxCloudCover:=config.GetValue('/Weather/Max/CloudCover',0);
  weather.MaxDewPoint:=config.GetValue('/Weather/Max/DewPoint',0);
  weather.MaxHumidity:=config.GetValue('/Weather/Max/Humidity',0);
  weather.MaxPressure:=config.GetValue('/Weather/Max/Pressure',0);
  weather.MaxRainRate:=config.GetValue('/Weather/Max/RainRate',0);
  weather.MaxSkyBrightness:=config.GetValue('/Weather/Max/SkyBrightness',0);
  weather.MaxSkyQuality:=config.GetValue('/Weather/Max/SkyQuality',0);
  weather.MaxSkyTemperature:=config.GetValue('/Weather/Max/SkyTemperature',0);
  weather.MaxStarFWHM:=config.GetValue('/Weather/Max/StarFWHM',0);
  weather.MaxTemperature:=config.GetValue('/Weather/Max/Temperature',0);
  weather.MaxWindDirection:=config.GetValue('/Weather/Max/WindDirection',0);
  weather.MaxWindGust:=config.GetValue('/Weather/Max/WindGust',0);
  weather.MaxWindSpeed:=config.GetValue('/Weather/Max/WindSpeed',0);
  if (BayerColor<>MenuItemDebayer.Checked)or(oldbayer<>DefaultBayerMode) or (oldBGneutralization<>BGneutralization) or
     (oldRed<>RedBalance)or(oldGreen<>GreenBalance)or(oldBlue<>BlueBalance)or(oldBalance<>BalanceFromCamera)
  then begin
    MenuItemDebayer.Checked:=BayerColor;
    MenuItemDebayer2.Checked:=BayerColor;
    MenuItemDebayerClick(MenuItemDebayer);
  end;
  DomeNoSafetyCheck:=config.GetValue('/Dome/NoSafetyCheck',false);
  mount.SlaveDome:=config.GetValue('/Dome/SlaveToMount',false);
  mount.DomeActionWait:=config.GetValue('/Dome/ActionWait',1);
  for i:=0 to DomeOpenActionNum-1 do begin
    n:=round(config.GetValue('/Dome/Open/Action'+inttostr(i),0));
    mount.DomeOpenActions[i]:=TDomeOpenAction(n);
  end;
  for i:=0 to DomeCloseActionNum-1 do begin
    n:=round(config.GetValue('/Dome/Close/Action'+inttostr(i),0));
    mount.DomeCloseActions[i]:=TDomeCloseAction(n);
  end;
  SMTPHost:=emailconfig.GetValue('/SMTP/Host','');
  SMTPPort:=emailconfig.GetValue('/SMTP/Port','');
  SMTPUser:=DecryptStr(hextostr(emailconfig.GetValue('/SMTP/User','')), encryptpwd);
  SMTPPasswd:=DecryptStr(hextostr(emailconfig.GetValue('/SMTP/Passwd','')), encryptpwd);
  SMTPSSLTLS:=emailconfig.GetValue('/SMTP/SSLTLS',true);
  MailFrom:=emailconfig.GetValue('/Mail/From','');
  MailTo:=emailconfig.GetValue('/Mail/To','');
  EmailEndSequence:=config.GetValue('/Mail/EndSequence',false);
  EmailAbortSequence:=config.GetValue('/Mail/AbortSequence',false);
  EmailAutoguider:=config.GetValue('/Mail/Autoguider',false);
  EmailAufofocus:=config.GetValue('/Mail/Aufofocus',false);
  EmailMeridianFlip:=config.GetValue('/Mail/MeridianFlip',false);
  EmailTargetInitialisation:=config.GetValue('/Mail/TargetInitialisation',false);
  VoiceDialog:=config.GetValue('/Voice/Dialog',false);
  VoiceSequence:=config.GetValue('/Voice/Sequence',false);
  VoiceError:=config.GetValue('/Voice/Error',false);
  VoiceEmail:=config.GetValue('/Voice/Email',false);

  if wheel.Status=devDisconnected then begin
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
    f_EditTargets.StepList.Columns[pcolfilter-1].PickList.Assign(FilterList);
    f_EditTargets.FlatFilterList.Items.Assign(FilterList);
    if f_EditTargets.FlatFilterList.Items.Count>0 then f_EditTargets.FlatFilterList.Items.Delete(0);
    SetFilterMenu;
  end;
  if (f_option<>nil)and(camera.Status=devDisconnected) then begin
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
    SetBinningList(posprev,poscapt);

    n:=config.GetValue('/Readout/Num',0);
    ReadoutList.Clear;
    for i:=0 to n-1 do begin
       str:=config.GetValue('/Readout/Mode'+IntToStr(i),'');
       ReadoutList.Add(str);
    end;
    f_option.ReadOutCapture.Items.Assign(ReadoutList);
    f_option.ReadOutPreview.Items.Assign(ReadoutList);
    f_option.ReadOutFocus.Items.Assign(ReadoutList);
    f_option.ReadOutAstrometry.Items.Assign(ReadoutList);

    hasGain:=config.GetValue('/Gain/hasGain',false);
    hasGainISO:=config.GetValue('/Gain/hasGainISO',false);
    Gain:=config.GetValue('/Gain/Gain',0);
    GainMin:=config.GetValue('/Gain/GainMin',0);
    GainMax:=config.GetValue('/Gain/GainMax',0);
    n:=config.GetValue('/Gain/NumISO',0);
    for i:=0 to n-1 do begin
       str:=config.GetValue('/Gain/ISO'+IntToStr(i),'');
       ISOList.Add(str);
    end;
    hasOffset:=config.GetValue('/Offset/hasOffset',false);
    Offset:=config.GetValue('/Offset/Offset',0);
    OffsetMin:=config.GetValue('/Offset/OffsetMin',0);
    OffsetMax:=config.GetValue('/Offset/OffsetMax',0);
    SetGainList;
    ShowFnumber;
  end;
  if (planetarium.PlanetariumType<>TPlanetariumType(config.GetValue('/Planetarium/Software',ord(plaNONE)))) and (not planetarium.Connected) then begin
     try
     planetarium.Terminate;
     planetarium.Connect('');
     except
     end;
     i:=config.GetValue('/Planetarium/Software',ord(plaNONE));
     case TPlanetariumType(i) of
       CDC: planetarium:=TPlanetarium_cdc.Create;
       SAMP:planetarium:=TPlanetarium_samp.Create;
       HNSKY:planetarium:=TPlanetarium_hnsky.Create;
       plaNONE: planetarium:=TPlanetarium_none.Create;
     end;
     planetarium.onConnect:=@PlanetariumConnect;
     planetarium.onDisconnect:=@PlanetariumDisconnect;
     planetarium.onShowMessage:=@NewMessage;
     f_planetariuminfo.planetarium:=planetarium;
     f_scriptengine.Planetarium:=planetarium;
     f_sequence.Planetarium:=planetarium;
  end;
  if autoguider.AutoguiderType<>TAutoguiderType(config.GetValue('/Autoguider/Software',2)) then begin
    try
    autoguider.Terminate;
    autoguider.Connect('');
    f_sequence.AutoguiderDisconnected;
    except
    end;
    i:=config.GetValue('/Autoguider/Software',2);
    case TAutoguiderType(i) of
      agPHD: autoguider:=T_autoguider_phd.Create;
      agLINGUIDER: autoguider:=T_autoguider_linguider.Create;
      agNONE: autoguider:=T_autoguider_none.Create;
      agDITHER: autoguider:=T_autoguider_dither.Create;
    end;
    autoguider.Mount:=mount;
    autoguider.onStatusChange:=@AutoguiderStatus;
    autoguider.onConnect:=@AutoguiderConnect;
    autoguider.onDisconnect:=@AutoguiderDisconnect;
    autoguider.onShowMessage:=@NewMessage;
    f_sequence.Autoguider:=autoguider;
    f_scriptengine.Autoguider:=autoguider;
    f_script.Autoguider:=autoguider;
    f_autoguider.Status.Text:=autoguider.Status;
    f_autoguider.DitherOnly:=autoguider.AutoguiderType=agDITHER;
    NewMessage(Format(rsAutoguider+': %s', [autoguider.Status]),1);
    f_autoguider.BtnConnect.Caption:=rsConnect;
    f_autoguider.BtnGuide.Caption:='Guide';
    f_autoguider.led.Brush.Color:=clGray;
    MenuAutoguiderConnect.Caption:=f_autoguider.BtnConnect.Caption;
    MenuAutoguiderGuide.Caption:=f_autoguider.BtnGuide.Caption;
    StatusBar1.Invalidate;
  end;
end;

procedure Tf_main.SaveScreenConfig;
begin
  // Set current tools value to the config
 screenconfig.SetValue('/Configuration/Version',ccdcielver);

 screenconfig.SetValue('/Tools/Connection/Parent',f_devicesconnection.Parent.Name);
 screenconfig.SetValue('/Tools/Connection/Visible',f_devicesconnection.Visible);
 screenconfig.SetValue('/Tools/Connection/Top',f_devicesconnection.Top);
 screenconfig.SetValue('/Tools/Connection/Left',f_devicesconnection.Left);

 screenconfig.SetValue('/Tools/Histogram/Parent',f_visu.Parent.Name);
 screenconfig.SetValue('/Tools/Histogram/Visible',f_visu.Visible);
 screenconfig.SetValue('/Tools/Histogram/Top',f_visu.Top);
 screenconfig.SetValue('/Tools/Histogram/Left',f_visu.Left);

 screenconfig.SetValue('/Tools/Messages/Parent',f_msg.Parent.Name);
 screenconfig.SetValue('/Tools/Messages/Visible',f_msg.Visible);
 screenconfig.SetValue('/Tools/Messages/Top',f_msg.Top);
 screenconfig.SetValue('/Tools/Messages/Left',f_msg.Left);

 screenconfig.SetValue('/Tools/Focuser/Parent',f_focuser.Parent.Name);
 screenconfig.SetValue('/Tools/Focuser/Visible',f_focuser.Visible or (not WantFocuser));
 screenconfig.SetValue('/Tools/Focuser/Top',f_focuser.Top);
 screenconfig.SetValue('/Tools/Focuser/Left',f_focuser.Left);

 screenconfig.SetValue('/Tools/Starprofile/Parent',f_starprofile.Parent.Name);
 screenconfig.SetValue('/Tools/Starprofile/Visible',f_starprofile.Visible);
 screenconfig.SetValue('/Tools/Starprofile/Top',f_starprofile.Top);
 screenconfig.SetValue('/Tools/Starprofile/Left',f_starprofile.Left);

 screenconfig.SetValue('/Tools/Magnifyer/Parent',f_magnifyer.Parent.Name);
 screenconfig.SetValue('/Tools/Magnifyer/Visible',f_magnifyer.Visible);
 screenconfig.SetValue('/Tools/Magnifyer/Top',f_magnifyer.Top);
 screenconfig.SetValue('/Tools/Magnifyer/Left',f_magnifyer.Left);

 screenconfig.SetValue('/Tools/Frame/Parent',f_frame.Parent.Name);
 screenconfig.SetValue('/Tools/Frame/Visible',f_frame.Visible);
 screenconfig.SetValue('/Tools/Frame/Top',f_frame.Top);
 screenconfig.SetValue('/Tools/Frame/Left',f_frame.Left);

 screenconfig.SetValue('/Tools/Rotator/Parent',f_rotator.Parent.Name);
 screenconfig.SetValue('/Tools/Rotator/Visible',f_rotator.Visible or (not WantRotator));
 screenconfig.SetValue('/Tools/Rotator/Top',f_rotator.Top);
 screenconfig.SetValue('/Tools/Rotator/Left',f_rotator.Left);

 screenconfig.SetValue('/Tools/Preview/Parent',f_preview.Parent.Name);
 screenconfig.SetValue('/Tools/Preview/Visible',f_preview.Visible);
 screenconfig.SetValue('/Tools/Preview/Top',f_preview.Top);
 screenconfig.SetValue('/Tools/Preview/Left',f_preview.Left);

 screenconfig.SetValue('/Tools/Capture/Parent',f_capture.Parent.Name);
 screenconfig.SetValue('/Tools/Capture/Visible',f_capture.Visible);
 screenconfig.SetValue('/Tools/Capture/Top',f_capture.Top);
 screenconfig.SetValue('/Tools/Capture/Left',f_capture.Left);

 screenconfig.SetValue('/Tools/Sequence/Parent',f_sequence.Parent.Name);
 screenconfig.SetValue('/Tools/Sequence/Visible',f_sequence.Visible);
 screenconfig.SetValue('/Tools/Sequence/Top',f_sequence.Top);
 screenconfig.SetValue('/Tools/Sequence/Left',f_sequence.Left);

 screenconfig.SetValue('/Tools/Filters/Parent',f_filterwheel.Parent.Name);
 screenconfig.SetValue('/Tools/Filters/Visible',f_filterwheel.Visible or (not WantWheel));
 screenconfig.SetValue('/Tools/Filters/Top',f_filterwheel.Top);
 screenconfig.SetValue('/Tools/Filters/Left',f_filterwheel.Left);

 screenconfig.SetValue('/Tools/CCDTemp/Parent',f_ccdtemp.Parent.Name);
 screenconfig.SetValue('/Tools/CCDTemp/Visible',f_ccdtemp.Visible);
 screenconfig.SetValue('/Tools/CCDTemp/Top',f_ccdtemp.Top);
 screenconfig.SetValue('/Tools/CCDTemp/Left',f_ccdtemp.Left);

 screenconfig.SetValue('/Tools/Mount/Parent',f_mount.Parent.Name);
 screenconfig.SetValue('/Tools/Mount/Visible',f_mount.Visible or (not WantMount));
 screenconfig.SetValue('/Tools/Mount/Top',f_mount.Top);
 screenconfig.SetValue('/Tools/Mount/Left',f_mount.Left);

 screenconfig.SetValue('/Tools/Autoguider/Parent',f_autoguider.Parent.Name);
 screenconfig.SetValue('/Tools/Autoguider/Visible',f_autoguider.Visible);
 screenconfig.SetValue('/Tools/Autoguider/Top',f_autoguider.Top);
 screenconfig.SetValue('/Tools/Autoguider/Left',f_autoguider.Left);

 screenconfig.SetValue('/Tools/Planetarium/Parent',f_planetarium.Parent.Name);
 screenconfig.SetValue('/Tools/Planetarium/Visible',f_planetarium.Visible);
 screenconfig.SetValue('/Tools/Planetarium/Top',f_planetarium.Top);
 screenconfig.SetValue('/Tools/Planetarium/Left',f_planetarium.Left);

 screenconfig.SetValue('/Tools/Script/Parent',f_script.Parent.Name);
 screenconfig.SetValue('/Tools/Script/Visible',f_script.Visible);
 screenconfig.SetValue('/Tools/Script/Top',f_script.Top);
 screenconfig.SetValue('/Tools/Script/Left',f_script.Left);

 screenconfig.SetValue('/Tools/Weather/Parent',f_weather.Parent.Name);
 screenconfig.SetValue('/Tools/Weather/Visible',f_weather.Visible or (not WantWeather));
 screenconfig.SetValue('/Tools/Weather/Top',f_weather.Top);
 screenconfig.SetValue('/Tools/Weather/Left',f_weather.Left);

 screenconfig.SetValue('/Tools/Safety/Parent',f_safety.Parent.Name);
 screenconfig.SetValue('/Tools/Safety/Visible',f_safety.Visible or (not WantSafety));
 screenconfig.SetValue('/Tools/Safety/Top',f_safety.Top);
 screenconfig.SetValue('/Tools/Safety/Left',f_safety.Left);

 screenconfig.SetValue('/Tools/Dome/Parent',f_dome.Parent.Name);
 screenconfig.SetValue('/Tools/Dome/Visible',f_dome.Visible or (not WantDome));
 screenconfig.SetValue('/Tools/Dome/Top',f_dome.Top);
 screenconfig.SetValue('/Tools/Dome/Left',f_dome.Left);

 screenconfig.SetValue('/Tools/Cover/Parent',f_cover.Parent.Name);
 screenconfig.SetValue('/Tools/Cover/Visible',f_cover.Visible or (not WantCover));
 screenconfig.SetValue('/Tools/Cover/Top',f_cover.Top);
 screenconfig.SetValue('/Tools/Cover/Left',f_cover.Left);

 screenconfig.SetValue('/Tools/Switch/Parent',f_switch.Parent.Name);
 screenconfig.SetValue('/Tools/Switch/Visible',f_switch.Visible or (not WantSwitch));
 screenconfig.SetValue('/Tools/Switch/Top',f_switch.Top);
 screenconfig.SetValue('/Tools/Switch/Left',f_switch.Left);

 screenconfig.SetValue('/Tools/Clock/Visible',MenuViewClock.Checked);

 screenconfig.SetValue('/Window/Maximized', WindowState=wsMaximized);
 screenconfig.SetValue('/Window/Top',Top);
 screenconfig.SetValue('/Window/Left',Left);
 screenconfig.SetValue('/Window/Width',Width);
 screenconfig.SetValue('/Window/Height',Height);
 screenconfig.SetValue('/Window/PanelRight',PanelRight.Width);
end;

procedure Tf_main.SaveSettings;
var i,n: integer;
begin
   SaveScreenConfig;
   config.SetValue('/Configuration/Version',ccdcielver);

   config.SetValue('/Log/LogLevel',LogLevel);
   config.SetValue('/Script/ScriptName',f_script.ComboBoxScript.Text);
   config.SetValue('/Temperature/Setpoint',f_ccdtemp.Setpoint.Value);
   config.SetValue('/Preview/Exposure',f_preview.ExpTime.Text);
   config.SetValue('/Preview/Binning',f_preview.Binning.Text);
   if hasGainISO then
     config.SetValue('/Preview/Gain',f_preview.ISObox.Text)
   else
     config.SetValue('/Preview/Gain',f_preview.GainEdit.Value);
   config.SetValue('/Capture/Exposure',f_capture.ExpTime.Text);
   config.SetValue('/Capture/StackNum',f_capture.StackNum.Value);
   config.SetValue('/Capture/Binning',f_capture.Binning.Text);
   config.SetValue('/Capture/FileName',f_capture.Fname.Text);
   config.SetValue('/Capture/Count',f_capture.SeqNum.Value);
   if hasGainISO then
     config.SetValue('/Capture/Gain',f_capture.ISObox.Text)
   else
     config.SetValue('/Capture/Gain',f_capture.GainEdit.Value);
   if hasOffset then begin
     config.SetValue('/Preview/Offset',f_preview.OffsetEdit.Value);
     config.SetValue('/Capture/Offset',f_capture.OffsetEdit.Value);
   end;

   config.SetValue('/Sequence/Targets',f_sequence.Filename);
   config.SetValue('/Sequence/Unattended',f_sequence.Unattended.Checked);
   config.SetValue('/Sequence/EditTarget/Width',f_EditTargets.Width);
   config.SetValue('/Sequence/EditTarget/Height',f_EditTargets.Height);
   config.SetValue('/Sequence/EditTarget/SepPos',f_EditTargets.Splitter1.Top);

   config.SetValue('/Visu/Gamma',f_visu.Gamma.Value);
   config.SetValue('/Visu/HistBar',f_visu.HistBar.Position);
   config.SetValue('/Visu/FlipHorz',f_visu.FlipHorz);
   config.SetValue('/Visu/FlipVert',f_visu.FlipVert);
   config.SetValue('/Visu/ClipRange',f_visu.BtnClipRange.Down);

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

   n:=ReadoutList.Count;
   config.SetValue('/Readout/Num',n);
   for i:=0 to n-1 do begin
      config.SetValue('/Readout/Mode'+IntToStr(i),ReadoutList[i]);
   end;

   config.SetValue('/Gain/hasGain',hasGain);
   config.SetValue('/Gain/hasGainISO',hasGainISO);
   config.SetValue('/Gain/Gain',Gain);
   config.SetValue('/Gain/GainMin',GainMin);
   config.SetValue('/Gain/GainMax',GainMax);
   n:=ISOList.Count;
   config.SetValue('/Gain/NumISO',n);
   for i:=0 to n-1 do begin
      config.SetValue('/Gain/ISO'+IntToStr(i),ISOList[i]);
   end;
   config.SetValue('/Offset/hasOffset',hasOffset);
   config.SetValue('/Offset/Offset',Offset);
   config.SetValue('/Offset/OffsetMin',OffsetMin);
   config.SetValue('/Offset/OffsetMax',OffsetMax);

   config.SetValue('/Rotator/Reverse',f_rotator.Reverse.Checked);
   config.SetValue('/Rotator/CalibrationAngle',rotator.CalibrationAngle);

   config.SetValue('/StarAnalysis/FocuserLastTemp',FocuserLastTemp);
   config.SetValue('/StarAnalysis/MagnitudeCalibration',MagnitudeCalibration);
   config.SetValue('/Astrometry/LastPixelSize',LastPixelSize);
end;

procedure Tf_main.SaveConfig;
var inif:TIniFile;
begin
  try
  screenconfig.Flush;
  config.Flush;
  if credentialconfig.Filename<>'' then
    credentialconfig.Flush;
  emailconfig.Flush;
  bpmconfig.Flush;
  if not ProfileFromCommandLine then begin
    DeleteFile(slash(ConfigDir)+'ccdciel.rc.tmp');
    inif:=TIniFile.Create(slash(ConfigDir)+'ccdciel.rc.tmp');
    inif.WriteString('main','profile',profile);
    inif.UpdateFile;
    inif.Free;
    DeleteFile(slash(ConfigDir)+'ccdciel.rc.bak');
    RenameFile(slash(ConfigDir)+'ccdciel.rc',slash(ConfigDir)+'ccdciel.rc.bak');
    RenameFile(slash(ConfigDir)+'ccdciel.rc.tmp',slash(ConfigDir)+'ccdciel.rc');
  end;
  NewMessage(rsConfiguratio,1);
  except
    on E: Exception do NewMessage('Error saving configuration: '+ E.Message,1);
  end;
end;

procedure Tf_main.OpenConfig(n: string);
var configver: string;
begin
 NewMessage(Format(rsUsingConfigu, [n]), 3);
 config.Filename:=slash(ConfigDir)+n;
 configver:=config.GetValue('/Configuration/Version','');
 screenconfig.Filename:=slash(ConfigDir)+'ScreenLayout.cfg';
 if FileExists(config.Filename+'.credential') then
   credentialconfig.Filename:=config.Filename+'.credential';
 emailconfig.Filename:=slash(ConfigDir)+'email.cfg';
 bpmconfig.Filename:=config.Filename+'.bpm';
 UpdConfig(configver);
end;

Procedure Tf_main.Connect(Sender: TObject);
begin
  if WantCamera and (CameraName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsCamera);
    MenuSetup.Click;
    exit;
  end;
  if WantWheel and (WheelName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsFilterWheel);
    MenuSetup.Click;
    exit;
  end;
  if WantFocuser and (FocuserName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsFocuser);
    MenuSetup.Click;
    exit;
  end;
  if WantRotator and (RotatorName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsRotator);
    MenuSetup.Click;
    exit;
  end;
  if WantMount and (MountName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsMount);
    MenuSetup.Click;
    exit;
  end;
  if WantDome and (DomeName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsDome);
    MenuSetup.Click;
    exit;
  end;
  if WantWatchdog and (WatchdogName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsWatchdog);
    MenuSetup.Click;
    exit;
  end;
  if WantWeather and (WeatherName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsWeatherStati);
    MenuSetup.Click;
    exit;
  end;
  if WantSafety and (SafetyName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsSafetyMonito);
    MenuSetup.Click;
    exit;
  end;
  if WantSwitch and (SwitchName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsSwitch);
    MenuSetup.Click;
    exit;
  end;
  if WantCover and (CoverName='') then begin
    f_devicesconnection.BtnConnect.Caption:=rsConnect;
    ShowMessage(rsPleaseConfig+blank+rsCoverCalibra);
    MenuSetup.Click;
    exit;
  end;

  f_devicesconnection.LabelCamera.Visible:=WantCamera;
  f_devicesconnection.LabelWheel.Visible:=WantWheel;
  f_devicesconnection.LabelFocuser.Visible:=WantFocuser;
  f_devicesconnection.LabelRotator.Visible:=WantRotator;
  f_devicesconnection.LabelMount.Visible:=WantMount;
  f_devicesconnection.LabelDome.Visible:=WantDome;
  f_devicesconnection.LabelWeather.Visible:=WantWeather;
  f_devicesconnection.LabelSafety.Visible:=WantSafety;
  f_devicesconnection.LabelSwitch.Visible:=WantSwitch;
  f_devicesconnection.LabelCover.Visible:=WantCover;
  f_devicesconnection.LabelWatchdog.Visible:=WantWatchdog;
  f_devicesconnection.PanelDev.Visible:=true;

  if WantCamera  then ConnectCamera(Sender);
  Application.ProcessMessages;
  if WantWheel   then ConnectWheel(Sender);
  Application.ProcessMessages;
  if WantFocuser then ConnectFocuser(Sender);
  Application.ProcessMessages;
  if WantRotator then ConnectRotator(Sender);
  Application.ProcessMessages;
  if WantMount   then ConnectMount(Sender);
  Application.ProcessMessages;
  if WantDome    then ConnectDome(Sender);
  Application.ProcessMessages;
  if WantWeather then ConnectWeather(Sender);
  Application.ProcessMessages;
  if WantSafety  then ConnectSafety(Sender);
  Application.ProcessMessages;
  if WantSwitch  then ConnectSwitch(Sender);
  Application.ProcessMessages;
  if WantCover  then ConnectCover(Sender);
  Application.ProcessMessages;
  if WantWatchdog then ConnectWatchdog(Sender);
  if f_autoguider.BtnConnect.Caption=rsConnect then AutoguiderConnectClick(Sender);
  if f_planetarium.BtnConnect.Caption=rsConnect then PlanetariumConnectClick(Sender);
end;

Procedure Tf_main.Disconnect(Sender: TObject);
begin
   if (sender=nil) or (MessageDlg(rsAreYouSureYo, mtConfirmation, mbYesNo, 0)=mrYes) then begin
     NewMessage(rsDisconnectin,9);
     if camera.Status=devConnected then camera.AbortExposure;
     f_preview.stop;
     f_capture.stop;
     Capture:=false;
     StatusBar1.Panels[panelstatus].Text:='';
     DisconnectCamera(Sender); // disconnect camera first
     DisconnectWheel(Sender);
     DisconnectFocuser(Sender);
     DisconnectRotator(Sender);
     DisconnectMount(Sender);
     DisconnectDome(Sender);
     DisconnectWeather(Sender);
     DisconnectSafety(Sender);
     DisconnectSwitch(Sender);
     DisconnectCover(Sender);
     DisconnectWatchdog(Sender);
   end;
end;

Procedure Tf_main.ConnectDevice(num: double);
begin
  case round(num) of
     1:  if WantCamera  then ConnectCamera(nil);
     2:  if WantWheel   then ConnectWheel(nil);
     3:  if WantFocuser then ConnectFocuser(nil);
     4:  if WantRotator then ConnectRotator(nil);
     5:  if WantMount   then ConnectMount(nil);
     6:  if WantDome    then ConnectDome(nil);
     7:  if WantWatchdog then ConnectWatchdog(nil);
     8:  if WantWeather then ConnectWeather(nil);
     9:  if WantSafety  then ConnectSafety(nil);
     10: if WantSwitch  then ConnectSwitch(nil);
     11: if WantCover   then ConnectCover(nil);
  end;
end;

Procedure Tf_main.DisconnectDevice(num: double);
begin
 case round(num) of
    1:  DisconnectCamera(nil);
    2:  DisconnectWheel(nil);
    3:  DisconnectFocuser(nil);
    4:  DisconnectRotator(nil);
    5:  DisconnectMount(nil);
    6:  DisconnectDome(nil);
    7:  DisconnectWatchdog(nil);
    8:  DisconnectWeather(nil);
    9:  DisconnectSafety(nil);
    10: DisconnectSwitch(nil);
    11: DisconnectCover(nil);
 end;
end;

Procedure Tf_main.CheckConnectionStatus;
var allcount, upcount, downcount, concount: integer;
procedure SetDisconnected;
begin
 AllDevicesConnected:=false;
 f_devicesconnection.led.Brush.Color:=clRed;
 f_devicesconnection.BtnProfile.Enabled:=True;
 f_devicesconnection.BtnConnect.Caption:=rsConnect;
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;
procedure SetConnected;
begin
 AllDevicesConnected:=true;
 f_devicesconnection.led.Brush.Color:=clLime;
 f_devicesconnection.BtnProfile.Enabled:=False;
 f_devicesconnection.BtnConnect.Caption:=rsDisconnect;
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;
procedure SetConnecting;
begin
 AllDevicesConnected:=false;
 f_devicesconnection.led.Brush.Color:=clYellow;
 f_devicesconnection.BtnProfile.Enabled:=False;
 f_devicesconnection.BtnConnect.Caption:=rsDisconnect;
 MenuConnect.Caption:=f_devicesconnection.BtnConnect.Caption;
end;

begin
allcount:=0; upcount:=0; downcount:=0; concount:=0;
 if WantCamera then begin
  inc(allcount);
  case camera.Status of
    devConnected: begin
                  inc(upcount);
                  CameraConnectTimer.Enabled:=false;
                  CameraConnectTimer.Enabled:=true;
                  end;
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
    devConnected: begin
                  inc(upcount);
                  end;
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
 if WantDome then begin
  inc(allcount);
  case dome.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantWeather then begin
  inc(allcount);
  case weather.Status of
    devConnected: inc(upcount);
    devDisconnected: inc(downcount);
    devConnecting: inc(concount);
  end;
 end;
 if WantSafety then begin
   inc(allcount);
   case safety.Status of
     devConnected: inc(upcount);
     devDisconnected: inc(downcount);
     devConnecting: inc(concount);
   end;
 end;
 if WantSwitch then begin
   inc(allcount);
   case switch.Status of
     devConnected: inc(upcount);
     devDisconnected: inc(downcount);
     devConnecting: inc(concount);
   end;
 end;
 if WantCover then begin
   inc(allcount);
   case cover.Status of
     devConnected: inc(upcount);
     devDisconnected: inc(downcount);
     devConnecting: inc(concount);
   end;
 end;
 if allcount=0 then SetDisconnected
 else if (upcount=allcount) then begin
   SetConnected;
   ConnectTimer.Enabled:=false;
   ConnectTimer.Enabled:=true;
 end
 else if (concount>0)or(upcount>0) then SetConnecting
 else SetDisconnected;
 StatusBar1.Invalidate;
end;

Procedure Tf_main.ConnectCamera(Sender: TObject);
var inditransfer: TIndiTransfert;
    indihost,inditransferdir: string;
    ok: boolean;
begin
   CameraInitialized:=false;
   case camera.CameraInterface of
    INDI : begin
           inditransfer:=TIndiTransfert(config.GetValue('/INDIcamera/IndiTransfert',ord(itNetwork)));
           inditransferdir:=config.GetValue('/INDIcamera/IndiTransfertDir',defTransfertPath);
           indihost:=config.GetValue('/INDIcamera/Server','');
           if inditransfer=itDisk then begin
             // some control to be sure we can use disk transfer
             ok:=(copy(indihost,1,3)='127')or(uppercase(indihost)='LOCALHOST'); // local indiserver
             ok:=ok and DirectoryIsWritable(inditransferdir);
             if not ok then begin
               inditransfer:=itNetwork;
               NewMessage('Cannot use ramdisk camera transfer, switch to network',3);
             end;
           end;
           camera.IndiTransfert:=inditransfer;
           camera.IndiTransfertDir:=inditransferdir;
           camera.Connect(config.GetValue('/INDIcamera/Server',''),
                          config.GetValue('/INDIcamera/ServerPort',''),
                          config.GetValue('/INDIcamera/Device',''),
                          config.GetValue('/INDIcamera/Sensor','CCD1'));
           end;
    ASCOM: begin
           camera.UseCameraStartTime:=config.GetValue('/ASCOMcamera/CameraDateObs',false);
           camera.FixPixelRange:=config.GetValue('/ASCOMcamera/FixPixelRange',false);
           camera.Connect(config.GetValue('/ASCOMcamera/Device',''));
           end;
    ASCOMREST: begin
           camera.UseCameraStartTime:=config.GetValue('/ASCOMRestcamera/CameraDateObs',false);
           camera.FixPixelRange:=config.GetValue('/ASCOMRestcamera/FixPixelRange',false);
           camera.Connect(config.GetValue('/ASCOMRestcamera/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestcamera/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestcamera/Protocol',0)],
                          'camera/'+IntToStr(config.GetValue('/ASCOMRestcamera/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestcamera/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestcamera/Pass','')), encryptpwd));

           end;
  end;
end;

Procedure Tf_main.DisconnectCamera(Sender: TObject);
begin
if camera.Status<>devDisconnected then camera.Disconnect;
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
  if camera.Temperature=NullCoord then f_ccdtemp.Visible:=False;
  f_ccdtemp.Current.Caption:=FormatFloat(f1,TempDisplay(TemperatureScale,camera.Temperature));
  buf:=FormatFloat(f0,TempDisplay(TemperatureScale,camera.TemperatureRange.min))+'...'+FormatFloat(f0,TempDisplay(TemperatureScale,camera.TemperatureRange.max));
  f_ccdtemp.Setpoint.Hint:=rsDesiredTempe+crlf+buf;
end;

procedure Tf_main.SetTemperature(Sender: TObject);
begin
  camera.Temperature:=TempCelsius(TemperatureScale,f_ccdtemp.Setpoint.Value);
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
    r: TNumRange;
begin
 r:=camera.ExposureRange;
 buf:=FormatFloat(f0,r.min)+'...'+FormatFloat(f0,r.max);
 buf:=rsExposureTime+crlf+buf;
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
   NewMessage(Format(rsCameraFrameX, [f_frame.FX.Text, f_frame.FY.Text,
     f_frame.FWidth.Text, f_frame.FHeight.Text]),2);
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
     NewMessage('Invalid frame values',1)
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
 if rxmax>10 then rxmax:=10;
 if rxstep<1 then rxstep:=1;
 if rymin<1 then rymin:=1;
 if rymax<rxmin then rymax:=rymin;
 if rymax>10 then rymax:=10;
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
 SetBinningList(posprev,poscapt);
end;

procedure Tf_main.SetBinningList(posprev,poscapt: integer);
begin
 f_preview.Binning.Items.Assign(BinningList);
 f_capture.Binning.Items.Assign(BinningList);
 f_EditTargets.StepList.Columns[pcolbin-1].PickList.Assign(BinningList);
 f_EditTargets.FlatBinning.Items.Assign(BinningList);
 f_preview.Binning.ItemIndex:=posprev;
 f_capture.Binning.ItemIndex:=poscapt;
 f_EditTargets.FlatBinning.ItemIndex:=0;
end;

procedure Tf_main.GainStatus(Sender: TObject);
begin
  ShowGain;
end;

procedure Tf_main.ShowGain;
begin
 camera.CanSetGain:=config.GetValue('/Sensor/CanSetGain',false);
 camera.CheckGain;
 hasGain:=camera.hasGain;
 hasGainISO:=camera.hasGainISO;
 ISOList.Assign(camera.ISOList);
 Gain:=camera.Gain;
 GainMin:=camera.GainMin;
 GainMax:=camera.GainMax;
 camera.CheckOffset;
 hasOffset:=camera.hasOffset;
 Offset:=camera.Offset;
 OffsetMin:=camera.OffsetMin;
 OffsetMax:=camera.OffsetMax;
 SetGainList;
end;

procedure Tf_main.SetGainList;
var gainprev,gaincapt,offsetprev,offsetcapt:string;
    i,posprev,poscapt,poffprev,poffcapt:integer;
begin
 if debug_msg then NewMessage('Camera gain:'+BoolToStr(hasGain,rsTrue,rsFalse)+' iso:'+BoolToStr(hasGainISO,rsTrue,rsFalse));
 gainprev:=config.GetValue('/Preview/Gain','');
 gaincapt:=config.GetValue('/Capture/Gain','');
 posprev:=Gain;
 poscapt:=Gain;
 offsetprev:=config.GetValue('/Preview/Offset','');
 offsetcapt:=config.GetValue('/Capture/Offset','');
 poffprev:=Offset;
 poffcapt:=Offset;
 if debug_msg then NewMessage('Want camera gain control: '+BoolToStr(camera.CanSetGain,rsTrue,rsFalse));
 f_capture.PanelGain.Visible:=camera.CanSetGain and (hasGain or hasGainISO);
 f_capture.PanelOffset.Visible:=f_capture.PanelGain.Visible and hasOffset;
 f_preview.PanelGain.Visible:=f_capture.PanelGain.Visible;
 f_preview.PanelOffset.Visible:=f_capture.PanelGain.Visible and hasOffset;
 f_EditTargets.PanelGain.Visible:=f_capture.PanelGain.Visible;
 f_EditTargets.StepList.Columns[pcolgain-1].Visible:=f_capture.PanelGain.Visible;
 f_EditTargets.StepList.Columns[pcoloffset-1].Visible:=f_capture.PanelOffset.Visible;
 f_option.AutofocusPanelGain.Visible:=f_capture.PanelGain.Visible;
 f_option.AutofocusPanelOffset.Visible:=f_capture.PanelOffset.Visible;
 f_option.SlewPanelGain.Visible:=f_capture.PanelGain.Visible;
 f_option.SlewPanelOffset.Visible:=f_capture.PanelOffset.Visible;
 if hasGainISO then begin
   f_capture.ISObox.Visible:=true;
   f_capture.GainEdit.Visible:=false;
   f_capture.ISObox.Items.Assign(ISOList);
   f_preview.ISObox.Visible:=true;
   f_preview.GainEdit.Visible:=false;
   f_preview.ISObox.Items.Assign(ISOList);
   f_EditTargets.FISObox.Items.Assign(ISOList);
   f_EditTargets.FISObox.Visible:=true;
   f_EditTargets.FGainEdit.Visible:=false;
   f_EditTargets.StepList.Columns[pcolgain-1].PickList.Assign(ISOList);
   f_option.AutofocusISObox.Visible:=true;
   f_option.AutofocusGainEdit.Visible:=false;
   f_option.AutofocusISObox.Items.Assign(ISOList);
   f_option.SlewISObox.Visible:=true;
   f_option.SlewGainEdit.Visible:=false;
   f_option.SlewISObox.Items.Assign(ISOList);
   for i:=0 to ISOList.Count-1 do begin;
      if ISOList[i]=gainprev then posprev:=i;
      if ISOList[i]=gaincapt then poscapt:=i;
   end;
   f_capture.ISObox.ItemIndex:=poscapt;
   f_preview.ISObox.ItemIndex:=posprev;
   f_EditTargets.FISObox.ItemIndex:=poscapt;
   f_option.AutofocusISObox.ItemIndex:=posprev;
   f_option.SlewISObox.ItemIndex:=posprev;
 end;
 if hasGain and (not hasGainISO) then begin
   f_capture.ISObox.Visible:=false;
   f_capture.GainEdit.Visible:=true;
   f_capture.GainEdit.Hint:=IntToStr(GainMin)+'...'+IntToStr(GainMax);
   f_preview.ISObox.Visible:=false;
   f_preview.GainEdit.Visible:=true;
   f_preview.GainEdit.Hint:=IntToStr(GainMin)+'...'+IntToStr(GainMax);
   f_EditTargets.FISObox.Visible:=false;
   f_EditTargets.FGainEdit.Visible:=true;
   f_EditTargets.FGainEdit.Hint:=IntToStr(GainMin)+'...'+IntToStr(GainMax);
   f_option.AutofocusISObox.Visible:=false;
   f_option.AutofocusGainEdit.Visible:=true;
   f_option.AutofocusGainEdit.Hint:=IntToStr(GainMin)+'...'+IntToStr(GainMax);
   f_option.SlewISObox.Visible:=false;
   f_option.SlewGainEdit.Visible:=true;
   f_option.SlewGainEdit.Hint:=IntToStr(GainMin)+'...'+IntToStr(GainMax);
   posprev:=StrToIntDef(gainprev,gain);
   poscapt:=StrToIntDef(gaincapt,gain);
   f_capture.GainEdit.Value:=poscapt;
   f_preview.GainEdit.Value:=posprev;
   f_EditTargets.FGainEdit.Value:=poscapt;
   f_option.AutofocusGainEdit.Value:=posprev;
   f_option.SlewGainEdit.Value:=posprev;
 end;
 if hasOffset then begin
   f_preview.OffsetEdit.Hint:=IntToStr(OffsetMin)+'...'+IntToStr(OffsetMax);
   f_capture.OffsetEdit.Hint:=IntToStr(OffsetMin)+'...'+IntToStr(OffsetMax);
   f_EditTargets.FOffsetEdit.Hint:=IntToStr(OffsetMin)+'...'+IntToStr(OffsetMax);
   f_option.AutofocusOffsetEdit.Hint:=IntToStr(OffsetMin)+'...'+IntToStr(OffsetMax);
   f_option.SlewOffsetEdit.Hint:=IntToStr(OffsetMin)+'...'+IntToStr(OffsetMax);
   poffprev:=StrToIntDef(offsetprev,Offset);
   poffcapt:=StrToIntDef(offsetcapt,Offset);
   f_preview.OffsetEdit.Value:=poffprev;
   f_capture.OffsetEdit.Value:=poffcapt;
   f_EditTargets.FOffsetEdit.Value:=poffcapt;
   f_option.AutofocusOffsetEdit.Value:=poffprev;
   f_option.SlewOffsetEdit.Value:=poffprev;
 end;
end;

procedure Tf_main.ShowFnumber;
begin
 if camera.hasFnumber then begin
  f_capture.PanelFnumber.Visible:=true;
  f_preview.PanelFnumber.Visible:=true;
  f_capture.Fnumber.Items.Assign(camera.FnumberList);
  f_preview.Fnumber.Items.Assign(camera.FnumberList);
  f_capture.Fnumber.Text:=camera.Fnumber;
  f_preview.Fnumber.Text:=camera.Fnumber;
  f_EditTargets.PanelFstop.Visible:=true;
  f_EditTargets.StepList.Columns[pcolfstop-1].Visible:=true;
  f_EditTargets.StepList.Columns[pcolfstop-1].PickList.Assign(camera.FnumberList);
  f_EditTargets.FFstopbox.Items.Assign(camera.FnumberList);
 end
 else begin
  f_capture.PanelFnumber.Visible:=false;
  f_preview.PanelFnumber.Visible:=false;
  f_EditTargets.PanelFstop.Visible:=false;
  f_EditTargets.StepList.Columns[pcolfstop-1].Visible:=false;
 end;
end;

Procedure Tf_main.ConnectWheel(Sender: TObject);
begin
  case wheel.WheelInterface of
    INCAMERA : wheel.Connect('');
    INDI : wheel.Connect(config.GetValue('/INDIwheel/Server',''),
                          config.GetValue('/INDIwheel/ServerPort',''),
                          config.GetValue('/INDIwheel/Device',''));
    ASCOM: wheel.Connect(config.GetValue('/ASCOMwheel/Device',''));
    ASCOMREST: wheel.Connect(config.GetValue('/ASCOMRestwheel/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestwheel/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestwheel/Protocol',0)],
                          'filterwheel/'+IntToStr(config.GetValue('/ASCOMRestwheel/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestwheel/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestwheel/Pass','')), encryptpwd));
    MANUAL : wheel.Connect('');
  end;
end;

Procedure Tf_main.DisconnectWheel(Sender: TObject);
begin
 if wheel.Status<>devDisconnected then wheel.Disconnect;
end;

Procedure Tf_main.ConnectFocuser(Sender: TObject);
begin
  focuser.UseExternalTemperature:=config.GetValue('/Focuser/ExternalTemperature',false);
  case focuser.FocuserInterface of
    INDI : focuser.Connect(config.GetValue('/INDIfocuser/Server',''),
                          config.GetValue('/INDIfocuser/ServerPort',''),
                          config.GetValue('/INDIfocuser/Device',''));
    ASCOM: focuser.Connect(config.GetValue('/ASCOMfocuser/Device',''));
    ASCOMREST: focuser.Connect(config.GetValue('/ASCOMRestfocuser/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestfocuser/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestfocuser/Protocol',0)],
                          'focuser/'+IntToStr(config.GetValue('/ASCOMRestfocuser/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestfocuser/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestfocuser/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectFocuser(Sender: TObject);
begin
 if focuser.Status<>devDisconnected then focuser.Disconnect;
end;

procedure Tf_main.SetFocusMode;
var r: TNumRange;
begin
   FocuserTemp:=focuser.Temperature; // first call to test ascom property
   if focuser.hasTemperature then begin
      f_focuser.PanelTemp.Visible:=true;
      if FocuserTemp<>NullCoord then f_focuser.Temp.Text:=FormatFloat(f1,TempDisplay(TemperatureScale,FocuserTemp));
      if FocuserLastTemp=NullCoord then FocuserLastTemp:=FocuserTemp;
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
  f_focuser.Position.Value:=focuser.Position;
  FocuserPositionMin:=0;
  FocuserPositionMax:=MAXWORD;
  r:=focuser.PositionRange;
  if r.step>0 then begin
   FocuserPositionMin:=round(r.min);
   FocuserPositionMax:=round(r.max);
   f_focuser.Position.Hint:=rsCurrentFocus+', '+
                   IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
    f_focuser.PosIncr.ItemIndex:=2;
  end;
  f_focuser.speed.Value:=focuser.Speed;
  f_focuser.timer.Value:=focuser.Timer;
  r:=focuser.RelPositionRange;
  if r.step>0 then begin
    f_focuser.RelIncr.Hint:=rsRelativeIncr+', '+
                    IntToStr(round(r.min))+'..'+IntToStr(round(r.max)) ;
    f_focuser.RelIncr.ItemIndex:=2;
  end;
  f_focuser.Ready:=true;
  FocuserTemperatureCompensation(true);
end;

Procedure Tf_main.ConnectRotator(Sender: TObject);
begin
  case rotator.RotatorInterface of
    INDI : rotator.Connect(config.GetValue('/INDIrotator/Server',''),
                          config.GetValue('/INDIrotator/ServerPort',''),
                          config.GetValue('/INDIrotator/Device',''));
    ASCOM: rotator.Connect(config.GetValue('/ASCOMrotator/Device',''));
    ASCOMREST: rotator.Connect(config.GetValue('/ASCOMRestrotator/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestrotator/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestrotator/Protocol',0)],
                          'rotator/'+IntToStr(config.GetValue('/ASCOMRestrotator/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestrotator/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestrotator/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectRotator(Sender: TObject);
begin
 if rotator.Status<>devDisconnected then rotator.Disconnect;
end;

Procedure Tf_main.ConnectMount(Sender: TObject);
begin
  case mount.MountInterface of
    INDI : mount.Connect(config.GetValue('/INDImount/Server',''),
                          config.GetValue('/INDImount/ServerPort',''),
                          config.GetValue('/INDImount/Device',''));
    ASCOM: mount.Connect(config.GetValue('/ASCOMmount/Device',''));
    ASCOMREST: mount.Connect(config.GetValue('/ASCOMRestmount/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestmount/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestmount/Protocol',0)],
                          'telescope/'+IntToStr(config.GetValue('/ASCOMRestmount/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestmount/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestmount/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectMount(Sender: TObject);
begin
 if mount.Status<>devDisconnected then mount.Disconnect;
end;

Procedure Tf_main.ConnectDome(Sender: TObject);
begin
  case dome.DomeInterface of
    INDI : dome.Connect(config.GetValue('/INDIdome/Server',''),
                          config.GetValue('/INDIdome/ServerPort',''),
                          config.GetValue('/INDIdome/Device',''));
    ASCOM: dome.Connect(config.GetValue('/ASCOMdome/Device',''));
    ASCOMREST: dome.Connect(config.GetValue('/ASCOMRestdome/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestdome/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestdome/Protocol',0)],
                          'dome/'+IntToStr(config.GetValue('/ASCOMRestdome/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestdome/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestdome/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectDome(Sender: TObject);
begin
 if dome.Status<>devDisconnected then dome.Disconnect;
end;

Procedure Tf_main.DomeStatus(Sender: TObject);
begin
case dome.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelDome.Font.Color:=clRed;
                      f_dome.Connected:=false;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsDome+' '+DevInterfaceName[ord(dome.DomeInterface)]+' "'+dome.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelDome.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelDome.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelDome.Font.Color:=clGreen;
                      NewMessage(Format(rsConnected, [rsDome]),1);
                      wait(1);
                      f_dome.Connected:=true;
                      DomeShutterChange(Sender);
                      DomeSlaveChange(Sender);
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.DomeShutterChange(Sender: TObject);
var ok: boolean;
begin
  if f_dome.Connected then begin
    ok:=dome.Shutter;
    if f_dome.Shutter<>ok then NewMessage(Format(rsDomeShutter, [BoolToStr(ok, rsOpen, rsClose)]));
    f_dome.Shutter:=ok;
  end;
end;

Procedure Tf_main.DomeSlaveChange(Sender: TObject);
var ok: boolean;
begin
  if f_dome.Connected then begin
    ok:=dome.hasSlaving;
    if f_dome.CanSlave<>ok then NewMessage(Format(rsDomeSlaving, [BoolToStr(ok, rsAvailable, rsUnavailable)]));
    f_dome.CanSlave:=dome.hasSlaving;
    if f_dome.CanSlave then begin
      ok:=dome.Slave;
      if f_dome.Slave<>ok then NewMessage(Format(rsDomeSlaving, [BoolToStr(ok, rsOn, rsOff)]));
      f_dome.Slave:=ok;
    end;
  end;
end;

procedure Tf_main.ParkDome(Sender: TObject);
begin
if MessageDlg(rsParkAndClose+'?',mtConfirmation,mbYesNo,0)=mrYes then begin
 if mount.Park or
   (MessageDlg(Format(rsMountIsNotPa, [crlf]), mtConfirmation, mbYesNo, 0)=mrYes)
 then begin
   NewMessage(rsStopDomeSlav,1);
   Dome.Slave:=false;
   NewMessage(rsParkDome,1);
   Dome.Park:=true;
   NewMessage(rsCloseDome,1);
   Dome.Shutter:=false;
 end;
end;
end;

procedure Tf_main.StartDomeSlaving(Sender: TObject);
begin
  dome.Slave:=true;
end;

Procedure Tf_main.ConnectWatchdog(Sender: TObject);
begin
   if watchdog=nil then exit;
   watchdog.Threshold:=strtointdef(config.GetValue('/INDIwatchdog/Threshold','10'),10);
   watchdog.Connect(config.GetValue('/INDIwatchdog/Server',''),
                  config.GetValue('/INDIwatchdog/ServerPort',''),
                  config.GetValue('/INDIwatchdog/Device','WatchDog'));
end;

Procedure Tf_main.DisconnectWatchdog(Sender: TObject);
begin
 if watchdog=nil then exit;
 watchdog.Disconnect;
end;

Procedure Tf_main.WatchdogStatus(Sender: TObject);
begin
 if watchdog=nil then exit;
 case watchdog.Status of
   devDisconnected:begin
                   f_devicesconnection.LabelWatchdog.Font.Color:=clRed;
                   end;
   devConnecting:  begin
                   NewMessage(Format(rsConnecting, [rsWatchdog+ellipsis]),2);
                   f_devicesconnection.LabelWatchdog.Font.Color:=clOrange;
                   end;
   devConnected:   begin
                   if f_devicesconnection.LabelWatchdog.Font.Color<>clGreen then NewMessage(Format(rsConnected, [rsWatchdog]),1);
                   f_devicesconnection.LabelWatchdog.Font.Color:=clGreen;
                   end;
 end;
 CheckConnectionStatus;
end;

Procedure Tf_main.ConnectWeather(Sender: TObject);
begin
  case weather.WeatherInterface of
    INDI : weather.Connect(config.GetValue('/INDIweather/Server',''),
                          config.GetValue('/INDIweather/ServerPort',''),
                          config.GetValue('/INDIweather/Device',''),
                          '');
    ASCOM: weather.Connect(config.GetValue('/ASCOMweather/Device',''));
    ASCOMREST: begin
               if config.GetValue('/ASCOMRestweather/DeviceType',0)=0 then
                  WeatherName:='observingconditions/'+IntToStr(config.GetValue('/ASCOMRestweather/Device',0))
                else
                  WeatherName:='safetymonitor/'+IntToStr(config.GetValue('/ASCOMRestweather/Device',0));
           weather.Connect(config.GetValue('/ASCOMRestweather/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestweather/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestweather/Protocol',0)],
                          WeatherName,
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestweather/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestweather/Pass','')), encryptpwd));
               end;
  end;
end;

Procedure Tf_main.DisconnectWeather(Sender: TObject);
begin
 if weather.Status<>devDisconnected then weather.Disconnect;
end;

Procedure Tf_main.WeatherStatus(Sender: TObject);
begin
case weather.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelWeather.Font.Color:=clRed;
                      f_weather.Connected:=false;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsWeatherStati+' '+DevInterfaceName[ord(weather.WeatherInterface)]+' "'+weather.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelWeather.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelWeather.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelWeather.Font.Color:=clGreen;
                      NewMessage(Format(rsConnected, [rsWeatherStati]),1);
                      f_weather.Connected:=true;
                   end;
end;
WeatherClearChange(Sender);
CheckConnectionStatus;
end;

Procedure Tf_main.WeatherClearChange(Sender: TObject);
var ok: boolean;
begin
  if f_weather.Connected then begin
    ok:=weather.Clear;
    if f_weather.Clear<>ok then begin
      f_weather.Clear:=ok;
      NewMessage(Format(rsWeatherMonit, [BoolToStr(f_weather.Clear, rsGood, rsBad)]), 1);
      if not f_weather.Clear then NewMessage(Format(rsWeatherIssue, [weather.WeatherMessage]));
      f_sequence.WeatherChange(f_weather.Clear);
    end;
  end;
end;

Procedure Tf_main.ConnectSafety(Sender: TObject);
begin
  case safety.SafetyInterface of
    INDI : safety.Connect(config.GetValue('/INDIsafety/Server',''),
                          config.GetValue('/INDIsafety/ServerPort',''),
                          config.GetValue('/INDIsafety/Device',''),
                          '');
    ASCOM: safety.Connect(config.GetValue('/ASCOMsafety/Device',''));
    ASCOMREST: safety.Connect(config.GetValue('/ASCOMRestsafety/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestsafety/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestsafety/Protocol',0)],
                          'safetymonitor/'+IntToStr(config.GetValue('/ASCOMRestsafety/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestsafety/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestsafety/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectSafety(Sender: TObject);
begin
 if safety.Status<>devDisconnected then safety.Disconnect;
end;

Procedure Tf_main.SafetyStatus(Sender: TObject);
begin
case safety.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelSafety.Font.Color:=clRed;
                      f_safety.Connected:=false;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsSafetyMonito+' '+DevInterfaceName[ord(safety.SafetyInterface)]+' "'+safety.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelSafety.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelSafety.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelSafety.Font.Color:=clGreen;
                      f_safety.Connected:=true;
                      NewMessage(Format(rsConnected, [rsSafetyMonito]),1);
                   end;
end;
SafetySafeChange(Sender);
CheckConnectionStatus;
end;

Procedure Tf_main.SafetySafeChange(Sender: TObject);
var ok : boolean;
    i,n,k: integer;
    param: string;
    output: TStringList;
begin
  if f_safety.Connected then begin
    ok:=safety.Safe;
    if f_safety.Safe<>ok then begin
     f_safety.Safe:=ok;
     NewMessage(rsSafetyMonito+': '+BoolToStr(f_safety.Safe, rsSafe, rsUnsafe), 3);
     if not f_safety.Safe then begin
       NewMessage(rsUnsafeCondit);
       // Run actions
       for i:=0 to SafetyActionNum-1 do begin
          n:=round(config.GetValue('/Safety/Actions/Action'+inttostr(i),0));
          if (n<0)or(n>ord(high(TSafetyAction))) then n:=ord(safNothing);
          param:=trim(config.GetValue('/Safety/Actions/Parameter'+inttostr(i),''));
          try
          case TSafetyAction(n) of
            safNothing: continue;
            safShowPrompt: begin
               k:=StrToIntDef(param,30);
               f_pause.Caption:=rsUnsafeCondit;
               f_pause.Text:=Format(rsTheSafetyMon, [crlf, rsCancel, rsContinue]);
               if f_pause.Wait(k,false) then begin
                 NewMessage(rsUnsafeCondit2);
                 exit;
               end;
               if f_safety.Safe then exit;  // safe again, ignore
            end;
            safAbortSequence: begin
               NewMessage(rsAbortTheCurr);
               // stop sequence
               if f_sequence.Running then begin
                  f_sequence.AbortSequence;
                  wait(5);
               end
               // stop other capture
               else if f_capture.Running then begin
                  f_capture.BtnStartClick(Sender);
                  wait(5);
               end;
            end;
            safStopTelescope: begin
               NewMessage(rsStopTelescop2);
               if mount<>nil then mount.AbortMotion;
               wait(1);
            end;
            safParkTelescope: begin
               NewMessage(rsParkTheTeles2);
               if mount<>nil then mount.Park:=true;
               wait(1);
            end;
            safStopDomeSlaving: begin
               NewMessage(rsStopDomeSlav);
               if dome<>nil then dome.Slave:=false;
               wait(1);
            end;
            safParkDome: begin
               NewMessage(rsParkDome);
               if dome<>nil then dome.Park:=true;
               wait(1);
            end;
            safCloseDome: begin
               NewMessage(rsCloseDome);
               if dome<>nil then dome.Shutter:=false;
               wait(1);
            end;
            safWarmCamera: begin
               NewMessage(rsWarmTheCamer);
               if camera<>nil then camera.Temperature:=20;
               wait(1);
            end;
            safAutoguiderShutdown: begin
               NewMessage(rsAutoguiderSh);
               if autoguider<>nil then autoguider.Shutdown;
               wait(1);
            end;
            safPlanetariumShutdown: begin
               NewMessage(rsPlanetariumS);
               if planetarium<>nil then planetarium.Shutdown;
               wait(1);
            end;
            safExternalCommand: begin
               NewMessage(rsCallExternal+': '+param);
               output:=TStringList.Create;
               k:=ExecProcess(param,output);
               NewMessage('exit code = '+inttostr(k),3);
               for k:=0 to output.Count-1 do
                  NewMessage(output[k],3);
               output.Free;
            end;
            safExitProgram: begin
               NewMessage(rsExitProgram);
               ShutdownProgram(self);
            end;
          end;
          except
            on E: Exception do NewMessage('Safety action '+inttostr(i)+': '+inttostr(n)+': '+ E.Message,1);
          end;
       end;
     end;
    end;
  end;
end;

Procedure Tf_main.ConnectSwitch(Sender: TObject);
begin
  case switch.SwitchInterface of
    INDI : switch.Connect(config.GetValue('/INDIswitch/Server',''),
                          config.GetValue('/INDIswitch/ServerPort',''),
                          config.GetValue('/INDIswitch/Device',''),
                          '');
    ASCOM: switch.Connect(config.GetValue('/ASCOMswitch/Device',''));
    ASCOMREST: switch.Connect(config.GetValue('/ASCOMRestswitch/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestswitch/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestswitch/Protocol',0)],
                          'switch/'+IntToStr(config.GetValue('/ASCOMRestswitch/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestswitch/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestswitch/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectSwitch(Sender: TObject);
begin
 if switch.Status<>devDisconnected then switch.Disconnect;
 f_switch.Clear;
end;

Procedure Tf_main.SwitchStatus(Sender: TObject);
begin
case switch.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelSwitch.Font.Color:=clRed;
                      f_switch.Connected:=false;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsSwitch+' '+DevInterfaceName[ord(switch.SwitchInterface)]+' "'+switch.DeviceName+'" '+ellipsis]), 2);
                      f_devicesconnection.LabelSwitch.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelSwitch.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelSwitch.Font.Color:=clGreen;
                      f_switch.Connected:=true;
                      NewMessage(Format(rsConnected, [rsSwitch]),1);
                   end;
end;
SwitchChange(Sender);
CheckConnectionStatus;
end;

Procedure Tf_main.SwitchChange(Sender: TObject);
begin
 if f_switch.Connected then begin
   f_switch.NumSwitch:=switch.NumSwitch;
   f_switch.Switch:=switch.Switch;
 end;
end;

Procedure Tf_main.SetSwitch(Sender: TObject);
begin
 if f_switch.Connected then begin
   switch.Switch:=f_switch.Switch;
 end;
end;

Procedure Tf_main.ConnectCover(Sender: TObject);
begin
  case cover.CoverInterface of
    INDI : cover.Connect(config.GetValue('/INDIcover/Server',''),
                          config.GetValue('/INDIcover/ServerPort',''),
                          config.GetValue('/INDIcover/Device',''),
                          '');
    ASCOM: cover.Connect(config.GetValue('/ASCOMcover/Device',''));
    ASCOMREST: cover.Connect(config.GetValue('/ASCOMRestcover/Host',''),
                          IntToStr(config.GetValue('/ASCOMRestcover/Port',0)),
                          ProtocolName[config.GetValue('/ASCOMRestcover/Protocol',0)],
                          'covercalibrator/'+IntToStr(config.GetValue('/ASCOMRestcover/Device',0)),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestcover/User','')), encryptpwd),
                          DecryptStr(hextostr(credentialconfig.GetValue('/ASCOMRestcover/Pass','')), encryptpwd));
  end;
end;

Procedure Tf_main.DisconnectCover(Sender: TObject);
begin
 if cover.Status<>devDisconnected then cover.Disconnect;
end;

Procedure Tf_main.CoverStatus(Sender: TObject);
begin
case cover.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelCover.Font.Color:=clRed;
                      f_cover.Connected:=false;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsCoverCalibra+' '+DevInterfaceName[ord(cover.CoverInterface)]+' "'+cover.DeviceName+'" '+ellipsis]), 2);
                      f_devicesconnection.LabelCover.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelCover.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelCover.Font.Color:=clGreen;
                      f_cover.Connected:=true;
                      CoverChange(Sender);
                      NewMessage(Format(rsConnected, [rsCoverCalibra]),1);
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.CoverChange(Sender: TObject);
begin
 try
  f_cover.lock:=true;

  if cover.HasCover then begin
    f_cover.PanelCover.Visible:=true;
    f_cover.Cover:=cover.CoverState;
  end
  else f_cover.PanelCover.Visible:=false;

  if cover.HasCalibrator then begin
    f_cover.PanelCalibrator.Visible:=true;
    f_cover.Calibrator:=cover.CalibratorState;
    f_cover.Brightness.MaxValue:=cover.MaxBrightness;
    if f_cover.Calibrator<>calOff then f_cover.Brightness.Value:=cover.Brightness;
  end
  else f_cover.PanelCalibrator.Visible:=false;

 finally
  f_cover.lock:=false;
 end;
end;

Procedure Tf_main.OpenCover(Sender: TObject);
begin
  if cover.HasCalibrator and ((cover.CalibratorState=calReady)or(cover.CalibratorState=calNotReady)) then
    cover.CalibratorOff;
  cover.OpenCover;
  CoverChange(Sender);
end;

Procedure Tf_main.CloseCover(Sender: TObject);
begin
  cover.CloseCover;
  CoverChange(Sender);
end;

Procedure Tf_main.BrightnessChange(Sender: TObject);
begin
  cover.Brightness:=f_cover.Brightness.Value;
  if f_cover.Light.Checked then
    cover.CalibratorOn;
end;

Procedure Tf_main.SetCalibratorLight(Sender: TObject);
begin
  if f_cover.Light.Checked then begin
    cover.Brightness:=f_cover.Brightness.Value;
    cover.CalibratorOn;
  end
  else begin
    cover.CalibratorOff;
  end;
  CoverChange(Sender);
end;

procedure Tf_main.TabMsgLevelChange(Sender: TObject);
 var i: integer;
begin
   i:=TabMsgLevel.TabIndex+1;
   if i<>LogLevel then begin
     LogLevel:=i;
     f_msg.msg.Clear;
     for i:=0 to AllMsg.Count-1 do begin
        if TIntList(AllMsg.Objects[i]).value<=LogLevel then
           f_msg.msg.Lines.Add(AllMsg[i]);
     end;
   end;
end;

procedure Tf_main.ShowMsgTabs(Sender: TObject);
begin
  if f_msg.ShowTabs then begin
   case f_msg.Parent.Align of
    alBottom: begin
       PanelMsgTabs.Height:=DoScaleY(40);
       PanelMsgTabs.Width:=min(PanelCenter.Width,f_msg.Width-f_msg.Title.Width);
       PanelMsgTabs.Left:=max(0,PanelCenter.Width-PanelMsgTabs.Width);
       PanelMsgTabs.Top:=PanelCenter.Height-PanelMsgTabs.Height;
       TabMsgLevel.TabPosition:=tpTop;
    end;
    alTop: begin
       PanelMsgTabs.Height:=DoScaleY(40);
       PanelMsgTabs.Width:=min(PanelCenter.Width,f_msg.Width-f_msg.Title.Width);
       PanelMsgTabs.Left:=max(0,PanelCenter.Width-PanelMsgTabs.Width);
       PanelMsgTabs.Top:=0;
       TabMsgLevel.TabPosition:=tpBottom;
    end;
    alLeft: begin
       PanelMsgTabs.Width:=DoScaleY(120);
       PanelMsgTabs.Height:=DoScaleY(120);
       PanelMsgTabs.Left:=0;
       PanelMsgTabs.Top:=min(f_msg.top,PanelCenter.Height-PanelMsgTabs.Height);
       TabMsgLevel.TabPosition:=tpLeft;
    end;
    else begin // right
       PanelMsgTabs.Width:=DoScaleY(120);
       PanelMsgTabs.Height:=DoScaleY(120);
       PanelMsgTabs.Left:=max(0,PanelCenter.Width-PanelMsgTabs.Width);
       PanelMsgTabs.Top:=min(f_msg.top+TBTabs.Height,PanelCenter.Height-PanelMsgTabs.Height);
       TabMsgLevel.TabPosition:=tpLeft;
    end;
   end;

    PanelMsgTabs.Visible:=true;
  end
  else begin
    PanelMsgTabs.Visible:=false;
  end;
end;

procedure Tf_main.PanelMsgTabsMouseEnter(Sender: TObject);
begin
  f_msg.msgMouseEnter(sender);
end;

procedure Tf_main.PanelMsgTabsMouseLeave(Sender: TObject);
begin
 f_msg.msgMouseLeave(sender);
end;

procedure Tf_main.NewMessage(msg: string; level: integer=1);
var buf: string;
    ilevel:TIntList;
begin
 if (msg<>'')and(f_msg<>nil) then begin
  if (GetCurrentThreadId<>MainThreadID) then begin
    exit;
  end;
  if level<9 then begin
  buf:=FormatDateTime('hh:nn:ss',now)+blank+msg;
  if AllMsg.Count>100 then
     AllMsg.Delete(0);
  ilevel:=TIntList.Create;
  ilevel.value:=level;
  AllMsg.AddObject(buf,ilevel);
  if level<=LogLevel then begin
    if f_msg.msg.Lines.Count>100 then f_msg.msg.Lines.Delete(0);
    f_msg.msg.Lines.Add(buf);
    f_msg.msg.SelStart:=f_msg.msg.GetTextLen-1;
    f_msg.msg.SelLength:=0;
  end;
  end;
  if LogToFile then begin
    WriteLog(IntToStr(level)+': '+msg);
  end;
  if VoiceError and (level=0) then speak(msg);
 end;
end;

procedure Tf_main.DeviceMessage(msg: string; level: integer=1);
begin
 if msg<>'' then begin
  if LogToFile then begin
    WriteDeviceLog(msg);
  end;
  if pos('[ERROR]',msg)>0 then NewMessage(msg,1);
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
                   f_sequence.CameraDisconnected;
                   StatusBar1.Panels[panelstatus].Text:='';
                   f_devicesconnection.LabelCamera.Font.Color:=clRed;
                   if TBVideo.Visible then begin
                     TBConnect.Click;
                     TBVideo.Visible:=false;
                     MenuTabVideo.Visible:=false;
                   end;
                   end;
   devConnecting:  begin
                   NewMessage(Format(rsConnecting, [rsCamera+' '+DevInterfaceName[ord(camera.CameraInterface)]+' "'+camera.DeviceName+'" '+ellipsis]),2);
                   f_devicesconnection.LabelCamera.Font.Color:=clOrange;
                   end;
   devConnected:   begin
                   if f_devicesconnection.LabelCamera.Font.Color<>clGreen then NewMessage(Format(rsConnected, [rsCamera]),1);
                   f_devicesconnection.LabelCamera.Font.Color:=clGreen;
                   wait(1);
                   cool:=camera.Cooler;
                   CameraCoolerChange(cool);
                   if config.GetValue('/Cooler/CameraAutoCool',false) then begin
                      f_ccdtemp.Setpoint.Value:=config.GetValue('/Cooler/CameraAutoCoolTemp',0);
                      f_ccdtemp.BtnSet.Click;
                   end;
                   if camera.hasVideo then begin
                      wait(1);
                      TBVideo.Visible:=true;
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
                   ReadoutList.Assign(camera.ReadOutList);
                   f_option.ReadOutCapture.Items.Assign(ReadoutList);
                   f_option.ReadOutPreview.Items.Assign(ReadoutList);
                   f_option.ReadOutFocus.Items.Assign(ReadoutList);
                   f_option.ReadOutAstrometry.Items.Assign(ReadoutList);
                   if config.GetValue('/Sensor/MaxADUFromCamera',true) then begin
                      MaxADU:=camera.MaxADU;
                      config.SetValue('/Sensor/MaxADU',MaxADU);
                   end;
                   if not WantMount then StatusTimer.Enabled:=true;
                   end;
 end;
 CheckConnectionStatus;
end;

Procedure Tf_main.CameraDisconnected(Sender: TObject);
begin
 // device disconnected from server.
 // disconnect from server to allow a clean reconnection
 NewMessage('Camera disconnected!',1);
 camera.Disconnect;
end;

Procedure Tf_main.CameraExposureAborted(Sender: TObject);
begin
 AbortTimer.Enabled:=true;
end;

procedure Tf_main.AbortTimerTimer(Sender: TObject);
begin
  AbortTimer.Enabled:=false;
  if not ExpectedStop then begin
    ExpectedStop:=false;
    if Capture and f_capture.Running then NewMessage(rsExposureAbor,1);
    if f_starprofile.AutofocusRunning then f_starprofile.Autofocus(nil,-1,-1,-1);
    NewMessage(rsAbort,9);
  end;
  f_preview.stop;
  f_capture.stop;
  Capture:=false;
  Preview:=false;
  StatusBar1.Panels[panelstatus].Text:=rsStop;
  MenuCaptureStart.Caption:=f_capture.BtnStart.Caption;
end;

procedure  Tf_main.CameraFnumberChange(f:string);
begin
 f_preview.Fnumber.Text:=f;
 f_capture.Fnumber.Text:=f;
end;

procedure  Tf_main.CameraTemperatureChange(t:double);
begin
 f_ccdtemp.Current.Caption:=FormatFloat(f1,TempDisplay(TemperatureScale,t));
 if camera.TemperatureRampActive then f_ccdtemp.BtnSet.Caption:=rsCancel else f_ccdtemp.BtnSet.Caption:=rsSet;
end;

procedure Tf_main.CameraCoolerPowerChange(t:double);
begin
 f_ccdtemp.Power.Caption:=FormatFloat(f1,t);
end;

procedure Tf_main.CameraCoolerChange(var v:boolean);
begin
 if f_ccdtemp.CCDcooler.Checked<>v then begin
    f_ccdtemp.CCDcooler.Checked:=v;
    NewMessage(Format(rsCameraCooler, [': '+BoolToStr(v, rsTrue, rsFalse)]),2);
 end;
end;

procedure Tf_main.ReadyForVideo(var v: boolean);
begin
 v:=(camera.Status=devConnected)and(not f_sequence.Running)and
    (not f_preview.Running)and(not f_capture.Running)and
    (not autofocusing)and(not learningvcurve);
end;

procedure Tf_main.CameraVideoPreviewChange(Sender: TObject);
begin
  f_video.Preview.Checked:=camera.VideoPreviewRunning;
end;

procedure Tf_main.CameraVideoRecordChange(Sender: TObject);
begin
 if camera.VideoRecordRunning then begin
   f_video.BtnStartRec.Enabled:=false;
   f_video.BtnStopRec.Enabled:=true;
   f_video.LabelRecording.Caption:=rsRecording+' ...';
 end
 else begin
  f_video.BtnStartRec.Enabled:=true;
  f_video.BtnStopRec.Enabled:=false;
  f_video.LabelRecording.Caption:='';
end;
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
  if f_video.PanelExposure1.Visible then
    f_video.ShowExposure(camera.VideoExposure);
  if f_video.PanelExposure2.Visible then
    f_video.ShowExposure(camera.StreamingExposure);
  f_video.Gain.Position:=max(min(round(camera.VideoGain),f_video.Gain.Max),f_video.Gain.Min);
  f_video.Gamma.Position:=max(min(round(camera.VideoGamma),f_video.Gamma.Max),f_video.Gamma.Min);
  f_video.Brightness.Position:=max(min(round(camera.VideoBrightness),f_video.Brightness.Max),f_video.Brightness.Min);
end;

procedure Tf_main.CameraVideoEncoderChange(Sender: TObject);
begin
  if f_video.PanelEncoder.Visible and (f_video.VideoEncoder.Items.Count>0) then
    f_video.VideoEncoder.ItemIndex:=camera.VideoEncoder;
end;

Procedure Tf_main.WheelStatus(Sender: TObject);
begin
case wheel.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelWheel.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsFilterWheel+' '+DevInterfaceName[ord(wheel.WheelInterface)]+' "'+wheel.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelWheel.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage(Format(rsConnected, [rsFilterWheel]),1);
                      f_devicesconnection.LabelWheel.Font.Color:=clGreen;
                      f_filterwheel.Filters.Items.Assign(wheel.FilterNames);
                      f_EditTargets.StepList.Columns[pcolfilter-1].PickList.Assign(wheel.FilterNames);
                      f_EditTargets.FlatFilterList.Items.Assign(wheel.FilterNames);
                      if f_EditTargets.FlatFilterList.Items.Count>0 then f_EditTargets.FlatFilterList.Items.Delete(0);
                      FilterList.Assign(wheel.FilterNames);
                      SetFilterMenu;
                      if (wheel.Filter>0)and(wheel.Filter<=f_filterwheel.Filters.Items.Count) then begin
                         f_filterwheel.Filters.ItemIndex:=round(wheel.Filter);
                         CurrentFilterOffset:=FilterOffset[round(wheel.Filter)];
                         filteroffset_initialized:=true;
                      end;
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.SetFilter(Sender: TObject);
begin
  wheel.Filter:=f_filterwheel.Filters.ItemIndex;
end;

procedure Tf_main.FilterChange(n:double);
var o,f: integer;
begin
f:=round(n);
if f=-1 then begin
  // wheel moving
  f:=0;
  f_filterwheel.Filters.Items[0]:=rsRotate+'...';
end
else begin
  f_filterwheel.Filters.Items[0]:=Filter0;
end;
if (f<0)or(f>f_filterwheel.Filters.Items.Count) then begin
   // receive an invalid value, ignore
   NewMessage('Error, receive filter position: '+inttostr(f),1);
   exit;
end;
// show new filter name
if (f_filterwheel.Filters.ItemIndex<>f) then  begin
   f_filterwheel.Filters.ItemIndex:=f;
end;
// set exposure factor
AutofocusExposureFact:=FilterExpFact[f];
// adjust focus
if (n>0)and(n<=MaxFilter)and(focuser.Status=devConnected)and(f_focuser.Ready) then begin
 if CurrentFilterOffset<>FilterOffset[f] then begin
   if filteroffset_initialized then begin
    o:=FilterOffset[f]-CurrentFilterOffset;
    f_focuser.FocusSpeed:=abs(o);
    if o>0 then
      FocusOUT(nil)
    else
      FocusIN(nil);
    CurrentFilterOffset:=FilterOffset[f];
   end
   else begin
    CurrentFilterOffset:=FilterOffset[f];
    filteroffset_initialized:=true;
   end;
 end;
end;
end;

procedure Tf_main.FilterNameChange(Sender: TObject);
begin
f_filterwheel.Filters.Items.Assign(wheel.FilterNames);
f_EditTargets.StepList.Columns[pcolfilter-1].PickList.Assign(wheel.FilterNames);
f_EditTargets.FlatFilterList.Items.Assign(wheel.FilterNames);
if f_EditTargets.FlatFilterList.Items.Count>0 then f_EditTargets.FlatFilterList.Items.Delete(0);
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
begin
f_about.ShowModal;
end;

procedure Tf_main.MenuUsergroupClick(Sender: TObject);
begin
  ExecuteFile(URL_USERGROUP);
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

procedure Tf_main.MenuStatusClick(Sender: TObject);
var url: string;
begin
  if (TCPDaemon<>nil)and(not TCPDaemon.Finished) then begin
    url:=StringReplace(URL_PROGRAMSTATUS,'$port',TCPIPServerPort,[]);
    ExecuteFile(url);
  end
  else begin
    ShowMessage('Server not running! Please check the option for TCP/IP server in the program preferences');
  end;
end;


procedure Tf_main.MenuShowLogClick(Sender: TObject);
{$ifdef mswindows}
var i: integer;
{$endif}
begin
  if LogFileOpen then begin
     {$ifdef mswindows}
     i:=ExecuteFile(LogFile);
     if i<=32 then
        ShowMessage('Error '+inttostr(i)+crlf+'Check if the file exist and set the application to use to open files with .log extension');
     {$else}
     ExecuteFile(LogFile);
     {$endif}
  end
  else
     ShowMessage(rsPleaseActiva);
end;

procedure Tf_main.MenuShowINDIlogClick(Sender: TObject);
{$ifdef mswindows}
var i: integer;
{$endif}
begin
  if DeviceLogFileOpen then begin
     {$ifdef mswindows}
     i:=ExecuteFile(DeviceLogFile);
     if i<=32 then
        ShowMessage('Error '+inttostr(i)+crlf+'Check if the file exist and set the application to use to open files with .log extension');
     {$else}
     ExecuteFile(DeviceLogFile);
     {$endif}
  end
  else
     ShowMessage(rsPleaseActiva);
end;

procedure Tf_main.MenuBrowseLogClick(Sender: TObject);
begin
  ExecuteFile(LogDir);
end;

Procedure Tf_main.FocuserStatus(Sender: TObject);
begin
case focuser.Status of
  devDisconnected:begin
                      f_devicesconnection.LabelFocuser.Font.Color:=clRed;
                  end;
  devConnecting:  begin
                      NewMessage(Format(rsConnecting, [rsFocuser+' '+DevInterfaceName[ord(focuser.FocuserInterface)]+' "'+focuser.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelFocuser.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      NewMessage(Format(rsConnected, [rsFocuser]),1);
                      f_devicesconnection.LabelFocuser.Font.Color:=clGreen;
                      FocuserConnectTimer.Enabled:=false;
                      FocuserConnectTimer.Enabled:=true;
                   end;
end;
CheckConnectionStatus;
end;

procedure Tf_main.FocuserPositionChange(n:double);
begin
  f_focuser.Position.Value:=round(n);
end;

procedure Tf_main.FocuserSpeedChange(n:double);
begin
  f_focuser.speed.Value:=round(n);
end;

procedure Tf_main.FocuserTimerChange(n:double);
begin
  f_focuser.timer.Value:=round(n);
end;

procedure Tf_main.FocuserTemperatureChange(n:double);
begin
if n<>NullCoord then begin
  f_focuser.Temp.Text:=FormatFloat(f1,TempDisplay(TemperatureScale,n));
  FocuserTemp:=n;
end;
end;


function Tf_main.FocuserTemperatureCompensation(canwait:boolean):boolean;
var p: integer;
    dt: double;
begin
  result:=false;
  if focuser.hasTemperature and (FocuserTempCoeff<>0.0) and (FocuserLastTemp<>NullCoord) then begin
    dt:=abs(FocuserLastTemp-FocuserTemp);
    // only if temperature change by more than 0.5C and less than 50C
    if (dt>0.5) and (dt<50) then begin
     result:=true;
     if canwait then begin
      p:=f_focuser.TempOffset(FocuserLastTemp,FocuserTemp);
      if focuser.hasAbsolutePosition and (p<>0) then begin
        NewMessage(Format(rsFocuserTempe2, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel,IntToStr(p)]),2);
        focuser.Position:=focuser.Position+p;
      end
      else if focuser.hasRelativePosition and (p<>0) then begin
        NewMessage(Format(rsFocuserTempe2, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel,IntToStr(p)]),2);
        if p>0 then focuser.FocusOut else focuser.FocusIn;
        focuser.RelPosition:=abs(p);
      end;
      wait(1);
     end;
    end;
  end;
end;

procedure Tf_main.FocusIN(Sender: TObject);
var n,p:integer;
begin
 n:=0;
 if focuser.hasAbsolutePosition then begin
    val(f_focuser.PosIncr.Text,p,n);
    if n=0 then begin
       focuser.Position:=f_focuser.Position.Value-p;
    end;
 end
 else if focuser.hasRelativePosition then begin
    val(f_focuser.RelIncr.Text,p,n);
    if n=0 then begin
      focuser.FocusIn;
      focuser.RelPosition:=p;
    end;
 end
 else begin
    p:=f_focuser.speed.Value;
    focuser.Speed:=p;
    focuser.FocusIn;
    p:=f_focuser.timer.Value;
    focuser.Timer:=p;
 end;
 if n<>0 then NewMessage(rsInvalidNumer,1);
end;

procedure Tf_main.FocusOUT(Sender: TObject);
var n,p:integer;
begin
 n:=0;
 if focuser.hasAbsolutePosition then begin
    val(f_focuser.PosIncr.Text,p,n);
    if n=0 then begin
       focuser.Position:=f_focuser.Position.Value+p;
    end;
 end
 else if focuser.hasRelativePosition then begin
    val(f_focuser.RelIncr.Text,p,n);
    if n=0 then begin
      focuser.FocusOut;
      focuser.RelPosition:=p;
    end;
 end
 else begin
    p:=f_focuser.speed.Value;
    focuser.Speed:=p;
    focuser.FocusOut;
    p:=f_focuser.timer.Value;
    focuser.Timer:=p;
 end;
 if n<>0 then NewMessage(rsInvalidNumer,1);
end;

procedure Tf_main.FocusSetAbsolutePosition(Sender: TObject);
var p: integer;
begin
 if focuser.hasAbsolutePosition then begin
   p:=f_focuser.Position.Value;
   focuser.Position:=p;
 end
end;

procedure Tf_main.FocusVcurveLearning(Sender: TObject);
begin
  if not focuser.hasAbsolutePosition then begin
    NewMessage(rsCannotGetFoc,1);
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
  end;
  f_vcurve.ShowHint:=ShowHint;
  if VcCenterpos<>NullCoord then f_vcurve.FocusPos.Value:=VcCenterpos else f_vcurve.FocusPos.Value:=focuser.Position;
  if VcHalfwidth<>NullCoord then f_vcurve.HalfWidth.Value:=VcHalfwidth else f_vcurve.HalfWidth.Value:=500;
  f_vcurve.Nsteps.Value:=VcNsteps;
  formpos(f_vcurve,mouse.CursorPos.x,mouse.CursorPos.y);
  f_vcurve.Show;
  f_vcurve.LoadCurve;
end;

function Tf_main.doVcurve(centerp,hw,n,nsum: integer;exp:double;bin,again,aoffset:integer):boolean;
var i,j,k,minpos,maxpos,step,sumpos,numpos:integer;
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
 f_preview.StackPreview.Checked:=false;
 NewMessage(Format(rsFromToBy, [IntToStr(minpos), IntToStr(centerp), IntToStr(step)]),2);
 if focuser.hasTemperature then begin
    NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
    AutofocusVcTemp1:=FocuserTemp;
 end;
 if step<1 then exit;
 hfdmin:=9999;
 SetLength(hfdlist,nsum);
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
   // use bad pixel map
   fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
   // use dark
   fits.DarkOn:=true;
   // average hfd for nsum exposures
   for j:=1 to nsum do begin
     if not f_preview.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeFocus,again,aoffset) then begin
       NewMessage(rsExposureFail,1);
       TerminateVcurve:=true;
     end;
     if TerminateVcurve then begin
       NewMessage(rsStopVcurveLe,1);
       LoadVcurve;
       f_vcurve.LoadCurve;
       exit;
     end;
     f_starprofile.showprofile(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
     hfdlist[j-1]:=f_starprofile.HFD;
     NewMessage('Measurement '+inttostr(j)+' hfd:'+FormatFloat(f1,f_starprofile.hfd)+' peak:'+FormatFloat(f1,f_starprofile.ValMax)+' snr:'+FormatFloat(f1,f_starprofile.SNR),2);
   end;
   hfd:=SMedian(hfdlist);
   // store result always from left to right
   AutofocusVc[k,1]:=focuser.Position;
   AutofocusVc[k,2]:=hfd;
   NewMessage('Vcurve n'+inttostr(i)+' pos:'+FormatFloat(f0,AutofocusVc[k,1])+' hfd:'+FormatFloat(f1,AutofocusVc[k,2])+' peak:'+FormatFloat(f1,f_starprofile.ValMax)+' snr:'+FormatFloat(f1,f_starprofile.SNR),2);
   if f_vcurve<>nil then
      f_vcurve.LearnProgress(i,AutofocusVc[k,1],AutofocusVc[k,2]);
   // find minimal hfd value
   if AutofocusVc[k,2]<hfdmin then begin
     hfdmin:=AutofocusVc[k,2];
   end;
 end;
 AutofocusVcNum:=n;
 AutofocusVcDir:=AutofocusMoveDir;
 if hfdmin=9999 then begin
   NewMessage(rsCannotDetect,1);
   exit;
 end;
 // search the central point of the flat central part of the curve to split right and left curve
 sumpos:=0;
 numpos:=0;
 for i:=0 to n do begin
   if abs(hfdmin-AutofocusVc[i,2])<(0.1*hfdmin) then begin
    inc(numpos);
    sumpos:=sumpos+i;
   end;
 end;
 PosFocus:=round(sumpos/numpos);
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
   NewMessage(rsCannotReachN,1);
   exit;
 end;
 if (PosNearL<0)or(PosNearR<0) then begin
   NewMessage(rsCannotReachS,1);
   exit;
 end;
 AutofocusVcNum:=n;
 if focuser.hasTemperature then begin
   AutofocusVcTemp2:=FocuserTemp;
   AutofocusVcTemp:=(AutofocusVcTemp1+AutofocusVcTemp2)/2;
   NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
 end
 else
   AutofocusVcTemp:=0;
 NewMessage('Near L:'+inttostr(round(AutofocusVc[PosNearL,1])),2);
 NewMessage('Center:'+inttostr(round(AutofocusVc[PosFocus,1])),2);
 NewMessage('Near R:'+inttostr(round(AutofocusVc[PosNearR,1])),2);
 result:=true;
end;

procedure Tf_main.LearnVcurve(Sender: TObject);
var bin: integer;
    x,y,xc,yc,xc1,yc1,s,s2,s3,s4,savepos: integer;
    SaveZoom,vmax: double;
begin
 if not focuser.hasAbsolutePosition then exit;
 // read parameters
 VcCenterpos:=f_vcurve.FocusPos.Value;
 VcHalfwidth:=f_vcurve.HalfWidth.Value;
 VcNsteps:=f_vcurve.Nsteps.Value;
 if VcNsteps>99 then begin
   VcNsteps:=99;
   f_vcurve.Nsteps.Text:='99';
 end;
 if (VcCenterpos=NullCoord)or(VcHalfwidth=NullCoord) then exit;
 savepos:=VcCenterpos;
 AutofocusVcFilterOffset:=CurrentFilterOffset;
 try
 // find a bright star
 focuser.Position:=VcCenterpos;
 wait(1);
 bin:=AutofocusBinning;
 // use bad pixel map
 fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
 // use dark
 fits.DarkOn:=true;
 if (not f_starprofile.FindStar) then begin
   if not f_preview.ControlExposure(f_preview.Exposure,bin,bin,LIGHT,ReadoutModeFocus,AutofocusGain,AutofocusOffset) then begin
      NewMessage(rsExposureFail,1);
      exit;
   end;
   x:=fits.HeaderInfo.naxis1 div 2;
   y:=fits.HeaderInfo.naxis2 div 2;
   s:=2*min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 3;
   fits.FindBrightestPixel(x,y,s,starwindow div 2,xc1,yc1,vmax);
   f_starprofile.FindStar:=(vmax>0);
   f_starprofile.StarX:=xc1;
   f_starprofile.StarY:=yc1;
 end
 else begin
   xc1 := round(f_starprofile.StarX);
   yc1 := round(f_starprofile.StarY);
 end;

 if not f_starprofile.FindStar then begin
   NewMessage(rsCannotFindAS,1);
   exit;
 end;
 learningvcurve:=true;
 // set focus frame around the star
 s:=min(Focuswindow,min(fits.HeaderInfo.naxis1 div 2,fits.HeaderInfo.naxis2 div 2));
 s2:=s div 2;
 Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
 Screen2CCD(x,y,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,xc,yc);
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
 NewMessage(rsStartLearnin,1);
 if not doVcurve(VcCenterpos,VcHalfwidth,VcNsteps,AutofocusNearNum,f_preview.Exposure,bin,AutofocusGain,AutofocusOffset) then begin
   // error return focuser to initial position
   focuser.Position:=savepos;
   wait(1);
   exit;
 end;
 // compute and save the curve
 ComputeVcSlope;
 SaveVcurve;
 AutofocusSlippageOffset:=0;
 config.SetValue('/StarAnalysis/AutofocusSlippageOffset',AutofocusSlippageOffset);
 // position focuser at new center
 if f_vcurve.Quality>0.9 then
    focuser.Position:=round((AutofocusVcpiL+AutofocusVcpiR)/2)
 else
    focuser.Position:=round(AutofocusVc[PosFocus,1]);
 wait(1);
 finally
 // reset camera
 fits.SetBPM(bpm,0,0,0,0);
 fits.DarkOn:=false;
 learningvcurve:=false;
 camera.ResetFrame;
 f_preview.StackPreview.Checked:=false;
 f_visu.Zoom:=SaveZoom;
 ImgZoom:=f_visu.Zoom;
 f_starprofile.FindStar:=false; {deselect the star so a new run will search for the new star position}
 StartPreviewExposure(self);
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
  try
  config.DeletePath('/StarAnalysis/Vcurve');
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcDir',AutofocusVcDir);
  config.SetValue('/StarAnalysis/Vcurve/AutofocusVcBinning',AutofocusBinning);
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
  except
    on E: Exception do NewMessage('Error saving Vcurve: '+ E.Message,1);
  end;
 end;
 // move the focuser at the center point as we like this curve
 focuser.Position:=round((AutofocusVcpiL+AutofocusVcpiR)/2);
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
                      NewMessage(Format(rsConnecting, [rsRotator+' '+DevInterfaceName[ord(rotator.RotatorInterface)]+' "'+rotator.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelRotator.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelRotator.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelRotator.Font.Color:=clGreen;
                      NewMessage(Format(rsConnected, [rsRotator]),1);
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
 f_rotator.Angle.Value:=rotator.Angle;
 f_rotator.SetCalibrated(rotator.CalibrationAngle<>0);
end;

Procedure Tf_main.RotatorRotate(Sender: TObject);
var a: double;
begin
 a:=f_rotator.Angle.Value;
 a:=rmod(a+360,360);
 rotator.Angle:=a;
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
                      NewMessage(Format(rsConnecting, [rsMount+' '+DevInterfaceName[ord(mount.MountInterface)]+' "'+mount.DeviceName+'" '+ellipsis]),2);
                      f_devicesconnection.LabelMount.Font.Color:=clOrange;
                   end;
  devConnected:   begin
                      if f_devicesconnection.LabelMount.Font.Color=clGreen then exit;
                      f_devicesconnection.LabelMount.Font.Color:=clGreen;
                      NewMessage(Format(rsConnected, [rsMount]),1);
                      wait(1);
                      if config.GetValue('/Mount/SetDateTime',false) then begin
                         if mount.SetDate(NowUTC,ObsTimeZone) then
                            NewMessage(rsTimeSendToTe, 3);
                      end;
                      if config.GetValue('/Mount/SetObservatory',false) then begin
                         if mount.SetSite(-ObsLongitude, ObsLatitude, ObsElevation) then
                            NewMessage(rsSiteSendToTe, 3);
                      end
                      else if config.GetValue('/Mount/GetObservatory',false) then begin
                         if mount.GetSite(ObsLongitude, ObsLatitude, ObsElevation) then begin
                           ObsLongitude:=to180(-ObsLongitude);
                           config.SetValue('/Info/ObservatoryLatitude',ObsLatitude);
                           config.SetValue('/Info/ObservatoryLongitude',ObsLongitude);
                           config.SetValue('/Info/ObservatoryElevation',ObsElevation);
                           NewMessage(rsSiteSetFromT, 3);
                         end;
                      end;
                      MountCoordChange(Sender);
                      StatusTimer.Enabled:=false; // let time to initialize before to do too much
                      StatusTimer.Enabled:=true;
                   end;
end;
CheckConnectionStatus;
end;

Procedure Tf_main.MountCoordChange(Sender: TObject);
var ra,de,a,h: double;
begin
 ra:=mount.RA;
 de:=mount.Dec;
 cmdEq2Hz(ra,de,a,h);
 f_mount.RA.Caption:=RAToStr(ra);
 f_mount.DE.Caption:=DEToStr(de);
 f_mount.AZ.Caption:=FormatFloat(f2,a);
 f_mount.ALT.Caption:=FormatFloat(f2,h);
end;

Procedure Tf_main.MountPiersideChange(Sender: TObject);
begin
  case mount.PierSide of
    pierEast: f_mount.Pierside.Caption:=rsEastPointing;
    pierWest: f_mount.Pierside.Caption:=rsWestPointing;
    pierUnknown: f_mount.Pierside.Caption:=rsUnknowPierSi;
  end;
end;

Procedure Tf_main.MountTrackingChange(Sender: TObject);
begin
   if mount.Tracking then begin
      f_mount.BtnTrack.Font.Color:=clGreen;
      f_sequence.MountTrackingStarted;
   end
   else begin
      f_mount.BtnTrack.Font.Color:=clRed;
      if (f_sequence.Running)and(not meridianflipping)and(not mount.MountSlewing)and(not WeatherPauseCapture) then f_sequence.MountTrackingStopped;
   end;
end;

Procedure Tf_main.MountParkChange(Sender: TObject);
begin
 if mount.Park then begin
    f_mount.BtnPark.Caption:=rsParked;
    f_mount.BtnPark.Font.Color:=clRed
 end
 else begin
    f_mount.BtnPark.Caption:=rsUnparked;
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
 // do not test for mount.tracking here
 mount.Track;
end;

procedure Tf_main.MountGoto(Sender: TObject);
var ra,de,err:double;
    tra,tde,objn: string;
begin
 if (AllDevicesConnected) and (mount.Status=devConnected) then begin
   if Mount.Park then begin
     NewMessage(rsTheTelescope);
     exit;
   end;
   if f_preview.Running then begin
     if f_preview.Loop then
       f_preview.BtnLoopClick(nil)
     else
       f_preview.BtnPreviewClick(nil);
     wait(2);
   end;
   if  astrometry.Busy then begin
     NewMessage(rsResolverAlre,1);
     exit;
   end;
   if (f_capture.Running or f_sequence.Running) then begin
      NewMessage(rsCannotStartW, 1);
      exit;
   end;
   FormPos(f_goto,mouse.CursorPos.X,mouse.CursorPos.Y);
   f_goto.Caption:=rsGoto;
   f_goto.msginfo.Caption:='';
   f_goto.PanelPxSz.Visible:=false;
   f_goto.PanelAltAz.Visible:=true;
   f_goto.ButtonOK.Caption:=rsGoto;
   f_goto.ShowModal;
   if f_goto.ModalResult=mrok then begin
     CancelAutofocus:=false;
     tra:= f_goto.Ra.Text;
     tde:=f_goto.De.Text;
     objn:=trim(f_goto.Obj.Text);
     if tra='' then
       ra:=NullCoord
     else
       ra:=StrToAR(tra);
     if tde='' then
       de:=NullCoord
     else
       de:=StrToDE(tde);
     if (ra<>NullCoord) and (de<>NullCoord) then begin
       if autoguider.State=GUIDER_GUIDING then begin
         NewMessage(rsStopAutoguid,2);
         autoguider.Guide(false);
         autoguider.WaitBusy(15);
       end;
       NewMessage(rsGoto+': '+objn,1);
       J2000ToMount(mount.EquinoxJD,ra,de);
       if astrometry.PrecisionSlew(ra,de,err) then begin
         f_capture.Fname.Text:=objn;
        end
       else NewMessage(format(rsError,[rsGoto+': '+objn]) ,1);
     end
     else NewMessage(rsInvalidCoord,1);
   end;
 end;
end;

Procedure Tf_main.AutoguiderConnectClick(Sender: TObject);
var i: integer;
begin
 if f_autoguider.BtnConnect.Caption=rsConnect then begin
   i:=config.GetValue('/Autoguider/Software',2);
   case TAutoguiderType(i) of
    agPHD:       autoguider.Connect(config.GetValue('/Autoguider/PHDhostname','localhost'),config.GetValue('/Autoguider/PHDport','4400'));
    agLINGUIDER: begin
               if config.GetValue('/Autoguider/LinGuiderUseUnixSocket',true) then begin
                 autoguider.Connect(config.GetValue('/Autoguider/Autoguider/LinGuiderSocket','/tmp/lg_ss'));
               end
               else begin
                autoguider.Connect(config.GetValue('/Autoguider/LinGuiderHostname','localhost'),config.GetValue('/Autoguider/LinGuiderPort','5656'));
               end;
    end;
    agNONE: exit;
  end;
 end else begin
   autoguider.Disconnect;
 end;
 f_autoguider.Status.Text:=autoguider.Status;
 NewMessage(Format(rsAutoguider+': %s', [autoguider.Status]),1);
end;

Procedure Tf_main.AutoguiderCalibrateClick(Sender: TObject);
begin
  autoguider.Calibrate;
end;

Procedure Tf_main.AutoguiderGuideClick(Sender: TObject);
var onoff:boolean;
begin
 if f_autoguider.BtnGuide.Caption=rsGuide then begin
    onoff:=true;
 end else begin
   onoff:=false;
 end;
 autoguider.Guide(onoff);
end;

Procedure Tf_main.AutoguiderDitherClick(Sender: TObject);
begin
 autoguider.Dither(DitherPixel, DitherRAonly, DitherWaitTime);
end;

Procedure Tf_main.AutoguiderConnect(Sender: TObject);
begin
 f_autoguider.BtnConnect.Caption:=rsDisconnect;
 MenuAutoguiderConnect.Caption:=f_autoguider.BtnConnect.Caption;
 autoguider.ConnectGear;
 autoguider.SettleTolerance(SettlePixel,SettleMinTime, SettleMaxTime);
end;

Procedure Tf_main.AutoguiderDisconnect(Sender: TObject);
var i: integer;
begin
 if not AppClose then begin
   NewMessage(format(rsDisconnected,[rsAutoguider]),1);
   f_sequence.AutoguiderDisconnected;
   // autoguider will be free automatically, create a new one for next connection
   i:=config.GetValue('/Autoguider/Software',2);
   case TAutoguiderType(i) of
     agPHD: autoguider:=T_autoguider_phd.Create;
     agLINGUIDER: autoguider:=T_autoguider_linguider.Create;
     agNONE: autoguider:=T_autoguider_none.Create;
     agDITHER: autoguider:=T_autoguider_dither.Create;
   end;
   autoguider.Mount:=mount;
   autoguider.onStatusChange:=@AutoguiderStatus;
   autoguider.onConnect:=@AutoguiderConnect;
   autoguider.onDisconnect:=@AutoguiderDisconnect;
   autoguider.onShowMessage:=@NewMessage;
   f_sequence.Autoguider:=autoguider;
   f_scriptengine.Autoguider:=autoguider;
   f_script.Autoguider:=autoguider;
   f_autoguider.Status.Text:=autoguider.Status;
   f_autoguider.DitherOnly:=autoguider.AutoguiderType=agDITHER;
   NewMessage(Format(rsAutoguider+': %s', [autoguider.Status]),1);
   f_autoguider.BtnConnect.Caption:=rsConnect;
   f_autoguider.BtnGuide.Caption:=rsGuide;
   f_autoguider.led.Brush.Color:=clGray;
   MenuAutoguiderConnect.Caption:=f_autoguider.BtnConnect.Caption;
   MenuAutoguiderGuide.Caption:=f_autoguider.BtnGuide.Caption;
   StatusBar1.Invalidate;
 end;
end;

Procedure Tf_main.AutoguiderStatus(Sender: TObject);
begin
 if f_autoguider.Status.Text<>autoguider.Status then NewMessage(Format(rsAutoguider+': %s', [autoguider.Status]),2);
 f_autoguider.Status.Text:=autoguider.Status;
 case autoguider.State of
   GUIDER_DISCONNECTED:begin
                       f_autoguider.led.Brush.Color:=clGray;
                       f_autoguider.BtnGuide.Caption:=rsGuide;
                       MenuAutoguiderGuide.Caption:=rsGuide;
                       end;
   GUIDER_IDLE        :begin
                       f_autoguider.led.Brush.Color:=clYellow;
                       f_autoguider.BtnGuide.Caption:=rsGuide;
                       MenuAutoguiderGuide.Caption:=rsGuide;
                       if (not meridianflipping)and(not autofocusing)and(not WeatherPauseCapture)and(not RecenteringTarget)
                          then f_sequence.AutoguiderIddle;
                       end;
   GUIDER_GUIDING     :begin
                       f_autoguider.led.Brush.Color:=clLime;
                       f_autoguider.BtnGuide.Caption:=rsStop;
                       MenuAutoguiderGuide.Caption:=rsStopGuiding;
                       end;
   GUIDER_BUSY        :begin
                       f_autoguider.led.Brush.Color:=clOrange;
                       f_autoguider.BtnGuide.Caption:=rsStop;
                       MenuAutoguiderGuide.Caption:=rsStopGuiding;
                       end;
   GUIDER_ALERT       :begin
                       f_autoguider.led.Brush.Color:=clRed;
                       f_autoguider.BtnGuide.Caption:=rsGuide;
                       MenuAutoguiderGuide.Caption:=rsGuide;
                       end;
 end;
 if autoguider.LastError<>'' then NewMessage(Format(rsAutoguider+': %s', [autoguider.LastError]),1);
 StatusBar1.Invalidate;
end;

Procedure Tf_main.AutoguiderGetSigma(axis:integer; out sigma: double);
var i,n,count: integer;
    val,runningMean,newMean,newS,runningS: double;
begin
 // compute RA or DEC RMS guide error
 // use the same method as PHD2 to get the same number
 // see DescriptiveStats::AddValue and DescriptiveStats::GetSigma in guiding_stats.cpp
 sigma:=0;
 runningS:=0;
 n:=Length(AutoguiderStat);
 if n<2 then exit;
 for i:=0 to n-1 do begin
   count:=i+1;
   Val:=AutoguiderStat[i,axis];
   if count=1 then begin
     runningMean := Val;
     newMean := Val;
   end
   else begin
     newMean := runningMean + (Val - runningMean) / count;
     newS := runningS + (Val - runningMean) * (Val - newMean);
     runningMean := newMean;
     runningS := newS;
   end;
 end;
 sigma:=sqrt(runningS / (count - 1));
end;

Procedure Tf_main.AutoguiderGuideStat;
var i,n: integer;
    ma,mi,smmax,ras,des,tots: double;
begin
   // Add current point to list, keep last 100 points
   n:=Length(AutoguiderStat);
   if n<100 then begin
     SetLength(AutoguiderStat,n+1);
   end
   else begin
     for i:=1 to n-1 do
       AutoguiderStat[i-1]:=AutoguiderStat[i];
     n:=n-1;
   end;
   AutoguiderStat[n,1]:=autoguider.RAdistance;
   AutoguiderStat[n,2]:=autoguider.Decdistance;
   AutoguiderStat[n,3]:=autoguider.Starmass;
   // show graph and stats
   AutoguiderGuideGraph(nil);
end;

Procedure Tf_main.AutoguiderGuideGraph(Sender: TObject);
var i,n: integer;
    ma,mi,smmax,ras,des,tots: double;
begin
   n:=Length(AutoguiderStat);
   if (n>0) and f_autoguider.ShowStat.Checked then begin
     if n>1 then begin
       // show guide error
       AutoguiderGetSigma(1,ras);
       AutoguiderGetSigma(2,des);
       tots:=sqrt(ras*ras+des*des);
       f_autoguider.Label1.Caption:='RA:'+FormatFloat(f2,ras)+'" Dec:'+FormatFloat(f2,des)+'" Tot:'+FormatFloat(f2,tots)+'"';
     end
     else begin
       f_autoguider.Label1.Caption:=' ';
     end;
     // prepare chart
     f_autoguider.GuideChartRAdist.Clear;
     f_autoguider.GuideChartDecdist.Clear;
     f_autoguider.GuideChartStarmass.Clear;
     smmax:=0; ma:=0; mi:=0;
     for i:=0 to Length(AutoguiderStat)-1 do begin
        // add ra and dec points
        f_autoguider.GuideChartRAdist.Add(AutoguiderStat[i,1]);
        f_autoguider.GuideChartDecdist.Add(AutoguiderStat[i,2]);
        // max values for starmass scaling
        ma:=max(ma,AutoguiderStat[i,1]);
        ma:=max(ma,AutoguiderStat[i,2]);
        mi:=min(mi,AutoguiderStat[i,1]);
        mi:=min(mi,AutoguiderStat[i,2]);
        smmax:=max(smmax,AutoguiderStat[i,3]);
     end;
     // restrict vertical scale to +/- 2"
     if ma<2 then begin
        ma:=2;
        f_autoguider.GuideChart.AxisList[0].range.Max:=ma;
        f_autoguider.GuideChart.AxisList[0].range.UseMax:=true;
     end
     else f_autoguider.GuideChart.AxisList[0].range.UseMax:=false;
     if mi>-2 then begin
        f_autoguider.GuideChart.AxisList[0].range.Min:=-2;
        f_autoguider.GuideChart.AxisList[0].range.UseMin:=true;
     end
     else f_autoguider.GuideChart.AxisList[0].range.UseMin:=false;
     // starmass scaling factor
     ma:=ma/smmax;
     for i:=0 to Length(AutoguiderStat)-1 do begin
        // add scaled starmass points
        f_autoguider.GuideChartStarmass.Add(AutoguiderStat[i,3]*ma);
     end;
   end;
end;

Procedure Tf_main.AutoguiderClearStat(Sender: TObject);
begin
  SetLength(AutoguiderStat,0);
end;

procedure Tf_main.MenuViewhdrClick(Sender: TObject);
var f: Tf_viewtext;
begin
 if fits.HeaderInfo.valid then begin
   f:=Tf_viewtext.Create(self);
   f.Caption:=rsImageStatist;
   f.Memo1.Lines:=fits.Header.Rows;
   if trim(fits.Title)='' then
      f.Caption:=rsFITSHeader
   else
      f.Caption:=SysToUTF8(fits.Title);
   FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
   f.Show;
 end;
end;

procedure Tf_main.MenuImgStatClick(Sender: TObject);
var f: Tf_viewtext;
    txt: string;
begin
 if fits.HeaderInfo.valid then begin
   f:=Tf_viewtext.Create(self);
   f.Width:=DoScaleX(250);
   f.Height:=DoScaleY(250);
   f.Caption:=rsImageStatist;
   txt:=fits.GetStatistics;
   if (WCScenterRA<>NullCoord) and (WCScenterDEC<>NullCoord)
   then begin
     txt:=txt+crlf+rsFromPlateSol+':'+crlf;
     txt:=txt+rsCenterRA+FormatFloat(f0,cdcWCSinfo.eqout)+':'+blank+RAToStr(WCScenterRA/15)+crlf;
     txt:=txt+rsCenterDec+FormatFloat(f0,cdcWCSinfo.eqout)+':'+blank+DEToStr(WCScenterDEC)+crlf;
     if (WCSwidth<>NullCoord) and (WCSheight<>NullCoord) then begin
        if WCSwidth>10 then
          txt:=txt+rsFOV+':'+blank+FormatFloat(f2, WCSwidth)+'x'+FormatFloat(f2, WCSheight)+sdeg+crlf
        else
          txt:=txt+rsFOV+':'+blank+FormatFloat(f2, WCSwidth*60)+'x'+FormatFloat(f2, WCSheight*60)+smin+crlf;
     end;
     if cdcWCSinfo.secpix>0 then begin
       txt:=txt+rsImageScale+':'+blank+FormatFloat(f2, cdcWCSinfo.secpix)+blank+ssec+'/'+rsPixels+crlf;
     end;
   end;
   f.Memo1.Text:=txt;
   FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
   f.Show;
 end;
end;

procedure Tf_main.MenuSaveConfigClick(Sender: TObject);
begin
 SaveSettings;
 SaveConfig;
end;

procedure Tf_main.MenuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_main.MenuSetupClick(Sender: TObject);
var configfile: string;
    loadopt: boolean;
    pt: TPoint;
    i:integer;
begin
  if camera.Status<>devDisconnected then begin
    ShowMessage(rsDisconnectTh);
    exit;
  end;
  loadopt:=false;
  f_setup.ShowHint:=ShowHint;
  f_setup.DefaultCameraInterface:=camera.CameraInterface;
  f_setup.DefaultMountInterface:=mount.MountInterface;
  f_setup.DefaultDomeInterface:=dome.DomeInterface;
  f_setup.DefaultWheelInterface:=wheel.WheelInterface;
  f_setup.DefaultFocuserInterface:=focuser.FocuserInterface;
  f_setup.DefaultRotatorInterface:=rotator.RotatorInterface;
  f_setup.DefaultWeatherInterface:=weather.WeatherInterface;
  f_setup.DefaultSafetyInterface:=safety.SafetyInterface;
  f_setup.DefaultSwitchInterface:=switch.SwitchInterface;
  f_setup.DefaultCoverInterface:=cover.CoverInterface;
  f_setup.profile:=profile;
  f_setup.LoadProfileList;
  f_setup.Loadconfig(config,credentialconfig);
  if sender is Tf_devicesconnection then begin
    f_setup.Pagecontrol1.ActivePageIndex:=0;
    f_setup.SetActivePageButton;
  end;
  pt.x:=PanelCenter.Left;
  pt.y:=PanelCenter.top;
  pt:=ClientToScreen(pt);
  FormPos(f_setup,pt.X,pt.Y);
  f_setup.ShowModal;

  if f_setup.ModalResult=mrOK then begin
    if profile<>f_setup.profile then begin
      SaveConfig;
      ProfileFromCommandLine:=false;
      profile:=f_setup.profile;
      if profile='default' then
         configfile:='ccdciel.conf'
      else
         configfile:='ccdciel_'+profile+'.conf';
      loadopt:=FileExistsUTF8(slash(ConfigDir)+configfile);
      OpenConfig(configfile);
      f_devicesconnection.ProfileLabel.Caption:=profile;
      caption:='CCDciel '+ccdcielver+blank+profile;
      ConfigDarkFile:=slash(ConfigDir)+'darkframe_'+profile+'.fits';
      if FileExistsUTF8(ConfigDarkFile) then begin
        if fits.DarkFrame=nil then begin
           fits.DarkFrame:=TFits.Create(nil);
           fits.DarkFrame.onMsg:=@NewMessage;
        end;
        fits.DarkFrame.LoadFromFile(ConfigDarkFile);
      end
      else begin
        fits.FreeDark;
      end;
    end;
    config.SetValue('/Devices/Timeout',f_setup.IndiTimeout.Text);

    config.SetValue('/Devices/Camera',true);
    config.SetValue('/Devices/FilterWheel',f_setup.DeviceFilterWheel.Checked);
    config.SetValue('/Devices/Focuser',f_setup.DeviceFocuser.Checked);
    config.SetValue('/Devices/Rotator',f_setup.DeviceRotator.Checked);
    config.SetValue('/Devices/Mount',f_setup.DeviceMount.Checked);
    config.SetValue('/Devices/Dome',f_setup.DeviceDome.Checked);
    config.SetValue('/Devices/Watchdog',f_setup.DeviceWatchdog.Checked);
    config.SetValue('/Devices/Weather',f_setup.DeviceWeather.Checked);
    config.SetValue('/Devices/Safety',f_setup.DeviceSafety.Checked);
    config.SetValue('/Devices/Switch',f_setup.DeviceSwitch.Checked);
    config.SetValue('/Devices/Cover',f_setup.DeviceCover.Checked);

    config.SetValue('/CameraInterface',ord(f_setup.CameraConnection));
    config.SetValue('/INDIcamera/Server',f_setup.CameraIndiServer.Text);
    config.SetValue('/INDIcamera/ServerPort',f_setup.CameraIndiPort.Text);
    if f_setup.CameraIndiDevice.Text<>'' then config.SetValue('/INDIcamera/Device',f_setup.CameraIndiDevice.Text);
    config.SetValue('/INDIcamera/Sensor',f_setup.CameraSensor);
    config.SetValue('/INDIcamera/AutoLoadConfig',f_setup.CameraAutoLoadConfig.Checked);
    config.SetValue('/INDIcamera/IndiTransfert',f_setup.CameraIndiTransfert.ItemIndex);
    config.SetValue('/INDIcamera/IndiTransfertDir',f_setup.CameraIndiTransfertDir.Text);
    config.SetValue('/ASCOMcamera/Device',f_setup.AscomCamera.Text);
    config.SetValue('/ASCOMcamera/FlipImage',f_setup.FlipImage.Checked);
    config.SetValue('/ASCOMcamera/CameraDateObs',f_setup.CameraDateObs.Checked);
    config.SetValue('/ASCOMcamera/FixPixelRange',f_setup.FixPixelRange.Checked);
    config.SetValue('/ASCOMRestcamera/Protocol',f_setup.CameraARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestcamera/Host',f_setup.CameraARestHost.Text);
    config.SetValue('/ASCOMRestcamera/Port',f_setup.CameraARestPort.Value);
    config.SetValue('/ASCOMRestcamera/Device',f_setup.CameraARestDevice.Value);
    config.SetValue('/ASCOMRestcamera/FlipImage',f_setup.FlipImage1.Checked);
    config.SetValue('/ASCOMRestcamera/CameraDateObs',f_setup.CameraDateObs1.Checked);
    config.SetValue('/ASCOMRestcamera/FixPixelRange',f_setup.FixPixelRange1.Checked);

    config.SetValue('/FilterWheelInterface',ord(f_setup.WheelConnection));
    config.SetValue('/INDIwheel/Server',f_setup.WheelIndiServer.Text);
    config.SetValue('/INDIwheel/ServerPort',f_setup.WheelIndiPort.Text);
    if f_setup.WheelIndiDevice.Text<>'' then config.SetValue('/INDIwheel/Device',f_setup.WheelIndiDevice.Text);
    config.SetValue('/INDIwheel/AutoLoadConfig',f_setup.WheelAutoLoadConfig.Checked);
    config.SetValue('/ASCOMwheel/Device',f_setup.AscomWheel.Text);
    config.SetValue('/ASCOMRestwheel/Protocol',f_setup.WheelARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestwheel/Host',f_setup.WheelARestHost.Text);
    config.SetValue('/ASCOMRestwheel/Port',f_setup.WheelARestPort.Value);
    config.SetValue('/ASCOMRestwheel/Device',f_setup.WheelARestDevice.Value);
    config.SetValue('/Manualwheel/Slots',f_setup.ManualFilterName.RowCount-1);
    for i:=1 to f_setup.ManualFilterName.RowCount-1 do
      config.SetValue('/Manualwheel/Slot'+inttostr(i),f_setup.ManualFilterName.Cells[1,i]);

    config.SetValue('/FocuserInterface',ord(f_setup.FocuserConnection));
    config.SetValue('/INDIfocuser/Server',f_setup.FocuserIndiServer.Text);
    config.SetValue('/INDIfocuser/ServerPort',f_setup.FocuserIndiPort.Text);
    if f_setup.FocuserIndiDevice.Text<>'' then config.SetValue('/INDIfocuser/Device',f_setup.FocuserIndiDevice.Text);
    config.SetValue('/INDIfocuser/AutoLoadConfig',f_setup.FocuserAutoLoadConfig.Checked);
    config.SetValue('/ASCOMfocuser/Device',f_setup.AscomFocuser.Text);
    config.SetValue('/ASCOMRestfocuser/Protocol',f_setup.FocuserARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestfocuser/Host',f_setup.FocuserARestHost.Text);
    config.SetValue('/ASCOMRestfocuser/Port',f_setup.FocuserARestPort.Value);
    config.SetValue('/ASCOMRestfocuser/Device',f_setup.FocuserARestDevice.Value);
    config.SetValue('/Focuser/ExternalTemperature',f_setup.FocuserExternalTemperature.Visible and f_setup.FocuserExternalTemperature.Checked);

    config.SetValue('/RotatorInterface',ord(f_setup.RotatorConnection));
    config.SetValue('/INDIrotator/Server',f_setup.RotatorIndiServer.Text);
    config.SetValue('/INDIrotator/ServerPort',f_setup.RotatorIndiPort.Text);
    if f_setup.RotatorIndiDevice.Text<>'' then config.SetValue('/INDIrotator/Device',f_setup.RotatorIndiDevice.Text);
    config.SetValue('/INDIrotator/AutoLoadConfig',f_setup.RotatorAutoLoadConfig.Checked);
    config.SetValue('/ASCOMrotator/Device',f_setup.AscomRotator.Text);
    config.SetValue('/ASCOMRestrotator/Protocol',f_setup.RotatorARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestrotator/Host',f_setup.RotatorARestHost.Text);
    config.SetValue('/ASCOMRestrotator/Port',f_setup.RotatorARestPort.Value);
    config.SetValue('/ASCOMRestrotator/Device',f_setup.RotatorARestDevice.Value);

    config.SetValue('/MountInterface',ord(f_setup.MountConnection));
    config.SetValue('/INDImount/Server',f_setup.MountIndiServer.Text);
    config.SetValue('/INDImount/ServerPort',f_setup.MountIndiPort.Text);
    if f_setup.MountIndiDevice.Text<>'' then config.SetValue('/INDImount/Device',f_setup.MountIndiDevice.Text);
    config.SetValue('/INDImount/AutoLoadConfig',f_setup.MountAutoLoadConfig.Checked);
    config.SetValue('/ASCOMmount/Device',f_setup.AscomMount.Text);
    config.SetValue('/Mount/SetDateTime',f_setup.MountSetDateTime.Checked);
    config.SetValue('/Mount/SetObservatory',f_setup.MountSetObservatory.Checked);
    config.SetValue('/Mount/GetObservatory',f_setup.MountGetObservatory.Checked);
    config.SetValue('/ASCOMRestmount/Protocol',f_setup.MountARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestmount/Host',f_setup.MountARestHost.Text);
    config.SetValue('/ASCOMRestmount/Port',f_setup.MountARestPort.Value);
    config.SetValue('/ASCOMRestmount/Device',f_setup.MountARestDevice.Value);

    config.SetValue('/DomeInterface',ord(f_setup.DomeConnection));
    config.SetValue('/INDIdome/Server',f_setup.DomeIndiServer.Text);
    config.SetValue('/INDIdome/ServerPort',f_setup.DomeIndiPort.Text);
    if f_setup.DomeIndiDevice.Text<>'' then config.SetValue('/INDIdome/Device',f_setup.DomeIndiDevice.Text);
    config.SetValue('/INDIdome/AutoLoadConfig',f_setup.DomeAutoLoadConfig.Checked);
    config.SetValue('/ASCOMdome/Device',f_setup.AscomDome.Text);
    config.SetValue('/ASCOMRestdome/Protocol',f_setup.DomeARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestdome/Host',f_setup.DomeARestHost.Text);
    config.SetValue('/ASCOMRestdome/Port',f_setup.DomeARestPort.Value);
    config.SetValue('/ASCOMRestdome/Device',f_setup.DomeARestDevice.Value);

    config.SetValue('/INDIwatchdog/Server',f_setup.WatchdogIndiServer.Text);
    config.SetValue('/INDIwatchdog/ServerPort',f_setup.WatchdogIndiPort.Text);
    if f_setup.WatchdogIndiDevice.Text<>'' then config.SetValue('/INDIwatchdog/Device',f_setup.WatchdogIndiDevice.Text);
    config.SetValue('/INDIwatchdog/Threshold',f_setup.WatchdogThreshold.Text);
    config.SetValue('/INDIwatchdog/AutoLoadConfig',f_setup.WatchdogAutoLoadConfig.Checked);

    config.SetValue('/WeatherInterface',ord(f_setup.WeatherConnection));
    config.SetValue('/INDIweather/Server',f_setup.WeatherIndiServer.Text);
    config.SetValue('/INDIweather/ServerPort',f_setup.WeatherIndiPort.Text);
    if f_setup.WeatherIndiDevice.Text<>'' then config.SetValue('/INDIweather/Device',f_setup.WeatherIndiDevice.Text);
    config.SetValue('/INDIweather/AutoLoadConfig',f_setup.WeatherAutoLoadConfig.Checked);
    config.SetValue('/ASCOMweather/Device',f_setup.AscomWeather.Text);
    config.SetValue('/ASCOMweather/DeviceType',f_setup.AscomWeatherType.ItemIndex);
    config.SetValue('/ASCOMRestweather/Protocol',f_setup.WeatherARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestweather/Host',f_setup.WeatherARestHost.Text);
    config.SetValue('/ASCOMRestweather/Port',f_setup.WeatherARestPort.Value);
    config.SetValue('/ASCOMRestweather/Device',f_setup.WeatherARestDevice.Value);
    config.SetValue('/ASCOMRestweather/DeviceType',f_setup.AscomRestWeatherType.ItemIndex);

    config.SetValue('/SafetyInterface',ord(f_setup.SafetyConnection));
    config.SetValue('/INDIsafety/Server',f_setup.SafetyIndiServer.Text);
    config.SetValue('/INDIsafety/ServerPort',f_setup.SafetyIndiPort.Text);
    if f_setup.SafetyIndiDevice.Text<>'' then config.SetValue('/INDIsafety/Device',f_setup.SafetyIndiDevice.Text);
    config.SetValue('/INDIsafety/AutoLoadConfig',f_setup.SafetyAutoLoadConfig.Checked);
    config.SetValue('/ASCOMsafety/Device',f_setup.AscomSafety.Text);
    config.SetValue('/ASCOMRestsafety/Protocol',f_setup.SafetyARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestsafety/Host',f_setup.SafetyARestHost.Text);
    config.SetValue('/ASCOMRestsafety/Port',f_setup.SafetyARestPort.Value);
    config.SetValue('/ASCOMRestsafety/Device',f_setup.SafetyARestDevice.Value);

    config.SetValue('/SwitchInterface',ord(f_setup.SwitchConnection));
    config.SetValue('/INDIswitch/Server',f_setup.SwitchIndiServer.Text);
    config.SetValue('/INDIswitch/ServerPort',f_setup.SwitchIndiPort.Text);
    if f_setup.SwitchIndiDevice.Text<>'' then config.SetValue('/INDIswitch/Device',f_setup.SwitchIndiDevice.Text);
    config.SetValue('/INDIswitch/AutoLoadConfig',f_setup.SwitchAutoLoadConfig.Checked);
    config.SetValue('/ASCOMswitch/Device',f_setup.AscomSwitch.Text);
    config.SetValue('/ASCOMRestswitch/Protocol',f_setup.SwitchARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestswitch/Host',f_setup.SwitchARestHost.Text);
    config.SetValue('/ASCOMRestswitch/Port',f_setup.SwitchARestPort.Value);
    config.SetValue('/ASCOMRestswitch/Device',f_setup.SwitchARestDevice.Value);

    config.SetValue('/CoverInterface',ord(f_setup.CoverConnection));
    config.SetValue('/INDIcover/Server',f_setup.CoverIndiServer.Text);
    config.SetValue('/INDIcover/ServerPort',f_setup.CoverIndiPort.Text);
    if f_setup.CoverIndiDevice.Text<>'' then config.SetValue('/INDIcover/Device',f_setup.CoverIndiDevice.Text);
    config.SetValue('/INDIcover/AutoLoadConfig',f_setup.CoverAutoLoadConfig.Checked);
    config.SetValue('/ASCOMcover/Device',f_setup.AscomCover.Text);
    config.SetValue('/ASCOMRestcover/Protocol',f_setup.CoverARestProtocol.ItemIndex);
    config.SetValue('/ASCOMRestcover/Host',f_setup.CoverARestHost.Text);
    config.SetValue('/ASCOMRestcover/Port',f_setup.CoverARestPort.Value);
    config.SetValue('/ASCOMRestcover/Device',f_setup.CoverARestDevice.Value);

    credentialconfig.Clear;
    if (f_setup.CameraARestUser.Text+f_setup.WheelARestUser.Text+f_setup.FocuserARestUser.Text+f_setup.RotatorARestUser.Text+
       f_setup.MountARestUser.Text+f_setup.DomeARestUser.Text+f_setup.WeatherARestUser.Text+f_setup.SafetyARestUser.Text+
       f_setup.SwitchARestUser.Text+f_setup.CoverARestUser.Text <> '')
       then
       begin
          credentialconfig.Filename:=config.Filename+'.credential';
          credentialconfig.SetValue('/ASCOMRestcamera/User',strtohex(encryptStr(f_setup.CameraARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestwheel/User',strtohex(encryptStr(f_setup.WheelARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestfocuser/User',strtohex(encryptStr(f_setup.FocuserARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestrotator/User',strtohex(encryptStr(f_setup.RotatorARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestmount/User',strtohex(encryptStr(f_setup.MountARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestdome/User',strtohex(encryptStr(f_setup.DomeARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestweather/User',strtohex(encryptStr(f_setup.WeatherARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestsafety/User',strtohex(encryptStr(f_setup.SafetyARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestswitch/User',strtohex(encryptStr(f_setup.SwitchARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestcover/User',strtohex(encryptStr(f_setup.CoverARestUser.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestcamera/Pass',strtohex(encryptStr(f_setup.CameraARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestwheel/Pass',strtohex(encryptStr(f_setup.WheelARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestfocuser/Pass',strtohex(encryptStr(f_setup.FocuserARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestrotator/Pass',strtohex(encryptStr(f_setup.RotatorARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestmount/Pass',strtohex(encryptStr(f_setup.MountARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestdome/Pass',strtohex(encryptStr(f_setup.DomeARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestweather/Pass',strtohex(encryptStr(f_setup.WeatherARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestsafety/Pass',strtohex(encryptStr(f_setup.SafetyARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestswitch/Pass',strtohex(encryptStr(f_setup.SwitchARestPass.Text, encryptpwd)));
          credentialconfig.SetValue('/ASCOMRestcover/Pass',strtohex(encryptStr(f_setup.CoverARestPass.Text, encryptpwd)));
       end
       else begin
          credentialconfig.Filename:='';
          DeleteFile(config.Filename+'.credential');
       end;

    DestroyDevices;
    CreateDevices;

    ShowActiveTools;

    SetConfig;
    if loadopt then SetOptions;


    if config.GetValue('/Filters/Num',-1)<0 then begin //new empty profile, open options
      f_option.PageControl1.ActivePageIndex:=1;
      MenuOptions.Click;
    end;
  end;
end;

procedure Tf_main.MenuOptionsClick(Sender: TObject);
var ok: boolean;
    i,n,k,FocusStarMagIndex: integer;
    x:double;
    buf,langname:string;
    fs : TSearchRec;
    pt: TPoint;
begin
   f_option.ShowHint:=ShowHint;
   f_option.LockTemp:=true;
   f_option.Caption:=Format(rsOptions, [profile]);
   f_option.onGetPixelSize:=@OptionGetPixelSize;
   f_option.onGetMaxADU:=@OptionGetMaxADU;
   f_option.onGetFocale:=@OptionGetFocaleLength;
   f_option.Languages.Clear;
   i:=FindFirstUTF8(slash(appdir) + slash('data') + slash('language') + 'ccdciel.*.po',0,fs);
   while i=0 do begin
     buf:=ExtractFileNameOnly(fs.Name);
     delete(buf,1,8);
     if buf='en' then langname:='English (US)'
     else if buf='en_GB' then langname:='English (GB)'
     else if buf='cs' then langname:='Czech'
     else if buf='de' then langname:='Deutch'
     else if buf='es' then langname:='Español'
     else if buf='fr' then langname:='Français'
     else if buf='it' then langname:='Italiano'
     else if buf='ru' then langname:='русский Russian'
     else langname:='';
     f_option.Languages.Items.Add(buf+', '+langname);
     i:=FindNextUTF8(fs);
   end;
   FindCloseUTF8(fs);
   for i:=0 to f_option.Languages.Items.Count-1 do begin
      buf:=f_option.Languages.Items[i];
      n:=pos(',',buf);
      if n>0 then buf:=copy(buf,1,n-1);
      if buf=lang then begin
        f_option.Languages.ItemIndex:=i;
        break;
      end;
   end;
   f_option.CbShowHints.Checked:=screenconfig.GetValue('/Hint/Show',true);
   f_option.CaptureDir.Text:=config.GetValue('/Files/CapturePath',defCapturePath);
   f_option.TempDir.Text:=config.GetValue('/Files/TmpDir',TmpDir);
   f_option.TCPIPport.Value:=config.GetValue('/Files/TCPIPConfigPort',3277);
   f_option.PythonCmd.Text:=config.GetValue('/Script/PythonCmd',PythonCmd);
   f_option.FolderOptions.RowCount:=SubDirCount;
   for i:=0 to SubDirCount-1 do begin
     f_option.FolderOptions.Cells[2,i]:=SubDirName[ord(SubDirOpt[i])];
     if SubDirActive[i] then
       f_option.FolderOptions.Cells[1,i]:='1'
     else
       f_option.FolderOptions.Cells[1,i]:='0'
   end;
   f_option.FileOptions.RowCount:=FileNameCount;
   for i:=0 to FileNameCount-1 do begin
    f_option.FileOptions.Cells[2,i]:=FileNameName[ord(FileNameOpt[i])];
    if FileNameActive[i] then
      f_option.FileOptions.Cells[1,i]:='1'
    else
      f_option.FileOptions.Cells[1,i]:='0'
   end;
   f_option.FilenameSep.ItemIndex:=0;
   for i:=0 to f_option.FilenameSep.Items.Count-1 do begin
     if f_option.FilenameSep.Items[i]=FilenameSep then begin
        f_option.FilenameSep.ItemIndex:=i;
     end;
   end;
   if FileSequenceWidth>0 then begin
      f_option.UseFileSequenceWidth.Checked:=true;
      f_option.FileSequenceWidth.Enabled:=true;
      f_option.FileSequenceWidth.Value:=FileSequenceWidth;
   end
   else begin
      f_option.UseFileSequenceWidth.Checked:=false;
      f_option.FileSequenceWidth.Enabled:=false;
   end;
   f_option.FilePack.checked:=config.GetValue('/Files/Pack',false);
   f_option.WantExif.Checked:=config.GetValue('/Files/Exif',WantExif);
   f_option.SaveBitmap.Checked:=config.GetValue('/Files/SaveBitmap',false);
   buf:=config.GetValue('/Files/SaveBitmapFormat','png');
   if buf='png' then f_option.SaveBitmapFormat.ItemIndex:=0
   else if buf='tif' then f_option.SaveBitmapFormat.ItemIndex:=1
   else if buf='jpg' then f_option.SaveBitmapFormat.ItemIndex:=2
   else if buf='bmp' then f_option.SaveBitmapFormat.ItemIndex:=3
   else f_option.SaveBitmapFormat.ItemIndex:=0;
   f_option.Logtofile.Checked:=config.GetValue('/Log/Messages',true);
   f_option.Logtofile.Hint:=Format(rsLogFilesAreS, [ExtractFilePath(LogFile)]);
   f_option.debug_msg.Checked:=config.GetValue('/Log/debug_msg',debug_msg);
   f_option.LoadObservatoryDB(config.GetValue('/Info/ObservatoryName',''));
   f_option.ObservatoryName.Text:=config.GetValue('/Info/ObservatoryName','');
   f_option.Latitude:=config.GetValue('/Info/ObservatoryLatitude',0.0);
   f_option.Longitude:=config.GetValue('/Info/ObservatoryLongitude',0.0);
   f_option.ObsElev.Value:=config.GetValue('/Info/ObservatoryElevation',0.0);
   f_option.ObserverName.Text:=config.GetValue('/Info/ObserverName','');
   f_option.TelescopeName.Text:=config.GetValue('/Info/TelescopeName','');
   f_option.HorizonFile.FileName:=config.GetValue('/Info/HorizonFile','');
   f_option.ElevationMin.Value:=config.GetValue('/Info/ElevationMin',10.0);
   f_option.DebayerPreview.Checked:=config.GetValue('/Color/Bayer',false);
   f_option.BayerMode.ItemIndex:=config.GetValue('/Color/BayerMode',4);
   f_option.RedBalance.Position:=round(100*config.GetValue('/Color/RedBalance',1.0));
   f_option.GreenBalance.Position:=round(100*config.GetValue('/Color/GreenBalance',0.7));
   f_option.BlueBalance.Position:=round(100*config.GetValue('/Color/BlueBalance',0.9));
   f_option.BalanceFromCamera.Checked:=config.GetValue('/Color/BalanceFromCamera',true);
   f_option.BGneutralization.Checked:=config.GetValue('/Color/BGneutralization',true);
   f_option.ClippingHigh.Value:=config.GetValue('/Color/ClippingOverflow',MAXWORD);
   f_option.ClippingLow.Value:=config.GetValue('/Color/ClippingUnderflow',0);
   f_option.BPMsigma.Value:=config.GetValue('/BadPixel/Sigma',5);
   f_option.StackShow.Checked:=config.GetValue('/PreviewStack/StackShow',false);
   f_option.VideoPreviewRate.Value:=config.GetValue('/Video/PreviewRate',5);
   f_option.VideoGroup.Visible:=(camera.CameraInterface=INDI);
   f_option.RefTreshold.Position:=config.GetValue('/RefImage/Treshold',128);
   f_option.RefColor.ItemIndex:=config.GetValue('/RefImage/Color',0);
   x:=config.GetValue('/Cooler/TemperatureSlope',TemperatureSlope);
   if abs(x)<0.001 then x:=0;
   f_option.TemperatureSlope.Value:=x;
   f_option.PanelTemperatureSlope.Visible:=(x<>0);
   f_option.CameraAutoCool.Checked:=config.GetValue('/Cooler/CameraAutoCool',false);
   f_option.CameraAutoCoolTemp.Value:=config.GetValue('/Cooler/CameraAutoCoolTemp',0);
   f_option.TemperatureScale.ItemIndex:=config.GetValue('/Cooler/TemperatureScale',TemperatureScale);
   if ReadoutList.Count>0 then begin
     f_option.ReadOutCapture.ItemIndex:=config.GetValue('/Readout/Capture',0);
     f_option.ReadOutPreview.ItemIndex:=config.GetValue('/Readout/Preview',0);
     f_option.ReadOutFocus.ItemIndex:=config.GetValue('/Readout/Focus',0);
     f_option.ReadOutAstrometry.ItemIndex:=config.GetValue('/Readout/Astrometry',0);
   end
   else begin
     f_option.ReadOutCapture.ItemIndex:=-1;
     f_option.ReadOutPreview.ItemIndex:=-1;
     f_option.ReadOutFocus.ItemIndex:=-1;
     f_option.ReadOutAstrometry.ItemIndex:=-1;
   end;
   f_option.FlatType.ItemIndex:=config.GetValue('/Flat/FlatType',ord(FlatType));
   f_option.FlatAutoExposure.Checked:=config.GetValue('/Flat/FlatAutoExposure',FlatAutoExposure);
   f_option.FlatMinExp.Value:=config.GetValue('/Flat/FlatMinExp',FlatMinExp);
   f_option.FlatMaxExp.Value:=config.GetValue('/Flat/FlatMaxExp',FlatMaxExp);
   f_option.FlatLevelMin.Value:=config.GetValue('/Flat/FlatLevelMin',FlatLevelMin);
   f_option.FlatLevelMax.Value:=config.GetValue('/Flat/FlatLevelMax',FlatLevelMax);
   f_option.DomeFlatTelescopeSlew.Checked:=config.GetValue('/Flat/DomeFlatTelescopeSlew',DomeFlatTelescopeSlew);
   f_option.DomeFlatTelescopeAz.Value:=config.GetValue('/Flat/DomeFlatTelescopeAz',DomeFlatTelescopeAz);
   f_option.DomeFlatTelescopeAlt.Value:=config.GetValue('/Flat/DomeFlatTelescopeAlt',DomeFlatTelescopeAlt);
   f_option.DomeFlatSetLight.Checked:=config.GetValue('/Flat/DomeFlatSetLight',DomeFlatSetLight);
   f_option.DomeFlatSetLightON.Text:=config.GetValue('/Flat/DomeFlatSetLightON',DomeFlatSetLightON);
   f_option.DomeFlatSetLightOFF.Text:=config.GetValue('/Flat/DomeFlatSetLightOFF',DomeFlatSetLightOFF);
   f_option.FocusWindow.Value:=config.GetValue('/StarAnalysis/Focus',Focuswindow);
   f_option.FocusWindow.MinValue:=4*Starwindow;
   f_option.Undersampled.Checked:=config.GetValue('/StarAnalysis/Undersampled',Undersampled);
   f_option.FilterList.Cells[0, 0]:=rsFilterName;
   f_option.FilterList.Cells[1, 0]:=rsFocuserOffse;
   f_option.FilterList.Cells[2, 0]:=rsExposureFact;
   for i:=1 to f_option.FilterList.RowCount-1 do begin
     f_option.FilterList.Cells[0,i]:='';
     f_option.FilterList.Cells[1,i]:='';
     f_option.FilterList.Cells[2,i]:='';
   end;
   for i:=1 to FilterList.Count-1 do begin
     f_option.FilterList.Cells[0,i]:=FilterList[i];
     f_option.FilterList.Cells[1,i]:=FormatFloat(f0,config.GetValue('/Filters/Offset'+IntToStr(i),0));
     f_option.FilterList.Cells[2,i]:=FormatFloat(f1,config.GetValue('/Filters/ExpFact'+IntToStr(i),1.0));
   end;
   f_option.FilterList.Row:=0;
   f_option.FilterList.Col:=0;
   f_option.SetAutofocusmode(TAutofocusMode(config.GetValue('/StarAnalysis/AutoFocusMode',ord(AutoFocusMode))));
   f_option.AutofocusMinSpeed.Value:=config.GetValue('/StarAnalysis/AutofocusMinSpeed',AutofocusMinSpeed);
   f_option.AutofocusMaxSpeed.Value:=config.GetValue('/StarAnalysis/AutofocusMaxSpeed',AutofocusMaxSpeed);
   f_option.AutofocusStartHFD.Value:=config.GetValue('/StarAnalysis/AutofocusStartHFD',AutofocusStartHFD);
   f_option.AutofocusNearHFD.Value:=config.GetValue('/StarAnalysis/AutofocusNearHFD',AutofocusNearHFD);
   f_option.AutofocusExp:=config.GetValue('/StarAnalysis/AutofocusExposure',AutofocusExposure);
   if hasGainISO then
     f_option.AutofocusISObox.ItemIndex:=config.GetValue('/StarAnalysis/AutofocusGain',AutofocusGain)
   else
     f_option.AutofocusGainEdit.Value:=config.GetValue('/StarAnalysis/AutofocusGain',AutofocusGain);
   f_option.AutofocusOffsetEdit.Value:=config.GetValue('/StarAnalysis/AutofocusOffset',AutofocusOffset);
   f_option.AutofocusBinning.Value:=config.GetValue('/StarAnalysis/AutofocusBinning',AutofocusBinning);
   if (camera.Status=devConnected)and(camera.BinXrange<>NullRange) then
       f_option.AutofocusBinning.MaxValue:=round(camera.BinXrange.max)
   else
       f_option.AutofocusBinning.MaxValue:=max(4,AutofocusBinning);
   f_option.FocuserBacklash.Value:=config.GetValue('/StarAnalysis/FocuserBacklash',focuser.Backlash);
   f_option.FocuserBacklashActive.checked:=config.GetValue('/StarAnalysis/FocuserBacklashActive',(focuser.Backlash<>0));
   if config.GetValue('/StarAnalysis/FocuserBacklashDirection',FocusDirIn) then
      f_option.FocuserBacklashDirection.ItemIndex:=0
   else
      f_option.FocuserBacklashDirection.ItemIndex:=1;
   f_option.FocuserDelay.Value:=config.GetValue('/StarAnalysis/FocuserDelay',FocuserDelay);
   f_option.FocuserTempCoeff.Value:=config.GetValue('/StarAnalysis/FocuserTempCoeff',FocuserTempCoeff);
   f_option.AutofocusTemp.Value:=config.GetValue('/StarAnalysis/AutofocusTemp',AutofocusTempChange);
   if TemperatureScale=1 then begin
      f_option.FocuserTempCoeff.Value:=f_option.FocuserTempCoeff.Value*5/9;
      f_option.AutofocusTemp.Value:=f_option.AutofocusTemp.Value*5/9;
   end;
   f_option.AutofocusPeriod.Value:=config.GetValue('/StarAnalysis/AutofocusPeriod',AutofocusPeriod);
   f_option.AutofocusTolerance.Value:=config.GetValue('/StarAnalysis/AutofocusTolerance',AutofocusTolerance);
   f_option.AutofocusMinSNR.Value:=config.GetValue('/StarAnalysis/AutofocusMinSNR',AutofocusMinSNR);
   f_option.AutofocusSlippageCorrection.Checked:=config.GetValue('/StarAnalysis/AutofocusSlippageCorrection',AutofocusSlippageCorrection);
   f_option.AutofocusSlippageOffset.Value:=config.GetValue('/StarAnalysis/AutofocusSlippageOffset',AutofocusSlippageOffset);
   FocusStarMagIndex:=config.GetValue('/StarAnalysis/AutofocusStarMag',4)-4;
   if (FocusStarMagIndex<0)or(FocusStarMagIndex>4) then FocusStarMagIndex:=0;
   f_option.FocusStarMag.ItemIndex:=FocusStarMagIndex;
   f_option.FocusStarMagAdjust.Checked:=config.GetValue('/StarAnalysis/FocusStarMagAdjust',false);
   f_option.AutofocusPrecisionSlew.Value:=config.GetValue('/StarAnalysis/AutofocusPrecisionSlew',2.0);
   ok:=config.GetValue('/StarAnalysis/AutofocusMoveDir',FocusDirIn);
   f_option.AutofocusMoveDirIn.Checked:=ok;
   f_option.AutofocusMoveDirOut.Checked:=not ok;
   f_option.AutofocusNearNum.Value:=config.GetValue('/StarAnalysis/AutofocusNearNum',AutofocusNearNum);
   ok:=config.GetValue('/StarAnalysis/AutofocusInPlace',true);
   f_option.AutofocusInPlace.Checked:=ok;
   f_option.AutofocusSlew.Checked:=not ok;
   f_option.AutofocusPauseGuider.Checked:=config.GetValue('/StarAnalysis/AutofocusPauseGuider',AutofocusPauseGuider);
   f_option.AutofocusMultiStarCenter.Checked:=config.GetValue('/StarAnalysis/AutofocusMultiStarCenter',AutofocusMultiStarCenter);
   f_option.AutofocusDynamicNumPoint.Value:=config.GetValue('/StarAnalysis/AutofocusDynamicNumPoint',AutofocusDynamicNumPoint);
   f_option.AutofocusDynamicMovement.Value:=config.GetValue('/StarAnalysis/AutofocusDynamicMovement',AutofocusDynamicMovement);
   f_option.AutofocusPlanetNumPoint.Value:=config.GetValue('/StarAnalysis/AutofocusPlanetNumPoint',AutofocusPlanetNumPoint);
   f_option.AutofocusPlanetMovement.Value:=config.GetValue('/StarAnalysis/AutofocusPlanetMovement',AutofocusPlanetMovement);
   f_option.CanSetGain.Checked:=config.GetValue('/Sensor/CanSetGain',camera.CanSetGain);
   f_option.MaxAdu.Value:=config.GetValue('/Sensor/MaxADU',MAXWORD);
   f_option.MaxAduFromCamera.Checked:=config.GetValue('/Sensor/MaxADUFromCamera',true);
   f_option.NotDisplayCapture.Checked:=not config.GetValue('/Visu/DisplayCapture',DisplayCapture);
   f_option.LowQualityDisplay.Checked:=config.GetValue('/Visu/LowQualityDisplay',LowQualityDisplay);
   f_option.ExpEarlyStart.Checked:=config.GetValue('/Sensor/ExpEarlyStart',ConfigExpEarlyStart);
   f_option.MeasureNewImage.Checked:=config.GetValue('/Files/MeasureNewImage',false) and f_option.ExpEarlyStart.Checked;
   f_option.CheckRecenterTarget.Checked:=config.GetValue('/PrecSlew/CheckRecenterTarget',false) and f_option.ExpEarlyStart.Checked;
   f_option.PixelSize.Value:=config.GetValue('/Astrometry/PixelSize',0.0);
   f_option.Focale.Value:=config.GetValue('/Astrometry/FocaleLength',0.0);
   f_option.PixelSizeFromCamera.Checked:=config.GetValue('/Astrometry/PixelSizeFromCamera',true);
   f_option.Resolver:=config.GetValue('/Astrometry/Resolver',ResolverAstrometryNet);
   if f_option.MaxAduFromCamera.Checked and (camera.Status=devConnected) then
      f_option.MaxAdu.Value:=round(camera.MaxAdu);
   if f_option.PixelSizeFromCamera.Checked and (camera.Status=devConnected) and (camera.PixelSizeX>0) then
      f_option.PixelSize.Value:=camera.PixelSizeX;
   f_option.FocaleFromTelescope.Checked:=config.GetValue('/Astrometry/FocaleFromTelescope',true);
   if f_option.FocaleFromTelescope.Checked then
      f_option.Focale.Value:=mount.FocaleLength;
   f_option.Tolerance.Value:=config.GetValue('/Astrometry/ScaleTolerance',0.5);
   f_option.MaxRadius.Value:=config.GetValue('/Astrometry/MaxRadius',15.0);
   f_option.AstrometryTimeout.Value:=round(config.GetValue('/Astrometry/Timeout',60.0));
   f_option.Downsample.Value:=config.GetValue('/Astrometry/DownSample',4);
   f_option.SourcesLimit.Value:=config.GetValue('/Astrometry/SourcesLimit',150);
   f_option.OtherOptions.Text:=config.GetValue('/Astrometry/OtherOptions','--no-fits2fits');
   f_option.AstUseScript.Checked:=config.GetValue('/Astrometry/AstUseScript',false);
   f_option.AstCustScript.Text:=config.GetValue('/Astrometry/AstCustScript','');
   f_option.AstCustScript.Visible:=f_option.AstUseScript.Checked;
   f_option.CygwinPath.Text:=config.GetValue('/Astrometry/CygwinPath','C:\cygwin');
   f_option.AstrometryPath.Text:=config.GetValue('/Astrometry/AstrometryPath','');
   f_option.AstrometryFallback.Checked:=config.GetValue('/Astrometry/Fallback',false);
   f_option.ElbrusFolder.Text:=config.GetValue('/Astrometry/ElbrusFolder','C:\Elbrus\Images');
   {$ifdef unix}
   f_option.ElbrusUnixpath.Text:=config.GetValue('/Astrometry/ElbrusUnixpath',ExpandFileName('~/Elbrus/Images'));
   {$endif}
   f_option.PlatesolveFolder.Text:=config.GetValue('/Astrometry/PlatesolveFolder','C:\PlateSolve2.28');
   f_option.PlatesolveWait.Value:=config.GetValue('/Astrometry/PlatesolveWait',0);
   f_option.ASTAPFolder.Text:=config.GetValue('/Astrometry/ASTAPFolder',
      {$ifdef mswindows}
      'C:\Program Files\astap'
      {$else}
        {$ifdef darwin}
        '/Applications/astap.app/Contents/MacOS'
        {$else}
        '/opt/astap'
        {$endif}
      {$endif}
      );
   f_option.ASTAPSearchRadius.Value:=config.GetValue('/Astrometry/ASTAPSearchRadius',30);
   f_option.ASTAPdownsample.Value:=config.GetValue('/Astrometry/ASTAPdownsample',2);{0 is automatic selection but gives an runtime error in old ASTAP versions. Make 0 default after 9/2020}
   f_option.PrecSlewBox.ItemIndex:=config.GetValue('/PrecSlew/Method',0);
   f_option.SlewPrec.Value:=config.GetValue('/PrecSlew/Precision',5.0);
   f_option.SlewRetry.Value:=config.GetValue('/PrecSlew/Retry',3);
   f_option.SlewExp.Value:=config.GetValue('/PrecSlew/Exposure',10);
   if hasGainISO then
     f_option.SlewISObox.ItemIndex:=config.GetValue('/PrecSlew/Gain',f_preview.ISObox.ItemIndex)
   else
     f_option.SlewGainEdit.Value:=config.GetValue('/PrecSlew/Gain',f_preview.GainEdit.Value);
   f_option.SlewOffsetEdit.Value:=config.GetValue('/PrecSlew/Offset',f_preview.OffsetEdit.Value);
   f_option.SlewBin.Value:=config.GetValue('/PrecSlew/Binning',1);
   if (camera.Status=devConnected)and(camera.BinXrange<>NullRange) then
       f_option.SlewBin.MaxValue:=round(camera.BinXrange.max)
   else
       f_option.SlewBin.MaxValue:=9;
   f_option.SlewDelay.Value:=config.GetValue('/PrecSlew/Delay',5);
   f_option.SlewFilter.Items.Assign(FilterList);
   f_option.SlewFilter.ItemIndex:=config.GetValue('/PrecSlew/Filter',0);
   f_option.RecenterTargetDistance.value:=config.GetValue('/PrecSlew/RecenterTargetDistance',10.0);
   if (mount.Status=devConnected)and(mount.PierSide=pierUnknown) then begin
      f_option.Panel13.Visible:=true;
      f_option.MeridianWarning.caption:='Mount is not reporting pier side, meridian process is unreliable.'
   end
   else begin
      f_option.MeridianWarning.caption:='';
      f_option.Panel13.Visible:=false;
   end;
   f_option.MeridianOption.ItemIndex:=config.GetValue('/Meridian/MeridianOption',0);
   f_option.MinutesPastMeridian.Value:=config.GetValue('/Meridian/MinutesPast',MinutesPastMeridian);
   f_option.MinutesPastMeridianMin.Value:=config.GetValue('/Meridian/MinutesPastMin',MinutesPastMeridianMin);
   f_option.MeridianFlipPauseBefore.Checked:=config.GetValue('/Meridian/MeridianFlipPauseBefore',false);
   f_option.MeridianFlipPauseAfter.Checked:=config.GetValue('/Meridian/MeridianFlipPauseAfter',false);
   f_option.MeridianFlipPauseTimeout.Value:=config.GetValue('/Meridian/MeridianFlipPauseTimeout',0);
   f_option.MeridianFlipPanel.Visible:=(f_option.MeridianOption.ItemIndex=1);
   f_option.MeridianFlipCalibrate.Checked:=config.GetValue('/Meridian/MeridianFlipCalibrate',false);
   f_option.MeridianFlipStopSlaving.Checked:=config.GetValue('/Meridian/MeridianFlipStopSlaving',false);
   f_option.MeridianFlipAutofocus.Checked:=config.GetValue('/Meridian/MeridianFlipAutofocus',false);
   f_option.AutoguiderBox.ItemIndex:=config.GetValue('/Autoguider/Software',2);
   f_option.PHDhostname.Text:=config.GetValue('/Autoguider/PHDhostname','localhost');
   f_option.PHDport.Text:=config.GetValue('/Autoguider/PHDport','4400');
   f_option.LinGuiderUseUnixSocket:=config.GetValue('/Autoguider/LinGuiderUseUnixSocket',true);
   f_option.LinGuiderSocket.Text:=config.GetValue('/Autoguider/LinGuiderSocket','/tmp/lg_ss');
   f_option.LinGuiderHostname.Text:=config.GetValue('/Autoguider/LinGuiderHostname','localhost');
   f_option.LinGuiderPort.Text:=config.GetValue('/Autoguider/LinGuiderPort','5656');
   f_option.DitherPixel.Value:=config.GetValue('/Autoguider/Dither/Pixel',1.0);
   f_option.DitherRAonly.Checked:=config.GetValue('/Autoguider/Dither/RAonly',true);
   f_option.DitherWaitTime.Value:=config.GetValue('/Autoguider/Dither/WaitTime',5);
   f_option.EarlyDither.checked:=config.GetValue('/Autoguider/Dither/EarlyDither',EarlyDither);
   f_option.SettlePixel.Value:=config.GetValue('/Autoguider/Settle/Pixel',1.0);
   f_option.SettleMinTime.Value:=config.GetValue('/Autoguider/Settle/MinTime',5);
   f_option.SettleMaxTime.Value:=config.GetValue('/Autoguider/Settle/MaxTime',30);
   f_option.CalibrationDelay.Value:=config.GetValue('/Autoguider/Settle/CalibrationDelay',300);
   f_option.StarLostCancelExposure.Value:=config.GetValue('/Autoguider/Recovery/StarLostCancelExposure',0);
   f_option.StarLostRestart.Value:=config.GetValue('/Autoguider/Recovery/RestartTimeout',0);
   f_option.StarLostCancel.Value:=config.GetValue('/Autoguider/Recovery/CancelTimeout',1800);
   f_option.GuideDriftMax.Value:=config.GetValue('/Autoguider/Recovery/MaxGuideDrift',100.0);
   f_option.GuideDriftCancelExposure.Checked:=config.GetValue('/Autoguider/Recovery/CancelExposure',false);
   f_option.GuideDriftRestartDelay.Value:=config.GetValue('/Autoguider/Recovery/RestartDelay',15);
   f_option.PlanetariumBox.ItemIndex:=config.GetValue('/Planetarium/Software',ord(plaNONE));
   f_option.CdChostname.Text:=config.GetValue('/Planetarium/CdChostname','localhost');
   f_option.CdCport.Text:=config.GetValue('/Planetarium/CdCport','');
   f_option.CheckBoxLocalCdc.Checked:=f_option.CdCport.Text='';
   f_option.PanelRemoteCdc.Visible:=not f_option.CheckBoxLocalCdc.Checked;
   f_option.WeatherRestartDelay.Value:=config.GetValue('/Weather/RestartDelay',5);
   f_option.ScrollBoxWeather.Visible:=(weather.Status=devConnected)and(not weather.hasStatus);
   if f_option.ScrollBoxWeather.Visible then begin
      f_option.PanelW1.Visible:=weather.hasCloudCover;
      f_option.PanelW2.Visible:=weather.hasDewPoint;
      f_option.PanelW3.Visible:=weather.hasHumidity;
      f_option.PanelW4.Visible:=weather.hasPressure;
      f_option.PanelW5.Visible:=weather.hasRainRate;
      f_option.PanelW6.Visible:=weather.hasSkyBrightness;
      f_option.PanelW7.Visible:=weather.hasSkyQuality;
      f_option.PanelW8.Visible:=weather.hasSkyTemperature;
      f_option.PanelW9.Visible:=weather.hasStarFWHM;
      f_option.PanelW10.Visible:=weather.hasTemperature;
      f_option.PanelW11.Visible:=weather.hasWindDirection;
      f_option.PanelW12.Visible:=weather.hasWindGust;
      f_option.PanelW13.Visible:=weather.hasWindSpeed;
      f_option.UseW1.Checked:=config.GetValue('/Weather/Use/CloudCover',false);
      f_option.UseW2.Checked:=config.GetValue('/Weather/Use/DewPoint',false);
      f_option.UseW3.Checked:=config.GetValue('/Weather/Use/Humidity',false);
      f_option.UseW4.Checked:=config.GetValue('/Weather/Use/Pressure',false);
      f_option.UseW5.Checked:=config.GetValue('/Weather/Use/RainRate',false);
      f_option.UseW6.Checked:=config.GetValue('/Weather/Use/SkyBrightness',false);
      f_option.UseW7.Checked:=config.GetValue('/Weather/Use/SkyQuality',false);
      f_option.UseW8.Checked:=config.GetValue('/Weather/Use/SkyTemperature',false);
      f_option.UseW9.Checked:=config.GetValue('/Weather/Use/StarFWHM',false);
      f_option.UseW10.Checked:=config.GetValue('/Weather/Use/Temperature',false);
      f_option.UseW11.Checked:=config.GetValue('/Weather/Use/WindDirection',false);
      f_option.UseW12.Checked:=config.GetValue('/Weather/Use/WindGust',false);
      f_option.UseW13.Checked:=config.GetValue('/Weather/Use/WindSpeed',false);
      f_option.FloatSpinEditMi1.value:=config.GetValue('/Weather/Min/CloudCover',0);
      f_option.FloatSpinEditMi2.value:=config.GetValue('/Weather/Min/DewPoint',0);
      f_option.FloatSpinEditMi3.value:=config.GetValue('/Weather/Min/Humidity',0);
      f_option.FloatSpinEditMi4.value:=config.GetValue('/Weather/Min/Pressure',0);
      f_option.FloatSpinEditMi5.value:=config.GetValue('/Weather/Min/RainRate',0);
      f_option.FloatSpinEditMi6.value:=config.GetValue('/Weather/Min/SkyBrightness',0);
      f_option.FloatSpinEditMi7.value:=config.GetValue('/Weather/Min/SkyQuality',0);
      f_option.FloatSpinEditMi8.value:=config.GetValue('/Weather/Min/SkyTemperature',0);
      f_option.FloatSpinEditMi9.value:=config.GetValue('/Weather/Min/StarFWHM',0);
      f_option.FloatSpinEditMi10.value:=config.GetValue('/Weather/Min/Temperature',0);
      f_option.FloatSpinEditMi11.value:=config.GetValue('/Weather/Min/WindDirection',0);
      f_option.FloatSpinEditMi12.value:=config.GetValue('/Weather/Min/WindGust',0);
      f_option.FloatSpinEditMi13.value:=config.GetValue('/Weather/Min/WindSpeed',0);
      f_option.FloatSpinEditMa1.value:=config.GetValue('/Weather/Max/CloudCover',0);
      f_option.FloatSpinEditMa2.value:=config.GetValue('/Weather/Max/DewPoint',0);
      f_option.FloatSpinEditMa3.value:=config.GetValue('/Weather/Max/Humidity',0);
      f_option.FloatSpinEditMa4.value:=config.GetValue('/Weather/Max/Pressure',0);
      f_option.FloatSpinEditMa5.value:=config.GetValue('/Weather/Max/RainRate',0);
      f_option.FloatSpinEditMa6.value:=config.GetValue('/Weather/Max/SkyBrightness',0);
      f_option.FloatSpinEditMa7.value:=config.GetValue('/Weather/Max/SkyQuality',0);
      f_option.FloatSpinEditMa8.value:=config.GetValue('/Weather/Max/SkyTemperature',0);
      f_option.FloatSpinEditMa9.value:=config.GetValue('/Weather/Max/StarFWHM',0);
      f_option.FloatSpinEditMa10.value:=config.GetValue('/Weather/Max/Temperature',0);
      f_option.FloatSpinEditMa11.value:=config.GetValue('/Weather/Max/WindDirection',0);
      f_option.FloatSpinEditMa12.value:=config.GetValue('/Weather/Max/WindGust',0);
      f_option.FloatSpinEditMa13.value:=config.GetValue('/Weather/Max/WindSpeed',0);
   end;
   for i:=0 to SafetyActionNum-1 do begin
      f_option.SafetyActions.Cells[1,i+1]:=SafetyActionName[round(config.GetValue('/Safety/Actions/Action'+inttostr(i),0))];
      f_option.SafetyActions.Cells[2,i+1]:=config.GetValue('/Safety/Actions/Parameter'+inttostr(i),'');
   end;
   f_option.DomeNoSafetyCheck.Checked:=config.GetValue('/Dome/NoSafetyCheck',false);
   f_option.DomeSlaveToMount.Checked:=config.GetValue('/Dome/SlaveToMount',false);
   f_option.DomeActionWait.Value:=config.GetValue('/Dome/ActionWait',1);
   for i:=0 to DomeOpenActionNum-1 do begin
      f_option.DomeOpenActions.Cells[1,i+1]:=DomeOpenActionName[round(config.GetValue('/Dome/Open/Action'+inttostr(i),0))];
   end;
   for i:=0 to DomeCloseActionNum-1 do begin
      f_option.DomeCloseActions.Cells[1,i+1]:=DomeCloseActionName[round(config.GetValue('/Dome/Close/Action'+inttostr(i),0))];
   end;
   f_option.smtp_host.Text:=emailconfig.GetValue('/SMTP/Host','');
   f_option.smtp_port.Text:=emailconfig.GetValue('/SMTP/Port','');
   f_option.smtp_user.Text:=DecryptStr(hextostr(emailconfig.GetValue('/SMTP/User','')), encryptpwd);
   f_option.smtp_pass.Text:=DecryptStr(hextostr(emailconfig.GetValue('/SMTP/Passwd','')), encryptpwd);
   f_option.mail_from.Text:=emailconfig.GetValue('/Mail/From','');
   f_option.mail_to.Text:=emailconfig.GetValue('/Mail/To','');
   f_option.smtp_ssltls.checked:=emailconfig.GetValue('/SMTP/SSLTLS',true);
   f_option.EmailCondition.Checked[0]:=config.GetValue('/Mail/EndSequence',false);
   f_option.EmailCondition.Checked[1]:=config.GetValue('/Mail/AbortSequence',false);
   f_option.EmailCondition.Checked[2]:=config.GetValue('/Mail/Autoguider',false);
   f_option.EmailCondition.Checked[3]:=config.GetValue('/Mail/Aufofocus',false);
   f_option.EmailCondition.Checked[4]:=config.GetValue('/Mail/MeridianFlip',false);
   f_option.EmailCondition.Checked[5]:=config.GetValue('/Mail/TargetInitialisation',false);
   f_option.CheckGroupVoice.Checked[0]:=config.GetValue('/Voice/Dialog',false);
   f_option.CheckGroupVoice.Checked[1]:=config.GetValue('/Voice/Sequence',false);
   f_option.CheckGroupVoice.Checked[2]:=config.GetValue('/Voice/Error',false);
   f_option.CheckGroupVoice.Checked[3]:=config.GetValue('/Voice/Email',false);

   f_option.LockTemp:=false;
   pt.x:=PanelCenter.Left;
   pt.y:=PanelCenter.top;
   pt:=ClientToScreen(pt);
   FormPos(f_option,pt.X,pt.Y);
   f_option.ShowModal;

   if f_option.ModalResult=mrOK then begin
     if trim(f_option.Labelmsg.Caption)<>'' then NewMessage(f_option.Labelmsg.Caption,1);
     buf:=f_option.Languages.Text;
     i:=pos(',',buf);
     if i>0 then buf:=copy(buf,1,i-1);
     config.SetValue('/Language',buf);
     screenconfig.SetValue('/Hint/Show',f_option.CbShowHints.Checked);
     config.SetValue('/Files/CapturePath',f_option.CaptureDir.Text);
     config.SetValue('/Files/TmpDir',f_option.TempDir.Text);
     config.SetValue('/Files/TCPIPConfigPort',f_option.TCPIPport.Value);
     config.SetValue('/Script/PythonCmd',f_option.PythonCmd.Text);
     for i:=0 to SubDirCount-1 do begin
       for n:=0 to SubDirCount-1 do
         if SubDirName[n]=f_option.FolderOptions.Cells[2,i] then break;
       config.SetValue('/Files/SubDirOpt'+inttostr(i),n);
       config.SetValue('/Files/SubDirActive'+inttostr(i),f_option.FolderOptions.Cells[1,i]='1');
     end;
     for i:=0 to FileNameCount-1 do begin
       for n:=0 to FileNameCount-1 do
         if FileNameName[n]=f_option.FileOptions.Cells[2,i] then break;
       config.SetValue('/Files/FileNameOpt'+inttostr(i),n);
       config.SetValue('/Files/FileNameActive'+inttostr(i),f_option.FileOptions.Cells[1,i]='1');
     end;
     config.SetValue('/Files/FileNameSep',f_option.FilenameSep.Text);
     if f_option.UseFileSequenceWidth.Checked then
        config.SetValue('/Files/FileSequenceWidth',f_option.FileSequenceWidth.Text)
     else
        config.SetValue('/Files/FileSequenceWidth',0);
     config.SetValue('/Files/Pack',f_option.FilePack.checked);
     config.SetValue('/Files/Exif',f_option.WantExif.Checked);
     config.SetValue('/StarAnalysis/Focus',f_option.FocusWindow.Value);
     config.SetValue('/StarAnalysis/Undersampled',f_option.Undersampled.Checked);
     n:=FilterList.Count-1;
     config.SetValue('/Filters/Num',n);
     for i:=1 to n do begin
        config.SetValue('/Filters/Filter'+IntToStr(i),FilterList[i]);
        config.SetValue('/Filters/Offset'+IntToStr(i),StrToIntDef(trim(f_option.FilterList.Cells[1,i]),0));
        config.SetValue('/Filters/ExpFact'+IntToStr(i),StrToFloatDef(trim(stringReplace(f_option.FilterList.Cells[2,i],',','.',[])),1.0));
     end;
     config.SetValue('/StarAnalysis/AutoFocusMode',ord(f_option.GetAutofocusMode));
     config.SetValue('/StarAnalysis/AutofocusMinSpeed',f_option.AutofocusMinSpeed.Value);
     config.SetValue('/StarAnalysis/AutofocusMaxSpeed',f_option.AutofocusMaxSpeed.Value);
     config.SetValue('/StarAnalysis/AutofocusStartHFD',f_option.AutofocusStartHFD.Value);
     config.SetValue('/StarAnalysis/AutofocusNearHFD',f_option.AutofocusNearHFD.Value);
     config.SetValue('/StarAnalysis/AutofocusExposure',f_option.AutofocusExp);
     if hasGainISO then
       config.SetValue('/StarAnalysis/AutofocusGain',f_option.AutofocusISObox.ItemIndex)
     else
       config.SetValue('/StarAnalysis/AutofocusGain',f_option.AutofocusGainEdit.Value);
     config.SetValue('/StarAnalysis/AutofocusOffset',f_option.AutofocusOffsetEdit.Value);
     config.SetValue('/StarAnalysis/AutofocusBinning',f_option.AutofocusBinning.Value);
     config.SetValue('/StarAnalysis/FocuserBacklash',f_option.FocuserBacklash.Value);
     config.SetValue('/StarAnalysis/FocuserBacklashActive',f_option.FocuserBacklashActive.checked);
     config.SetValue('/StarAnalysis/FocuserBacklashDirection',(f_option.FocuserBacklashDirection.ItemIndex=0));
     config.SetValue('/StarAnalysis/FocuserDelay',f_option.FocuserDelay.Value);
     x:=f_option.FocuserTempCoeff.Value;
     if TemperatureScale=1 then x:=x*9/5;
     config.SetValue('/StarAnalysis/FocuserTempCoeff',x);
     x:=f_option.AutofocusTemp.Value;
     if TemperatureScale=1 then x:=x*9/5;
     config.SetValue('/StarAnalysis/AutofocusTemp',x);
     config.SetValue('/StarAnalysis/AutofocusPeriod',f_option.AutofocusPeriod.Value);
     config.SetValue('/StarAnalysis/AutofocusTolerance',f_option.AutofocusTolerance.Value);
     config.SetValue('/StarAnalysis/AutofocusMinSNR',f_option.AutofocusMinSNR.Value);
     config.SetValue('/StarAnalysis/AutofocusSlippageCorrection',f_option.AutofocusSlippageCorrection.Checked);
     config.SetValue('/StarAnalysis/AutofocusSlippageOffset',f_option.AutofocusSlippageOffset.Value);
     config.SetValue('/StarAnalysis/FocusStarMagAdjust',f_option.FocusStarMagAdjust.Checked);
     config.SetValue('/StarAnalysis/AutofocusStarMag',f_option.FocusStarMag.ItemIndex+4);
     config.SetValue('/StarAnalysis/AutofocusPrecisionSlew',f_option.AutofocusPrecisionSlew.Value);
     config.SetValue('/StarAnalysis/AutofocusMoveDir',f_option.AutofocusMoveDirIn.Checked);
     config.SetValue('/StarAnalysis/AutofocusNearNum',f_option.AutofocusNearNum.Value);
     config.SetValue('/StarAnalysis/AutofocusInPlace',f_option.AutofocusInPlace.Checked);
     if f_option.AutofocusInPlace.Checked then
       config.SetValue('/StarAnalysis/AutofocusPauseGuider',f_option.AutofocusPauseGuider.Checked)
     else
       config.SetValue('/StarAnalysis/AutofocusPauseGuider',true);
     config.SetValue('/StarAnalysis/AutofocusMultiStarCenter',f_option.AutofocusMultiStarCenter.Checked);
     config.SetValue('/StarAnalysis/AutofocusDynamicNumPoint',f_option.AutofocusDynamicNumPoint.Value);
     config.SetValue('/StarAnalysis/AutofocusDynamicMovement',f_option.AutofocusDynamicMovement.Value);
     config.SetValue('/StarAnalysis/AutofocusPlanetNumPoint',f_option.AutofocusPlanetNumPoint.Value);
     config.SetValue('/StarAnalysis/AutofocusPlanetMovement',f_option.AutofocusPlanetMovement.Value);
     config.SetValue('/Log/Messages',f_option.Logtofile.Checked);
     config.SetValue('/Log/debug_msg',f_option.debug_msg.Checked);
     config.SetValue('/Files/SaveBitmap',f_option.SaveBitmap.Checked);
     case f_option.SaveBitmapFormat.ItemIndex of
       0: buf:='png';
       1: buf:='tif';
       2: buf:='jpg';
       3: buf:='bmp';
       else buf:='png';
     end;
     config.SetValue('/Files/SaveBitmapFormat',buf);
     config.SetValue('/Info/ObservatoryName',f_option.ObservatoryName.Text);
     config.SetValue('/Info/ObservatoryLatitude',f_option.Latitude);
     config.SetValue('/Info/ObservatoryLongitude',f_option.Longitude);
     config.SetValue('/Info/ObservatoryElevation',f_option.ObsElev.Value);
     config.SetValue('/Info/ObserverName',f_option.ObserverName.Text);
     config.SetValue('/Info/TelescopeName',f_option.TelescopeName.Text);
     config.SetValue('/Info/HorizonFile',f_option.HorizonFile.FileName);
     config.SetValue('/Info/ElevationMin',f_option.ElevationMin.Value);
     f_option.SaveObservatoryDB;
     config.SetValue('/Color/Bayer',f_option.DebayerPreview.Checked);
     config.SetValue('/Color/BayerMode',f_option.BayerMode.ItemIndex);
     config.SetValue('/Color/RedBalance',f_option.RedBalance.Position/100);
     config.SetValue('/Color/GreenBalance',f_option.GreenBalance.Position/100);
     config.SetValue('/Color/BlueBalance',f_option.BlueBalance.Position/100);
     config.SetValue('/Color/BalanceFromCamera',f_option.BalanceFromCamera.Checked);
     config.SetValue('/Color/BGneutralization',f_option.BGneutralization.Checked);
     config.SetValue('/Color/ClippingOverflow',f_option.ClippingHigh.Value);
     config.SetValue('/Color/ClippingUnderflow',f_option.ClippingLow.Value);
     config.SetValue('/BadPixel/Sigma',f_option.BPMsigma.Value);
     config.SetValue('/PreviewStack/StackShow',f_option.StackShow.Checked);
     config.SetValue('/Video/PreviewRate',f_option.VideoPreviewRate.Value);
     config.SetValue('/RefImage/Treshold',f_option.RefTreshold.Position);
     config.SetValue('/RefImage/Color',f_option.RefColor.ItemIndex);
     config.SetValue('/Cooler/TemperatureSlope',f_option.TemperatureSlope.Value);
     config.SetValue('/Cooler/CameraAutoCool',f_option.CameraAutoCool.Checked);
     config.SetValue('/Cooler/CameraAutoCoolTemp',f_option.CameraAutoCoolTemp.Value);
     config.SetValue('/Cooler/TemperatureScale',f_option.TemperatureScale.ItemIndex);
     if ReadoutList.Count>0 then begin
       config.SetValue('/Readout/Capture',f_option.ReadOutCapture.ItemIndex);
       config.SetValue('/Readout/Preview',f_option.ReadOutPreview.ItemIndex);
       config.SetValue('/Readout/Focus',f_option.ReadOutFocus.ItemIndex);
       config.SetValue('/Readout/Astrometry',f_option.ReadOutAstrometry.ItemIndex);
     end;
     config.SetValue('/Flat/FlatType',f_option.FlatType.ItemIndex);
     config.SetValue('/Flat/FlatAutoExposure',f_option.FlatAutoExposure.Checked);
     config.SetValue('/Flat/FlatMinExp',f_option.FlatMinExp.Value);
     config.SetValue('/Flat/FlatMaxExp',f_option.FlatMaxExp.Value);
     config.SetValue('/Flat/FlatLevelMin',f_option.FlatLevelMin.Value);
     config.SetValue('/Flat/FlatLevelMax',f_option.FlatLevelMax.Value);
     config.SetValue('/Flat/DomeFlatTelescopeSlew',f_option.DomeFlatTelescopeSlew.Checked);
     config.SetValue('/Flat/DomeFlatTelescopeAz',f_option.DomeFlatTelescopeAz.Value);
     config.SetValue('/Flat/DomeFlatTelescopeAlt',f_option.DomeFlatTelescopeAlt.Value);
     config.SetValue('/Flat/DomeFlatSetLight',f_option.DomeFlatSetLight.Checked);
     config.SetValue('/Flat/DomeFlatSetLightON',f_option.DomeFlatSetLightON.Text);
     config.SetValue('/Flat/DomeFlatSetLightOFF',f_option.DomeFlatSetLightOFF.Text);
     config.SetValue('/Sensor/CanSetGain',f_option.CanSetGain.Checked);
     config.SetValue('/Sensor/MaxADUFromCamera',f_option.MaxAduFromCamera.Checked);
     config.SetValue('/Sensor/MaxADU',f_option.MaxAdu.Value);
     config.SetValue('/Sensor/ExpEarlyStart',f_option.ExpEarlyStart.Checked);
     config.SetValue('/Visu/DisplayCapture',not f_option.NotDisplayCapture.Checked);
     config.SetValue('/Visu/LowQualityDisplay',f_option.LowQualityDisplay.Checked);
     config.SetValue('/Files/MeasureNewImage',f_option.MeasureNewImage.Checked and f_option.ExpEarlyStart.Checked);
     config.SetValue('/PrecSlew/CheckRecenterTarget',f_option.CheckRecenterTarget.Checked and f_option.ExpEarlyStart.Checked);
     config.SetValue('/Astrometry/Resolver',f_option.Resolver);
     config.SetValue('/Astrometry/PixelSizeFromCamera',f_option.PixelSizeFromCamera.Checked);
     config.SetValue('/Astrometry/FocaleFromTelescope',f_option.FocaleFromTelescope.Checked);
     config.SetValue('/Astrometry/PixelSize',f_option.PixelSize.Value);
     config.SetValue('/Astrometry/FocaleLength',f_option.Focale.Value);
     config.SetValue('/Astrometry/ScaleTolerance',f_option.Tolerance.Value);
     config.SetValue('/Astrometry/MaxRadius',f_option.MaxRadius.Value);
     config.SetValue('/Astrometry/Timeout',f_option.AstrometryTimeout.Value);
     config.SetValue('/Astrometry/DownSample',f_option.Downsample.Value);
     config.SetValue('/Astrometry/SourcesLimit',f_option.SourcesLimit.Value);
     config.SetValue('/Astrometry/OtherOptions',f_option.OtherOptions.Text);
     config.SetValue('/Astrometry/AstUseScript',f_option.AstUseScript.Checked);
     config.SetValue('/Astrometry/AstCustScript',f_option.AstCustScript.Text);
     config.SetValue('/Astrometry/AstrometryPath',trim(f_option.AstrometryPath.Text));
     config.SetValue('/Astrometry/CygwinPath',f_option.CygwinPath.Text);
     config.SetValue('/Astrometry/Fallback',f_option.AstrometryFallback.Checked);
     config.SetValue('/Astrometry/ElbrusFolder',f_option.ElbrusFolder.Text);
     {$ifdef unix}
     config.SetValue('/Astrometry/ElbrusUnixpath',f_option.ElbrusUnixpath.Text);
     {$endif}
     config.SetValue('/Astrometry/PlatesolveFolder',f_option.PlatesolveFolder.Text);
     config.SetValue('/Astrometry/PlatesolveWait',f_option.PlatesolveWait.Value);
     config.SetValue('/Astrometry/ASTAPFolder',f_option.ASTAPFolder.Text);
     config.SetValue('/Astrometry/ASTAPSearchRadius',f_option.ASTAPSearchRadius.Value);
     config.SetValue('/Astrometry/ASTAPdownsample',f_option.ASTAPdownsample.Value);
     config.SetValue('/PrecSlew/Method',f_option.PrecSlewBox.ItemIndex);
     config.SetValue('/PrecSlew/Precision',f_option.SlewPrec.Value);
     config.SetValue('/PrecSlew/Retry',f_option.SlewRetry.Value);
     config.SetValue('/PrecSlew/Exposure',f_option.SlewExp.Value);
     if hasGainISO then
       config.SetValue('/PrecSlew/Gain',f_option.SlewISObox.ItemIndex)
     else
       config.SetValue('/PrecSlew/Gain',f_option.SlewGainEdit.Value);
     config.SetValue('/PrecSlew/Offset',f_option.SlewOffsetEdit.Value);
     config.SetValue('/PrecSlew/Binning',f_option.SlewBin.Value);
     config.SetValue('/PrecSlew/Delay',f_option.SlewDelay.Value);
     config.SetValue('/PrecSlew/Filter',f_option.SlewFilter.ItemIndex);
     config.SetValue('/PrecSlew/RecenterTargetDistance',f_option.RecenterTargetDistance.value);
     config.SetValue('/Meridian/MeridianOption',f_option.MeridianOption.ItemIndex);
     config.SetValue('/Meridian/MinutesPast',f_option.MinutesPastMeridian.Value);
     config.SetValue('/Meridian/MinutesPastMin',f_option.MinutesPastMeridianMin.Value);
     config.SetValue('/Meridian/MeridianFlipPauseBefore',f_option.MeridianFlipPauseBefore.Checked);
     config.SetValue('/Meridian/MeridianFlipPauseAfter',f_option.MeridianFlipPauseAfter.Checked);
     config.SetValue('/Meridian/MeridianFlipPauseTimeout',f_option.MeridianFlipPauseTimeout.Value);
     config.SetValue('/Meridian/MeridianFlipCalibrate',f_option.MeridianFlipCalibrate.Checked);
     config.SetValue('/Meridian/MeridianFlipAutofocus',f_option.MeridianFlipAutofocus.Checked);
     config.SetValue('/Meridian/MeridianFlipStopSlaving',f_option.MeridianFlipStopSlaving.Checked);
     config.SetValue('/Autoguider/Software',f_option.AutoguiderBox.ItemIndex);
     config.SetValue('/Autoguider/PHDhostname',f_option.PHDhostname.Text);
     config.SetValue('/Autoguider/PHDport',f_option.PHDport.Text);
     config.SetValue('/Autoguider/LinGuiderUseUnixSocket',f_option.LinGuiderUseUnixSocket);
     config.SetValue('/Autoguider/LinGuiderSocket',f_option.LinGuiderSocket.Text);
     config.SetValue('/Autoguider/LinGuiderHostname',f_option.LinGuiderHostname.Text);
     config.SetValue('/Autoguider/LinGuiderPort',f_option.LinGuiderPort.Text);
     config.SetValue('/Autoguider/Dither/Pixel',f_option.DitherPixel.Value);
     config.SetValue('/Autoguider/Dither/RAonly',f_option.DitherRAonly.Checked);
     config.SetValue('/Autoguider/Dither/WaitTime',f_option.DitherWaitTime.Value);
     config.SetValue('/Autoguider/Dither/EarlyDither',f_option.EarlyDither.checked);
     config.SetValue('/Autoguider/Settle/Pixel',f_option.SettlePixel.Value);
     config.SetValue('/Autoguider/Settle/MinTime',f_option.SettleMinTime.Value);
     config.SetValue('/Autoguider/Settle/MaxTime',f_option.SettleMaxTime.Value);
     config.SetValue('/Autoguider/Settle/CalibrationDelay',f_option.CalibrationDelay.Value);
     config.SetValue('/Autoguider/Recovery/StarLostCancelExposure',f_option.StarLostCancelExposure.Value);
     config.SetValue('/Autoguider/Recovery/RestartTimeout',f_option.StarLostRestart.Value);
     config.SetValue('/Autoguider/Recovery/CancelTimeout',f_option.StarLostCancel.Value);
     config.SetValue('/Autoguider/Recovery/MaxGuideDrift',f_option.GuideDriftMax.Value);
     config.SetValue('/Autoguider/Recovery/CancelExposure',f_option.GuideDriftCancelExposure.Checked);
     config.SetValue('/Autoguider/Recovery/RestartDelay',f_option.GuideDriftRestartDelay.Value);
     config.SetValue('/Planetarium/Software',f_option.PlanetariumBox.ItemIndex);
     config.SetValue('/Planetarium/CdChostname',f_option.CdChostname.Text);
     config.SetValue('/Planetarium/CdCport',trim(f_option.CdCport.Text));
     config.SetValue('/Weather/RestartDelay',f_option.WeatherRestartDelay.Value);
     if f_option.ScrollBoxWeather.Visible then begin
        config.SetValue('/Weather/Use/CloudCover',f_option.UseW1.Checked);
        config.SetValue('/Weather/Use/DewPoint',f_option.UseW2.Checked);
        config.SetValue('/Weather/Use/Humidity',f_option.UseW3.Checked);
        config.SetValue('/Weather/Use/Pressure',f_option.UseW4.Checked);
        config.SetValue('/Weather/Use/RainRate',f_option.UseW5.Checked);
        config.SetValue('/Weather/Use/SkyBrightness',f_option.UseW6.Checked);
        config.SetValue('/Weather/Use/SkyQuality',f_option.UseW7.Checked);
        config.SetValue('/Weather/Use/SkyTemperature',f_option.UseW8.Checked);
        config.SetValue('/Weather/Use/StarFWHM',f_option.UseW9.Checked);
        config.SetValue('/Weather/Use/Temperature',f_option.UseW10.Checked);
        config.SetValue('/Weather/Use/WindDirection',f_option.UseW11.Checked);
        config.SetValue('/Weather/Use/WindGust',f_option.UseW12.Checked);
        config.SetValue('/Weather/Use/WindSpeed',f_option.UseW13.Checked);
        config.SetValue('/Weather/Min/CloudCover',f_option.FloatSpinEditMi1.value);
        config.SetValue('/Weather/Min/DewPoint',f_option.FloatSpinEditMi2.value);
        config.SetValue('/Weather/Min/Humidity',f_option.FloatSpinEditMi3.value);
        config.SetValue('/Weather/Min/Pressure',f_option.FloatSpinEditMi4.value);
        config.SetValue('/Weather/Min/RainRate',f_option.FloatSpinEditMi5.value);
        config.SetValue('/Weather/Min/SkyBrightness',f_option.FloatSpinEditMi6.value);
        config.SetValue('/Weather/Min/SkyQuality',f_option.FloatSpinEditMi7.value);
        config.SetValue('/Weather/Min/SkyTemperature',f_option.FloatSpinEditMi8.value);
        config.SetValue('/Weather/Min/StarFWHM',f_option.FloatSpinEditMi9.value);
        config.SetValue('/Weather/Min/Temperature',f_option.FloatSpinEditMi10.value);
        config.SetValue('/Weather/Min/WindDirection',f_option.FloatSpinEditMi11.value);
        config.SetValue('/Weather/Min/WindGust',f_option.FloatSpinEditMi12.value);
        config.SetValue('/Weather/Min/WindSpeed',f_option.FloatSpinEditMi13.value);
        config.SetValue('/Weather/Max/CloudCover',f_option.FloatSpinEditMa1.value);
        config.SetValue('/Weather/Max/DewPoint',f_option.FloatSpinEditMa2.value);
        config.SetValue('/Weather/Max/Humidity',f_option.FloatSpinEditMa3.value);
        config.SetValue('/Weather/Max/Pressure',f_option.FloatSpinEditMa4.value);
        config.SetValue('/Weather/Max/RainRate',f_option.FloatSpinEditMa5.value);
        config.SetValue('/Weather/Max/SkyBrightness',f_option.FloatSpinEditMa6.value);
        config.SetValue('/Weather/Max/SkyQuality',f_option.FloatSpinEditMa7.value);
        config.SetValue('/Weather/Max/SkyTemperature',f_option.FloatSpinEditMa8.value);
        config.SetValue('/Weather/Max/StarFWHM',f_option.FloatSpinEditMa9.value);
        config.SetValue('/Weather/Max/Temperature',f_option.FloatSpinEditMa10.value);
        config.SetValue('/Weather/Max/WindDirection',f_option.FloatSpinEditMa11.value);
        config.SetValue('/Weather/Max/WindGust',f_option.FloatSpinEditMa12.value);
        config.SetValue('/Weather/Max/WindSpeed',f_option.FloatSpinEditMa13.value);
     end;
     for i:=0 to SafetyActionNum-1 do begin
        k:=-1;
        for n:=0 to ord(high(TSafetyAction)) do begin
           if SafetyActionName[n]=trim(f_option.SafetyActions.Cells[1,i+1]) then begin
             k:=n;
             break;
           end;
        end;
        if k<0 then k:=0;
        config.SetValue('/Safety/Actions/Action'+inttostr(i),k);
        config.SetValue('/Safety/Actions/Parameter'+inttostr(i),trim(f_option.SafetyActions.Cells[2,i+1]));
     end;
     config.SetValue('/Dome/NoSafetyCheck',f_option.DomeNoSafetyCheck.Checked);
     config.SetValue('/Dome/SlaveToMount',f_option.DomeSlaveToMount.Checked);
     config.SetValue('/Dome/ActionWait',f_option.DomeActionWait.Value);
     for i:=0 to DomeOpenActionNum-1 do begin
        k:=-1;
        for n:=0 to ord(high(TDomeOpenAction)) do begin
           if DomeOpenActionName[n]=trim(f_option.DomeOpenActions.Cells[1,i+1]) then begin
             k:=n;
             break;
           end;
        end;
        if k<0 then k:=0;
        config.SetValue('/Dome/Open/Action'+inttostr(i),k);
     end;
     for i:=0 to DomeCloseActionNum-1 do begin
        k:=-1;
        for n:=0 to ord(high(TDomeCloseAction)) do begin
           if DomeCloseActionName[n]=trim(f_option.DomeCloseActions.Cells[1,i+1]) then begin
             k:=n;
             break;
           end;
        end;
        if k<0 then k:=0;
        config.SetValue('/Dome/Close/Action'+inttostr(i),k);
     end;
     emailconfig.SetValue('/SMTP/Host',f_option.smtp_host.Text);
     emailconfig.SetValue('/SMTP/Port',f_option.smtp_port.Text);
     emailconfig.SetValue('/SMTP/User',strtohex(encryptStr(f_option.smtp_user.Text, encryptpwd)));
     emailconfig.SetValue('/SMTP/Passwd',strtohex(encryptStr(f_option.smtp_pass.Text, encryptpwd)));
     emailconfig.SetValue('/Mail/From',f_option.mail_from.Text);
     emailconfig.SetValue('/Mail/To',f_option.mail_to.Text);
     emailconfig.SetValue('/SMTP/SSLTLS',f_option.smtp_ssltls.checked);
     config.SetValue('/Mail/EndSequence',f_option.EmailCondition.Checked[0]);
     config.SetValue('/Mail/AbortSequence',f_option.EmailCondition.Checked[1]);
     config.SetValue('/Mail/Autoguider',f_option.EmailCondition.Checked[2]);
     config.SetValue('/Mail/Aufofocus',f_option.EmailCondition.Checked[3]);
     config.SetValue('/Mail/MeridianFlip',f_option.EmailCondition.Checked[4]);
     config.SetValue('/Mail/TargetInitialisation',f_option.EmailCondition.Checked[5]);
     config.SetValue('/Voice/Dialog',f_option.CheckGroupVoice.Checked[0]);
     config.SetValue('/Voice/Sequence',f_option.CheckGroupVoice.Checked[1]);
     config.SetValue('/Voice/Error',f_option.CheckGroupVoice.Checked[2]);
     config.SetValue('/Voice/Email',f_option.CheckGroupVoice.Checked[3]);

     SaveConfig;

     SetOptions;

     if (lang<>config.GetValue('/Language',lang)) then begin
       lang:=config.GetValue('/Language',lang);
       lang:=u_translation.translate(lang);
       SetLang;
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

procedure Tf_main.OptionGetMaxADU(Sender: TObject);
begin
  if camera.Status=devConnected then
     f_option.MaxAdu.Value:=round(camera.MaxADU);
end;

procedure Tf_main.OptionGetPixelSize(Sender: TObject);
begin
   if (camera.Status=devConnected) and (camera.PixelSizeX>0) then
      f_option.PixelSize.Value:=camera.PixelSizeX;
end;

procedure Tf_main.OptionGetFocaleLength(Sender: TObject);
begin
   if (mount.Status=devConnected) and (mount.FocaleLength>0) then
      f_option.Focale.Value:=mount.FocaleLength;
end;

procedure Tf_main.MenuViewConnectionClick(Sender: TObject);
begin
  f_devicesconnection.Visible:=MenuViewConnection.Checked;
end;

procedure Tf_main.MenuViewDomeClick(Sender: TObject);
begin
  f_dome.Visible:=MenuViewDome.Checked;
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
  if MenuViewClock.Checked then begin
     StatusBar1.Panels[panelclock].Width:=DoScaleY(65);
     StatusBar1Resize(nil);
     TimerStampTimer.Enabled:=true;
     TimerStampTimerTimer(nil);
  end
  else begin
     StatusBar1.Panels[panelclock].Width:=0;
     StatusBar1Resize(nil);
     TimerStampTimer.Enabled:=false;
  end;
end;

procedure Tf_main.MenuViewFocuserClick(Sender: TObject);
begin
  f_focuser.Visible:=MenuViewFocuser.Checked;
end;

procedure Tf_main.MenuViewMagnifyerClick(Sender: TObject);
begin
  f_magnifyer.Visible:=MenuViewMagnifyer.Checked;
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

procedure Tf_main.MenuViewSafetyClick(Sender: TObject);
begin
  f_safety.Visible:=MenuViewSafety.Checked;
end;

procedure Tf_main.MenuViewCoverClick(Sender: TObject);
begin
  f_cover.Visible:=MenuViewCover.Checked;
end;

procedure Tf_main.MenuViewSwitchClick(Sender: TObject);
begin
  f_switch.Visible:=MenuViewSwitch.Checked;
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

procedure Tf_main.MenuViewWeatherClick(Sender: TObject);
begin
  f_weather.Visible:=MenuViewWeather.Checked;
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
toolmenu:=nil;
if sender is TPanel then begin
    if TPanel(Sender).Tag>0 then npm:=TMenuItem(TPanel(Sender).tag);
    if source is TLabel then begin
     if TFrame(TLabel(Source).Parent).tag>0 then toolmenu:=TMenuItem(TFrame(TLabel(Source).Parent).tag);
     TFrame(TLabel(Source).Parent).Parent:=TPanel(Sender);
     TFrame(TLabel(Source).Parent).Top:=Y;
     TFrame(TLabel(Source).Parent).Left:=X;
     if TPanel(Sender).Width>TPanel(Sender).Height then begin
        TFrame(TLabel(Source).Parent).Align:=alLeft;
     end else begin
        TFrame(TLabel(Source).Parent).Align:=alTop;
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

procedure Tf_main.PanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 if Source is TLabel then Accept:=TLabel(Source).Parent is TFrame
 else if Source is TMemo then Accept:=TPanel(TMemo(Source).Parent).Parent is TFrame
 else if Source is TDragObject then  Accept:=TDragObject(Source).Control is TFrame
 else if source is TFrame then Accept:=true
 else Accept:=false;
 if (Sender is TPanel)and(TPanel(Sender).ControlCount=0) then begin
   if State=dsDragEnter then
      TPanel(Sender).Color:=clBtnHighlight;
   if State=dsDragLeave then
      TPanel(Sender).Color:=clDefault;
 end;
end;

procedure Tf_main.ButtonDragDrop(Sender, Source: TObject; X, Y: Integer);
var pnl: TPanel;
begin
 pnl:=nil;
 if sender is TToolButton then begin
   if TToolButton(Sender)=TBConnect then
      pnl:=PanelRight1
   else
   if TToolButton(Sender)=TBFocus then
      pnl:=PanelRight2
   else
   if TToolButton(Sender)=TBCapture then
      pnl:=PanelRight3
   else
   if TToolButton(Sender)=TBSequence then
      pnl:=PanelRight4
   else
   if TToolButton(Sender)=TBVideo then
      pnl:=PanelRight5;
   if pnl<>nil then
      PanelDragDrop(pnl,Source,X,Y);
 end;
end;

procedure Tf_main.ButtonDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 if Source is TLabel then Accept:=TLabel(Source).Parent is TFrame
 else if Source is TMemo then Accept:=TPanel(TMemo(Source).Parent).Parent is TFrame
 else if Source is TDragObject then  Accept:=TDragObject(Source).Control is TFrame
 else if source is TFrame then Accept:=true
 else Accept:=false;
end;

Procedure Tf_main.StopExposure(Sender: TObject);
begin
  camera.AbortExposure;
  Preview:=false;
  Capture:=false;
  StatusBar1.Panels[panelstatus].Text:=rsStop;
end;

procedure Tf_main.ResetPreviewStack(Sender: TObject);
begin
   fits.ClearImage;
end;

Procedure Tf_main.StartPreviewExposureAsync(Data: PtrInt);
begin
  StartPreviewExposure(nil);
end;

Procedure Tf_main.StartPreviewExposure(Sender: TObject);
var e: double;
    buf,f: string;
    p,binx,biny,i,x,y,w,h,sx,sy,sw,sh: integer;
begin
// ! can run out of main thread
if (camera.Status=devConnected) and ((not f_capture.Running) or autofocusing) and (not learningvcurve)and(not f_video.Running) then begin
  Preview:=true;
  ExpectedStop:=false;
  // be sure mount is tracking, but not repeat after every frame
  if (Sender<>nil)and(not mount.Tracking) then
    mount.Track;
  // check exposure time
  e:=f_preview.Exposure;
  if e<0 then begin
    NewMessage(Format(rsInvalidExpos, [f_preview.ExpTime.Text]),1);
    f_preview.stop;
    Preview:=false;
    exit;
  end;
  // check focuser temperature compensation
  if (camera.FrameType=LIGHT) and not (autofocusing or learningvcurve or f_starprofile.ChkAutofocus.Down) then begin
    FocuserTemperatureCompensation(true);
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
           NewMessage(Format(rsInvalidBinni, [f_preview.Binning.Text]),1);
           f_preview.stop;
           Preview:=false;
           exit;
         end;
     if (camera.BinX<>binx)or(camera.BinY<>biny) then begin
        NewMessage(rsSetBinning+blank+inttostr(binx)+'x'+inttostr(biny),2);
        camera.SetBinning(binx,biny);
     end;
  end;
  sx:=StrToIntDef(f_frame.FX.Text,-1);
  sy:=StrToIntDef(f_frame.FY.Text,-1);
  sw:=StrToIntDef(f_frame.FWidth.Text,-1);
  sh:=StrToIntDef(f_frame.FHeight.Text,-1);
  if (sx>=0)and(sy>=0)and(sw>0)and(sh>0) then begin
    camera.GetFrame(x,y,w,h,true);
    if (x<>sx)or(y<>sy)or(w<>sw)or(h<>sh) then
      camera.SetFrame(sx,sy,sw,sh);
  end;
  if camera.hasFnumber then begin
    f:=f_preview.Fnumber.Text;
    if (camera.Fnumber<>f) then begin
      if (f>'') then begin
       NewMessage(rsSet+blank+rsFStop+'='+f,2);
       camera.Fnumber:=f;
      end
      else NewMessage(rsInvalid+blank+rsFStop+blank+f_preview.Fnumber.Text, 0);
    end;
  end;
  if camera.CanSetGain then begin
    if camera.Gain<>f_preview.Gain then begin
      camera.Gain:=f_preview.Gain;
    end;
    if camera.hasOffset then begin
       if camera.Offset<>f_preview.Offset then camera.Offset:=f_preview.Offset;
    end;
  end;
  if camera.hasReadOut then begin
     camera.readoutmode:=ReadoutModePreview;
  end;
  if camera.FrameType<>LIGHT then camera.FrameType:=LIGHT;
  camera.ObjectName:=rsPreview;
  fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  camera.StackNum:=-1; //unlimited
  camera.AddFrames:=f_preview.StackPreview.Checked;
  camera.StartExposure(e);
end
else begin
   f_preview.stop;
   Preview:=false;
   StatusBar1.Panels[panelstatus].Text:='';
   if not AllDevicesConnected then NewMessage(rsSomeDefinedD,1);
end;
end;

procedure Tf_main.StartCaptureExposureAsync(Data: PtrInt);
begin
  StartCaptureExposure(nil);
end;

Procedure Tf_main.RecenterTarget;
var tra,tde,err: double;
    savecapture,restartguider : boolean;
begin
  savecapture:=Capture;
  RecenteringTarget:=true;
  try
    f_preview.StackPreview.Checked:=false;
    Capture:=false;
    if (astrometryResolver<>ResolverNone)and(Mount.Status=devConnected)and(f_sequence.Running)and
       (f_sequence.TargetCoord)and(f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord)
    then begin
      // stop autoguider
      restartguider:=(autoguider.State=GUIDER_GUIDING);
      if restartguider then begin
        NewMessage(rsStopAutoguid,2);
        autoguider.Guide(false);
        autoguider.WaitBusy(15);
      end;
      try
      // center target
      tra:=rad2deg*f_sequence.TargetRA/15;
      tde:=rad2deg*f_sequence.TargetDE;
      LocalToMount(mount.EquinoxJD,tra,tde);
      astrometry.PrecisionSlew(tra,tde,err);
      except
      end;
      // restart guider
      if restartguider then begin
        NewMessage(rsRestartAutog,2);
        autoguider.Guide(false);
        autoguider.WaitBusy(5);
        autoguider.Guide(true);
        autoguider.WaitGuiding(SettleMaxTime);
      end;
    end;
  finally
    Capture:=savecapture;
    RecenteringTarget:=false;
  end;
end;


function Tf_main.PrepareCaptureExposure(canwait:boolean):boolean;
var e,x: double;
    buf,txt,r: string;
    waittime,i: integer;
    ftype:TFrameType;
begin
// If called with canwait=false this function only check for operation
// that can prevent an immediate start of the exposure. In this case it
// return false.
// Call with canwait=true to really do all the operation need before
// the exposure is started: auto-focus, dithering, meridian flip, wait for weather.
result:=false;
if not f_capture.Running then begin
  NewMessage(rsCaptureStopp2, 0);
  exit;
end;
if (AllDevicesConnected)and(not autofocusing)and(not learningvcurve)and(not f_video.Running) then begin
  if (f_capture.FrameType.ItemIndex>=0)and(f_capture.FrameType.ItemIndex<=ord(High(TFrameType))) then
    ftype:=TFrameType(f_capture.FrameType.ItemIndex)
  else
    ftype:=LIGHT;
  // wait if paused
  if WeatherPauseCapture then begin
    if canwait then begin
    if f_sequence.Running then begin
     if (ftype=LIGHT) then begin
       WeatherCapturePaused:=true;
       f_sequence.StatusMsg.Caption:=rsSequencePaus;
       NewMessage(f_sequence.StatusMsg.Caption);
       // stop guiding and mount tracking now
       if (autoguider<>nil)and(autoguider.Running) then begin
          NewMessage(rsStopAutoguid,2);
          autoguider.Guide(false);
       end;
       mount.AbortMotion;
       while WeatherPauseCapture and f_capture.Running do begin
          Wait(5);
       end;
       // tracking and guiding is restarted by the sequence before we go here
       WeatherCapturePaused:=false;
       // continue if not aborted
       if WeatherPauseCanceled then exit;
       // check if autofocus before start, we must redo it now to also recenter the target
       try
       if f_sequence.Running and (f_sequence.CurrentPlan<>nil) then begin
          f_capture.FocusNow:=f_sequence.CurrentPlan.Steps[f_sequence.CurrentPlan.CurrentStep].autofocusstart;
          NeedRecenterTarget:=(astrometryResolver<>ResolverNone)and(Mount.Status=devConnected)and(f_sequence.TargetCoord)
                               and(f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord);
       end;
       except
       end;
       NewMessage(rsContinueSequ);
     end
     else
       NewMessage(Format(rsIgnoreWeathe, [FrameName[ord(ftype)]]));
    end
    else begin
      // capture running without a sequence, just show a message
      NewMessage(rsWeatherCondi, 1);
    end;
    end
    else begin
      exit; // cannot start now
    end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  if f_sequence.Running and f_sequence.EditingTarget then begin
    if canwait then begin
       wait(1);
       Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
       exit; // will be restarted after editing is done
    end
    else begin
      exit; // cannot start now
    end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  if PauseSequence then begin
    if canwait then begin
       f_pause.Caption:=rsPauseSequenc2;
       f_pause.Text:=rsTheSequenceI+crlf+rsClickContinu+crlf+rsClickCancelT;
       if not f_pause.Wait then begin
         f_sequence.BtnStop.Click;
         wait(1);
       end;
       PauseSequence:=false;
    end
    else begin
      exit; // cannot start now
    end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  // check if we need to cancel running preview
  if f_preview.Running then begin
   if canwait then begin
     NewMessage(rsStopPreview,1);
     StatusBar1.Panels[panelstatus].Text:=rsStopPreview;
     if f_preview.Loop then
       f_preview.BtnLoopClick(nil)
     else
       f_preview.BtnPreviewClick(nil);
     wait(2);
    end
    else begin
      exit; // cannot start now
    end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  // check for meridian and do flip now if required
  e:=StrToFloatDef(f_capture.ExpTime.Text,0);
  if canwait then begin
    while true do begin
      CheckMeridianFlip(e,true,waittime);
      if not f_capture.Running then begin
        // stop current capture if meridian flip failed
        NewMessage(rsMeridianFlip+', '+rsCannotStartC,1);
        f_capture.Stop;
        Capture:=false;
        if EmailMeridianFlip then begin
          r:=email(rsMeridianFlip,rsMeridianFlip+', '+rsCannotStartC);
          if r='' then r:=rsEmailSentSuc;
          NewMessage(r,9);
        end;
        exit;
      end;
      // check if we need to wait for flip before to continue (time to meridian < exposure time)
      if waittime>0 then begin
        f_capture.DitherNum:=0; // no dither after flip
        // wait meridian
        NewMessage(rsWaitMeridian2,1);
        f_pause.Caption:=rsWaitMeridian2;
        f_pause.Text:=rsWaitMeridian2;
        if not f_pause.Wait(waittime,true) then begin
          NewMessage(rsMeridianFlip3+', '+rsCannotStartC,1);
          exit;
        end;
      end
      else
        break;  //  meridian flip done
    end;
  end
  else begin
    if CheckMeridianFlip(e,false,waittime) then
       exit;  // cannot start now
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  // check focuser temperature compensation
  if (camera.FrameType=LIGHT) then begin
    if canwait then begin
       FocuserTemperatureCompensation(true);
    end
    else begin
       if FocuserTemperatureCompensation(false) then
          exit; // cannot start now
    end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  // check if dithering is required
  if f_capture.CheckBoxDither.Checked and (f_capture.DitherNum>=f_capture.DitherCount.Value) then begin
   if canwait then begin
    CaptureDither;
   end
   else begin
    exit; // cannot start now
   end;
  end;
  // check if dithering running, can also be started from CameraProgress;
  if autoguider.Dithering then begin
   if canwait then begin
     autoguider.WaitDithering(SettleMaxTime);
     wait(1);
   end
   else begin
    exit; // cannot start now
   end;
  end;
  // check if refocusing is required
  if (ftype=LIGHT) and ( // only for light frame
     f_capture.FocusNow  // start of step
     or (f_capture.CheckBoxFocus.Checked and (f_capture.FocusNum>=f_capture.FocusCount.Value)) // every n frame
     or ((AutofocusPeriod>0) and (AutoFocusLastTime<>NullCoord) and                            // every n minutes
        ((minperday*(now-AutoFocusLastTime))>=AutofocusPeriod))
     or (focuser.hasTemperature and (AutofocusTempChange<>0.0) and                             // temperature change
        (AutofocusLastTemp<>NullCoord) and (f_starprofile.AutofocusDone) and
        (abs(AutofocusLastTemp-FocuserTemp)>=AutofocusTempChange))
        )
     then begin
    if canwait then begin
     f_capture.FocusNum:=0;
     f_capture.FocusNow:=false;
     // do autofocus
     if AutoAutofocus then begin
       if f_capture.Running then begin
         // ok, continue
         f_capture.DitherNum:=0; // no dither after focus
       end else begin
         NewMessage(rsCaptureStopp,1);
         f_capture.Stop;
         Capture:=false;
         exit;
       end;
     end else begin
       // failed, cancel current capture
       NewMessage(rsAutofocusFai+', '+rsStopCapture,1);
       f_capture.Stop;
       Capture:=false;
       exit;
     end;
   end
   else begin
    exit; // cannot start now
   end;
  end
  else
   if (ftype=LIGHT) and (f_capture.CheckBoxFocus.Checked or (AutofocusPeriod>0)or(AutofocusTempChange<>0.0)) then begin
      // Show message when next autofocus is due
      txt:='';
      if f_capture.CheckBoxFocus.Checked then begin
        i:=f_capture.FocusCount.Value-f_capture.FocusNum;
        buf:=blank+inttostr(i)+blank+LowerCase(rsImages);
        if txt='' then txt:=buf else txt:=txt+', '+rsOr+blank+buf;
      end;
      if (AutofocusPeriod>0)and(AutoFocusLastTime<>NullCoord) then begin
        i:=round(AutofocusPeriod-(minperday*(now-AutoFocusLastTime)));
        buf:=blank+inttostr(i)+blank+rsMinutes;
        if txt='' then txt:=buf else txt:=txt+', '+rsOr+blank+buf;
      end;
      if focuser.hasTemperature and (AutofocusTempChange<>0.0)and(AutofocusLastTemp<>NullCoord)and(f_starprofile.AutofocusDone) then begin
        x:=AutofocusTempChange-(abs(AutofocusLastTemp-FocuserTemp));
        buf:=blank+FormatFloat(f1,x)+blank+'C';
        if txt='' then txt:=buf else txt:=txt+', '+rsOr+blank+buf;
      end;
      if txt>'' then NewMessage(rsAutofocusDue+blank+txt,3);
   end;
   if not f_capture.Running then begin
     NewMessage(rsCaptureStopp2, 0);
     exit;
   end;
  // check if target need recenter
  if NeedRecenterTarget then begin
     if canwait then begin
       NewMessage(rsRecenterTarg);
       RecenterTarget;
       NeedRecenterTarget:=false;
     end
     else begin
       exit; // cannot start now
     end;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  if not f_capture.Running then begin
    NewMessage(rsCaptureStopp2, 0);
    exit;
  end;
  // All OK
  result:=true;
end;
end;

procedure Tf_main.CaptureDither;
begin
  // reset frame counter
  f_capture.DitherNum:=0;
  if autoguider.State=GUIDER_GUIDING then begin
    // start dithering
    NewMessage(rsDithering+ellipsis,1);
    StatusBar1.Panels[panelstatus].Text:=rsDithering+ellipsis;
    autoguider.Dither(DitherPixel, DitherRAonly, DitherWaitTime);
  end else begin
    NewMessage(rsNotAutoguidi,1);
  end;
end;

Procedure Tf_main.StartCaptureExposure(Sender: TObject);
begin
  if PrepareCaptureExposure(true) then // do all requirement and check it's OK
     StartCaptureExposureNow
  else begin
     if f_sequence.Running and (f_sequence.EditingTarget or f_sequence.Restarting) then exit;
     // not ready to start now
     NewMessage(rsCannotStartC+', '+rsAbort,9);
     f_capture.Stop;
     Capture:=false;
     StatusBar1.Panels[panelstatus].Text := '';
     if not AllDevicesConnected then NewMessage(rsSomeDefinedD,1);
  end;
end;

Procedure Tf_main.StartCaptureExposureNow;
var e: double;
    buf,f: string;
    p,binx,biny,i,x,y,w,h,sx,sy,sw,sh,cc,cs: integer;
    ftype:TFrameType;
begin
if (AllDevicesConnected)and(not autofocusing)and (not learningvcurve) then begin
  if (f_capture.FrameType.ItemIndex>=0)and(f_capture.FrameType.ItemIndex<=ord(High(TFrameType))) then
    ftype:=TFrameType(f_capture.FrameType.ItemIndex)
  else
    ftype:=LIGHT;
  f_preview.StackPreview.Checked:=false;
  f_capture.Running:=true;
  LockRestartExposure:=false;
  MenuCaptureStart.Caption:=rsStop;
  Preview:=false;
  Capture:=true;
  ExpectedStop:=false;
  // check exposure time
  e:=StrToFloatDef(f_capture.ExpTime.Text,-1);
  if e<0 then begin
    NewMessage(Format(rsInvalidExpos, [f_capture.ExpTime.Text]),1);
    f_capture.Stop;
    Capture:=false;
    exit;
  end;
  CameraExposureRemain:=e;
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
          NewMessage(Format(rsInvalidBinni, [f_capture.Binning.Text]),1);
          f_capture.Stop;
          Capture:=false;
          exit;
        end;
     if (camera.BinX<>binx)or(camera.BinY<>biny) then begin
        NewMessage(rsSetBinning+blank+inttostr(binx)+'x'+inttostr(biny),2);
        camera.SetBinning(binx,biny);
     end;
  end;
  // check and set frame
  sx:=StrToIntDef(f_frame.FX.Text,-1);
  sy:=StrToIntDef(f_frame.FY.Text,-1);
  sw:=StrToIntDef(f_frame.FWidth.Text,-1);
  sh:=StrToIntDef(f_frame.FHeight.Text,-1);
  if (sx>=0)and(sy>=0)and(sw>0)and(sh>0) then begin
    camera.GetFrame(x,y,w,h,true);
    if (x<>sx)or(y<>sy)or(w<>sw)or(h<>sh) then
      camera.SetFrame(sx,sy,sw,sh);
  end;
  // check and set f-stop
  if camera.hasFnumber then begin
    f:=f_capture.Fnumber.Text;
    if (camera.Fnumber<>f) then begin
      if (f>'') then begin
       NewMessage('Set F-Stop '+f,2);
       camera.Fnumber:=f;
      end
      else NewMessage('Invalid F-Stop '+f_capture.Fnumber.Text,0);
    end;
  end;
  // check and set gain
  if camera.CanSetGain then begin
    if camera.Gain<>f_capture.Gain then begin
      camera.Gain:=f_capture.Gain;
    end;
    if camera.hasOffset then begin
       if camera.Offset<>f_capture.Offset then camera.Offset:=f_capture.Offset;
    end;
  end;
  // check and set frame
  if camera.FrameType<>ftype then camera.FrameType:=ftype;
  if ftype<>LIGHT then begin
     f_capture.CheckBoxDither.Checked:=false;
     f_capture.CheckBoxFocus.Checked:=false;
  end;
  // set readout mode
  if camera.hasReadOut then begin
     camera.readoutmode:=ReadoutModeCapture;
  end;
  // set object for filename
  camera.ObjectName:=f_capture.Fname.Text;
  // disable BPM
  fits.SetBPM(bpm,0,0,0,0);
  // stacking
  f_preview.StackPreview.Checked:=false;
  camera.AddFrames:=f_capture.PanelStack.Visible and (f_capture.StackNum.Value>1);
  if camera.AddFrames then
    camera.StackNum:=f_capture.StackNum.Value
  else
    camera.StackNum:=1;
  fits.DarkOn:=camera.AddFrames;
  // show message
  cc:=f_capture.SeqCount;
  if (camera.AddFrames)and(EarlyNextExposure and (not SkipEarlyExposure))and(cc>1) then cc:=cc-1;
  if  camera.AddFrames then begin
    cs:=camera.StackCount+1;
    if cs>camera.StackNum then cs:=1;
    NewMessage(Format(rsStartingExpo, [f_capture.FrameType.Text, inttostr(cc)+'/'+f_capture.SeqNum.Text, f_capture.ExpTime.Text])+' '+Format('Stack %d / %d',[cs,camera.StackNum]),1)
  end
  else
    NewMessage(Format(rsStartingExpo, [f_capture.FrameType.Text, inttostr(cc)+'/'+f_capture.SeqNum.Text, f_capture.ExpTime.Text]),1);
  // increment dither
  if (not camera.AddFrames)or(camera.StackCount>=camera.StackNum) then f_capture.DitherNum:=f_capture.DitherNum+1;
  // start exposure for time e
  camera.StartExposure(e);
end
else begin
   // camera not connected
   NewMessage(rsCannotStartC+', autofocus='+BoolToStr(Autofocusing,True),9);
   f_capture.Stop;
   Capture:=false;
   StatusBar1.Panels[panelstatus].Text := '';
   if not AllDevicesConnected then NewMessage(rsSomeDefinedD,1);
end;
end;

procedure Tf_main.CameraProgress(n:double);
var txt: string;
    i: integer;
begin
 CameraExposureRemain:=n;
 if (n<=0) then begin
   if meridianflipping or autofocusing then exit;
   if ((f_capture.Running)or(f_preview.Running)) then begin
     i:=round(n);
     case i of
       -11 : txt:=rsDisplay+ellipsis;
       -10 : txt:=rsReadImage+ellipsis;
       -9 : txt:=rsUnknownStatu+ellipsis;
       -5 : txt:=rsError2+ellipsis;
       -4 : txt:=rsDownloading+ellipsis;
       -3 : txt:=rsReadCCD+ellipsis;
       -1 : txt:=rsWaitStart+ellipsis;
        0 : txt:=rsIdle+ellipsis;
       else txt:=rsUnknownStatu+ellipsis;
     end;
     if Capture then begin
       StatusBar1.Panels[panelstatus].Text := rsCapture+blank+inttostr(f_capture.SeqCount)+'/'+f_capture.SeqNum.Text+' '+txt;
       if (i=-4) and EarlyDither and f_capture.CheckBoxDither.Checked and (f_capture.DitherNum>=f_capture.DitherCount.Value) then begin
         StatusBar1.Panels[panelstatus].Text:=rsDithering+ellipsis;
         CaptureDither;
       end;
     end
     else begin
        StatusBar1.Panels[panelstatus].Text := txt;
     end;
   end
   else begin
      StatusBar1.Panels[panelstatus].Text := '';
   end;
 end else begin
  if n>=10 then txt:=FormatFloat(f0, n)
           else txt:=FormatFloat(f1, n);
  if Capture then begin
    if f_capture.Running then
      StatusBar1.Panels[panelstatus].Text := rsCapture+blank+inttostr(f_capture.SeqCount)+'/'+f_capture.SeqNum.Text+blank+rsExp+blank+txt+blank+rsSec;
  end
  else begin
     StatusBar1.Panels[panelstatus].Text := rsExp+blank+txt+blank+rsSec;
  end;
 end;
end;

procedure Tf_main.CameraNewImage(Sender: TObject);
begin
  if Capture then begin
    // save file first
    if not ((FlatAutoExposure and (camera.FrameType=FLAT))or(SaveBitmap)) then begin
      {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'save fits file');{$endif}
      if (not camera.AddFrames)or(camera.StackCount>=camera.StackNum) then CameraSaveNewImage;
      {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'saved');{$endif}
      ImageSaved:=true;
    end
    else
      ImageSaved:=false;
  end;
  Application.QueueAsyncCall(@CameraNewImageAsync,0);
end;

procedure Tf_main.ShowLastImage(Sender: TObject);
begin
 if f_visu.BtnShowImage.Down then begin
  fits.LoadStream;
  DrawHistogram(true,true);
  DrawImage;
  Image1.Invalidate;
 end
 else begin
  img_Width:=0;
  img_Height:=0;
  ImaBmp.SetSize(0,0);
  ClearImage;
  Image1.Invalidate;
 end;
end;

procedure Tf_main.CameraNewImageAsync(Data: PtrInt);
var buf: string;
    displayimage: boolean;
begin
 try
  StatusBar1.Panels[panelstatus].Text:='';
  ImgFrameX:=FrameX;
  ImgFrameY:=FrameY;
  ImgFrameW:=FrameW;
  ImgFrameH:=FrameH;
  displayimage:=DisplayCapture or f_visu.BtnShowImage.Down or (not capture) or (Autofocusing) or (FlatAutoExposure and (camera.FrameType=FLAT));
  if displayimage and (not fits.ImageValid) then begin
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'fits loadstream');{$endif}
     fits.LoadStream;
  end;
  if displayimage then begin
  try
    // draw image
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawHistogram');{$endif}
    DrawHistogram(true,false);
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawImage');{$endif}
    DrawImage;
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawImage end');{$endif}
    if (GetCurrentThreadId=MainThreadID) then CheckSynchronize;
    except
      on E: Exception do NewMessage('CameraNewImage, DrawImage :'+ E.Message,1);
    end;
  end
  else begin
    img_Width:=0;
    img_Height:=0;
    ImaBmp.SetSize(0,0);
    ClearImage;
    Image1.Invalidate;
  end;
  try
  // process autofocus frame
  DoAutoFocus;
  except
    on E: Exception do NewMessage('CameraNewImage, Autofocus :'+ E.Message,1);
  end;
  // process capture
  if Capture then begin
     if not ImageSaved then begin
     // process automatic flat
       if FlatAutoExposure and (camera.FrameType=FLAT) then begin
         {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'flat auto exposure');{$endif}
         case FlatType of
           ftSKY : begin
                   if not CameraNewSkyFlat then exit;
                   end;
           ftDome :begin
                   if not CameraNewDomeFlat then exit;
                   end;
         end;
         {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'save flat image');{$endif}
         CameraSaveNewImage;
       end
       else
         CameraSaveNewImage;
     end;
     // image measurement
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'image measurement');{$endif}
     if displayimage then CameraMeasureNewImage;
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'image measurement end');{$endif}
     if (not EarlyNextExposure) or SkipEarlyExposure then begin
       // Next exposure delayed after image display
       // start the exposure now
       {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'start exposure');{$endif}
       if (not camera.AddFrames)or(camera.StackCount>=camera.StackNum) then begin
         f_capture.SeqCount:=f_capture.SeqCount+1;
         f_capture.FocusNum:=f_capture.FocusNum+1;
       end;
       if f_capture.SeqCount<=f_capture.SeqNum.Value then begin
          // next exposure
          if f_capture.Running then Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
       end else begin
          // end capture
          Capture:=false;
          f_capture.Stop;
          NewMessage(rsStopCapture+', '+Format(rsCaptureSFini, [inttostr(f_capture.SeqCount-1)+'/'+f_capture.SeqNum.Text]), 2);
          StatusBar1.Panels[panelstatus].Text := Format(rsCaptureSFini, [inttostr(f_capture.SeqCount-1)+'/'+f_capture.SeqNum.Text]);
          MenuCaptureStart.Caption:=f_capture.BtnStart.Caption
       end;
     end
     else begin
       if (camera.AddFrames)and(camera.StackCount<camera.StackNum) then begin
         f_capture.SeqCount:=f_capture.SeqCount-1;
         f_capture.FocusNum:=f_capture.FocusNum-1;
       end;
     end;
  end
  // process preview
  else if Preview then begin
    buf:=rsPreview+blank+FormatDateTime('hh:nn:ss', now);
    if camera.ImageFormat<>'.fits' then buf:=buf+' '+UpperCase(camera.ImageFormat);
    buf:=buf+'  '+inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
    if camera.StackCount>1 then buf:=buf+','+blank+Format(rsStackOfFrame, [inttostr(camera.StackCount)]);
    StatusBar1.Panels[panelfile].Text:=buf;
    if (not EarlyNextExposure) or Autofocusing then begin
      // Next exposure delayed after image display
      // start the exposure now
      if f_preview.Loop and f_preview.Running and (not CancelAutofocus) then begin
         {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'start exposure');{$endif}
         Application.QueueAsyncCall(@StartPreviewExposureAsync,0)
      end
      else begin
         // end preview
         f_preview.stop;
         Preview:=false;
         NewMessage(rsEndPreview,2);
         StatusBar1.Panels[panelstatus].Text:='';
      end;
    end;
  end;

 finally
 CameraProcessingImage:=false;
 end;
end;

procedure Tf_main.CameraNewExposure(Sender: TObject);
begin
  // This function is called early when a new image is received
  // to start the next exposure as soon as possible.
  if Capture then begin
    // prepare for next exposure
    f_capture.SeqCount:=f_capture.SeqCount+1;
    if f_capture.SeqCount<=f_capture.SeqNum.Value then begin
       // next exposure
       if f_capture.Running then begin
         f_capture.FocusNum:=f_capture.FocusNum+1;
         // only check if an operation is need before the next exposure
         if PrepareCaptureExposure(false) then begin
           // nothing to wait, start now
           SkipEarlyExposure:=false;
           StartCaptureExposureNow;
         end
         else begin
           // Some operation is need.
           // process later in CameraNewImage
           f_capture.SeqCount:=f_capture.SeqCount-1;
           f_capture.FocusNum:=f_capture.FocusNum-1;
           SkipEarlyExposure:=true;
         end;
       end;
    end else begin
       // process end capture later in CameraNewImage
       f_capture.SeqCount:=f_capture.SeqCount-1;
       EarlyNextExposure:=false;
    end;
  end
  else if Preview then begin
    // next exposure
    if f_preview.Loop and f_preview.Running and (not CancelAutofocus) then begin
       Application.QueueAsyncCall(@StartPreviewExposureAsync,0)
    end
    else begin
      // process end preview later in CameraNewImage
      EarlyNextExposure:=false;
    end;
  end;
end;

function Tf_main.CameraNewDomeFlat: boolean;
var exp,newexp: double;
begin
  result:=false;
  NewMessage(Format(rsFlatLevel, [inttostr(round(fits.imageMean))]),2);
  if AdjustDomeFlat then begin
    // adjust exposure time only once per series
    exp:=StrToFloatDef(f_capture.ExpTime.Text,FlatMinExp);
    if ((fits.imageMean<FlatLevelMin)or(fits.imageMean>FlatLevelMax))
       and ((exp>FlatMinExp)or(exp<FlatMaxExp))
    then begin
      newexp:=exp*((FlatLevelMin+FlatLevelMax)/2)/fits.imageMean;
      if newexp<FlatMinExp then begin
         // min configured value
         newexp:=FlatMinExp;
         AdjustDomeFlat:=false;
         NewMessage(rsReachConfigu,1);
         NewMessage(rsStopFlatCapt,1);
         exit;
      end;
      if newexp>FlatMaxExp then begin
        // max configured value
         newexp:=FlatMaxExp;
         AdjustDomeFlat:=false;
         NewMessage(rsReachConfigu2,1);
         NewMessage(rsStopFlatCapt,1);
         exit;
      end;
      f_capture.ExposureTime:=newexp;
      if newexp<>exp then NewMessage(Format(rsAdjustFlatEx, [f_capture.ExpTime.Text]),2);
      if f_capture.Running then Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
      // retry with new exposure
      exit;
    end
    else
      AdjustDomeFlat:=false;
  end;
  // save this flat
  result:=true;
end;

function Tf_main.CameraNewSkyFlat: boolean;
var exp,newexp: double;
begin
 result:=false;
 NewMessage(Format(rsFlatLevel, [inttostr(round(fits.imageMean))]),2);
 // new exposure time from image level
 exp:=StrToFloatDef(f_capture.ExpTime.Text,FlatMinExp);
 newexp:=exp*((FlatLevelMin+FlatLevelMax)/2)/fits.imageMean;
 // check if current image level is in range
 if (fits.imageMean<FlatLevelMin)or(fits.imageMean>FlatLevelMax) then begin
   // will need a too short exposure
   if newexp<FlatMinExp then begin
      newexp:=FlatMinExp;
      // wait for dusk
      if FlatWaitDusk then begin
         NewMessage(rsSkyIsStillTo,2);
         StatusBar1.Panels[panelstatus].Text:=rsWaitingForDu;
         wait(30);
      end
      // or abort
      else begin
         Capture:=false;
         f_capture.Stop;
         StatusBar1.Panels[panelstatus].Text:=rsStop;
         NewMessage(rsReachConfigu,1);
         NewMessage(rsStopFlatCapt,1);
         exit;
      end;
   end;
   // will need a too long exposure
   if newexp>FlatMaxExp then begin
      newexp:=FlatMaxExp;
      // wait for dawn
      if FlatWaitDawn then begin
        NewMessage(rsSkyIsStillTo2,2);
        StatusBar1.Panels[panelstatus].Text:=rsWaitingForDa;
        wait(30);
      end
      // or abort
      else begin
        Capture:=false;
        f_capture.Stop;
        StatusBar1.Panels[panelstatus].Text:=rsStop;
        NewMessage(rsReachConfigu2,1);
        NewMessage(rsStopFlatCapt,1);
        exit;
      end;
   end;
   // while waiting, maintain telescope position for sky flat
   if FlatWaitDusk or FlatWaitDawn then mount.SlewToSkyFlatPosition;
   // retry with a new exposure
   f_capture.ExposureTime:=newexp;
   if newexp<>exp then NewMessage(Format(rsAdjustFlatEx, [f_capture.ExpTime.Text]),2);
   if f_capture.Running then Application.QueueAsyncCall(@StartCaptureExposureAsync,0);
   exit;
 end;
 // maintain telescope position for sky flat
 if FlatWaitDusk or FlatWaitDawn then mount.SlewToSkyFlatPosition;
 // set newexposure time
 f_capture.ExposureTime:=newexp;
 // here we have a valid flat to save
 result:=true;
end;

procedure Tf_main.CameraSaveNewImage;
var dt,dn: Tdatetime;
    fn,fd,buf,fileseqstr,fileseqext,blankrep: string;
    framestr,objectstr,binstr,expstr,filterstr: string;
    ccdtemp: double;
    fileseqnum,i,n: integer;
    UseFileSequenceNumber: boolean;
begin
try
 dt:=NowUTC;
 dn:=now-0.5;
 {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Camera save new image');{$endif}
 // construct path
 fd:=slash(config.GetValue('/Files/CapturePath',defCapturePath));
 if copy(fd,1,1)='.' then fd:=ExpandFileName(slash(Appdir)+fd);
 if not fits.Header.Valueof('FRAME',framestr) then framestr:=f_capture.FrameType.Text;
 framestr:=trim(framestr);
 if not fits.Header.Valueof('OBJECT',objectstr) then objectstr:=f_capture.Fname.Text;
 objectstr:=StringReplace(objectstr,' ','',[rfReplaceAll]);
 objectstr:=StringReplace(objectstr,'/','_',[rfReplaceAll]);
 objectstr:=StringReplace(objectstr,'\','_',[rfReplaceAll]);
 objectstr:=StringReplace(objectstr,':','_',[rfReplaceAll]);
 if not fits.Header.Valueof('EXPTIME',expstr) then expstr:=f_capture.ExpTime.Text;
 expstr:=trim(expstr);
 if fits.Header.Valueof('XBINNING',binstr) then begin
   if not fits.Header.Valueof('YBINNING',buf) then buf:=binstr;
   binstr:=trim(binstr)+'x'+trim(buf);
 end
 else binstr:=trim(f_capture.Binning.Text);
 if not fits.Header.Valueof('FILTER',filterstr) then filterstr:='';
 filterstr:=trim(filterstr);
 for i:=0 to SubDirCount-1 do begin
   case SubDirOpt[i] of
     sdSeq : if SubDirActive[i] and f_sequence.Running then fd:=slash(fd+trim(CurrentSeqName));
     sdFrt : if SubDirActive[i] then fd:=slash(fd+framestr);
     sdObj : if SubDirActive[i] then fd:=slash(fd+objectstr);
     sdStep: if SubDirActive[i] and f_sequence.Running then begin
                if f_sequence.StepTotalCount>1 then begin
                  fd:=slash(fd+trim(CurrentStepName)+'_'+IntToStr(f_sequence.StepRepeatCount))
                end
                else begin
                  fd:=slash(fd+trim(CurrentStepName));
                end;
             end;
     sdExp : if SubDirActive[i] then begin
               if FlatAutoExposure and (framestr=trim(FrameName[3])) then
                  fd:=slash(fd+'auto')
               else
                  fd:=slash(fd+StringReplace(expstr,'.','_',[])+'s');
             end;
     sdBin : if SubDirActive[i] then fd:=slash(fd+binstr);
     sdDate: if SubDirActive[i] then fd:=fd+slash(FormatDateTime('yyyymmdd',dt));
     sdNight: if SubDirActive[i] then fd:=fd+slash(FormatDateTime('yyyymmdd',dn));
   end;
 end;
 ForceDirectoriesUTF8(fd);
 // construct file name
 fn:='';
 UseFileSequenceNumber:=false;
 for i:=0 to FileNameCount-1 do begin
   case FileNameOpt[i] of
     fnObj : if FileNameActive[i] then begin
             if framestr=trim(FrameName[0]) then begin
                 fn:=fn+wordspace(objectstr)+FilenameSep;
             end
             else
                fn:=fn+framestr+FilenameSep;
             end;
     fnFilter: if FileNameActive[i] and (filterstr<>'')and(framestr<>trim(FrameName[1]))and(framestr<>trim(FrameName[2])) then
                fn:=fn+filterstr+FilenameSep;

     fnExp : if FileNameActive[i] then begin
               if FlatAutoExposure and (framestr=trim(FrameName[3])) then
                  fn:=fn+'auto'+FilenameSep
               else begin
                  fn:=fn+StringReplace(expstr,'.',FilenameSep,[])+'s'+FilenameSep;
               end;
             end;
     fnBin : if FileNameActive[i] then begin
               fn:=fn+binstr+FilenameSep;
             end;
     fnTemp: if FileNameActive[i] and fits.Header.Valueof('CCD-TEMP',ccdtemp) then
                fn:=fn+formatfloat(f1,ccdtemp)+'C'+FilenameSep;
     fnDate: if FileNameActive[i] then begin
                if f_capture.ExposureTime>=1.0 then
                   fn:=fn+FormatDateTime('yyyymmdd'+FilenameSep+'hhnnss',dt)+FilenameSep
                else
                   fn:=fn+FormatDateTime('yyyymmdd'+FilenameSep+'hhnnsszzz',dt)+FilenameSep;
             end
             else
                UseFileSequenceNumber:=true;
     fnGain: if FileNameActive[i] and fits.Header.Valueof('GAIN',buf) then begin
                fn:=fn+trim(buf)+FilenameSep;
             end;
     fnFocuspos: if FileNameActive[i] and fits.Header.Valueof('FOCUSPOS',buf) then begin
                  fn:=fn+trim(buf)+FilenameSep;
             end;
   end;
 end;
 if FilenameSep='_' then
    blankrep:='-'
 else
    blankrep:='_';
 fn:=StringReplace(fn,' ',blankrep,[rfReplaceAll]);
 fn:=StringReplace(fn,'/',blankrep,[rfReplaceAll]);
 fn:=StringReplace(fn,'\',blankrep,[rfReplaceAll]);
 fn:=StringReplace(fn,':',blankrep,[rfReplaceAll]);
 if fn<>'' then
    delete(fn,length(fn),1); // remove last _
 // sequence number must always be at the end
 if UseFileSequenceNumber then begin
   fileseqnum:=1;
   fileseqstr:=IntToStr(fileseqnum);
   if FilePack then
     fileseqext:='.fits.fz'
   else
     fileseqext:='.fits';
   if FileSequenceWidth>0 then fileseqstr:=PadZeros(IntToStr(fileseqnum),FileSequenceWidth);
   while FileExistsUTF8(slash(fd)+fn+FilenameSep+fileseqstr+fileseqext) do begin
     inc(fileseqnum);
     fileseqstr:=IntToStr(fileseqnum);
     if FileSequenceWidth>0 then fileseqstr:=PadZeros(IntToStr(fileseqnum),FileSequenceWidth);
   end;
   fn:=fn+FilenameSep+fileseqstr;
 end;
 fn:=slash(fd)+fn+'.fits';
 // save the file
 fits.SaveToFile(fn,FilePack);
 inc(CurrentDoneCount);
 if FilePack then begin
   NewMessage(Format(rsSavedFile, [fn+'.fz']),1);
   buf:=Format(rsSaved, [fn+'.fz']);
 end
 else begin
   NewMessage(Format(rsSavedFile, [fn]),1);
   buf:=Format(rsSaved, [fn]);
 end;
 if camera.ImageFormat<>'.fits' then buf:=UpperCase(camera.ImageFormat)+' '+buf;
 if fits.HeaderInfo.valid then buf:=buf+' '+inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
 StatusBar1.Panels[panelfile].Text:=buf;
 StatusBar1.Panels[panelstatus].Text := '';
 // save as bitmap
 if SaveBitmap then begin
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Save bitmap');{$endif}
   fn:=ChangeFileExt(fn,'.'+SaveBitmapFormat);
   fits.SaveToBitmap(fn);
   NewMessage(Format(rsSavedFile, [fn]),1);
 end;
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Image saved');{$endif}
 except
   on E: Exception do begin
     NewMessage('Error saving image to disk: '+ E.Message,1);
     f_capture.Stop;
     Capture:=false;
     if f_sequence.Running then f_sequence.AbortSequence;
   end;
 end;
end;

procedure Tf_main.CameraMeasureNewImage;
var cra,cde,eq,pa,dist,sharp: double;
begin
try
 // measure image but not plot
 if MeasureNewImage and (camera.FrameType=LIGHT) then begin
   if AutofocusMode=afPlanet then begin
     sharp:=image_sharpness(fits.image);
     NewMessage(Format(rsImageSharpne, [formatfloat(f2, sharp)]));
   end
   else begin
     if EarlyNextExposure then begin
       if camera.LastExposureTime>=30 then begin
          MeasureImage(false);
       end
       else NewMessage(format(rsExposureTime4, [30]), 3);
     end
     else NewMessage(rsNoMeasuremen, 3);
   end;
 end;
 // check if target need to be recentered
 if CheckRecenterTarget and(camera.FrameType=LIGHT)and
    (not NeedRecenterTarget)and(not CheckRecenterBusy)and(not astrometry.Busy)
    then begin
      if (astrometryResolver<>ResolverNone) then begin
        if (f_sequence.Running)and(f_sequence.TargetCoord)and(f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord) then begin
          if (camera.LastExposureTime>(AstrometryTimeout+5)) then begin
            try
            CheckRecenterBusy:=true;
            astrometry.SolveCurrentImage(true);
            if (not astrometry.Busy)and astrometry.LastResult then begin
               if astrometry.CurrentCoord(cra,cde,eq,pa) then begin
                 cra:=cra*15*deg2rad;
                 cde:=cde*deg2rad;
                 J2000ToApparent(cra,cde);
                 dist:=60*rad2deg*rmod(AngularDistance(f_sequence.TargetRA,f_sequence.TargetDE,cra,cde)+pi2,pi2);
                 NewMessage(Format(rsDistanceToTa, [FormatFloat(f2, dist)]),3);
                 NeedRecenterTarget:=dist>max(RecenterTargetDistance,1.5*SlewPrecision);
                 if NeedRecenterTarget then NewMessage(rsTargetWillBe2,2);
               end;
            end;
            finally
            CheckRecenterBusy:=false;
            end;
          end
          else NewMessage(rsNoCenteringM, 3);
        end
        else NewMessage(rsNoCenteringM2, 3);
      end
      else NewMessage(rsNoCenteringM3, 3);
 end;
 except
   on E: Exception do NewMessage('CameraMeasureNewImage :'+ E.Message,1);
 end;
end;

procedure Tf_main.CameraVideoFrame(Sender: TObject);
begin
ImgFrameX:=FrameX;
ImgFrameY:=FrameY;
ImgFrameW:=FrameW;
ImgFrameH:=FrameH;
DrawHistogram(true,false);
DrawImage(false);
end;

Procedure Tf_main.ShowHistogramPos(msg:string);
begin
  StatusBar1.Panels[panelcursor].Text:=msg;
end;

Procedure Tf_main.Redraw(Sender: TObject);
begin
  DrawHistogram(false,false);
  DrawImage;
end;

Procedure Tf_main.ZoomImage(Sender: TObject);
begin
  ImgZoom:=f_visu.Zoom;
  PlotImage;
end;

Procedure Tf_main.DrawImage(WaitCursor:boolean=false);
var tmpbmp:TBGRABitmap;
    co: TBGRAPixel;
    s,cx,cy: integer;
begin
if fits.HeaderInfo.naxis>0 then begin
  try
  if WaitCursor then screen.Cursor:=crHourGlass;
  trpOK:=false;
  fits.Gamma:=f_visu.Gamma.Value;
  fits.VisuMax:=round(f_visu.ImgMax);
  fits.VisuMin:=round(f_visu.ImgMin);
  fits.MaxADU:=MaxADU;
  fits.Overflow:= 0.9995*ClippingOverflow;
  fits.Underflow:=ClippingUnderflow;
  fits.MarkOverflow:=f_visu.Clipping;
  fits.Invert:=f_visu.Invert;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'FITS GetBGRABitmap');{$endif}
  fits.GetBGRABitmap(ImaBmp);
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'FITS GetBGRABitmap end');{$endif}
  ImgPixRatio:=fits.HeaderInfo.pixratio;
  if (fits.HeaderInfo.pixratio<>1) then begin
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Fix pixelratio');{$endif}
    tmpbmp:=TBGRABitmap.Create(ImaBmp);
    ImaBmp.SetSize(round(fits.HeaderInfo.pixratio*ImaBmp.Width),ImaBmp.Height);
    ImaBmp.Canvas.StretchDraw(rect(0,0,ImaBmp.Width,ImaBmp.Height),tmpbmp.Bitmap);
    tmpbmp.Free;
  end;
  if refmask then begin
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Put ref image mask');{$endif}
    ImaBmp.StretchPutImage(rect(0,0,ImaBmp.Width,ImaBmp.Height),refbmp,dmLinearBlend);
  end;
  img_Width:=ImaBmp.Width;
  img_Height:=ImaBmp.Height;
  if f_visu.BullsEye then begin
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'BullsEye');{$endif}
    co:=ColorToBGRA(clRed);
    cx:=img_Width div 2;
    cy:=img_Height div 2;
    imabmp.DrawHorizLine(0,cy,img_Width,co);
    imabmp.DrawVertLine(cx,0,img_Height,co);
    s:=min(img_Height,img_Width) div 3;
    imabmp.EllipseAntialias(cx,cy,s,s,co,1);
    s:=min(img_Height,img_Width) div 8;
    imabmp.EllipseAntialias(cx,cy,s,s,co,1);

  end;
  if fits.HeaderInfo.solved and (cdcWCSinfo.secpix<>0) then plot_north(imabmp);
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PlotImage');{$endif}
  PlotImage;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PlotImage end');{$endif}
  finally
  screen.Cursor:=crDefault;
  end;
end;
end;

Procedure Tf_main.ClearImage;
begin
ScrBmp.FillRect(0,0,ScrBmp.Width,ScrBmp.Height,clDarkBlue);
EndX:=-1;
end;

Procedure Tf_main.PlotImage;
var r1,r2: double;
    w,h,px,py: integer;
    tmpbmp,str: TBGRABitmap;
    rmode: TResampleMode;
begin
if (img_Height=0)or(img_Width=0) then exit;
r1:=ScrBmp.Width/imabmp.Width;
r2:=ScrBmp.Height/imabmp.Height;
ZoomMin:=minvalue([1.0,r1,r2]);
if (ZoomMin<1)and((ImgZoom<ZoomMin)or(abs(ImgZoom-ZoomMin)<0.01)) then ImgZoom:=0;
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'ClearImage');{$endif}
ClearImage;
if LowQualityDisplay then begin
  imabmp.ResampleFilter:=rfBox;
  rmode:=rmSimpleStretch;
end
else begin
  imabmp.ResampleFilter:=rfBestQuality;
  rmode:=rmFineResample;
end;
if ImgZoom=0 then begin
  // adjust
  r1:=img_Width/img_Height;
  w:=ScrBmp.width;
  h:=ScrBmp.height;
  r2:=w/h;
  if r1>r2 then begin
    h:=trunc(w/r1);
    ImgScale0:=h/img_Height;
    px:=0;
    py:=(ScrBmp.Height-h) div 2;
  end else begin
    w:=trunc(h*r1);
    ImgScale0:=w/img_Width;
    px:=(ScrBmp.width-w) div 2;
    py:=0;
  end;
  OrigX:=round(px/ImgScale0);
  OrigY:=round(py/ImgScale0);
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Resample');{$endif}
  str:=ImaBmp.Resample(w,h,rmode) as TBGRABitmap;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PutImage');{$endif}
  ScrBmp.PutImage(px,py,str,dmSet);
  str.Free;
end
else if ImgZoom=1 then begin
   // zoom 1
   px:=round(ImgCx)-((img_Width-ScrBmp.Width) div 2);
   py:=round(ImgCy)-((img_Height-ScrBmp.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PutImage');{$endif}
   ScrBmp.PutImage(px,py,imabmp,dmSet);
end
else begin
   // other zoom
   if ImgZoom<ZoomMin then ImgZoom:=ZoomMin;
   tmpbmp:=TBGRABitmap.Create(round(ScrBmp.Width/ImgZoom),round(ScrBmp.Height/ImgZoom),clDarkBlue);
   px:=round(ImgCx)-((img_Width-tmpbmp.Width) div 2);
   py:=round(ImgCy)-((img_Height-tmpbmp.Height) div 2);
   OrigX:=px;
   OrigY:=py;
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PutImage');{$endif}
   tmpbmp.PutImage(px,py,ImaBmp,dmSet);
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Resample');{$endif}
   str:=tmpbmp.Resample(ScrBmp.Width,ScrBmp.Height,rmSimpleStretch) as TBGRABitmap;
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PutImage');{$endif}
   ScrBmp.PutImage(0,0,str,dmSet);
   str.Free;
   tmpbmp.Free;
end;

if f_visu.FlipHorz then {$ifdef debug_raw}begin; writeln(FormatDateTime(dateiso,Now)+blank+'HorizontalFlip');{$endif}ScrBmp.HorizontalFlip;{$ifdef debug_raw}end;{$endif}
if f_visu.FlipVert then {$ifdef debug_raw}begin; writeln(FormatDateTime(dateiso,Now)+blank+'VerticalFlip');{$endif}ScrBmp.VerticalFlip;{$ifdef debug_raw}end;{$endif}
Image1.Invalidate;
MagnifyerTimer.Enabled:=true;
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'PlotImage end');{$endif}
end;

procedure Tf_main.plot_north(bmp:TBGRABitmap);

var scale,s,c: double;
    xpos,ypos,Nleng,Eleng: single;
    polex,poley: double;
begin
if fits.HeaderInfo.solved and
 (cdcWCSinfo.secpix<>0)
 then begin
  scale:=img_Width/ScrBmp.Width;
  polex:=WCSpoleX+scale/2;
  poley:=WCSpoleY-scale/2;
  // is the pole in the image?
  if (polex>0)and(polex<img_Width)and(poley>0)and(poley<img_Height) then begin
    // mark the pole
    Nleng:=6*scale;
    bmp.DrawLineAntialias(polex-Nleng,poley,polex+Nleng,poley,ColorToBGRA(clOrange),scale);
    bmp.DrawLineAntialias(polex,poley-Nleng,polex,poley+Nleng,ColorToBGRA(clOrange),scale);
  end
  else begin
    // draw arrow to north pole
    xpos:=27*scale;
    ypos:=27*scale;
    Nleng:=24*scale;
    Eleng:=6*scale;

    sincos(WCSxyNrot,s,c);
    bmp.ArrowEndAsClassic(false,false,3);
    bmp.DrawLineAntialias(xpos,ypos,xpos+Nleng*s,ypos+Nleng*c,ColorToBGRA(clOrange),scale);
    bmp.ArrowEndAsNone;
    sincos(WCSxyErot,s,c);
    bmp.DrawLineAntialias(xpos,ypos,xpos+Eleng*s,ypos+Eleng*c,ColorToBGRA(clOrange),scale);

    bmp.FontHeight:=round(12*scale);
    bmp.TextOut(xpos,ypos,'  '+FormatFloat(f1,Rmod(cdcWCSinfo.rot+360,360)),clOrange);
  end;
 end;
end;

procedure Tf_main.Image1Paint(Sender: TObject);
var x,y,x1,y1,x2,y2,xr1,yr1,xr2,yr2,xxc,yyc,s,r,rc: integer;
    i,size: integer;
begin
  ScrBmp.Draw(Image1.Canvas,0,0,true);
  if PolarAlignmentOverlay then begin
     Fits2Screen(round(f_polaralign.StartX+PolarAlignmentOverlayOffsetX),round(f_polaralign.StartY+PolarAlignmentOverlayOffsetY),f_visu.FlipHorz,f_visu.FlipVert,x1,y1);
     Fits2Screen(round(f_polaralign.EndX+PolarAlignmentOverlayOffsetX),round(f_polaralign.EndY+PolarAlignmentOverlayOffsetY),f_visu.FlipHorz,f_visu.FlipVert,x2,y2);
     r:=DoScaleX(2);
     Image1.Canvas.brush.Style:=bsClear;
     Image1.Canvas.Pen.Color:=clGreen;
     Image1.Canvas.Pen.Mode:=pmCopy;
     Image1.Canvas.Pen.Style:=psSolid;
     Image1.Canvas.Pen.Width:=r;
     r:=4*r;
     CircleIntersect(x1,y1,r,x2,y2,xr1,yr1);
     CircleIntersect(x2,y2,r,x1,y1,xr2,yr2);
     Image1.Canvas.Line(xr1,yr1,xr2,yr2);
     image1.Canvas.Ellipse(x1-r,y1-r,x1+r,y1+r);
     Image1.Canvas.Pen.Color:=clPurple;
     image1.Canvas.Ellipse(x2-r,y2-r,x2+r,y2+r);
  end;
  if f_starprofile.FindStar and(f_starprofile.StarX>0)and(f_starprofile.StarY>0) then begin
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
     s:=max(3,round(max(ImgZoom,ImgScale0)*2.5*f_starprofile.HFD));
     r:=max(3,round(max(ImgZoom,ImgScale0)*f_starprofile.HFD));
     with Image1.Canvas do begin
        Pen.Color:=clLime;
        brush.Style:=bsClear;
        if r>0 then begin
          if Collimation then begin
            rc:=r*2;
            Line(x-rc,y,x+rc,y);
            Line(x,y-rc,x,y+rc);
            for i:=1 to CollimationCircle do begin
               rc:=round(r*2*i/CollimationCircle);
               EllipseC(x,y,rc,rc);
            end;
          end
          else begin
            Frame(x-s,y-s,x+s,y+s);
            EllipseC(x,y,r,r);
          end;
        end;
        brush.Style:=bsSolid;
     end;
  end;
  if (f_photometry<>nil) and f_photometry.Visible and (f_photometry.hfd>0) then begin
     Fits2Screen(round(f_photometry.StarX),round(f_photometry.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
     s:=max(3,round(max(ImgZoom,ImgScale0)*2.5*f_photometry.hfd));
     with Image1.Canvas do begin
        Pen.Color:=clTeal;
        Frame(x-s,y-s,x+s,y+s);
     end;
  end;
  if Length(fits.StarList)>0 then begin
     // draw all star boxes
     Image1.Canvas.pen.Color:=clRed;
     Image1.Canvas.pen.Mode:=pmMerge;
     Image1.Canvas.pen.Width:=DoScaleX(1);
     Image1.Canvas.Brush.Style:=bsClear;
     Image1.Canvas.Font.Color:=clYellow;
     Image1.Canvas.Font.Size:=DoScaleX(10);
     for i:=0 to Length(fits.StarList)-1 do
     begin
        if f_starprofile.AutofocusRunning and
           InplaceAutofocus and
           (fits.StarList[i].snr<AutofocusMinSNR)  // do not plot stars not used by autofocus
           then continue;
        Fits2Screen(round(fits.StarList[i].x),round(fits.StarList[i].y),f_visu.FlipHorz,f_visu.FlipVert,x,y);
        size:=max(3,round(max(ImgZoom,ImgScale0)*2.5*fits.StarList[i].hfd));
        Image1.Canvas.Rectangle(x-size,y-size, x+size, y+size);
        Image1.Canvas.TextOut(x+size,y+size,floattostrf(fits.StarList[i].hfd, ffgeneral, 2,1));
     end;
     if trpOK then begin
        {draw trapezium}
        Image1.Canvas.pen.Color:=clYellow;
        Image1.Canvas.pen.Width:=DoScaleX(2);
        Image1.Canvas.Font.Size:=DoScaleX(30);
        // x1,y1,x2,y2
        Fits2Screen(trpx1,trpy1,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        image1.Canvas.textout(x,y,floattostrF(median_bottom_left, ffgeneral, 3,2));
        Image1.Canvas.MoveTo(x,y);
        Fits2Screen(trpx2,trpy2,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        // x2,y2,x3,y3
        Fits2Screen(trpx3,trpy3,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        image1.Canvas.textout(x,y,floattostrF(median_top_right, ffgeneral, 3,2));
        Image1.Canvas.MoveTo(x,y);
        // x3,y3,x4,y4
        Fits2Screen(trpx4,trpy4,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        image1.Canvas.textout(x,y,floattostrF(median_top_left, ffgeneral, 3,2));
        Image1.Canvas.MoveTo(x,y);
        // x4,y4,x1,y1
        Fits2Screen(trpx1,trpy1,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);

        {draw diagonal}
        Fits2Screen(img_width div 2,img_height div 2,f_visu.FlipHorz,f_visu.FlipVert,xxc,yyc);
        // xxc,yyc,x1,y1
        image1.Canvas.textout(xxc,yyc,floattostrF(median_center, ffgeneral, 3,2));
        Image1.Canvas.MoveTo(xxc,yyc);
        Fits2Screen(trpx1,trpy1,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        // xxc,yyc,x2,y2
        Image1.Canvas.MoveTo(xxc,yyc);
        Fits2Screen(trpx2,trpy2,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        image1.Canvas.textout(x,y,floattostrF(median_bottom_right, ffgeneral, 3,2));
        // xxc,yyc,x3,y3
        Image1.Canvas.MoveTo(xxc,yyc);
        Fits2Screen(trpx3,trpy3,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
        // xxc,yyc,x4,y4
        Image1.Canvas.MoveTo(xxc,yyc);
        Fits2Screen(trpx4,trpy4,f_visu.FlipHorz,f_visu.FlipVert,x,y);
        Image1.Canvas.LineTo(x,y);
     end;
     Image1.Canvas.brush.Style:=bsSolid;
     Image1.Canvas.pen.Width:=1;
     Image1.Canvas.pen.Mode:=pmCopy;
  end;
end;

procedure  Tf_main.StarSelection(Sender: TObject);
begin
  // redraw star box
  image1.Invalidate;
  if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
end;

Procedure Tf_main.DrawHistogram(SetLevel,ResetCursor: boolean);
begin
  if fits.HeaderInfo.naxis>0 then begin
     f_visu.DrawHistogram(fits,SetLevel,ResetCursor);
  end;
end;

procedure Tf_main.MenuIndiSettingsClick(Sender: TObject);
begin
  if not GUIready then begin
     f_indigui:=Tf_indigui.Create(self);
     f_indigui.onDestroy:=@GUIdestroy;
     f_indigui.IndiServer:=config.GetValue('/INDIcamera/Server','');
     f_indigui.IndiPort:=config.GetValue('/INDIcamera/ServerPort','');
     GUIready:=true;
  end;
  FormPos(f_indigui,mouse.CursorPos.X,mouse.CursorPos.Y);
  f_indigui.Show;
end;

procedure Tf_main.MenuPolarAlignmentClick(Sender: TObject);
var pt: TPoint;
begin
  if camera.Status<>devConnected then begin
    ShowMessage(Format(rsNotConnected, [rsCamera]));
    exit;
  end;
  if (mount.Status=devConnected)and(mount.Park) then begin
    NewMessage(rsTheTelescope);
    exit;
  end;
  if f_preview.Loop then f_preview.BtnLoopClick(nil);
  f_polaralign.Mount:=mount;
  f_polaralign.Wheel:=wheel;
  pt.x:=0;
  pt.y:=PanelCenter.top;
  pt:=ClientToScreen(pt);
  FormPos(f_polaralign,pt.X,pt.Y);
  f_polaralign.Show;
end;

procedure Tf_main.PolaralignClose(Sender: TObject);
begin
  image1.Invalidate;
end;

procedure Tf_main.PhotometryClose(Sender: TObject);
begin
  image1.Invalidate;
end;

procedure Tf_main.MenuCollimationClick(Sender: TObject);
var pt: TPoint;
begin
  if camera.Status<>devConnected then begin
    ShowMessage(Format(rsNotConnected, [rsCamera]));
    exit;
  end;
  pt.x:=0;
  pt.y:=PanelCenter.top;
  pt:=ClientToScreen(pt);
  f_collimation.onStart:=@CollimationStart;
  f_collimation.onStop:=@CollimationStop;
  f_collimation.onCenterStar:=@CollimationCenterStar;
  f_collimation.onCircleChange:=@CollimationCircleChange;
  FormPos(f_collimation,pt.X,pt.Y);
  f_collimation.Show;
end;

procedure Tf_main.CollimationCenterStar(Sender: TObject);
begin
  if Collimation then begin
    CollimationStop(Sender);
    wait(2);
  end;
  if not f_visu.BullsEye then f_visu.BtnBullsEyeClick(Sender);
  f_preview.Loop:=true;
  if not f_preview.Running then begin
    f_preview.Running:=true;
    StartPreviewExposure(self);
  end;
end;

procedure Tf_main.CollimationCircleChange(Sender: TObject);
begin
  if Collimation then begin
    CollimationCircle:=f_collimation.CircleNum.Value;
  end;
end;

procedure Tf_main.CollimationStart(Sender: TObject);
begin
  if not Collimation then begin
    Collimation:=true;
    if f_preview.Running then begin
      if f_preview.Loop then
        f_preview.BtnLoopClick(nil)
      else
        f_preview.BtnPreviewClick(nil);
      wait(2);
    end;
    if f_visu.BullsEye then f_visu.BtnBullsEyeClick(Sender);
    Collimation:=true;
    CollimationCircle:=f_collimation.CircleNum.Value;
    FocusStart(nil);
  end;
end;

procedure Tf_main.CollimationStop(Sender: TObject);
begin
 if Collimation then begin
   Collimation:=false;
   FocusStop(nil);
 end
 else if f_preview.Running then begin
   f_preview.Running:=false;
   f_preview.Loop:=false;
   StopExposure(Sender);
 end;
end;


procedure Tf_main.MenuAscomSetupClick(Sender: TObject);
{$ifdef mswindows}
var
  n: integer;
  V: variant;
  dev: WideString;
  buf: string;
  IsConnected: boolean;
{$endif}
begin
{$ifdef mswindows}
  // check no capture is running
  if  (f_sequence.Running or f_preview.Running or f_capture.Running or autofocusing or learningvcurve) then begin
    ShowMessage('Cannot open the device configuration now!');
    exit;
  end;
  // check device
  n:=TButton(Sender).Tag;
  case n of
    1 : begin dev:=widestring(config.GetValue('/ASCOMcamera/Device',''));IsConnected:=(camera<>nil)and(camera.Status<>devDisconnected); end;
    2 : begin dev:=widestring(config.GetValue('/ASCOMwheel/Device',''));IsConnected:=(wheel<>nil)and(wheel.Status<>devDisconnected); end;
    3 : begin dev:=widestring(config.GetValue('/ASCOMfocuser/Device',''));IsConnected:=(focuser<>nil)and(focuser.Status<>devDisconnected); end;
    4 : begin dev:=widestring(config.GetValue('/ASCOMmount/Device',''));IsConnected:=(mount<>nil)and(mount.Status<>devDisconnected); end;
    5 : begin dev:=widestring(config.GetValue('/ASCOMrotator/Device',''));IsConnected:=(rotator<>nil)and(rotator.Status<>devDisconnected); end;
    6 : begin dev:=widestring(config.GetValue('/ASCOMweather/Device',''));IsConnected:=(weather<>nil)and(weather.Status<>devDisconnected); end;
    7 : begin dev:=widestring(config.GetValue('/ASCOMsafety/Device',''));IsConnected:=(safety<>nil)and(safety.Status<>devDisconnected); end;
    8 : begin dev:=widestring(config.GetValue('/ASCOMdome/Device',''));IsConnected:=(dome<>nil)and(dome.Status<>devDisconnected); end;
    10: begin dev:=widestring(config.GetValue('/ASCOMswitch/Device',''));IsConnected:=(switch<>nil)and(switch.Status<>devDisconnected); end;
    11: begin dev:=widestring(config.GetValue('/ASCOMcover/Device',''));IsConnected:=(cover<>nil)and(cover.Status<>devDisconnected); end;
    else begin dev:=''; IsConnected:=false; end;
  end;
  if dev='' then exit;
  // if connect, it need to be disconnected
  if IsConnected then begin
    if MessageDlg(Format(rsDeviceIsConn, [dev, crlf]), mtConfirmation, mbYesNo, 0)=mrYes then begin
      case n of
        1 : DisConnectCamera(nil);
        2 : DisConnectWheel(nil);
        3 : DisConnectFocuser(nil);
        4 : DisConnectMount(nil);
        5 : DisConnectRotator(nil);
        6 : DisConnectWeather(nil);
        7 : DisConnectSafety(nil);
        8 : DisConnectDome(nil);
        10: DisConnectSwitch(nil);
        11: DisConnectCover(nil);
      end;
    end
    else begin
      exit;
    end;
  end;
  try
    // Setup dialog
    V := CreateOleObject(string(dev));
    V.SetupDialog;
    V:=Unassigned;
  except
    on E: Exception do begin
        buf:=E.Message;
        ShowMessage('Setup error : ' + buf+crlf+'Check the device is not connected to another application.');
    end;
  end;
  // reconnect
  if IsConnected then begin
    case n of
      1 : ConnectCamera(nil);
      2 : ConnectWheel(nil);
      3 : ConnectFocuser(nil);
      4 : ConnectMount(nil);
      5 : ConnectRotator(nil);
      6 : ConnectWeather(nil);
      7 : ConnectSafety(nil);
      8 : ConnectDome(nil);
      10: ConnectSwitch(nil);
      11: ConnectCover(nil);
    end;
  end;
{$endif}
end;

procedure Tf_main.MenuAlpacaSetupClick(Sender: TObject);
var n: integer;
    devt,dev,num,host,port,protocol,url: string;
begin
// check device
n:=TButton(Sender).Tag;
case n of
  0 : begin devt:=''; dev:='server'; end;
  1 : begin devt:='ASCOMRestcamera'; dev:='camera'; end;
  2 : begin devt:='ASCOMRestwheel'; dev:='filterwheel'; end;
  3 : begin devt:='ASCOMRestfocuser'; dev:='focuser'; end;
  4 : begin devt:='ASCOMRestmount'; dev:='telescope'; end;
  5 : begin devt:='ASCOMRestrotator'; dev:='rotator'; end;
  6 : begin devt:='ASCOMRestweather'; dev:='observingconditions'; end;
  7 : begin devt:='ASCOMRestsafety'; dev:='safetymonitor'; end;
  8 : begin devt:='ASCOMRestdome'; dev:='dome'; end;
  10: begin devt:='ASCOMRestswitch'; dev:='switch'; end;
  11: begin devt:='ASCOMRestcover'; dev:='covercalibrator'; end;
  else begin dev:=''; end;
end;
if dev='' then exit;
if devt<>'' then begin
  num:=config.GetValue('/'+devt+'/Device','');
  host:=config.GetValue('/'+devt+'/Host','');
  port:=config.GetValue('/'+devt+'/Port','');
  protocol:=config.GetValue('/'+devt+'/Protocol','');
  if (num='')or(host='')or(port='')or(protocol='') then exit;
  if protocol='1' then protocol:='https:'
                  else protocol:='http:';
  url:=protocol+'//'+host+':'+port+'/setup/v1/'+dev+'/'+num+'/setup';
end
else begin
  devt:='ASCOMRestcamera';
  host:=config.GetValue('/'+devt+'/Host','');
  port:=config.GetValue('/'+devt+'/Port','');
  protocol:=config.GetValue('/'+devt+'/Protocol','');
  if (host='')or(port='')or(protocol='') then exit;
  if protocol='1' then protocol:='https:'
                  else protocol:='http:';
  url:=protocol+'//'+host+':'+port+'/setup';
end;
ExecuteFile(url);
end;

procedure Tf_main.MenuItemCleanupClick(Sender: TObject);
begin
   fits.ClearStarList;
   DrawImage;
end;

procedure Tf_main.MenuItemUnselectClick(Sender: TObject);
begin
  f_starprofile.FindStar:=false;
  DrawImage;
end;

procedure Tf_main.MenuItemPhotometryClick(Sender: TObject);
begin
if fits.HeaderInfo.valid then begin
  f_photometry.Show;
  MeasureAtPos(MouseDownX,MouseDownY,true);
  Image1.Invalidate;
end;
end;

procedure Tf_main.MagnitudeCalibrationChange(Sender: TObject);
begin
  if (f_photometry<>nil) and f_photometry.Visible then begin
    MeasureAtPos(MouseDownX,MouseDownY,true);
  end;
end;

procedure Tf_main.MenuItemDebayerClick(Sender: TObject);
begin
 if TMenuItem(Sender).Checked then begin
  BayerColor:=True;
  if fits.HeaderInfo.naxis>0 then begin
    fits.LoadStream;
    DrawHistogram(true,true);
    DrawImage;
    NewMessage(rsImageDebayer,1);
  end;
 end
 else begin
  BayerColor:=False;
  if fits.HeaderInfo.naxis>0 then begin
    fits.LoadStream;
    DrawHistogram(true,true);
    DrawImage;
    NewMessage(rsImageUnDebay,1);
  end;
 end;
 MenuItemDebayer.Checked:=BayerColor;
 MenuItemDebayer2.Checked:=BayerColor;
end;

procedure Tf_main.MenuMountParkClick(Sender: TObject);
begin
  f_mount.BtnPark.Click;
end;

procedure Tf_main.MenuMountTrackClick(Sender: TObject);
begin
  f_mount.BtnTrack.Click;
end;

procedure Tf_main.MenuOpenPictureClick(Sender: TObject);
var fext: string;
begin
  if OpenPictureDialog1.Execute then begin
   fext:=uppercase(extractfileext(OpenPictureDialog1.FileName));
   if ((fext='.FIT') or (fext='.FITS') or (fext='.FTS') or (fext='.FZ')) then
      LoadFitsFile(OpenPictureDialog1.FileName) {load fits file}
   else if pos(fext+',',UpperCase(rawext))>0  then
      LoadRawFile(OpenPictureDialog1.FileName) {load camera raw file}
   else
      LoadPictureFile(OpenPictureDialog1.FileName); {load picture file}
  end;
end;

procedure Tf_main.MenuOpenClick(Sender: TObject);
var fn: string;
begin
  OpenDialog1.Title:=Format(rsOpenFITSFile, ['']);
  if OpenDialog1.Execute then begin
     fn:=OpenDialog1.FileName;
     LoadFitsFile(fn);
  end;
end;

procedure Tf_main.MenuSaveClick(Sender: TObject);
var fn: string;
begin
if fits.HeaderInfo.naxis>0 then begin
   if SaveDialogFits.Execute then begin
      fn:=SaveDialogFits.FileName;
      SaveFitsFile(fn);
   end;
end;
end;

procedure Tf_main.MenuSavePictureClick(Sender: TObject);
var fn: string;
begin
if fits.HeaderInfo.naxis>0 then begin
   if SaveDialogPicture.Execute then begin
      fn:=SaveDialogPicture.FileName;
      fits.SaveToBitmap(fn);
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
  f.onMsg:=@NewMessage;
  f.DisableBayer:=true;
  try
  mem.LoadFromFile(reffile);
  f.Stream:=mem;
  f.LoadStream;
  if f.HeaderInfo.naxis>0 then begin
    f.Gamma:=f_visu.Gamma.Value;
    f.VisuMax:=round(f_visu.ImgMax);
    f.VisuMin:=round(f_visu.ImgMin);
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
  OpenDialog1.Title:=rsOpenReferenc;
  if OpenDialog1.Execute then begin
    OpenRefImage(OpenDialog1.FileName);
  end;
end;

procedure Tf_main.MenuClearRefClick(Sender: TObject);
begin
  ClearRefImage(Sender);
end;

procedure Tf_main.MenuFilterClick(Sender: TObject);
begin
  wheel.Filter:=TMenuItem(Sender).Tag;
end;

procedure Tf_main.MenuFocusaidClick(Sender: TObject);
begin
  f_starprofile.ChkFocus.Down:=not f_starprofile.ChkFocus.Down;
end;

procedure Tf_main.MenuFocuserCalibrationClick(Sender: TObject);
begin
  if focuser.Status<>devConnected then begin
    ShowMessage(Format(rsNotConnected, [rsFocuser]));
    exit;
  end;
  if camera.Status<>devConnected then begin
    ShowMessage(Format(rsNotConnected, [rsCamera]));
    exit;
  end;
  if not(focuser.hasRelativePosition or focuser.hasAbsolutePosition) then begin
    ShowMessage(rsTheFocuserDo);
    exit;
  end;
  if not AllDevicesConnected then begin
    ShowMessage(rsSomeDefinedD);
    exit;
  end;
  f_focusercalibration.focuser:=focuser;
  f_focusercalibration.onCalibration:=@FocuserCalibration;
  f_focusercalibration.onCalibrationClose:=@FocuserCalibrationClose;
  formpos(f_focusercalibration,mouse.CursorPos.x,mouse.CursorPos.y);
  f_focusercalibration.Show;
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

Procedure Tf_main.FocuserCalibration(Sender: TObject);
var i,j,k,x,y,xc,yc,rs,s,s2,s3,s4,bin,fgain,foffset,cc: integer;
    savepos,step,minstep,maxstep,newpos,initj,numhfd1,numhfd2: integer;
    vmax,exp,a1,a2,b1,b2,r1,r2,pi1,pi2,hfdmax,hfdmin,hfddyn:double;
    FocAbsolute,initstep,OutOfRange: boolean;
    hfd1,hfd2: array[0..100]of array[1..2] of double;
    step1: array[0..100] of integer;
    p:array of TDouble2;
    buf: string;

 procedure relIn(p:integer);
 begin
   if p>maxstep then p:=maxstep;
   if p<minstep then p:=minstep;
   focuser.FocusIn;
   focuser.RelPosition:=p;
   savepos:=savepos-p;
 end;
 procedure relOut(p:integer);
 begin
   if p>maxstep then p:=maxstep;
   if p<minstep then p:=minstep;
   focuser.FocusOut;
   focuser.RelPosition:=p;
   savepos:=savepos+p;
 end;

begin
  FocAbsolute:=focuser.hasAbsolutePosition;
  if FocAbsolute then begin
      minstep:=round(focuser.PositionRange.min);
      maxstep:=round(focuser.PositionRange.max);
  end
  else begin
    if focuser.hasRelativePosition then begin
      minstep:=round(focuser.RelPositionRange.min);
      maxstep:=round(focuser.RelPositionRange.max);
    end
    else begin
      buf:=rsTheFocuserDo;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;
  end;
  f_focusercalibration.FocAbsolute:=FocAbsolute;
  if  f_capture.Running  then begin
    buf:=rsCannotRunCal;
    NewMessage(buf,1);
    f_focusercalibration.CalibrationCancel(buf);
    exit;
  end;
  if f_preview.Running then begin
    if f_preview.Loop then
      f_preview.BtnLoopClick(nil)
    else
      f_preview.BtnPreviewClick(nil);
    wait(2);
  end;
  OutOfRange:=false;
  if FocAbsolute then
    savepos:=focuser.Position
  else
    savepos:=0;
  step:=f_focusercalibration.MinStep;
  hfdmax:=f_focusercalibration.MaxHfd;
  // check window size is enough to reach hfd=20
  if (Starwindow)< (2.5*hfdmax) then
    Starwindow:=max(20,round(2.5*hfdmax));
  if Focuswindow < (4*Starwindow) then
    Focuswindow:=4*Starwindow;
  if (not f_starprofile.FindStar)and(fits.HeaderInfo.valid) then begin
    x:=fits.HeaderInfo.naxis1 div 2;
    y:=fits.HeaderInfo.naxis2 div 2;
    rs:=2*min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 3;
    fits.FindBrightestPixel(x,y,rs,starwindow div 2,xc,yc,vmax);
    f_starprofile.FindStar:=(vmax>0);
    f_starprofile.StarX:=xc;
    f_starprofile.StarY:=yc;
  end;
  if f_starprofile.FindStar then begin
    try
     bin:=camera.BinX;
     exp:=f_preview.Exposure;
     fgain:=f_preview.Gain;
     foffset:=f_preview.Offset;
     s:=min(Focuswindow,min(fits.HeaderInfo.naxis1 div 2,fits.HeaderInfo.naxis2 div 2));
     s2:=s div 2;
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
     Screen2CCD(x,y,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,xc,yc);
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
     SaveFocusZoom:=f_visu.Zoom;
     f_visu.Zoom:=0;
     ImgZoom:=0;
     f_preview.StackPreview.Checked:=false;
     NewMessage(rsFocuserCalib2,1);
     hfdmin:=9999;
     j:=0;
     initstep:=true;
     initj:=0;
     numhfd1:=0;
     TerminateFocuserCalibration:=false;
     // measure in AutofocusMoveDir direction
     if AutofocusMoveDir then
       NewMessage(rsSetFocusDire,2)
     else
       NewMessage(rsSetFocusDire2,2);
     repeat
       if not f_preview.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeFocus,fgain,foffset) then begin
          buf:=rsExposureFail;
          NewMessage(buf,1);
          f_focusercalibration.CalibrationCancel(buf);
          exit;
       end;
       if TerminateFocuserCalibration then begin
         buf:=rsRequestToSto;
         NewMessage(buf,1);
         f_focusercalibration.CalibrationCancel(buf);
         exit;
       end;
       f_starprofile.showprofile(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
       if j=0 then begin
         if FocAbsolute then
           NewMessage(Format(rsStartPositio, [IntToStr(focuser.Position),
             FormatFloat(f1, f_starprofile.hfd), FormatFloat(f1,
             f_starprofile.ValMaxCalibrated), FormatFloat(f1, f_starprofile.SNR)]),2)
         else
           NewMessage(Format(rsStartPositio, [IntToStr(savepos), FormatFloat(
             f1, f_starprofile.hfd), FormatFloat(f1, f_starprofile.ValMaxCalibrated),
             FormatFloat(f1, f_starprofile.SNR)]),2);
       end
       else begin
         if FocAbsolute then
           NewMessage(Format(rsMeasurementP, [inttostr(j), IntToStr(
             focuser.Position), IntToStr(step), FormatFloat(f1,
             f_starprofile.hfd), FormatFloat(f1, f_starprofile.ValMaxCalibrated),
             FormatFloat(f1, f_starprofile.SNR)]),2)
         else
           NewMessage(Format(rsMeasurementP, [inttostr(j), IntToStr(savepos),
             IntToStr(step), FormatFloat(f1, f_starprofile.hfd), FormatFloat(
             f1, f_starprofile.ValMaxCalibrated), FormatFloat(f1, f_starprofile.SNR)]),2);
       end;
       if FocAbsolute then
          hfd1[j,1]:=focuser.Position
       else
          hfd1[j,1]:=savepos;
       hfd1[j,2]:=f_starprofile.hfd;
       step1[j]:=step;
       if f_starprofile.hfd<hfdmin then hfdmin:=f_starprofile.hfd;
       numhfd1:=j;
       f_focusercalibration.ProgressR(j,hfd1[j,1],hfd1[j,2]);
       if initstep and (j>0) then begin
         if (hfd1[j,2]-hfd1[0,2])>hfd1[0,2] then begin
            initstep:=false;
            initj:=j;
         end
         else begin
            if step<10 then
              step:=2*step
            else
              step:=round(1.5*step);
         end;
       end;
       inc(j);
       if (f_starprofile.hfd<hfdmax) then begin
         // set new focuser position
         if AutofocusMoveDir then begin
           if FocAbsolute then begin
             newpos:=focuser.Position-step;
             if newpos>minstep then
                focuser.Position:=newpos
             else
                OutOfRange:=true;
           end
           else begin
             relIn(step);
           end;
         end
         else begin
            if FocAbsolute then begin
              newpos:=focuser.Position+step;
              if newpos<maxstep then
                 focuser.Position:=newpos
              else
                 OutOfRange:=true;
            end
            else begin
              relOut(step);
            end;
         end;
         wait(1);
       end;
    until (f_starprofile.hfd>=hfdmax)or(OutOfRange)or(initstep and(j>30));
    if (initstep) then begin
      buf:=rsTheFocuserDo2;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;
    if OutOfRange then begin
      buf:=rsReachFocuser;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;
    // measure in reverse direction
    if AutofocusMoveDir then begin
      NewMessage(rsSetFocusDire2,2);
      if FocAbsolute then begin
        focuser.Position:=round(hfd1[0,1]);
      end
      else begin
        relOut(savepos);
      end;
    end
    else begin
      NewMessage(rsSetFocusDire,2);
      if FocAbsolute then begin
        focuser.Position:=round(hfd1[0,1]);
      end
      else begin
        relIn(savepos);
      end;
    end;
    wait(1);
    numhfd2:=0;
    j:=0;
    repeat
      if not f_preview.ControlExposure(exp,bin,bin,LIGHT,ReadoutModeFocus,fgain,foffset) then begin
         buf:=rsExposureFail;
         NewMessage(buf,1);
         f_focusercalibration.CalibrationCancel(buf);
         exit;
      end;
      if TerminateFocuserCalibration then begin
        buf:=rsRequestToSto;
        NewMessage(buf,1);
        f_focusercalibration.CalibrationCancel(buf);
        exit;
      end;
      f_starprofile.showprofile(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
      if j=0 then begin
        if FocAbsolute then
          NewMessage(Format(rsStartPositio, [IntToStr(focuser.Position),
            FormatFloat(f1, f_starprofile.hfd), FormatFloat(f1,
            f_starprofile.ValMaxCalibrated), FormatFloat(f1, f_starprofile.SNR)]),2)
        else
          NewMessage(Format(rsStartPositio, [IntToStr(savepos), FormatFloat(f1,
            f_starprofile.hfd), FormatFloat(f1, f_starprofile.ValMaxCalibrated),
            FormatFloat(f1, f_starprofile.SNR)]),2);
      end
      else begin
        if FocAbsolute then
          NewMessage(Format(rsMeasurementP, [inttostr(j), IntToStr(
            focuser.Position), IntToStr(step), FormatFloat(f1, f_starprofile.hfd
            ), FormatFloat(f1, f_starprofile.ValMaxCalibrated), FormatFloat(f1,
            f_starprofile.SNR)]),2)
        else
          NewMessage(Format(rsMeasurementP, [inttostr(j), IntToStr(savepos),
            IntToStr(step), FormatFloat(f1, f_starprofile.hfd), FormatFloat(f1,
            f_starprofile.ValMaxCalibrated), FormatFloat(f1, f_starprofile.SNR)]),2);
      end;
      if FocAbsolute then
         hfd2[j,1]:=focuser.Position
      else
         hfd2[j,1]:=savepos;
      hfd2[j,2]:=f_starprofile.hfd;
      if f_starprofile.hfd<hfdmin then hfdmin:=f_starprofile.hfd;
      numhfd2:=j;
      f_focusercalibration.ProgressL(j,hfd2[j,1],hfd2[j,2]);
      inc(j);
      if (f_starprofile.hfd<hfdmax) then begin
        // set new focuser position
        if AutofocusMoveDir then begin
          if FocAbsolute then begin
            newpos:=focuser.Position+step;
            if newpos<focuser.PositionRange.max then
               focuser.Position:=newpos
            else
               OutOfRange:=true;
          end
          else begin
            relOut(step);
          end;
        end
        else begin
          if FocAbsolute then begin
            newpos:=focuser.Position-step;
            if newpos>focuser.PositionRange.min then
               focuser.Position:=newpos
            else
               OutOfRange:=true;
          end
          else begin
            relIn(step);
          end;
        end;
        wait(1);
      end;
   until (f_starprofile.hfd>=hfdmax)or(OutOfRange)or(j>30);
    if (j>30) then begin
      buf:=rsTheFocuserDo3;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;
    if OutOfRange then begin
      buf:=rsReachFocuser;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;

    // compute values
    k:=numhfd1-initj+1;
    if k<3 then begin
      buf:=rsNotEnoughMea;
      NewMessage(buf,1);
      f_focusercalibration.CalibrationCancel(buf);
      exit;
    end;
    SetLength(p,k);
    for i:=0 to k-1 do begin
       p[i,1]:=hfd1[initj+i,1];
       p[i,2]:=hfd1[initj+i,2];
    end;
    LeastSquares(p,a1,b1,r1);
    pi1:=-b1/a1;
    k:=min(3,numhfd2+1);
    if k>2 then begin
      SetLength(p,k);
      for i:=0 to k-1 do begin
         p[i,1]:=hfd2[(numhfd2-k+1)+i,1];
         p[i,2]:=hfd2[(numhfd2-k+1)+i,2];
      end;
      LeastSquares(p,a2,b2,r2);
      pi2:=-b2/a2;
      cc:=round((pi1+pi2)/2);
    end
    else
      cc:=round(hfd1[0,1]);
    AutofocusBinning:=bin;
    AutofocusNearHFD:=hfdmin+(hfdmax-hfdmin)/2;
    AutofocusStartHFD:=AutofocusNearHFD+(hfdmax-AutofocusNearHFD)/2;
    // set safe values
    AutofocusNearNum:=3;
    AutofocusTolerance:=2*hfd1[0,2];
    AutofocusMinSNR:=3;
    AutofocusInPlace:=True;
    // vcurve parameters
    if FocAbsolute then begin
      // dynamic is more easy for new user, just store vcurve parameters for future use
      VcCenterpos:=cc;
      VcHalfwidth:=round(abs((hfdmax-b1)/a1-cc));
      VcNsteps:=15;
      AutoFocusMode:=afDynamic;
    end
    else
      AutoFocusMode:=afDynamic;
    // dynamic
    hfddyn:=min(AutofocusNearHFD,2.5*hfdmin);
    AutofocusDynamicNumPoint:=7;
    AutofocusDynamicMovement:=round(abs(((hfddyn-b1)/a1-cc)/3));
    AutofocusPlanetNumPoint:=AutofocusDynamicNumPoint;
    AutofocusPlanetMovement:=AutofocusDynamicMovement;
    // iterative
    AutofocusMaxSpeed:=step;
    for i:=1 to numhfd1 do begin
      if (hfd1[i,2]-hfd1[0,2])>(0.1*hfd1[0,2]) then begin
        AutofocusMinSpeed:=step1[i] div 2;
        break;
      end;
    end;
    // save config
    f_focusercalibration.ValueListEditor1.Clear;
    f_focusercalibration.ValueListEditor1.InsertRow(rsBinning, inttostr(AutofocusBinning)+'x'+inttostr(AutofocusBinning), true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsStartFocus,inttostr(round(AutofocusStartHFD)),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsNearFocus,inttostr(round(AutofocusNearHFD)),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsNumberOfExpo,inttostr(AutofocusNearNum),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsAutofocusTol,FormatFloat(f1,AutofocusTolerance),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsMinSNR,FormatFloat(f1,AutofocusMinSNR),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsStarDetectio,inttostr(Starwindow),true);
    f_focusercalibration.ValueListEditor1.InsertRow(rsFocusWindowS,inttostr(Focuswindow),true);
    if AutofocusMoveDir then
      f_focusercalibration.ValueListEditor1.InsertRow(rsMoveDirectio,rsIn,true)
    else
      f_focusercalibration.ValueListEditor1.InsertRow(rsMoveDirectio,rsOut,true);
    f_focusercalibration.ValueListEditor2.Clear;
    if AutoFocusMode=afVcurve then
       f_focusercalibration.ValueListEditor2.InsertRow(rsAutofocusMet,rsVCurve,true)
    else
       f_focusercalibration.ValueListEditor2.InsertRow(rsAutofocusMet,rsDynamic,true);
    f_focusercalibration.ValueListEditor2.InsertRow(rsStayInPlace,BoolToStr(AutofocusInPlace,rsTrue, rsFalse),true);
    if FocAbsolute then begin
      f_focusercalibration.ValueListEditor2.InsertRow(rsFocusPositio,inttostr(VcCenterpos),true);
      f_focusercalibration.ValueListEditor2.InsertRow(rsMaxOffset,inttostr(VcHalfwidth),true);
      f_focusercalibration.ValueListEditor2.InsertRow(rsNumberOfStep,inttostr(VcNsteps),true);
    end;
    f_focusercalibration.ValueListEditor2.InsertRow(rsNumberOfDyna,inttostr(AutofocusDynamicNumPoint),true);
    f_focusercalibration.ValueListEditor2.InsertRow(rsMovementBetw,inttostr(AutofocusDynamicMovement),true);
    f_focusercalibration.ValueListEditor2.InsertRow(rsInitialMovem,inttostr(AutofocusMaxSpeed),true);
    f_focusercalibration.ValueListEditor2.InsertRow(rsFinalMovemen,inttostr(AutofocusMinSpeed),true);
    finally
      f_starprofile.FindStar:=false;
      if FocAbsolute then
         focuser.Position:=savepos
      else begin
        if focuser.LastDirection=FocusDirIn then
           relout(abs(savepos))
        else
           relin(abs(savepos));
      end;
      wait(1);
      camera.ResetFrame;
      f_preview.Loop:=false;
      f_preview.BtnPreviewClick(nil);
    end;
  end
  else begin
    buf:=rsSelectAStarF;
    NewMessage(buf,1);
    f_focusercalibration.CalibrationCancel(buf);
  end;
end;

Procedure Tf_main.FocuserCalibrationClose(Sender: TObject);
begin
 SetOptions;
end;

Procedure Tf_main.FocusStart(Sender: TObject);
var x,y,xc,yc,s,s2,s3,s4: integer;
    vmax:double;
begin
if  f_capture.Running  then begin
  NewMessage(rsCannotStartM,1);
  f_starprofile.ChkFocusDown(false);
  exit;
 end;
if (AutofocusMode=afPlanet)and(Sender<>nil) then begin
  SaveFocusZoom:=f_visu.Zoom;
  f_preview.StackPreview.Checked:=false;
  if not f_preview.Loop then f_preview.Loop:=true;
  if not f_preview.Running then begin
    f_preview.Running:=true;
    StartPreviewExposure(self);
  end;
  NewMessage(rsFocusAidStar,1);
end
else begin
  if (not f_starprofile.FindStar)and(fits.HeaderInfo.valid) then begin
    x:=fits.HeaderInfo.naxis1 div 2;
    y:=fits.HeaderInfo.naxis2 div 2;
    s:=2*min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 3;
    fits.FindBrightestPixel(x,y,s,starwindow div 2,xc,yc,vmax);
    f_starprofile.FindStar:=(vmax>0);
    f_starprofile.StarX:=xc;
    f_starprofile.StarY:=yc;
  end;
  if f_starprofile.FindStar then begin
     s:=min(Focuswindow,min(fits.HeaderInfo.naxis1 div 2,fits.HeaderInfo.naxis2 div 2));
     s2:=s div 2;
     Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
     Screen2CCD(x,y,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,xc,yc);
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
     SaveFocusZoom:=f_visu.Zoom;
     f_visu.Zoom:=0;
     ImgZoom:=0;
     f_preview.StackPreview.Checked:=false;
     if not f_preview.Loop then f_preview.Loop:=true;
     if not f_preview.Running then begin
       f_preview.Running:=true;
       StartPreviewExposure(self);
     end;
     if (Sender<>nil) then NewMessage(rsFocusAidStar,1);
  end
  else begin
    f_starprofile.ChkFocusDown(false);
    NewMessage(rsSelectAStarF,1);
  end;
end;
end;

Procedure Tf_main.FocusStop(Sender: TObject);
begin
   if  f_capture.Running then exit;
   f_preview.Running:=false;
   f_preview.Loop:=false;
   camera.AbortExposure;
   fits.SetBPM(bpm,0,0,0,0);
   fits.DarkOn:=false;
   camera.ResetFrame;
   f_visu.Zoom:=SaveFocusZoom;
   ImgZoom:=f_visu.Zoom;
   f_starprofile.StarX:=-1;
   f_starprofile.StarY:=-1;
   f_starprofile.FindStar:=false;
   StartPreviewExposure(nil);
   if Sender<>nil then NewMessage(rsFocusAidStop,1);
   if (Sender<>nil)and focuser.hasTemperature then NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
end;

procedure Tf_main.LoadFocusStar(focusmag:integer);
var f: textfile;
    buf,fn,id: string;
    ra,de: double;
begin
 if focusmag=FocusStarMag then exit;

 fn:='focus_star_4';
 SetLength(FocusStars,10000);
 NFocusStars:=0;
 if (focusmag<4) then focusmag:=4;
 if (focusmag>8) then focusmag:=8;
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
        SetLength(FocusStars,NFocusStars+10000);
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
 SetLength(FocusStars,NFocusStars+1);
 FocusStarMag:=focusmag;
 except
   NewMessage('Error loading focus star list '+slash(DataDir)+slash('stars')+fn,1);
 end;
end;

function Tf_main.FindFocusStar(tra, tde:double; out sra,sde: double; out id: string): Boolean;
var i: integer;
    CurST,d,dmin,hh,hl,ta,th,a,h: double;
    tm,sm: TPierSide;
begin
  // all parameters coordinates of the date
  result:=false;
  dmin:=pi2;
  CurST:=CurrentSidTim;
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
var tra,tde,teq,tpa,sra,sde,err: double;
    sid,r: string;
    focusretry,maxretry: integer;
    tpos,pslew,savecapture,saveearlystart,restartguider,pauseguider: boolean;
    configfocusmag,newfocusmag,focusmagdiff: integer;
    savefilterfact,newfilterfact: double;
begin
 maxretry:=3;
 result:=false;
 CancelAutofocus:=false;
 if autofocusing then begin
   NewMessage(rsAutofocusAlr,1);
   exit;
 end;
 if (AutofocusMode=afNone) then begin
   // do not abort if autofocus is not configured and not using a focuser
   if focuser.Status<>devConnected then
     result:=true;
   NewMessage(rsAutoFocusErr+': '+rsPleaseConfig2,1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
 end;
 if (camera.Status<>devConnected)or(focuser.Status<>devConnected) then begin
  // abort if configured but focuser not connected
  NewMessage(rsCameraOrFocu,1);
  f_starprofile.ChkAutofocusDown(false);
  exit;
 end;
 if(AutofocusMode=afVcurve) and (config.GetValue('/StarAnalysis/Vcurve/AutofocusVcBinning',AutofocusBinning)<>AutofocusBinning) then begin
   NewMessage(Format(rsPleaseRunVcu, [inttostr(AutofocusBinning)]),1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
 end;
 if (AutofocusMode=afVcurve) and(AutofocusVcNum<=0) then begin
   NewMessage(rsPleaseRunThe2, 1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
 end;
 if (AutofocusMode=afVcurve) and(AutofocusVcDir<>AutofocusMoveDir) then begin
   NewMessage(rsPleaseRunThe,1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
 end;
 if EarlyNextExposure then Application.ProcessMessages; // save previous image
 NewMessage(rsAutofocusNow,1);
 autofocusing:=true;
 savecapture:=Capture;
 saveearlystart:=EarlyNextExposure;
 f_preview.StackPreview.Checked:=false;
 try
 Capture:=false;
 tpos:=false;
 pslew:=false;
 restartguider:=(Autoguider.State<>GUIDER_DISCONNECTED);
 pauseguider:=false;
 if InplaceAutofocus then begin
   // stay in place for autofocus
   try
   NewMessage(rsStayAtTheCur,2);
   if AutofocusPauseGuider then begin
     // pause autoguider
     pauseguider:=Autoguider.State=GUIDER_GUIDING;
     if pauseguider then begin
       NewMessage(rsPauseAutogui,2);
       autoguider.Pause(True);
       Wait(2);
     end;
   end;
   if CancelAutofocus then exit;
   // do autofocus
   if focuser.hasTemperature then NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
   f_starprofile.ChkAutofocusDown(true);
   while f_starprofile.ChkAutofocus.Down do begin
    sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
    if CancelAutofocus then begin
      f_starprofile.ChkAutofocusDown(false);
      exit;
    end;
   end;
   // check result
   if f_starprofile.AutofocusResult then begin
     result:=true;
   end
   else begin
      NewMessage(rsInPlaceAutof,1);
      NewMessage(rsSequenceWill,1);
      result:=true;
      if EmailAufofocus then begin
        r:=email(rsInPlaceAutof, rsInPlaceAutof+', '+rsSequenceWill);
        if r='' then r:=rsEmailSentSuc;
        NewMessage(r,9);
      end;
   end;
   finally
   if AutofocusPauseGuider then begin
     // restart autoguider, never let in pause in case autofocus is aborted
     if pauseguider then begin
       NewMessage(rsResumeAutogu,2);
       autoguider.Pause(false);
       Wait(5);
     end
     else if restartguider and (not CancelAutofocus)then begin
       NewMessage(rsRestartAutog,2);
       autoguider.Guide(false);
       wait(5);
       autoguider.Guide(true);
       autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
       if autoguider.State<>GUIDER_GUIDING then begin
          NewMessage(rsFailedToStar,1);
          result:=false;
       end;
     end;
   end;
   end;
 end
 else
 begin
   // move to focus star
   if restartguider and (Autoguider.State=GUIDER_GUIDING) then begin
     // stop autoguider
     NewMessage(rsStopAutoguid,2);
     autoguider.Guide(false);
     autoguider.WaitBusy(15);
   end;
   // get current position from target object
   if (f_sequence.Running) and (f_sequence.TargetCoord) then begin
     NewMessage(rsGetCurrentPo,2);
     if (f_sequence.TargetRA<>NullCoord)and(f_sequence.TargetDE<>NullCoord) then begin
       tra:=f_sequence.TargetRA;
       tde:=f_sequence.TargetDE;
       tpos:=true;
       pslew:=true;
     end;
   end;
   // get current position from last capture image
   if (not tpos)and(f_capture.Running)and(f_capture.SeqCount>1) and fits.HeaderInfo.valid and (astrometryResolver<>ResolverNone) then begin
     NewMessage(rsGetCurrentPo2,2);
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
      NewMessage(rsCannotSolveC,2);
     end;
   end;
   // get current position from telescope
   if (not tpos) then begin
    NewMessage(rsGetCurrentPo3,2);
    tra:=mount.RA;
    tde:=mount.Dec;
    MountToLocal(mount.EquinoxJD,tra,tde);
    tra:=deg2rad*15*tra;
    tde:=deg2rad*tde;
    pslew:=false;
    tpos:=true;
   end;
   if CancelAutofocus then exit;
   // Adjust star magnitude and exposure factor
   savefilterfact:=AutofocusExposureFact;
   newfocusmag:=FocusStarMag;
   newfilterfact:=AutofocusExposureFact;
   if FocusStarMagAdjust then begin
     // find the magnitude difference to apply based on the configured exposure factor
     // the upper limit is set to 4 * 2.512**diff
     if AutofocusExposureFact<4 then
       focusmagdiff:=0
     else if AutofocusExposureFact<10 then // 4 * 2.512**1
       focusmagdiff:=1
     else if AutofocusExposureFact<25 then // 4 * 2.512**2
       focusmagdiff:=2
     else if AutofocusExposureFact<63 then // 4 * 2.512**3
       focusmagdiff:=3
     else
       focusmagdiff:=4;
     configfocusmag:=config.GetValue('/StarAnalysis/AutofocusStarMag',4);
     // new focus magnitude is limited to 4
     newfocusmag:=max(4,configfocusmag-focusmagdiff);
     // real difference
     focusmagdiff:=configfocusmag-newfocusmag;
     // new exposure factor
     if focusmagdiff>0 then begin
        newfilterfact:=AutofocusExposureFact/(2.512**focusmagdiff);
        NewMessage(Format(rsAdjustAutofo, [inttostr(newfocusmag), FormatFloat(f2, newfilterfact)]));
     end;
   end;
   // load the new star list, nothing is done if the magnitude do not change
   LoadFocusStar(newfocusmag);
   // Loop star list until focus success
   focusretry:=0;
   FocusStarsBlacklist:='';
   try
   repeat
     inc(focusretry);
     // search focus star
     if FindFocusStar(tra,tde,sra,sde,sid) then begin
       // slew to star
       NewMessage(Format(rsSlewToFocusS, [sid]),2);
       sra:=rad2deg*sra/15;
       sde:=rad2deg*sde;
       LocalToMount(mount.EquinoxJD,sra,sde);
       astrometry.AutofocusPrecisionSlew(sra,sde,err);
       if CancelAutofocus then exit;
     end
     else begin
      NewMessage(rsCannotFindAF,1);
      if NFocusStars=0 then NewMessage(rsStarDatabase,1);
      exit;
     end;
     wait(1);
     if CancelAutofocus then exit;
     // do autofocus
     if focuser.hasTemperature then NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
     // set the new exposure factor here, because PrecisionSlew can set another filter
     AutofocusExposureFact:=newfilterfact;
     f_starprofile.ChkAutofocusDown(true);
     while f_starprofile.ChkAutofocus.Down do begin
      sleep(100);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      if CancelAutofocus then begin
        f_starprofile.ChkAutofocusDown(false);
        exit;
      end;
     end;
     // if not successful, blacklist the current star and try another
     if not f_starprofile.AutofocusResult then begin
        FocusStarsBlacklist:=FocusStarsBlacklist+' '+sid+' ';
        NewMessage(rsAutofocusFai2,2);
     end;
     if CancelAutofocus then exit;
   until f_starprofile.AutofocusResult or (focusretry>=maxretry);
   finally
     // reset exposure factor
     AutofocusExposureFact := savefilterfact;
   end;
   if not f_starprofile.AutofocusResult then begin
      NewMessage(Format(rsAutofocusFai3, [inttostr(maxretry)]),1);
      if EmailAufofocus then begin
        r:=email(rsAutofocusFai, rsAutofocusFai+crlf+Format(rsAutofocusFai3, [inttostr(maxretry)]));
        if r='' then r:=rsEmailSentSuc;
        NewMessage(r,9);
      end;
   end;
   if CancelAutofocus then exit;
   if ReturnToTarget then begin
     // recenter to previous position
     NewMessage(rsReturnToTarg,2);
     tra:=rad2deg*tra/15;
     tde:=rad2deg*tde;
     LocalToMount(mount.EquinoxJD,tra,tde);
     if pslew then begin
        result:=astrometry.PrecisionSlew(tra,tde,err);
     end else begin
        result:=mount.Slew(tra,tde);
     end;
     if CancelAutofocus then exit;
    end
   else begin
    result:=true;
   end;
   // start autoguider
   if restartguider then begin
    NewMessage(rsRestartAutog,2);
    autoguider.Guide(false);
    wait(5);
    autoguider.Guide(true);
    autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
    if autoguider.State<>GUIDER_GUIDING then begin
       NewMessage(rsFailedToStar,1);
       result:=false;
    end;
   end;
 end;
 Wait(2);
 finally
   autofocusing:=false;
   CancelAutofocus:=false;
   Capture:=savecapture;
   EarlyNextExposure:=saveearlystart;
 end;
end;

procedure Tf_main.cmdAutomaticAutofocus(var ok: boolean);
begin
  ok:=AutoAutofocus;
end;

procedure Tf_main.cmdAutofocus(var ok: boolean);
begin
 f_starprofile.ChkAutofocusDown(true);
 while f_starprofile.ChkAutofocus.Down do begin
  sleep(100);
  if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
 end;
 ok:=f_starprofile.AutofocusResult;
end;

Procedure Tf_main.AutoFocusStart(Sender: TObject);
var x,y,rx,ry,xc,yc,ns,n,i,s,s2,s3,s4: integer;
    hfdlist: array of double;
    vmax,meanhfd, med: double;
    buf: string;
begin
  CancelAutofocus:=false;
  f_starprofile.AutofocusResult:=false;
  SaveAutofocusBinning:=f_preview.Binning.Text;
  SaveAutofocusBX:=camera.BinX;
  SaveAutofocusBY:=camera.BinY;
  SaveAutofocusPreviewGain:=f_preview.Gain;
  SaveAutofocusPreviewOffset:=f_preview.Offset;
  SaveAutofocusGain:=camera.Gain;
  SaveAutofocusOffset:=camera.Offset;
  camera.GetFrame(SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH);
  if (camera.Status<>devConnected)or(focuser.Status<>devConnected) then begin
   NewMessage(rsCameraOrFocu,1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
  end;
  if not AllDevicesConnected then begin
    NewMessage(rsSomeDefinedD,1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if f_preview.Running then begin
    if f_preview.Loop then
      f_preview.BtnLoopClick(nil)
    else
      f_preview.BtnPreviewClick(nil);
    wait(2);
  end;
  if  astrometry.Busy then begin
   NewMessage(rsCannotStartA2,1);
   f_starprofile.ChkAutofocusDown(false);
   exit;
  end;
  if (AutofocusMode=afNone) then begin
    NewMessage(rsPleaseConfig2,1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if (f_capture.Running and (not autofocusing)) then begin
    NewMessage(rsCannotStartA3,1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if (AutofocusMode=afVcurve) and((AutofocusVcNum<=0)or(AutofocusVcpiL<0)or(AutofocusVcpiR<0)) then begin
    NewMessage(rsPleaseRunThe2, 1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if (AutofocusMode=afVcurve) and(AutofocusVcDir<>AutofocusMoveDir) then begin
    NewMessage(rsPleaseRunThe,1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if (AutofocusMode=afVcurve) and (config.GetValue('/StarAnalysis/Vcurve/AutofocusVcBinning',AutofocusBinning)<>AutofocusBinning) then begin
    NewMessage(Format(rsPleaseRunVcu, [inttostr(AutofocusBinning)]),1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  // protect again wrong settting
  if AutofocusExposureFact<=0 then AutofocusExposureFact:=1;
  if AutofocusExposure<=0 then AutofocusExposure:=1;
  // start a new exposure as the current frame is probably not a preview
  f_preview.Exposure:=AutofocusExposure*AutofocusExposureFact;
  f_preview.Gain:=AutofocusGain;
  f_preview.Offset:=AutofocusOffset;
  f_preview.Binning.Text:=inttostr(AutofocusBinning)+'x'+inttostr(AutofocusBinning);
  camera.SetBinning(AutofocusBinning,AutofocusBinning);
  fits.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  fits.DarkOn:=true;
  if not f_preview.ControlExposure(AutofocusExposure*AutofocusExposureFact,AutofocusBinning,AutofocusBinning,LIGHT,ReadoutModeFocus,AutofocusGain,AutofocusOffset) then begin
    NewMessage(rsExposureFail,1);
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if CancelAutofocus then begin
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if InplaceAutofocus then begin  // use multiple stars
    if AutofocusMode<>afPlanet then begin
     // first measurement with a big window to find median star diameter
     s:=starwindow; {use configured star window}
     rx:=img_Width-6*s; {search area}
     ry:=img_Height-6*s;
     fits.GetStarList(rx,ry,s); {search stars in fits image}
     ns:=Length(fits.StarList);
     if ns>0 then begin
       SetLength(hfdlist,ns);
       for i:=0 to ns-1 do
         hfdlist[i]:=fits.StarList[i].hfd;
       med:=SMedian(hfdlist);            {median of starshfd}
       s:=min(max(14,round(3.0*med)),s); {reasonable window to measure this star}
     end
     else
       s:=20; {no star found, try with small default window}

     if AutofocusMultiStarCenter then begin  // reduce search area to image center
       if max(img_Height,img_Width)/min(img_Height,img_Width)>1.4 then // format ratio > 4/3
         rx:=round(min(img_Height,img_Width)-4*s)  // format 3/2, use full height
       else
         rx:=round(2*min(img_Height,img_Width)/3); // format 4/3 or 1/1 use 2/3 height
       ry:=rx;
     end
     else begin
       rx:=img_Width-6*s; {search area}
       ry:=img_Height-6*s;
     end;

     fits.GetStarList(rx,ry,s); {search stars in fits image}
     ns:=Length(fits.StarList);
     // store star list
     if ns>0 then begin
        // compute median HFD
        SetLength(hfdlist,ns);
        for i:=0 to ns-1 do
            hfdlist[i]:=fits.StarList[i].hfd;
        meanhfd:=SMedian(hfdlist);
        n:=0;
        SetLength(AutofocusStarList,ns);
        for i:=0 to ns-1 do begin
          // filter by hfd to remove galaxies and others outliers
          if abs(fits.StarList[i].hfd-meanhfd)<(0.5*meanhfd) then begin
            inc(n);
            AutofocusStarList[n-1,1]:=fits.StarList[i].x;
            AutofocusStarList[n-1,2]:=fits.StarList[i].y;
          end;
        end;
        SetLength(AutofocusStarList,n);
     end
     else begin  // no star, manual action is required
        SetLength(AutofocusStarList,0);
        f_starprofile.ChkAutofocusDown(false);
        NewMessage(Format(rsAutofocusCan, [crlf]),1);
        if LogToFile then begin
          buf:=slash(LogDir)+'focus_fail_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.fits';
          fits.SaveToFile(buf);
          NewMessage(Format(rsSavedFile, [buf]),2);
        end;
        exit;
     end;
   end
  end
  else begin // single star
    x:=fits.HeaderInfo.naxis1 div 2;
    y:=fits.HeaderInfo.naxis2 div 2;
    s:=2*min(fits.HeaderInfo.naxis1,fits.HeaderInfo.naxis2) div 3;
    fits.FindBrightestPixel(x,y,s,starwindow div 2,xc,yc,vmax);
    f_starprofile.FindStar:=(vmax>0);
    f_starprofile.StarX:=xc;
    f_starprofile.StarY:=yc;
    Image1.Invalidate;
    wait(1);
    if f_starprofile.FindStar then begin  // star selected OK
       // set focus frame
       s:=min(Focuswindow,min(fits.HeaderInfo.naxis1 div 2,fits.HeaderInfo.naxis2 div 2));
       s2:=s div 2;
       Fits2Screen(round(f_starprofile.StarX),round(f_starprofile.StarY),f_visu.FlipHorz,f_visu.FlipVert,x,y);
       Screen2CCD(x,y,f_visu.FlipHorz,f_visu.FlipVert,camera.VerticalFlip,xc,yc);
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
       // set star position in the frame center
       f_starprofile.StarX:=s2;
       f_starprofile.StarY:=s2;
       // reset zoom
       SaveFocusZoom:=f_visu.Zoom;
       f_visu.Zoom:=0;
       ImgZoom:=0;
    end
    else begin   // no star, manual action is required
      f_starprofile.ChkAutofocusDown(false);
      NewMessage(Format(rsAutofocusCan, [crlf]),1);
      exit;
    end;
  end;
  if CancelAutofocus then begin
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  f_starprofile.InitAutofocus(false);
  f_preview.StackPreview.Checked:=false;
  if CancelAutofocus then begin
    f_starprofile.ChkAutofocusDown(false);
    exit;
  end;
  if not f_preview.Loop then f_preview.Loop:=true;
  if not f_preview.Running then begin
     f_preview.Running:=true;
     StartPreviewExposure(self);
  end;
  if focuser.hasTemperature then NewMessage(Format(rsFocuserTempe, [FormatFloat(f1, TempDisplay(TemperatureScale,FocuserTemp))+TempLabel]),2);
  if f_starprofile.PreFocusPos>0 then
     NewMessage(Format(rsAutoFocusSta, [inttostr(f_starprofile.PreFocusPos)]),2)
  else
     NewMessage(rsAutoFocusSta2,2);
end;

Procedure Tf_main.AutoFocusStop(Sender: TObject);
begin
   if  f_capture.Running and (not autofocusing) then exit;
   f_preview.Running:=false;
   f_preview.Loop:=false;
   if (not f_capture.Running) and (not f_starprofile.AutofocusResult) then camera.AbortExposure;
   fits.SetBPM(bpm,0,0,0,0);
   fits.DarkOn:=false;
   f_preview.Binning.Text:=SaveAutofocusBinning;
   camera.SetBinning(SaveAutofocusBX,SaveAutofocusBY);
   camera.SetFrame(SaveAutofocusFX,SaveAutofocusFY,SaveAutofocusFW,SaveAutofocusFH);
   f_visu.Zoom:=SaveFocusZoom;
   f_preview.Gain:=SaveAutofocusPreviewGain;
   f_preview.Offset:=SaveAutofocusPreviewOffset;
   if camera.CanSetGain then begin
      camera.Gain:=SaveAutofocusGain;
      if hasOffset then camera.Offset:=SaveAutofocusOffset;
   end;
   ImgZoom:=f_visu.Zoom;
   f_starprofile.StarX:=-1;
   f_starprofile.StarY:=-1;
   f_starprofile.FindStar:=false;
   if f_starprofile.AutofocusResult then begin
     NewMessage(rsAutoFocusSuc,1);
   end
   else begin
     NewMessage(rsAutoFocusErr,1);
     if f_starprofile.PreFocusPos>0 then begin  // only for absolute position focuser
       NewMessage(Format(rsReturnTheFoc, [inttostr(f_starprofile.PreFocusPos)]),2);
       f_focuser.FocusPosition:=f_starprofile.PreFocusPos;
       FocusSetAbsolutePosition(nil);
       Wait(1);
     end;
   end;
   if (not f_capture.Running)and(not CancelAutofocus) then StartPreviewExposure(nil);
   f_starprofile.TimerHideGraph.Interval:=5000;
   f_starprofile.TimerHideGraph.Enabled:=true;
 end;

Procedure Tf_main.DoAutoFocus;
var c,dx,dy,vmax,dm: double;
    sx,sy,sw: integer;
begin
if (fits.HeaderInfo.valid)and(Preview or Capture) then begin // not on control exposure
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'check autofocus');{$endif}
  if f_starprofile.AutofocusRunning then
    // process autofocus
    f_starprofile.Autofocus(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow)
  else if (AutofocusMode=afPlanet) and (f_starprofile.ChkFocus.Down) then
    f_starprofile.ShowSharpness(fits)
  else if Collimation or f_starprofile.ChkFocus.Down then begin
    c:=fits.HeaderInfo.naxis1/2;
    f_starprofile.showprofile(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
    if not f_starprofile.FindStar then begin
      // try to re-acquire star in full window
      sw:=starwindow div 2;
      fits.FindBrightestPixel(round(c),round(c),round(2*c)-sw,sw,sx,sy,vmax);
      if vmax>0 then begin
        f_starprofile.showprofile(fits,sx,sy,Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
        if Collimation and (not f_starprofile.FindStar) then begin
          // for collimation, automatically increase the detection window
          dm:=(c-Starwindow)/3;
          sw:=round(Starwindow+dm);
          repeat
             f_starprofile.showprofile(fits,sx,sy,sw,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
             sw:=round(sw+dm);
          until f_starprofile.FindStar or (sw>c);
        end;
      end;
    end;
    // recenter star
    sx:=StrToIntDef(f_frame.FX.Text,-1);
    sy:=StrToIntDef(f_frame.FY.Text,-1);
    if f_starprofile.FindStar  then begin
      dx:=f_starprofile.StarX - c;
      dy:=f_starprofile.StarY - c;
      if (abs(dx)>2)or(abs(dy)>2) then begin
        sx:=sx+round(dx);
        sy:=sy-round(dy);
        f_frame.FX.Text:=IntToStr(sx);
        f_frame.FY.Text:=IntToStr(sy);
        f_starprofile.StarX:=c;
        f_starprofile.StarY:=c;
      end;
    end;
  end
  else if f_starprofile.FindStar  then
    // only refresh star profile
    f_starprofile.showprofile(fits,round(f_starprofile.StarX),round(f_starprofile.StarY),Starwindow,fits.HeaderInfo.focallen,fits.HeaderInfo.pixsz1);
end;
end;

procedure Tf_main.GUIdestroy(Sender: TObject);
begin
  GUIready:=false;
end;

procedure Tf_main.AstrometryStart(Sender: TObject);
begin
  // update Menu
  MenuResolve.Enabled:=false;
  MenuResolveSlewCenter.Enabled:=false;
  MenuResolveSlew.Enabled:=false;
  MenuResolveSync.Enabled:=false;
  MenuResolveRotate.Enabled:=false;
  MenuResolveSyncRotator.Enabled:=false;
  MenuResolveDSO.Enabled:=false;
  MenuResolvePlanetarium.Enabled:=false;
  MenuShowCCDFrame.Enabled:=false;
  MenuResolve2.Enabled:=false;
  MenuResolveSlewCenter2.Enabled:=false;
  MenuResolveSlew2.Enabled:=false;
  MenuResolveSync2.Enabled:=false;
  MenuResolveRotate2.Enabled:=false;
  MenuResolveSyncRotator2.Enabled:=false;
  MenuResolveDSO2.Enabled:=false;
  MenuResolvePlanetarium2.Enabled:=false;
  MenuShowCCDFrame2.Enabled:=false;
  {$ifdef mswindows}
  MenuViewAstrometryLog.Enabled:=false;
  {$endif}
  MenuStopAstrometry.Visible:=true;
  MenuStopAstrometry2.Visible:=true;
end;

procedure Tf_main.AstrometryEnd(Sender: TObject);
var resulttxt,buf:string;
    dist: double;
begin
  // update Menu
  MenuStopAstrometry.Visible:=false;
  MenuResolve.Enabled:=true;
  MenuResolveSlewCenter.Enabled:=true;
  MenuResolveSlew.Enabled:=true;
  MenuResolveSync.Enabled:=true;
  MenuResolveRotate.Enabled:=true;
  MenuResolveSyncRotator.Enabled:=true;
  MenuResolveDSO.Enabled:=true;
  MenuResolvePlanetarium.Enabled:=true;
  MenuShowCCDFrame.Enabled:=true;
  MenuViewAstrometryLog.Enabled:=true;
  MenuStopAstrometry2.Visible:=false;
  MenuResolve2.Enabled:=true;
  MenuResolveSlewCenter2.Enabled:=true;
  MenuResolveSlew2.Enabled:=true;
  MenuResolveSync2.Enabled:=true;
  MenuResolveRotate2.Enabled:=true;
  MenuResolveSyncRotator2.Enabled:=true;
  MenuResolveDSO2.Enabled:=true;
  MenuResolvePlanetarium2.Enabled:=true;
  MenuShowCCDFrame2.Enabled:=true;
  MenuViewAstrometryLog2.Enabled:=true;
  if astrometry.LastResult then begin
     LoadFitsFile(astrometry.ResultFile);
     resulttxt:=blank;
     resulttxt:=resulttxt+Format(rsSolvedInSeco, [inttostr(round((now-astrometry.StartTime)*secperday))]);
     if (WCScenterRA<>NullCoord) and (WCScenterDEC<>NullCoord) and
        (astrometry.InitRA<>NullCoord) and (astrometry.InitDEC<>NullCoord)
     then begin
        dist:=rad2deg*AngularDistance(deg2rad*astrometry.InitRA,deg2rad*astrometry.InitDEC,deg2rad*WCScenterRA,deg2rad*WCScenterDEC);
        resulttxt:=resulttxt+' , '+rsOffset+blank;
        if dist>1 then
          resulttxt:=resulttxt+FormatFloat(f3,dist)+sdeg
        else
          resulttxt:=resulttxt+FormatFloat(f2,60*dist)+smin;
     end;
     if (WCSwidth<>NullCoord) and (WCSheight<>NullCoord) then begin
        if WCSwidth>10 then
          resulttxt:=resulttxt+' , '+rsFOV+blank+FormatFloat(f2, WCSwidth)+'x'+FormatFloat(f2, WCSheight)+sdeg
        else
          resulttxt:=resulttxt+' , '+rsFOV+blank+FormatFloat(f2, WCSwidth*60)+'x'+FormatFloat(f2, WCSheight*60)+smin;
     end;
     if cdcWCSinfo.secpix>0 then
        resulttxt:=resulttxt+' , '+FormatFloat(f2, cdcWCSinfo.secpix)+blank+ssec+'/'+rsPixels;
     if astrometry.LastError>'' then NewMessage(astrometry.Resolver+': '+astrometry.LastError,1);
     NewMessage(Format(rsResolveSucce, [rsAstrometry])+resulttxt,3);
  end else begin
    NewMessage(Format(rsResolveError, [astrometry.Resolver])+' '+astrometry.LastError,1);
    if LogToFile then begin
       buf:=slash(LogDir)+'astrometry_fail_'+FormatDateTime('yyyymmdd_hhnnss',now)+'.fits';
       fits.SaveToFile(buf);
       NewMessage(Format(rsSavedFile, [buf]),2);
    end;
  end;
end;

procedure Tf_main.EndControlExposure(Sender: TObject);
begin
  StatusBar1.Panels[panelstatus].Text:='';
end;

procedure Tf_main.MenuStopAstrometryClick(Sender: TObject);
begin
  astrometry.StopAstrometry;
end;

procedure Tf_main.MenuTabClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0 : TBConnect.Click;
    1 : TBFocus.Click;
    2 : TBCapture.Click;
    3 : TBSequence.Click;
    4 : TBVideo.Click;
  end;
end;

procedure Tf_main.SelectTab(Sender: TObject);
var i:integer;
begin
  TToolButton(sender).Down:=true;
  i:=TToolButton(sender).tag;
  PageControlRight.ActivePageIndex:=i;
end;

procedure Tf_main.Splitter1Moved(Sender: TObject);
begin
  FormResize(Sender);
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
    f: Tf_viewtext;
begin
  logf:=slash(TmpDir)+'ccdcieltmp.log';
  if FileExistsUTF8(logf) then begin
    f:=Tf_viewtext.Create(self);
    f.Caption:=rsAstrometryRe;
    f.Memo1.Clear;
    f.Memo1.Lines.LoadFromFile(logf);
    FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
    f.Show;
  end;
end;

function Tf_main.CheckImageInfo: boolean;
var ra,dec,px,p,fl: double;
    i: integer;
begin
  if fits.HeaderInfo.valid then begin
    ra:=fits.HeaderInfo.ra;
    dec:=fits.HeaderInfo.dec;
    px:=fits.HeaderInfo.scale;
    p:=fits.HeaderInfo.pixsz1;
    fl:=fits.HeaderInfo.focallen;
    if px=0 then px:=LastPixelSize;
    if (ra=NullCoord)or(dec=NullCoord)or(px=0) then begin
      FormPos(f_goto,mouse.CursorPos.X,mouse.CursorPos.Y);
      f_goto.Caption:=rsResolve;
      f_goto.PanelAltAz.Visible:=false;
      f_goto.PanelPxSz.Visible:=true;
      f_goto.ButtonOK.Caption:=rsResolve;
      f_goto.msginfo.Caption:=rsApproximateC;
      f_goto.Obj.Text:='';
      f_goto.focallength:=fl;
      f_goto.pixelsize:=p;
      if ra<>NullCoord then f_goto.Ra.Text:=RAToStr(ra/15) else f_goto.Ra.Text:='';
      if dec<>NullCoord then f_goto.De.Text:=DEToStr(dec) else f_goto.De.Text:='';
      if px<>0 then f_goto.PxSz.Text:=FormatFloat(f2,px) else f_goto.PxSz.Text:='';
      f_goto.ActiveControl:=f_goto.Obj;
      f_goto.ShowModal;
      if f_goto.ModalResult=mrok then begin
        ra:=StrToAR(f_goto.Ra.Text);
        dec:=StrToDE(f_goto.De.Text);
        px:=StrToFloatDef(f_goto.PxSz.Text,0);
        LastPixelSize:=px;
        i:=fits.Header.Indexof('END');
        if i<7 then i:=7;  // skip mandatory keywords
        if i>=fits.Header.Rows.Count then
          i:=fits.Header.Rows.Count-1;
        if px<>0 then fits.Header.Insert(i,'SECPIX1',px,'');
        if dec<>NullCoord then fits.Header.Insert(i,'DEC',dec,'');
        if ra<>NullCoord then fits.Header.Insert(i,'RA',15*ra,'');
        result:=true;
      end
      else begin
        NewMessage(rsStopAstromet2);
        result:=false;
      end;
    end
    else
      result:=true;
  end;
end;

procedure Tf_main.MenuResolveClick(Sender: TObject);
begin
  if not CheckImageInfo then exit;
  astrometry.SolveCurrentImage(false);
end;

procedure Tf_main.ResolveSlewCenter(Sender: TObject);
var xx,yy,x,y,Timeout: integer;
    wt: boolean;
    endt: TDateTime;
begin
 if (Mount.Status<>devConnected)or(Camera.Status<>devConnected) then begin
   NewMessage(rsCameraAndMou,1);
   exit;
 end;
 wt:=(Sender<>nil);
 if fits.HeaderInfo.valid then begin
   xx:=fits.HeaderInfo.naxis1 div 2;
   yy:=fits.HeaderInfo.naxis2 div 2;
   Fits2Screen(xx,yy,f_visu.FlipHorz,f_visu.FlipVert,x,y);
   astrometry.SlewScreenXY(x,y);
   if wt then begin
     Timeout:=600;
     endt:=now+Timeout/secperday;
     while (astrometry.SlewBusy)and(now<endt) do begin
        sleep(100);
        if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
     end;
   end;
 end;
end;

procedure Tf_main.MenuResolveSlewCenterClick(Sender: TObject);
begin
 if not CheckImageInfo then exit;
 ResolveSlewCenter(nil);
end;

procedure Tf_main.MenuResolveSyncClick(Sender: TObject);
begin
  if not CheckImageInfo then exit;
  astrometry.SyncCurrentImage(false);
end;

procedure Tf_main.ResolveRotate(Sender: TObject);
var ra,de,eq,pa: double;
begin
 if (rotator.Status<>devConnected)or(Camera.Status<>devConnected) then begin
   NewMessage(rsCameraAndRot,1);
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
  if not CheckImageInfo then exit;
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
         if not CheckImageInfo then exit;
         fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
         astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@ResolveSyncRotator);
       end;
     end;
  end
  else
     NewMessage(rsRotatorIsNot,1);
 end;

end;

procedure Tf_main.MenuResolveSlewClick(Sender: TObject);
begin
  if not CheckImageInfo then exit;
  astrometry.SlewScreenXY(MouseDownX,MouseDownY);
end;

procedure Tf_main.MenuResolveDSOClick(Sender: TObject);
var
  Save_Cursor:TCursor;
begin
  if fits.HeaderInfo.valid then begin
     Save_Cursor := Screen.Cursor; {loading Hyperleda could take some time}
     Screen.Cursor := crHourglass; { Show hourglass cursor }
     if fits.HeaderInfo.solved then begin
        if sender=MenuResolveHyperLeda then
          load_hyperleda
        else
          load_deep;
        DrawImage; {cleanup to avoid label overlap}
        plot_deepsky(fits,imabmp.Canvas,Image1.Height);
        PlotImage;
     end else begin
       if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
         if not CheckImageInfo then exit;
         fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
         if sender=MenuResolveHyperLeda then
           astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryPlotHyperleda)
         else
           astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryPlotDSO);
       end;
     end;
     Screen.Cursor:=Save_Cursor;
  end;
end;

procedure Tf_main.AstrometryPlotDSO(Sender: TObject);
begin
if astrometry.LastResult then begin
  load_deep;
  DrawImage;
  plot_deepsky(fits,imabmp.Canvas,Image1.Height);
  PlotImage;
end;
end;

procedure Tf_main.AstrometryPlotHyperleda(Sender: TObject);
begin
if astrometry.LastResult then begin
  load_hyperleda;
  DrawImage;
  plot_deepsky(fits,imabmp.Canvas,Image1.Height);
  PlotImage;
end;
end;

procedure Tf_main.MenuResolvePlanetariumClick(Sender: TObject);
begin
  if fits.HeaderInfo.valid then begin
   if planetarium.Connected then begin
      if fits.HeaderInfo.solved then begin
        fits.SaveToFile(slash(TmpDir)+'ccdcielsolved.fits');
        if planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits') then
           NewMessage(rsSendImageToP,1)
        else
           NewMessage(rsPlanetariumE+blank+planetarium.LastErrorTxt,1);
      end else begin
        if (not astrometry.Busy) and (fits.HeaderInfo.naxis>0) then begin
          if not CheckImageInfo then exit;
          fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
          astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryToPlanetarium);
        end;
      end;
   end
   else
      NewMessage(rsPlanetariumI,1);
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
          if not CheckImageInfo then exit;
          fits.SaveToFile(slash(TmpDir)+'ccdcieltmp.fits');
          astrometry.StartAstrometry(slash(TmpDir)+'ccdcieltmp.fits',slash(TmpDir)+'ccdcielsolved.fits',@AstrometryToPlanetariumFrame);
        end;
      end;
   end
   else
      NewMessage(rsPlanetariumI,1);
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
if n=0 then
  n:=cdcwcs_getinfo(addr(wcsinfo),0)
else begin
  NewMessage(Format(rsErrorProcess, [TmpDir]),1);
  exit;
end;

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
if n=0 then
   n:=cdcwcs_getinfo(addr(wcsinfo),0)
else begin
  NewMessage(Format(rsErrorProcess, [TmpDir]),1);
  exit;
end;

if (n=0) and planetarium.Connected then begin

  ra:=wcsinfo.cra;
  dec:=wcsinfo.cdec;
  rot:=wcsinfo.rot;
  sizeH:=wcsinfo.secpix*wcsinfo.wp/3600;
  sizeV:=wcsinfo.secpix*wcsinfo.hp/3600;


  if((ra=NullCoord) or (dec=NullCoord) or (sizeV=0) or (sizeH=0) or (rot=NullCoord)) then
  begin
    NewMessage(rsUnableToFind,1);
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
       NewMessage(rsCCDFrameSent,1)
    else
       NewMessage(rsPlanetariumE+blank+planetarium.LastErrorTxt,1);
  end;

 end;
end;

procedure Tf_main.AstrometryToPlanetarium(Sender: TObject);
begin
if astrometry.LastResult and planetarium.Connected then begin
  if planetarium.ShowImage(slash(TmpDir)+'ccdcielsolved.fits') then
     NewMessage(rsSendImageToP,1)
  else
     NewMessage(rsPlanetariumE+blank+planetarium.LastErrorTxt,1);
end;
end;

Procedure Tf_main.PlanetariumConnectClick(Sender: TObject);
var i: integer;
begin
 if TPlanetariumType(config.GetValue('/Planetarium/Software',ord(plaNONE)))=plaNONE then exit;
 if f_planetarium.BtnConnect.Caption=rsConnect then begin
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
 f_planetarium.BtnConnect.Caption:=rsDisconnect;
 MenuPlanetariumConnect.Caption:=f_planetarium.BtnConnect.Caption;
 f_planetarium.led.Brush.Color:=clLime;
 f_planetarium.Status.Text:=Format(rsConnected2, [PlanetariumName[ord(planetarium.PlanetariumType)]]);
 NewMessage(rsPlanetarium+': '+Format(rsConnected,[PlanetariumName[ord(planetarium.PlanetariumType)]]),1);
 planetarium.InitTimer.Enabled:=true;
 StatusBar1.Invalidate;
end;

Procedure Tf_main.PlanetariumDisconnect(Sender: TObject);
var i: integer;
begin
 if not AppClose then begin
   f_planetarium.led.Brush.Color:=clGray;
   f_planetarium.Status.Text:=rsDisconnected3;
   f_planetarium.BtnConnect.Caption:=rsConnect;
   MenuPlanetariumConnect.Caption:=f_planetarium.BtnConnect.Caption;
   NewMessage(rsPlanetarium+': '+Format(rsDisconnected,[PlanetariumName[ord(planetarium.PlanetariumType)]]),1);
   wait(1);
   i:=config.GetValue('/Planetarium/Software',ord(plaNONE));
   case TPlanetariumType(i) of
     CDC: planetarium:=TPlanetarium_cdc.Create;
     SAMP:planetarium:=TPlanetarium_samp.Create;
     HNSKY:planetarium:=TPlanetarium_hnsky.Create;
     plaNONE: planetarium:=TPlanetarium_none.Create;
   end;
   planetarium.onConnect:=@PlanetariumConnect;
   planetarium.onDisconnect:=@PlanetariumDisconnect;
   planetarium.onShowMessage:=@NewMessage;
   f_planetariuminfo.planetarium:=planetarium;
   f_scriptengine.Planetarium:=planetarium;
   f_sequence.Planetarium:=planetarium;
   StatusBar1.Invalidate;
 end;
end;

Procedure Tf_main.PlanetariumNewTarget(Sender: TObject);
var ra,de,err:double;
    tra,tde,objn: string;
begin
 if planetarium.Connected and (AllDevicesConnected)and(Mount.Status=devConnected)and(Camera.Status=devConnected) then begin
    if Mount.Park then begin
      NewMessage(rsTheTelescope);
      exit;
    end;
    if f_preview.Running then begin
      if f_preview.Loop then
        f_preview.BtnLoopClick(nil)
      else
        f_preview.BtnPreviewClick(nil);
      wait(2);
    end;
    if  astrometry.Busy then begin
     NewMessage(rsResolverAlre,1);
     exit;
    end;
    if (f_capture.Running or f_sequence.Running) then begin
      NewMessage(rsCannotStartW, 1);
      exit;
    end;
    f_planetariuminfo.Ra.Text  := '-';
    f_planetariuminfo.De.Text  := '-';
    f_planetariuminfo.PA.Text  := '-';
    f_planetariuminfo.Obj.Text := '';
    f_planetariuminfo.onNewTarget := nil;
    FormPos(f_planetariuminfo,mouse.CursorPos.X,mouse.CursorPos.Y);
    f_planetariuminfo.ShowModal;
    if f_planetariuminfo.ModalResult=mrOK then begin
      CancelAutofocus:=false;
      tra:= f_planetariuminfo.Ra.Text;
      tde:=f_planetariuminfo.De.Text;
      objn:=trim(f_planetariuminfo.Obj.Text);
      NewMessage(Format(rsMoveToNewPla, [objn]),1);
       if tra='-' then
         ra:=NullCoord
       else
         ra:=StrToAR(tra);
       if tde='-' then
         de:=NullCoord
       else
         de:=StrToDE(tde);
      if (ra<>NullCoord) and (de<>NullCoord) then begin
        if MessageDlg(Format(rsPleaseConfir, [objn, tra, tde]), mtConfirmation,mbOKCancel, 0)=mrOK then begin
          J2000ToMount(mount.EquinoxJD,ra,de);
          if astrometry.PrecisionSlew(ra,de,err) then begin
            f_capture.Fname.Text:=objn;
            NewMessage(Format(rsPlanetariumT, [objn]),1);
          end
          else NewMessage(rsPlanetariumT2,1);
        end;
      end
      else NewMessage(rsInvalidCoord,1);
    end;
 end else begin
   NewMessage(rsBeforeToUseT,1);
   if not AllDevicesConnected then NewMessage(rsSomeDefinedD,1);
 end;
end;

procedure Tf_main.SaveFitsFile(fn:string);
var fext:string;
    pack: boolean;
begin
  if fits.HeaderInfo.valid then begin
     fext:=uppercase(extractfileext(fn));
     pack:= (fext='.FZ');
     if pack then fn:=copy(fn,1,length(fn)-3);
     fits.SaveToFile(fn,pack);
  end;
end;

procedure Tf_main.LoadFitsFile(fn:string);
var imgsize: string;
    n,oldw,oldh:integer;
    c: TcdcWCScoord;
    x1,y1,x2,y2,ulra,uldec: double;
begin
   oldw:=fits.HeaderInfo.naxis1;
   oldh:=fits.HeaderInfo.naxis2;
   StatusBar1.Panels[panelstatus].Text:='';
   fits.LoadFromFile(fn);
   if fits.HeaderInfo.valid then begin
     if fits.HeaderInfo.solved then begin
       try
       n:=cdcwcs_initfitsfile(pchar(fn),0);
       if n=0 then
          n:=cdcwcs_getinfo(addr(cdcWCSinfo),0)
       else begin
         NewMessage(Format(rsErrorProcess, [TmpDir]),1);
       end;
       if (n=0) and (abs(cdcWCSinfo.cdec)<89.99) then begin
         // rotation from upper left corner
         c.x:=0;
         c.y:=cdcWCSinfo.hp;
         n:=cdcwcs_xy2sky(@c,0);
         ulra:=c.ra;
         uldec:=c.dec;
         n:=cdcwcs_sky2xy(@c,0);
         x1:=c.x;
         y1:=c.y;
         c.ra:=ulra;
         c.dec:=uldec+0.01;
         n:=cdcwcs_sky2xy(@c,0);
         x2:=c.x;
         y2:=c.y;
         WCSxyNrot := arctan2((x2 - x1), (y1 - y2));
         c.ra:=ulra+0.01;
         c.dec:=uldec;
         n:=cdcwcs_sky2xy(@c,0);
         x2:=c.x;
         y2:=c.y;
         WCSxyErot := arctan2((x2 - x1), (y1 - y2));
         // center
         c.x:=0.5+cdcWCSinfo.wp/2;
         c.y:=0.5+cdcWCSinfo.hp/2;
         n:=cdcwcs_xy2sky(@c,0);
         WCScenterRA:=c.ra;
         WCScenterDEC:=c.dec;
         // FOV
         if (cdcWCSinfo.secpix>0) then begin
           WCSwidth:=cdcWCSinfo.wp*cdcWCSinfo.secpix/3600;
           WCSheight:=cdcWCSinfo.hp*cdcWCSinfo.secpix/3600;
         end else begin
           WCSwidth:=NullCoord;
           WCSheight:=NullCoord;
         end;

         // pole
         c.ra:=0;
         c.dec:=deg2rad*89.99999*Sgn(cdcWCSinfo.cdec);
         PrecessionFK5(jdtoday,jd2000,c.ra,c.dec);
         c.ra:=rad2deg*c.ra;
         c.dec:=rad2deg*c.dec;
         n:=cdcwcs_sky2xy(@c,0);
         if n=0 then begin
           WCSpoleX:=c.x;
           WCSpoleY:=cdcWCSinfo.hp-c.y;
         end
         else begin
           WCSpoleX:=NullCoord;
           WCSpoleY:=NullCoord;
         end;
       end
       else begin
         cdcWCSinfo.secpix:=0;
         WCScenterRA:=NullCoord;
         WCScenterDEC:=NullCoord;
         WCSpoleX:=NullCoord;
         WCSpoleY:=NullCoord;
         WCSwidth:=NullCoord;
         WCSheight:=NullCoord;
       end;
       except
         cdcWCSinfo.secpix:=0;
         WCScenterRA:=NullCoord;
         WCScenterDEC:=NullCoord;
         WCSpoleX:=NullCoord;
         WCSpoleY:=NullCoord;
         WCSwidth:=NullCoord;
         WCSheight:=NullCoord;
       end;
     end
       else begin
        cdcWCSinfo.secpix:=0;
        WCScenterRA:=NullCoord;
        WCScenterDEC:=NullCoord;
        WCSpoleX:=NullCoord;
        WCSpoleY:=NullCoord;
        WCSwidth:=NullCoord;
        WCSheight:=NullCoord;
       end;
     if (oldw<>fits.HeaderInfo.naxis1)or(oldh<>fits.HeaderInfo.naxis2) then begin
       ImgCx:=0;
       ImgCy:=0;
     end;
     DrawHistogram(true,true);
     DrawImage;
     imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
     NewMessage(Format(rsOpenFile, [fn]),2);
     StatusBar1.Panels[panelfile].Text:=Format(rsOpenFile, [fn])+' '+imgsize;
   end
   else begin
    NewMessage(Format(rsInvalidOrUns, [fn]),1);
   end;
end;

procedure Tf_main.LoadRawFile(fn:string);
var RawStream, FitsStream: TMemoryStream;
    imgsize, rmsg, ext: string;
begin
 {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'LoadRawFile'+blank+fn);{$endif}
 // create resources
 RawStream:=TMemoryStream.Create;
 FitsStream:=TMemoryStream.Create;
 try
   // load picture
   ext:=ExtractFileExt(fn);
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'LoadFromFile');{$endif}
   RawStream.LoadFromFile(fn);
   {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'RawToFits');{$endif}
   RawToFits(RawStream,ext,FitsStream,rmsg);
   if rmsg<>'' then NewMessage(rmsg,1);
   if FitsStream.size<2880 then
      NewMessage('Invalid file '+fn,0)
   else begin
     // assign new image
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Copy FITS stream');{$endif}
     fits.Stream:=FitsStream;
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Load FITS stream');{$endif}
     fits.LoadStream;
     // draw new image
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawHistogram');{$endif}
     DrawHistogram(true,true);
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawImage');{$endif}
     DrawImage;
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'DrawImage end');{$endif}
     imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
     NewMessage(Format(rsOpenFile, [fn]),2);
     StatusBar1.Panels[panelfile].Text:=Format(rsOpenFile, [fn])+' '+imgsize;
   end;
 finally
   // Free resources
   RawStream.Free;
   FitsStream.Free;
 end;
 {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'LoadRawFile end');{$endif}
end;

procedure Tf_main.LoadPictureFile(fn:string);
var PictStream, FitsStream: TMemoryStream;
    imgsize,ext: string;
begin
 // create resources
 PictStream:=TMemoryStream.Create;
 FitsStream:=TMemoryStream.Create;
 try
   // load picture
   ext:=copy(ExtractFileExt(fn),2,99);
   PictStream.LoadFromFile(fn);
   PictureToFits(PictStream,ext,FitsStream);
   if FitsStream.size<2880 then
      NewMessage('Invalid file '+fn,0)
   else begin
     // assign new image
     fits.Stream:=FitsStream;
     fits.LoadStream;
     // draw new image
     DrawHistogram(true,true);
     DrawImage;
     imgsize:=inttostr(fits.HeaderInfo.naxis1)+'x'+inttostr(fits.HeaderInfo.naxis2);
     NewMessage(Format(rsOpenFile, [fn]),2);
     StatusBar1.Panels[panelfile].Text:=Format(rsOpenFile, [fn])+' '+imgsize;
   end;
 finally
   // Free resources
   PictStream.Free;
   FitsStream.Free;
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

procedure Tf_main.CancelRestartExposure(delay: integer);
begin
if f_capture.Running and (not LockRestartExposure) then begin
  try
  LockRestartExposure:=true;
  NewMessage(rsCancelExposu, 3);
  camera.AbortExposureButNotSequence;
  if delay<=0 then delay:=1;
  NewMessage(format(rsWaitingDSeco, [delay]),3);
  wait(delay);
  NewMessage(rsRestartExpos, 3);
  camera.RestartExposure;
  finally
  LockRestartExposure:=false;
  end;
end;
end;

 procedure Tf_main.CCDCIELMessageHandler(var Message: TLMessage);
var buf:string;
begin
  if AppClose then exit;
  case Message.wParam of
    M_AutoguiderStatusChange: AutoguiderStatus(nil);
    M_AutoguiderMessage: if autoguider.ErrorDesc<>'' then begin
                          buf:=autoguider.ErrorDesc;
                          autoguider.ErrorDesc:='';
                          NewMessage(buf,1);
                         end;
    M_AutoguiderCancelExposure: begin
                          CancelRestartExposure(autoguider.RestartDelay);
                         end;
    M_AutoguiderGuideStat: AutoguiderGuideStat;
    M_AstrometryDone: begin
                      try
                      buf:=PChar(Message.LParam);
                      StrDispose(PChar(Message.LParam));
                      except
                      buf:='';
                      end;
                      astrometry.AstrometryDone(buf);
                      end;
    M_AstrometryMsg: begin
                      try
                      buf:=PChar(Message.LParam);
                      StrDispose(PChar(Message.LParam));
                      except
                      buf:='';
                      end;
                      if buf<>'' then NewMessage(buf,1);
                      end;
    else
      NewMessage(Format(rsReceiveUnkno, [inttostr(Message.wParam)]),1);
  end;
end;

procedure Tf_main.StatusTimerTimer(Sender: TObject);
begin
 // Periodic check
 StatusTimer.Enabled:=false;
 CheckMeridianFlip;
 StatusTimer.Enabled:=true;
 StatusBar1.Invalidate;
end;

procedure Tf_main.TimerStampTimerTimer(Sender: TObject);
begin
   StatusBar1.Panels[panelclock].Text:=TimeToStr(now);
end;

function Tf_main.CheckMeridianFlip(nextexposure:double; canwait:boolean; out waittime:integer):boolean;
var ra,de,hh,a,h,tra,tde,err: double;
    CurSt: double;
    MeridianDelay1,MeridianDelay2,NextDelay,hhmin,waittimeout,nretry: integer;
    slewtopos,slewtoimg, restartguider, SaveCapture, ok: boolean;
  procedure DoAbort;
  begin
    NewMessage(rsMeridianFlip,1);
    mount.AbortMotion;
    if f_capture.Running then CameraExposureAborted(nil);
    if autoguider.Running and (autoguider.State=GUIDER_GUIDING) then autoguider.Guide(false);
    meridianflipping:=false;
    if f_sequence.Running then f_sequence.ForceNextTarget;
  end;
begin
  waittime:=-1;
  result:=false;
  if (mount.Status=devConnected) and
  (mount.Park=false) and
  (mount.Tracking) and
  (not autofocusing) and
  (not mount.MountSlewing) and
  ((not meridianflipping)or(nextexposure<>0)) then begin
    CurST:=CurrentSidTim;
    ra:=mount.RA;
    de:=mount.Dec;
    MountToLocal(mount.EquinoxJD,ra,de);
    ra:=deg2rad*15*ra;
    hh:=CurSt-ra;
    Eq2Hz(hh,deg2rad*de,a,h) ;
    a:=rad2deg*rmod(a+pi,pi2);
    h:=rad2deg*h;
    if hh>pi then hh:=hh-pi2;
    if hh<-pi then hh:=hh+pi2;
    hhmin:=round(rad2deg*60*hh/15);
    f_mount.TimeToMeridian.Caption:=inttostr(abs(hhmin));
    f_mount.AZ.Caption:=FormatFloat(f2,a);
    f_mount.ALT.Caption:=FormatFloat(f2,h);
    if hhmin<=0 then f_mount.LabelMeridian.Caption:=rsMeridianIn
                else f_mount.LabelMeridian.Caption:=rsMeridianSinc;
    if ((f_capture.Running  or f_sequence.Busy) and (nextexposure=0)) or (f_sequence.Running and f_sequence.EditingTarget) then exit;
    if MeridianOption=0 then exit; // fork mount
    if mount.PierSide=pierEast then exit; // already on the right side
    if (MeridianOption=1) then begin
      MeridianDelay1:=MinutesPastMeridianMin-hhmin;
      if mount.PierSide=pierUnknown
        then MeridianDelay2:=MeridianDelay1
        else MeridianDelay2:=MinutesPastMeridian-hhmin;
    end
    else begin
      MeridianDelay1:=-hhmin;
      MeridianDelay2:=MeridianDelay1;
    end;
    NextDelay:=ceil(nextexposure/60);
    if MeridianDelay1>0  then begin // before meridian limit
      if MeridianDelay2>NextDelay then begin // enough time for next exposure or no capture in progress
         waittime:=-1;
         exit;
      end else if (NextDelay>0)and(MeridianDelay1>0) then begin // too short for next exposure
       result:=true;
       if canwait then begin
        if (MeridianOption=1) then begin   // if autoflip, wait
           wait(2);
           meridianflipping:=true;
           NewMessage(Format(rsWaitMeridian, [inttostr(MeridianDelay1)]),2);
           StatusBar1.Panels[panelstatus].Text := rsWaitMeridian2;
           waittime:=15+abs(round(rad2deg*3600*hh/15)); // time to wait for meridian
           exit;
        end else begin
         waittime:=-1;   // if abort, continue
         exit;
        end;
       end
       else begin
         // cannot wait now
         exit;
       end;
      end;
    end;
    if ((MeridianDelay1<=0)and(mount.PierSide=pierWest)) or (MeridianDelay1=0) then begin
     result:=true;
     if canwait then begin
      // Do meridian action
      if MeridianOption=1 then begin  // Flip
       if abs(MeridianDelay1)<150 then begin // maximum flip time + 30min
        try
        meridianflipping:=true;
        if mount.PierSide=pierUnknown then begin
          NewMessage(rsMountIsNotRe,1);
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
        // suspend dome slaving
        if MeridianFlipStopSlaving and (dome.Status=devConnected) then begin
           NewMessage(Format(rsDomeSlaving,[rsOff]),2);
           dome.Slave:=false;
        end;
        wait(2);
        // Pause before
        if MeridianFlipPauseBefore then begin
          waittimeout:=60*MeridianDelay2;
          if waittimeout<=0 then waittimeout:=30;
          f_pause.Caption:=rsPause;
          f_pause.Text:=rsMeridianFlip2+crlf+rsClickContinu2;
          if not f_pause.Wait(waittimeout) then begin
            meridianflipping:=false;
            NewMessage(rsMeridianFlip3,1);
            DoAbort;
            exit;
          end;
        end;
        // flip
        nretry:=0;
        NewMessage(rsMeridianFlip4,1);
        StatusBar1.Panels[panelstatus].Text := rsMeridianFlip5;
        mount.FlipMeridian;
        wait(2);
        while (mount.PierSide=pierWest) do begin
          // if still on wrong side wait the target move further and retry
          mount.AbortMotion;
          inc(nretry);
          if nretry>5 then begin
            NewMessage(format(rsMeridianFlip11, [5]), 1);
            DoAbort;
            exit;
          end;
          f_pause.Caption:=rsMeridianFlip6;
          f_pause.Text:=Format(rsTheMountIndi, [rsWestPointing])+'. '+Format(rsWaitDSeconds, [300]) ;
          NewMessage(f_pause.Text,1);
          if f_pause.Wait(300,true,rsRetry,rsAbort) then begin
            NewMessage(rsMeridianFlip4,1);
            StatusBar1.Panels[panelstatus].Text := rsMeridianFlip5;
            mount.FlipMeridian;
            wait(2);
          end else begin
            DoAbort;
            exit;
          end;
        end;
        if mount.PierSide=pierUnknown then begin
          NewMessage(rsWait1Minute,2);
          wait(60); // ensure we not do the flip two time
        end;
        NewMessage(rsMeridianFlip7,2);
        // resume dome slaving
        if MeridianFlipStopSlaving and (dome.Status=devConnected) then begin
           NewMessage(Format(rsDomeSlaving,[rsOn]),2);
           dome.Slave:=true;
           wait(30);
        end;
        // Pause after
        if MeridianFlipPauseAfter then begin
          SaveCapture:=Capture;
          try
          Capture:=false; // allow preview and refocusing
          waittimeout:=60*MeridianFlipPauseTimeout;
          f_pause.Caption:=rsPause;
          f_pause.Text:=rsMeridianFlip7+crlf+rsClickContinu;
          ok:=f_pause.Wait(waittimeout);
          finally
            Capture:=SaveCapture;
          end;
          if not ok then begin
            NewMessage(rsMeridianFlip9,1);
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
          NewMessage(rsRecenterOnLa,2);
          try
          Capture:=false;  // do not save the control images
          tra:=rad2deg*tra/15;
          tde:=rad2deg*tde;
          LocalToMount(mount.EquinoxJD,tra,tde);
          astrometry.PrecisionSlew(tra,tde,err);
          wait(2);
          finally
            Capture:=true;
          end;
          if (astrometry.LastResult)and(astrometry.LastSlewErr>(SlewPrecision/60)) then begin
            f_pause.Caption:=rsPause;
            f_pause.Text:=Format(rsRecenterImag, [crlf, FormatFloat(f1, astrometry.LastSlewErr)]);
            NewMessage(f_pause.Text,1);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        // precision slew with saved image
        if slewtoimg  then begin
          NewMessage(rsRecenterOnLa2,2);
          try
          Capture:=false;  // do not save the control images
          LoadFitsFile(slash(TmpDir)+'meridianflip.fits');
          DeleteFileUTF8(slash(TmpDir)+'meridianflip.fits');
          ResolveSlewCenter(Self);
          wait(2);
          finally
            Capture:=true;
          end;
          if (astrometry.LastResult)and(astrometry.LastSlewErr>(SlewPrecision/60)) then begin
            f_pause.Caption:=rsPause;
            f_pause.Text:=Format(rsRecenterImag, [crlf, FormatFloat(f1,astrometry.LastSlewErr)]);
            NewMessage(f_pause.Text,1);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        // start autoguider
        if restartguider then begin
          NewMessage(rsRestartAutog,2);
          autoguider.Guide(false);
          wait(5);
          if MeridianFlipCalibrate then
            autoguider.Calibrate
          else
            autoguider.Guide(true);
          autoguider.WaitGuiding(CalibrationDelay+SettleMaxTime);
          if autoguider.State<>GUIDER_GUIDING then begin
            f_pause.Caption:=rsPause;
            f_pause.Text:=rsFailedToStar;
            NewMessage(f_pause.Text,1);
            if not f_pause.Wait(120) then begin
               DoAbort;
               exit;
            end;
          end;
        end;
        Wait(2);
        f_capture.DitherNum:=0; // no dither after flip
        NewMessage(rsMeridianFlip10,1);
        StatusBar1.Panels[panelstatus].Text := '';
        finally
          meridianflipping:=false;
        end;
      end else begin  // Abort
        NewMessage('Unexpected time past meridian '+inttostr(hhmin),0);
        DoAbort;
      end;
      end else begin  // Abort
        DoAbort;
      end;
    end
    else begin
      // cannot wait now
      exit;
    end;
  end;
  end;
end;

procedure Tf_main.CheckMeridianFlip;
var t:integer;
begin
 CheckMeridianFlip(0,true,t);
end;

procedure Tf_main.MeasureImage(plot: boolean); {measure the median HFD of the image and mark stars with a square proportional to HFD value}
var
 i,rx,ry,s,nhfd,nhfd_center,nhfd_outer_ring,nhfd_top_left,nhfd_top_right,nhfd_bottom_left,nhfd_bottom_right : integer;
 hfd1,xc,yc, median_worst,median_best,scale_factor,med, median_outer_ring : double;
 hfdlist,hfdlist_top_left,hfdlist_top_right,hfdlist_bottom_left,hfdlist_bottom_right, hfdlist_center,hfdlist_outer_ring :array of double;
 Saved_Cursor : TCursor;
 mess1,mess2 : string;
begin

  if not fits.HeaderInfo.valid then exit;

  Saved_Cursor := Screen.Cursor;
  Screen.Cursor := crHourglass; { Show hourglass cursor since analysing will take some time}

  if plot then DrawImage; {draw clean image}

  // first measurement with a big window to find median star diameter
  s:=starwindow; {use configured star window}
  rx:=img_Width-6*s; {search area}
  ry:=img_Height-6*s;
  fits.GetStarList(rx,ry,s); {search stars in fits image}
  nhfd:=Length(fits.StarList);
  if nhfd>0 then begin
    SetLength(hfdlist,nhfd);
    for i:=0 to nhfd-1 do
      hfdlist[i]:=fits.StarList[i].hfd;
    med:=SMedian(hfdlist);            {median of starshfd}
    s:=min(max(14,round(3.0*med)),s); {reasonable window to measure this star}
  end
  else
    s:=20; {no star found, try with small default window}

  // new measurement with adjusted window
  rx:=img_Width-6*s; {search area}
  ry:=img_Height-6*s;

  fits.GetStarList(rx,ry,s); {search stars in fits image}

//  Uncomment to help star detection debugging
//  PrintStarList;

  nhfd:=Length(fits.StarList);{number of stars detected for HFD statistics}
  SetLength(hfdlist,nhfd);

  nhfd_center:=0;
  nhfd_outer_ring:=0;

  nhfd_top_left:=0;{set counters at zero}
  nhfd_top_right:=0;
  nhfd_bottom_left:=0;
  nhfd_bottom_right:=0;

  SetLength(hfdlist_center,nhfd);{set array length to maximum number of stars available}
  SetLength(hfdlist_outer_ring,nhfd);

  SetLength(hfdlist_top_left,nhfd);
  SetLength(hfdlist_top_right,nhfd);
  SetLength(hfdlist_bottom_left,nhfd);
  SetLength(hfdlist_bottom_right,nhfd);

  for i:=0 to nhfd-1 do
  begin
     hfd1:=fits.StarList[i].hfd;
     hfdlist[i]:=hfd1;
     xc:=fits.StarList[i].x;
     yc:=fits.StarList[i].y;

     if  sqr(xc - (img_width div 2) )+sqr(yc - (img_height div 2))<sqr(0.25)*(sqr(img_width div 2)+sqr(img_height div 2)) then begin hfdlist_center[nhfd_center]:=hfd1; inc(nhfd_center); end {store center(<25% diameter) HFD values}
     else
     begin
       if  sqr(xc - (img_width div 2) )+sqr(yc - (img_height div 2))>sqr(0.75)*(sqr(img_width div 2)+sqr(img_height div 2)) then begin hfdlist_outer_ring[nhfd_outer_ring]:=hfd1; inc(nhfd_outer_ring);  end;{store out ring (>75% diameter) HFD values}

       if ( (xc<(img_width div 2)) and (yc<(img_height div 2)) ) then begin hfdlist_bottom_left[nhfd_bottom_left]:=hfd1;   inc(nhfd_bottom_left); end;{store corner HFD values}
       if ( (xc>(img_width div 2)) and (yc<(img_height div 2)) ) then begin hfdlist_bottom_right[nhfd_bottom_right]:=hfd1; inc(nhfd_bottom_right);end;
       if ( (xc<(img_width div 2)) and (yc>(img_height div 2)) ) then begin hfdlist_top_left[nhfd_top_left]:=hfd1;         inc(nhfd_top_left);    end;
       if ( (xc>(img_width div 2)) and (yc>(img_height div 2)) ) then begin hfdlist_top_right[nhfd_top_right]:=hfd1;       inc(nhfd_top_right);   end;
     end;
  end;
  SetLength(hfdlist_center,nhfd_center);
  SetLength(hfdlist_outer_ring,nhfd_outer_ring);
  SetLength(hfdlist_bottom_left,nhfd_bottom_left);
  SetLength(hfdlist_bottom_right,nhfd_bottom_right);
  SetLength(hfdlist_top_left,nhfd_top_left);
  SetLength(hfdlist_top_right,nhfd_top_right);

  if nhfd>0 then
  begin
    if ((nhfd_center>2) and (nhfd_outer_ring>2)) then  {enough information for curvature calculation}
    begin
      median_center:=SMedian(hfdlist_center);
      median_outer_ring:=SMedian(hfdlist_outer_ring);
      mess1:='  '+Format(rsOffAxisAberr, [floattostrF(median_outer_ring-median_center, ffgeneral, 3, 2)]); {off-axis aberration measured in delta HFD. Works also for defocussed images}
    end
    else
    mess1:='';


    if ((nhfd_top_left>2) and (nhfd_top_right>2) and (nhfd_bottom_left>2) and (nhfd_bottom_right>2)) then {enough information for tilt calculation}
    begin
      median_top_left:=SMedian(hfdList_top_left);
      median_top_right:=SMedian(hfdList_top_right);
      median_bottom_left:=SMedian(hfdList_bottom_left);
      median_bottom_right:=SMedian(hfdList_bottom_right);

      median_best:=min(min(median_top_left, median_top_right),min(median_bottom_left,median_bottom_right));{find best corner}
      median_worst:=max(max(median_top_left, median_top_right),max(median_bottom_left,median_bottom_right));{find worst corner}

      scale_factor:=img_height*0.33/median_worst;
      trpx1:=round(-median_bottom_left*scale_factor+img_width/2);trpy1:=round(-median_bottom_left*scale_factor+img_height/2);{calculate coordinates counter clockwise}
      trpx2:=round(+median_bottom_right*scale_factor+img_width/2);trpy2:=round(-median_bottom_right*scale_factor+img_height/2);
      trpx3:=round(+median_top_right*scale_factor+img_width/2);trpy3:=round(+median_top_right*scale_factor+img_height/2);
      trpx4:=round(-median_top_left*scale_factor+img_width/2);trpy4:=round(+median_top_left*scale_factor+img_height/2);
      trpOK:=true;

      mess2:='  '+Format(rsTiltIndicati2, [floattostrF(median_worst-median_best,ffgeneral,3,2)]); {estimate tilt value in delta HFD}
    end
    else begin
      trpOK:=false;
      mess2:='';
    end;


    NewMessage(Format(rsFoundDStars, [nhfd])+': '+Format(rsImageMedianH, [formatfloat(f1, SMedian(hfdList))+ mess2+mess1]), 1); {Report median HFD, tilt and off-axis aberration (was curvature}
  end
  else
    NewMessage(rsNoStarDetect,1);

  SetLength(hfdlist,0);{release memory. Could also be done with hfdlist:=nil}

  SetLength(hfdlist_center,0);
  SetLength(hfdlist_outer_ring,0);

  SetLength(hfdlist_top_left,0);
  SetLength(hfdlist_top_right,0);
  SetLength(hfdlist_bottom_left,0);
  SetLength(hfdlist_bottom_right,0);

  if plot then
    PlotImage
  else
    fits.ClearStarList;

  Screen.Cursor := saved_cursor;
end;

procedure Tf_main.PrintStarList;
var f: Tf_viewtext;
    i: integer;
begin
 if Length(fits.StarList)>0 then begin
   f:=Tf_viewtext.Create(self);
   f.Width:=DoScaleX(700);
   f.Height:=DoScaleY(400);
   f.Caption:='Star list';
   f.Memo1.Clear;
   f.Memo1.Lines.Add(Format('%5s %10s %10s %10s %10s %6s %6s %10s ',['num','x','y','hfd','fwhm','max','bg','snr']));
   for i:=0 to Length(fits.StarList)-1 do
      f.Memo1.Lines.Add(Format('%5d %10.3f %10.3f %10.3f %10.3f %6d %6d %10.3f ',
         [i+1,fits.StarList[i].x,img_Height-fits.StarList[i].y,fits.StarList[i].hfd,fits.StarList[i].fwhm,
         round(fits.StarList[i].vmax+fits.StarList[i].bg),round(fits.StarList[i].bg),fits.StarList[i].snr]));
   FormPos(f,mouse.CursorPos.X,mouse.CursorPos.Y);
   f.Show;
 end;
end;

procedure Tf_main.MagnifyerTimerTimer(Sender: TObject);
begin
  MagnifyerTimer.Enabled:=false;
  UpdateMagnifyer(Mx,My);
end;

procedure Tf_main.MeasureTimerTimer(Sender: TObject);
begin
  MeasureTimer.Enabled:=false;
  MeasureAtPos(Mx,My,false);
end;

Procedure Tf_main.UpdateMagnifyer(x,y:integer);
var xx,yy,px,py: integer;
    z: double;
    tmpbmp,str: TBGRABitmap;
begin
if (f_magnifyer.isVisible)and(fits.HeaderInfo.naxis1>0)and(ImgScale0<>0)and(x>0)and(y>0) then begin
 Screen2fits(x,y,f_visu.FlipHorz,f_visu.FlipVert,xx,yy);
 z:=max(2,3*ImgZoom);
 tmpbmp:=TBGRABitmap.Create(round(f_magnifyer.Image1.Width/z),round(f_magnifyer.Image1.Height/z),clDarkBlue);
 try
   px:=tmpbmp.Width div 2 - xx;
   py:=tmpbmp.Height div 2 - yy;
   px:=min(0,max(px,tmpbmp.Width-fits.HeaderInfo.naxis1));
   py:=min(0,max(py,tmpbmp.Height-fits.HeaderInfo.naxis2));
   tmpbmp.PutImage(px,py,ImaBmp,dmSet);
   str:=tmpbmp.Resample(f_magnifyer.Image1.Width,f_magnifyer.Image1.Height,rmSimpleStretch) as TBGRABitmap;
   if f_visu.FlipHorz then Str.HorizontalFlip;
   if f_visu.FlipVert then Str.VerticalFlip;
   try
     f_magnifyer.Image1.Picture.Assign(str);
   finally
     str.Free;
   end;
 finally
   tmpbmp.Free;
 end;
 f_magnifyer.Image1.Invalidate;
end;
end;

procedure Tf_main.MeasureAtPos(x,y:integer; photometry:boolean);
var xx,yy,n: integer;
    val,xxc,yyc,rc,s:integer;
    sval:string;
    ra,de: double;
    c: TcdcWCScoord;
    bg,bgdev,xc,yc,hfd,fwhm,vmax,dval,snr,flux,mag,magerr: double;
begin
 Screen2fits(x,y,f_visu.FlipHorz,f_visu.FlipVert,xx,yy);
 if (xx>0)and(xx<fits.HeaderInfo.naxis1)and(yy>0)and(yy<fits.HeaderInfo.naxis2) then
    if fits.preview_axis=1 then begin
      if fits.HeaderInfo.bitpix>0 then begin
        val:=trunc(fits.image[0,yy,xx]);
        sval:=inttostr(val);
      end
      else begin
       dval:=fits.imageMin+fits.image[0,yy,xx]/fits.imageC;
       sval:=FormatFloat(f3,dval);
      end;
    end
    else if (fits.preview_axis=3) then begin
      if fits.HeaderInfo.bitpix>0 then begin
        val:=trunc(fits.imageMin+fits.image[0,yy,xx]/fits.imageC);
        sval:=inttostr(val);
        val:=trunc(fits.imageMin+fits.image[1,yy,xx]/fits.imageC);
        sval:=sval+'/'+inttostr(val);
        val:=trunc(fits.imageMin+fits.image[2,yy,xx]/fits.imageC);
        sval:=sval+'/'+inttostr(val);
      end
      else begin
       dval:=fits.imageMin+fits.image[0,yy,xx]/fits.imageC;
       sval:=FormatFloat(f3,dval);
       dval:=fits.imageMin+fits.image[1,yy,xx]/fits.imageC;
       sval:=sval+'/'+FormatFloat(f3,dval);
       dval:=fits.imageMin+fits.image[2,yy,xx]/fits.imageC;
       sval:=sval+'/'+FormatFloat(f3,dval);
      end;
    end
 else sval:='';
 s:=Starwindow div 2;
 if (xx>s)and(xx<(fits.HeaderInfo.naxis1-s))and(yy>s)and(yy<(fits.HeaderInfo.naxis2-s)) then begin
   fits.FindStarPos(xx,yy,s,xxc,yyc,rc,vmax,bg,bgdev);
   if vmax>0 then begin
     fits.GetHFD2(xxc,yyc,2*rc,xc,yc,bg,bgdev,hfd,fwhm,vmax,snr,flux);
     if (hfd>0)and(Undersampled or (hfd>0.7)) then begin
       sval:=sval+' HFD='+FormatFloat(f1,hfd)+' FWHM='+FormatFloat(f1,fwhm);
       if flux>0 then begin
         sval:=sval+' '+rsFlux+'='+FormatFloat(f0, flux)+' SNR='+FormatFloat(f1, snr);
         if photometry and (f_photometry<>nil) and f_photometry.Visible then begin
           f_photometry.StarX:=xc;
           f_photometry.StarY:=yc;
           f_photometry.hfd:=hfd;
           f_photometry.Memo1.Clear;
           if (fits.HeaderInfo.exptime<>0) and (fits.HeaderInfo.airmass<>0) then begin
             if MagnitudeCalibration<>NullCoord then begin
               f_photometry.Memo1.Lines.Add(rsSimplifiedPh);
               f_photometry.Memo1.Lines.Add(rsExposureTime2+' : '+FormatFloat(f3,fits.HeaderInfo.exptime)+blank+rsSeconds);
               f_photometry.Memo1.Lines.Add(rsAirmass+' : '+FormatFloat(f4, fits.HeaderInfo.airmass));
               mag:=MagnitudeCalibration-2.5*log10(flux/fits.HeaderInfo.exptime)-atmospheric_absorption(fits.HeaderInfo.airmass);
             end
             else begin
               f_photometry.Memo1.Lines.Add(rsSimplifiedPh2);
               mag:=-2.5*log10(flux/fits.HeaderInfo.exptime)-atmospheric_absorption(fits.HeaderInfo.airmass);
             end;
           end
           else begin
             if MagnitudeCalibration<>NullCoord then begin
               f_photometry.Memo1.Lines.Add(rsSimplifiedPh3);
               mag:=MagnitudeCalibration-2.5*log10(flux);
             end
             else begin
               f_photometry.Memo1.Lines.Add(rsSimplifiedPh2);
               mag:=-2.5*log10(flux);
             end;
           end;
           f_photometry.mag:=mag;
           magerr:=2.5*log10(1+1/snr);
           f_photometry.Memo1.Lines.Add('');
           f_photometry.Memo1.Lines.Add(rsStar+' X/Y'+' : '+FormatFloat(f3, xc)+' / '+FormatFloat(f3, img_Height-yc));
           if fits.HeaderInfo.floatingpoint then
             f_photometry.Memo1.Lines.Add(rsMaximumInten+' : '+FormatFloat(f3, (vmax+bg)))
           else
             f_photometry.Memo1.Lines.Add(rsMaximumInten+' : '+FormatFloat(f0, (vmax+bg)));
           f_photometry.Memo1.Lines.Add(rsBackground+' : '+FormatFloat(f3, bg)+', '+rsStdDev+blank+FormatFloat(f3, bgdev));
           if fits.HeaderInfo.floatingpoint then
             f_photometry.Memo1.Lines.Add(rsFlux+' : '+FormatFloat(f3, flux))
           else
             f_photometry.Memo1.Lines.Add(rsFlux+' : '+FormatFloat(f0, flux));
           f_photometry.Memo1.Lines.Add('SNR'+' : '+FormatFloat(f1,snr)+', '+'+/- '+FormatFloat(f3,magerr)+blank+LowerCase(rsMagnitude));
           f_photometry.Memo1.Lines.Add(rsMagnitude+' : '+FormatFloat(f3, mag));
         end;
       end
       else begin
         sval:=sval+blank+rsSaturated;
         if photometry and (f_photometry<>nil) and f_photometry.Visible then begin
           f_photometry.hfd:=-1;
           f_photometry.Memo1.Clear;
           f_photometry.Memo1.Lines.Add(rsSaturated);
           f_photometry.mag:=NullCoord;
         end;
       end;
     end
     else begin
       if photometry and (f_photometry<>nil) and f_photometry.Visible then begin
         f_photometry.hfd:=-1;
         f_photometry.Memo1.Clear;
         f_photometry.Memo1.Lines.Add(rsNoStarFound);
         f_photometry.mag:=NullCoord;
       end;
     end;
   end
   else begin
     if photometry and (f_photometry<>nil) and f_photometry.Visible then begin
       f_photometry.hfd:=-1;
       f_photometry.Memo1.Clear;
       f_photometry.Memo1.Lines.Add(rsNoStarFound);
       f_photometry.mag:=NullCoord;
     end;
   end;
 end
 else begin
   if photometry and (f_photometry<>nil) and f_photometry.Visible then begin
     f_photometry.hfd:=-1;
     f_photometry.Memo1.Clear;
     f_photometry.Memo1.Lines.Add(rsNoStarFound);
     f_photometry.mag:=NullCoord;
   end;
 end;
 if fits.HeaderInfo.solved and (cdcWCSinfo.secpix<>0) then begin
   c.x:=xx;
   c.y:=cdcWCSinfo.hp-yy;
   n:=cdcwcs_xy2sky(@c,0);
   if n=0 then begin
     ra:=c.ra/15;
     de:=c.dec;
     J2000ToMount(mount.EquinoxJD,ra,de);
     StatusBar1.Panels[panelstatus].Text:=ARToStr3(ra)+' '+DEToStr(de);
   end;
 end;
 yy:=img_Height-yy;
 StatusBar1.Panels[panelcursor].Text:=inttostr(xx)+'/'+inttostr(yy)+': '+sval;
end;

procedure Tf_main.StartServer;
begin
  try
    TCPDaemon := TTCPDaemon.Create;
    TCPDaemon.onErrorMsg := @TCPShowError;
    TCPDaemon.onShowSocket := @TCPShowSocket;
    TCPDaemon.onExecuteCmd:=@TCPcmd;
    TCPDaemon.onExecuteJSON:=@TCPjsoncmd;
    TCPDaemon.onGetImage:=@TCPgetimage;
    TCPDaemon.IPaddr := '0.0.0.0';
    TCPDaemon.IPport := TCPIPConfigPort;
    TCPIPServerPort := TCPDaemon.IPport;
    TCPDaemon.Start;
  except

  end;
end;

procedure Tf_main.StopServer;
var
  i: integer;
begin
  if (TCPDaemon = nil)or TCPDaemon.Finished then
    exit;
  try
    screen.cursor := crHourglass;
    NewMessage(rsTCPIPServerS,1);
    for i := 1 to Maxclient do
      if (TCPDaemon.TCPThrd[i] <> nil) and (TCPDaemon.TCPThrd[i].sock <> nil) and (not TCPDaemon.TCPThrd[i].terminated) then
      begin
        TCPDaemon.TCPThrd[i].stoping := True;
      end;
    TCPDaemon.stoping := True;
    Wait(1);
    screen.cursor := crDefault;
  except
    screen.cursor := crDefault;
  end;
end;

procedure Tf_main.RestartServer;
begin
  StopServer;
  wait(2);
  StartServer;
end;

procedure Tf_main.TCPShowError(var msg: string);
begin
  NewMessage(Format(rsSocketErrorS, [msg, '']),1);
end;

procedure Tf_main.TCPShowSocket(var msg: string);
begin
  TCPIPServerPort:=trim(msg);
  NewMessage(Format(rsTCPIPServerL, [msg]),1);
end;

procedure Tf_main.TCPgetimage(n: string;  var img: Tmemorystream);
var jpg: TJPEGImage;
begin
  img.clear;
  try
  jpg:= TJPEGImage.Create;
  if n='scrimage' then
    jpg.Assign(ScrBmp)
  else if n='fullimage' then
      jpg.Assign(ImaBmp)
  else if n='lastfocus' then
      jpg.Assign(f_starprofile.LastFocusimage)
  else begin
      jpg.SetSize(Image1.Width,Image1.Height);
      PanelCenter.PaintTo(jpg.Canvas,0,0);
  end;
  jpg.SaveToStream(img);
  jpg.free;
  except
    img.clear;
  end;
end;

function Tf_main.jsoncmd_status(attrib,value:Tstringlist):string;
var frx,fry,frw,frh,i,j,n: integer;
    resp:string;
    noparams:boolean;
  function CleanText(str:string):string;
  begin
   result:=StringReplace(str,'"','''',[rfReplaceAll]);
   result:=StringReplace(result,';','.',[rfReplaceAll]);
   result:=StringReplace(result,CRLF,'; ',[rfReplaceAll]);
   result:=StringReplace(result,CR,' ',[rfReplaceAll]);
   result:=StringReplace(result,LF,' ',[rfReplaceAll]);
  end;
begin
  result:='';
  resp:='';
  noparams:=attrib.IndexOf('params.0')<0;

  if noparams or (value.IndexOf('devices')>0) then
    resp:=resp+'"devices": {"connected": '+BoolToStr(AllDevicesConnected,'true','false')+'}, ';
  if noparams or (value.IndexOf('planetarium')>0) then begin
    resp:=resp+'"planetarium": {"connected": '+BoolToStr((f_planetarium<>nil)and(not planetarium.Terminated)and(planetarium.Connected),'true','false')+'}, ';
  end;
  if noparams or (value.IndexOf('autoguider')>0) then begin
    resp:=resp+'"autoguider": {"connected": '+BoolToStr((f_autoguider<>nil)and(autoguider.State<>GUIDER_DISCONNECTED),'true','false');
    if (f_autoguider<>nil)and(autoguider.State<>GUIDER_DISCONNECTED) then
       resp:=resp+', "guiding": '+BoolToStr((autoguider.State=GUIDER_GUIDING),'true','false')+
                  ', "alert": '+BoolToStr((autoguider.State=GUIDER_ALERT),'true','false');
    resp:=resp+'}, ';
  end;
  if (noparams and WantSafety) or (value.IndexOf('safety')>0) then begin
    resp:=resp+'"safety": {"connected": '+BoolToStr((safety.Status=devConnected),'true','false');
    if safety.Status=devConnected then
       resp:=resp+', "safe": '+BoolToStr((f_safety.Safe),'true','false');
    resp:=resp+'}, ';
  end;
  if (noparams and WantWeather) or (value.IndexOf('weather')>0) then begin
    resp:=resp+'"weather": {"connected": '+BoolToStr((weather.Status=devConnected),'true','false');
    if weather.Status=devConnected then begin
      resp:=resp+', "clear": '+BoolToStr((f_weather.Clear),'true','false');
      if (not f_weather.Clear) then
         resp:=resp+', "weathermessage": "'+CleanText(weather.WeatherMessage)+'"';
    end;
    resp:=resp+'}, ';
  end;
  if (noparams and WantDome) or (value.IndexOf('dome')>0) then begin
    resp:=resp+'"dome": {"connected": '+BoolToStr((dome.Status=devConnected),'true','false');
    if dome.Status=devConnected then
      resp:=resp+', "shutter": '+BoolToStr((f_dome.Shutter),'true','false') +
                 ', "slaving": '+BoolToStr((f_dome.CanSlave)and(f_dome.Slave),'true','false');
    resp:=resp+'}, ';
  end;
  if (noparams and WantMount) or (value.IndexOf('mount')>0) then begin
    resp:=resp+'"mount": {"connected": '+BoolToStr((mount.Status=devConnected),'true','false');
    if mount.Status=devConnected then begin
      resp:=resp+', "park": '+BoolToStr((mount.Park),'true','false');
      if not mount.Park then begin
        resp:=resp+', "ra": "'+CleanText(trim(f_mount.RA.Caption))+'"';
        resp:=resp+', "dec": "'+CleanText(trim(f_mount.DE.Caption))+'"';
        resp:=resp+', "pierside": "'+CleanText(trim(f_mount.Pierside.Caption))+'"';
        resp:=resp+', "timetomeridian": "'+CleanText(trim(f_mount.LabelMeridian.Caption)+' '+trim(f_mount.TimeToMeridian.Caption)+' '+trim(f_mount.label4.Caption))+'"';
      end;
    end;
    resp:=resp+'}, ';
  end;
  if (noparams and WantCamera) or (value.IndexOf('camera')>0) then begin
    resp:=resp+'"camera": {"connected": '+BoolToStr(camera.Status=devConnected,'true','false');
    if camera.Status=devConnected then begin
      resp:=resp+', "binning": "'+inttostr(camera.BinX)+'x'+inttostr(camera.BinY)+'"';
      camera.GetFrame(frx,fry,frw,frh);
      resp:=resp+', "frame": "'+inttostr(frx)+'/'+inttostr(fry)+'/'+inttostr(frw)+'/'+inttostr(frh)+'"';
      resp:=resp+', "cooler": '+BoolToStr(f_ccdtemp.CCDcooler.Checked,'true','false');
      resp:=resp+', "temperature": '+f_ccdtemp.Current.Caption;
    end;
    resp:=resp+'}, ';
  end;
  if (noparams and WantWheel) or (value.IndexOf('wheel')>0) then begin
    resp:=resp+'"wheel": {"connected": '+BoolToStr(wheel.Status=devConnected,'true','false');
    if wheel.Status=devConnected then begin
       resp:=resp+', "filter": "'+CleanText(f_filterwheel.Filters.Text)+'"';
    end;
    resp:=resp+'}, ';
  end;
  if (noparams and WantRotator) or (value.IndexOf('rotator')>0) then begin
    resp:=resp+'"rotator": {"connected": '+BoolToStr(rotator.Status=devConnected,'true','false');
    if rotator.Status=devConnected then begin
       resp:=resp+', "position": '+CleanText(f_rotator.Angle.Text);
    end;
    resp:=resp+'}, ';
  end;
  if (noparams and WantFocuser) or (value.IndexOf('focuser')>0) then begin
    resp:=resp+'"focuser": {"connected": '+BoolToStr(focuser.Status=devConnected,'true','false');
    if focuser.Status=devConnected then begin
       if focuser.hasAbsolutePosition then resp:=resp+', "position": '+f_focuser.Position.Text;
       if focuser.hasTemperature then resp:=resp+', "temperature": '+f_focuser.Temp.Text;
       resp:=resp+', "focusermessage": "'+CleanText(f_starprofile.LastFocusMsg)+'"';
    end;
    resp:=resp+'}, ';
  end;
  if noparams or (value.IndexOf('sequence')>0) then begin
    resp:=resp+'"sequence": {"running": '+BoolToStr(f_sequence.Running,'true','false');
    if f_sequence.Running then begin
      resp:=resp+', "name": "'+CleanText(trim(CurrentSeqName))+'"';
      resp:=resp+', "target": "'+CleanText(trim(CurrentTargetName))+'"';
      resp:=resp+', "step": "'+CleanText(trim(CurrentStepName))+'"';
      resp:=resp+', "status": "'+CleanText(trim(f_sequence.StatusMsg.Caption))+'"';
      resp:=resp+', "delay": "'+CleanText(trim(f_sequence.DelayMsg.Caption))+'"';
      resp:=resp+', "sequencecompletion": '+FormatFloat(f0,f_sequence.PercentComplete*100);
      resp:=resp+', "targetcompletion": '+FormatFloat(f0,f_sequence.TargetPercentComplete*100);
    end;
    resp:=resp+'}, ';
  end;
  if noparams or (value.IndexOf('sequence')>0) or (value.IndexOf('capture')>0) then begin
    resp:=resp+'"capture": {"running": '+BoolToStr(f_capture.Running,'true','false');
    if f_capture.Running then begin
       resp:=resp+', "total": '+f_capture.SeqNum.Text;
       resp:=resp+', "current": '+inttostr(f_capture.SeqCount);
       resp:=resp+', "exposure": '+formatfloat(f3,f_capture.ExposureTime);
       resp:=resp+', "exposureremain": '+formatfloat(f3,CameraExposureRemain);
    end;
    resp:=resp+'}, ';
  end;
  if noparams or (value.IndexOf('log')>0) then begin
    n:=f_msg.msg.Lines.Count-1;
    if n<30 then j:=0
            else j:=n-30;
    resp:=resp+'"log": [';
    for i:=j to n do
      resp:=resp+'"'+CleanText(f_msg.msg.Lines[i])+'",';
    delete(resp,length(resp),1);
    resp:=resp+'], ';
  end;
  if resp>'' then begin
    delete(resp,length(resp)-1,2);
    result:=result+'"result": {'+resp+'}'
  end
  else
    result:=result+'"error": {"code": -32602, "message": "Invalid params"}';
end;

function Tf_main.TCPjsoncmd(id:string; attrib,value:Tstringlist):string;
var p: integer;
    rpcversion,method,buf,buf1,buf2:string;
const tr='true';
      fa='false';
begin
try
  p:=attrib.IndexOf('jsonrpc');
  if p>=0 then
    rpcversion:=value[p]
  else
    rpcversion:='2.0';
  if rpcversion<>'2.0' then begin
    result:='{"jsonrpc": "2.0", "error": {"code": -32600, "message": "JSON-RPC version not supported"}, "id": '+id+'}';
    exit;
  end;
  result:='{"jsonrpc": "2.0", ';
  p:=attrib.IndexOf('method');
  if p>=0 then
    method:=uppercase(value[p])
  else
    method:='';

  if method='STATUS' then begin
    result:=result+jsoncmd_status(attrib,value);
  end
  // return variable value
  else if method='DEVICES_CONNECTED' then result:=result+'"result": '+BoolToStr(AllDevicesConnected,tr,fa)
  else if method='TELESCOPE_CONNECTED' then result:=result+'"result": '+BoolToStr(mount.Status=devConnected,tr,fa)
  else if method='TELESCOPE_PARKED' then result:=result+'"result": '+BoolToStr(mount.Park,tr,fa)
  else if method='TELESCOPE_TRACKING' then result:=result+'"result": '+BoolToStr(mount.Tracking,tr,fa)
  else if method='TELESCOPE_SLEWING' then result:=result+'"result": '+BoolToStr(mount.MountSlewing,tr,fa)
  else if method='TELESCOPE_EQMOD' then result:=result+'"result": '+BoolToStr(mount.IsEqmod,tr,fa)
  else if method='AUTOGUIDER_CONNECTED' then result:=result+'"result": '+BoolToStr((Autoguider.State<>GUIDER_DISCONNECTED),tr,fa)
  else if method='AUTOGUIDER_RUNNING' then result:=result+'"result": '+BoolToStr(Autoguider.Running,tr,fa)
  else if method='AUTOGUIDER_GUIDING' then result:=result+'"result": '+BoolToStr((Autoguider.State=GUIDER_GUIDING),tr,fa)
  else if method='WHEEL_CONNECTED' then result:=result+'"result": '+BoolToStr((wheel.Status=devConnected),tr,fa)
  else if method='FOCUSER_CONNECTED' then result:=result+'"result": '+BoolToStr((Focuser.Status=devConnected),tr,fa)
  else if method='CAMERA_CONNECTED' then result:=result+'"result": '+BoolToStr((Camera.Status=devConnected),tr,fa)
  else if method='PLANETARIUM_CONNECTED' then result:=result+'"result": '+BoolToStr(Planetarium.Connected,tr,fa)
  else if method='PREVIEW_RUNNING' then result:=result+'"result": '+BoolToStr(f_Preview.Running,tr,fa)
  else if method='PREVIEW_LOOP_RUNNING' then result:=result+'"result": '+BoolToStr(f_Preview.Loop,tr,fa)
  else if method='CAPTURE_RUNNING' then result:=result+'"result": '+BoolToStr(f_Capture.Running,tr,fa)
  else if method='TELESCOPERA' then result:=result+'"result": '+FormatFloat(f6,mount.RA)
  else if method='TELESCOPEDE' then result:=result+'"result": '+FormatFloat(f6,mount.Dec)
  else if method='CCDTEMP' then result:=result+'"result": '+FormatFloat(f2,camera.Temperature)
  else if method='FOCUSERPOSITION' then result:=result+'"result": '+IntToStr(focuser.Position)
  else if method='TIMENOW' then result:=result+'"result": "'+FormatDateTime(dateiso,now)+'"'
  else if method='DIRECTORYSEPARATOR' then result:=result+'"result": "'+DirectorySeparator+'"'
  else if method='APPDIR' then result:=result+'"result": "'+Appdir+'"'
  else if method='TMPDIR' then result:=result+'"result": "'+TmpDir+'"'
  else if method='CAPTUREDIR' then result:=result+'"result": "'+config.GetValue('/Files/CapturePath',defCapturePath)+'"'
  // execute command without parameter
  else if method='TELESCOPE_ABORTMOTION' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_MountAbortMotion+'"}'
  else if method='TELESCOPE_TRACK' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_MountTrack+'"}'
  else if method='EQMOD_CLEARPOINTS' then result:=result+'"result":{"status": "'+ f_scriptengine.cmd_EqmodClearPoints+'"}'
  else if method='EQMOD_CLEARSYNCDELTA' then result:=result+'"result":{"status": "'+ f_scriptengine.cmd_EqmodClearSyncDelta+'"}'
  else if method='EQMOD_STDSYNC' then result:=result+'"result":{"status": "'+ f_scriptengine.cmd_EqmodStdSync+'"}'
  else if method='EQMOD_APPENDSYNC' then result:=result+'"result":{"status": "'+ f_scriptengine.cmd_EqmodAppendSync+'"}'
  else if method='AUTOGUIDER_CONNECT' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderConnect+'"}'
  else if method='AUTOGUIDER_CALIBRATE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderCalibrate+'"}'
  else if method='AUTOGUIDER_STARTGUIDING' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderStartGuiding+'"}'
  else if method='AUTOGUIDER_STOPGUIDING' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderStopGuiding+'"}'
  else if method='AUTOGUIDER_PAUSE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderPause+'"}'
  else if method='AUTOGUIDER_UNPAUSE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderUnPause+'"}'
  else if method='AUTOGUIDER_DITHER' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderDither+'"}'
  else if method='AUTOGUIDER_SHUTDOWN' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoguiderShutdown+'"}'
  else if method='WHEEL_GETFILTER' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Wheel_GetFilter+'"}'
  else if method='PREVIEW_SINGLE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Preview_Single+'"}'
  else if method='PREVIEW_LOOP' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Preview_Loop+'"}'
  else if method='PREVIEW_WAITLOOP' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Preview_WaitLoop+'"}'
  else if method='PREVIEW_STOP' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Preview_Stop+'"}'
  else if method='CAPTURE_START' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Capture_Start+'"}'
  else if method='CAPTURE_STOP' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_Capture_Stop+'"}'
  else if method='ASTROMETRY_SOLVE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AstrometrySolve+'"}'
  else if method='ASTROMETRY_SYNC' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AstrometrySync+'"}'
  else if method='ASTROMETRY_SLEW_IMAGE_CENTER' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AstrometrySlewImageCenter+'"}'
  else if method='PLANETARIUM_CONNECT' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_PlanetariumConnect+'"}'
  else if method='PLANETARIUM_SHOWIMAGE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_PlanetariumShowImage+'"}'
  else if method='PLANETARIUM_SHUTDOWN' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_PlanetariumShutdown+'"}'
  else if method='PROGRAM_SHUTDOWN' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_ProgramShutdown+'"}'
  else if method='CLEAR_REFERENCE_IMAGE' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_ClearReferenceImage+'"}'
  else if method='AUTOFOCUS' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutoFocus+'"}'
  else if method='AUTOMATICAUTOFOCUS' then result:=result+'"result":{"status": "'+f_scriptengine.cmd_AutomaticAutoFocus+'"}'
  // execute command with parameter
  else if method='DEVICESCONNECTION' then begin
    if uppercase(trim(value[attrib.IndexOf('params.0')]))='TRUE' then buf:='ON' else buf:='OFF';
    buf:=f_scriptengine.cmd_DevicesConnection(buf);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='TELESCOPE_PARK' then begin
    if uppercase(trim(value[attrib.IndexOf('params.0')]))='TRUE' then buf:='ON' else buf:='OFF';
    buf:=f_scriptengine.cmd_MountPark(buf);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='TELESCOPE_SLEW' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf2:=trim(value[attrib.IndexOf('params.1')]);
    buf:=f_scriptengine.cmd_MountSlew(buf1,buf2);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='TELESCOPE_SLEWASYNC' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf2:=trim(value[attrib.IndexOf('params.1')]);
    buf:=f_scriptengine.cmd_MountSlewAsync(buf1,buf2);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='TELESCOPE_SYNC' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf2:=trim(value[attrib.IndexOf('params.1')]);
    buf:=f_scriptengine.cmd_MountSync(buf1,buf2);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='WHEEL_SETFILTER' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Wheel_SetFilter(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='FOCUSER_SETPOSITION' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Focuser_SetPosition(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CCD_SETTEMPERATURE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Ccd_SetTemperature(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='PREVIEW_SETEXPOSURE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Preview_SetExposure(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='PREVIEW_SETBINNING' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Preview_SetBinning(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETEXPOSURE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetExposure(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETBINNING' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetBinning(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETOBJECTNAME' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetObjectName(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETCOUNT' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetCount(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETFRAMETYPE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetFrameType(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='CAPTURE_SETDITHER' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_Capture_SetDither(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='SEQUENCE_START' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_SequenceStart(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='SAVE_FITS_FILE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_SaveFitsFile(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='OPEN_FITS_FILE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_OpenFitsFile(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='OPEN_REFERENCE_IMAGE' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_OpenReferenceImage(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  else if method='LOGMSG' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    NewMessage(buf1);
    result:=result+'"result":{"status": "'+msgOK+'"}';
  end
  else if method='PLANETARIUM_SHOWIMAGE_FOV' then begin
    buf1:=trim(value[attrib.IndexOf('params.0')]);
    buf:=f_scriptengine.cmd_PlanetariumShowImage(buf1);
    result:=result+'"result":{"status": "'+buf+'"}';
  end
  // method not found
  else begin
    result:=result+'"error": {"code": -32601, "message": "Method not found"}';
  end;
  result:=result+', "id": '+id+'}'

except
  on E: Exception do result := '{"jsonrpc": "2.0", "error": {"code": -32603, "message": "Internal error:'+E.Message+'"}, "id": '+id+'}';
end;
end;

function Tf_main.TCPcmd(s: string):string;
var i,j,n,frx,fry,frw,frh: integer;
    pctseq,pcttarget,pctstep,pctexp: double;
    lblseq,lbltarget,lblstep,lblexp: string;
begin
 if (s = 'STATUS') then begin
   result:=StatusBar1.Hint;
 end
 else if (s = 'SEQUENCE') then begin
   result:=f_sequence.Title3.Caption+tab+
           f_sequence.Title2.Caption+tab+
           f_sequence.StatusMsg.Caption+tab+
           f_sequence.DelayMsg.Caption;
 end
 else if (s = 'CAPTURE') then begin
   result:=StatusBar1.Panels[panelstatus].Text+' '+StatusBar1.Panels[panelfile].Text;
 end
 else if (s = 'LOG') then begin
   result:='';
   n:=f_msg.msg.Lines.Count-1;
   if n<10 then
     j:=0
   else
    j:=n-10;
   for i:=j to n do
     result:=result+f_msg.msg.Lines[i]+crlf;
 end
 else if (s = 'HTML_STATUS') then begin
   result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"'
           + ' "http://www.w3.org/TR/html4/loose.dtd">'+crlf
           + '<head><meta http-equiv="Content-Type" content="text/html; charset=utf-8"><meta http-equiv="refresh" content="60"><title>'+Format(rsStatus, ['CCDciel'])+'</title>'+crlf
           + '<style type="text/css">.gauge {  width: 125px;  height: 62px;  position: relative;  overflow: hidden;  text-align:center;  margin: auto; }'+crlf
           + '.gauge-1 { z-index: 1;  background-color: rgba(80,80,255,.2);  width: 100%;  height: 100%; border-radius: 125px 125px 0px 0px;}'+crlf
           + '.gauge-2 {  z-index: 3;  position: absolute;  background-color: rgba(255,255,255,1);  width: 75%;  height: 75%;  bottom:0;  margin-left: 50%;  margin-right: auto; transform: translateX(-50%); border-radius: 125px 125px 0px 0px;}'+crlf
           + '.gauge-3 {  z-index: 2;  position: absolute;  background-color: rgba(80,80,255,1);  width: 100%;  height: 100%; margin-left: auto;  margin-right: auto;  border-radius: 0px 0px 100px 100px;  transform-origin: center top;  transform:rotate(0.0turn);}'+crlf
           + '.gauge-pct {  z-index: 4;  color: rgba(0,0,0,.2);  font-size: 1.2em;  line-height: 25px;  position: absolute;  width: 100%;  height: 100%;  top: 27%;  margin-left: auto;  margin-right: auto;}'+crlf
           + '</style></head> <body onload="ScrollLog();">'+crlf;

   result:=result + '<h1>'+Format(rsStatus, ['CCDciel'])+'<br></h1>'+crlf;

   if AllDevicesConnected then
     result:=result+Format(rsConnected+'</font></b>', [rsDevices+'<b><font color="green">'])
   else
     result:=result+Format(rsDisconnected+'</font></b>', [rsDevices+'<b><font color="red">']);
   if (f_autoguider<>nil)and(autoguider.State=GUIDER_DISCONNECTED) then
     result:=result+', '+Format(rsDisconnected+'</font></b>',[rsAutoguider+'<b><font color="red">'])
   else if (autoguider.State=GUIDER_GUIDING) then
     result:=result+', '+Format(rsGuiding+'</font></b>',[rsAutoguider+'<b><font color="green">'])
   else
     result:=result+', '+Format(rsConnected+'</font></b>',[rsAutoguider+'<b><font color="orange">']);
   if (f_planetarium<>nil)and(not planetarium.Terminated)and(planetarium.Connected) then
     result:=result+', '+Format(rsConnected+'</font></b>',[rsPlanetarium+'<b><font color="green">'])
   else
     result:=result+', '+Format(rsDisconnected+'</font></b>',[rsPlanetarium+'<b><font color="red">']);
   result:=result+'<br>'+crlf;

   result:=result+'<table style="width: 520px; text-align:left;">'+crlf;
   if safety.Status=devConnected then begin
     result:=result+'<tr><td colspan="3" style="vertical-align:top;"><b>'+rsSafetyMonito+': </b>';
     result:=result+BoolToStr(f_safety.Safe, '<font color="green">'+rsSafe+'</font>', '<font color="red">'+rsUnsafe+'</font>');
     result:=result+'</td></tr>';
   end;
   if weather.Status=devConnected then begin
     result:=result+'<tr><td colspan="3" style="vertical-align:top;"><b>'+rsWeatherStati+': </b>';
     result:=result+BoolToStr(f_weather.Clear, '<font color="green">'+rsGood+'</font>', '<font color="red">'+rsBad+'</font>');
     if not f_weather.Clear then result:=result+blank+weather.WeatherMessage;
     result:=result+'</td></tr>';
   end;
   if dome.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsDome+': </b></td><td colspan="2" style="vertical-align:top;">';
     result:=result+Format(rsDomeShutter, [BoolToStr(f_dome.Shutter, rsOpen, '<font color="red">'+rsClose+'</font>')]);
     if f_dome.CanSlave then result:=result+', '+Format(rsDomeSlaving, [BoolToStr(f_dome.Slave, rsOn, '<font color="red">'+rsOff+'</font>')]);
     result:=result+'</td></tr>';
   end;
   if mount.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsMount+': </b></td><td colspan="2" style="vertical-align:top;">';
     if mount.Park then
       result:=result+'<font color="red">'+rsParked+'</font>'
     else begin
       result:=result+rsUnparked;
       if mount.Tracking then result:=result+', '+rsTracking
          else result:=result+', <font color="red">'+rsNotTracking+'</font>';
       result:=result+', ';
       result:=result+rsRA+': '+f_mount.RA.Caption+' ';
       result:=result+rsDec+': '+f_mount.DE.Caption+'<br>';
       result:=result+f_mount.Pierside.Caption+', '+rsMeridianIn+' '+f_mount.TimeToMeridian.Caption+' '+rsMinutes;
       result:=result+'<br>';
     end;
     result:=result+'</td></tr>';
   end;
   if camera.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsCamera+': </b></td><td colspan="2" style="vertical-align:top;">';
     result:=result+rsBinning+': '+inttostr(camera.BinX)+'x'+inttostr(camera.BinY)+', ';
     camera.GetFrame(frx,fry,frw,frh);
     result:=result+rsFrame+': X='+inttostr(frx)+' Y='+inttostr(fry)+' '+rsWidth+'='+inttostr(frw)+' '+rsHeight+'='+inttostr(frh)+'<br>';
     result:=result+rsSensorTemperatu+': '+f_ccdtemp.Current.Caption+', '+rsCooler+'='+BoolToStr(f_ccdtemp.CCDcooler.Checked, rsOn, '<font color="red">'+rsOff+'</font>')+'<br>';
     result:=result+'</td></tr>';
   end;
   if wheel.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsFilters+': </b></td><td colspan="2" style="vertical-align:top;">';
     result:=result+rsFilter+': '+f_filterwheel.Filters.Text;
     result:=result+'</td></tr>';
   end;
   if rotator.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsRotator+': </b></td><td colspan="2" style="vertical-align:top;">';
     result:=result+rsPA+': '+f_rotator.Angle.Text;
     result:=result+'</td></tr>';
   end;
   if focuser.Status=devConnected then begin
     result:=result+'<tr><td style="vertical-align:top;"><b>'+rsFocuser+': </b></td><td style="vertical-align:top;">';
     if focuser.hasAbsolutePosition then result:=result+rsPos+': '+f_focuser.Position.Text+'<br>';
     if focuser.hasTemperature then result:=result+rsTemp+': '+f_focuser.Temp.Text+'<br>';
     result:=result+StringReplace(f_starprofile.LastFocusMsg,crlf,'<br>',[]);
     if true or (f_starprofile.PtSourceMeasure.Count>0) then begin
        result:=result+'</td><td style="vertical-align:top;">';
        result:=result+'<img src="lastfocus.jpg" alt="Last focus graph">';
     end;
     result:=result+'</td></tr>';
   end;
   result:=result+'</table>';

   pctseq:=0;pcttarget:=0;pctstep:=0;pctexp:=0;
   lblseq:='&nbsp;';lbltarget:='&nbsp;';lblstep:='&nbsp;';lblexp:='&nbsp;';
   if f_sequence.Running then begin
      pctseq := f_sequence.PercentComplete;
      pcttarget:= f_sequence.TargetPercentComplete;
      result:=result+'<b>'+rsSequence+': '+'</b>'+f_sequence.StatusMsg.Caption+'<br>'
   end
   else begin
     result:=result+'<b>'+rsSequence+': '+'</b>'+rsStop+'<br>';
   end;
   if f_capture.Running then begin
      pctstep:=(f_capture.SeqCount-1)/f_capture.SeqNum.Value;
      if f_capture.ExposureTime>0 then pctexp := (f_capture.ExposureTime-CameraExposureRemain)/f_capture.ExposureTime;
      lblexp:=f_capture.ExpTime.Text;
   end;
   if CurrentSeqName<>'' then lblseq:=copy(CurrentSeqName,1,14);
   if CurrentTargetName<>'' then lbltarget:=copy(CurrentTargetName,1,14);
   if CurrentStepName<>'' then lblstep:=copy(CurrentStepName,1,14);
   result:=result+'<table style="width: 520px; text-align:center; overflow: hidden;"><tr><td>'+crlf
           +'<div class="gauge"><div class="gauge-1"></div><div class="gauge-2"></div>'
           +'<div class="gauge-3" style="transform:rotate('+formatfloat(f2,pctseq/2)+'turn);"></div>'
           +'<div class="gauge-pct"> <h2>'+formatfloat(f0,pctseq*100)+'%</h2> </div>'
           +'</div>'+rsSequence+'<br>'+lblseq+'</td><td>'+crlf
           +'<div class="gauge"><div class="gauge-1"></div><div class="gauge-2"></div>'
           +'<div class="gauge-3" style="transform:rotate('+formatfloat(f2,pcttarget/2)+'turn);"></div>'
           +'<div class="gauge-pct"> <h2>'+formatfloat(f0,pcttarget*100)+'%</h2> </div>'
           +'</div>'+rsTargets+'<br>'+lbltarget+'</td><td>'+crlf
           +'<div class="gauge"><div class="gauge-1"></div><div class="gauge-2"></div>'
           +'<div class="gauge-3" style="transform:rotate('+formatfloat(f2,pctstep/2)+'turn);"></div>'
           +'<div class="gauge-pct"> <h2>'+formatfloat(f0,pctstep*100)+'%</h2> </div>'
           +'</div>'+rsStep+'<br>'+lblstep+'</td><td>'+crlf
           +'<div class="gauge"><div class="gauge-1"></div><div class="gauge-2"></div>'
           +'<div class="gauge-3" style="transform:rotate('+formatfloat(f2,pctexp/2)+'turn);"></div>'
           +'<div class="gauge-pct"> <h2>'+formatfloat(f0,pctexp*100)+'%</h2> </div>'
           +'</div>'+rsExposure+'<br>'+lblexp+'</td></tr></table>'+crlf;

   if f_sequence.DelayMsg.Caption<>'' then
     result:=result+'<b>'+'Operation'+': '+'</b>'+f_sequence.DelayMsg.Caption+'<br>'+crlf;
   if capture then
      result:=result+'<b>'+rsCapture+': '+'</b>'
   else if preview then
      result:=result+'<b>'+rsPreview+': '+'</b>';
   result:=result+StatusBar1.Panels[panelstatus].Text+'<br>'+crlf;

   result:=result+'<a href="fullimage.jpg" target="_blank"><img src="scrimage.jpg" width="520" alt="Last image on screen"></a>' +'<br>'+crlf;
   result:=result+StatusBar1.Panels[panelfile].Text+'<br>'+crlf;

   n:=f_msg.msg.Lines.Count-1;
   if n<30 then j:=0
           else j:=n-30;
   result:=result+'<div id="log" style="height:120px;width:520px;border:1px solid #ccc;overflow:auto;">';
   for i:=j to n do
     result:=result+f_msg.msg.Lines[i]+'<br>'+crlf;
   result:=result+'</div>'+'<br>'+crlf;
   result:=result+'<script type="application/javascript"> function ScrollLog() {' +
           'var elmnt = document.getElementById("log");' +
           'elmnt.scrollTop += 99999; } </script>';

   result:=result+'</body></html>'+crlf;
 end
 else result:='Unknown command: '+s;
end;

procedure Tf_main.ShutdownProgram(Sender: TObject);
begin
 f_pause.Caption:='Program shutdown';
 f_pause.Text:='The program will be closed in 5 minutes';
 NewMessage(f_pause.Text,1);
 if f_pause.Wait(300) then begin
   ConfirmClose:=false;
   Close;
 end
 else
   NewMessage('Program shutdown canceled.',1);
end;

procedure Tf_main.ShowStatus(str: string);
begin
StatusBar1.Panels[panelcursor].Text := str;
end;

end.


