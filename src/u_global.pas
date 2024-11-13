unit u_global;

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
{$modeswitch advancedrecords}

interface

uses u_ccdconfig, dynlibs, LMessages, Graphics,
  Classes, SysUtils,LCLType;

const
  DomeOpenActionNum=5;
  DomeCloseActionNum=5;
  BPMMax=30000;
  SaveFilterNum = 999;

type
  TNotifyMsg = procedure(msg:string; level: integer=1) of object;
  TNotifyStr = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TNotifyInt = procedure(i: integer) of object;
  TNotifyBoolConst= procedure(v: boolean) of object;
  TNotifyBool= procedure(var v: boolean) of object;
  TRunScript = procedure(scname,scpath,scargs: string) of object;
  TSolve = procedure(f:pointer; out ra,de,pa,scale: double; out ok:boolean) of object;

  TDevInterface = (INDI, ASCOM, INCAMERA, INTELESCOPE, ASCOMREST, MANUAL);
  TFrameType =(LIGHT, BIAS, DARK, FLAT);
  TFlatType=(ftNone,ftSKY,ftDome);
  TDomeFlatPositionType=(DomeFlatPositionAltAz,DomeFlatPositionPark,DomeFlatPositionHome);
  TAutoguiderType=(agPHD,agLINGUIDER,agNONE,agDITHER,agINTERNAL);
  TAutoguiderState=(GUIDER_DISCONNECTED,GUIDER_IDLE,GUIDER_GUIDING,GUIDER_BUSY,GUIDER_ALERT,GUIDER_INITIALIZING);
  TPlanetariumType=(CDC, SAMP, HNSKY,plaNONE);
  TEqmodAlign=(alADDPOINT,alSTDSYNC,alUNSUPPORTED);
  TAlignmentMode=(algAltAz,algPolar,algGermanPolar);
  TTrackRate =(trSidereal, trLunar, trSolar, trCustom);
  TBayerMode=(bayerGR,bayerRG,bayerBG,bayerGB,bayerCamera,bayerUnsupported);
  TAutofocusMode=(afVcurve,afDynamic,afIterative,afNone,afPlanet);
  TAutofocusVcurveStep=(vcsStartL,vcsStartR,vcsNearL,vcsNearR,vcsCheckL,vcsCheckR,vcsFocusL,vcsFocusR);
  TAutofocusDynamicStep=(afdStart,afdMeasure,afdEnd);
  TAutofocusPlanetStep=(afpStart,afpMeasure,afpEnd);
  TIndiTransfert=(itNetwork,itDisk);
  TSubDirList=(sdSeq,sdFrt,sdObj,sdStep,sdExp,sdBin,sdDate,sdNight);
  TFilenameList=(fnObj,fnFilter,fnExp,fnBin,fnTemp,fnDate,fnGain,fnFocuspos,fnPierSide,fnOffset,fnStep,fnCtype);
  TSafetyAction=(safNothing,safShowPrompt,safAbortSequence,safStopTelescope,safParkTelescope,safStopDomeSlaving,safParkDome,safCloseDome,safWarmCamera,safAutoguiderShutdown,safPlanetariumShutdown,safExternalCommand,safExitProgram);
  TDomeOpenAction=(dopNothing,dopOpenDome,dopUnparkdome,dopUnparkTelescope,dopStartTelescope,dopStartdomeSlaving);
  TDomeCloseAction=(dclNothing,dclStopTelescope,dclParkTelescope,dclStopDomeSlaving,dclParkDome,dclCloseDome);
  TDomeOpenActions=array[0..DomeOpenActionNum-1] of TDomeOpenAction;
  TDomeCloseActions=array[0..DomeCloseActionNum-1] of TDomeCloseAction;
  TFileFormat = (ffFITS, ffASTROTIFF);

  TCustomFrameType = record
    Name,ScriptOn,ScriptOff,ParamOn,ParamOff: string;
  end;

  coordvector = array[1..3] of double;
  rotmatrix = array[1..3, 1..3] of double;

  TBpm=array[1..BPMMax]of array[1..2] of integer;

  TSaveFilter = array[0..SaveFilterNum] of string;

  TDouble2 = array[1..2] of double;
  TArrayDouble2 = array of TDouble2;

  TNumRange = record
               min,max,step: double;
               class operator =(a,b : TNumRange) : Boolean;
              end;

  TONumRange = Class(TObject)
               public
                 range: TNumRange;
               end;

  TScriptType = (stUnknown, stPascal, stPython);

  TFilterExp = Class(TObject)
               public
                 ExpFact: double;
               end;

  TIntList = Class(TObject)
               public
                 value: integer;
               end;

  TStep   = Class(TObject)
              public
              steptype: integer; //0=capture, 1=script, 2=switch
              // capture options
              exposure : double;
              stackcount: integer;
              count: integer;
              donecount: integer;
              dither: boolean;
              dithercount: integer;
              autofocusstart: boolean;
              autofocus: boolean;
              autofocuscount: integer;
              filter: integer;
              binx,biny: integer;
              gain: integer;
              offset: integer;
              frtype: integer;
              fstop: string;
              // script options
              scriptname: string;
              scriptpath: string;
              scriptargs: string;
              // switch options
              switchnickname: string;
              switchname: string;
              switchvalue: string;
              // global options
              description: string;
              constructor Create;
              procedure Assign(Source: Tstep);
              function exposure_str: string;
              function stackcount_str: string;
              function count_str: string;
              function filter_str: string;
              function binning_str: string;
              function frtype_str: string;
              function description_str: string;
              function dithercount_str: string;
              function autofocuscount_str: string;
              function gain_str: string;
              function offset_str: string;
              function id: LongWord;
            end;

  TFocusStar = record
                 ra,de: double;
                 id: string;
               end;

  Thorizonlist = array [0..361] of single;

  TAutoguiderStatistics = record
       RAdistance, Decdistance: single;
       Starmass: single;
       Guiding: boolean;
  end;

  TAltAzPosition = class(TObject)
    az1,alt1: double;
    az2,alt2: double;
  end;

  TCustomHeaderElement = record
       key,value: shortstring;
  end;

  TRoi = class(TObject)
    x,y,w,h: integer;
    name: string;
  end;

  // libcdcwcs
 type
   {$ifdef cpu32}
    Intwcs=Int32;
   {$else}
    Intwcs=integer;
   {$endif}
    TcdcWCScoord = record
      ra, dec, x, y: double;
      n: Intwcs;
    end;
    PcdcWCScoord = ^TcdcWCScoord;

    TcdcWCSinfo = record   // this structure must not be changed
      cra, cdec, dra, ddec, secpix, eqout, rot: double;
      wp, hp, sysout: Intwcs;
    end;
    PcdcWCSinfo = ^TcdcWCSinfo;
    Tcdcwcs_initfitsfile = function(fn: PChar; wcsnum:Intwcs): Intwcs; cdecl;
    Tcdcwcs_release = function(wcsnum:Intwcs): Intwcs; cdecl;
    Tcdcwcs_sky2xy = function(p: PcdcWCScoord; wcsnum:Intwcs): Intwcs; cdecl;
    Tcdcwcs_xy2sky = function(p: PcdcWCScoord; wcsnum:Intwcs): Intwcs; cdecl;
    Tcdcwcs_getinfo = function(p: PcdcWCSinfo; wcsnum:Intwcs): Intwcs; cdecl;

  var
    cdcwcslib: TLibHandle;
    cdcwcs_initfitsfile: Tcdcwcs_initfitsfile;
    cdcwcs_release: Tcdcwcs_release;
    cdcwcs_getinfo: Tcdcwcs_getinfo;
    cdcwcs_sky2xy: Tcdcwcs_sky2xy;
    cdcwcs_xy2sky: Tcdcwcs_xy2sky;

  //  zlib
  type
    Tuncompress = function(dest:Pointer; destLen: Pointer; source: Pointer; sourceLen: UInt64):longint; cdecl;
  var
    uncompress: Tuncompress;
    zlibok: boolean;
    zlib: TLibHandle;


  const
    maxfitslist=15;  // must corespond to value in cdcwcs.c
    wcsmain=0; wcsguide=1; wcsfind=2; wcspreview=3; wcsfits=4;

  {$i revision.inc}

const
  ccdcielver = '0.9.88';
  ccdciel_version='Version beta '+ccdcielver;
  TargetFileVersion = 5;
  Maxclient = 100;
  MaxCustomHeaders = 50;
  blank=' ';
  blank80='                                                                                ';
  clOrange=$1080EF;
  clDarkBlue=$300D0E;
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  tab = #09;
  secperday = 3600*24;
  minperday = 60*24;
  pi2 = 2 * pi;
  pid2 = pi / 2;
  rad2deg=180/pi;
  deg2rad=pi/180;
  secarc = deg2rad / 3600;
  musec = deg2rad / 3600 / 1000000; // 1 microarcsec for rounding test
  siderealrate = 15.041067178669; // arcsec/second
  j2000 = ' (J2000)';
  jd2000 = 2451545.0;
  abek = secarc * 20.49552;  // aberration constant
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord:double=-9999;
  NullInt: integer=-9999;
  Binning0 = '1x1';
  dateiso = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';
  dateisoshort = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  datehms = 'hh":"nn":"ss';
  roundf1=10;
  roundf2=100;
  roundf3=1000;
  f0 = '0';
  f1 = '0.0';
  f2 = '0.00';
  f3 = '0.000';
  f4 = '0.0000';
  f5 = '0.00000';
  f6 = '0.000000';
  f1v = '0.#';
  f2v = '0.##';
  f3v = '0.###';
  f4v = '0.####';
  f9v = '0.#########';
  f1mc= '+0.0h;-0.0h; ';
  MeridianCrossing='MC';
  azNorth=0;
  azSouth=1;
  b80 ='                                                                                ';
  NRestrictKey=8;
  RestrictKey: array[1..NRestrictKey] of string = ('SIMPLE','BITPIX','NAXIS','NAXIS1','NAXIS2','NAXIS3','XTENSION','END');
  FocusDirIn=true;
  FocusDirOut=false;
  FrameName: array[0..ord(high(TFrameType))] of string =('Light   ','Bias    ','Dark    ','Flat    '); // add space to header as INDI do
  FlatTimeName: array[0..1] of string=('Dusk','Dawn');
  PierSideName: array[0..3] of string=('pierEast', 'pierWest', 'pierUnknown','pierNotImplemented');
  TrackRateName :array [0..3] of string = ('TRACK_SIDEREAL','TRACK_LUNAR','TRACK_SOLAR','TRACK_CUSTOM');
  ResolverAstrometryNet=0;
  ResolverElbrus=1;
  ResolverNone=2;
  ResolverPlateSolve=3;
  ResolverAstap=4;
  ResolverName: array[0..4] of string =('Astrometry.Net','Elbrus','No resolver','PlateSolve','ASTAP');
  PlanetariumName: array[0..2] of string =('Cartes du Ciel', 'SAMP', 'HNSKY');
  SafetyActionName: array[0..ord(high(TSafetyAction))] of string=('','','','','','','','','','','','','');
  DomeOpenActionName: array[0..ord(high(TDomeOpenAction))] of string=('','','','','','');
  DomeCloseActionName: array[0..ord(high(TDomeCloseAction))] of string=('','','','','','');
  DefaultFNlistcount=12;
  DefaultFNlist: array[0..DefaultFNlistcount-1] of string=('1','1.2','1.4','1.8','2','2.8','4','5.6','8','11','16','22');
  LM_CCDCIEL=LM_USER + 1;
  M_AutoguiderStatusChange=1000;
  M_AutoguiderMessage=1001;
  M_AutoguiderCancelExposure=1002;
  M_AutoguiderGuideStat=1003;
  M_AutoguiderAbortTarget=1004;
  M_AstrometryDone=1100;
  M_AstrometryMsg=1101;
  M_Message=1200;
  M_AbortSequence=1201;
  M_SequenceCancelExposure=1202;
  StarLostStatus='Star lost';
  MaxCmdArg = 10;
  MaxMenulevel=10;
  MaxFilter=100;
  MaxSwitches=100;
  WaitResponseTime=120;
  ZoomMax=15;
  GuideZoomMax=15;
  FinderZoomMax=15;
  msgOK = 'OK!';
  msgFailed = 'Failed!';
  ldeg = 'd';
  lmin = 'm';
  lsec = 's';
  sdeg = #$C2+#$B0;
  smin = '''';
  ssec = '"';
  ellipsis = #$E2+#$80+#$A6;
  URL_DOWNLOAD='https://sourceforge.net/projects/ccdciel/files/';
  URL_BUGREPORT='https://www.ap-i.net/mantis/set_project.php?project_id=3';
  URL_CHANGELOG='https://groups.io/g/ccdciel/search?q=%23newversion&ct=1';
  URL_ONLINEHELP='https://www.ap-i.net/ccdciel/en/documentation/start';
  URL_USERGROUP='https://groups.io/g/ccdciel';
  URL_PROGRAMSTATUS='http://localhost:$port';
  URL_SCRIPTLIST='https://vega.ap-i.net/pub/ccdciel/scripts/script.list';
  URL_SCRIPTDOWNLOAD='https://vega.ap-i.net/pub/ccdciel/scripts/';
  SkyFlatTxt='SkyFlat';
  ScriptTxt='Script';
  SwitchTxt='Switch';
  SubDirCount=8;
  FileNameCount=12;
  MinFrameSize=5;
  SafetyActionNum=15;
  encryptpwd='m=Nrv"wE+W^RA?$b:]w<!t1v]pcTT>3$B?3";~OG9\7$,n[~8KLaUrfCgvRh$=DnlXK]Vxr^0!.HAA';

  {$ifdef linux}
    SharedDir = '../share/ccdciel';
    defTransfertPath='/mnt/ramdisk';
    libwcs = 'libpaswcs.so.1';
    libz = 'libz.so.1';
    fpackcmd = 'fpack';
    funpackcmd = 'funpack';
    defCdCpath = 'skychart';
    defHNSKYpath = 'hnsky';
    defSAMPpath = 'aladin';
    defPHDpath = 'phd2';
    hostOS = 'linux';
    defAstrometryNetOpt='';
  {$endif}
  {$ifdef darwin}
    SharedDir = './';
    defTransfertPath='/Volumes/ramdisk';
    libwcs = 'libccdcielwcs.dylib';
    libz = 'libz.dylib';
    fpackcmd = 'fpack';
    funpackcmd = 'funpack';
    defCdCpath = '/Applications/Cartes du Ciel/skychart.app/Contents/MacOS/skychart';
    defHNSKYpath = 'hnsky';
    defSAMPpath = '/Applications/Aladin.app/Contents/MacOS/Aladin';
    defPHDpath = '/Applications/PHD2.app/Contents/MacOS/PHD2';
    hostOS = 'darwin';
    defAstrometryNetOpt='';
  {$endif}
  {$ifdef mswindows}
    SharedDir = '.\';
    defTransfertPath='Z:\';
    libwcs = 'libccdcielwcs.dll';
    libz = 'zlib1.dll';
    fpackcmd = 'fpack.exe';
    funpackcmd = 'funpack.exe';
    defCdCpath = 'C:\Program Files\Ciel\skychart.exe';
    defHNSKYpath = 'C:\Program Files\hnsky\hnsky.exe';
    defSAMPpath = 'C:\Program Files\Aladin\Aladin.exe';
    defPHDpath = 'C:\Program Files (x86)/PHDGuiding2/phd2.exe';
    hostOS = 'windows';
    defAstrometryNetOpt='--no-fits2fits';
  {$endif}
  {$ifdef darwin}
    OpenFileCMD: string = 'open';
  {$else}
    OpenFileCMD: string = 'xdg-open';   // default FreeDesktop.org
    {$endif}
  AscomInvalidArchitecture='program with an incorrect format';

var
  onMsgGlobal: TNotifyMsg;
  Appdir,ConfigDir,LogDir,TmpDir,DataDir,ScriptsDir,HomeDir,SequenceDir: UTF8String;
  defCapturePath, defPython, TCPIPConfigPort, TCPIPServerPort: string;
  CameraName,WheelName,FocuserName,RotatorName,MountName,DomeName,WatchdogName,WeatherName,SafetyName,SwitchName,CoverName,GuideCameraName,FinderCameraName: string;
  ConfigFlatFile,ConfigDarkFile,ConfigGuiderDarkFile,cdate: string;
  LastCapturePath: string;
  isAdmin, UacEnabled, debug_msg: boolean;
  AllDevicesConnected: boolean;
  ConfirmClose, ScreenScaling, LogToFile: boolean;
  LogLevel: integer;
  PythonCmd: string;
  config,screenconfig,credentialconfig,emailconfig,bpmconfig,globalconfig: TCCDConfig;
  profile: string;
  ProfileFromCommandLine: boolean;
  lang: string;
  ScreenMargin: integer;
  FilterList,BinningList,ReadoutList: TStringList;
  FilterOffset: array [0..MaxFilter] of integer;
  FilterExpFact: array [0..MaxFilter] of double;
  Filter0: string;
  CurrentFilterOffset: integer;
  filteroffset_initialized: boolean;
  ReadoutModeCapture,ReadoutModePreview,ReadoutModeFocus,ReadoutModeAstrometry: integer;
  UseReadoutMode: boolean;
  compile_time, compile_version, compile_system, lclver: string;
  WCSxyNrot,WCSxyErot,WCScenterRA,WCScenterDEC,WCSpoleX,WCSpoleY,WCSwidth,WCSheight: double;
  DitherPixel, SettlePixel, DitherWaitTime: double;
  DitherRAonly: boolean;
  SettleMinTime, SettleMaxTime, CalibrationDelay: integer;
  MeridianOption,MinutesPastMeridian, MinutesPastMeridianMin, MeridianFlipPauseTimeout: integer;
  MeridianFlipPauseBefore, MeridianFlipPauseAfter,MeridianFlipCalibrate,MeridianFlipAutofocus,
  MeridianFlipStopSlaving,MeridianFlipUseSetPierSide: boolean;
  MeridianScript,MeridianScriptPath,MeridianScriptArgs: string;
  PHD2GuideSetLock,InternalGuiderSetLockPosition: boolean;
  PHD2GuideLockX, PHD2GuideLockY: double;
  astrometryResolver: integer;
  LastROIname: string;
  OrigX, OrigY,img_Height,img_Width,GuideOrigX, GuideOrigY,guideimg_Height,guideimg_Width : integer;
  FinderOrigX, FinderOrigY,finderimg_Height,finderimg_Width : integer;
  ImgFrameX,ImgFrameY,ImgFrameW,ImgFrameH: integer;
  ImgScale0,ImgPixRatio: double;
  ImgZoom,ZoomMin,SplitZoom: double;
  GuideImgCx,GuideImgCy,FinderImgCx,FinderImgCy: double;
  SplitImage: boolean;
  GuideImgScale0,GuideImgPixRatio,GuideImgZoom,GuideZoomMin: double;
  ScrWidth,ScrHeigth,ScrGuideWidth,ScrGuideHeigth,ScrFinderWidth,ScrFinderHeigth,SplitMargin: integer;
  FinderImgScale0,FinderImgPixRatio,FinderImgZoom,FinderZoomMin: double;
  MaxADU, ClippingOverflow, ClippingUnderflow: double;
  MsgHandle: THandle;
  LastPixelSize: double;
  ObsLongitude, ObsLatitude, ObsElevation, ObsTimeZone, ObsRefA, ObsRefB: double;
  ObsWeather: boolean;
  ObsTemperature, ObsPressure, ObsHumidity, ObsTlr: double;
  BayerColor: boolean;
  DefaultBayerMode:TBayerMode;
  BalanceFromCamera,BGneutralization: boolean;
  RedBalance,GreenBalance,BlueBalance: double;
  ColorizeSpectra: boolean;
  MaxVideoPreviewRate: integer;
  TemperatureScale: integer;
  TempLabel: string;
  TemperatureSlope: double;
  FocuserTemp, FocuserLastTemp, AutofocusLastTemp, FocuserTempCoeff, AutofocusTempChange, AutoFocusLastTime: double;
  HFM_Threshold: double;
  FocuserPositionMin, FocuserPositionMax: integer;
  Starwindow,Focuswindow: integer;
  AutofocusMode:TAutofocusMode;
  AutofocusMinSpeed,AutofocusMaxSpeed,AutofocusNearNum,FocuserDelay,AutofocusBinning,AutofocusPeriod: integer;
  AutofocusStartHFD,AutofocusNearHFD: double;
  AutofocusExposure,AutofocusExposureFact:double;
  AutofocusGain, AutofocusOffset: integer;
  AutofocusMoveDir: boolean;
  PosStartL,PosStartR,PosNearL,PosNearR,PosFocus,AutofocusVcNum,AutofocusVcSkipNum,VcCenterpos,VcHalfwidth,VcNsteps:integer;
  AutofocusVc: array[0..100]of array[1..2] of double;
  AutofocusVcDir, AutofocusSlippageCorrection: boolean;
  AutofocusVcSlopeL,AutofocusVcSlopeR,AutofocusVcPID,AutofocusVcpiL,AutofocusVcpiR: double;
  AutofocusVcStep:TAutofocusVcurveStep;
  AutofocusDynamicMovement,AutofocusDynamicNumPoint,AutofocusVcFilterOffset,AutofocusSlippageOffset,AutoFocusDefocus: integer;
  AutofocusDynamicStep:TAutofocusDynamicStep;
  AutofocusPlanetMovement,AutofocusPlanetNumPoint: integer;
  AutofocusPlanetStep:TAutofocusPlanetStep;
  AutofocusVcCheckNum: integer;
  AutofocusVcCheckHFDlist: array of double;
  AutofocusVcTemp, AutofocusVcTemp1, AutofocusVcTemp2: double;
  AutofocusInPlace, InplaceAutofocus, AutofocusPauseGuider, AutofocusMultiStarCenter: boolean;
  AutofocusMultiStarCenterPct: integer;
  AutofocusStarList: TArrayDouble2;
  CancelAutofocus, Autofocusing, TerminateFocuserCalibration: Boolean;
  GotoInProgress, CancelGoto: boolean;
  ExpectedStop: boolean;
  CameraProcessingImage: boolean;
  CameraProcessingNum: PtrInt;
  MagnitudeCalibration: double;
  Undersampled: boolean;
  bpm: TBpm;
  bpmNum,bpmX,bpmY,bpmAxis: integer;
  BPMsigma: double;
  NFocusStars: integer;
  FocusStars: array of TFocusStar;
  FocusStarMag: integer;
  FocusStarMagAdjust: boolean;
  FocusStarsBlacklist: string;
  AutofocusTolerance, AutofocusMinSNR: double;
  WaitTillrunning, cancelWaitTill: boolean;
  MeridianFlipping: boolean;
  horizonlist: Thorizonlist;
  HorizonMax, HorizonMin, ElevationMin: double;
  jdtoday,nutl,nuto,abp,abe,ecl,sunl: double;
  AzimuthOrigin: integer;
  NutMAT: rotmatrix;
  ConfigExpEarlyStart, EarlyNextExposure, SkipEarlyExposure, EarlyDither: boolean;
  FlatAutoExposure,doFlatAutoExposure,FlatWaitDusk,FlatWaitDawn,AdjustDomeFlat,DomeFlatTelescopeSlew,DomeFlatSetLight,AdjustFlatLight: boolean;
  FlatType: TFlatType;
  FlatMinExp,FlatMaxExp,DomeFlatTelescopeAz,DomeFlatTelescopeAlt,DomeFlatExpAdjust: double;
  DomeFlatPosition: TDomeFlatPositionType;
  FlatLevelMin,FlatLevelMax,DomeFlatLevel: integer;
  FlatSlewTime: TDateTime;
  DomeFlatSetLightON,DomeFlatSetLightOFF: string;
  DomeNoSafetyCheck: boolean;
  CustomFrameType: array of TCustomFrameType;
  NumCustomFrameType,CurrentCustomFrameType: integer;
  SubDirOpt: array[0..SubDirCount-1] of TSubDirList;
  SubDirActive: array[0..SubDirCount-1] of Boolean;
  FilenameOpt: array[0..FileNameCount-1] of TFilenameList;
  FilenameActive: array[0..FileNameCount-1] of Boolean;
  FilenameSep, FilenameSeqSep: String;
  FilePack,FileStackFloat: boolean;
  FitsFileExt: string;
  FileSequenceWidth: integer;
  UseRotator: boolean;
  CanSetGainOffset, hasGain, hasGainISO, hasOffset: boolean;
  ISOList: TStringList;
  Gain,GainMin,GainMax: integer;
  Offset,OffsetMin,OffsetMax: integer;
  SubDirName: array[0..SubDirCount-1] of string;
  FilenameName: array[0..FileNameCount-1] of string;
  CurrentSeqName, CurrentTargetName, CurrentStepName: string;
  CurrentStepNum,CurrentDoneCount: integer;
  WeatherPauseCapture,WeatherCapturePaused,WeatherPauseCanceled,WeatherCancelRestart: boolean;
  WeatherRestartDelay: integer;
  SaveFormat: TFileFormat;
  MeasureNewImage,SaveBitmap: boolean;
  SaveBitmapFormat:string;
  MaxThreadCount: integer;
  PauseSequence: boolean;
  DummyDouble: double;
  DummyBool: boolean;
  DevInterfaceName: array[0..5] of string=('INDI','ASCOM','In camera','In mount','ASCOM Alpaca','Manual');
  ProtocolName: array[0..1] of string=('http:','https:');
  CheckRecenterTarget,NeedRecenterTarget,RecenteringTarget,CheckRecenterBusy: boolean;
  AstrometryTimeout,RecenterTargetDistance,SlewPrecision: double;
  PolarAlignmentOverlay, PolarAlignmentLock: boolean;
  PolarAlignmentStartx,PolarAlignmentStarty,PolarAlignmentEndx,PolarAlignmentEndy,PolarAlignmentAzx,PolarAlignmentAzy:double;
  PolarAlignmentOverlayOffsetX,PolarAlignmentOverlayOffsetY: double;
  MailTo,MailFrom,SMTPHost,SMTPPort,SMTPUser,SMTPPasswd : String;
  SMTPSSLTLS: boolean;
  EmailEndSequence,EmailAbortSequence,EmailAutoguider,EmailAufofocus,EmailMeridianFlip,EmailTargetInitialisation: boolean;
  ManualFilterNames: TStringList;
  VoiceDialog,VoiceSequence,VoiceError,VoiceEmail: boolean;
  DisplayCapture,LowQualityDisplay: boolean;
  WantExif: boolean;
  Collimation: boolean;
  CollimationCircle: integer;
  AutoguiderStat: array of TAutoguiderStatistics;
  colorGreen, colorBlue, colorRed, colorText, colorBg, colorGray, colorLightGray: Tcolor;
  AutoguiderAlert,AutoguiderStarting,LockRestartExposure: boolean;
  AutoguiderAlertTime,AutoguiderMsgTime: double;
  MountTrackingAlert: boolean;
  MountTrackingAlertTime: double;
  SaveStack, StackAlign, StackRotation, StackUseDark, StackUseFlat, StackDebayer: boolean;
  StackOperation: integer;
  RunningCapture,RunningPreview: boolean;
  ImageInspection, TriangleInspection: boolean;
  TriangleInspectionAngle: double;
  LastHfd,LastDrift: double;
  CustomHeaderNum: integer;
  CustomHeaders: array [1..MaxCustomHeaders] of TCustomHeaderElement;
  CdCAdjustFrame: boolean;
  PlanetariumShowAstrometry:boolean;
  NumSwitches: integer;
  IndistarterConfig, IndistarterConfigdir: string;
  IndistarterAutostart: boolean;
  {internal guider}
  InternalguiderRunning,InternalguiderCalibrating,InternalguiderCalibratingBacklash,InternalguiderGuiding,StopInternalguider,InternalguiderCapturingDark: boolean;
  {finder}
  UseFinder,FinderPreviewLoop: boolean;

  procedure globalmsg(str:string);
  function Str2Frametype(str:string):integer;

implementation

class operator TNumRange.=(a,b : TNumRange) : Boolean;
begin
  result:=(a.min=b.min)and(a.max=b.max)and(a.step=b.step);
end;


procedure globalmsg(str:string);
begin
  if Assigned(onMsgGlobal) then onMsgGlobal(str);
end;

function Str2Frametype(str:string):integer;
var i: integer;
begin
  result:=ord(LIGHT);
  str:=UpperCase(trim(str));
  if str='LIGHT' then result:=ord(LIGHT)
  else if str='BIAS' then result:=ord(BIAS)
  else if str='DARK' then result:=ord(DARK)
  else if str='FLAT' then result:=ord(FLAT)
  else begin
    for i:=0 to NumCustomFrameType-1 do begin
      if UpperCase(CustomFrameType[i].Name)=str then
        result:=ord(high(TFrameType))+i+1;
    end;
  end;
end;

////////////////////  TStep  /////////////////////////////

constructor TStep.Create;
begin
  steptype:=0;
  exposure:=1;
  stackcount:=1;
  count:=1;
  donecount:=0;
  filter:=0;
  binx:=1;
  biny:=1;
  frtype:=ord(LIGHT);
  dither:=false;
  dithercount:=1;
  autofocusstart:=false;
  autofocus:=false;
  autofocuscount:=10;
  fstop:='';
  gain:=1;
  offset:=0;
  description:='Step1';
  scriptname:='';
  scriptpath:='';
  scriptargs:='';
  switchnickname:='';
  switchname:='';
  switchvalue:='';
end;

procedure TStep.Assign(Source: Tstep);
begin
  exposure:=Source.exposure;
  stackcount:=Source.stackcount;
  count:=Source.count;
  donecount:=Source.donecount;
  filter:=Source.filter;
  binx:=Source.binx;
  biny:=Source.biny;
  gain:=Source.gain;
  offset:=Source.offset;
  frtype:=Source.frtype;
  dither:=Source.dither;
  dithercount:=Source.dithercount;
  autofocusstart:=Source.autofocusstart;
  autofocus:=Source.autofocus;
  autofocuscount:=Source.autofocuscount;
  fstop:=Source.fstop;
  description:=Source.description;
  steptype:=Source.steptype;
  scriptname:=Source.scriptname;
  scriptpath:=Source.scriptpath;
  scriptargs:=Source.scriptargs;
  switchnickname:=Source.switchnickname;
  switchname:=Source.switchname;
  switchvalue:=Source.switchvalue;
end;

function TStep.exposure_str: string;
begin
 Result:=FloatToStr(exposure);
end;

function TStep.stackcount_str: string;
begin
  Result:=IntToStr(stackcount);
end;

function TStep.count_str: string;
begin
  Result:=IntToStr(count);
end;

function TStep.dithercount_str: string;
begin
  Result:=IntToStr(dithercount);
end;

function TStep.autofocuscount_str: string;
begin
  Result:=IntToStr(autofocuscount);
end;

function TStep.filter_str: string;
begin
  if (FilterList.Count=0)or(filter<0)or(filter>(FilterList.Count-1)) then
    Result:=''
  else
    Result:=FilterList[filter];
end;

function TStep.binning_str: string;
begin
  Result:=IntToStr(binx)+'x'+IntToStr(biny);
end;

function TStep.frtype_str: string;
var i:integer;
begin
  if (frtype<0) then
    Result:=trim(FrameName[0])
  else if frtype<=ord(High(TFrameType)) then
    Result:=trim(FrameName[frtype])
  else begin
    i:=frtype-ord(High(TFrameType))-1;
    if (i>=0) and (i<NumCustomFrameType) then
      Result:=trim(CustomFrameType[i].Name)
    else
      Result:=trim(FrameName[0]);
  end;
end;

function TStep.description_str: string;
begin
  Result:=description;
end;

function TStep.gain_str: string;
begin
  Result:=IntToStr(gain);
end;

function TStep.offset_str: string;
begin
  Result:=IntToStr(offset);
end;

function TStep.id: LongWord;
var buf: string;
begin
  // if any of this change we consider it another step
  buf:=trim(description)+frtype_str+exposure_str+binning_str+filter_str;
  if CanSetGainOffset then begin
    if hasGain or hasGainISO then buf:=buf+gain_str;
    if hasOffset then buf:=buf+offset_str;
  end;
  result:=Hash(buf);
end;

end.

