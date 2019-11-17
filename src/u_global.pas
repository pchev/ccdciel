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

uses u_ccdconfig, dynlibs, LMessages,
  Classes, SysUtils,LCLType;

const
  DomeOpenActionNum=5;
  DomeCloseActionNum=5;

type
  TNotifyMsg = procedure(msg:string; level: integer=1) of object;
  TNotifyStr = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TNotifyBoolConst= procedure(v: boolean) of object;
  TNotifyBool= procedure(var v: boolean) of object;

  TDevInterface = (INDI, ASCOM, INCAMERA, INTELESCOPE, ASCOMREST);
  TFrameType =(LIGHT, BIAS, DARK, FLAT);
  TFlatType=(ftNone,ftSKY,ftDome);
  TAutoguiderType=(agPHD,agLINGUIDER,agNONE,agDITHER);
  TAutoguiderState=(GUIDER_DISCONNECTED,GUIDER_IDLE,GUIDER_GUIDING,GUIDER_BUSY,GUIDER_ALERT);
  TPlanetariumType=(CDC, SAMP, HNSKY);
  TEqmodAlign=(alADDPOINT,alSTDSYNC,alUNSUPPORTED);
  TBayerMode=(bayerGR,bayerRG,bayerBG,bayerGB,bayerCamera);
  TAutofocusMode=(afVcurve,afDynamic,afIterative,afNone);
  TAutofocusVcurveStep=(vcsStartL,vcsStartR,vcsNearL,vcsNearR,vcsCheckL,vcsCheckR,vcsFocusL,vcsFocusR);
  TAutofocusDynamicStep=(afdStart,afdMeasure,afdEnd);
  TIndiTransfert=(itNetwork,itDisk);
  TSubDirList=(sdSeq,sdFrt,sdObj,sdStep,sdExp,sdBin,sdDate,sdNight);
  TFilenameList=(fnObj,fnFilter,fnExp,fnBin,fnTemp,fnDate,fnGain);
  TSafetyAction=(safNothing,safShowPrompt,safAbortSequence,safStopTelescope,safParkTelescope,safStopDomeSlaving,safParkDome,safCloseDome,safWarmCamera,safAutoguiderShutdown,safPlanetariumShutdown,safExternalCommand,safExitProgram);
  TDomeOpenAction=(dopNothing,dopOpenDome,dopUnparkdome,dopUnparkTelescope,dopStartTelescope,dopStartdomeSlaving);
  TDomeCloseAction=(dclNothing,dclStopTelescope,dclParkTelescope,dclStopDomeSlaving,dclParkDome,dclCloseDome);
  TDomeOpenActions=array[0..DomeOpenActionNum-1] of TDomeOpenAction;
  TDomeCloseActions=array[0..DomeCloseActionNum-1] of TDomeCloseAction;

  coordvector = array[1..3] of double;
  rotmatrix = array[1..3, 1..3] of double;

  TBpm=array[1..1000]of array[1..2] of integer;

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

  TScriptDir = Class(TObject)
               public
                 path: string;
               end;

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
              exposure : double;
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
              frtype: TFrameType;
              description: string;
              constructor Create;
              procedure Assign(Source: Tstep);
              function exposure_str: string;
              function count_str: string;
              function filter_str: string;
              function binning_str: string;
              function frtype_str: string;
              function description_str: string;
              function dithercount_str: string;
              function autofocuscount_str: string;
            end;

  TStepDone = array of integer;

  TTarget = Class(TObject)
              public
              objectname, planname, path: shortstring;
              starttime,endtime,startmeridian,endmeridian,ra,de,pa: double;
              startrise,endset,darknight,skip: boolean;
              repeatcount,repeatdone: integer;
              FlatBinX,FlatBinY,FlatCount: integer;
              FlatGain: integer;
              FlatFilters: shortstring;
              preview,astrometrypointing,updatecoord,inplaceautofocus,autoguiding: boolean;
              delay, previewexposure: double;
              plan :TComponent;
              DoneList: TStepDone;
              constructor Create;
              destructor Destroy; override;
              procedure Assign(Source: TTarget);
              function previewexposure_str: string;
              function delay_str: string;
              function repeatcount_str: string;
            end;

  TFocusStar = record
                 ra,de: double;
                 id: string;
               end;

  Thorizonlist = array [0..361] of single;


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

  {$i revision.inc}

const
  ccdcielver = '0.9.65';
  ccdciel_version='Version beta '+ccdcielver;
  TargetFileVersion = 4;
  Maxclient = 100;
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
  jd2000 = 2451545.0;
  abek = secarc * 20.49552;  // aberration constant
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord:double=-9999;
  NullInt: integer=-9999;
  Binning0 = '1x1';
  dateiso = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';
  dateisoshort = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  f0 = '0';
  f1 = '0.0';
  f2 = '0.00';
  f3 = '0.000';
  f4 = '0.0000';
  f5 = '0.00000';
  f6 = '0.000000';
  e6 = '+0.000000E+00;-0.000000E+00;+0.000000E+00';
  f1mc= '+0.0h;-0.0h; ';
  MeridianCrossing='MC';
  b80 ='                                                                                ';
  FocusDirIn=true;
  FocusDirOut=false;
  FrameName: array[0..ord(high(TFrameType))] of string =('Light   ','Bias    ','Dark    ','Flat    '); // add space to header as INDI do
  FlatTimeName: array[0..1] of string=('Dusk','Dawn');
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
  LM_CCDCIEL=LM_USER + 1;
  M_AutoguiderStatusChange=1000;
  M_AutoguiderMessage=1001;
  M_AstrometryDone=1100;
  MaxCmdArg = 10;
  MaxScriptDir=2;
  MaxMenulevel=10;
  MaxFilter=20;
  WaitResponseTime=120;
  ZoomMax=15;
  msgOK = 'OK!';
  msgFailed = 'Failed!';
  ldeg = 'd';
  lmin = 'm';
  lsec = 's';
  sdeg = #$C2+#$B0;
  smin = '''';
  ssec = '"';
  ellipsis = '...';
  URL_DOWNLOAD='https://sourceforge.net/projects/ccdciel/files/';
  URL_BUGREPORT='https://www.ap-i.net/mantis/set_project.php?project_id=3';
  URL_ONLINEHELP='https://www.ap-i.net/ccdciel/en/documentation/start';
  URL_USERGROUP='https://groups.io/g/ccdciel';
  URL_PROGRAMSTATUS='http://localhost:3277';
  SkyFlatTxt='SkyFlat';
  ScriptTxt='Script';
  SubDirCount=8;
  FileNameCount=7;
  MinFrameSize=5;
  SafetyActionNum=15;
  encryptpwd='m=Nrv"wE+W^RA?$b:]w<!t1v]pcTT>3$B?3";~OG9\7$,n[~8KLaUrfCgvRh$=DnlXK]Vxr^0!.HAA';

  {$ifdef linux}
    SharedDir = '../share/ccdciel';
    defCapturePath='/tmp';
    defTransfertPath='/ramdisk';
    libwcs = 'libpaswcs.so.1';
    libz = 'libz.so.1';
  {$endif}
  {$ifdef darwin}
    SharedDir = './';
    defCapturePath='/tmp';
    defTransfertPath='/tmp';
    libwcs = 'libccdcielwcs.dylib';
    libz = 'libz.dylib';
  {$endif}
  {$ifdef mswindows}
    SharedDir = '.\';
    defCapturePath='C:\';
    defTransfertPath='C:\';
    libwcs = 'libccdcielwcs.dll';
    libz = 'zlib1.dll';
  {$endif}
  {$ifdef darwin}
    OpenFileCMD: string = 'open';
  {$else}
    OpenFileCMD: string = 'xdg-open';   // default FreeDesktop.org
    {$endif}
  AscomInvalidArchitecture='program with an incorrect format';

var
  onMsgGlobal: TNotifyMsg;
  Appdir,ConfigDir,LogDir,TmpDir,DataDir: UTF8String;
  CameraName,WheelName,FocuserName,RotatorName,MountName,DomeName,WatchdogName,WeatherName,SafetyName: string;
  ConfigDarkFile,cdate: string;
  isAdmin, debug_msg: boolean;
  AllDevicesConnected: boolean;
  ConfirmClose, ScreenScaling, LogToFile: boolean;
  LogLevel: integer;
  ScriptDir: array[1..MaxScriptDir] of TScriptDir;
  config,screenconfig,credentialconfig: TCCDConfig;
  profile: string;
  ProfileFromCommandLine: boolean;
  lang: string;
  FilterList,BinningList,ReadoutList: TStringList;
  FilterOffset: array [0..MaxFilter] of integer;
  FilterExpFact: array [0..MaxFilter] of double;
  Filter0: string;
  CurrentFilterOffset: integer;
  filteroffset_initialized: boolean;
  ReadoutModeCapture,ReadoutModePreview,ReadoutModeFocus,ReadoutModeAstrometry: integer;
  compile_time, compile_version, compile_system, lclver: string;
  CurrentSequenceFile: string;
  DitherPixel, SettlePixel, DitherWaitTime: double;
  DitherRAonly: boolean;
  SettleMinTime, SettleMaxTime, CalibrationDelay: integer;
  MeridianOption,MinutesPastMeridian, MinutesPastMeridianMin, MeridianFlipPauseTimeout: integer;
  MeridianFlipPauseBefore, MeridianFlipPauseAfter,MeridianFlipCalibrate,MeridianFlipAutofocus,MeridianFlipStopSlaving: boolean;
  astrometryResolver: integer;
  OrigX, OrigY,img_Height,img_Width : integer;
  ImgFrameX,ImgFrameY,ImgFrameW,ImgFrameH: integer;
  ImgScale0,ImgPixRatio: double;
  ImgZoom,ZoomMin: double;
  ScrWidth,ScrHeigth: integer;
  MaxADU, ClippingOverflow, ClippingUnderflow: double;
  MsgHandle: THandle;
  ObsLongitude, ObsLatitude, ObsElevation, ObsTimeZone: double;
  BayerColor: boolean;
  DefaultBayerMode:TBayerMode;
  RedBalance,GreenBalance,BlueBalance: double;
  MaxVideoPreviewRate: integer;
  TemperatureScale: integer;
  TempLabel: string;
  TemperatureSlope: double;
  FocuserTemp, FocuserLastTemp, AutofocusLastTemp, FocuserTempCoeff, AutofocusTempChange, AutoFocusLastTime: double;
  FocuserPositionMin, FocuserPositionMax: integer;
  Starwindow,Focuswindow: integer;
  AutofocusMode:TAutofocusMode;
  AutofocusMinSpeed,AutofocusMaxSpeed,AutofocusNearNum,FocuserDelay,AutofocusBinning,AutofocusPeriod: integer;
  AutofocusStartHFD,AutofocusNearHFD: double;
  AutofocusExposure,AutofocusExposureFact:double;
  AutofocusMoveDir: boolean;
  PosStartL,PosStartR,PosNearL,PosNearR,PosFocus,AutofocusVcNum,AutofocusVcSkipNum,VcCenterpos,VcHalfwidth,VcNsteps:integer;
  AutofocusVc: array[0..100]of array[1..2] of double;
  AutofocusVcDir, AutofocusSlippageCorrection: boolean;
  AutofocusVcSlopeL,AutofocusVcSlopeR,AutofocusVcPID,AutofocusVcpiL,AutofocusVcpiR: double;
  AutofocusVcStep:TAutofocusVcurveStep;
  AutofocusDynamicMovement,AutofocusDynamicNumPoint,AutofocusVcFilterOffset,AutofocusSlippageOffset: integer;
  AutofocusDynamicStep:TAutofocusDynamicStep;
  AutofocusVcCheckNum: integer;
  AutofocusVcCheckHFDlist: array of double;
  AutofocusVcTemp, AutofocusVcTemp1, AutofocusVcTemp2: double;
  AutofocusInPlace, InplaceAutofocus, AutofocusPauseGuider: boolean;
  AutofocusStarList: TArrayDouble2;
  CancelAutofocus, Autofocusing, TerminateFocuserCalibration: Boolean;
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
  NutMAT: rotmatrix;
  ConfigExpEarlyStart, EarlyNextExposure, SkipEarlyExposure: boolean;
  FlatAutoExposure,FlatWaitDusk,FlatWaitDawn,AdjustDomeFlat,DomeFlatTelescopeSlew,DomeFlatSetLight,AdjustFlatLight: boolean;
  FlatType: TFlatType;
  FlatMinExp,FlatMaxExp,DomeFlatTelescopeAz,DomeFlatTelescopeAlt: double;
  FlatLevelMin,FlatLevelMax: integer;
  FlatSlewTime: TDateTime;
  DomeFlatSetLightON,DomeFlatSetLightOFF: string;
  DomeNoSafetyCheck: boolean;
  SubDirOpt: array[0..SubDirCount-1] of TSubDirList;
  SubDirActive: array[0..SubDirCount-1] of Boolean;
  FilenameOpt: array[0..FileNameCount-1] of TFilenameList;
  FilenameActive: array[0..FileNameCount-1] of Boolean;
  FilenameSep: String;
  FileSequenceWidth: integer;
  hasGain, hasGainISO: boolean;
  ISOList: TStringList;
  Gain,GainMin,GainMax: integer;
  SubDirName: array[0..SubDirCount-1] of string;
  FilenameName: array[0..FileNameCount-1] of string;
  CurrentSeqName, CurrentTargetName, CurrentStepName: string;
  CurrentStepNum,CurrentDoneCount: integer;
  WeatherPauseCapture,WeatherCapturePaused,WeatherPauseCanceled,WeatherCancelRestart: boolean;
  WeatherRestartDelay: integer;
  MeasureNewImage: boolean;
  PauseSequence: boolean;
  DummyDouble: double;
  DummyBool: boolean;
  DevInterfaceName: array[0..4] of string=('INDI','ASCOM','In camera','In mount','ASCOM Alpaca');
  ProtocolName: array[0..1] of string=('http:','https:');
  CheckRecenterTarget,NeedRecenterTarget,RecenteringTarget,CheckRecenterBusy: boolean;
  AstrometryTimeout,RecenterTargetDistance,SlewPrecision: double;
  PolarAlignmentOverlay: boolean;
  PolarAlignmentOverlayOffsetX,PolarAlignmentOverlayOffsetY: double;

  procedure globalmsg(str:string);

implementation

class operator TNumRange.=(a,b : TNumRange) : Boolean;
begin
  result:=(a.min=b.min)and(a.max=b.max)and(a.step=b.step);
end;


procedure globalmsg(str:string);
begin
  if Assigned(onMsgGlobal) then onMsgGlobal(str);
end;

////////////////////  TTarget  /////////////////////////////

constructor TTarget.Create;
begin
  inherited Create;
  plan:=nil;
  objectname:='None';
  planname:='';
  path:='';
  starttime:=NullCoord;
  endtime:=NullCoord;
  startmeridian:=NullCoord;
  endmeridian:=NullCoord;
  ra:=NullCoord;
  de:=NullCoord;
  pa:=NullCoord;
  astrometrypointing:=false;
  updatecoord:=false;
  inplaceautofocus:=AutofocusInPlace;
  autoguiding:=false;
  repeatcount:=1;
  repeatdone:=0;
  preview:=False;
  delay:=1;
  previewexposure:=1;
  darknight:=false;
  skip:=false;
  SetLength(DoneList,0);
end;

destructor TTarget.Destroy;
begin
  try
  if (plan<>nil) then FreeAndNil(plan);
  except
  end;
  SetLength(DoneList,0);
  Inherited Destroy;
end;

procedure TTarget.Assign(Source: TTarget);
var i: integer;
begin
  objectname:=Source.objectname;
  planname:=Source.planname;
  path:=Source.path;
  if plan<>nil then FreeAndNil(plan);
  plan:=Source.plan;
  starttime:=Source.starttime;
  endtime:=Source.endtime;
  startrise:=Source.startrise;
  endset:=Source.endset;
  startmeridian:=Source.startmeridian;
  endmeridian:=Source.endmeridian;
  ra:=Source.ra;
  de:=Source.de;
  pa:=Source.pa;
  astrometrypointing:=source.astrometrypointing;
  updatecoord:=Source.updatecoord;
  repeatcount:=Source.repeatcount;
  repeatdone:=Source.repeatdone;
  inplaceautofocus:=Source.inplaceautofocus;
  autoguiding:=Source.autoguiding;
  preview:=Source.preview;
  delay:=Source.delay;
  previewexposure:=Source.previewexposure;
  FlatCount:=Source.FlatCount;
  FlatBinX:=Source.FlatBinX;
  FlatBinY:=Source.FlatBinY;
  FlatGain:=Source.FlatGain;
  FlatFilters:=Source.FlatFilters;
  darknight:=Source.darknight;
  skip:=Source.skip;
  SetLength(DoneList,Length(Source.DoneList));
  for i:=0 to Length(Source.DoneList)-1 do DoneList[i]:=Source.DoneList[i];
end;

function TTarget.previewexposure_str: string;
begin
 Result:=FloatToStr(previewexposure);
end;

function TTarget.delay_str: string;
begin
  Result:=FloatToStr(delay);
end;

function TTarget.repeatcount_str: string;
begin
  Result:=IntToStr(repeatcount);
end;

////////////////////  TStep  /////////////////////////////

constructor TStep.Create;
begin
  exposure:=1;
  count:=1;
  donecount:=0;
  filter:=0;
  binx:=1;
  biny:=1;
  frtype:=LIGHT;
  dither:=false;
  dithercount:=1;
  autofocusstart:=false;
  autofocus:=false;
  autofocuscount:=10;
  description:='Step1';
end;

procedure TStep.Assign(Source: Tstep);
begin
  exposure:=Source.exposure;
  count:=Source.count;
  donecount:=Source.donecount;
  filter:=Source.filter;
  binx:=Source.binx;
  biny:=Source.biny;
  gain:=Source.gain;
  frtype:=Source.frtype;
  dither:=Source.dither;
  dithercount:=Source.dithercount;
  autofocusstart:=Source.autofocusstart;
  autofocus:=Source.autofocus;
  autofocuscount:=Source.autofocuscount;
  description:=Source.description;
end;

function TStep.exposure_str: string;
begin
 Result:=FloatToStr(exposure);
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
  i:=ord(frtype);
  if (i<0)or(i>ord(High(frtype))) then
    Result:=''
  else
    Result:=FrameName[ord(frtype)];
end;

function TStep.description_str: string;
begin
  Result:=description;
end;


end.

