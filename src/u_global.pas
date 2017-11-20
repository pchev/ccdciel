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
  Classes, SysUtils;

type
  TNotifyMsg = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TNotifyBool= procedure(var v: boolean) of object;

  TDevInterface = (INDI, ASCOM, INCAMERA, INTELESCOPE);
  TFrameType =(LIGHT, BIAS, DARK, FLAT);
  TFlatType=(ftNone,ftSKY,ftDome);
  TAutoguiderType=(PHD,LINGUIDER);
  TAutoguiderState=(GUIDER_DISCONNECTED,GUIDER_IDLE,GUIDER_GUIDING,GUIDER_BUSY,GUIDER_ALERT);
  TPlanetariumType=(CDC, SAMP, HNSKY);
  TEqmodAlign=(alADDPOINT,alSTDSYNC,alUNSUPPORTED);
  TBayerMode=(bayerGR,bayerRG,bayerBG,bayerGB);
  TAutofocusMode=(afVcurve,afMean,afIterative,afNone);
  TAutofocusVcurveStep=(vcsStartL,vcsStartR,vcsNearL,vcsNearR,vcsCheckL,vcsCheckR,vcsFocusL,vcsFocusR);
  TAutofocusMeanStep=(afmStart,afmMeasure,afmEnd);
  TIndiTransfert=(itNetwork,itDisk);

  coordvector = array[1..3] of double;
  rotmatrix = array[1..3, 1..3] of double;

  TBpm=array[1..1000]of array[1..2] of integer;

  TDouble2 = array[1..2] of double;

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

  TStep   = Class(TObject)
              public
              exposure, delay : double;
              count, repeatcount: integer;
              dither: boolean;
              dithercount: integer;
              autofocusstart: boolean;
              autofocus: boolean;
              autofocuscount: integer;
              filter: integer;
              binx,biny: integer;
              frtype: TFrameType;
              description: string;
              constructor Create;
              procedure Assign(Source: Tstep);
              function exposure_str: string;
              function delay_str: string;
              function count_str: string;
              function repeatcount_str: string;
              function filter_str: string;
              function binning_str: string;
              function frtype_str: string;
              function description_str: string;
              function dithercount_str: string;
              function autofocuscount_str: string;
            end;

  TTarget = Class(TObject)
              public
              objectname, planname, path: string;
              starttime,endtime,ra,de,pa: double;
              startrise,endset: boolean;
              repeatcount: integer;
              FlatBinX,FlatBinY,FlatCount: integer;
              FlatFilters: string;
              preview,astrometrypointing,updatecoord,inplaceautofocus: boolean;
              delay, previewexposure: double;
              plan :TComponent;
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
  ccdcielver = '0.9.14';
  ccdciel_version='Version beta '+ccdcielver;
  TargetFileVersion = 2;
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
  jd2000 = 2451545.0;
  abek = secarc * 20.49552;  // aberration constant
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord:double=-9999;
  Filter0 = 'No change';
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
  b80 ='                                                                                ';
  FocusDirIn=true;
  FocusDirOut=false;
  FrameName: array[0..ord(high(TFrameType))] of string =('Light   ','Bias    ','Dark    ','Flat    ');
  FlatTimeName: array[0..1] of string=('Dusk','Dawn');
  ResolverAstrometryNet=0;
  ResolverElbrus=1;
  ResolverNone=2;
  ResolverPlateSolve=3;
  ResolverName: array[0..3] of string =('Astrometry.Net','Elbrus','No resolver','PlateSolve');
  PlanetariumName: array[0..2] of string =('Cartes du Ciel', 'SAMP', 'HNSKY');
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
  URL_DOWNLOAD='https://sourceforge.net/projects/ccdciel/files/';
  URL_BUGREPORT='https://www.ap-i.net/mantis/set_project.php?project_id=3';
  URL_ONLINEHELP='https://www.ap-i.net/ccdciel/en/documentation/start';
  SkyFlatTxt='SkyFlat';
  ScriptTxt='Script';


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

var
  onMsgGlobal: TNotifyMsg;
  Appdir,ConfigDir,LogDir,TmpDir,DataDir: UTF8String;
  CameraName,WheelName,FocuserName,RotatorName,MountName,WatchdogName: string;
  ConfirmClose, ScreenScaling: boolean;
  ScriptDir: array[1..MaxScriptDir] of TScriptDir;
  config: TCCDConfig;
  profile: string;
  FilterList,BinningList: TStringList;
  FilterOffset: array [0..MaxFilter] of integer;
  FilterExpFact: array [0..MaxFilter] of double;
  CurrentFilterOffset: integer;
  filteroffset_initialized: boolean;
  compile_time, compile_version, compile_system, lclver: string;
  DitherPixel, SettlePixel: double;
  DitherRAonly: boolean;
  SettleMinTime, SettleMaxTime, CalibrationDelay: integer;
  MeridianOption,MinutesPastMeridian, MinutesPastMeridianMin, MeridianFlipPauseTimeout: integer;
  MeridianFlipPauseBefore, MeridianFlipPauseAfter,MeridianFlipCalibrate,MeridianFlipAutofocus: boolean;
  astrometryResolver: integer;
  OrigX, OrigY,img_Height,img_Width : integer;
  ImgFrameX,ImgFrameY,ImgFrameW,ImgFrameH: integer;
  ImgScale0,ImgPixRatio: double;
  ImgZoom,ZoomMin: double;
  MsgHandle: THandle;
  ObsLongitude, ObsLatitude, ObsTimeZone: double;
  BayerColor: boolean;
  BayerMode:TBayerMode;
  RedBalance,GreenBalance,BlueBalance: double;
  MaxVideoPreviewRate: integer;
  TemperatureSlope: double;
  FocuserTemp, FocuserLastTemp, FocuserTempCoeff: double;
  FocuserPositionMin, FocuserPositionMax: integer;
  AutofocusMode:TAutofocusMode;
  AutofocusMinSpeed,AutofocusMaxSpeed,AutofocusNearNum,FocuserBacklash,AutofocusBinning: integer;
  AutofocusStartHFD,AutofocusNearHFD: double;
  AutofocusExposure,AutofocusExposureFact:double;
  AutofocusMoveDir: boolean;
  PosStartL,PosStartR,PosNearL,PosNearR,PosFocus,AutofocusVcNum,AutofocusVcSkipNum,VcCenterpos,VcHalfwidth,VcNsteps:integer;
  AutofocusVc: array[0..100]of array[1..2] of double;
  AutofocusVcDir, AutofocusSlippageCorrection: boolean;
  AutofocusVcSlopeL,AutofocusVcSlopeR,AutofocusVcPID,AutofocusVcpiL,AutofocusVcpiR: double;
  AutofocusVcStep:TAutofocusVcurveStep;
  AutofocusMeanMovement,AutofocusMeanNumPoint,AutofocusVcFilterOffset,AutofocusSlippageOffset: integer;
  AutofocusMeanStep:TAutofocusMeanStep;
  AutofocusVcCheckNum: integer;
  AutofocusVcCheckHFDlist: array of double;
  AutofocusVcTemp, AutofocusVcTemp1, AutofocusVcTemp2: double;
  CancelAutofocus, InplaceAutofocus, Autofocusing: Boolean;
  bpm: TBpm;
  bpmNum,bpmX,bpmY,bpmAxis,BPMsigma: integer;
  NFocusStars: integer;
  FocusStars: array of TFocusStar;
  FocusStarsBlacklist: string;
  AutofocusTolerance, AutofocusMinSNR: double;
  WaitTillrunning, cancelWaitTill: boolean;
  horizonlist: Thorizonlist;
  HorizonMax, HorizonMin, ElevationMin: double;
  jdtoday,nutl,nuto,abp,abe,ecl,sunl: double;
  NutMAT: rotmatrix;
  FlatAutoExposure,FlatWaitDusk,FlatWaitDawn: boolean;
  FlatType: TFlatType;
  FlatMinExp,FlatMaxExp: double;
  FlatLevelMin,FlatLevelMax: integer;
  FlatSlewTime: TDateTime;

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
  ra:=NullCoord;
  de:=NullCoord;
  pa:=NullCoord;
  astrometrypointing:=false;
  updatecoord:=false;
  inplaceautofocus:=false;
  repeatcount:=1;
  preview:=False;
  delay:=1;
  previewexposure:=1;
end;

destructor TTarget.Destroy;
begin
  try
  if (plan<>nil) then FreeAndNil(plan);
  except
  end;
  Inherited Destroy;
end;

procedure TTarget.Assign(Source: TTarget);
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
  ra:=Source.ra;
  de:=Source.de;
  pa:=Source.pa;
  astrometrypointing:=source.astrometrypointing;
  updatecoord:=Source.updatecoord;
  repeatcount:=Source.repeatcount;
  inplaceautofocus:=Source.inplaceautofocus;
  preview:=Source.preview;
  delay:=Source.delay;
  previewexposure:=Source.previewexposure;
  FlatCount:=Source.FlatCount;
  FlatBinX:=Source.FlatBinX;
  FlatBinY:=Source.FlatBinY;
  FlatFilters:=Source.FlatFilters;
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
  delay:=1;
  count:=1;
  repeatcount:=1;
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
  delay:=Source.delay;
  count:=Source.count;
  repeatcount:=Source.repeatcount;
  filter:=Source.filter;
  binx:=Source.binx;
  biny:=Source.biny;
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

function TStep.delay_str: string;
begin
  Result:=FloatToStr(delay);
end;

function TStep.count_str: string;
begin
  Result:=IntToStr(count);
end;

function TStep.repeatcount_str: string;
begin
  Result:=IntToStr(repeatcount);
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
  if FilterList.Count=0 then
    Result:=''
  else
    Result:=FilterList[filter];
end;

function TStep.binning_str: string;
begin
  Result:=IntToStr(binx)+'x'+IntToStr(biny);
end;

function TStep.frtype_str: string;
begin
  Result:=FrameName[ord(frtype)];
end;

function TStep.description_str: string;
begin
  Result:=description;
end;


end.

