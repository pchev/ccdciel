unit cu_camera;

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

uses  cu_fits, cu_mount, cu_wheel, cu_focuser, cu_weather, u_global, u_utils, u_refraction,
  indiapi, math, u_translation, cu_waitthread, fu_rotator, cu_rotator,
  LazSysUtils, Classes, Forms, SysUtils, ExtCtrls;

type

TVideoRecordMode=(rmDuration,rmFrame,rmUnlimited);

T_camera = class(TComponent)
  protected
    FCameraInterface: TDevInterface;
    FStatus: TDeviceStatus;
    FWheelStatus: TDeviceStatus;
    FonMsg,FonDeviceMsg: TNotifyMsg;
    FonExposureProgress: TNotifyNum;
    FonFilterChange: TNotifyNum;
    FonFrameChange: TNotifyEvent;
    FonTemperatureChange: TNotifyNum;
    FonCoolerPowerChange: TNotifyNum;
    FonCoolerChange: TNotifyBool;
    FonFnumberChange: TNotifyStr;
    FonGainStatus: TNotifyEvent;
    FonStatusChange: TNotifyEvent;
    FonFilterNameChange: TNotifyEvent;
    FonWheelStatusChange: TNotifyEvent;
    FonNewImage, FonNewExposure: TNotifyEvent;
    FonVideoFrame: TNotifyEvent;
    FonAbortExposure,FonCameraDisconnected: TNotifyEvent;
    FonVideoPreviewChange,FonVideoSizeChange,FonVideoRateChange,FonVideoRecordChange: TNotifyEvent;
    FonFPSChange,FonVideoExposureChange : TNotifyEvent;
    FonEncoderChange: TNotifyEvent;
    FonSequenceInfo: TNotifyEvent;
    FImgStream: TMemoryStream;
    FVideoStream: TMemoryStream;
    FFilterNames: TStringList;
    FObjectName: string;
    Fdevice: string;
    FImageFormat: string;
    FDriverInfo: string;
    FCameraXSize,FCameraYSize: integer;
    FCCDname: string;
    FFits: TFits;
    FStackCount, FStackNum, FStackStarted, FStackOperation: integer;
    FStackExpStart, FStackDate, FStackSaveDir: string;
    FStackAlign, FStackUseDark, FStackUseFlat, FStackDebayer,FStackAllow8bit: boolean;
    FStackAlignX,FStackAlignY,FStackStarX,FStackStarY,FStackAlignRot: double;
    FMount: T_mount;
    Fwheel: T_wheel;
    FFocuser: T_focuser;
    FRotator: T_rotator;
    Ff_Rotator: Tf_rotator;
    FWeather: T_weather;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FhasVideo: boolean;
    FVerticalFlip: boolean;
    FDslr: boolean;
    FASCOMFlipImage: boolean;
    FAddFrames,FSaveFrames,FAlignFrames,FPrepareStack,FStackRotation: boolean;
    FVideoSizes, FVideoRates,FFNumberList,FVideoEncoder:TStringList;
    FTemperatureRampActive, FCancelTemperatureRamp: boolean;
    FIndiTransfert: TIndiTransfert;
    FIndiTransfertDir,FIndiTransfertPrefix: string;
    FhasGain,FhasOffset,FhasGainISO,FCanSetGain,FhasCfaInfo,FhasFnumber,FhasCoolerPower: boolean;
    FUseCameraStartTime,FhasLastExposureStartTime,FhasLastExposureDuration: boolean;
    FGainMin, FGainMax, FOffsetMin, FOffsetMax: integer;
    FISOList: TStringList;
    FhasFastReadout, FhasReadOut: boolean;
    FReadOutList: TStringList;
    Ftimestart,Ftimeend,FMidExposureTime: double;
    FImgNum:PtrInt;
    Fexptime: double;
    FFixPixelRange: boolean;
    FGuideCamera,FFinderCamera: boolean;
    FGuidePixelScale,FGuideLockX,FGuideLockY: double;
    FsequenceRunning, FTargetCoord: boolean;
    FStepTotalCount,FStepRepeatCount: integer;
    FTargetRA,FTargetDE: Double;
    FCoordinateOrigin: string;
    FCameraTimeout: integer;
    WaitExposure, ControlExposureOK: boolean;
    procedure msg(txt: string; level:integer=3);
    procedure NewImage;
    procedure TryNextExposure(Data: PtrInt);
    procedure WriteHeaders(f:TFits; stackresult:Boolean=true; UpdateFromCamera:Boolean=true);
    procedure NewVideoFrame(rawframe: boolean);
    procedure WriteVideoHeader(width,height,naxis,bitpix: integer);
    function GetBinX:integer; virtual; abstract;
    function GetBinY:integer; virtual; abstract;
    function GetVerticalFlip: boolean;
    procedure SetFrametype(f:TFrameType); virtual; abstract;
    function  GetFrametype:TFrameType; virtual; abstract;
    function GetBinXrange:TNumRange; virtual; abstract;
    function GetBinYrange:TNumRange; virtual; abstract;
    function GetExposureRange:TNumRange; virtual; abstract;
    function GetTemperatureRange:TNumRange; virtual; abstract;
    function  GetTemperature: double; virtual; abstract;
    procedure SetTemperature(value:double); virtual; abstract;
    procedure SetTemperatureRamp(value:double);
    procedure SetTemperatureRampAsync(Data: PtrInt);
    function  GetCoolerPower: double; virtual; abstract;
    function  GetCooler: boolean; virtual; abstract;
    procedure SetCooler(value:boolean); virtual; abstract;
    procedure SetFilter(num:integer); virtual; abstract;
    function  GetFilter:integer; virtual; abstract;
    procedure SetFilterNames(value:TStringList); virtual; abstract;
    function GetMaxX: double; virtual; abstract;
    function GetMaxY: double; virtual; abstract;
    function GetMaxADU: double; virtual; abstract;
    function GetPixelSize: double; virtual; abstract;
    function GetPixelSizeX: double; virtual; abstract;
    function GetPixelSizeY: double; virtual; abstract;
    function GetBitperPixel: double; virtual; abstract;
    function GetColor: boolean;  virtual; abstract;
    function GetImageFormat: string; virtual; abstract;
    procedure SetTimeout(num:integer); virtual; abstract;
    function GetVideoPreviewRunning: boolean;  virtual; abstract;
    function GetVideoRecordRunning: boolean;  virtual; abstract;
    function GetMissedFrameCount: cardinal; virtual; abstract;
    function GetVideoRecordDuration:integer; virtual; abstract;
    procedure SetVideoRecordDuration(value:integer); virtual; abstract;
    function GetVideoRecordFrames:integer; virtual; abstract;
    procedure SetVideoRecordFrames(value:integer); virtual; abstract;
    function GetVideoSize:string;virtual; abstract;
    procedure SetVideoSize(value:string); virtual; abstract;
    function GetVideoRate:string;virtual; abstract;
    procedure SetVideoRate(value:string); virtual; abstract;
    function GetFPS:double;virtual; abstract;
    function GetVideoRecordDir:string; virtual; abstract;
    procedure SetVideoRecordDir(value:string); virtual; abstract;
    function GetVideoRecordFile:string; virtual; abstract;
    procedure SetVideoRecordFile(value:string); virtual; abstract;
    function GetVideoExposure:integer; virtual; abstract;
    function GetVideoGain:integer; virtual; abstract;
    function GetVideoGamma:integer; virtual; abstract;
    function GetVideoBrightness:integer; virtual; abstract;
    procedure SetVideoExposure(value:integer); virtual; abstract;
    procedure SetVideoGain(value:integer); virtual; abstract;
    procedure SetVideoGamma(value:integer); virtual; abstract;
    procedure SetVideoBrightness(value:integer); virtual; abstract;
    function GetVideoExposureRange:TNumRange; virtual; abstract;
    function GetVideoGainRange:TNumRange; virtual; abstract;
    function GetVideoGammaRange:TNumRange; virtual; abstract;
    function GetVideoBrightnessRange:TNumRange; virtual; abstract;
    function GetVideoPreviewDivisor:integer; virtual; abstract;
    procedure SetVideoPreviewDivisor(value:integer); virtual; abstract;
    function GetVideoPreviewLimit:integer; virtual; abstract;
    procedure SetVideoPreviewLimit(value:integer); virtual; abstract;
    procedure SetGain(value: integer); virtual; abstract;
    function GetGain: integer; virtual; abstract;
    procedure SetOffset(value: integer); virtual; abstract;
    function GetOffset: integer; virtual; abstract;
    procedure SetReadOutMode(value: integer); virtual; abstract;
    function GetReadOutMode: integer; virtual; abstract;
    procedure SetFnumber(value: string); virtual; abstract;
    function GetFnumber: string; virtual; abstract;
    function GetStreamingExposureRange:TNumRange; virtual; abstract;
    function GetStreamingExposure:double; virtual; abstract;
    procedure SetStreamingExposure(value:double); virtual; abstract;
    function GetVideoEncoder: integer; virtual; abstract;
    procedure SetVideoEncoder(value:integer); virtual; abstract;
    function GetFullWellCapacity: double; virtual; abstract;
  private
    lockvideoframe: boolean;
    TempFinal: Double;
    TempNow,TempRamp: double;
    Nstep: integer;
    RampTimer: TTimer;
    FonEndControlExposure: TNotifyEvent;
    FRunScript: TRunScript;
    FSolve: TSolve;
    procedure RampTimerTimer(Sender: TObject);
    procedure EndExposure(Sender: TObject);
    function Solve(f:TFits; out ra,de,pa,scale: double): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  ControlExposure(exp:double; pbinx,pbiny: integer; frmt:TFrameType; preadoutmode,pgain,poffset:integer; quiet:boolean=false):boolean;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
    function  InitFrameType(t: integer):TFrameType;
    Procedure SetBinning(binX,binY: integer); virtual; abstract;
    Procedure StartExposure(exptime: double); virtual; abstract;
    procedure RestartExposure; virtual; abstract;
    Procedure AbortExposure; virtual; abstract;
    procedure AbortExposureButNotSequence; virtual; abstract;
    procedure SetFrame(x,y,width,height: integer); virtual; abstract;
    procedure GetFrame(out x,y,width,height: integer; refresh:boolean=false); virtual; abstract;
    procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); virtual; abstract;
    procedure ResetFrame; virtual; abstract;
    procedure GetStreamFrame(out x,y,width,height: integer); virtual; abstract;
    procedure CfaInfo(out OffsetX, OffsetY: integer; out CType: string);  virtual; abstract;
    function  CheckGain: boolean; virtual; abstract;
    function  CheckOffset: boolean; virtual; abstract;
    Procedure SetActiveDevices(focuser,filters,telescope: string); virtual; abstract;
    procedure StartVideoPreview; virtual; abstract;
    procedure StopVideoPreview; virtual; abstract;
    procedure StartVideoRecord(mode:TVideoRecordMode); virtual; abstract;
    procedure StopVideoRecord; virtual; abstract;
    property DeviceName: string read FDevice;
    property DriverInfo: string read FDriverInfo;
    property CCDname: string read FCCDname;
    property IsDSLR: boolean read FDslr;
    property Fits: TFits read FFits write FFits;
    property Mount: T_mount read FMount write FMount;
    property Wheel: T_wheel read Fwheel write Fwheel;
    property Focuser: T_focuser read FFocuser write FFocuser;
    property Rotator: T_rotator read FRotator write FRotator;
    property f_Rotator: Tf_rotator read Ff_Rotator write Ff_Rotator;
    property Weather: T_weather read FWeather write FWeather;
    property ObjectName: string read FObjectName write FObjectName;
    property CameraInterface: TDevInterface read FCameraInterface;
    property Status: TDeviceStatus read FStatus;
    property WheelStatus: TDeviceStatus read FWheelStatus;
    property ImgStream: TMemoryStream read FImgStream;
    property ImageFormat: string read GetImageFormat;
    property AddFrames: boolean read FAddFrames write FAddFrames;
    property PrepareStack: boolean read FPrepareStack write FPrepareStack;
    property SaveFrames: boolean read FSaveFrames write FSaveFrames;
    property AlignFrames: boolean read FAlignFrames write FAlignFrames;
    property StackRotation: boolean read FStackRotation write FStackRotation;
    property StackOperation: integer read FStackOperation write FStackOperation;
    property StackUseDark: boolean read FStackUseDark write FStackUseDark;
    property StackUseFlat: boolean read FStackUseFlat write FStackUseFlat;
    property StackDebayer: boolean read FStackDebayer write FStackDebayer;
    property StackAllow8bit: boolean read FStackAllow8bit write FStackAllow8bit;
    property VerticalFlip: boolean read GetVerticalFlip;
    property ASCOMFlipImage: boolean read FASCOMFlipImage write FASCOMFlipImage;
    property hasVideo: boolean read FhasVideo;
    property VideoStream: TMemoryStream read FVideoStream;
    property VideoPreviewRunning: boolean read GetVideoPreviewRunning;
    property VideoRecordRunning: boolean read GetVideoRecordRunning;
    property MissedFrameCount: Cardinal read GetMissedFrameCount;
    property VideoRecordDuration: integer read GetVideoRecordDuration write SetVideoRecordDuration;
    property VideoRecordFrames: integer read GetVideoRecordFrames write SetVideoRecordFrames;
    property VideoSizes:TStringList read FVideoSizes;
    property VideoRates:TStringList read FVideoRates;
    property VideoSize:string read GetVideoSize write SetVideoSize;
    property VideoRate:string read GetVideoRate write SetVideoRate;
    property VideoRecordDir:string read GetVideoRecordDir write SetVideoRecordDir;
    property VideoRecordFile:string read GetVideoRecordFile write SetVideoRecordFile;
    property VideoExposure: integer read GetVideoExposure write SetVideoExposure;
    property VideoExposureRange: TNumRange read GetVideoExposureRange;
    property StreamingExposure: double read GetStreamingExposure write SetStreamingExposure;
    property StreamingExposureRange: TNumRange read GetStreamingExposureRange;
    property VideoGain: integer read GetVideoGain write SetVideoGain;
    property VideoGainRange: TNumRange read GetVideoGainRange;
    property VideoGamma: integer read GetVideoGamma write SetVideoGamma;
    property VideoGammaRange: TNumRange read GetVideoGammaRange;
    property VideoBrightness: integer read GetVideoBrightness write SetVideoBrightness;
    property VideoBrightnessRange: TNumRange read GetVideoBrightnessRange;
    property VideoPreviewDivisor: integer read GetVideoPreviewDivisor write SetVideoPreviewDivisor;
    property VideoPreviewLimit: integer read GetVideoPreviewLimit write SetVideoPreviewLimit;
    property CoolerPower: Double read GetCoolerPower;
    property Cooler: boolean read GetCooler write SetCooler;
    property Temperature: double read GetTemperature write SetTemperatureRamp;
    property TemperatureRampActive: Boolean read FTemperatureRampActive;
    property BinX: Integer read getBinX;
    property BinY: Integer read getBinY;
    property FrameType: TFrameType read GetFrametype write SetFrametype;
    property BinXrange: TNumRange read GetbinXrange;
    property BinYrange: TNumRange read GetbinYrange;
    property ExposureRange: TNumRange read GetExposureRange;
    property TemperatureRange: TNumRange read GetTemperatureRange;
    property MaxX: double read GetMaxX;
    property MaxY: double read GetMaxY;
    property MaxADU: double read GetMaxADU;
    property PixelSize: double read GetPixelSize;
    property PixelSizeX: double read GetPixelSizeX;
    property PixelSizeY: double read GetPixelSizeY;
    property BitperPixel: double read GetBitperPixel;
    property Color: boolean read GetColor;
    property FPS: double read GetFPS;
    property StackNum: integer read FStackNum write FStackNum;
    property StackCount: integer read FStackCount;
    property StackStarted: integer read FStackStarted write FStackStarted;
    property Filter: integer read GetFilter write SetFilter;
    property FilterNames: TStringList read FFilterNames write SetFilterNames;
    property Timeout: integer read FTimeout write SetTimeout;
    property AutoLoadConfig: boolean read FAutoLoadConfig write FAutoLoadConfig;
    property IndiTransfert: TIndiTransfert read FIndiTransfert write FIndiTransfert;
    property IndiTransfertDir: string read FIndiTransfertDir write FIndiTransfertDir;
    property Gain: integer read GetGain write SetGain;
    property CanSetGain: boolean read FCanSetGain write FCanSetGain;
    property hasGain: boolean read FhasGain;
    property GainMin: integer read FGainMin;
    property GainMax: integer read FGainMax;
    property hasGainISO: boolean read FhasGainISO;
    property ISOList: TStringList read FISOList;
    property Offset: integer read GetOffset write SetOffset;
    property hasOffset: boolean read FhasOffset;
    property OffsetMin: integer read FOffsetMin;
    property OffsetMax: integer read FOffsetMax;
    property FnumberList: TstringList read FFNumberList;
    property LastExposureTime:double read Fexptime;
    property hasCfaInfo: boolean read FhasCfaInfo;
    property hasReadOut: boolean read FhasReadOut;
    property ReadOutList: TStringList read FReadOutList;
    property ReadOutMode: integer read GetReadOutMode write SetReadOutMode;
    property hasFnumber: boolean read FhasFnumber;
    property Fnumber: string read GetFnumber write SetFnumber;
    property UseCameraStartTime: boolean read FUseCameraStartTime write FUseCameraStartTime;
    property VideoEncoders: TStringList read FVideoEncoder;
    property VideoEncoder: Integer read GetVideoEncoder write SetVideoEncoder;
    property FixPixelRange: boolean read FFixPixelRange write FFixPixelRange;
    property FinderCamera: boolean read FFinderCamera write FFinderCamera;
    property GuideCamera: boolean read FGuideCamera write FGuideCamera;
    property GuidePixelScale: double read FGuidePixelScale write FGuidePixelScale;
    property GuideLockX: double read FGuideLockX write FGuideLockX;
    property GuideLockY: double read FGuideLockY write FGuideLockY;
    property FullWellCapacity: double read GetFullWellCapacity;
    property onMsg: TNotifyMsg read FonMsg write FonMsg;
    property onDeviceMsg: TNotifyMsg read FonDeviceMsg write FonDeviceMsg;
    property onExposureProgress: TNotifyNum read FonExposureProgress write FonExposureProgress;
    property onTemperatureChange: TNotifyNum read FonTemperatureChange write FonTemperatureChange;
    property onCoolerPowerChange: TNotifyNum read FonCoolerPowerChange write FonCoolerPowerChange;
    property onCoolerChange: TNotifyBool read FonCoolerChange write FonCoolerChange;
    property onFnumberChange: TNotifyStr read FonFnumberChange write FonFnumberChange;
    property onFilterChange: TNotifyNum read FonFilterChange write FonFilterChange;
    property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
    property onGainStatus: TNotifyEvent read FonGainStatus write FonGainStatus;
    property onFrameChange: TNotifyEvent read FonFrameChange write FonFrameChange;
    property onFilterNameChange: TNotifyEvent read FonFilterNameChange write FonFilterNameChange;
    property onWheelStatusChange: TNotifyEvent read FonWheelStatusChange write FonWheelStatusChange;
    property onNewImage: TNotifyEvent read FonNewImage write FonNewImage;
    property onNewExposure: TNotifyEvent read FonNewExposure write FonNewExposure;
    property onVideoFrame: TNotifyEvent read FonVideoFrame write FonVideoFrame;
    property onCameraDisconnected: TNotifyEvent read FonCameraDisconnected write FonCameraDisconnected;
    property onAbortExposure: TNotifyEvent read FonAbortExposure write FonAbortExposure;
    property onVideoPreviewChange: TNotifyEvent read FonVideoPreviewChange write FonVideoPreviewChange;
    property onVideoRecordChange: TNotifyEvent read FonVideoRecordChange write FonVideoRecordChange;
    property onVideoSizeChange: TNotifyEvent read FonVideoSizeChange write FonVideoSizeChange;
    property onVideoRateChange: TNotifyEvent read FonVideoRateChange write FonVideoRateChange;
    property onFPSChange: TNotifyEvent read FonFPSChange write FonFPSChange;
    property onVideoExposureChange: TNotifyEvent read FonVideoExposureChange write FonVideoExposureChange;
    property onEncoderChange: TNotifyEvent read FonEncoderChange write FonEncoderChange;
    property onSequenceInfo: TNotifyEvent read FonSequenceInfo write FonSequenceInfo;
    property SequenceRunning: Boolean read FsequenceRunning write FsequenceRunning;
    property StepTotalCount: Integer read FStepTotalCount write FStepTotalCount;
    property StepRepeatCount: Integer read FStepRepeatCount write FStepRepeatCount;
    property TargetCoord: Boolean read FTargetCoord write FTargetCoord;
    property TargetRA: Double read FTargetRA write FTargetRA;
    property TargetDE: Double read FTargetDE write FTargetDE;
    property CameraTimeout: integer read FCameraTimeout write FCameraTimeout;
    property onEndControlExposure: TNotifyEvent read FonEndControlExposure write FonEndControlExposure;
    property onRunScript: TRunScript read FRunScript write FRunScript;
    property onSolve: TSolve read FSolve write FSolve;
end;


implementation

constructor T_camera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCameraTimeout:=60; // in seconds, must be enough to download image from any camera
  FImgNum:=0;
  FTimeOut:=100;
  FIndiTransfert:=itNetwork;
  FIndiTransfertDir:=defTransfertPath;
  FIndiTransfertPrefix:='ccdciel_tmp';
  FVerticalFlip:=false;
  FASCOMFlipImage:=false;
  FStatus := devDisconnected;
  FDriverInfo := '';
  FCCDname:='';
  FDslr:=false;
  FFilterNames:=TStringList.Create;
  FImgStream:=TMemoryStream.Create;
  FAddFrames:=false;
  FPrepareStack:=false;
  FAlignFrames:=false;
  FStackUseDark:=false;
  FStackUseFlat:=false;
  FStackDebayer:=false;
  FStackAllow8bit:=false;
  FSaveFrames:=false;
  FStackSaveDir:='';
  FhasVideo:=false;
  FVideoStream:=TMemoryStream.Create;;
  lockvideoframe:=false;
  FVideoSizes:=TStringList.Create;
  FVideoRates:=TStringList.Create;
  FFNumberList:=TStringList.Create;
  FVideoEncoder:=TStringList.Create;
  FTemperatureRampActive:=false;
  FCancelTemperatureRamp:=false;
  FStackCount:=0;
  FStackNum:=-1;
  FISOList:=TStringList.Create;
  FGainMin:=0;
  FGainMax:=0;
  FhasGain:=false;
  FhasGainISO:=false;
  FhasCfaInfo:=false;
  FOffsetMin:=0;
  FOffsetMax:=0;
  FhasOffset:=false;
  FReadOutList:=TStringList.Create;
  FhasFastReadout:=false;
  FhasReadOut:=false;
  FhasFnumber:=false;
  FUseCameraStartTime:=false;
  FhasLastExposureStartTime:=false;
  FhasLastExposureDuration:=false;
  FFixPixelRange:=false;
  FImageFormat:='.fits';
  Fexptime:=0;
  FCanSetGain:=false;
  FhasCoolerPower:=false;
  RampTimer:=TTimer.Create(self);
  RampTimer.Enabled:=false;
  RampTimer.Interval:=1000;
  RampTimer.OnTimer:=@RampTimerTimer;
  FGuideCamera:=false;
  FFinderCamera:=false;
  FGuidePixelScale:=-1;
  FGuideLockX:=-1;
  FGuideLockY:=-1;
  FsequenceRunning:=false;
  FStepTotalCount:=1;
  FStepRepeatCount:=1;
  FTargetCoord:=false;
  FTargetRA:=NullCoord;
  FTargetDE:=NullCoord;
  FCoordinateOrigin:='Telescope pointing';
end;

destructor  T_camera.Destroy;
begin
  if FImgStream<>nil then FreeAndNil(FImgStream);
  FFilterNames.Free;
  FVideoStream.Free;
  FVideoSizes.Free;
  FVideoRates.Free;
  FFNumberList.Free;
  FISOList.Free;
  FReadOutList.Free;
  FVideoEncoder.Free;
  inherited Destroy;
end;

procedure T_camera.msg(txt: string; level:integer=3);
begin
 if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
end;

function T_camera.GetVerticalFlip: boolean;
begin
  result:=FVerticalFlip or (not FASCOMFlipImage);
end;

procedure T_camera.SetTemperatureRamp(value:double);
begin
  if TemperatureSlope>0 then begin
    TempFinal:=value;
    Application.QueueAsyncCall(@SetTemperatureRampAsync,0);
  end else begin
    msg(Format(rsSetTemperatu, [formatfloat(f1, TempDisplay(TemperatureScale,value))+TempLabel]));
    SetTemperature(value);
  end;
end;

procedure T_camera.SetTemperatureRampAsync(Data: PtrInt);
var TempStart,tsl: double;
    TempStep: integer;
begin
  if TemperatureScale=0 then
     tsl:=TemperatureSlope
  else
     tsl:=TemperatureSlope*5/9; // F -> C
  TempStep:=round(60.0/tsl);
  if TempStep<1 then TempStep:=1;
  if FTemperatureRampActive then begin
     FCancelTemperatureRamp:=true;
     msg(rsTemperatureR,1);
     exit;
  end;
  msg(Format(rsSetTemperatu2, [formatfloat(f1, TempDisplay(TemperatureScale,TempFinal))+TempLabel]));
  FTemperatureRampActive:=true;
  TempStart:=GetTemperature;
  TempNow:=TempStart;
  if TempFinal>TempStart then
     TempRamp:=1.0
  else
     TempRamp:=-1.0;
  Nstep:=round(abs(TempStart-TempFinal));
  TempNow:=TempNow+TempRamp;
  SetTemperature(TempNow);
  dec(Nstep);
  RampTimer.Interval:=TempStep*1000;
  RampTimer.Enabled:=true;
end;

procedure T_camera.RampTimerTimer(Sender: TObject);
var finished: boolean;
begin
  RampTimer.Enabled:=false;
  finished:=(Nstep<=0)or((TempRamp>0)and(GetCoolerPower<1));
  if finished then begin
    SetTemperature(TempFinal);
    msg(rsSetTemperatu3);
    FTemperatureRampActive:=false;
    if Assigned(FonTemperatureChange) then FonTemperatureChange(GetTemperature);
  end
  else begin
    if FCancelTemperatureRamp then begin
       FCancelTemperatureRamp:=false;
       FTemperatureRampActive:=false;
       msg(rsTemperatureR2,0);
       if Assigned(FonTemperatureChange) then FonTemperatureChange(GetTemperature);
       exit;
    end;
    TempNow:=TempNow+TempRamp;
    SetTemperature(TempNow);
    dec(Nstep);
    RampTimer.Enabled:=true;
  end;
end;

procedure T_camera.NewImage;
var f,fs:TFits;
    n,xi,yi,xc,yc,ri: integer;
    xs,ys,xn,yn,hfd,fwhm,vmax,snr,bg,bgdev,flux,rot,scale,ra,de : double;
    stackok,alok,savebayer: boolean;
    mem: TMemoryStream;
    objectstr,fn: string;
    ft:TFrameType;
    wcs: TcdcWCScoord;
const datefmt = 'yyyy"-"mm"-"dd"T"hh"-"nn"-"ss';
begin
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'NewImage');{$endif}
if FAddFrames then begin  // stack preview frames
  if (FStackNum>0)and(FStackCount>=FStackNum) then FFits.ClearImage;
  if FSaveFrames then begin
    // save original image
    mem:=TMemoryStream.Create;
    FImgStream.Position:=0;
    mem.CopyFrom(FImgStream,FImgStream.Size);
    fs:=TFits.Create(nil);
    fs.onMsg:=onMsg;
    fs.Stream:=mem;
  end;
  // load temporary image
  f:=TFits.Create(nil);
  f.onMsg:=onMsg;
  if FStackUseDark then begin
    f.DarkOn:=true;
    f.DarkFrame:=FFits.DarkFrame;
    f.SetBPM(bpm,bpmNum,bpmX,bpmY,bpmAxis);
  end
  else begin
    f.DarkOn:=false;
    f.SetBPM(bpm,0,0,0,0);
  end;
  if FStackUseFlat then begin
    f.FlatOn:=true;
    f.FlatFrame:=FFits.FlatFrame;
  end
  else begin
    f.FlatOn:=false;
  end;
  savebayer:=BayerColor;
  BayerColor:=FStackDebayer;
  f.DisableBayer:=not FStackDebayer;
  f.Stream:=FImgStream;
  FImgStream:=TMemoryStream.Create;
  // load, subtract dark and debayer
  f.LoadStream;
  f.DarkOn:=false;
  f.SetBPM(bpm,0,0,0,0);
  BayerColor:=savebayer;
  // if debayered, load the new color stream
  if (f.preview_axis<>f.HeaderInfo.naxis)and(f.preview_axis=3) then f.LoadRGB;
  // convert 8bit to 16bit to avoid quick overflow
  if (not FStackAllow8bit)and(f.HeaderInfo.bitpix=8) then
     f.Bitpix8to16;
  // check frame is compatible
  if FFits.SameFormat(f) then begin
     stackok:=true;
     if FStackAlign then begin
        // align frame on ref star or center
        alok:=false;
        if FStackRotation then begin
          alok:=Solve(f,ra,de,rot,scale);
          rot:=deg2rad*(rot-FStackAlignRot);
          wcs.ra:=FStackAlignX*15;
          wcs.dec:=FStackAlignY;
          n:=cdcwcs_sky2xy(@wcs,wcsfits);
          if n=0 then begin
            xn:=wcs.x;
            yn:=wcs.y;
            xs:=xn-(0.5+f.HeaderInfo.naxis1/2);
            ys:=(0.5+f.HeaderInfo.naxis2/2)-yn;
          end
          else
            alok:=false;
        end
        else begin
          rot:=0;
          xi:=round(FStackStarX);
          yi:=round(FStackStarY);
          f.FindStarPos(xi,yi,50,xc,yc,ri,vmax,bg,bgdev);
          if vmax>0 then begin
            f.GetHFD2(xc,yc,2*ri,xn,yn,bg,bgdev,hfd,fwhm,vmax,snr,flux,false);
            if ((hfd>0)and(Undersampled or (hfd>0.7))) and (hfd<10) then begin
              xs:=FStackAlignX-xn;
              ys:=FStackAlignY-yn;
              FStackStarX:=xn;
              FStackStarY:=yn;
              alok:=true;
             end;
          end;
        end;
        if alok then begin
          f.Shift(xs,ys,rot);
          stackok:=true;
        end
        else begin
          stackok:=false;
          msg(rsAlignmentSta,0);
        end;
     end;
     if stackok then begin
       inc(FStackCount);
       case FStackOperation of
        0 : FFits.Math(f,moAdd);                       // add frame
        1 : FFits.Math(f,moRunMean,false,FStackCount); // mean of frames
       end;
       msg(Format('%d frame stacked',[FStackCount]));
     end
     else begin
       msg('frame ignored');
     end;
  end
  else begin
     FStackExpStart:=FormatDateTime(dateiso,Ftimestart);
     FStackDate:=FormatDateTime(datefmt,Ftimestart);
     FFits.Math(f,moAdd,true);  // start a new stack
     FStackCount:=1;
     FStackAlign:=false;
     if FAlignFrames then begin
        if FStackRotation then begin
          FStackAlign:=Solve(FFits,FStackAlignX,FStackAlignY,FStackAlignRot,scale);
          if FStackAlign then msg(Format('Stacking with reference image %s/%s, PA %s',[RAToStr(FStackAlignX),DEToStr(FStackAlignY),FormatFloat(f1,FStackAlignRot)]))
                         else msg('Fail to solve the exposure, no alignment is used');
        end
        else begin
          // search alignment star
          FFits.FindBrightestPixel(FFits.HeaderInfo.naxis1 div 2, FFits.HeaderInfo.naxis2 div 2,min(FFits.HeaderInfo.naxis1,FFits.HeaderInfo.naxis2) div 2,20,xi,yi,vmax);
          if vmax>0 then begin
            FFits.FindStarPos(xi,yi,20,xc,yc,ri,vmax,bg,bgdev);
            if vmax>0 then begin
              FFits.GetHFD2(xc,yc,2*ri,xs,ys,bg,bgdev,hfd,fwhm,vmax,snr,flux,false);
              if ((hfd>0)and(Undersampled or (hfd>0.7))) and (hfd<10) then begin
                 FStackAlign:=true;
                 FStackAlignX:=xs;
                 FStackAlignY:=ys;
                 FStackStarX:=xs;
                 FStackStarY:=ys;
                 msg(Format(rsStackingWith, [inttostr(round(xs)), inttostr(round(FFits.HeaderInfo.naxis2-ys))]));
              end;
            end;
          end;
          if not FStackAlign then msg(rsNoAlignmentS,0);
       end;
     end;
     msg(Format('%d frame stacked',[FStackCount]));
  end;
  // update image
  FFits.Header.Assign(f.Header);
  WriteHeaders(FFits,true,false);
  Ffits.GetFitsInfo;
  f.free;
  if FSaveFrames and (fs<>nil) then begin
    // write header
    WriteHeaders(fs,false,true);
    // save image
    ft:=FrameType;
    if ft=LIGHT then
       objectstr:=SafeFileName(FObjectName)
    else
       objectstr:=trim(FrameName[ord(ft)]);
    if FStackCount=1 then begin
      if Assigned(FonSequenceInfo) then FonSequenceInfo(self);
      FStackSaveDir:=CapturePath(fs,FrameName[ord(ft)],objectstr,FormatFloat(f9v,Fexptime),inttostr(BinX),(objectstr=rsPreview),FsequenceRunning,IsDSLR,FStepTotalCount,FStepRepeatCount);
      FStackSaveDir:=slash(FStackSaveDir)+'stack_'+objectstr+'_'+FStackDate;
      if copy(FStackSaveDir,1,1)='.' then FStackSaveDir:=ExpandFileName(slash(Appdir)+FStackSaveDir);
      ForceDirectories(FStackSaveDir);
      msg('Saving individual frames to '+FStackSaveDir);
    end;
    fn:=CaptureFilename(fs,FStackSaveDir,FrameName[ord(ft)],objectstr,FormatFloat(f9v,Fexptime),inttostr(BinX),false,IsDSLR);
    fn:=slash(FStackSaveDir)+fn+FitsFileExt;
    fs.SaveToFile(fn);
    fs.free;
  end;
  if Assigned(FonNewImage) then FonNewImage(self);
end
else begin  // normal capture
  FStackCount:=0;
  if ImgStream.Size>0 then begin
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'set fits stream');{$endif}
  Ffits.Stream:=FImgStream;
  FImgStream:=TMemoryStream.Create;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'write headers');{$endif}
  WriteHeaders(FFits);
  end;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'process new image');{$endif}
  if Assigned(FonNewImage) then FonNewImage(self);
end;
end;

procedure T_camera.TryNextExposure(Data: PtrInt);
begin
 if EarlyNextExposure and Assigned(FonNewExposure) and(not Autofocusing) then begin
   if CameraProcessingImage and (CameraProcessingNum=FImgNum-1) then begin
     if debug_msg then msg('wait CameraProcessingImage');
     WaitExecute(10,@TryNextExposure,Data);
   end
   else begin
     CameraProcessingImage:=true;
     CameraProcessingNum:=data;
     FonNewExposure(self);
   end;
 end;
end;

procedure T_camera.WriteHeaders(f:TFits; stackresult:Boolean=true; UpdateFromCamera:Boolean=true);
var origin,observer,telname,instrum,objname,siso,CType,roworder,buf: string;
    focal_length,pixscale1,pixscale2,ccdtemp,st,ra,de,fstop,shutter,multr,multg,multb: double;
    hbitpix,hnaxis,hnaxis1,hnaxis2,hnaxis3,hbin1,hbin2,cgain,focuserpos: integer;
    hfilter,hframe,hinstr,hdateobs,pierside : string;
    hCloudCover,hDewPoint,hHumidity,hPressure,hRainRate,hSkyBrightness,hSkyQuality,
    hSkyTemperature,hStarFWHM,hTemperature,hWindDirection,hWindGust,hWindSpeed: double;
    hbzero,hbscale,hdmin,hdmax,hra,hdec,hexp,hpix1,hpix2,hairmass,focusertemp: double;
    haz,hal,pa: double;
    gamma,voffset,coffset,OffsetX,OffsetY: integer;
    Frx,Fry,Frwidth,Frheight: integer;
    hasfocusertemp,hasfocuserpos: boolean;
    i,j,k,n: integer;
    x: double;

    function ExtractWeather(val,key: string): double;
    var i: integer;
    begin
    try
      i:=pos(crlf+key+'=',val);
      if i>0 then begin
        delete(val,1,i+length(crlf+key+'=')-1);
        i:=pos(crlf,val);
        result:=StrToFloatDef(trim(copy(val,1,i-1)),NullCoord);
      end
      else begin
        result:=NullCoord;
      end;
    except
      result:=NullCoord;
    end;
    end;

begin
  // get header values from camera (set by INDI driver or libraw)
  if not f.Header.Valueof('BITPIX',hbitpix) then hbitpix:=f.HeaderInfo.bitpix;
  if not f.Header.Valueof('NAXIS',hnaxis)   then hnaxis:=f.HeaderInfo.naxis;
  if not f.Header.Valueof('NAXIS1',hnaxis1) then hnaxis1:=f.HeaderInfo.naxis1;
  if not f.Header.Valueof('NAXIS2',hnaxis2) then hnaxis2:=f.HeaderInfo.naxis2;
  if not f.Header.Valueof('NAXIS3',hnaxis3) then hnaxis3:=f.HeaderInfo.naxis3;
  if not f.Header.Valueof('BZERO',hbzero)   then hbzero:=f.HeaderInfo.bzero;
  if not f.Header.Valueof('BSCALE',hbscale) then hbscale:=f.HeaderInfo.bscale;
  if not f.Header.Valueof('ROWORDER',roworder) then roworder:=topdown;
  if not f.Header.Valueof('EXPTIME',hexp)   then hexp:=Fexptime;
  if not f.Header.Valueof('PIXSIZE1',hpix1) then hpix1:=-1;
  if not f.Header.Valueof('PIXSIZE2',hpix2) then hpix2:=-1;
  if not f.Header.Valueof('XBINNING',hbin1) then hbin1:=-1;
  if not f.Header.Valueof('YBINNING',hbin2) then hbin2:=-1;
  if (hpix1>0) and (hbin1>0) then hpix1:=hpix1*hbin1;
  if (hpix2>0) and (hbin2>0) then hpix2:=hpix2*hbin2;
  if hpix1<0 then if not f.Header.Valueof('XPIXSZ',hpix1) then hpix1:=-1;
  if hpix2<0 then if not f.Header.Valueof('YPIXSZ',hpix2) then hpix2:=-1;
  if not f.Header.Valueof('FRAME',hframe) then
    if not f.Header.Valueof('IMAGETYP',hframe) then
      hframe:='Light   ';
  if not f.Header.Valueof('FILTER',hfilter) then hfilter:='';
  if not f.Header.Valueof('DATAMIN',hdmin)  then hdmin:=f.HeaderInfo.dmin;
  if not f.Header.Valueof('DATAMAX',hdmax)  then hdmax:=f.HeaderInfo.dmax;
  if not f.Header.Valueof('CAMERA',hinstr) then if not f.Header.Valueof('INSTRUME',hinstr) then hinstr:='';
  if not f.Header.Valueof('FOCALLEN',focal_length)  then focal_length:=-1;
  if not f.Header.Valueof('ISOSPEED',siso)  then siso:='';
  if not f.Header.Valueof('F_STOP',fstop)  then fstop:=-1;
  if not f.Header.Valueof('SHUTTER',shutter)  then shutter:=-1;
  if not f.Header.Valueof('BAYERPAT',CType)  then CType:='';
  if not f.Header.Valueof('XBAYROFF',OffsetX)  then OffsetX:=-1;
  if not f.Header.Valueof('YBAYROFF',OffsetY)  then OffsetY:=-1;
  if not f.Header.Valueof('MULT_R',multr)  then multr:=-1;
  if not f.Header.Valueof('MULT_G',multg)  then multg:=-1;
  if not f.Header.Valueof('MULT_B',multb)  then multb:=-1;
  if not f.Header.Valueof('DATE-OBS',hdateobs) then hdateobs:=FormatDateTime(dateisoshort,NowUTC);
  if not f.Header.Valueof('AIRMASS',hairmass) then hairmass:=-1;
  // get other values
  pierside:='';
  hra:=NullCoord; hdec:=NullCoord;
  if Assigned(FonSequenceInfo) then FonSequenceInfo(self);
  if FsequenceRunning and FTargetCoord then begin
     hra:=FTargetRA;
     hdec:=FTargetDE;
     if (hra<>NullCoord)and(hdec<>NullCoord) then begin
       ApparentToJ2000(hra,hdec);
       hra:=rad2deg*hra;
       hdec:=rad2deg*hdec;
       FCoordinateOrigin:='Target coordinates';
     end;
  end;
  if (FMount<>nil)and(Fmount.Status=devConnected) then begin
     case mount.PierSide of
       pierEast: pierside:='EAST';
       pierWest: pierside:='WEST';
     end;
     if (hra=NullCoord)or(hdec=NullCoord) then begin
       hra:=Fmount.RA;
       hdec:=Fmount.Dec;
       if (hra<>NullCoord)and(hdec<>NullCoord) then begin
         MountToJ2000(Fmount.EquinoxJD,hra,hdec);
         hra:=15*hra;
         FCoordinateOrigin:='Telescope pointing';
       end;
     end;
  end;
  haz:=NullCoord; hal:=NullCoord;
  if (hra<>NullCoord)and(hdec<>NullCoord) then begin
     ra:=deg2rad*hra;
     de:=deg2rad*hdec;
     J2000ToApparent(ra,de);
     st:=SidTimT(FMidExposureTime+ObsTimeZone/24);
     Eq2Hz(st-ra,de,haz,hal) ;
     Refraction(hal,true);
     haz:=round(100*rad2deg*rmod(haz+pi,pi2))/100;
     hal:=round(100*rad2deg*hal)/100;
     hairmass:=round(10000*AirMass(hal))/10000;
  end;
  if (hfilter='')and(Fwheel<>nil)and(Fwheel.Status=devConnected) then begin
     hfilter:=Fwheel.CurrentFilterName;
  end;
  if (FRotator<>nil)and(Ff_rotator<>nil)and(FRotator.Status=devConnected) then
     pa:=Ff_rotator.Angle.Value
  else
     pa:=NullCoord;
  ccdtemp:=Temperature;
  objname:=FObjectName;
  if config<>nil then begin
    origin:=config.GetValue('/Info/ObservatoryName','');
    observer:=config.GetValue('/Info/ObserverName','');
    telname:=config.GetValue('/Info/TelescopeName','');
    instrum:=config.GetValue('/Info/InstrumentName','');
    if not FGuideCamera then begin
      if FFinderCamera then begin
        focal_length:=config.GetValue('/Astrometry/FinderFocalLength',0.0);
      end
      else begin
      if config.GetValue('/Astrometry/FocaleFromTelescope',true)
      then begin
         if focal_length<0 then focal_length:=Fmount.FocaleLength;
      end
      else begin
         focal_length:=config.GetValue('/Astrometry/FocaleLength',0.0);
      end;
      end;
    end;
    if not (FGuideCamera or config.GetValue('/Astrometry/PixelSizeFromCamera',true)) then begin
      hpix1:=config.GetValue('/Astrometry/PixelSize',5.0);
      hpix2:=hpix1;
      if (hbin1>0) then hpix1:=hpix1*hbin1;
      if (hbin2>0) then hpix2:=hpix2*hbin2;
    end;
  end;
  if (not FGuideCamera)and(focal_length<1) then begin
    if FFinderCamera then
      msg(rsErrorUnknown, 0)
    else
      msg(rsErrorUnknowT,0);
  end;
  if (focal_length>50000) then msg('Error: Is the telescope focal length really '+FormatFloat(f0,focal_length)+'mm ?',0);
  try
   GetFrame(Frx,Fry,Frwidth,Frheight);
  except
   Frwidth:=0;
  end;
  try
   gamma:=GetVideoGamma;
   voffset:=GetVideoBrightness;
  except
   gamma:=NullInt;
   voffset:=NullInt;
  end;
  try
   cgain:=NullInt;
   if FhasGain then cgain:=GetGain;
  except
   cgain:=NullInt;
  end;
  try
   if FhasGainISO and (siso='') then siso:=FISOList[GetGain];
  except
   siso:='';
  end;
  try
   coffset:=voffset;
   if FhasOffset then coffset:=GetOffset;
  except
   cgain:=voffset;
  end;
  hasfocuserpos:=false;
  try
  if (FFocuser<>nil)and(FFocuser.hasAbsolutePosition) then begin
     focuserpos:=FFocuser.Position;
     hasfocuserpos:=true;
  end;
  except
   hasfocuserpos:=false;
  end;
  hasfocusertemp:=false;
  try
  if (FFocuser<>nil)and(FFocuser.hasTemperature) then begin
     focusertemp:=FFocuser.Temperature;
     hasfocusertemp:=true;
  end;
  except
   hasfocuserpos:=false;
  end;
  if hasFnumber and (fstop<0) then begin
    fstop:=StrToFloatDef(GetFnumber,-1);
  end;
  if UpdateFromCamera and (CType='') then begin
    try
     if FhasCfaInfo and (hbin1<=1) and (hbin2<=1)  then begin
       CfaInfo(OffsetX,OffsetY,CType);
     end;
    except
    end;
  end;
  hCloudCover:=NullCoord; hDewPoint:=NullCoord; hHumidity:=NullCoord; hPressure:=NullCoord; hRainRate:=NullCoord;
  hSkyBrightness:=NullCoord; hSkyQuality:=NullCoord; hSkyTemperature:=NullCoord; hStarFWHM:=NullCoord;
  hTemperature:=NullCoord; hWindDirection:=NullCoord; hWindGust:=NullCoord; hWindSpeed:=NullCoord;
  if (FWeather<>nil)and(FWeather.Status=devConnected) then begin
    buf:=FWeather.WeatherDetail;
    hCloudCover:=ExtractWeather(buf,'CloudCover');
    hDewPoint:=ExtractWeather(buf,'DewPoint');
    hHumidity:=ExtractWeather(buf,'Humidity');
    hPressure:=ExtractWeather(buf,'Pressure');
    hRainRate:=ExtractWeather(buf,'RainRate');
    hSkyBrightness:=ExtractWeather(buf,'SkyBrightness');
    hSkyQuality:=ExtractWeather(buf,'SkyQuality');
    hSkyTemperature:=ExtractWeather(buf,'SkyTemperature');
    hStarFWHM:=ExtractWeather(buf,'StarFWHM');
    hTemperature:=ExtractWeather(buf,'Temperature');
    hWindDirection:=ExtractWeather(buf,'WindDirection');
    hWindGust:=ExtractWeather(buf,'WindGust');
    hWindSpeed:=ExtractWeather(buf,'WindSpeed');
  end;
  // write new header
  i:=f.Header.Indexof('END');
  if i>0 then f.Header.Delete(i);
  f.Header.Insert(0,'SIMPLE',true,'file does conform to FITS standard');
  f.Header.Insert(1,'BITPIX',hbitpix,'number of bits per data pixel');
  f.Header.Insert(2,'NAXIS',hnaxis,'number of data axes');
  f.Header.Insert(3,'NAXIS1',hnaxis1 ,'length of data axis 1');
  f.Header.Insert(4,'NAXIS2',hnaxis2 ,'length of data axis 2');
  if hnaxis=3 then f.Header.Insert(-1,'NAXIS3',hnaxis3 ,'length of data axis 3');
  if (hbzero<>0)or(hbscale<>1) then begin
    f.Header.Insert(-1,'BZERO',hbzero,'offset data range to that of unsigned short');
    f.Header.Insert(-1,'BSCALE',hbscale,'default scaling factor');
  end;
  f.Header.Insert(-1,'ROWORDER',roworder,'Order of the rows in image array');
  i:=f.Header.Indexof('HIERARCH');
  if i>0 then
     i:=i-1
  else
     i:=-1;
  if hdmax>0 then begin
    f.Header.Insert(i,'DATAMIN',hdmin,'Minimum value');
    f.Header.Insert(i,'DATAMAX',hdmax,'Maximum value');
  end;
  f.Header.Insert(i,'DATE',FormatDateTime(dateisoshort,NowUTC),'Date data written');
  if origin<>'' then f.Header.Insert(i,'ORIGIN',origin,'Observatory name');
  f.Header.Insert(i,'SITELAT',ObsLatitude,'Observatory latitude');
  f.Header.Insert(i,'SITELONG',-ObsLongitude,'Observatory longitude'); //Internal longitude is East negative for historical reason
  if observer<>'' then f.Header.Insert(i,'OBSERVER',observer,'Observer name');
  if telname<>'' then f.Header.Insert(i,'TELESCOP',telname,'Telescope used for acquisition');
  if instrum='' then begin
    if hinstr<>'' then begin
      f.Header.Insert(i,'INSTRUME',hinstr,'Instrument used for acquisition');
    end;
  end
  else begin
    f.Header.Insert(i,'INSTRUME',instrum,'Instrument used for acquisition');
  end;
  if hfilter<>'' then f.Header.Insert(i,'FILTER',hfilter,'Filter');
  f.Header.Insert(i,'SWCREATE','CCDciel '+ccdciel_version+'-'+RevisionStr+blank+compile_system,'');
  if objname<>'' then f.Header.Insert(i,'OBJECT',objname,'Observed object name');
  if objname=rsPreview then begin
    f.Header.Delete('FRAME');
    f.Header.Delete('IMAGETYP');
  end
  else begin
    f.Header.Insert(i,'IMAGETYP',hframe,'Image Type');
  end;
  if (FStackCount<=1)or(not stackresult) then begin
    if FhasLastExposureStartTime then
      f.Header.Insert(i,'DATE-OBS',hdateobs,'UTC start date from camera')
    else
      f.Header.Insert(i,'DATE-OBS',hdateobs,'UTC start date of observation');
    if shutter>0 then begin
      f.Header.Insert(i,'EXPTIME',shutter,'[s] Camera Exposure Time');
      if hexp>0 then f.Header.Insert(i,'REQTIME',hexp,'[s] Requested Exposure Time');
    end
    else begin
      if FhasLastExposureDuration then
         f.Header.Insert(i,'EXPTIME',hexp,'[s] Exposure Time from camera')
      else
         f.Header.Insert(i,'EXPTIME',hexp,'[s] Total Exposure Time');
    end;
  end
  else begin
    f.Header.Insert(i,'DATE-OBS',FStackExpStart,'UTC start date of observation');
    f.Header.Insert(i,'EXPTIME',hexp*FStackCount,'[s] Total Exposure Time');
    f.Header.Insert(i,'STACKCNT',FStackCount,'Number of stacked frames');
    f.Header.Insert(i,'STACKEXP',hexp,'[s] Individual frame exposure Time');
    case FStackOperation of
      0: f.Header.Insert(i,'STACKOP','ADD','Stacking operation');
      1: f.Header.Insert(i,'STACKOP','MEAN','Stacking operation');
    end;
    if FStackAlign and (not FStackRotation) then f.Header.Insert(i,'STACKALN',FormatFloat(f0,FStackAlignX)+'/'+FormatFloat(f0,hnaxis2-FStackAlignY),'Alignment star x/y position');
  end;
  if cgain<>NullInt then f.Header.Insert(i,'GAIN',cgain,'Camera gain setting in manufacturer units');
  if siso<>'' then f.Header.Insert(i,'GAIN',siso,'Camera ISO');
  if gamma<>NullInt then f.Header.Insert(i,'GAMMA',gamma,'Video gamma');
  if coffset<>NullInt then f.Header.Insert(i,'OFFSET',coffset,'Camera offset setting in manufacturer units');
  if fstop>0 then f.Header.Insert(i,'F_STOP',fstop ,'Camera F-stop');
  if hpix1>0 then f.Header.Insert(i,'XPIXSZ',hpix1 ,'[um] Pixel Size X, binned');
  if hpix2>0 then f.Header.Insert(i,'YPIXSZ',hpix2 ,'[um] Pixel Size Y, binned');
  if hpix1>0 then f.Header.Insert(i,'PIXSIZE1',hpix1 ,'[um] Pixel Size X, binned');
  if hpix2>0 then f.Header.Insert(i,'PIXSIZE2',hpix2 ,'[um] Pixel Size Y, binned');
  if hbin1>0 then f.Header.Insert(i,'XBINNING',hbin1 ,'Binning factor X');
  if hbin2>0 then f.Header.Insert(i,'YBINNING',hbin2 ,'Binning factor Y');
  if CType<>'' then begin
     if OffsetX>=0 then f.Header.Insert(i,'XBAYROFF',OffsetX ,'X offset of Bayer array');
     if OffsetY>=0 then f.Header.Insert(i,'YBAYROFF',OffsetY ,'Y offset of Bayer array');
     f.Header.Insert(i,'BAYERPAT',CType ,'Bayer color pattern');
     if multr>0 then f.Header.Insert(i,'MULT_R',multr ,'R multiplier');
     if multg>0 then f.Header.Insert(i,'MULT_G',multg ,'G multiplier');
     if multb>0 then f.Header.Insert(i,'MULT_B',multb ,'B multiplier');
  end;
  if (focal_length>0) then f.Header.Insert(i,'FOCALLEN',focal_length,'[mm] Telescope focal length');
  if ccdtemp<>NullCoord then f.Header.Insert(i,'CCD-TEMP',ccdtemp ,'CCD temperature (Celsius)');
  if Frwidth<>0 then begin
    f.Header.Insert(i,'FRAMEX',Frx,'Frame start x');
    f.Header.Insert(i,'FRAMEY',Fry,'Frame start y');
    f.Header.Insert(i,'FRAMEHGT',Frheight,'Frame height');
    f.Header.Insert(i,'FRAMEWDH',Frwidth,'Frame width');
  end;
  if (haz<>NullCoord)and(hal<>NullCoord) then begin
    f.Header.Insert(i,'CENTAZ',FormatFloat(f2,haz),'[deg] Azimuth of center of image, origin North');
    f.Header.Insert(i,'CENTALT',FormatFloat(f2,hal),'[deg] Altitude of center of image');
  end;
  if hairmass>0 then f.Header.Insert(i,'AIRMASS',hairmass ,'Airmass');
  if hasfocuserpos then f.Header.Insert(i,'FOCUSPOS',focuserpos ,'Focuser position in steps');
  if hasfocusertemp then f.Header.Insert(i,'FOCUSTEM',focusertemp ,'Focuser temperature (Celsius)');
  if pa<>NullCoord then f.Header.Insert(i,'ROTATANG',pa ,'[deg] Rotator angle')
  else f.Header.Delete('ROTATANG');
  if pierside<>'' then f.Header.Insert(i,'PIERSIDE',pierside,'Telescope side of pier');
  if (hra<>NullCoord)and(hdec<>NullCoord) then begin
    f.Header.Insert(i,'OBJCTRA',trim(RAToStrB(hra/15)),'[hh mm ss] '+FCoordinateOrigin+' RA');
    f.Header.Insert(i,'OBJCTDEC',trim(DEToStrB(hdec)),'[+dd mm ss] '+FCoordinateOrigin+' DEC');
    f.Header.Insert(i,'EQUINOX',2000.0,'');
    f.Header.Insert(i,'RA',hra,'[deg] '+FCoordinateOrigin+' RA');
    f.Header.Insert(i,'DEC',hdec,'[deg] '+FCoordinateOrigin+' DEC');
    f.Header.Insert(i,'CRVAL1',hra,'[deg] '+FCoordinateOrigin+' RA');
    f.Header.Insert(i,'CRVAL2',hdec,'[deg] '+FCoordinateOrigin+' DEC');
    if (hpix1>0)and(hpix2>0)and(focal_length>0)  then begin
       pixscale1:=3600*rad2deg*arctan(hpix1/1000/focal_length);
       pixscale2:=3600*rad2deg*arctan(hpix2/1000/focal_length);
       f.Header.Insert(i,'SECPIX1',pixscale1,'image scale arcseconds per pixel');
       f.Header.Insert(i,'SECPIX2',pixscale2,'image scale arcseconds per pixel');
       f.Header.Insert(i,'SCALE',pixscale1,'image scale arcseconds per pixel');
    end
    else if FGuideCamera and (FGuidePixelScale>0) then begin
       f.Header.Insert(i,'SECPIX1',FGuidePixelScale,'image scale arcseconds per pixel');
       f.Header.Insert(i,'SECPIX2',FGuidePixelScale,'image scale arcseconds per pixel');
       f.Header.Insert(i,'SCALE',FGuidePixelScale,'image scale arcseconds per pixel');
    end;
  end;
  if FGuideCamera and (FGuideLockX>0)and (FGuideLockY>0) then begin
     f.Header.Insert(i,'GUIDEX',FGuideLockX,'guide lock position X');
     f.Header.Insert(i,'GUIDEY',FGuideLockY,'guide lock position Y');
  end;
  if hCloudCover<>NullCoord then f.Header.Insert(i,'AOCCLOUD',hCloudCover,'Cloud coverage in percent');
  if hTemperature<>NullCoord then f.Header.Insert(i,'AOCAMBT',hTemperature,'Ambient temperature in degrees C');
  if hDewPoint<>NullCoord then f.Header.Insert(i,'AOCDEW',hDewPoint,'Dew point in degrees C');
  if hHumidity<>NullCoord then f.Header.Insert(i,'AOCHUM',hHumidity,'Humidity in percent');
  if hPressure<>NullCoord then f.Header.Insert(i,'AOCBAROM',hPressure,'Barometric pressure in hPa');
  if hRainRate<>NullCoord then f.Header.Insert(i,'AOCRAIN',hRainRate,'Rain rate in mm/hour');
  if hWindDirection<>NullCoord then f.Header.Insert(i,'AOCWINDD',hWindDirection,'Wind direction in degrees (0..360)');
  if hWindGust<>NullCoord then f.Header.Insert(i,'AOCWINDG',hWindGust,'Wind gust in m/s');
  if hWindSpeed<>NullCoord then f.Header.Insert(i,'AOCWIND',hWindSpeed,'Wind speed in m/s');
  if hSkyBrightness<>NullCoord then f.Header.Insert(i,'AOCSKYBR',hSkyBrightness,'Sky brightness in Lux');
  if hSkyQuality<>NullCoord then f.Header.Insert(i,'AOCSKYQU',hSkyQuality,'Sky quality (magnitudes per square arcsecond)');
  if hSkyTemperature<>NullCoord then f.Header.Insert(i,'AOCSKYT',hSkyTemperature,'Sky temperature in degrees C');
  if hStarFWHM<>NullCoord then f.Header.Insert(i,'AOCFWHM',hStarFWHM,'Seeing FWHM in arc seconds');
  // custom headers
  for j:=1 to CustomHeaderNum do begin
    // try integer value
    val(CustomHeaders[j].value,k,n);
    if n=0 then begin
      f.Header.Insert(i,CustomHeaders[j].key,k,'');
    end
    else begin
      // try floating point value
      val(CustomHeaders[j].value,x,n);
      if n=0 then begin
        f.Header.Insert(i,CustomHeaders[j].key,x,'');
      end
      else begin
        // string value
        f.Header.Insert(i,CustomHeaders[j].key,StringReplace(CustomHeaders[j].value,'''','',[rfReplaceAll]),'');
      end;
    end;
  end;
  f.Header.Add('END','','');
end;

procedure T_camera.NewVideoFrame(rawframe: boolean);
var x,y,w,h: integer;
begin
  if lockvideoframe then exit;
  lockvideoframe:=true;
  try
  // set header for raw frame, it is set in stream from mjpeg
  if rawframe then begin
    GetStreamFrame(x,y,w,h);
    FVideoStream.Position:=0;
    if Color then begin
      WriteVideoHeader(w,h,3,8);
    end else begin
      WriteVideoHeader(w,h,2,8);
    end;
  end;
  FFits.VideoStream:=FVideoStream;
  FVideoStream:=TMemoryStream.Create;
  if Assigned(FonVideoFrame) then FonVideoFrame(self);
  finally
    lockvideoframe:=false;
  end;
end;

procedure T_camera.WriteVideoHeader(width,height,naxis,bitpix: integer);
var
    hbitpix,hnaxis,hnaxis1,hnaxis2,hnaxis3: integer;
    hframe,hdateobs : string;
    hbzero,hbscale,hdmin,hdmax: double;
begin
  // simplified video header
  hbitpix:=bitpix;
  hnaxis:=naxis;
  if naxis=2 then begin
    hnaxis1:=width;
    hnaxis2:=height;
  end;
  if naxis=3 then begin
     hnaxis1:=4;       // 32bit video stream from INDI
     hnaxis2:=width;
     hnaxis3:=height;
  end;
  hframe:='Video   ';
  hbzero:=0;
  hbscale:=1;
  hdmin:=0;
  hdmax:=0;
  hdateobs:=FormatDateTime(dateisoshort,NowUTC);
  // write new header
  Ffits.Header.ClearHeader;
  Ffits.Header.Add('SIMPLE',true,'file does conform to FITS standard');
  Ffits.Header.Add('BITPIX',hbitpix,'number of bits per data pixel');
  Ffits.Header.Add('NAXIS',hnaxis,'number of data axes');
  Ffits.Header.Add('NAXIS1',hnaxis1 ,'length of data axis 1');
  Ffits.Header.Add('NAXIS2',hnaxis2 ,'length of data axis 2');
  if hnaxis=3 then Ffits.Header.Add('NAXIS3',hnaxis3 ,'length of data axis 3');;
  Ffits.Header.Add('BZERO',hbzero,'offset data range to that of unsigned short');
  Ffits.Header.Add('BSCALE',hbscale,'default scaling factor');
  Ffits.Header.Add('DATAMIN',hdmin,'Minimum value');
  Ffits.Header.Add('DATAMAX',hdmax,'Maximum value');
  Ffits.Header.Add('DATE',FormatDateTime(dateisoshort,NowUTC),'Date data written');
  Ffits.Header.Add('SWCREATE','CCDciel '+ccdciel_version+'-'+RevisionStr,'');
  Ffits.Header.Add('IMAGETYP',hframe,'Image Type');
  Ffits.Header.Add('DATE-OBS',hdateobs,'UTC start date of observation');
  Ffits.Header.Add('END','','');
  Ffits.GetFitsInfo;
end;

function T_camera.InitFrameType(t: integer):TFrameType;
var ctype: integer;
begin
  if FFinderCamera or FGuideCamera then begin
    // Only used by main camera
    exit;
  end;
  if (t>=0)and(t<=ord(High(TFrameType))) then begin
    // one of the standard image type: light,bias,dark,flat
    result:=TFrameType(t);
    ctype:=-1;
  end
  else begin
    // a custom image type
    result:=LIGHT;
    if t>ord(High(TFrameType)) then
      ctype:=t-ord(High(TFrameType))-1
    else
      ctype:=-1; // undefined type, same as light
  end;
  if ctype<>CurrentCustomFrameType then begin
    // custom type change
    if (CurrentCustomFrameType>=0)and(CustomFrameType[CurrentCustomFrameType].ScriptOff<>'') then begin
      // Deactivation script
      FRunScript(CustomFrameType[CurrentCustomFrameType].ScriptOff,slash(ConfigDir),CustomFrameType[CurrentCustomFrameType].ParamOff);
    end;
    if (ctype>=0)and(CustomFrameType[ctype].ScriptOn<>'') then begin
      // Activation script
      FRunScript(CustomFrameType[ctype].ScriptOn,slash(ConfigDir),CustomFrameType[ctype].ParamOn);
    end;
    CurrentCustomFrameType:=ctype;
  end;
end;

function T_camera.ControlExposure(exp:double; pbinx,pbiny: integer; frmt:TFrameType; preadoutmode,pgain,poffset:integer; quiet:boolean=false):boolean;
var SaveonNewImage: TNotifyEvent;
    endt: TDateTime;
begin
result:=false;
if Status=devConnected then begin
  if not quiet then begin
    if exp>=1 then
      msg(Format(rsTakeControlE, [FormatFloat(f1, exp)]),3)
    else
      msg(Format(rsTakeControlE, [FormatFloat(f4, exp)]),3);
  end;
  SaveonNewImage:=FonNewImage;
  onNewImage:=@EndExposure;
  // set readout first so it can be overridden by specific binning or gain
  if UseReadoutMode and hasReadOut then begin
     readoutmode:=preadoutmode;
  end;
  if (pbinx<>BinX)or(pbiny<>BinY) then SetBinning(pbinx,pbiny);
  WaitExposure:=true;
  ExpectedStop:=false;
  ControlExposureOK:=false;
  AddFrames:=false;
  if CanSetGain then begin
    if hasGainISO and (pgain<>NullInt) then begin
       if Gain<>pgain then Gain:=pgain;
    end;
    if hasGain and (not hasGainISO) and (pgain<>NullInt) then begin
       if Gain<>pgain then Gain:=pgain;
    end;
    if hasOffset  and (poffset<>NullInt) then begin
       if Offset<>poffset then Offset:=poffset;
    end;
  end;
  InitFrameType(ord(frmt));
  if FrameType<>frmt then FrameType:=frmt;
  StartExposure(exp);
  endt:=now+(exp+FCameraTimeout)/secperday; // large timeout for DSLR that not support hardware ROI
  while WaitExposure and(now<endt) and (not CancelAutofocus) do begin
    Sleep(100);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
  result:=ControlExposureOK;
  onNewImage:=SaveonNewImage;
  if result and Assigned(SaveonNewImage) then SaveonNewImage(self);
  if not quiet then Wait(1);
end;
end;

procedure T_camera.EndExposure(Sender: TObject);
begin
  ControlExposureOK:=true;
  WaitExposure:=false;
  if assigned(FonEndControlExposure) then FonEndControlExposure(self);
end;

function T_camera.Solve(f:TFits; out ra,de,pa,scale: double):boolean;
begin
  if Assigned(FSolve) then begin
    FSolve(@f,ra,de,pa,scale,result);
  end
  else
    result:=false;
end;

end.

