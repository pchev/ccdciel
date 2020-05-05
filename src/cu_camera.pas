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

uses  cu_fits, cu_mount, cu_wheel, cu_focuser, u_global, u_utils,  indiapi, math, u_translation,
  LCLVersion, Classes, Forms, SysUtils, ExtCtrls;

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
    FonVideoPreviewChange,FonVideoSizeChange,FonVideoRateChange: TNotifyEvent;
    FonFPSChange,FonVideoExposureChange : TNotifyEvent;
    FImgStream: TMemoryStream;
    FVideoStream: TMemoryStream;
    FFilterNames: TStringList;
    FObjectName: string;
    Fdevice: string;
    FImageFormat: string;
    FCameraXSize,FCameraYSize: integer;
    FFits: TFits;
    FStackCount: integer;
    FStackAlign: boolean;
    FStackAlignX,FStackAlignY,FStackStarX,FStackStarY: double;
    FMount: T_mount;
    Fwheel: T_wheel;
    FFocuser: T_focuser;
    FTimeOut: integer;
    FAutoLoadConfig: boolean;
    FhasVideo: boolean;
    FVerticalFlip: boolean;
    FASCOMFlipImage: boolean;
    FAddFrames: boolean;
    FVideoSizes, FVideoRates,FFNumberList:TStringList;
    FTemperatureRampActive, FCancelTemperatureRamp: boolean;
    FIndiTransfert: TIndiTransfert;
    FIndiTransfertDir,FIndiTransfertPrefix: string;
    FhasGain,FhasGainISO,FCanSetGain,FhasCfaInfo,FhasFnumber,FhasCoolerPower: boolean;
    FUseCameraStartTime,FhasLastExposureStartTime,FhasLastExposureDuration: boolean;
    FGainMin, FGainMax: integer;
    FISOList: TStringList;
    FhasFastReadout, FhasReadOut: boolean;
    FReadOutList: TStringList;
    Ftimestart,Ftimeend,FMidExposureTime: double;
    FImgNum:PtrInt;
    Fexptime: double;
    FFixPixelRange: boolean;
    procedure msg(txt: string; level:integer=3);
    procedure NewImage;
    procedure TryNextExposure(Data: PtrInt);
    procedure WriteHeaders;
    procedure NewVideoFrame;
    procedure WriteVideoHeader(width,height,naxis,bitpix: integer);
    function GetBinX:integer; virtual; abstract;
    function GetBinY:integer; virtual; abstract;
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
    procedure SetGain(value: integer); virtual; abstract;
    function GetGain: integer; virtual; abstract;
    procedure SetReadOutMode(value: integer); virtual; abstract;
    function GetReadOutMode: integer; virtual; abstract;
    procedure SetFnumber(value: string); virtual; abstract;
    function GetFnumber: string; virtual; abstract;
    function GetStreamingExposureRange:TNumRange; virtual; abstract;
    function GetStreamingExposure:double; virtual; abstract;
    procedure SetStreamingExposure(value:double); virtual; abstract;
  private
    lockvideoframe: boolean;
    TempFinal: Double;
    TempNow,TempRamp: double;
    Nstep: integer;
    RampTimer: TTimer;
    procedure RampTimerTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); virtual; abstract;
    Procedure Disconnect; virtual; abstract;
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
    Procedure SetActiveDevices(focuser,filters,telescope: string); virtual; abstract;
    procedure StartVideoPreview; virtual; abstract;
    procedure StopVideoPreview; virtual; abstract;
    procedure StartVideoRecord(mode:TVideoRecordMode); virtual; abstract;
    procedure StopVideoRecord; virtual; abstract;
    property DeviceName: string read FDevice;
    property Fits: TFits read FFits write FFits;
    property Mount: T_mount read FMount write FMount;
    property Wheel: T_wheel read Fwheel write Fwheel;
    property Focuser: T_focuser read FFocuser write FFocuser;
    property ObjectName: string read FObjectName write FObjectName;
    property CameraInterface: TDevInterface read FCameraInterface;
    property Status: TDeviceStatus read FStatus;
    property WheelStatus: TDeviceStatus read FWheelStatus;
    property ImgStream: TMemoryStream read FImgStream;
    property ImageFormat: string read GetImageFormat;
    property AddFrames: boolean read FAddFrames write FAddFrames;
    property VerticalFlip: boolean read FVerticalFlip;
    property ASCOMFlipImage: boolean read FASCOMFlipImage write FASCOMFlipImage;
    property hasVideo: boolean read FhasVideo;
    property VideoStream: TMemoryStream read FVideoStream;
    property VideoPreviewRunning: boolean read GetVideoPreviewRunning;
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
    property StackCount: integer read FStackCount;
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
    property FnumberList: TstringList read FFNumberList;
    property LastExposureTime:double read Fexptime;
    property hasCfaInfo: boolean read FhasCfaInfo;
    property hasReadOut: boolean read FhasReadOut;
    property ReadOutList: TStringList read FReadOutList;
    property ReadOutMode: integer read GetReadOutMode write SetReadOutMode;
    property hasFnumber: boolean read FhasFnumber;
    property Fnumber: string read GetFnumber write SetFnumber;
    property UseCameraStartTime: boolean read FUseCameraStartTime write FUseCameraStartTime;
    property FixPixelRange: boolean read FFixPixelRange write FFixPixelRange;
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
    property onVideoSizeChange: TNotifyEvent read FonVideoSizeChange write FonVideoSizeChange;
    property onVideoRateChange: TNotifyEvent read FonVideoRateChange write FonVideoRateChange;
    property onFPSChange: TNotifyEvent read FonFPSChange write FonFPSChange;
    property onVideoExposureChange: TNotifyEvent read FonVideoExposureChange write FonVideoExposureChange;
end;

const CameraTimeout=60; // in seconds, must be enough to download image from any camers

implementation

uses
{$if lcl_major > 1}
LazSysUtils;
{$else}
LazUTF8SysUtils;
{$endif}

constructor T_camera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImgNum:=0;
  FTimeOut:=100;
  FIndiTransfert:=itNetwork;
  FIndiTransfertDir:=defTransfertPath;
  FIndiTransfertPrefix:='ccdciel_tmp';
  FVerticalFlip:=false;
  FASCOMFlipImage:=false;
  FStatus := devDisconnected;
  FFilterNames:=TStringList.Create;
  FImgStream:=TMemoryStream.Create;
  FAddFrames:=false;
  FhasVideo:=false;
  FVideoStream:=TMemoryStream.Create;;
  lockvideoframe:=false;
  FVideoSizes:=TStringList.Create;
  FVideoRates:=TStringList.Create;
  FFNumberList:=TStringList.Create;
  FTemperatureRampActive:=false;
  FCancelTemperatureRamp:=false;
  FStackCount:=0;
  FISOList:=TStringList.Create;
  FGainMin:=0;
  FGainMax:=0;
  FhasGain:=false;
  FhasGainISO:=false;
  FhasCfaInfo:=false;
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
end;

destructor  T_camera.Destroy;
begin
  FImgStream.Free;
  FFilterNames.Free;
  FVideoStream.Free;
  FVideoSizes.Free;
  FVideoRates.Free;
  FFNumberList.Free;
  FISOList.Free;
  FReadOutList.Free;
  inherited Destroy;
end;

procedure T_camera.msg(txt: string; level:integer=3);
begin
 if Assigned(FonMsg) then FonMsg(Fdevice+': '+txt,level);
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
begin
  RampTimer.Enabled:=false;
  if Nstep>0 then begin
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
  end
  else begin
    SetTemperature(TempFinal);
    msg(rsSetTemperatu3);
    FTemperatureRampActive:=false;
    if Assigned(FonTemperatureChange) then FonTemperatureChange(GetTemperature);
  end;
end;

procedure T_camera.NewImage;
var f:TFits;
    xi,yi,xc,yc,ri: integer;
    xs,ys,hfd,fwhm,vmax,snr,bg,bgdev,flux : double;
    alok: boolean;
begin
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'NewImage');{$endif}
if FAddFrames then begin  // stack preview frames
  // load temporary image
  f:=TFits.Create(nil);
  f.onMsg:=onMsg;
  f.Stream:=ImgStream;
  f.LoadStream;
  // convert 8bit to 16bit to avoid quick overflow
  if f.HeaderInfo.bitpix=8 then
     f.Bitpix8to16;
  // substract dark if loaded and compatible
  if f.SameFormat(FFits.DarkFrame) then
     f.Math(FFits.DarkFrame,moSub);
  // check frame is compatible
  if FFits.SameFormat(f) then begin
     if FStackAlign then begin
        // align frame on ref star
        alok:=false;
        xi:=round(FStackStarX);
        yi:=round(FStackStarY);
        f.FindStarPos(xi,yi,50,xc,yc,ri,vmax,bg,bgdev);
        if vmax>0 then begin
          f.GetHFD2(xc,yc,2*ri,xs,ys,bg,bgdev,hfd,fwhm,vmax,snr,flux,false);
          if ((hfd>0)and(Undersampled or (hfd>0.7))) and (hfd<10) then begin
             f.Shift(FStackAlignX-xs,FStackAlignY-ys);
             FStackStarX:=xs;
             FStackStarY:=ys;
             alok:=true;
           end;
       end;
       if not alok then msg(rsAlignmentSta,0);
     end;
     FFits.Math(f,moAdd);       // add frame
     inc(FStackCount);
  end
  else begin
     FFits.Math(f,moAdd,true);  // start a new stack
     FStackCount:=1;
     FStackAlign:=false;
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
            msg(Format(rsStackingWith, [inttostr(round(xs)), inttostr(round(ys))
              ]));
         end;
       end;
     end;
     if not FStackAlign then msg(rsNoAlignmentS,0);
  end;
  // update image
  FFits.Header.Assign(f.Header);
  WriteHeaders;
  Ffits.GetFitsInfo;
  f.free;
  if Assigned(FonNewImage) then FonNewImage(self);
end
else begin  // normal capture
  FStackCount:=0;
  if ImgStream.Size>0 then begin
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'set fits stream');{$endif}
  Ffits.Stream:=ImgStream;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'write headers');{$endif}
  WriteHeaders;
  FFits.ApplyDark;
  FFits.ApplyBPM;
  end;
  {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'process new image');{$endif}
  if Assigned(FonNewImage) then FonNewImage(self);
end;
end;

procedure T_camera.TryNextExposure(Data: PtrInt);
begin
 if EarlyNextExposure and Assigned(FonNewExposure) and(not Autofocusing) then begin
   if CameraProcessingImage and (CameraProcessingNum=FImgNum-1) then begin
     sleep(10);
     CheckSynchronize;
     Application.QueueAsyncCall(@TryNextExposure,Data);
   end
   else begin
     CameraProcessingImage:=true;
     CameraProcessingNum:=data;
     FonNewExposure(self);
   end;
 end;
end;

procedure T_camera.WriteHeaders;
var origin,observer,telname,objname,siso,CType: string;
    focal_length,pixscale1,pixscale2,ccdtemp,st,ra,de,fstop,shutter,multr,multg,multb: double;
    hbitpix,hnaxis,hnaxis1,hnaxis2,hnaxis3,hbin1,hbin2,cgain,focuserpos: integer;
    hfilter,hframe,hinstr,hdateobs : string;
    hbzero,hbscale,hdmin,hdmax,hra,hdec,hexp,hpix1,hpix2,hairmass,focusertemp: double;
    haz,hal: double;
    gamma,offset,OffsetX,OffsetY: integer;
    Frx,Fry,Frwidth,Frheight: integer;
    hasfocusertemp,hasfocuserpos: boolean;
    i: integer;
begin
  // get header values from camera (set by INDI driver or libraw)
  if not Ffits.Header.Valueof('BITPIX',hbitpix) then hbitpix:=Ffits.HeaderInfo.bitpix;
  if not Ffits.Header.Valueof('NAXIS',hnaxis)   then hnaxis:=Ffits.HeaderInfo.naxis;
  if not Ffits.Header.Valueof('NAXIS1',hnaxis1) then hnaxis1:=Ffits.HeaderInfo.naxis1;
  if not Ffits.Header.Valueof('NAXIS2',hnaxis2) then hnaxis2:=Ffits.HeaderInfo.naxis2;
  if not Ffits.Header.Valueof('NAXIS3',hnaxis3) then hnaxis3:=Ffits.HeaderInfo.naxis3;
  if not Ffits.Header.Valueof('BZERO',hbzero)   then hbzero:=Ffits.HeaderInfo.bzero;
  if not Ffits.Header.Valueof('BSCALE',hbscale) then hbscale:=Ffits.HeaderInfo.bscale;
  if not Ffits.Header.Valueof('EXPTIME',hexp)   then hexp:=Fexptime;
  if not Ffits.Header.Valueof('PIXSIZE1',hpix1) then hpix1:=-1;
  if not Ffits.Header.Valueof('PIXSIZE2',hpix2) then hpix2:=-1;
  if not Ffits.Header.Valueof('XBINNING',hbin1) then hbin1:=-1;
  if not Ffits.Header.Valueof('YBINNING',hbin2) then hbin2:=-1;
  if (hpix1>0) and (hbin1>0) then hpix1:=hpix1*hbin1;
  if (hpix2>0) and (hbin2>0) then hpix2:=hpix2*hbin2;
  if hpix1<0 then if not Ffits.Header.Valueof('XPIXSZ',hpix1) then hpix1:=-1;
  if hpix2<0 then if not Ffits.Header.Valueof('YPIXSZ',hpix2) then hpix2:=-1;
  if not Ffits.Header.Valueof('FRAME',hframe)   then hframe:='Light   ';
  if not Ffits.Header.Valueof('FILTER',hfilter) then hfilter:='';
  if not Ffits.Header.Valueof('DATAMIN',hdmin)  then hdmin:=Ffits.HeaderInfo.dmin;
  if not Ffits.Header.Valueof('DATAMAX',hdmax)  then hdmax:=Ffits.HeaderInfo.dmax;
  if not Ffits.Header.Valueof('CAMERA',hinstr) then if not Ffits.Header.Valueof('INSTRUME',hinstr) then hinstr:='';
  if not Ffits.Header.Valueof('FOCALLEN',focal_length)  then focal_length:=-1;
  if not Ffits.Header.Valueof('ISOSPEED',siso)  then siso:='';
  if not Ffits.Header.Valueof('F_STOP',fstop)  then fstop:=-1;
  if not Ffits.Header.Valueof('SHUTTER',shutter)  then shutter:=-1;
  if not Ffits.Header.Valueof('BAYERPAT',CType)  then CType:='';
  if not Ffits.Header.Valueof('XBAYROFF',OffsetX)  then OffsetX:=-1;
  if not Ffits.Header.Valueof('YBAYROFF',OffsetY)  then OffsetY:=-1;
  if not Ffits.Header.Valueof('MULT_R',multr)  then multr:=-1;
  if not Ffits.Header.Valueof('MULT_G',multg)  then multg:=-1;
  if not Ffits.Header.Valueof('MULT_B',multb)  then multb:=-1;
  if not Ffits.Header.Valueof('DATE-OBS',hdateobs) then hdateobs:=FormatDateTime(dateisoshort,NowUTC);
  if not Ffits.Header.Valueof('AIRMASS',hairmass) then hairmass:=-1;
  // get other values
  hra:=NullCoord; hdec:=NullCoord;
  if (FMount<>nil)and(Fmount.Status=devConnected) then begin
     hra:=Fmount.RA;
     hdec:=Fmount.Dec;
     MountToJ2000(Fmount.EquinoxJD,hra,hdec);
     hra:=15*hra;
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
  ccdtemp:=Temperature;
  objname:=FObjectName;
  if config<>nil then begin
    origin:=config.GetValue('/Info/ObservatoryName','');
    observer:=config.GetValue('/Info/ObserverName','');
    telname:=config.GetValue('/Info/TelescopeName','');
    if config.GetValue('/Astrometry/FocaleFromTelescope',true)
    then begin
       if focal_length<0 then focal_length:=Fmount.FocaleLength
    end
    else
       focal_length:=config.GetValue('/Astrometry/FocaleLength',0);
    if not config.GetValue('/Astrometry/PixelSizeFromCamera',true) then begin
      hpix1:=config.GetValue('/Astrometry/PixelSize',5.0);
      hpix2:=hpix1;
      if (hbin1>0) then hpix1:=hpix1*hbin1;
      if (hbin2>0) then hpix2:=hpix2*hbin2;
    end;
  end;
  if (focal_length<1) then msg(rsErrorUnknowT,0);
  if (focal_length>50000) then msg('Error: Is the telescope focal length really '+FormatFloat(f0,focal_length)+'mm ?',0);
  try
   GetFrame(Frx,Fry,Frwidth,Frheight);
  except
   Frwidth:=0;
  end;
  try
   gamma:=GetVideoGamma;
   offset:=GetVideoBrightness;
  except
   gamma:=NullInt;
   offset:=NullInt;
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
  if CType='' then begin
    try
     if FhasCfaInfo and (hbin1<=1) and (hbin2<=1)  then begin
       CfaInfo(OffsetX,OffsetY,CType);
       if FASCOMFlipImage and (not odd(FCameraYSize)) then
          OffsetY:=(OffsetY+1) mod 2;
     end;
    except
    end;
  end;
  // write new header
  i:=FFits.Header.Indexof('END');
  if i>0 then FFits.Header.Delete(i);
  Ffits.Header.Insert(0,'SIMPLE',true,'file does conform to FITS standard');
  Ffits.Header.Insert(1,'BITPIX',hbitpix,'number of bits per data pixel');
  Ffits.Header.Insert(2,'NAXIS',hnaxis,'number of data axes');
  Ffits.Header.Insert(3,'NAXIS1',hnaxis1 ,'length of data axis 1');
  Ffits.Header.Insert(4,'NAXIS2',hnaxis2 ,'length of data axis 2');
  if hnaxis=3 then Ffits.Header.Insert(-1,'NAXIS3',hnaxis3 ,'length of data axis 3');;
  Ffits.Header.Insert(-1,'EXTEND',true,'FITS dataset may contain extensions');
  Ffits.Header.Insert(-1,'BZERO',hbzero,'offset data range to that of unsigned short');
  Ffits.Header.Insert(-1,'BSCALE',hbscale,'default scaling factor');
  i:=FFits.Header.Indexof('HIERARCH');
  if i>0 then
     i:=i-1
  else
     i:=-1;
  if hdmax>0 then begin
    Ffits.Header.Insert(i,'DATAMIN',hdmin,'Minimum value');
    Ffits.Header.Insert(i,'DATAMAX',hdmax,'Maximum value');
  end;
  Ffits.Header.Insert(i,'DATE',FormatDateTime(dateisoshort,NowUTC),'Date data written');
  if origin<>'' then Ffits.Header.Insert(i,'ORIGIN',origin,'Observatory name');
  Ffits.Header.Insert(i,'SITELAT',ObsLatitude,'Observatory latitude');
  Ffits.Header.Insert(i,'SITELONG',-ObsLongitude,'Observatory longitude'); //Internal longitude is East negative for historical reason
  if observer<>'' then Ffits.Header.Insert(i,'OBSERVER',observer,'Observer name');
  if telname<>'' then Ffits.Header.Insert(i,'TELESCOP',telname,'Telescope used for acquisition');
  if hinstr<>'' then Ffits.Header.Insert(i,'INSTRUME',hinstr,'Instrument used for acquisition');
  if hfilter<>'' then Ffits.Header.Insert(i,'FILTER',hfilter,'Filter');
  Ffits.Header.Insert(i,'SWCREATE','CCDciel '+ccdciel_version+'-'+RevisionStr+blank+compile_system,'');
  if objname<>'' then Ffits.Header.Insert(i,'OBJECT',objname,'Observed object name');
  Ffits.Header.Insert(i,'IMAGETYP',hframe,'Image Type');
  if FhasLastExposureStartTime then
    Ffits.Header.Insert(i,'DATE-OBS',hdateobs,'UTC start date from camera')
  else
    Ffits.Header.Insert(i,'DATE-OBS',hdateobs,'UTC start date of observation');
  if shutter>0 then begin
    Ffits.Header.Insert(i,'EXPTIME',shutter,'[s] Camera Exposure Time');
    if hexp>0 then Ffits.Header.Insert(i,'REQTIME',hexp,'[s] Requested Exposure Time');
  end
  else begin
    if FhasLastExposureDuration then
       Ffits.Header.Insert(i,'EXPTIME',hexp,'[s] Exposure Time from camera')
    else
       Ffits.Header.Insert(i,'EXPTIME',hexp,'[s] Total Exposure Time');
  end;
  if FStackCount>1 then Ffits.Header.Insert(i,'STACKCNT',FStackCount,'Number of stacked frames');
  if cgain<>NullInt then Ffits.Header.Insert(i,'GAIN',cgain,'Camera gain setting in manufacturer units');
  if siso<>'' then Ffits.Header.Insert(i,'GAIN',siso,'Camera ISO');
  if gamma<>NullInt then Ffits.Header.Insert(i,'GAMMA',gamma,'Video gamma');
  if offset<>NullInt then Ffits.Header.Insert(i,'OFFSET',offset,'Video offset,brightness');
  if fstop>0 then Ffits.Header.Insert(i,'F_STOP',fstop ,'Camera F-stop');
  if hpix1>0 then Ffits.Header.Insert(i,'XPIXSZ',hpix1 ,'[um] Pixel Size X, binned');
  if hpix2>0 then Ffits.Header.Insert(i,'YPIXSZ',hpix2 ,'[um] Pixel Size Y, binned');
  if hpix1>0 then Ffits.Header.Insert(i,'PIXSIZE1',hpix1 ,'[um] Pixel Size X, binned');
  if hpix2>0 then Ffits.Header.Insert(i,'PIXSIZE2',hpix2 ,'[um] Pixel Size Y, binned');
  if hbin1>0 then Ffits.Header.Insert(i,'XBINNING',hbin1 ,'Binning factor X');
  if hbin2>0 then Ffits.Header.Insert(i,'YBINNING',hbin2 ,'Binning factor Y');
  if CType<>'' then begin
     if OffsetX>=0 then Ffits.Header.Insert(i,'XBAYROFF',OffsetX ,'X offset of Bayer array');
     if OffsetY>=0 then Ffits.Header.Insert(i,'YBAYROFF',OffsetY ,'Y offset of Bayer array');
     Ffits.Header.Insert(i,'BAYERPAT',CType ,'Bayer color pattern');
     if multr>0 then Ffits.Header.Insert(i,'MULT_R',multr ,'R multiplier');
     if multg>0 then Ffits.Header.Insert(i,'MULT_G',multg ,'G multiplier');
     if multb>0 then Ffits.Header.Insert(i,'MULT_B',multb ,'B multiplier');
  end;
  Ffits.Header.Insert(i,'FOCALLEN',focal_length,'[mm] Telescope focal length');
  if ccdtemp<>NullCoord then Ffits.Header.Insert(i,'CCD-TEMP',ccdtemp ,'CCD temperature (Celsius)');
  if Frwidth<>0 then begin
    Ffits.Header.Insert(i,'FRAMEX',Frx,'Frame start x');
    Ffits.Header.Insert(i,'FRAMEY',Fry,'Frame start y');
    Ffits.Header.Insert(i,'FRAMEHGT',Frheight,'Frame height');
    Ffits.Header.Insert(i,'FRAMEWDH',Frwidth,'Frame width');
  end;
  if (haz<>NullCoord)and(hal<>NullCoord) then begin
    Ffits.Header.Insert(i,'CENTAZ',haz,'[deg] Azimuth of center of image');
    Ffits.Header.Insert(i,'CENTALT',hal,'[deg] Altitude of center of image');
    Ffits.Header.Insert(i,'OBJCTAZ',haz,'[deg] Azimuth of center of image');
    Ffits.Header.Insert(i,'OBJCTALT',hal,'[deg] Altitude of center of image');
  end;
  if hairmass>0 then Ffits.Header.Insert(i,'AIRMASS',hairmass ,'Airmass');
  if hasfocuserpos then Ffits.Header.Insert(i,'FOCUSPOS',focuserpos ,'Focuser position in steps');
  if hasfocusertemp then Ffits.Header.Insert(i,'FOCUSTEM',focusertemp ,'Focuser temperature (Celsius)');
  if (hra<>NullCoord)and(hdec<>NullCoord) then begin
    Ffits.Header.Insert(i,'EQUINOX',2000.0,'');
    Ffits.Header.Insert(i,'RA',hra,'[deg] Telescope pointing RA');
    Ffits.Header.Insert(i,'DEC',hdec,'[deg] Telescope pointing DEC');
    Ffits.Header.Insert(i,'OBJCTRA',trim(RAToStrB(hra/15)),'[hh mm ss] Telescope pointing RA');
    Ffits.Header.Insert(i,'OBJCTDEC',trim(DEToStrB(hdec)),'[+dd mm ss] Telescope pointing DEC');
    if (hpix1>0)and(hpix2>0)and(focal_length>0)  then begin
       pixscale1:=3600*rad2deg*arctan(hpix1/1000/focal_length);
       pixscale2:=3600*rad2deg*arctan(hpix2/1000/focal_length);
       Ffits.Header.Insert(i,'SECPIX1',pixscale1,'image scale arcseconds per pixel');
       Ffits.Header.Insert(i,'SECPIX2',pixscale2,'image scale arcseconds per pixel');
       Ffits.Header.Insert(i,'SCALE',pixscale1,'image scale arcseconds per pixel');
    end;
  end;
  Ffits.Header.Add('END','','');
end;

procedure T_camera.NewVideoFrame;
var x,y,w,h: integer;
begin
  if lockvideoframe then exit;
  lockvideoframe:=true;
  try
  GetStreamFrame(x,y,w,h);
  FVideoStream.Position:=0;
  if Color then begin
    WriteVideoHeader(w,h,3,8);
  end else begin
    WriteVideoHeader(w,h,2,8);
  end;
  FFits.VideoStream:=FVideoStream;
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
  Ffits.Header.Add('EXTEND',true,'FITS dataset may contain extensions');
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

end.

