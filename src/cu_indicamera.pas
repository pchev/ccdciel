unit cu_indicamera;

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

{$define debug_raw}

interface

uses cu_camera, indibaseclient, indiblobclient, indibasedevice, indiapi, indicom, ws_websocket2, u_libraw,
     cu_fits, u_global, u_utils, math, ExtCtrls, Forms, Classes, SysUtils, LCLType, LCLVersion, u_translation;

type

T_indicamera = class;
TIndiWebSocketClientConnection = class(TWebSocketClientConnection)
  fFramedText: string;
  fFramedStream: TMemoryStream;
  FonNewFile: TNotifyEvent;
  procedure ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;
  procedure ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;
  procedure ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;
  procedure ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;
  procedure SyncBinFrame;
public
  constructor Create(aHost, aPort, aResourceName: string;
       aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
       aCookie: string = '-'; aVersion: integer = 8); override;
  destructor Destroy; override;
  property onNewFile: TNotifyEvent read FonNewFile write FonNewFile;
end;

T_indicamera = class(T_camera)
private
   indiclient: TIndiBaseClient;
   indiblob: TIndiBlobClient;
   indiws: TIndiWebSocketClientConnection;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   GetCCDSizeTimer: TTimer;
   CCDDevice: Basedevice;
   CCDport: ITextVectorProperty;
   CCDexpose: INumberVectorProperty;
   CCDexposeValue: INumber;
   CCDbinning: INumberVectorProperty;
   CCDbinX,CCDbinY: INumber;
   CCDframe: INumberVectorProperty;
   CCDframeX,CCDframeY,CCDframeWidth,CCDframeHeight: INumber;
   CCDframeReset: ISwitchVectorProperty;
   FrameReset: ISwitch;
   CCDFrameType: ISwitchVectorProperty;
   FrameLight, FrameBias, FrameDark,FrameFlat: ISwitch;
   CCDPreview: ISwitchVectorProperty;
   CCDPreviewDisabled: ISwitch;
   CCDCompression: ISwitchVectorProperty;
   CCDcompress, CCDraw: ISwitch;
   CCDimageFormat: ISwitchVectorProperty;
   CCDimageFits,CCDimageXisf,CCDimageRaw,CCDimageJpeg: ISwitch;
   CCDWebsocket: ISwitchVectorProperty;
   CCDWebsocketON, CCDWebsocketOFF: ISwitch;
   CCDWebsocketSetting: INumberVectorProperty;
   CCDAbortExposure: ISwitchVectorProperty;
   CCDAbort: ISwitch;
   CCDCooler: ISwitchVectorProperty;
   CCDCoolerOn,CCDCoolerOff: ISwitch;
   CCDTemperature: INumberVectorProperty;
   CCDinfo: INumberVectorProperty;
   CCDmaxx,CCDmaxy,CCDpixelsize,CCDpixelsizex,CCDpixelsizey,CCDbitperpixel : INumber;
   CCDColorSpace: ISwitchVectorProperty;
   CCDColorGray,CCDColorRGB: ISwitch;
   CCDCfa: ITextVectorProperty;
   CfaOffsetX,CfaOffsetY,CfaType: IText;
   CCDVideoStream: ISwitchVectorProperty;
   VideoStreamOn,VideoStreamOff: ISwitch;
   RecordOptions: INumberVectorProperty;
   RecordOptionDuration,RecordOptionFrames: INumber;
   RecordFile: ITextVectorProperty;
   RecordFileDir,RecordFilename: IText;
   RecordStream: ISwitchVectorProperty;
   RecordStreamOn,RecordStreamOff,RecordDuration,RecordFrames: ISwitch;
   CCDVideoSize:ISwitchVectorProperty;
   CCDVideoRates:ISwitchVectorProperty;
   VideoFPS: INumberVectorProperty;
   FPSest,FPSavg: INumber;
   ImageAdjustments: INumberVectorProperty;
   IBrightness,IGamma,IGain,IExposure:INumber;
   StreamOptions: INumberVectorProperty;
   StreamRate:INumber;
   CCDIso: ISwitchVectorProperty;
   Guiderexpose: INumberVectorProperty;
   GuiderexposeValue: INumber;
   Guiderbinning: INumberVectorProperty;
   GuiderbinX,GuiderbinY: INumber;
   GuiderCompression: ISwitchVectorProperty;
   Guidercompress, Guiderraw: ISwitch;
   GuiderAbortExposure: ISwitchVectorProperty;
   GuiderAbort: ISwitch;
   Guiderinfo: INumberVectorProperty;
   Guidermaxx,Guidermaxy,Guiderpixelsize,Guiderpixelsizex,Guiderpixelsizey,Guiderbitperpixel : INumber;
   WheelSlot: INumberVectorProperty;
   Slot: INumber;
   FilterName: ITextVectorProperty;
   ActiveDevices: ITextVectorProperty;
   UploadMode: ISwitchVectorProperty;
   UploadClient, UploadLocal, UploadBoth: ISwitch;
   UploadSettings: ITextVectorProperty;
   UploadDir, UploadPrefix: IText;
   CCDfilepath: ITextVectorProperty;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   CameraFnumber: INumberVectorProperty;
   CameraFnumberValue: INumber;
   CameraAperture: ISwitchVectorProperty;
   FhasBlob,Fready,FWheelReady,Fconnected,UseMainSensor: boolean;
   Findiserver, Findiserverport, Findidevice, Findisensor, Findideviceport: string;
   FVideoMsg: boolean;
   lockvideostream:boolean;
   timedout: double;
   ExposureTimer: TTimer;
   stX,stY,stWidth,stHeight: integer;
   FSensorList: TStringList;
   FNotAbortSequence: boolean;
   procedure ExposureTimerTimer(sender: TObject);
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure GetCCDSizeTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewBlobProperty(indiProp: IndiProperty);
   procedure NewBlob(bp: IBLOB);
   procedure NewBlobMessage(mp: IMessage);
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(mp: IMessage);
   procedure NewProperty(indiProp: IndiProperty);
   procedure NewNumber(nvp: INumberVectorProperty);
   procedure NewText(tvp: ITextVectorProperty);
   procedure NewSwitch(svp: ISwitchVectorProperty);
   procedure NewLight(lvp: ILightVectorProperty);
   procedure NewImageFile(ft: string; sz,blen:integer; data: TMemoryStream);
   procedure DeleteDevice(dp: Basedevice);
   procedure DeleteProperty(indiProp: IndiProperty);
   procedure ServerConnected(Sender: TObject);
   procedure ServerDisconnected(Sender: TObject);
   procedure LoadConfig;
   procedure ConnectWs;
   procedure DisconnectWs;
   procedure WsNewFile(Sender: TObject);
 protected
   function GetBinX:integer; override;
   function GetBinY:integer; override;
   procedure SetFrametype(f:TFrameType); override;
   function  GetFrametype:TFrameType; override;
   function GetBinXrange:TNumRange; override;
   function GetBinYrange:TNumRange; override;
   function GetExposureRange:TNumRange; override;
   function GetTemperatureRange:TNumRange; override;
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   function  GetTemperature: double; override;
   procedure SetTemperature(value:double); override;
   function  GetCooler: boolean; override;
   procedure SetCooler(value:boolean); override;
   function GetMaxX: double; override;
   function GetMaxY: double; override;
   function GetMaxADU: double; override;
   function GetPixelSize: double; override;
   function GetPixelSizeX: double; override;
   function GetPixelSizeY: double; override;
   function GetBitperPixel: double; override;
   function GetImageFormat: string; override;
   function GetColor: boolean;  override;
   procedure SetTimeout(num:integer); override;
   function GetVideoPreviewRunning: boolean;  override;
   function GetMissedFrameCount: cardinal; override;
   function GetVideoRecordDuration:integer; override;
   procedure SetVideoRecordDuration(value:integer); override;
   function GetVideoRecordFrames:integer; override;
   procedure SetVideoRecordFrames(value:integer); override;
   function GetVideoSize:string; override;
   procedure SetVideoSize(value:string); override;
   function GetVideoRate:string;override;
   procedure SetVideoRate(value:string); override;
   function GetFPS:double; override;
   function GetVideoRecordDir:string; override;
   procedure SetVideoRecordDir(value:string); override;
   function GetVideoRecordFile:string; override;
   procedure SetVideoRecordFile(value:string); override;
   function GetVideoExposure:integer; override;
   function GetVideoGain:integer; override;
   function GetVideoGamma:integer; override;
   function GetVideoBrightness:integer; override;
   procedure SetVideoExposure(value:integer); override;
   procedure SetVideoGain(value:integer); override;
   procedure SetVideoGamma(value:integer); override;
   procedure SetVideoBrightness(value:integer); override;
   function GetVideoExposureRange:TNumRange; override;
   function GetVideoGainRange:TNumRange; override;
   function GetVideoGammaRange:TNumRange; override;
   function GetVideoBrightnessRange:TNumRange; override;
   function GetVideoPreviewDivisor:integer; override;
   procedure SetVideoPreviewDivisor(value:integer); override;
   procedure SetGain(value: integer); override;
   function GetGain: integer; override;
   procedure SetReadOutMode(value: integer); override;
   function GetReadOutMode: integer; override;
   procedure SetFnumber(value: string); override;
   function GetFnumber: string; override;

 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   Procedure Disconnect; override;
   Procedure StartExposure(exptime: double); override;
   procedure RestartExposure; override;
   Procedure SetBinning(sbinX,sbinY: integer); override;
   procedure SetFrame(x,y,width,height: integer); override;
   procedure GetFrame(out x,y,width,height: integer; refresh:boolean=false); override;
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); override;
   procedure ResetFrame; override;
   procedure CfaInfo(out OffsetX, OffsetY: integer; out CType: string);  override;
   function  CheckGain:boolean; override;
   Procedure AbortExposure; override;
   procedure AbortExposureButNotSequence; override;
   Procedure SetActiveDevices(afocuser,afilters,atelescope: string); override;
   procedure StartVideoPreview; override;
   procedure StopVideoPreview; override;
   procedure StartVideoRecord(mode:TVideoRecordMode); override;
   procedure StopVideoRecord; override;
end;

implementation
uses
{$if lcl_major > 1}
LazSysUtils;
{$else}
LazUTF8SysUtils;
{$endif}

procedure T_indicamera.CreateIndiClient;
begin
if csDestroying in ComponentState then exit;
  indiclient:=TIndiBaseClient.Create;
  indiclient.Timeout:=FTimeOut;
  indiclient.onNewDevice:=@NewDevice;
  indiclient.onNewMessage:=@NewMessage;
  indiclient.onNewProperty:=@NewProperty;
  indiclient.onNewNumber:=@NewNumber;
  indiclient.onNewText:=@NewText;
  indiclient.onNewSwitch:=@NewSwitch;
  indiclient.onNewLight:=@NewLight;
  indiclient.onDeleteDevice:=@DeleteDevice;
  indiclient.onDeleteProperty:=@DeleteProperty;
  indiclient.onServerConnected:=@ServerConnected;
  indiclient.onServerDisconnected:=@ServerDisconnected;
  indiblob:=TIndiBlobClient.Create;
  indiblob.Timeout:=FTimeOut;
  indiblob.onNewProperty:=@NewBlobProperty;
  indiblob.onNewBlob:=@NewBlob;
  indiblob.onNewMessage:=@NewBlobMessage;
  {$ifdef camera_debug}
  indiclient.ProtocolTrace:=true;  // this traces can be very big, never include that in a release
  indiclient.ProtocolRawFile:='/tmp/ccdciel_indicamera.raw';
  indiclient.ProtocolTraceFile:='/tmp/ccdciel_indicamera.log';
  indiclient.ProtocolErrorFile:='/tmp/ccdciel_indicamera.err';
  {$endif}
  ClearStatus;
end;

constructor T_indicamera.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FCameraInterface:=INDI;
 FVerticalFlip:=true;
 FSensorList:=TStringList.Create;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findisensor:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=60000;
 InitTimer.OnTimer:=@InitTimerTimer;
 ConnectTimer:=TTimer.Create(nil);
 ConnectTimer.Enabled:=false;
 ConnectTimer.Interval:=3000;
 ConnectTimer.OnTimer:=@ConnectTimerTimer;
 ExposureTimer:=TTimer.Create(nil);
 ExposureTimer.Enabled:=false;
 ExposureTimer.Interval:=1000;
 ExposureTimer.OnTimer:=@ExposureTimerTimer;
 GetCCDSizeTimer:=TTimer.Create(nil);
 GetCCDSizeTimer.Enabled:=false;
 GetCCDSizeTimer.Interval:=100;
 GetCCDSizeTimer.OnTimer:=@GetCCDSizeTimerTimer;
 CreateIndiClient;
 lockvideostream:=false;
 FVideoMsg:=false;
end;

destructor  T_indicamera.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 ExposureTimer.Enabled:=false;
 GetCCDSizeTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 indiblob.Free;
 FSensorList.Free;
 FreeAndNil(ExposureTimer);
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 FreeAndNil(GetCCDSizeTimer);
 inherited Destroy;
end;

procedure T_indicamera.ClearStatus;
begin
    CCDDevice:=nil;
    CCDport:=nil;
    CCDexpose:=nil;
    CCDbinning:=nil;
    CCDframe:=nil;
    CCDframeReset:=nil;
    CCDFrameType:=nil;
    CCDPreview:=nil;
    CCDCompression:=nil;
    CCDimageFormat:=nil;
    CCDWebsocket:=nil;
    CCDWebsocketSetting:=nil;
    CCDAbortExposure:=nil;
    CCDColorSpace:=nil;
    CCDCfa:=nil;
    FhasCfaInfo:=false;
    CCDVideoStream:=nil;
    RecordOptions:=nil;
    RecordFile:=nil;
    RecordStream:=nil;
    CCDVideoSize:=nil;
    CCDVideoRates:=nil;
    VideoFPS:=nil;
    ImageAdjustments:=nil;
    IBrightness:=nil;
    IGamma:=nil;
    IGain:=nil;
    IExposure:=nil;
    StreamOptions:=nil;
    CCDCooler:=nil;
    CCDTemperature:=nil;
    CCDinfo:=nil;
    Guiderexpose:=nil;
    Guiderbinning:=nil;
    GuiderCompression:=nil;
    GuiderAbortExposure:=nil;
    Guiderinfo:=nil;
    WheelSlot:=nil;
    FilterName:=nil;
    ActiveDevices:=nil;
    UploadMode:=nil;
    UploadSettings:=nil;
    CCDfilepath:=nil;
    configprop:=nil;
    CameraFnumber:=nil;
    FhasFnumber:=false;
    CameraAperture:=nil;
    FhasBlob:=false;
    FhasVideo:=false;
    Fready:=false;
    FWheelReady:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    FWheelStatus:=devDisconnected;
    CCDIso:=nil;
    FhasGainISO:=false;
    FhasGain:=false;
    FISOList.Clear;
    FCameraXSize:=-1;
    FCameraYSize:=-1;
    stX:=-1;
    stY:=-1;
    stWidth:=-1;
    stHeight:=-1;
    FSensorList.Clear;
    FNotAbortSequence:=false;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
end;

procedure T_indicamera.CheckStatus;
begin
    if Fconnected and
       ((configprop<>nil)or(not FAutoloadConfig)) and
       FhasBlob and (
       ((Findisensor='CCD2')and(Guiderexpose<>nil))or
       ((Findisensor<>'CCD2')and(CCDexpose<>nil))
        ) and
       (CCDframe<>nil) and
       (FCameraXSize<0) and
       (FCameraYSize<0) and
       (not GetCCDSizeTimer.Enabled)
    then begin
       GetCCDSizeTimer.Enabled:=true;
       UseMainSensor:=(Findisensor<>'CCD2');
    end;
    if Fconnected and
       (WheelSlot<>nil) and
       (FilterName<>nil)
    then begin
      FWheelStatus:=devConnected;
      if (not FWheelReady) then begin
        FWheelReady:=true;
        if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
      end;
    end;
    if Fconnected and
       (not hasVideo) and
       (CCDVideoStream<>nil) and
       (RecordOptions <>nil) and
       (RecordFile<>nil) and
       (RecordStream<>nil)
    then begin
       FhasVideo:=true;
       if Assigned(FonStatusChange) then FonStatusChange(self);
    end;
end;

Procedure T_indicamera.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
if (indiclient=nil)or(indiclient.Terminated)or(indiblob=nil)or(indiblob.Terminated) then CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Fdevice:=cp3;
  Findisensor:=cp4;
  Findideviceport:=cp5;
  FStatus := devDisconnected;
  FWheelStatus:=devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  indiblob.SetServer(Findiserver,Findiserverport);
  indiblob.watchDevice(Findidevice);
  indiblob.ConnectServer;
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  FWheelStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg('Camera already connected',0);
end;

procedure T_indicamera.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
       msg(rsNoResponseFr,0);
       msg('Is "'+Findidevice+'" a running camera driver?',0);
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS',0)
    else if not FhasBlob then
       msg('Missing property INDI_BLOB',0)
    else if ((Findisensor='CCD2')and(Guiderexpose=nil)) then
       msg(Findisensor+' missing property GUIDER_EXPOSURE',0)
    else if ((Findisensor<>'CCD2')and(CCDexpose=nil)) then
       msg(Findisensor+' missing property CCD_EXPOSURE',0)
    else if (CCDframe=nil) then
       msg(Findisensor+' missing property CCD_FRAME',0);
    Disconnect;
  end;
end;

Procedure T_indicamera.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
indiblob.Terminate;
ClearStatus;
end;

procedure T_indicamera.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indicamera.GetCCDSizeTimerTimer(Sender: TObject);
var xr,yr,widthr,heightr: TNumRange;
begin
 GetCCDSizeTimer.Enabled:=false;
 FCameraXSize:=0;
 FCameraYSize:=0;
 if UseMainSensor then begin
   GetFrameRange(xr,yr,widthr,heightr);
   FCameraXSize:=round(widthr.max);
   FCameraYSize:=round(heightr.max);
   stWidth:=-1;
   GetFrame(stX,stY,stWidth,stHeight);
 end;
 if (not Fready) then begin
    Fready:=true;
    if FAutoloadConfig then begin
      LoadConfig;
    end;
    if Assigned(FonStatusChange) then FonStatusChange(self);
 end;
end;

procedure T_indicamera.ConnectTimerTimer(Sender: TObject);
var i: integer;
begin
 ConnectTimer.Enabled:=False;
 if ((not FhasBlob) or (CCDport=nil)) and (not Fready) and InitTimer.Enabled then begin
   ConnectTimer.Enabled:=true;
 end;
 if (CCDport<>nil)and(Findideviceport<>'') then begin
     CCDport.tp[0].text:=Findideviceport;
     indiclient.sendNewText(CCDport);
     msg('Set port '+Findideviceport);
 end;
 indiclient.connectDevice(Findidevice);
 if FhasBlob then begin
   indiblob.setBLOBMode(B_ONLY,Findidevice);
   for i:=0 to FSensorList.Count-1 do begin
     if FSensorList[i]<>Findisensor then
        indiblob.setBLOBMode(B_NEVER,Findidevice,FSensorList[i]);
   end;
 end;
 if Fready and (FStatus<>devConnected) then begin
   if (CCDWebsocket<>nil)and(CCDWebsocketON.s=ISS_ON) then
      ConnectWs;
   FStatus := devConnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
 end;
end;

procedure T_indicamera.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  FWheelStatus := devDisconnected;
  if (indiws<>nil)and(not indiws.IsTerminated) then DisconnectWs;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,0);
  CreateIndiClient;
end;

procedure T_indicamera.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     CCDDevice:=dp;
  end;
end;

procedure T_indicamera.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indicamera.DeleteProperty(indiProp: IndiProperty);
begin
  if indiProp.getName='CCD_WEBSOCKET_SETTINGS' then
     CCDWebsocketSetting:=nil;
end;

procedure T_indicamera.NewMessage(mp: IMessage);
begin
  if FVideoMsg then begin
    msg(mp.msg,3);
  end
  else begin
    if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  end;
  mp.Free;
end;

procedure T_indicamera.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    TxtProp: ITextVectorProperty;
    Txt: IText;
    buf: string;
    i,fmin,fmax: integer;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(CCDport=nil)and(propname='DEVICE_PORT') then begin
     CCDport:=indiProp.getText;
  end
  else if (proptype=INDI_TEXT)and(propname='DRIVER_INFO') then begin
     buf:='';
     TxtProp:=indiProp.getText;
     if TxtProp<>nil then begin
       Txt:=IUFindText(TxtProp,'DRIVER_EXEC');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text+', ';
       Txt:=IUFindText(TxtProp,'DRIVER_VERSION');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text+', ';
       Txt:=IUFindText(TxtProp,'DRIVER_INTERFACE');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text;
       msg(buf,9);
     end;
  end
  else if (proptype=INDI_SWITCH)and(configprop=nil)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     if (configload=nil)or(configsave=nil) then configprop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(CCDexpose=nil)and(propname='CCD_EXPOSURE') then begin
     CCDexpose:=indiProp.getNumber;
     CCDexposeValue:=IUFindNumber(CCDexpose,'CCD_EXPOSURE_VALUE');
     if CCDexposeValue=nil then CCDexpose:=nil;
  end
  else if (proptype=INDI_NUMBER)and(CCDbinning=nil)and(propname='CCD_BINNING') then begin
     CCDbinning:=indiProp.getNumber;
     CCDbinX:=IUFindNumber(CCDbinning,'HOR_BIN');
     CCDbinY:=IUFindNumber(CCDbinning,'VER_BIN');
     if (CCDbinX=nil)or(CCDbinY=nil) then CCDbinning:=nil;
  end
  else if (proptype=INDI_NUMBER)and(CCDframe=nil)and(propname='CCD_FRAME') then begin
     CCDframe:=indiProp.getNumber;
     CCDframeX:=IUFindNumber(CCDframe,'X');
     CCDframeY:=IUFindNumber(CCDframe,'Y');
     CCDframeWidth:=IUFindNumber(CCDframe,'WIDTH');
     CCDframeHeight:=IUFindNumber(CCDframe,'HEIGHT');
     if (CCDframeX=nil)or(CCDframeY=nil)or(CCDframeWidth=nil)or(CCDframeHeight=nil) then CCDframe:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDframeReset=nil)and(propname='CCD_FRAME_RESET') then begin
     CCDframeReset:=indiProp.getSwitch;
     FrameReset:=IUFindSwitch(CCDframeReset,'RESET');
     if FrameReset=nil then CCDframeReset:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDFrameType=nil)and(propname='CCD_FRAME_TYPE') then begin
     CCDFrameType:=indiProp.getSwitch;
     FrameLight:=IUFindSwitch(CCDFrameType,'FRAME_LIGHT');
     FrameBias:=IUFindSwitch(CCDFrameType,'FRAME_BIAS');
     FrameDark:=IUFindSwitch(CCDFrameType,'FRAME_DARK');
     FrameFlat:=IUFindSwitch(CCDFrameType,'FRAME_FLAT');
     if (FrameLight=nil)or(FrameBias=nil)or(FrameDark=nil)or(FrameFlat=nil) then CCDFrameType:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDCompression=nil)and(propname='CCD_COMPRESSION') then begin
     CCDCompression:=indiProp.getSwitch;
     CCDcompress:=IUFindSwitch(CCDCompression,'CCD_COMPRESS');
     if CCDcompress=nil then CCDcompress:=IUFindSwitch(CCDCompression,'COMPRESS');;
     CCDraw:=IUFindSwitch(CCDCompression,'CCD_RAW');
     if CCDraw=nil then CCDraw:=IUFindSwitch(CCDCompression,'RAW');
     if (CCDcompress=nil)or(CCDraw=nil) then CCDCompression:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDPreview=nil)and(propname='CCD_PREVIEW') then begin  // Indigo specific
     CCDPreview:=indiProp.getSwitch;
     CCDPreviewDisabled:=IUFindSwitch(CCDPreview,'DISABLED');
     if (CCDPreviewDisabled=nil) then CCDPreview:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDimageFormat=nil)and(propname='CCD_IMAGE_FORMAT') then begin  // Indigo specific
     CCDimageFormat:=indiProp.getSwitch;
     CCDimageFits:=IUFindSwitch(CCDimageFormat,'FITS');
     CCDimageXisf:=IUFindSwitch(CCDimageFormat,'XISF');
     CCDimageRaw:=IUFindSwitch(CCDimageFormat,'RAW');
     CCDimageJpeg:=IUFindSwitch(CCDimageFormat,'JPEG');
     if (CCDimageFits=nil)or(CCDimageXisf=nil)or(CCDimageRaw=nil)or(CCDimageJpeg=nil) then CCDimageFormat:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDAbortExposure=nil)and(propname='CCD_ABORT_EXPOSURE') then begin
     CCDAbortExposure:=indiProp.getSwitch;
     CCDAbort:=IUFindSwitch(CCDAbortExposure,'ABORT');
     if (CCDAbort=nil) then CCDAbortExposure:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDWebsocket=nil)and(propname='CCD_WEBSOCKET') then begin
     CCDWebsocket:=indiProp.getSwitch;
     CCDWebsocketON:=IUFindSwitch(CCDWebsocket,'WEBSOCKET_ENABLED');
     CCDWebsocketOFF:=IUFindSwitch(CCDWebsocket,'WEBSOCKET_DISABLED');
     if (CCDWebsocketON=nil)or(CCDWebsocketOFF=nil) then CCDWebsocket:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_WEBSOCKET_SETTINGS') then begin
     CCDWebsocketSetting:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(CCDCooler=nil)and(propname='CCD_COOLER') then begin
     CCDCooler:=indiProp.getSwitch;
     CCDCoolerOn:=IUFindSwitch(CCDCooler,'COOLER_ON');
     CCDCoolerOff:=IUFindSwitch(CCDCooler,'COOLER_OFF');
     if (CCDCoolerOn=nil)or(CCDCoolerOff=nil) then CCDCooler:=nil;
  end
  else if (proptype=INDI_NUMBER)and(CCDTemperature=nil)and(propname='CCD_TEMPERATURE') then begin
     CCDTemperature:=indiProp.getNumber;
  end
  else if (proptype=INDI_NUMBER)and(CCDinfo=nil)and(propname='CCD_INFO') then begin
     CCDinfo:=indiProp.getNumber;
     CCDmaxx:=IUFindNumber(CCDinfo,'CCD_MAX_X');
     CCDmaxy:=IUFindNumber(CCDinfo,'CCD_MAX_Y');
     CCDpixelsize:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE');
     CCDpixelsizex:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE_X');
     CCDpixelsizey:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE_Y');
     CCDbitperpixel:=IUFindNumber(CCDinfo,'CCD_BITSPERPIXEL');
     if (CCDmaxx=nil)or(CCDmaxy=nil)or(CCDpixelsize=nil)or(CCDpixelsizex=nil)or(CCDpixelsizey=nil)or(CCDbitperpixel=nil) then CCDinfo:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDColorSpace=nil)and(propname='CCD_COLOR_SPACE') then begin
     CCDColorSpace:=indiProp.getSwitch;
     CCDColorGray:=IUFindSwitch(CCDColorSpace,'CCD_COLOR_GRAY');
     CCDColorRGB:=IUFindSwitch(CCDColorSpace,'CCD_COLOR_RGB');
     if (CCDColorGray=nil)or(CCDColorRGB=nil) then CCDColorSpace:=nil;
  end
  else if (proptype=INDI_NUMBER)and(Guiderexpose=nil)and(propname='GUIDER_EXPOSURE') then begin
     Guiderexpose:=indiProp.getNumber;
     GuiderexposeValue:=IUFindNumber(Guiderexpose,'GUIDER_EXPOSURE_VALUE');
     if GuiderexposeValue=nil then Guiderexpose:=nil;
  end
  else if (proptype=INDI_NUMBER)and(Guiderbinning=nil)and(propname='GUIDER_BINNING') then begin
     Guiderbinning:=indiProp.getNumber;
     GuiderbinX:=IUFindNumber(Guiderbinning,'HOR_BIN');
     GuiderbinY:=IUFindNumber(Guiderbinning,'VER_BIN');
     if (GuiderbinX=nil)or(GuiderbinY=nil) then Guiderbinning:=nil;
  end
  else if (proptype=INDI_SWITCH)and(GuiderCompression=nil)and(propname='GUIDER_COMPRESSION') then begin
     GuiderCompression:=indiProp.getSwitch;
     Guidercompress:=IUFindSwitch(GuiderCompression,'GUIDER_COMPRESS');
     if Guidercompress=nil then Guidercompress:=IUFindSwitch(GuiderCompression,'COMPRESS');;
     Guiderraw:=IUFindSwitch(GuiderCompression,'GUIDER_RAW');
     if Guiderraw=nil then Guiderraw:=IUFindSwitch(GuiderCompression,'RAW');
     if (Guidercompress=nil)or(Guiderraw=nil) then GuiderCompression:=nil;
  end
  else if (proptype=INDI_SWITCH)and(GuiderAbortExposure=nil)and(propname='GUIDER_ABORT_EXPOSURE') then begin
     GuiderAbortExposure:=indiProp.getSwitch;
     GuiderAbort:=IUFindSwitch(GuiderAbortExposure,'ABORT');
     if (GuiderAbort=nil) then GuiderAbortExposure:=nil;
  end
  else if (proptype=INDI_NUMBER)and(Guiderinfo=nil)and(propname='GUIDER_INFO') then begin
     Guiderinfo:=indiProp.getNumber;
     Guidermaxx:=IUFindNumber(CCDinfo,'GUIDER_MAX_X');
     Guidermaxy:=IUFindNumber(CCDinfo,'GUIDER_MAX_Y');
     Guiderpixelsize:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE');
     Guiderpixelsizex:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE_X');
     Guiderpixelsizey:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE_Y');
     Guiderbitperpixel:=IUFindNumber(CCDinfo,'GUIDER_BITSPERPIXEL');
     if (Guidermaxx=nil)or(Guidermaxy=nil)or(Guiderpixelsize=nil)or(Guiderpixelsizex=nil)or(Guiderpixelsizey=nil)or(Guiderbitperpixel=nil) then Guiderinfo:=nil;
  end
  else if (proptype=INDI_NUMBER)and(WheelSlot=nil)and(propname='FILTER_SLOT') then begin
     WheelSlot:=indiProp.getNumber;
     Slot:=IUFindNumber(WheelSlot,'FILTER_SLOT_VALUE');
     if Slot=nil then WheelSlot:=nil;
   end
  else if (proptype=INDI_TEXT)and(FilterName=nil)and(propname='FILTER_NAME') then begin
     FilterName:=indiProp.getText;
     FFilterNames.Clear;
     FFilterNames.Add(Filter0);
     for i:=0 to FilterName.ntp-1 do begin  // do not check property name because there is variation
        FFilterNames.Add(FilterName.tp[i].text);
     end;
  end
  else if (proptype=INDI_SWITCH)and(UploadMode=nil)and(propname='UPLOAD_MODE') then begin
     UploadMode:=indiProp.getSwitch;
     UploadClient:=IUFindSwitch(UploadMode,'UPLOAD_CLIENT');
     UploadLocal:=IUFindSwitch(UploadMode,'UPLOAD_LOCAL');
     UploadBoth:=IUFindSwitch(UploadMode,'UPLOAD_BOTH');
     if (UploadClient=nil)or(UploadLocal=nil)or(UploadBoth=nil) then UploadMode:=nil;
  end
  else if (proptype=INDI_TEXT)and(UploadSettings=nil)and(propname='UPLOAD_SETTINGS') then begin
     UploadSettings:=indiProp.getText;
     UploadDir:=IUFindText(UploadSettings,'UPLOAD_DIR');
     UploadPrefix:=IUFindText(UploadSettings,'UPLOAD_PREFIX');
     if (UploadDir=nil)or(UploadPrefix=nil) then UploadSettings:=nil;
  end
  else if (proptype=INDI_TEXT)and(CCDfilepath=nil)and(propname='CCD_FILE_PATH') then begin
     CCDfilepath:=indiProp.getText;
  end
  else if (proptype=INDI_SWITCH)and(CCDVideoStream=nil)and(propname='CCD_VIDEO_STREAM') then begin
     CCDVideoStream:=indiProp.getSwitch;
     VideoStreamOn:=IUFindSwitch(CCDVideoStream,'STREAM_ON');
     VideoStreamOff:=IUFindSwitch(CCDVideoStream,'STREAM_OFF');
     if (VideoStreamOn=nil)or(VideoStreamOff=nil) then CCDVideoStream:=nil;
  end
  else if (proptype=INDI_NUMBER)and(RecordOptions=nil)and(propname='RECORD_OPTIONS') then begin
     RecordOptions:=indiProp.getNumber;
     RecordOptionDuration:=IUFindNumber(RecordOptions,'RECORD_DURATION');
     RecordOptionFrames:=IUFindNumber(RecordOptions,'RECORD_FRAME_TOTAL');
     if (RecordOptionDuration=nil)or(RecordOptionFrames=nil) then RecordOptions:=nil;
  end
  else if (proptype=INDI_TEXT)and(RecordFile=nil)and(propname='RECORD_FILE') then begin
     RecordFile:=indiProp.getText();
     RecordFileDir:=IUFindText(RecordFile,'RECORD_FILE_DIR');
     RecordFilename:=IUFindText(RecordFile,'RECORD_FILE_NAME');
     if (RecordFileDir=nil)or(RecordFilename=nil) then RecordFile:=nil;
  end
  else if (proptype=INDI_SWITCH)and(RecordStream=nil)and(propname='RECORD_STREAM') then begin
     RecordStream:=indiProp.getSwitch;
     RecordStreamOn:=IUFindSwitch(RecordStream,'RECORD_ON');
     RecordStreamOff:=IUFindSwitch(RecordStream,'RECORD_OFF');
     RecordDuration:=IUFindSwitch(RecordStream,'RECORD_DURATION_ON');
     RecordFrames:=IUFindSwitch(RecordStream,'RECORD_FRAME_ON');
     if (RecordStreamOn=nil)or(RecordStreamOff=nil)or(RecordDuration=nil)or(RecordFrames=nil) then RecordStream:=nil;
  end
  else if (proptype=INDI_SWITCH)and(CCDVideoSize=nil)and(propname='V4L2_SIZE_DISCRETE') then begin
     CCDVideoSize:=indiProp.getSwitch;
     FVideoSizes.Clear;
     if CCDVideoSize<>nil then for i:=0 to CCDVideoSize.nsp-1 do begin
        FVideoSizes.Add(CCDVideoSize.sp[i].name);
     end;
  end
  else if (proptype=INDI_SWITCH)and(CCDVideoRates=nil)and(propname='V4L2_FRAMEINT_DISCRETE') then begin
     CCDVideoRates:=indiProp.getSwitch;
     FVideoRates.Clear;
     if CCDVideoRates<>nil then for i:=0 to CCDVideoRates.nsp-1 do begin
        FVideoRates.Add(CCDVideoRates.sp[i].name);
     end;
  end
  else if (proptype=INDI_NUMBER)and(VideoFPS=nil)and(propname='FPS') then begin
     VideoFPS:=indiProp.getNumber;
     FPSest:=IUFindNumber(VideoFPS,'EST_FPS');
     FPSavg:=IUFindNumber(VideoFPS,'AVG_FPS');
     if (FPSest=nil)or(FPSavg=nil) then VideoFPS:=nil;
  end
  else if (proptype=INDI_NUMBER)and(StreamOptions=nil)and(propname='STREAM_OPTIONS') then begin
     StreamOptions:=indiProp.getNumber;
     StreamRate:=IUFindNumber(StreamOptions,'STREAM_RATE');
     if (StreamRate=nil) then StreamOptions:=nil;
  end
  else if (proptype=INDI_NUMBER)and(ImageAdjustments=nil)and((propname='CCD_GAIN')or(propname='CCD_CONTROLS')or(propname='Image Adjustments')) then begin
     ImageAdjustments:=indiProp.getNumber;
     IBrightness:=IUFindNumber(ImageAdjustments,'Brightness');
     IGamma:=IUFindNumber(ImageAdjustments,'Gamma');
     IGain:=IUFindNumber(ImageAdjustments,'Gain');
     if IGain=nil then IGain:=IUFindNumber(ImageAdjustments,'GAIN');
     IExposure:=IUFindNumber(ImageAdjustments,'Exposure');
     if IExposure=nil then IExposure:=IUFindNumber(ImageAdjustments,'Exposure (Absolute)');
     FhasGain:=(IGain<>nil);
     if FhasGain then begin
        FGainMin:=round(IGain.min);
        FGainMax:=round(IGain.max);
        if assigned(FonGainStatus) then FonGainStatus(self);
     end;
  end
  else if (proptype=INDI_SWITCH)and(CCDIso=nil)and((propname='CCD_ISO')or(propname='DSLR_ISO')) then begin
     CCDIso:=indiProp.getSwitch;
     FISOList.Clear;
     for i:=0 to CCDIso.nsp-1 do begin
        if debug_msg then msg('Found ISO: '+CCDIso.sp[i].lbl);
        FISOList.Add(CCDIso.sp[i].lbl);
     end;
     FhasGainISO:=(FISOList.Count>0);
     if assigned(FonGainStatus) then FonGainStatus(self);
  end
  else if (proptype=INDI_TEXT)and(propname='CCD_CFA') then begin
     CCDCfa:=indiProp.getText();
     CfaOffsetX:=IUFindText(CCDCfa,'CFA_OFFSET_X');
     CfaOffsetY:=IUFindText(CCDCfa,'CFA_OFFSET_Y');
     CfaType:=IUFindText(CCDCfa,'CFA_TYPE');
     if (CfaOffsetX=nil)or(CfaOffsetY=nil)or(CfaType=nil) then CCDCfa:=nil;
     FhasCfaInfo:=(CCDCfa<>nil);
  end
  else if (proptype=INDI_NUMBER)and(propname='f-number') then begin
    CameraFnumber:=indiProp.getNumber();
    CameraFnumberValue:=IUFindNumber(CameraFnumber,'f-number');
    if CameraFnumberValue=nil then CameraFnumber:=nil;
    FhasFnumber:=(CameraFnumber<>nil);
    FFNumberList.Clear;
    if FhasFnumber then
      for i:=0 to DefaultFNlistcount-1 do begin
        FFNumberList.Add(DefaultFNlist[i]);
    end;
  end
  else if (proptype=INDI_SWITCH)and(propname='aperture') then begin
    CameraAperture:=indiProp.getSwitch();
    FFNumberList.Clear;
    for i:=0 to CameraAperture.nsp-1 do begin
      msg('aperture: '+CameraAperture.sp[i].Name+blank+CameraAperture.sp[i].lbl);
      FFNumberList.Add(CameraAperture.sp[i].lbl);
    end;
    FhasFnumber:=(FFNumberList.Count>0);
  end
  else if (proptype=INDI_TEXT)and(ActiveDevices=nil)and(propname='ACTIVE_DEVICES') then begin
     ActiveDevices:=indiProp.getText;
  end;
  CheckStatus;
end;

procedure T_indicamera.NewNumber(nvp: INumberVectorProperty);
begin
  if (UseMainSensor and (nvp=CCDexpose))or((not UseMainSensor) and (nvp=Guiderexpose)) then begin
     if debug_msg then msg('progress: '+formatfloat(f1,nvp.np[0].value));
     if nvp.s=IPS_ALERT then begin
        msg(rsError2, 0);
        if assigned(FonAbortExposure) then FonAbortExposure(self);
     end
     else if Assigned(FonExposureProgress) then begin
        if nvp.np[0].value >0 then
           FonExposureProgress(nvp.np[0].value)
        else
           FonExposureProgress(-4);
     end;
  end
  else if nvp=CCDframe then begin
    // ignore CCDFrame change because this can be just a binning requirement, see: https://indilib.org/forum/ccds-dslrs/4956-indi-asi-driver-bug-causes-false-binned-images.html?start=12#38430
  end
  else if nvp=WheelSlot then begin
     if Assigned(FonFilterChange) then FonFilterChange(Slot.value);
  end
  else if nvp=CCDTemperature then begin
     if Assigned(FonTemperatureChange) then FonTemperatureChange(nvp.np[0].value);
  end
  else if nvp=CameraFnumber then begin
     if Assigned(FonFnumberChange) then FonFnumberChange(FormatFloat(f1,CameraFnumberValue.value));
  end
  else if nvp=VideoFPS then begin
     if Assigned(FonFPSChange) then FonFPSChange(self);
  end
  else if nvp=ImageAdjustments then begin
     if Assigned(FonVideoExposureChange) then FonVideoExposureChange(self);
  end
  ;
 end;

procedure T_indicamera.NewText(tvp: ITextVectorProperty);
var i: integer;
    ft: string;
    data: TMemoryStream;
begin
if tvp=CCDfilepath then begin
  if tvp.s=IPS_OK then begin // ignore extra messages from Indigo
    if debug_msg then msg('receive file '+CCDfilepath.tp[0].text);
    {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'INDI receive new file path '+CCDfilepath.tp[0].text );{$endif}
    ft:=ExtractFileExt(CCDfilepath.tp[0].text);
    data:=TMemoryStream.Create;
    try
      {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Load from file');{$endif}
      data.LoadFromFile(CCDfilepath.tp[0].text);
      {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Delete file');{$endif}
      DeleteFile(CCDfilepath.tp[0].text);
      NewImageFile(ft,0,data.Size,data);
    finally
      data.Free;
    end;
  end;
end
else if tvp=FilterName then begin
    FFilterNames.Clear;
    FFilterNames.Add(Filter0);
    for i:=0 to tvp.ntp-1 do begin
       FFilterNames.Add(tvp.tp[i].text);
    end;
    if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
end;
end;

procedure T_indicamera.NewSwitch(svp: ISwitchVectorProperty);
var propname: string;
    sw: ISwitch;
    cool: boolean;
    i: integer;
begin
  //  writeln('NewSwitch: '+svp.name);
  propname:=svp.name;
  if (propname='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      if Assigned(FonCameraDisconnected) then FonCameraDisconnected(self);
    end;
  end
  else if svp=CCDAbortExposure then begin
   if FNotAbortSequence then begin
    FNotAbortSequence:=false;
   end
   else begin
    if UseMainSensor then begin
      if Assigned(FonAbortExposure) then FonAbortExposure(self);
    end;
   end;
  end
  else if svp=GuiderAbortExposure then begin
    if (not UseMainSensor) then begin
      if Assigned(FonAbortExposure) then FonAbortExposure(self);
    end;
  end
  else if svp=CCDCooler then begin
      cool:=GetCooler;
      if Assigned(FonCoolerChange) then FonCoolerChange(cool);
  end
  else if svp=CameraAperture then begin
     sw:=IUFindOnSwitch(CameraAperture);
     if Assigned(FonFnumberChange) then FonFnumberChange(sw.lbl);
  end
  else if svp=CCDVideoStream then begin
      if Assigned(FonVideoPreviewChange) then FonVideoPreviewChange(self);
  end
  else if svp=CCDVideoSize then begin
      if Assigned(FonVideoSizeChange) then FonVideoSizeChange(self);
  end
  else if svp=CCDVideoRates then begin
      if Assigned(FonVideoRateChange) then FonVideoRateChange(self);
  end
  else if svp=CCDWebsocket then begin
      if (CCDWebsocketON.s=ISS_ON) then
         ConnectWs
      else if (CCDWebsocketOFF.s=ISS_ON) then
         DisconnectWs;
  end
  else if svp=CCDIso then begin
     FISOList.Clear;
     for i:=0 to CCDIso.nsp-1 do begin
        if debug_msg then msg('Found ISO: '+CCDIso.sp[i].lbl);
        FISOList.Add(CCDIso.sp[i].lbl);
     end;
     FhasGainISO:=(FISOList.Count>0);
     if assigned(FonGainStatus) then FonGainStatus(self);
  end
  ;
end;

procedure T_indicamera.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

/// Blob

procedure T_indicamera.NewBlobProperty(indiProp: IndiProperty);
var proptype: INDI_TYPE;
    propname: string;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;
  if (proptype = INDI_BLOB) then begin
     FSensorList.Add(propname);
     if propname=Findisensor then FhasBlob:=true;
     CheckStatus;
  end
end;

procedure T_indicamera.NewBlob(bp: IBLOB);
begin
 if debug_msg then msg('receive blob');
 if bp.bloblen>0 then begin
   bp.blob.Position:=0;
   NewImageFile(bp.format,bp.size,bp.bloblen,bp.blob);
 end;
end;

procedure T_indicamera.NewBlobMessage(mp: IMessage);
begin
  // message is processed in main client
  mp.Free;
end;

///

procedure T_indicamera.NewImageFile(ft: string; sz,blen:integer; data: TMemoryStream);
var source,dest: array of char;
    sourceLen,destLen:UInt64;
    i: integer;
    tmpf,rmsg: string;
begin
 // report any change to NewText() in use with RAM disk transfer
 {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'INDI receive blob');{$endif}
 ExposureTimer.Enabled:=false;
 FMidExposureTime:=(Ftimestart+NowUTC)/2;
 if debug_msg then msg('receive blob');
 // if possible start next exposure now
 TryNextExposure(FImgNum);
 ft:=trim(ft);
 if blen>0 then begin
   data.Position:=0;
   if RightStr(ft,2)='.z' then begin //compressed
        if zlibok then begin
          {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'uncompress file');{$endif}
          if debug_msg then msg('uncompress file');
          sourceLen:=blen;
          if sz>0 then
             destLen:=sz
          else if CCDframe<>nil then
             destLen:=round(CCDframeWidth.value*CCDframeHeight.value*2)+(10*2880)
          else
             destLen:=10000*10000*2;
          SetLength(source,sourceLen);
          SetLength(dest,destLen);
          data.Read(source[0],sourceLen);
          i:=uncompress(@dest[0],@destLen,@source[0],sourceLen);
          if i=0 then begin
             data.Clear;
             data.Write(dest[0],destLen);
          end;
          SetLength(source,0);
          SetLength(dest,0);
          ft:=LeftStr(ft,length(ft)-2);
        end;
   end;
   if ft='.fits.fz' then begin // receive a packed FITS file
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'funpack');{$endif}
     FImageFormat:='.fits';
     if assigned(FonExposureProgress) then FonExposureProgress(-10);
     if debug_msg then msg('this is a packed fits file');
     if debug_msg then msg('run funpack');
     tmpf:=slash(TmpDir)+'tmppack.fits.fz';
     data.SaveToFile(tmpf);
     i:=UnpackFits(tmpf,FImgStream,rmsg);
     if i<>0 then begin
        msg('funpack error '+inttostr(i)+': '+rmsg,0);
        if assigned(FonAbortExposure) then FonAbortExposure(self);
        exit;
     end;
     DeleteFile(tmpf);
     if debug_msg then msg('NewImage');
     if assigned(FonExposureProgress) then FonExposureProgress(-11);
     NewImage;
   end
   else if ft='.fits' then begin // receive a FITS file
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'received fits file');{$endif}
     FImageFormat:='.fits';
     if assigned(FonExposureProgress) then FonExposureProgress(-10);
     if debug_msg then msg('this is a fits file');
     if debug_msg then msg('copy stream');
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'copy blob stream');{$endif}
     FImgStream.Clear;
     FImgStream.Position:=0;
     FImgStream.CopyFrom(data,data.Size);
     if debug_msg then msg('NewImage');
     if assigned(FonExposureProgress) then FonExposureProgress(-11);
     NewImage;
   end
   else if (ft='.jpeg')or(ft='.jpg')or(ft='.tiff')or(ft='.png') then begin // receive an image file
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'received '+ft+' file');{$endif}
     FImageFormat:=ft;
     if assigned(FonExposureProgress) then FonExposureProgress(-10);
     if debug_msg then msg('this is a '+ft+' file');
     if debug_msg then msg('copy '+ft+' stream to fits');
     msg(Format(rsWarningImage, [uppercase(ft)]), 1);
     PictureToFits(data,copy(ft,2,99),FImgStream,false,GetPixelSizeX,GetPixelSizeY,GetBinX,GetBinY);
     if FImgStream.Size<2880 then begin
        msg('Invalid file received '+ft,0);
        AbortExposure;
     end;
     if debug_msg then msg('NewImage');
     if assigned(FonExposureProgress) then FonExposureProgress(-11);
     NewImage;
   end
   else if pos(UpperCase(ft)+',',UpperCase(rawext))>0 then begin
     {$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'received '+ft+' raw file');{$endif}
     FImageFormat:=ft;
     if assigned(FonExposureProgress) then FonExposureProgress(-10);
     if debug_msg then msg('this is a '+ft+' file');
     if debug_msg then msg('copy '+ft+' stream to fits');
     RawToFits(data,FImgStream,rmsg,GetPixelSizeX,GetPixelSizeY,GetBinX,GetBinY);
     if rmsg<>'' then msg(rmsg,1);
     if FImgStream.Size<2880 then begin
        msg('Invalid file received '+ft,0);
        AbortExposure;
     end;
     if debug_msg then msg('NewImage');
     if assigned(FonExposureProgress) then FonExposureProgress(-11);
     NewImage;
   end
   else if pos('.stream',ft)>0 then begin // video stream
     if debug_msg then msg('this is a video stream');
     if lockvideostream then exit; // skip extra frames if we cannot follow the rate
     lockvideostream:=true;
     if debug_msg then msg('process this frame');
     try
       if debug_msg then msg('copy frame');
       FVideoStream.Clear;
       FVideoStream.Position:=0;
       FVideoStream.CopyFrom(data,data.Size);
       if debug_msg then msg('NewVideoFrame');
       NewVideoFrame;
     finally
       lockvideostream:=false;
     end;
   end
   else begin
        FImageFormat:=ft;
        msg('Invalid file format '+ft+', a FITS file is required',0);
        AbortExposure;
        NewImage;
   end;
 end;
end;

Procedure T_indicamera.StartExposure(exptime: double);
begin
FNotAbortSequence:=false;
if FIndiTransfert=itDisk then begin
   if (UploadSettings=nil)or(UploadMode=nil) then
      FIndiTransfert:=itNetwork;
end;
case FIndiTransfert of
  itNetwork:begin
            if (UploadMode<>nil)and(UploadLocal.s=ISS_ON) then begin
               IUResetSwitch(UploadMode);
               UploadClient.s:=ISS_ON;
               indiclient.sendNewSwitch(UploadMode);
            end;
            end;
  itDisk:   begin
            if (UploadClient.s=ISS_ON) then begin
               IUResetSwitch(UploadMode);
               UploadLocal.s:=ISS_ON;
               indiclient.sendNewSwitch(UploadMode);
            end;
            if (UploadDir.text<>FIndiTransfertDir)or(UploadPrefix.text<>FIndiTransfertPrefix) then begin
               UploadDir.text:=FIndiTransfertDir;
               UploadPrefix.text:=FIndiTransfertPrefix;
               indiclient.sendNewText(UploadSettings);
            end;
            end;
end;
if UseMainSensor then begin
  if (not zlibok)and(CCDCompression<>nil)and(CCDcompress.s=ISS_ON) then begin
    IUResetSwitch(CCDCompression);
    CCDraw.s:=ISS_ON;
    indiclient.sendNewSwitch(CCDCompression);
    indiclient.WaitBusy(CCDCompression);
  end;
  if CCDPreview<>nil then begin // Indigo specific
    if CCDPreviewDisabled.s=ISS_OFF then begin
      IUResetSwitch(CCDPreview);
      CCDPreviewDisabled.s:=ISS_ON;
      indiclient.sendNewSwitch(CCDPreview);
    end;
  end;
  if CCDexpose<>nil then begin;
    CCDexposeValue.value:=exptime;
    indiclient.sendNewNumber(CCDexpose);
  end;
end else begin
  if (not zlibok)and(GuiderCompression<>nil)and(Guidercompress.s=ISS_ON) then begin
    IUResetSwitch(GuiderCompression);
    Guiderraw.s:=ISS_ON;
    indiclient.sendNewSwitch(GuiderCompression);
    indiclient.WaitBusy(GuiderCompression);
  end;
  if Guiderexpose<>nil then begin;
    GuiderexposeValue.value:=exptime;
    indiclient.sendNewNumber(Guiderexpose);
  end;
end;
{$ifdef debug_raw}writeln(FormatDateTime(dateiso,Now)+blank+'Start Exposure'+blank+formatfloat(f3,exptime));{$endif}
Fexptime:=exptime;
Ftimestart:=NowUTC;
inc(FImgNum);
timedout:=now+(exptime+CameraTimeout)/secperday;
ExposureTimer.Enabled:=true;
end;

procedure T_indicamera.RestartExposure;
begin
if (Fexptime>0)   then
   StartExposure(Fexptime)
else
   if assigned(FonAbortExposure) then FonAbortExposure(self);
end;

procedure T_indicamera.ExposureTimerTimer(sender: TObject);
begin
 ExposureTimer.Enabled:=false;
 if now>timedout then begin
    msg(rsNoResponseFr2, 0);
    if assigned(FonAbortExposure) then FonAbortExposure(self);
 end
 else
   ExposureTimer.Enabled:=true;
end;

function T_indicamera.GetExposureRange:TNumRange;
begin
if UseMainSensor then begin
 if CCDexpose<>nil then begin
    result.min:=CCDexposeValue.min;
    result.max:=CCDexposeValue.max;
    result.step:=CCDexposeValue.step;
 end
 else result:=NullRange;
end else begin
   if Guiderexpose<>nil then begin
      result.min:=GuiderexposeValue.min;
      result.max:=GuiderexposeValue.max;
      result.step:=GuiderexposeValue.step;
   end
   else result:=NullRange;
end;
end;

Procedure T_indicamera.AbortExposureButNotSequence;
begin
  FNotAbortSequence:=true;
  AbortExposure;
end;

Procedure T_indicamera.AbortExposure;
begin
ExposureTimer.Enabled:=false;
if UseMainSensor then begin
  if CCDAbortExposure<>nil then begin
    IUResetSwitch(CCDAbortExposure);
    CCDAbort.s:=ISS_ON;
    indiclient.sendNewSwitch(CCDAbortExposure);
  end;
end else begin
   if GuiderAbortExposure<>nil then begin
     IUResetSwitch(GuiderAbortExposure);
     GuiderAbort.s:=ISS_ON;
     indiclient.sendNewSwitch(GuiderAbortExposure);
   end;
end;
end;

function T_indicamera.GetBinX:integer;
begin
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    result:=round(CCDbinX.value);
 end
 else result:=1;
end else begin
   if Guiderbinning<>nil then begin
      result:=round(GuiderbinX.value);
   end
   else result:=1;
end;
end;

function T_indicamera.GetBinY:integer;
begin
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    result:=round(CCDbinY.value);
 end
 else result:=1;
end else begin
   if Guiderbinning<>nil then begin
      result:=round(GuiderbinY.value);
   end
   else result:=1;
end;
end;

function T_indicamera.GetBinXrange:TNumRange;
begin
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    result.min:=CCDbinX.min;
    result.max:=CCDbinX.max;
    result.step:=CCDbinX.step;
 end
 else result:=UnitRange;
end else begin
   if Guiderbinning<>nil then begin
      result.min:=GuiderbinX.min;
      result.max:=GuiderbinX.max;
      result.step:=GuiderbinX.step;
   end
   else result:=UnitRange;
end;
end;

function T_indicamera.GetBinYrange:TNumRange;
begin
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    result.min:=CCDbinY.min;
    result.max:=CCDbinY.max;
    result.step:=CCDbinY.step;
 end
 else result:=UnitRange;
end else begin
   if Guiderbinning<>nil then begin
      result.min:=GuiderbinY.min;
      result.max:=GuiderbinY.max;
      result.step:=GuiderbinY.step;
   end
   else result:=UnitRange;
end;
end;

Procedure T_indicamera.SetBinning(sbinX,sbinY: integer);
begin
msg(Format(rsSetBinningX, [inttostr(sbinX), inttostr(sbinY)]));
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    CCDbinX.value:=sbinX;
    CCDbinY.value:=sbinY;
    indiclient.sendNewNumber(CCDbinning);
    indiclient.WaitBusy(CCDbinning);
    if stWidth>0 then SetFrame(stX,stY,stWidth,stHeight);
 end;
end else begin
 if Guiderbinning<>nil then begin
    GuiderbinX.value:=sbinX;
    GuiderbinY.value:=sbinY;
    indiclient.sendNewNumber(Guiderbinning);
    indiclient.WaitBusy(Guiderbinning);
 end;
end;
end;

procedure T_indicamera.SetFrametype(f:TFrameType);
begin
  if UseMainSensor and (CCDFrameType<>nil) then begin
     msg(Format(rsSetFrameType, [FrameName[ord(f)]]));
     IUResetSwitch(CCDFrameType);
     case f of
        LIGHT : FrameLight.s:=ISS_ON;
        BIAS  : FrameBias.s:=ISS_ON;
        DARK  : FrameDark.s:=ISS_ON;
        FLAT  : FrameFlat.s:=ISS_ON;
        else FrameLight.s:=ISS_ON;
     end;
     indiclient.sendNewSwitch(CCDFrameType);
  end;
end;

function  T_indicamera.GetFrametype:TFrameType;
begin
  if UseMainSensor and (CCDFrameType<>nil) then begin
     if FrameLight.s=ISS_ON then result:=LIGHT
     else if FrameBias.s=ISS_ON then result:=BIAS
     else if FrameDark.s=ISS_ON then result:=DARK
     else if FrameFlat.s=ISS_ON then result:=FLAT;
  end
  else result:=LIGHT;
end;

procedure T_indicamera.SetFrame(x,y,width,height: integer);
var Xmax,Ymax: integer;
begin
  if UseMainSensor and (CCDframe<>nil) then begin
     Xmax:=round(FCameraXSize);
     Ymax:=round(FCameraYSize);
     // check range
     if width<MinFrameSize then width:=MinFrameSize;
     if height<MinFrameSize then height:=MinFrameSize;
     if x>(Xmax-MinFrameSize) then x:=Xmax-MinFrameSize;
     if y>(Ymax-MinFrameSize) then y:=Ymax-MinFrameSize;
     if (x+width)>Xmax then width:=Xmax-x;
     if (y+height)>Ymax then height:=Ymax-y;
     // force even values
     x:=round(x+0.5);
     y:=round(y+0.5);
     CCDframeX.value:=x;
     CCDframeY.value:=y;
     CCDframeWidth.value:=width;
     CCDframeHeight.value:=height;
     indiclient.sendNewNumber(CCDframe);
     stX:=x;
     stY:=y;
     stWidth:=width;
     stHeight:=height;
     indiclient.WaitBusy(CCDframe);
     if assigned(FonFrameChange) then FonFrameChange(self);
  end;
end;

procedure T_indicamera.GetFrame(out x,y,width,height: integer; refresh:boolean=false);
begin
  if UseMainSensor and (CCDframe<>nil) then begin
     if (stWidth>0)and(not refresh) then begin
       x := stX;
       y := stY;
       width := stWidth;
       height := stHeight;
     end
     else begin
       x      := round(CCDframeX.value);
       y      := round(CCDframeY.value);
       width  := round(CCDframeWidth.value);
       height := round(CCDframeHeight.value);
       stX:=x;
       stY:=y;
       stWidth:=width;
       stHeight:=height;
     end;
  end else begin
     x:= 0;
     y:= 0;
     width:=0;
     height:=0;
  end;
end;

procedure T_indicamera.GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
begin
  if UseMainSensor and (CCDframe<>nil) then begin
     xr.min:=CCDframeX.min;
     xr.max:=max(CCDframeX.max,FCameraXSize);
     xr.step:=CCDframeX.step;
     yr.min:=CCDframeY.min;
     yr.max:=max(CCDframeY.max,FCameraYSize);
     yr.step:=CCDframeY.step;
     widthr.min:=CCDframeWidth.min;
     widthr.max:=max(CCDframeWidth.max,FCameraXSize);
     widthr.step:=CCDframeWidth.step;
     heightr.min:=CCDframeHeight.min;
     heightr.max:=max(CCDframeHeight.max,FCameraYSize);
     heightr.step:=CCDframeHeight.step;
  end else begin
     xr:= NullRange;
     yr:= NullRange;
     widthr:=NullRange;
     heightr:=NullRange;
  end;
end;

procedure T_indicamera.ResetFrame;
begin
  if UseMainSensor then begin
     // Must not set the binning to 1x1 as CCD_FRAME_RESET do
     if CCDframe<>nil then begin
       CCDframeX.value:=CCDframeX.min;
       CCDframeY.value:=CCDframeY.min;
       CCDframeWidth.value:=FCameraXSize;
       CCDframeHeight.value:=FCameraYSize;
       indiclient.sendNewNumber(CCDframe);
       stX:=round(CCDframeX.min);
       stY:=round(CCDframeY.min);
       stWidth:=FCameraXSize;
       stHeight:=FCameraYSize;
       indiclient.WaitBusy(CCDframe);
       if assigned(FonFrameChange) then FonFrameChange(self);
    end;
  end;
end;

procedure T_indicamera.CfaInfo(out OffsetX, OffsetY: integer; out CType: string);
begin
   if CCDCfa<>nil then begin
      OffsetX:=StrToIntDef(CfaOffsetX.Text,0);
      OffsetY:=StrToIntDef(CfaOffsetY.Text,0);
      CType:=CfaType.Text;
   end
   else begin
     OffsetX:=0;
     OffsetY:=0;
     CType:='';
   end;
end;

function T_indicamera.GetTemperatureRange:TNumRange;
begin
 if CCDTemperature<>nil then begin
    result.min:=CCDTemperature.np[0].min;
    result.max:=CCDTemperature.np[0].max;
    result.step:=CCDTemperature.np[0].step;
 end
 else result:=NullRange;
end;

function  T_indicamera.GetTemperature: double;
begin
 if CCDTemperature<>nil then begin
    result:=CCDTemperature.np[0].value;
 end
 else result:=NullCoord;
end;

procedure T_indicamera.SetTemperature(value:double);
begin
 if CCDTemperature<>nil then begin
    CCDTemperature.np[0].value:=value;
    indiclient.sendNewNumber(CCDTemperature);
 end;
end;

function  T_indicamera.GetCooler: boolean;
begin
 if CCDCooler<>nil then begin
    result:=(CCDCoolerOn.s=ISS_ON)and((CCDCoolerOn.s=ISS_ON)xor(CCDCoolerOff.s=ISS_ON)); // sometime both are ON, bug in driver?
 end
 else result:=false;
end;

procedure T_indicamera.SetCooler(value:boolean);
begin
 if CCDCooler<>nil then begin
    msg(Format(rsSetCooler, [': '+BoolToStr(value, rsTrue, rsFalse)]));
    IUResetSwitch(CCDCooler);
    if value then CCDCoolerOn.s:=ISS_ON
             else CCDCoolerOff.s:=ISS_ON;
    indiclient.sendNewSwitch(CCDCooler);
 end;
end;

Procedure T_indicamera.SetActiveDevices(afocuser,afilters,atelescope: string);
var tp:IText;
begin
  if ActiveDevices<>nil then begin
     if afocuser<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_FOCUSER');
        if tp<>nil then begin
           tp.text:=afocuser;
        end;
     end;
     if afilters<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_FILTER');
        if tp<>nil then begin
           tp.text:=afilters;
        end;
     end;
     if atelescope<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_TELESCOPE');
        if tp<>nil then begin
           tp.text:=atelescope;
        end;
     end;
     indiclient.sendNewText(ActiveDevices);
  end;
end;

procedure T_indicamera.SetFilter(num:integer);
begin
if WheelSlot<>nil then begin;
  msg(Format(rsSetFilterPos, [inttostr(num)]));
  Slot.value:=num;
  indiclient.sendNewNumber(WheelSlot);
  indiclient.WaitBusy(WheelSlot);
end;
end;

function  T_indicamera.GetFilter:integer;
begin
if WheelSlot<>nil then begin;
  result:=round(Slot.value);
end
else result:=0;
end;

procedure T_indicamera.SetFilterNames(value:TStringList);
var i:integer;
begin
if (FilterName<>nil)and(value.Count=FilterName.ntp) then begin
  for i:=0 to value.Count-1 do begin
     FilterName.tp[i].text:=FFilterNames[i];
  end;
  indiclient.sendNewText(FilterName);
  indiclient.WaitBusy(FilterName);
end;
end;

function T_indicamera.GetMaxX: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDmaxx.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guidermaxx.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetMaxY: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDmaxy.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guidermaxy.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetMaxADU: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=2**CCDbitperpixel.value-1;
   end
   else result:=MAXWORD;
  end else begin
     if Guiderinfo<>nil then begin
        result:=2**Guiderbitperpixel.value-1;
     end
     else result:=MAXWORD;
  end;
end;

function T_indicamera.GetPixelSize: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDpixelsize.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guiderpixelsize.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetPixelSizeX: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDpixelsizex.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guiderpixelsizex.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetPixelSizeY: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDpixelsizey.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guiderpixelsizey.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetBitperPixel: double;
begin
  if UseMainSensor then begin
   if CCDinfo<>nil then begin
      result:=CCDbitperpixel.value;
   end
   else result:=-1;
  end else begin
     if Guiderinfo<>nil then begin
        result:=Guiderbitperpixel.value;
     end
     else result:=-1;
  end;
end;

function T_indicamera.GetColor: boolean;
begin
  if CCDColorSpace<>nil then begin
     result:=(CCDColorRGB.s=ISS_ON);
  end
  else begin
    result:=false;
  end;
end;

procedure T_indicamera.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
 indiblob.Timeout:=FTimeOut;
end;

function T_indicamera.CheckGain:boolean;
begin
  result:=(FhasGainISO or FhasGain);
end;

procedure T_indicamera.SetGain(value: integer);
begin
if FCanSetGain then begin
  if (CCDIso<>nil) and FhasGainISO then begin
    IUResetSwitch(CCDIso);
    CCDIso.sp[value].s := ISS_ON;
    indiclient.sendNewSwitch(CCDIso);
  end
  else if (IGain<>nil) and FhasGain then begin
    IGain.value:=value;
    indiclient.sendNewNumber(ImageAdjustments);
  end;
end;
end;

function T_indicamera.GetGain: integer;
var i: integer;
begin
  result:=NullInt;
  if (CCDIso<>nil) and FhasGainISO then begin
    for i := 0 to CCDIso.nsp - 1 do
      if CCDIso.sp[i].s = ISS_ON then begin
         result:=i;
         break;
      end;
  end
  else if (IGain<>nil) and FhasGain then begin
      result:=round(IGain.value);
  end;
end;

procedure T_indicamera.SetReadOutMode(value: integer);
begin
// no INDI property ?
end;

function T_indicamera.GetReadOutMode: integer;
begin
result:=0;
end;

procedure T_indicamera.SetFnumber(value: string);
var x: double;
    i:integer;
    s:ISwitch;
begin
msg('Set aperture '+value);
  if CameraFnumber<>nil then begin
    x:=StrToFloatDef(value,0);
    if x>0 then begin
      CameraFnumberValue.value:=RoundFloat(x,roundf2);
      indiclient.sendNewNumber(CameraFnumber);
    end;
  end
  else if CameraAperture<>nil then begin
    IUResetSwitch(CameraAperture);
    for i:=0 to CameraAperture.nsp-1 do begin
      if CameraAperture.sp[i].lbl=value then begin
         CameraAperture.sp[i].s:=ISS_ON;
         break;
      end;
    end;
    s:=IUFindOnSwitch(CameraAperture);
    if s<>nil then indiclient.sendNewSwitch(CameraAperture);
  end;
end;

function T_indicamera.GetFnumber: string;
var s: ISwitch;
begin
  if CameraFnumber<>nil then
    result:=FormatFloat(f1,CameraFnumberValue.value)
  else if CameraAperture<>nil then begin
    s:=IUFindOnSwitch(CameraAperture);
    result:=trim(s.lbl);
  end
  else
    result:='';
msg('Get aperture '+Result);
end;

function T_indicamera.GetImageFormat: string;
var s: ISwitch;
begin
 if CCDimageFormat=nil then
    result:=FImageFormat
 else begin
    s:=IUFindOnSwitch(CCDimageFormat);
    if s=CCDimageFits then result:='.fits'
    else if s=CCDimageXisf then result:='.xisf'
    else if s=CCDimageRaw then result:='.raw'
    else if s=CCDimageJpeg then result:='.jpeg'
    else result:=FImageFormat;
 end;
end;

procedure T_indicamera.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

procedure T_indicamera.StartVideoPreview;
begin
 if CCDVideoStream<>nil then begin
  FVideoMsg:=true;
  IUResetSwitch(CCDVideoStream);
  VideoStreamOn.s:=ISS_ON;
  indiclient.sendNewSwitch(CCDVideoStream);
 end;
end;

procedure T_indicamera.StopVideoPreview;
begin
 if CCDVideoStream<>nil then begin
  IUResetSwitch(CCDVideoStream);
  VideoStreamOff.s:=ISS_ON;
  indiclient.sendNewSwitch(CCDVideoStream);
 end;
end;

function T_indicamera.GetVideoPreviewRunning: boolean;
begin
 result:=false;
 if CCDVideoStream<>nil then begin
   result:=(VideoStreamOn.s=ISS_ON);
 end;
end;

function T_indicamera.GetMissedFrameCount: cardinal;
begin
 result:=indiclient.MissedFrameCount;
end;

function T_indicamera.GetVideoRecordDuration:integer;
begin
 result:=0;
 if RecordOptions<>nil then begin
    result:=round(RecordOptionDuration.value);
 end;
end;

procedure T_indicamera.SetVideoRecordDuration(value:integer);
begin
 if RecordOptions<>nil then begin;
   RecordOptionDuration.value:=value;
   indiclient.sendNewNumber(RecordOptions);
 end;
end;

function T_indicamera.GetVideoRecordFrames:integer;
begin
  result:=0;
  if RecordOptions<>nil then begin
     result:=round(RecordOptionFrames.value);
  end;
end;

procedure T_indicamera.SetVideoRecordFrames(value:integer);
begin
  if RecordOptions<>nil then begin;
    RecordOptionFrames.value:=value;
    indiclient.sendNewNumber(RecordOptions);
  end;
end;

procedure T_indicamera.StartVideoRecord(mode:TVideoRecordMode);
begin
  if RecordStream<>nil then begin
    FVideoMsg:=true;
    IUResetSwitch(RecordStream);
    case mode of
       rmDuration : RecordDuration.s:=ISS_ON;
       rmFrame    : RecordFrames.s:=ISS_ON;
       rmUnlimited: RecordStreamOn.s:=ISS_ON;
    end;
    indiclient.sendNewSwitch(RecordStream);
  end;
end;

procedure T_indicamera.StopVideoRecord;
begin
  if RecordStream<>nil then begin
    IUResetSwitch(RecordStream);
    RecordStreamOff.s:=ISS_ON;
    indiclient.sendNewSwitch(RecordStream);
  end;
end;

function T_indicamera.GetVideoSize:string;
var sw:ISwitch;
begin
  result:='';
  if CCDVideoSize<>nil then begin
     sw:=IUFindOnSwitch(CCDVideoSize);
     if sw<>nil then result:=sw.name;
  end;
end;

procedure T_indicamera.SetVideoSize(value:string);
var sw:ISwitch;
begin
  if CCDVideoSize<>nil then begin
    sw:=IUFindSwitch(CCDVideoSize,value);
    if sw<>nil then begin
      IUResetSwitch(CCDVideoSize);
      sw.s:=ISS_ON;
      indiclient.sendNewSwitch(CCDVideoSize);
    end;
  end;
end;

function T_indicamera.GetVideoRate:string;
var sw:ISwitch;
begin
  result:='';
  if CCDVideoRates<>nil then begin
     sw:=IUFindOnSwitch(CCDVideoRates);
     if sw<>nil then result:=sw.name;
  end;
end;

procedure T_indicamera.SetVideoRate(value:string);
var sw:ISwitch;
begin
  if CCDVideoRates<>nil then begin
    sw:=IUFindSwitch(CCDVideoRates,value);
    if sw<>nil then begin
      IUResetSwitch(CCDVideoRates);
      sw.s:=ISS_ON;
      indiclient.sendNewSwitch(CCDVideoRates);
    end;
  end;
end;

function T_indicamera.GetFPS:double;
begin
 result:=0;
 if VideoFPS<>nil then begin
   result:=FPSavg.value;
 end;
end;

function T_indicamera.GetVideoRecordDir:string;
begin
 result:='';
 if RecordFile<>nil then begin
   result:=RecordFileDir.text;
 end;
end;

procedure T_indicamera.SetVideoRecordDir(value:string);
begin
 if RecordFile<>nil then begin
   RecordFileDir.text:=value;
   indiclient.sendNewText(RecordFile);
 end;
end;

function T_indicamera.GetVideoRecordFile:string;
begin
 result:='';
 if RecordFile<>nil then begin
   result:=RecordFilename.text;
 end;
end;

procedure T_indicamera.SetVideoRecordFile(value:string);
begin
 if RecordFile<>nil then begin
   RecordFilename.text:=value;
   indiclient.sendNewText(RecordFile);
 end;
end;


function T_indicamera.GetVideoExposure:integer;
begin
 result:=0;
 if IExposure<>nil then begin
    result:=round(IExposure.value);
 end;
end;

function T_indicamera.GetVideoGain:integer;
begin
 result:=NullInt;
 if IGain<>nil then begin
    result:=round(IGain.value);
 end;
end;

function T_indicamera.GetVideoGamma:integer;
begin
 result:=NullInt;
 if IGamma<>nil then begin
    result:=round(IGamma.value);
 end;
end;

function T_indicamera.GetVideoBrightness:integer;
begin
 result:=NullInt;
 if IBrightness<>nil then begin
    result:=round(IBrightness.value);
 end;
end;

procedure T_indicamera.SetVideoExposure(value:integer);
begin
 if IExposure<>nil then begin;
   IExposure.value:=value;
   indiclient.sendNewNumber(ImageAdjustments);
 end;
end;

procedure T_indicamera.SetVideoGain(value:integer);
begin
 if IGain<>nil then begin;
   IGain.value:=value;
   indiclient.sendNewNumber(ImageAdjustments);
 end;
end;

procedure T_indicamera.SetVideoGamma(value:integer);
begin
 if IGamma<>nil then begin;
   IGamma.value:=value;
   indiclient.sendNewNumber(ImageAdjustments);
 end;
end;

procedure T_indicamera.SetVideoBrightness(value:integer);
begin
 if IBrightness<>nil then begin;
   IBrightness.value:=value;
   indiclient.sendNewNumber(ImageAdjustments);
 end;
end;

function T_indicamera.GetVideoExposureRange:TNumRange;
begin
 result:=NullRange;
 if IExposure<>nil then begin
    result.min:=IExposure.min;
    result.max:=IExposure.max;
    result.step:=IExposure.step;
 end
end;

function T_indicamera.GetVideoGainRange:TNumRange;
begin
 result:=NullRange;
 if (IGain<>nil)and(not FhasGainISO) then begin
    result.min:=IGain.min;
    result.max:=IGain.max;
    result.step:=IGain.step;
 end
end;

function T_indicamera.GetVideoGammaRange:TNumRange;
begin
 result:=NullRange;
 if IGamma<>nil then begin
    result.min:=IGamma.min;
    result.max:=IGamma.max;
    result.step:=IGamma.step;
 end
end;

function T_indicamera.GetVideoBrightnessRange:TNumRange;
begin
 result:=NullRange;
 if IBrightness<>nil then begin
    result.min:=IBrightness.min;
    result.max:=IBrightness.max;
    result.step:=IBrightness.step;
 end
end;

function T_indicamera.GetVideoPreviewDivisor:integer;
begin
  result:=0;
  if StreamOptions<>nil then begin
     result:=round(StreamRate.value);
  end;
end;

procedure T_indicamera.SetVideoPreviewDivisor(value:integer);
begin
 if StreamOptions<>nil then begin;
   StreamRate.value:=value;
   indiclient.sendNewNumber(StreamOptions);
 end;
end;

procedure T_indicamera.ConnectWs;
var h,p: string;
    wsport:INumber;
begin
try
  if CCDWebsocketSetting=nil then exit;
  if (indiws<>nil)and(not indiws.IsTerminated) then exit;
  h:=Findiserver;
  wsport:=IUFindNumber(CCDWebsocketSetting,'WS_SETTINGS_PORT');
  if wsport=nil then exit;
  p:=IntToStr(round(wsport.Value));
  msg('Connect to WebSocket '+h+':'+p,3);
  indiws:=TIndiWebSocketClientConnection.Create(h,p,'/');
  indiws.FreeOnTerminate:=true;
  indiws.OnNewfile:=@WsNewFile;
  indiws.Start;
except
end;
end;

procedure T_indicamera.DisconnectWs;
begin
try
  msg('Disconnect from WebSocket,3');
  indiws.Close(wsCloseNormal,'');
except
end;
end;

procedure T_indicamera.WsNewFile(Sender: TObject);
begin
if indiws<>nil then
  NewImageFile(indiws.fFramedText,0,indiws.fFramedStream.Size,indiws.fFramedStream);
end;

{ TIndiWebSocketClientConnection }

procedure TIndiWebSocketClientConnection.ProcessText(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: string);
begin
  fFramedText := aData;
end;

procedure TIndiWebSocketClientConnection.ProcessTextContinuation(aFinal, aRes1,
  aRes2, aRes3: boolean; aData: string);
begin
  fFramedText := fFramedText + aData;
end;

procedure TIndiWebSocketClientConnection.ProcessStream(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: TMemoryStream);
begin
  fFramedStream.Size := 0;
  fFramedStream.CopyFrom(aData, aData.Size);
  if (aFinal) then
  begin
    Synchronize(@SyncBinFrame);
  end;
end;

procedure TIndiWebSocketClientConnection.ProcessStreamContinuation(aFinal,
  aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin
  fFramedStream.CopyFrom(aData, aData.Size);
  if (aFinal) then
  begin
    Synchronize(@SyncBinFrame);
  end;
end;

procedure TIndiWebSocketClientConnection.SyncBinFrame;
begin
  if fFramedStream.Size>0 then begin
    fFramedStream.Position:=0;
    if Assigned(FOnNewfile) then FOnNewfile(self);
  end;
end;

constructor TIndiWebSocketClientConnection.Create(aHost, aPort, aResourceName: string;
    aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
    aCookie: string = '-'; aVersion: integer = 8);
begin
  inherited;
  fFramedText := '';
  fFramedStream := TMemoryStream.Create;
end;

destructor TIndiWebSocketClientConnection.Destroy;
begin
  fFramedStream.free;
  inherited;
end;

end.

