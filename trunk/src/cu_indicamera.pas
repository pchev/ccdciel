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

interface

uses cu_camera, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, ExtCtrls, Classes, SysUtils;

type

T_indicamera = class(T_camera)
private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   CCDDevice: Basedevice;
   CCDport: ITextVectorProperty;
   CCDexpose: INumberVectorProperty;
   CCDexposeValue: INumber;
   CCDbinning: INumberVectorProperty;
   CCDbinX,CCDbinY: INumber;
   CCDframe: INumberVectorProperty;
   CCDframeX,CCDframeY,CCDframeWidth,CCDframeHeight: INumber;
   CCDframeReset: ISwitchVectorProperty;
   CCDFrameType: ISwitchVectorProperty;
   FrameLight, FrameBias, FrameDark,FrameFlat: ISwitch;
   CCDCompression: ISwitchVectorProperty;
   CCDcompress, CCDraw: ISwitch;
   CCDAbortExposure: ISwitchVectorProperty;
   CCDAbort: ISwitch;
   CCDTemperature: INumberVectorProperty;
   CCDinfo: INumberVectorProperty;
   CCDmaxx,CCDmaxy,CCDpixelsize,CCDpixelsizex,CCDpixelsizey,CCDbitperpixel : INumber;
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
   FhasBlob,Fready,Fconnected,UseMainSensor: boolean;
   Findiserver, Findiserverport, Findidevice, Findisensor, Findideviceport: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(txt: string);
   procedure NewProperty(indiProp: IndiProperty);
   procedure NewNumber(nvp: INumberVectorProperty);
   procedure NewText(tvp: ITextVectorProperty);
   procedure NewSwitch(svp: ISwitchVectorProperty);
   procedure NewLight(lvp: ILightVectorProperty);
   procedure NewBlob(bp: IBLOB);
   procedure ServerConnected(Sender: TObject);
   procedure ServerDisconnected(Sender: TObject);
   procedure msg(txt: string);
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
   function GetMaxX: double; override;
   function GetMaxY: double; override;
   function GetPixelSize: double; override;
   function GetPixelSizeX: double; override;
   function GetPixelSizeY: double; override;
   function GetBitperPixel: double; override;
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''); override;
   Procedure Disconnect; override;
   Procedure StartExposure(exptime: double); override;
   Procedure SetBinning(sbinX,sbinY: integer); override;
   procedure SetFrame(x,y,width,height: integer); override;
   procedure GetFrame(out x,y,width,height: integer); override;
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); override;
   procedure ResetFrame; override;
   Procedure AbortExposure; override;
   Procedure SetActiveDevices(focuser,filters,telescope: string); override;
end;

implementation

procedure T_indicamera.CreateIndiClient;
begin
if csDestroying in ComponentState then exit;
  indiclient:=TIndiBaseClient.Create;
  indiclient.onNewDevice:=@NewDevice;
  indiclient.onNewMessage:=@NewMessage;
  indiclient.onNewProperty:=@NewProperty;
  indiclient.onNewNumber:=@NewNumber;
  indiclient.onNewText:=@NewText;
  indiclient.onNewSwitch:=@NewSwitch;
  indiclient.onNewLight:=@NewLight;
  indiclient.onNewBlob:=@NewBlob;
  indiclient.onServerConnected:=@ServerConnected;
  indiclient.onServerDisconnected:=@ServerDisconnected;
  ClearStatus;
end;

constructor T_indicamera.Create;
begin
 inherited Create;
 FCameraInterface:=INDI;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findisensor:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=10000;
 InitTimer.OnTimer:=@InitTimerTimer;
 CreateIndiClient;
end;

destructor  T_indicamera.Destroy;
begin
 InitTimer.Enabled:=false;
 indiclient.Free;
 FreeAndNil(InitTimer);
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
    CCDCompression:=nil;
    CCDAbortExposure:=nil;
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
    FhasBlob:=false;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    FWheelStatus:=devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
end;

procedure T_indicamera.CheckStatus;
begin
    if Fconnected and
       FhasBlob and (
       ((Findisensor='CCD2')and(Guiderexpose<>nil))or
       ((Findisensor<>'CCD2')and(CCDexpose<>nil)) )
    then begin
       FStatus := devConnected;
       UseMainSensor:=(Findisensor<>'CCD2');
      if (not Fready) and Assigned(FonStatusChange) then FonStatusChange(self);
      Fready:=true;
      if (WheelSlot<>nil) and (FilterName<>nil) then begin
        FWheelStatus:=devConnected;
        if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
      end;
    end;
end;

procedure T_indicamera.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_indicamera.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string='');
begin
if (indiclient=nil)or(indiclient.Terminated) then CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Findisensor:=cp4;
  Findideviceport:=cp5;
  FStatus := devDisconnected;
  FWheelStatus:=devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  FWheelStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg('Already connected');
end;

procedure T_indicamera.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (not Fready) then begin
     msg('No response from server');
     msg('Is "'+Findidevice+'" a running camera driver?');
     Disconnect;
  end;
end;

Procedure T_indicamera.Disconnect;
begin
indiclient.Terminate;
ClearStatus;
end;

procedure T_indicamera.ServerConnected(Sender: TObject);
begin
   if (CCDport<>nil)and(Findideviceport<>'') then begin
      CCDport.tp[0].text:=Findideviceport;
      indiclient.sendNewText(CCDport);
   end;
   indiclient.connectDevice(Findidevice);
   if (Findisensor='CCD1')or(Findisensor='CCD2') then
       indiclient.setBLOBMode(B_ALSO,Findidevice,Findisensor)
   else
       indiclient.setBLOBMode(B_ALSO,Findidevice);
end;

procedure T_indicamera.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  FWheelStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  if Assigned(FonWheelStatusChange) then FonWheelStatusChange(self);
  msg('Camera server disconnected');
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

procedure T_indicamera.NewMessage(txt: string);
begin
  msg(txt);
end;

procedure T_indicamera.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    i: integer;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype = INDI_BLOB) then begin
     FhasBlob:=true;
  end
  else if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     CCDport:=indiProp.getText;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_EXPOSURE') then begin
     CCDexpose:=indiProp.getNumber;
     CCDexposeValue:=IUFindNumber(CCDexpose,'CCD_EXPOSURE_VALUE');
     if CCDexposeValue=nil then CCDexpose:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_BINNING') then begin
     CCDbinning:=indiProp.getNumber;
     CCDbinX:=IUFindNumber(CCDbinning,'HOR_BIN');
     CCDbinY:=IUFindNumber(CCDbinning,'VER_BIN');
     if (CCDbinX=nil)or(CCDbinY=nil) then CCDbinning:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_FRAME') then begin
     CCDframe:=indiProp.getNumber;
     CCDframeX:=IUFindNumber(CCDframe,'X');
     CCDframeY:=IUFindNumber(CCDframe,'Y');
     CCDframeWidth:=IUFindNumber(CCDframe,'WIDTH');
     CCDframeHeight:=IUFindNumber(CCDframe,'HEIGHT');
     if (CCDframeX=nil)or(CCDframeY=nil)or(CCDframeWidth=nil)or(CCDframeHeight=nil) then CCDframe:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='CCD_FRAME_RESET') then begin
     CCDframeReset:=indiProp.getSwitch;
  end
  else if (proptype=INDI_SWITCH)and(propname='CCD_FRAME_TYPE') then begin
     CCDFrameType:=indiProp.getSwitch;
     FrameLight:=IUFindSwitch(CCDFrameType,'FRAME_LIGHT');
     FrameBias:=IUFindSwitch(CCDFrameType,'FRAME_BIAS');
     FrameDark:=IUFindSwitch(CCDFrameType,'FRAME_DARK');
     FrameFlat:=IUFindSwitch(CCDFrameType,'FRAME_FLAT');
     if (FrameLight=nil)or(FrameBias=nil)or(FrameDark=nil)or(FrameFlat=nil) then CCDFrameType:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='CCD_COMPRESSION') then begin
     CCDCompression:=indiProp.getSwitch;
     CCDcompress:=IUFindSwitch(CCDCompression,'CCD_COMPRESS');
     if CCDcompress=nil then CCDcompress:=IUFindSwitch(CCDCompression,'COMPRESS');;
     CCDraw:=IUFindSwitch(CCDCompression,'CCD_RAW');
     if CCDraw=nil then CCDraw:=IUFindSwitch(CCDCompression,'RAW');
     if (CCDcompress=nil)or(CCDraw=nil) then CCDCompression:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='CCD_ABORT_EXPOSURE') then begin
     CCDAbortExposure:=indiProp.getSwitch;
     CCDAbort:=IUFindSwitch(CCDAbortExposure,'ABORT');
     if (CCDAbort=nil) then CCDAbortExposure:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_TEMPERATURE') then begin
     CCDTemperature:=indiProp.getNumber;
  end
  else if (proptype=INDI_NUMBER)and(propname='CCD_INFO') then begin
     CCDinfo:=indiProp.getNumber;
     CCDmaxx:=IUFindNumber(CCDinfo,'CCD_MAX_X');
     CCDmaxy:=IUFindNumber(CCDinfo,'CCD_MAX_Y');
     CCDpixelsize:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE');
     CCDpixelsizex:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE_X');
     CCDpixelsizey:=IUFindNumber(CCDinfo,'CCD_PIXEL_SIZE_Y');
     CCDbitperpixel:=IUFindNumber(CCDinfo,'CCD_BITSPERPIXEL');
     if (CCDmaxx=nil)or(CCDmaxy=nil)or(CCDpixelsize=nil)or(CCDpixelsizex=nil)or(CCDpixelsizey=nil)or(CCDbitperpixel=nil) then CCDinfo:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='GUIDER_EXPOSURE') then begin
     Guiderexpose:=indiProp.getNumber;
     GuiderexposeValue:=IUFindNumber(Guiderexpose,'GUIDER_EXPOSURE_VALUE');
     if GuiderexposeValue=nil then Guiderexpose:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='GUIDER_BINNING') then begin
     Guiderbinning:=indiProp.getNumber;
     GuiderbinX:=IUFindNumber(Guiderbinning,'HOR_BIN');
     GuiderbinY:=IUFindNumber(Guiderbinning,'VER_BIN');
     if (GuiderbinX=nil)or(GuiderbinY=nil) then Guiderbinning:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='GUIDER_COMPRESSION') then begin
     GuiderCompression:=indiProp.getSwitch;
     Guidercompress:=IUFindSwitch(GuiderCompression,'GUIDER_COMPRESS');
     if Guidercompress=nil then Guidercompress:=IUFindSwitch(GuiderCompression,'COMPRESS');;
     Guiderraw:=IUFindSwitch(GuiderCompression,'GUIDER_RAW');
     if Guiderraw=nil then Guiderraw:=IUFindSwitch(GuiderCompression,'RAW');
     if (Guidercompress=nil)or(Guiderraw=nil) then GuiderCompression:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='GUIDER_ABORT_EXPOSURE') then begin
     GuiderAbortExposure:=indiProp.getSwitch;
     GuiderAbort:=IUFindSwitch(GuiderAbortExposure,'ABORT');
     if (GuiderAbort=nil) then GuiderAbortExposure:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='GUIDER_INFO') then begin
     Guiderinfo:=indiProp.getNumber;
     Guidermaxx:=IUFindNumber(CCDinfo,'GUIDER_MAX_X');
     Guidermaxy:=IUFindNumber(CCDinfo,'GUIDER_MAX_Y');
     Guiderpixelsize:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE');
     Guiderpixelsizex:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE_X');
     Guiderpixelsizey:=IUFindNumber(CCDinfo,'GUIDER_PIXEL_SIZE_Y');
     Guiderbitperpixel:=IUFindNumber(CCDinfo,'GUIDER_BITSPERPIXEL');
     if (Guidermaxx=nil)or(Guidermaxy=nil)or(Guiderpixelsize=nil)or(Guiderpixelsizex=nil)or(Guiderpixelsizey=nil)or(Guiderbitperpixel=nil) then Guiderinfo:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='FILTER_SLOT') then begin
     WheelSlot:=indiProp.getNumber;
     Slot:=IUFindNumber(WheelSlot,'FILTER_SLOT_VALUE');
     if Slot=nil then WheelSlot:=nil;
  end
  else if (proptype=INDI_TEXT)and(propname='FILTER_NAME') then begin
     FilterName:=indiProp.getText;
     FFilterNames.Clear;
     for i:=0 to FilterName.ntp-1 do begin
        FFilterNames.Add(FilterName.tp[i].text);
     end;
  end
  else if (proptype=INDI_SWITCH)and(propname='UPLOAD_MODE') then begin
     UploadMode:=indiProp.getSwitch;
     UploadClient:=IUFindSwitch(UploadMode,'UPLOAD_CLIENT');
     UploadLocal:=IUFindSwitch(UploadMode,'UPLOAD_LOCAL');
     UploadBoth:=IUFindSwitch(UploadMode,'UPLOAD_BOTH');
     if (UploadClient=nil)or(UploadLocal=nil)or(UploadBoth=nil) then UploadMode:=nil;
  end
  else if (proptype=INDI_TEXT)and(propname='ACTIVE_DEVICES') then begin
     ActiveDevices:=indiProp.getText;
  end;
  CheckStatus;
end;

procedure T_indicamera.NewNumber(nvp: INumberVectorProperty);
begin
  if (UseMainSensor and (nvp=CCDexpose))or((not UseMainSensor) and (nvp=Guiderexpose)) then begin
     if Assigned(FonExposureProgress) then FonExposureProgress(nvp.np[0].value);
  end
  else if nvp=CCDframe then begin
     if Assigned(FonFrameChange) then FonFrameChange(self);
  end
  else if nvp=WheelSlot then begin
     if Assigned(FonFilterChange) then FonFilterChange(Slot.value);
  end
  else if nvp=CCDTemperature then begin
     if Assigned(FonTemperatureChange) then FonTemperatureChange(nvp.np[0].value);
  end;
 end;

procedure T_indicamera.NewText(tvp: ITextVectorProperty);
var propname: string;
    i: integer;
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
propname:=tvp.name;
if (propname='FILTER_NAME') then begin
    FFilterNames.Clear;
    for i:=0 to tvp.ntp-1 do begin
       FFilterNames.Add(tvp.tp[i].text);
    end;
    if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
end;
end;

procedure T_indicamera.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indicamera.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indicamera.NewBlob(bp: IBLOB);
begin
 if bp.bloblen>0 then begin
   { TODO : uncompress }
   FImgStream.Position:=0;
   bp.blob.Position:=0;
   FImgStream.CopyFrom(bp.blob,bp.bloblen);
   if Assigned(FonNewImage) then FonNewImage(self);
 end;
end;

Procedure T_indicamera.StartExposure(exptime: double);
begin
if (UploadMode<>nil)and(UploadLocal.s=ISS_ON) then begin
   IUResetSwitch(UploadMode);
   UploadClient.s:=ISS_ON;
   indiclient.sendNewSwitch(UploadMode);
end;
if UseMainSensor then begin
  if (CCDCompression<>nil)and(CCDcompress.s=ISS_ON) then begin
    IUResetSwitch(CCDCompression);
    CCDraw.s:=ISS_ON;
    indiclient.sendNewSwitch(CCDCompression);
    indiclient.WaitBusy(CCDCompression);
  end;
  if CCDexpose<>nil then begin;
    CCDexposeValue.value:=exptime;
    indiclient.sendNewNumber(CCDexpose);
  end;
end else begin
  if (GuiderCompression<>nil)and(Guidercompress.s=ISS_ON) then begin
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

Procedure T_indicamera.AbortExposure;
begin
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
if UseMainSensor then begin
 if CCDbinning<>nil then begin
    CCDbinX.value:=sbinX;
    CCDbinY.value:=sbinY;
    indiclient.sendNewNumber(CCDbinning);
    indiclient.WaitBusy(CCDbinning);
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
begin
  if UseMainSensor and (CCDframe<>nil) then begin
     CCDframeX.value:=x;
     CCDframeY.value:=y;
     CCDframeWidth.value:=width;
     CCDframeHeight.value:=height;
     indiclient.sendNewNumber(CCDframe);
  end;
end;

procedure T_indicamera.GetFrame(out x,y,width,height: integer);
begin
  if UseMainSensor and (CCDframe<>nil) then begin
     x      := round(CCDframeX.value);
     y      := round(CCDframeY.value);
     width  := round(CCDframeWidth.value);
     height := round(CCDframeHeight.value);
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
     xr.max:=CCDframeX.max;
     xr.step:=CCDframeX.step;
     yr.min:=CCDframeY.min;
     yr.max:=CCDframeY.max;
     yr.step:=CCDframeY.step;
     widthr.min:=CCDframeWidth.min;
     widthr.max:=CCDframeWidth.max;
     widthr.step:=CCDframeWidth.step;
     heightr.min:=CCDframeHeight.min;
     heightr.max:=CCDframeHeight.max;
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
    if CCDframeReset<>nil then begin
       CCDframeReset.sp[0].s:=ISS_ON;
       indiclient.sendNewSwitch(CCDframeReset);
    end else if CCDframe<>nil then begin
       CCDframeX.value:=CCDframeX.min;
       CCDframeY.value:=CCDframeY.min;
       CCDframeWidth.value:=CCDframeWidth.max;
       CCDframeHeight.value:=CCDframeHeight.max;
       indiclient.sendNewNumber(CCDframe);
    end;
  end;
end;

function T_indicamera.GetTemperatureRange:TNumRange;
begin
//if UseMainSensor then begin
 if CCDTemperature<>nil then begin
    result.min:=CCDTemperature.np[0].min;
    result.max:=CCDTemperature.np[0].max;
    result.step:=CCDTemperature.np[0].step;
 end
 else result:=NullRange;
//end;
end;

function  T_indicamera.GetTemperature: double;
begin
//if UseMainSensor then begin
 if CCDTemperature<>nil then begin
    result:=CCDTemperature.np[0].value;
 end
 else result:=NullCoord;
//end;
end;

procedure T_indicamera.SetTemperature(value:double);
begin
//if UseMainSensor then begin
 if CCDTemperature<>nil then begin
    CCDTemperature.np[0].value:=value;
    indiclient.sendNewNumber(CCDTemperature);
 end;
//end;
end;

Procedure T_indicamera.SetActiveDevices(focuser,filters,telescope: string);
var tp:IText;
begin
  if ActiveDevices<>nil then begin
     if focuser<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_FOCUSER');
        if tp<>nil then begin
           tp.text:=focuser;
        end;
     end;
     if filters<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_FILTER');
        if tp<>nil then begin
           tp.text:=filters;
        end;
     end;
     if telescope<>'' then begin
        tp:=IUFindText(ActiveDevices,'ACTIVE_TELESCOPE');
        if tp<>nil then begin
           tp.text:=telescope;
        end;
     end;
     indiclient.sendNewText(ActiveDevices);
  end;
end;

procedure T_indicamera.SetFilter(num:integer);
begin
if WheelSlot<>nil then begin;
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


end.

