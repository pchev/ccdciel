unit cu_ascomcamera;

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
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses  cu_camera, u_global,
  {$ifdef mswindows}
    u_translation, u_utils, cu_fits, indiapi, math,
    Variants, comobj, LazSysUtils, ActiveX,
  {$endif}
   Forms, ExtCtrls, Classes, SysUtils, LCLType;

type
T_ascomcamera = class(T_camera)
 private
   {$ifdef mswindows}
   V: variant;
   nf: integer;
   newtimestart,timedout:double;
   FPixelSizeX,FPixelSizeY: double;
   {$endif}
   FInterfaceVersion: integer;
   FOffsetX, FOffsetY: integer;
   FCType: string;
   FMaxBinX,FMaxBinY,FBinX,FBinY:integer;
   FHasTemperature, FCanSetTemperature: boolean;
   stCCDtemp,stCoolerPower : double;
   stCooler : boolean;
   FFrametype:TFrameType;
   ExposureTimer: TTimer;
   StatusTimer: TTimer;
   function Connected: boolean;
   procedure ExposureTimerTimer(sender: TObject);
   procedure StatusTimerTimer(sender: TObject);
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
   function  GetCoolerPower: Double; override;
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
   function GetVideoRecordRunning: boolean;  override;
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
   function GetVideoPreviewLimit:integer; override;
   procedure SetVideoPreviewLimit(value:integer); override;
   procedure SetGain(value: integer); override;
   function GetGain: integer; override;
   procedure SetOffset(value: integer); override;
   function GetOffset: integer; override;
   procedure SetReadOutMode(value: integer); override;
   function GetReadOutMode: integer; override;
   procedure SetFnumber(value: string); override;
   function GetFnumber: string; override;
   function GetStreamingExposureRange:TNumRange; override;
   function GetStreamingExposure:double; override;
   procedure SetStreamingExposure(value:double); override;
   function GetVideoEncoder: integer; override;
   procedure SetVideoEncoder(value:integer); override;
   function GetFullWellCapacity: double; override;

public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   procedure Disconnect;  override;
   function GetV: variant;
   Procedure StartExposure(exptime: double); override;
   procedure RestartExposure; override;
   Procedure SetBinning(sbinX,sbinY: integer); override;
   procedure SetFrame(x,y,width,height: integer); override;
   procedure GetFrame(out x,y,width,height: integer; refresh:boolean=false); override;
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); override;
   procedure ResetFrame; override;
   procedure GetStreamFrame(out x,y,width,height: integer);  override;
   procedure CfaInfo(out OffsetX, OffsetY: integer; out CType: string);  override;
   function  CheckGain:boolean; override;
   function  CheckOffset:boolean; override;
   Procedure AbortExposure; override;
   procedure AbortExposureButNotSequence; override;
   Procedure SetActiveDevices(afocuser,afilters,atelescope: string); override;
   procedure StartVideoPreview; override;
   procedure StopVideoPreview; override;
   procedure StartVideoRecord(mode:TVideoRecordMode); override;
   procedure StopVideoRecord; override;
end;

const statusinterval=1000;

implementation

constructor T_ascomcamera.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 stCooler:=false;
 stCCDtemp:=NullCoord;
 stCoolerPower:=NullCoord;
 FInterfaceVersion:=1;
 FCameraXSize:=-1;
 FCameraYSize:=-1;
 FMaxBinX:=1;
 FMaxBinY:=1;
 FBinX:=1;
 FBinY:=1;
 FOffsetX:=0;
 FOffsetY:=0;
 FCType:='';
 FHasTemperature:=false;
 FCanSetTemperature:=false;
 FCameraInterface:=ASCOM;
 FVerticalFlip:=false;
 ExposureTimer:=TTimer.Create(nil);
 ExposureTimer.Enabled:=false;
 ExposureTimer.Interval:=1000;
 ExposureTimer.OnTimer:=@ExposureTimerTimer;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomcamera.Destroy;
begin
 ExposureTimer.Free;
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomcamera.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
{$ifdef mswindows}
var readmodes,roitem: Variant;
    i,n: integer;
    buf: string;
{$endif}
begin
{$ifdef mswindows}
 try
 FhasLastExposureStartTime:=FUseCameraStartTime;
 FhasLastExposureDuration:=FUseCameraStartTime;
 stCooler:=false;
 stCCDtemp:=NullCoord;
 FStatus := devConnecting;
 Fdevice:=cp1;
 if Assigned(FonStatusChange) then FonStatusChange(self);
 V:=Unassigned;
 V:=CreateOleObject(Fdevice);
 V.connected:=true;
 if V.connected then begin
    FStatus := devConnected;
    try
    FDriverInfo:=V.DriverInfo;
    msg(FDriverInfo,9);
    except
    end;
    try
    msg('Driver version: '+V.DriverVersion,9);
    except
      msg('Error: unknown driver version',9);
    end;
    try
    FInterfaceVersion:=V.InterfaceVersion;
    except
      FInterfaceVersion:=1;
    end;
    msg('Interface version: '+inttostr(FInterfaceVersion),9);
    try
    FCameraXSize:=V.CameraXSize;
    except
     on E: Exception do begin
       FCameraXSize:=-1;
       msg('Error: cannot get frame size X from camera: ' + E.Message,0);
     end;
    end;
    try
    FCameraYSize:=V.CameraYSize;
    except
     on E: Exception do begin
       FCameraYSize:=-1;
       msg('Error: cannot get frame size Y from camera: ' + E.Message,0);
     end;
    end;
    if debug_msg then msg('Camera size='+inttostr(FCameraXSize)+'/'+inttostr(FCameraYSize));
    try
     FMaxBinX:=V.MaxBinX;
    except
     FMaxBinX:=1;
    end;
    try
    FMaxBinY:=V.MaxBinY;
    except
     FMaxBinY:=1;
    end;
    try
     FBinX:=V.BinX;
    except
     FBinX:=1;
    end;
    try
     FBinY:=V.BinY;
    except
     FBinY:=1;
    end;
    FPixelSizeX:=0;
    FPixelSizeY:=0;
    try
      FPixelSizeX:=round(int(V.PixelSizeX*100))/100;
      FPixelSizeY:=round(int(V.PixelSizeY*100))/100;
    except
      on E: Exception do begin
        msg('Error: cannot get pixel size from camera: ' + E.Message,0);
      end;
    end;
    Fccdname:=Fdevice;
    try
      Fccdname:=V.Name;
      buf:=V.SensorName;
      if buf<>'' then Fccdname:=Fccdname+'-'+buf;
    except
    end;
    try
      FhasCoolerPower:=V.CanGetCoolerPower;
    except
      FhasCoolerPower:=false;
    end;
    try
      FCanSetTemperature:=V.CanSetCCDTemperature;
    except
      FCanSetTemperature:=false;
    end;
    try
      DummyDouble:=V.CCDTemperature;
      FHasTemperature:=true;
    except
      FHasTemperature:=false;
    end;
    FReadOutList.Clear;
    try
      FhasFastReadout:=V.CanFastReadout;
    except
      FhasFastReadout:=false;
    end;
    if FhasFastReadout then begin
      FhasReadOut:=true;
      FReadOutList.Add('High quality');
      FReadOutList.Add('Fast');
    end
    else begin
      try
        readmodes:=V.ReadoutModes;
        n:=readmodes.Count;
        for i:=0 to n-1 do begin
          roitem:=readmodes.Item(i);
          FReadOutList.Add(string(roitem));
        end;
        FhasReadOut:=true;
      except
        FhasReadOut:=false;
      end;
    end;
    FhasCfaInfo:=false;
    try
      FOffsetX:=0;
      FOffsetY:=0;
      FCType:='';
      try
        FOffsetX:=V.BayerOffsetX;
        FOffsetY:=V.BayerOffsetY;
        i:=V.SensorType;
        case i of
          0: FCType:='';       // Camera produces monochrome array with no Bayer encoding
          1: FCType:='';       // Camera produces color image directly, requiring not Bayer decoding
          2: FCType:='RGGB';   // Camera produces RGGB encoded Bayer array images
          3: FCType:='CMYG';   // Camera produces CMYG encoded Bayer array images
          4: FCType:='CMYG2';  // Camera produces CMYG2 encoded Bayer array images
          5: FCType:='LRGB';   // Camera produces Kodak TRUESENSE Bayer LRGB array images
          else FCType:='';
        end;
      except
      end;
      if FCType<>'' then FhasCfaInfo:=true;
    except
      FhasCfaInfo:=false;
    end;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    StatusTimer.Enabled:=true;
    msg(rsConnected3);
 end
 else
    Disconnect;
 except
   on E: Exception do msg(Format(rsConnectionEr, [E.Message]),0);
 end;
{$endif}
end;

procedure T_ascomcamera.Disconnect;
begin
{$ifdef mswindows}
  StatusTimer.Enabled:=false;
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  try
  if not VarIsEmpty(V) then begin
    V.connected:=false;
    V:=Unassigned;
    msg(rsDisconnected3,1);
  end;
  except
    on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
  end;
{$endif}
end;

function T_ascomcamera.GetV: variant;
begin
  result:=V;
end;

function T_ascomcamera.Connected: boolean;
begin
result:=false;
{$ifdef mswindows}
if not VarIsEmpty(V) then begin
  try
  result:=V.connected;
  except
   result:=false;
  end;
end;
{$endif}
end;

procedure T_ascomcamera.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var t: double;
    c: boolean;
{$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    try
    c:=GetCooler;
    if c<>stCooler then begin
       stCooler:=c;
       if Assigned(FonCoolerChange) then FonCoolerChange(stCooler);
    end;
    if FhasCoolerPower then begin
       t:=GetCoolerPower;
       if (t<>stCoolerPower) then begin
         stCoolerPower:=t;
         if Assigned(FonCoolerPowerChange) then FonCoolerPowerChange(stCoolerPower);
       end;
    end;
    if FHasTemperature then begin
       t:=GetTemperature;
       if (t<>stCCDtemp) then begin
         stCCDtemp:=t;
         if Assigned(FonTemperatureChange) then FonTemperatureChange(stCCDtemp);
       end;
    end;
    except
     on E: Exception do msg(Format(rsError, [E.Message]),0);
    end;
  end;
 {$endif}
end;

procedure T_ascomcamera.RestartExposure;
begin
  if (Fexptime>0)   then
        StartExposure(Fexptime)
  else
     if assigned(FonAbortExposure) then FonAbortExposure(self);
end;

Procedure T_ascomcamera.StartExposure(exptime: double);
{$ifdef mswindows}
var li: boolean;
{$endif}
begin
{$ifdef mswindows}
  case FFrametype of
    LIGHT: li:=true;
    BIAS : li:=false;
    DARK : li:=false;
    FLAT : li:=true;
  end;
  try
     Fexptime:=exptime;
     if debug_msg then msg('start exposure.');
     newtimestart:=NowUTC;
     V.StartExposure(exptime,li);
     newtimestart:=(newtimestart+NowUTC)/2;
     inc(FImgNum);
     Ftimeend:=now+(exptime)/secperday;
     timedout:=now+(exptime+CameraTimeout)/secperday;
     if exptime>=10 then ExposureTimer.Interval:=1000
     else ExposureTimer.Interval:=500;
     ExposureTimer.Enabled:=true;
     StatusTimer.Enabled:=true;
  except
     on E: Exception do begin
        msg(Format(rsStartExposur, [E.Message]),0);
        if assigned(FonAbortExposure) then FonAbortExposure(self);
     end;
  end;
{$endif}
end;

procedure T_ascomcamera.ExposureTimerTimer(sender: TObject);
{$ifdef mswindows}
  //{$define DirectArray}        // Use FPC SafeArray to dynamic array, this is limited to 2D images
  {$ifndef DirectArray}
    //{$define DirectOleaut32}    // Call SafeArray functions in oleaut32.dll directly
                                // else use FPC wrapper to oleaut32
{$endif}
type Timgdata = array of longint;
var ok: boolean;
    i,j,c,xs,ys,tmp: integer;
    nax1,nax2,state,bitpx: integer;
    pix,piy,expt,ElectronsPerADU,rexp: double;
    dateobs,frname:string;
    {$ifdef DirectArray}
      img: array of array of LongInt;     // 2D dynamic array for the image data
    {$else}
      imgvar: Variant;                    // ImageArray variant
      pimgdata: ^Timgdata;                // pointer to image data
      {$ifdef DirectOleaut32}
        imgsafearray: PSafeArray;         // SafeArray object for direct call
      {$endif}
    {$endif}
    Dims, LBoundX, HBoundX,LBoundY, HBoundY : Integer;
    p2:array[0..1] of integer;
    p3:array[0..2] of integer;

    lii,es: integer;
    ii: smallint;
    b: array[0..2880]of char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
    n, nb: byte;
    w,ww,pxdiv,newsaturation: word;
    {$endif}
begin
 ExposureTimer.Enabled:=false;
 {$ifdef mswindows}
 try
 if (now<timedout)and(not VarIsEmpty(V)) then begin
    state:=5;
    try
      state:=V.CameraState;
    except
      on E: Exception do begin
         msg('Error reading camera state '+E.Message,0);
      end;
    end;
    ok:=false;
    try
      if state=0 then ok:=V.ImageReady;
    except
      on E: Exception do begin
        msg('Error reading camera image availability: ' + E.Message,0);
        if assigned(FonAbortExposure) then FonAbortExposure(self);
        exit;
      end;
    end;
    if debug_msg then msg(' status:'+inttostr(state)+', image ready:'+BoolToStr(ok, rsTrue, rsFalse));
    if (not ok) then begin
      // in progress
      if assigned(FonExposureProgress) then
      case state of
        0 : FonExposureProgress(0);  // iddle
        1 : FonExposureProgress(-1); // wait start
        2 : begin                    // exposure in progress
              rexp:=secperday*(Ftimeend-now);
              if rexp>0 then
                FonExposureProgress(rexp)
              else
                FonExposureProgress(-4);
            end;
        3 : begin StatusTimer.Enabled:=false; FonExposureProgress(-3);  end; // read ccd
        4 : begin StatusTimer.Enabled:=false; FonExposureProgress(-4); exposuretimer.Interval:=250;  end; // downloading
        5 : FonExposureProgress(-5); // error
        else FonExposureProgress(-9);
      end;
      ExposureTimer.Enabled:=true;
      exit;
    end;
 end
 else begin
   ok:=false;
   msg(rsNoResponseFr2,0);
   if assigned(FonAbortExposure) then FonAbortExposure(self);
   StatusTimer.Enabled:=true;
 end;

 if ok then begin
   try
   FImageFormat:='.fits';
   Ftimestart:=newtimestart;
   FMidExposureTime:=(Ftimestart+NowUTC)/2;
   if assigned(FonExposureProgress) then FonExposureProgress(-10);
   if debug_msg then msg('read image.');

   try

   {$ifdef DirectArray}
      img:=V.ImageArray;
   {$else}
     imgvar:=V.ImageArray;
     {$ifdef DirectOleaut32}
        imgsafearray:=PSafeArray(VarArrayAsPSafeArray(imgvar));
     {$endif}
   {$endif}

   except
     on E: Exception do begin
       msg('Error accessing ImageArray: ' + E.Message,0);
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
   end;

   es:=0;
   {$ifdef DirectArray}
      es:=4;   // cannot get array type, this may crash later
   {$else}
     {$ifdef DirectOleaut32}
       es:=SafeArrayGetElemsize(imgsafearray);
     {$else}
       es:=SafeArrayGetElemsize(PSafeArray(VarArrayAsPSafeArray(imgvar)));
     {$endif}
   {$endif}
   if es<>4 then begin
     msg('Error ImageArray unsupported element size=' + inttostr(es));
     if assigned(FonAbortExposure) then FonAbortExposure(self);
     exit;
   end;

   Dims:=0;
   {$ifdef DirectArray}
      Dims:=2;   // cannot get array dimension in advance, this limit this method to 2D images
   {$else}
     {$ifdef DirectOleaut32}
       dims:=SafeArrayGetDim(imgsafearray);
     {$else}
       dims:=VarArrayDimCount(imgvar);
     {$endif}
   {$endif}

   if (Dims<2)or(Dims>3) then begin
     msg('Error ImageArray unsupported Dimension=' + inttostr(Dims));
     if assigned(FonAbortExposure) then FonAbortExposure(self);
     exit;
   end;

   try
     {$ifdef DirectArray}
       LBoundX:=0;
       LBoundY:=0;
       HBoundX:=length(img)-1;
       HBoundY:=length(img[0])-1;
     {$else}
       {$ifdef DirectOleaut32}
         SafeArrayGetLBound(imgsafearray,1,LBoundX);
         SafeArrayGetUBound(imgsafearray,1,HBoundX);
         SafeArrayGetLBound(imgsafearray,2,LBoundY);
         SafeArrayGetUBound(imgsafearray,2,HBoundY);
       {$else}
         LBoundX:=VarArrayLowBound(imgvar,1);
         HBoundX:=VarArrayHighBound(imgvar,1);
         LBoundY:=VarArrayLowBound(imgvar,2);
         HBoundY:=VarArrayHighBound(imgvar,2);
       {$endif}
     {$endif}
     xs:=HBoundX-LBoundX+1;
     ys:=HBoundY-LBoundY+1;
     if ((xs<ys) and (FDriverInfo='') and (ccdname='ASCOM.ASCOM_QHY5.Camera')) then 
       begin 
         tmp:=xs; xs:=ys; ys:=tmp;//qhy5 v1 fix for swapped width & height 
       end;
     if (xs<=0) or (ys<=0) then raise Exception.Create('Null size image');
   except
     on E: Exception do begin
       msg('Error reading ImageArray size: ' + E.Message,0);
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
   end;
   if debug_msg then msg('width:'+inttostr(xs)+' height:'+inttostr(ys));

   nax1:=xs;
   nax2:=ys;
   pix:=FPixelSizeX;
   piy:=FPixelSizeY;
   frname:=FrameName[ord(FFrametype)];
   dateobs:=FormatDateTime(dateiso,Ftimestart);
   if FhasLastExposureStartTime then begin
     try
       dateobs:=V.LastExposureStartTime;
     except
       FhasLastExposureStartTime:=false;
     end;
   end;
   expt:=Fexptime;
   if FhasLastExposureDuration then begin
     try
       expt:=V.LastExposureDuration;
     except
       FhasLastExposureDuration:=false;
     end;
   end;
   try
     ElectronsPerADU:=V.ElectronsPerADU;
   except
     ElectronsPerADU:=-1;
   end;
   bitpx:=round(GetBitperPixel);

   {$ifndef DirectArray}
     {$ifdef DirectOleaut32}
     i:=SafeArrayAccessData(imgsafearray,pimgdata);
     if i<>0 then begin
       msg('Error SafeArray AccessData: ' +  hexStr(i,10));
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
     {$else}
     try
     pimgdata:=VarArrayLock(imgvar);
     except
       on E: Exception do begin
         msg('Error SafeArray AccessData: ' + E.Message,0);
         if assigned(FonAbortExposure) then FonAbortExposure(self);
         exit;
       end;
     end;
     {$endif}
   {$endif}

   // count used bit by pixel
   pxdiv:=1;
   if FFixPixelRange and (bitpx=16) then begin
     nb:=16;
     w:=0;
     for i:=LBoundY to ys-1 do begin
       for j := LBoundX to xs-1 do begin
         {$ifdef DirectArray}
           ww:=img[j,i];
         {$else}
           ww:=Timgdata(pimgdata)[j+i*xs];
         {$endif}
         if ww<65535 then
           w:=w or ww;
       end;
     end;
     for n:=16 downto 1 do begin
       if w and 1 <>0 then begin
         nb:=n;
         break;
       end;
       w:=w div 2;
     end;
     pxdiv:=2**(16-nb); // divisor need to recover original pixel range
     newsaturation:=2**nb-1; // new saturation value to replace 65535
   end;

   pxdiv:=1;
   if debug_msg then msg('set fits header');
   hdr:=TFitsHeader.Create;
   hdr.ClearHeader;
   hdr.Add('SIMPLE',true,'file does conform to FITS standard');
   hdr.Add('BITPIX',bitpx,'number of bits per data pixel');
   hdr.Add('NAXIS',2,'number of data axes');
   hdr.Add('NAXIS1',nax1 ,'length of data axis 1');
   hdr.Add('NAXIS2',nax2 ,'length of data axis 2');
   if bitpx=16 then begin
     hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
     hdr.Add('BSCALE',1,'default scaling factor');
   end;
   if FASCOMFlipImage then
     hdr.Add('ROWORDER',bottomup,'Order of the rows in image array')
   else
     hdr.Add('ROWORDER',topdown,'Order of the rows in image array');
   hdr.Add('EXPTIME',expt,'Total Exposure Time (s)');
   hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
   hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
   hdr.Add('XBINNING',BinX ,'Binning factor in width');
   hdr.Add('YBINNING',BinY ,'Binning factor in height');
   hdr.Add('FRAME',frname,'Frame Type');
   hdr.Add('INSTRUME',ccdname,'CCD Name');
   if ElectronsPerADU>0 then hdr.Add('EGAIN',ElectronsPerADU,' Electronic gain in e-/ADU');
   if FFixPixelRange then begin
     hdr.Add('COMMENT','Detected '+inttostr(nb)+' bit per pixel camera image','');
     if pxdiv=1 then
       hdr.Add('COMMENT','Pixel values are using the original range','')
     else
       hdr.Add('COMMENT','Pixel values are divided by '+inttostr(pxdiv)+' to recover original range','');
     hdr.Add('MAXADU',newsaturation,'Maximum pixel value');
   end;
   hdr.Add('DATE-OBS',dateobs,'UTC start date of observation');
   hdr.Add('END','','');
   hdrmem:=hdr.GetStream;
   FImgStream.Clear;
   FImgStream.position:=0;
   hdrmem.Position:=0;
   if debug_msg then msg('write header');
   FImgStream.CopyFrom(hdrmem,hdrmem.Size);
   hdrmem.Free;
   hdr.Free;
   if debug_msg then msg('write image '+inttostr(bitpx)+'bit');
   if bitpx=16 then begin
     // Save 16bit FITS
     if Dims=2 then begin
       for i:=LBoundY to ys-1 do begin
          if FASCOMFlipImage then
             p2[1]:=ys-1-i
          else
             p2[1]:=i;
          for j:=LBoundX to xs-1 do begin
            p2[0]:=j;
            {$ifdef DirectArray}
              lii:=img[p2[0],p2[1]];
            {$else}
              lii:=Timgdata(pimgdata)[p2[0]+p2[1]*xs];
            {$endif}
            if FFixPixelRange then lii:=lii div pxdiv;
            if lii>0 then
               ii:=lii-32768
            else
               ii:=-32768;
            ii:=NtoBE(ii);
            FImgStream.Write(ii,sizeof(smallint));
          end;
       end;
     {$ifndef DirectArray}
     end
     else if Dims=3 then begin
       p3[2]:=0; // only the first plane { #todo : implement full color if someday a camera use this format }
       for i:=LBoundY to ys-1 do begin
          if FASCOMFlipImage then
             p3[1]:=ys-1-i
          else
             p3[1]:=i;
          for j:=LBoundX to xs-1 do begin
            p3[0]:=j;
            lii:=Timgdata(pimgdata)[p3[0]+p3[1]*xs];
            if FFixPixelRange then lii:=lii div pxdiv;
            if lii>0 then
               ii:=lii-32768
            else
               ii:=-32768;
            ii:=NtoBE(ii);
            FImgStream.Write(ii,sizeof(smallint));
          end;
       end;
       {$endif}
     end;
   end
   else begin
     // Save 32bit FITS
     if Dims=2 then begin
       for i:=LBoundY to ys-1 do begin
          if FASCOMFlipImage then
             p2[1]:=ys-1-i
          else
             p2[1]:=i;
          for j:=LBoundX to xs-1 do begin
            p2[0]:=j;
            {$ifdef DirectArray}
              lii:=img[p2[0],p2[1]];
            {$else}
              lii:=Timgdata(pimgdata)[p2[0]+p2[1]*xs];
            {$endif}
            lii:=NtoBE(lii);
            FImgStream.Write(lii,sizeof(longint));
          end;
       end;
     {$ifndef DirectArray}
     end
     else if Dims=3 then begin
       p3[2]:=0; // only the first plane { #todo : implement full color if someday a camera use this format }
       for i:=LBoundY to ys-1 do begin
          if FASCOMFlipImage then
             p3[1]:=ys-1-i
          else
             p3[1]:=i;
          for j:=LBoundX to xs-1 do begin
            p3[0]:=j;
            lii:=Timgdata(pimgdata)[p3[0]+p3[1]*xs];
            lii:=NtoBE(lii);
            FImgStream.Write(lii,sizeof(longint));
          end;
       end;
       {$endif}
     end;
   end;

   if debug_msg then msg('pad fits');
   b:='';
   c:=FImgStream.Size mod 2880;
   if c>0 then begin
     c:=2880-c;
     FillChar(b,c,0);
     FImgStream.Write(b,c);
   end;

   {$ifndef DirectArray}
     if debug_msg then msg('release imagearray');
     {$ifdef DirectOleaut32}
       SafeArrayUnaccessData(imgsafearray);
     {$else}
       VarArrayUnlock(imgvar);
     {$endif}
   {$endif}

   // if possible start next exposure now
   TryNextExposure(FImgNum);
   if debug_msg then msg('display image');
   if assigned(FonExposureProgress) then FonExposureProgress(-11);
   NewImage;
   finally
   StatusTimer.Enabled:=true;
   end;
 end;
 except
    on E: Exception do msg('Error reading image: ' + E.Message,0);
 end;
 {$endif}
end;

Procedure T_ascomcamera.SetBinning(sbinX,sbinY: integer);
{$ifdef mswindows}
var oldx,oldy,newx,newy,fsx,fsy,fnx,fny: integer;
    scale:double;
{$endif}
begin
 {$ifdef mswindows}
   try
   if debug_msg then msg('Request binning '+inttostr(sbinX)+','+inttostr(sbinY));
   oldx:=FBinX;
   oldy:=FBinY;
   if debug_msg then msg('Old binning '+inttostr(oldx)+','+inttostr(oldy));
   if (oldx<>sbinX)or(oldy<>sbinY) then begin
     msg(Format(rsSetBinningX, [inttostr(sbinX), inttostr(sbinY)]));
     GetFrame(fsx,fsy,fnx,fny);
     if debug_msg then msg('Current frame '+inttostr(fsx)+','+inttostr(fsy)+'/'+inttostr(fnx)+'x'+inttostr(fny));
     scale:=oldx/sbinX;
     fsx:=trunc(fsx*scale);
     fnx:=trunc(fnx*scale);
     scale:=oldy/sbinY;
     fsy:=trunc(fsy*scale);
     fny:=trunc(fny*scale);
     newx:=FCameraXSize div sbinX;
     newy:=FCameraYSize div sbinY;
     V.BinX:=sbinX;
     V.BinY:=sbinY;
     FBinX:=sbinX;
     FBinY:=sbinY;
     if (fsx=0)and(fsy=0)and((abs(newx-fnx)/fnx)<0.1)and((abs(newy-fny)/fny)<0.1)
        then SetFrame(0,0,newx,newy)
        else SetFrame(fsx,fsy,fnx,fny);
     Wait(1);
   end;
   except
    on E: Exception do msg('Camera '+Fdevice+' Set binning error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascomcamera.SetFrame(x,y,width,height: integer);
{$ifdef mswindows}
var Xmax,Ymax,w,h,bx,by: integer;
{$endif}
begin
 {$ifdef mswindows}
   try
   if debug_msg then msg('Request frame '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height));
   w:=FCameraXSize;
   h:=FCameraYSize;
   bx:=FBinX;
   by:=FBinY;
   if debug_msg then msg('XSize='+inttostr(w)+' YSize='+inttostr(h)+' BinX='+inttostr(bx)+' BinY='+inttostr(by));
   Xmax:= w div bx;
   Ymax:= h div by;
   if debug_msg then msg('Xmax='+inttostr(Xmax)+' Ymax='+inttostr(Ymax));
   // check range
   if width<MinFrameSize then width:=MinFrameSize;
   if height<MinFrameSize then height:=MinFrameSize;
   if x<0 then x:=0;
   if y<0 then y:=0;
   if x>(Xmax-MinFrameSize) then x:=Xmax-MinFrameSize;
   if y>(Ymax-MinFrameSize) then y:=Ymax-MinFrameSize;
   if (x+width)>Xmax then width:=Xmax-x;
   if (y+height)>Ymax then height:=Ymax-y;
   // force even values
   x:=round(x+0.5);
   y:=round(y+0.5);
   if debug_msg then msg('Set frame '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height));
   V.StartX:=x;
   V.StartY:=y;
   V.NumX:=width;
   V.NumY:=height;
   Wait(1);
   if Assigned(FonFrameChange) then FonFrameChange(self);
   except
    on E: Exception do msg('Set frame error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascomcamera.GetFrame(out x,y,width,height: integer; refresh:boolean=false);
begin
 {$ifdef mswindows}
   try
   // do not cache the value, so ignore the refresh parameter
   x      := V.StartX;
   y      := V.StartY;
   width  := V.NumX;
   height := V.NumY;
   if debug_msg then msg('Current frame: '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height),3);
   except
    on E: Exception do msg('Get frame error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascomcamera.GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
begin
 xr:=NullRange;yr:=NullRange;widthr:=NullRange;heightr:=NullRange;
 {$ifdef mswindows}
   try
   xr.min:=0;
   xr.max:=FCameraXSize-1;
   xr.step:=1;
   yr.min:=0;
   yr.max:=FCameraYSize-1;
   yr.step:=1;
   widthr.min:=1;
   widthr.max:=FCameraXSize;
   widthr.step:=1;
   heightr.min:=1;
   heightr.max:=FCameraYSize;
   heightr.step:=1;
   if debug_msg then msg('Get frame range :'+inttostr(round(widthr.max))+'x'+inttostr(round(heightr.max)));
   except
    on E: Exception do msg('Get frame range error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascomcamera.ResetFrame;
{$ifdef mswindows}
var w,h,bx,by: integer;
{$endif}
begin
{$ifdef mswindows}
  try
  w:=FCameraXSize;
  h:=FCameraYSize;
  bx:=FBinX;
  by:=FBinY;
  if debug_msg then   msg('ResetFrame: XSize='+inttostr(w)+' YSize='+inttostr(h)+' BinX='+inttostr(bx)+' BinY='+inttostr(by));
  w:=w div bx;
  h:=h div by;
  SetFrame(0,0,w,h);
  Wait(1);
  except
   on E: Exception do msg('Reset frame error: ' + E.Message,0);
  end;
{$endif}
end;

procedure T_ascomcamera.CfaInfo(out OffsetX, OffsetY: integer; out CType: string);
begin
 OffsetX:=FOffsetX;
 OffsetY:=FOffsetY;
 CType:=FCType;
end;

Procedure T_ascomcamera.AbortExposureButNotSequence;
begin
 {$ifdef mswindows}
   try
    ExposureTimer.Enabled:=false;
    StatusTimer.Enabled:=true;
    WaitExposure:=false;
    if V.CanAbortExposure then
      V.AbortExposure
    else if V.CanStopExposure then
      V.StopExposure;
   except
    on E: Exception do msg('Abort exposure error: ' + E.Message,0);
   end;
 {$endif}
end;

Procedure T_ascomcamera.AbortExposure;
begin
 {$ifdef mswindows}
   try
    ExposureTimer.Enabled:=false;
    StatusTimer.Enabled:=true;
    WaitExposure:=false;
    if V.CanAbortExposure then
      V.AbortExposure
    else if V.CanStopExposure then
      V.StopExposure;
    if assigned(FonAbortExposure) then FonAbortExposure(self);
   except
    on E: Exception do msg('Abort exposure error: ' + E.Message,0);
   end;
 {$endif}
end;

Procedure T_ascomcamera.SetActiveDevices(afocuser,afilters,atelescope: string);
begin
  // not in ascom
end;

function T_ascomcamera.GetBinX:integer;
begin
 result:=1;
 {$ifdef mswindows}
   result:=FBinX;
 {$endif}
end;

function T_ascomcamera.GetBinY:integer;
begin
 result:=1;
 {$ifdef mswindows}
   result:=FBinY;
 {$endif}
end;

procedure T_ascomcamera.SetFrametype(f:TFrameType);
begin
 {$ifdef mswindows}
  msg(Format(rsSetFrameType, [FrameName[ord(f)]]));
  FFrametype:=f;
 {$endif}
end;

function  T_ascomcamera.GetFrametype:TFrameType;
begin
  result:=FFrametype;
end;

function T_ascomcamera.GetBinXrange:TNumRange;
begin
 result:=UnitRange;
 {$ifdef mswindows}
   result.max:=FMaxBinX;
 {$endif}
end;

function T_ascomcamera.GetBinYrange:TNumRange;
begin
 result:=UnitRange;
 {$ifdef mswindows}
   result.max:=FMaxBinY;
 {$endif}
end;

function T_ascomcamera.GetExposureRange:TNumRange;
begin
  result:=NullRange;
  {$ifdef mswindows}
    try
    result.max:=V.ExposureMax;
    except
     result:=NullRange;
    end;
    try
    result.min:=V.ExposureMin;
    except
     result:=NullRange;
    end;
    try
    result.step:=V.ExposureResolution;
    except
     result:=NullRange;
    end;
  {$endif}
end;

function T_ascomcamera.GetTemperatureRange:TNumRange;
begin
  // not in ascom
  result.min:=-50;
  result.max:=50;
  result.step:=0;
end;

procedure T_ascomcamera.SetFilter(num:integer);
begin
 {$ifdef mswindows}
   try
   msg(Format(rsSetFilterPos, [inttostr(num)]));
   V.Position:=num-1;
   Wait(1);
   except
    on E: Exception do msg('Set filter error: ' + E.Message,0);
   end;
 {$endif}
end;

function  T_ascomcamera.GetFilter:integer;
begin
 result:=0;
 {$ifdef mswindows}
   try
   result:=V.Position+1;
   except
    on E: Exception do msg('Get filter error: ' + E.Message,0);
   end;
 {$endif}
end;

procedure T_ascomcamera.SetFilterNames(value:TStringList);
{$ifdef mswindows}
var i:integer;
{$endif}
begin
 {$ifdef mswindows}
 try
  if (value.Count=nf) then begin
    for i:=0 to value.Count-1 do begin
       FFilterNames[i]:=value[i];
    end;
  end;
  except
  end;
 {$endif}
end;

function  T_ascomcamera.GetTemperature: double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
   try
   if FHasTemperature then
      result:=V.CCDTemperature
   else
      result:=NullCoord;
   except
     result:=NullCoord;
   end;
 {$endif}
end;

procedure T_ascomcamera.SetTemperature(value:double);
begin
 {$ifdef mswindows}
   try
   if FCanSetTemperature then begin
      SetCooler(true);
      V.SetCCDTemperature:=value;
   end;
   except
    on E: Exception do msg('Set temperature error: ' + E.Message,0);
   end;
 {$endif}
end;

function  T_ascomcamera.GetCoolerPower: Double;
begin
 result:=NullCoord;
 {$ifdef mswindows}
   if not FhasCoolerPower then exit;
   try
     result:=V.CoolerPower;
   except
     result:=NullCoord;
   end;
 {$endif}
end;

function  T_ascomcamera.GetCooler: boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
     result:=V.CoolerOn;
   except
     result:=false;
   end;
 {$endif}
end;

procedure T_ascomcamera.SetCooler(value:boolean);
begin
{$ifdef mswindows}
  try
  if (V.CoolerOn<>value) then begin
     msg(Format(rsSetCooler, [': '+BoolToStr(value, rsTrue, rsFalse)]));
     V.CoolerOn:=value;
     Wait(1);
  end;
  except
   on E: Exception do msg('Set cooler error: ' + E.Message,0);
  end;
{$endif}
end;

function T_ascomcamera.GetMaxX: double;
begin
 result:=-1;
{$ifdef mswindows}
 result:=FCameraXSize;
{$endif}
end;

function T_ascomcamera.GetMaxY: double;
begin
 result:=-1;
{$ifdef mswindows}
 result:=FCameraYSize;
{$endif}
end;

function T_ascomcamera.GetMaxADU: double;
begin
 result:=MAXWORD;
{$ifdef mswindows}
  try
     result:=V.MaxADU;
  except
     result:=MAXWORD;
  end;
{$endif}
end;

function T_ascomcamera.GetPixelSize: double;
begin
 result:=-1;
{$ifdef mswindows}
  try
     result:=FPixelSizeX;
  except
     result:=-1;
  end;
{$endif}
end;

function T_ascomcamera.GetPixelSizeX: double;
begin
 result:=-1;
{$ifdef mswindows}
  try
     result:=FPixelSizeX;
  except
     result:=-1;
  end;
{$endif}
end;

function T_ascomcamera.GetPixelSizeY: double;
begin
 result:=-1;
{$ifdef mswindows}
  try
     result:=FPixelSizeY;
  except
     result:=-1;
  end;
{$endif}
end;

function T_ascomcamera.GetBitperPixel: double;
begin
 result:=-1;
{$ifdef mswindows}
 if GetMaxADU<=MAXWORD then
   result:=16
 else
   result:=32;
{$endif}
end;

function T_ascomcamera.GetColor: boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
      result:=(V.SensorType=1);  // Camera produces color image directly, requiring not Bayer decoding
   except
      result:=false;
   end;
 {$endif}
end;

procedure T_ascomcamera.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomcamera.CheckGain:boolean;
{$ifdef mswindows}
var i,n: integer;
    isol: string;
{$endif}
begin
  result:=false;
  {$ifdef mswindows}
    if FStatus<>devConnected then exit;
    try
    if debug_msg then msg('Check camera gain');
    // check Gain property
       i:=V.Gain;
       if debug_msg then msg('Gain='+inttostr(i));
       try
       // check Gain range
          FGainMin:=V.GainMin;
          FGainMax:=V.GainMax;
          FhasGain:=true;
          if debug_msg then msg('GainMin='+IntToStr(FGainMin)+' GainMax='+inttostr(FGainMax));
       except
       // No Gain range
          on E: Exception do begin
              FhasGain:=false;
              if debug_msg then msg('Camera GainMin or GainMax exception: '+E.Message);
          end;
       end;
       try
       // Check ISO list
          if debug_msg then msg('Check camera gains list');
          n:=V.Gains.Count;
          FISOList.Clear;
          for i:=0 to n-1 do begin
            isol:=V.Gains.item[i];
            FISOList.Add(isol);
          end;
          FhasGainISO:=FISOList.Count>0;
          if debug_msg then msg('Found '+IntToStr(FISOList.Count)+' gains');
       except
       // No ISO list
          on E: Exception do begin
             FhasGainISO:=false;
             FISOList.Clear;
             if debug_msg then msg('Camera Gains list exception: '+E.Message);
          end;
       end;
    except
    // No Gain property at all
       on E: Exception do begin
          FhasGain:=false;
          FhasGainISO:=false;
          if debug_msg then msg('Camera Gain exception: '+E.Message);
       end;
    end;
    result:=(FhasGainISO or FhasGain);
  {$endif}
end;

function  T_ascomcamera.CheckOffset:boolean;
{$ifdef mswindows}
var i:integer;
{$endif}
begin
  FhasOffset:=false;
  {$ifdef mswindows}
  if FInterfaceVersion>=3 then begin
    try
      i:=V.Offset;
      FOffsetMin:=V.OffsetMin;
      FOffsetMax:=V.OffsetMax;
      FhasOffset:=true;
    except
      FhasOffset:=false;
    end;
  end;
  {$endif}
  result:=FhasOffset;
end;

procedure T_ascomcamera.SetGain(value: integer);
begin
 {$ifdef mswindows}
 if FCanSetGain and (FhasGainISO or FhasGain) then begin
   try
      V.Gain:=value;
   except
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetGain: integer;
begin
 result:=NullInt;
 {$ifdef mswindows}
 if (FhasGainISO or FhasGain) then begin
   try
      result:=V.Gain;
   except
      result:=NullInt;
   end;
 end;
 {$endif}
end;

procedure  T_ascomcamera.SetOffset(value: integer);
begin
 {$ifdef mswindows}
 if FCanSetGain and FhasOffset then begin
   try
      V.Offset:=value;
   except
   end;
 end;
 {$endif}
end;

function  T_ascomcamera.GetOffset: integer;
begin
 result:=NullInt;
 {$ifdef mswindows}
 if FhasOffset then begin
   try
      result:=V.Offset;
   except
      result:=NullInt;
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.SetReadOutMode(value: integer);
begin
 {$ifdef mswindows}
 try
 if FhasReadOut then begin
   if FhasFastReadout then begin
     if value=0 then V.FastReadout:=false
                else V.FastReadout:=true
   end
   else begin
     V.ReadoutMode:=value;
   end;
 end;
 except
    on E: Exception do msg('Set ReadOut='+inttostr(value)+': '+E.Message,0);
 end;
 {$endif}
end;

function T_ascomcamera.GetReadOutMode: integer;
begin
  result:=0;
  {$ifdef mswindows}
  try
  if (FhasReadOut) then begin
     if FhasFastReadout then begin
       if V.FastReadout then result:=1
                        else result:=0;
     end
     else begin
       result:=V.ReadoutMode;
     end;
  end;
  except
     on E: Exception do msg('Get ReadOut: '+E.Message,0);
  end;
  {$endif}
end;

function T_ascomcamera.GetFullWellCapacity: double;
begin
  result:=0;
  {$ifdef mswindows}
  if FStatus<>devConnected then exit;
  try
    result := V.FullWellCapacity;
  except
    on E: Exception do msg('Get FullWellCapacity: '+E.Message,0);
  end;
  {$endif}
end;

procedure T_ascomcamera.SetFnumber(value: string);
begin
  //unsupported
end;

function T_ascomcamera.GetFnumber: string;
begin
  result:='';
  //unsupported
end;

procedure T_ascomcamera.StartVideoPreview;
begin
// todo
end;

procedure T_ascomcamera.StopVideoPreview;
begin
// todo
end;

function T_ascomcamera.GetVideoPreviewRunning: boolean;
begin
 result:=false;
 // todo
end;

function T_ascomcamera.GetVideoRecordRunning: boolean;
begin
 result:=false;
 // todo
end;

function T_ascomcamera.GetMissedFrameCount: cardinal;
begin
 result:=0;
 // todo
end;

function T_ascomcamera.GetVideoRecordDuration:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetVideoRecordDuration(value:integer);
begin
 // todo
end;

function T_ascomcamera.GetVideoRecordFrames:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetVideoRecordFrames(value:integer);
begin
 // todo
end;

procedure T_ascomcamera.StartVideoRecord(mode:TVideoRecordMode);
begin
 // todo
end;

procedure T_ascomcamera.StopVideoRecord;
begin
 // todo
end;

function T_ascomcamera.GetVideoSize:string;
begin
 result:='';
 // todo
end;

procedure T_ascomcamera.SetVideoSize(value:string);
begin
 // todo
end;

function T_ascomcamera.GetVideoRate:string;
begin
 result:='';
 // todo
end;

procedure T_ascomcamera.SetVideoRate(value:string);
begin
 // todo
end;

function T_ascomcamera.GetFPS:double;
begin
 result:=0;
 //todo
end;

function T_ascomcamera.GetVideoRecordDir:string;
begin
 result:='';
 // todo
end;

procedure T_ascomcamera.SetVideoRecordDir(value:string);
begin
 // todo
end;

function T_ascomcamera.GetVideoRecordFile:string;
begin
 result:='';
 // todo
end;

procedure T_ascomcamera.SetVideoRecordFile(value:string);
begin
 // todo
end;

function T_ascomcamera.GetVideoExposure:integer;
begin
 result:=0;
 // todo
end;

function T_ascomcamera.GetVideoGain:integer;
begin
 result:=NullInt;
 // todo
end;

function T_ascomcamera.GetVideoGamma:integer;
begin
 result:=NullInt;
 // todo
end;

function T_ascomcamera.GetVideoBrightness:integer;
begin
 result:=NullInt;
 // todo
end;

procedure T_ascomcamera.SetVideoExposure(value:integer);
begin
 // todo
end;

procedure T_ascomcamera.SetVideoGain(value:integer);
begin
 // todo
end;

procedure T_ascomcamera.SetVideoGamma(value:integer);
begin
 // todo
end;

procedure T_ascomcamera.SetVideoBrightness(value:integer);
begin
 // todo
end;

function T_ascomcamera.GetVideoExposureRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomcamera.GetVideoGainRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomcamera.GetVideoGammaRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomcamera.GetVideoBrightnessRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomcamera.GetVideoPreviewDivisor:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetVideoPreviewDivisor(value:integer);
begin
 // todo
end;

function T_ascomcamera.GetVideoPreviewLimit:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetVideoPreviewLimit(value:integer);
begin
 // todo
end;

function T_ascomcamera.GetStreamingExposureRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomcamera.GetStreamingExposure:double;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetStreamingExposure(value:double);
begin
 // todo
end;

procedure T_ascomcamera.GetStreamFrame(out x,y,width,height: integer);
begin
 // todo
 x:=0; y:=0; width:=0; height:=0;
end;

function T_ascomcamera.GetVideoEncoder: integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomcamera.SetVideoEncoder(value:integer);
begin
 // todo
end;

function T_ascomcamera.GetImageFormat: string;
begin
 result:=FImageFormat;
end;


initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

