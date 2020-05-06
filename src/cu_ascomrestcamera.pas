unit cu_ascomrestcamera;

{$mode objfpc}{$H+}

{
Copyright (C) 2019 Patrick Chevalley

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

uses  cu_camera, cu_ascomrest, u_global,
    u_translation, u_utils, cu_fits, indiapi, math,
    LCLVersion,
   Forms, ExtCtrls, Classes, SysUtils, LCLType;

type
T_ascomrestcamera = class(T_camera)
 private
   V : TAscomRest;
   nf: integer;
   timedout:double;
   FPixelSizeX,FPixelSizeY: double;
   Fccdname: string;
   FMaxBinX,FMaxBinY,FBinX,FBinY:integer;
   FHasTemperature, FCanSetTemperature: boolean;
   stCCDtemp,stCoolerPower : double;
   stCooler : boolean;
   stX,stY,stWidth,stHeight: integer;
   stGain: integer;
   FFrametype:TFrameType;
   FOffsetX, FOffsetY: integer;
   FCType: string;
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
   function  GetTemperatureReal: double;
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
   function GetGainReal: integer;
   procedure SetReadOutMode(value: integer); override;
   function GetReadOutMode: integer; override;
   procedure SetFnumber(value: string); override;
   function GetFnumber: string; override;
   function GetStreamingExposureRange:TNumRange; override;
   function GetStreamingExposure:double; override;
   procedure SetStreamingExposure(value:double); override;
   function GetVideoEncoder: integer; override;
   procedure SetVideoEncoder(value:integer); override;

public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   procedure Disconnect;  override;
   Procedure StartExposure(exptime: double); override;
   procedure RestartExposure; override;
   Procedure SetBinning(sbinX,sbinY: integer); override;
   procedure SetFrame(x,y,width,height: integer); override;
   procedure GetFrame(out x,y,width,height: integer; refresh:boolean=false); override;
   procedure GetFrameReal(out x,y,width,height: integer);
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); override;
   procedure ResetFrame; override;
   procedure GetStreamFrame(out x,y,width,height: integer);  override;
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

const statusinterval=5000;

implementation

uses
{$if lcl_major > 1}
LazSysUtils;
{$else}
LazUTF8SysUtils;
{$endif}

constructor T_ascomrestcamera.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3200;
 stCooler:=false;
 stCCDtemp:=NullCoord;
 FCameraXSize:=-1;
 FCameraYSize:=-1;
 FMaxBinX:=1;
 FMaxBinY:=1;
 FBinX:=1;
 FBinY:=1;
 stX:=-1;
 stY:=-1;
 stWidth:=-1;
 stHeight:=-1;
 stGain:=-1;
 FOffsetX:=0;
 FOffsetY:=0;
 FCType:='';
 FHasTemperature:=false;
 FCanSetTemperature:=false;
 FCameraInterface:=ASCOMREST;
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

destructor  T_ascomrestcamera.Destroy;
begin
 ExposureTimer.Free;
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestcamera.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
var rlist: array of string;
    i,n,x,y: integer;
    buf: string;
begin
 try
  FhasLastExposureStartTime:=FUseCameraStartTime;
  FhasLastExposureDuration:=FUseCameraStartTime;
  stCooler:=false;
  stCCDtemp:=NullCoord;
  FStatus := devConnecting;
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  V.Device:=Fdevice;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V.Timeout:=5000;
  V.Put('Connected',true); // try to connect if authorized by server
  if V.Get('connected').AsBool then begin
    V.Timeout:=120000;
    try
    msg('Driver version: '+V.Get('driverversion').AsString,9);
    except
      msg('Error: unknown driver version',9);
    end;
    try
    FCameraXSize:=V.Get('cameraxsize').AsInt;
    except
     on E: Exception do begin
       FCameraXSize:=-1;
       msg('Error: cannot get frame size X from camera: ' + E.Message,0);
     end;
    end;
    try
    FCameraYSize:=V.Get('cameraysize').AsInt;
    except
     on E: Exception do begin
       FCameraYSize:=-1;
       msg('Error: cannot get frame size Y from camera: ' + E.Message,0);
     end;
    end;
    if debug_msg then msg('Camera size='+inttostr(FCameraXSize)+'/'+inttostr(FCameraYSize));
    try
     FMaxBinX:=V.Get('maxbinx').AsInt;
    except
     FMaxBinX:=1;
    end;
    try
    FMaxBinY:=V.Get('maxbiny').AsInt;
    except
     FMaxBinY:=1;
    end;
    try
     FBinX:=V.Get('binx').AsInt;
    except
     FBinX:=1;
    end;
    try
     FBinY:=V.Get('biny').AsInt;
    except
     FBinY:=1;
    end;
    FPixelSizeX:=0;
    FPixelSizeY:=0;
    try
      FPixelSizeX:=round(V.Get('pixelsizex').AsFloat*100)/100;
      FPixelSizeY:=round(V.Get('pixelsizey').AsFloat*100)/100;
    except
      on E: Exception do begin
        msg('Error: cannot get pixel size from camera: ' + E.Message,0);
      end;
    end;
    Fccdname:=Fdevice;
    try
      Fccdname:=V.Get('name').AsString;
      Fccdname:=Fccdname+'-'+V.Get('sensorname').AsString;
    except
    end;
    try
      FhasCoolerPower:=V.Get('cangetcoolerpower').AsBool;
    except
      FhasCoolerPower:=false;
    end;
    try
      FCanSetTemperature:=V.Get('cansetccdtemperature').AsBool;
    except
      FCanSetTemperature:=false;
    end;
    try
      DummyDouble:=V.Get('ccdtemperature').AsFloat;
      FHasTemperature:=true;
    except
      FHasTemperature:=false;
    end;
    FReadOutList.Clear;
    try
      FhasFastReadout:=V.Get('canfastreadout').AsBool;
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
        rlist:=V.Get('readoutmodes').AsStringArray;
        n:=Length(rlist);
        for i:=0 to n-1 do begin
          FReadOutList.Add(rlist[i]);
        end;
        SetLength(rlist,0);
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
        FOffsetX:=V.Get('bayeroffsetx').AsInt;
        FOffsetY:=V.Get('bayeroffsety').AsInt;
        i:=V.Get('SensorType').AsInt;
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
    FStatus := devConnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    StatusTimer.Interval:=10;
    StatusTimer.Enabled:=true;
    msg(rsConnected3);
  end
  else
     Disconnect;
 except
   on E: Exception do begin
      msg(Format(rsConnectionEr, [E.Message]),0);
      Disconnect;
   end;
 end;
end;

procedure T_ascomrestcamera.Disconnect;
begin
  StatusTimer.Enabled:=false;
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  try
    msg(rsDisconnected3,0);
    // the server is responsible for device disconnection
  except
    on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
  end;
end;

function T_ascomrestcamera.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

procedure T_ascomrestcamera.StatusTimerTimer(sender: TObject);
var t: double;
    c: boolean;
begin
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     msg(rsDisconnected3,0);
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
       t:=GetTemperatureReal;
       if (t<>stCCDtemp) then begin
         stCCDtemp:=t;
         if Assigned(FonTemperatureChange) then FonTemperatureChange(stCCDtemp);
       end;
    end;
    except
     on E: Exception do msg(Format(rsError, [E.Message]),0);
    end;
  end;
 finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
 end;
end;

procedure T_ascomrestcamera.RestartExposure;
begin
     if (Fexptime>0)   then
        StartExposure(Fexptime)
     else
        if assigned(FonAbortExposure) then FonAbortExposure(self);
end;

Procedure T_ascomrestcamera.StartExposure(exptime: double);
var li: string;
begin
  if FStatus<>devConnected then exit;
  case FFrametype of
    LIGHT: li:='true';
    BIAS : li:='false';
    DARK : li:='false';
    FLAT : li:='true';
  end;
  try
     Fexptime:=exptime;
     if debug_msg then msg('start exposure.');
     Ftimestart:=NowUTC;
     V.Put('startexposure',['Duration',formatfloat(f4,exptime),'Light',li]);
     Ftimestart:=(Ftimestart+NowUTC)/2;
     inc(FImgNum);
     Ftimeend:=now+(exptime)/secperday;
     timedout:=now+(exptime+CameraTimeout)/secperday;
     if exptime>=10 then ExposureTimer.Interval:=1000
     else ExposureTimer.Interval:=500;
     ExposureTimer.Enabled:=true;
     StatusTimer.Enabled:=true;
  except
     on E: Exception do msg(Format(rsStartExposur, [E.Message]),0);
  end;
end;

procedure T_ascomrestcamera.ExposureTimerTimer(sender: TObject);
var ok: boolean;
    imgarray:TImageArray;
    i,j,k,c,xs,ys: integer;
    nax1,nax2,state: integer;
    pix,piy,expt: double;
    dateobs,ccdname,frname:string;
    Dims,x,y: Integer;
    lii: integer;
    ii: smallint;
    b: array[0..2880]of char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
    n, nb: byte;
    w,ww,pxdiv,newsaturation: word;
begin
 ExposureTimer.Enabled:=false;
 try
 if (now<timedout) then begin
    state:=5;
    try
      state:=V.Get('camerastate').AsInt;
    except
      msg('Error reading camera state '+inttostr(state),0);
    end;
    ok:=false;
    try
      if state=0 then ok:=V.Get('imageready').AsBool;
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
        2 : FonExposureProgress(secperday*(Ftimeend-now)); // exposure in progress
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
   FMidExposureTime:=(Ftimestart+NowUTC)/2;
   FImageFormat:='.fits';
   if assigned(FonExposureProgress) then FonExposureProgress(-10);
   if debug_msg then msg('read image.');
   try
   imgarray:=V.GetImageArray;
   except
     on E: Exception do begin
       msg('Error accessing ImageArray: ' + E.Message,0);
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
   end;
   // if possible start next exposure now
   TryNextExposure(FImgNum);
   Dims:=imgarray.nplane;
   if (Dims<2)or(Dims>3) then begin
     msg('Error ImageArray unsupported Dimension=' + inttostr(Dims));
     if assigned(FonAbortExposure) then FonAbortExposure(self);
     exit;
   end;
   xs:=imgarray.width;
   ys:=imgarray.height;
   if debug_msg then msg('width:'+inttostr(xs)+' height:'+inttostr(ys));
   nax1:=xs;
   nax2:=ys;
   pix:=FPixelSizeX;
   piy:=FPixelSizeY;
   ccdname:=Fccdname;
   frname:=FrameName[ord(FFrametype)];
   dateobs:=FormatDateTime(dateiso,Ftimestart);
   if FhasLastExposureStartTime then begin
     try
       dateobs:=V.Get('lastexposurestarttime').AsString;
     except
       FhasLastExposureStartTime:=false;
     end;
   end;
   expt:=Fexptime;
   if FhasLastExposureDuration then begin
     try
       expt:=V.Get('lastexposureduration').AsFloat;
     except
       FhasLastExposureDuration:=false;
     end;
   end;
   // count used bit by pixel
   pxdiv:=1;
   if FFixPixelRange then begin
     nb:=16;
     w:=0;
     for i:=0 to ys-1 do begin
       for j := 0 to xs-1 do begin
         ww:=imgarray.img[0,i,j];
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
   if debug_msg then msg('set fits header');
   hdr:=TFitsHeader.Create;
   hdr.ClearHeader;
   hdr.Add('SIMPLE',true,'file does conform to FITS standard');
   hdr.Add('BITPIX',16,'number of bits per data pixel');
   if dims=2 then begin
   hdr.Add('NAXIS',2,'number of data axes');
   hdr.Add('NAXIS1',nax1 ,'length of data axis 1');
   hdr.Add('NAXIS2',nax2 ,'length of data axis 2');
   end
   else begin
     hdr.Add('NAXIS',3,'number of data axes');
     hdr.Add('NAXIS1',3 ,'length of data axis 1');
     hdr.Add('NAXIS2',nax1 ,'length of data axis 1');
     hdr.Add('NAXIS3',nax2 ,'length of data axis 2');
   end;
   hdr.Add('EXTEND',true,'FITS dataset may contain extensions');
   hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
   hdr.Add('BSCALE',1,'default scaling factor');
   hdr.Add('EXPTIME',expt,'Total Exposure Time (s)');
   hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
   hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
   hdr.Add('XBINNING',BinX ,'Binning factor in width');
   hdr.Add('YBINNING',BinY ,'Binning factor in height');
   hdr.Add('FRAME',frname,'Frame Type');
   hdr.Add('INSTRUME',ccdname,'CCD Name');
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
   if debug_msg then msg('write image');
   if Dims=2 then begin
     for i:=0 to ys-1 do begin
        if FASCOMFlipImage then
           y:=ys-1-i
        else
           y:=i;
        for j:=0 to xs-1 do begin
          x:=j;
          lii:=imgarray.img[0,y,x];
          if FFixPixelRange then lii:=lii div pxdiv;
          if lii>0 then
             ii:=lii-32768
          else
             ii:=-32768;
          ii:=NtoBE(ii);
          FImgStream.Write(ii,sizeof(smallint));
        end;
     end;
   end
   else if Dims=3 then begin
     for k:=0 to 2 do begin
     for i:=0 to ys-1 do begin
        if FASCOMFlipImage then
           y:=ys-1-i
        else
           y:=i;
        for j:=0 to xs-1 do begin
          x:=j;
          lii:=imgarray.img[k,y,x];
          if FFixPixelRange then lii:=lii div pxdiv;
          if lii>0 then
             ii:=lii-32768
          else
             ii:=-32768;
          ii:=NtoBE(ii);
          FImgStream.Write(ii,sizeof(smallint));
        end;
     end;
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
   if debug_msg then msg('release imagearray');
   imgarray.Free;
   if assigned(FonExposureProgress) then FonExposureProgress(-11);
   NewImage;
   finally
   StatusTimer.Enabled:=true;
   end;
 end;
 except
    on E: Exception do msg('Error reading image: ' + E.Message,0);
 end;
end;

Procedure T_ascomrestcamera.SetBinning(sbinX,sbinY: integer);
var oldx,oldy,newx,newy,fsx,fsy,fnx,fny: integer;
    scale:double;
begin
   if FStatus<>devConnected then exit;
   try
   if debug_msg then msg('Request binning '+inttostr(sbinX)+','+inttostr(sbinY));
   oldx:=FBinX;
   oldy:=FBinY;
   if debug_msg then msg('Old binning '+inttostr(oldx)+','+inttostr(oldy));
   if (oldx<>sbinX)or(oldy<>sbinY) then begin
     msg(Format(rsSetBinningX, [inttostr(sbinX), inttostr(sbinY)]));
     GetFrameReal(fsx,fsy,fnx,fny);
     if debug_msg then msg('Current frame '+inttostr(fsx)+','+inttostr(fsy)+'/'+inttostr(fnx)+'x'+inttostr(fny));
     scale:=oldx/sbinX;
     fsx:=trunc(fsx*scale);
     fnx:=trunc(fnx*scale);
     scale:=oldy/sbinY;
     fsy:=trunc(fsy*scale);
     fny:=trunc(fny*scale);
     newx:=FCameraXSize div sbinX;
     newy:=FCameraYSize div sbinY;
     V.Put('BinX',sbinX);
     V.Put('BinY',sbinY);
     FBinX:=sbinX;
     FBinY:=sbinY;
     if (fsx=0)and(fsy=0)and((abs(newx-fnx)/fnx)<0.1)and((abs(newy-fny)/fny)<0.1)
        then SetFrame(0,0,newx,newy)
        else SetFrame(fsx,fsy,fnx,fny);
     Wait(1);
   end;
   except
    on E: Exception do msg('Set binning error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestcamera.SetFrame(x,y,width,height: integer);
var Xmax,Ymax,w,h,bx,by: integer;
begin
   if FStatus<>devConnected then exit;
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
   if x>(Xmax-MinFrameSize) then x:=Xmax-MinFrameSize;
   if y>(Ymax-MinFrameSize) then y:=Ymax-MinFrameSize;
   if (x+width)>Xmax then width:=Xmax-x;
   if (y+height)>Ymax then height:=Ymax-y;
   // force even values
   x:=round(x+0.5);
   y:=round(y+0.5);
   if debug_msg then msg('Set frame '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height));
   V.Put('StartX',x);
   V.Put('StartY',y);
   V.Put('NumX',width);
   V.Put('NumY',height);
   stX      := x;
   stY      := y;
   stWidth  := width;
   stHeight := height;
   Wait(1);
   if Assigned(FonFrameChange) then FonFrameChange(self);
   except
    on E: Exception do begin
      msg('Set frame error: ' + E.Message,0);
      stX:=-1;
    end;
   end;
end;

procedure T_ascomrestcamera.GetFrame(out x,y,width,height: integer; refresh:boolean=false);
begin
  // ignore the refresh parameter
  if (stX<0)or(stY<0)or(stWidth<0)or(stHeight<0) then begin
    GetFrameReal(x,y,width,height);
  end
  else begin
    x:=stX;
    y:=stY;
    width:=stWidth;
    height:=stHeight;
  end;
end;

procedure T_ascomrestcamera.GetFrameReal(out x,y,width,height: integer);
begin
   if FStatus<>devConnected then exit;
   try
   x      := V.Get('startx').AsInt;
   y      := V.Get('starty').AsInt;
   width  := V.Get('numx').AsInt;
   height := V.Get('numy').AsInt;
   if debug_msg then msg('Current frame: '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height),3);
   except
    on E: Exception do msg('Get frame error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestcamera.GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
begin
 xr:=NullRange;yr:=NullRange;widthr:=NullRange;heightr:=NullRange;
   if FStatus<>devConnected then exit;
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
end;

procedure T_ascomrestcamera.ResetFrame;
var w,h,bx,by: integer;
begin
  if FStatus<>devConnected then exit;
  try
  w:=FCameraXSize;
  h:=FCameraYSize;
  bx:=FBinX;
  by:=FBinY;
  if debug_msg then msg('ResetFrame: XSize='+inttostr(w)+' YSize='+inttostr(h)+' BinX='+inttostr(bx)+' BinY='+inttostr(by));
  w:=w div bx;
  h:=h div by;
  SetFrame(0,0,w,h);
  Wait(1);
  except
   on E: Exception do msg('Reset frame error: ' + E.Message,0);
  end;
end;

Procedure T_ascomrestcamera.AbortExposureButNotSequence;
begin
   if FStatus<>devConnected then exit;
   try
    ExposureTimer.Enabled:=false;
    StatusTimer.Enabled:=true;
    V.Put('abortexposure');
   except
    on E: Exception do msg('Abort exposure error: ' + E.Message,0);
   end;
end;

Procedure T_ascomrestcamera.AbortExposure;
begin
   if FStatus<>devConnected then exit;
   try
    ExposureTimer.Enabled:=false;
    StatusTimer.Enabled:=true;
    V.Put('abortexposure');
    if assigned(FonAbortExposure) then FonAbortExposure(self);
   except
    on E: Exception do msg('Abort exposure error: ' + E.Message,0);
   end;
end;

Procedure T_ascomrestcamera.SetActiveDevices(afocuser,afilters,atelescope: string);
begin
  // not in ascom
end;

function T_ascomrestcamera.GetBinX:integer;
begin
 result:=FBinX;
end;

function T_ascomrestcamera.GetBinY:integer;
begin
 result:=FBinY;
end;

procedure T_ascomrestcamera.SetFrametype(f:TFrameType);
begin
  if FStatus<>devConnected then exit;
  msg(Format(rsSetFrameType, [FrameName[ord(f)]]));
  FFrametype:=f;
end;

function  T_ascomrestcamera.GetFrametype:TFrameType;
begin
  result:=FFrametype;
end;

function T_ascomrestcamera.GetBinXrange:TNumRange;
begin
   result:=UnitRange;
  if FStatus<>devConnected then exit;
   result.max:=FMaxBinX;
end;

function T_ascomrestcamera.GetBinYrange:TNumRange;
begin
   result:=UnitRange;
  if FStatus<>devConnected then exit;
   result.max:=FMaxBinY;
end;

function T_ascomrestcamera.GetExposureRange:TNumRange;
begin
  result:=NullRange;
  if FStatus<>devConnected then exit;
    try
    result.max:=V.Get('exposuremax').AsFloat;
    except
     result.max:=3600;
    end;
    try
    result.min:=V.Get('exposuremin').AsFloat;
    except
    result.min:=0.001;
    end;
    try
    result.step:=V.Get('exposureresolution').AsFloat;
    except
    end;
end;

function T_ascomrestcamera.GetTemperatureRange:TNumRange;
begin
  // not in ascom
  result.min:=-50;
  result.max:=50;
  result.step:=0;
end;

procedure T_ascomrestcamera.SetFilter(num:integer);
begin
   if FStatus<>devConnected then exit;
   try
   msg(Format(rsSetFilterPos, [inttostr(num)]));
   V.Put('Position',num-1);
   Wait(1);
   except
    on E: Exception do msg('Set filter error: ' + E.Message,0);
   end;
end;

function  T_ascomrestcamera.GetFilter:integer;
begin
 result:=0;
   if FStatus<>devConnected then exit;
   try
   result:=V.Get('position').AsInt+1;
   except
    on E: Exception do msg('Get filter error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestcamera.SetFilterNames(value:TStringList);
var i:integer;
begin
 if FStatus<>devConnected then exit;
 try
  if (value.Count=nf) then begin
    for i:=0 to value.Count-1 do begin
       FFilterNames[i]:=value[i];
    end;
  end;
  except
  end;
end;

function  T_ascomrestcamera.GetTemperature: double;
begin
 if FStatus=devConnected then
    result:=stCCDtemp
 else
    result:=NullCoord;
end;

function  T_ascomrestcamera.GetTemperatureReal: double;
begin
 result:=NullCoord;
 if FStatus<>devConnected then exit;
   try
   if FHasTemperature then
      result:=V.Get('ccdtemperature').AsFloat
   else
      result:=NullCoord;
   except
     result:=NullCoord;
   end;
end;

procedure T_ascomrestcamera.SetTemperature(value:double);
begin
   if FStatus<>devConnected then exit;
   try
   if FCanSetTemperature then begin
      SetCooler(true);
      V.Put('SetCCDTemperature',value);
   end;
   except
    on E: Exception do msg('Set temperature error: ' + E.Message,0);
   end;
end;

function  T_ascomrestcamera.GetCoolerPower: Double;
begin
 result:=NullCoord;
   if (FStatus<>devConnected)or(not FhasCoolerPower) then exit;
   try
     result:=V.Get('coolerpower').AsFloat;
   except
     result:=NullCoord;
   end;
end;

function  T_ascomrestcamera.GetCooler: boolean;
begin
 result:=false;
   if FStatus<>devConnected then exit;
   try
     result:=V.Get('cooleron').AsBool;
   except
     result:=false;
   end;
end;

procedure T_ascomrestcamera.SetCooler(value:boolean);
begin
  if FStatus<>devConnected then exit;
  try
  if (V.Get('cooleron').AsBool<>value) then begin
     msg(Format(rsSetCooler, [': '+BoolToStr(value, rsTrue, rsFalse)]));
     V.Put('CoolerOn',value);
  end;
  except
   on E: Exception do msg('Set cooler error: ' + E.Message,0);
  end;
end;

function T_ascomrestcamera.GetMaxX: double;
begin
  if FStatus<>devConnected then exit;
 result:=FCameraXSize;
end;

function T_ascomrestcamera.GetMaxY: double;
begin
  if FStatus<>devConnected then exit;
 result:=FCameraYSize;
end;

function T_ascomrestcamera.GetMaxADU: double;
begin
 result:=MAXWORD;
  if FStatus<>devConnected then exit;
  try
     result:=V.Get('maxadu').AsFloat;
  except
     result:=MAXWORD;
  end;
end;

function T_ascomrestcamera.GetPixelSize: double;
begin
 result:=-1;
  if FStatus<>devConnected then exit;
  try
     result:=FPixelSizeX;
  except
     result:=-1;
  end;
end;

function T_ascomrestcamera.GetPixelSizeX: double;
begin
 result:=-1;
  if FStatus<>devConnected then exit;
  try
     result:=FPixelSizeX;
  except
     result:=-1;
  end;
end;

function T_ascomrestcamera.GetPixelSizeY: double;
begin
 result:=-1;
  if FStatus<>devConnected then exit;
  try
     result:=FPixelSizeY;
  except
     result:=-1;
  end;
end;

function T_ascomrestcamera.GetBitperPixel: double;
begin
result:=16;
end;

function T_ascomrestcamera.GetColor: boolean;
begin
 result:=false;
  if FStatus<>devConnected then exit;
   try
      result:=(V.Get('sensortype').AsInt=1);  // Camera produces color image directly, requiring not Bayer decoding
   except
      result:=false;
   end;
end;

procedure T_ascomrestcamera.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomrestcamera.CheckGain:boolean;
var i,n: integer;
    isol: string;
    gainlist: array of string;
begin
  result:=false;
  if FStatus<>devConnected then exit;
    try
    // check Gain property
       i:=V.Get('gain').AsInt;
       try
       // check Gain range
          FGainMin:=V.Get('gainmin').AsInt;
          FGainMax:=V.Get('gainmax').AsInt;
          FhasGain:=true;
       except
       // No Gain range
          FhasGain:=false;
       end;
       try
       // Check ISO list
          gainlist:=V.Get('gains').AsStringArray;
          n:=Length(gainlist);
          FISOList.Clear;
          for i:=0 to n-1 do begin
            isol:=gainlist[i];
            FISOList.Add(isol);
          end;
          SetLength(gainlist,0);
          FhasGainISO:=FISOList.Count>0;
       except
       // No ISO list
          FhasGainISO:=false;
          FISOList.Clear;
       end;
    except
    // No Gain property at all
       FhasGain:=false;
       FhasGainISO:=false;
    end;
    result:=(FhasGainISO or FhasGain);
end;

procedure T_ascomrestcamera.SetGain(value: integer);
begin
 if FStatus<>devConnected then exit;
 if FCanSetGain and (FhasGainISO or FhasGain) then begin
   try
      V.Put('Gain',value);
      stGain:=value;
   except
   end;
 end;
end;

function T_ascomrestcamera.GetGain: integer;
begin
 if FStatus<>devConnected then
   result:=NullInt
 else begin
   if stGain=-1 then begin
      stGain:=GetGainReal;
   end;
   result:=stGain;
 end;
end;

function T_ascomrestcamera.GetGainReal: integer;
begin
 result:=NullInt;
 if FStatus<>devConnected then exit;
 if (FhasGainISO or FhasGain) then begin
   try
      result:=V.Get('gain').AsInt;
   except
      result:=NullInt;
   end;
 end;
end;

procedure T_ascomrestcamera.CfaInfo(out OffsetX, OffsetY: integer; out CType: string);
begin
 OffsetX:=FOffsetX;
 OffsetY:=FOffsetY;
 CType:=FCType;
end;

procedure T_ascomrestcamera.SetReadOutMode(value: integer);
begin
 if FStatus<>devConnected then exit;
 try
 if FhasReadOut then begin
   if FhasFastReadout then begin
     if value=0 then V.Put('FastReadout',false)
                else V.Put('FastReadout',true);
   end
   else begin
     V.Put('ReadoutMode',value);
   end;
 end;
 except
    on E: Exception do msg('Set ReadOut='+inttostr(value)+': '+E.Message,0);
 end;
end;

function T_ascomrestcamera.GetReadOutMode: integer;
begin
  result:=0;
 if FStatus<>devConnected then exit;
  try
  if (FhasReadOut) then begin
     if FhasFastReadout then begin
       if V.Get('fastreadout').AsBool then result:=1
          else result:=0;
     end
     else begin
       result:=V.Get('readoutmode').AsInt;
     end;
  end;
  except
     on E: Exception do msg('Get ReadOut: '+E.Message,0);
  end;
end;

procedure T_ascomrestcamera.SetFnumber(value: string);
begin
  //unsupported
end;

function T_ascomrestcamera.GetFnumber: string;
begin
  result:='';
  //unsupported
end;

procedure T_ascomrestcamera.StartVideoPreview;
begin
// todo
end;

procedure T_ascomrestcamera.StopVideoPreview;
begin
// todo
end;

function T_ascomrestcamera.GetVideoPreviewRunning: boolean;
begin
 result:=false;
 // todo
end;

function T_ascomrestcamera.GetMissedFrameCount: cardinal;
begin
 result:=0;
 // todo
end;

function T_ascomrestcamera.GetVideoRecordDuration:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomrestcamera.SetVideoRecordDuration(value:integer);
begin
 // todo
end;

function T_ascomrestcamera.GetVideoRecordFrames:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomrestcamera.SetVideoRecordFrames(value:integer);
begin
 // todo
end;

procedure T_ascomrestcamera.StartVideoRecord(mode:TVideoRecordMode);
begin
 // todo
end;

procedure T_ascomrestcamera.StopVideoRecord;
begin
 // todo
end;

function T_ascomrestcamera.GetVideoSize:string;
begin
 result:='';
 // todo
end;

procedure T_ascomrestcamera.SetVideoSize(value:string);
begin
 // todo
end;

function T_ascomrestcamera.GetVideoRate:string;
begin
 result:='';
 // todo
end;

procedure T_ascomrestcamera.SetVideoRate(value:string);
begin
 // todo
end;

function T_ascomrestcamera.GetFPS:double;
begin
 result:=0;
 //todo
end;

function T_ascomrestcamera.GetVideoRecordDir:string;
begin
 result:='';
 // todo
end;

procedure T_ascomrestcamera.SetVideoRecordDir(value:string);
begin
 // todo
end;

function T_ascomrestcamera.GetVideoRecordFile:string;
begin
 result:='';
 // todo
end;

procedure T_ascomrestcamera.SetVideoRecordFile(value:string);
begin
 // todo
end;

function T_ascomrestcamera.GetVideoExposure:integer;
begin
 result:=0;
 // todo
end;

function T_ascomrestcamera.GetVideoGain:integer;
begin
 result:=NullInt;
 // todo
end;

function T_ascomrestcamera.GetVideoGamma:integer;
begin
 result:=NullInt;
 // todo
end;

function T_ascomrestcamera.GetVideoBrightness:integer;
begin
 result:=NullInt;
 // todo
end;

procedure T_ascomrestcamera.SetVideoExposure(value:integer);
begin
 // todo
end;

procedure T_ascomrestcamera.SetVideoGain(value:integer);
begin
 // todo
end;

procedure T_ascomrestcamera.SetVideoGamma(value:integer);
begin
 // todo
end;

procedure T_ascomrestcamera.SetVideoBrightness(value:integer);
begin
 // todo
end;

function T_ascomrestcamera.GetVideoExposureRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomrestcamera.GetVideoGainRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomrestcamera.GetVideoGammaRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomrestcamera.GetVideoBrightnessRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomrestcamera.GetVideoPreviewDivisor:integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomrestcamera.SetVideoPreviewDivisor(value:integer);
begin
 // todo
end;

function T_ascomrestcamera.GetStreamingExposureRange:TNumRange;
begin
 result:=NullRange;
 // todo
end;

function T_ascomrestcamera.GetStreamingExposure:double;
begin
 result:=0;
 // todo
end;

procedure T_ascomrestcamera.SetStreamingExposure(value:double);
begin
 // todo
end;

procedure T_ascomrestcamera.GetStreamFrame(out x,y,width,height: integer);
begin
 // todo
 x:=0; y:=0; width:=0; height:=0;
end;

function T_ascomrestcamera.GetVideoEncoder: integer;
begin
 result:=0;
 // todo
end;

procedure T_ascomrestcamera.SetVideoEncoder(value:integer);
begin
 // todo
end;

function T_ascomrestcamera.GetImageFormat: string;
begin
 result:=FImageFormat;
end;

end.

