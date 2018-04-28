unit cu_ascomcamera;

{$mode objfpc}{$H+}

//{$define debug_ascom}

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

uses  cu_camera, u_global, u_translation,
  {$ifdef mswindows}
    u_utils, cu_fits, lazutf8sysutils, indiapi,
    Variants, comobj,
  {$endif}
  Forms, ExtCtrls, Classes, SysUtils, LCLType;

type
T_ascomcamera = class(T_camera)
 private
   {$ifdef mswindows}
   V: variant;
   nf: integer;
   timestart,timeend,timedout,Fexptime:double;
   stX,stY,stWidth,stHeight: integer;
   {$endif}
   stCCDtemp : double;
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
   function  GetCooler: boolean; override;
   procedure SetCooler(value:boolean); override;
   function GetMaxX: double; override;
   function GetMaxY: double; override;
   function GetMaxADU: double; override;
   function GetPixelSize: double; override;
   function GetPixelSizeX: double; override;
   function GetPixelSizeY: double; override;
   function GetBitperPixel: double; override;
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

public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''); override;
   procedure Disconnect;  override;
   Procedure StartExposure(exptime: double); override;
   Procedure SetBinning(sbinX,sbinY: integer); override;
   procedure SetFrame(x,y,width,height: integer); override;
   procedure GetFrame(out x,y,width,height: integer); override;
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange); override;
   procedure ResetFrame; override;
   function  CheckGain:boolean; override;
   Procedure AbortExposure; override;
   Procedure SetActiveDevices(afocuser,afilters,atelescope: string); override;
   procedure StartVideoPreview; override;
   procedure StopVideoPreview; override;
   procedure StartVideoRecord(mode:TVideoRecordMode); override;
   procedure StopVideoRecord; override;
end;


implementation

constructor T_ascomcamera.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 stCooler:=false;
 stCCDtemp:=NullCoord;
 FCameraInterface:=ASCOM;
 FVerticalFlip:=false;
 ExposureTimer:=TTimer.Create(nil);
 ExposureTimer.Enabled:=false;
 ExposureTimer.Interval:=100;
 ExposureTimer.OnTimer:=@ExposureTimerTimer;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomcamera.Destroy;
begin
 ExposureTimer.Free;
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomcamera.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string='');
begin
{$ifdef mswindows}
 try
 FStatus := devConnecting;
 Fdevice:=cp1;
 V:=Unassigned;
 V:=CreateOleObject(Fdevice);
 V.connected:=true;
 if V.connected then begin
    FStatus := devConnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    StatusTimer.Enabled:=true;
    msg(rsConnected3);
 end
 else
    Disconnect;
 except
   on E: Exception do msg(Format(rsConnectionEr, [E.Message]));
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
    msg(rsDisconnected3);
  end;
  except
    on E: Exception do msg(Format(rsDisconnectio, [E.Message]));
  end;
{$endif}
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
var x,y,width,height: integer;
    t: double;
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
    if V.CanSetCCDTemperature then begin
       t:=GetTemperature;
       if (t<>stCCDtemp) then begin
         stCCDtemp:=t;
         if Assigned(FonTemperatureChange) then FonTemperatureChange(stCCDtemp);
    end;
    end;
    GetFrame(x,y,width,height);
    if (x<>stX)or(y<>stY)or(width<>stWidth)or(height<>stHeight) then begin
       stX:=x;
       stY:=y;
       stWidth:=width;
       stHeight:=height;
       if Assigned(FonFrameChange) then FonFrameChange(self);
    end;
    except
     on E: Exception do msg(Format(rsError, [E.Message]));
    end;
  end;
 {$endif}
end;

Procedure T_ascomcamera.StartExposure(exptime: double);
{$ifdef mswindows}
var li: boolean;
{$endif}
begin
{$ifdef mswindows}
if Connected then begin
  case FFrametype of
    LIGHT: li:=true;
    BIAS : li:=false;
    DARK : li:=false;
    FLAT : li:=true;
  end;
  try
     {$ifdef debug_ascom}msg('start exposure.');{$endif}
     V.StartExposure(exptime,li);
     timestart:=NowUTC;
     timeend:=now+(exptime)/secperday;
     timedout:=now+(exptime+60)/secperday;
     Fexptime:=exptime;
     if exptime>=10 then ExposureTimer.Interval:=1000
     else if exptime>=1 then ExposureTimer.Interval:=500
     else if exptime>=0.5 then ExposureTimer.Interval:=100
     else ExposureTimer.Interval:=50;
     ExposureTimer.Enabled:=true;
  except
     on E: Exception do msg(Format(rsStartExposur, [E.Message]));
  end;
end;
{$endif}
end;

procedure T_ascomcamera.ExposureTimerTimer(sender: TObject);
{$ifdef mswindows}
var ok: boolean;
    i,j,c,xs,ys: integer;
    nax1,nax2,state: integer;
    pix,piy: double;
    dateobs,ccdname,frname:string;
    img: array of array of LongInt;
    ii: smallint;
    b: array[0..2880]of char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
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
      msg('Error reading camera state');
    end;
    ok:=false;
    try
      ok:=V.ImageReady;
    except
      on E: Exception do begin
        msg('Error reading camera image availability: ' + E.Message);
        if assigned(FonAbortExposure) then FonAbortExposure(self);
        exit;
      end;
    end;
    {$ifdef debug_ascom}msg(' status:'+inttostr(state)+', image ready:'+BoolToStr(ok, rsTrue, rsFalse));{$endif}
    if (not ok) then begin
      // in progress
      if assigned(FonExposureProgress) then FonExposureProgress(secperday*(timeend-now));
      ExposureTimer.Enabled:=true;
      exit;
    end;
 end
 else begin
   ok:=false;
   msg(rsTimeout);
   if assigned(FonAbortExposure) then FonAbortExposure(self);
 end;

 if ok then begin
   if assigned(FonExposureProgress) then FonExposureProgress(0);
   {$ifdef debug_ascom}msg('read image.');{$endif}
   try
   img:=V.ImageArray;
   except
     on E: Exception do begin
       msg('Error accessing ImageArray: ' + E.Message);
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
   end;
   xs:=length(img);
   ys:=length(img[0]);
   {$ifdef debug_ascom}msg('width:'+inttostr(xs)+' height:'+inttostr(ys));{$endif}
   nax1:=xs;
   nax2:=ys;
   pix:=0;
   piy:=0;
   try
     pix:=V.PixelSizeX;
     piy:=V.PixelSizeY;
   except
     on E: Exception do begin
       msg('Error: cannot get pixel size from camera: ' + E.Message);
     end;
   end;
   ccdname:=Fdevice;
   try
     ccdname:=V.Name;
     ccdname:=ccdname+'-'+V.SensorName;
   except
   end;
   frname:=FrameName[ord(FFrametype)];
   dateobs:=FormatDateTime(dateisoshort,timestart);
   {$ifdef debug_ascom}msg('set fits header');{$endif}
   hdr:=TFitsHeader.Create;
   hdr.ClearHeader;
   hdr.Add('SIMPLE',true,'file does conform to FITS standard');
   hdr.Add('BITPIX',16,'number of bits per data pixel');
   hdr.Add('NAXIS',2,'number of data axes');
   hdr.Add('NAXIS1',nax1 ,'length of data axis 1');
   hdr.Add('NAXIS2',nax2 ,'length of data axis 2');
   hdr.Add('EXTEND',true,'FITS dataset may contain extensions');
   hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
   hdr.Add('BSCALE',1,'default scaling factor');
   hdr.Add('EXPTIME',Fexptime,'Total Exposure Time (s)');
   hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
   hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
   hdr.Add('XBINNING',BinX ,'Binning factor in width');
   hdr.Add('YBINNING',BinY ,'Binning factor in height');
   hdr.Add('FRAME',frname,'Frame Type');
   hdr.Add('INSTRUME',ccdname,'CCD Name');
   hdr.Add('DATE-OBS',dateobs,'UTC start date of observation');
   hdr.Add('END','','');
   hdrmem:=hdr.GetStream;
   FImgStream.Clear;
   FImgStream.position:=0;
   hdrmem.Position:=0;
   {$ifdef debug_ascom}msg('write header');{$endif}
   FImgStream.CopyFrom(hdrmem,hdrmem.Size);
   hdrmem.Free;
   hdr.Free;
   {$ifdef debug_ascom}msg('write image');{$endif}
   for i:=0 to ys-1 do begin
      for j:=0 to xs-1 do begin
        ii:=img[j,ys-1-i]-32768;
        ii:=NtoBE(ii);
        FImgStream.Write(ii,sizeof(smallint));
      end;
   end;
   {$ifdef debug_ascom}msg('pad fits');{$endif}
   c:=2880-(FImgStream.Size mod 2880);
   FillChar(b,c,0);
   FImgStream.Write(b,c);
   {$ifdef debug_ascom}msg('display image');{$endif}
   NewImage;
 end;
 except
    on E: Exception do msg('Error reading image: ' + E.Message);
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
 if Connected then begin
   try
   oldx:=V.BinX;
   oldy:=V.BinY;
   if (oldx<>sbinX)or(oldy<>sbinY) then begin
     msg(Format(rsSetBinningX, [inttostr(sbinX), inttostr(sbinY)]));
     GetFrame(fsx,fsy,fnx,fny);
     scale:=oldx/sbinX;
     fsx:=trunc(fsx*scale);
     fnx:=trunc(fnx*scale);
     scale:=oldy/sbinY;
     fsy:=trunc(fsy*scale);
     fny:=trunc(fny*scale);
     newx:=V.CameraXSize div sbinX;
     newy:=V.CameraYSize div sbinY;
     V.BinX:=sbinX;
     V.BinY:=sbinY;
     if (fsx=0)and(fsy=0)and((abs(newx-fnx)/fnx)<0.1)and((abs(newy-fny)/fny)<0.1)
        then SetFrame(0,0,newx,newy)
        else SetFrame(fsx,fsy,fnx,fny);
     Wait(1);
   end;
   except
    on E: Exception do msg('Camera '+Fdevice+' Set binning error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.SetFrame(x,y,width,height: integer);
{$ifdef mswindows}
var Xmax,Ymax: integer;
{$endif}
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   Xmax:= V.CameraXSize div V.BinX;
   Ymax:= V.CameraYSize div V.BinY;
   // force even values
   x:=round(x+0.5);
   y:=round(y+0.5);
   // check range
   if x>Xmax then x:=Xmax-1;
   if y>Ymax then y:=Ymax-1;
   if (x+width)>Xmax then width:=Xmax-x;
   if (y+height)>Ymax then height:=Ymax-y;
   {$ifdef debug_ascom}msg('set frame '+inttostr(width)+'x'+inttostr(height));{$endif}
   V.StartX:=x;
   V.StartY:=y;
   V.NumX:=width;
   V.NumY:=height;
   Wait(1);
   except
    on E: Exception do msg('Set frame error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.GetFrame(out x,y,width,height: integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   x      := V.StartX;
   y      := V.StartY;
   width  := V.NumX;
   height := V.NumY;
   except
    on E: Exception do msg('Get frame error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
begin
 xr:=NullRange;yr:=NullRange;widthr:=NullRange;heightr:=NullRange;
 {$ifdef mswindows}
 if Connected then begin
   try
   xr.min:=0;
   xr.max:=V.CameraXSize-1;
   xr.step:=1;
   yr.min:=0;
   yr.max:=V.CameraYSize-1;
   yr.step:=1;
   widthr.min:=1;
   widthr.max:=V.CameraXSize;
   widthr.step:=1;
   heightr.min:=1;
   heightr.max:=V.CameraYSize;
   heightr.step:=1;
   except
    on E: Exception do msg('Get frame range error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.ResetFrame;
{$ifdef mswindows}
var w,h: integer;
{$endif}
begin
{$ifdef mswindows}
if Connected then begin
  try
  w:=V.CameraXSize div V.BinX;
  h:=V.CameraYSize div V.BinX;
  SetFrame(0,0,w,h);
  Wait(1);
  except
   on E: Exception do msg('Reset frame error: ' + E.Message);
  end;
end;
{$endif}
end;

Procedure T_ascomcamera.AbortExposure;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
    msg(rsAbortExposur);
    V.AbortExposure;
   except
    on E: Exception do msg('Abort exposure error: ' + E.Message);
   end;
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
 if Connected then begin
   try
   result:=V.BinX;
   except
    result:=1;
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetBinY:integer;
begin
 result:=1;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.BinY;
   except
    result:=1;
   end;
 end;
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
 if Connected then begin
   try
   result.max:=V.MaxBinX;
   except
    result.max:=1;
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetBinYrange:TNumRange;
begin
 result:=UnitRange;
 {$ifdef mswindows}
 if Connected then begin
   try
   result.max:=V.MaxBinY;
   except
    result:=UnitRange;
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetExposureRange:TNumRange;
begin
  result:=NullRange;
  {$ifdef mswindows}
  if Connected then begin
    try
    result.max:=V.ExposureMax;
    result.min:=V.ExposureMin;
    result.step:=V.ExposureResolution;
    except
     result:=NullRange;
    end;
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
 if Connected then begin
   try
   msg(Format(rsSetFilterPos, [inttostr(num)]));
   V.Position:=num-1;
   Wait(1);
   except
    on E: Exception do msg('Set filter error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomcamera.GetFilter:integer;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position+1;
   except
    on E: Exception do msg('Get filter error: ' + E.Message);
   end;
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
 if Connected then begin
   try
   if V.CanSetCCDTemperature then
      result:=V.CCDTemperature
   else
      result:=NullCoord;
   except
     result:=NullCoord;
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.SetTemperature(value:double);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   if V.CanSetCCDTemperature then begin
      SetCooler(true);
      V.SetCCDTemperature:=value;
   end;
   except
    on E: Exception do msg('Set temperature error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomcamera.GetCooler: boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if Connected then begin
   try
     result:=V.CoolerOn;
   except
     result:=false;
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.SetCooler(value:boolean);
begin
{$ifdef mswindows}
if Connected and (V.CoolerOn<>value) then begin
  try
     msg(Format(rsSetCooler, [': '+BoolToStr(value, rsTrue, rsFalse)]));
     V.CoolerOn:=value;
  except
   on E: Exception do msg('Set cooler error: ' + E.Message);
  end;
end;
{$endif}
end;

function T_ascomcamera.GetMaxX: double;
begin
 result:=-1;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.CameraXSize;
  except
     result:=-1;
  end;
end
else result:=-1;
{$endif}
end;

function T_ascomcamera.GetMaxY: double;
begin
 result:=-1;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.CameraYSize;
  except
     result:=-1;
  end;
end
else result:=-1;
{$endif}
end;

function T_ascomcamera.GetMaxADU: double;
begin
 result:=MAXWORD;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.MaxADU;
  except
     result:=MAXWORD;
  end;
end
else result:=MAXWORD;
{$endif}
end;

function T_ascomcamera.GetPixelSize: double;
begin
 result:=-1;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.PixelSizeX;
  except
     result:=-1;
  end;
end
else result:=-1;
{$endif}
end;

function T_ascomcamera.GetPixelSizeX: double;
begin
 result:=-1;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.PixelSizeX;
  except
     result:=-1;
  end;
end
else result:=-1;
{$endif}
end;

function T_ascomcamera.GetPixelSizeY: double;
begin
 result:=-1;
{$ifdef mswindows}
if Connected then begin
  try
     result:=V.PixelSizeY;
  except
     result:=-1;
  end;
end
else result:=-1;
{$endif}
end;

function T_ascomcamera.GetBitperPixel: double;
begin
 result:=-1;
{$ifdef mswindows}
result:=16;
{$endif}
end;

function T_ascomcamera.GetColor: boolean;
begin
 result:=false;
 {$ifdef mswindows}
 if Connected then begin
   try
      result:=(V.SensorType=1);  // Camera produces color image directly, requiring not Bayer decoding
   except
      result:=false;
   end;
 end
 else result:=false;
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
  if Connected then begin
    try
    // check Gain property
       i:=V.Gain;
       try
       // check Gain range
          FGainMin:=V.GainMin;
          FGainMax:=V.GainMax;
          FhasGain:=true;
       except
       // No Gain range
          FhasGain:=false;
       end;
       try
       // Check ISO list
          n:=V.Gains.Count;
          FISOList.Clear;
          for i:=0 to n-1 do begin
            isol:=V.Gains.item[i];
            FISOList.Add(isol);
          end;
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
  {$endif}
end;

procedure T_ascomcamera.SetGain(value: integer);
begin
 {$ifdef mswindows}
 if Connected and (FhasGainISO or FhasGain) then begin
   try
      V.Gain:=value;
   except
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetGain: integer;
begin
 result:=0;
 {$ifdef mswindows}
 if Connected and (FhasGainISO or FhasGain) then begin
   try
      result:=V.Gain;
   except
      result:=0;
   end;
 end
 else result:=0;
 {$endif}
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
 result:=0;
 // todo
end;

function T_ascomcamera.GetVideoGamma:integer;
begin
 result:=0;
 // todo
end;

function T_ascomcamera.GetVideoBrightness:integer;
begin
 result:=0;
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

end.

