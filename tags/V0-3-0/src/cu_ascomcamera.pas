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

interface

uses  cu_camera, u_global, cu_fits, lazutf8sysutils,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
  Forms, ExtCtrls, Classes, SysUtils;

type
T_ascomcamera = class(T_camera)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   Fdevice: string;
   nf: integer;
   FFrametype:TFrameType;
   ExposureTimer: TTimer;
   StatusTimer: TTimer;
   timestart,timeend,timeout,Fexptime:double;
   stCCDtemp : double;
   stX,stY,stWidth,stHeight: integer;
   function Connected: boolean;
   procedure ExposureTimerTimer(sender: TObject);
   procedure StatusTimerTimer(sender: TObject);
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
   procedure Disconnect;  override;
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

constructor T_ascomcamera.Create;
begin
 inherited Create;
 FCameraInterface:=ASCOM;
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
 Fdevice:=cp1;
 V:=Unassigned;
 V:=CreateOleObject(WideString(Fdevice));
 V.connected:=true;
 if V.connected then begin
    FStatus := devConnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
    StatusTimer.Enabled:=true;
 end
 else
    Disconnect;
 except
   on E: EOleException do msg('Error: ' + E.Message);
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
  end;
  except
    on E: EOleException do msg('Error: ' + E.Message);
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
{$endif}
begin
 {$ifdef mswindows}
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
    if V.CanSetCCDTemperature and
       (V.CCDTemperature<>stCCDtemp) then begin
         stCCDtemp:=V.CCDTemperature;
         if Assigned(FonTemperatureChange) then FonTemperatureChange(stCCDtemp);
    end;
    GetFrame(x,y,width,height);
    if (x<>stX)or(y<>stY)or(width<>stWidth)or(height<>stHeight) then begin
       stX:=x;
       stY:=y;
       stWidth:=width;
       stHeight:=height;
       if Assigned(FonFrameChange) then FonFrameChange(self);
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
     V.StartExposure(exptime,li);
     timestart:=NowUTC;
     timeend:=now+(exptime)/secperday;
     timeout:=now+(exptime+10)/secperday;
     Fexptime:=exptime;
     if exptime>=10 then ExposureTimer.Interval:=1000
     else if exptime>=1 then ExposureTimer.Interval:=500
     else if exptime>=0.5 then ExposureTimer.Interval:=100
     else ExposureTimer.Interval:=50;
     ExposureTimer.Enabled:=true;
  except
     on E: EOleException do msg('Error: ' + E.Message);
  end;
end;
{$endif}
end;

procedure T_ascomcamera.ExposureTimerTimer(sender: TObject);
{$ifdef mswindows}
var ok: boolean;
    i,j,c,sz,xs,ys: integer;
    nax1,nax2,pix,piy,state: integer;
    dateobs,ccdname,frname:string;
    img: array of array of LongInt;
    ii: smallint;
    b: char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
    {$endif}
begin
 ExposureTimer.Enabled:=false;
 {$ifdef mswindows}
 try
 if (now<timeout)and(not VarIsEmpty(V)) then begin
    state:=V.CameraState;
    ok:=V.ImageReady;
    if (not ok)and(state>=1)and(state<=4) then begin
      if assigned(FonExposureProgress) then FonExposureProgress(secperday*(timeend-now));
      ExposureTimer.Enabled:=true;
      exit;
    end;
 end
 else ok:=false;

 if ok then begin
   if assigned(FonExposureProgress) then FonExposureProgress(0);
   nax1:=V.NumX;
   nax2:=V.NumY;
   pix:=V.PixelSizeX;
   piy:=V.PixelSizeX;
   ccdname:=V.Name+'-'+V.SensorName;
   frname:=FrameName[ord(FFrametype)];
   dateobs:=FormatDateTime(dateisoshort,timestart);
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
   FImgStream.position:=0;
   hdrmem.Position:=0;
   FImgStream.CopyFrom(hdrmem,hdrmem.Size);
   hdrmem.Free;
   xs:=V.NumX;
   ys:=V.NumY;
   sz:=2*xs*ys;
   SetLength(img,xs,ys);
   img:=V.ImageArray;
   for i:=0 to ys-1 do begin
      for j:=0 to xs-1 do begin
        ii:=img[j,ys-1-i]-32768;
        ii:=NtoBE(ii);
        FImgStream.Write(ii,sizeof(smallint));
      end;
   end;
   b:=' ';
   c:=sz mod 2880;
   for i:=1 to c do FImgStream.Write(b,1);
   NewImage;
 end;
 except
    on E: EOleException do msg('Error: ' + E.Message);
 end;
 {$endif}
end;

Procedure T_ascomcamera.SetBinning(sbinX,sbinY: integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.BinX:=sbinX;
   V.BinY:=sbinY;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

procedure T_ascomcamera.SetFrame(x,y,width,height: integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.StartX:=x;
   V.StartY:=y;
   V.NumX:=width;
   V.NumY:=height;
   except
    on E: EOleException do msg('Error: ' + E.Message);
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
    on E: EOleException do msg('Error: ' + E.Message);
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
    on E: EOleException do msg('Error: ' + E.Message);
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
  w:=V.CameraXSize;
  h:=V.CameraYSize;
  SetBinning(1,1);
  SetFrame(0,0,w,h);
  except
   on E: EOleException do msg('Error: ' + E.Message);
  end;
end;
{$endif}
end;

Procedure T_ascomcamera.AbortExposure;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
    V.AbortExposure;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

Procedure T_ascomcamera.SetActiveDevices(focuser,filters,telescope: string);
begin

end;

procedure T_ascomcamera.msg(txt: string);
begin
 if Assigned(FonMsg) then FonMsg(txt);
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
  FFrametype:=f;
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
   V.Position:=num-1;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function  T_ascomcamera.GetFilter:integer;
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   result:=V.Position+1;
   except
    on E: EOleException do msg('Error: ' + E.Message);
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
  if (value.Count=nf) then begin
    for i:=0 to value.Count-1 do begin
       FFilterNames[i]:=value[i];
    end;
  end;
 {$endif}
end;

function  T_ascomcamera.GetTemperature: double;
begin
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
      V.CoolerOn:=true;
      V.SetCCDTemperature:=value;
   end;
   except
    on E: EOleException do msg('Error: ' + E.Message);
   end;
 end;
 {$endif}
end;

function T_ascomcamera.GetMaxX: double;
begin
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

function T_ascomcamera.GetPixelSize: double;
begin
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
{$ifdef mswindows}
result:=16;
{$endif}
end;

end.

