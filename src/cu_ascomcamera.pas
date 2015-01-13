unit cu_ascomcamera;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses  u_global, cu_fits, lazutf8sysutils,
  {$ifdef mswindows}
    Variants, comobj,
  {$endif}
  Forms, ExtCtrls, Classes, SysUtils;

type
T_ascomcamera = class(TObject)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   Fdevice: string;
   FImgStream: TMemoryStream;
   FStatus: TDeviceStatus;
   FWheelStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonExposureProgress: TNotifyNum;
   FonTemperatureChange: TNotifyNum;
   FonFrameChange: TNotifyEvent;
   FonFilterChange: TNotifyNum;
   FonStatusChange: TNotifyEvent;
   FonWheelStatusChange: TNotifyEvent;
   FonFilterNameChange: TNotifyEvent;
   FonNewImage: TNotifyEvent;
   FFilterNames: TStringList;
   nf: integer;
   FFrametype:TFrameType;
   ExposureTimer: TTimer;
   StatusTimer: TTimer;
   timestart,timeend,timeout,Fexptime:double;
   stCCDtemp : double;
   stX,stY,stWidth,stHeight: integer;
   procedure msg(txt: string);
   function GetBinX:integer;
   function GetBinY:integer;
   procedure SetFrametype(f:TFrameType);
   function  GetFrametype:TFrameType;
   function GetBinXrange:TNumRange;
   function GetBinYrange:TNumRange;
   function GetExposureRange:TNumRange;
   function GetTemperatureRange:TNumRange;
   procedure SetFilter(num:integer);
   function  GetFilter:integer;
   procedure SetFilterNames(value:TStringList);
   function  GetTemperature: double;
   procedure SetTemperature(value:double);
   function Connected: boolean;
   procedure ExposureTimerTimer(sender: TObject);
   procedure StatusTimerTimer(sender: TObject);
   function GetMaxX: double;
   function GetMaxY: double;
   function GetPixelSize: double;
   function GetPixelSizeX: double;
   function GetPixelSizeY: double;
   function GetBitperPixel: double;
 public
   constructor Create;
   destructor  Destroy; override;
   procedure Connect;
   procedure Disconnect;
   Procedure StartExposure(exptime: double);
   Procedure SetBinning(binX,binY: integer);
   procedure SetFrame(x,y,width,height: integer);
   procedure GetFrame(out x,y,width,height: integer);
   procedure GetFrameRange(out xr,yr,widthr,heightr: TNumRange);
   procedure ResetFrame;
   Procedure AbortExposure;
   Procedure SetActiveDevices(focuser,filters,telescope: string);
   property Device: string read Fdevice write Fdevice;
   property Status: TDeviceStatus read FStatus;
   property ImgStream: TMemoryStream read FImgStream;
   property Temperature: double read GetTemperature write SetTemperature;
   property BinX: Integer read getBinX;
   property BinY: Integer read getBinY;
   property FrameType: TFrameType read GetFrametype write SetFrametype;
   property BinXrange: TNumRange read GetbinXrange;
   property BinYrange: TNumRange read GetbinYrange;
   property ExposureRange: TNumRange read GetExposureRange;
   property TemperatureRange: TNumRange read GetTemperatureRange;
   property Filter: integer read GetFilter write SetFilter;
   property FilterNames: TStringList read FFilterNames write SetFilterNames;
   property MaxX: double read GetMaxX;
   property MaxY: double read GetMaxY;
   property PixelSize: double read GetPixelSize;
   property PixelSizeX: double read GetPixelSizeX;
   property PixelSizeY: double read GetPixelSizeY;
   property BitperPixel: double read GetBitperPixel;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onFrameChange: TNotifyEvent read FonFrameChange write FonFrameChange;
   property onExposureProgress: TNotifyNum read FonExposureProgress write FonExposureProgress;
   property onTemperatureChange: TNotifyNum read FonTemperatureChange write FonTemperatureChange;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
   property onWheelStatusChange: TNotifyEvent read FonWheelStatusChange write FonWheelStatusChange;
   property onFilterNameChange: TNotifyEvent read FonFilterNameChange write FonFilterNameChange;
   property onNewImage: TNotifyEvent read FonNewImage write FonNewImage;
   property onFilterChange: TNotifyNum read FonFilterChange write FonFilterChange;
end;


implementation

constructor T_ascomcamera.Create;
begin
 inherited Create;
 FStatus := devDisconnected;
 FFilterNames:=TStringList.Create;
 FImgStream:=TMemoryStream.Create;
 ExposureTimer:=TTimer.Create(nil);
 ExposureTimer.Enabled:=false;
 ExposureTimer.Interval:=10;
 ExposureTimer.OnTimer:=@ExposureTimerTimer;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=1000;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomcamera.Destroy;
begin
 FImgStream.Free;
 ExposureTimer.Free;
 StatusTimer.Free;
 FFilterNames.Free;
 inherited Destroy;
end;

procedure T_ascomcamera.Connect;
begin
{$ifdef mswindows}
 try
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
var x,y,width,height: integer;
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
    nax1,nax2,pix,piy: integer;
    dateobs,ccdname,framename:string;
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
 c:=0;
 repeat
    ok:=V.ImageReady;
    if ok then break;
    if assigned(FonExposureProgress)and((c mod 10)=0) then FonExposureProgress(secperday*(timeend-now));
    Sleep(100);
    Application.ProcessMessages;
    inc(c);
 until now>timeout;
 if ok then begin
   nax1:=V.NumX;
   nax2:=V.NumY;
   pix:=V.PixelSizeX;
   piy:=V.PixelSizeX;
   ccdname:=V.Name+'-'+V.SensorName;
   case FFrametype of
      LIGHT: framename:='Light';
      BIAS : framename:='Bias';
      DARK : framename:='Dark';
      FLAT : framename:='Flat';
   end;
   dateobs:=''''+FormatDateTime(dateiso,timestart)+'''';
   hdr:=TFitsHeader.Create;
   hdr.ClearHeader;
   hdr.Add('SIMPLE','T','file does conform to FITS standard');
   hdr.Add('BITPIX',16,'number of bits per data pixel');
   hdr.Add('NAXIS',2,'number of data axes');
   hdr.Add('NAXIS1',nax1 ,'length of data axis 1');
   hdr.Add('NAXIS2',nax2 ,'length of data axis 2');
   hdr.Add('EXTEND','F','no extensions');
   hdr.Add('COMMENT','FITS (Flexible Image Transport System) format is defined in ''Astronomy','');
   hdr.Add('COMMENT','and Astrophysics'', volume 376, page 359; bibcode: 2001A&A...376..359H','');
   hdr.Add('BZERO',32768,'offset data range to that of unsigned short');
   hdr.Add('BSCALE',1,'default scaling factor');
   hdr.Add('EXPTIME',Fexptime,'Total Exposure Time (s)');
   hdr.Add('PIXSIZE1',pix ,'Pixel Size 1 (microns)');
   hdr.Add('PIXSIZE2',piy ,'Pixel Size 2 (microns)');
   hdr.Add('XBINNING',BinX ,'Binning factor in width');
   hdr.Add('YBINNING',BinY ,'Binning factor in height');
   hdr.Add('FRAME',framename,'Frame Type');
//   hdr.Add('FILTER','red','Filter');
//  hdr.Add('DATAMIN','0','Minimum value');
//   hdr.Add('DATAMAX','2000','Maximum value');
//   hdr.Add('OBJCTRA','0','Object RA');
//   hdr.Add('OBJCTDEC','0','Object DEC');
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
   if Assigned(FonNewImage) then FonNewImage(self);
 end;
 except
    on E: EOleException do msg('Error: ' + E.Message);
 end;
 {$endif}
end;

Procedure T_ascomcamera.SetBinning(binX,binY: integer);
begin
 {$ifdef mswindows}
 if Connected then begin
   try
   V.BinX:=binX;
   V.BinY:=binY;
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
var i:integer;
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
      result:=0;
   except
    on E: EOleException do msg('Error: ' + E.Message);
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

