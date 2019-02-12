unit cu_ascomrestcamera;

{$mode objfpc}{$H+}

//{$define debug_ascom}

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
   timedout,Fexptime:double;
   FPixelSizeX,FPixelSizeY: double;
   Fccdname: string;
   FCameraXSize,FCameraYSize,FMaxBinX,FMaxBinY,FBinX,FBinY:integer;
   FHasTemperature, FCanSetTemperature: boolean;
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
   procedure SetReadOutMode(value: integer); override;
   function GetReadOutMode: integer; override;

public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
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
    i,n: integer;
begin
 try
  FStatus := devConnecting;
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  V.Device:=Fdevice;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V.Timeout:=2000;
  V.Put('connected',true); // try to connect if authorized by server
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
    FStatus := devConnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
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
 finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
 end;
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
     {$ifdef debug_ascom}msg('start exposure.');{$endif}
     V.Put('startexposure',['Duration',formatfloat(f4,exptime),'Light',li]);
     Ftimestart:=NowUTC;
     Ftimeend:=now+(exptime)/secperday;
     timedout:=now+(exptime+CameraTimeout)/secperday;
     Fexptime:=exptime;
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
    pix,piy: double;
    dateobs,ccdname,frname:string;
    Dims,x,y: Integer;
    lii: integer;
    ii: smallint;
    b: array[0..2880]of char;
    hdr: TFitsHeader;
    hdrmem: TMemoryStream;
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
    {$ifdef debug_ascom}msg(' status:'+inttostr(state)+', image ready:'+BoolToStr(ok, rsTrue, rsFalse));{$endif}
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
   {$ifdef debug_ascom}msg('clear old image.');{$endif}
   FFits.ClearImage;
   if assigned(FonExposureProgress) then FonExposureProgress(-10);
   if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   {$ifdef debug_ascom}msg('read image.');{$endif}
   try
   imgarray:=V.GetImageArray;
   except
     on E: Exception do begin
       msg('Error accessing ImageArray: ' + E.Message,0);
       if assigned(FonAbortExposure) then FonAbortExposure(self);
       exit;
     end;
   end;
   Dims:=imgarray.nplane;
   if (Dims<2)or(Dims>3) then begin
     msg('Error ImageArray unsupported Dimension=' + inttostr(Dims));
     if assigned(FonAbortExposure) then FonAbortExposure(self);
     exit;
   end;
   xs:=imgarray.width;
   ys:=imgarray.height;
   {$ifdef debug_ascom}msg('width:'+inttostr(xs)+' height:'+inttostr(ys));{$endif}
   nax1:=xs;
   nax2:=ys;
   pix:=FPixelSizeX;
   piy:=FPixelSizeY;
   ccdname:=Fccdname;
   frname:=FrameName[ord(FFrametype)];
   dateobs:=FormatDateTime(dateisoshort,Ftimestart);
   {$ifdef debug_ascom}msg('set fits header');{$endif}
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
   if Dims=2 then begin
     for i:=0 to ys-1 do begin
        if FASCOMFlipImage then
           y:=ys-1-i
        else
           y:=i;
        for j:=0 to xs-1 do begin
          x:=j;
          lii:=imgarray.img[0,y,x];
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
   {$ifdef debug_ascom}msg('pad fits');{$endif}
   b:='';
   c:=2880-(FImgStream.Size mod 2880);
   FillChar(b,c,0);
   FImgStream.Write(b,c);
   {$ifdef debug_ascom}msg('release imagearray');{$endif}
   imgarray.Free;
   {$ifdef debug_ascom}msg('display image');{$endif}
   if assigned(FonExposureProgress) then FonExposureProgress(-11);
   if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
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
   {$ifdef debug_ascom}msg('Request binning '+inttostr(sbinX)+','+inttostr(sbinY));{$endif}
   oldx:=FBinX;
   oldy:=FBinY;
   {$ifdef debug_ascom}msg('Old binning '+inttostr(oldx)+','+inttostr(oldy));{$endif}
   if (oldx<>sbinX)or(oldy<>sbinY) then begin
     msg(Format(rsSetBinningX, [inttostr(sbinX), inttostr(sbinY)]));
     GetFrame(fsx,fsy,fnx,fny);
     {$ifdef debug_ascom}msg('Current frame '+inttostr(fsx)+','+inttostr(fsy)+'/'+inttostr(fnx)+'x'+inttostr(fny));{$endif}
     scale:=oldx/sbinX;
     fsx:=trunc(fsx*scale);
     fnx:=trunc(fnx*scale);
     scale:=oldy/sbinY;
     fsy:=trunc(fsy*scale);
     fny:=trunc(fny*scale);
     newx:=FCameraXSize div sbinX;
     newy:=FCameraYSize div sbinY;
     V.Put('binX',sbinX);
     V.Put('binY',sbinY);
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
   {$ifdef debug_ascom}msg('Request frame '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height));{$endif}
   w:=FCameraXSize;
   h:=FCameraYSize;
   bx:=FBinX;
   by:=FBinY;
   {$ifdef debug_ascom}
     msg('XSize='+inttostr(w)+' YSize='+inttostr(h)+' BinX='+inttostr(bx)+' BinY='+inttostr(by));
   {$endif}
   Xmax:= w div bx;
   Ymax:= h div by;
   {$ifdef debug_ascom}
     msg('Xmax='+inttostr(Xmax)+' Ymax='+inttostr(Ymax));
   {$endif}
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
   {$ifdef debug_ascom}msg('Set frame '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height));{$endif}
   V.Put('startx',x);
   V.Put('starty',y);
   V.Put('numx',width);
   V.Put('numy',height);
   Wait(1);
   if Assigned(FonFrameChange) then FonFrameChange(self);
   except
    on E: Exception do msg('Set frame error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestcamera.GetFrame(out x,y,width,height: integer);
var Cx,Cy,Cwidth,Cheight: integer;
begin
   if FStatus<>devConnected then exit;
   try
   x      := V.Get('startx').AsInt;
   y      := V.Get('starty').AsInt;
   width  := V.Get('numx').AsInt;
   height := V.Get('numy').AsInt;
   // Consistency check for buggy qhy drivers
   Cx:=max(x,0);
   Cy:=max(y,0);
   Cwidth:=min(width,FCameraXSize div FBinX);
   Cheight:=min(height,FCameraYSize div FBinY);
   if (Cx<>x)or(Cy<>y)or(Cwidth<>width)or(Cheight<>height) then  begin
     msg('Correct driver wrong frame size: '+inttostr(x)+','+inttostr(y)+'/'+inttostr(width)+'x'+inttostr(height),1);
     msg('Set new value : '+inttostr(Cx)+','+inttostr(Cy)+'/'+inttostr(Cwidth)+'x'+inttostr(Cheight),1);
     V.Put('startx',Cx);
     V.Put('starty',Cy);
     V.Put('numx',Cwidth);
     V.Put('numy',Cheight);
     x:=Cx;
     y:=Cy;
     width:=Cwidth;
     height:=Cheight;
   end;
   //
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
   {$ifdef debug_ascom}msg('Get frame range :'+inttostr(round(widthr.max))+'x'+inttostr(round(heightr.max)));{$endif}
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
  {$ifdef debug_ascom}
    msg('ResetFrame: XSize='+inttostr(w)+' YSize='+inttostr(h)+' BinX='+inttostr(bx)+' BinY='+inttostr(by));
  {$endif}
  w:=w div bx;
  h:=h div by;
  SetFrame(0,0,w,h);
  Wait(1);
  except
   on E: Exception do msg('Reset frame error: ' + E.Message,0);
  end;
end;

Procedure T_ascomrestcamera.AbortExposure;
begin
   if FStatus<>devConnected then exit;
   try
    msg(rsAbortExposur);
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
   V.Put('position',num-1);
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
      V.Put('setccdtemperature',value);
   end;
   except
    on E: Exception do msg('Set temperature error: ' + E.Message,0);
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
     V.Put('cooleron',value);
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
 if (FhasGainISO or FhasGain) then begin
   try
      V.Put('gain',value);
   except
   end;
 end;
end;

function T_ascomrestcamera.GetGain: integer;
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

procedure T_ascomrestcamera.SetReadOutMode(value: integer);
begin
 if FStatus<>devConnected then exit;
 try
 if FhasReadOut then begin
   if FhasFastReadout then begin
     if value=0 then V.Put('fastreadout',false)
                else V.Put('fastreadout',true);
   end
   else begin
     V.Put('readoutmode',value);
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


end.

