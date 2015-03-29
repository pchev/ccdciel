unit u_global;

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

uses u_ccdconfig, dynlibs,
  Classes, SysUtils;

type
  TNotifyMsg = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TDeviceStatus = (devDisconnected, devConnecting, devConnected);

  TDevInterface = (INDI, ASCOM, INCAMERA, INTELESCOPE);
  TFrameType =(LIGHT, BIAS, DARK, FLAT);
  TAutoguiderType=(PHD);
  TAutoguiderState=(GUIDER_DISCONNECTED,GUIDER_IDLE,GUIDER_GUIDING,GUIDER_BUSY,GUIDER_ALERT);
  TPlanetariumType=(CDC, SAMP);

  TNumRange = record
               min,max,step: double;
              end;

  TTarget = Class(TObject)
              public
              objectname, plan: string;
              starttime,endtime,ra,de: double;
              repeatcount: integer;
              preview: boolean;
              delay, previewexposure: double;
              constructor Create;
              procedure Assign(Source: TTarget);
              function previewexposure_str: string;
              function delay_str: string;
              function repeatcount_str: string;
            end;

  TPlan   = Class(TObject)
              public
              exposure, delay, previewexposure: double;
              count, repeatcount: integer;
              preview: boolean;
              filter: integer;
              binx,biny: integer;
              frtype: TFrameType;
              description: string;
              constructor Create;
              function exposure_str: string;
              function previewexposure_str: string;
              function delay_str: string;
              function count_str: string;
              function repeatcount_str: string;
              function filter_str: string;
              function binning_str: string;
              function frtype_str: string;
              function description_str: string;
            end;

  // libcdcwcs
 type
    TcdcWCScoord = record
      ra, dec, x, y: double;
      n: integer;
    end;
    PcdcWCScoord = ^TcdcWCScoord;

    TcdcWCSinfo = record
      cra, cdec, dra, ddec, secpix, eqout, rot: double;
      wp, hp, sysout: integer;
    end;
    PcdcWCSinfo = ^TcdcWCSinfo;
    Tcdcwcs_initfitsfile = function(fn: PChar; wcsnum:integer): integer; cdecl;
    Tcdcwcs_release = function(wcsnum:integer): integer; cdecl;
    Tcdcwcs_sky2xy = function(p: PcdcWCScoord; wcsnum:integer): integer; cdecl;
    Tcdcwcs_xy2sky = function(p: PcdcWCScoord; wcsnum:integer): integer; cdecl;
    Tcdcwcs_getinfo = function(p: PcdcWCSinfo; wcsnum:integer): integer; cdecl;

  var
    cdcwcslib: TLibHandle;
    cdcwcs_initfitsfile: Tcdcwcs_initfitsfile;
    cdcwcs_release: Tcdcwcs_release;
    cdcwcs_getinfo: Tcdcwcs_getinfo;
    cdcwcs_sky2xy: Tcdcwcs_sky2xy;
    cdcwcs_xy2sky: Tcdcwcs_xy2sky;

  const
    maxfitslist=15;  // must corespond to value in cdcwcs.c

  {$i revision.inc}

const
  ccdciel_version='Version Alpha 0.0.1';
  ccdcielver = '0.0.1a';
  blank=' ';
  clOrange=$1080EF;
  clDarkBlue=$300D0E;
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  secperday = 3600*24;
  pi2 = 2 * pi;
  rad2deg=180/pi;
  deg2rad=pi/180;
  jd2000 = 2451545.0;
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord=-9999;
  Filter0 = 'No change';
  dateiso = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';
  dateisoshort = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  f0 = '0';
  f1 = '0.0';
  f2 = '0.00';
  b80 ='                                                                                ';
  FrameName: array[0..ord(high(TFrameType))] of string =('Light   ','Bias    ','Dark    ','Flat    ');
  {$ifdef linux}
    defCapturePath='/tmp';
    libcdcwcs = 'libcdcwcs.so';
    libz = 'libz.so.1';
  {$endif}
  {$ifdef darwin}
    defCapturePath='/tmp';
    libcdcwcs = 'libcdcwcs.dylib';
    libz = 'libz.dylib';
  {$endif}
  {$ifdef mswindows}
    defCapturePath='C:\';
    libcdcwcs = 'libcdcwcs.dll';
    libz = 'zlib1.dll';
  {$endif}

var
  ConfigDir,LogDir,TmpDir: UTF8String;
  config: TCCDConfig;
  Filters: TStringList;
  compile_time, compile_version, compile_system, lclver: string;
  DitherPixel, SettlePixel: double;
  DitherRAonly: boolean;
  SettleMinTime, SettleMaxTime: integer;

implementation

////////////////////  TTarget  /////////////////////////////

constructor TTarget.Create;
begin
  inherited Create;
  objectname:='None';
  plan:='';
  starttime:=0.0;
  endtime:=23.99999;
  ra:=NullCoord;
  de:=NullCoord;
  repeatcount:=1;
  preview:=False;
  delay:=1;
  previewexposure:=1;
end;

procedure TTarget.Assign(Source: TTarget);
begin
  objectname:=Source.objectname;
  plan:=Source.plan;
  starttime:=Source.starttime;
  endtime:=Source.endtime;
  ra:=Source.ra;
  de:=Source.de;
  repeatcount:=Source.repeatcount;
  preview:=Source.preview;
  delay:=Source.delay;
  previewexposure:=Source.previewexposure;
end;

function TTarget.previewexposure_str: string;
begin
 Result:=FloatToStr(previewexposure);
end;

function TTarget.delay_str: string;
begin
  Result:=FloatToStr(delay);
end;

function TTarget.repeatcount_str: string;
begin
  Result:=IntToStr(repeatcount);
end;

////////////////////  TPlan  /////////////////////////////

constructor TPlan.Create;
begin
  exposure:=1;
  delay:=1;
  previewexposure:=1;
  count:=1;
  repeatcount:=1;
  preview:=false;
  filter:=0;
  binx:=1;
  biny:=1;
  frtype:=LIGHT;
  description:='Step description';
end;

function TPlan.exposure_str: string;
begin
 Result:=FloatToStr(exposure);
end;

function TPlan.previewexposure_str: string;
begin
 Result:=FloatToStr(previewexposure);
end;

function TPlan.delay_str: string;
begin
  Result:=FloatToStr(delay);
end;

function TPlan.count_str: string;
begin
  Result:=IntToStr(count);
end;

function TPlan.repeatcount_str: string;
begin
  Result:=IntToStr(repeatcount);
end;

function TPlan.filter_str: string;
begin
  if Filters.Count=0 then
    Result:=''
  else
    Result:=Filters[filter];
end;

function TPlan.binning_str: string;
begin
  Result:=IntToStr(binx)+'x'+IntToStr(biny);
end;

function TPlan.frtype_str: string;
begin
  Result:=FrameName[ord(frtype)];
end;

function TPlan.description_str: string;
begin
  Result:=description;
end;


end.

