unit u_global;

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

uses u_ccdconfig,
  Classes, SysUtils;

type
  TNotifyMsg = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TDeviceStatus = (devDisconnected, devConnecting, devConnected);

  TDevInterface = (INDI, ASCOM, INCAMERA, INTELESCOPE);
  TFrameType =(LIGHT, BIAS, DARK, FLAT);

  TNumRange = record
               min,max,step: double;
              end;

  TTarget = Class(TObject)
              objectname, plan: string;
              starttime,endtime,ra,de: double;
            end;

  TPlan   = Class(TObject)
              exposure, delay: double;
              count, repeatcount: integer;
              filter: integer;
              binx,biny: integer;
              frtype: TFrameType;
              description: string;
            end;

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
  {$ifdef mswindows}
  defCapturePath='C:\';
  {$else}
  defCapturePath='/tmp';
  {$endif}
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord=-9999;
  dateiso = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz';
  dateisoshort = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';
  f0 = '0';
  f1 = '0.0';
  f2 = '0.00';
  b80 ='                                                                                ';
  FrameName: array[0..ord(high(TFrameType))] of string =('Light   ','Bias    ','Dark    ','Flat    ');

var
  ConfigDir,LogDir,TmpDir: UTF8String;
  config: TCCDConfig;
  compile_time, compile_version, compile_system, lclver: string;

implementation

end.

