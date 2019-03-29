unit cu_autoguider_dither;

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

uses cu_autoguider, u_global, indiapi, u_utils, math,
  u_translation, Forms, Classes, SysUtils;

type

  T_autoguider_dither = class(T_autoguider)
  protected
    Procedure ProcessEvent(txt:string); override;
    procedure Execute; override;
    procedure StarLostTimerTimer(Sender: TObject); override;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure Connect(cp1: string; cp2:string=''); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean); override;
    procedure Dither(pixel:double; raonly:boolean); override;
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
  end;

implementation


Constructor T_autoguider_dither.Create ;
begin
  inherited Create;
  FAutoguiderType:=agDITHER;
  FStatus:='Dither only';
  FState:=GUIDER_IDLE;
  FRunning:=true;
end;

Destructor T_autoguider_dither.Destroy;
begin
  inherited Destroy;
end;

Procedure T_autoguider_dither.Connect(cp1: string; cp2:string='');
begin

end;

procedure T_autoguider_dither.Disconnect;
begin
  start;
end;

procedure T_autoguider_dither.Execute;
begin
  // nothing to do, just free itself
end;

Procedure T_autoguider_dither.ProcessEvent(txt:string);
begin
end;

procedure T_autoguider_dither.ConnectGear;
begin
end;

procedure T_autoguider_dither.Shutdown;
begin
end;

procedure T_autoguider_dither.SettleTolerance(pixel:double; mintime,maxtime: integer);
begin
end;

function T_autoguider_dither.WaitBusy(maxwait:integer=5):boolean;
begin
  result:=true;
end;

function T_autoguider_dither.WaitGuiding(maxwait:integer=5):boolean;
begin
  result:=true;
end;

function T_autoguider_dither.WaitDithering(maxwait:integer=5):boolean;
begin
  result:=true;
end;

procedure T_autoguider_dither.Calibrate;
begin
end;

procedure T_autoguider_dither.Guide(onoff:boolean; recalibrate:boolean=false);
begin
  if onoff then FState:=GUIDER_GUIDING
           else FState:=GUIDER_IDLE;
end;

procedure T_autoguider_dither.Pause(onoff:boolean);
begin
end;

procedure T_autoguider_dither.Dither(pixel:double; raonly:boolean);
var speed,dist,timeend: double;
    direction,duration:integer;
begin
  // Here the pixel parameter represent the maximum move in arc-seconds.
  if (FMount<>nil)and(FMount.Status=devConnected)and(FMount.Tracking) then begin
     // RA Move
     speed:=abs(FMount.GuideRateRa)*3600;  // arcsec/sec
     if speed>0 then begin
       dist:=(Random(round(2*pixel*1000))-round(pixel*1000))/1000; // random in +/- pixel arcsec
       if dist>0 then direction:=2  // east
                 else direction:=3; // west
       duration:=max(50,round(1000*abs(dist)/speed));  // millisecond
       Fmount.PulseGuide(direction,duration);
     end;
     if not raonly then begin
        // DEC Move
        speed:=abs(FMount.GuideRateDe)*3600;  // arcsec/sec
        if speed>0 then begin
          dist:=(Random(round(2*pixel*1000))-round(pixel*1000))/1000; // random in +/- pixel arcsec
          if dist>0 then direction:=0  // north
                    else direction:=1; // south
          duration:=max(50,round(1000*abs(dist)/speed));  // millisecond
          Fmount.PulseGuide(direction,duration);
        end;
     end;
     // wait for move completed
     timeend:=now+30/secperday;
     while now<timeend do begin
       sleep(500);
       if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
       if not FMount.PulseGuiding then break;
     end;
     // wait 5 second more to stabilize
     Wait(5);
  end;
end;

procedure T_autoguider_dither.StarLostTimerTimer(Sender: TObject);
begin
end;

end.

