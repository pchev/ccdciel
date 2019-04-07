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
  u_translation, Forms, ExtCtrls, Classes, SysUtils;

type

  T_autoguider_dither = class(T_autoguider)
  private
    StatusTimer: TTimer;
    procedure StatusTimerTimer(sender: TObject);
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
    procedure Dither(pixel:double; raonly:boolean; waittime:double); override;
    function WaitBusy(maxwait:integer=5):boolean; override;
    function WaitGuiding(maxwait:integer=5):boolean; override;
    function WaitDithering(maxwait:integer=5):boolean; override;
  end;

const statusinterval=5000;


implementation


Constructor T_autoguider_dither.Create ;
begin
  inherited Create;
  FAutoguiderType:=agDITHER;
  FStatus:='Dither only';
  FState:=GUIDER_DISCONNECTED;
  FRunning:=true;
  StatusTimer:=TTimer.Create(nil);
  StatusTimer.Interval:=statusinterval;
  StatusTimer.OnTimer:=@StatusTimerTimer;
  StatusTimer.Enabled:=true;
end;

Destructor T_autoguider_dither.Destroy;
begin
  StatusTimer.Free;
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
end;

procedure T_autoguider_dither.Pause(onoff:boolean);
begin
end;

procedure T_autoguider_dither.StatusTimerTimer(sender: TObject);
var newstate: TAutoguiderState;
begin
try
  StatusTimer.Enabled:=false;
  newstate:=FState;
  if (FMount=nil)or(FMount.Status<>devConnected) then begin
     newstate:=GUIDER_DISCONNECTED;
  end
  else if (FMount.Tracking) then begin
     newstate:=GUIDER_GUIDING;
  end
  else begin
     newstate:=GUIDER_IDLE;
  end;
  if newstate<>FState then begin
     FState:=newstate;
     if assigned(FonStatusChange) then FonStatusChange(self);
  end;
finally
  StatusTimer.Enabled:=true;
end;
end;

procedure T_autoguider_dither.Dither(pixel:double; raonly:boolean; waittime:double);
var speed,dist,timeend: double;
    direction,durationra,durationdec:integer;
begin
  // Here the pixel parameter represent the mean move in arc-seconds.
  if (FMount<>nil)and(FMount.Status=devConnected)and(FMount.Tracking) then begin
     // set as mean value, need max value
     pixel:=2*pixel;
     // Try to set 1x sidereal rate
     FMount.GuideRateRa:=siderealrate/3600;
     // RA Move
     speed:=abs(FMount.GuideRateRa)*3600;  // arcsec/sec
     if speed>0 then begin
       dist:=(Random(round(2*pixel*1000))-round(pixel*1000))/1000; // random in +/- pixel arcsec
       if dist>0 then direction:=2  // east
                 else direction:=3; // west
       durationra:=max(50,round(1000*abs(dist)/speed));  // millisecond
       Fmount.PulseGuide(direction,durationra);
     end
     else
       DisplayMessage('Cannot get mount RA guide rate');
     if not raonly then begin
        // Try to set 1x sidereal rate
        FMount.GuideRateDe:=siderealrate/3600;
        // DEC Move
        speed:=abs(FMount.GuideRateDe)*3600;  // arcsec/sec
        if speed>0 then begin
          dist:=(Random(round(2*pixel*1000))-round(pixel*1000))/1000; // random in +/- pixel arcsec
          if dist>0 then direction:=0  // north
                    else direction:=1; // south
          durationdec:=max(50,round(1000*abs(dist)/speed));  // millisecond
          Fmount.PulseGuide(direction,durationdec);
        end
        else
          DisplayMessage('Cannot get mount DEC guide rate');
     end;
     // wait for move completed
     timeend:=now+((max(durationra,durationdec)+10)/secperday);
     while now<timeend do begin
       sleep(500);
       if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
       if not FMount.PulseGuiding then break;
     end;
     // wait more to stabilize
     Wait(waittime);
  end
  else
     DisplayMessage('Mount not ready for pulse guiding');
end;

procedure T_autoguider_dither.StarLostTimerTimer(Sender: TObject);
begin
end;

end.

