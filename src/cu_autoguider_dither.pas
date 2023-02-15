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
    Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False); override;
    procedure Disconnect; override;
    procedure Shutdown; override;
    procedure ConnectGear; override;
    procedure SettleTolerance(pixel:double; mintime,maxtime: integer); override;
    procedure Calibrate; override;
    procedure Guide(onoff:boolean; recalibrate:boolean=false); override;
    procedure Pause(onoff:boolean; settle:boolean=true); override;
    procedure Dither(pixel:double; raonly:boolean; waittime:double); override;
    procedure SetLockPosition(x,y: double); override;
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
  FStatus:=rsDitherOnly;
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

Procedure T_autoguider_dither.Connect(cp1: string; cp2:string=''; cp3:string=''; cb1:boolean=False);
begin
  start;
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

procedure T_autoguider_dither.SetLockPosition(x,y:double);
begin
end;

procedure T_autoguider_dither.Pause(onoff:boolean; settle:boolean=true);
begin
end;

procedure T_autoguider_dither.StatusTimerTimer(sender: TObject);
var newstate: TAutoguiderState;
    restarttimer: boolean;
begin
try
  restarttimer:=true;
  StatusTimer.Enabled:=false;
  newstate:=FState;
  if (FMount=nil)or(FMount.Status<>devConnected) then begin
     newstate:=GUIDER_DISCONNECTED;
  end
  else
   if FMount.CanPulseGuide then begin
      if (FMount.Tracking) then begin
         newstate:=GUIDER_GUIDING;
      end
      else begin
         newstate:=GUIDER_IDLE;
      end;
    end
    else begin
       newstate:=GUIDER_ALERT;
       restarttimer:=false;
       DisplayMessage('Mount does not support pulse guiding');
   end;
  if newstate<>FState then begin
     FState:=newstate;
     if assigned(FonStatusChange) then FonStatusChange(self);
  end;
finally
  StatusTimer.Enabled:=restarttimer;
end;
end;

procedure T_autoguider_dither.Dither(pixel:double; raonly:boolean; waittime:double);
var duration,timeend,maxduration,tde,cosde: double;
    direction,durationra,durationdec:integer;
begin
if (FMount<>nil)and(FMount.CanPulseGuide) then begin
  // Here the pixel parameter represent the mean pulse duration.
  if (FMount<>nil)and(FMount.Status=devConnected)and(FMount.Tracking) then begin
     // set as mean value, need max value
     maxduration:=2*pixel;
     // RA move
     tde:=FMount.Dec;
     if abs(tde)<90 then
        cosde:=cos(deg2rad*tde)
     else
        cosde:=1;
     duration:=(Random(round(2*maxduration*1000))-round(maxduration*1000))/1000; // random in +/- maxduration
     if duration>0 then direction:=2  // east
                   else direction:=3; // west
     if cosde<>0 then
        duration:=duration/cosde;
     durationra:=min(30000,max(50,round(1000*abs(duration))));  // millisecond, maximum 30 seconds
     Fmount.PulseGuide(direction,durationra);
     if not raonly then begin
        // DEC move
        duration:=(Random(round(2*maxduration*1000))-round(maxduration*1000))/1000; // random in +/- maxduration
        if duration>0 then direction:=0  // north
                      else direction:=1; // south
        durationdec:=min(30000,max(50,round(1000*abs(duration))));  // millisecond
        Fmount.PulseGuide(direction,durationdec);
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
end;

procedure T_autoguider_dither.StarLostTimerTimer(Sender: TObject);
begin
end;

end.

