unit cu_ascomdome;

{$mode objfpc}{$H+}

{
Copyright (C) 2018 Patrick Chevalley

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

uses cu_dome, u_global,
    {$ifdef mswindows}
    u_translation, indiapi, Variants, comobj, math,
    {$endif}
    Forms, ExtCtrls,Classes, SysUtils;
type
T_ascomdome = class(T_dome)
 private
   {$ifdef mswindows}
   V: variant;
   {$endif}
   stShutter,stSlave,stPark: boolean;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;

   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   function WaitDomePark(maxtime:integer):boolean;
   function WaitShutter(onoff:boolean; maxtime:integer):boolean;
   procedure SetTimeout(num:integer); override;
   function GetPark: boolean; override;
   procedure SetPark(value:boolean); override;
   function GetShutter: boolean; override;
   procedure SetShutter(value:boolean); override;
   function GetSlave: boolean; override;
   procedure SetSlave(value:boolean); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   function GetV: variant;
end;

const statusinterval=1000;
      waitpoll=500;

implementation

constructor T_ascomdome.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDomeInterface:=ASCOM;
 FInterfaceVersion:=1;
 stShutter:=false;
 stSlave:=false;
 stPark:=false;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomdome.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomdome.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
 {$ifdef mswindows}
  try
  FStatus := devConnecting;
  Fdevice:=cp1;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V:=Unassigned;
  V:=CreateOleObject(Fdevice);
  try
  FInterfaceVersion:=V.InterfaceVersion;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=3 then begin
    V.Connect;
    WaitConnecting(30000);
  end
  else
    V.Connected:=true;
  if Connected then begin
     try
     msg(V.DriverInfo,9);
     except
     end;
     try
     msg('Driver version: '+V.DriverVersion,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FhasPark:=V.CanPark;
     except
       FhasPark:=false;
     end;
     try
     FhasSlaving:=V.CanSlave;
     except
       FhasSlaving:=false;
     end;
     try
     FhasShutter:=V.CanSetShutter;
     except
       FhasShutter:=false;
     end;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do msg('Connection error: ' + E.Message,0);
  end;
 {$endif}
end;

procedure T_ascomdome.Disconnect;
begin
 {$ifdef mswindows}
   StatusTimer.Enabled:=false;
   try
   if not VarIsEmpty(V) then begin
     if FInterfaceVersion>=3 then begin
       V.Disconnect;
       WaitConnecting(30000);
     end
     else
       V.Connected:=false;
     V:=Unassigned;
   end;
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
 {$endif}
end;

function T_ascomdome.GetV: variant;
begin
 {$ifdef mswindows}
 result:=V;
 {$endif}
end;

function T_ascomdome.Connected: boolean;
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

function T_ascomdome.WaitConnecting(maxtime:integer):boolean;
{$ifdef mswindows}
var timemax,lastcheck: double;
    ok: boolean;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
   timemax:=now+maxtime/1000/secperday;
   lastcheck:=0;
   repeat
      if now>(lastcheck+waitpoll/1000/secperday) then begin
         ok:=not V.Connecting;
         if ok then break;
         lastcheck:=now;
      end;
      sleep(10);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   until ok or (now>timemax);
   result:=(now<timemax);
   if debug_msg then msg('finish to wait for connecting '+BoolToStr(result,true),9);
 except
   on E: Exception do begin
     msg(Format(rsConnectionEr, [E.Message]),0);
     result:=false;
   end;
 end;
 {$endif}
end;

procedure T_ascomdome.StatusTimerTimer(sender: TObject);
{$ifdef mswindows}
var s: boolean;
{$endif}
begin
 {$ifdef mswindows}
 try
 StatusTimer.Enabled:=false;
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
  end
  else begin
     try
       s:=GetShutter;
       if s<>stShutter then begin
         stShutter:=s;
         if Assigned(FonShutterChange) then FonShutterChange(self);
       end;
       s:=GetSlave;
       if s<>stSlave then begin
         stSlave:=s;
         if Assigned(FonSlaveChange) then FonSlaveChange(self);
       end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
  if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
 {$endif}
end;

procedure T_ascomdome.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomdome.GetPark: boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if FhasPark then result:=V.AtPark;
   except
    result:=false;
   end;
 {$endif}
end;

function T_ascomdome.WaitDomePark(maxtime:integer):boolean;
{$ifdef mswindows}
var timemax,lastcheck: double;
    ok: boolean;
{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 try
 if FhasPark then begin
   timemax:=now+maxtime/1000/secperday;
   lastcheck:=0;
   repeat
      if now>(lastcheck+waitpoll/1000/secperday) then begin
         ok:=V.AtPark;
         if ok then break;
         lastcheck:=now;
      end;
      sleep(10);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   until ok or (now>timemax);
   result:=(now<timemax);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

procedure T_ascomdome.SetPark(value:boolean);
begin
 {$ifdef mswindows}
   try
   if FhasPark and value then begin  // no ASCOM unpark
     if value<>GetPark then begin
       V.Park;
       WaitDomePark(60000);
     end;
   end;
   except
    on E: Exception do msg('Park error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascomdome.GetShutter: boolean;
{$ifdef mswindows}
var i: integer;
{$endif}
begin
 result:=false;
 {$ifdef mswindows}
   try
   if FhasShutter then begin
     i:=V.ShutterStatus;
     result:=(i=0);  // open
   end
   else
     result:=not GetPark;  // Use park status when shutter control is not implemented
   except
    result:=false;
   end;
 {$endif}
end;

function T_ascomdome.WaitShutter(onoff:boolean; maxtime:integer):boolean;
{$ifdef mswindows}
var ShutterState:integer;
    timemax,lastcheck: double;
    ok: boolean;

{$endif}
begin
 result:=true;
 {$ifdef mswindows}
 if onoff then ShutterState:=0
          else ShutterState:=1;
 try
 if FhasShutter then begin
   timemax:=now+maxtime/1000/secperday;
   lastcheck:=0;
   repeat
      if now>(lastcheck+waitpoll/1000/secperday) then begin
         ok:=(V.ShutterStatus=ShutterState);
         if ok then break;
         lastcheck:=now;
      end;
      sleep(10);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
   until ok or (now>timemax);
   result:=(now<timemax);
 end;
 except
   result:=false;
 end;
 {$endif}
end;

procedure T_ascomdome.SetShutter(value:boolean);
begin
 {$ifdef mswindows}
   try
   if FhasShutter then begin
     if value<>GetShutter then begin
       if value then V.OpenShutter
                else V.CloseShutter;
       WaitShutter(value,60000);
     end;
   end;
   except
    on E: Exception do msg('Set shutter error: ' + E.Message,0);
   end;
 {$endif}
end;

function T_ascomdome.GetSlave: boolean;
begin
 result:=false;
 {$ifdef mswindows}
   try
   if FhasSlaving then result:=V.Slaved;
   except
    result:=false;
   end;
 {$endif}
end;

procedure T_ascomdome.SetSlave(value:boolean);
begin
 {$ifdef mswindows}
   try
   if FhasSlaving then begin
     if value<>GetSlave then begin
       V.Slaved:=value;
     end;
   end;
   except
    on E: Exception do msg('Slave error: ' + E.Message,0);
   end;
 {$endif}
end;


initialization
{$ifdef mswindows}
{$if defined(cpui386) or defined(cpux86_64)}
// some Ascom driver raise this exceptions
SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
{$endif}
{$endif}

end.

