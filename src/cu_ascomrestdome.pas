unit cu_ascomrestdome;

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

uses cu_dome, cu_ascomrest, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;
type
T_ascomrestdome = class(T_dome)
 private
   V: TAscomRest;
   stShutter,stSlave,stPark: boolean;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval,waitpoll: integer;
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
end;

implementation

constructor T_ascomrestdome.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3201;
 FDomeInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 stShutter:=false;
 stSlave:=false;
 stPark:=false;
 statusinterval:=1000;
 waitpoll:=500;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestdome.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestdome.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
  try
  FStatus := devConnecting;
  V.Host:=cp1;
  V.Port:=cp2;
  V.Protocol:=cp3;
  V.User:=cp5;
  V.Password:=cp6;
  Fdevice:=cp4;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  V.Device:=Fdevice;
  V.Timeout:=5000;
  try
  FInterfaceVersion:=V.Get('interfaceversion').AsInt;
  except
    FInterfaceVersion:=1;
  end;
  msg('Interface version: '+inttostr(FInterfaceVersion),9);
  if FInterfaceVersion>=3 then begin
    V.Put('Connect');
    WaitConnecting(30000);
  end
  else
    V.Put('Connected',true);
  if V.Get('connected').AsBool then begin
     V.Timeout:=120000;
     try
     msg(V.Get('driverinfo').AsString,9);
     except
     end;
     try
     msg('Driver version: '+V.Get('driverversion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FhasPark:=V.Get('canpark').AsBool;
     except
       FhasPark:=false;
     end;
     try
     FhasSlaving:=V.Get('canslave').AsBool;
     except
       FhasSlaving:=false;
     end;
     try
     FhasShutter:=V.Get('cansetshutter').AsBool;
     except
       FhasShutter:=false;
     end;
     if isLocalIP(V.RemoteIP) then begin
       statusinterval:=1000;
       waitpoll:=500;
     end
     else begin
       statusinterval:=5000;
       waitpoll:=1000;
     end;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
  end
  else
     Disconnect;
  except
    on E: Exception do begin
         msg('Connection error: ' + E.Message,0);
         Disconnect;
    end;

  end;
end;

procedure T_ascomrestdome.Disconnect;
begin
   StatusTimer.Enabled:=false;
   try
   if FInterfaceVersion>=3 then begin
     V.Put('Disconnect');
     WaitConnecting(30000);
   end
   else
     V.Put('Connected',false);
   except
    on E: Exception do msg(Format(rsDisconnectio, [E.Message]),0);
   end;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   msg(rsDisconnected3,1);
end;

function T_ascomrestdome.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   on E: Exception do begin
     if debug_msg then msg('Error Connected : '+ E.Message);
     result:=false;
   end;
  end;
end;

function T_ascomrestdome.WaitConnecting(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('connecting').AsBool)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 except
  on E: Exception do begin
    msg(Format(rsConnectionEr, [E.Message]),0);
    result:=false;
  end;
 end;
end;

procedure T_ascomrestdome.StatusTimerTimer(sender: TObject);
var s: boolean;
begin
 StatusTimer.Enabled:=false;
 try
  if Connected then begin
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
end;

procedure T_ascomrestdome.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomrestdome.GetPark: boolean;
begin
 result:=false;
   if FStatus<>devConnected then exit;
   try
   if FhasPark then result:=V.Get('atpark').AsBool;
   except
    result:=false;
   end;
end;

function T_ascomrestdome.WaitDomePark(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 if FStatus<>devConnected then exit;
 try
 if FhasPark then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (not V.Get('atpark').AsBool)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
end;

procedure T_ascomrestdome.SetPark(value:boolean);
begin
   if FStatus<>devConnected then exit;
   try
   if FhasPark and value then begin  // no ASCOM unpark
     if value<>GetPark then begin
       V.Put('park');
     end;
   end;
   except
    on E: Exception do msg('Park error: ' + E.Message,0);
   end;
end;

function T_ascomrestdome.GetShutter: boolean;
var i: integer;
begin
 result:=false;
   if FStatus<>devConnected then exit;
   try
   if FhasShutter then begin
     i:=V.Get('shutterstatus').AsInt;
     result:=(i=0);  // open
   end
   else
     result:=not GetPark;  // Use park status when shutter control is not implemented
   except
    result:=false;
   end;
end;

function T_ascomrestdome.WaitShutter(onoff:boolean; maxtime:integer):boolean;
var ShutterState,count,maxcount:integer;
begin
 result:=true;
 if FStatus<>devConnected then exit;
 if onoff then ShutterState:=0
          else ShutterState:=1;
 try
 if FhasShutter then begin
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('shutterstatus').AsInt<>ShutterState)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 end;
 except
   result:=false;
 end;
end;

procedure T_ascomrestdome.SetShutter(value:boolean);
begin
   if FStatus<>devConnected then exit;
   try
   if FhasShutter then begin
     if value<>GetShutter then begin
       if value then V.Put('openshutter')
                else V.Put('closeshutter');
       WaitShutter(value,60000);
     end;
   end;
   except
    on E: Exception do msg('Set shutter error: ' + E.Message,0);
   end;
end;

function T_ascomrestdome.GetSlave: boolean;
begin
 result:=false;
   if FStatus<>devConnected then exit;
   try
   if FhasSlaving then result:=V.Get('slaved').AsBool;
   except
    result:=false;
   end;
end;

procedure T_ascomrestdome.SetSlave(value:boolean);
begin
   if FStatus<>devConnected then exit;
   try
   if FhasSlaving then begin
     if value<>GetSlave then begin
       V.Put('Slaved',value);
     end;
   end;
   except
    on E: Exception do msg('Slave error: ' + E.Message,0);
   end;
end;

end.

