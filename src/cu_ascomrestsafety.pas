unit cu_ascomrestsafety;

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

uses cu_safety, cu_ascomrest, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestsafety = class(T_safety)
 private
   V: TAscomRest;
   stSafe: boolean;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval, waitpoll: integer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   function  GetSafe:boolean; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
end;

implementation

constructor T_ascomrestsafety.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3205;
 FSafetyInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 statusinterval:=2000;
 waitpoll:=500;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestsafety.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestsafety.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
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
     if isLocalIP(V.RemoteIP) then
       statusinterval:=2000
     else
       statusinterval:=10000;
     msg(rsConnected3);
     FStatus := devConnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     StatusTimer.Enabled:=true;
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

procedure T_ascomrestsafety.Disconnect;
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

function T_ascomrestsafety.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

function T_ascomrestsafety.WaitConnecting(maxtime:integer):boolean;
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

procedure T_ascomrestsafety.StatusTimerTimer(sender: TObject);
var s: boolean;
begin
 StatusTimer.Enabled:=false;
 try
  if not Connected then begin
     FStatus := devDisconnected;
     if Assigned(FonStatusChange) then FonStatusChange(self);
     msg(rsDisconnected3,1);
  end
  else begin
    try
      s:=GetSafe;
      if s<>stSafe then begin
        stSafe:=s;
        if Assigned(FonSafeChange) then FonSafeChange(self);
      end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function  T_ascomrestsafety.Getsafe:boolean;
begin
 result:=false;
 if FStatus<>devConnected then exit;
 try
   result:=V.Get('issafe').AsBool;
   except
    on E: Exception do msg('Get IsSafe error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestsafety.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

