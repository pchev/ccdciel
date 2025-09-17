unit cu_ascomrestrotator;

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

uses cu_rotator, cu_ascomrest, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestrotator = class(T_rotator)
 private
   V: TAscomRest;
   stAngle: double;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval,waitpoll: integer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   procedure SetAngle(p:double); override;
   function  GetAngle:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetDriverReverse:boolean; override;
   procedure SetDriverReverse(value:boolean); override;

   function  WaitRotatorMoving(maxtime:integer):boolean;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   procedure Sync(p:double); override;
   Procedure Halt; override;
end;

implementation

constructor T_ascomrestrotator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3204;
 FRotatorInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 waitpoll:=500;
 statusinterval:=2000;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestrotator.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestrotator.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
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
  if FInterfaceVersion>=4 then begin
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
     if isLocalIP(V.RemoteIP) then begin
       waitpoll:=500;
       statusinterval:=2000;
     end
     else begin
       waitpoll:=1000;
       statusinterval:=3000;
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
      msg(Format(rsConnectionEr, [E.Message]),0);
      Disconnect;
   end;
  end;
end;

procedure T_ascomrestrotator.Disconnect;
begin
   StatusTimer.Enabled:=false;
   try
   if FInterfaceVersion>=4 then begin
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

function T_ascomrestrotator.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

function T_ascomrestrotator.WaitConnecting(maxtime:integer):boolean;
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

procedure T_ascomrestrotator.StatusTimerTimer(sender: TObject);
var p: double;
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
      p:=GetAngle;
      if p<>stAngle then begin
        stAngle:=p;
        if Assigned(FonAngleChange) then FonAngleChange(self);
      end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function T_ascomrestrotator.WaitRotatorMoving(maxtime:integer):boolean;
var count,maxcount:integer;
begin
 result:=true;
 if FStatus<>devConnected then exit;
 try
   maxcount:=maxtime div waitpoll;
   count:=0;
   while (V.Get('ismoving').AsBool)and(count<maxcount) do begin
      sleep(waitpoll);
      if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
      inc(count);
   end;
   result:=(count<maxcount);
 except
   result:=false;
 end;
end;

procedure T_ascomrestrotator.SetAngle(p:double);
begin
 if FStatus<>devConnected then exit;
 try
   //msg('Rotator '+Fdevice+' move to internal '+FormatFloat(f1,p));
   V.Put('moveabsolute',['Position',FormatFloat(f2,p)]);
   WaitRotatorMoving(120000);

   except
    on E: Exception do msg('Error, can''t move to. ' + E.Message,0);
   end;
end;

function  T_ascomrestrotator.GetAngle:double;
begin
 result:=0;
 if FStatus<>devConnected then exit;
 try
   result:=V.Get('position').AsFloat;
 except
    on E: Exception do msg('Get position error: ' + E.Message,0);
 end;
end;

procedure T_ascomrestrotator.Sync(p:double);
begin
 if FStatus<>devConnected then exit;
 try
   //msg('Rotator '+Fdevice+' sync '+FormatFloat(f1,p));
   V.Put('sync',['Position',FormatFloat(f2,p)]);

   except
    // ignore if not supported
   end;
end;

function T_ascomrestrotator.GetDriverReverse:boolean;
begin
 result:=false;
 if FStatus<>devConnected then exit;
   try
   if V.Get('canreverse').AsBool then result:=V.Get('reverse').AsBool;
   except
    result:=false;
   end;
end;

procedure T_ascomrestrotator.SetDriverReverse(value:boolean);
begin
 if FStatus<>devConnected then exit;
   try
   if V.Get('canreverse').AsBool then V.Put('Reverse',value);
   except
   end;
end;

Procedure T_ascomrestrotator.Halt;
begin
 if FStatus<>devConnected then exit;
   try
    V.Put('halt');
   except
    on E: Exception do msg('Halt error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestrotator.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

end.

