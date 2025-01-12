unit cu_ascomrestcover;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses cu_cover, cu_ascomrest, u_global, u_utils,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;

type
T_ascomrestcover = class(T_cover)
 private
   V: TAscomRest;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;
   statusinterval,waitpoll: integer;
   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function WaitConnecting(maxtime:integer):boolean;
 protected
   function GetCoverState: TCoverStatus; override;
   function GetCalibratorState: TCalibratorStatus; override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   procedure Disconnect; override;
   Procedure OpenCover; override;
   Procedure CloseCover; override;
   function GetBrightness: integer; override;
   Procedure CalibratorOn(value: integer); override;
   Procedure CalibratorOff; override;
end;

implementation

constructor T_ascomrestcover.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 V:=TAscomRest.Create(self);
 V.ClientId:=3208;
 FCoverInterface:=ASCOMREST;
 FInterfaceVersion:=1;
 statusinterval:=1000;
 waitpoll:=500;
 StatusTimer:=TTimer.Create(nil);
 StatusTimer.Enabled:=false;
 StatusTimer.Interval:=statusinterval;
 StatusTimer.OnTimer:=@StatusTimerTimer;
end;

destructor  T_ascomrestcover.Destroy;
begin
 StatusTimer.Free;
 inherited Destroy;
end;

procedure T_ascomrestcover.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
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
  if FInterfaceVersion>=2 then begin
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
       st_cov:=TCoverStatus(V.Get('coverstate').AsInt);
     except
       st_cov:=covNotPresent;
     end;
     FHasCover:=(st_cov<>covNotPresent);
     try
       st_cal:=TCalibratorStatus(V.Get('calibratorstate').AsInt);
     except
       st_cal:=calNotPresent;
     end;
     FHasCalibrator:=(st_cal<>calNotPresent);
     if FHasCalibrator then begin
       try
       FMaxBrightness:=V.Get('maxbrightness').AsInt;
       except
         FMaxBrightness:=0;
       end;
     end;
     if isLocalIP(V.RemoteIP) then
       statusinterval:=1000
     else
       statusinterval:=5000;
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

procedure T_ascomrestcover.Disconnect;
begin
   StatusTimer.Enabled:=false;
   try
   if FInterfaceVersion>=2 then begin
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

function T_ascomrestcover.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('connected').AsBool;
  except
   result:=false;
  end;
end;

function T_ascomrestcover.WaitConnecting(maxtime:integer):boolean;
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

procedure T_ascomrestcover.StatusTimerTimer(sender: TObject);
var scal:TCalibratorStatus;
    scov:TCoverStatus;
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
      scal:=GetCalibratorState;
      scov:=GetCoverState;
      if (scal<>st_cal)or(scov<>st_cov) then begin
        st_cov:=scov;
        st_cal:=scal;
        if Assigned(FonCoverChange) then FonCoverChange(self);
      end;
     except
     on E: Exception do msg('Status error: ' + E.Message,0);
    end;
  end;
  finally
   if FStatus=devConnected then StatusTimer.Enabled:=true;
  end;
end;

function T_ascomrestcover.GetCoverState: TCoverStatus;
begin
 result:=covUnknown;
 if FStatus<>devConnected then exit;
   try
   result:=TCoverStatus(V.Get('coverstate').AsInt);
   except
    on E: Exception do msg('CoverState error: ' + E.Message,0);
   end;
end;

function  T_ascomrestcover.GetCalibratorState: TCalibratorStatus;
begin
 result:=calUnknown;
 if FStatus<>devConnected then exit;
 try
   result:=TCalibratorStatus(V.Get('calibratorstate').AsInt);
   except
    on E: Exception do msg('CalibratorState error: ' + E.Message,0);
   end;
end;

procedure T_ascomrestcover.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

Procedure T_ascomrestcover.OpenCover;
begin
 if FStatus<>devConnected then exit;
 try
   V.Put('opencover');
 except
   on E: Exception do msg('OpenCover error: ' + E.Message,0);
 end;
end;

Procedure T_ascomrestcover.CloseCover;
begin
 if FStatus<>devConnected then exit;
 try
   V.Put('closecover');
 except
   on E: Exception do msg('CloseCover error: ' + E.Message,0);
 end;
end;

function T_ascomrestcover.GetBrightness: integer;
begin
 result:=0;
 if FStatus<>devConnected then exit;
 try
   result:=V.Get('brightness').AsInt;
   except
    on E: Exception do msg('Get Brightness error: ' + E.Message,0);
   end;
end;

Procedure T_ascomrestcover.CalibratorOn(value: integer);
begin
 if FStatus<>devConnected then exit;
 try
   V.Put('calibratoron',['Brightness',inttostr(value)]);
 except
   on E: Exception do msg('CalibratorOn error: ' + E.Message,0);
 end;
end;

Procedure T_ascomrestcover.CalibratorOff;
begin
 if FStatus<>devConnected then exit;
 try
   V.Put('calibratoroff');
 except
   on E: Exception do msg('CalibratorOff error: ' + E.Message,0);
 end;
end;

end.

