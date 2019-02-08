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

uses cu_dome, cu_ascomrest, u_global,
    u_translation, indiapi,
    Forms, ExtCtrls,Classes, SysUtils;
type
T_ascomrestdome = class(T_dome)
 private
   V: TAscomRest;
   stShutter,stSlave,stPark: boolean;
   FInterfaceVersion: integer;
   StatusTimer: TTimer;

   procedure StatusTimerTimer(sender: TObject);
   function  Connected: boolean;
   function  InterfaceVersion: integer;
 protected
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

const statusinterval=10000;

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

function  T_ascomrestdome.InterfaceVersion: integer;
begin
 result:=1;
  try
   result:=V.Get('InterfaceVersion').AsInt;
  except
    result:=1;
  end;
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
  V.Put('Connected',true);
  if V.Get('Connected').AsBool then begin
     FInterfaceVersion:=InterfaceVersion;
     try
     msg('Driver version: '+V.Get('DriverVersion').AsString,9);
     except
       msg('Error: unknown driver version',9);
     end;
     try
     FhasPark:=V.Get('CanPark').AsBool;
     except
       FhasPark:=false;
     end;
     try
     FhasSlaving:=V.Get('CanSlave').AsBool;
     except
       FhasSlaving:=false;
     end;
     try
     FhasShutter:=V.Get('CanSetShutter').AsBool;
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
    on E: Exception do begin
         msg('Connection error: ' + E.Message,0);
         Disconnect;
    end;

  end;
end;

procedure T_ascomrestdome.Disconnect;
begin
   StatusTimer.Enabled:=false;
   FStatus := devDisconnected;
   if Assigned(FonStatusChange) then FonStatusChange(self);
   try
     msg(rsDisconnected3,0);
     V.Put('Connected',false);
   except
     on E: Exception do msg('Disconnection error: ' + E.Message,0);
   end;
end;

function T_ascomrestdome.Connected: boolean;
begin
result:=false;
  try
  result:=V.Get('Connected').AsBool;
  except
   result:=false;
  end;
end;

procedure T_ascomrestdome.StatusTimerTimer(sender: TObject);
var s: boolean;
begin
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
end;

procedure T_ascomrestdome.SetTimeout(num:integer);
begin
 FTimeOut:=num;
end;

function T_ascomrestdome.GetPark: boolean;
begin
 result:=false;
   try
   if FhasPark then result:=V.Get('AtPark').AsBool;
   except
    result:=false;
   end;
end;

procedure T_ascomrestdome.SetPark(value:boolean);
begin
   try
   if FhasPark and value then V.Put('Park'); // no ASCOM unpark
   except
    on E: Exception do msg('Park error: ' + E.Message,0);
   end;
end;

function T_ascomrestdome.GetShutter: boolean;
var i: integer;
begin
 result:=false;
   try
   if FhasShutter then begin
     i:=V.Get('ShutterStatus').AsInt;
     result:=(i=0);  // open
   end;
   except
    result:=false;
   end;
end;

procedure T_ascomrestdome.SetShutter(value:boolean);
begin
   try
   if FhasShutter then begin
     if value then V.Put('OpenShutter')
              else V.Put('CloseShutter');
   end;
   except
    on E: Exception do msg('Set shutter error: ' + E.Message,0);
   end;
end;

function T_ascomrestdome.GetSlave: boolean;
begin
 result:=false;
   try
   if FhasSlaving then result:=V.Get('Slaved').AsBool;
   except
    result:=false;
   end;
end;

procedure T_ascomrestdome.SetSlave(value:boolean);
begin
   try
   if FhasSlaving then V.Put('Slaved',value);
   except
    on E: Exception do msg('Slave error: ' + E.Message,0);
   end;
end;

end.

