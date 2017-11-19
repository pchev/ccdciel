unit cu_indiwatchdog;

{$mode objfpc}{$H+}

{
Copyright (C) 2017 Patrick Chevalley

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

uses cu_watchdog, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, u_utils, ExtCtrls, Forms, Classes, SysUtils;

type

T_indiwatchdog = class(T_watchdog)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   HeartbeatTimer: TTimer;
   WatchdogDevice: Basedevice;
   heartbeat: INumberVectorProperty;
   heartbeatvalue: INumber;
   configprop: ISwitchVectorProperty;
   configload,configsave,configdefault: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure HeartbeatTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(txt: string);
   procedure NewProperty(indiProp: IndiProperty);
   procedure NewNumber(nvp: INumberVectorProperty);
   procedure NewText(tvp: ITextVectorProperty);
   procedure NewSwitch(svp: ISwitchVectorProperty);
   procedure NewLight(lvp: ILightVectorProperty);
   procedure DeleteDevice(dp: Basedevice);
   procedure DeleteProperty(indiProp: IndiProperty);
   procedure ServerConnected(Sender: TObject);
   procedure ServerDisconnected(Sender: TObject);
   procedure LoadConfig;
   procedure SetHeartbeat(value: double);
protected
   procedure SetTimeout(num:integer); override;
   procedure SetTThreshold(num:integer); override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   Procedure Disconnect; override;

end;

implementation

procedure T_indiwatchdog.CreateIndiClient;
begin
if csDestroying in ComponentState then exit;
  indiclient:=TIndiBaseClient.Create;
  indiclient.onNewDevice:=@NewDevice;
  indiclient.onNewMessage:=@NewMessage;
  indiclient.onNewProperty:=@NewProperty;
  indiclient.onNewNumber:=@NewNumber;
  indiclient.onNewText:=@NewText;
  indiclient.onNewSwitch:=@NewSwitch;
  indiclient.onNewLight:=@NewLight;
  indiclient.onDeleteDevice:=@DeleteDevice;
  indiclient.onDeleteProperty:=@DeleteProperty;
  indiclient.onServerConnected:=@ServerConnected;
  indiclient.onServerDisconnected:=@ServerDisconnected;
  ClearStatus;
end;

constructor T_indiwatchdog.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=60000;
 InitTimer.OnTimer:=@InitTimerTimer;
 ConnectTimer:=TTimer.Create(nil);
 ConnectTimer.Enabled:=false;
 ConnectTimer.Interval:=3000;
 ConnectTimer.OnTimer:=@ConnectTimerTimer;
 HeartbeatTimer:=TTimer.Create(nil);
 HeartbeatTimer.Enabled:=false;
 HeartbeatTimer.Interval:=60000;
 HeartbeatTimer.OnTimer:=@HeartbeatTimerTimer;
 CreateIndiClient;
end;

destructor  T_indiwatchdog.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 HeartbeatTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 FreeAndNil(HeartbeatTimer);
 inherited Destroy;
end;

procedure T_indiwatchdog.ClearStatus;
begin
    WatchdogDevice:=nil;
    heartbeat:=nil;
    heartbeatvalue:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indiwatchdog.CheckStatus;
begin
    if Fconnected and
       (configprop<>nil) and
       (heartbeat<>nil) and
       (heartbeatvalue<>nil)
    then begin
       FStatus := devConnected;
       if (not Fready) then begin
         Fready:=true;
         if FAutoloadConfig then begin
           LoadConfig;
         end;
         HeartbeatTimer.Enabled:=true;
         SetHeartbeat(FThreshold);
         if Assigned(FonStatusChange) then FonStatusChange(self);
       end;
    end;
end;

Procedure T_indiwatchdog.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
if (indiclient=nil)or(indiclient.Terminated) then CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  FStatus := devDisconnected;
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg(' Watchdog already connected');
end;

procedure T_indiwatchdog.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (WatchdogDevice=nil)or(not Fready) then begin
    msg('Watchdog '+Findidevice+' Error');
    if not Fconnected then begin
      msg('No response from server');
      msg('Is "'+Findidevice+'" a running Watchdog driver?');
    end
    else if (configprop=nil) then
       msg('Watchdog '+Findidevice+' Missing property CONFIG_PROCESS')
    else if (heartbeatvalue=nil) then
       msg('Watchdog '+Findidevice+' Missing property WATCHDOG_HEARTBEAT');
    Disconnect;
  end;
end;

Procedure T_indiwatchdog.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
HeartbeatTimer.Enabled:=False;
SetHeartbeat(0);
wait(1);
indiclient.disconnectDevice(Findidevice);
wait(1);
indiclient.Terminate;
ClearStatus;
end;

procedure T_indiwatchdog.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indiwatchdog.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  indiclient.connectDevice(Findidevice);
end;

procedure T_indiwatchdog.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Watchdog '+Findidevice+' server disconnected');
  CreateIndiClient;
end;

procedure T_indiwatchdog.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     WatchdogDevice:=dp;
  end;
end;

procedure T_indiwatchdog.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indiwatchdog.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indiwatchdog.NewMessage(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+txt);
end;

procedure T_indiwatchdog.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_SWITCH)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     configdefault:=IUFindSwitch(configprop,'CONFIG_DEFAULT');
     if (configload=nil)or(configsave=nil)or(configdefault=nil) then configprop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='WATCHDOG_HEARTBEAT') then begin
     heartbeat:=indiProp.getNumber;
     heartbeatvalue:=IUFindNumber(heartbeat,'WATCHDOG_HEARTBEAT_VALUE');
  end;
  CheckStatus;
end;

procedure T_indiwatchdog.NewNumber(nvp: INumberVectorProperty);
begin
end;

procedure T_indiwatchdog.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indiwatchdog.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indiwatchdog.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indiwatchdog.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

procedure T_indiwatchdog.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

procedure T_indiwatchdog.SetTThreshold(num:integer);
begin
 FThreshold:=num;
 HeartbeatTimer.Interval:=round(60000*FThreshold/4);
 SetHeartbeat(FThreshold);
end;

procedure T_indiwatchdog.SetHeartbeat(value: double);
begin
 if (heartbeat<>nil) and (heartbeatvalue<>nil) then begin
    heartbeatvalue.Value:=value;
    indiclient.sendNewNumber(heartbeat);
 end;
end;

procedure T_indiwatchdog.HeartbeatTimerTimer(Sender: TObject);
begin
  SetHeartbeat(FThreshold);
end;


end.

