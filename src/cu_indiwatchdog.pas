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

uses cu_watchdog, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, u_utils, ExtCtrls, Forms, Classes, SysUtils;

type

T_indiwatchdog = class(T_watchdog)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   ReadyTimer: TTimer;
   HeartbeatTimer: TTimer;
   WatchdogDevice: Basedevice;
   connectprop: ISwitchVectorProperty;
   connecton,connectoff: ISwitch;
   heartbeat: INumberVectorProperty;
   heartbeatvalue: INumber;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure ReadyTimerTimer(Sender: TObject);
   procedure HeartbeatTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(mp: IMessage);
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
  indiclient.Timeout:=FTimeOut;
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
 ConnectTimer.Interval:=1000;
 ConnectTimer.OnTimer:=@ConnectTimerTimer;
 ReadyTimer:=TTimer.Create(nil);
 ReadyTimer.Enabled:=false;
 ReadyTimer.Interval:=2000;
 ReadyTimer.OnTimer:=@ReadyTimerTimer;
 HeartbeatTimer:=TTimer.Create(nil);
 HeartbeatTimer.Enabled:=false;
 HeartbeatTimer.Interval:=60000;
 HeartbeatTimer.OnTimer:=@HeartbeatTimerTimer;
end;

destructor  T_indiwatchdog.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 ReadyTimer.Enabled:=false;
 HeartbeatTimer.Enabled:=false;
 if indiclient<>nil then indiclient.onServerDisconnected:=nil;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 FreeAndNil(ReadyTimer);
 FreeAndNil(HeartbeatTimer);
 inherited Destroy;
end;

procedure T_indiwatchdog.ClearStatus;
begin
    WatchdogDevice:=nil;
    heartbeat:=nil;
    heartbeatvalue:=nil;
    connectprop:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indiwatchdog.CheckStatus;
begin
    if Fconnected and
       (heartbeat<>nil) and
       (heartbeatvalue<>nil)
    then begin
      ReadyTimer.Enabled := false;
      ReadyTimer.Enabled := true;
    end;
end;

procedure T_indiwatchdog.ReadyTimerTimer(Sender: TObject);
begin
  ReadyTimer.Enabled := false;
  FStatus := devConnected;
  if (not Fready) then begin
    Fready:=true;
    HeartbeatTimer.Enabled:=true;
    SetHeartbeat(FThreshold);
    if Assigned(FonStatusChange) then FonStatusChange(self);
  end;
end;

Procedure T_indiwatchdog.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  FDevice:=cp3;
  FStatus := devDisconnected;
  msg('Connecting to INDI server "'+Findiserver+':'+Findiserverport+'" for device "'+Findidevice+'"',9);
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg(' Watchdog already connected',0);
end;

procedure T_indiwatchdog.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (WatchdogDevice=nil)or(not Fready) then begin
    msg(rsError2,1);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running Watchdog driver?',0);
    end
    else if (configprop=nil) then
       msg('Watchdog '+Findidevice+' Missing property CONFIG_PROCESS',0)
    else if (heartbeatvalue=nil) then
       msg('Watchdog '+Findidevice+' Missing property WATCHDOG_HEARTBEAT',0);
    Disconnect;
  end;
end;

Procedure T_indiwatchdog.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
HeartbeatTimer.Enabled:=False;
if indiclient<>nil then begin
  SetHeartbeat(0);
  wait(1);
  indiclient.disconnectDevice(Findidevice);
  wait(1);
  indiclient.Terminate;
end;
ClearStatus;
end;

procedure T_indiwatchdog.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indiwatchdog.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (connectprop<>nil) then begin
    if (connectoff.s=ISS_ON) then begin
      indiclient.connectDevice(Findidevice);
      exit;
    end;
  end
  else begin
    ConnectTimer.Enabled:=true;
    exit;
  end;
end;

procedure T_indiwatchdog.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsserver+' '+rsDisconnected3,0);
end;

procedure T_indiwatchdog.NewDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     msg('INDI server send new device: "'+dp.getDeviceName+'"',9);
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

procedure T_indiwatchdog.NewMessage(mp: IMessage);
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.Free;
end;

procedure T_indiwatchdog.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    TxtProp: ITextVectorProperty;
    Txt: IText;
    buf: string;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_SWITCH)and(configprop=nil)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     if (configload=nil)or(configsave=nil) then configprop:=nil;
  end
  else if (proptype=INDI_SWITCH)and(connectprop=nil)and(propname='CONNECTION') then begin
     connectprop:=indiProp.getSwitch;
     connecton:=IUFindSwitch(connectprop,'CONNECT');
     connectoff:=IUFindSwitch(connectprop,'DISCONNECT');
     if (connecton=nil)or(connectoff=nil) then connectprop:=nil;
  end
  else if (proptype=INDI_TEXT)and(propname='DRIVER_INFO') then begin
     buf:='';
     TxtProp:=indiProp.getText;
     if TxtProp<>nil then begin
       Txt:=IUFindText(TxtProp,'DRIVER_EXEC');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text+', ';
       Txt:=IUFindText(TxtProp,'DRIVER_VERSION');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text+', ';
       Txt:=IUFindText(TxtProp,'DRIVER_INTERFACE');
       if Txt<>nil then buf:=buf+Txt.lbl+': '+Txt.Text;
       msg(buf,9);
     end;
  end
  else if (proptype=INDI_NUMBER)and(heartbeat=nil)and(propname='WATCHDOG_HEARTBEAT') then begin
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
var sw: ISwitch;
begin
  if (svp.name='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      Disconnect;
    end;
  end;
end;

procedure T_indiwatchdog.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indiwatchdog.SetTimeout(num:integer);
begin
  FTimeOut:=num;
  if indiclient<>nil then indiclient.Timeout:=FTimeOut;
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

