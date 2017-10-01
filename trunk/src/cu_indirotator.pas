unit cu_indirotator;

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

uses cu_rotator, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indirotator = class(T_rotator)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   RotatorDevice: Basedevice;
   Rotatorport: ITextVectorProperty;
   RotatorAngle: INumberVectorProperty;
   RotatorAbort: ISwitchVectorProperty;
   RotatorReverse: ISwitchVectorProperty;
   RotatorReverseEnable,RotatorReverseDisable: ISwitch;
   configprop: ISwitchVectorProperty;
   configload,configsave,configdefault: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
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
   procedure msg(txt: string);
 protected
   procedure SetAngle(p:double); override;
   function  GetAngle:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetDriverReverse:boolean; override;
   procedure SetDriverReverse(value:boolean); override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   Procedure Disconnect; override;
   Procedure Halt; override;

end;

implementation

procedure T_indirotator.CreateIndiClient;
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

constructor T_indirotator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FRotatorInterface:=INDI;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=60000;
 InitTimer.OnTimer:=@InitTimerTimer;
 ConnectTimer:=TTimer.Create(nil);
 ConnectTimer.Enabled:=false;
 ConnectTimer.Interval:=3000;
 ConnectTimer.OnTimer:=@ConnectTimerTimer;
 CreateIndiClient;
end;

destructor  T_indirotator.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 inherited Destroy;
end;

procedure T_indirotator.ClearStatus;
begin
    RotatorDevice:=nil;
    Rotatorport:=nil;
    RotatorAngle:=nil;
    RotatorAbort:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indirotator.CheckStatus;
begin
    if Fconnected and
       (configprop<>nil) and
       (RotatorAngle<>nil)
    then begin
       FStatus := devConnected;
       if (not Fready) then begin
         Fready:=true;
         if FAutoloadConfig then begin
           LoadConfig;
         end;
         if Assigned(FonStatusChange) then FonStatusChange(self);
       end;
    end;
end;

procedure T_indirotator.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+txt);
end;

Procedure T_indirotator.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
if (indiclient=nil)or(indiclient.Terminated) then CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Findideviceport:=cp4;
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg('Focuser already connected');
end;

procedure T_indirotator.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (RotatorDevice=nil)or(not Fready) then begin
    msg('Error');
    if not Fconnected then begin
      msg('No response from server');
      msg('Is "'+Findidevice+'" a running rotator driver?');
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS')
    else if (RotatorAngle=nil) then
       msg('Missing property ABS_ROTATOR_ANGLE');
    Disconnect;
  end;
end;

Procedure T_indirotator.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
ClearStatus;
end;

procedure T_indirotator.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indirotator.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (Rotatorport=nil) and (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
  if (Rotatorport<>nil)and(Findideviceport<>'') then begin
     Rotatorport.tp[0].text:=Findideviceport;
     indiclient.sendNewText(Rotatorport);
     msg('Set port '+Findideviceport);
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indirotator.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Focuser server disconnected');
  CreateIndiClient;
end;

procedure T_indirotator.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     RotatorDevice:=dp;
  end;
end;

procedure T_indirotator.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indirotator.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indirotator.NewMessage(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+txt);
end;

procedure T_indirotator.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Rotatorport:=indiProp.getText;
  end
  else if (proptype=INDI_SWITCH)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     configdefault:=IUFindSwitch(configprop,'CONFIG_DEFAULT');
     if (configload=nil)or(configsave=nil)or(configdefault=nil) then configprop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='ABS_ROTATOR_ANGLE') then begin
     RotatorAngle:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(propname='ROTATOR_REVERSE') then begin
     RotatorReverse:=indiProp.getSwitch;
     RotatorReverseEnable:=IUFindSwitch(RotatorReverse,'REVERSE_ENABLED');
     RotatorReverseDisable:=IUFindSwitch(RotatorReverse,'REVERSE_DISABLED');
     if (RotatorReverseEnable=nil)or(RotatorReverseDisable=nil) then RotatorReverse:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='ROTATOR_ABORT_MOTION') then begin
     RotatorAbort:=indiProp.getSwitch;
  end;
  CheckStatus;
end;

procedure T_indirotator.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=RotatorAngle then begin
     if Assigned(FonAngleChange) then FonAngleChange(self);
  end;
end;

procedure T_indirotator.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indirotator.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indirotator.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indirotator.SetAngle(p:double);
begin
if RotatorAngle<>nil then begin
  RotatorAngle.np[0].value:=p;
  indiclient.sendNewNumber(RotatorAngle);
  indiclient.WaitBusy(RotatorAngle);
end;
end;

function  T_indirotator.GetAngle:double;
begin
if RotatorAngle<>nil then begin;
  result:=round(RotatorAngle.np[0].value);
end
else result:=0;
end;

function T_indirotator.GetDriverReverse:boolean;
begin
  result:=false;
  if RotatorReverse<>nil then begin
    result := (RotatorReverseEnable.s=ISS_ON);
  end;
end;

procedure T_indirotator.SetDriverReverse(value:boolean);
begin
  if RotatorReverse<>nil then begin
     IUResetSwitch(RotatorReverse);
     if value then
        RotatorReverseEnable.s:=ISS_ON
     else
       RotatorReverseDisable.s:=ISS_ON;
     indiclient.sendNewSwitch(RotatorReverse);
  end;
end;

Procedure T_indirotator.Halt;
begin
  if RotatorAbort<>nil then begin
     RotatorAbort.sp[0].s:=ISS_ON;
     indiclient.sendNewSwitch(RotatorAbort);
  end;
end;

procedure T_indirotator.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

procedure T_indirotator.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

end.

