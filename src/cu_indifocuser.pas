unit cu_indifocuser;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

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

uses cu_focuser, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, u_utils, math, ExtCtrls, Forms, Classes, SysUtils;

type

T_indifocuser = class(T_focuser)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   FocuserDevice: Basedevice;
   Focuserport: ITextVectorProperty;
   FocusMotion: ISwitchVectorProperty;
   FocusInward,FocusOutward: ISwitch;
   FocusSpeed: INumberVectorProperty;
   FocusTimer: INumberVectorProperty;
   FocusRelativePosition: INumberVectorProperty;
   FocusAbsolutePosition: INumberVectorProperty;
   FocusAbort: ISwitchVectorProperty;
   FocusPreset: INumberVectorProperty;
   FocusGotoPreset: ISwitchVectorProperty;
   FocusTemperature: INumberVectorProperty;
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
   procedure msg(txt: string);
 protected
   procedure SetPosition(p:integer); override;
   function  GetPosition:integer; override;
   procedure SetRelPosition(p:integer); override;
   function  GetRelPosition:integer; override;
   procedure SetSpeed(p:integer); override;
   function  GetSpeed:integer; override;
   procedure SetTimer(p:integer); override;
   function  GetTimer:integer; override;
   function  GethasAbsolutePosition: boolean; override;
   function  GethasRelativePosition: boolean; override;
   function  GethasTimerSpeed: boolean; override;
   function  GetPositionRange: TNumRange; override;
   function  GetRelPositionRange: TNumRange; override;
   procedure SetTimeout(num:integer); override;
   function  GetTemperature:double; override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');  override;
   Procedure Disconnect; override;
   procedure FocusIn; override;
   procedure FocusOut; override;
end;

implementation

procedure T_indifocuser.CreateIndiClient;
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

constructor T_indifocuser.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FFocuserInterface:=INDI;
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

destructor  T_indifocuser.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 inherited Destroy;
end;

procedure T_indifocuser.ClearStatus;
begin
    FocuserDevice:=nil;
    Focuserport:=nil;
    FocusMotion:=nil;
    FocusInward:=nil;
    FocusOutward:=nil;
    FocusSpeed:=nil;
    FocusTimer:=nil;
    FocusRelativePosition:=nil;
    FocusAbsolutePosition:=nil;
    FocusAbort:=nil;
    FocusPreset:=nil;
    FocusGotoPreset:=nil;
    FocusTemperature:=nil;
    FhasTemperature:=false;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indifocuser.CheckStatus;
begin
    if Fconnected and
       (configprop<>nil) and
       (FocusMotion<>nil) and
       ((FocusAbsolutePosition<>nil)or(FocusRelativePosition<>nil)or(FocusTimer<>nil))
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

procedure T_indifocuser.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+txt);
end;

Procedure T_indifocuser.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
begin
if (indiclient=nil)or(indiclient.Terminated) then CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Findideviceport:=cp4;
  Fdevice:=Findidevice;
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

procedure T_indifocuser.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (FocuserDevice=nil)or(not Fready) then begin
    msg('Error');
    if not Fconnected then begin
      msg('No response from server');
      msg('Is "'+Findidevice+'" a running focuser driver?');
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS')
    else if (FocusMotion=nil) then
       msg('Missing property FOCUS_MOTION')
    else if ((FocusAbsolutePosition=nil)and(FocusRelativePosition=nil)and(FocusTimer=nil)) then
       msg('One of the properties ABS_FOCUS_POSITION, REL_FOCUS_POSITION, FOCUS_TIMER is required');
    Disconnect;
  end;
end;

Procedure T_indifocuser.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
ClearStatus;
end;

procedure T_indifocuser.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indifocuser.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (Focuserport=nil) and (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
  if (Focuserport<>nil)and(Findideviceport<>'') then begin
     Focuserport.tp[0].text:=Findideviceport;
     indiclient.sendNewText(Focuserport);
     msg('Set port '+Findideviceport);
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indifocuser.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Focuser server disconnected');
  CreateIndiClient;
end;

procedure T_indifocuser.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     FocuserDevice:=dp;
  end;
end;

procedure T_indifocuser.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indifocuser.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indifocuser.NewMessage(mp: IMessage);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+mp.msg);
  mp.Free;
end;

procedure T_indifocuser.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Focuserport:=indiProp.getText;
  end
  else if (proptype=INDI_SWITCH)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     configdefault:=IUFindSwitch(configprop,'CONFIG_DEFAULT');
     if (configload=nil)or(configsave=nil)or(configdefault=nil) then configprop:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='FOCUS_MOTION') then begin
     FocusMotion:=indiProp.getSwitch;
     FocusInward:=IUFindSwitch(FocusMotion,'FOCUS_INWARD');
     FocusOutward:=IUFindSwitch(FocusMotion,'FOCUS_OUTWARD');
     if (FocusInward=nil)or(FocusOutward=nil) then FocusMotion:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='FOCUS_SPEED') then begin
     FocusSpeed:=indiProp.getNumber;
  end
  else if (proptype=INDI_NUMBER)and(propname='FOCUS_TIMER') then begin
     FocusTimer:=indiProp.getNumber;
  end
  else if (proptype=INDI_NUMBER)and(propname='REL_FOCUS_POSITION') then begin
     FocusRelativePosition:=indiProp.getNumber;
  end
  else if (proptype=INDI_NUMBER)and(propname='ABS_FOCUS_POSITION') then begin
     FocusAbsolutePosition:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(propname='FOCUS_ABORT_MOTION') then begin
     FocusAbort:=indiProp.getSwitch;
  end
  else if (proptype=INDI_NUMBER)and(propname='Presets') then begin
     FocusPreset:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(propname='Goto') then begin
     FocusGotoPreset:=indiProp.getSwitch;
  end
  else if (proptype=INDI_NUMBER)and(propname='FOCUS_TEMPERATURE') then begin
     FocusTemperature:=indiProp.getNumber();
     FhasTemperature:=true;
  end;
  CheckStatus;
end;

procedure T_indifocuser.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=FocusAbsolutePosition then begin
     if Assigned(FonPositionChange) then FonPositionChange(nvp.np[0].value);
  end
  else if (FocusAbsolutePosition=nil)and(nvp=FocusRelativePosition) then begin
     if Assigned(FonPositionChange) then FonPositionChange(nvp.np[0].value);
  end
  else if nvp=FocusSpeed then begin
     if Assigned(FonSpeedChange) then FonSpeedChange(nvp.np[0].value);
  end
  else if nvp=FocusTimer then begin
     if Assigned(FonTimerChange) then FonTimerChange(nvp.np[0].value);
  end
  else if nvp=FocusTemperature then begin
     if Assigned(FonTemperatureChange) then FonTemperatureChange(nvp.np[0].value);
  end;
end;

procedure T_indifocuser.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indifocuser.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indifocuser.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indifocuser.SetPosition(p:integer);
var n: integer;
begin
if FocusAbsolutePosition<>nil then begin
  if PositionRange<>NullRange then
     n:=max(min(p,round(PositionRange.max)),round(PositionRange.min))
   else
     n:=p;
  FocusAbsolutePosition.np[0].value:=n;
  indiclient.sendNewNumber(FocusAbsolutePosition);
  FocuserLastTemp:=FocuserTemp;
  indiclient.WaitBusy(FocusAbsolutePosition,60000);
end;
end;

function  T_indifocuser.GetPosition:integer;
begin
if FocusAbsolutePosition<>nil then begin;
  result:=round(FocusAbsolutePosition.np[0].value);
end
else result:=0;
end;

function  T_indifocuser.GetPositionRange: TNumRange;
begin
if FocusAbsolutePosition<>nil then begin;
  result.min:=FocusAbsolutePosition.np[0].min;
  result.max:=FocusAbsolutePosition.np[0].max;
  result.step:=FocusAbsolutePosition.np[0].step;
end
else result:=NullRange;
end;

procedure T_indifocuser.SetRelPosition(p:integer);
var n: integer;
begin
if FocusRelativePosition<>nil then begin
  if RelPositionRange<>NullRange then
     n:=max(min(p,round(RelPositionRange.max)),round(RelPositionRange.min))
   else
     n:=p;
  FocusRelativePosition.np[0].value:=n;
  indiclient.sendNewNumber(FocusRelativePosition);
  FocuserLastTemp:=FocuserTemp;
  indiclient.WaitBusy(FocusRelativePosition,60000);
  if FDelay>0 then wait(FDelay);
end;
end;

function  T_indifocuser.GetRelPosition:integer;
begin
if FocusRelativePosition<>nil then begin;
  result:=round(FocusRelativePosition.np[0].value);
end
else result:=0;
end;

function  T_indifocuser.GetRelPositionRange: TNumRange;
begin
if FocusRelativePosition<>nil then begin;
  result.min:=FocusRelativePosition.np[0].min;
  result.max:=FocusRelativePosition.np[0].max;
  result.step:=FocusRelativePosition.np[0].step;
end
else result:=NullRange;
end;

procedure T_indifocuser.SetSpeed(p:integer);
begin
if (FocusSpeed<>nil)and(FocusSpeed.np[0].value<>p) then begin
  FocusSpeed.np[0].value:=p;
  indiclient.sendNewNumber(FocusSpeed);
  indiclient.WaitBusy(FocusSpeed);
end;
end;

function  T_indifocuser.GetSpeed:integer;
begin
if FocusSpeed<>nil then begin;
  result:=round(FocusSpeed.np[0].value);
end
else result:=0;
end;

procedure T_indifocuser.SetTimer(p:integer);
begin
if (FocusTimer<>nil) then begin
  FocusTimer.np[0].value:=p;
  indiclient.sendNewNumber(FocusTimer);
  indiclient.WaitBusy(FocusTimer);
  if FDelay>0 then wait(FDelay);
end;
end;

function  T_indifocuser.GetTimer:integer;
begin
if FocusTimer<>nil then begin;
  result:=round(FocusTimer.np[0].value);
end
else result:=0;
end;

procedure T_indifocuser.FocusIn;
begin
 if (FocusMotion<>nil)and(FocusInward.s=ISS_OFF) then begin
   IUResetSwitch(FocusMotion);
   FocusInward.s:=ISS_ON;
   indiclient.sendNewSwitch(FocusMotion);
   indiclient.WaitBusy(FocusMotion);
 end;
 FLastDirection:=FocusDirIn;
 FFocusdirection:=-1;
end;

procedure T_indifocuser.FocusOut;
begin
 if (FocusMotion<>nil)and(FocusOutward.s=ISS_OFF) then begin
   IUResetSwitch(FocusMotion);
   FocusOutward.s:=ISS_ON;
   indiclient.sendNewSwitch(FocusMotion);
   indiclient.WaitBusy(FocusMotion);
 end;
 FLastDirection:=FocusDirOut;
 FFocusdirection:=1;
end;

function  T_indifocuser.GethasAbsolutePosition: boolean;
begin
 result:=FocusAbsolutePosition<>nil;
end;

function  T_indifocuser.GethasRelativePosition: boolean;
begin
  result:=FocusRelativePosition<>nil;
end;

function  T_indifocuser.GethasTimerSpeed: boolean;
begin
  result:=(FocusSpeed<>nil)and(FocusTimer<>nil);
end;

procedure T_indifocuser.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

procedure T_indifocuser.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

function  T_indifocuser.GetTemperature:double;
begin
  if FocusTemperature<>nil then begin;
    result:=round(FocusTemperature.np[0].value);
  end
  else result:=0;
end;

end.

