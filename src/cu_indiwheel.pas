unit cu_indiwheel;

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

uses cu_wheel, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indiwheel = class(T_wheel)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   ReadyTimer: TTimer;
   WheelDevice: Basedevice;
   Wheelport: ITextVectorProperty;
   WheelSlot: INumberVectorProperty;
   Slot: INumber;
   FilterName: ITextVectorProperty;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure ReadyTimerTimer(Sender: TObject);
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
 protected
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   function  GetFilterNames:TStringList; override;
   procedure SetTimeout(num:integer); override;
   procedure SetCamera(value: TObject); override;
   function  GetStatus: TDeviceStatus; override;
public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   Procedure Disconnect; override;
end;

implementation

procedure T_indiwheel.CreateIndiClient;
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

constructor T_indiwheel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FWheelInterface:=INDI;
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
 ReadyTimer:=TTimer.Create(nil);
 ReadyTimer.Enabled:=false;
 ReadyTimer.Interval:=2000;
 ReadyTimer.OnTimer:=@ReadyTimerTimer;
end;

destructor  T_indiwheel.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 ReadyTimer.Enabled:=false;
 if indiclient<>nil then indiclient.onServerDisconnected:=nil;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 FreeAndNil(ReadyTimer);
 inherited Destroy;
end;

procedure T_indiwheel.ClearStatus;
begin
    WheelDevice:=nil;
    Wheelport:=nil;
    WheelSlot:=nil;
    FilterName:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indiwheel.CheckStatus;
begin
    if Fconnected and
       ((configprop<>nil)or(not FAutoloadConfig)) and
       (WheelSlot<>nil) and
       (FilterName<>nil)
    then begin
      ReadyTimer.Enabled := false;
      ReadyTimer.Enabled := true;
    end;
end;

procedure T_indiwheel.ReadyTimerTimer(Sender: TObject);
begin
  ReadyTimer.Enabled := false;
  FStatus := devConnected;
  if (not Fready) then begin
    Fready:=true;
    if FAutoloadConfig then begin
      LoadConfig;
    end;
    if Assigned(FonStatusChange) then FonStatusChange(self);
  end;
end;

Procedure T_indiwheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Fdevice:=cp3;
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
else msg('Filters already connected',0);
end;

procedure T_indiwheel.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (WheelDevice=nil)or(not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running wheel driver?',0);
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS',0)
    else if (WheelSlot=nil) then
       msg('Missing property FILTER_SLOT',0)
    else if (FilterName=nil) then
       msg('Missing property FILTER_NAME',0);
    Disconnect;
  end;
end;

Procedure T_indiwheel.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
ClearStatus;
end;

procedure T_indiwheel.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indiwheel.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (Wheelport=nil) and (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
  if (Wheelport<>nil)and(Findideviceport<>'') then begin
     Wheelport.tp[0].text:=Findideviceport;
     indiclient.sendNewText(Wheelport);
     msg('Set port '+Findideviceport);
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indiwheel.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,0);
end;

procedure T_indiwheel.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     WheelDevice:=dp;
  end;
end;

procedure T_indiwheel.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indiwheel.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indiwheel.NewMessage(mp: IMessage);
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.Free;
end;

procedure T_indiwheel.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    i: integer;
    TxtProp: ITextVectorProperty;
    Txt: IText;
    buf: string;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(Wheelport=nil)and(propname='DEVICE_PORT') then begin
     Wheelport:=indiProp.getText;
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
  else if (proptype=INDI_SWITCH)and(configprop=nil)and(propname='CONFIG_PROCESS') then begin
      configprop:=indiProp.getSwitch;
      configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
      configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
      if (configload=nil)or(configsave=nil) then configprop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(WheelSlot=nil)and(propname='FILTER_SLOT') then begin
     WheelSlot:=indiProp.getNumber;
     Slot:=IUFindNumber(WheelSlot,'FILTER_SLOT_VALUE');
     if Slot=nil then WheelSlot:=nil;
  end
  else if (proptype=INDI_TEXT)and(FilterName=nil)and(propname='FILTER_NAME') then begin
     FilterName:=indiProp.getText;
     FFilterNames.Clear;
     FFilterNames.Add(Filter0);
     for i:=0 to FilterName.ntp-1 do begin
        if debug_msg then msg('Filter '+inttostr(i+1)+' : '+FilterName.tp[i].text,3);
        FFilterNames.Add(FilterName.tp[i].text);
     end;
  end;
  CheckStatus;
end;

procedure T_indiwheel.NewNumber(nvp: INumberVectorProperty);
begin
  if (nvp=WheelSlot) and Assigned(FonFilterChange) then begin
     if nvp.s=IPS_BUSY then
       FonFilterChange(-1) // report moving
     else
       FonFilterChange(Slot.value);
  end;
end;

procedure T_indiwheel.NewText(tvp: ITextVectorProperty);
var propname: string;
    i: integer;
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
 propname:=tvp.name;
 if (propname='FILTER_NAME') then begin
     FFilterNames.Clear;
     FFilterNames.Add(Filter0);
     for i:=0 to tvp.ntp-1 do begin
        if debug_msg then msg('Filter '+inttostr(i+1)+' : '+FilterName.tp[i].text,3);
        FFilterNames.Add(tvp.tp[i].text);
     end;
     if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
 end;
end;

procedure T_indiwheel.NewSwitch(svp: ISwitchVectorProperty);
var sw: ISwitch;
begin
 if (svp.name='CONNECTION') then begin
   sw:=IUFindOnSwitch(svp);
   if (sw<>nil)and(sw.name='DISCONNECT') then begin
     Disconnect;
   end;
 end;
end;

procedure T_indiwheel.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indiwheel.SetFilter(num:integer);
begin
if (WheelSlot<>nil)and(num>0)and(Slot.value<>num) then begin;
  if num>Slot.max then num:=round(Slot.max);
  if num<Slot.min then num:=round(Slot.min);
  msg(Format(rsSetFilterPos, [inttostr(num)]));
  Slot.value:=num;
  indiclient.sendNewNumber(WheelSlot);
  if Assigned(FonFilterChange) then FonFilterChange(-1);
  indiclient.WaitBusy(WheelSlot);
  if Assigned(FonFilterChange) then FonFilterChange(Slot.value);
end;
end;

function  T_indiwheel.GetFilter:integer;
begin
if WheelSlot<>nil then begin;
  if WheelSlot.s=IPS_BUSY then
     result:=-1 // report moving
  else begin
     result:=round(Slot.value);
     if result>Slot.max then result:=round(Slot.max);
     if result<Slot.min then result:=round(Slot.min);
  end;
end
else result:=0;
end;

procedure T_indiwheel.SetFilterNames(value:TStringList);
var i:integer;
begin
if (FilterName<>nil)and(value.Count=FilterName.ntp) then begin
  for i:=1 to value.Count-1 do begin  // skip filter0
     FilterName.tp[i-1].text:=value[i];
  end;
  indiclient.sendNewText(FilterName);
  indiclient.WaitBusy(FilterName);
end;
end;

function  T_indiwheel.GetFilterNames:TStringList;
begin
  result:=FFilterNames;
end;

function  T_indiwheel.GetStatus: TDeviceStatus;
begin
  result:=FStatus;
end;

procedure T_indiwheel.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 if indiclient<>nil then indiclient.Timeout:=FTimeOut;
end;

procedure T_indiwheel.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

procedure T_indiwheel.SetCamera(value: TObject);
begin
 Fcameraobj:=value;
end;

end.

