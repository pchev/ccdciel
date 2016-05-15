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

uses cu_wheel, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indiwheel = class(T_wheel)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   WheelDevice: Basedevice;
   Wheelport: ITextVectorProperty;
   WheelSlot: INumberVectorProperty;
   Slot: INumber;
   FilterName: ITextVectorProperty;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
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
   procedure msg(txt: string);
 protected
   procedure SetFilter(num:integer); override;
   function  GetFilter:integer; override;
   procedure SetFilterNames(value:TStringList); override;
   procedure SetTimeout(num:integer); override;
public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
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

constructor T_indiwheel.Create;
begin
 inherited Create;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=10000;
 InitTimer.OnTimer:=@InitTimerTimer;
 CreateIndiClient;
end;

destructor  T_indiwheel.Destroy;
begin
 InitTimer.Enabled:=false;
 indiclient.Free;
 FreeAndNil(InitTimer);
 inherited Destroy;
end;

procedure T_indiwheel.ClearStatus;
begin
    WheelDevice:=nil;
    Wheelport:=nil;
    WheelSlot:=nil;
    FilterName:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indiwheel.CheckStatus;
begin
    if Fconnected and
       (WheelSlot<>nil) and
       (FilterName<>nil)
    then begin
       FStatus := devConnected;
      if (not Fready) and Assigned(FonStatusChange) then FonStatusChange(self);
      Fready:=true;
    end;
end;

procedure T_indiwheel.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_indiwheel.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
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
else msg('Filters already connected');
end;

procedure T_indiwheel.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (WheelDevice=nil)or(not Fready) then begin
     msg('No response from server');
     msg('Is "'+Findidevice+'" a running wheel driver?');
     Disconnect;
  end;
end;

Procedure T_indiwheel.Disconnect;
begin
indiclient.Terminate;
ClearStatus;
end;

procedure T_indiwheel.ServerConnected(Sender: TObject);
begin
   if (Wheelport<>nil)and(Findideviceport<>'') then begin
      Wheelport.tp[0].text:=Findideviceport;
      indiclient.sendNewText(Wheelport);
   end;
   indiclient.connectDevice(Findidevice);
end;

procedure T_indiwheel.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Filter wheel server disconnected');
  CreateIndiClient;
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

procedure T_indiwheel.NewMessage(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

procedure T_indiwheel.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    i: integer;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Wheelport:=indiProp.getText;
  end
  else if (proptype=INDI_NUMBER)and(propname='FILTER_SLOT') then begin
     WheelSlot:=indiProp.getNumber;
     Slot:=IUFindNumber(WheelSlot,'FILTER_SLOT_VALUE');
     if Slot=nil then WheelSlot:=nil;
  end
  else if (proptype=INDI_TEXT)and(propname='FILTER_NAME') then begin
     FilterName:=indiProp.getText;
     FFilterNames.Clear;
     FFilterNames.Add(Filter0);
     for i:=0 to FilterName.ntp-1 do begin
        FFilterNames.Add(FilterName.tp[i].text);
     end;
  end;
  CheckStatus;
end;

procedure T_indiwheel.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=WheelSlot then begin
     if Assigned(FonFilterChange) then FonFilterChange(Slot.value);
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
        FFilterNames.Add(tvp.tp[i].text);
     end;
     if Assigned(FonFilterNameChange) then FonFilterNameChange(self);
 end;
end;

procedure T_indiwheel.NewSwitch(svp: ISwitchVectorProperty);
begin
//  writeln('NewSwitch: '+svp.name);
end;

procedure T_indiwheel.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indiwheel.SetFilter(num:integer);
begin
if (WheelSlot<>nil)and(num>0) then begin;
  if num>Slot.max then num:=round(Slot.max);
  if num<Slot.min then num:=round(Slot.min);
  Slot.value:=num;
  indiclient.sendNewNumber(WheelSlot);
  indiclient.WaitBusy(WheelSlot);
end;
end;

function  T_indiwheel.GetFilter:integer;
begin
if WheelSlot<>nil then begin;
  result:=round(Slot.value);
  if result>Slot.max then result:=round(Slot.max);
  if result<Slot.min then result:=round(Slot.min);
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

procedure T_indiwheel.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

end.

