unit cu_indiwheel;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

interface

uses indibaseclient, indibasedevice, indiapi, indicom,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indiwheel = class(TIndiBaseClient)
 private
   InitTimer: TTimer;
   WheelDevice: Basedevice;
   Wheelport: ITextVectorProperty;
   WheelSlot: INumberVectorProperty;
   Slot: INumber;
   FilterName: ITextVectorProperty;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonFilterChange: TNotifyNum;
   FonFilterNameChange: TNotifyEvent;
   FonDestroy: TNotifyEvent;
   FFilterNames: TStringList;
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
   procedure ServerConnected(Sender: TObject);
   procedure ServerDisconnected(Sender: TObject);
   procedure SetFilter(num:integer);
   function  GetFilter:integer;
   procedure SetFilterNames(value:TStringList);
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect;
   Procedure Disconnect;
   property indiserver: string read Findiserver write Findiserver;
   property indiserverport: string read Findiserverport write Findiserverport;
   property indidevice: string read Findidevice write Findidevice;
   property indideviceport: string read Findideviceport write Findideviceport;
   property Filter: integer read GetFilter write SetFilter;
   property FilterNames: TStringList read FFilterNames write SetFilterNames;
   property Status: TDeviceStatus read FStatus;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onFilterChange: TNotifyNum read FonFilterChange write FonFilterChange;
   property onFilterNameChange: TNotifyEvent read FonFilterNameChange write FonFilterNameChange;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
   property onDestroy: TNotifyEvent read FonDestroy write FonDestroy;
end;

implementation

constructor T_indiwheel.Create;
begin
 inherited Create;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 FFilterNames:=TStringList.Create;
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=10000;
 InitTimer.OnTimer:=@InitTimerTimer;
 onNewDevice:=@NewDevice;
 onNewMessage:=@NewMessage;
 onNewProperty:=@NewProperty;
 onNewNumber:=@NewNumber;
 onNewText:=@NewText;
 onNewSwitch:=@NewSwitch;
 onNewLight:=@NewLight;
 onServerConnected:=@ServerConnected;
 onServerDisconnected:=@ServerDisconnected;
end;

destructor  T_indiwheel.Destroy;
begin
 if assigned(FonDestroy) then FonDestroy(self);
 onNewDevice:=nil;
 onNewMessage:=nil;
 onNewProperty:=nil;
 onNewNumber:=nil;
 onNewText:=nil;
 onNewSwitch:=nil;
 onNewLight:=nil;
 onNewBlob:=nil;
 onServerConnected:=nil;
 onServerDisconnected:=nil;
 if FFilterNames<>nil then FreeAndNil(FFilterNames);
 if InitTimer<>nil then FreeAndNil(InitTimer);
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

Procedure T_indiwheel.Connect;
begin
if not Connected then begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  SetServer(indiserver,indiserverport);
  watchDevice(indidevice);
  ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg('Already connected');
end;

procedure T_indiwheel.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (WheelDevice=nil)or(not Fready) then begin
     msg('No response from server');
     msg('Is "'+indidevice+'" a running wheel driver?');
     Disconnect;
  end;
end;

Procedure T_indiwheel.Disconnect;
begin
Terminate;
ClearStatus;
end;

procedure T_indiwheel.ServerConnected(Sender: TObject);
begin
   if (Wheelport<>nil)and(Findideviceport<>'') then begin
      Wheelport.tp[0].text:=Findideviceport;
      sendNewText(Wheelport);
   end;
   connectDevice(Findidevice);
end;

procedure T_indiwheel.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Filter wheel server disconnected');
end;

procedure T_indiwheel.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     WheelDevice:=dp;
  end;
end;

procedure T_indiwheel.NewMessage(txt: string);
begin
  msg(txt);
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
if WheelSlot<>nil then begin;
  Slot.value:=num;
  sendNewNumber(WheelSlot);
  WaitBusy(WheelSlot);
end;
end;

function  T_indiwheel.GetFilter:integer;
begin
if WheelSlot<>nil then begin;
  result:=round(Slot.value);
end
else result:=0;
end;

procedure T_indiwheel.SetFilterNames(value:TStringList);
var i:integer;
begin
if (FilterName<>nil)and(value.Count=FilterName.ntp) then begin
  for i:=0 to value.Count-1 do begin
     FilterName.tp[i].text:=value[i];
  end;
  sendNewText(FilterName);
  WaitBusy(FilterName);
end;
end;

end.

