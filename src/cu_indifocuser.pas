unit cu_indifocuser;

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

T_indifocuser = class(TIndiBaseClient)
 private
   InitTimer: TTimer;
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
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonPositionChange: TNotifyNum;
   FonTimerChange: TNotifyNum;
   FonSpeedChange: TNotifyNum;
   FonDestroy: TNotifyEvent;
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
   procedure SetPosition(p:integer);
   function  GetPosition:integer;
   procedure SetRelPosition(p:integer);
   function  GetRelPosition:integer;
   procedure SetSpeed(p:integer);
   function  GetSpeed:integer;
   procedure SetTimer(p:integer);
   function  GetTimer:integer;
   function  GethasAbsolutePosition: boolean;
   function  GethasRelativePosition: boolean;
   function  GethasTimerSpeed: boolean;
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect;
   Procedure Disconnect;
   procedure FocusIn;
   procedure FocusOut;
   property indiserver: string read Findiserver write Findiserver;
   property indiserverport: string read Findiserverport write Findiserverport;
   property indidevice: string read Findidevice write Findidevice;
   property indideviceport: string read Findideviceport write Findideviceport;
   property hasAbsolutePosition: boolean read GethasAbsolutePosition;
   property hasRelativePosition: boolean read GethasRelativePosition;
   property hasTimerSpeed: boolean read GethasTimerSpeed;
   property Position: integer read GetPosition write SetPosition;
   property RelPosition: integer read GetRelPosition write SetRelPosition;
   property Speed: integer read GetSpeed write SetSpeed;
   property Timer: integer read GetTimer write SetTimer;
   property Status: TDeviceStatus read FStatus;
   property onDestroy: TNotifyEvent read FonDestroy write FonDestroy;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onPositionChange: TNotifyNum read FonPositionChange write FonPositionChange;
   property onSpeedChange: TNotifyNum read FonSpeedChange write FonSpeedChange;
   property onTimerChange: TNotifyNum read FonTimerChange write FonTimerChange;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
end;

implementation

constructor T_indifocuser.Create;
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

destructor  T_indifocuser.Destroy;
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
 if InitTimer<>nil then FreeAndNil(InitTimer);
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
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indifocuser.CheckStatus;
begin
    if Fconnected and
       (FocusMotion<>nil) and
       (FocusAbsolutePosition<>nil)
    then begin
       FStatus := devConnected;
      if (not Fready) and Assigned(FonStatusChange) then FonStatusChange(self);
      Fready:=true;
    end;
end;

procedure T_indifocuser.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_indifocuser.Connect;
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

procedure T_indifocuser.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (FocuserDevice=nil)or(not Fready) then begin
     msg('No response from server');
     msg('Is "'+indidevice+'" a running focuser driver?');
     Disconnect;
  end;
end;

Procedure T_indifocuser.Disconnect;
begin
Terminate;
ClearStatus;
end;

procedure T_indifocuser.ServerConnected(Sender: TObject);
begin
   if (Focuserport<>nil)and(Findideviceport<>'') then begin
      Focuserport.tp[0].text:=Findideviceport;
      sendNewText(Focuserport);
   end;
   connectDevice(Findidevice);
end;

procedure T_indifocuser.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Focuser server disconnected');
end;

procedure T_indifocuser.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     FocuserDevice:=dp;
  end;
end;

procedure T_indifocuser.NewMessage(txt: string);
begin
  msg(txt);
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
begin
if FocusAbsolutePosition<>nil then begin
  FocusAbsolutePosition.np[0].value:=p;
  sendNewNumber(FocusAbsolutePosition);
  WaitBusy(FocusAbsolutePosition);
end;
end;

function  T_indifocuser.GetPosition:integer;
begin
if FocusAbsolutePosition<>nil then begin;
  result:=round(FocusAbsolutePosition.np[0].value);
end
else result:=0;
end;

procedure T_indifocuser.SetRelPosition(p:integer);
begin
if FocusRelativePosition<>nil then begin
  FocusRelativePosition.np[0].value:=p;
  sendNewNumber(FocusRelativePosition);
  WaitBusy(FocusRelativePosition);
end;
end;

function  T_indifocuser.GetRelPosition:integer;
begin
if FocusRelativePosition<>nil then begin;
  result:=round(FocusRelativePosition.np[0].value);
end
else result:=0;
end;


procedure T_indifocuser.SetSpeed(p:integer);
begin
if (FocusSpeed<>nil)and(FocusSpeed.np[0].value<>p) then begin
  FocusSpeed.np[0].value:=p;
  sendNewNumber(FocusSpeed);
  WaitBusy(FocusSpeed);
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
  sendNewNumber(FocusTimer);
  WaitBusy(FocusTimer);
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
   sendNewSwitch(FocusMotion);
   WaitBusy(FocusMotion);
 end;
end;

procedure T_indifocuser.FocusOut;
begin
 if (FocusMotion<>nil)and(FocusOutward.s=ISS_OFF) then begin
   IUResetSwitch(FocusMotion);
   FocusOutward.s:=ISS_ON;
   sendNewSwitch(FocusMotion);
   WaitBusy(FocusMotion);
 end;
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

end.

