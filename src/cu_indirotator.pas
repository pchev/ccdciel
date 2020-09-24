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

uses cu_rotator, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indirotator = class(T_rotator)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   ReadyTimer: TTimer;
   RotatorDevice: Basedevice;
   RotatorAngle: INumberVectorProperty;
   RotatorAbort: ISwitchVectorProperty;
   RotatorReverse: ISwitchVectorProperty;
   RotatorReverseEnable,RotatorReverseDisable: ISwitch;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice: string;
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
   procedure SetAngle(p:double); override;
   function  GetAngle:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetDriverReverse:boolean; override;
   procedure SetDriverReverse(value:boolean); override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   Procedure Disconnect; override;
   Procedure Halt; override;

end;

implementation

procedure T_indirotator.CreateIndiClient;
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

constructor T_indirotator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FRotatorInterface:=INDI;
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
 ReadyTimer:=TTimer.Create(nil);
 ReadyTimer.Enabled:=false;
 ReadyTimer.Interval:=2000;
 ReadyTimer.OnTimer:=@ReadyTimerTimer;
end;

destructor  T_indirotator.Destroy;
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

procedure T_indirotator.ClearStatus;
begin
    RotatorDevice:=nil;
    RotatorAngle:=nil;
    RotatorAbort:=nil;
    RotatorReverse:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    FCalibrationAngle:=0;
    FReverse:=False;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indirotator.CheckStatus;
begin
    if Fconnected and
       ((configprop<>nil)or(not FAutoloadConfig)) and
       (RotatorAngle<>nil)
    then begin
      ReadyTimer.Enabled := false;
      ReadyTimer.Enabled := true;
    end;
end;

procedure T_indirotator.ReadyTimerTimer(Sender: TObject);
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

Procedure T_indirotator.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Fdevice:=cp3;
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg(' Rotator already connected',0);
end;

procedure T_indirotator.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (RotatorDevice=nil)or(not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running rotator driver?',0);
    end
    else if (configprop=nil) then
       msg('Rotator '+Findidevice+' Missing property CONFIG_PROCESS',0)
    else if (RotatorAngle=nil) then
       msg('Rotator '+Findidevice+' Missing property ABS_ROTATOR_ANGLE',0);
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
  if (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indirotator.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,0);
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

procedure T_indirotator.NewMessage(mp: IMessage);
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.free;
end;

procedure T_indirotator.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    TxtProp: ITextVectorProperty;
    Txt: IText;
    buf: string;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DRIVER_INFO') then begin
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
  else if (proptype=INDI_NUMBER)and(RotatorAngle=nil)and(propname='ABS_ROTATOR_ANGLE') then begin
     RotatorAngle:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(RotatorReverse=nil)and(propname='ROTATOR_REVERSE') then begin
     RotatorReverse:=indiProp.getSwitch;
     RotatorReverseEnable:=IUFindSwitch(RotatorReverse,'REVERSE_ENABLED');
     RotatorReverseDisable:=IUFindSwitch(RotatorReverse,'REVERSE_DISABLED');
     if (RotatorReverseEnable=nil)or(RotatorReverseDisable=nil) then RotatorReverse:=nil;
  end
  else if (proptype=INDI_SWITCH)and(RotatorAbort=nil)and(propname='ROTATOR_ABORT_MOTION') then begin
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
var sw: ISwitch;
begin
  if (svp.name='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      Disconnect;
    end;
  end;
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
  if indiclient<>nil then indiclient.Timeout:=FTimeOut;
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

