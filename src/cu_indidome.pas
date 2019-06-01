unit cu_indidome;

{$mode objfpc}{$H+}

{
Copyright (C) 2018 Patrick Chevalley

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

uses cu_dome, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

type

T_indidome = class(T_dome)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   DomeDevice: Basedevice;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   DomeShutterProp: ISwitchVectorProperty;
   DomeShutterOpen,DomeShutterClose: ISwitch;
   DomeParkProp: ISwitchVectorProperty;
   DomePark,DomeUnpark: ISwitch;
   DomeAutosyncProp: ISwitchVectorProperty;
   DomeAutosyncEnable,DomeAutosyncDisable: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice: string;
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
   Procedure Disconnect; override;

end;

implementation

procedure T_indidome.CreateIndiClient;
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

constructor T_indidome.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FDomeInterface:=INDI;
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
 CreateIndiClient;
end;

destructor  T_indidome.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 inherited Destroy;
end;

procedure T_indidome.ClearStatus;
begin
    DomeDevice:=nil;
    configprop:=nil;
    DomeShutterProp:=nil;
    DomeParkProp:=nil;
    DomeAutosyncProp:=nil;
    Fready:=false;
    Fconnected := false;
    FhasPark:=false;
    FhasSlaving:=false;
    FhasShutter:=false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indidome.CheckStatus;
begin
    if Fconnected and
       (configprop<>nil) and
       ((DomeShutterProp<>nil)or(DomeParkProp<>nil))  // rolloff define only Park
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

Procedure T_indidome.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
if (indiclient=nil)or(indiclient.Terminated) then CreateIndiClient;
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
else msg(' Dome already connected',0);
end;

procedure T_indidome.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (DomeDevice=nil)or(not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running dome driver?',0);
    end
    else begin
      if (configprop=nil) then msg('Dome '+Findidevice+' Missing property CONFIG_PROCESS',0);
      if (DomeShutterProp=nil) then msg('Dome '+Findidevice+' Missing property DOME_SHUTTER',0);
      if (DomeParkProp=nil) then msg('Dome '+Findidevice+' Missing property DOME_PARK',0);
      if (DomeAutosyncProp=nil) then msg('Dome '+Findidevice+' Missing property DOME_AUTOSYNC',0);
    end;
    Disconnect;
  end;
end;

Procedure T_indidome.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
ClearStatus;
end;

procedure T_indidome.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indidome.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
  indiclient.connectDevice(Findidevice);
end;

procedure T_indidome.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,0);
  CreateIndiClient;
end;

procedure T_indidome.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     DomeDevice:=dp;
  end;
end;

procedure T_indidome.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indidome.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indidome.NewMessage(mp: IMessage);
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.free;
end;

procedure T_indidome.NewProperty(indiProp: IndiProperty);
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
  else if (proptype=INDI_SWITCH)and(DomeShutterProp=nil)and(propname='DOME_SHUTTER') then begin
     DomeShutterProp:=indiProp.getSwitch;
     DomeShutterOpen:=IUFindSwitch(DomeShutterProp,'SHUTTER_OPEN');
     DomeShutterClose:=IUFindSwitch(DomeShutterProp,'SHUTTER_CLOSE');
     if (DomeShutterOpen=nil)or(DomeShutterClose=nil) then DomeShutterProp:=nil;
     FhasShutter:=DomeShutterProp<>nil
  end
  else if (proptype=INDI_SWITCH)and(DomeParkProp=nil)and(propname='DOME_PARK') then begin
     DomeParkProp:=indiProp.getSwitch;
     DomePark:=IUFindSwitch(DomeParkProp,'PARK');
     DomeUnpark:=IUFindSwitch(DomeParkProp,'UNPARK');
     if (DomePark=nil)or(DomeUnpark=nil) then DomeParkProp:=nil;
     FhasPark:=DomeParkProp<>nil;
  end
  else if (proptype=INDI_SWITCH)and(DomeAutosyncProp=nil)and(propname='DOME_AUTOSYNC') then begin
     DomeAutosyncProp:=indiProp.getSwitch;
     DomeAutosyncEnable:=IUFindSwitch(DomeAutosyncProp,'DOME_AUTOSYNC_ENABLE');
     DomeAutosyncDisable:=IUFindSwitch(DomeAutosyncProp,'DOME_AUTOSYNC_DISABLE');
     if (DomeAutosyncEnable=nil)or(DomeAutosyncDisable=nil) then DomeAutosyncProp:=nil;
     FhasSlaving:=DomeAutosyncProp<>nil;
  end;
  CheckStatus;
end;

procedure T_indidome.NewNumber(nvp: INumberVectorProperty);
begin
end;

procedure T_indidome.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indidome.NewSwitch(svp: ISwitchVectorProperty);
var sw: ISwitch;
begin
  if (svp.name='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      Disconnect;
    end;
  end;
  if svp=DomeShutterProp then begin
     if Assigned(FonShutterChange) then FonShutterChange(self);
  end;
  if (svp=DomeParkProp)and(DomeShutterProp=nil) then begin
     if Assigned(FonShutterChange) then FonShutterChange(self);
  end;
  if svp=DomeAutosyncProp then begin
     if Assigned(FonSlaveChange) then FonSlaveChange(self);
  end;
end;

procedure T_indidome.NewLight(lvp: ILightVectorProperty);
begin
end;

procedure T_indidome.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

procedure T_indidome.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

function T_indidome.GetPark: boolean;
begin
 result:=false;
 if DomeParkProp<>nil then begin
   result := (DomePark.s=ISS_ON);
 end;
end;

procedure T_indidome.SetPark(value:boolean);
begin
 if DomeParkProp<>nil then begin
    IUResetSwitch(DomeParkProp);
    if value then
       DomePark.s:=ISS_ON
    else
       DomeUnpark.s:=ISS_ON;
    indiclient.sendNewSwitch(DomeParkProp);
    indiclient.WaitBusy(DomeParkProp,60000);
 end;
end;

function T_indidome.GetShutter: boolean;
begin
 result:=false;
 if DomeShutterProp<>nil then begin
   result := (DomeShutterOpen.s=ISS_ON);
 end
 else
   result := not GetPark;
end;

procedure T_indidome.SetShutter(value:boolean);
begin
 if DomeShutterProp<>nil then begin
    IUResetSwitch(DomeShutterProp);
    if value then
      DomeShutterOpen.s:=ISS_ON
    else
      DomeShutterClose.s:=ISS_ON;
    indiclient.sendNewSwitch(DomeShutterProp);
    indiclient.WaitBusy(DomeShutterProp,60000);
 end
 else begin
    SetPark(not value);
 end;
end;

function T_indidome.GetSlave: boolean;
begin
 result:=false;
 if DomeAutosyncProp<>nil then begin
   result := (DomeAutosyncEnable.s=ISS_ON);
 end;
end;

procedure T_indidome.SetSlave(value:boolean);
begin
 if DomeAutosyncProp<>nil then begin
    IUResetSwitch(DomeAutosyncProp);
    if value then
      DomeAutosyncEnable.s:=ISS_ON
    else
      DomeAutosyncDisable.s:=ISS_ON;
    indiclient.sendNewSwitch(DomeAutosyncProp);
    indiclient.WaitBusy(DomeAutosyncProp,60000);
 end;
end;


end.
