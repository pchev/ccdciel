unit cu_indiswitch;

{$mode objfpc}{$H+}

{
Copyright (C) 2021 Patrick Chevalley

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

uses cu_switch, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, ExtCtrls, Forms, Classes, SysUtils;

const MaxSwitchProp = 100;

type

T_indiswitch = class(T_switch)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   ReadyTimer: TTimer;
   SwitchDevice: Basedevice;
   connectprop: ISwitchVectorProperty;
   connecton,connectoff: ISwitch;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
   Fready,Fconnected,FConnectDevice: boolean;
   Findiserver, Findiserverport, Findidevice: string;
   SwitchPropList: array[0..MaxSwitchProp] of TObject;
   NumSwitchProp: integer;
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
   function GetSwitch: TSwitchList; override;
   procedure SetSwitch(value: TSwitchList); override;
   procedure SetTimeout(num:integer); override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');  override;
   Procedure Disconnect; override;
end;

implementation

procedure T_indiswitch.CreateIndiClient;
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

constructor T_indiswitch.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FSwitchInterface:=INDI;
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
end;

destructor  T_indiswitch.Destroy;
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

procedure T_indiswitch.ClearStatus;
var i: integer;
begin
    SwitchDevice:=nil;
    NumSwitchProp:=0;
    for i:=0 to MaxSwitchProp do SwitchPropList[i]:=nil;
    connectprop:=nil;
    configprop:=nil;
    Fready:=false;
    Fconnected := false;
    FConnectDevice:=false;
    FStatus := devDisconnected;
    FNumSwitch:=0;
    SetLength(FSwitch,FNumSwitch);
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indiswitch.CheckStatus;
begin
    if Fconnected and
       (NumSwitchProp>0)
    then begin
      ReadyTimer.Enabled := false;
      ReadyTimer.Enabled := true;
    end;
end;

procedure T_indiswitch.ReadyTimerTimer(Sender: TObject);
begin
  ReadyTimer.Enabled := false;
  FStatus := devConnected;
  if (not Fready) then begin
    Fready:=true;
    if FAutoloadConfig and FConnectDevice then LoadConfig;
    if Assigned(FonStatusChange) then FonStatusChange(self);
  end;
end;

Procedure T_indiswitch.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
begin
CreateIndiClient;
if not indiclient.Connected then begin
  Findiserver:=cp1;
  Findiserverport:=cp2;
  Findidevice:=cp3;
  Fdevice:=cp3;
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Connecting to INDI server "'+Findiserver+':'+Findiserverport+'" for device "'+Findidevice+'"',9);
  indiclient.SetServer(Findiserver,Findiserverport);
  indiclient.watchDevice(Findidevice);
  indiclient.ConnectServer;
  FStatus := devConnecting;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  InitTimer.Enabled:=true;
end
else msg(' Switch already connected',0);
end;

procedure T_indiswitch.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (SwitchDevice=nil)or(not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running switch driver?',0);
    end
    else if (configprop=nil) then
       msg('Switch '+Findidevice+' Missing property CONFIG_PROCESS',0)
    else if (NumSwitchProp=0) then
       msg('Switch '+Findidevice+' No switch found',0);
    Disconnect;
  end;
end;

Procedure T_indiswitch.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
try
if (indiclient<>nil)and(not indiclient.Terminated) then
  indiclient.Terminate;
except
end;
ClearStatus;
end;

procedure T_indiswitch.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indiswitch.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (connectprop<>nil) then begin
    if (connectoff.s=ISS_ON) then begin
      FConnectDevice:=true;
      indiclient.connectDevice(Findidevice);
      exit;
    end;
  end
  else begin
    ConnectTimer.Enabled:=true;
    exit;
  end;
end;

procedure T_indiswitch.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,1);
end;

procedure T_indiswitch.NewDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     msg('INDI server send new device: "'+dp.getDeviceName+'"',9);
     Fconnected:=true;
     SwitchDevice:=dp;
  end;
end;

procedure T_indiswitch.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indiswitch.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indiswitch.NewMessage(mp: IMessage);
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.free;
end;

procedure T_indiswitch.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    TxtProp: ITextVectorProperty;
    SwProp: ISwitchVectorProperty;
    NumProp: INumberVectorProperty;
    Txt: IText;
    buf: string;
    i,j: integer;
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
  else if (proptype=INDI_SWITCH)and(connectprop=nil)and(propname='CONNECTION') then begin
     connectprop:=indiProp.getSwitch;
     connecton:=IUFindSwitch(connectprop,'CONNECT');
     connectoff:=IUFindSwitch(connectprop,'DISCONNECT');
     if (connecton=nil)or(connectoff=nil) then connectprop:=nil;
  end
  else if (proptype=INDI_SWITCH)and(configprop=nil)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     if (configload=nil)or(configsave=nil) then configprop:=nil;
  end
  else if (propname='DEBUG')or(propname='SIMULATION')or(propname='POLLING_PERIOD')or(propname='CONNECTION_MODE')or(propname='DEVICE_PORT')or
       (propname='DEVICE_BAUD_RATE')or(propname='DEVICE_AUTO_SEARCH')or(propname='DEVICE_PORT_SCAN')or(propname='FIRMWARE_INFO')
  then begin
      // ignore this properties
      // everything else is considered as a switch
  end
  else if (proptype=INDI_SWITCH) and (NumSwitchProp<MaxSwitchProp)  then begin
     SwProp:=indiProp.getSwitch;
     if (SwProp.r=ISR_NOFMANY) and ((SwProp.p=IP_RO)or(SwProp.p=IP_RW))
     then begin
       inc(NumSwitchProp);
       SwitchPropList[NumSwitchProp]:=SwProp;
       j:=ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).nsp;
       SetLength(FSwitch,FNumSwitch+j);
       for i:=0 to j-1 do begin
         FSwitch[FNumSwitch+i].Name:=ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).group+'; '+ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).lbl+'; '+ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).sp[i].lbl;
         FSwitch[FNumSwitch+i].IndiType:=INDI_SWITCH;
         FSwitch[FNumSwitch+i].IndiProp:=NumSwitchProp;
         FSwitch[FNumSwitch+i].IndiIndex:=i;
         FSwitch[FNumSwitch+i].CanWrite:=ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).p<>IP_RO;
         FSwitch[FNumSwitch+i].MultiState:=false;
         FSwitch[FNumSwitch+i].Min:=0;
         FSwitch[FNumSwitch+i].Max:=0;
         FSwitch[FNumSwitch+i].Step:=0;
         FSwitch[FNumSwitch+i].Value:=0;
         FSwitch[FNumSwitch+i].Checked:=ISwitchVectorProperty(SwitchPropList[NumSwitchProp]).sp[i].s=ISS_ON;
       end;
       FNumSwitch:=FNumSwitch+j;
     end;
  end
  else if (proptype=INDI_NUMBER) and (NumSwitchProp<MaxSwitchProp) then begin
     NumProp:=indiProp.getNumber;
     if ((NumProp.p=IP_RO)or(NumProp.p=IP_RW))
     then begin
       inc(NumSwitchProp);
       SwitchPropList[NumSwitchProp]:=NumProp;
       j:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).nnp;
       SetLength(FSwitch,FNumSwitch+j);
       for i:=0 to j-1 do begin
         FSwitch[FNumSwitch+i].Name:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).group+'; '+INumberVectorProperty(SwitchPropList[NumSwitchProp]).lbl+'; '+INumberVectorProperty(SwitchPropList[NumSwitchProp]).np[i].lbl;
         FSwitch[FNumSwitch+i].IndiType:=INDI_NUMBER;
         FSwitch[FNumSwitch+i].IndiProp:=NumSwitchProp;
         FSwitch[FNumSwitch+i].IndiIndex:=i;
         FSwitch[FNumSwitch+i].CanWrite:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).p<>IP_RO;
         FSwitch[FNumSwitch+i].MultiState:=true;
         FSwitch[FNumSwitch+i].Min:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).np[i].min;
         FSwitch[FNumSwitch+i].Max:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).np[i].max;
         FSwitch[FNumSwitch+i].Step:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).np[i].step;
         FSwitch[FNumSwitch+i].Value:=INumberVectorProperty(SwitchPropList[NumSwitchProp]).np[i].Value;
         FSwitch[FNumSwitch+i].Checked:=FSwitch[FNumSwitch+i].Value<>0;
       end;
       FNumSwitch:=FNumSwitch+j;
     end;
  end;

  CheckStatus;
end;

procedure T_indiswitch.NewNumber(nvp: INumberVectorProperty);
var i: integer;
begin
  for i:=1 to NumSwitchProp do begin
    if nvp=INumberVectorProperty(SwitchPropList[i]) then begin
      if Assigned(FonSwitchChange) then FonSwitchChange(self);
    end;
  end;
end;

procedure T_indiswitch.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indiswitch.NewSwitch(svp: ISwitchVectorProperty);
var sw: ISwitch;
    i: integer;
begin
  if (svp.name='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      Disconnect;
    end;
  end
  else begin
    for i:=1 to NumSwitchProp do begin
      if svp=ISwitchVectorProperty(SwitchPropList[i]) then begin
        if Assigned(FonSwitchChange) then FonSwitchChange(self);
      end;
    end;
  end;
end;

procedure T_indiswitch.NewLight(lvp: ILightVectorProperty);
begin
end;

function  T_indiswitch.GetSwitch:TSwitchList;
var i: integer;
begin
 SetLength(result,FNumSwitch);
 if (NumSwitchProp=0)or(FNumSwitch=0) then exit;
 for i:=0 to FNumSwitch-1 do begin
   result[i].Name       := FSwitch[i].Name;
   result[i].IndiType   := FSwitch[i].IndiType;
   result[i].IndiProp   := FSwitch[i].IndiProp;
   result[i].IndiIndex  := FSwitch[i].IndiIndex;
   result[i].CanWrite   := FSwitch[i].CanWrite;
   result[i].MultiState := FSwitch[i].MultiState;
   result[i].Min        := FSwitch[i].Min;
   result[i].Max        := FSwitch[i].Max;
   result[i].Step       := FSwitch[i].Step;
   case FSwitch[i].IndiType of
     INDI_SWITCH : begin
       result[i].Value      := FSwitch[i].Value;
       result[i].Checked    :=(ISwitchVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).sp[FSwitch[i].IndiIndex].s=ISS_ON);
     end;
     INDI_NUMBER : begin
       result[i].Value      := INumberVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).np[FSwitch[i].IndiIndex].Value;
       result[i].Checked    := result[i].Value<>0;
     end;
   end;
 end;
end;

procedure T_indiswitch.SetSwitch(value: TSwitchList);
var i,j: integer;
    modified: boolean;
    t: INDI_TYPE;
begin
 if (NumSwitchProp=0)or(FNumSwitch=0) then exit;
 j:=1;
 modified:=false;
 t:=INDI_SWITCH;
 for i:=0 to FNumSwitch-1 do begin
   if FSwitch[i].IndiProp<>j then begin
     if modified then
       case t of
         INDI_SWITCH: indiclient.sendNewSwitch(ISwitchVectorProperty(SwitchPropList[j]));
         INDI_NUMBER: indiclient.sendNewNumber(INumberVectorProperty(SwitchPropList[j]));
       end;
     modified:=false;
     j:=FSwitch[i].IndiProp;
   end;
   t:=FSwitch[i].IndiType;
   case t of
     INDI_SWITCH: begin
         modified:=modified or ( value[i].Checked <> (ISwitchVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).sp[FSwitch[i].IndiIndex].s=ISS_ON) );
         if value[i].Checked then
           ISwitchVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).sp[FSwitch[i].IndiIndex].s:=ISS_ON
         else
           ISwitchVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).sp[FSwitch[i].IndiIndex].s:=ISS_OFF;
     end;
     INDI_NUMBER: begin
         modified:=modified or ( value[i].Value <> (INumberVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).np[FSwitch[i].IndiIndex].Value) );
         INumberVectorProperty(SwitchPropList[FSwitch[i].IndiProp]).np[FSwitch[i].IndiIndex].Value := value[i].Value;
     end;
   end;
 end;
 if modified then
   case t of
     INDI_SWITCH: indiclient.sendNewSwitch(ISwitchVectorProperty(SwitchPropList[j]));
     INDI_NUMBER: indiclient.sendNewNumber(INumberVectorProperty(SwitchPropList[j]));
   end;
end;

procedure T_indiswitch.SetTimeout(num:integer);
begin
 FTimeOut:=num;
  if indiclient<>nil then indiclient.Timeout:=FTimeOut;
end;

procedure T_indiswitch.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

end.

