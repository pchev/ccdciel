unit cu_indimount;

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

uses cu_mount, indibaseclient, indibasedevice, indiapi, indicom,
     u_global, u_utils, ExtCtrls, Forms, Classes, SysUtils;

type

T_indimount = class(T_mount)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   MountDevice: Basedevice;
   Mountport: ITextVectorProperty;
   coord_prop: INumberVectorProperty;
   coord_ra:   INumber;
   coord_dec:  INumber;
   CoordSet: ISwitchVectorProperty;
   CoordSetTrack,CoordSetSlew,CoordSetSync: ISwitch;
   parkprop: ISwitchVectorProperty;
   swpark,swunpark: ISwitch;
   AbortmotionProp: ISwitchVectorProperty;
   TelescopeInfo: INumberVectorProperty;
   TelescopeAperture, TelescopeFocale: INumber;
   eod_coord:  boolean;
   configprop: ISwitchVectorProperty;
   configload,configsave,configdefault: ISwitch;
   SyncManage: ISwitchVectorProperty;
   SyncClearDelta: ISwitch;
   AlignList: ISwitchVectorProperty;
   AlignListClear: ISwitch;
   AlignSyncMode: ISwitchVectorProperty;
   AlignStdSync,AlignAppendSync: ISwitch;
   AlignMode: ISwitchVectorProperty;
   AlignNo,AlignNearest,AlignNstar: ISwitch;
   Pier_Side: ISwitchVectorProperty;
   Pier_East,Pier_West: ISwitch;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure ClearStatus;
   procedure CheckStatus;
   procedure NewDevice(dp: Basedevice);
   procedure NewMessage(mp:IMessage);
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
   procedure SetPark(value:Boolean); override;
   function  GetPark:Boolean; override;
   function  GetRA:double; override;
   function  GetDec:double; override;
   function  GetPierSide: TPierSide; override;
   function  GetEquinox: double;  override;
   function  GetAperture:double;  override;
   function  GetFocaleLength:double; override;
   procedure SetTimeout(num:integer); override;
   function  GetSyncMode:TEqmodAlign; override;
   procedure SetSyncMode(value:TEqmodAlign); override;
   function GetMountSlewing:boolean; override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''); override;
   Procedure Disconnect; override;
   function FlipMeridian:boolean; override;
   function Slew(sra,sde: double):boolean; override;
   function SlewAsync(sra,sde: double):boolean; override;
   function Sync(sra,sde: double):boolean; override;
   function Track:boolean; override;
   procedure AbortMotion; override;
   function ClearAlignment:boolean; override;
   function ClearDelta:boolean; override;
end;

implementation

procedure T_indimount.CreateIndiClient;
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

constructor T_indimount.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FMountInterface:=INDI;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 eod_coord:=true;
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

destructor  T_indimount.Destroy;
begin
 InitTimer.Enabled:=false;
 ConnectTimer.Enabled:=false;
 indiclient.onServerDisconnected:=nil;
 indiclient.Free;
 FreeAndNil(InitTimer);
 FreeAndNil(ConnectTimer);
 inherited Destroy;
end;

procedure T_indimount.ClearStatus;
begin
    MountDevice:=nil;
    Mountport:=nil;
    TelescopeInfo:=nil;
    parkprop:=nil;
    coord_prop:=nil;
    CoordSet:=nil;
    AbortmotionProp:=nil;
    configprop:=nil;
    SyncManage:=nil;
    AlignList:=nil;
    AlignSyncMode:=nil;
    AlignMode:=nil;
    Pier_Side:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indimount.CheckStatus;
begin
    if Fconnected and
       (configprop<>nil) and
       (coord_prop<>nil)
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
    FIsEqmod:=(SyncManage<>nil)and(AlignList<>nil)and(AlignSyncMode<>nil)and(AlignMode<>nil);
 end;

procedure T_indimount.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(Findidevice+': '+txt);
end;

Procedure T_indimount.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string='');
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
else msg('Mount already connected');
end;

procedure T_indimount.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (MountDevice=nil)or(not Fready) then begin
    msg('Error');
    if not Fconnected then begin
      msg('No response from server');
      msg('Is "'+Findidevice+'" a running telescope mount driver?');
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS')
    else if (coord_prop=nil) then
       msg('Missing property EQUATORIAL_EOD_COORD or EQUATORIAL_COORD');
    Disconnect;
  end;
end;

Procedure T_indimount.Disconnect;
begin
InitTimer.Enabled:=False;
ConnectTimer.Enabled:=False;
indiclient.Terminate;
ClearStatus;
end;

procedure T_indimount.ServerConnected(Sender: TObject);
begin
   ConnectTimer.Enabled:=True;
end;

procedure T_indimount.ConnectTimerTimer(Sender: TObject);
begin
  ConnectTimer.Enabled:=False;
  if (Mountport=nil) and (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
  if (Mountport<>nil)and(Findideviceport<>'') then begin
     Mountport.tp[0].text:=Findideviceport;
     indiclient.sendNewText(Mountport);
     msg('Set port '+Findideviceport);
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indimount.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Mount server disconnected');
  CreateIndiClient;
end;

procedure T_indimount.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     MountDevice:=dp;
  end;
end;

procedure T_indimount.DeleteDevice(dp: Basedevice);
begin
  if dp.getDeviceName=Findidevice then begin
     Disconnect;
  end;
end;

procedure T_indimount.DeleteProperty(indiProp: IndiProperty);
begin
  { TODO :  check if a vital property is removed ? }
end;

procedure T_indimount.NewMessage(mp:IMessage);
const k=8;
  blacklist: array[1..k] of string =(
             'Timed guide','End Timed guide',   // pulse guide message
             'Starting Goto','Aligned Eqmod',   // extra eqmod goto message, must be [debug]
             'Setting Eqmod','Slewing mount',   // extra eqmod goto message, must be [debug]
             'Iterative Goto','Iterative goto'  // extra eqmod goto message, must be [debug]
             );
var ok: boolean;
    i: integer;
begin
  ok:=true;
  for i:=1 to k do begin
    if pos(blacklist[i],mp.msg)>0 then ok:=false;
  end;
  if ok then begin
     if Assigned(FonMsg) then FonMsg(Findidevice+': '+mp.msg);
  end else begin
    if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  end;
  mp.Free;
end;

procedure T_indimount.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Mountport:=indiProp.getText;
  end
  else if (proptype=INDI_SWITCH)and(propname='CONFIG_PROCESS') then begin
     configprop:=indiProp.getSwitch;
     configload:=IUFindSwitch(configprop,'CONFIG_LOAD');
     configsave:=IUFindSwitch(configprop,'CONFIG_SAVE');
     configdefault:=IUFindSwitch(configprop,'CONFIG_DEFAULT');
     if (configload=nil)or(configsave=nil)or(configdefault=nil) then configprop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='EQUATORIAL_EOD_COORD') then begin
      coord_prop:=indiProp.getNumber;
      coord_ra:=IUFindNumber(coord_prop,'RA');
      coord_dec:=IUFindNumber(coord_prop,'DEC');
      eod_coord:=true;
      if (coord_ra=nil)or(coord_dec=nil) then coord_prop:=nil;
   end
   else if (proptype=INDI_NUMBER)and(coord_prop=nil)and(propname='EQUATORIAL_COORD') then begin
      coord_prop:=indiProp.getNumber;
      coord_ra:=IUFindNumber(coord_prop,'RA');
      coord_dec:=IUFindNumber(coord_prop,'DEC');
      eod_coord:=false;
      if (coord_ra=nil)or(coord_dec=nil) then coord_prop:=nil;
   end
   else if (proptype=INDI_SWITCH)and(propname='ON_COORD_SET') then begin
      CoordSet:=indiProp.getSwitch;
      CoordSetTrack:=IUFindSwitch(CoordSet,'TRACK');
      CoordSetSlew:=IUFindSwitch(CoordSet,'SLEW');
      CoordSetSync:=IUFindSwitch(CoordSet,'SYNC');
   end
   else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_PARK') then begin
      parkprop:=indiProp.getSwitch;
      swpark:=IUFindSwitch(parkprop,'PARK');
      swunpark:=IUFindSwitch(parkprop,'UNPARK');
      if (swpark=nil)or(swunpark=nil) then parkprop:=nil;
      if Assigned(FonParkChange) then FonParkChange(self);
   end
   else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_ABORT_MOTION') then begin
      AbortmotionProp:=indiProp.getSwitch;
   end
   else if (proptype=INDI_NUMBER)and(propname='TELESCOPE_INFO') then begin
      TelescopeInfo:=indiProp.getNumber;
      TelescopeAperture:=IUFindNumber(TelescopeInfo,'TELESCOPE_APERTURE');
      TelescopeFocale:=IUFindNumber(TelescopeInfo,'TELESCOPE_FOCAL_LENGTH');
      if (TelescopeAperture=nil)or(TelescopeFocale=nil) then TelescopeInfo:=nil;
   end
   else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_PIER_SIDE') then begin
      Pier_Side:=indiProp.getSwitch;
      Pier_East:=IUFindSwitch(Pier_Side,'PIER_EAST');
      Pier_West:=IUFindSwitch(Pier_Side,'PIER_WEST');
      if (Pier_East=nil)or(Pier_West=nil) then Pier_Side:=nil;
      if Assigned(FonPiersideChange) then FonPiersideChange(self);
   end
   else if (proptype=INDI_SWITCH)and(propname='ALIGNMODE') then begin
      AlignMode:=indiProp.getSwitch;
      AlignNo:=IUFindSwitch(AlignMode,'NOALIGN');
      AlignNearest:=IUFindSwitch(AlignMode,'ALIGNNEAREST');
      AlignNstar:=IUFindSwitch(AlignMode,'ALIGNNSTAR');
      if (AlignNo=nil)or(AlignNearest=nil)or(AlignNstar=nil) then AlignMode:=nil;
   end
   else if (proptype=INDI_SWITCH)and(propname='ALIGNLIST') then begin
      AlignList:=indiProp.getSwitch;
      AlignListClear:=IUFindSwitch(AlignList,'ALIGNLISTCLEAR');
      if AlignListClear=nil then AlignList:=nil;
   end
   else if (proptype=INDI_SWITCH)and(propname='ALIGNSYNCMODE') then begin
      AlignSyncMode:=indiProp.getSwitch;
      AlignStdSync:=IUFindSwitch(AlignSyncMode,'ALIGNSTANDARDSYNC');
      AlignAppendSync:=IUFindSwitch(AlignSyncMode,'ALIGNAPPENDSYNC');
      if (AlignStdSync=nil)or(AlignAppendSync=nil) then AlignSyncMode:=nil;
   end
   else if (proptype=INDI_SWITCH)and(propname='SYNCMANAGE') then begin
      SyncManage:=indiProp.getSwitch;
      SyncClearDelta:=IUFindSwitch(SyncManage,'SYNCCLEARDELTA');
      if (SyncClearDelta=nil) then SyncManage:=nil;
   end;
   CheckStatus;
end;

procedure T_indimount.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=coord_prop then begin
     if Assigned(FonCoordChange) then FonCoordChange(self);
  end;
end;

procedure T_indimount.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indimount.NewSwitch(svp: ISwitchVectorProperty);
begin
  if svp=parkprop then begin
     if Assigned(FonParkChange) then FonParkChange(self);
  end
  else if svp=Pier_Side then begin
     if Assigned(FonPiersideChange) then FonPiersideChange(self);
  end;
end;

procedure T_indimount.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

procedure T_indimount.SetPark(value:Boolean);
begin
if parkprop<>nil then begin
 IUResetSwitch(parkprop);
 if value then
    swpark.s:=ISS_ON
 else
    swunpark.s:=ISS_ON;
 indiclient.sendNewSwitch(parkprop);
 indiclient.WaitBusy(parkprop,120000);
end;
end;

function T_indimount.GetPark:Boolean;
begin
if parkprop<>nil then begin
  result:=(swpark.s=ISS_ON);
end
else result:=false;
end;

function  T_indimount.GetRA:double;
begin
if coord_prop<>nil then begin
  result:=rmod(coord_ra.value+24,24);
end
else result:=NullCoord;
end;

function  T_indimount.GetDec:double;
begin
if coord_prop<>nil then begin
  result:=coord_dec.value;
end
else result:=NullCoord;
end;

function  T_indimount.GetPierSide:TPierSide;
begin
result:=pierUnknown;
if Pier_Side<>nil then begin
  if Pier_East.s=ISS_ON then result:=pierEast
  else if Pier_West.s=ISS_ON then result:=pierWest;
end;
end;

function  T_indimount.GetEquinox: double;
begin
 if eod_coord then result:=0
              else result:=2000;
end;

function  T_indimount.GetAperture:double;
begin
if TelescopeInfo<>nil then begin
  result:=TelescopeAperture.value;
end
else result:=-1;
end;

function  T_indimount.GetFocaleLength:double;
begin
if TelescopeInfo<>nil then begin
  result:=TelescopeFocale.value;
end
else result:=-1;
end;

function T_indimount.SlewAsync(sra,sde: double):Boolean;
var slewtimeout:integer;
begin
  result:=false;
  if (CoordSet<>nil) and (CoordSetTrack<>nil) and (coord_prop<>nil) then begin
    IUResetSwitch(CoordSet);
    CoordSetTrack.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
    indiclient.WaitBusy(CoordSet);
    coord_ra.value:=sra;
    coord_dec.value:=sde;
    indiclient.sendNewNumber(coord_prop);
    result:=true;
  end;
end;

function T_indimount.Slew(sra,sde: double):Boolean;
var slewtimeout:integer;
begin
  result:=false;
  if (CoordSet<>nil) and (CoordSetTrack<>nil) and (coord_prop<>nil) then begin
    FMountSlewing:=true;
    IUResetSwitch(CoordSet);
    CoordSetTrack.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
    indiclient.WaitBusy(CoordSet);
    if (15*abs(coord_ra.value-sra)+abs(coord_dec.value-sde))>0.5 then slewtimeout:=240000 else slewtimeout:=30000;
    coord_ra.value:=sra;
    coord_dec.value:=sde;
    indiclient.sendNewNumber(coord_prop);
    indiclient.WaitBusy(coord_prop,slewtimeout,10000);
    FMountSlewing:=false;
    result:=true;
  end;
end;

function T_indimount.GetMountSlewing:boolean;
var islewing: boolean;
begin
  if coord_prop=nil then
    islewing:=false
  else
    islewing:=coord_prop.s=IPS_BUSY;
  result:=(islewing or FMountSlewing);
end;

function T_indimount.FlipMeridian:boolean;
var sra,sde,ra1,ra2: double;
    pierside1,pierside2:TPierSide;
begin
  result:=false;
  sra:=GetRA;
  sde:=GetDec;
  pierside1:=GetPierSide;
  if pierside1=pierEast then exit; // already right side
  if (sra=NullCoord)or(sde=NullCoord) then exit;
  // point one hour to the east
  ra1:=sra+1;
  if ra1>=24 then ra1:=ra1-24;
  slew(ra1,sde);
  Wait(2);
  // point one hour to the west to force the flip
  ra2:=sra-1;
  if ra2<0 then ra2:=ra2+24;
  slew(ra2,sde);
  Wait(2);
  // return to position
  slew(sra,sde);
  Wait(2);
  // check result
  pierside2:=GetPierSide;
  result:=(pierside2<>pierside1)
end;

function T_indimount.Sync(sra,sde: double):Boolean;
begin
  result:=false;
  if (CoordSet<>nil) and (CoordSetSync<>nil) and (coord_prop<>nil) then begin
    IUResetSwitch(CoordSet);
    CoordSetSync.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
    indiclient.WaitBusy(CoordSet);
    coord_ra.value:=sra;
    coord_dec.value:=sde;
    indiclient.sendNewNumber(coord_prop);
    indiclient.WaitBusy(coord_prop);
    result:=true;
  end;
end;

function T_indimount.Track:Boolean;
begin
  result:=false;
  if (CoordSet<>nil) and (CoordSetTrack<>nil) and (coord_prop<>nil) then begin
    IUResetSwitch(CoordSet);
    CoordSetTrack.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
    indiclient.WaitBusy(CoordSet);
    indiclient.sendNewNumber(coord_prop);
    indiclient.WaitBusy(coord_prop);
    result:=true;
  end;
end;

procedure T_indimount.AbortMotion;
var ab: ISwitch;
begin
 if AbortmotionProp<>nil then begin
   ab:=IUFindSwitch(AbortmotionProp,'ABORT');
   if ab<>nil then begin
     ab.s:=ISS_ON;
     indiclient.sendNewSwitch(AbortmotionProp);
     msg('Stop telescope motion.');
   end;
 end;
end;

procedure T_indimount.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 indiclient.Timeout:=FTimeOut;
end;

procedure T_indimount.LoadConfig;
begin
  if configprop<>nil then begin
    IUResetSwitch(configprop);
    configload.s:=ISS_ON;
    indiclient.sendNewSwitch(configprop);
  end;
end;

// Eqmod specific

function  T_indimount.GetSyncMode:TEqmodAlign;
begin
  result:=alUNSUPPORTED;
  if AlignSyncMode<>nil then begin
    if AlignStdSync.s = ISS_ON then result:=alSTDSYNC
    else if AlignAppendSync.s = ISS_ON then result:=alADDPOINT;
  end;
end;

procedure T_indimount.SetSyncMode(value:TEqmodAlign);
begin
  if (AlignSyncMode<>nil)and(value<>alUNSUPPORTED) then begin
     IUResetSwitch(AlignSyncMode);
     if value=alSTDSYNC then AlignStdSync.s := ISS_ON
     else if value=alADDPOINT then AlignAppendSync.s := ISS_ON;
     indiclient.sendNewSwitch(AlignSyncMode);
  end;
end;

function T_indimount.ClearAlignment:boolean;
begin
  result:=false;
  if AlignList<>nil then begin
    IUResetSwitch(AlignList);
    AlignListClear.s:=ISS_ON;
    indiclient.sendNewSwitch(AlignList);
    result:=true;
  end;
end;

function T_indimount.ClearDelta:boolean;
begin
  result:=false;
  if SyncManage<>nil then begin
    IUResetSwitch(SyncManage);
    SyncClearDelta.s:=ISS_ON;
    indiclient.sendNewSwitch(SyncManage);
    result:=true;
  end;
end;


end.

