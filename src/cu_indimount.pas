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

uses cu_mount, indibaseclient, indibasedevice, indiapi, indicom, u_translation,
     u_global, u_utils, ExtCtrls, Forms, Classes, SysUtils;

type

T_indimount = class(T_mount)
 private
   indiclient: TIndiBaseClient;
   InitTimer: TTimer;
   ConnectTimer: TTimer;
   ReadyTimer: TTimer;
   MountDevice: Basedevice;
   coord_prop: INumberVectorProperty;
   coord_ra:   INumber;
   coord_dec:  INumber;
   CoordSet: ISwitchVectorProperty;
   CoordSetTrack,CoordSetSlew,CoordSetSync: ISwitch;
   TrackState: ISwitchVectorProperty;
   TrackOn,TrackOff: ISwitch;
   parkprop: ISwitchVectorProperty;
   swpark,swunpark: ISwitch;
   AbortmotionProp: ISwitchVectorProperty;
   TelescopeInfo: INumberVectorProperty;
   TelescopeAperture, TelescopeFocale: INumber;
   eod_coord:  boolean;
   configprop: ISwitchVectorProperty;
   configload,configsave: ISwitch;
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
   GeographicCoord_prop: INumberVectorProperty;
   geo_lat: INumber;
   geo_lon: INumber;
   geo_elev: INumber;
   DateTime_Prop: ITextVectorProperty;
   dateutc : IText;
   utcoffset : IText;
   Guide_NS: INumberVectorProperty;
   Guide_N:  INumber;
   Guide_S:  INumber;
   Guide_WE: INumberVectorProperty;
   Guide_W:  INumber;
   Guide_E:  INumber;
   Guide_Rate: INumberVectorProperty;
   Guide_Rate_NS:  INumber;
   Guide_Rate_WE:  INumber;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice: string;
   procedure CreateIndiClient;
   procedure InitTimerTimer(Sender: TObject);
   procedure ConnectTimerTimer(Sender: TObject);
   procedure ReadyTimerTimer(Sender: TObject);
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
 protected
   function  GetTracking:Boolean; override;
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
   function GetGuideRateRa: double; override;
   function GetGuideRateDe: double; override;
   procedure SetGuideRateRa(value:double); override;
   procedure SetGuideRateDe(value:double); override;
   function GetPulseGuiding: boolean; override;
 public
   constructor Create(AOwner: TComponent);override;
   destructor  Destroy; override;
   Procedure Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string=''); override;
   Procedure Disconnect; override;
   function FlipMeridian:boolean; override;
   function Slew(sra,sde: double):boolean; override;
   function SlewAsync(sra,sde: double):boolean; override;
   function Sync(sra,sde: double):boolean; override;
   function Track:boolean; override;
   procedure AbortMotion; override;
   function ClearAlignment:boolean; override;
   function ClearDelta:boolean; override;
   function GetSite(var long,lat,elev: double): boolean; override;
   function SetSite(long,lat,elev: double): boolean; override;
   function GetDate(var utc,offset: double): boolean; override;
   function SetDate(utc,offset: double): boolean; override;
   function PulseGuide(direction,duration:integer): boolean; override;
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
 eod_coord:=true;
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

destructor  T_indimount.Destroy;
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

procedure T_indimount.ClearStatus;
begin
    MountDevice:=nil;
    TelescopeInfo:=nil;
    parkprop:=nil;
    TrackState:=nil;
    coord_prop:=nil;
    CoordSet:=nil;
    AbortmotionProp:=nil;
    configprop:=nil;
    SyncManage:=nil;
    AlignList:=nil;
    AlignSyncMode:=nil;
    AlignMode:=nil;
    Pier_Side:=nil;
    GeographicCoord_prop:=nil;
    DateTime_Prop:=nil;
    Guide_NS:=nil;
    Guide_WE:=nil;
    Guide_Rate:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indimount.CheckStatus;
begin
    if Fconnected and
       ((configprop<>nil)or(not FAutoloadConfig)) and
       (coord_prop<>nil)
    then begin
      ReadyTimer.Enabled := false;
      ReadyTimer.Enabled := true;
    end;
end;

procedure T_indimount.ReadyTimerTimer(Sender: TObject);
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
  FIsEqmod:=(SyncManage<>nil)and(AlignList<>nil)and(AlignSyncMode<>nil)and(AlignMode<>nil);
end;

Procedure T_indimount.Connect(cp1: string; cp2:string=''; cp3:string=''; cp4:string=''; cp5:string=''; cp6:string='');
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
else msg('Mount already connected',0);
end;

procedure T_indimount.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (MountDevice=nil)or(not Fready) then begin
    msg(rsError2,0);
    if not Fconnected then begin
      msg(rsNoResponseFr,0);
      msg('Is "'+Findidevice+'" a running telescope mount driver?',0);
    end
    else if (configprop=nil) then
       msg('Missing property CONFIG_PROCESS',0)
    else if (coord_prop=nil) then
       msg('Missing property EQUATORIAL_EOD_COORD or EQUATORIAL_COORD',0);
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
  if (not Fready) and InitTimer.Enabled then begin
    ConnectTimer.Enabled:=true;
  end;
 indiclient.connectDevice(Findidevice);
end;

procedure T_indimount.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg(rsServer+' '+rsDisconnected3,0);
end;

procedure T_indimount.NewDevice(dp: Basedevice);
begin
  msg('INDI server send new device: "'+dp.getDeviceName+'"',9);
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
begin
  if Assigned(FonDeviceMsg) then FonDeviceMsg(Findidevice+': '+mp.msg);
  mp.Free;
end;

procedure T_indimount.NewProperty(indiProp: IndiProperty);
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
  else if (proptype=INDI_NUMBER)and(coord_prop=nil)and(propname='EQUATORIAL_EOD_COORD') then begin
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
   else if (proptype=INDI_SWITCH)and(CoordSet=nil)and(propname='ON_COORD_SET') then begin
      CoordSet:=indiProp.getSwitch;
      CoordSetTrack:=IUFindSwitch(CoordSet,'TRACK');
      CoordSetSlew:=IUFindSwitch(CoordSet,'SLEW');
      CoordSetSync:=IUFindSwitch(CoordSet,'SYNC');
   end
   else if (proptype=INDI_SWITCH)and(parkprop=nil)and(propname='TELESCOPE_PARK') then begin
      parkprop:=indiProp.getSwitch;
      swpark:=IUFindSwitch(parkprop,'PARK');
      swunpark:=IUFindSwitch(parkprop,'UNPARK');
      if (swpark=nil)or(swunpark=nil) then parkprop:=nil;
      if Assigned(FonParkChange) then FonParkChange(self);
   end
   else if (proptype=INDI_SWITCH)and(TrackState=nil)and(propname='TELESCOPE_TRACK_STATE') then begin
      TrackState:=indiProp.getSwitch;
      TrackOn:=IUFindSwitch(TrackState,'TRACK_ON');
      TrackOff:=IUFindSwitch(TrackState,'TRACK_OFF');
      if (TrackOn=nil)or(TrackOff=nil) then TrackState:=nil;
      if Assigned(FonTrackingChange) then FonTrackingChange(self);
   end
   else if (proptype=INDI_SWITCH)and(AbortmotionProp=nil)and(propname='TELESCOPE_ABORT_MOTION') then begin
      AbortmotionProp:=indiProp.getSwitch;
   end
   else if (proptype=INDI_NUMBER)and(TelescopeInfo=nil)and(propname='TELESCOPE_INFO') then begin
      TelescopeInfo:=indiProp.getNumber;
      TelescopeAperture:=IUFindNumber(TelescopeInfo,'TELESCOPE_APERTURE');
      TelescopeFocale:=IUFindNumber(TelescopeInfo,'TELESCOPE_FOCAL_LENGTH');
      if (TelescopeAperture=nil)or(TelescopeFocale=nil) then TelescopeInfo:=nil;
   end
   else if (proptype=INDI_SWITCH)and(Pier_Side=nil)and(propname='TELESCOPE_PIER_SIDE') then begin
      Pier_Side:=indiProp.getSwitch;
      Pier_East:=IUFindSwitch(Pier_Side,'PIER_EAST');
      Pier_West:=IUFindSwitch(Pier_Side,'PIER_WEST');
      if (Pier_East=nil)or(Pier_West=nil) then Pier_Side:=nil;
      if Assigned(FonPiersideChange) then FonPiersideChange(self);
   end
   else if (proptype = INDI_NUMBER) and(GeographicCoord_prop=nil)and (propname = 'GEOGRAPHIC_COORD') then
   begin
     GeographicCoord_prop := indiProp.getNumber();
     geo_lat := IUFindNumber(GeographicCoord_prop, 'LAT');
     geo_lon := IUFindNumber(GeographicCoord_prop, 'LONG');
     geo_elev := IUFindNumber(GeographicCoord_prop, 'ELEV');
      if (geo_lat=nil)or(geo_lon=nil)or(geo_elev=nil) then GeographicCoord_prop:=nil;
   end
   else if (proptype = INDI_TEXT) and(DateTime_Prop=nil)and (propname = 'TIME_UTC') then
   begin
     DateTime_Prop := indiProp.getText();
     dateutc := IUFindText(DateTime_Prop, 'UTC');
     utcoffset := IUFindText(DateTime_Prop, 'OFFSET');
     if (dateutc=nil)or(utcoffset=nil) then DateTime_Prop:=nil;
   end
   else if (proptype=INDI_NUMBER)and(Guide_NS=nil)and(propname='TELESCOPE_TIMED_GUIDE_NS') then begin
      Guide_NS:=indiProp.getNumber;
      Guide_N:=IUFindNumber(Guide_NS,'TIMED_GUIDE_N');
      Guide_S:=IUFindNumber(Guide_NS,'TIMED_GUIDE_S');
      if (Guide_N=nil)or(Guide_S=nil) then Guide_NS:=nil;
   end
   else if (proptype=INDI_NUMBER)and(Guide_WE=nil)and(propname='TELESCOPE_TIMED_GUIDE_WE') then begin
      Guide_WE:=indiProp.getNumber;
      Guide_W:=IUFindNumber(Guide_WE,'TIMED_GUIDE_W');
      Guide_E:=IUFindNumber(Guide_WE,'TIMED_GUIDE_E');
      if (Guide_W=nil)or(Guide_E=nil) then Guide_WE:=nil;
      FCanPulseGuide:=(Guide_WE<>nil); //at least guide ra is available
   end
   else if (proptype=INDI_NUMBER)and(Guide_Rate=nil)and(propname='GUIDE_RATE') then begin
      Guide_Rate:=indiProp.getNumber;
      Guide_Rate_NS:=IUFindNumber(Guide_Rate,'GUIDE_RATE_NS');
      Guide_Rate_WE:=IUFindNumber(Guide_Rate,'GUIDE_RATE_WE');
      if (Guide_Rate_NS=nil)or(Guide_Rate_WE=nil) then Guide_Rate:=nil;
   end
   else if (proptype=INDI_SWITCH)and(AlignMode=nil)and(propname='ALIGNMODE') then begin
      AlignMode:=indiProp.getSwitch;
      AlignNo:=IUFindSwitch(AlignMode,'NOALIGN');
      AlignNearest:=IUFindSwitch(AlignMode,'ALIGNNEAREST');
      AlignNstar:=IUFindSwitch(AlignMode,'ALIGNNSTAR');
      if (AlignNo=nil)or(AlignNearest=nil)or(AlignNstar=nil) then AlignMode:=nil;
   end
   else if (proptype=INDI_SWITCH)and(AlignList=nil)and(propname='ALIGNLIST') then begin
      AlignList:=indiProp.getSwitch;
      AlignListClear:=IUFindSwitch(AlignList,'ALIGNLISTCLEAR');
      if AlignListClear=nil then AlignList:=nil;
   end
   else if (proptype=INDI_SWITCH)and(AlignSyncMode=nil)and(propname='ALIGNSYNCMODE') then begin
      AlignSyncMode:=indiProp.getSwitch;
      AlignStdSync:=IUFindSwitch(AlignSyncMode,'ALIGNSTANDARDSYNC');
      AlignAppendSync:=IUFindSwitch(AlignSyncMode,'ALIGNAPPENDSYNC');
      if (AlignStdSync=nil)or(AlignAppendSync=nil) then AlignSyncMode:=nil;
   end
   else if (proptype=INDI_SWITCH)and(SyncManage=nil)and(propname='SYNCMANAGE') then begin
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
var sw: ISwitch;
begin
  if (svp.name='CONNECTION') then begin
    sw:=IUFindOnSwitch(svp);
    if (sw<>nil)and(sw.name='DISCONNECT') then begin
      Disconnect;
    end;
  end
  else if svp=parkprop then begin
     if Assigned(FonParkChange) then FonParkChange(self);
  end
  else if svp=TrackState then begin
     if Assigned(FonTrackingChange) then FonTrackingChange(self);
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
 if value then begin
    msg(rsPark);
    swpark.s:=ISS_ON
 end
 else begin
    msg(rsUnpark);
    swunpark.s:=ISS_ON;
 end;
 indiclient.sendNewSwitch(parkprop);
 indiclient.WaitBusy(parkprop,120000,2000);
 indiclient.WaitBusy(coord_prop,120000,2000);
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
begin
  result:=false;
  if (CoordSet<>nil) and (CoordSetTrack<>nil) and (coord_prop<>nil) then begin
    if Equinox=0 then
       msg(Format(rsSlewToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
    else
       msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
    IUResetSwitch(CoordSet);
    CoordSetTrack.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
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
    if Equinox=0 then
       msg(Format(rsSlewToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
    else
       msg(Format(rsSlewToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
    FMountSlewing:=true;
    IUResetSwitch(CoordSet);
    CoordSetTrack.s:=ISS_ON;
    indiclient.sendNewSwitch(CoordSet);
    if (15*abs(coord_ra.value-sra)+abs(coord_dec.value-sde))>0.5 then slewtimeout:=240000 else slewtimeout:=30000;
    coord_ra.value:=sra;
    coord_dec.value:=sde;
    indiclient.sendNewNumber(coord_prop);
    indiclient.WaitBusy(coord_prop,slewtimeout,10000);
    msg(rsSlewComplete);
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
  msg(rsMeridianFlip5);
  // point one hour to the east of meridian
  ra1:=rmod(24+1+rad2deg*CurrentSidTim/15,24);
  slew(ra1,sde);
  Wait(2);
  // point one hour to the west of target to force the flip
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
    if Equinox=0 then
       msg(Format(rsSyncToEQ, ['Local', ARToStr3(sra), DEToStr(sde)]))
    else
       msg(Format(rsSyncToEQ, ['J'+inttostr(round(Equinox)) ,ARToStr3(sra), DEToStr(sde)]));
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

function  T_indimount.GetTracking:Boolean;
begin
  result:=true;
  if (TrackState<>nil) and (TrackOn<>nil) then begin
     result:=(TrackOn.s=ISS_ON);
  end;
end;

function T_indimount.Track:Boolean;
begin
  result:=false;
  if (TrackState<>nil)then begin
    IUResetSwitch(TrackState);
    TrackOn.s:=ISS_ON;
    indiclient.sendNewSwitch(TrackState);
    result:=true;
  end
  else if (CoordSet<>nil) and (CoordSetTrack<>nil) and (coord_prop<>nil) then begin
    msg(rsStartTracking);
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
   end;
 end;
 if TrackState<>nil then begin
    IUResetSwitch(TrackState);
    TrackOff.s:=ISS_ON;
    indiclient.sendNewSwitch(TrackState);
 end;
 msg(rsStopTelescop);
end;

procedure T_indimount.SetTimeout(num:integer);
begin
 FTimeOut:=num;
 if indiclient<>nil then indiclient.Timeout:=FTimeOut;
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
     if value=alSTDSYNC then begin
       AlignStdSync.s := ISS_ON;
       msg('align mode Std Sync');
     end
     else if value=alADDPOINT then begin
       AlignAppendSync.s := ISS_ON;
       msg('align mode Add Point');
     end;
     indiclient.sendNewSwitch(AlignSyncMode);
  end;
end;

function T_indimount.ClearAlignment:boolean;
begin
  result:=false;
  if AlignList<>nil then begin
    msg('clear alignment');
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
    msg('clear delta sync');
    IUResetSwitch(SyncManage);
    SyncClearDelta.s:=ISS_ON;
    indiclient.sendNewSwitch(SyncManage);
    result:=true;
  end;
end;

function T_indimount.GetSite(var long,lat,elev: double): boolean;
begin
  result:=false;
  if GeographicCoord_prop<>nil then begin
    long:=geo_lon.Value;
    lat:=geo_lat.Value;
    elev:=geo_elev.Value;
    result:=true;
  end;
end;

function T_indimount.SetSite(long,lat,elev: double): boolean;
begin
  result:=false;
  if GeographicCoord_prop<>nil then begin
    geo_lon.Value  := long;
    geo_lat.Value  := lat;
    geo_elev.Value := elev;
    indiclient.sendNewNumber(GeographicCoord_prop);
    result:=true;
  end;
end;

function T_indimount.GetDate(var utc,offset: double): boolean;
begin
  result:=false;
  if DateTime_Prop<>nil then begin
    utc:=DateIso2DateTime(dateutc.Text);
    offset:=StrToFloatDef(trim(utcoffset.Text),0);
    result:=true;
  end;
end;

function T_indimount.SetDate(utc,offset: double): boolean;
begin
  result:=false;
  if DateTime_Prop<>nil then begin
    dateutc.Text   := FormatDateTime(dateisoshort,utc);
    utcoffset.Text := FormatFloat(f2,offset);
    indiclient.sendNewText(DateTime_Prop);
    result:=true;
  end;
end;


function T_indimount.GetGuideRateRa: double;
begin
  if Guide_Rate<>nil then begin
    result:=Guide_Rate_WE.value;
    result:=result*siderealrate/3600; // deg/sec, ascom compatibility
  end
  else result:=0;
end;

function T_indimount.GetGuideRateDe: double;
begin
  if Guide_Rate<>nil then begin
    result:=Guide_Rate_NS.value;
    result:=result*siderealrate/3600; // deg/sec, ascom compatibility
  end
  else result:=0;
end;

procedure T_indimount.SetGuideRateRa(value:double);
begin
  if Guide_Rate<>nil then begin
    Guide_Rate_WE.value:=value*3600/siderealrate;
    indiclient.sendNewNumber(Guide_Rate);
  end;
end;

procedure T_indimount.SetGuideRateDe(value:double);
begin
  if Guide_Rate<>nil then begin
    Guide_Rate_NS.value:=value*3600/siderealrate;
    indiclient.sendNewNumber(Guide_Rate);
  end;
end;

function T_indimount.PulseGuide(direction,duration:integer): boolean;
begin
result:=false;
if Guide_NS<>nil then begin
  case direction of
    0: begin
         Guide_N.Value:=duration;
         Guide_S.Value:=0;
         indiclient.sendNewNumber(Guide_NS);
         result:=true;
       end;
    1: begin
         Guide_N.Value:=0;
         Guide_S.Value:=duration;
         indiclient.sendNewNumber(Guide_NS);
         result:=true;
       end;
  end;
end;
if Guide_WE<>nil then begin
  case direction of
    2: begin
         Guide_E.Value:=duration;
         Guide_W.Value:=0;
         indiclient.sendNewNumber(Guide_WE);
         result:=true;
       end;
    3: begin
         Guide_E.Value:=0;
         Guide_W.Value:=duration;
         indiclient.sendNewNumber(Guide_WE);
         result:=true;
       end;
  end;
end;
end;

function T_indimount.GetPulseGuiding: boolean;
begin
  result:=false;
  if Guide_NS<>nil then
    result:=result or (Guide_NS.s=IPS_BUSY);
  if Guide_WE<>nil then
    result:=result or (Guide_WE.s=IPS_BUSY);
end;

end.

