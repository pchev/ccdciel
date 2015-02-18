unit eqmod_int;

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
     ExtCtrls, Forms, Classes, SysUtils;

type
  TNotifyMsg = procedure(msg:string) of object;
  TNotifyNum = procedure(d: double) of object;
  TDeviceStatus = (devDisconnected, devConnecting, devConnected);
  TNumRange = record
               min,max,step: double;
              end;

const
  UnitRange:TNumRange = (min:1;max:1;step:1);
  NullRange:TNumRange = (min:0;max:0;step:0);
  NullCoord=-9999;


type

T_indieqmod = class(TIndiBaseClient)
 private
   InitTimer: TTimer;
   MountDevice: Basedevice;
   Mountport: ITextVectorProperty;
   coord_prop: INumberVectorProperty;
   coord_ra:   INumber;
   coord_dec:  INumber;
   horz_prop: INumberVectorProperty;
   horz_az:   INumber;
   horz_alt:  INumber;
   TelescopeInfo: INumberVectorProperty;
   TelescopeAperture, TelescopeFocale: INumber;
   Mlst: INumberVectorProperty;
   PierSide: ISwitchVectorProperty;
   Sim: ISwitchVectorProperty;
   AbortMotion: ISwitchVectorProperty;
   MotionNS: ISwitchVectorProperty;
   MotionN: ISwitch;
   MotionS: ISwitch;
   MotionWE: ISwitchVectorProperty;
   MotionW: ISwitch;
   MotionE: ISwitch;
   RevDec: ISwitchVectorProperty;
   SlewMode: ISwitchVectorProperty;
   SlewSpeed: INumberVectorProperty;
   RAslew, DEslew: INumber;
   TrackM: ISwitchVectorProperty;
   TrackSidereal,TrackLunar,TrackSolar,TrackCustom: ISwitch;
   TrackR: INumberVectorProperty;
   TrackRra, TrackRde: INumber;
   eod_coord:  boolean;
   Fready,Fconnected: boolean;
   Findiserver, Findiserverport, Findidevice, Findideviceport: string;
   FSimulation: Boolean;
   FSlewPreset: TStringList;
   FStatus: TDeviceStatus;
   FonMsg: TNotifyMsg;
   FonStatusChange: TNotifyEvent;
   FonCoordChange: TNotifyEvent;
   FonAltAZChange: TNotifyEvent;
   FonLSTChange: TNotifyEvent;
   FonPierSideChange: TNotifyEvent;
   FonSlewSpeedChange: TNotifyEvent;
   FonSlewModeChange: TNotifyEvent;
   FonRevDecChange: TNotifyEvent;
   FonTrackModeChange: TNotifyEvent;
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
   function  GetRA:double;
   function  GetDec:double;
   function  GetAZ:double;
   function  GetALT:double;
   function  GetLST:double;
   function  GetEquinox: double;
   function  GetAperture:double;
   function  GetPierSideLbl: string;
   function  GetFocaleLength:double;
   function  GetRevDec: boolean;
   procedure SetRevDec(value: boolean);
   function  GetRASlewSpeed:integer;
   procedure SetRASlewSpeed(value:integer);
   function  GetDESlewSpeed:integer;
   procedure SetDESlewSpeed(value:integer);
   function  GetActiveSlewPreset: integer;
   procedure SetActiveSlewPreset(value: integer);
   function  GetRASlewSpeedRange: TNumRange;
   function  GetDESlewSpeedRange: TNumRange;
   function  GetTrackmode: integer;
   procedure SetTrackmode(value: integer);
   procedure msg(txt: string);
 public
   constructor Create;
   destructor  Destroy; override;
   Procedure Connect;
   Procedure Disconnect;
   procedure MotionNorth;
   procedure MotionSouth;
   procedure MotionWest;
   procedure MotionEast;
   procedure MotionStop;
   property indiserver: string read Findiserver write Findiserver;
   property indiserverport: string read Findiserverport write Findiserverport;
   property indidevice: string read Findidevice write Findidevice;
   property indideviceport: string read Findideviceport write Findideviceport;
   property Simulation: Boolean read FSimulation write FSimulation;
   property RA: double read GetRA;
   property Dec: double read GetDec;
   property AZ: double read GetAZ;
   property ALT: double read GetALT;
   property LST: double read GetLST;
   property PierSideLbl: string read GetPierSideLbl;
   property Equinox: double read GetEquinox;
   property Aperture: double read GetAperture;
   property FocaleLength: double read GetFocaleLength;
   property ReverseDec: Boolean read GetRevDec write SetRevDec;
   property SlewPreset: TStringList read FSlewPreset;
   property ActiveSlewPreset: integer read GetActiveSlewPreset write SetActiveSlewPreset;
   property RASlewSpeedRange: TNumRange read GetRASlewSpeedRange;
   property DESlewSpeedRange: TNumRange read GetDESlewSpeedRange;
   property RASlewSpeed: integer read GetRASlewSpeed write SetRASlewSpeed;
   property DESlewSpeed: integer read GetDESlewSpeed write SetDESlewSpeed;
   property TrackMode: integer read GetTrackmode write SetTrackmode;
   property Status: TDeviceStatus read FStatus;
   property onDestroy: TNotifyEvent read FonDestroy write FonDestroy;
   property onMsg: TNotifyMsg read FonMsg write FonMsg;
   property onStatusChange: TNotifyEvent read FonStatusChange write FonStatusChange;
   property onCoordChange: TNotifyEvent read FonCoordChange write FonCoordChange;
   property onAltAZChange: TNotifyEvent read FonAltAZChange write FonAltAZChange;
   property onLSTChange: TNotifyEvent read FonLSTChange write FonLSTChange;
   property onPierSideChange: TNotifyEvent read FonPierSideChange write FonPierSideChange;
   property onRevDecChange: TNotifyEvent read FonRevDecChange write FonRevDecChange;
   property onSlewSpeedChange: TNotifyEvent read FonSlewSpeedChange write FonSlewSpeedChange;
   property onSlewModeChange: TNotifyEvent read FonSlewModeChange write FonSlewModeChange;
   property onTrackModeChange: TNotifyEvent read FonTrackModeChange write FonTrackModeChange;
end;

implementation

constructor T_indieqmod.Create;
begin
 inherited Create;
 FSlewPreset:=TStringList.Create;
 ClearStatus;
 Findiserver:='localhost';
 Findiserverport:='7624';
 Findidevice:='';
 Findideviceport:='';
 InitTimer:=TTimer.Create(nil);
 InitTimer.Enabled:=false;
 InitTimer.Interval:=3000;
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

destructor  T_indieqmod.Destroy;
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
 FSlewPreset.Free;
 inherited Destroy;
end;

procedure T_indieqmod.ClearStatus;
begin
    MountDevice:=nil;
    Mountport:=nil;
    TelescopeInfo:=nil;
    coord_prop:=nil;
    horz_prop:=nil;
    PierSide:=nil;
    Sim:=nil;
    Mlst:=nil;
    AbortMotion:=nil;
    MotionNS:=nil;
    MotionWE:=nil;
    RevDec:=nil;
    SlewMode:=nil;
    SlewSpeed:=nil;
    TrackM:=nil;
    TrackR:=nil;
    Fready:=false;
    Fconnected := false;
    FStatus := devDisconnected;
    FSlewPreset.Clear;
    if Assigned(FonStatusChange) then FonStatusChange(self);
end;

procedure T_indieqmod.CheckStatus;
begin
    if Fconnected and
       (coord_prop<>nil) and
       (horz_prop<>nil) and
       (Mlst<>nil) and
       (PierSide<>nil) and
       (Sim<>nil) and
       (AbortMotion<>nil) and
       (MotionNS<>nil) and
       (MotionWE<>nil) and
       (RevDec<>nil) and
       (TrackM<>nil) and
       (TrackR<>nil) and
       (SlewMode<>nil) and
       (SlewSpeed<>nil)
    then begin
       FStatus := devConnected;
       if (not Fready) and Assigned(FonStatusChange) then FonStatusChange(self);
       Fready:=true;
    end;
end;

procedure T_indieqmod.msg(txt: string);
begin
  if Assigned(FonMsg) then FonMsg(txt);
end;

Procedure T_indieqmod.Connect;
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

procedure T_indieqmod.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Enabled:=false;
  if (MountDevice=nil)or(not Fready) then begin
     msg('No response from server');
     msg('Is "'+indidevice+'" a running telescope mount driver?');
     Disconnect;
  end;
end;

Procedure T_indieqmod.Disconnect;
begin
Terminate;
ClearStatus;
end;

procedure T_indieqmod.ServerConnected(Sender: TObject);
begin
   if (Mountport<>nil)and(Findideviceport<>'') then begin
      Mountport.tp[0].text:=Findideviceport;
      sendNewText(Mountport);
   end;
   if (Sim<>nil) and FSimulation then begin
     IUResetSwitch(Sim);
     Sim.sp[0].s:=ISS_ON;
     sendNewSwitch(Sim);
   end;
   connectDevice(Findidevice);
end;

procedure T_indieqmod.ServerDisconnected(Sender: TObject);
begin
  FStatus := devDisconnected;
  if Assigned(FonStatusChange) then FonStatusChange(self);
  msg('Mount server disconnected');
end;

procedure T_indieqmod.NewDevice(dp: Basedevice);
begin
  //writeln('Newdev: '+dp.getDeviceName);
  if dp.getDeviceName=Findidevice then begin
     Fconnected:=true;
     MountDevice:=dp;
  end;
end;

procedure T_indieqmod.NewMessage(txt: string);
begin
  msg(txt);
end;

procedure T_indieqmod.NewProperty(indiProp: IndiProperty);
var propname: string;
    proptype: INDI_TYPE;
    i: integer;
begin
  propname:=indiProp.getName;
  proptype:=indiProp.getType;

  if (proptype=INDI_TEXT)and(propname='DEVICE_PORT') then begin
     Mountport:=indiProp.getText;
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
  else if (proptype=INDI_NUMBER)and(propname='HORIZONTAL_COORD') then begin
      horz_prop:=indiProp.getNumber;
      horz_az:=IUFindNumber(horz_prop,'AZ');
      horz_alt:=IUFindNumber(horz_prop,'ALT');
      if (horz_az=nil)or(horz_alt=nil) then horz_prop:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='TELESCOPE_INFO') then begin
     TelescopeInfo:=indiProp.getNumber;
     TelescopeAperture:=IUFindNumber(TelescopeInfo,'TELESCOPE_APERTURE');
     TelescopeFocale:=IUFindNumber(TelescopeInfo,'TELESCOPE_FOCAL_LENGTH');
     if (TelescopeAperture=nil)or(TelescopeFocale=nil) then TelescopeInfo:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='TIME_LST') then begin
     Mlst:=indiProp.getNumber;
  end
  else if (proptype=INDI_SWITCH)and(propname='PIERSIDE') then begin
     PierSide:=indiProp.getSwitch;
  end
  else if (proptype=INDI_SWITCH)and(propname='SIMULATION') then begin
     Sim:=indiProp.getSwitch;
  end
  else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_ABORT_MOTION') then begin
     AbortMotion:=indiProp.getSwitch;
  end
  else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_MOTION_NS') then begin
     MotionNS:=indiProp.getSwitch;
     MotionN:=IUFindSwitch(MotionNS,'MOTION_NORTH');
     MotionS:=IUFindSwitch(MotionNS,'MOTION_SOUTH');
     if (MotionN=nil)or(MotionS=nil) then MotionNS:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='TELESCOPE_MOTION_WE') then begin
     MotionWE:=indiProp.getSwitch;
     MotionW:=IUFindSwitch(MotionWE,'MOTION_WEST');
     MotionE:=IUFindSwitch(MotionWE,'MOTION_EAST');
     if (MotionW=nil)or(MotionE=nil) then MotionWE:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='REVERSEDEC') then begin
     RevDec:=indiProp.getSwitch;
  end
  else if (proptype=INDI_SWITCH)and(propname='SLEWMODE') then begin
     SlewMode:=indiProp.getSwitch;
     for i:=0 to SlewMode.nsp-1 do begin
       FSlewPreset.Add(SlewMode.sp[i].lbl);
     end;
  end
  else if (proptype=INDI_NUMBER)and(propname='SLEWSPEEDS') then begin
     SlewSpeed:=indiProp.getNumber;
     RAslew:=IUFindNumber(SlewSpeed,'RASLEW');
     DEslew:=IUFindNumber(SlewSpeed,'DESLEW');
     if (RAslew=nil)or(DEslew=nil) then SlewSpeed:=nil;
  end
  else if (proptype=INDI_NUMBER)and(propname='TRACKRATES') then begin
     TrackR:=indiProp.getNumber;
     TrackRra:=IUFindNumber(TrackR,'RATRACKRATE');
     TrackRde:=IUFindNumber(TrackR,'DETRACKRATE');
     if (TrackRra=nil)or(TrackRde=nil) then TrackR:=nil;
  end
  else if (proptype=INDI_SWITCH)and(propname='TRACKMODE') then begin
     TrackM:=indiProp.getSwitch;
     TrackSidereal:=IUFindSwitch(TrackM,'SIDEREAL');
     TrackLunar:=IUFindSwitch(TrackM,'LUNAR');
     TrackSolar:=IUFindSwitch(TrackM,'SOLAR');
     TrackCustom:=IUFindSwitch(TrackM,'CUSTOM');
     if (TrackSidereal=nil)or(TrackLunar=nil)or(TrackSolar=nil)or(TrackCustom=nil) then TrackM:=nil;
  end
  ;
  CheckStatus;
end;

procedure T_indieqmod.NewNumber(nvp: INumberVectorProperty);
begin
  if nvp=coord_prop then begin
     if Assigned(FonCoordChange) then FonCoordChange(self);
  end else if nvp=horz_prop then begin
     if Assigned(FonAltAZChange) then FonAltAZChange(self);
  end else if nvp=Mlst then begin
     if Assigned(FonLSTChange) then FonLSTChange(self);
  end else if nvp=SlewSpeed then begin
     if Assigned(FonSlewSpeedChange) then FonSlewSpeedChange(self);
  end;
end;

procedure T_indieqmod.NewText(tvp: ITextVectorProperty);
begin
//  writeln('NewText: '+tvp.name+' '+tvp.tp[0].text);
end;

procedure T_indieqmod.NewSwitch(svp: ISwitchVectorProperty);
begin
  if svp=PierSide then begin
     if Assigned(FonPierSideChange) then FonPierSideChange(self);
  end else if svp=SlewMode then begin
        if Assigned(FonSlewModeChange) then FonSlewModeChange(self);
  end else if svp=RevDec then begin
        if Assigned(FonRevDecChange) then FonRevDecChange(self);
  end else if svp=TrackM then begin
        if Assigned(FonTrackModeChange) then FonTrackModeChange(self);
  end;
end;

procedure T_indieqmod.NewLight(lvp: ILightVectorProperty);
begin
//  writeln('NewLight: '+lvp.name);
end;

function  T_indieqmod.GetRA:double;
begin
if coord_prop<>nil then begin;
  result:=coord_ra.value;
end
else result:=NullCoord;
end;

function  T_indieqmod.GetDec:double;
begin
if coord_prop<>nil then begin;
  result:=coord_dec.value;
end
else result:=NullCoord;
end;

function  T_indieqmod.GetAZ:double;
begin
if horz_prop<>nil then begin;
  result:=horz_az.value;
end
else result:=NullCoord;
end;

function  T_indieqmod.GetALT:double;
begin
if horz_prop<>nil then begin;
  result:=horz_alt.value;
end
else result:=NullCoord;
end;

function  T_indieqmod.GetLST:double;
begin
if Mlst<>nil then begin;
  result:=Mlst.np[0].value;
end
else result:=0;
end;

function  T_indieqmod.GetEquinox: double;
begin
 if eod_coord then result:=0
              else result:=2000;
end;

function  T_indieqmod.GetAperture:double;
begin
if TelescopeInfo<>nil then begin;
  result:=TelescopeAperture.value;
end
else result:=-1;
end;

function  T_indieqmod.GetFocaleLength:double;
begin
if TelescopeInfo<>nil then begin;
  result:=TelescopeFocale.value;
end
else result:=-1;
end;

function  T_indieqmod.GetPierSideLbl: string;
var sw:ISwitch;
begin
 if PierSide<>nil then begin
    sw:=IUFindOnSwitch(PierSide);
    result:=sw.lbl;
 end
 else result:='?';
end;

function  T_indieqmod.GetRevDec: boolean;
begin
 if RevDec<>nil then begin
   result:=RevDec.sp[0].s=ISS_ON;
 end;
end;

procedure T_indieqmod.SetRevDec(value: boolean);
begin
 if RevDec<>nil then begin
   IUResetSwitch(RevDec);
   if value then
      RevDec.sp[0].s:=ISS_ON
   else
      RevDec.sp[1].s:=ISS_ON;
   sendNewSwitch(RevDec);
 end;
end;

procedure T_indieqmod.MotionNorth;
begin
if MotionNS<>nil then begin;
  IUResetSwitch(MotionNS);
  MotionN.s:=ISS_ON;
  sendNewSwitch(MotionNS);
end;
end;

procedure T_indieqmod.MotionSouth;
begin
if MotionNS<>nil then begin;
  IUResetSwitch(MotionNS);
  MotionS.s:=ISS_ON;
  sendNewSwitch(MotionNS);
end;
end;

procedure T_indieqmod.MotionWest;
begin
if MotionWE<>nil then begin;
  IUResetSwitch(MotionWE);
  MotionW.s:=ISS_ON;
  sendNewSwitch(MotionWE);
end;
end;

procedure T_indieqmod.MotionEast;
begin
if MotionWE<>nil then begin;
  IUResetSwitch(MotionWE);
  MotionE.s:=ISS_ON;
  sendNewSwitch(MotionWE);
end;
end;

procedure T_indieqmod.MotionStop;
begin
if (MotionNS<>nil)and(MotionWE<>nil) then begin;
  IUResetSwitch(MotionNS);
  sendNewSwitch(MotionNS);
  IUResetSwitch(MotionWE);
  sendNewSwitch(MotionWE);
end;
end;

function  T_indieqmod.GetRASlewSpeed:integer;
begin
  if SlewSpeed<>nil then begin
     result:=trunc(RAslew.value);
     if result=0 then result:=1;
  end;
end;

procedure T_indieqmod.SetRASlewSpeed(value:integer);
begin
  if SlewSpeed<>nil then begin
     RAslew.value:=value;
     sendNewNumber(SlewSpeed);
  end;
end;

function  T_indieqmod.GetDESlewSpeed:integer;
begin
  if SlewSpeed<>nil then begin
     result:=trunc(DEslew.value);
     if result=0 then result:=1;
  end;
end;

procedure T_indieqmod.SetDESlewSpeed(value:integer);
begin
  if SlewSpeed<>nil then begin
     DEslew.value:=value;
     sendNewNumber(SlewSpeed);
  end;
end;

function T_indieqmod.GetRASlewSpeedRange: TNumRange;
begin
  if SlewSpeed<>nil then begin
     result.min:=RAslew.min;
     result.max:=RAslew.max;
     result.step:=RAslew.step;
  end;
end;

function T_indieqmod.GetDESlewSpeedRange: TNumRange;
begin
  if SlewSpeed<>nil then begin
     result.min:=DEslew.min;
     result.max:=DEslew.max;
     result.step:=DEslew.step;
  end;
end;

function T_indieqmod.GetActiveSlewPreset: integer;
var i: integer;
begin
 if SlewMode<>nil then begin
   for i := 0 to SlewMode.nsp-1 do
    if SlewMode.sp[i].s = ISS_ON then
       exit(i);
 end;
end;

procedure T_indieqmod.SetActiveSlewPreset(value: integer);
begin
 if SlewMode<>nil then begin
    IUResetSwitch(SlewMode);
    SlewMode.sp[value].s := ISS_ON;
    sendNewSwitch(SlewMode);
 end;
end;

function  T_indieqmod.GetTrackmode: integer;
var i: integer;
begin
 if TrackM<>nil then begin
   result:=-1;
   for i := 0 to TrackM.nsp-1 do
    if SlewMode.sp[i].s = ISS_ON then
       result:=i;
 end;
end;

procedure T_indieqmod.SetTrackmode(value: integer);
begin
 if TrackM<>nil then begin
    IUResetSwitch(TrackM);
    if value>=0 then TrackM.sp[value].s:=ISS_ON;
    sendNewSwitch(TrackM);
 end;
end;

end.

